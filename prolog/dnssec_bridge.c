#include <SWI-Prolog.h>
#include <HsFFI.h>
#include <stdint.h>
#include <stdlib.h>
#include "Bridge_stub.h"

/* All term references belong to the current foreign call. Haskell copies
 * text before returning it to SWI and never retains a term reference. */
int dns_term_kind(uintptr_t term) {
    term_t t = (term_t)term;
    if (PL_get_nil(t)) return 0;
    if (PL_is_list(t)) return 1;
    if (PL_is_atom(t)) return 2;
    if (PL_is_string(t)) return 3;
    if (PL_is_integer(t)) return 4;
    if (PL_is_compound(t)) return 5;
    return -1;
}
char* dns_term_text(uintptr_t term) {
    char *text = NULL;
    size_t length;
    if (!PL_get_nchars((term_t)term, &length, &text,
                      CVT_ATOM | CVT_STRING | CVT_INTEGER | BUF_MALLOC | REP_UTF8))
        return NULL;
    /* Embedded NUL is rejected rather than silently truncating input. */
    for (size_t i = 0; i < length; i++) {
        if (text[i] == '\0') { PL_free(text); return NULL; }
    }
    return text;
}
void dns_text_free(char *text) { PL_free(text); }
char* dns_term_functor(uintptr_t term, size_t *arity) {
    atom_t name;
    if (!PL_get_name_arity_sz((term_t)term, &name, arity)) return NULL;
    term_t atom = PL_new_term_ref();
    PL_put_atom(atom, name);
    return dns_term_text(atom);
}
uintptr_t dns_term_arg(uintptr_t term, size_t index) {
    term_t arg = PL_new_term_ref();
    return PL_get_arg_sz(index, (term_t)term, arg) ? (uintptr_t)arg : 0;
}
uintptr_t dns_list_part(uintptr_t term, int tail) {
    term_t head = PL_new_term_ref(), rest = PL_new_term_ref();
    if (!PL_get_list((term_t)term, head, rest)) return 0;
    return (uintptr_t)(tail ? rest : head);
}

static foreign_t check(term_t predicate) {
    if (!PL_is_ground(predicate)) return PL_instantiation_error(predicate);
    if (!PL_is_acyclic(predicate)) return PL_type_error("acyclic_term", predicate);
    char *error = NULL;
    HsInt32 result = dns_check((HsWord)(uintptr_t)predicate, &error);
    if (result < 0) {
        term_t exception = PL_new_term_ref();
        int built = PL_unify_term(exception,
            PL_FUNCTOR_CHARS, "error", 2,
                PL_FUNCTOR_CHARS, "dnssec_input_error", 1,
                    PL_UTF8_CHARS, error ? error : "Haskell verification failed",
                PL_CHARS, "dnssec:check/2");
        free(error);
        return built ? PL_raise_exception(exception) : FALSE;
    }
    return result != 0;
}

install_t install(void) {
    int argc = 1;
    char *argv[] = { "dnssec_native", NULL };
    char **args = argv;
    hs_init(&argc, &args);
    PL_register_foreign_in_module("dnssec", "check", 2, check, 0);
}

install_t uninstall(void) { hs_exit(); }
