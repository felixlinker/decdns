#!/usr/bin/env python3
"""Print query filenames where both outputs contain different answers."""

import ipaddress
import re
from pathlib import Path


REAL_DIR = Path(__file__).resolve().parent
QUERIES = REAL_DIR / "100k_mixed.txt"
TOOL_OUTPUT = REAL_DIR / "tool"
DIG_OUTPUT = REAL_DIR / "dig"


def tool_answer(text: str, record_type: str) -> set[tuple[str, ...]] | None:
    """Read the answer list from the tool's final Prolog query term."""
    query = re.search(r"^query\(.*", text, re.MULTILINE)
    if query is None:
        return None

    line = query.group(0)
    # The answer list is the third top-level argument. Find its opening bracket
    # and scan to the matching close, accounting for nested lists and strings.
    depth = 0
    quoted = False
    escaped = False
    answer_start = None
    list_index = 0
    for index, char in enumerate(line):
        if quoted:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                quoted = False
            continue
        if char == '"':
            quoted = True
        elif char == '[':
            if depth == 0:
                list_index += 1
            if list_index == 2 and answer_start is None:
                answer_start = index
            if answer_start is not None:
                depth += 1
        elif char == ']' and answer_start is not None:
            depth -= 1
            if depth == 0:
                answer_text = line[answer_start + 1:index]
                break
    else:
        return None

    terms = split_terms(answer_text)
    if not terms:
        return None
    parsed = set()
    for term in terms:
        value = parse_tool_record(term, record_type)
        if value is None:
            return None
        parsed.add(value)
    return parsed


def split_terms(value: str) -> list[str]:
    terms = []
    start = 0
    depth = 0
    quoted = False
    escaped = False
    for index, char in enumerate(value):
        if quoted:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                quoted = False
        elif char == '"':
            quoted = True
        elif char in "([":
            depth += 1
        elif char in ")]":
            depth -= 1
        elif char == "," and depth == 0:
            terms.append(value[start:index].strip())
            start = index + 1
    last = value[start:].strip()
    if last:
        terms.append(last)
    return terms


def tool_name(list_term: str) -> str | None:
    match = re.fullmatch(r"\[(.*?)\]", list_term)
    if match is None:
        return None
    labels = re.findall(r"'((?:\\.|[^'])*)'|([a-zA-Z0-9_*~-]+)", match.group(1))
    parts = [quoted or atom for quoted, atom in labels]
    return ".".join(reversed(parts)).lower() + "."


def parse_tool_record(term: str, record_type: str) -> tuple[str, ...] | None:
    match = re.fullmatch(r"([a-z0-9]+)\((.*)\)", term)
    if match is None or match.group(1).upper() != record_type:
        return None
    args = split_terms(match.group(2))

    if record_type in {"A", "AAAA"} and len(args) == 1:
        value = re.fullmatch(r'"((?:\\.|[^"\\])*)"', args[0])
        if value is not None:
            try:
                return (ipaddress.ip_address(value.group(1)).compressed.lower(),)
            except ValueError:
                return None
    if record_type in {"CNAME", "DNAME", "NS"} and len(args) == 1:
        name = tool_name(args[0])
        return (name,) if name is not None else None
    if record_type == "MX" and len(args) == 2:
        name = tool_name(args[1])
        return (args[0], name) if name is not None else None
    if record_type == "SOA" and len(args) == 7:
        names = [tool_name(args[0]), tool_name(args[1])]
        if None not in names:
            return (*names, *args[2:])
    if record_type == "TXT" and len(args) == 1:
        value = re.fullmatch(r'"((?:\\.|[^"\\])*)"', args[0])
        if value is not None:
            return (value.group(1),)
    if record_type == "DNSKEY" and len(args) == 3:
        value = re.fullmatch(r'"((?:\\.|[^"\\])*)"', args[2])
        if value is not None:
            return (args[0], args[1], "".join(value.group(1).split()))
    if record_type == "DS" and len(args) == 4:
        value = re.fullmatch(r'"((?:\\.|[^"\\])*)"', args[3])
        if value is not None:
            return (args[0], args[1], args[2], value.group(1).lower())
    return None


def dig_records(text: str) -> list[tuple[str, str, str]]:
    records = []
    for line in text.splitlines():
        fields = line.split(None, 4)
        if len(fields) == 5 and fields[2].upper() == "IN":
            records.append((fields[0].lower(), fields[3].upper(), fields[4]))
    return records


def normalize_dig(record_type: str, rdata: str) -> tuple[str, ...] | None:
    fields = rdata.split()
    if record_type in {"A", "AAAA"} and len(fields) == 1:
        try:
            return (ipaddress.ip_address(fields[0]).compressed.lower(),)
        except ValueError:
            return None
    if record_type in {"CNAME", "DNAME", "NS"} and len(fields) == 1:
        return (fields[0].lower().rstrip(".") + ".",)
    if record_type == "MX" and len(fields) == 2:
        return (fields[0], fields[1].lower().rstrip(".") + ".")
    if record_type == "SOA" and len(fields) == 7:
        return (fields[0].lower().rstrip(".") + ".",
                fields[1].lower().rstrip(".") + ".", *fields[2:])
    if record_type == "TXT":
        chunks = re.findall(r'"((?:\\.|[^"\\])*)"', rdata)
        return ("".join(chunks),) if chunks else None
    if record_type == "DNSKEY" and len(fields) >= 4 and fields[1] == "3":
        return (fields[0], fields[2], "".join(fields[3:]))
    if record_type == "DS" and len(fields) == 4:
        return (fields[0], fields[1], fields[2], fields[3].lower())
    return None


def dig_answer(text: str, name: str, record_type: str) -> set[tuple[str, ...]] | None:
    records = dig_records(text)
    if not records:
        return None

    # Follow aliases in the trace output, but compare only the requested RR
    # type. Referrals and DNSSEC records are ignored.
    pending = [name.lower()]
    visited = set()
    answers = set()
    while pending:
        owner = pending.pop()
        if owner in visited:
            continue
        visited.add(owner)
        for record_owner, rrtype, rdata in records:
            if record_owner != owner:
                continue
            if rrtype == "CNAME" and record_type != "CNAME":
                target = normalize_dig("CNAME", rdata)
                if target is not None:
                    pending.append(target[0])
            elif rrtype == record_type:
                answer = normalize_dig(record_type, rdata)
                if answer is not None:
                    answers.add(answer)
    return answers or None


def main() -> None:
    for line_number, line in enumerate(QUERIES.read_text().splitlines(), start=1):
        fields = line.split()
        if len(fields) != 2:
            continue
        name, record_type = fields
        filename = f"{line_number}.txt"
        tool_path = TOOL_OUTPUT / filename
        dig_path = DIG_OUTPUT / filename
        if not tool_path.is_file() or not dig_path.is_file():
            continue

        tool = tool_answer(tool_path.read_text(), record_type)
        dig = dig_answer(dig_path.read_text(), name, record_type)
        if tool is not None and dig is not None and tool != dig:
            print(filename)


if __name__ == "__main__":
    main()
