#!/usr/bin/env python3
"""Run each query in 100k_mixed.txt through dns-regressor and dig +trace."""

import argparse
from itertools import islice
from pathlib import Path
import subprocess


REAL_DIR = Path(__file__).resolve().parent
REPO_DIR = REAL_DIR.parent
QUERIES = REAL_DIR / "100k_mixed.txt"
TOOL_OUTPUT = REAL_DIR / "tool"
DIG_OUTPUT = REAL_DIR / "dig"
TOOL_TIMEOUT_SECONDS = 15


def run_and_save(command: list[str], output_path: Path, timeout: int | None = None) -> None:
    try:
        result = subprocess.run(
            command,
            cwd=REPO_DIR,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            timeout=timeout,
            check=False,
        )
        output = result.stdout
        if result.returncode != 0:
            output += f"\n[command exited with status {result.returncode}]\n"
    except subprocess.TimeoutExpired as error:
        output = error.stdout or ""
        if isinstance(output, bytes):
            output = output.decode(errors="replace")
        output += f"\n[command timed out after {timeout} seconds]\n"

    output_path.write_text(output)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--limit",
        type=int,
        metavar="N",
        help="run only the first N queries",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=TOOL_TIMEOUT_SECONDS,
        metavar="SECONDS",
        help=f"tool query timeout in seconds (default: {TOOL_TIMEOUT_SECONDS})",
    )
    args = parser.parse_args()
    if args.limit is not None and args.limit < 0:
        parser.error("--limit must be non-negative")
    if args.timeout <= 0:
        parser.error("--timeout must be positive")

    TOOL_OUTPUT.mkdir(exist_ok=True)
    DIG_OUTPUT.mkdir(exist_ok=True)

    lines = enumerate(QUERIES.read_text().splitlines(), start=1)
    if args.limit is not None:
        lines = islice(lines, args.limit)

    for line_number, line in lines:
        fields = line.split()
        if len(fields) != 2:
            raise ValueError(f"expected NAME TYPE on line {line_number}: {line!r}")
        name, record_type = fields

        tool_output_path = TOOL_OUTPUT / f"{line_number}.txt"
        if not tool_output_path.exists():
            run_and_save(
                ["dns-regressor-prolog", "query", "root-addresses", "root-keys", record_type, name],
                tool_output_path,
                timeout=args.timeout,
            )

        dig_output_path = DIG_OUTPUT / f"{line_number}.txt"
        if not dig_output_path.exists():
            run_and_save(
                ["dig", "+trace", name, record_type],
                dig_output_path,
            )

        print(f"Finished query on line {line_number}", flush=True)


if __name__ == "__main__":
    main()
