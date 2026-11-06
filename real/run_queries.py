#!/usr/bin/env python3
"""Evaluate a random pending query with decdns and dig through one proxy."""

import argparse
import os
from pathlib import Path
import random
import signal
import subprocess
import time


REAL_DIR = Path(__file__).resolve().parent
REPO_DIR = REAL_DIR.parent
QUERIES = REAL_DIR / "100k_mixed.txt"
TOOL_OUTPUT = REAL_DIR / "tool"
DIG_OUTPUT = REAL_DIR / "dig"
TOOL_TIMEOUT_SECONDS = 30


def terminate(process: subprocess.Popen) -> None:
    """Stop the timed command and its children, including the Haskell process."""
    try:
        os.killpg(process.pid, signal.SIGTERM)
    except ProcessLookupError:
        return
    try:
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        pass
    # The wrapper may exit before a child that ignores SIGTERM. Always clean
    # up the whole group before draining stdout or returning from a stop.
    try:
        os.killpg(process.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    process.wait()


def run_and_save(command: list[str], output_path: Path, timeout: int) -> None:
    with subprocess.Popen(
        command,
        cwd=REPO_DIR,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        start_new_session=True,
    ) as process:
        try:
            try:
                output, _ = process.communicate(timeout=timeout)
            except subprocess.TimeoutExpired:
                terminate(process)
                output, _ = process.communicate()
                output += f"\n[command timed out after {timeout} seconds]\n"
            if process.returncode != 0:
                output += f"\n[command exited with status {process.returncode}]\n"
        except BaseException:
            terminate(process)
            raise

    # A stopped process must never leave an empty/truncated completed result.
    temporary = output_path.with_suffix(".tmp")
    try:
        temporary.write_text(output)
        temporary.replace(output_path)
    finally:
        temporary.unlink(missing_ok=True)


def evaluate(queries: Path, tool_dir: Path, dig_dir: Path, timeout: int,
             dig_timeout: int, limit: int | None = None) -> int:
    entries = []
    for line_number, line in enumerate(queries.read_text().splitlines(), start=1):
        fields = line.split()
        if len(fields) != 2:
            raise ValueError(f"expected NAME TYPE on line {line_number}: {line!r}")
        entries.append((line_number, *fields))
    # Sampling without replacement visits every entry in random order and
    # terminates even when almost all queries already have paired results.
    random.shuffle(entries)
    tool_dir.mkdir(parents=True, exist_ok=True)
    dig_dir.mkdir(parents=True, exist_ok=True)
    completed = 0
    for line_number, name, record_type in entries:
        if limit is not None and completed >= limit:
            break
        tool_path = tool_dir / f"{line_number}.txt"
        dig_path = dig_dir / f"{line_number}.txt"
        if tool_path.is_file() and dig_path.is_file():
            continue
        # Discard a lone old result: both outputs must come from this attempt.
        # If stopped between programs, the missing counterpart causes BOTH to
        # be rerun on the next start.
        tool_path.unlink(missing_ok=True)
        dig_path.unlink(missing_ok=True)
        print(f"Evaluating line {line_number}: {name} {record_type}", flush=True)
        started = time.monotonic()
        run_and_save(
            ["/usr/bin/time", "-v", "decdns", "--timeout", str(timeout),
             "query", "root-addresses", "root-keys", record_type, name,
             "+RTS", "-N2", "-RTS"],
            tool_path, timeout + 5,
        )
        run_and_save(
            ["/usr/bin/time", "-v", "dig", "-4", "@198.41.0.4", name, record_type,
             "+trace", "+norecurse", "+dnssec", "+nocookie", "+noadflag",
             "+bufsize=1232", "+time=3", "+tries=1"],
            dig_path, dig_timeout,
        )
        completed += 1
        print(f"Finished line {line_number} in {time.monotonic() - started:.2f}s", flush=True)
    print(f"Evaluation finished: {completed} query pairs written", flush=True)
    return completed


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--queries", type=Path, default=QUERIES)
    parser.add_argument("--tool-output", type=Path, default=TOOL_OUTPUT)
    parser.add_argument("--dig-output", type=Path, default=DIG_OUTPUT)
    parser.add_argument("--limit", type=int, metavar="N", help="evaluate at most N pending query pairs")
    parser.add_argument("--timeout", type=int, default=TOOL_TIMEOUT_SECONDS, metavar="SECONDS",
                        help="tool query timeout (default: 30)")
    parser.add_argument("--dig-timeout", type=int, default=30, metavar="SECONDS",
                        help="timeout for the whole dig trace (default: 30)")
    args = parser.parse_args()
    if args.limit is not None and args.limit < 0:
        parser.error("--limit must be non-negative")
    if args.timeout <= 0 or args.dig_timeout <= 0:
        parser.error("timeouts must be positive")
    # New files remain writable to the host user's group through bind mounts.
    os.umask(0o002)

    def stop(_signal, _frame):
        raise KeyboardInterrupt

    signal.signal(signal.SIGTERM, stop)
    try:
        evaluate(args.queries, args.tool_output, args.dig_output,
                 args.timeout, args.dig_timeout, args.limit)
    except KeyboardInterrupt:
        print("Evaluation stopped; incomplete pairs will be rerun on restart", flush=True)


if __name__ == "__main__":
    main()
