#!/usr/bin/env python3
"""Pre-release gate for pyvolca — run before tagging ``pyvolca-v<version>``.

Answers, locally and automatically, "can I release pyvolca right now?". It
checks that the version is new, the changelog records it, the tree is clean,
and — the decisive one — that the engine release this pyvolca requires (its
wire floor, from :data:`volca._compat.MIN_ENGINE_HINT`) is already tagged. The
last point is what would have answered "no, not yet" when the engine carrying
the wire hadn't shipped.

    python scripts/release_precheck.py             # full gate
    python scripts/release_precheck.py --no-tests  # skip the slow pytest+build leg

Prints a PASS/WARN/FAIL table and the exact tag commands when green. Exit
non-zero on any FAIL. Stdlib only, plus the ``volca`` package it ships beside.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import re
import subprocess
import sys
import urllib.error
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent  # the pyvolca/ package dir
sys.path.insert(0, str(ROOT / "src"))

from volca._compat import MIN_ENGINE_HINT  # noqa: E402

PASS, WARN, FAIL = "PASS", "WARN", "FAIL"
Row = tuple[str, str, str]  # (status, check name, detail)

_SEMVER_RE = re.compile(r"^v?(\d+)\.(\d+)\.(\d+)$")  # tolerate a leading "v"


def _semver(text: str) -> tuple[int, int, int] | None:
    m = _SEMVER_RE.match(text.strip())
    return (int(m[1]), int(m[2]), int(m[3])) if m else None


def _git(*args: str) -> str:
    """Stripped stdout of ``git -C pyvolca <args>`` ('' on any error)."""
    proc = subprocess.run(["git", "-C", str(ROOT), *args], capture_output=True, text=True)
    return proc.stdout.strip()


def _have(module: str) -> bool:
    return importlib.util.find_spec(module) is not None


def _project_version() -> str:
    text = (ROOT / "pyproject.toml").read_text(encoding="utf-8")
    m = re.search(r'^version\s*=\s*"([^"]+)"', text, re.MULTILINE)
    return m.group(1) if m else ""


# -- individual checks --------------------------------------------------------


def check_tag_absent(v: str) -> Row:
    if _git("tag", "--list", f"pyvolca-v{v}"):
        return FAIL, "tag is free", f"pyvolca-v{v} already exists"
    return PASS, "tag is free", f"pyvolca-v{v} not yet created"


def check_newer_than_pypi(v: str) -> Row:
    try:
        req = urllib.request.Request(
            "https://pypi.org/pypi/pyvolca/json", headers={"Accept": "application/json"}
        )
        with urllib.request.urlopen(req, timeout=10) as resp:
            data = json.loads(resp.read())
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return PASS, "newer than PyPI", "package not on PyPI yet (first release)"
        return WARN, "newer than PyPI", f"PyPI query failed (HTTP {e.code})"
    except (urllib.error.URLError, TimeoutError, OSError) as e:
        return WARN, "newer than PyPI", f"PyPI unreachable ({e})"
    if v in data.get("releases", {}):
        return FAIL, "newer than PyPI", f"{v} is already published"
    latest = data.get("info", {}).get("version", "")
    vt, lt = _semver(v), _semver(latest)
    if latest and (vt is None or lt is None):
        # Don't claim "newer" on a comparison we couldn't actually make.
        return WARN, "newer than PyPI", f"can't compare {v} to non-semver latest {latest!r}"
    if vt and lt and vt <= lt:
        return FAIL, "newer than PyPI", f"{v} is not newer than {latest}"
    return PASS, "newer than PyPI", f"{v} > {latest or '(none)'}"


def check_changelog(v: str) -> Row:
    text = (ROOT / "CHANGELOG.md").read_text(encoding="utf-8")
    if any(line.startswith(f"## [{v}]") for line in text.splitlines()):
        return PASS, "changelog entry", f"## [{v}] present"
    return FAIL, "changelog entry", f"no '## [{v}]' section in CHANGELOG.md"


def check_clean_tree() -> Row:
    dirty = _git("status", "--porcelain", "--", ".")
    if dirty:
        n = len(dirty.splitlines())
        return FAIL, "clean tree", f"{n} uncommitted path(s) under pyvolca/"
    return PASS, "clean tree", "no uncommitted changes under pyvolca/"


def check_engine_released() -> Row:
    """The pivotal gate: the engine that speaks our wire floor must be tagged."""
    floor = _semver(MIN_ENGINE_HINT)
    tags = [t for t in (_semver(t) for t in _git("tag", "--list", "v[0-9]*").splitlines()) if t]
    if floor and tags and max(tags) >= floor:
        newest = ".".join(map(str, max(tags)))
        return PASS, "engine released", f"v{newest} >= v{MIN_ENGINE_HINT}"
    return FAIL, "engine released", (
        f"needs engine >= v{MIN_ENGINE_HINT}; no such tag yet — release the engine first"
    )


def _leg(name: str, cmd: list[str]) -> Row:
    proc = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True)
    if proc.returncode == 0:
        return PASS, name, "ok"
    tail = (proc.stdout + proc.stderr).strip().splitlines()[-3:]
    return FAIL, name, " / ".join(tail) or f"exit {proc.returncode}"


def check_tests_and_build(no_tests: bool, v: str) -> list[Row]:
    if no_tests:
        return [(WARN, "tests + build", "skipped (--no-tests)")]
    rows: list[Row] = []
    if _have("pytest"):
        rows.append(_leg("pytest", [sys.executable, "-m", "pytest", "-q"]))
    else:
        rows.append((WARN, "pytest", "not installed — `pip install pytest`"))
    if _have("build"):
        rows.append(_leg("python -m build", [sys.executable, "-m", "build"]))
    else:
        rows.append((WARN, "python -m build", "not installed — `pip install build`"))
    artifacts = sorted((ROOT / "dist").glob(f"pyvolca-{v}*"))
    if not _have("twine"):
        rows.append((WARN, "twine check", "not installed — `pip install twine`"))
    elif not artifacts:
        rows.append((WARN, "twine check", "no built artifacts for this version in dist/"))
    else:
        rows.append(_leg("twine check", [sys.executable, "-m", "twine", "check", *map(str, artifacts)]))
    return rows


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Pre-release gate for pyvolca.")
    parser.add_argument(
        "--no-tests", action="store_true", help="skip the slow pytest + build + twine leg"
    )
    ns = parser.parse_args(argv)

    v = _project_version()
    if not v:
        print("FAIL  could not read version from pyproject.toml")
        return 1

    print(f"pyvolca release precheck — version {v}\n")
    rows: list[Row] = [
        check_tag_absent(v),
        check_newer_than_pypi(v),
        check_changelog(v),
        check_clean_tree(),
        check_engine_released(),
        *check_tests_and_build(ns.no_tests, v),
    ]

    width = max(len(name) for _, name, _ in rows)
    for status, name, detail in rows:
        print(f"  {status}  {name.ljust(width)}  {detail}")

    if any(s == FAIL for s, _, _ in rows):
        print("\nNOT READY — fix the FAILs above before tagging.")
        return 1
    print(f"\nREADY. To publish pyvolca {v}:")
    print(f"    git tag pyvolca-v{v}")
    print(f"    git push origin pyvolca-v{v}")
    print("(the tag triggers pyvolca-release.yml → PyPI)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
