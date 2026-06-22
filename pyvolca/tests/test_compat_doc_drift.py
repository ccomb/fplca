"""The README compatibility block must stay in sync with volca._compat.

``scripts/gen_api_md.py`` derives the block from the wire-compatibility policy;
this test asserts the committed README already reflects it, so a change to
``REQUIRED_WIRE`` / ``MIN_ENGINE_HINT`` (or the pyvolca version) that wasn't
regenerated fails CI instead of silently shipping a stale claim. Pure Python:
no engine binary required.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path

# The generator lives in scripts/, outside the importable package, so load it
# by path rather than via a regular import.
_SCRIPT = Path(__file__).resolve().parent.parent / "scripts" / "gen_api_md.py"
_spec = importlib.util.spec_from_file_location("gen_api_md", _SCRIPT)
assert _spec is not None and _spec.loader is not None
gen_api_md = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(gen_api_md)


def test_readme_compatibility_in_sync() -> None:
    readme = gen_api_md.README.read_text(encoding="utf-8")
    expected = gen_api_md.splice(
        readme,
        gen_api_md.COMPAT_BEGIN_MARKER,
        gen_api_md.COMPAT_END_MARKER,
        gen_api_md.render_compatibility(),
    )
    assert expected == readme, (
        "pyvolca/README.md compatibility block is stale. Run "
        "`python scripts/gen_api_md.py --write` and commit the result."
    )
