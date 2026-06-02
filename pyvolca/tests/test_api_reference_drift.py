"""The README api-reference block must stay in sync with the generator.

``scripts/gen_api_md.py`` produces the block; this test asserts the committed
README already reflects that output, so a stale block — or a reintroduced
version-dependent signature — fails CI instead of silently shipping. Pure
Python introspection: no engine binary required.
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


def test_readme_api_reference_in_sync() -> None:
    readme = gen_api_md.README.read_text(encoding="utf-8")
    expected = gen_api_md.splice_readme(readme, gen_api_md.render_reference())
    assert expected == readme, (
        "pyvolca/README.md api-reference block is stale. Run "
        "`python scripts/gen_api_md.py --write` and commit the result."
    )
