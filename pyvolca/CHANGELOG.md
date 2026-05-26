# Changelog

All notable changes to **pyvolca** are documented here. Versions follow
[semver](https://semver.org/); breaking changes bump the minor while we're
in 0.x.

The next-release section is **drafted by [git-cliff](https://git-cliff.org/)**
from commits that touched `pyvolca/` since the last `pyvolca-v*` tag, then
hand-edited for narrative and breaking-change callouts (`cliff.toml` only
groups commits by Conventional Commits prefix — it doesn't know which renames
are user-visible breaks). Workflow:

```bash
cd pyvolca
git cliff --unreleased                        # preview against current HEAD
git cliff --unreleased --tag pyvolca-v0.X.Y   # render as a released section
```

Then paste the rendered block at the top of this file and tighten wording.

## [0.4.0] - 2026-05-26

Engine wire format changed in three independent PRs since 0.3.1 — this
release realigns the client. **Any pyvolca ≤ 0.3.1 will raise `KeyError`
on most `get_activity` calls against a current engine.**

### Breaking changes

- **`TechnosphereExchange.is_input` / `is_reference` are now derived
  properties**, computed from the new `role: TechRole` field
  (`"ReferenceProduct" | "Coproduct" | "ReferenceInput" | "Input"`).
  Constructors take `role=` instead. (#73, #76)
- **`BiosphereExchange.is_input` is now a derived property**, computed
  from the new `direction: BioDirection` field
  (`"Resource" | "Emission"`). Constructors take `direction=` instead.
  The `flow_category` field is gone — biosphere flows now carry a
  structured `compartment: Compartment | None` (medium + optional
  subcompartment). (#73, #76)
- **`TechnosphereExchange.flow_category` removed.** Product taxonomy
  lives on the producing activity's classifications, not on the exchange.
  (#73)
- **`ExchangeDetail.flow` is now a tagged sum**:
  `{"kind": "technosphere" | "biosphere" | "waste", "flow": <flow>}`.
  Hand-rolled callers that walked `ed["flow"]` flat must unwrap one level.
  (#76, #83)
- **`list_methods` returns `id` instead of `methodId`** on each method
  dict — the rename mirrors the engine's stripped JSON convention. (#73)
- **Download / install root moved** from `user_cache_dir("pyvolca")` to
  `user_data_dir("volca", appauthor=False)`, shared with the shell and
  PowerShell installers. Helpers `cached_binary` / `cached_data_dir` are
  renamed `installed_binary` / `installed_data_dir`. `$VOLCA_HOME`
  overrides the auto-detected root. (#21)

### Added

- **`WasteExchange`** — third top-level `Exchange` variant matching the
  engine's `WasteFlow`. Activities containing waste exchanges (typical in
  EcoSpold2, ILCD, and SimaPro `Final waste flows`) now parse without a
  `ValueError`. Carries `flow_name`, `amount`, `unit`, `is_input`,
  `target_activity`, `target_location`, `target_process_id`, `comment`,
  plus `is_waste = True` for duck-typing dispatch. Exported from
  `volca`. (#83)
- **`Compartment`** dataclass (frozen, hashable) and **`TechRole`** /
  `BioDirection` literal aliases exported from `volca`, so callers can
  match on roles / directions without redefining the type. (#73)
- **`installed_binary()` semver scan** — falls back to scanning
  `$VOLCA_HOME` for the highest `X.Y.Z` dir containing the binary when
  `latest.json` is absent. Lets `Server()` pick up engines installed via
  `install.sh` / `install.ps1`, which don't write a manifest. (#21)

### Changed

- Internal: `parse_exchange_detail` validates that the flow envelope's
  `kind` matches the inner `Exchange` tag and raises if they disagree,
  instead of silently dropping the discriminator. (#76)
- README rewritten to lead with the "hosted server vs local engine"
  choice and explain where artefacts are installed. (#21)

## [0.3.1] - 2026-04-29

### Added

- `download()` helper fetches the matching VoLCA engine binary + data
  bundle from GitHub Releases, verifies SHA-256, and extracts under the
  per-user cache. Idempotent + concurrency-safe (one fcntl/msvcrt lock
  per cache root). `Server()` picks up the cached binary automatically.
  (#7)

## [0.3.0] - 2026-04-24

### Added

- PyPI packaging metadata (`pyproject.toml`, classifiers, LICENSE,
  trusted publishing). (#3, #4)
- Dedicated CI workflow for the pyvolca package.

## [0.2.0] - earlier

### Added

- `VoLCAError` with clear diagnostics for non-JSON responses.
- Typed dataclasses for the API surface (`Activity`, `ActivityDetail`,
  `SupplyChain`, `LCIAResult`, `LCIABatchResult`, …).
- `compare_activities` helper.
- Agribalyse decompose helpers.
- `Server` child-process wrapper.
