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

## [0.7.0] - 2026-06-24

Co-product allocation shares are now visible from the typed client.

### Added

- `Activity.allocation_percent` / `Activity.allocation_formula` — a co-product's
  share of its parent activity's burden. They populate every
  `ActivityDetail.all_products` entry, so a multi-output process (e.g. a cheese
  that also yields whey, cream and permeate) shows how its impact is split across
  products. `None` for single-output processes.
- `ActivityDetail.allocation_percent` — the share of the process you fetched.

### Changed

- `ActivityDetail.is_allocated` (and `agribalyse.is_allocated`) now read the
  structured shares on `all_products` instead of scraping the description text:
  more reliable, and it matches the factor the engine actually applied (the
  description's allocation comment is rounded and can drift — e.g. 52.62% in text
  vs 51.4% applied). `agribalyse.decompose` enumerates co-products the same way,
  keeping the text parse as a fallback for older databases.

## [0.6.0] - 2026-06-21

Breaking: an activity's name fields are renamed for clarity. Needs the companion engine release (the wire keys changed too).

### Breaking changes

- `Activity.name` → `activity_name` and `Activity.product` → `product_name` — and the same on `ConsumerResult`, `SupplyChainEntry`, `PathStep`, and `ActivityDetail` (whose `reference_product` / `reference_product_amount` / `reference_product_unit` become `product_name` / `product_amount` / `product_unit`). A technosphere exchange's `target_activity` is now `target_activity_name`.
- Why: an activity can yield several products, so the *name* belongs to the activity (shared across its products) while the *product* is what tells them apart — `name` / `product` invited mixing the two. A "process" is an `(activity, product)` pair, addressed by `process_id`; it has no name of its own (compose a label from `activity_name` + `product_name`).
- Migration: `.name` → `.activity_name`, `.product` → `.product_name`, `.reference_product` → `.product_name`, `.target_activity` → `.target_activity_name`.

## [0.5.1] - 2026-05-31

Two bug fixes for engine 0.7.0. No API change — just upgrade.

### Fixes

- Substitutions now reach the engine. The client was sending field names
  0.7.0 no longer recognised, so substitutions were silently ignored; it
  now sends the names the engine expects. (#108)
- Cross-database supply-chain edges now resolve. Each edge keeps its source
  and target database, so a process id that exists in more than one loaded
  database is routed to the right one. (#108)

## [0.5.0] - 2026-05-27

Three convergent themes: surface pagination truthfully (no silent
truncation), wire enums as enums, and type the high-traffic dicts. Every
public method now returns a typed dataclass, every paginated endpoint
exposes the envelope, and every enum-shaped string is an enum.

If you need a brand-new engine field that pyvolca doesn't model yet, use
the escape hatch ``c.call("operation_id", ...)`` — returns the raw JSON
dict.

### Breaking changes — pagination surfacing

- **`SupplyChain.has_more`** is a new derived property. When
  ``len(entries) < filtered_activities`` the server truncated the
  result; downstream LCA work on a truncated chain would be silently
  wrong. Callers should check this flag.
- **`get_characterization`** now returns a typed
  ``CharacterizationResult`` with ``matches`` / ``shown`` / ``has_more``
  derived. Previously returned a bare dict.
- **`SearchResults.from_raw`** is strict when a fetch callback is
  wired: missing wire keys (``total``, ``offset``, ``limit``,
  ``hasMore``) raise instead of silently defaulting to a truncated
  total. Test fixtures (``fetch=None``) keep permissive defaults.
- **`SearchResults.__iter__`** now raises ``RuntimeError`` when the
  server returns ``hasMore=True`` with no items — previously it
  silently stopped, which let callers consume an incomplete result set
  without learning the engine's pagination contract was broken.

### Breaking changes — StrEnums (renamed string fields)

All stringly-typed enum-shaped fields are now ``str`` subclasses so
equality with the raw wire string still works, but typos fail at
construction.

- **`DatabaseStatus`**: ``DatabaseInfo.status`` is now
  ``DatabaseStatus.LOADED`` / ``UNLOADED`` / ``PARTIALLY_LINKED``.
- **`TechRole`** (was ``Literal``): ``TechnosphereExchange.role`` is
  now ``TechRole.INPUT`` / ``REFERENCE_PRODUCT`` / etc.
- **`BioDirection`** (was ``Literal``): ``BiosphereExchange.direction``
  is now ``BioDirection.RESOURCE`` / ``EMISSION``.
- **`AggregateScope`**: ``AggregateResult.scope`` and the
  ``aggregate(scope=)`` argument are now ``AggregateScope.DIRECT`` /
  ``SUPPLY_CHAIN`` / ``BIOSPHERE``. Raw strings still accepted on the
  argument for one-liners, but the return value is always the enum.
- **`AggregateOp`**: ``aggregate(aggregate=)`` is now
  ``AggregateOp.SUM_QUANTITY`` / ``COUNT`` / ``SHARE``.

### Breaking changes — typed list returns

Previously bare ``dict`` / ``list[dict]`` returns, now typed dataclasses.

- **`Client.list_methods`** → ``list[Method]`` (carries ``id``,
  ``name``, ``category``, ``unit``, ``factor_count``, ``collection``).
- **`Client.list_classifications`** → ``list[ClassificationSystem]``.
- **`Client.list_presets`** → ``list[Preset]`` with structured
  ``filters: list[PresetFilter]``.
- **`Client.get_version`** → ``ServerVersion``.
- **`Client.get_inventory`** → ``InventoryResult`` with ``root``,
  ``flows: list[InventoryFlow]``, ``statistics: InventoryStatistics``.
- **`Client.get_flow_mapping`** → ``FlowMapping`` with derived
  ``coverage_pct``.
- **`Client.get_contributing_flows`** → ``ContributingFlows``.
- **`Client.get_contributing_activities`** → ``ContributingActivities``.

Caveat: the engine does not report a total count for the contributing
endpoints, so pyvolca cannot derive ``has_more`` for them. Pass a
generous ``limit`` if exhaustive coverage matters.

### Breaking changes — typed substitutions

- **`Substitution`** is a new frozen dataclass replacing the
  ``{"from", "to", "consumer"}`` dict form. ``get_supply_chain``,
  ``get_inventory``, ``get_impacts``, and ``get_impacts_batch`` now
  accept ``list[Substitution]`` (preferred) or the legacy dict (for
  back-compat one-liners). The dict form validates the three required
  keys locally — typos like ``"comsumer"`` raise before hitting the
  engine.

### Other notes

- **`Client.use(db_name)`** is now implemented via ``__dict__.copy()``
  so attributes added to ``__init__`` propagate automatically (no
  manual mirror to keep in sync).

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
