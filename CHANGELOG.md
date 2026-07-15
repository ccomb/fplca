# Changelog

## [Unreleased]

### Fixed
- The `name=` filter on the supply-chain and consumers endpoints (REST and
  MCP) now filters. A name matching nothing previously disabled the filter
  and returned every entry — with a matching `filteredActivities` count — so
  a caller could not tell "no match" from "no filter". It now returns an
  empty result.
- A scoring integrity error (a regionalized score whose tables are internally
  inconsistent — mismatched lengths, absent weights) now fails the request
  with a 500 instead of silently scoring the category 0. A consumer could not
  tell that 0 from a real score. Coverage gaps are unaffected: an unmapped
  flow still contributes nothing and is reported as before. In sensitivity
  responses the error lands on the affected perturbation entry, which already
  carries per-entry errors.

### Added
- Delete-activities accepts an `ids` list to delete exactly the named
  processes, on the API (`"ids": [...]`) and the CLI (repeatable `--id`).
  Previously the only selection mode was a filter, so deleting a known list
  of processes required a deliberately unsatisfiable filter plus the `extra`
  list. `ids` cannot be combined with filter fields — an ambiguous request
  is refused, not guessed at.
- `volca server` starts without `--config`: it runs on the built-in defaults
  with no databases, ready to receive uploads or API-driven loads. Launchers
  no longer have to write an empty TOML file just to satisfy the flag. An
  explicit `--config` path that does not exist still fails loudly.
- The `aggregate` primitive gains a `consumption` scope that answers "how much
  of X is consumed across the whole upstream chain" — total electricity or
  heat feeding a product, grass eaten by cattle. Each row is one scaled
  consumer→supplier link, so filtering by the consuming activity
  (`filter_consumer`, `filter_consumer_not`) avoids the double counting that
  summing cumulative production would give (for example counting the same
  electricity once per voltage level). Grouping by `consumer_name` shows who
  consumes what. Available on the REST endpoint, the MCP tool, and pyvolca.

## [0.9.2] - 2026-07-14

### Added
- `server --port 0 --desktop` now asks the OS to bind an available loopback
  port and prints the actual `VOLCA_PORT=N` only after the listening socket is
  reserved. Launchers can therefore avoid reserve-then-release port races.
  With port 0 the server is only reachable from the local machine.

## [0.9.1] - 2026-07-11

### Added
- A characterization method declared in the TOML configuration can carry
  `[[methods.patches]]` blocks: declarative adjustments that rescale or
  replace the matched characterization factors every time the collection
  loads. A patch selects factors by impact category, flow name or prefix,
  CAS number, or subcompartment, and is re-applied to the freshly parsed
  source file on each load — so reloading never compounds it. A patch that
  matches no factor is reported at load time instead of being silently
  ignored.
- Bulk impact scoring — the `POST /db/{db}/impacts/{collection}` endpoint and
  the `score_activities` MCP tool — can now exclude long-term emissions, as
  scoring a single activity already could.

### Changed
- The wire-format revision advertised on `/api/v1/version` is now `2`, because
  the export download changed shape (below). pyvolca ≥ 0.7.2 requires an engine
  ≥ v0.9.1 and refuses older ones with a clear message; an older pyvolca gets a
  warning telling it to upgrade.
- Database exports download as raw bytes instead of base64-encoded JSON,
  matching how uploads already work — a third less data on the wire and far
  less memory on both ends for big files. Any export approximation warnings
  now travel in the `X-Volca-Export-Warnings` response header.

### Fixed
- Emissions to immediate groundwater are characterized again in ecotoxicity and
  human toxicity, inheriting the method's unspecified-water factor exactly like
  releases to a river or a lake. They previously scored zero — on a witness
  concrete process a single iron-ion flow was worth about 10% of the freshwater
  ecotoxicity score. Long-term groundwater keeps its explicit zero.
- Two flow-name synonym bridges (`Flupyrsulfuron-methyl sodium ↔
  Flupyrsulfuron-methyl`, `Pyrethrins ↔ Pyrethrum`) so EF 3.1 (adapted 1.03)
  characterizes flows it only lists under a sibling name, instead of silently
  scoring them zero in freshwater ecotoxicity.
- A database holding activities with several products can now be exported to
  ILCD. Each product becomes its own ILCD process, instead of the whole export
  being refused. This unblocks exporting databases read from SimaPro CSV, where
  two unrelated processes can share a name and so look like one multi-product
  activity.
- Exporting a large database to a zipped format (ILCD, EcoSpold 2) no longer
  stalls. The time spent packing the archive grew with the square of the number
  of files, so a full Agribalyse ILCD export — some fifty thousand files —
  exhausted memory and never returned.

## [0.9.0] - 2026-07-06

A characterization-accuracy release. EF 3.1 scores on Agribalyse and ecoinvent
now track the published references far more closely, the plugin framework that
carried no external users is gone, and a database can be loaded or unloaded from
every surface rather than only at start-up.

### Added
- A database can be loaded and unloaded from every surface — the REST API, the
  MCP server, the CLI, and the web UI — not only at server start-up.
- Scoring-set breakdowns can show a human-readable name for computed indicators
  (for example "Ecotoxicity, freshwater" instead of the raw key `etf`), via an
  optional `[methods.scoring.labels]` table in the scoring configuration. A
  label naming an unknown scoring variable is rejected when the configuration
  loads instead of being silently ignored.
- Scoring can optionally exclude long-term emissions — those a method releases
  beyond its time horizon — so results line up with inventories that account for
  them separately.
- A characterization method can be loaded from a bare `.csv` file, not only a
  zip archive; an unsupported file type now fails with a clear message instead
  of being silently misread.

### Changed
- Much closer EF 3.1 agreement with the published Agribalyse 3.2 and ecoinvent
  references, the sum of many corrections to how inventory flows are matched to
  characterization factors: CAS-guided matching with ambiguous bridges dropped,
  a curated and linted registry of name bridges for refrigerants and pesticides,
  a region-fallback chain for water flows, sub-compartment gating, a generalized
  density bridge, an ore-grade resource fallback, and a preference for the
  verbatim flow name when unit-suffixed homonyms collide. Many products that
  previously scored short — pesticide-heavy processes especially — are now
  characterized.
- Auto-extracted flow synonyms are an opt-in candidate set rather than always
  applied, so the shipped mapping rests on the curated bridges.

### Fixed
- Long-term emissions are characterized with the method's long-term factor
  rather than its default, and ionising radiation against its kBq reference unit.
- Water no longer collapses across regions: SimaPro factors are treated as
  name-regionalized rather than keyed on the consumer location.
- An unspecified chromium emission is treated as trivalent, and elemental metal
  emissions bridge to their ionic toxicity factor — correcting large human
  toxicity over-counts.
- EcoSpold 1 flow identity now includes the sub-category, and a SimaPro
  name-less multi-product block stays a single activity instead of splitting.

### Removed
- The plugin framework — its eight-handle registry, the `/analyze` REST
  endpoint, and the `plugin list` CLI command — is gone. It carried a single
  built-in implementation and no external users; flow-to-factor mapping is now a
  plain internal cascade.

### Performance
- Method tables are built once, off the request path, with a parallelized
  cascade and synonym-group memoization — a large speedup on the first scoring
  after a database loads.

## [0.8.1] - 2026-06-24

### Fixed
- Aggregate single scores (e.g. a PEF score) now compute correctly on JRC ILCD
  method collections such as EF 3.1. They previously failed with `Unknown
  variable` whenever a collection held several methods sharing one coarse
  damage category — for EF 3.1 the four climate-change methods, the freshwater
  ecotoxicity methods, and the resource-depletion methods each collapsed
  together, so their per-method scoring variables could not resolve.
  SimaPro-adapted methods were never affected.

## [0.8.0] - 2026-06-24

### Added
- A loaded database can be exported to any of the five supported formats —
  SimaPro CSV, EcoSpold 1, EcoSpold 2, ILCD, and Brightway Excel — from both the
  API and the CLI, so a database can be moved between tools or re-saved after edits.
- A loaded database can be edited in place: copy it under a new name, delete a
  filtered selection of activities, or relink it against a dependency through a
  name-to-name alias map.
- Activity records now carry separate `activity_name` and `product_name` fields,
  instead of the old `name`/`product` that blurred an activity and its reference
  product.
- A partial EcoSpold2 import (a handful of `.spold` files cut from a full
  database) now becomes analyzable by loading its matching ecoinvent background
  as a dependency: each input is linked to the exact background activity it
  names, by `activityLinkId` identity. Previously only nil-link inputs were
  resolved, so partial imports stayed unresolved however the background was
  loaded.
- When the loaded background is a *different* release than the import was cut
  from, the exact identity won't be present, so linking falls back to the usual
  attribute matching (name, location, unit). Those approximate links are flagged
  on the database setup view (and in the load log) so you can verify the
  dependency is the release you intended rather than trust a cross-version match
  as exact.
- `/api/v1/version` now reports a `wireVersion` integer. Clients read it at
  connect time to confirm they speak this engine's JSON format, so a version
  mismatch fails with a clear message instead of a confusing decode error.

### Changed
- Far broader EF 3.1 impact coverage: flows are matched by CAS number across
  naming schemes, a substance registry bridges nomenclatures, sub-compartments
  fall back sensibly, energy-carrier flows are characterized from their energy
  content, land use is regionalized with per-country factors, and a large synonym
  set links ecoinvent and Agribalyse flows to the method. Many products that
  previously scored short are now characterized.
- Large database uploads stream as raw bytes instead of base64-encoded JSON,
  cutting memory use and time on big files.
- A supplier substitution applies across every consumer of the substituted
  product, not only the process you queried.

### Fixed
- ecoinvent waste-treatment activities import and score correctly: the reference
  flow stays in the technosphere (these activities are no longer dropped) and
  cross-database waste keeps the correct sign.
- EcoSpold packages whose datasets live in a sub-directory now load.
- Requesting a well-formed but non-existent process returns a clear "activity not
  found" instead of a confusing error.
- A missing reference CSV is reported as an error instead of crashing the load.

## [0.7.0] - 2026-05-29

A month of engine work: a third flow kind for waste, regionalized impact
assessment, more input formats, and a hardening pass that turns silent
miscounts into explicit errors.

### Added
- Brightway Excel (`.xlsx`) inventories can now be loaded directly.
- Regionalized LCIA scoring via openLCA JSON-LD `ImpactCategory`, including
  uploading openLCA JSON-LD methods through the method pipeline.
- `WasteFlow` / `WasteExchange` as a third top-level flow kind, with an
  exact-match cross-database waste linker and explicit reporting of orphan
  (unlinked) waste.
- Per-database `geography_policy` controlling how activities are matched across
  databases during cross-DB linking.
- Sensitivity analysis: a rank-1 perturbation primitive and a sweep endpoint.
- SimaPro pedigree (uncertainty) matrix parsed and exposed through the API.
- Configurable per-instance upload size limit (hosting policy), enforced both in
  the upload handler and at the HTTP layer.
- macOS Intel (x86_64) engine build target and published release assets.
- One-liner installers for Linux, macOS, and Windows.
- MCP: batched LCIA and scoring sets, columnar `score_activities`, and
  source-native `activity_type` surfaced through search/score/get_activity.
- `/api/v1/licenses` endpoint plus `NOTICE` / `THIRD_PARTY_LICENSES`.

### Changed
- The cross-database dependency pin is now authoritative and persisted to cache,
  and databases auto-relink on every load.
- `Flow` split into `TechnosphereFlow` and `BiosphereFlow` (with `WasteFlow` as
  the third kind) so flow handling is total over the type system.
- Service errors are reported as 4xx, not 5xx: `InvalidUUID` → 400,
  `FlowNotFound` → 404, and cross-DB invariant breakages surface as client
  errors instead of 500s.
- Large LCIA speedups: batched multi-method scoring (~22× on PEF), precomputed
  per-activity weights for regionalized methods, and coalesced matrix solves.
- Docker images standardized on musl with a fully-static build and ARM64
  support.
- pyvolca: typed returns, string enums, and lazily paginated search/consumer
  results.

### Fixed
- Characterization no longer silently returns zero on a compartment,
  subcompartment, or unit mismatch — the gap is surfaced instead of undercounted.
- Regionalized LCIA returns a partial score on tainted columns rather than
  failing the whole computation.
- SimaPro: sign preserved on substitution (Materials/fuels) exchanges, reference
  amounts normalized to the canonical base unit, and split-location products
  exposed to the cross-DB linker.

## [0.6.0] - 2026-05-01

Packaging and distribution milestone (not previously recorded here).

### Added
- GitHub Actions build matrix producing release assets for Linux, macOS, and
  Windows, driven by a tag-based release pipeline with a relocatable data bundle.
- `pyvolca` published to PyPI, with per-exchange comments surfaced through the
  API and Python bindings.

### Changed
- SimaPro location extraction and reference-amount normalization improvements.

## [0.5.0] - 2026-02-02

### Added
- Desktop application (Tauri) for Windows and Linux with branded installer
- Console output panel with live log streaming in the web UI
- Loading screen shown while the backend starts in the desktop app
- MUMPS direct solver support on all platforms for faster matrix solving

### Changed
- Database upload now uses pure Haskell zip-archive (no external tools needed)
- Unified cross-platform build system (single bash script for Linux, macOS, Windows)
- Build dependency versions centralized in versions.env

### Fixed
- Console output panel showing empty in desktop app (optimized binary mismatch)
- Upload cancel now navigates back to databases list
- CSS and fonts bundled locally for offline use in desktop app

## [0.4.0] - 2026-01-18

### Added
- Database upload: load and unload your own EcoSpold databases (BYOL)
- Location aliases in configuration for targeted location overrides
- Production amount displayed in search results and activity header
- Product column in activity search results
- Database format column on databases page

### Changed
- Inventory page split into separate Resources and Emissions tables
- Redesigned left menu with white Explore/Lab sections
- Unified column order and shared ActivityRow component across activity tables

### Fixed
- EcoSpold1 exchanges without location now resolved via name lookup
- Zero-amount missing supplier exchange warnings suppressed
- Dynamic CPU detection for parallel loading (no more hardcoded worker count)
- Frontend minified with SWC for smaller bundles

## [0.3] - 2025-12-24

### Added
- Multi-database support with `--config volca.toml` configuration file
- EcoSpold1 parser for older LCA databases (Ecoinvent 2.x, BAFU)
- SimaPro CSV parser for Agribalyse
- LCIA impact assessment with method loading, flow mapping, and score computation
- Activity aliases configuration for resolving EcoSpold1 supplier links
- HTTP Basic Auth for API and web interface (`--password` or `VOLCA_PASSWORD`)
- Database management API endpoints (`/databases`, `/databases/{name}/activate`)
- LCIA methods API endpoint (`/methods`)
- Databases page in web UI with table layout
- LCIA tab in activity details view

### Changed
- Cache system now uses automatic schema-based invalidation (no manual version bumping)
- Cache filename simplified to `volca.cache.{dbName}.bin.zst`
- Per-database PETSc solver cache for instant database switching
- Web UI redesign: split details tabs into individual pages, sticky headers, improved left menu
- Database name included in URLs for bookmarkable multi-database views

### Fixed
- Double-click navigation and search focus issues
- Navigation history properly returns to search results
- Search removed 2-character minimum requirement

## [0.2] - 2025-12-04

### Added
- Details view with tabs for upstream activities, emissions, natural resources, and products
- Graph view with force-directed layout
- Activity search with multi-word filtering and pagination
- Products tab showing all outputs from multi-product activities
- URL routing for bookmarkable views

### Changed
- Renamed project from acv-engine to volca

## [0.1] - 2025-11-09

### Added
- Core LCA engine with EcoSpold2 XML parsing
- Matrix computation with PETSc/SLEPc
- REST API, CLI, and web interface with Tree and Inventory views
- Database caching for fast startup
