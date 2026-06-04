# Changelog

## [Unreleased]

### Added
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
