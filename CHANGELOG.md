# Changelog

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
- Multi-database support with `--config fplca.toml` configuration file
- EcoSpold1 parser for older LCA databases (Ecoinvent 2.x, BAFU)
- SimaPro CSV parser for Agribalyse
- LCIA impact assessment with method loading, flow mapping, and score computation
- Activity aliases configuration for resolving EcoSpold1 supplier links
- HTTP Basic Auth for API and web interface (`--password` or `FPLCA_PASSWORD`)
- Database management API endpoints (`/databases`, `/databases/{name}/activate`)
- LCIA methods API endpoint (`/methods`)
- Databases page in web UI with table layout
- LCIA tab in activity details view

### Changed
- Cache system now uses automatic schema-based invalidation (no manual version bumping)
- Cache filename simplified to `fplca.cache.{dbName}.bin.zst`
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
- Renamed project from acv-engine to fplca

## [0.1] - 2025-11-09

### Added
- Core LCA engine with EcoSpold2 XML parsing
- Matrix computation with PETSc/SLEPc
- REST API, CLI, and web interface with Tree and Inventory views
- Database caching for fast startup
