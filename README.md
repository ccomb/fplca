# fpLCA

**fpLCA** is a Life Cycle Assessment engine that turns LCA databases into inspectable, queryable answers — fast.

It loads EcoSpold2, EcoSpold1, and SimaPro CSV databases, builds supply chain dependency trees, computes life cycle inventories using sparse matrix algebra, and applies characterization methods for impact assessment. Everything runs in-memory against your own data.

## What It Does

- **Browse** activities and flows across multiple databases
- **Explore** supply chain trees and force-directed dependency graphs
- **Compute** life cycle inventories (LCI) and impact scores (LCIA)
- **Link** databases across nomenclatures (e.g., Agribalyse referencing Ecoinvent)
- **Upload** your own databases via the web UI, without touching config files

---

## Key Features

- **Multiple database formats**: EcoSpold2 (.spold), EcoSpold1 (.xml), SimaPro CSV
- **Archive support**: Load databases directly from .zip, .7z, .gz, or .xz archives — no manual extraction needed
- **Cross-database linking**: Resolve supplier references across databases, with configurable dependencies and topological load ordering
- **Web interface**: Multi-page Elm app with search, tree view, graph view, inventory, LCIA, and database management
- **Desktop application**: Native Windows/Linux app — no installation or configuration needed
- **REST API and CLI**: Scriptable interface for automation and integration
- **Fast cache**: Per-database cache with automatic schema-based invalidation; loading Ecoinvent goes from ~45s cold to ~2-3s cached
- **Optional access control**: Single-code login with cookie-based session

---

## Getting Started

### Desktop Application (Recommended)

Download and run the installer for Windows or Linux from the releases page. The desktop app bundles the complete engine and opens a browser-based UI automatically.

### Web Server

```bash
# Build (requires PETSc/SLEPc — see Building section)
./build.sh

# Run with a config file
cabal run fplca -- --config fplca.toml server --port 8081
# Open http://localhost:8081
```

### Command Line

```bash
# Search activities
fplca --data ./ECOINVENT3.12 activities --name "electricity" --geo "DE"

# Supply chain tree
fplca --data ./ECOINVENT3.12 tree "12345678-..." --tree-depth 3

# Life cycle inventory
fplca --data ./ECOINVENT3.12 inventory "12345678-..."

# Impact assessment
fplca --data ./ECOINVENT3.12 lcia "12345678-..." --method ./EF-3.1.xml
```

---

## Configuration

A TOML config file enables multi-database setups with explicit dependency ordering:

```toml
[server]
port = 8081
host = "127.0.0.1"
password = "mysecret"          # optional — omit to disable auth

[[databases]]
name = "ecoinvent-3.12"
displayName = "Ecoinvent 3.12"
path = "DBs/ecoinvent3.12.7z"  # archives supported natively
description = "Cutoff System Model, EcoSpold2"
load = true

[[databases]]
name = "agribalyse-3.2"
displayName = "Agribalyse 3.2"
path = "DBs/AGB32_final.CSV"
description = "SimaPro CSV"
load = false

[[databases]]
name = "my-sector-db"
displayName = "Sector DB"
path = "DBs/sector.CSV"
depends = ["agribalyse-3.2"]   # loads agribalyse first, then links
load = true

[[methods]]
name = "EF-3.1"
path = "../EF-v3.1/ILCD/lciamethods"
```

The `depends` field ensures dependency databases load first and their flows are available for cross-database linking. Setting `load = true` on a database transitively loads all its dependencies.

---

## Web Interface

| Page | What it shows |
|------|--------------|
| Activities | Searchable list with name, geography, and product filters |
| Tree | Hierarchical upstream dependency view |
| Graph | Force-directed network with configurable cutoff |
| Inventory | Environmental flows split into Emissions and Resources |
| LCIA | Impact scores per category with flow mapping statistics |
| Databases | Load, unload, upload, and configure cross-database links |
| Database Setup | Data path picker, dependency editor, linking diagnostics |

---

## REST API

```
GET  /api/v1/databases                              List databases and status
POST /api/v1/databases/upload                       Upload a database archive
POST /api/v1/databases/{name}/load                  Load a configured database
POST /api/v1/databases/{name}/finalize              Finalize cross-DB linking
GET  /api/v1/db/{name}/activity/{id}                Activity details
GET  /api/v1/db/{name}/activity/{id}/tree           Supply chain tree
GET  /api/v1/db/{name}/activity/{id}/inventory      Life cycle inventory
GET  /api/v1/db/{name}/activity/{id}/lcia/{method}  LCIA score
GET  /api/v1/search/activities?db=&name=&geo=       Search activities
GET  /api/v1/search/flows?db=&q=                    Search flows
GET  /api/v1/methods                                List LCIA methods
POST /api/v1/auth                                   Login (returns session cookie)
```

---

## CLI Commands

### Global Options

| Option | Description |
|--------|-------------|
| `--config FILE` | TOML config file for multi-database setup |
| `--data PATH` | Single database path (alternative to `--config`) |
| `--format FORMAT` | Output format: `json` (default), `csv`, `table`, `pretty` |
| `--jsonpath PATH` | Field to extract for CSV output (e.g., `srResults`) |
| `--tree-depth N` | Maximum tree depth (default: 2) |
| `--no-cache` | Disable caching (for development) |

### Search

```bash
# Find activities by name, geography, product
fplca activities --name "electricity" --geo "DE" --limit 10
fplca activities --product "steel" --limit 10 --offset 20

# Find flows by keyword
fplca flows --query "carbon dioxide" --limit 5
```

### Analysis

```bash
# Activity details
fplca activity "12345678-..."

# Supply chain tree
fplca tree "12345678-..." --tree-depth 3

# Life cycle inventory
fplca inventory "12345678-..."

# Impact assessment
fplca lcia "12345678-..." --method ./EF-3.1.xml

# Matrix export (Ecoinvent universal format)
fplca export-matrices ./output_dir
```

---

## Building

### Linux / macOS

```bash
./build.sh              # Download PETSc/SLEPc and build everything
./build.sh --test       # Build and run tests
./build.sh --desktop    # Build desktop application
```

The build script downloads and compiles PETSc and SLEPc automatically if not already present.

### Windows (MSYS2)

1. Install [MSYS2](https://www.msys2.org/) and open the "MSYS2 UCRT64" terminal
2. Install dependencies:
   ```bash
   pacman -S make python git \
             mingw-w64-ucrt-x86_64-gcc \
             mingw-w64-ucrt-x86_64-gcc-fortran \
             mingw-w64-ucrt-x86_64-cmake \
             mingw-w64-ucrt-x86_64-openblas \
             mingw-w64-ucrt-x86_64-msmpi \
             mingw-w64-ucrt-x86_64-zlib
   ```
3. Install [GHCup](https://www.haskell.org/ghcup/) for the compiler toolchain
4. Run:
   ```bash
   ./build.sh            # Same script as Linux/macOS
   ./build.sh --desktop  # Builds Windows installer (.exe)
   ```

### Docker

```bash
docker build -t fplca .
docker run -p 8081:8081 -v /path/to/data:/data fplca
```

---

## Performance

Databases are loaded entirely into memory. A schema-aware cache (`.bin.zst` per database, stored in `cache/`) makes subsequent startups fast:

| Database | Cold load | Cached load |
|----------|-----------|-------------|
| Ecoinvent 3.12 (25k activities) | ~45s | ~2-3s |
| Small sector database | ~2s | <0.5s |

Matrix solving uses PETSc sparse solvers. Inventory computation for a typical supply chain takes under 15 seconds on a large database.

---

## Testing

```bash
./build.sh --test

# Or manually
export LD_LIBRARY_PATH="petsc-3.24.2/arch-linux-c-opt/lib:slepc-3.24.1/arch-linux-c-opt/lib"
cabal test --test-show-details=streaming
```

Tests cover matrix construction (sign convention), inventory calculation (golden values), parsers, and matrix export format compliance.

---

## License

GNU Affero General Public License 3.0 or later — see LICENSE for details.
