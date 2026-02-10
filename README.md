# fplca

**fplca** is a high-performance Life Cycle Assessment (LCA) engine written in Haskell. It provides a command-line interface, REST API, web interface, and native desktop application for analyzing environmental impacts across product supply chains.

## What It Does

fplca computes environmental impacts by:
- Loading LCA databases (EcoSpold2, EcoSpold1, SimaPro CSV)
- Building supply chain dependency trees
- Computing life cycle inventories (LCI) using sparse matrix algebra
- Applying characterization methods for impact assessment (LCIA)

All computations run in-memory with PETSc-accelerated linear solvers for performance on large databases (25,000+ activities).

---

## Key Features

- **Multiple database formats**: EcoSpold2 (.spold), EcoSpold1 (.xml), SimaPro CSV
- **Database upload**: Bring your own databases via web UI (zip, 7z, tar archives)
- **Cross-database linking**: Reference processes across different databases (e.g., Agribalyse → Ecoinvent)
- **Web interface**: Single-page app with search, visualization, and analysis tools
- **Desktop application**: Native Windows/Linux app with bundled backend (no installation needed)
- **CLI and REST API**: Scriptable interface for automation and integration
- **Smart caching**: Automatic schema-based cache invalidation, sub-second startup
- **Multiple output formats**: JSON, CSV, pretty-printed tables

---

## Getting Started

### Desktop Application (Recommended)

Download and run the installer for Windows or Linux from the releases page. The desktop app bundles the complete backend and requires no additional setup.

### Web Server

```bash
# Build and run (requires PETSc/SLEPc, see Building section)
./build.sh
cabal run fplca -- --config fplca.toml server

# Open browser to http://localhost:8080
```

### Command-Line Interface

```bash
# Search for activities
fplca --data ./ECOINVENT3.12 activities --name "electricity" --geo "DE"

# Build supply chain tree
fplca tree "12345678-1234-1234-1234-123456789abc" --tree-depth 3

# Compute inventory
fplca inventory "12345678-1234-1234-1234-123456789abc"

# Impact assessment
fplca lcia "12345678-1234-1234-1234-123456789abc" --method ./methods/EF-3.1.xml
```

---

## Configuration

For multi-database setups, use a TOML configuration file:

```toml
# fplca.toml

[server]
port = 8080
host = "127.0.0.1"

# Ecoinvent 3.12 (EcoSpold2 format)
[[databases]]
name = "ecoinvent-3.12"
displayName = "Ecoinvent 3.12"
path = "../ecoinvent/ECOINVENT3.12/datasets"
description = "Ecoinvent 3.12 Cutoff System Model"
active = true
default = true

# Agribalyse (SimaPro CSV format)
[[databases]]
name = "agribalyse-3.2"
displayName = "Agribalyse 3.2"
path = "../agribalyse/AGB32_final.CSV"
active = true

# LCIA Methods
[[methods]]
name = "EF-3.1"
path = "../EF-v3.1/ILCD/lciamethods"
active = true
```

Then run:
```bash
fplca --config fplca.toml server
```

---

## Web Interface

The web UI provides:

- **Activity Search**: Find processes by name, product, or geography with pagination
- **Supply Chain Tree**: Hierarchical view of upstream dependencies
- **Graph View**: Force-directed network visualization
- **Inventory**: Environmental flows split into Emissions and Resources tables
- **LCIA**: Impact assessment scores across environmental categories
- **Database Management**: Load, unload, and configure databases with cross-database linking
- **Upload**: Import your own databases (zip/7z/tar archives with EcoSpold files)
- **Console**: Live log streaming from the backend

The UI is a single-page Elm application with URL routing for bookmarkable views.

---

## REST API

Start the server:
```bash
fplca --config fplca.toml server [--port 8080] [--password secret]
```

### Endpoints

**Databases**
- `GET /api/v1/databases` - List all configured databases with status
- `POST /api/v1/databases` - Upload database archive
- `DELETE /api/v1/databases/{name}` - Remove uploaded database

**Activities**
- `GET /api/v1/activities?db={name}&name={query}` - Search activities
- `GET /api/v1/activity/{uuid}` - Get activity details
- `GET /api/v1/activity/{uuid}/tree` - Supply chain tree
- `GET /api/v1/activity/{uuid}/inventory` - Life cycle inventory

**Flows**
- `GET /api/v1/flows?db={name}&query={term}` - Search flows

**Impact Assessment**
- `GET /api/v1/methods` - List available LCIA methods
- `POST /api/v1/lcia/{uuid}` - Compute LCIA with method

**Console**
- `GET /api/v1/console-stream` - Server-sent events for log streaming

### Authentication

Optional HTTP Basic Auth:
```bash
# Via command-line flag
fplca --config fplca.toml server --password mysecret

# Via environment variable
FPLCA_PASSWORD=mysecret fplca --config fplca.toml server
```

---

## CLI Commands

### Global Options

| Option | Description | Example |
|--------|-------------|---------|
| `--config FILE` | Configuration file with multiple databases | `--config fplca.toml` |
| `--data PATH` | Single database path (alternative to --config) | `--data ./ECOINVENT3.9.1` |
| `--format FORMAT` | Output format: `json\|csv\|table\|pretty` | `--format json` |
| `--jsonpath PATH` | JSONPath for CSV extraction (required with CSV) | `--jsonpath "srResults"` |
| `--tree-depth N` | Maximum tree depth for calculations | `--tree-depth 3` |
| `--no-cache` | Disable caching (for development) | `--no-cache` |

### Search Commands

```bash
# Find activities by name
fplca --data ./data activities --name "electricity"

# Geographic filtering
fplca activities --name "transport" --geo "DE" --limit 5

# Product-based search
fplca activities --product "steel" --limit 10 --offset 20

# Find flows by keyword
fplca flows --query "carbon dioxide" --limit 5
```

### Analysis Commands

```bash
# Activity details
fplca activity "12345678-1234-1234-1234-123456789abc"

# Supply chain tree (default depth: 2)
fplca tree "12345678-1234-1234-1234-123456789abc"

# Life cycle inventory
fplca inventory "12345678-1234-1234-1234-123456789abc"

# Impact assessment
fplca lcia "12345678-1234-1234-1234-123456789abc" \
  --method "./methods/EF-3.1.xml" \
  --output results.xml \
  --csv results.csv
```

### Matrix Export

```bash
# Export full database matrices (Ecoinvent universal format)
fplca export-matrices ./output_dir

# Debug targeted matrix slices
fplca debug-matrices "12345678-1234-1234-1234-123456789abc" \
  --output ./debug_output
```

---

## Output Formats

### JSON (Default)
```bash
fplca --format json activities --limit 2
```
```json
{
  "srResults": [
    {"prsId": "12345...", "prsName": "electricity production", "prsLocation": "DE"}
  ],
  "srTotal": 156,
  "srLimit": 2
}
```

### CSV with JSONPath

CSV format requires `--jsonpath` to specify which data to extract:

```bash
# Extract search results
fplca --format csv --jsonpath "srResults" activities --limit 5
```
```csv
prsId,prsLocation,prsName
12345678-1234-1234-1234-123456789abc,DE,electricity production
```

```bash
# Extract activity exchanges
fplca --format csv --jsonpath "piActivity.pfaExchanges" activity "12345..."
```

```bash
# Extract tree edges
fplca --format csv --jsonpath "teEdges" tree "12345..."
```

```bash
# Extract inventory flows
fplca --format csv --jsonpath "ieFlows" inventory "12345..."
```

| Command | Recommended JSONPath | Extracts |
|---------|---------------------|----------|
| `activities` | `srResults` | Activity search results |
| `flows` | `srResults` | Flow search results |
| `activity <uuid>` | `piActivity.pfaExchanges` | Activity exchanges |
| `tree <uuid>` | `teEdges` | Supply chain connections |
| `inventory <uuid>` | `ieFlows` | Environmental flows |

---

## Building

### Linux/macOS

```bash
./build.sh              # Download PETSc/SLEPc and build
./build.sh --test       # Build and run tests
./build.sh --desktop    # Build desktop application
```

The build script automatically downloads and compiles PETSc and SLEPc if not already present.

### Windows (MSYS2)

1. Install MSYS2 from https://www.msys2.org/
2. Open "MSYS2 UCRT64" terminal from Start menu
3. Install dependencies:
   ```bash
   pacman -S make python git \
             mingw-w64-ucrt-x86_64-gcc \
             mingw-w64-ucrt-x86_64-gcc-fortran \
             mingw-w64-ucrt-x86_64-cmake \
             mingw-w64-ucrt-x86_64-openblas \
             mingw-w64-ucrt-x86_64-msmpi \
             mingw-w64-ucrt-x86_64-zlib
   ```
4. Install Haskell:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```
5. Build:
   ```bash
   ./build.sh              # Same script as Linux/macOS
   ./build.sh --desktop    # Builds Windows installer (.exe)
   ```

### Docker

```bash
docker build -t fplca .
docker run -p 8080:8080 -v /path/to/data:/data fplca
```

---

## Performance

### Memory Management
- **In-memory processing**: Entire database loaded into RAM for speed
- **Smart caching**: Per-database cache files (`fplca.cache.{dbName}.bin.zst`) with automatic schema-based invalidation
- **Matrix optimization**: Sparse unboxed matrix storage with minimal overhead

### Scaling Guidelines

| Database Size | Cache Load (RSS) | Cold Start | Cache Time | Tree Solve |
|---------------|------------------|------------|------------|------------|
| Sample (3 activities) | ~50 MB | ~1s | < 0.1s | < 100ms |
| EcoInvent 3.12 (25k activities) | ~500 MB* | ~45s | ~0.5s | 5-15s |
| Custom large DB (50k+ activities) | ~1 GB* | ~120s | ~1s | 15-60s |

*With GHC RTS heap cap (`+RTS -M1G`). Without cap, GHC may allocate 5-7GB arena but only use ~300MB live data.

### Memory Control with GHC RTS

Control memory usage using GHC runtime options:

```bash
# Limit heap to 800MB for shared systems
fplca --data ./data activities --limit 5 +RTS -M800M -H256M -A16M -c -RTS

# Web server with moderate heap
fplca --data ./data server +RTS -M1G -H512M -A16M -c -I30 -RTS
```

**RTS Options:**
- `-M<size>`: Maximum heap size (hard cap)
- `-H<size>`: Initial heap size (pre-allocates for performance)
- `-A<size>`: Allocation area size (nursery for young generation GC)
- `-c`: Enable compacting GC (reduces fragmentation)
- `-I<sec>`: Idle GC interval (forces cleanup during idle)

---

## Cross-Database Linking

fplca supports linking processes across different databases. For example, Agribalyse activities can reference Ecoinvent background processes.

When uploading or configuring a database, the system:
1. Analyzes missing supplier references
2. Suggests dependency databases that can fulfill those references
3. Auto-loads dependency databases when finalizing
4. Validates completeness metrics (% of suppliers resolved)

This is managed through the database setup page in the web UI or via the setup API endpoints.

---

## Database Upload

The web interface supports uploading your own databases:

1. Navigate to the Upload page
2. Select a database archive (zip, 7z, or tar)
3. Supported formats:
   - EcoSpold2 directories (.spold files)
   - EcoSpold1 directories (.xml files)
   - Single EcoSpold1 XML files with multiple datasets
   - SimaPro CSV files
4. Configure cross-database dependencies if needed
5. Database is loaded and cached for instant future access

Uploaded databases are stored in the `uploads/` directory and persist across server restarts.

---

## Testing

The project includes 31 automated tests covering:
- **Matrix construction**: Validates sign convention (prevents double negation bug)
- **Inventory calculation**: Golden tests with SAMPLE.min3 (CO2=0.96kg, Zinc=0.00072kg)
- **Parser tests**: Validates parsers with SAMPLE datasets
- **Matrix export**: Validates Ecoinvent universal matrix format compliance

Run tests:
```bash
./build.sh --test

# Or manually
cabal test --test-show-details=streaming
```

---

## Architecture

### Backend (Haskell)
- **LCA.Types**: Core data structures (Process, Exchange, Flow, ProcessTree)
- **LCA.Tree**: Process tree construction with circular dependency handling
- **LCA.Inventory**: LCI calculation via recursive tree traversal
- **LCA.Matrix**: Sparse matrix computation with PETSc solvers
- **LCA.DatabaseManager**: Multi-database state management with STM
- **EcoSpold.Loader**: Lazy on-demand loading with caching
- **EcoSpold.CrossDatabaseLinking**: Cross-database supplier resolution
- **SimaPro.Parser**: SimaPro CSV format parser

### Frontend (Elm)
- **Single-page architecture**: Custom Spa.Page framework with Effect type
- **Shared state**: Cross-page state in Shared.elm (databases, caches, console)
- **Page modules**: Activities, Tree, Graph, Inventory, LCIA, Databases, Upload, DatabaseSetup
- **RemoteData pattern**: `NotAsked | Loading | Loaded a | Failed String` for async state
- **URL routing**: Bookmarkable views with Route.elm

### Desktop (Tauri + Rust)
- Bundles Haskell backend with WebView-based frontend
- Includes loading screen with progress logs
- Windows installer packages all dependencies (PETSc, SLEPc, MS-MPI)

---

## License

GNU AFFERO GENERAL PUBLIC LICENSE 3.0 or later - See LICENSE file for details.

---

## Version

Current version: 0.6.0-dev

See CHANGELOG.md for release history and detailed changes.
