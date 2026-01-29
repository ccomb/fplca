# fplca

**fplca** is a high-performance Life Cycle Assessment (LCA) engine written in Haskell. It runs entirely in memory and provides both a command-line interface and REST API for LCA computations.

## Features

- **📊 LCA Computations**: Complete life cycle inventory (LCI) and impact assessment (LCIA)
- **🌳 Supply Chain Trees**: Recursive dependency tree visualization and analysis
- **🔍 Search & Discovery**: Find activities and flows across LCA databases
- **📈 Multiple Output Formats**: JSON, CSV with JSONPath extraction, Pretty tables
- **🚀 High Performance**: In-memory processing with PETSc linear algebra
- **🌐 REST API**: HTTP server with comprehensive endpoints and web UI
- **💾 Smart Caching**: Automatic schema-based cache invalidation
- **🗄️ Multi-Database**: Load and switch between multiple databases instantly
- **📋 Multiple Formats**: EcoSpold2 (.spold), EcoSpold1 (.xml), SimaPro CSV

---

## Overview & Capabilities

### Core LCA Functions
- **Load LCA databases** in multiple formats:
  - EcoSpold2 (.spold) - Ecoinvent 3.x
  - EcoSpold1 (.xml) - Ecoinvent 2.x, BAFU
  - SimaPro CSV - Agribalyse
- **Build process dependency trees** with circular dependency handling
- **Compute life cycle inventories** (LCI) via matrix operations
- **Apply characterization methods** for impact assessment (LCIA)
- **Export results** in multiple formats (ILCD XML, CSV, JSON)

### Advanced Features
- **Matrix-based computation** using PETSc for efficient large-scale solving
- **Automatic loop detection** in supply chains
- **Flexible tree depth limits** for performance optimization
- **Multi-threaded processing** with automatic CPU core detection
- **Database quality validation** with comprehensive health checks

---

## Command Reference

### Global Options

| Option | Description | Example |
|--------|-------------|---------|
| `--config FILE` | Configuration file with multiple databases | `--config fplca.toml` |
| `--data PATH` | Single database path (alternative to --config) | `--data ./ECOINVENT3.9.1` |
| `--format FORMAT` | Output format: `json\|csv\|table\|pretty` | `--format json` |
| `--jsonpath PATH` | JSONPath for CSV extraction (required with CSV) | `--jsonpath "srResults"` |
| `--tree-depth N` | Maximum tree depth for calculations | `--tree-depth 3` |
| `--no-cache` | Disable caching (for development) | `--no-cache` |

**Note:** Use `--config` for multi-database mode or `--data` for single-database mode (not both).

### Commands

#### 🔍 Search Commands

**Search Activities**
```bash
# Find activities by name
fplca --data ./data activities --name "electricity"

# Geographic filtering
fplca activities --name "transport" --geo "DE" --limit 5

# Product-based search
fplca activities --product "steel" --limit 10 --offset 20
```

**Search Flows**
```bash
# Find flows by keyword
fplca flows --query "carbon dioxide" --limit 5

# Language-specific search
fplca flows --query "CO2" --lang "en" --limit 10
```

#### 📊 Activity Analysis

**Activity Information**
```bash
# Get complete activity details
fplca activity "12345678-1234-1234-1234-123456789abc"
```

**Supply Chain Tree**
```bash
# Build dependency tree (default depth: 2)
fplca tree "12345678-1234-1234-1234-123456789abc"

# Custom tree depth
fplca --tree-depth 4 tree "12345678-1234-1234-1234-123456789abc"
```

**Life Cycle Inventory**
```bash
# Compute full inventory
fplca inventory "12345678-1234-1234-1234-123456789abc"
```

#### 🧮 Impact Assessment

**LCIA Computation**
```bash
# Compute impacts with method file
fplca lcia "12345678-1234-1234-1234-123456789abc" \
  --method "./methods/PEF_v3.1.xml"

# Export to XML and CSV
fplca lcia "12345678-1234-1234-1234-123456789abc" \
  --method "./methods/PEF_v3.1.xml" \
  --output results.xml \
  --csv results.csv
```

#### 🔧 Matrix Export & Debugging

**Export Matrices (Ecoinvent Universal Format)**
```bash
# Export full database matrices in universal format
fplca export-matrices ./output_dir
```

**Debug Matrices**
```bash
# Export targeted matrix slices for debugging
fplca debug-matrices "12345678-1234-1234-1234-123456789abc" \
  --output ./debug_output
```

#### 🌐 API Server

**Start Server**
```bash
# Start on default port (8080)
fplca --data ./data server

# Custom port
fplca --data ./data server --port 3000

# With password protection (HTTP Basic Auth)
fplca --data ./data server --password mysecret
# Or via environment variable
FPLCA_PASSWORD=mysecret fplca --data ./data server

# Web interface available at http://localhost:8080/
# API endpoints at http://localhost:8080/api/v1/
```

---

## Output Formats

### JSON Format
```bash
fplca --format json activities --limit 2
```
```json
{
  "srResults": [
    {"prsId": "12345...", "prsName": "electricity production", "prsLocation": "DE"},
    {"prsId": "67890...", "prsName": "transport by truck", "prsLocation": "EU"}
  ],
  "srTotal": 156,
  "srLimit": 2
}
```

### Pretty Format (Default)
```bash
fplca activities --limit 2  # Uses pretty format by default
```
```json
{
    "srResults": [
        {
            "prsId": "12345678-1234-1234-1234-123456789abc",
            "prsLocation": "DE",
            "prsName": "electricity production"
        }
    ],
    "srTotal": 156
}
```

### CSV Format with JSONPath

**⚠️ Important: CSV format requires `--jsonpath` to specify which data to extract**

#### Extract Search Results
```bash
fplca --format csv --jsonpath "srResults" activities --limit 5
```
```csv
prsId,prsLocation,prsName
12345678-1234-1234-1234-123456789abc,DE,electricity production
87654321-4321-4321-4321-cba987654321,FR,transport by truck
```

#### Extract Activity Exchanges
```bash
fplca --format csv --jsonpath "piActivity.pfaExchanges" activity "12345..."
```
```csv
ewuFlowName,ewuFlowCategory,ewuUnitName,ewuExchange.techAmount,ewuExchange.techIsInput
electricity,technosphere,kWh,1.0,false
natural gas,technosphere,m3,2.5,true
carbon dioxide,air,kg,0.85,false
```

#### Extract Tree Edges
```bash
fplca --format csv --jsonpath "teEdges" tree "12345..."
```
```csv
teFlow.fiName,teFlow.fiCategory,teFrom,teTo,teQuantity,teUnit
electricity,technosphere,12345...,67890...,1.0,kWh
natural gas,technosphere,67890...,11111...,2.5,m3
```

#### Extract Inventory Flows
```bash
fplca --format csv --jsonpath "ieFlows" inventory "12345..."
```
```csv
ifdFlow.flowName,ifdFlow.flowCategory,ifdQuantity,ifdUnitName,ifdIsEmission
carbon dioxide,air,1.25,kg,true
methane,air,0.02,kg,true
coal,natural resources,-2.5,kg,false
```

### JSONPath Reference

| Command | Recommended JSONPath | Extracts |
|---------|---------------------|----------|
| `activities` | `srResults` | Activity search results |
| `flows` | `srResults` | Flow search results |
| `activity <uuid>` | `piActivity.pfaExchanges` | Activity exchanges |
| `tree <uuid>` | `teEdges` | Supply chain connections |
| `inventory <uuid>` | `ieFlows` | Environmental flows |

---

## Configuration File

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

# Ecoinvent 3.9.1 (EcoSpold2 format)
[[databases]]
name = "ecoinvent-3.9.1"
displayName = "Ecoinvent 3.9.1"
path = "../ecoinvent/ECOINVENT3.9.1/datasets"
active = true

# Agribalyse (SimaPro CSV format)
[[databases]]
name = "agribalyse-3.2"
displayName = "Agribalyse 3.2"
path = "../agribalyse/AGB32_final.CSV"
description = "French LCI database (SimaPro CSV format)"
active = true

# BAFU (EcoSpold1 format)
[[databases]]
name = "bafu-2025"
displayName = "BAFU 2025"
path = "../BAFU/LCI ecoSpold v1 Files"
active = true

# LCIA Methods
[[methods]]
name = "EF-3.1"
path = "../EF-v3.1/ILCD/lciamethods"
active = true
```

### Database Options

| Field | Required | Description |
|-------|----------|-------------|
| `name` | Yes | Internal identifier (used in cache filenames) |
| `displayName` | Yes | Human-readable name for UI |
| `path` | Yes | Path to database files |
| `description` | No | Optional description |
| `active` | Yes | If true, database is pre-loaded at startup |
| `default` | No | If true, this database is selected by default |

### Activity Aliases

For EcoSpold1 databases, you may need to map flow names to activity names when supplier links are ambiguous:

```toml
[[databases]]
name = "bafu-2025"
path = "../BAFU/LCI ecoSpold v1 Files"
active = true
# Map "flow name" → "activity name|location"
activityAliases."Electricity, production mix {ENTSO-E}" = "Electricity, high voltage, at grid {ENTSO-E}|ENTSO-E"
activityAliases."Natural gas, at consumer {RER}" = "Natural gas, high pressure, at consumer {DE}|DE"
```

---

## REST API Endpoints

When running in server mode, the following endpoints are available:

### Databases
- `GET /api/v1/databases` - List all configured databases with status
- `POST /api/v1/databases/{name}/activate` - Switch to a different database

### Activities
- `GET /api/v1/activities` - Search activities
- `GET /api/v1/activity/{uuid}` - Get activity details
- `GET /api/v1/activity/{uuid}/tree` - Get supply chain tree
- `GET /api/v1/activity/{uuid}/inventory` - Get life cycle inventory

### Flows
- `GET /api/v1/flows` - Search flows
- `GET /api/v1/flow/{uuid}` - Get flow details

### Impact Assessment
- `GET /api/v1/methods` - List available LCIA methods
- `POST /api/v1/lcia/{uuid}` - Compute LCIA with method

### Example API Usage
```bash
# Start server with config file
fplca --config fplca.toml server

# List databases
curl "http://localhost:8080/api/v1/databases"

# Switch database
curl -X POST "http://localhost:8080/api/v1/databases/ecoinvent-3.9.1/activate"

# Search activities
curl "http://localhost:8080/api/v1/activities?name=electricity&limit=5"

# Get activity tree
curl "http://localhost:8080/api/v1/activity/12345.../tree?depth=3"

# Compute inventory
curl "http://localhost:8080/api/v1/activity/12345.../inventory"
```

---

## Usage Examples

### Basic Workflow
```bash
# 1. Search for an activity
fplca --data ./ECOINVENT3.9.1 activities --name "electricity production" --geo "DE" --limit 1

# 2. Get the activity UUID from results, then analyze its supply chain
fplca tree "12345678-1234-1234-1234-123456789abc"

# 3. Compute environmental inventory
fplca inventory "12345678-1234-1234-1234-123456789abc"

# 4. Export tree edges to CSV for further analysis
fplca --format csv --jsonpath "teEdges" tree "12345678-1234-1234-1234-123456789abc" > supply_chain.csv
```

### Data Analysis Workflows

**Export Activity Network to CSV**
```bash
# Get all exchanges for detailed analysis
fplca --format csv --jsonpath "piActivity.pfaExchanges" \
  activity "12345678-1234-1234-1234-123456789abc" > exchanges.csv
```

**Inventory Analysis**
```bash
# Extract biosphere flows for impact assessment
fplca --format csv --jsonpath "ieFlows" \
  inventory "12345678-1234-1234-1234-123456789abc" > inventory.csv
```

**Multi-format Output**
```bash
# JSON for programmatic use
fplca --format json inventory "12345..." > inventory.json

# CSV for spreadsheet analysis
fplca --format csv --jsonpath "ieFlows" inventory "12345..." > inventory.csv

# Pretty format for human reading
fplca --format pretty inventory "12345..."
```

### Performance Optimization

**Caching**
```bash
# First run builds cache (slower)
fplca --data ./ECOINVENT3.9.1 activities --name "steel"

# Subsequent runs use cache (much faster)
fplca --data ./ECOINVENT3.9.1 activities --name "aluminum"

# Disable cache for development
fplca --no-cache --data ./ECOINVENT3.9.1 activities --name "cement"
```

**Tree Depth Control**
```bash
# Fast shallow analysis
fplca --tree-depth 1 tree "12345..."

# Comprehensive deep analysis (slower but complete)
fplca --tree-depth 5 tree "12345..."
```

---

## Data Quality & Validation

The engine performs automatic database validation:

- ✅ **Orphaned flows check**: Ensures all flows are properly linked
- ✅ **Reference products**: Validates all activities have reference outputs
- ✅ **Exchange balance**: Checks input/output ratios for sanity
- ✅ **Geographic coverage**: Reports available locations
- ✅ **Unit consistency**: Validates unit relationships
- ✅ **Matrix properties**: Analyzes sparsity and conditioning

### Sample Validation Output
```
Database Quality Validation:
  ✓ No orphaned flows found
  ✓ All activities have reference products
  ✓ Average exchange balance: 2.8:1 (outputs:inputs)
  ✓ Database quality: Excellent

Performance Characteristics:
  Matrix density: 12.4% (very sparse - excellent for performance)
  Total matrix entries: 2,847,392 non-zero values
  Solver complexity: O(n^1.5) for 15,842 activities
  Expected solve time: ~15-25 seconds (MUMPS direct solver)
```

---

## Performance & Scaling

### Memory Management
- **In-memory processing**: Entire database loaded into RAM for speed
- **Smart caching**: Per-database cache files (`fplca.cache.{dbName}.bin.zst`) with automatic schema-based invalidation
- **Matrix optimization**: Unboxed sparse matrix storage with minimal overhead
- **Memory optimizations**: UUID interning, unboxed vectors, strict evaluation
- **Configurable limits**: GHC RTS options to control memory usage (see below)

### Cache System
Cache files are stored in the working directory with the format `fplca.cache.{dbName}.bin.zst`. The cache includes:
- Pre-parsed activities, flows, and units
- Pre-computed sparse matrices (technosphere A, biosphere B)
- Automatic invalidation when the data schema changes (no manual version bumping needed)

### Computation Performance
- **PETSc integration**: High-performance linear algebra via MUMPS solver
- **Multi-threading**: Automatic CPU core detection and utilization
- **Matrix precomputation**: Pre-factored matrices for fast repeated solves
- **Depth limiting**: Configurable tree depth to balance accuracy vs speed

### Scaling Guidelines

| Database Size | Live Data | Cache Load (RSS) | Cold Start | Cache Time | Tree Solve |
|---------------|-----------|------------------|------------|------------|------------|
| Sample (3 activities) | ~10 MB | ~50 MB | ~1s | < 0.1s | < 100ms |
| EcoInvent 3.11 (25k activities) | ~305 MB | ~500 MB* | ~45s | ~0.5s | 5-15s |
| Custom large DB (50k+ activities) | ~600 MB | ~1 GB* | ~120s | ~1s | 15-60s |

*With GHC RTS heap cap (`+RTS -M1G`). Without cap, GHC may allocate 5-7GB arena but only use ~300MB live data.

### Memory Control with GHC RTS Options

You can control memory usage using GHC runtime system (RTS) options. Add them after your command:

**Cache Load (Memory-Efficient)**
```bash
# Limit heap to 800MB for shared systems
fplca --data ./data activities --limit 5 +RTS -M800M -H256M -A16M -c -RTS
```

**Cold Start (Performance)**
```bash
# Allow larger heap for initial parsing
fplca --no-cache --data ./data activities +RTS -M5G -H2G -A64M -RTS
```

**Web Server (Balanced)**
```bash
# Moderate heap with automatic garbage collection
fplca --data ./data server +RTS -M1G -H512M -A16M -c -I30 -RTS
```

**RTS Options Explained:**
- `-M<size>`: Maximum heap size (hard cap, prevents excessive memory use)
- `-H<size>`: Initial heap size (pre-allocates for performance)
- `-A<size>`: Allocation area size (nursery for young generation GC)
- `-c`: Enable compacting GC (reduces memory fragmentation)
- `-I<sec>`: Idle GC interval (forces cleanup during idle periods)

**Why use RTS options?**
- GHC pre-allocates large heap (~5-7GB) by default for performance
- Actual live data is much smaller (~300MB for EcoInvent 3.11)
- RTS caps prevent memory waste on shared systems
- No performance penalty when caps are reasonable

---

## Error Handling & Troubleshooting

### Common Issues

**CSV Format Errors**
```bash
# ❌ Error: CSV requires JSONPath
fplca --format csv activities

# ✅ Correct: Specify JSONPath
fplca --format csv --jsonpath "srResults" activities
```

**Invalid JSONPath**
```bash
# ❌ Error: Path not found
fplca --format csv --jsonpath "invalidPath" activities
# Output: Error extracting JSONPath 'invalidPath': Path component 'invalidPath' not found

# ✅ Correct paths for each command
fplca --format csv --jsonpath "srResults" activities      # Search results
fplca --format csv --jsonpath "teEdges" tree "uuid"       # Tree edges
fplca --format csv --jsonpath "ieFlows" inventory "uuid"  # Inventory flows
```

**Memory Issues**
```bash
# If running out of memory, cap the heap with RTS options
fplca inventory "uuid" +RTS -M1G -RTS

# Or reduce tree depth for complex calculations
fplca --tree-depth 1 inventory "uuid"

# Or disable caching during development
fplca --no-cache inventory "uuid"
```

### Validation Errors
The CLI validates options before execution:
- `--format csv` requires `--jsonpath`
- `--jsonpath` can only be used with `--format csv`
- Invalid UUIDs are caught early with helpful messages

---

## Building

### Linux/macOS

```bash
./build.sh              # Download PETSc/SLEPc and build
./build.sh --test       # Build and run tests
./build.sh --desktop    # Build desktop application
```

### Windows

Windows builds require MSYS2 with UCRT64 environment. All builds use the same bash script.

1. Install MSYS2 from https://www.msys2.org/
2. Open "MSYS2 UCRT64" terminal (from Start menu)
3. Install dependencies:
   ```bash
   pacman -S make python git \
             mingw-w64-ucrt-x86_64-gcc \
             mingw-w64-ucrt-x86_64-gcc-fortran \
             mingw-w64-ucrt-x86_64-cmake \
             mingw-w64-ucrt-x86_64-make \
             mingw-w64-ucrt-x86_64-openblas \
             mingw-w64-ucrt-x86_64-msmpi \
             mingw-w64-ucrt-x86_64-pkgconf \
             mingw-w64-ucrt-x86_64-zlib \
             mingw-w64-ucrt-x86_64-tools-git
   ```
4. Install Haskell toolchain:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```
5. Run build:
   ```bash
   ./build.sh              # Same script as Linux/macOS
   ./build.sh --test
   ./build.sh --desktop
   ```

### Docker

```bash
docker build -t fplca .
docker run -p 8080:8080 -v /path/to/data:/data fplca
```

### Configuration Files

- `versions.env` - Central version definitions for all tools (PETSc, GHC, Node, etc.)
- `petsc.env` - PETSc/SLEPc build configuration
- `lib.sh` - Shared bash functions for build scripts

---

## License

GNU AFFERO GENERAL PUBLIC LICENSE 3.0 or later - See LICENSE file for details.

---
