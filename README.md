# acv-engine

**acv-engine** is a high-performance Life Cycle Assessment (LCA) engine written in Haskell. It runs entirely in memory and provides both a command-line interface and REST API for LCA computations.

## Features

- **📊 LCA Computations**: Complete life cycle inventory (LCI) and impact assessment (LCIA)
- **🌳 Supply Chain Trees**: Recursive dependency tree visualization and analysis
- **🔍 Search & Discovery**: Find activities and flows across LCA databases
- **📈 Multiple Output Formats**: JSON, CSV with JSONPath extraction, Pretty tables
- **🚀 High Performance**: In-memory processing with PETSc linear algebra
- **🌐 REST API**: HTTP server with comprehensive endpoints
- **💾 Smart Caching**: Automatic caching for fast repeated queries
- **📋 EcoSpold Support**: Native .spold file format parsing (EcoInvent)

---

## Overview & Capabilities

### Core LCA Functions
- **Load EcoSpold databases** (.spold files from EcoInvent)
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
| `--data PATH` | Data directory containing .spold files | `--data ./ECOINVENT3.9.1` |
| `--format FORMAT` | Output format: `json\|csv\|table\|pretty` | `--format json` |
| `--jsonpath PATH` | JSONPath for CSV extraction (required with CSV) | `--jsonpath "srResults"` |
| `--tree-depth N` | Maximum tree depth for calculations | `--tree-depth 3` |
| `--no-cache` | Disable caching (for development) | `--no-cache` |

### Commands

#### 🔍 Search Commands

**Search Activities**
```bash
# Find activities by name
acv-cli --data ./data activities --name "electricity"

# Geographic filtering
acv-cli activities --name "transport" --geo "DE" --limit 5

# Product-based search
acv-cli activities --product "steel" --limit 10 --offset 20
```

**Search Flows**
```bash
# Find flows by keyword
acv-cli flows --query "carbon dioxide" --limit 5

# Language-specific search
acv-cli flows --query "CO2" --lang "en" --limit 10
```

#### 📊 Activity Analysis

**Activity Information**
```bash
# Get complete activity details
acv-cli activity "12345678-1234-1234-1234-123456789abc"
```

**Supply Chain Tree**
```bash
# Build dependency tree (default depth: 2)
acv-cli tree "12345678-1234-1234-1234-123456789abc"

# Custom tree depth
acv-cli --tree-depth 4 tree "12345678-1234-1234-1234-123456789abc"
```

**Life Cycle Inventory**
```bash
# Compute full inventory
acv-cli inventory "12345678-1234-1234-1234-123456789abc"
```

#### 🧮 Impact Assessment

**LCIA Computation**
```bash
# Compute impacts with method file
acv-cli lcia "12345678-1234-1234-1234-123456789abc" \
  --method "./methods/PEF_v3.1.xml"

# Export to XML and CSV
acv-cli lcia "12345678-1234-1234-1234-123456789abc" \
  --method "./methods/PEF_v3.1.xml" \
  --output results.xml \
  --csv results.csv
```

#### 🌐 API Server

**Start Server**
```bash
# Start on default port (8080)
acv-cli --data ./data server

# Custom port
acv-cli --data ./data server --port 3000
```

---

## Output Formats

### JSON Format
```bash
acv-cli --format json activities --limit 2
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
acv-cli activities --limit 2  # Uses pretty format by default
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
acv-cli --format csv --jsonpath "srResults" activities --limit 5
```
```csv
prsId,prsLocation,prsName
12345678-1234-1234-1234-123456789abc,DE,electricity production
87654321-4321-4321-4321-cba987654321,FR,transport by truck
```

#### Extract Activity Exchanges
```bash
acv-cli --format csv --jsonpath "piActivity.pfaExchanges" activity "12345..."
```
```csv
ewuFlowName,ewuFlowCategory,ewuUnitName,ewuExchange.techAmount,ewuExchange.techIsInput
electricity,technosphere,kWh,1.0,false
natural gas,technosphere,m3,2.5,true
carbon dioxide,air,kg,0.85,false
```

#### Extract Tree Edges
```bash
acv-cli --format csv --jsonpath "teEdges" tree "12345..."
```
```csv
teFlow.fiName,teFlow.fiCategory,teFrom,teTo,teQuantity,teUnit
electricity,technosphere,12345...,67890...,1.0,kWh
natural gas,technosphere,67890...,11111...,2.5,m3
```

#### Extract Inventory Flows
```bash
acv-cli --format csv --jsonpath "ieFlows" inventory "12345..."
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

## REST API Endpoints

When running in server mode, the following endpoints are available:

### Activities
- `GET /api/v1/activities` - Search activities
- `GET /api/v1/activity/{uuid}` - Get activity details
- `GET /api/v1/activity/{uuid}/tree` - Get supply chain tree
- `GET /api/v1/activity/{uuid}/inventory` - Get life cycle inventory

### Flows
- `GET /api/v1/flows` - Search flows
- `GET /api/v1/flow/{uuid}` - Get flow details

### Impact Assessment
- `POST /api/v1/lcia/{uuid}` - Compute LCIA with method

### Example API Usage
```bash
# Start server
acv-cli --data ./ECOINVENT3.9.1 server --port 8080

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
acv-cli --data ./ECOINVENT3.9.1 activities --name "electricity production" --geo "DE" --limit 1

# 2. Get the activity UUID from results, then analyze its supply chain
acv-cli tree "12345678-1234-1234-1234-123456789abc"

# 3. Compute environmental inventory
acv-cli inventory "12345678-1234-1234-1234-123456789abc"

# 4. Export tree edges to CSV for further analysis
acv-cli --format csv --jsonpath "teEdges" tree "12345678-1234-1234-1234-123456789abc" > supply_chain.csv
```

### Data Analysis Workflows

**Export Activity Network to CSV**
```bash
# Get all exchanges for detailed analysis
acv-cli --format csv --jsonpath "piActivity.pfaExchanges" \
  activity "12345678-1234-1234-1234-123456789abc" > exchanges.csv
```

**Inventory Analysis**
```bash
# Extract biosphere flows for impact assessment
acv-cli --format csv --jsonpath "ieFlows" \
  inventory "12345678-1234-1234-1234-123456789abc" > inventory.csv
```

**Multi-format Output**
```bash
# JSON for programmatic use
acv-cli --format json inventory "12345..." > inventory.json

# CSV for spreadsheet analysis
acv-cli --format csv --jsonpath "ieFlows" inventory "12345..." > inventory.csv

# Pretty format for human reading
acv-cli --format pretty inventory "12345..."
```

### Performance Optimization

**Caching**
```bash
# First run builds cache (slower)
acv-cli --data ./ECOINVENT3.9.1 activities --name "steel"

# Subsequent runs use cache (much faster)
acv-cli --data ./ECOINVENT3.9.1 activities --name "aluminum"

# Disable cache for development
acv-cli --no-cache --data ./ECOINVENT3.9.1 activities --name "cement"
```

**Tree Depth Control**
```bash
# Fast shallow analysis
acv-cli --tree-depth 1 tree "12345..."

# Comprehensive deep analysis (slower but complete)
acv-cli --tree-depth 5 tree "12345..."
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
- **Smart caching**: Automatic disk cache for repeated database loads
- **Matrix optimization**: Sparse matrix storage for memory efficiency
- **Lazy evaluation**: On-demand tree construction to reduce memory usage

### Computation Performance
- **PETSc integration**: High-performance linear algebra via MUMPS solver
- **Multi-threading**: Automatic CPU core detection and utilization
- **Matrix precomputation**: Pre-factored matrices for fast repeated solves
- **Depth limiting**: Configurable tree depth to balance accuracy vs speed

### Scaling Guidelines

| Database Size | RAM Required | Typical Load Time | Tree Computation |
|---------------|--------------|-------------------|------------------|
| Sample (3 activities) | 50 MB | < 1 second | < 100ms |
| EcoInvent 3.9.1 (20k activities) | 8-12 GB | 30-60 seconds | 5-30 seconds |
| Custom large DB (50k+ activities) | 16+ GB | 2-5 minutes | 30-120 seconds |

---

## Error Handling & Troubleshooting

### Common Issues

**CSV Format Errors**
```bash
# ❌ Error: CSV requires JSONPath
acv-cli --format csv activities

# ✅ Correct: Specify JSONPath
acv-cli --format csv --jsonpath "srResults" activities
```

**Invalid JSONPath**
```bash
# ❌ Error: Path not found
acv-cli --format csv --jsonpath "invalidPath" activities
# Output: Error extracting JSONPath 'invalidPath': Path component 'invalidPath' not found

# ✅ Correct paths for each command
acv-cli --format csv --jsonpath "srResults" activities      # Search results
acv-cli --format csv --jsonpath "teEdges" tree "uuid"       # Tree edges
acv-cli --format csv --jsonpath "ieFlows" inventory "uuid"  # Inventory flows
```

**Memory Issues**
```bash
# If running out of memory, try reducing tree depth
acv-cli --tree-depth 1 inventory "uuid"

# Or disable caching to free memory
acv-cli --no-cache inventory "uuid"
```

### Validation Errors
The CLI validates options before execution:
- `--format csv` requires `--jsonpath`
- `--jsonpath` can only be used with `--format csv`
- Invalid UUIDs are caught early with helpful messages

---

## License

MIT License - See LICENSE file for details.

---

## Contributing & Development

This is a research/educational tool designed for LCA practitioners and developers. The codebase prioritizes:

- **Correctness**: Rigorous LCA methodology implementation
- **Performance**: Efficient large-scale computation
- **Usability**: Clear CLI interface and comprehensive documentation
- **Extensibility**: Modular design for easy feature addition

For development setup, see `CLAUDE.md` for detailed build instructions and architecture overview.