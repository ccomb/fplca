# volca-bench

Benchmarks for VoLCA's hot paths, run on **real** LCA fixtures (Agribalyse,
Ecoinvent, BAFU…). Emits a JSON file with one entry per benched capability,
intended to be consumed by an external page that publishes the numbers.

## What it measures

| Capability | What | Aligned N |
|---|---|---|
| `parser.ecospold2` | Parse N EcoSpold 2 process files (XML) in parallel | 1000 processes |
| `parser.ecospold1` | Parse N EcoSpold 1 process files (XML) in parallel | 1000 processes |
| `parser.simapro` | Parse a SimaPro CSV export (whole file) | actual N |
| `parser.ilcd` | Parse an ILCD process directory in parallel | actual N |
| `parser.method_ilcd_xml` | Parse one ILCD method XML | actual N CFs |
| `parser.method_csv` | Parse a generic CSV method | actual N CFs |
| `parser.method_simapro_csv` | Parse a SimaPro method CSV | actual N CFs |
| `parser.method_olca_json` | Parse an openLCA-JSON method | actual N CFs |
| `loader.single_db` | Load + link + index one database end-to-end | actual N processes |
| `loader.multi_db_cross_link` | Cross-DB supplier resolution between two databases | total inputs walked |
| `solve.scaling_vector` | Solve `(I − A) x = d` for one product | matrix size |
| `solve.inventory_matrix` | Solve + biosphere matvec (`g = B · x`) for one product | matrix size |
| `solve.batch_multi_rhs` | Multi-RHS LCI solve for 100 products in parallel | 100 products |
| `lcia.synthetic.fanout` | Score 27 LCIA methods one-by-one (synthetic) | 27 methods |
| `lcia.synthetic.set_batched` | Score 27 LCIA methods in one batched matvec | 27 methods |
| `lcia.real.score_method` | Score one product against a real LCIA method | actual N CFs |

The four process-parsers all run on N=1000 processes so their timings can
be compared at a glance: `EcoSpold 2: 4.2 s · EcoSpold 1: 6.1 s · …`.
A bench is **omitted** rather than scaled if its fixture has fewer than N
items, to keep the visual comparison meaningful.

`lcia.real.regional` (regionalised cross-DB scoring) is the one capability
still pending — it needs a method file with regional CFs and a
location-hierarchy fixture, which we don't ship by default. The hook is
in place; it can be added next to `lcia.real.score_method` when those
fixtures land.

## Required env vars

The bench ships no fixtures: each capability looks at a dedicated env var
to find its data, and is omitted (with a clear log line) when the var is
unset or the path doesn't exist.

| Env var | Path | Used by |
|---|---|---|
| `VOLCA_BENCH_AGRIBALYSE` | unzipped SimaPro CSV (`AGB32_final.CSV`) | `parser.simapro` |
| `VOLCA_BENCH_ECOINVENT` | extracted Ecoinvent dir or its `datasets/` subdir | `parser.ecospold2` |
| `VOLCA_BENCH_BAFU` | BAFU `LCI ecoSpold v1 Files/` directory | `parser.ecospold1`, fallback DB for loader/solve/lcia.real |
| `VOLCA_BENCH_ILCD` | one ILCD database directory (with `processes/`, `flows/`, `flowproperties/`, `unitgroups/`) | `parser.ilcd` |
| `VOLCA_BENCH_METHOD_EF_ILCD` | one ILCD method XML file | `parser.method_ilcd_xml` |
| `VOLCA_BENCH_METHOD_CSV` | one generic CSV method file | `parser.method_csv` |
| `VOLCA_BENCH_METHOD_SIMAPRO_CSV` | one SimaPro CSV method file | `parser.method_simapro_csv` |
| `VOLCA_BENCH_METHOD_OLCA_JSON` | one openLCA-JSON impact-category file | `parser.method_olca_json` |

## Running

```bash
export VOLCA_BENCH_AGRIBALYSE=/path/to/AGB32_final.CSV
export VOLCA_BENCH_ECOINVENT="/path/to/ecoinvent_3.11_cutoff_ecoSpold02.7z.d"
export VOLCA_BENCH_BAFU="/path/to/BAFU/LCI ecoSpold v1 Files"
# ... etc
cabal bench volca-bench --benchmark-options="--output bench-results.json"
```

The synthetic LCIA benches (`lcia.synthetic.*`) need no fixture and always
run.

The output path can also be set via `VOLCA_BENCH_OUTPUT`; default is
`bench-results.json` in the current directory.

## Where to get the fixtures

- **Agribalyse 3.2** — public, [agribalyse.ademe.fr](https://agribalyse.ademe.fr).
- **BAFU 2025** — public,
  [bafu.admin.ch](https://www.bafu.admin.ch/bafu/en/home/topics/economy-consumption/info-specialists/life-cycle-inventories.html).
- **Ecoinvent 3.11 cutoff (EcoSpold 2)** — licensed,
  [ecoinvent.org](https://ecoinvent.org). Required for `parser.ecospold2`.
- **EF / PEF method packs** — licensed/public depending on version;
  source: EU JRC.

## Output schema

`bench-results.json` is versioned via `schema_version`. Each entry carries
the human strings (`label`, `description`) plus the structured metric
(`unit_of_work.{kind,n}`, `mean`, `stddev`, `derived.items_per_second`).
The reader doesn't need to know LCA to interpret a row.

```json
{
  "schema_version": 1,
  "metadata": {
    "git_sha": "…",
    "ghc": "9.6.7",
    "timestamp_iso": "2026-05-15T14:32:00Z",
    "hardware": { "cpu": "…", "cores": 24, "ram_gb": 90.0, "os": "Linux" }
  },
  "results": [
    {
      "capability": "parser.ecospold2",
      "label": "Parse 1000 EcoSpold 2 process files (XML)",
      "description": "Reads and deserialises 1000 EcoSpold 2 process files …",
      "unit_of_work": { "kind": "ecospold2_files", "n": 1000 },
      "metric": "seconds",
      "fixture": { "source": "ecoinvent", "slice": "first 1000 by name" },
      "mean": 4.231,
      "stddev": 0.118,
      "samples": 5,
      "derived": { "items_per_second": 236.4 }
    }
  ]
}
```

`mean` and `stddev` are always in **seconds**. The `metric` field is a
display hint for the consumer (use `"milliseconds"` for the sub-second
benches if you want a friendlier rendering).
