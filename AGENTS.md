# AGENTS.md

Guidance for AI agents working in the VoLCA engine repository.

## What this is

VoLCA is an in-memory Life Cycle Assessment (LCA) engine in Haskell (GHC 9.12.4,
package `volca`, Apache-2.0). It loads EcoSpold 1/2, SimaPro CSV, ILCD, and Excel
databases, builds supply-chain dependency trees, computes life-cycle inventories (LCI)
via sparse-matrix algebra, and applies characterization methods for impact assessment.
Multiple databases load simultaneously with cross-database flow linking and what-if
substitutions. It exposes a Servant REST API and an MCP server, plus a CLI/REPL.

See `README.md` for the full feature spec.

## Glossary

- **LCI** — life-cycle inventory: the raw physical flows (emissions, resources) for a product.
- **LCIA** — impact assessment: LCI flows × characterization factors → impact scores.
- **CF** — characterization factor: one flow's contribution to one impact category.
- **Activity / Exchange / Flow** — a process / one input-output line / the substance or product exchanged.

## Commands

```bash
./gen-version.sh        # Generate src/Version.hs (git metadata) — REQUIRED before a native build
./build.sh              # Build the engine
./build.sh --test       # Build + run the test suite
./build.sh --coverage   # Build + tests + HTML coverage report
cabal test lca-tests --test-show-details=streaming        # Run tests directly
cabal test lca-tests --test-options="--match /Inventory/" # Run a single spec group
```

`src/Version.hs` is generated, not committed — a native `cabal build`/`cabal test`
fails without running `./gen-version.sh` first. Tests use hspec + hspec-discover;
spec modules are `*Spec.hs` under `test/` and auto-discovered. New behavior needs a
`*Spec.hs`; integration fixtures (sample databases, golden values) live in `test-data/`.

## Module spine

| Area | Modules | Role |
|------|---------|------|
| Domain | `Types` | Central domain types (Activity, Exchange, Flow, …) |
| Solve | `Matrix`, `SharedSolver` | Sparse technosphere/biosphere matrices; MUMPS-backed LCI solve |
| Parsers | `EcoSpold.*`, `SimaPro.*`, `ILCD.*`, `BrightwayExcel.*`, `Method.Parser*` | Database + method format readers |
| Methods | `Method.Mapping`, `Method.FlowResolver`, `Method.ChemSynonyms` | CF mapping (UUID→CAS→name→synonym cascade), flow resolution |
| Database | `Database.*` (`Loader`, `CrossLinking`, `MatrixBuild`, `Manager`) | Load, cross-link, build matrices, manage lifecycle |
| Analysis | `Service`, `Service.Aggregate`, `Tree` | Analysis ops, grouping, supply-chain traversal |
| Search | `Search.BM25`, `Search.Fuzzy`, `Search.Normalize` | Activity/flow search |
| API | `API.Routes`, `API.MCP` | Servant REST routes and MCP tool surface (shared resource registry) |
| CLI | `CLI.*`, `app/Main.hs` | Arg parsing, commands, HTTP client, REPL; entrypoint (server/client/repl modes) |

A Python client lives in `pyvolca/` (own `pyproject.toml`); the MUMPS binding in
`mumps-hs/`. The core is otherwise pure Haskell.

## Where to start

- **A new MCP tool or REST endpoint** → `API.Routes` holds the shared resource registry that drives *both* REST and MCP; add it once there, not in two places.
- **A new database format** → a new parser namespace (e.g. `Foo.Parser`), wired into `Database.Loader`.
- **Wrong or empty LCI numbers** → `Database.MatrixBuild` plus `Matrix` / `SharedSolver`; check supplier resolution in `Database.CrossLinking`.
- **Characterization mismatches** → `Method.Mapping` (the UUID→CAS→name→synonym cascade) and `Method.FlowResolver`.
- **Search relevance** → `Search.BM25` / `Search.Fuzzy` / `Search.Normalize`.

## Engineering rules

### Code safety
- **Make impossible states impossible** — use the type system.
- **Name domain values with their own type, not a bare primitive.** When a `Text`/`String`/`Int`/`Double` carries domain meaning that crosses more than one signature — database names, flow and activity UUIDs, units, amounts — wrap it in a `newtype` (with a smart constructor where validation applies). This is why `RootDb`/`ThisDb` are newtypes over `Text` and not raw `Text`, so two database names can't be silently swapped. Two guard-rails: don't wrap values that never leave a single expression (boilerplate ≠ safety), and when a comment is *enumerating* the valid strings, reach for a **sum type**, not a newtype.
- **No wildcard patterns on sum types** — exhaustive matches only. They hide incomplete matches from the compiler.
- **No runtime crashes**: never use `error`, `throw`, `undefined`, or partial functions (`head`, `fromJust`). Functions that can fail must return `Either Text a` or `Maybe a`, never crash.
- **No silent errors or silent misbehaviour**: never return zero, empty, or a fallback value when a lookup fails, a unit conversion can't be done, or data is missing. Surface it: `Either Text a` propagated to a 4xx/5xx, a `reportProgress Warning` log line, or a `toolError` in MCP. A score that silently undercounts is worse than a clear failure — the consumer can't tell something is wrong.
- Builds are `-Wall`-clean — introduce no new warnings (incomplete patterns, unused binds). This is what backs the exhaustive-match rule above.

### Code style
- Prefer short point-free style over explicit case pattern matching.
- Don't overload functions or write diagonal code (deeply nested, staircase-shaped logic). Extract, share and reuse relevant abstractions.
- **Pure domain, effectful edges**.
- Avoid long function signatures. They are a smell — split the function or gather arguments in a product type.
- Each type should have a sensible domain or technical meaning.
- Use advanced Haskell patterns when they improve expressivity and reduce line count.

### Design philosophy
- Think like "Out Of The Tar Pit": most complexity is accidental. Minimize mutable state, keep logic declarative, separate state / control / computation.
- Simplicity: perfection = nothing left to remove. Avoid over-engineering and cognitive load.
- **Pre-1.0 (`v0.y.z`): no backward-compatibility obligation yet** — keep wire formats and APIs clean rather than carrying cruft. Reassess at `v1.0.0`.
- Use language servers for fast diagnostics: HLS for Haskell, pyright for the `pyvolca/` Python client.

### Open-source boundary
- This engine stands alone — keep deployment/SaaS concerns and any customer- or product-specific names out of code, comments, and PRs.
- Name things after the standard or format, not the vendor or tool that produces them.

### Formatting
- Haskell is formatted with `fourmolu` — version pinned in `versions.env` (`FOURMOLU_VERSION`), style in `fourmolu.yaml`. You don't run it by hand:
  - CI fails any PR with unformatted Haskell.
  - A repo pre-commit hook auto-formats staged `.hs` — enable once per clone: `git config core.hooksPath .githooks`.
  - Claude Code also auto-formats `.hs` on edit (`.claude/settings.json`).

### Commits & PRs
- NEVER use `git add -A` — always add specific files explicitly.
- **Keep commit messages tight**: explain *why* the change was made and any non-obvious technical choices; don't restate the diff. Subject line + a few short paragraphs max.
- **Atomic commits — one subject per commit.** If the message needs "and also", split it.
- One PR = one subject. The PR description explains the why and the final state, not every commit.
