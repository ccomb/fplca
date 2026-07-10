# pyvolca

Python client for [VoLCA](https://github.com/ccomb/volca) — Life Cycle Assessment engine over Agribalyse and ecoinvent.

> **Full guide and tutorials**: <https://volca.run/docs/python/>
> **Issues / source**: <https://github.com/ccomb/volca>

## Install

```bash
pip install pyvolca
```

Requires Python ≥ 3.10 and a running VoLCA engine. Use `Server` (below) to run one as a child process, or point `Client` at any reachable instance.

## Compatibility

pyvolca speaks one revision of the engine's JSON wire format; the engine advertises its revision as `wireVersion` on `/api/v1/version`. pyvolca checks it the first time it talks to the engine — too old fails with a clear error, newer than this client knows warns. pyvolca and engine version numbers move independently: `wireVersion` carries compatibility, not the version numbers.

| pyvolca | wire | compatible engine |
|---------|------|-------------------|
| `0.5.x` | (pre-`wireVersion`) | `v0.5.0` … `v0.7.x` |
| `0.6.x` | `1` | `≥ v0.8.0` |

<!-- BEGIN: compatibility -->

_Generated from `volca._compat` — run `python scripts/gen_api_md.py` to regenerate._

This build of **pyvolca 0.7.2** speaks wire format **1** and requires a VoLCA engine **≥ v0.8.0**.

<!-- END: compatibility -->

## First choose: connect to an existing server, or start one locally

`pyvolca` is only the Python client library. It does not contain the VoLCA databases and it does not install the VoLCA engine binary.

Most users should start with one of these two modes:

- **You already have access to a VoLCA server** (for example a hosted server prepared by someone else): use `Client` only. You do not need `volca.toml`, and you do not need to install the VoLCA server locally.
- **You want Python to start a local VoLCA engine process for you**: use `download()` once to fetch the VoLCA engine binary and reference data into the shared volca install dir (see [Where artefacts are installed](#where-artefacts-are-installed)), then use `Server` to start it from Python. `volca.toml` is still a normal file path passed to `Server(config=...)`; put it in your project directory, or pass an absolute path. Do not put it inside your virtualenv or inside `site-packages`.

For a hosted server, the minimal connection looks like this:

```python
# no-test  — replace with your real hosted VoLCA server URL and credentials.
from volca import Client

c = Client(
    base_url="https://your-volca-server.example.com",
    db="agribalyse-3.2",
    password="your-api-token-or-password",
)

print(c.list_databases())
```

Use `download()` + `Server` only when you deliberately want to download and launch the engine from Python:

```python
# no-test  — downloads the engine and needs a real engine config/database.
from volca import Client, Server, download

installed = download()  # cached after the first run

with Server(config="./volca.toml", binary=str(installed.binary)) as srv:
    c = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
    print(c.list_databases())
```

In this local mode, `download()` stores the engine binary and reference data in the shared volca install dir (see below). `Server(config="./volca.toml")` still means “read `./volca.toml` relative to the current working directory”.

### Where artefacts are installed

`download()` writes to the same OS-native location as the `install.sh` / `install.ps1` shell installers, so any of the three tools populate the same directory:

| Platform | Default install root |
|---|---|
| Linux   | `${XDG_DATA_HOME:-~/.local/share}/volca/` |
| macOS   | `~/Library/Application Support/volca/` |
| Windows | `%LOCALAPPDATA%\volca\` |

Override with `VOLCA_HOME=/full/path` (full path; skips OS detection).

If you ran `install.sh` or `install.ps1` first, `Server()` finds the installed engine without an extra `download()` call. If you previously used `pyvolca < 0.4` it cached artefacts under `<user_cache_dir>/pyvolca/` (Linux: `~/.cache/pyvolca/`); that directory is no longer read and can be removed (`rm -rf ~/.cache/pyvolca`).

## Local managed-server quick start

```python
# no-test  — needs a real engine; the snippets below run against a mocked Client.
from volca import Client, Server

with Server(config="volca.toml") as srv:
    c = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
    plants = c.search_activities(name="wheat flour, at plant", limit=5)
    chain = c.get_supply_chain(plants[0].process_id, name="at farm")
    score = c.get_impacts(plants[0].process_id, method_id=c.list_methods()[0].id)
```

This example starts a local engine process from Python. `Server` reads `port` and `password` from the TOML config. The engine self-stops after `idle_timeout` seconds without traffic (default 5 min).

> Examples below assume `c` is a `Client` instance — construct it with the snippet above, or against an already-running server: `c = Client(base_url="http://localhost:8080", db="agribalyse-3.2", password="…")`.

## Discover what's available

> *Which databases are loaded? Which LCIA methods can I score against? What classification systems can I filter on?*

```python
for db in c.list_databases():
    print(f"  {db.name} [{db.status}]: {db.activity_count} activities")

for m in c.list_methods()[:5]:
    print(f"  {m.id}  {m.name} [{m.unit}]")
```

Other listings: `c.list_classifications()` returns the classification systems and their values for the current database; `c.list_presets()` returns named filter presets configured in the engine. Use `c.load_database(name)` / `c.unload_database(name)` to manage memory if a database isn't auto-loaded.

## Find an activity

> *Which activity in the database represents the product I want to assess?*

```python
plants = c.search_activities(name="wheat flour, at plant", page_size=5)
print(f"{len(plants)} matches; showing page 1 ({plants.page_size} items)")
for a in plants:
    print(f"{a.process_id}  {a.activity_name} → {a.product_name} ({a.location})")
```

`search_activities` returns a `SearchResults[Activity]` — a paginated wire envelope. Iterate it to walk every match across all pages (subsequent pages fetched on demand, then cached so re-iteration is free); `len(results)` is the server-reported total. Use `results.page(n, page_size=M)` for explicit page access, or pass `page=N` + `page_size=M` to jump straight to a page (both are required together — `page=` alone is rejected since the offset can't be derived without committing to a page size). Each `Activity` is a process — an `(activity, product)` pair — carrying `process_id`, `activity_name`, `location`, `product_name`, `product_amount`, `product_unit`. A process has no name of its own; compose a label from `activity_name` + `product_name`. Narrow the query with `geo="FR"`, `classification=`/`classification_value=` (ISIC/CPC), or set `exact=True` for an exact-name match. To search by flow name (technosphere products and biosphere flows) instead of activity name, use `c.search_flows(query=...)`.

## Inspect an activity

> *What goes into making this product? What does it emit? What's its reference unit?*

```python
detail = c.get_activity(plants[0].process_id)
for ex in detail.technosphere_inputs:
    print(f"{ex.amount:.4g} {ex.unit} of {ex.flow_name} ← {ex.target_activity_name}")
```

`get_activity` returns a typed `ActivityDetail`. Use `.inputs` / `.outputs` / `.technosphere_inputs` to filter the exchanges; each entry is an `Exchange` — either a `TechnosphereExchange` (an input or output of an intermediate product) or a `BiosphereExchange` (resource extracted or pollutant emitted).

## Trace the upstream supply chain

> *What's the full upstream chain — every ingredient, recursively, down to the farm or mine?*

```python
chain = c.get_supply_chain(plants[0].process_id, name="at farm", limit=20)
print(f"{chain.filtered_activities} of {chain.total_activities} upstream activities match 'at farm'")
for entry in chain.entries[:5]:
    print(f"  {entry.quantity:.4g} {entry.unit} of {entry.activity_name} ({entry.location})")
```

For *"how exactly does this root reach a specific upstream supplier?"*, use `get_path_to(process_id, target=...)` — returns a `PathResult` of ordered `PathStep`s root → target with cumulative quantities and step ratios.

## Find downstream consumers

> *Where is this supplier used? Which products depend on it?*

```python
result = c.get_consumers(plants[0].process_id, max_depth=2, page_size=10)
for cons in result.consumers:
    print(f"  depth={cons.depth}  {cons.activity_name} ({cons.location})")
```

Returns a `ConsumersResponse` whose `consumers` field is a `SearchResults[ConsumerResult]` — same paginated iterator semantics as `search_activities`. When `include_edges=True`, `result.edges` carries the technosphere edges so callers can reconstruct supplier→consumer paths without a second round trip. Pass `classification_filters=[...]` to restrict to a category.

## Compute the life-cycle inventory

> *What are the cumulative biosphere flows (CO₂, water, methane, …) per functional unit, before applying any characterization method?*

```python
inv = c.get_inventory(plants[0].process_id, limit=20)
for f in inv.flows[:5]:
    print(f"  {f.quantity:.4g} {f.unit_name}  {f.flow_name}")
print(f"  {inv.statistics.emission_quantity:.4g} emissions / "
      f"{inv.statistics.resource_quantity:.4g} resources")
# Substitutions are accepted: c.get_inventory(pid, substitutions=[...])
```

`InventoryResult` carries the typed `flows` list (one `InventoryFlow` per row) plus a `statistics` roll-up with per-direction totals and `top_categories`. The inventory is what every LCIA method runs on top of. If you only need *grouped* views (by name, location, classification, etc.), reach for `c.aggregate(scope="biosphere", group_by=...)` instead — same data, summarized.

## Compute environmental impacts (LCIA)

> *What's the carbon footprint of this product? Which emissions dominate the score?*

```python
score = c.get_impacts(plants[0].process_id, method_id="EF3.1-climate-change", top_flows=5)
print(f"{score.score:.4g} {score.unit}")
for c_flow in score.top_contributors:
    print(f"  {c_flow.share_pct:.1f}%  {c_flow.flow_name}")
```

`LCIAResult` carries the score, unit, optional `normalized_score` / `weighted_score` (in Pt), and the top contributing biosphere flows with their `share_pct`.

> *Compute every impact category in one go — climate, water, land use, …*

```python
batch = c.get_impacts_batch(plants[0].process_id)
for r in batch.results:
    print(f"  {r.category}: {r.score:.4g} {r.unit}")
if batch.single_score is not None:
    print(f"PEF single score: {batch.single_score:.4g} {batch.single_score_unit}")
```

`LCIABatchResult` also surfaces formula-based scoring sets (PEF, ECS…) via `scoring_results` and `scoring_indicators`, so you can render a per-indicator chart alongside the aggregate single score.

## Drill into what drives a single impact

> *I have a climate-change score. Which biosphere flows account for it? Which upstream activities?*

`get_impacts(...).top_contributors` already returns the top biosphere flows for a single LCIA call. For a deeper or differently-bounded view — and for the *activity* attribution view — use the standalone drill-down endpoints:

```python
flows = c.get_contributing_flows(
    plants[0].process_id,
    method_id="EF3.1-climate-change",
    limit=10,
)
for f in flows.top_flows:
    print(f"  {f.share_pct:.1f}%  {f.flow_name}")

acts = c.get_contributing_activities(
    plants[0].process_id,
    method_id="EF3.1-climate-change",
    limit=10,
)
for a in acts.activities:
    print(f"  {a.share_pct:.1f}%  {a.activity_name} ({a.location})")
```

`ContributingFlows.top_flows` and `ContributingActivities.activities` are typed lists; both carriers also expose `method`, `unit`, and `total_score`. Note: the engine doesn't report a total count for these endpoints, so neither result derives a `has_more` flag — pass a generous `limit` and inspect the `share_pct` totals if you need exhaustive coverage.

> *Which characterization factors does a method apply, and to which database flows?*

```python
char = c.get_characterization(method_id="EF3.1-climate-change", limit=20)
```

Useful for sanity-checking method coverage or building custom indicators on top of the engine's mapping.

## Aggregate flows by group

> *What are the top emitting substances? How do flows break down by category, location, or classification?*

```python
agg = c.aggregate(
    plants[0].process_id,
    scope="biosphere",
    group_by="name",
    aggregate="sum_quantity",
)
for g in agg.groups[:5]:
    print(f"  {g.quantity:.4g} {g.unit or ''} of {g.key}")
```

`scope` selects what to aggregate over: `"direct"` (just this activity's exchanges), `"supply_chain"` (cumulative upstream), or `"biosphere"` (all elementary flows). `group_by` accepts `"name"`, `"flow_id"`, `"unit"`, `"location"`, `"target_name"`, or `"classification.<system>"`.

## Compare two activities

> *How does variant A differ from variant B? Which inputs change?*

```python
from volca import compare_activities

diff = compare_activities(c, plants[0].process_id, plants[1].process_id, scope="direct")
print(f"  matched: {len(diff.matched)}, only-left: {len(diff.left_only)}, only-right: {len(diff.right_only)}")
for row in diff.matched[:3]:
    print(f"    {row.key}: {row.left:.4g} → {row.right:.4g}  (Δ={row.delta:+.4g})")
```

A client-side merge over two `aggregate` calls. Groups by `flow_id` (default) so matching is stable across naming variants. Pass `scope="supply_chain"` to compare cumulative inputs instead of direct exchanges.

## Run counterfactuals (substitutions)

> *What if I used organic wheat instead of conventional? Recycled aluminium instead of virgin? — without reloading the database.*

The engine applies a Sherman–Morrison rank-1 update, so substitutions are fast regardless of database size. Works on `get_supply_chain`, `get_inventory`, and `get_impacts`.

```python
subs = [{
    "from": "old-supplier-pid",      # the activity to replace
    "to":   "new-supplier-pid",      # the replacement
    "consumer": "consumer-pid",      # the activity that directly uses the old supplier
}]
score = c.get_impacts(plants[0].process_id, method_id="EF3.1-climate-change", substitutions=subs)
```

Multiple substitutions chain in one call — the `consumer` field disambiguates *where* in the chain each swap applies.

## Handle errors

> *The activity doesn't exist, the engine is down, or the request is malformed — what do I catch?*

```python
from volca import VoLCAError

try:
    score = c.get_impacts("nonexistent-pid", method_id="EF3.1-climate-change")
except VoLCAError as e:
    print(f"  failed: {e.status_code} — {e.body[:80]}")
```

`VoLCAError.status_code` is the HTTP status when the engine returned one; `body` is the raw response body.

## Switch databases

> *I want to run the same workflow against ecoinvent instead of Agribalyse — without rebuilding the client.*

```python
ei = c.use("ecoinvent-3.10")
ei_results = ei.search_activities(name="electricity, high voltage")
```

`Client.use(db_name)` returns a new `Client` targeting a different database while sharing the HTTP session and dispatch table — no spec re-fetch.

## Refresh IDE autocomplete after upgrading the engine

> *I just upgraded the VoLCA server. How do I get my editor to see the new endpoints?*

```python
c.refresh_stubs()
```

Pyvolca dispatches dynamically against the engine's OpenAPI spec, so it ships without `.pyi` stubs. `refresh_stubs()` refetches the spec and writes stubs into the installed package directory; restart your language server to pick them up.

## API reference

<!-- BEGIN: api-reference -->

_This reference is generated from the installed package. Run `python scripts/gen_api_md.py` to regenerate._

## Classes

### `AggregateOp`

How values are reduced within a bucket.

``SUM_QUANTITY`` — sum of quantities (default). ``COUNT`` — number of
matching entries. ``SHARE`` — each bucket's percentage of the filtered
total (0..100).

### `AggregateScope`

What the ``/aggregate`` primitive groups over.

``DIRECT`` — direct exchanges of the activity. ``SUPPLY_CHAIN`` — the
upstream activities reachable via cumulative flow. ``BIOSPHERE`` — only
biosphere flows in the supply chain.

### `BioDirection`

Direction of a biosphere exchange.

``RESOURCE`` — extraction from the environment (input).
``EMISSION`` — release to the environment (output).

### `Client`

HTTP client for the VoLCA HTTP API.

Usage::

    c = Client(db="agribalyse-3.2", password="1234")
    plants = c.search_activities(name="at plant")
    chain = c.get_supply_chain(plants[0].process_id, name="at farm")

Substitutions can be passed to ``get_supply_chain``, ``get_inventory``,
and ``get_impacts`` to compute results with a different upstream
supplier — fast::

    subs = [{"from": old_pid, "to": new_pid, "consumer": consumer_pid}]
    result = c.get_impacts(pid, method_id=mid, substitutions=subs)

**Constructor**: `Client(base_url: str = 'http://localhost:8080', db: str = '', password: str = '')`

#### Methods

##### `Client.add_dependency(dep_name: str, db_name: str | None = None) -> dict`

Declare ``dep_name`` as a dependency of the target database.

Returns the engine's ``DatabaseSetupInfo`` dict describing the updated
dependency topology.

##### `Client.aggregate(process_id: str, scope: AggregateScope | str, *, is_input: bool | None = None, max_depth: int | None = None, filter_name: str | None = None, filter_name_not: list[str] | str | None = None, filter_unit: str | None = None, preset: str | None = None, filter_classification: list[ClassificationFilter] | None = None, filter_target_name: str | None = None, filter_is_reference: bool | None = None, group_by: str | None = None, aggregate: AggregateOp | str | None = None) -> AggregateResult`

SQL-group-by aggregation over direct exchanges, supply chain, or biosphere flows.

Args:
    scope: :class:`AggregateScope` member (``DIRECT`` / ``SUPPLY_CHAIN``
        / ``BIOSPHERE``) or the equivalent wire string. Strings are
        accepted for one-liner ergonomics but bypass static checking.
    group_by: omit for a single-bucket result (just the totals).
        Supported keys: ``"name"``, ``"flow_id"``, ``"name_prefix"``,
        ``"unit"``, ``"location"``, ``"target_name"``,
        ``"classification.<system>"``.
    aggregate: :class:`AggregateOp` member or wire string
        (``"sum_quantity"`` — default, ``"count"``, or ``"share"``).

##### `Client.call(operation_id: str, **kwargs) -> Any`

Escape hatch: call any OpenAPI operation by operationId.

Returns the raw JSON (no dataclass wrapping). Use this for
operations that don't have an ergonomic wrapper yet, or for new
endpoints added after the installed pyvolca was released.

##### `Client.copy_database(new_name: str, db_name: str | None = None) -> dict`

Copy a loaded database in memory under a new name.

``new_name`` is a path segment; the source defaults to ``self.db``.
Returns the engine's ``ActivateResponse`` dict
(``{"success", "message", "database"?}``). Raises VoLCAError if the
engine reports ``success=false``.

##### `Client.delete_activities(*, name: str = '', location: str = '', product: str = '', classifications: list[dict | tuple] | None = None, exact: bool = False, keep: list[str] | None = None, extra: list[str] | None = None, db_name: str | None = None) -> dict`

Delete activities selected by filter, sparing/adding explicit ids.

Builds a ``DeleteSelectionRequest``: the filter fields select the whole
matching set, ``keep`` spares matched process ids, and ``extra`` adds
ones the filter missed. ``classifications`` is a list of
``{"system", "value", "exact"}`` dicts or ``(system, value, exact)``
tuples.

Returns the ``DeleteSelectionResponse`` dict
(``{"success", "message", "deleted"}``); raises VoLCAError on
``success=false``.

##### `Client.export_database(fmt: str, db_name: str | None = None) -> bytes`

Export a loaded database, returning the serialized bytes.

``fmt`` is one of ``simapro|ecospold1|ecospold2|ilcd|brightway`` —
validated client-side; an unknown value raises VoLCAError before any
request. Single-file formats carry their bytes directly; EcoSpold 2 /
ILCD multi-file trees come back zipped.

The engine streams the payload as raw bytes. Best-effort approximation
warnings arrive in the ``X-Volca-Export-Warnings`` response header
(percent-encoded, newline-joined) and are surfaced through
:mod:`warnings`. Raises VoLCAError on an HTTP error.

##### `Client.export_to_file(fmt: str, out_path: str, db_name: str | None = None) -> None`

Export a database (see :meth:`export_database`) and write it to a file.

##### `Client.get_activity(process_id: str) -> ActivityDetail`

Fetch an activity's full detail.

Returns a typed ActivityDetail. Use ``act.inputs`` / ``act.outputs`` /
``act.technosphere_inputs`` to filter exchanges instead of walking
``act.exchanges`` directly.

##### `Client.get_characterization(method_id: str, *, flow: str | None = None, limit: int | None = None) -> CharacterizationResult`

Look up characterization factors for a method matched to database flows.

Returns a :class:`CharacterizationResult` carrying ``matches`` (total
rows the filter selected) and ``shown`` (rows actually returned under
``limit``). Check ``result.has_more`` to detect truncation.

##### `Client.get_consumers(process_id: str, *, name: str | None = None, location: str | None = None, product: str | None = None, preset: str | None = None, classification_filters: list[ClassificationFilter] | None = None, page: int | None = None, page_size: int | None = None, limit: int | None = None, offset: int | None = None, max_depth: int | None = None, include_edges: bool = False) -> ConsumersResponse`

Find all activities that transitively consume this supplier.

Args:
    max_depth: Max hops from supplier. 1 = direct consumers only.
    classification_filters: ClassificationFilter entries restricting
        the results. Multiple filters are AND-combined by the server.
        Mode is :class:`MatchMode.EXACT` or :class:`MatchMode.CONTAINS`.
    include_edges: When True, the response carries every technosphere
        edge whose endpoints are both reachable from the supplier.
        Callers can walk these to reconstruct supplier→consumer paths
        without a second ``get_path_to`` round-trip.

Returns a :class:`ConsumersResponse` whose ``consumers`` attribute is
a :class:`SearchResults[ConsumerResult]` (iterate it to walk every
consumer across all pages) and whose ``edges`` attribute carries
the traversal subgraph (empty by default).

##### `Client.get_contributing_activities(process_id: str, method_id: str, *, collection: str = 'methods', limit: int | None = None) -> ContributingActivities`

Which upstream activities drive a given impact category.

Same engine-side limitation as :meth:`get_contributing_flows`: no
total exposed, so ``has_more`` cannot be derived. Inspect
``share_pct`` totals to gauge coverage.

##### `Client.get_contributing_flows(process_id: str, method_id: str, *, collection: str = 'methods', limit: int | None = None) -> ContributingFlows`

Which elementary flows drive a given impact category.

Returns a :class:`ContributingFlows`. Caveat: the engine does not
report the total flow count, so pyvolca cannot derive ``has_more``
from the response. Pass a generous ``limit`` if you need exhaustive
coverage and inspect ``share_pct`` totals.

##### `Client.get_flow_mapping(method_id: str) -> FlowMapping`

Get the characterization-factor-to-database-flow mapping coverage.

:class:`FlowMapping.coverage_pct` summarises how many of the DB's
biosphere flows the method has a CF for; ``flows`` is the per-flow
breakdown including unmatched rows (``cf_value=None``).

##### `Client.get_impacts(process_id: str, method_id: str, *, collection: str = 'methods', top_flows: int | None = None, substitutions: list[SubstitutionLike] | None = None) -> LCIAResult`

Compute the LCIA score for a single impact category on an activity.

Use :meth:`get_impacts_batch` to retrieve every category in a method
collection at once (and any configured scoring sets).

Args:
    collection: Method collection name. Defaults to ``"methods"`` for
        single-method calls; most engines expose methods under a
        single collection.
    top_flows: Max top contributing flows to return (default 5).

##### `Client.get_impacts_batch(process_id: str, *, collection: str = 'methods', substitutions: list[SubstitutionLike] | None = None) -> LCIABatchResult`

Compute LCIA for every impact category in a collection, in one call.

The response carries the per-method :class:`LCIAResult` list plus any
formula-based scoring sets declared in the engine config (PEF, ECS…).
``scoring_indicators`` gives the per-variable breakdown of each
scoring set, pre-multiplied by the set's ``displayMultiplier``.

Uses a direct HTTP call: the batch endpoint has no operationId in the
OpenAPI spec (the dispatcher primary is the single-method variant), so
this wrapper bypasses ``_call`` and builds the URL itself.

##### `Client.get_inputs(process_id: str) -> list[Exchange]`

Return the input exchanges of an activity (richer metadata than ``get_activity``).

Uses a direct HTTP call because ``/inputs`` has no operationId
(it's a non-Resources auxiliary endpoint).

##### `Client.get_inventory(process_id: str, *, flow: str | None = None, limit: int | None = None, substitutions: list[SubstitutionLike] | None = None) -> InventoryResult`

Compute the life-cycle inventory (cumulative biosphere flows) for an activity.

Returns an :class:`InventoryResult` with the per-elementary-flow
totals scaled to one functional unit of the activity's reference
product. Use :meth:`get_impacts` to apply a characterization method
to the inventory; use :meth:`aggregate` with ``scope="biosphere"``
for grouped views.

Args:
    flow: Substring filter on flow name.
    limit: Cap on returned flow rows. (Server returns full inventory
        otherwise — the engine doesn't paginate this endpoint.)
    substitutions: Upstream supplier swaps; see :meth:`get_supply_chain`.

##### `Client.get_outputs(process_id: str) -> list[Exchange]`

Return the output exchanges of an activity. See :meth:`get_inputs` for notes.

##### `Client.get_path_to(process_id: str, target: str) -> PathResult`

Find the shortest upstream path from process to first activity whose name matches target.

Returns a PathResult whose path is ordered root → target. Each step
includes cumulative_quantity, scaling_factor, and (except the root)
local_step_ratio.

##### `Client.get_supply_chain(process_id: str, *, name: str | None = None, location: str | None = None, limit: int | None = None, min_quantity: float | None = None, max_depth: int | None = None, preset: str | None = None, classification_filters: list[ClassificationFilter] | None = None, substitutions: list[SubstitutionLike] | None = None, include_edges: bool | None = None) -> SupplyChain`

Get the flat supply chain of an activity.

Returns a :class:`SupplyChain`. Check ``result.has_more`` to detect
when ``limit`` truncated ``entries`` below ``filtered_activities`` —
further downstream analysis on a truncated chain would be wrong
without flagging the gap.

Args:
    max_depth: Max hops from root. 1 = direct inputs only.
    classification_filters: Restrict entries to those matching any
        of the given ClassificationFilter triples. Multiple filters
        are AND-combined by the server.
    substitutions: When provided, the call is upgraded to POST and
        the scaling vector is recomputed with the substituted
        suppliers. Accepts :class:`Substitution` (preferred) or the
        legacy ``{"from", "to", "consumer"}`` dict form; ``consumer``
        is optional — omit it for a global swap.

##### `Client.get_tree(process_id: str) -> dict`

Fetch the recursive activity tree used by the analysis SPA.

``/tree`` has no operationId in the OpenAPI spec — it's kept for the
SPA's lazy-expanding graph widget and intentionally not exposed as
a Resource. Included here as a direct HTTP call for scripts that
need the same shape.

##### `Client.get_version()`

Return server build metadata: version, git hash/tag, build target.

Uses a direct HTTP call — ``/api/v1/version`` has no operationId
since it predates the Resources ADT.

##### `Client.list_classifications()`

List classification systems and their values for the current database.

``ClassificationSystem.activity_count`` tells how widely each system
is populated — useful for picking a filter dimension with enough
signal.

##### `Client.list_databases()`

List every database declared in the engine config.

The typed entries carry ``depends_on``, so callers can derive
cross-DB dependency sets from declared topology rather than
hardcoding allowlists.

##### `Client.list_methods()`

List every LCIA method available in the engine.

Each :class:`Method` carries ``id``, ``name``, ``category``, ``unit``,
``factor_count``, and the parent ``collection``. Pass ``m.id`` to
:meth:`get_impacts` as ``method_id``.

##### `Client.list_presets()`

List classification presets configured in this instance.

Each :class:`Preset` carries its ``filters`` (list of
:class:`PresetFilter` triples). Apply by passing ``preset=p.name``
to filtering endpoints.

##### `Client.load_database(db_name: str) -> dict`

Load a database into memory so it answers queries.

Declared dependencies are loaded first; has no effect if the
database is already loaded.

##### `Client.refresh_stubs()`

Fetch the OpenAPI spec from the server and refresh the dispatch table.

Also regenerates the `.pyi` type stubs in the installed pyvolca
package directory so IDE autocomplete reflects the current engine.
Useful when the engine is upgraded without reinstalling pyvolca.

This is the explicit "the engine was upgraded" path — the likeliest
place to meet a wire mismatch — so it runs the same one-shot gate as
:meth:`_load_operations`, refusing a spec pyvolca can't decode.

##### `Client.relink(dep_db: str, mapping_csv: str, db_name: str | None = None) -> dict`

Re-link a database against a dependency using a name→name alias CSV.

``mapping_csv`` is the CSV *text* (header row + source/target columns),
sent inline so the engine needs no filesystem access. Returns the
``RelinkResponse`` dict (``{"dbName", "unresolvedBefore",
"unresolvedAfter", "crossDBLinks", "dependsOn"}``).

##### `Client.relink_from_file(dep_db: str, mapping_path: str, db_name: str | None = None) -> dict`

Read a mapping CSV file and call :meth:`relink` with its text.

##### `Client.remove_dependency(dep_name: str, db_name: str | None = None) -> dict`

Remove ``dep_name`` from the target database's dependencies.

Returns the updated ``DatabaseSetupInfo`` dict.

##### `Client.search_activities(name: str | None = None, *, geo: str | None = None, product: str | None = None, preset: str | None = None, classification: str | None = None, classification_value: str | None = None, classification_match: MatchModeLike | None = None, page: int | None = None, page_size: int | None = None, limit: int | None = None, offset: int | None = None, exact: bool = False) -> SearchResults[Activity]`

Search activities in the current database.

All filters are AND-combined and case-insensitive. ``name`` and
``product`` match by substring unless ``exact=True``.

Returns a paginated :class:`SearchResults` — iterate it to walk
every match across all pages (subsequent pages fetched on demand),
or use ``.page(n)`` for explicit page access. ``len(results)`` is
the server-reported total across all pages.

Args:
    name: Substring (or exact match) on activity name.
    geo: Geography code (``"FR"``, ``"GLO"``, ``"RoW"``…).
    product: Substring on the reference product name.
    preset: Apply a named classification preset configured in the engine.
    classification: System name (``"ISIC rev.4 ecoinvent"``).
    classification_value: Substring within that system's value.
    classification_match: How ``classification_value`` is compared —
        :class:`MatchMode.CONTAINS` (default, substring) or
        :class:`MatchMode.EXACT` (case-insensitive equality). Ignored
        when ``classification`` is unset.
    page: 1-based page number. Must be paired with ``page_size`` —
        offset cannot be derived from page alone.
    page_size: Items per page (becomes the wire-level ``limit``).
        Alone (no ``page``) means "page 1 with this size".
    limit: Wire-level cap on returned items. Prefer ``page_size``.
    offset: Wire-level starting index. Prefer ``page`` + ``page_size``.
    exact: When True, ``name`` and ``product`` are matched exactly.

Returns:
    :class:`SearchResults[Activity]` — iterable across all pages.

##### `Client.search_flows(query: str | None = None, *, page: int | None = None, page_size: int | None = None, limit: int | None = None, offset: int | None = None) -> SearchResults[Flow]`

Search flows (technosphere products and biosphere flows) in the current database.

Returns a paginated :class:`SearchResults[Flow]` — iterate to walk
every match across all pages, or use ``.page(n)`` for explicit
access. See :meth:`search_activities` for the pagination contract.

Args:
    query: Substring matched case-insensitively against flow names.
    page / page_size: Web-style pagination; convert to wire-level
        ``offset`` / ``limit``.
    limit / offset: Wire-level escape hatch.

##### `Client.unload_database(db_name: str) -> dict`

Unload a database from memory to free RAM. The disk copy is kept.

Refused if another loaded database still depends on it.

##### `Client.use(db_name: str) -> 'Client'`

Return a new client targeting a different database.

Shares the underlying HTTP session, dispatch table, and any other
Client-level state with the original — only ``db`` is overridden.
New fields added to :meth:`Client.__init__` propagate automatically
(no manual mirror to keep in sync).

### `DatabaseStatus`

Lifecycle state of a database in the engine.

``UNLOADED`` — declared in the engine config but not yet loaded.
``PARTIALLY_LINKED`` — loaded, but some cross-DB flow references could
not be resolved against currently-loaded dependencies.
``LOADED`` — loaded and fully linked.

Inherits from :class:`str`, so ``dataclasses.asdict(db)["status"]``
serialises as the bare wire string.

### `MatchMode`

How a :class:`ClassificationFilter` value is compared against the entry.

``EXACT`` — case-insensitive equality. ``CONTAINS`` — case-insensitive
substring. Inherits from :class:`str` so ``json.dumps(MatchMode.EXACT)``
and ``dataclasses.asdict(filter)["mode"]`` both serialise as the bare
string ``"exact"`` / ``"contains"``.

### `Server`

Manages the VoLCA server process.

Usage::

    with Server(config="volca.toml") as srv:
        client = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
        activities = client.search_activities(name="at plant")

**Constructor**: `Server(config: str = 'volca.toml', port: int = 0, binary: str = 'volca')`

#### Properties

##### `base_url`

``http://localhost:<port>`` — pass to :class:`Client(base_url=…)`.

Always loopback: the managed server only listens locally.

#### Methods

##### `Server.is_alive()`

Health check — GET /api/v1/db, return True if 200.

##### `Server.start(idle_timeout: int = 300, wait_timeout: int = 120) -> None`

Spawn the engine process if it is not already serving, and wait until ready.

Args:
    idle_timeout: Seconds without an HTTP request before the engine
        shuts itself down. Default 5 min.
    wait_timeout: How long to poll for the server to become healthy
        before raising :class:`TimeoutError`.

No-op if a healthy server is already reachable on ``base_url``.

##### `Server.stop()`

Stop the server via shutdown endpoint, then terminate process.

### `TechRole`

Role a technosphere exchange plays within its host activity.

``REFERENCE_PRODUCT`` — the activity's reference output product.
``COPRODUCT`` — a secondary output (in allocated activities).
``REFERENCE_INPUT`` — the reference input (in waste-treatment activities).
``INPUT`` — any other technosphere input.

## Exceptions

### `DownloadError`

Raised when the download or verification fails.

### `VoLCAError`

Error from the VoLCA API.

**Constructor**: `VoLCAError(message: str, status_code: int | None = None, body: str = '')`

## Data types

### `Activity`

One activity in a database — the row returned by /activities search.

``process_id`` is the engine's canonical address (``activityUUID_productUUID``)
and is what you pass to every detail endpoint (:meth:`Client.get_activity`,
:meth:`Client.get_supply_chain`, :meth:`Client.get_impacts`, …).
``activity_name`` is the activity name (e.g. ``"wheat flour, at plant"``);
``product_name`` is the reference output product (e.g. ``"wheat flour"``);
``product_amount`` and ``product_unit`` describe the functional unit
(typically ``1.0`` of ``"kg"`` / ``"MJ"`` / etc.). ``location`` is the
geography code (``"FR"``, ``"GLO"``, ``"RoW"``…). A process has no name of
its own — compose a label from ``activity_name`` + ``product_name``.

``allocation_percent`` is this product's share (0..100) of the parent
activity's exchanges in a multi-output (allocated) process — e.g. a
cheese activity that also yields whey, cream and permeate gives each
product its own share, summing to ~100. It is ``None`` for single-output
processes. ``allocation_formula`` carries the raw symbolic formula when
the source expressed the share as an expression rather than a number,
else ``None``.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `activity_name` | `str` | — |
| `location` | `str` | — |
| `product_name` | `str` | — |
| `product_amount` | `float` | — |
| `product_unit` | `str` | — |
| `allocation_percent` | `float \| None` | None |
| `allocation_formula` | `str \| None` | None |

### `ActivityContribution`

One upstream activity's contribution to an LCIA score.

Returned in :class:`ContributingActivities.activities`. ``share_pct`` is
the percentage of the total impact this activity contributes (0..100).

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `activity_name` | `str` | — |
| `product_name` | `str` | — |
| `location` | `str` | — |
| `contribution` | `float` | — |
| `share_pct` | `float` | — |

### `ActivityDetail`

Typed wrapper around the JSON returned by GET /activity/{pid}.

Use the .inputs / .outputs / .technosphere_inputs convenience properties
instead of walking the raw exchanges list.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `activity_name` | `str` | — |
| `location` | `str` | — |
| `unit` | `str` | — |
| `description` | `list[str]` | — |
| `classifications` | `dict[str, str]` | — |
| `product_name` | `str \| None` | — |
| `product_amount` | `float \| None` | — |
| `product_unit` | `str \| None` | — |
| `all_products` | `list[Activity]` | — |
| `exchanges` | `list[Union[TechnosphereExchange, BiosphereExchange, WasteExchange]]` | — |

#### Properties

##### `allocation_percent`

This process's own allocation share (0..100), or ``None``.

A multi-output process splits the parent activity's burden across its
co-products; every :attr:`all_products` entry carries its share. This
returns the share of *this* process — the entry whose ``process_id``
matches — and ``None`` for single-output processes.

##### `inputs`

Every input exchange — technosphere inputs and biosphere resources.

Equivalent to filtering :attr:`exchanges` by ``e.is_input``. Mixed
kinds: callers needing only one variant should use
:attr:`technosphere_inputs` or filter manually.

##### `is_allocated`

True iff the activity splits its burden across several co-products.

Reads the structured ``allocation_percent`` the engine sets on each
:attr:`all_products` entry (authoritative), not the description text.

##### `outputs`

Every output exchange — products and biosphere emissions.

Includes the reference product, coproducts (in allocated
activities), and all biosphere emissions.

##### `technosphere_inputs`

Only the technosphere inputs (ingredients from other activities).

Excludes biosphere inputs (resource extractions) and waste
outputs. The common case when answering "what does this activity
consume from upstream?".

### `ActivityDiff`

Result of ``compare_activities``.

| Field | Type | Default |
|-------|------|---------|
| `scope` | `str` | — |
| `group_by` | `str` | — |
| `matched` | `list[ActivityDiffRow]` | list() |
| `left_only` | `list[ActivityDiffRow]` | list() |
| `right_only` | `list[ActivityDiffRow]` | list() |

### `ActivityDiffRow`

One matched or unmatched flow in an activity comparison.

| Field | Type | Default |
|-------|------|---------|
| `key` | `str` | — |
| `left` | `float \| None` | — |
| `right` | `float \| None` | — |
| `unit` | `str \| None` | — |

#### Properties

##### `delta`

right - left (0 if one side is missing).

### `AggregateGroup`

One bucket inside an AggregateResult.

| Field | Type | Default |
|-------|------|---------|
| `key` | `str` | — |
| `quantity` | `float` | — |
| `count` | `int` | — |
| `unit` | `str \| None` | None |
| `share` | `float \| None` | None |

### `AggregateResult`

Result of a Client.aggregate() call.

``filtered_total`` is the sum across all items matching the filters (the
top-level number). ``groups`` is the per-bucket breakdown when ``group_by``
was set; empty otherwise.

| Field | Type | Default |
|-------|------|---------|
| `scope` | `AggregateScope` | — |
| `filtered_total` | `float` | — |
| `filtered_unit` | `str \| None` | — |
| `filtered_count` | `int` | — |
| `groups` | `list[AggregateGroup]` | list() |

### `BiosphereExchange`

An exchange with the environment (resource extraction or emission).

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | — |
| `compartment` | `Compartment \| None` | — |
| `amount` | `float` | — |
| `unit` | `str` | — |
| `direction` | `BioDirection` | — |
| `comment` | `str \| None` | None |
| `is_biosphere` | `bool` | True |
| `is_waste` | `bool` | False |

#### Properties

##### `is_input`

True for resource extractions (``direction`` is ``RESOURCE``).

Biosphere inputs are resource extractions; outputs are emissions
to the environment.

##### `is_reference`

Always False — biosphere exchanges cannot be reference flows.

The reference flow defines the functional unit and is always a
technosphere product (see :class:`TechnosphereExchange.is_reference`).

### `CharacterizationFactor`

One characterization factor matched against a database biosphere flow.

Returned in the ``factors`` list of :class:`CharacterizationResult`.
``match_strategy`` records how the CF was matched to the DB flow
(``"uuid"``, ``"cas"``, ``"name"``, ``"synonym"``, ``"fuzzy"``).

| Field | Type | Default |
|-------|------|---------|
| `method_flow_name` | `str` | — |
| `cf_value` | `float` | — |
| `cf_unit` | `str` | — |
| `direction` | `str` | — |
| `db_flow_name` | `str` | — |
| `flow_id` | `str` | — |
| `flow_unit` | `str` | — |
| `category` | `str` | — |
| `match_strategy` | `str` | — |
| `compartment` | `str \| None` | None |

### `CharacterizationResult`

Result of :meth:`Client.get_characterization`.

The engine truncates ``factors`` to ``shown`` rows (server-side ``limit``).
``matches`` is the unfiltered total: use :attr:`has_more` to detect when
the slice is incomplete.

| Field | Type | Default |
|-------|------|---------|
| `method` | `str` | — |
| `unit` | `str` | — |
| `matches` | `int` | — |
| `shown` | `int` | — |
| `factors` | `list[CharacterizationFactor]` | list() |

#### Properties

##### `has_more`

True when the server truncated below ``matches``.

### `ClassificationFilter`

Filter a supply-chain/consumers query by a classification (system, value, mode).

Matches one classification system entry, e.g.
``ClassificationFilter("Category", "Agricultural\\Food", "exact")`` or
``ClassificationFilter("Category", "Agricultural\\Food", MatchMode.EXACT)``.
Multiple filters are AND-combined by the server.

| Field | Type | Default |
|-------|------|---------|
| `system` | `str` | — |
| `value` | `str` | — |
| `mode` | `MatchMode` | <MatchMode.CONTAINS: 'contains'> |

### `ClassificationSystem`

One classification system declared by a database.

``values`` are the distinct entries in this system; ``activity_count`` is
how many activities carry at least one classification under this system
(helps callers pick a worthwhile filter dimension).

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | — |
| `values` | `list[str]` | list() |
| `activity_count` | `int` | 0 |

### `Compartment`

Biosphere compartment (medium + optional subcompartment).

Frozen so it's hashable and immutable — callers can use it as a dict key
when grouping flows by compartment, and accidental mutation is rejected.

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | — |
| `sub` | `str \| None` | None |

### `ConsumerResult`

Activity that consumes a given supplier, with BFS depth.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `activity_name` | `str` | — |
| `location` | `str` | — |
| `product_name` | `str` | — |
| `product_amount` | `float` | — |
| `product_unit` | `str` | — |
| `depth` | `int` | — |
| `classifications` | `dict[str, str]` | dict() |

### `ConsumersResponse`

Reverse supply chain (/consumers) — paginated consumer list plus
optional edge set. Mirrors :class:`SupplyChain` so callers have a
uniform {entries, edges} shape in both traversal directions.

``consumers`` is a :class:`SearchResults[ConsumerResult]` — iterate it
to walk every consumer across all pages. ``edges`` is populated only
when ``include_edges=True``.

| Field | Type | Default |
|-------|------|---------|
| `consumers` | `SearchResults[ConsumerResult]` | — |
| `edges` | `list[SupplyChainEdge]` | list() |

### `ContributingActivities`

Top upstream activities driving an LCIA score.

Same engine-side limitation as :class:`ContributingFlows`: the server
reports no total, so pyvolca cannot derive ``has_more``. Pass a generous
``limit`` and inspect ``share_pct`` if exhaustive coverage matters.

| Field | Type | Default |
|-------|------|---------|
| `method` | `str` | — |
| `unit` | `str` | — |
| `total_score` | `float` | — |
| `activities` | `list[ActivityContribution]` | list() |

### `ContributingFlows`

Top elementary flows driving an LCIA score.

Note: the engine does not report a total — ``top_flows`` is whatever the
server returned under ``limit``, but pyvolca cannot tell whether more
flows were truncated. If you need exhaustive coverage, pass a generous
``limit`` and inspect ``share_pct`` totals.

| Field | Type | Default |
|-------|------|---------|
| `method` | `str` | — |
| `unit` | `str` | — |
| `total_score` | `float` | — |
| `top_flows` | `list[FlowContribution]` | list() |

### `DatabaseInfo`

One entry of :meth:`Client.list_databases`.

``depends_on`` names the databases this one links against for cross-DB
flow resolution — mirrors the ``dependsOn`` list surfaced by the relink
endpoint. Derived from the engine's declared topology, not runtime state.

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | — |
| `display_name` | `str` | — |
| `status` | `DatabaseStatus` | — |
| `path` | `str` | — |
| `load_at_startup` | `bool` | False |
| `is_uploaded` | `bool` | False |
| `activity_count` | `int` | 0 |
| `description` | `str \| None` | None |
| `format` | `str \| None` | None |
| `depends_on` | `list[str]` | list() |

### `Flow`

A technosphere product or biosphere flow as returned by /flows.

Mirrors the server's :code:`FlowSearchResult`. ``synonyms`` maps
language code → list of synonym strings (empty when the database
carries no synonym index).

| Field | Type | Default |
|-------|------|---------|
| `id` | `str` | — |
| `name` | `str` | — |
| `category` | `str` | — |
| `unit_name` | `str` | — |
| `synonyms` | `dict[str, list[str]]` | dict() |

### `FlowContribution`

Top contributing elementary flow for an impact category.

Emitted inside ``LCIAResult.top_contributors``.

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | — |
| `contribution` | `float` | — |
| `share_pct` | `float` | — |
| `flow_id` | `str` | — |
| `category` | `str` | — |
| `cf_value` | `float` | 0.0 |
| `compartment` | `str \| None` | None |

### `FlowMapping`

CF-coverage report for one method against the current database.

``matched_flows / total_flows`` is the coverage ratio: how many of the
database's biosphere flows have a CF in this method. Mirrors the engine
response of :meth:`Client.get_flow_mapping`.

| Field | Type | Default |
|-------|------|---------|
| `method_name` | `str` | — |
| `method_unit` | `str` | — |
| `total_flows` | `int` | — |
| `matched_flows` | `int` | — |
| `flows` | `list[FlowMappingEntry]` | list() |

#### Properties

##### `coverage_pct`

Matched fraction expressed as 0..100. Returns 0 when total is 0.

### `FlowMappingEntry`

One DB biosphere flow and the CF (if any) assigned to it.

``cf_value`` is ``None`` when this DB flow has no characterization factor
in the method — that flow contributes 0 to the score for the method.
``match_strategy`` records how the mapping was resolved (``"uuid"``,
``"cas"``, ``"name"``, ``"synonym"``, ``"fuzzy"``).

| Field | Type | Default |
|-------|------|---------|
| `flow_id` | `str` | — |
| `flow_name` | `str` | — |
| `flow_category` | `str` | — |
| `cf_value` | `float \| None` | None |
| `cf_flow_name` | `str \| None` | None |
| `match_strategy` | `str \| None` | None |

### `Installed`

Result of :func:`download`.

| Field | Type | Default |
|-------|------|---------|
| `binary` | `Path` | — |
| `data_dir` | `Path` | — |
| `version` | `str` | — |
| `data_version` | `str` | — |

### `InventoryFlow`

One row of an inventory: a biosphere flow scaled to the functional unit.

``is_emission`` distinguishes outputs (releases) from inputs (resource
extraction). ``flow_id`` is the database UUID; ``compartment`` is the
medium label (e.g. ``"air/urban air"``) when the source dataset declared
one. ``category`` is the engine-normalised category used for grouping.

| Field | Type | Default |
|-------|------|---------|
| `flow_id` | `str` | — |
| `flow_name` | `str` | — |
| `quantity` | `float` | — |
| `unit_name` | `str` | — |
| `is_emission` | `bool` | — |
| `category` | `str` | — |
| `compartment` | `str \| None` | None |

### `InventoryResult`

Life-cycle inventory of an activity: cumulative biosphere flows.

Returned by :meth:`Client.get_inventory`. The engine does not paginate —
``flows`` is the full inventory (filtered by ``flow=`` substring when
requested). ``statistics`` carries the per-direction roll-ups and the
most-populated categories.

``root`` is the activity the inventory was computed for. ``total_flows``,
``emission_flows``, ``resource_flows`` mirror the engine's metadata block.

| Field | Type | Default |
|-------|------|---------|
| `root` | `Activity` | — |
| `total_flows` | `int` | — |
| `emission_flows` | `int` | — |
| `resource_flows` | `int` | — |
| `flows` | `list[InventoryFlow]` | — |
| `statistics` | `InventoryStatistics` | — |

### `InventoryStatistics`

Roll-up totals of an inventory result.

``emission_quantity`` and ``resource_quantity`` are sums by direction;
``total_quantity`` is the sum of absolute values. ``top_categories``
lists ``(category_name, flow_count)`` pairs ordered by frequency.

| Field | Type | Default |
|-------|------|---------|
| `total_quantity` | `float` | — |
| `emission_quantity` | `float` | — |
| `resource_quantity` | `float` | — |
| `top_categories` | `list[tuple[str, int]]` | list() |

### `LCIABatchResult`

Batch LCIA: every impact category in a method collection, for one activity.

Returned by :meth:`Client.get_impacts_batch`. Carries the per-method
impact results plus any formula-based scoring sets configured in the
engine TOML (PEF, ECS, or any named set).

``scoring_indicators`` gives the per-variable normalized-weighted
breakdown of each scoring set — already multiplied by the set's
``displayMultiplier`` and expressed in its display unit (see
:class:`ScoringIndicator`). Lets callers render per-indicator charts
alongside the aggregate ``scoring_results``.

| Field | Type | Default |
|-------|------|---------|
| `results` | `list[LCIAResult]` | — |
| `single_score` | `float \| None` | None |
| `single_score_unit` | `str \| None` | None |
| `norm_weight_set_name` | `str \| None` | None |
| `available_nw_sets` | `list[str]` | list() |
| `scoring_results` | `dict[str, dict[str, float]]` | dict() |
| `scoring_units` | `dict[str, str]` | dict() |
| `scoring_indicators` | `dict[str, dict[str, ScoringIndicator]]` | dict() |

### `LCIAResult`

LCIA score for one impact category on one activity.

Returned directly by :meth:`Client.get_impacts`, and nested inside
:class:`LCIABatchResult.results` (one entry per impact category).

| Field | Type | Default |
|-------|------|---------|
| `method_id` | `str` | — |
| `method_name` | `str` | — |
| `category` | `str` | — |
| `damage_category` | `str` | — |
| `score` | `float` | — |
| `unit` | `str` | — |
| `mapped_flows` | `int` | — |
| `functional_unit` | `str` | — |
| `normalized_score` | `float \| None` | None |
| `weighted_score` | `float \| None` | None |
| `top_contributors` | `list[FlowContribution]` | list() |

### `Method`

One LCIA method, returned by :meth:`Client.list_methods`.

Pass ``id`` to :meth:`Client.get_impacts` as ``method_id``. ``collection``
is the parent method collection (e.g. ``"ef-31"``), forwarded to
:meth:`Client.get_impacts` / :meth:`Client.get_impacts_batch` as their
``collection`` argument.

| Field | Type | Default |
|-------|------|---------|
| `id` | `str` | — |
| `name` | `str` | — |
| `category` | `str` | — |
| `unit` | `str` | — |
| `factor_count` | `int` | — |
| `collection` | `str` | — |

### `PathResult`

Shortest upstream path from a root process to a matching activity.

| Field | Type | Default |
|-------|------|---------|
| `path` | `list[PathStep]` | — |
| `path_length` | `int` | — |
| `total_ratio` | `float` | — |

### `PathStep`

One step in the supply chain path returned by get_path_to.

Note: the /path endpoint is hand-built (aeson `object [...]`) but now
emits camelCase keys (``processId``, ``activityName``,
``cumulativeQuantity``, …) like the rest of the API.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `activity_name` | `str` | — |
| `location` | `str` | — |
| `unit` | `str` | — |
| `cumulative_quantity` | `float` | — |
| `scaling_factor` | `float` | — |
| `local_step_ratio` | `float \| None` | None |

### `Preset`

A named classification preset declared in the engine config.

Apply by passing ``preset=preset.name`` to filtering endpoints (the engine
expands it server-side into the ``filters`` triples).

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | — |
| `label` | `str` | — |
| `description` | `str \| None` | — |
| `filters` | `list[PresetFilter]` | list() |

### `PresetFilter`

One filter triple inside a :class:`Preset`.

| Field | Type | Default |
|-------|------|---------|
| `system` | `str` | — |
| `value` | `str` | — |
| `mode` | `MatchMode` | <MatchMode.CONTAINS: 'contains'> |

### `ScoringIndicator`

One per-variable entry inside ``LCIABatchResult.scoring_indicators``.

``value`` is pre-multiplied by the scoring set's ``displayMultiplier``
(configured in the scoring TOML) and expressed in the set's display unit.
``category`` is the indicator's display name: the scoring set's
``labels`` entry when one is configured (typically for computed
variables), otherwise the impact category the variable was resolved
from, or as a last resort the raw variable key.

| Field | Type | Default |
|-------|------|---------|
| `category` | `str` | — |
| `value` | `float` | — |

### `SearchResults`

Paginated wire envelope, mirrors Haskell ``SearchResults a``.

Carries one page of results plus pagination metadata. Iterating walks
every page lazily, fetching subsequent pages on demand via the
``_fetch`` callback. ``len()`` returns ``total`` — the server-reported
count across *all* pages, not just the items currently held.

Wire fields (``results``, ``total``, ``offset``, ``limit``, ``has_more``,
``search_time_ms``) mirror the server type exactly. Page-style helpers
(``page_size``, ``page(n)``) are client conveniences computed from them.

Pages fetched during iteration are cached on the instance — re-iterating
replays the cache without hitting the server. Wrap in ``list(...)`` to
materialise eagerly if you prefer.

| Field | Type | Default |
|-------|------|---------|
| `results` | `list[~T]` | — |
| `total` | `int` | — |
| `offset` | `int` | — |
| `limit` | `int` | — |
| `has_more` | `bool` | — |
| `search_time_ms` | `float` | — |
| `_fetch` | `Optional[Callable[[int, int \| None], dict]]` | None |
| `_parse` | `Optional[Callable[[dict], ~T]]` | None |
| `_fetched` | `list[~T]` | list() |
| `_exhausted` | `bool` | False |

#### Properties

##### `page_size`

Server-applied limit (page size for further fetches).

### `ServerVersion`

Server build metadata returned by :meth:`Client.get_version`.

``git_tag`` is None for untagged dev builds. ``build_target`` names the
platform triple the binary was compiled for (e.g. ``"x86_64-linux"``).
``wire_version`` is the engine's advertised JSON wire-format revision, or
None for engines that predate it (everything up to v0.7.x).

| Field | Type | Default |
|-------|------|---------|
| `version` | `str` | — |
| `git_hash` | `str` | — |
| `git_tag` | `str \| None` | — |
| `build_target` | `str` | — |
| `wire_version` | `int \| None` | None |

### `Substitution`

Replace one supplier with another in the upstream supply chain.

All fields are process_ids. ``consumer`` identifies which downstream
consumer's input to rewrite, scoping the swap to one edge — the same
upstream supplier can be replaced by different alternatives in different
parts of the tree. Omit it (leave ``None``) to apply the swap globally,
replacing the supplier on every consumer at once.

Frozen so callers can put it in a set / dict key and re-use the same
substitution across multiple calls without aliasing risk.

| Field | Type | Default |
|-------|------|---------|
| `from_pid` | `str` | — |
| `to_pid` | `str` | — |
| `consumer` | `str \| None` | None |

### `SupplyChain`

Flat supply chain of an activity.

``total_activities`` is the unfiltered upstream count; ``filtered_activities``
is what remains after the server applies ``classification_filters`` /
``min_quantity`` / ``preset``. ``entries`` is the slice the server actually
returned — it may be shorter than ``filtered_activities`` when ``limit``
truncates. Use :attr:`has_more` to detect that case rather than comparing
lengths by hand.

| Field | Type | Default |
|-------|------|---------|
| `root` | `Activity` | — |
| `total_activities` | `int` | — |
| `filtered_activities` | `int` | — |
| `entries` | `list[SupplyChainEntry]` | list() |
| `edges` | `list[SupplyChainEdge]` | list() |

#### Properties

##### `has_more`

True when the server truncated ``entries`` below ``filtered_activities``.

Surfacing this lets callers detect silent truncation: if you passed
``limit=100`` and ``filtered_activities`` is 500, downstream LCA work
would be wrong without flagging the gap.

### `SupplyChainEdge`

A consumer→supplier link in the supply chain.

``from``/``to`` are Python keywords, so the process ids are stored under
``from_id``/``to_id``. ``from_db``/``to_db`` carry each endpoint's database
name, which is required to route edges across databases (the same process
id can exist in more than one loaded DB).

| Field | Type | Default |
|-------|------|---------|
| `from_id` | `str` | — |
| `from_db` | `str` | — |
| `to_id` | `str` | — |
| `to_db` | `str` | — |
| `amount` | `float` | — |

### `SupplyChainEntry`

One activity in a :class:`SupplyChain.entries` list.

``quantity`` is the cumulative amount of this activity's reference
product consumed per functional unit of the root activity, in ``unit``.
``scaling_factor`` is the multiplier the solver applied to this
activity to produce ``quantity`` — i.e. ``quantity = ref_output * scaling_factor``.
``classifications`` mirrors the producing activity's classifications
(ISIC, CPC, Category, …) so callers can filter by taxonomy without a
second :meth:`Client.get_activity` round trip.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `activity_name` | `str` | — |
| `location` | `str` | — |
| `quantity` | `float` | — |
| `unit` | `str` | — |
| `scaling_factor` | `float` | — |
| `classifications` | `dict[str, str]` | dict() |

### `TechnosphereExchange`

An exchange with another activity. Carries no compartment — the
producing activity's classifications describe the product taxonomy.

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | — |
| `amount` | `float` | — |
| `unit` | `str` | — |
| `role` | `TechRole` | — |
| `target_activity_name` | `str \| None` | — |
| `target_location` | `str \| None` | — |
| `target_process_id` | `str \| None` | — |
| `comment` | `str \| None` | None |
| `is_biosphere` | `bool` | False |
| `is_waste` | `bool` | False |

#### Properties

##### `is_input`

True for technosphere inputs (``role`` is ``INPUT`` or ``REFERENCE_INPUT``).

Lets callers split exchanges into inputs vs. outputs without
knowing the four-role taxonomy.

##### `is_reference`

True for reference roles (``REFERENCE_PRODUCT`` / ``REFERENCE_INPUT``).

The reference exchange is the one that defines the activity's
functional unit — the basis the LCA result is normalised to.

### `WasteExchange`

An exchange of a waste flow with a treatment activity.

Shares the technosphere matrix with product flows but tracked as its own
kind so callers can tell a "waste sent to landfill" output apart from a
product input. Orphan waste (no linked treatment) contributes zero impact
— same cut-off semantics as an orphan technosphere input.

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | — |
| `amount` | `float` | — |
| `unit` | `str` | — |
| `is_input` | `bool` | — |
| `target_activity_name` | `str \| None` | — |
| `target_location` | `str \| None` | — |
| `target_process_id` | `str \| None` | — |
| `comment` | `str \| None` | None |
| `is_biosphere` | `bool` | False |
| `is_waste` | `bool` | True |

#### Properties

##### `is_reference`

Always False — waste flows never define an activity's functional unit.

Treatment activities have a ``ReferenceInput`` instead, exposed
via :class:`TechnosphereExchange`.

## Functions

### `compare_activities(client: Client, pid_left: str, pid_right: str, *, scope: str = 'direct', group_by: str = 'flow_id', is_input: bool | None = True, **aggregate_kwargs) -> ActivityDiff`

Diff two activities by flow_id (default) at the requested scope.

Returns three lists:
- ``matched``: flows present in both activities (with left, right, delta).
- ``left_only``: flows present only in the left activity.
- ``right_only``: flows present only in the right activity.

Default ``is_input=True`` restricts the comparison to inputs, which is the
common case for "what does this variant consume differently?". Pass
``is_input=None`` to include outputs as well.

### `download(version: Optional[str] = None, repo: str = 'ccomb/volca', *, force: bool = False) -> Installed`

Download the volca binary + data bundle for the current platform.

Idempotent: if both artefacts are already extracted under the install
root and ``force=False``, returns immediately without network.

Args:
    version: GH Release tag (``v0.7.0``); ``None`` resolves the latest.
    repo: GitHub repo slug. Default ``ccomb/volca``.
    force: Re-download even if the install root looks complete.

Returns:
    :class:`Installed` with the resolved paths and versions.

## Type aliases

### `Exchange`

Type alias: `Union[TechnosphereExchange, BiosphereExchange, WasteExchange]`.

<!-- END: api-reference -->

## See also

- Full guide and tutorials: <https://volca.run/docs/python/>
- VoLCA engine: <https://github.com/ccomb/volca>
- Examples folder: [`examples/`](examples/)

## License

Apache-2.0
