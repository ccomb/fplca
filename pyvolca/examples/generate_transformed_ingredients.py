"""Generate transformed ingredient variants by substituting raw ingredient activities.

Strategy
--------
Ecobalyse tracks raw ingredients in three variants per ingredient family:
  - FR  (alias suffix -fr):      French production, Agribalyse reference activity
  - OI  (alias suffix -default): Import origin, may use Ecoinvent or other DB
  - BIO (alias suffix -organic): Organic, Agribalyse or Ginko organic activity

Agribalyse also contains many *transformed* products (frozen vegetables, purees,
juices, at-plant products ...) that use one of these raw ingredient activities
somewhere in their upstream supply chain — either directly or via an intermediate
market/consumption-mix activity.

The goal is to generate new transformed ingredient variants:
for every transformed product T that uses raw variant V_src, we create a new
version of T that uses each other raw variant V_tgt instead of V_src.

Algorithm:
1. Parse activities.json to build ingredient groups: for each alias prefix
   (e.g. "radish") collect all known variants (radish-fr, radish-organic,
   radish-default) with their upstream LCA activityName and database.
2. For each variant in each group, call VoLCA get_consumers with a
   classification filter restricted to transformed-food categories (from the
   "transformed" preset in volca.toml). This returns only genuine
   transformation activities, regardless of how many intermediate market or
   transport steps sit between the raw ingredient and the transformation.
3. Collect the union of all consumers across all variants in the group.
   For each consumer C, record which variant(s) it already uses as input.
4. For each consumer C and each variant V_tgt that C does NOT yet use:
   - Call get_path_to(C, V_src.activityName) to get the exact supply chain
     path from C to V_src (the variant C already uses).
   - Derive the from_existing block: existingActivity=path[0],
     upstreamPath=path[1:-1], replace.from=path[-1]=V_src, replace.to=V_tgt.
5. Also generate an activities.json entry for each new transformed activity,
   copying physical metadata (density, inediblePart, rawToCookedRatio ...)
   from the corresponding raw variant.

Outputs two files:
  generated_activities_to_create.json  — from_existing blocks to merge into activities_to_create.json
  generated_activities.json            — new entries to merge into activities.json

Usage:
    python generate_transformed_ingredients.py \\
        --activities /path/to/ecobalyse-data/activities.json \\
        --output-dir /path/to/output \\
        [--volca-url http://localhost:8080]
"""

import argparse
import json
import re
import sys
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from uuid import uuid4

from volca import Client
from volca.types import ConsumerResult, PathResult


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# VoLCA database name for the main Agribalyse database (where transformed products live)
AGRIBALYSE_DB = "agribalyse-3.2"

# Mapping from activities.json "source" field to VoLCA database name
DB_MAP: dict[str, str] = {
    "Agribalyse 3.2": "agribalyse-3.2",
    "Ecoinvent 3.9.1": "ecoinvent-3.9.1",
    "Ecoinvent 3.11": "ecoinvent-3.11",
    "Ginko 2025": "ginko",
    "WFLDB": "wfldb",
    "PastoEco": "pastoeco",
}

# Alias suffixes that identify raw ingredient variants (longest first to avoid partial matches)
VARIANT_SUFFIXES = ["-organic", "-default", "-fr"]

# Classification filters matching the "transformed" preset in volca.toml
# Sent as a single request with repeated query parameters (OR semantics server-side)
TRANSFORMED_FILTERS: list[tuple[str, str]] = [
    ("Category", "Agricultural\\Food"),
    ("Category", "Agricultural\\Plant oils\\Transformation"),
    ("Category", "Agricultural\\Plant production\\Cereals\\Transformation"),
    ("Category", "Agricultural\\Plant production\\Fruits\\Transformation"),
    ("Category", "Agricultural\\Plant production\\Legumes\\Transformation"),
    ("Category", "Agricultural\\Plant production\\Nuts\\Transformation"),
    ("Category", "Agricultural\\Plant production\\Oil seeds\\Transformation"),
    ("Category", "Agricultural\\Plant production\\Roots and tubers\\Transformation"),
    ("Category", "Agricultural\\Plant production\\Vegetables\\Transformation"),
]


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------

@dataclass
class VariantInfo:
    alias: str            # e.g. "carrot-organic"
    scenario: str         # "reference", "organic", "import"
    default_origin: str   # e.g. "France"
    activity_name: str    # top-level activityName (same for all metadata within one entry)
    source: str           # e.g. "Agribalyse 3.2", "Ecoinvent 3.9.1"
    location: str | None  # top-level location field (used when searching VoLCA)
    raw_meta: dict        # full metadata dict (for copying physical properties)
    process_id: str | None = None  # resolved after VoLCA lookup


# ---------------------------------------------------------------------------
# Alias / group helpers
# ---------------------------------------------------------------------------

def strip_variant_suffix(alias: str) -> str | None:
    """Return the base ingredient name by stripping a known variant suffix, or None."""
    for suffix in VARIANT_SUFFIXES:
        if alias.endswith(suffix):
            return alias[:-len(suffix)]
    return None


def slugify(text: str) -> str:
    """Convert an activity name to a simple hyphenated slug for use in aliases."""
    text = text.lower()
    text = re.sub(r"[^a-z0-9]+", "-", text)
    return text.strip("-")


# ---------------------------------------------------------------------------
# Step 1: Parse activities.json
# ---------------------------------------------------------------------------

def parse_ingredient_groups(activities_json: list[dict]) -> dict[str, list[VariantInfo]]:
    """Return {base_name: [VariantInfo, ...]} for all food ingredient variants."""
    variants: list[VariantInfo] = []

    for entry in activities_json:
        if "food" not in entry.get("scopes", []):
            continue
        if "ingredient" not in entry.get("categories", []):
            continue

        activity_name = entry.get("activityName", "")
        source = entry.get("source", "Agribalyse 3.2")
        location = entry.get("location")

        for meta in entry.get("metadata", []):
            alias = meta.get("alias", "")
            scenario = meta.get("scenario")
            if not scenario:
                continue
            if strip_variant_suffix(alias) is None:
                continue  # skip -2025 and other non-standard aliases
            if "food" not in meta.get("scopes", []):
                continue
            variants.append(VariantInfo(
                alias=alias,
                scenario=scenario,
                default_origin=meta.get("defaultOrigin", ""),
                activity_name=activity_name,
                source=source,
                location=location,
                raw_meta=meta,
            ))

    groups: dict[str, list[VariantInfo]] = defaultdict(list)
    for v in variants:
        base = strip_variant_suffix(v.alias)
        groups[base].append(v)  # type: ignore[index]

    return dict(groups)


def filter_actionable_groups(
    groups: dict[str, list[VariantInfo]]
) -> dict[str, list[VariantInfo]]:
    """Keep only groups that have at least one reference AND one non-reference variant
    with a DIFFERENT activityName (substitution is meaningful)."""
    result = {}
    for base, vs in groups.items():
        refs = [v for v in vs if v.scenario == "reference"]
        non_refs = [v for v in vs if v.scenario != "reference"]
        if not refs or not non_refs:
            continue
        ref_names = {v.activity_name for v in refs}
        if any(v.activity_name not in ref_names for v in non_refs):
            result[base] = vs
    return result


# ---------------------------------------------------------------------------
# Step 2: Resolve process_ids via VoLCA
# ---------------------------------------------------------------------------

def resolve_process_ids(
    groups: dict[str, list[VariantInfo]],
    client: Client,
) -> None:
    """Resolve process_id for each variant in-place. Unresolved variants get None."""
    cache: dict[tuple[str, str], str | None] = {}

    for variants in groups.values():
        for v in variants:
            key = (v.source, v.activity_name)
            if key in cache:
                v.process_id = cache[key]
                continue

            db_name = DB_MAP.get(v.source)
            if not db_name:
                print(f"  [WARN] Unknown source database '{v.source}' for alias {v.alias!r}",
                      file=sys.stderr)
                cache[key] = None
                v.process_id = None
                continue

            db_client = client.use(db_name)
            results = db_client.search_activities(
                name=v.activity_name,
                geo=v.location or "FR",
            )
            exact = [r for r in results if r.name == v.activity_name]
            pid = exact[0].process_id if exact else None
            if pid is None:
                print(f"  [WARN] Could not resolve '{v.activity_name}' in {db_name!r}",
                      file=sys.stderr)
            cache[key] = pid
            v.process_id = pid


# ---------------------------------------------------------------------------
# Step 3c: Find transformed consumers for each variant
# ---------------------------------------------------------------------------

def collect_consumers(
    variants: list[VariantInfo],
    client: Client,
    max_depth: int = 2,
) -> tuple[dict[str, set[str]], dict[str, ConsumerResult]]:
    """
    For each variant that lives in a database containing food-transformed products
    (Agribalyse or any database that has it as dependency), call get_consumers with
    the transformed-food classification filter.

    Databases that hold transformed food products: agribalyse-3.2 and those that
    depend on it (ginko, pastoeco, ecobalyse ...).  Pure Ecoinvent activities are
    unlikely to have Agribalyse-based food-transformed consumers, so we skip them.

    Returns:
      consumer_to_found_variants: {consumer_pid: {variant_alias, ...}}
      consumer_info: {consumer_pid: ConsumerResult}
    """
    # Databases where transformed food products live
    FOOD_TRANSFORM_DBS = {"agribalyse-3.2", "ginko", "pastoeco", "ecobalyse", "ecoplus"}

    consumer_to_found_variants: dict[str, set[str]] = defaultdict(set)
    consumer_info: dict[str, ConsumerResult] = {}

    for v in variants:
        if v.process_id is None:
            continue
        db_name = DB_MAP.get(v.source)
        if db_name not in FOOD_TRANSFORM_DBS:
            continue  # e.g. pure Ecoinvent variants have no Agribalyse consumers

        db_client = client.use(db_name)
        try:
            consumers = db_client.get_consumers(
                v.process_id,
                classification_filters=TRANSFORMED_FILTERS,
                max_depth=max_depth,
            )
        except Exception as exc:
            print(f"  [WARN] get_consumers failed for {v.alias!r} in {db_name!r}: {exc}",
                  file=sys.stderr)
            continue
        for c in consumers:
            if "2025" in c.name:
                continue  # skip -2025 organic variants; they are not stable Agribalyse activities
            consumer_to_found_variants[c.process_id].add(v.alias)
            consumer_info[c.process_id] = c

    return dict(consumer_to_found_variants), consumer_info


# ---------------------------------------------------------------------------
# Steps 3d-3f: Generate from_existing blocks and activities.json entries
# ---------------------------------------------------------------------------

def make_from_existing(
    path: PathResult,
    source: VariantInfo,
    target: VariantInfo,
    existing_aliases: set[str],
) -> dict | None:
    """
    Build a from_existing block for activities_to_create.json.
    path.path[0] = existingActivity (the transformed product)
    path.path[1:-1] = upstreamPath (intermediate steps)
    path.path[-1] = replace.from (the source raw ingredient activity)
    Returns None if the alias already exists.
    """
    steps = path.path
    # Derive alias: slug of transformed product name + target variant suffix
    variant_suffix = target.alias.split("-")[-1]  # "fr", "organic", or "default"
    base_slug = slugify(steps[0].name)[:60]  # cap length
    alias = f"{base_slug}-{variant_suffix}"

    if alias in existing_aliases:
        return None  # skip duplicates

    to_entry: dict = {"name": target.activity_name}
    if target.source != "Agribalyse 3.2":
        to_entry["database"] = target.source

    return {
        "activityCreationType": "from_existing",
        "alias": alias,
        "comment": "auto-generated",
        "database": "Agribalyse 3.2",
        "existingActivity": {"name": steps[0].name},
        "newName": f"{steps[0].name} [{target.alias}] {{{{{alias}}}}}",
        "replacementPlan": {
            "upstreamPath": [{"name": s.name} for s in steps[1:-1]],
            "replace": [{"from": {"name": steps[-1].name}, "to": to_entry}],
        },
    }


def make_activities_entry(fe_block: dict, target: VariantInfo) -> dict:
    """Build an activities.json entry for a new transformed ingredient variant."""
    meta = target.raw_meta
    alias = fe_block["alias"]
    # Display name: reuse raw variant's displayName logic or construct one
    display_name = meta.get("displayName", alias)

    return {
        "activityName": fe_block["newName"],
        "alias": alias,
        "categories": ["ingredient", "material"],
        "database": "Ecobalyse",
        "displayName": display_name,
        "metadata": [{
            "alias": alias,
            "scenario": target.scenario,
            "defaultOrigin": meta.get("defaultOrigin"),
            "id": str(uuid4()),
            "displayName": display_name,
            "cropGroup": meta.get("cropGroup"),
            "ingredientCategories": meta.get("ingredientCategories", []),
            "inediblePart": meta.get("inediblePart"),
            "ingredientDensity": meta.get("ingredientDensity"),
            "rawToCookedRatio": meta.get("rawToCookedRatio"),
            "scopes": ["food", "food2"],
            "transportCooling": meta.get("transportCooling"),
            "visible": True,
        }],
        "scopes": ["food", "food2"],
        "source": "Ecobalyse",
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument(
        "--activities",
        default="activities.json",
        help="Path to activities.json (default: activities.json)",
    )
    parser.add_argument(
        "--activities-to-create",
        default="activities_to_create.json",
        help="Path to activities_to_create.json (used to skip existing aliases)",
    )
    parser.add_argument(
        "--output-dir",
        default=".",
        help="Directory for output files (default: current directory)",
    )
    parser.add_argument(
        "--volca-url",
        default="http://localhost:8080",
        help="VoLCA API base URL (default: http://localhost:8080)",
    )
    parser.add_argument(
        "--max-depth",
        type=int,
        default=2,
        help="Max BFS depth for get_consumers (default: 2 = direct + one intermediate like market mix)",
    )
    args = parser.parse_args()

    # Load inputs
    activities_path = Path(args.activities)
    print(f"Loading {activities_path} ...")
    with activities_path.open() as f:
        activities_json: list[dict] = json.load(f)

    atc_path = Path(args.activities_to_create)
    existing_aliases: set[str] = set()
    if atc_path.exists():
        with atc_path.open() as f:
            for entry in json.load(f):
                if a := entry.get("alias"):
                    existing_aliases.add(a)

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # Connect to VoLCA
    client = Client(base_url=args.volca_url, db=AGRIBALYSE_DB)
    agribalyse_client = client.use(AGRIBALYSE_DB)  # used for get_path_to

    # Step 1: Parse and group
    print("Parsing ingredient groups ...")
    all_groups = parse_ingredient_groups(activities_json)
    groups = filter_actionable_groups(all_groups)
    print(f"  {len(all_groups)} total groups, {len(groups)} actionable (have substitutable variants)")

    # Step 2: Resolve process_ids
    print("Resolving process IDs via VoLCA ...")
    resolve_process_ids(groups, client)
    resolvable = sum(
        1 for vs in groups.values()
        if any(v.process_id is not None for v in vs)
    )
    print(f"  {resolvable}/{len(groups)} groups have at least one resolved variant")

    # Step 3c-f: Generate entries
    output_fe: list[dict] = []
    output_ae: list[dict] = []
    all_generated_aliases: set[str] = set(existing_aliases)

    for base, variants in groups.items():
        resolved = [v for v in variants if v.process_id is not None]
        if not resolved:
            continue

        # Collect consumers for all variants in one pass
        consumer_to_found, consumer_info = collect_consumers(resolved, client, max_depth=args.max_depth)

        # variant lookup by alias
        variant_by_alias = {v.alias: v for v in variants}

        for consumer_pid, found_aliases in consumer_to_found.items():
            missing = [v for v in variants if v.alias not in found_aliases
                       and v.process_id is not None]
            if not missing:
                continue

            # Pick one found variant as path-to source
            source_alias = next(iter(found_aliases))
            source = variant_by_alias.get(source_alias)
            if source is None or source.process_id is None:
                continue

            # get_path_to accepts only bare activityUUID; strip combined activityUUID_productUUID
            activity_pid = consumer_pid.split("_")[0]
            try:
                path = agribalyse_client.get_path_to(activity_pid, source.activity_name)
            except Exception as exc:
                print(f"  [WARN] get_path_to failed for consumer {consumer_info[consumer_pid].name!r}: {exc}",
                      file=sys.stderr)
                continue

            for target in missing:
                fe = make_from_existing(path, source, target, all_generated_aliases)
                if fe is None:
                    continue
                ae = make_activities_entry(fe, target)
                output_fe.append(fe)
                output_ae.append(ae)
                all_generated_aliases.add(fe["alias"])

    # Write outputs
    fe_path = output_dir / "generated_activities_to_create.json"
    ae_path = output_dir / "generated_activities.json"

    with fe_path.open("w") as f:
        json.dump(output_fe, f, indent=2, ensure_ascii=False)
    with ae_path.open("w") as f:
        json.dump(output_ae, f, indent=2, ensure_ascii=False)

    print(f"\nDone.")
    print(f"  {len(output_fe)} from_existing blocks → {fe_path}")
    print(f"  {len(output_ae)} activities.json entries → {ae_path}")


if __name__ == "__main__":
    main()
