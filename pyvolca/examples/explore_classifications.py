"""Explore classification values in Agribalyse.

Dumps all distinct classification keys/values to understand how to distinguish
raw ingredients (at farm) from processed ingredients (at plant).

Usage:
    python -m examples.explore_classifications
"""

import sys
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from volca import Client


def main():
    c = Client(base_url="http://localhost:8080", db="agribalyse-3.2")

    # ── Step 1: Get all activities ────────────────────────────────────
    print("Fetching all activities...")
    all_activities = c.search_activities(limit=10000)
    print(f"  Total activities: {len(all_activities)}")

    # ── Step 2: Sample activities and collect classifications ─────────
    sample_size = min(200, len(all_activities))
    print(f"\nFetching classifications for {sample_size} activities...")

    key_counts: Counter = Counter()        # classification key → count
    value_counts: dict[str, Counter] = {}  # key → (value → count)
    name_vs_class: list[tuple[str, dict]] = []  # (name, classifications)

    for i, act in enumerate(all_activities[:sample_size]):
        detail = c.get_activity(act.process_id)
        classifications = detail.get("piActivity", {}).get("pfaClassifications", {})
        name_vs_class.append((act.name, classifications))

        for k, v in classifications.items():
            key_counts[k] += 1
            value_counts.setdefault(k, Counter())[v] += 1

        if (i + 1) % 50 == 0:
            print(f"  ... {i + 1}/{sample_size}")

    # ── Step 3: Print classification key distribution ─────────────────
    print(f"\n{'='*60}")
    print("CLASSIFICATION KEYS (across {sample_size} activities)")
    print(f"{'='*60}")
    for k, count in key_counts.most_common():
        print(f"  {k}: {count} activities")

    # ── Step 4: Print value distribution per key ──────────────────────
    for k in sorted(value_counts):
        vc = value_counts[k]
        print(f"\n{'─'*60}")
        print(f"KEY: {k!r}  ({len(vc)} distinct values)")
        print(f"{'─'*60}")
        for val, cnt in vc.most_common(30):
            print(f"  [{cnt:3d}] {val}")
        if len(vc) > 30:
            print(f"  ... and {len(vc) - 30} more values")

    # ── Step 5: Cross-reference name patterns with classifications ────
    print(f"\n{'='*60}")
    print("NAME PATTERN vs CLASSIFICATION CORRELATION")
    print(f"{'='*60}")

    patterns = ["at farm", "at plant", "at feed plant", "at slaughterhouse",
                "at packaging", "at distribution", "at store"]
    for pattern in patterns:
        matches = [(name, cls) for name, cls in name_vs_class
                   if pattern in name.lower()]
        if not matches:
            continue
        print(f"\n  Pattern: '{pattern}'  ({len(matches)} matches in sample)")
        cls_vals: Counter = Counter()
        for _, cls in matches:
            for k, v in cls.items():
                cls_vals[f"{k}={v}"] += 1
        for kv, cnt in cls_vals.most_common(10):
            print(f"    [{cnt:3d}] {kv}")

    # ── Step 6: Supply chain classifications for a known product ──────
    print(f"\n{'='*60}")
    print("SUPPLY CHAIN CLASSIFICATIONS (Wheat flour, type 55)")
    print(f"{'='*60}")

    flour_results = c.search_activities(name="Wheat flour, type 55", limit=5)
    if flour_results:
        flour = flour_results[0]
        print(f"  Root: {flour.name} ({flour.location})")
        chain = c.get_supply_chain(flour.process_id, limit=200)
        print(f"  Supply chain: {chain.total_activities} activities, showing {len(chain.entries)}")

        sc_class_vals: dict[str, Counter] = {}
        for entry in chain.entries:
            for k, v in entry.classifications.items():
                sc_class_vals.setdefault(k, Counter())[v] += 1

        for k in sorted(sc_class_vals):
            vc = sc_class_vals[k]
            print(f"\n  KEY: {k!r}  ({len(vc)} distinct values)")
            for val, cnt in vc.most_common(20):
                # Mark farm/plant entries
                tag = ""
                if "farm" in val.lower():
                    tag = " ← FARM"
                elif "plant" in val.lower():
                    tag = " ← PLANT"
                print(f"    [{cnt:3d}] {val}{tag}")

        # Show a few entries with their classifications
        print(f"\n  Sample entries with classifications:")
        for entry in chain.entries[:15]:
            cls_str = ", ".join(f"{k}={v}" for k, v in entry.classifications.items())
            print(f"    {entry.quantity:.4f} {entry.unit} {entry.name}")
            if cls_str:
                print(f"      Classifications: {cls_str}")
    else:
        print("  Wheat flour not found, skipping supply chain analysis")


if __name__ == "__main__":
    main()
