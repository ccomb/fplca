"""Ingredient substitution workflow using fpLCA.

Steps:
1. Find all "at plant" activities
2. For selected ones, find upstream "at farm" ingredients with quantities
3. Export to CSV

Usage:
    python -m examples.ingredient_substitution
"""

import csv
import sys
from pathlib import Path

# Add parent to path for development (before pip install)
sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from fplca import Client, Server


def to_csv(rows: list, filename: str, fields: list[str]) -> None:
    """Write a list of objects to CSV."""
    with open(filename, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        for row in rows:
            writer.writerow({k: getattr(row, k) for k in fields})
    print(f"  Written {len(rows)} rows to {filename}")


def main():
    config = "fplca.toml"
    db_name = "agribalyse-3.2"

    with Server(config=config) as srv:
        c = Client(port=srv.port, db=db_name)

        # Step 1: Find all "at plant" activities
        print("Step 1: Searching for 'at plant' activities...")
        plants = c.search_activities(name="at plant", limit=10000)
        print(f"  Found {len(plants)} 'at plant' activities")
        to_csv(
            plants,
            "at_plant.csv",
            ["process_id", "name", "location", "product", "product_amount", "product_unit"],
        )

        # Step 2: For the first few, find upstream "at farm" ingredients
        print("\nStep 2: Finding upstream 'at farm' ingredients...")
        for activity in plants[:5]:
            print(f"\n  Activity: {activity.name} ({activity.location})")
            chain = c.get_supply_chain(activity.process_id, name="at farm", limit=50)
            print(f"    Total upstream: {chain.total_activities}")
            print(f"    'at farm' matches: {chain.filtered_activities}")

            if chain.entries:
                safe_name = activity.name.replace("/", "_").replace(" ", "_")[:50]
                to_csv(
                    chain.entries,
                    f"{safe_name}_ingredients.csv",
                    ["process_id", "name", "location", "quantity", "unit", "scaling_factor"],
                )

                # Show top 5 ingredients
                for entry in chain.entries[:5]:
                    path_str = " → ".join(n.name for n in entry.path) if entry.path else ""
                    print(
                        f"    {entry.quantity:.4f} {entry.unit} of "
                        f"{entry.name} ({entry.location})"
                    )
                    if path_str:
                        print(f"      Path: {path_str}")


if __name__ == "__main__":
    main()
