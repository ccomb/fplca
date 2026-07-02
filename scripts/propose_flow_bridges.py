#!/usr/bin/env python3
"""Propose curated flow-name bridges for data/flows.csv from the auto-candidate set.

The LCIA matcher bridges a database's biosphere-flow name to a method's CF-flow
name via the curated registry (``data/flows.csv``). Auto-extraction (ILCD
othernames / PubChem) is kept only as an OFFLINE candidate generator -- it is
never injected into runtime matching, because its transitive closure fuses
unrelated substances (junk hubs). This tool turns that candidate set into a
short, human-reviewable list of *legitimate* additions.

A naive "auto-candidates minus registry" diff is useless: the candidate set has
~130k pairs, almost all junk. Instead we diff two engine flow-mappings -- the
curated-only state vs. curated+candidates -- and keep only the pairs that
actually change coverage: a database flow that gains a CF *only* because a
candidate bridged it to a real CF-flow name. That is the ~100-item set worth a
human's time.

Each surfaced pair is then classified against the same rules the Haskell
``RegistryLintSpec`` enforces, so a proposed row that survives will pass CI:

* wrong-origin -- source and target disagree on carbon origin
  (fossil / biogenic / land-use-change). REJECT: methods split these on purpose.
* collision   -- source and target both carry their OWN distinct CF in some
  method (e.g. Dinitrogen tetroxide vs Nitrogen dioxide: ecotox 3.84 vs 11.96).
  A single global synonym would collapse them via first-match-wins. REJECT.
* oversize    -- adding the pair grows an equivalence class past the lint bound.
  REJECT: an over-connected bridge is the junk-hub failure mode.
* ok          -- a genuine name variant (IUPAC/common, spelling, typo, trade
  name). PROPOSE, emitted as a ready-to-append flows.csv row.

Two subcommands:

    # 1. dump a database's per-method flow-mapping (run once per engine state)
    scripts/propose_flow_bridges.py dump \\
        --engine http://127.0.0.1:8095/api/v1 --database agribalyse-3-2 \\
        --out fm_curated.json
    # ...restart the engine with the candidate synonyms active, then:
    scripts/propose_flow_bridges.py dump \\
        --engine http://127.0.0.1:8095/api/v1 --database agribalyse-3-2 \\
        --out fm_candidates.json

    # 2. classify the difference into a review report + a flows.csv patch
    scripts/propose_flow_bridges.py propose \\
        --curated fm_curated.json --candidates fm_candidates.json \\
        --registry data/flows.csv --out-patch proposed_rows.csv

A maintainer reviews the report, drops anything the chemistry doesn't justify,
appends the accepted rows to data/flows.csv, and commits (bump data/VERSION).
"""
from __future__ import annotations

import argparse
import csv
import io
import json
import re
import sys
import urllib.parse
import urllib.request
from collections import defaultdict

# Mirror of RegistryLintSpec.maxClassSize -- keep in sync with the Haskell lint.
MAX_CLASS_SIZE = 12
LULUC_PHRASES = ("land transformation", "land use change", "peat oxidation", "soil or biomass stock")


# --------------------------------------------------------------------------- #
# pure helpers (mirror SynonymDB.normalizeName / RegistryLintSpec closely)     #
# --------------------------------------------------------------------------- #
def normalize(name: str) -> str:
    """Lowercase + collapse whitespace. Enough to line names up across sources."""
    return re.sub(r"\s+", " ", name.strip().lower())


def origin_families(name: str) -> frozenset[str]:
    """Carbon-origin qualifiers a name declares (see RegistryLintSpec.originQualifiers)."""
    s = name.lower()
    fam = set()
    if "biogenic" in s or "non-fossil" in s:
        fam.add("biogenic")
    if "fossil" in s and "non-fossil" not in s:
        fam.add("fossil")
    if any(p in s for p in LULUC_PHRASES):
        fam.add("luluc")
    return frozenset(fam)


def connected_components(edges: list[tuple[str, str]]) -> list[set[str]]:
    """Union-find closure over normalized name pairs -> equivalence classes."""
    parent: dict[str, str] = {}

    def find(x: str) -> str:
        parent.setdefault(x, x)
        root = x
        while parent[root] != root:
            root = parent[root]
        while parent[x] != root:  # path compression
            parent[x], x = root, parent[x]
        return root

    for a, b in edges:
        parent[find(a)] = find(b)
    groups: dict[str, set[str]] = defaultdict(set)
    for n in parent:
        groups[find(n)].add(n)
    return list(groups.values())


# --------------------------------------------------------------------------- #
# dump subcommand                                                              #
# --------------------------------------------------------------------------- #
def _get(url: str):
    with urllib.request.urlopen(url, timeout=600) as r:
        return json.load(r)


def dump(engine: str, database: str, out: str) -> None:
    methods = _get(f"{engine}/methods")
    print(f"{len(methods)} sub-methods", file=sys.stderr)
    result = {}
    for m in methods:  # key by method UUID: a category name can repeat across collections
        fm = _get(f"{engine}/db/{database}/method/{urllib.parse.quote(m['id'])}/flow-mapping")
        flows = {
            e["flowId"]: {"n": e["flowName"], "cf": e["cfValue"], "cfn": e.get("cfFlowName")}
            for e in fm.get("flows", [])
            if e.get("cfValue") is not None
        }
        result[m["id"]] = {"name": m["name"], "coll": m["collection"], "flows": flows}
        print(f"  [{m['collection']}] {m['name']}: {fm.get('matchedFlows')}/{fm.get('totalFlows')}", file=sys.stderr)
    with open(out, "w") as fh:
        json.dump(result, fh)
    print(f"wrote {out}", file=sys.stderr)


# --------------------------------------------------------------------------- #
# propose subcommand                                                           #
# --------------------------------------------------------------------------- #
def load_registry_edges(path: str) -> list[tuple[str, str]]:
    edges = []
    with open(path, newline="") as fh:
        rd = csv.reader(fh)
        next(rd, None)  # header
        for row in rd:
            if len(row) >= 2 and row[0] and row[1]:
                edges.append((normalize(row[0]), normalize(row[1])))
    return edges


def candidate_only_bridges(curated: dict, candidates: dict) -> dict:
    """flow-name -> aggregated evidence, for flows matched only under candidates."""
    by_name: dict[str, dict] = defaultdict(
        lambda: {"cfn": set(), "methods": set(), "maxcf": 0.0}
    )
    for mid in set(curated) | set(candidates):
        cur_f = curated.get(mid, {}).get("flows", {})
        cand = candidates.get(mid, {})
        for fid, e in cand.get("flows", {}).items():
            if fid in cur_f:
                continue  # already characterized without the candidate layer
            rec = by_name[e["n"]]
            if e.get("cfn"):
                rec["cfn"].add(e["cfn"])
            rec["methods"].add(cand.get("name"))
            rec["maxcf"] = max(rec["maxcf"], abs(e["cf"]))
    return by_name


def method_name_cfs(dump_json: dict) -> dict:
    """(methodId -> normalized flow name -> cf), to spot distinct-CF collisions."""
    out: dict[str, dict[str, float]] = {}
    for mid, m in dump_json.items():
        d: dict[str, float] = {}
        for e in m["flows"].values():
            d[normalize(e["n"])] = e["cf"]
        out[mid] = d
    return out


def classify(curated: dict, candidates: dict, registry_edges: list[tuple[str, str]]) -> list[dict]:
    bridges = candidate_only_bridges(curated, candidates)
    per_method = method_name_cfs(candidates)
    reg_norm = {n for e in registry_edges for n in e}

    proposals = []
    for name, ev in bridges.items():
        src = normalize(name)
        # target = the CF-flow name it reached; pick the capitalized spelling
        targets = sorted(ev["cfn"])
        target = targets[0] if targets else None
        verdict, reason = "ok", ""

        if not target:
            verdict, reason = "reject", "no CF-flow name (non-synonym match, e.g. regional/UUID path)"
        elif normalize(target) == src:
            verdict, reason = "skip", "already identical after normalization"
        elif frozenset((src, normalize(target))) <= reg_norm or (src, normalize(target)) in {
            (a, b) for e in registry_edges for a, b in (e, e[::-1])
        }:
            verdict, reason = "skip", "already in registry"
        else:
            fs, ft = origin_families(name), origin_families(target)
            tgt = normalize(target)
            # collision: src and tgt both carry their OWN distinct CF in some method
            collide = any(
                src in d and tgt in d and abs(d[src] - d[tgt]) > 1e-9
                for d in per_method.values()
            )
            if fs and ft and fs != ft:
                verdict, reason = "reject", f"wrong-origin: {sorted(fs)} vs {sorted(ft)}"
            elif collide:
                verdict, reason = "reject", "collision: source and target carry distinct CFs (would corrupt via first-match-wins)"
            elif ev["maxcf"] == 0:
                # a genuine name variant never needs a synonym to score 0; a zero-CF
                # target is almost always a biogenic / deliberately-excluded flow
                # (e.g. bare "Carbon dioxide" -> "Carbon dioxide, biogenic"), which
                # the origin check cannot see because the source carries no qualifier.
                verdict, reason = "reject", "zero-CF target (would assert no impact; verify it is not a biogenic/excluded flow)"

        proposals.append(
            {
                "name1": name,
                "name2": target or "",
                "verdict": verdict,
                "reason": reason,
                "methods": len(ev["methods"]),
                "maxcf": ev["maxcf"],
            }
        )

    # oversize check on the accepted set, closed together with the registry
    accepted = [p for p in proposals if p["verdict"] == "ok"]
    proposed_edges = [(normalize(p["name1"]), normalize(p["name2"])) for p in accepted]
    for cls in connected_components(registry_edges + proposed_edges):
        if len(cls) > MAX_CLASS_SIZE:
            for p in accepted:
                if normalize(p["name1"]) in cls or normalize(p["name2"]) in cls:
                    p["verdict"], p["reason"] = "reject", f"oversize: would join a class of {len(cls)} (>{MAX_CLASS_SIZE})"
    return sorted(proposals, key=lambda p: (-p["methods"], -p["maxcf"]))


def emit_patch(proposals: list[dict]) -> str:
    buf = io.StringIO()
    w = csv.writer(buf, lineterminator="\n", quoting=csv.QUOTE_MINIMAL)
    for p in proposals:
        if p["verdict"] == "ok":
            w.writerow([p["name1"], p["name2"]])
    return buf.getvalue()


def propose(curated_path: str, candidates_path: str, registry: str, out_patch: str | None) -> None:
    curated = json.load(open(curated_path))
    candidates = json.load(open(candidates_path))
    reg_edges = load_registry_edges(registry)
    proposals = classify(curated, candidates, reg_edges)

    by_verdict = defaultdict(list)
    for p in proposals:
        by_verdict[p["verdict"]].append(p)

    print(f"candidate-only flows: {len(proposals)}")
    for v in ("ok", "reject", "skip"):
        print(f"  {v:7} {len(by_verdict[v])}")
    print("\n=== PROPOSED (review, then append to data/flows.csv) ===")
    for p in by_verdict["ok"]:
        print(f"  {p['methods']:2d}m maxcf={p['maxcf']:<10.3g} {p['name1'][:40]:40} <- {p['name2']}")
    print("\n=== REJECTED (kept out on purpose) ===")
    for p in by_verdict["reject"]:
        print(f"  {p['name1'][:40]:40} <- {p['name2'][:30]:30} : {p['reason']}")

    if out_patch:
        with open(out_patch, "w") as fh:
            fh.write(emit_patch(by_verdict["ok"]))
        print(f"\nwrote {len(by_verdict['ok'])} rows to {out_patch}")


# --------------------------------------------------------------------------- #
# selftest: the three rejection rules + one legit pass, on synthetic dumps      #
# --------------------------------------------------------------------------- #
def selftest() -> None:
    # method M1: source flows with no curated CF; candidate layer bridges them.
    curated = {"M1": {"name": "tox", "coll": "c", "flows": {}}}
    candidates = {
        "M1": {
            "name": "tox",
            "coll": "c",
            "flows": {
                "f1": {"n": "Tetrachloroethylene", "cf": 1.0, "cfn": "tetrachloroethene"},  # legit
                "f2": {"n": "Carbon dioxide, fossil", "cf": 1.0, "cfn": "Carbon dioxide, biogenic"},  # wrong-origin
                "f3": {"n": "Dinitrogen tetroxide", "cf": 3.0, "cfn": "Nitrogen dioxide"},  # collision (below)
                "f4": {"n": "Nitrogen dioxide", "cf": 9.0, "cfn": "Nitrogen dioxide"},  # native, distinct cf
            },
        }
    }
    got = {p["name1"]: p["verdict"] for p in classify(curated, candidates, [])}
    expect = {
        "Tetrachloroethylene": "ok",
        "Carbon dioxide, fossil": "reject",
        "Dinitrogen tetroxide": "reject",
    }
    for k, v in expect.items():
        assert got.get(k) == v, f"selftest FAIL: {k} -> {got.get(k)}, expected {v}"
    assert got["Tetrachloroethylene"] == "ok"
    print("selftest OK:", {k: got[k] for k in expect})


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)

    d = sub.add_parser("dump", help="dump a database's per-method flow-mapping from a running engine")
    d.add_argument("--engine", required=True, help="engine base URL, e.g. http://127.0.0.1:8095/api/v1")
    d.add_argument("--database", required=True)
    d.add_argument("--out", required=True)

    p = sub.add_parser("propose", help="classify curated-vs-candidates into a review report + patch")
    p.add_argument("--curated", required=True, help="flow-mapping dump, curated-only engine")
    p.add_argument("--candidates", required=True, help="flow-mapping dump, curated+candidate engine")
    p.add_argument("--registry", default="data/flows.csv")
    p.add_argument("--out-patch", help="write accepted rows here (ready to append to flows.csv)")

    sub.add_parser("selftest", help="run the built-in classifier checks")

    a = ap.parse_args()
    if a.cmd == "dump":
        dump(a.engine, a.database, a.out)
    elif a.cmd == "propose":
        propose(a.curated, a.candidates, a.registry, a.out_patch)
    elif a.cmd == "selftest":
        selftest()


if __name__ == "__main__":
    main()
