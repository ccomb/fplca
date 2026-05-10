# Flow-mapping audit procedure

When a database (e.g. BAFU in EcoSpold1) is characterized by a method (e.g.
EF3.1 adapted, exported from SimaPro), the engine has to link each
characterization factor to the matching elementary flow. This linking
relies on a cascade — UUID → CAS → normalized name → hand-curated
synonym pair — and is never perfect. This document describes how to
detect, diagnose, and close those gaps.

## How linking works

`Method.Mapping.lookupCFForFlow` runs the cascade per flow. A flow that
matches by UUID needs no further work; otherwise the engine tries CAS,
then a normalized-name lookup, then the synonym pair table from
`data/flows.csv`. When **none** of those produce a hit, the flow goes
uncharacterized — it does not contribute to the score.

Two failure modes to keep separate in your head:

1. **Genuine method gap.** The CF method has no characterization for the
   substance/compartment in question. The flow being uncharacterized is
   the correct outcome.
2. **Mapping bug.** A homologous CF exists in the method, but the names
   don't match closely enough for the cascade to bridge them. The flow
   *should* contribute. This is what we want to catch and fix.

The post-scoring suggester distinguishes the two by attaching a list of
**similar CFs** (`similar_cfs`) to each uncharacterized flow. An empty
list = genuine gap. A non-empty list = candidate mapping bug, ranked by
similarity.

## The three similarity signals

`Method.Mapping.findSimilarCFs` stacks three signals; each candidate
carries the `reason` that won, so you know what to verify:

* `jaccard` — token overlap on the normalized names. Catches word-order
  and punctuation variants (e.g. `"Methane, biogenic" ↔ "Methane biogenic"`).
* `synonym_expansion` — token overlap *after* expanding both sides via
  the vendored PubChem snapshot. This is what bridges
  `"CO2" ↔ "Carbon dioxide"` — pure tokenization can never see they
  relate.
* `cas_bridge` — when the flow's CAS matches a CF's CAS, the candidate
  is surfaced at score 0.95 regardless of name overlap. Highest-confidence
  reason; catches cases where one side has CAS and the other doesn't.

Score range is `[0, 1]`; the headline value is the `max` of the three
signals.

## The chemical synonyms snapshot

`data/chem_synonyms.csv` is a vendored snapshot of PubChem synonyms,
filtered down to the CAS numbers actually present in the loaded
databases. It feeds the `synonym_expansion` signal and is the most
effective lever to widen the suggester's recall without manual work.

**Regenerate when**: a new database is added, or a new CAS appears in an
updated database release.

**How**:

```bash
scripts/build_chem_synonyms.py \
    --extract-cas-from /path/to/bafu /path/to/agribalyse \
    --out data/chem_synonyms.csv
```

The script politely paces itself against PubChem's REST API
(~5 requests/second). For a few thousand CAS that's a few minutes. The
output is deterministic given the same input set, so commit it as a
single atomic change: `data: regenerate chem_synonyms snapshot for
<reason>`. Scoring stays reproducible from the vendored CSV — no
runtime network call.

## The audit loop

The structural changes (suggester, diagnostics, comparison tool) get
committed once. The recurring work is **enriching `data/flows.csv`** with
hand-curated synonym pairs PubChem doesn't capture (LCA-specific
phrasings, package-naming quirks). Run this loop per database/method pair
you care about:

1. **Baseline the gap.** Pick a representative activity that exists in
   both the trusted database (e.g. SimaPro / Agribalyse) and the
   database under test (e.g. BAFU):

   ```text
   compare_impacts
     database_a=BAFU       process_id_a=<bafu-pid>     method_id_a=<EF3.1>
     database_b=Agribalyse process_id_b=<simapro-pid>  method_id_b=<EF3.1>
   ```

   Record `delta.relative_pct`. That's the metric you're driving down.

2. **Audit the unmatched flows.**

   ```text
   get_flow_mapping
     database=BAFU
     method_id=<EF3.1>
     verbose=true
     process_id=<bafu-pid>
   ```

   The `unmatched_db_flows` list is ranked by inventory contribution,
   so the top entries are where your effort matters most.

3. **Confirm semantically.** For each top entry, look at the
   `candidates` list. Open the BAFU flow and the suggested CF in their
   source files (XML, CSV) and verify they're the same substance — same
   CAS, same compartment if specified, same chemistry. Don't trust the
   score blindly: a high `jaccard` score on short names can mislead.

4. **Add a synonym pair.** Append one row to `data/flows.csv`:

   ```csv
   "BAFU exact name","EF3.1 exact name"
   ```

   Commit each pair (or a small thematic batch — e.g. all radon
   isotopes) as its own commit:

   ```text
   flows: link "<bafu name>" ↔ "<ef name>"
   ```

   Atomic commits keep `git bisect` and revert trivial when a pair
   turns out to be wrong.

5. **Verify the delta moved.** Re-run step 1. The `delta.relative_pct`
   should shrink. If it didn't, the synonym was wrong — revert.

6. **Stop when** the residual `delta.relative_pct` is within an
   acceptable band (e.g. < 5% per impact category) **or** when
   `loUncharacterized` (via `get_impacts include_diagnostics=true`)
   contains only entries with `similar_cfs == []` — those are genuine
   method gaps, not mapping bugs.

## When to add a flows.csv pair vs regenerate chem_synonyms

* Use `data/flows.csv` for **LCA-specific synonyms PubChem doesn't have**
  — packaging-naming variants, region-specific abbreviations, dataset
  authors' personal phrasings.
* Use `scripts/build_chem_synonyms.py` for **chemical synonyms** —
  formulas, IUPAC variants, common trade names. These belong in PubChem
  and the snapshot picks them up automatically.

The two lists are not redundant. `flows.csv` is small and hand-curated;
`chem_synonyms.csv` is large and machine-generated. A pair that
"feels chemical" probably belongs in PubChem; a pair that "feels
process-specific" belongs in `flows.csv`.

## Configuration

Add to your TOML config (path resolved relative to the config file):

```toml
chem-synonyms = "data/chem_synonyms.csv"
```

Without that line, the snapshot is treated as empty and the suggester
degrades to plain Jaccard — still useful, just blind to formula↔name
pairs like CO2↔Carbon dioxide.

## Scope note

This procedure targets BAFU↔EF3.1, both the **adapted** (SimaPro CSV)
and **original** (ILCD XML) distributions of the method. The fixes
shipped on the `flow-mapping-audit` branch cover both:

* Wire `normalizeCompartment` into the scoring cascade (otherwise
  BAFU's `emissions to air/...` compartments never match the method's
  `air/...` keying — silent score-of-zero bug).
* Compartment entries bridging BAFU's `low. pop.` / `high. pop.` /
  `low. pop., long-term` to ILCD's `non-urban air or from high stacks`
  / `urban air close to ground` / `... (long-term)`.
* Synonym pair `Dinitrogen monoxide ↔ nitrous oxide` for ILCD, which
  doesn't carry CAS on BAFU's emissions side so the cascade can't
  bridge by CAS.

Validated on `Wheat grains IP, at farm {CH}` (BAFU): Climate change
score matches between adapted and original to 0.09%.

## Known limitation: geolocation-aware CF selection

EF3.1 ships **per-country characterization factors** for many regionalized
categories (Acidification, Eutrophication, Particulate matter, Land use,
Water use, …). In SimaPro CSV, country variants appear as
`Air;low. pop.;Ammonia, CH;...;0.747;kg` (suffix on the substance name).
In ILCD XML, each `<factor>` carries a `<location>XX</location>` element.
The CFs differ — e.g. Ammonia Acidification:

| Country | CF (mol H+ eq / kg) |
|---|---|
| CH | 0.747 |
| FR | 0.857 |
| DE | 4.0 |
| NO | 11.491 |
| US | 3.02 |

VoLCA's cascade today **ignores both the suffix and the `<location>`
element**. It indexes CFs by `(name, medium, sub)` and collapses every
country into one key — `M.fromListWith preferBetter` picks an arbitrary
winner. On Swiss wheat (BAFU `... {CH}`), Climate change still works
because GWP100 is intrinsically global (no `<location>`), but
Acidification, Eutrophication etc. fall back to whichever country's CF
the parser inserted last. Adapted scoring picks the no-suffix `Ammonia`
default (3.02 = US-like); original scoring picks Norway (11.491). Both
are wrong for CH — the correct value is 0.747.

Symptom in the audit: cross-method comparisons (`compare_impacts`
adapted vs original) on regionalized categories show large deltas
(×3-10) that **do not** indicate mapping bugs — they're the same flow
matching different per-country CFs on each side.

Fixing this properly is a separate workstream. It needs:

1. `MethodCF` to carry an optional `mcfLocation :: Maybe Text` (parsed
   from `<location>` in ILCD, from the suffix in SimaPro CSV).
2. `MethodTables` to index CFs by `(name, medium, sub, location)` with
   a `Nothing` (global) fallback.
3. `lookupCFForFlow` to take the activity's geography and prefer the
   matching location's CF, falling back to global.

Until that lands, treat scores on regionalized impact categories as
**indicative within one method distribution**, not as cross-method
ground truth.
