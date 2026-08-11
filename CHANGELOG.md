# Changelog

## [Unreleased]

### Added
- A hosted server can refuse changes in its operator's own words:
  `read_only_message` under `[hosting]` replaces the default read-only
  sentence on every surface (the REST API, the MCP tools, and the shutdown
  endpoint). `GET /api/v1/hosting` reports the message alongside `read_only`,
  so a client can explain the situation before a change is even attempted.

### Fixed
- The macOS download for Apple Silicon now runs on a Mac that has no developer
  tools. It was built against the build machine's Homebrew copies of OpenBLAS
  and the Fortran runtime and looked for them at the same paths on yours, so it
  stopped at startup with `Library not loaded: .../libopenblas.0.dylib` unless
  you happened to have Homebrew and those exact formulas. Both are now built
  into the binary, as they already were on Linux. The Intel download still
  needs Homebrew: no way of building OpenBLAS into it produces a library that
  computes correctly there, and the cause sits upstream.
- The Windows download now carries the runtime libraries the program loads.
  Only `volca.exe` was in the zip, so it stopped with `libgfortran-5.dll
  introuvable` on a machine without MSYS2.
- A database read from a SimaPro export can be written back out as one. Long
  descriptions run to several paragraphs, and the export held them on a single
  line with SimaPro's own in-cell line break. Reading one turned that break
  back into a real newline, and the writer then refused the whole database
  because of it, so no such database could be exported at all. Descriptions are
  now written the way SimaPro writes them, and the paragraph breaks survive the
  round trip instead of being flattened to spaces. A line break in a name or a
  geography is still refused: there it means nothing and would tear the row
  apart on re-import.

### Changed
- Every download now includes `THIRD-PARTY-LICENSES.md`, naming the numerical
  libraries built into the program and their terms. The Windows zip also
  carries the full licence texts of the runtime libraries beside it.
- Assistant answers now carry their `web_url` deep links when the engine runs
  behind a reverse proxy that serves the web interface upstream. The proxy
  declares itself with the standard `X-Forwarded-Prefix` header; the links then
  carry that prefix and the forwarded protocol. Before, an engine running
  without a bundled frontend emitted no links at all, even when a proxy in
  front of it served those very pages.

## [0.9.5] - 2026-08-04

### Added
- You can adjust what an activity consumes and emits without re-describing it.
  `POST /api/v1/db/{db}/activity/{process-id}/exchanges`, `volca database
  edit-exchanges`, `edit_exchanges` in pyvolca and the same tool for an
  assistant all take the same short list: lines to remove, lines to restate,
  lines to add. This reaches activities that writing could not. An activity a
  database file brought in has an identity its parser minted, so no description
  addresses it — and a description could not carry back its classification,
  synonyms, parameters, pedigree or coproducts anyway. Naming only what changes
  leaves all of that exactly as it was. A line is named the way you already
  read it: an input by its provider, a waste output by its treatment, an
  emission by its flow. The reference product and any coproduct are out of
  reach, because changing those changes what the activity is. If a selector
  matches nothing, the edit is refused rather than reported as done; if it
  matches several lines, all of them change and the answer says how many. As
  with writing, a database the engine reads from its configuration is refused:
  copy it first. This is wire revision 7.

- You can ask why a flow scores with the characterization factor it does.
  `GET /api/v1/db/{db}/method/{method}/explain-cf/{flow}` and the `explain_cf`
  MCP tool replay the factor lookup for one flow and answer in sentences the
  engine writes itself: which rung of the cascade found the factor, which line
  of the method it came from, whether a synonym or a CAS number tied the two
  together, and how the amount was carried onto the factor's basis. The reply
  also lists the rungs tried before that one, including any refused because the
  flow's subcompartment forbids them. A contributions table gains the short
  version of the same answer: each flow now carries how its factor was found,
  so the whole table is annotated without asking per row. This is wire revision
  6.
- A factor found but unusable is no longer indistinguishable from no factor at
  all. When a flow's unit cannot be converted to the basis its factor is
  written in, the flow scores nothing, which used to look exactly like an
  uncharacterized flow; the reason is now recorded per flow, reported in the
  explanation, and named in the warning the engine already emitted.

- Activities can be written into a database you uploaded or copied, instead of
  only deleted from it. `POST /api/v1/db/{db}/activities` adds them,
  `PUT /api/v1/db/{db}/activity/{process-id}` rewrites one, and
  `volca database create-activities` / `replace-activity` do the same from the
  command line reading the same JSON document. You never supply a process id:
  it is derived from the activity name, location, product name and product
  unit, so writing the same description twice corrects one activity rather than
  making two. Writing is strict where importing is tolerant - a supplier that
  does not resolve, an amount that is not a finite number, a unit that cannot
  reach the supplier's are all refused, and a batch comes back with every
  complaint at once rather than one per round trip. A database the engine reads
  from its configuration is refused outright: it is background data the whole
  installation shares. This is wire revision 5.
- A server can say what it is called. `name` in `[server]` is repeated in the
  MCP handshake, both in `serverInfo` and in the first line of the
  instructions an assistant reads. Someone connecting several VoLCA servers
  at once, one per set of loaded databases, can now tell which one answered
  instead of guessing from the data.

### Changed
- Editing a database you uploaded or copied now survives a restart, whatever
  format it is stored in. Previously the sources and the matrix cache kept the
  pre-edit set, so a restart quietly brought every removed activity back. The
  edits are now recorded in a journal beside the database's own files and
  replayed when it is loaded again; the files themselves are never rewritten.
  That is what lets an EcoSpold 1, SimaPro CSV, ILCD or Brightway Excel
  database be edited at all: rewriting them would have worked only for
  EcoSpold 2, because the others derive each process identity from names,
  categories and the position of a dataset in its file, so saving would have
  given every activity a new identity at the next read. A copy now gets a
  directory of its own the moment it is made, holding its identity and its
  edits but no data - it reads the files of the database it was copied from,
  which can no longer be deleted while a copy still needs them. A configured
  database the engine only reads is still edited in memory alone, and the
  delete response says so with two fields, `transient` and `warnings`.

### Fixed
- A copy made from an edited database now keeps the source's edits. What was
  copied is the source as it stands, but those edits live in the source's
  journal and the copy's home had none, so the next load read the untouched
  files and quietly brought back everything the source had removed before the
  copy was made. The copy's journal now starts as a snapshot of the source's;
  edits the source makes afterwards stay the source's alone - the copy forks
  at the current point, as a branch should.
- Every path in a configuration file now means the same thing. A relative
  `path` under `[[databases]]`, `[[flow-synonyms]]`, `[[compartment-mappings]]`,
  `[[units]]`, `[[energy-densities]]` or `geographies` was read next to the
  configuration file, while the same key under `[[methods]]`, `chem_synonyms`
  or `substance_edges` was read next to wherever the engine happened to be
  started from. Two neighbouring lines of one file therefore pointed at two
  different places, and the usual way out was to write absolute paths, which
  makes the file describe one machine. All of them now follow the file, so a
  configuration and the data beside it can be moved or shared as one directory.
  Absolute paths are untouched.
- A flow search now tells same-named flows apart. Agribalyse 3.2 carries seven
  `Deltamethrin` flows that differ only by compartment; a search returned seven
  rows with the same name, medium and unit, in an order that interleaved them.
  Each result now carries its sub-compartment in a `compartment` field, next to
  `category`, which has always held the medium alone, and every sort order
  continues through the remaining displayed columns, so flows that look alike
  arrive adjacent and ordered instead of scattered.
- An uploaded database whose `meta.toml` recorded a path containing a backslash
  or a quote came back with that character doubled. The file escapes them as
  its format requires and nothing undid it on the way back, which on Windows
  meant a nested data path that resolved to nothing.
- An amount written as a signed power of ten is read instead of refused.
  SimaPro writes a scale factor that way — `1*10^-3*50` — and the engine
  understood `10^3` but not `10^-6`: the exponent was read by the
  exponentiation rule itself, which knows numbers but not signs, so the minus
  failed the whole expression and the amount fell back to whatever the cell
  began with. The exponent now goes through the same rule as any other signed
  operand. Exponentiation stays right-associative and still binds tighter than
  multiplication, so `2^3^2` is 512 and `-2^2` is −4.
- A regionalized factor now comes from the method's most specific line, not from
  whichever line the file happened to list last. When a method writes both a
  medium-level factor and one naming the flow's own subcompartment, and both
  carry a location, the two used to collide and the later row won: reordering
  the same file changed the score. The line that names the subcompartment wins,
  and between two of the same kind the larger factor does, as everywhere else.
- An emission to the sea is now characterized by an impact category that writes
  no sea-water factor of its own. A method file spells out a subcompartment only
  when its factor differs from the medium-level ("unspecified") one, so a
  category with nothing different to say about the sea writes nothing at all —
  and the engine was reading that silence as a refusal. Marine eutrophication is
  that case in EF 3.1: it writes no subcompartment line anywhere, because the
  JRC original gives it one and the same factor for fresh water, unspecified
  water and sea water (`nitrogen, total`: 1.0 in all three). Withholding its
  medium-level factor scored a nitrogen discharge to the sea as nothing, and a
  farmed salmon, which discharges most of its nitrogen straight into the sea,
  was scoring a fifth of its marine eutrophication. Categories that do
  distinguish the sea are untouched: they write the line, and the line wins.
  Which side a method lands on is now reported when it loads, so a category
  whose sea factors were lost on import can be told from one that never had any.
- A SimaPro amount cell the engine cannot read is now zero and warned about,
  instead of the number the cell happens to begin with. The reader used to stop
  at the first character that is not part of a number and keep what it had, so
  `1,5 kg` became 1.5 and `124902,34825322*1/Qp` — an expression whose parameter
  went missing — became a hundred and twenty-five thousand. An amount that is
  wrong by orders of magnitude and looks ordinary is the hardest kind to find;
  the import now names the cell it could not read and the value it used.
- A SimaPro amount written as a sum is now added up. An exporter may state a
  quantity in place, `0,45+0,247+,067`, and drop the integer part of a term.
  The reader wanted a digit before every decimal point, so one such term made
  the whole expression unreadable and the amount silently became the number it
  began with: 0.45 where the file says 0.764. In Agribalyse 3.2 this fell on
  the pesticide emission mixes — the shares of a mix stopped adding up to the
  kilogram they divide, and the freshwater ecotoxicity of the cereal crops
  built on them came out about 10% low. And when an amount really cannot be
  read, the import now says so: a warning names the process, the text it could
  not read, and the value used in its place.
- The MCP handshake announced version `0.6.0` whatever the build was; it now
  reports the running version.
- The dependency chosen for an uploaded database is now written into its
  `meta.toml`. It used to live only in memory and in the binary matrix cache,
  so a restart between choosing the dependency and finalizing the database lost
  it silently, and the database came back linked to nothing.

## [0.9.4] - 2026-08-01

### Added
- A quality report says what is malformed in a database, a question no score
  answers: an entry with two reference products, or coproducts allocated to
  90%, still computes — it just computes something wrong in silence.
  `GET /api/v1/db/{db}/quality-report` and the `get_quality_report` MCP tool
  run five structural checks: exactly one reference exchange per entry,
  coproduct percentages summing to 100 (±0.5 for source rounding), duplicate
  activities, non-finite amounts and zero reference amounts, and absent
  description, classification, location or unit. Each finding carries its
  severity, the activity it sits on and a readable detail; `limit` caps the
  list while `offenderCount` still covers all of it. A check with nothing to
  judge reports `applicable: false` rather than a passing zero. The report is
  a structural scan needing no matrices, so it answers on a staged database
  as well as a loaded one — which is when it is most worth reading.
- An instance can declare itself read-only. `read_only = true` in `[hosting]`
  makes it answer every analysis request and refuse every state change:
  loading and unloading, uploads, deletes, copies, relinks, dependency edits,
  and `POST /api/v1/shutdown` and `/api/v1/idle-timeout/{n}`, which decide how
  long the process lives. Refusals are `403` on REST and tool errors on MCP;
  nothing is silently ignored. This is what makes one instance safe to put in
  front of many unrelated callers, none of whom should be able to change the
  working set or end the server for the others.
- The `[hosting]` quotas now bound what a caller may keep. `max_uploads` caps
  how many databases of their own a caller holds, and `max_loaded_uploads` how
  many of those sit in memory at once; a copy spends the same budget as an
  upload. Both count uploaded databases only — the databases the config
  declares are what an uploaded inventory links against, so counting those
  would forbid the very thing uploading is for. Negative means unlimited, and
  where there is no `[hosting]` section at all (local runs, the CLI, the
  desktop app) neither applies. A refusal names what clears the way, unloading
  or deleting one, rather than stopping at a number.
- A method can now write an exception to one of its own wildcard rules: a
  substance starting with `!` takes its flows back out of the patterns declared
  for the same impact category. Some open families hold members that do not
  belong to the quantity the category counts, and no set of prefixes separates
  them — `Occupation, industrial area, benthos` is the sea floor and shares its
  prefix with a real factory yard. Writing the family out as a list instead
  would be the stale, per-database list that patterns exist to avoid. An
  exception that matches nothing is announced at load time, like a pattern that
  matches nothing. Exporting such a method to SimaPro, openLCA or ILCD leaves
  the exception rows out — those formats have no way to say "except this", and a
  row written as a flow would characterize exactly what it removes; VoLCA's own
  CSV keeps them and reads them back.
- The quality report now says when a geography was never declared. Nearly every
  process of some SimaPro databases leaves the `Geography` field at
  `Unspecified` — 97% of Agribalyse 3.2 — and the only geography left is the
  code inside the dataset name (`… {FR}`, `…//[RER]`, `…/CN U`). VoLCA reads
  it, which is what makes those databases usable, but the result is a reading of
  a name and not a declaration, and downstream the two are the same text. An
  EcoSpold dataset with no geography at all is filled in with `GLO`, likewise
  silently. The new `undeclaredGeography` check counts both, one finding per
  entry, so a maintainer can see how much of a database's geography is source
  data before treating it as such.
- A warning now reports factor lines that match a flow but cannot be
  converted into its unit — a per-kilogram factor against a flow measured in
  cubic metres, for instance. Refusing such a factor is correct (the
  dimensions do not agree), but the refusal used to be invisible: the flow
  simply scored zero, indistinguishable from a flow the method does not
  cover. Each affected method now says at load time how many flows are
  affected and names samples — for its global factors and its regionalized
  ones alike — so a silent undercount of this kind can no longer hide.
- The database quality report now flags exchange amounts too small to have
  been measured. Below 1e-27 — whatever the unit — a value is smaller than
  anything an instrument can produce (a hydrogen atom weighs 1.7e-27 kg), so
  it is a residue of computation wearing the costume of data. An ordinary
  exchange this small is a warning; a reference exchange this small is a
  danger, because normalization divides every other amount in the process
  by it.

- A characterization-coverage report tells database maintainers which flow
  names a method scores only through a name bridge. When a database names a
  substance differently from the method that characterizes it — `Bromomethane`
  versus `Methane, bromo-, Halon 1001`, say — VoLCA still scores the flow by
  matching on a synonym or CAS number. A tool that matches factors by their
  exact name, as many downstream consumers do, has no such bridge and scores
  that flow as zero without warning. The report lists each bridged flow grouped
  under the name the method itself uses, so the fix is a rename. It is available
  as the `get_characterization_coverage` MCP tool and at
  `GET /api/v1/db/{db}/characterization-coverage`, with one entry per loaded
  method collection so two method versions can be compared side by side.
- Method collections can be exported as an ILCD LCIA-method package (`ilcd`):
  a zip of one method dataset per impact category plus the flow datasets they
  reference (`lciamethods/` + `flows/`), which loads straight back. It carries
  the most metadata of any of the method export formats — methodology,
  description, per-factor direction, location and CAS all round-trip natively,
  the way a real EF package does. What ILCD's method profile cannot hold — a
  per-factor flow unit (it stores one reference unit per method), damage
  categories, normalization/weighting sets and formula scoring sets — is
  reported in export warnings, never dropped silently. Available through
  `POST /api/v1/method-collections/{name}/export` with `{"format": "ilcd"}`,
  `volca method export NAME --format ilcd`, and pyvolca's
  `export_method_collection(name, fmt="ilcd")`.
- The quality report flags individual allocation percentages outside the
  0-100% range, alongside the existing check that a block's percentages sum to
  100. A single factor can be out of range — a negative share, or more than the
  whole — while the block total still lands on 100.
- The quality report validates flow CAS numbers by their check digit: a CAS
  registry number confirms itself, so a corrupt one — which silently breaks
  the name-to-CAS bridge that matches flows across databases — is flagged. The
  zero-padded and canonical spellings both pass.
- The quality report flags oxygen-demand and organic-carbon measures recorded
  in a physically impossible order: within one entry BOD5 must not exceed COD,
  nor dissolved organic carbon exceed total. A reversed pair is a measurement
  or transcription error. Checked only where both members of a pair are
  present.
- The quality report checks that land transformation balances within each
  activity: the areas transformed *to* a use must match the areas transformed
  *from* another, since a parcel changed into one state was changed out of
  another. A gap beyond one percent — a dropped or mistyped line — is flagged,
  compared per unit so only comparable areas are summed.
- A computed-checks report joins the structural quality report:
  `GET /db/{db}/computed-quality-report` and the `get_computed_quality_report`
  MCP tool score every entry of a loaded database against a method collection
  and report per-category score outliers (median/MAD on a log scale within
  (category, reference-unit) groups — a mg-read-as-kg slip lands three orders
  of magnitude out), entries whose every score is zero, and negative category
  scores (info: avoided-production credits and waste treatment produce them
  legitimately). Separate from the structural report on purpose — that one
  stays identical on staged and loaded databases; this one needs the matrices
  and a loaded method collection.
- The quality report flags distinct activity names that merge under
  SimaPro's 80-character name truncation — each colliding name gets its own
  finding, so an export bound for SimaPro can be repaired before the names
  collapse into one process there.
- The quality report counts the exchanges that carry no pedigree scores, per
  entry — only in databases that carry pedigree scores at all, so formats
  that cannot publish them are not drowned in noise.
- The quality report lists the entries whose reference product nothing in
  the database consumes. Informational by nature: expected for a final
  product, a dangling intermediate in a background database.
- Method collections can be exported as openLCA JSON-LD (`openlca`): a zip
  of one `ImpactCategory` document per impact category, in the olca-schema
  archive layout, that loads straight back. Flow UUIDs, per-factor
  directions (`INPUT`/`OUTPUT`) and location codes are native to this
  format, so regionalized collections round-trip without the name-suffix
  projection the CSV formats use. What it cannot carry (methodology
  labels, damage categories, normalization/weighting and scoring sets) is
  reported in export warnings. The openLCA reader now also picks up the
  document-level `category` field as the impact category's group label,
  and method files load in a deterministic order on every machine.
- Method collections can be exported as columnar CSV — one column per
  impact category, one row per substance: the file you open in a
  spreadsheet. `POST /api/v1/method-collections/{name}/export` with
  `{"format": "csv"}`, `volca method export NAME --format csv`, or
  pyvolca's `export_method_collection(name, fmt="csv")`. Anything the
  format cannot carry (flow directions the compartment does not imply,
  damage categories, normalization/weighting sets, formula scoring sets)
  is reported in export warnings, never dropped silently.
- The columnar CSV method format itself grew the columns real methods
  need, read back by the parser and emitted by the writer: optional `cas`
  and `unit` key columns (real methods mix kg, m3 and MJ flows inside one
  category), and a `top/sub/qualifier` compartment path so subcompartment
  distinctions survive — in EF 3.1, nine factors out of ten are
  subcompartment-specific. Legacy files parse exactly as before, and
  quoted fields now work in these files too.
- Method collections can now be exported as SimaPro method CSV, the inverse
  of the SimaPro method import: `POST /method-collections/{name}/export`,
  `volca method export NAME --format simapro --out FILE`, and
  `export_method_collection` in the Python client. The file carries the
  collection's impact categories, damage categories and
  normalization/weighting sets, so a method imported in one format (for
  example an ILCD Environmental Footprint package) can be handed to a
  SimaPro user. Regionalized factors are written as name-suffixed substances
  (`Water, FR`) and land occupation/transformation factors under the `Raw`
  compartment — the conventions SimaPro method files use themselves; whatever
  the format cannot carry — a factor without a compartment, a factor whose
  direction the compartment column cannot express, formula scoring sets — is
  listed in the export warnings instead of being dropped silently.
- A collection-coverage endpoint:
  `GET /db/{db}/method-collection/{collection}/coverage` reports how many of
  a database's emission and resource flows at least one method of a
  collection characterizes, as a distinct count. No sum over the per-method
  figures can recover it, because a collection's methods overlap on the same
  flows. Exposed in pyvolca as `Client.get_collection_coverage`.

### Changed
- A server started with `--idle-timeout` now follows real work, not traffic.
  A connected MCP assistant polls its server all day (`ping`, `tools/list`),
  and those calls used to hold the server open with nobody behind them; now
  only an actual tool call counts. A matrix solve also counts as use in its
  own right, so an analysis that outlasts the request that asked for it no
  longer has the server exit underneath it.
- An EcoSpold 1 dataset published as `process_<uuid>.xml` (or `<uuid>.xml`)
  now keeps that identifier as its activity UUID instead of getting one
  minted from its name and location. Two releases of such a database can be
  compared dataset by dataset: a renamed dataset no longer reads as one
  deletion plus one creation. The process ids of these databases change once
  when the new engine first loads them, and their caches rebuild
  automatically. Files named any other way, and files holding several
  datasets, keep the minted UUID.
- The engine now advertises wire revision 4 on `/api/v1/version`. The three
  quality-report routes added since 0.9.3 arrived without a revision bump, so
  a client had no way to know whether an engine offered them: an engine too
  old to have the route answers 404, and so does an engine asked about a
  database it has not loaded. Clients gate on revision 4 and can tell the two
  apart. `pyvolca` understands the new revision.
- Log lines now say which database they belong to. When several databases
  load at once their lines used to interleave in one anonymous stream, so a
  page following one load would show another's progress. Each line of
  `GET /api/v1/logs` and of the `/api/v1/logs/stream` SSE feed is now a
  `{db, text}` object — `db` names the database whose operation emitted the
  line, or is null for lines that belong to no particular one. The terminal
  output is unchanged.

### Fixed
- Parametric coal flows no longer score zero energy. A flow carrying its
  calorific value in its name — `Coal, 26.4 MJ per kg`, `Coal, brown, 8 MJ per
  kg` — recovers an energy factor through its family, but coal splits into
  hard and brown and the fallback rightly refuses to pick between the two, so
  every parametric coal variant contributed nothing to fossil resource use
  without saying so. Four registry rows now attach each variant to its own
  family. The conversion still uses the calorific value written in the name,
  so the rows change which factor is found, never the energy accounted for.
- A classification preset that does not resolve is now refused instead of
  quietly filtering nothing. Asking a server for its raw agricultural products
  by a preset name it does not carry — a typo, or a config that never declared
  it — used to answer with every activity in the database, which reads like a
  result. The refusal names the presets the instance does carry. The MCP
  `aggregate` and `get_supply_chain` tools had a second form of the same
  problem: both advertise a `preset` parameter and neither ever read it, so
  even a valid preset was dropped there.
- "Land occupied", in the shipped Plain indicators method, no longer counts the
  sea. Its rule takes every `Occupation, …` flow, and that family holds the open
  ocean and the sea floor alongside fields and roads. Anything fed from the sea
  was reported as standing on it: one aggregated fish-meal process declares 679
  m²·year of `Occupation, sea and ocean`, and a farmed trout came out at 388
  m²·year of land — it now reads 0.87. A land crop is barely touched, since all
  it loses is the water a cargo ship crossed on its behalf: across Agribalyse's
  455 farm-gate products the median change is −0.004%, an apricot orchard goes
  from 6.96202 to 6.96198, and nothing anywhere rises. Sea, seabed and benthic
  occupation are now excepted; inland water bodies stay counted, a reservoir
  being a real surface somebody flooded.
- A region-tagged flow now gets the density that goes with the factor it
  borrows. When a method has no line for `Water, SERC`, VoLCA lends it the line
  written for `Water` — but that line can be denominated per kilogram while the
  flow is measured in cubic metres, and the density that bridges the two was
  only ever looked up under the flow's full name, region tag included. The tag
  was stripped to find the factor and not to find the density, so the flow ended
  up holding a factor of a dimension it could not reach, and scored nothing.
  Both lookups now strip it the same way.
- A density is now read in both directions. It relates two dimensions — mass to
  energy for a calorific value, mass to volume for a density proper — and a flow
  can meet a factor from either side of it, but only one side was handled: a
  flow in kilograms against a per-cubic-metre factor converted, while the same
  substance in cubic metres against a per-kilogram factor scored zero. A
  non-positive density is now refused outright rather than divided by, and the
  refusal is reported like any other.
- A SimaPro activity is now placed by the location its producer wrote down,
  rather than by one guessed from a name. SimaPro cuts its "Process name"
  field at 80 characters, which on a long name takes the `{FR}` tag off the
  end and leaves only a slash the name has for its own reasons — so
  "Bresaola … Already packed - PP/PE | No preparation" was filed under Peru,
  PE being the plastic. Reading a slash off the end of a name is how the
  WFLDB convention states a location and still works, but it is a reading of
  the name and now loses to a tag, wherever the tag sits. On Agribalyse 3.2
  this moves 73 activities, mostly to France, and retires the four
  "locations" that named no place: `PE` where it meant polyethylene,
  `F-Organic`, `F-Org(Farrrowin` and `Mid-western`. Names stop being cut
  short too, which separates 39 pairs of activities that until now shared one
  identity. Databases whose names really do end in a location — WFLDB, which
  relies on it 1876 times — are unaffected.
- Locations the databases actually use now have a place in the geography
  hierarchy. `data/geographies.csv` listed 91 codes; the databases use several
  hundred, so a Kenyan, Ukrainian or Brazilian-state activity had no wider
  location at all and its characterisation factors fell straight through to the
  global average with nothing said. The table now covers ecoinvent's whole
  vocabulary — every country in its UN subregion and continent, every province
  and grid inside the country it belongs to, and the regional aggregates the
  databases name. Containment comes from Natural Earth's public-domain country
  polygons rather than from hand-written guesses; codes whose membership is a
  judgement call, such as the aluminium industry's IAI areas, carry their
  region and nothing finer. The global codes close every fallback list, so a
  nearer regional factor always wins over the global average.
- A location whose name contains a comma is no longer cut in half. The
  geographies file was split on commas without regard for quoting, so
  `Europe, Western` — the location of 863 Agribalyse activities — parsed as a
  code called `Europe` followed by a stray field, and matched nothing. The file
  now goes through a real CSV reader and is decoded as UTF-8 whatever the
  system locale; a file it cannot read — bad quoting, bad encoding, duplicated
  codes — is reported instead of being quietly replaced by the built-in
  fallback table.

- A method's own per-unit factor lines no longer cancel each other. SimaPro
  names can bake the unit into the flow name — "Gas, natural/m3" and
  "Gas, natural/kg" are the same substance declared in two units, with two
  densities — and name normalization collapses both onto one key, where a
  single winner was crowned. The flow declared in the losing unit then read a
  dimensionally incompatible factor whose unit conversion silently zeroed its
  score: on a real SimaPro database, natural gas contributed nothing to
  fossil resource use, undercounting every gas-heated product by a factor of
  four. Unit-suffixed flows now match the factor line written for their exact
  name first, so each variant scores in its own unit; a lone variant still
  borrows its base resource's factor as before.
- The CAS bridge no longer guesses when one CAS number covers factor lines
  with different values. Water is the canonical case: every water flow shares
  one CAS, but a water-use method values each region differently and
  deliberately leaves rain, ocean and turbined water out — bridging them all
  to one arbitrary line (the world-average factor) made water scores explode
  on databases whose flows carry CAS numbers. Such a CAS class is now left to
  name matching alone; a CAS that identifies a single factor value still
  bridges. The refusal covers the per-location CAS bridge too, so an excluded
  flow cannot pick up a regional factor instead, and a flow that names its
  own region keeps that region's value rather than the consuming activity's.
  Region-located factor lines and subcompartment variants keep working as
  before — their variance is dispatched by location or arbitrated to the
  medium-level default, not guessed.
- Flows from a SimaPro CSV database now carry their CAS numbers. The parser
  used to leave every flow's CAS empty, so a characterization factor that
  could only reach its flow by CAS never matched on a SimaPro-sourced
  database. The CAS now comes from the file's own substance registry — the
  trailing blocks that list every substance with its CAS — filling about 89%
  of biosphere flows on a real export, so methods can match these flows by
  CAS instead of relying on name and synonym matching alone. A database
  cached before this change rebuilds its cache once on the next load, so
  already-imported databases pick the CAS up too instead of serving the
  old CAS-less flows forever.
- openLCA JSON-LD method files now load with the right factor directions,
  compartments, and reference unit — including files openLCA itself
  exported. Such files carry no per-factor direction, so every factor used
  to default to output — a resource factor (water withdrawal, land
  occupation) then matched against the wrong synonym view and could
  silently miss its flow. The direction now comes from the flow's category
  path (`resource/…`, `Raw materials`, `Land use` → input; `Emission to …`
  → output), or from the impact category's own direction when the path
  says nothing either way; a factor that states its direction explicitly
  is untouched. The parser also reads the olca-schema spellings a genuine
  export uses — the category path as a plain string on the flow reference
  (its `Elementary flows` root is dropped) and `refUnit` — where it
  previously only understood its own exporter's shape and silently lost
  the compartment and reference unit.
- Numbers in columnar CSV method files and normalization/weighting CSV
  files now parse exactly. The previous number parser drifted in the last
  decimal (`1.2227e-3` came back as `1.2227000000000002e-3`) — every such
  characterization or normalization factor was off by one ulp. A malformed
  value (`1,23` once imported as `1.0`, truncated at the comma) or a
  non-finite one (`NaN`) is now rejected instead of imported as a wrong
  number.
- Characterization factors loaded from an ILCD method package now parse
  exactly. The XML reader used that same drifting number parser, so a factor
  written as `0.0000010897906999999999` loaded one ulp off the value the file
  states; it now reads the correctly-rounded number.
- SimaPro CSV rows with quoted fields now parse correctly in files with
  Windows (CRLF) line endings. The carriage return left at the end of each
  line made the CSV reader give up and fall back to a naive split, which tore
  a quoted field apart at the separator it contains — a substance or category
  name like `"Ecotoxicity; freshwater"` landed in the wrong columns.
- The free-text comment on a SimaPro Products row (Agribalyse uses it for
  modelling notes such as edible fraction and raw-to-cooked ratios) now
  reaches the reference product and coproduct exchanges, so it shows up in
  activity details and exports like input and emission comments already did.
  Its data-quality pedigree prefix is decoded the same way too, and the
  SimaPro writer emits both back on the Products row instead of leaving the
  comment column empty.
- Multi-line SimaPro comments now display as real lines. SimaPro exports a
  multi-line comment on one physical line with an invisible control character
  (DEL, `\x7f`) between the lines; the parser now decodes it as a line break
  on every exchange comment, and the SimaPro writer encodes line breaks back
  the same way on export.
- The per-method `uniqueDbFlowsMatched` figure on the mapping-status endpoint
  now counts every database flow the method actually characterizes — probed
  with the same lookup scoring uses — instead of only the flows a factor
  resolved to directly. The old count missed every flow reached through a
  fallback (a factor covering a substance across many compartments counted as
  one flow), under-reporting a method's real reach several-fold on typical
  databases.

## [0.9.3] - 2026-07-15

### Changed
- The wire-format revision advertised on `/api/v1/version` is now `3`. Nothing
  breaks: every wire change in this release is additive, and existing clients
  keep working (pyvolca 0.8.x prints at most an upgrade hint). The revision
  exists so a client can tell whether the engine understands the new delete
  `ids` selection — an older engine would silently ignore the unknown key and
  treat the request as an empty filter, i.e. "everything", which is exactly
  the kind of guess a destructive operation must never make.

### Fixed
- `exact=true` on the activity search now applies to the `product=` filter
  too: it becomes a case-insensitive equality check on the reference product
  name, as it already was for names and geographies. Previously the flag was
  silently ignored for products, so an exact search could return near-miss
  substring matches.
- The `name=` filter on the supply-chain and consumers endpoints (REST and
  MCP) now filters. A name matching nothing previously disabled the filter
  and returned every entry — with a matching `filteredActivities` count — so
  a caller could not tell "no match" from "no filter". It now returns an
  empty result.
- A scoring integrity error (a regionalized score whose tables are internally
  inconsistent — mismatched lengths, absent weights) now fails the request
  with a 500 instead of silently scoring the category 0. A consumer could not
  tell that 0 from a real score. Coverage gaps are unaffected: an unmapped
  flow still contributes nothing and is reported as before. In sensitivity
  responses the error lands on the affected perturbation entry, which already
  carries per-entry errors.

### Added
- A supplier-gap report: `GET /db/{db}/gap-report` and the `get_gap_report`
  MCP tool list everything a database still demands but nothing supplies,
  after internal resolution and cross-database linking. Each missing product
  (name, location, unit) carries the blocking reason, how many consumer edges
  demand it, the distinct consumers, the total demanded amount, and the top
  consuming processes. It is the natural read right after a relink: it answers
  "what is missing to switch this database's background dependency?" without
  rebuilding the list by hand from the linking statistics. An optional `limit`
  keeps only the biggest gaps; the header counts always cover the full report.
- Delete-activities accepts an `ids` list to delete exactly the named
  processes, on the API (`"ids": [...]`) and the CLI (repeatable `--id`).
  Previously the only selection mode was a filter, so deleting a known list
  of processes required a deliberately unsatisfiable filter plus the `extra`
  list. `ids` cannot be combined with filter fields — an ambiguous request
  is refused, not guessed at.
- EcoSpold 2 `mathematicalRelation` formulas are now read. Dataset
  `<parameter>` variables are kept on the activity (value and raw formula),
  and each exchange formula is checked against the dataset's parameters and
  exchange variables as a consistency control. The amount stored in the file
  always stays authoritative: a formula that evaluates to a different value
  is reported as a divergence warning at load time, and the formulas that
  cannot be evaluated (unsupported functions, cross-dataset references) are
  summarized in one warning per dataset instead of being silently ignored.
- `volca server` starts without `--config`: it runs on the built-in defaults
  with no databases, ready to receive uploads or API-driven loads. Launchers
  no longer have to write an empty TOML file just to satisfy the flag. An
  explicit `--config` path that does not exist still fails loudly.
- Each factor listed by `GET /method/{id}/factors` now carries its
  compartment, location, and unit. A method routinely holds several factors
  for one substance name — emitted to air vs. water, or one per region — and
  without these fields such rows looked like duplicates.
- The `aggregate` primitive gains a `consumption` scope that answers "how much
  of X is consumed across the whole upstream chain" — total electricity or
  heat feeding a product, grass eaten by cattle. Each row is one scaled
  consumer→supplier link, so filtering by the consuming activity
  (`filter_consumer`, `filter_consumer_not`) avoids the double counting that
  summing cumulative production would give (for example counting the same
  electricity once per voltage level). Grouping by `consumer_name` shows who
  consumes what. Available on the REST endpoint, the MCP tool, and pyvolca.
- The relink mapping CSV's `source_location` and `target_location` columns
  now steer the linking instead of being informational. A row with a source
  location applies only to demands at that exact location (an exact row wins
  over a name-only row for the same name). A row with a target location
  designates the supplier literally: the link goes to that name at that
  location, bypassing the geography policy — so "a French process consuming
  Swiss cement: replace it with the French cement?" is answered row by row in
  the mapping. When nothing supplies the designated target, the relink
  reports a new `alias_target_missing` blocker (visible in the linking stats
  and the gap report) rather than silently falling back.

### Changed
- A mapping row now preempts the direct name cascade instead of being a
  last-resort retry. A curated row is a stronger statement of intent than a
  coincidental direct name match; names the mapping does not mention resolve
  exactly as before.
- The matrix-cache format changed with the new blocker (manual schema bump
  8 → 9): the first start after upgrading rebuilds each database cache once.

## [0.9.2] - 2026-07-14

### Added
- `server --port 0 --desktop` now asks the OS to bind an available loopback
  port and prints the actual `VOLCA_PORT=N` only after the listening socket is
  reserved. Launchers can therefore avoid reserve-then-release port races.
  With port 0 the server is only reachable from the local machine.

## [0.9.1] - 2026-07-11

### Added
- A characterization method declared in the TOML configuration can carry
  `[[methods.patches]]` blocks: declarative adjustments that rescale or
  replace the matched characterization factors every time the collection
  loads. A patch selects factors by impact category, flow name or prefix,
  CAS number, or subcompartment, and is re-applied to the freshly parsed
  source file on each load — so reloading never compounds it. A patch that
  matches no factor is reported at load time instead of being silently
  ignored.
- Bulk impact scoring — the `POST /db/{db}/impacts/{collection}` endpoint and
  the `score_activities` MCP tool — can now exclude long-term emissions, as
  scoring a single activity already could.

### Changed
- The wire-format revision advertised on `/api/v1/version` is now `2`, because
  the export download changed shape (below). pyvolca ≥ 0.7.2 requires an engine
  ≥ v0.9.1 and refuses older ones with a clear message; an older pyvolca gets a
  warning telling it to upgrade.
- Database exports download as raw bytes instead of base64-encoded JSON,
  matching how uploads already work — a third less data on the wire and far
  less memory on both ends for big files. Any export approximation warnings
  now travel in the `X-Volca-Export-Warnings` response header.

### Fixed
- Emissions to immediate groundwater are characterized again in ecotoxicity and
  human toxicity, inheriting the method's unspecified-water factor exactly like
  releases to a river or a lake. They previously scored zero — on a witness
  concrete process a single iron-ion flow was worth about 10% of the freshwater
  ecotoxicity score. Long-term groundwater keeps its explicit zero.
- Two flow-name synonym bridges (`Flupyrsulfuron-methyl sodium ↔
  Flupyrsulfuron-methyl`, `Pyrethrins ↔ Pyrethrum`) so EF 3.1 (adapted 1.03)
  characterizes flows it only lists under a sibling name, instead of silently
  scoring them zero in freshwater ecotoxicity.
- A database holding activities with several products can now be exported to
  ILCD. Each product becomes its own ILCD process, instead of the whole export
  being refused. This unblocks exporting databases read from SimaPro CSV, where
  two unrelated processes can share a name and so look like one multi-product
  activity.
- Exporting a large database to a zipped format (ILCD, EcoSpold 2) no longer
  stalls. The time spent packing the archive grew with the square of the number
  of files, so a full Agribalyse ILCD export — some fifty thousand files —
  exhausted memory and never returned.

## [0.9.0] - 2026-07-06

A characterization-accuracy release. EF 3.1 scores on Agribalyse and ecoinvent
now track the published references far more closely, the plugin framework that
carried no external users is gone, and a database can be loaded or unloaded from
every surface rather than only at start-up.

### Added
- A database can be loaded and unloaded from every surface — the REST API, the
  MCP server, the CLI, and the web UI — not only at server start-up.
- Scoring-set breakdowns can show a human-readable name for computed indicators
  (for example "Ecotoxicity, freshwater" instead of the raw key `etf`), via an
  optional `[methods.scoring.labels]` table in the scoring configuration. A
  label naming an unknown scoring variable is rejected when the configuration
  loads instead of being silently ignored.
- Scoring can optionally exclude long-term emissions — those a method releases
  beyond its time horizon — so results line up with inventories that account for
  them separately.
- A characterization method can be loaded from a bare `.csv` file, not only a
  zip archive; an unsupported file type now fails with a clear message instead
  of being silently misread.

### Changed
- Much closer EF 3.1 agreement with the published Agribalyse 3.2 and ecoinvent
  references, the sum of many corrections to how inventory flows are matched to
  characterization factors: CAS-guided matching with ambiguous bridges dropped,
  a curated and linted registry of name bridges for refrigerants and pesticides,
  a region-fallback chain for water flows, sub-compartment gating, a generalized
  density bridge, an ore-grade resource fallback, and a preference for the
  verbatim flow name when unit-suffixed homonyms collide. Many products that
  previously scored short — pesticide-heavy processes especially — are now
  characterized.
- Auto-extracted flow synonyms are an opt-in candidate set rather than always
  applied, so the shipped mapping rests on the curated bridges.

### Fixed
- Long-term emissions are characterized with the method's long-term factor
  rather than its default, and ionising radiation against its kBq reference unit.
- Water no longer collapses across regions: SimaPro factors are treated as
  name-regionalized rather than keyed on the consumer location.
- An unspecified chromium emission is treated as trivalent, and elemental metal
  emissions bridge to their ionic toxicity factor — correcting large human
  toxicity over-counts.
- EcoSpold 1 flow identity now includes the sub-category, and a SimaPro
  name-less multi-product block stays a single activity instead of splitting.

### Removed
- The plugin framework — its eight-handle registry, the `/analyze` REST
  endpoint, and the `plugin list` CLI command — is gone. It carried a single
  built-in implementation and no external users; flow-to-factor mapping is now a
  plain internal cascade.

### Performance
- Method tables are built once, off the request path, with a parallelized
  cascade and synonym-group memoization — a large speedup on the first scoring
  after a database loads.

## [0.8.1] - 2026-06-24

### Fixed
- Aggregate single scores (e.g. a PEF score) now compute correctly on JRC ILCD
  method collections such as EF 3.1. They previously failed with `Unknown
  variable` whenever a collection held several methods sharing one coarse
  damage category — for EF 3.1 the four climate-change methods, the freshwater
  ecotoxicity methods, and the resource-depletion methods each collapsed
  together, so their per-method scoring variables could not resolve.
  SimaPro-adapted methods were never affected.

## [0.8.0] - 2026-06-24

### Added
- A loaded database can be exported to any of the five supported formats —
  SimaPro CSV, EcoSpold 1, EcoSpold 2, ILCD, and Brightway Excel — from both the
  API and the CLI, so a database can be moved between tools or re-saved after edits.
- A loaded database can be edited in place: copy it under a new name, delete a
  filtered selection of activities, or relink it against a dependency through a
  name-to-name alias map.
- Activity records now carry separate `activity_name` and `product_name` fields,
  instead of the old `name`/`product` that blurred an activity and its reference
  product.
- A partial EcoSpold2 import (a handful of `.spold` files cut from a full
  database) now becomes analyzable by loading its matching ecoinvent background
  as a dependency: each input is linked to the exact background activity it
  names, by `activityLinkId` identity. Previously only nil-link inputs were
  resolved, so partial imports stayed unresolved however the background was
  loaded.
- When the loaded background is a *different* release than the import was cut
  from, the exact identity won't be present, so linking falls back to the usual
  attribute matching (name, location, unit). Those approximate links are flagged
  on the database setup view (and in the load log) so you can verify the
  dependency is the release you intended rather than trust a cross-version match
  as exact.
- `/api/v1/version` now reports a `wireVersion` integer. Clients read it at
  connect time to confirm they speak this engine's JSON format, so a version
  mismatch fails with a clear message instead of a confusing decode error.

### Changed
- Far broader EF 3.1 impact coverage: flows are matched by CAS number across
  naming schemes, a substance registry bridges nomenclatures, sub-compartments
  fall back sensibly, energy-carrier flows are characterized from their energy
  content, land use is regionalized with per-country factors, and a large synonym
  set links ecoinvent and Agribalyse flows to the method. Many products that
  previously scored short are now characterized.
- Large database uploads stream as raw bytes instead of base64-encoded JSON,
  cutting memory use and time on big files.
- A supplier substitution applies across every consumer of the substituted
  product, not only the process you queried.

### Fixed
- ecoinvent waste-treatment activities import and score correctly: the reference
  flow stays in the technosphere (these activities are no longer dropped) and
  cross-database waste keeps the correct sign.
- EcoSpold packages whose datasets live in a sub-directory now load.
- Requesting a well-formed but non-existent process returns a clear "activity not
  found" instead of a confusing error.
- A missing reference CSV is reported as an error instead of crashing the load.

## [0.7.0] - 2026-05-29

A month of engine work: a third flow kind for waste, regionalized impact
assessment, more input formats, and a hardening pass that turns silent
miscounts into explicit errors.

### Added
- Brightway Excel (`.xlsx`) inventories can now be loaded directly.
- Regionalized LCIA scoring via openLCA JSON-LD `ImpactCategory`, including
  uploading openLCA JSON-LD methods through the method pipeline.
- `WasteFlow` / `WasteExchange` as a third top-level flow kind, with an
  exact-match cross-database waste linker and explicit reporting of orphan
  (unlinked) waste.
- Per-database `geography_policy` controlling how activities are matched across
  databases during cross-DB linking.
- Sensitivity analysis: a rank-1 perturbation primitive and a sweep endpoint.
- SimaPro pedigree (uncertainty) matrix parsed and exposed through the API.
- Configurable per-instance upload size limit (hosting policy), enforced both in
  the upload handler and at the HTTP layer.
- macOS Intel (x86_64) engine build target and published release assets.
- One-liner installers for Linux, macOS, and Windows.
- MCP: batched LCIA and scoring sets, columnar `score_activities`, and
  source-native `activity_type` surfaced through search/score/get_activity.
- `/api/v1/licenses` endpoint plus `NOTICE` / `THIRD_PARTY_LICENSES`.

### Changed
- The cross-database dependency pin is now authoritative and persisted to cache,
  and databases auto-relink on every load.
- `Flow` split into `TechnosphereFlow` and `BiosphereFlow` (with `WasteFlow` as
  the third kind) so flow handling is total over the type system.
- Service errors are reported as 4xx, not 5xx: `InvalidUUID` → 400,
  `FlowNotFound` → 404, and cross-DB invariant breakages surface as client
  errors instead of 500s.
- Large LCIA speedups: batched multi-method scoring (~22× on PEF), precomputed
  per-activity weights for regionalized methods, and coalesced matrix solves.
- Docker images standardized on musl with a fully-static build and ARM64
  support.
- pyvolca: typed returns, string enums, and lazily paginated search/consumer
  results.

### Fixed
- Characterization no longer silently returns zero on a compartment,
  subcompartment, or unit mismatch — the gap is surfaced instead of undercounted.
- Regionalized LCIA returns a partial score on tainted columns rather than
  failing the whole computation.
- SimaPro: sign preserved on substitution (Materials/fuels) exchanges, reference
  amounts normalized to the canonical base unit, and split-location products
  exposed to the cross-DB linker.

## [0.6.0] - 2026-05-01

Packaging and distribution milestone (not previously recorded here).

### Added
- GitHub Actions build matrix producing release assets for Linux, macOS, and
  Windows, driven by a tag-based release pipeline with a relocatable data bundle.
- `pyvolca` published to PyPI, with per-exchange comments surfaced through the
  API and Python bindings.

### Changed
- SimaPro location extraction and reference-amount normalization improvements.

## [0.5.0] - 2026-02-02

### Added
- Desktop application (Tauri) for Windows and Linux with branded installer
- Console output panel with live log streaming in the web UI
- Loading screen shown while the backend starts in the desktop app
- MUMPS direct solver support on all platforms for faster matrix solving

### Changed
- Database upload now uses pure Haskell zip-archive (no external tools needed)
- Unified cross-platform build system (single bash script for Linux, macOS, Windows)
- Build dependency versions centralized in versions.env

### Fixed
- Console output panel showing empty in desktop app (optimized binary mismatch)
- Upload cancel now navigates back to databases list
- CSS and fonts bundled locally for offline use in desktop app

## [0.4.0] - 2026-01-18

### Added
- Database upload: load and unload your own EcoSpold databases (BYOL)
- Location aliases in configuration for targeted location overrides
- Production amount displayed in search results and activity header
- Product column in activity search results
- Database format column on databases page

### Changed
- Inventory page split into separate Resources and Emissions tables
- Redesigned left menu with white Explore/Lab sections
- Unified column order and shared ActivityRow component across activity tables

### Fixed
- EcoSpold1 exchanges without location now resolved via name lookup
- Zero-amount missing supplier exchange warnings suppressed
- Dynamic CPU detection for parallel loading (no more hardcoded worker count)
- Frontend minified with SWC for smaller bundles

## [0.3] - 2025-12-24

### Added
- Multi-database support with `--config volca.toml` configuration file
- EcoSpold1 parser for older LCA databases (Ecoinvent 2.x, BAFU)
- SimaPro CSV parser for Agribalyse
- LCIA impact assessment with method loading, flow mapping, and score computation
- Activity aliases configuration for resolving EcoSpold1 supplier links
- HTTP Basic Auth for API and web interface (`--password` or `VOLCA_PASSWORD`)
- Database management API endpoints (`/databases`, `/databases/{name}/activate`)
- LCIA methods API endpoint (`/methods`)
- Databases page in web UI with table layout
- LCIA tab in activity details view

### Changed
- Cache system now uses automatic schema-based invalidation (no manual version bumping)
- Cache filename simplified to `volca.cache.{dbName}.bin.zst`
- Per-database PETSc solver cache for instant database switching
- Web UI redesign: split details tabs into individual pages, sticky headers, improved left menu
- Database name included in URLs for bookmarkable multi-database views

### Fixed
- Double-click navigation and search focus issues
- Navigation history properly returns to search results
- Search removed 2-character minimum requirement

## [0.2] - 2025-12-04

### Added
- Details view with tabs for upstream activities, emissions, natural resources, and products
- Graph view with force-directed layout
- Activity search with multi-word filtering and pagination
- Products tab showing all outputs from multi-product activities
- URL routing for bookmarkable views

### Changed
- Renamed project from acv-engine to volca

## [0.1] - 2025-11-09

### Added
- Core LCA engine with EcoSpold2 XML parsing
- Matrix computation with PETSc/SLEPc
- REST API, CLI, and web interface with Tree and Inventory views
- Database caching for fast startup
