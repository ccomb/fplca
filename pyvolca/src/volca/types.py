"""Data types for VoLCA API responses."""

import dataclasses
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Callable, ClassVar, Generic, Iterator, Literal, TypeVar, Union


_CAMEL_BOUNDARY = re.compile(r"(?<!^)(?=[A-Z])")


def _to_snake(s: str) -> str:
    """camelCase → snake_case. Idempotent on already-snake strings."""
    return _CAMEL_BOUNDARY.sub("_", s).lower()


@dataclass
class FromJson:
    """Mixin: build the dataclass from a JSON dict by snake-casing keys.

    Picks only the keys that match declared fields. Subclasses with nested
    dataclass fields, recursive parsing, or envelope unwrapping should
    override `from_json`.
    """

    @classmethod
    def from_json(cls, d: dict) -> Any:
        names = {f.name for f in dataclasses.fields(cls)}
        return cls(**{k: v for k, v in ((_to_snake(k), v) for k, v in d.items()) if k in names})


T = TypeVar("T")


@dataclass
class SearchResults(Generic[T]):
    """Paginated wire envelope, mirrors Haskell ``SearchResults a``.

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
    """

    results: list[T]
    total: int
    offset: int
    limit: int
    has_more: bool
    search_time_ms: float

    # Page fetcher: (offset, limit) -> raw JSON dict in the SearchResults
    # wire shape. ``limit`` may be None to let the server apply its own
    # default. None on detached envelopes (single-page in-memory results).
    _fetch: Callable[[int, int | None], dict] | None = field(
        default=None, repr=False, compare=False
    )
    _parse: Callable[[dict], T] | None = field(default=None, repr=False, compare=False)
    # Items fetched lazily during iteration past ``results``. Cached so that
    # a second iteration replays without re-hitting the server.
    _fetched: list[T] = field(default_factory=list, repr=False, compare=False)
    _exhausted: bool = field(default=False, repr=False, compare=False)

    @property
    def page_size(self) -> int:
        """Server-applied limit (page size for further fetches)."""
        return self.limit

    def __len__(self) -> int:
        return self.total

    def __getitem__(self, i: "int | slice") -> "T | list[T]":
        """Index or slice the *current* page only.

        Use ``list(sr)`` first when you need indexing/slicing across all
        pages — ``__getitem__`` deliberately stays local to avoid hidden
        round trips.
        """
        return self.results[i]

    def __iter__(self) -> Iterator[T]:
        """Yield items across all pages, fetching subsequent pages on demand.

        Yields the initial page, then any already-cached follow-up pages,
        then continues fetching until ``has_more`` is False. Subsequent
        iterations replay from the cache.
        """
        yield from self.results
        yield from self._fetched
        if self._exhausted or not self.has_more or self._fetch is None or self._parse is None:
            self._exhausted = True
            return
        offset = self.offset + len(self.results) + len(self._fetched)
        limit = self.limit
        while True:
            raw = self._fetch(offset, limit)
            items = [self._parse(x) for x in raw.get("results", [])]
            if not items:
                # Server claims hasMore but returned nothing — stop rather
                # than loop forever on a broken pagination contract.
                self._exhausted = True
                return
            self._fetched.extend(items)
            yield from items
            if not raw.get("hasMore", False):
                self._exhausted = True
                return
            offset = raw.get("offset", offset) + len(items)

    def page(self, n: int, *, page_size: int | None = None) -> "SearchResults[T]":
        """Fetch a specific page (1-based). Returns a fresh SearchResults.

        ``page_size`` overrides the current ``limit`` for the fetched page;
        the returned envelope's ``limit`` reflects what the server actually
        applied.
        """
        if n < 1:
            raise ValueError(f"page must be >= 1, got {n}")
        if self._fetch is None or self._parse is None:
            raise RuntimeError(
                "SearchResults has no fetcher attached — cannot fetch additional pages. "
                "This SearchResults was likely constructed in-memory (e.g. a test fixture)."
            )
        ps = page_size if page_size is not None else self.limit
        offset = (n - 1) * ps
        raw = self._fetch(offset, ps)
        return SearchResults.from_raw(raw, parse=self._parse, fetch=self._fetch)

    @classmethod
    def from_raw(
        cls,
        raw: dict,
        *,
        parse: Callable[[dict], T],
        fetch: Callable[[int, int | None], dict] | None = None,
    ) -> "SearchResults[T]":
        """Build from the wire envelope.

        Wire keys: ``results``, ``total``, ``offset``, ``limit``, ``hasMore``,
        ``searchTimeMs``. ``fetch`` is the callback used by iteration and
        ``page(n)`` to retrieve further pages. Omit only when the envelope
        is a single-page snapshot (``hasMore=False``) — otherwise iteration
        would silently truncate and the constructor raises.
        """
        items = [parse(x) for x in raw.get("results", [])]
        has_more = raw.get("hasMore", False)
        if fetch is None and has_more:
            raise ValueError(
                "SearchResults envelope reports hasMore=True but no fetch callback "
                "was provided. Iteration would silently truncate. Pass fetch=, or "
                "set hasMore=False on test fixtures."
            )
        return cls(
            results=items,
            total=raw.get("total", len(items)),
            offset=raw.get("offset", 0),
            limit=raw.get("limit", len(items)),
            has_more=has_more,
            search_time_ms=raw.get("searchTimeMs", 0.0),
            _fetch=fetch,
            _parse=parse,
        )


@dataclass
class DatabaseInfo(FromJson):
    """One entry of :meth:`Client.list_databases`.

    ``depends_on`` names the databases this one links against for cross-DB
    flow resolution — mirrors the ``dependsOn`` list surfaced by the relink
    endpoint. Derived from the engine's declared topology, not runtime state.
    """

    name: str
    display_name: str
    status: str  # "unloaded" | "partially_linked" | "loaded"
    path: str
    load_at_startup: bool = False
    is_uploaded: bool = False
    activity_count: int = 0
    description: str | None = None
    format: str | None = None
    depends_on: list[str] = field(default_factory=list)


@dataclass
class ScoringIndicator(FromJson):
    """One per-variable entry inside ``LCIABatchResult.scoring_indicators``.

    ``value`` is pre-multiplied by the scoring set's ``displayMultiplier``
    (configured in the scoring TOML) and expressed in the set's display unit.
    ``category`` names the impact category the variable was resolved from.
    """

    category: str
    value: float


@dataclass
class FlowContribution(FromJson):
    """Top contributing elementary flow for an impact category.

    Emitted inside ``LCIAResult.top_contributors``.
    """

    flow_name: str
    contribution: float  # in the impact unit
    share_pct: float  # 0..100
    flow_id: str
    category: str  # e.g. "air/urban air"
    cf_value: float = 0.0  # raw characterization factor
    compartment: str | None = None


@dataclass
class LCIAResult:
    """LCIA score for one impact category on one activity.

    Returned directly by :meth:`Client.get_impacts`, and nested inside
    :class:`LCIABatchResult.results` (one entry per impact category).
    """

    method_id: str
    method_name: str
    category: str
    damage_category: str
    score: float
    unit: str
    mapped_flows: int
    functional_unit: str
    normalized_score: float | None = None
    weighted_score: float | None = None  # in Pt
    top_contributors: list[FlowContribution] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "LCIAResult":
        return cls(
            method_id=d["methodId"],
            method_name=d["methodName"],
            category=d["category"],
            damage_category=d["damageCategory"],
            score=d["score"],
            unit=d["unit"],
            mapped_flows=d["mappedFlows"],
            functional_unit=d["functionalUnit"],
            normalized_score=d.get("normalizedScore"),
            weighted_score=d.get("weightedScore"),
            top_contributors=[FlowContribution.from_json(c) for c in d.get("topContributors", [])],
        )


@dataclass
class LCIABatchResult:
    """Batch LCIA: every impact category in a method collection, for one activity.

    Returned by :meth:`Client.get_impacts_batch`. Carries the per-method
    impact results plus any formula-based scoring sets configured in the
    engine TOML (PEF, ECS, or any named set).

    ``scoring_indicators`` gives the per-variable normalized-weighted
    breakdown of each scoring set — already multiplied by the set's
    ``displayMultiplier`` and expressed in its display unit (see
    :class:`ScoringIndicator`). Lets callers render per-indicator charts
    alongside the aggregate ``scoring_results``.
    """

    results: list[LCIAResult]
    single_score: float | None = None  # sum of weighted scores, in Pt
    single_score_unit: str | None = None
    norm_weight_set_name: str | None = None
    available_nw_sets: list[str] = field(default_factory=list)
    scoring_results: dict[str, dict[str, float]] = field(default_factory=dict)
    scoring_units: dict[str, str] = field(default_factory=dict)
    scoring_indicators: dict[str, dict[str, ScoringIndicator]] = field(default_factory=dict)

    @classmethod
    def from_json(cls, d: dict) -> "LCIABatchResult":
        raw_indicators = d.get("scoringIndicators", {})
        return cls(
            results=[LCIAResult.from_json(r) for r in d.get("results", [])],
            single_score=d.get("singleScore"),
            single_score_unit=d.get("singleScoreUnit"),
            norm_weight_set_name=d.get("normWeightSetName"),
            available_nw_sets=d.get("availableNWsets", []),
            scoring_results=d.get("scoringResults", {}),
            scoring_units=d.get("scoringUnits", {}),
            scoring_indicators={
                set_name: {var: ScoringIndicator.from_json(si) for var, si in per_set.items()}
                for set_name, per_set in raw_indicators.items()
            },
        )


class MatchMode(str, Enum):
    """How a :class:`ClassificationFilter` value is compared against the entry.

    ``EXACT`` — case-insensitive equality. ``CONTAINS`` — case-insensitive
    substring. Inherits from :class:`str` so ``json.dumps(MatchMode.EXACT)``
    and ``dataclasses.asdict(filter)["mode"]`` both serialise as the bare
    string ``"exact"`` / ``"contains"``.
    """

    EXACT = "exact"
    CONTAINS = "contains"


MatchModeLike = Union[MatchMode, Literal["exact", "contains"]]
"""Internal alias: a :class:`MatchMode` member or its literal string form.

Pyright autocompletes both shapes and rejects typos (``"exct"``, ``"Exact"``)
statically; the constructor normalises to :class:`MatchMode` at runtime."""


@dataclass(init=False, frozen=True)
class ClassificationFilter:
    """Filter a supply-chain/consumers query by a classification (system, value, mode).

    Matches one classification system entry, e.g.
    ``ClassificationFilter("Category", "Agricultural\\\\Food", "exact")`` or
    ``ClassificationFilter("Category", "Agricultural\\\\Food", MatchMode.EXACT)``.
    Multiple filters are AND-combined by the server.
    """

    system: str
    value: str
    mode: MatchMode = MatchMode.CONTAINS

    def __init__(
        self,
        system: str,
        value: str,
        mode: MatchModeLike = MatchMode.CONTAINS,
    ):
        if isinstance(mode, MatchMode):
            resolved = mode
        else:
            try:
                resolved = MatchMode(mode)
            except ValueError:
                valid = ", ".join(repr(m.value) for m in MatchMode)
                raise ValueError(
                    f"mode must be one of {valid} (or a MatchMode member); got {mode!r}"
                ) from None
        object.__setattr__(self, "system", system)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "mode", resolved)


@dataclass
class Activity(FromJson):
    process_id: str
    name: str
    location: str
    product: str
    product_amount: float
    product_unit: str


@dataclass
class Flow(FromJson):
    """A technosphere product or biosphere flow as returned by /flows.

    Mirrors the server's :code:`FlowSearchResult`. ``synonyms`` maps
    language code → list of synonym strings (empty when the database
    carries no synonym index).
    """

    id: str
    name: str
    category: str
    unit_name: str
    synonyms: dict[str, list[str]] = field(default_factory=dict)


@dataclass
class ConsumerResult(FromJson):
    """Activity that consumes a given supplier, with BFS depth."""
    process_id: str
    name: str
    location: str
    product: str
    product_amount: float
    product_unit: str
    depth: int  # hops from the queried supplier (1 = direct consumer)
    classifications: dict[str, str] = field(default_factory=dict)  # ISIC / CPC / Category, mirrors SupplyChainEntry


@dataclass
class SupplyChainEntry(FromJson):
    process_id: str
    name: str
    location: str
    quantity: float
    unit: str
    scaling_factor: float
    classifications: dict[str, str] = field(default_factory=dict)


@dataclass
class PathStep:
    """One step in the supply chain path returned by get_path_to.

    Note: the /path endpoint emits snake_case JSON directly (built via
    aeson's `object [...]` rather than generic ToJSON), so it bypasses
    the engine's stripLowerPrefix transform.
    """
    process_id: str
    name: str
    location: str
    unit: str
    cumulative_quantity: float
    scaling_factor: float
    local_step_ratio: float | None = None  # absent on root step

    @classmethod
    def from_json(cls, d: dict) -> "PathStep":
        return cls(
            process_id=d["process_id"],
            name=d["name"],
            location=d["location"],
            unit=d["unit"],
            cumulative_quantity=d["cumulative_quantity"],
            scaling_factor=d["scaling_factor"],
            local_step_ratio=d.get("local_step_ratio"),
        )


@dataclass
class PathResult:
    """Shortest upstream path from a root process to a matching activity."""
    path: list[PathStep]
    path_length: int
    total_ratio: float

    @classmethod
    def from_json(cls, d: dict) -> "PathResult":
        return cls(
            path=[PathStep.from_json(s) for s in d["path"]],
            path_length=d["path_length"],
            total_ratio=d["total_ratio"],
        )


@dataclass
class SupplyChainEdge:
    """`from`/`to` are Python keywords, so they're stored under from_id/to_id."""
    from_id: str
    to_id: str
    amount: float

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChainEdge":
        return cls(from_id=d["edgeFrom"], to_id=d["edgeTo"], amount=d["edgeAmount"])


@dataclass
class SupplyChain:
    root: Activity
    total_activities: int
    filtered_activities: int
    entries: list[SupplyChainEntry] = field(default_factory=list)
    edges: list[SupplyChainEdge] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChain":
        return cls(
            root=Activity.from_json(d["root"]),
            total_activities=d["totalActivities"],
            filtered_activities=d["filteredActivities"],
            entries=[SupplyChainEntry.from_json(e) for e in d["supplyChain"]],
            edges=[SupplyChainEdge.from_json(e) for e in d.get("edges", [])],
        )


@dataclass
class ConsumersResponse:
    """Reverse supply chain (/consumers) — paginated consumer list plus
    optional edge set. Mirrors :class:`SupplyChain` so callers have a
    uniform {entries, edges} shape in both traversal directions.

    ``consumers`` is a :class:`SearchResults[ConsumerResult]` — iterate it
    to walk every consumer across all pages. ``edges`` is populated only
    when ``include_edges=True``.
    """
    consumers: "SearchResults[ConsumerResult]"
    edges: list[SupplyChainEdge] = field(default_factory=list)

    @classmethod
    def from_json(
        cls,
        d: dict,
        *,
        fetch: Callable[[int, int | None], dict] | None = None,
    ) -> "ConsumersResponse":
        """Parse the /consumers wire envelope.

        ``fetch`` is a page fetcher returning the inner ``results`` envelope
        for ``(offset, limit)`` — used by ``SearchResults`` for lazy
        iteration. The client wires this so users get pagination for free;
        callers building ConsumersResponse manually (e.g. tests) can omit
        it and the resulting SearchResults is "detached" (one page only).
        """
        inner_fetch: Callable[[int, int | None], dict] | None
        if fetch is None:
            inner_fetch = None
        else:
            def inner_fetch(o: int, l: int | None) -> dict:
                return fetch(o, l)["results"]
        return cls(
            consumers=SearchResults.from_raw(
                d["results"], parse=ConsumerResult.from_json, fetch=inner_fetch,
            ),
            edges=[SupplyChainEdge.from_json(e) for e in d.get("edges", [])],
        )


# ---------------------------------------------------------------------------
# Exchanges (typed)
# ---------------------------------------------------------------------------

def _exchange_comment(ewu: dict | None, inner: dict) -> str | None:
    """Pick the per-exchange free-text comment.

    The wire exposes it in two places: ``exComment`` flat on the
    ``ExchangeWithUnit`` envelope, and ``comment`` inside the inner
    ``Exchange`` (mirrored from the source format's free-text field).
    Prefer the flat one when present; fall back to inner.
    """
    if ewu is not None:
        flat = ewu.get("exComment")
        if flat is not None:
            return flat
    return inner.get("comment")


@dataclass(frozen=True)
class Compartment:
    """Biosphere compartment (medium + optional subcompartment).

    Frozen so it's hashable and immutable — callers can use it as a dict key
    when grouping flows by compartment, and accidental mutation is rejected.
    """

    name: str
    sub: str | None = None

    @classmethod
    def from_json(cls, c: dict | None) -> "Compartment | None":
        if c is None:
            return None
        return cls(name=c.get("name", ""), sub=c.get("sub"))


# Roles a technosphere exchange can play within its host activity.
TechRole = Literal["ReferenceProduct", "Coproduct", "ReferenceInput", "Input"]


def _role_is_input(role: TechRole) -> bool:
    return role in ("Input", "ReferenceInput")


def _role_is_reference(role: TechRole) -> bool:
    return role in ("ReferenceProduct", "ReferenceInput")


@dataclass
class TechnosphereExchange:
    """An exchange with another activity. Carries no compartment — the
    producing activity's classifications describe the product taxonomy.
    """

    flow_name: str
    amount: float
    unit: str
    role: TechRole
    target_activity: str | None
    target_location: str | None
    target_process_id: str | None
    comment: str | None = None

    is_biosphere: bool = False  # discriminator for callers using duck typing
    is_waste: bool = False

    @property
    def is_input(self) -> bool:
        return _role_is_input(self.role)

    @property
    def is_reference(self) -> bool:
        return _role_is_reference(self.role)

    @classmethod
    def from_json(cls, ewu: dict) -> "TechnosphereExchange":
        inner = ewu["exchange"]
        return cls(
            flow_name=ewu["flowName"],
            amount=inner["amount"],
            unit=ewu["unitName"],
            role=inner["role"],
            target_activity=ewu.get("targetActivity"),
            target_location=ewu.get("targetLocation"),
            target_process_id=ewu.get("targetProcessId"),
            comment=_exchange_comment(ewu, inner),
        )


BioDirection = Literal["Resource", "Emission"]


def _direction_is_input(direction: BioDirection) -> bool:
    return direction == "Resource"


@dataclass
class BiosphereExchange:
    """An exchange with the environment (resource extraction or emission)."""

    flow_name: str
    compartment: Compartment | None
    amount: float
    unit: str
    direction: BioDirection  # "Resource" = extraction, "Emission" = release
    comment: str | None = None

    is_biosphere: bool = True  # discriminator for callers using duck typing
    is_waste: bool = False

    @property
    def is_input(self) -> bool:
        return _direction_is_input(self.direction)

    @property
    def is_reference(self) -> bool:
        return False

    @classmethod
    def from_json(cls, ewu: dict) -> "BiosphereExchange":
        inner = ewu["exchange"]
        return cls(
            flow_name=ewu["flowName"],
            compartment=Compartment.from_json(ewu.get("compartment")),
            amount=inner["amount"],
            unit=ewu["unitName"],
            direction=inner["direction"],
            comment=_exchange_comment(ewu, inner),
        )


@dataclass
class WasteExchange:
    """An exchange of a waste flow with a treatment activity.

    Shares the technosphere matrix with product flows but tracked as its own
    kind so callers can tell a "waste sent to landfill" output apart from a
    product input. Orphan waste (no linked treatment) contributes zero impact
    — same cut-off semantics as an orphan technosphere input.
    """

    flow_name: str
    amount: float
    unit: str
    is_input: bool  # True = consumed by treatment process; False = generated (typical case)
    target_activity: str | None
    target_location: str | None
    target_process_id: str | None
    comment: str | None = None

    is_biosphere: bool = False
    is_waste: bool = True

    @property
    def is_reference(self) -> bool:
        return False

    @classmethod
    def from_json(cls, ewu: dict) -> "WasteExchange":
        inner = ewu["exchange"]
        return cls(
            flow_name=ewu["flowName"],
            amount=inner["amount"],
            unit=ewu["unitName"],
            is_input=inner["isInput"],
            target_activity=ewu.get("targetActivity"),
            target_location=ewu.get("targetLocation"),
            target_process_id=ewu.get("targetProcessId"),
            comment=_exchange_comment(ewu, inner),
        )


Exchange = Union[TechnosphereExchange, BiosphereExchange, WasteExchange]


def parse_exchange(ewu: dict) -> Exchange:
    """Parse an `ExchangeWithUnit` JSON dict (as returned by GET /activity).

    The inner `exchange` object is tagged with a `"tag"` discriminator
    (``"TechnosphereExchange"``, ``"BiosphereExchange"`` or
    ``"WasteExchange"``) and carries all variant-specific fields flat at the
    same level.
    """
    tag = ewu["exchange"].get("tag")
    if tag == "TechnosphereExchange":
        return TechnosphereExchange.from_json(ewu)
    if tag == "BiosphereExchange":
        return BiosphereExchange.from_json(ewu)
    if tag == "WasteExchange":
        return WasteExchange.from_json(ewu)
    raise ValueError(f"Unknown exchange variant tag: {tag!r}")


def parse_exchange_detail(ed: dict) -> Exchange:
    """Parse an ``ExchangeDetail`` JSON dict (returned by GET /activity/{pid}/inputs|outputs).

    The flow is a tagged sum: ``{"kind": "technosphere"|"biosphere"|"waste",
    "flow": <flow>}``. The flow's ``kind`` lines up with the exchange variant
    tag.
    """
    inner = ed["exchange"]
    flow_outer = ed.get("flow") or {}
    flow_kind = flow_outer.get("kind")
    flow_payload = flow_outer.get("flow") or {}
    unit = ed.get("exchangeUnitName", "")
    comment = _exchange_comment(ed, inner)
    tag = inner.get("tag")
    if tag == "TechnosphereExchange":
        if flow_kind not in (None, "technosphere"):
            raise ValueError(
                f"TechnosphereExchange carried flow kind {flow_kind!r}"
            )
        target = ed.get("targetActivity") or {}
        return TechnosphereExchange(
            flow_name=flow_payload.get("name", ""),
            amount=inner["amount"],
            unit=unit,
            role=inner["role"],
            target_activity=target.get("name"),
            target_location=target.get("location"),
            target_process_id=target.get("processId"),
            comment=comment,
        )
    if tag == "BiosphereExchange":
        if flow_kind not in (None, "biosphere"):
            raise ValueError(
                f"BiosphereExchange carried flow kind {flow_kind!r}"
            )
        return BiosphereExchange(
            flow_name=flow_payload.get("name", ""),
            compartment=Compartment.from_json(flow_payload.get("compartment")),
            amount=inner["amount"],
            unit=unit,
            direction=inner["direction"],
            comment=comment,
        )
    if tag == "WasteExchange":
        if flow_kind not in (None, "waste"):
            raise ValueError(
                f"WasteExchange carried flow kind {flow_kind!r}"
            )
        target = ed.get("targetActivity") or {}
        return WasteExchange(
            flow_name=flow_payload.get("name", ""),
            amount=inner["amount"],
            unit=unit,
            is_input=inner["isInput"],
            target_activity=target.get("name"),
            target_location=target.get("location"),
            target_process_id=target.get("processId"),
            comment=comment,
        )
    raise ValueError(f"Unknown exchange variant tag: {tag!r}")


# ---------------------------------------------------------------------------
# Typed activity detail
# ---------------------------------------------------------------------------

@dataclass
class ActivityDetail:
    """Typed wrapper around the JSON returned by GET /activity/{pid}.

    Use the .inputs / .outputs / .technosphere_inputs convenience properties
    instead of walking the raw exchanges list.
    """

    process_id: str
    name: str
    location: str
    unit: str
    description: list[str]
    classifications: dict[str, str]
    reference_product: str | None
    reference_product_amount: float | None
    reference_product_unit: str | None
    all_products: list[Activity]
    exchanges: list[Exchange]

    @classmethod
    def from_json(cls, d: dict) -> "ActivityDetail":
        # The /activity endpoint returns ActivityInfo: `activity` is the ActivityForAPI.
        pfa = d["activity"]
        return cls(
            process_id=pfa["processId"],
            name=pfa["name"],
            location=pfa["location"],
            unit=pfa["unit"],
            description=pfa.get("description", []),
            classifications=pfa.get("classifications", {}),
            reference_product=pfa.get("referenceProduct"),
            reference_product_amount=pfa.get("referenceProductAmount"),
            reference_product_unit=pfa.get("referenceProductUnit"),
            all_products=[Activity.from_json(a) for a in pfa.get("allProducts", [])],
            exchanges=[parse_exchange(e) for e in pfa.get("exchanges", [])],
        )

    @property
    def inputs(self) -> list[Exchange]:
        return [e for e in self.exchanges if e.is_input]

    @property
    def outputs(self) -> list[Exchange]:
        return [e for e in self.exchanges if not e.is_input]

    @property
    def technosphere_inputs(self) -> list[TechnosphereExchange]:
        return [
            e for e in self.exchanges
            if isinstance(e, TechnosphereExchange) and e.is_input
        ]

    @property
    def is_allocated(self) -> bool:
        """True iff description contains a parseable allocation block.

        Implemented in volca/agribalyse.py to keep Agribalyse-specific text
        parsing out of the generic types module.
        """
        from .agribalyse import parse_allocation
        return parse_allocation(self.description) is not None


# ---------------------------------------------------------------------------
# Aggregation (for the /aggregate primitive)
# ---------------------------------------------------------------------------

@dataclass
class AggregateGroup(FromJson):
    """One bucket inside an AggregateResult."""
    key: str
    quantity: float
    count: int
    unit: str | None = None
    share: float | None = None


@dataclass
class AggregateResult:
    """Result of a Client.aggregate() call.

    ``filtered_total`` is the sum across all items matching the filters (the
    top-level number). ``groups`` is the per-bucket breakdown when ``group_by``
    was set; empty otherwise.
    """
    scope: str
    filtered_total: float
    filtered_unit: str | None
    filtered_count: int
    groups: list[AggregateGroup] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "AggregateResult":
        return cls(
            scope=d["scope"],
            filtered_total=d["filteredTotal"],
            filtered_unit=d.get("filteredUnit"),
            filtered_count=d["filteredCount"],
            groups=[AggregateGroup.from_json(g) for g in d.get("groups", [])],
        )
