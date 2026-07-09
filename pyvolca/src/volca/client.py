"""HTTP client for the VoLCA HTTP API, dispatched by OpenAPI operationId.

Design
------
The client fetches the engine's OpenAPI spec once (from
``GET /api/v1/openapi.json``) and builds a dispatch table keyed on
``operationId``. Each public method is a thin typed wrapper that calls
``self._call("operation_id", **python_kwargs)`` and lets the dispatcher
handle path substitution, query-string assembly, and JSON decoding.

This removes ~250 lines of hand-written query-param plumbing and
decouples pyvolca's PyPI release cadence from engine endpoint changes:
when the engine renames a query parameter, pyvolca picks it up on the
next spec fetch with no code change. See ``docs/guides/pyvolca.md`` for
the user-facing view.

Kwarg name translation
----------------------
Python wrappers use snake_case kwargs (``process_id``, ``min_quantity``).
The OpenAPI spec carries the original Servant names (``processId``,
``min-quantity``). ``_call`` canonicalizes by trying each kwarg name
against a list of candidate wire names:

  1. The name as-is (``name``, ``limit``)
  2. snake_case → camelCase (``process_id`` → ``processId``)
  3. snake_case → kebab-case (``min_quantity`` → ``min-quantity``)

and picks the first that matches a spec parameter. Unknown kwargs raise
``VoLCAError`` so typos are caught at call time.

Substitutions
-------------
If a wrapper is called with ``substitutions=[{...}]``, ``_call`` upgrades
the operation from GET to POST and sends the substitution body. Works
transparently for ``get_inventory``, ``get_supply_chain``, and
``get_impacts`` — all the endpoints that have POST-with-substitutions
variants in the Servant API.
"""

from __future__ import annotations

import urllib.parse
import warnings
from enum import Enum
from pathlib import Path
from typing import Any

import requests

from .types import (
    Activity,
    ActivityDetail,
    AggregateOp,
    AggregateResult,
    AggregateScope,
    CharacterizationResult,
    ClassificationFilter,
    ClassificationSystem,
    ConsumerResult,
    ConsumersResponse,
    ContributingActivities,
    ContributingFlows,
    DatabaseInfo,
    Exchange,
    Flow,
    FlowMapping,
    InventoryResult,
    LCIABatchResult,
    LCIAResult,
    MatchMode,
    Method,
    PathResult,
    Preset,
    SearchResults,
    ServerVersion,
    Substitution,
    SupplyChain,
    parse_exchange_detail,
)
from . import _compat


class VoLCAError(Exception):
    """Error from the VoLCA API."""

    def __init__(self, message: str, status_code: int | None = None, body: str = ""):
        self.status_code = status_code
        self.body = body
        super().__init__(message)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _snake_to_camel(s: str) -> str:
    """``process_id`` → ``processId``."""
    head, *tail = s.split("_")
    return head + "".join(p.capitalize() for p in tail)


def _snake_to_kebab(s: str) -> str:
    """``min_quantity`` → ``min-quantity``."""
    return s.replace("_", "-")


def _candidate_wire_names(py_name: str) -> list[str]:
    """Wire names to try for a Python kwarg, in priority order.

    The same Python name may map to different wire forms depending on
    whether the spec places it in ``path`` (Servant uses camelCase) or in
    ``query`` (Servant sometimes uses snake_case, sometimes kebab-case,
    sometimes camelCase — the aggregate endpoint is snake_case, for
    example). Return every plausible form; the caller matches against
    the spec's parameter list.
    """
    seen = []
    for cand in (py_name, _snake_to_camel(py_name), _snake_to_kebab(py_name)):
        if cand not in seen:
            seen.append(cand)
    return seen


_EXPORT_FORMATS = frozenset(
    {"simapro", "ecospold1", "ecospold2", "ilcd", "brightway"}
)
"""Target keywords accepted by ``POST /api/v1/db/{dbName}/export``.

Mirrors the engine's ``parseExportFormat`` (case-folded). Validated
client-side so a typo fails before the round-trip with the same message
shape the engine would have returned."""


_FORMATTED_SCALARS = (str, int, float)


def _format_query_value(value: Any) -> Any:
    """Convert a Python value to a query-string-ready form.

    Booleans become ``"true"``/``"false"``. Lists remain lists (requests
    encodes repeated keys for list values). :class:`enum.Enum` members are
    serialised via their ``.value`` so StrEnums round-trip cleanly.
    Everything else is stringified.
    """
    if value is None:
        return None
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, Enum):
        return _format_query_value(value.value)
    if isinstance(value, _FORMATTED_SCALARS):
        return str(value)
    if isinstance(value, list):
        return [_format_query_value(v) for v in value]
    return str(value)


def _coerce_class_filter(c: dict | tuple) -> dict:
    """Normalize one classification filter to the wire ``DeleteClassFilter``.

    Accepts a ``{"system", "value", "exact"?}`` dict or a
    ``(system, value, exact?)`` tuple. ``exact`` defaults to False. Raises
    VoLCAError on a malformed entry rather than silently dropping fields.
    """
    if isinstance(c, dict):
        missing = {"system", "value"} - set(c)
        if missing:
            raise VoLCAError(
                f"Classification filter missing keys: {sorted(missing)}. "
                "Expected {'system', 'value', 'exact'?}."
            )
        return {
            "system": c["system"],
            "value": c["value"],
            "exact": bool(c.get("exact", False)),
        }
    if isinstance(c, (tuple, list)):
        if len(c) not in (2, 3):
            raise VoLCAError(
                f"Classification filter tuple must be (system, value[, exact]); "
                f"got {len(c)} items."
            )
        system, value = c[0], c[1]
        exact = bool(c[2]) if len(c) == 3 else False
        return {"system": system, "value": value, "exact": exact}
    raise VoLCAError(
        f"Classification filter must be a dict or tuple, got {type(c).__name__}."
    )


def _resolve_page_args(
    page: int | None,
    page_size: int | None,
    limit: int | None,
    offset: int | None,
) -> tuple[int | None, int]:
    """Reconcile the page/page_size convenience kwargs with wire-level limit/offset.

    Returns ``(wire_limit, wire_offset)``. ``wire_limit`` may be None — that
    leaves it off the request entirely so the engine applies its own
    default page size (matching what the web UI gets).

    Raises VoLCAError if the kwargs combination cannot map to a single
    unambiguous ``(offset, limit)``. The page-style and wire-style families
    cannot mix, and ``page=N`` without ``page_size`` is rejected: we refuse
    to fabricate a page size, since assuming one would silently misalign
    the offset whenever the engine's default differs.
    """
    page_style = page is not None or page_size is not None
    wire_style = offset is not None  # bare `limit=N` is just a cap, not pagination
    if page_style and wire_style:
        raise VoLCAError(
            "Mix of page-style (page=, page_size=) and wire-style (offset=) "
            "pagination kwargs. Use one or the other."
        )
    if page is not None and page_size is None:
        raise VoLCAError(
            "page=N requires an explicit page_size=M — offset cannot be derived "
            "from page alone without committing to a page size."
        )
    if page is not None and page < 1:
        raise VoLCAError(f"page must be >= 1, got {page}")
    if page_size is not None and page_size < 1:
        raise VoLCAError(f"page_size must be >= 1, got {page_size}")
    if page_style:
        # page_size alone (no page=) means "page 1 with this size".
        return page_size, ((page or 1) - 1) * (page_size or 0)
    return limit, offset or 0


SubstitutionLike = Substitution | dict
"""A :class:`Substitution` or a legacy ``{"from", "to", "consumer"}`` dict.

``consumer`` is optional — omit it for a global swap. The dict form is
accepted for backwards-compat one-liner ergonomics; the typed form is
preferred (catches typos at construction time)."""


def _substitution_body(substitutions: list[SubstitutionLike]) -> dict:
    """Build the request body for substitution endpoints.

    Accepts :class:`Substitution` instances or the legacy dict form with
    ``from`` / ``to`` keys and an optional ``consumer`` key (omit it for a
    global swap).
    """

    def coerce(s: SubstitutionLike) -> dict:
        if isinstance(s, Substitution):
            return s.to_wire()
        # dict form — validate the required keys here so typos like
        # ``"comsumer"`` fail with a clear error rather than at the engine.
        missing = {"from", "to"} - set(s)
        if missing:
            raise VoLCAError(
                f"Substitution dict missing keys: {sorted(missing)}. "
                "Use a Substitution(from_pid=, to_pid=, consumer=) instead."
            )
        body = {"from": s["from"], "to": s["to"]}
        if "consumer" in s:
            body["consumer"] = s["consumer"]
        return body

    return {"substitutions": [coerce(s) for s in substitutions]}


# ---------------------------------------------------------------------------
# Spec loader
# ---------------------------------------------------------------------------


class _Operation:
    """Dispatch entry for a single operationId.

    Immutable after the spec is parsed. Built by ``_parse_spec``.
    """

    __slots__ = ("operation_id", "method", "path_template", "path_params", "query_params")

    def __init__(
        self,
        operation_id: str,
        method: str,
        path_template: str,
        path_params: list[str],
        query_params: list[str],
    ):
        self.operation_id = operation_id
        self.method = method
        self.path_template = path_template
        self.path_params = path_params
        self.query_params = query_params

    @property
    def wire_names(self) -> set[str]:
        return set(self.path_params) | set(self.query_params)


def _parse_spec(spec: dict) -> dict[str, _Operation]:
    """Walk an OpenAPI spec and index operations by ``operationId``.

    Only operations that carry an explicit ``operationId`` are indexed;
    infrastructure endpoints like ``/auth`` and ``/version`` (which have
    no matching entry in ``API.Resources``) are skipped.
    """
    ops: dict[str, _Operation] = {}
    for path, item in spec.get("paths", {}).items():
        for method, op in item.items():
            if not isinstance(op, dict):
                continue
            operation_id = op.get("operationId")
            if not operation_id:
                continue
            path_params: list[str] = []
            query_params: list[str] = []
            for param in op.get("parameters", []):
                loc = param.get("in")
                name = param.get("name")
                if loc == "path":
                    path_params.append(name)
                elif loc == "query":
                    query_params.append(name)
            ops[operation_id] = _Operation(
                operation_id=operation_id,
                method=method.upper(),
                path_template=path,
                path_params=path_params,
                query_params=query_params,
            )
    return ops


# ---------------------------------------------------------------------------
# Public client
# ---------------------------------------------------------------------------


class Client:
    """HTTP client for the VoLCA HTTP API.

    Usage::

        c = Client(db="agribalyse-3.2", password="1234")
        plants = c.search_activities(name="at plant")
        chain = c.get_supply_chain(plants[0].process_id, name="at farm")

    Substitutions can be passed to ``get_supply_chain``, ``get_inventory``,
    and ``get_impacts`` to compute results with a different upstream
    supplier — fast::

        subs = [{"from": old_pid, "to": new_pid, "consumer": consumer_pid}]
        result = c.get_impacts(pid, method_id=mid, substitutions=subs)
    """

    def __init__(
        self,
        base_url: str = "http://localhost:8080",
        db: str = "",
        password: str = "",
    ):
        """Build a client targeting one VoLCA server and one default database.

        Args:
            base_url: Server URL (no trailing slash needed).
            db: Default database name. Used by every operation that takes a
                ``dbName`` path capture; pass ``db_name=`` per call to override.
            password: Bearer token sent in the Authorization header on every
                request. The Server class reads this from ``server.password``
                in the engine TOML.
        """
        self.base_url = base_url.rstrip("/")
        self.db = db
        self._session = requests.Session()
        self._session.headers["Accept"] = "application/json"
        if password:
            self._session.headers["Authorization"] = f"Bearer {password}"
        # Lazily fetched on first _call() invocation.
        self._operations: dict[str, _Operation] | None = None
        # One-shot wire-compatibility gate (see _ensure_compatible).
        self._checked = False

    # -- Spec / dispatch plumbing --

    def _load_operations(self) -> dict[str, _Operation]:
        """Fetch the OpenAPI spec and build the dispatch table (cached)."""
        if self._operations is None:
            self._ensure_compatible()
            spec = self._json(self._session.get(f"{self.base_url}/api/v1/openapi.json"))
            self._operations = _parse_spec(spec)
        return self._operations

    def _ensure_compatible(self) -> None:
        """One-shot wire-compatibility gate, run before the first real call.

        Placed inside the spec-fetch branch so it fires once per client, right
        before we first depend on the engine's wire — and never for a client
        that was handed a preloaded operation table (the offline fixtures).
        ``get_version`` is a direct GET, so this does not recurse.
        """
        if self._checked:
            return
        _compat.check(self.get_version())
        self._checked = True

    def refresh_stubs(self) -> None:
        """Fetch the OpenAPI spec from the server and refresh the dispatch table.

        Also regenerates the `.pyi` type stubs in the installed pyvolca
        package directory so IDE autocomplete reflects the current engine.
        Useful when the engine is upgraded without reinstalling pyvolca.

        This is the explicit "the engine was upgraded" path — the likeliest
        place to meet a wire mismatch — so it runs the same one-shot gate as
        :meth:`_load_operations`, refusing a spec pyvolca can't decode.
        """
        self._ensure_compatible()
        spec = self._json(self._session.get(f"{self.base_url}/api/v1/openapi.json"))
        self._operations = _parse_spec(spec)
        from . import _stub_gen
        _stub_gen.write_stubs_for_spec(spec)

    def _call(
        self,
        operation_id: str,
        *,
        substitutions: list[dict] | None = None,
        **kwargs: Any,
    ) -> Any:
        """Dispatch an OpenAPI operation by ``operationId``.

        Path captures and query parameters come from the spec. Python
        kwarg names are canonicalized against the spec's parameter list
        (see :func:`_candidate_wire_names`). ``db_name`` defaults to the
        instance's ``self.db`` when the operation expects ``dbName`` and
        it wasn't explicitly passed.

        If ``substitutions`` is given and the spec's path supports POST
        with a ``SubstitutionRequest`` body, the operation is upgraded
        from GET to POST.
        """
        ops = self._load_operations()
        op = ops.get(operation_id)
        if op is None:
            raise VoLCAError(
                f"Unknown operationId {operation_id!r} (not in OpenAPI spec). "
                f"Is the engine outdated, or does this operation only exist in MCP?"
            )

        # Auto-inject db_name from instance state if the op needs it and
        # the caller didn't pass it explicitly.
        if "dbName" in op.path_params and "db_name" not in kwargs and "dbName" not in kwargs:
            if not self.db:
                raise VoLCAError(
                    f"Operation {operation_id!r} requires a database but "
                    f"Client(db=...) is empty. Pass db_name= explicitly or "
                    f"construct the client with db=...."
                )
            kwargs["db_name"] = self.db

        # Drop None-valued kwargs — they shouldn't become query string entries.
        kwargs = {k: v for k, v in kwargs.items() if v is not None}

        # Split kwargs into path captures vs. query params, canonicalizing names.
        path_values: dict[str, Any] = {}
        query_values: list[tuple[str, Any]] = []
        unknown: list[str] = []

        for py_name, value in kwargs.items():
            wire_name = _resolve_wire_name(py_name, op)
            if wire_name is None:
                unknown.append(py_name)
                continue
            if wire_name in op.path_params:
                path_values[wire_name] = value
            else:
                # Query param: list values emit repeated keys.
                formatted = _format_query_value(value)
                if isinstance(formatted, list):
                    for item in formatted:
                        query_values.append((wire_name, item))
                else:
                    query_values.append((wire_name, formatted))

        if unknown:
            raise VoLCAError(
                f"Operation {operation_id!r} got unknown kwargs: {sorted(unknown)}. "
                f"Accepted parameters: path={sorted(op.path_params)}, "
                f"query={sorted(op.query_params)}."
            )

        # Verify all required path captures were supplied. Anything missing
        # is a bug in the calling wrapper, but surface it clearly.
        missing_path = [p for p in op.path_params if p not in path_values]
        if missing_path:
            raise VoLCAError(
                f"Operation {operation_id!r} missing required path captures: "
                f"{missing_path}"
            )

        # Substitute path captures. The spec uses `{name}` placeholders.
        url_path = op.path_template
        for name, value in path_values.items():
            url_path = url_path.replace("{" + name + "}", str(value))
        url = self.base_url + url_path

        # Upgrade to POST when substitutions are supplied.
        method = op.method
        body: dict | None = None
        if substitutions:
            method = "POST"
            body = _substitution_body(substitutions)

        # Send.
        if method == "GET":
            r = self._session.get(url, params=query_values)
        elif method == "POST":
            r = self._session.post(url, params=query_values, json=body or {})
        elif method == "DELETE":
            r = self._session.delete(url, params=query_values)
        elif method == "PUT":
            r = self._session.put(url, params=query_values, json=body or {})
        else:
            raise VoLCAError(f"Unsupported HTTP method {method!r} for {operation_id!r}")

        return self._json(r)

    # -- Response parsing --

    @staticmethod
    def _json(r: requests.Response):
        """Parse JSON response, raising a clear error on failure."""
        try:
            r.raise_for_status()
        except requests.HTTPError:
            body = r.text[:200]
            raise VoLCAError(
                f"{r.status_code} {r.reason} for {r.request.method} {r.url}: {body}",
                status_code=r.status_code,
                body=r.text,
            ) from None
        if not r.content:
            raise VoLCAError(
                f"Empty response for {r.request.method} {r.url} (status {r.status_code})",
                status_code=r.status_code,
                body="",
            )
        try:
            return r.json()
        except requests.exceptions.JSONDecodeError:
            hint = ""
            if "<!DOCTYPE" in r.text[:50] or "<html" in r.text[:50]:
                if r.history:
                    hint = (
                        f" (redirected from {r.history[0].url} — "
                        "auth headers are dropped on redirect, try using https://)"
                    )
                else:
                    hint = " (got HTML — is the URL correct?)"
            raise VoLCAError(
                f"Non-JSON response for {r.request.method} {r.url} "
                f"(status {r.status_code}){hint}",
                status_code=r.status_code,
                body=r.text,
            ) from None

    # -- Session state --

    def use(self, db_name: str) -> "Client":
        """Return a new client targeting a different database.

        Shares the underlying HTTP session, dispatch table, and any other
        Client-level state with the original — only ``db`` is overridden.
        New fields added to :meth:`Client.__init__` propagate automatically
        (no manual mirror to keep in sync).
        """
        c = object.__new__(Client)
        c.__dict__ = self.__dict__.copy()
        c.db = db_name
        return c

    def get_version(self) -> ServerVersion:
        """Return server build metadata: version, git hash/tag, build target.

        Uses a direct HTTP call — ``/api/v1/version`` has no operationId
        since it predates the Resources ADT.
        """
        return ServerVersion.from_json(
            self._json(self._session.get(f"{self.base_url}/api/v1/version"))
        )

    # ------------------------------------------------------------------
    # Typed wrappers
    # ------------------------------------------------------------------
    #
    # Each wrapper is a 2–3 line thunk around self._call. The dispatcher
    # handles URL assembly and query encoding. Wrappers exist only to
    # provide:
    #
    #   - Python-idiomatic kwargs (snake_case) with type hints
    #   - Dataclass return types
    #   - IDE autocomplete for the common operations
    #
    # Operations without a hand-written wrapper are still reachable via
    # ``client.call(operation_id, **kw)`` — see the ``call`` method below.

    def call(self, operation_id: str, **kwargs: Any) -> Any:
        """Escape hatch: call any OpenAPI operation by operationId.

        Returns the raw JSON (no dataclass wrapping). Use this for
        operations that don't have an ergonomic wrapper yet, or for new
        endpoints added after the installed pyvolca was released.
        """
        return self._call(operation_id, **kwargs)

    # -- Database management --

    def list_databases(self) -> list[DatabaseInfo]:
        """List every database declared in the engine config.

        The typed entries carry ``depends_on``, so callers can derive
        cross-DB dependency sets from declared topology rather than
        hardcoding allowlists.
        """
        raw = self._call("list_databases")["databases"]
        return [DatabaseInfo.from_json(d) for d in raw]

    def load_database(self, db_name: str) -> dict:
        """Load a database into memory so it answers queries.

        Declared dependencies are loaded first; has no effect if the
        database is already loaded.
        """
        return self._call("load_database", db_name=db_name)

    def unload_database(self, db_name: str) -> dict:
        """Unload a database from memory to free RAM. The disk copy is kept.

        Refused if another loaded database still depends on it.
        """
        return self._call("unload_database", db_name=db_name)

    # -- Database write operations --
    #
    # These mutate a loaded database (copy / delete / relink / export /
    # dependency wiring). The engine does NOT register them as Resources, so
    # they carry no operationId and are unreachable through the OpenAPI
    # dispatcher. Each method therefore builds its URL directly, exactly like
    # load_database / unload_database above.
    #
    # The engine reports failures in-band as ``{"success": false, ...}`` (HTTP
    # 200) for some of these handlers, so _require_success surfaces those as
    # VoLCAError rather than letting a failed call look like a success.

    def _db(self, db_name: str | None) -> str:
        """Resolve the target database, falling back to ``self.db``.

        Raises VoLCAError when neither an explicit ``db_name`` nor a
        client-level default is available — never silently targets ``""``.
        """
        name = db_name or self.db
        if not name:
            raise VoLCAError(
                "No database specified and Client(db=...) is empty. "
                "Pass db_name= or construct the client with db=...."
            )
        return name

    @staticmethod
    def _require_success(payload: dict, action: str) -> dict:
        """Raise VoLCAError if the engine reported an in-band failure.

        Handlers that return ``{"success": false, "message": ...}`` with HTTP
        200 would otherwise look like a success. Surface the engine's own
        message instead of silently returning the failure envelope.
        """
        if payload.get("success") is False:
            raise VoLCAError(
                f"{action} failed: {payload.get('message', 'no message')}"
            )
        return payload

    def copy_database(self, new_name: str, db_name: str | None = None) -> dict:
        """Copy a loaded database in memory under a new name.

        ``new_name`` is a path segment; the source defaults to ``self.db``.
        Returns the engine's ``ActivateResponse`` dict
        (``{"success", "message", "database"?}``). Raises VoLCAError if the
        engine reports ``success=false``.
        """
        src = self._db(db_name)
        payload = self._json(
            self._session.post(f"{self.base_url}/api/v1/db/{src}/copy/{new_name}")
        )
        return self._require_success(payload, "copy_database")

    def delete_activities(
        self,
        *,
        name: str = "",
        location: str = "",
        product: str = "",
        classifications: list[dict | tuple] | None = None,
        exact: bool = False,
        keep: list[str] | None = None,
        extra: list[str] | None = None,
        db_name: str | None = None,
    ) -> dict:
        """Delete activities selected by filter, sparing/adding explicit ids.

        Builds a ``DeleteSelectionRequest``: the filter fields select the whole
        matching set, ``keep`` spares matched process ids, and ``extra`` adds
        ones the filter missed. ``classifications`` is a list of
        ``{"system", "value", "exact"}`` dicts or ``(system, value, exact)``
        tuples.

        Returns the ``DeleteSelectionResponse`` dict
        (``{"success", "message", "deleted"}``); raises VoLCAError on
        ``success=false``.
        """
        # Omit blank filters entirely: sending "name":"" makes the engine treat
        # an empty string as a real (unsatisfiable) name filter, so a
        # product-only delete would match nothing and still report success.
        body: dict = {
            "classifications": [
                _coerce_class_filter(c) for c in (classifications or [])
            ],
            "exact": exact,
            "keep": keep or [],
            "extra": extra or [],
        }
        for key, value in (("name", name), ("location", location), ("product", product)):
            if value:
                body[key] = value
        target = self._db(db_name)
        payload = self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/delete", json=body
            )
        )
        return self._require_success(payload, "delete_activities")

    def relink(
        self, dep_db: str, mapping_csv: str, db_name: str | None = None
    ) -> dict:
        """Re-link a database against a dependency using a name→name alias CSV.

        ``mapping_csv`` is the CSV *text* (header row + source/target columns),
        sent inline so the engine needs no filesystem access. Returns the
        ``RelinkResponse`` dict (``{"dbName", "unresolvedBefore",
        "unresolvedAfter", "crossDBLinks", "dependsOn"}``).
        """
        target = self._db(db_name)
        body = {"depDb": dep_db, "mappingCsv": mapping_csv}
        return self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/relink", json=body
            )
        )

    def relink_from_file(
        self, dep_db: str, mapping_path: str, db_name: str | None = None
    ) -> dict:
        """Read a mapping CSV file and call :meth:`relink` with its text."""
        csv_text = Path(mapping_path).read_text(encoding="utf-8")
        return self.relink(dep_db, csv_text, db_name=db_name)

    def export_database(self, fmt: str, db_name: str | None = None) -> bytes:
        """Export a loaded database, returning the serialized bytes.

        ``fmt`` is one of ``simapro|ecospold1|ecospold2|ilcd|brightway`` —
        validated client-side; an unknown value raises VoLCAError before any
        request. Single-file formats carry their bytes directly; EcoSpold 2 /
        ILCD multi-file trees come back zipped.

        The engine streams the payload as raw bytes. Best-effort approximation
        warnings arrive in the ``X-Volca-Export-Warnings`` response header
        (percent-encoded, newline-joined) and are surfaced through
        :mod:`warnings`. Raises VoLCAError on an HTTP error.
        """
        fmt_norm = fmt.strip().lower()
        if fmt_norm not in _EXPORT_FORMATS:
            raise VoLCAError(
                f"unknown export format: {fmt!r} "
                f"(expected {'|'.join(sorted(_EXPORT_FORMATS))})"
            )
        target = self._db(db_name)
        resp = self._session.post(
            f"{self.base_url}/api/v1/db/{target}/export",
            json={"format": fmt_norm},
        )
        if resp.status_code >= 400:
            raise VoLCAError(
                f"export_database failed (HTTP {resp.status_code}): "
                f"{resp.text[:500]}"
            )
        header = resp.headers.get("X-Volca-Export-Warnings", "")
        for line in urllib.parse.unquote(header).split("\n"):
            if line:
                warnings.warn(line, stacklevel=2)
        return resp.content

    def export_to_file(
        self, fmt: str, out_path: str, db_name: str | None = None
    ) -> None:
        """Export a database (see :meth:`export_database`) and write it to a file."""
        Path(out_path).write_bytes(self.export_database(fmt, db_name=db_name))

    def add_dependency(self, dep_name: str, db_name: str | None = None) -> dict:
        """Declare ``dep_name`` as a dependency of the target database.

        Returns the engine's ``DatabaseSetupInfo`` dict describing the updated
        dependency topology.
        """
        target = self._db(db_name)
        return self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/add-dependency/{dep_name}"
            )
        )

    def remove_dependency(self, dep_name: str, db_name: str | None = None) -> dict:
        """Remove ``dep_name`` from the target database's dependencies.

        Returns the updated ``DatabaseSetupInfo`` dict.
        """
        target = self._db(db_name)
        return self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/remove-dependency/{dep_name}"
            )
        )

    def list_presets(self) -> list[Preset]:
        """List classification presets configured in this instance.

        Each :class:`Preset` carries its ``filters`` (list of
        :class:`PresetFilter` triples). Apply by passing ``preset=p.name``
        to filtering endpoints.
        """
        return [Preset.from_json(p) for p in self._call("list_presets")]

    # -- Search --

    def search_activities(
        self,
        name: str | None = None,
        *,
        geo: str | None = None,
        product: str | None = None,
        preset: str | None = None,
        classification: str | None = None,
        classification_value: str | None = None,
        page: int | None = None,
        page_size: int | None = None,
        limit: int | None = None,
        offset: int | None = None,
        exact: bool = False,
    ) -> SearchResults[Activity]:
        """Search activities in the current database.

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
            page: 1-based page number. Must be paired with ``page_size`` —
                offset cannot be derived from page alone.
            page_size: Items per page (becomes the wire-level ``limit``).
                Alone (no ``page``) means "page 1 with this size".
            limit: Wire-level cap on returned items. Prefer ``page_size``.
            offset: Wire-level starting index. Prefer ``page`` + ``page_size``.
            exact: When True, ``name`` and ``product`` are matched exactly.

        Returns:
            :class:`SearchResults[Activity]` — iterable across all pages.
        """
        wire_limit, wire_offset = _resolve_page_args(page, page_size, limit, offset)
        common: dict[str, Any] = dict(
            name=name,
            geo=geo,
            product=product,
            preset=preset,
            classification=classification,
            classification_value=classification_value,
            exact=exact,
        )

        def fetch(o: int, l: int | None) -> dict:
            return self._call("search_activities", **common, limit=l, offset=o)

        raw = fetch(wire_offset, wire_limit)
        return SearchResults.from_raw(raw, parse=Activity.from_json, fetch=fetch)

    def search_flows(
        self,
        query: str | None = None,
        *,
        page: int | None = None,
        page_size: int | None = None,
        limit: int | None = None,
        offset: int | None = None,
    ) -> SearchResults[Flow]:
        """Search flows (technosphere products and biosphere flows) in the current database.

        Returns a paginated :class:`SearchResults[Flow]` — iterate to walk
        every match across all pages, or use ``.page(n)`` for explicit
        access. See :meth:`search_activities` for the pagination contract.

        Args:
            query: Substring matched case-insensitively against flow names.
            page / page_size: Web-style pagination; convert to wire-level
                ``offset`` / ``limit``.
            limit / offset: Wire-level escape hatch.
        """
        wire_limit, wire_offset = _resolve_page_args(page, page_size, limit, offset)

        def fetch(o: int, l: int | None) -> dict:
            return self._call("search_flows", q=query, limit=l, offset=o)

        raw = fetch(wire_offset, wire_limit)
        return SearchResults.from_raw(raw, parse=Flow.from_json, fetch=fetch)

    def list_classifications(self) -> list[ClassificationSystem]:
        """List classification systems and their values for the current database.

        ``ClassificationSystem.activity_count`` tells how widely each system
        is populated — useful for picking a filter dimension with enough
        signal.
        """
        return [ClassificationSystem.from_json(c) for c in self._call("list_classifications")]

    # -- Activity details --

    def get_activity(self, process_id: str) -> ActivityDetail:
        """Fetch an activity's full detail.

        Returns a typed ActivityDetail. Use ``act.inputs`` / ``act.outputs`` /
        ``act.technosphere_inputs`` to filter exchanges instead of walking
        ``act.exchanges`` directly.
        """
        return ActivityDetail.from_json(self._call("get_activity", process_id=process_id))

    def get_inputs(self, process_id: str) -> list[Exchange]:
        """Return the input exchanges of an activity (richer metadata than ``get_activity``).

        Uses a direct HTTP call because ``/inputs`` has no operationId
        (it's a non-Resources auxiliary endpoint).
        """
        raw = self._json(
            self._session.get(
                f"{self.base_url}/api/v1/db/{self.db}/activity/{process_id}/inputs"
            )
        )
        return [parse_exchange_detail(e) for e in raw]

    def get_outputs(self, process_id: str) -> list[Exchange]:
        """Return the output exchanges of an activity. See :meth:`get_inputs` for notes."""
        raw = self._json(
            self._session.get(
                f"{self.base_url}/api/v1/db/{self.db}/activity/{process_id}/outputs"
            )
        )
        return [parse_exchange_detail(e) for e in raw]

    # -- Supply chain --

    def get_supply_chain(
        self,
        process_id: str,
        *,
        name: str | None = None,
        location: str | None = None,
        limit: int | None = None,
        min_quantity: float | None = None,
        max_depth: int | None = None,
        preset: str | None = None,
        classification_filters: list[ClassificationFilter] | None = None,
        substitutions: list[SubstitutionLike] | None = None,
        include_edges: bool | None = None,
    ) -> SupplyChain:
        """Get the flat supply chain of an activity.

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
        """
        classifications = [f.system for f in classification_filters or []]
        classification_values = [f.value for f in classification_filters or []]
        classification_modes = [f.mode.value for f in classification_filters or []]
        raw = self._call(
            "get_supply_chain",
            process_id=process_id,
            name=name,
            location=location,
            limit=limit,
            min_quantity=min_quantity,
            max_depth=max_depth,
            preset=preset,
            classification=classifications or None,
            classification_value=classification_values or None,
            classification_mode=classification_modes or None,
            include_edges=include_edges,
            substitutions=substitutions,
        )
        return SupplyChain.from_json(raw)

    # -- Aggregate primitive --

    def aggregate(
        self,
        process_id: str,
        scope: AggregateScope | str,
        *,
        is_input: bool | None = None,
        max_depth: int | None = None,
        filter_name: str | None = None,
        filter_name_not: list[str] | str | None = None,
        filter_unit: str | None = None,
        preset: str | None = None,
        filter_classification: list[ClassificationFilter] | None = None,
        filter_target_name: str | None = None,
        filter_is_reference: bool | None = None,
        group_by: str | None = None,
        aggregate: AggregateOp | str | None = None,
    ) -> AggregateResult:
        """SQL-group-by aggregation over direct exchanges, supply chain, or biosphere flows.

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
        """
        # filter_classification goes over the wire as "System=Value[:exact]" strings.
        if filter_classification:
            filter_strings = [
                f"{f.system}={f.value}" + (":exact" if f.mode is MatchMode.EXACT else "")
                for f in filter_classification
            ]
        else:
            filter_strings = None
        # filter_name_not: accept list or comma-string, send as comma-string.
        if isinstance(filter_name_not, list):
            filter_name_not_csv: str | None = ",".join(filter_name_not)
        else:
            filter_name_not_csv = filter_name_not
        raw = self._call(
            "aggregate",
            process_id=process_id,
            scope=scope,
            is_input=is_input,
            max_depth=max_depth,
            filter_name=filter_name,
            filter_name_not=filter_name_not_csv,
            filter_unit=filter_unit,
            preset=preset,
            filter_classification=filter_strings,
            filter_target_name=filter_target_name,
            filter_is_reference=filter_is_reference,
            group_by=group_by,
            aggregate=aggregate,
        )
        return AggregateResult.from_json(raw)

    # -- Consumers (reverse supply chain) --

    def get_consumers(
        self,
        process_id: str,
        *,
        name: str | None = None,
        location: str | None = None,
        product: str | None = None,
        preset: str | None = None,
        classification_filters: list[ClassificationFilter] | None = None,
        page: int | None = None,
        page_size: int | None = None,
        limit: int | None = None,
        offset: int | None = None,
        max_depth: int | None = None,
        include_edges: bool = False,
    ) -> ConsumersResponse:
        """Find all activities that transitively consume this supplier.

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
        """
        classifications = [f.system for f in classification_filters or []]
        classification_values = [f.value for f in classification_filters or []]
        classification_modes = [f.mode.value for f in classification_filters or []]
        wire_limit, wire_offset = _resolve_page_args(page, page_size, limit, offset)
        common: dict[str, Any] = dict(
            process_id=process_id,
            name=name,
            location=location,
            product=product,
            preset=preset,
            classification=classifications or None,
            classification_value=classification_values or None,
            classification_mode=classification_modes or None,
            max_depth=max_depth,
            include_edges=include_edges,
        )

        def fetch(o: int, l: int | None) -> dict:
            return self._call("get_consumers", **common, limit=l, offset=o)

        raw = fetch(wire_offset, wire_limit)
        return ConsumersResponse.from_json(raw, fetch=fetch)

    def get_path_to(self, process_id: str, target: str) -> PathResult:
        """Find the shortest upstream path from process to first activity whose name matches target.

        Returns a PathResult whose path is ordered root → target. Each step
        includes cumulative_quantity, scaling_factor, and (except the root)
        local_step_ratio.
        """
        return PathResult.from_json(
            self._call("get_path_to", process_id=process_id, target=target)
        )

    # -- Tree (SPA-only endpoint, no operationId — direct HTTP) --

    def get_tree(self, process_id: str) -> dict:
        """Fetch the recursive activity tree used by the analysis SPA.

        ``/tree`` has no operationId in the OpenAPI spec — it's kept for the
        SPA's lazy-expanding graph widget and intentionally not exposed as
        a Resource. Included here as a direct HTTP call for scripts that
        need the same shape.
        """
        return self._json(
            self._session.get(
                f"{self.base_url}/api/v1/db/{self.db}/activity/{process_id}/tree"
            )
        )

    # -- Inventory & impacts --

    def get_inventory(
        self,
        process_id: str,
        *,
        flow: str | None = None,
        limit: int | None = None,
        substitutions: list[SubstitutionLike] | None = None,
    ) -> InventoryResult:
        """Compute the life-cycle inventory (cumulative biosphere flows) for an activity.

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
        """
        return InventoryResult.from_json(
            self._call(
                "get_inventory",
                process_id=process_id,
                flow=flow,
                limit=limit,
                substitutions=substitutions,
            )
        )

    def get_impacts(
        self,
        process_id: str,
        method_id: str,
        *,
        collection: str = "methods",
        top_flows: int | None = None,
        substitutions: list[SubstitutionLike] | None = None,
    ) -> LCIAResult:
        """Compute the LCIA score for a single impact category on an activity.

        Use :meth:`get_impacts_batch` to retrieve every category in a method
        collection at once (and any configured scoring sets).

        Args:
            collection: Method collection name. Defaults to ``"methods"`` for
                single-method calls; most engines expose methods under a
                single collection.
            top_flows: Max top contributing flows to return (default 5).
        """
        return LCIAResult.from_json(
            self._call(
                "get_impacts",
                process_id=process_id,
                collection=collection,
                method_id=method_id,
                top_flows=top_flows,
                substitutions=substitutions,
            )
        )

    def get_impacts_batch(
        self,
        process_id: str,
        *,
        collection: str = "methods",
        substitutions: list[SubstitutionLike] | None = None,
    ) -> LCIABatchResult:
        """Compute LCIA for every impact category in a collection, in one call.

        The response carries the per-method :class:`LCIAResult` list plus any
        formula-based scoring sets declared in the engine config (PEF, ECS…).
        ``scoring_indicators`` gives the per-variable breakdown of each
        scoring set, pre-multiplied by the set's ``displayMultiplier``.

        Uses a direct HTTP call: the batch endpoint has no operationId in the
        OpenAPI spec (the dispatcher primary is the single-method variant), so
        this wrapper bypasses ``_call`` and builds the URL itself.
        """
        if not self.db:
            raise VoLCAError(
                "get_impacts_batch requires a database; construct Client(db=...)."
            )
        url = (
            f"{self.base_url}/api/v1/db/{self.db}/activity/{process_id}"
            f"/impacts/{collection}"
        )
        if substitutions:
            r = self._session.post(url, json=_substitution_body(substitutions))
        else:
            r = self._session.get(url)
        return LCIABatchResult.from_json(self._json(r))

    # -- Methods --

    def list_methods(self) -> list[Method]:
        """List every LCIA method available in the engine.

        Each :class:`Method` carries ``id``, ``name``, ``category``, ``unit``,
        ``factor_count``, and the parent ``collection``. Pass ``m.id`` to
        :meth:`get_impacts` as ``method_id``.
        """
        return [Method.from_json(m) for m in self._call("list_methods")]

    def get_flow_mapping(self, method_id: str) -> FlowMapping:
        """Get the characterization-factor-to-database-flow mapping coverage.

        :class:`FlowMapping.coverage_pct` summarises how many of the DB's
        biosphere flows the method has a CF for; ``flows`` is the per-flow
        breakdown including unmatched rows (``cf_value=None``).
        """
        return FlowMapping.from_json(self._call("get_flow_mapping", method_id=method_id))

    def get_characterization(
        self,
        method_id: str,
        *,
        flow: str | None = None,
        limit: int | None = None,
    ) -> CharacterizationResult:
        """Look up characterization factors for a method matched to database flows.

        Returns a :class:`CharacterizationResult` carrying ``matches`` (total
        rows the filter selected) and ``shown`` (rows actually returned under
        ``limit``). Check ``result.has_more`` to detect truncation.
        """
        return CharacterizationResult.from_json(
            self._call("get_characterization", method_id=method_id, flow=flow, limit=limit)
        )

    def get_contributing_flows(
        self,
        process_id: str,
        method_id: str,
        *,
        collection: str = "methods",
        limit: int | None = None,
    ) -> ContributingFlows:
        """Which elementary flows drive a given impact category.

        Returns a :class:`ContributingFlows`. Caveat: the engine does not
        report the total flow count, so pyvolca cannot derive ``has_more``
        from the response. Pass a generous ``limit`` if you need exhaustive
        coverage and inspect ``share_pct`` totals.
        """
        return ContributingFlows.from_json(
            self._call(
                "get_contributing_flows",
                process_id=process_id,
                collection=collection,
                method_id=method_id,
                limit=limit,
            )
        )

    def get_contributing_activities(
        self,
        process_id: str,
        method_id: str,
        *,
        collection: str = "methods",
        limit: int | None = None,
    ) -> ContributingActivities:
        """Which upstream activities drive a given impact category.

        Same engine-side limitation as :meth:`get_contributing_flows`: no
        total exposed, so ``has_more`` cannot be derived. Inspect
        ``share_pct`` totals to gauge coverage.
        """
        return ContributingActivities.from_json(self._call(
            "get_contributing_activities",
            process_id=process_id,
            collection=collection,
            method_id=method_id,
            limit=limit,
        ))


def _resolve_wire_name(py_name: str, op: _Operation) -> str | None:
    """Match a Python kwarg name to a spec parameter name, or return None."""
    spec_params = op.wire_names
    for candidate in _candidate_wire_names(py_name):
        if candidate in spec_params:
            return candidate
    return None
