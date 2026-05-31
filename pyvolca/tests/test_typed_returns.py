"""Tests for the 0.5.0 typed-return wave (Pattern 3) and pagination surfacing.

Covers:
- :class:`SupplyChain.has_more` derived flag
- :class:`CharacterizationResult.has_more` derived flag
- Typed dataclass returns: :class:`Method`, :class:`ClassificationSystem`,
  :class:`Preset`, :class:`ServerVersion`, :class:`InventoryResult`,
  :class:`FlowMapping`, :class:`ContributingFlows`, :class:`ContributingActivities`
- :class:`Substitution` dataclass and dict-form back-compat
- :class:`AggregateScope` / :class:`AggregateOp` enum acceptance
- :class:`DatabaseStatus`, :class:`TechRole`, :class:`BioDirection`
  StrEnum string parity (``f"{X.A}"`` returns ``"a"``)
"""

from __future__ import annotations

import pytest

from volca import (
    Activity,
    AggregateOp,
    AggregateScope,
    BioDirection,
    CharacterizationResult,
    ClassificationSystem,
    ContributingActivities,
    ContributingFlows,
    DatabaseStatus,
    FlowMapping,
    InventoryResult,
    Method,
    Preset,
    ServerVersion,
    Substitution,
    SupplyChain,
    SupplyChainEdge,
    SupplyChainEntry,
    TechRole,
    VoLCAError,
)
from volca.client import _substitution_body


# ---------------------------------------------------------------------------
# Pattern 1 — has_more surfacing
# ---------------------------------------------------------------------------


class TestSupplyChainHasMore:
    def _root(self) -> Activity:
        return Activity(
            process_id="a", name="A", location="FR",
            product="p", product_amount=1.0, product_unit="kg",
        )

    def _entry(self, name: str) -> SupplyChainEntry:
        return SupplyChainEntry(
            process_id=f"{name}_pid", name=name, location="FR",
            quantity=1.0, unit="kg", scaling_factor=1.0,
        )

    def test_has_more_false_when_entries_match_filtered(self):
        sc = SupplyChain(
            root=self._root(), total_activities=100, filtered_activities=3,
            entries=[self._entry("a"), self._entry("b"), self._entry("c")],
        )
        assert sc.has_more is False

    def test_has_more_true_when_truncated(self):
        sc = SupplyChain(
            root=self._root(), total_activities=100, filtered_activities=50,
            entries=[self._entry("a"), self._entry("b")],  # only 2 of 50
        )
        assert sc.has_more is True


class TestCharacterizationHasMore:
    def test_has_more_false_when_shown_equals_matches(self):
        result = CharacterizationResult(
            method="EF v3.1", unit="kg CO2 eq", matches=42, shown=42, factors=[],
        )
        assert result.has_more is False

    def test_has_more_true_when_truncated(self):
        result = CharacterizationResult(
            method="EF v3.1", unit="kg CO2 eq", matches=420, shown=50, factors=[],
        )
        assert result.has_more is True


# ---------------------------------------------------------------------------
# Pattern 3a — typed list returns: client wiring
# ---------------------------------------------------------------------------


class TestListMethodsTyped:
    def test_list_methods_returns_typed_method(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response([
            {"id": "EF3.1-cc", "name": "Climate change", "category": "climate",
             "unit": "kg CO2 eq", "factorCount": 420, "collection": "ef-31"},
        ])
        out = client.list_methods()
        assert isinstance(out, list)
        assert isinstance(out[0], Method)
        assert out[0].id == "EF3.1-cc"
        assert out[0].factor_count == 420
        assert out[0].collection == "ef-31"


class TestListClassificationsTyped:
    def test_list_classifications_returns_typed(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response([
            {"name": "ISIC rev.4 ecoinvent", "values": ["1061", "1071"], "activityCount": 200},
        ])
        out = client.list_classifications()
        assert isinstance(out[0], ClassificationSystem)
        assert out[0].name == "ISIC rev.4 ecoinvent"
        assert out[0].values == ["1061", "1071"]
        assert out[0].activity_count == 200


class TestListPresetsTyped:
    def test_list_presets_returns_typed(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response([
            {"name": "food", "label": "Food",
             "description": "Edible products",
             "filters": [{"system": "Category", "value": "Food", "mode": "exact"}]},
        ])
        out = client.list_presets()
        assert isinstance(out[0], Preset)
        assert out[0].name == "food"
        assert out[0].filters[0].system == "Category"
        # The preset filter's mode is a MatchMode enum, not a bare string.
        from volca import MatchMode
        assert out[0].filters[0].mode is MatchMode.EXACT


class TestServerVersionTyped:
    def test_get_version_returns_typed(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({
            "version": "0.5.0", "gitHash": "abc123",
            "gitTag": "pyvolca-v0.5.0", "buildTarget": "x86_64-linux",
        })
        v = client.get_version()
        assert isinstance(v, ServerVersion)
        assert v.version == "0.5.0"
        assert v.build_target == "x86_64-linux"

    def test_git_tag_empty_becomes_none(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({
            "version": "0.5.0", "gitHash": "abc123",
            "gitTag": "", "buildTarget": "x86_64-linux",
        })
        v = client.get_version()
        assert v.git_tag is None


# ---------------------------------------------------------------------------
# Pattern 3b — Substitution dataclass
# ---------------------------------------------------------------------------


class TestSubstitution:
    def test_to_wire_uses_stripped_keys(self):
        s = Substitution(from_pid="old_pid", to_pid="new_pid", consumer="downstream_pid")
        assert s.to_wire() == {
            "from": "old_pid",
            "to": "new_pid",
            "consumer": "downstream_pid",
        }

    def test_substitution_body_accepts_typed(self):
        s = Substitution(from_pid="A", to_pid="B", consumer="C")
        body = _substitution_body([s])
        assert body == {"substitutions": [
            {"from": "A", "to": "B", "consumer": "C"},
        ]}

    def test_substitution_body_accepts_dict_form(self):
        body = _substitution_body([{"from": "A", "to": "B", "consumer": "C"}])
        assert body == {"substitutions": [
            {"from": "A", "to": "B", "consumer": "C"},
        ]}

    def test_substitution_dict_typo_raises_locally(self):
        # "comsumer" is a typo — caught before hitting the engine.
        with pytest.raises(VoLCAError, match="missing keys"):
            _substitution_body([{"from": "A", "to": "B", "comsumer": "C"}])

    def test_substitution_is_frozen_and_hashable(self):
        s = Substitution(from_pid="A", to_pid="B", consumer="C")
        # Frozen — can be used as a set/dict key.
        assert {s}  # builds without error


class TestSupplyChainEdge:
    def test_from_json_captures_db_endpoints(self):
        # edgeFromDb/edgeToDb are required to route edges across databases.
        edge = SupplyChainEdge.from_json({
            "edgeFrom": "supplier_pid",
            "edgeFromDb": "ecoinvent",
            "edgeTo": "consumer_pid",
            "edgeToDb": "agribalyse",
            "edgeAmount": 0.5,
        })
        assert edge == SupplyChainEdge(
            from_id="supplier_pid",
            from_db="ecoinvent",
            to_id="consumer_pid",
            to_db="agribalyse",
            amount=0.5,
        )


# ---------------------------------------------------------------------------
# Pattern 2 — StrEnums: str equality + value formatting
# ---------------------------------------------------------------------------


class TestStrEnumFormatting:
    """StrEnums must format as their wire value (not ``EnumName.MEMBER``).

    Critical for callers using f-strings to log status / scope / role: the
    wire value is what humans expect to see.
    """

    def test_database_status_formats_as_value(self):
        assert f"{DatabaseStatus.LOADED}" == "loaded"
        assert str(DatabaseStatus.LOADED) == "loaded"

    def test_database_status_equals_raw_string(self):
        # str inheritance: equality with the wire value still works.
        assert DatabaseStatus.LOADED == "loaded"

    def test_tech_role_formats_as_value(self):
        assert f"{TechRole.INPUT}" == "Input"
        assert TechRole.REFERENCE_PRODUCT == "ReferenceProduct"

    def test_bio_direction_formats_as_value(self):
        assert f"{BioDirection.RESOURCE}" == "Resource"
        assert BioDirection.EMISSION == "Emission"

    def test_aggregate_scope_formats_as_value(self):
        assert f"{AggregateScope.SUPPLY_CHAIN}" == "supply_chain"

    def test_aggregate_op_formats_as_value(self):
        assert f"{AggregateOp.SUM_QUANTITY}" == "sum_quantity"


class TestAggregateAcceptsEnumOrString:
    def test_aggregate_accepts_enum(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({
            "scope": "biosphere", "filteredTotal": 1.0, "filteredCount": 1,
            "groups": [],
        })
        client.aggregate("pid", AggregateScope.BIOSPHERE, aggregate=AggregateOp.COUNT)
        params = dict(session.get.call_args[1]["params"])
        assert params["scope"] == "biosphere"
        assert params["aggregate"] == "count"

    def test_aggregate_accepts_raw_string(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({
            "scope": "direct", "filteredTotal": 1.0, "filteredCount": 1,
            "groups": [],
        })
        client.aggregate("pid", "direct", aggregate="share")
        params = dict(session.get.call_args[1]["params"])
        assert params["scope"] == "direct"
        assert params["aggregate"] == "share"


# ---------------------------------------------------------------------------
# Pattern 3a — InventoryResult parsing
# ---------------------------------------------------------------------------


class TestInventoryParsing:
    def test_from_json_unpacks_metadata_flows_statistics(self):
        result = InventoryResult.from_json({
            "metadata": {
                "rootActivity": {
                    "processId": "p", "name": "n", "location": "FR",
                    "product": "p", "productAmount": 1.0, "productUnit": "kg",
                },
                "totalFlows": 3, "emissionFlows": 2, "resourceFlows": 1,
            },
            "flows": [
                {
                    "flow": {"id": "f1", "name": "CO2", "compartment": {"name": "air", "sub": "urban air"}},
                    "quantity": 0.5, "unitName": "kg", "isEmission": True, "category": "air/urban air",
                },
            ],
            "statistics": {
                "totalQuantity": 1.5, "emissionQuantity": 0.5, "resourceQuantity": 1.0,
                "topCategories": [["air/urban air", 1]],
            },
        })
        assert result.root.process_id == "p"
        assert result.total_flows == 3
        assert result.flows[0].flow_name == "CO2"
        assert result.flows[0].compartment == "air/urban air"
        assert result.statistics.total_quantity == 1.5
        assert result.statistics.top_categories == [("air/urban air", 1)]


# ---------------------------------------------------------------------------
# Pattern 3a — FlowMapping with derived coverage_pct
# ---------------------------------------------------------------------------


class TestFlowMapping:
    def test_coverage_pct_derived(self):
        m = FlowMapping(method_name="EF", method_unit="kg CO2 eq",
                        total_flows=200, matched_flows=180, flows=[])
        assert m.coverage_pct == pytest.approx(90.0)

    def test_coverage_pct_zero_safe(self):
        m = FlowMapping(method_name="EF", method_unit="kg CO2 eq",
                        total_flows=0, matched_flows=0, flows=[])
        assert m.coverage_pct == 0.0


# ---------------------------------------------------------------------------
# Pattern 3a — ContributingFlows / ContributingActivities parsing
# ---------------------------------------------------------------------------


class TestClientUseSharesState:
    """Client.use(db) must share session and dispatch table with the parent.

    Previously the implementation hardcoded specific fields, so a new field
    on __init__ would silently fail to propagate. The 0.5.0 implementation
    uses ``__dict__.copy()`` so new fields propagate by construction.
    """

    def test_use_returns_new_client_with_overridden_db(self, mocked_client):
        client, _ = mocked_client
        derived = client.use("other_db")
        assert derived.db == "other_db"
        assert client.db != derived.db

    def test_use_shares_session_and_operations(self, mocked_client):
        client, session = mocked_client
        derived = client.use("other_db")
        # Same underlying objects — no spec re-fetch, no new pool.
        assert derived._session is session
        assert derived._operations is client._operations

    def test_use_propagates_new_attributes(self, mocked_client):
        """Any attribute added to the parent is reflected on the copy."""
        client, _ = mocked_client
        client._test_marker = "abc"
        derived = client.use("other_db")
        assert derived._test_marker == "abc"


class TestSearchResultsStrictMode:
    """from_raw must require wire keys when a fetch is wired.

    The pre-0.5.0 behavior defaulted ``total``/``limit`` to ``len(items)``
    if missing, which would silently undercount a real engine response. In
    production (fetch wired), this now raises so schema drift surfaces.
    """

    def test_missing_total_raises_when_fetch_wired(self):
        from volca import Activity, SearchResults

        with pytest.raises(ValueError, match="missing required keys"):
            SearchResults.from_raw(
                {"results": [], "offset": 0, "limit": 20, "hasMore": False},
                parse=Activity.from_json,
                fetch=lambda o, l: {},
            )

    def test_missing_keys_tolerated_in_detached_fixtures(self):
        from volca import Activity, SearchResults

        # No fetch — caller is building a fixture by hand. Permissive defaults.
        sr = SearchResults.from_raw(
            {"results": []},
            parse=Activity.from_json,
        )
        assert sr.total == 0
        assert sr.has_more is False


class TestContributingParsing:
    def test_contributing_flows_from_json(self):
        cf = ContributingFlows.from_json({
            "method": "EF v3.1", "unit": "kg CO2 eq", "totalScore": 0.82,
            "topFlows": [
                {"flowName": "CO2", "contribution": 0.4, "sharePct": 48.0,
                 "flowId": "ef-co2", "category": "air", "cfValue": 1.0},
            ],
        })
        assert cf.method == "EF v3.1"
        assert cf.total_score == 0.82
        assert cf.top_flows[0].flow_name == "CO2"

    def test_contributing_activities_from_json(self):
        ca = ContributingActivities.from_json({
            "method": "EF v3.1", "unit": "kg CO2 eq", "totalScore": 0.82,
            "activities": [
                {"processId": "p", "activityName": "Farm", "productName": "wheat",
                 "location": "FR", "contribution": 0.31, "sharePct": 38.2},
            ],
        })
        assert ca.activities[0].activity_name == "Farm"
        assert ca.activities[0].share_pct == 38.2
