"""Offline orchestration tests for Client.resolve_activities.

search_activities carries its own wire tests; these pin the batch
orchestration: total mapping, dedup, argument passthrough, the client-side
exactness guard, and fail-loud on errors.
"""

from __future__ import annotations

from types import SimpleNamespace
from unittest import mock

import pytest

from volca.client import Client, VoLCAError


def _act(name: str, product: str = "p") -> SimpleNamespace:
    return SimpleNamespace(activity_name=name, product_name=product)


@pytest.fixture()
def client() -> Client:
    c = Client(base_url="http://test.local")
    c.search_activities = mock.Mock(return_value=mock.Mock(results=[]))  # type: ignore[method-assign]
    return c


def test_mapping_is_total_and_ordered(client: Client):
    wheat = _act("Wheat")
    client.search_activities.side_effect = lambda **kw: mock.Mock(
        results=[wheat] if kw.get("name") == "Wheat" else []
    )
    out = client.resolve_activities(["Rye", "Wheat"])
    assert list(out) == ["Rye", "Wheat"]
    assert out == {"Rye": [], "Wheat": [wheat]}


def test_arguments_reach_search(client: Client):
    client.resolve_activities(["Wheat"], geo="FR", exact=False, limit=3)
    client.search_activities.assert_called_once_with(
        name="Wheat", geo="FR", exact=False, limit=3
    )


def test_by_product_searches_product(client: Client):
    client.resolve_activities(["Wheat flour"], by="product")
    client.search_activities.assert_called_once_with(
        product="Wheat flour", geo=None, exact=True, limit=5
    )


def test_exact_drops_near_misses_from_lax_engines(client: Client):
    """An engine that ignores exact= returns substring matches; the client
    re-checks equality (casefold) so only the true match survives."""
    client.search_activities.return_value = mock.Mock(
        results=[
            _act("m", product="Product A"),
            _act("m", product="produCt c"),
            _act("m", product="product C, organic"),
        ]
    )
    out = client.resolve_activities(["Product C"], by="product")
    assert [a.product_name for a in out["Product C"]] == ["produCt c"]


def test_fuzzy_keeps_engine_ranking(client: Client):
    ranked = [_act("Wheat flour"), _act("Wheat, grain")]
    client.search_activities.return_value = mock.Mock(results=ranked)
    assert client.resolve_activities(["wheat"], exact=False) == {"wheat": ranked}


def test_duplicates_searched_once(client: Client):
    out = client.resolve_activities(["Wheat", "Wheat", "Rye"])
    assert client.search_activities.call_count == 2
    assert list(out) == ["Wheat", "Rye"]


def test_invalid_by_is_refused(client: Client):
    with pytest.raises(VoLCAError, match="'name' or 'product'"):
        client.resolve_activities(["Wheat"], by="location")  # type: ignore[arg-type]
    client.search_activities.assert_not_called()


def test_empty_input_makes_no_calls(client: Client):
    assert client.resolve_activities([]) == {}
    client.search_activities.assert_not_called()


def test_search_error_propagates(client: Client):
    client.search_activities.side_effect = VoLCAError("boom", status_code=500)
    with pytest.raises(VoLCAError, match="boom"):
        client.resolve_activities(["Wheat", "Rye"])
