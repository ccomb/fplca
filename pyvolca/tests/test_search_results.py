"""Tests for :class:`volca.SearchResults` — the lazy-paginated wire envelope."""

from __future__ import annotations

import pytest

from volca import Activity, SearchResults


def _activity_dict(name: str) -> dict:
    return {
        "processId": name,
        "name": name,
        "location": "FR",
        "product": "p",
        "productAmount": 1.0,
        "productUnit": "kg",
    }


def _page(items: list[str], offset: int, limit: int, total: int) -> dict:
    return {
        "results": [_activity_dict(i) for i in items],
        "total": total,
        "offset": offset,
        "limit": limit,
        "hasMore": offset + limit < total,
        "searchTimeMs": 0.1,
    }


class TestSearchResultsBasics:
    def test_len_returns_total_across_all_pages(self):
        sr = SearchResults.from_raw(
            _page(["a", "b"], offset=0, limit=2, total=42),
            parse=Activity.from_json,
            fetch=lambda o, l: _page([], o, l or 2, 42),  # never called, just to satisfy from_raw
        )
        assert len(sr) == 42

    def test_getitem_indexes_current_page(self):
        sr = SearchResults.from_raw(
            _page(["a", "b", "c"], offset=0, limit=3, total=3),
            parse=Activity.from_json,
        )
        assert sr[0].name == "a"
        assert sr[2].name == "c"

    def test_page_size_mirrors_wire_limit(self):
        sr = SearchResults.from_raw(
            _page(["a"], offset=0, limit=50, total=1),
            parse=Activity.from_json,
        )
        assert sr.page_size == 50

    def test_has_more_reflected_from_wire(self):
        sr = SearchResults.from_raw(
            _page(["a", "b"], offset=0, limit=2, total=10),
            parse=Activity.from_json,
            fetch=lambda o, l: _page([], o, l or 2, 10),  # never called, just to satisfy from_raw
        )
        assert sr.has_more is True

    def test_getitem_supports_slice_on_current_page(self):
        sr = SearchResults.from_raw(
            _page(["a", "b", "c"], offset=0, limit=3, total=3),
            parse=Activity.from_json,
        )
        sliced = sr[:2]
        assert isinstance(sliced, list)
        assert [a.name for a in sliced] == ["a", "b"]

    def test_search_time_ms_preserved(self):
        sr = SearchResults.from_raw(
            {"results": [], "total": 0, "offset": 0, "limit": 20,
             "hasMore": False, "searchTimeMs": 42.5},
            parse=Activity.from_json,
        )
        assert sr.search_time_ms == 42.5


class TestSearchResultsIteration:
    def test_single_page_iteration(self):
        sr = SearchResults.from_raw(
            _page(["a", "b"], offset=0, limit=20, total=2),
            parse=Activity.from_json,
        )
        names = [a.name for a in sr]
        assert names == ["a", "b"]

    def test_lazy_multi_page_fetch(self):
        """Iteration fetches subsequent pages on demand via the callback."""
        calls: list[tuple[int, int | None]] = []

        def fetch(offset: int, limit: int | None) -> dict:
            calls.append((offset, limit))
            if offset == 2:
                return _page(["c", "d"], offset=2, limit=2, total=5)
            if offset == 4:
                return _page(["e"], offset=4, limit=2, total=5)
            raise AssertionError(f"Unexpected fetch at offset={offset}")

        sr = SearchResults.from_raw(
            _page(["a", "b"], offset=0, limit=2, total=5),
            parse=Activity.from_json,
            fetch=fetch,
        )
        names = [a.name for a in sr]
        assert names == ["a", "b", "c", "d", "e"]
        # Two follow-up fetches: offsets 2 and 4.
        assert calls == [(2, 2), (4, 2)]

    def test_from_raw_rejects_detached_has_more(self):
        """No fetcher + hasMore=True is unsafe (would silently truncate). Constructor refuses it."""
        with pytest.raises(ValueError, match="hasMore=True but no fetch callback"):
            SearchResults.from_raw(
                _page(["a", "b"], offset=0, limit=2, total=10),
                parse=Activity.from_json,
            )

    def test_iteration_raises_on_empty_page_with_has_more(self):
        """Server claims hasMore=True but returns no items — surface the broken
        pagination contract loudly. Silently stopping would let callers consume
        an incomplete result set without ever learning the engine misbehaved.
        """
        def fetch(offset: int, limit: int | None) -> dict:
            # Buggy server: claims hasMore but returns nothing.
            return {
                "results": [],
                "total": 100,
                "offset": offset,
                "limit": limit or 2,
                "hasMore": True,
                "searchTimeMs": 0.0,
            }

        sr = SearchResults.from_raw(
            _page(["a", "b"], offset=0, limit=2, total=100),
            parse=Activity.from_json,
            fetch=fetch,
        )
        with pytest.raises(RuntimeError, match="Pagination contract broken"):
            list(sr)

    def test_reiteration_replays_from_cache(self):
        """A second iteration must not re-hit the server — fetched pages are cached."""
        calls: list[tuple[int, int | None]] = []

        def fetch(offset: int, limit: int | None) -> dict:
            calls.append((offset, limit))
            return _page(["c", "d"], offset=2, limit=2, total=4)

        sr = SearchResults.from_raw(
            _page(["a", "b"], offset=0, limit=2, total=4),
            parse=Activity.from_json,
            fetch=fetch,
        )
        first = [a.name for a in sr]
        second = [a.name for a in sr]
        assert first == ["a", "b", "c", "d"]
        assert second == first
        # Exactly one follow-up fetch — the second iteration replays the cache.
        assert len(calls) == 1


class TestPageMethod:
    def test_page_one_fetches_offset_zero(self):
        captured: list[tuple[int, int | None]] = []

        def fetch(offset: int, limit: int | None) -> dict:
            captured.append((offset, limit))
            return _page(["x"], offset=offset, limit=limit or 20, total=1)

        sr = SearchResults.from_raw(
            _page(["a"], offset=0, limit=20, total=100),
            parse=Activity.from_json,
            fetch=fetch,
        )
        sr.page(1)
        assert captured == [(0, 20)]

    def test_page_n_uses_current_page_size(self):
        captured: list[tuple[int, int | None]] = []

        def fetch(offset: int, limit: int | None) -> dict:
            captured.append((offset, limit))
            return _page(["x"], offset=offset, limit=limit or 20, total=100)

        sr = SearchResults.from_raw(
            _page(["a"], offset=0, limit=20, total=100),
            parse=Activity.from_json,
            fetch=fetch,
        )
        sr.page(3)  # offset = (3-1)*20 = 40
        assert captured == [(40, 20)]

    def test_page_size_override(self):
        captured: list[tuple[int, int | None]] = []

        def fetch(offset: int, limit: int | None) -> dict:
            captured.append((offset, limit))
            return _page(["x"], offset=offset, limit=limit or 20, total=100)

        sr = SearchResults.from_raw(
            _page(["a"], offset=0, limit=20, total=100),
            parse=Activity.from_json,
            fetch=fetch,
        )
        sr.page(2, page_size=50)
        assert captured == [(50, 50)]

    def test_page_zero_raises(self):
        sr = SearchResults.from_raw(
            _page(["a"], offset=0, limit=20, total=1),
            parse=Activity.from_json,
            fetch=lambda o, l: _page([], o, l or 20, 0),
        )
        with pytest.raises(ValueError, match="page must be >= 1"):
            sr.page(0)

    def test_page_without_fetcher_raises(self):
        sr = SearchResults.from_raw(
            _page(["a"], offset=0, limit=20, total=1),
            parse=Activity.from_json,
        )
        with pytest.raises(RuntimeError, match="no fetcher"):
            sr.page(2)


class TestConsumersResponseLazyPagination:
    def test_consumers_iteration_walks_all_pages(self, mocked_client, make_response):
        from volca import ConsumersResponse

        client, session = mocked_client

        def consumer_dict(pid: str) -> dict:
            return {
                "processId": pid,
                "name": pid,
                "location": "FR",
                "product": "p",
                "productAmount": 1.0,
                "productUnit": "kg",
                "depth": 1,
            }

        def envelope(items: list[str], offset: int, limit: int, total: int) -> dict:
            return {
                "results": {
                    "results": [consumer_dict(i) for i in items],
                    "total": total,
                    "offset": offset,
                    "limit": limit,
                    "hasMore": offset + limit < total,
                    "searchTimeMs": 0.1,
                },
                "edges": [],
            }

        # Override the fixture spec to include `offset` on get_consumers.
        from volca.client import _Operation
        client._operations["get_consumers"] = _Operation(
            operation_id="get_consumers",
            method="GET",
            path_template="/api/v1/db/{dbName}/activity/{processId}/consumers",
            path_params=["dbName", "processId"],
            query_params=["limit", "offset", "max-depth", "include-edges"],
        )
        session.get.side_effect = [
            make_response(envelope(["a", "b"], 0, 2, 3)),
            make_response(envelope(["c"], 2, 2, 3)),
        ]
        resp: ConsumersResponse = client.get_consumers("root_pid", page_size=2)
        names = [c.name for c in resp.consumers]
        assert names == ["a", "b", "c"]
        assert len(resp.consumers) == 3
