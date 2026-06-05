"""Offline request-shaping tests for the database write operations.

These endpoints (copy / delete / relink / export / add-/remove-dependency)
carry no operationId — they bypass the OpenAPI dispatcher and build their
URLs directly, like ``load_database`` / ``unload_database``. They also do
not exist in any released engine binary, so these tests never touch a live
engine: they mock ``Client._session`` and assert on the wire shape
(URL, JSON body, base64 decoding, format validation, error surfacing).
"""

from __future__ import annotations

import pytest

from volca.client import Client, VoLCAError


def _ok(session, json_body: dict) -> None:
    """Wire the mocked session's POST to return a 200 with ``json_body``."""
    from tests.conftest import _make_response

    session.post.return_value = _make_response(json_body)


# ---------------------------------------------------------------------------
# delete
# ---------------------------------------------------------------------------


class TestDelete:
    def test_body_shape_with_dict_classifications(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok", "deleted": 3})
        result = client.delete_activities(
            name="wheat",
            location="FR",
            classifications=[{"system": "ISIC", "value": "01", "exact": True}],
            exact=True,
            keep=["a_b"],
            extra=["c_d"],
        )
        assert result["deleted"] == 3
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/delete"
        body = session.post.call_args[1]["json"]
        # Blank filters are omitted entirely: product was not supplied, so its
        # key is absent rather than sent as "".
        assert body == {
            "name": "wheat",
            "location": "FR",
            "classifications": [{"system": "ISIC", "value": "01", "exact": True}],
            "exact": True,
            "keep": ["a_b"],
            "extra": ["c_d"],
        }

    def test_tuple_classifications_coerced(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok", "deleted": 0})
        client.delete_activities(classifications=[("CPC", "012"), ("ISIC", "01", True)])
        body = session.post.call_args[1]["json"]
        assert body["classifications"] == [
            {"system": "CPC", "value": "012", "exact": False},
            {"system": "ISIC", "value": "01", "exact": True},
        ]

    def test_defaults_are_empty(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok", "deleted": 0})
        client.delete_activities(product="x")
        body = session.post.call_args[1]["json"]
        assert body["classifications"] == []
        assert body["keep"] == []
        assert body["extra"] == []
        assert body["exact"] is False
        # An unsupplied name is omitted, never sent as "": a regression to the
        # always-send-empty-string body would make the engine read "name":""
        # as a real (unsatisfiable) filter and silently delete nothing.
        assert "name" not in body

    def test_blank_filters_are_omitted(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok", "deleted": 0})
        client.delete_activities(product="milk")
        body = session.post.call_args[1]["json"]
        assert "name" not in body
        assert "location" not in body
        assert body["product"] == "milk"

    def test_malformed_classification_dict_raises(self, mocked_client):
        client, _ = mocked_client
        with pytest.raises(VoLCAError, match="missing keys"):
            client.delete_activities(classifications=[{"system": "ISIC"}])

    def test_in_band_failure_raises(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": False, "message": "nothing matched"})
        with pytest.raises(VoLCAError, match="nothing matched"):
            client.delete_activities(name="x")


# ---------------------------------------------------------------------------
# copy
# ---------------------------------------------------------------------------


class TestCopy:
    def test_url_and_default_db(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "copied", "database": None})
        client.copy_database("clone")
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/copy/clone"

    def test_explicit_db_override(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok"})
        client.copy_database("clone", db_name="other")
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/other/copy/clone"

    def test_in_band_failure_raises(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": False, "message": "already exists"})
        with pytest.raises(VoLCAError, match="already exists"):
            client.copy_database("clone")
