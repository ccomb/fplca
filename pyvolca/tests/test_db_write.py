"""Offline request-shaping tests for the database write operations.

These endpoints (copy / delete / relink / export / add-/remove-dependency)
carry no operationId — they bypass the OpenAPI dispatcher and build their
URLs directly. They also do not exist in any released engine binary, so these
tests never touch a live engine: they mock ``Client._session`` and assert on
the wire shape (URL, JSON body, base64 decoding, format validation, error
surfacing).
"""

from __future__ import annotations

import base64

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


# ---------------------------------------------------------------------------
# relink
# ---------------------------------------------------------------------------


class TestRelink:
    def test_body_and_url(self, mocked_client):
        client, session = mocked_client
        _ok(
            session,
            {
                "dbName": "testdb",
                "unresolvedBefore": 10,
                "unresolvedAfter": 2,
                "crossDBLinks": 8,
                "dependsOn": ["ecoinvent-3.9"],
            },
        )
        result = client.relink("ecoinvent-3.9", "src,dst\nfoo,bar\n")
        assert result["unresolvedAfter"] == 2
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/relink"
        body = session.post.call_args[1]["json"]
        assert body == {"depDb": "ecoinvent-3.9", "mappingCsv": "src,dst\nfoo,bar\n"}

    def test_from_file_reads_text(self, mocked_client, tmp_path):
        client, session = mocked_client
        _ok(session, {"dbName": "testdb", "unresolvedBefore": 0,
                      "unresolvedAfter": 0, "crossDBLinks": 0, "dependsOn": []})
        csv = tmp_path / "map.csv"
        csv.write_text("src,dst\nfoo,bar\n", encoding="utf-8")
        client.relink_from_file("dep", str(csv))
        body = session.post.call_args[1]["json"]
        assert body == {"depDb": "dep", "mappingCsv": "src,dst\nfoo,bar\n"}


# ---------------------------------------------------------------------------
# dependencies
# ---------------------------------------------------------------------------


class TestDependencies:
    def test_add_dependency_url(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"dependsOn": ["dep"]})
        client.add_dependency("dep")
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/add-dependency/dep"

    def test_remove_dependency_url(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"dependsOn": []})
        client.remove_dependency("dep", db_name="other")
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/other/remove-dependency/dep"


# ---------------------------------------------------------------------------
# export
# ---------------------------------------------------------------------------


class TestExport:
    def test_base64_decode_returns_raw_bytes(self, mocked_client):
        client, session = mocked_client
        raw = b"PK\x03\x04 zipped db bytes"
        _ok(session, {"success": True, "message": "ok",
                      "data": base64.b64encode(raw).decode()})
        out = client.export_database("ecospold2")
        assert out == raw
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/export"
        assert session.post.call_args[1]["json"] == {"format": "ecospold2"}

    def test_format_normalized_before_send(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok",
                      "data": base64.b64encode(b"x").decode()})
        client.export_database("  SimaPro  ")
        assert session.post.call_args[1]["json"] == {"format": "simapro"}

    def test_unknown_format_raises_before_request(self, mocked_client):
        client, session = mocked_client
        with pytest.raises(VoLCAError, match="unknown export format"):
            client.export_database("parquet")
        session.post.assert_not_called()

    def test_in_band_failure_raises(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": False, "message": "not loaded", "data": None})
        with pytest.raises(VoLCAError, match="not loaded"):
            client.export_database("simapro")

    def test_missing_data_raises(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok", "data": None})
        with pytest.raises(VoLCAError, match="no data field"):
            client.export_database("simapro")

    def test_to_file_writes_decoded_bytes(self, mocked_client, tmp_path):
        client, session = mocked_client
        raw = b"hello bytes"
        _ok(session, {"success": True, "message": "ok",
                      "data": base64.b64encode(raw).decode()})
        out = tmp_path / "export.csv"
        client.export_to_file("simapro", str(out))
        assert out.read_bytes() == raw


# ---------------------------------------------------------------------------
# default-db guard
# ---------------------------------------------------------------------------


class TestNoDefaultDb:
    def _client(self):
        return Client(base_url="http://test.local", db="")

    def test_copy_without_db_raises(self):
        with pytest.raises(VoLCAError, match="No database specified"):
            self._client().copy_database("clone")

    def test_export_without_db_raises(self):
        with pytest.raises(VoLCAError, match="No database specified"):
            self._client().export_database("simapro")
