"""Offline tests for the database upload + staged-database lifecycle.

These endpoints (upload / setup / set-data-path / finalize / delete) carry no
operationId — they bypass the OpenAPI dispatcher and build their URLs
directly. The tests mock ``Client._session`` and assert on the wire shape
(URL, query params, streamed body, in-band-failure surfacing).
"""

from __future__ import annotations

import pytest

from volca.client import Client, VoLCAError


def _resp(session_attr, json_body: dict) -> None:
    from tests.conftest import _make_response

    session_attr.return_value = _make_response(json_body)


class TestUploadDatabase:
    def test_bytes_source_streams_with_query_metadata(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": True, "message": "ok", "slug": "agb", "format": "simapro-csv"})
        out = client.upload_database(b"raw archive bytes", "Agribalyse", description="v3.2")
        assert out["slug"] == "agb"
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/upload"
        kw = session.post.call_args[1]
        assert kw["params"] == {"name": "Agribalyse", "description": "v3.2"}
        assert kw["headers"]["Content-Type"] == "application/octet-stream"
        assert kw["data"] == b"raw archive bytes"

    def test_description_omitted_when_absent(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": True, "message": "ok", "slug": "x", "format": "ilcd"})
        client.upload_database(b"bytes", "NoDesc")
        assert session.post.call_args[1]["params"] == {"name": "NoDesc"}

    def test_path_source_is_streamed_as_file_object(self, mocked_client, tmp_path):
        client, session = mocked_client
        _resp(session.post, {"success": True, "message": "ok", "slug": "s", "format": "ecospold2"})
        archive = tmp_path / "db.zip"
        archive.write_bytes(b"PK\x03\x04zip")
        client.upload_database(str(archive), "FromFile")
        data = session.post.call_args[1]["data"]
        # requests streams the opened file object rather than a bytes blob.
        assert getattr(data, "name", None) == str(archive)

    def test_in_band_failure_raises(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": False, "message": "Uploads are disabled on this plan.", "slug": None, "format": None})
        with pytest.raises(VoLCAError, match="Uploads are disabled"):
            client.upload_database(b"bytes", "Blocked")


class TestStagedLifecycle:
    def test_get_setup_targets_default_db(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, {"name": "testdb", "isReady": False, "missingSuppliers": ["x"]})
        out = client.get_setup()
        assert session.get.call_args[0][0] == "http://test.local/api/v1/db/testdb/setup"
        assert out["missingSuppliers"] == ["x"]

    def test_set_data_path_sends_path_body(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"name": "testdb", "dataPath": "sub/data.csv"})
        client.set_data_path("sub/data.csv")
        assert session.post.call_args[0][0] == "http://test.local/api/v1/db/testdb/set-data-path"
        assert session.post.call_args[1]["json"] == {"path": "sub/data.csv"}

    def test_finalize_success_returns_payload(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": True, "message": "loaded"})
        out = client.finalize_database()
        assert session.post.call_args[0][0] == "http://test.local/api/v1/db/testdb/finalize"
        assert out["message"] == "loaded"

    def test_finalize_in_band_failure_raises(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": False, "message": "unresolved suppliers"})
        with pytest.raises(VoLCAError, match="unresolved suppliers"):
            client.finalize_database()

    def test_delete_uses_http_delete(self, mocked_client):
        client, session = mocked_client
        _resp(session.delete, {"success": True, "message": "gone"})
        client.delete_database()
        session.delete.assert_called_once()
        assert session.delete.call_args[0][0] == "http://test.local/api/v1/db/testdb"

    def test_delete_in_band_failure_raises(self, mocked_client):
        client, session = mocked_client
        _resp(session.delete, {"success": False, "message": "in use"})
        with pytest.raises(VoLCAError, match="in use"):
            client.delete_database()
