"""Offline orchestration tests for Client.ensure_database.

The wire shape of each primitive (list/upload/setup/finalize/load) is covered
by its own tests; these pin the idempotent orchestration: what gets called,
in which mode, and what never gets called.
"""

from __future__ import annotations

from unittest import mock

import pytest

from volca.client import Client, VoLCAError
from volca.types import DatabaseInfo, DatabaseStatus


def _info(slug: str, display: str, status: DatabaseStatus) -> DatabaseInfo:
    return DatabaseInfo(
        name=slug, display_name=display, status=status, path=f"/data/{slug}"
    )


@pytest.fixture()
def client() -> Client:
    c = Client(base_url="http://test.local")
    # Orchestration only: every primitive is replaced by a mock.
    c.list_databases = mock.Mock(return_value=[])  # type: ignore[method-assign]
    c.upload_database = mock.Mock(return_value={"slug": "fresh-1-0"})  # type: ignore[method-assign]
    c.get_setup = mock.Mock(return_value={"isReady": True})  # type: ignore[method-assign]
    c.finalize_database = mock.Mock(return_value={"success": True})  # type: ignore[method-assign]
    c.load_database = mock.Mock(return_value={"success": True})  # type: ignore[method-assign]
    return c


def test_absent_database_is_uploaded_finalized(client: Client, tmp_path):
    archive = tmp_path / "fresh-1.0.xlsx"
    archive.write_bytes(b"x")
    slug = client.ensure_database(str(archive))
    assert slug == "fresh-1-0"
    client.upload_database.assert_called_once_with(str(archive), name="fresh-1.0")
    client.finalize_database.assert_called_once_with("fresh-1-0")
    client.load_database.assert_not_called()


def test_loaded_database_is_left_alone(client: Client):
    client.list_databases.return_value = [
        _info("fresh-1-0", "fresh-1.0", DatabaseStatus.LOADED)
    ]
    slug = client.ensure_database("ignored/fresh-1.0.xlsx")
    assert slug == "fresh-1-0"
    client.upload_database.assert_not_called()
    client.finalize_database.assert_not_called()
    client.load_database.assert_not_called()


def test_unloaded_database_is_loaded_not_reuploaded(client: Client):
    client.list_databases.return_value = [
        _info("fresh-1-0", "fresh-1.0", DatabaseStatus.UNLOADED)
    ]
    slug = client.ensure_database("ignored/fresh-1.0.xlsx")
    assert slug == "fresh-1-0"
    client.upload_database.assert_not_called()
    client.load_database.assert_called_once_with("fresh-1-0")


def test_match_by_slug_works_too(client: Client):
    client.list_databases.return_value = [
        _info("fresh-1-0", "Fresh database", DatabaseStatus.LOADED)
    ]
    assert client.ensure_database(b"raw", name="fresh-1-0") == "fresh-1-0"
    client.upload_database.assert_not_called()


def test_unready_upload_fails_before_finalize(client: Client):
    client.get_setup.return_value = {
        "isReady": False,
        "missingSuppliers": ["ecoinvent-3.11"],
    }
    with pytest.raises(VoLCAError, match="ecoinvent-3.11"):
        client.ensure_database("ignored/fresh-1.0.xlsx")
    client.finalize_database.assert_not_called()


def test_bytes_source_requires_a_name(client: Client):
    with pytest.raises(VoLCAError, match="name= is required"):
        client.ensure_database(b"raw bytes")
    client.upload_database.assert_not_called()
