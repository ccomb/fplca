"""End-to-end smoke test against a running VoLCA engine.

Skipped unless ``VOLCA_SMOKE_URL`` is set in the environment, pointing at
a reachable engine. The test fetches the spec via HTTP, exercises the
runtime dispatcher on a handful of no-database-required operations, and
verifies the request/response round trip works.

Run with::

    VOLCA_SMOKE_URL=http://localhost:8080 pytest pyvolca/tests/test_smoke.py

To auto-start a server for the test, use ``Server`` from ``volca.server``::

    with Server(config="volca.toml") as srv:
        os.environ["VOLCA_SMOKE_URL"] = srv.base_url
        ...
"""

from __future__ import annotations

import os

import pytest

from volca.client import Client


SMOKE_URL = os.environ.get("VOLCA_SMOKE_URL")


@pytest.fixture(scope="module")
def live_client() -> Client:
    if not SMOKE_URL:
        pytest.skip("VOLCA_SMOKE_URL not set — smoke test needs a running engine.")
    c = Client(base_url=SMOKE_URL, db="")
    # Force spec load up-front so failures surface here instead of inside
    # individual operation tests.
    c._load_operations()
    return c


def test_spec_is_fetched_and_has_operations(live_client):
    assert live_client._operations is not None
    assert len(live_client._operations) >= 17, (
        f"Expected ≥17 stamped operations from /api/v1/openapi.json, "
        f"got {len(live_client._operations)}"
    )


def test_list_databases_returns_a_list(live_client):
    """Round-trip through the dispatcher for a no-path-capture operation."""
    dbs = live_client.list_databases()
    assert isinstance(dbs, list)


def test_list_methods_returns_a_list(live_client):
    methods = live_client.list_methods()
    assert isinstance(methods, list)


def test_get_version_via_direct_call(live_client):
    """``get_version`` goes through direct HTTP (no operationId), sanity check."""
    v = live_client.get_version()
    assert "version" in v
    assert "gitHash" in v


def test_unknown_operation_surfaces_as_voLCAError(live_client):
    from volca.client import VoLCAError
    with pytest.raises(VoLCAError, match="Unknown operationId"):
        live_client.call("does_not_exist_anywhere")
