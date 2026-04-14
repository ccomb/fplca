"""Shared pytest fixtures for pyvolca tests.

The offline tests mock ``requests.Session`` so no real engine is required.
The drift test reads a committed OpenAPI spec to check that every
hand-written wrapper's operationId exists in the engine's current surface.
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path
from typing import Any, Callable
from unittest.mock import MagicMock

import pytest

from volca.client import Client


# ---------------------------------------------------------------------------
# A minimal OpenAPI fixture that covers the operations the tests exercise.
# Kept in sync with the real engine by the drift test (see test_drift.py).
# ---------------------------------------------------------------------------


@pytest.fixture(scope="session")
def fixture_spec() -> dict[str, Any]:
    """A hand-crafted OpenAPI 3 spec with the shape the dispatcher expects."""
    return {
        "openapi": "3.0.0",
        "paths": {
            "/api/v1/db": {
                "get": {
                    "operationId": "list_databases",
                    "parameters": [],
                },
            },
            "/api/v1/db/{dbName}/activity/{processId}": {
                "get": {
                    "operationId": "get_activity",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "processId", "in": "path", "required": True, "schema": {"type": "string"}},
                    ],
                },
            },
            "/api/v1/db/{dbName}/activities": {
                "get": {
                    "operationId": "search_activities",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "name", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "geo", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "product", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "exact", "in": "query", "required": False, "schema": {"type": "boolean"}},
                        {"name": "preset", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "classification", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "classification-value", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "limit", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "offset", "in": "query", "required": False, "schema": {"type": "integer"}},
                    ],
                },
            },
            "/api/v1/db/{dbName}/activity/{processId}/supply-chain": {
                "get": {
                    "operationId": "get_supply_chain",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "processId", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "name", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "min-quantity", "in": "query", "required": False, "schema": {"type": "number"}},
                        {"name": "max-depth", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "preset", "in": "query", "required": False, "schema": {"type": "string"}},
                    ],
                },
            },
            "/api/v1/db/{dbName}/activity/{processId}/impacts/{collection}/{methodId}": {
                "get": {
                    "operationId": "get_impacts",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "processId", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "collection", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "methodId", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "top-flows", "in": "query", "required": False, "schema": {"type": "integer"}},
                    ],
                },
            },
            "/api/v1/db/{dbName}/activity/{processId}/aggregate": {
                "get": {
                    "operationId": "aggregate",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "processId", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "scope", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "is_input", "in": "query", "required": False, "schema": {"type": "boolean"}},
                        {"name": "max_depth", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "filter_name", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "preset", "in": "query", "required": False, "schema": {"type": "string"}},
                    ],
                },
            },
        },
    }


@pytest.fixture()
def mocked_client(fixture_spec) -> tuple[Client, MagicMock]:
    """A Client whose session is mocked out, preloaded with ``fixture_spec``.

    Returns ``(client, session_mock)`` — tests assert on session calls
    to verify the dispatcher built the correct URL / method / params.
    """
    client = Client(base_url="http://test.local", db="testdb", password="secret")
    mock_session = MagicMock()
    client._session = mock_session
    # Preload the operation table to skip the real spec fetch.
    from volca.client import _parse_spec
    client._operations = _parse_spec(fixture_spec)
    return client, mock_session


def _make_response(json_body: Any, status: int = 200) -> MagicMock:
    """Build a mock ``requests.Response`` that the client's _json accepts."""
    r = MagicMock()
    r.status_code = status
    r.reason = "OK" if status < 400 else "ERROR"
    r.content = json.dumps(json_body).encode()
    r.text = json.dumps(json_body)
    r.json.return_value = json_body
    r.history = []
    r.raise_for_status = MagicMock()
    return r


@pytest.fixture()
def make_response() -> Callable[..., MagicMock]:
    """Factory for synthetic response objects in offline tests."""
    return _make_response


# ---------------------------------------------------------------------------
# Live spec for the drift test.
# ---------------------------------------------------------------------------


@pytest.fixture(scope="session")
def live_spec() -> dict[str, Any] | None:
    """The OpenAPI spec dumped from the currently-built engine binary.

    Returns None if the binary isn't built — callers should skip their
    drift test rather than fail CI hard in that case.
    """
    # Walk up from pyvolca/tests/ to find the cabal dist-newstyle dir.
    here = Path(__file__).resolve()
    # .../volca-public/pyvolca/tests/conftest.py
    #                   ^~~~~~~~~ 2 levels up = pyvolca dir
    volca_public = here.parent.parent.parent
    candidates = list(
        (volca_public / "dist-newstyle").rglob("build/*/ghc-*/volca-*/x/volca/opt/build/volca/volca")
    )
    if not candidates:
        return None
    binary = sorted(candidates)[-1]
    try:
        out = subprocess.check_output([str(binary), "dump-openapi"], timeout=30)
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return None
    return json.loads(out)
