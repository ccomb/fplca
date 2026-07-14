import ast
import importlib.util
import threading
import urllib.parse
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import pytest


EXAMPLE = Path(__file__).parents[1] / "examples" / "convert_database.py"


def load_example():
    spec = importlib.util.spec_from_file_location("convert_database", EXAMPLE)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_converter_rejects_unsafe_server_slugs():
    example = load_example()
    assert example.safe_slug("database-1.0") == "database-1.0"
    for slug in ("../outside", "/absolute", "nested/path", "nested\\path", ""):
        with pytest.raises(ValueError):
            example.safe_slug(slug)


def test_pyvolca_080_export_fallback_surfaces_warnings():
    example = load_example()

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_POST(self):
            self.send_response(200)
            self.send_header(
                "X-Volca-Export-Warnings", urllib.parse.quote("first warning")
            )
            self.end_headers()
            self.wfile.write(b"export")

    class Client:
        db = "database"

        def export_database(self, _fmt):
            raise example.VoLCAError("export_database failed (HTTP 406)")

    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    managed = type(
        "Server", (),
        {"base_url": f"http://127.0.0.1:{server.server_port}", "password": "token"},
    )()
    try:
        with pytest.warns(UserWarning, match="first warning"):
            assert example.export_bytes(Client(), managed, "ilcd") == b"export"
    finally:
        server.shutdown()
        server.server_close()


def test_export_fallback_only_handles_the_known_406_prefix():
    example = load_example()

    class Client:
        db = "database"

        def export_database(self, _fmt):
            raise example.VoLCAError("another failure whose body mentions HTTP 406")

    managed = type(
        "Server", (), {"base_url": "http://127.0.0.1:1", "password": "x"}
    )()
    with pytest.raises(example.VoLCAError, match="another failure"):
        example.export_bytes(Client(), managed, "ilcd")


def test_converter_uses_only_public_pyvolca_lifecycle():
    source = EXAMPLE.read_text()
    tree = ast.parse(source)
    imported = {
        name.name
        for node in ast.walk(tree)
        if isinstance(node, ast.ImportFrom) and node.module == "volca"
        for name in node.names
    }
    calls = {
        node.func.attr
        for node in ast.walk(tree)
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
    }

    assert {"Client", "Server", "download"} <= imported
    assert {"load_database", "export_database"} <= calls
    assert {
        "upload_database",
        "finalize_database",
        "unload_database",
        "delete_database",
    }.isdisjoint(calls)
    assert "[[databases]]" in source
    assert "._session" not in source
    assert "import requests" not in source
