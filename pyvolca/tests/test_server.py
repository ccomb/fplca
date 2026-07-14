"""Offline tests for volca.server.Server binary resolution.

No engine is spawned — these exercise _find_binary's resolution order,
which must not be fooled by a directory that happens to share the binary
name (the common case: running a script from a source checkout that has a
``volca/`` package directory in the working tree).
"""

from __future__ import annotations

import io
from pathlib import Path
from unittest import mock

import pytest

from volca import _download
from volca.server import Server


def test_find_binary_skips_same_named_directory(tmp_path: Path, monkeypatch):
    """A ``volca`` *directory* in cwd must not be returned as the binary."""
    (tmp_path / "volca").mkdir()
    monkeypatch.chdir(tmp_path)
    installed = tmp_path / "install" / "volca"
    installed.parent.mkdir()
    installed.write_bytes(b"")
    with mock.patch.object(_download, "installed_binary", return_value=installed):
        srv = Server(config="absent.toml", binary="volca")
        assert srv._find_binary() == str(installed)


def test_find_binary_devtree_skips_same_named_directory(tmp_path: Path, monkeypatch):
    """Dev-tree fallback: a ``./volca`` directory must not shadow ``./dist/volca``."""
    (tmp_path / "volca").mkdir()
    (tmp_path / "dist").mkdir()
    (tmp_path / "dist" / "volca").write_bytes(b"")
    monkeypatch.chdir(tmp_path)
    with mock.patch.object(_download, "installed_binary", return_value=None), \
         mock.patch("volca.server.shutil.which", return_value=None):
        srv = Server(config="absent.toml", binary="volca")
        assert srv._find_binary() == "./dist/volca"


def test_find_binary_accepts_explicit_file(tmp_path: Path):
    """An explicit path to a real file is returned as-is."""
    binary = tmp_path / "volca"
    binary.write_bytes(b"")
    srv = Server(config="absent.toml", binary=str(binary))
    assert srv._find_binary() == str(binary)


def test_find_binary_raises_when_nothing_found(tmp_path: Path, monkeypatch):
    """No file, no install, no PATH, no dev tree → clear FileNotFoundError."""
    monkeypatch.chdir(tmp_path)
    with mock.patch.object(_download, "installed_binary", return_value=None), \
         mock.patch("volca.server.shutil.which", return_value=None):
        srv = Server(config="absent.toml", binary="volca")
        with pytest.raises(FileNotFoundError):
            srv._find_binary()


def test_auto_port_preserves_explicit_zero_compatibility(tmp_path: Path):
    config = tmp_path / "volca.toml"
    config.write_text("[server]\nport = 8123\n")

    assert Server(config=str(config)).port == 8123
    assert Server(config=str(config), port=0).port == 8123
    assert Server(config=str(config), port="auto").port == 0


def test_await_bound_port_accepts_engine_announcement():
    process = mock.Mock()
    process.stdout = io.StringIO("VOLCA_PORT=43123\n")
    process.poll.return_value = None
    srv = Server(config="absent.toml", port="auto")
    srv._process = process

    srv._await_bound_port(wait_timeout=1)

    assert srv.port == 43123


@pytest.mark.parametrize("line", ["VOLCA_PORT=0\n", "VOLCA_PORT=70000\n"])
def test_await_bound_port_rejects_invalid_port(line: str):
    process = mock.Mock()
    process.stdout = io.StringIO(line)
    process.poll.return_value = None
    srv = Server(config="absent.toml", port="auto")
    srv._process = process

    with pytest.raises(RuntimeError, match="invalid port"):
        srv._await_bound_port(wait_timeout=1)


def test_start_dynamic_port_uses_desktop_announcement(monkeypatch):
    process = mock.Mock()
    process.stdout = io.StringIO("VOLCA_PORT=43123\n")
    process.poll.return_value = None
    srv = Server(config="absent.toml", port="auto")
    monkeypatch.setattr(srv, "_find_binary", lambda: "/tmp/volca")
    monkeypatch.setattr(srv, "is_alive", lambda: True)
    check_wire = mock.Mock()
    monkeypatch.setattr(srv, "_check_wire", check_wire)

    with mock.patch("volca.server.subprocess.Popen", return_value=process) as popen:
        srv.start(wait_timeout=1)

    command = popen.call_args.args[0]
    assert command[-1] == "--desktop"
    assert command[command.index("--port") + 1] == "0"
    assert srv.port == 43123
    check_wire.assert_called_once_with()
