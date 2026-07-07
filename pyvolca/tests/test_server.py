"""Offline tests for volca.server.Server binary resolution.

No engine is spawned — these exercise _find_binary's resolution order,
which must not be fooled by a directory that happens to share the binary
name (the common case: running a script from a source checkout that has a
``volca/`` package directory in the working tree).
"""

from __future__ import annotations

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
