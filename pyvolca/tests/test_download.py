"""Pure-logic tests for volca._download.

Network-dependent paths (the actual download() entry point) are not
exercised here — they're covered by the install.sh integration tests
and by manual verification after a real release.
"""

from __future__ import annotations

import json
import threading
import time
from pathlib import Path
from unittest import mock

import pytest

from volca import _download


# ---------------------------------------------------------------------------
# SHA256SUMS parser
# ---------------------------------------------------------------------------


def test_parse_sha256sums_basic():
    h1 = "a" * 64
    h2 = "b" * 64
    h3 = "c" * 64
    text = (
        f"{h1}  volca-0.7.0-linux-amd64.tar.gz\n"
        f"{h2}  volca-0.7.0-macos-arm64.tar.gz\n"
        "0bad  volca-data-1.tar.gz\n"  # 4-char hash should NOT match
        f"{h3}  volca-data-1.tar.gz\n"
    )
    parsed = _download._parse_sha256sums(text)
    assert parsed["volca-0.7.0-linux-amd64.tar.gz"] == h1
    assert parsed["volca-0.7.0-macos-arm64.tar.gz"] == h2
    # The 4-char "0bad" line is rejected; the 64-char one wins.
    assert parsed["volca-data-1.tar.gz"] == h3


def test_parse_sha256sums_rejects_short_hashes():
    text = "abc  short.tar.gz\n"
    assert _download._parse_sha256sums(text) == {}


def test_parse_sha256sums_handles_star_prefix():
    # GNU sha256sum binary mode emits "<hash> *<file>".
    valid = "a" * 64
    text = f"{valid} *file.tar.gz\n"
    assert _download._parse_sha256sums(text) == {"file.tar.gz": valid}


# ---------------------------------------------------------------------------
# verify
# ---------------------------------------------------------------------------


def test_verify_success(tmp_path: Path):
    f = tmp_path / "x.bin"
    f.write_bytes(b"hello")
    # sha256("hello") = 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
    _download._verify(f, "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")


def test_verify_mismatch(tmp_path: Path):
    f = tmp_path / "x.bin"
    f.write_bytes(b"hello")
    with pytest.raises(_download.DownloadError, match="SHA256 mismatch"):
        _download._verify(f, "0" * 64)


# ---------------------------------------------------------------------------
# platform_slug
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "system,machine,expected",
    [
        ("Linux", "x86_64", ("linux-amd64", "tar.gz")),
        ("Linux", "amd64", ("linux-amd64", "tar.gz")),
        ("Linux", "aarch64", ("linux-arm64", "tar.gz")),
        ("Linux", "arm64", ("linux-arm64", "tar.gz")),
        ("Darwin", "arm64", ("macos-arm64", "tar.gz")),
        ("Windows", "AMD64", ("windows-amd64", "zip")),
    ],
)
def test_platform_slug(system, machine, expected):
    with mock.patch("platform.system", return_value=system), \
         mock.patch("platform.machine", return_value=machine):
        assert _download._platform_slug() == expected


def test_platform_slug_unsupported():
    with mock.patch("platform.system", return_value="OS/2"), \
         mock.patch("platform.machine", return_value="i386"):
        with pytest.raises(_download.DownloadError):
            _download._platform_slug()


def test_platform_slug_macos_x86_64_rejected():
    with mock.patch("platform.system", return_value="Darwin"), \
         mock.patch("platform.machine", return_value="x86_64"):
        with pytest.raises(_download.DownloadError):
            _download._platform_slug()


# ---------------------------------------------------------------------------
# cached_binary
# ---------------------------------------------------------------------------


def test_cached_binary_no_manifest(tmp_path: Path):
    with mock.patch.object(_download, "_cache_root", return_value=tmp_path):
        assert _download.cached_binary() is None


def test_cached_binary_explicit_version(tmp_path: Path):
    bin_dir = tmp_path / "0.7.0"
    bin_dir.mkdir()
    bin_path = bin_dir / "volca"
    bin_path.write_bytes(b"")
    with mock.patch.object(_download, "_cache_root", return_value=tmp_path), \
         mock.patch.object(_download, "_binary_name", return_value="volca"):
        assert _download.cached_binary("v0.7.0") == bin_path
        assert _download.cached_binary("0.7.0") == bin_path


def test_cached_binary_via_manifest(tmp_path: Path):
    bin_dir = tmp_path / "0.7.0"
    bin_dir.mkdir()
    (bin_dir / "volca").write_bytes(b"")
    (tmp_path / "latest.json").write_text(json.dumps({"version": "0.7.0"}))
    with mock.patch.object(_download, "_cache_root", return_value=tmp_path), \
         mock.patch.object(_download, "_binary_name", return_value="volca"):
        result = _download.cached_binary()
        assert result == bin_dir / "volca"


# ---------------------------------------------------------------------------
# _exclusive_lock
# ---------------------------------------------------------------------------


def test_exclusive_lock_acquires_and_releases(tmp_path: Path):
    """Smoke check: the context manager runs to completion without raising."""
    lock_path = tmp_path / ".lock"
    with _download._exclusive_lock(lock_path):
        assert lock_path.exists()
    # Re-entering after release works.
    with _download._exclusive_lock(lock_path):
        pass


def test_exclusive_lock_serialises_threads(tmp_path: Path):
    """Two threads racing on the same lock must execute their critical
    sections strictly serially.

    Each thread sleeps inside the lock; if the lock leaks, the in-flight
    counter goes above 1 and we see overlap.
    """
    lock_path = tmp_path / ".lock"
    in_flight = 0
    max_in_flight = 0
    lock = threading.Lock()

    def worker():
        nonlocal in_flight, max_in_flight
        with _download._exclusive_lock(lock_path):
            with lock:
                in_flight += 1
                max_in_flight = max(max_in_flight, in_flight)
            time.sleep(0.05)
            with lock:
                in_flight -= 1

    threads = [threading.Thread(target=worker) for _ in range(4)]
    for t in threads:
        t.start()
    for t in threads:
        t.join(timeout=5)

    assert all(not t.is_alive() for t in threads), "thread deadlocked on lock"
    assert max_in_flight == 1, f"lock allowed {max_in_flight} concurrent holders"
