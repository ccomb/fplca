"""Server lifecycle management for fpLCA."""

import shutil
import subprocess
import time
from pathlib import Path

import requests


class Server:
    """Manages the fpLCA server process.

    Usage::

        with Server(config="fplca.toml") as srv:
            client = Client(port=srv.port, db="agribalyse-3.2")
            activities = client.search_activities(name="at plant")
    """

    def __init__(self, config: str = "fplca.toml", port: int = 8081, binary: str = "fplca"):
        self.config = config
        self.port = port
        self.binary = binary
        self._process: subprocess.Popen | None = None

    @property
    def base_url(self) -> str:
        return f"http://localhost:{self.port}"

    def _find_binary(self) -> str:
        """Find the fplca binary: explicit path, package bin/, or PATH."""
        if Path(self.binary).exists():
            return self.binary
        found = shutil.which(self.binary)
        if found:
            return found
        # Try common locations
        for candidate in ["./fplca", "./dist/fplca"]:
            if Path(candidate).exists():
                return candidate
        raise FileNotFoundError(
            f"Cannot find '{self.binary}' binary. "
            "Set binary= parameter or add fplca to PATH."
        )

    def is_alive(self) -> bool:
        """Health check — GET /api/v1/database, return True if 200."""
        try:
            r = requests.get(f"{self.base_url}/api/v1/database", timeout=2)
            return r.status_code == 200
        except requests.ConnectionError:
            return False

    def start(self, idle_timeout: int = 300, wait_timeout: int = 120) -> None:
        """Start server if not running. Wait until ready."""
        if self.is_alive():
            return

        binary = self._find_binary()
        cmd = [
            binary,
            "--config", self.config,
            "server",
            "--port", str(self.port),
            "--idle-timeout", str(idle_timeout),
        ]
        self._process = subprocess.Popen(
            cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )

        # Poll until server is ready
        deadline = time.monotonic() + wait_timeout
        while time.monotonic() < deadline:
            if self.is_alive():
                return
            time.sleep(0.5)

        raise TimeoutError(
            f"Server did not become ready within {wait_timeout}s"
        )

    def stop(self) -> None:
        """Stop the server via shutdown endpoint, then terminate process."""
        try:
            requests.post(f"{self.base_url}/api/v1/shutdown", timeout=5)
        except requests.ConnectionError:
            pass
        if self._process:
            self._process.terminate()
            self._process.wait(timeout=10)
            self._process = None

    def __enter__(self) -> "Server":
        self.start()
        return self

    def __exit__(self, *_) -> None:
        self.stop()
