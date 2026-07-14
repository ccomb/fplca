# /// script
# requires-python = ">=3.10"
# dependencies = ["pyvolca==0.8.0"]
# ///
"""Download VoLCA, import one database, and export one or more formats."""

import argparse
import json
import os
import re
import secrets
import shutil
import socket
import tempfile
import urllib.parse
import urllib.request
import warnings
from pathlib import Path

from volca import Client, Server, VoLCAError, download

EXT = {
    "simapro": ".simapro.csv",
    "ilcd": ".ilcd.zip",
    "ecospold1": ".ecospold1.xml",
    "ecospold2": ".ecospold2.zip",
    "brightway": ".brightway.xlsx",
}


def safe_slug(slug):
    if not isinstance(slug, str) or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}", slug):
        raise ValueError("server returned an unsafe database slug")
    return slug


def free_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def export_bytes(client, server, fmt):
    """Use pyvolca, with a narrow 0.8.0 fallback for its missing Accept header."""
    try:
        return client.export_database(fmt)
    except VoLCAError as error:
        if not str(error).startswith("export_database failed (HTTP 406)"):
            raise
        request = urllib.request.Request(
            f"{server.base_url}/api/v1/db/{client.db}/export",
            data=json.dumps({"format": fmt}).encode(), method="POST",
            headers={
                "Authorization": f"Bearer {server.password}",
                "Content-Type": "application/json",
                "Accept": "application/octet-stream",
            },
        )
        with urllib.request.urlopen(request, timeout=1800) as response:
            for line in urllib.parse.unquote(
                response.headers.get("X-Volca-Export-Warnings", "")
            ).splitlines():
                warnings.warn(line, stacklevel=2)
            return response.read()


def publish(data, output, force):
    with tempfile.NamedTemporaryFile(dir=output.parent, delete=False) as stream:
        temporary = Path(stream.name)
        stream.write(data)
    try:
        if force:
            os.replace(temporary, output)
            temporary = None
        else:
            os.link(temporary, output)
    finally:
        if temporary:
            temporary.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True, type=Path)
    parser.add_argument("-f", "--format", nargs="+", required=True, choices=EXT)
    parser.add_argument("-o", "--out", type=Path, default=Path.cwd())
    parser.add_argument("--engine-version", default="v0.9.1")
    parser.add_argument("--force", action="store_true")
    args = parser.parse_args()

    source, out = args.source.expanduser().resolve(), args.out.expanduser().resolve()
    if not source.is_file():
        raise FileNotFoundError(source)
    out.mkdir(parents=True, exist_ok=True)
    installed = download(version=args.engine_version)

    with tempfile.TemporaryDirectory(prefix="volca-convert-") as raw:
        root, port, password = Path(raw), free_port(), secrets.token_urlsafe(32)
        db_name = safe_slug(source.stem)
        local_source = root / source.name
        shutil.copy2(source, local_source)
        config = root / "volca.toml"
        config.write_text(
            f'[server]\nhost="127.0.0.1"\nport={port}\npassword="{password}"\n\n'
            f'[[databases]]\nname={json.dumps(db_name)}\n'
            f'path={json.dumps(str(local_source))}\nload=false\n'
        )
        with Server(config=str(config), binary=str(installed.binary)) as server:
            client = Client(server.base_url, db=db_name, password=server.password)
            client.load_database(db_name)
            outputs = {
                fmt: out / (db_name + EXT[fmt])
                for fmt in dict.fromkeys(args.format)
            }
            if not args.force:
                existing = [path for path in outputs.values() if os.path.lexists(path)]
                if existing:
                    raise FileExistsError(existing[0])
            for fmt, output in outputs.items():
                publish(export_bytes(client, server, fmt), output, args.force)
                print(f"{fmt}: {output}")


if __name__ == "__main__":
    main()
