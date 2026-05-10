#!/bin/bash
# Build a fully-static musl-linked volca binary inside Alpine, then extract
# it to ./dist-musl/volca on the host. The image is tagged volca:musl.
#
# Usage:
#   ./docker-build-musl.sh           # build image and extract binary
#   ./docker-build-musl.sh --image-only   # build image only, skip extraction
#
# After:
#   ./dist-musl/volca --help
#   file ./dist-musl/volca       # statically linked
#   ldd  ./dist-musl/volca       # not a dynamic executable

set -e

cd "$(dirname "$0")"

EXTRACT=true
if [[ "$1" == "--image-only" ]]; then
    EXTRACT=false
fi

TAG="volca:musl"

GIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
if ! git diff --quiet HEAD 2>/dev/null; then
    GIT_HASH="${GIT_HASH}-dirty"
fi
GIT_TAG=$(git describe --tags --exact-match HEAD 2>/dev/null || echo "")

echo "Building musl image: hash=$GIT_HASH tag=${GIT_TAG:-none}"

docker build \
    -f docker/Dockerfile.musl \
    --build-arg GIT_HASH="$GIT_HASH" \
    --build-arg GIT_TAG="$GIT_TAG" \
    -t "$TAG" .

if [[ "$EXTRACT" == "true" ]]; then
    mkdir -p dist-musl
    CID=$(docker create "$TAG")
    trap 'docker rm -f "$CID" >/dev/null 2>&1 || true' EXIT
    docker cp "$CID:/usr/local/bin/volca" dist-musl/volca
    docker rm -f "$CID" >/dev/null
    trap - EXIT
    echo ""
    echo "Extracted: $(pwd)/dist-musl/volca ($(du -h dist-musl/volca | cut -f1))"
    echo ""
    file dist-musl/volca
    echo ""
    if file dist-musl/volca | grep -q "statically linked"; then
        echo "[OK] Fully static, no runtime libc dependency."
    else
        echo "[WARN] Binary does not appear statically linked:"
        ldd dist-musl/volca || true
        exit 1
    fi
fi
