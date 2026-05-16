#!/bin/bash
# Build the volca Docker image (fully-static musl binary on Alpine).
# Run from the volca directory.
#
# Usage:
#   ./docker-build.sh                       # build image, tag `volca`
#   ./docker-build.sh -t mytag              # build with custom tag
#   ./docker-build.sh --extract             # build + copy binary to ./dist/volca
#   ./docker-build.sh -t mytag --extract    # both

set -e

cd "$(dirname "$0")/.."

TAG="volca"
EXTRACT=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        -t)
            TAG="$2"
            shift 2
            ;;
        --extract)
            EXTRACT=true
            shift
            ;;
        *)
            echo "ERROR: unknown argument: $1" >&2
            echo "Usage: $0 [-t TAG] [--extract]" >&2
            exit 1
            ;;
    esac
done

GIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
if ! git diff --quiet HEAD 2>/dev/null; then
    GIT_HASH="${GIT_HASH}-dirty"
fi
GIT_TAG=$(git describe --tags --exact-match HEAD 2>/dev/null || echo "")

# shellcheck source=../versions.env
source versions.env

echo "Building Docker image: tag=$TAG hash=$GIT_HASH git-tag=${GIT_TAG:-none} alpine=$ALPINE_VERSION"

docker build \
    -f docker/Dockerfile \
    --build-arg ALPINE_VERSION="$ALPINE_VERSION" \
    --build-arg GIT_HASH="$GIT_HASH" \
    --build-arg GIT_TAG="$GIT_TAG" \
    -t "$TAG" .

if [[ "$EXTRACT" == "true" ]]; then
    mkdir -p dist
    CID=$(docker create "$TAG")
    trap 'docker rm -f "$CID" >/dev/null 2>&1 || true' EXIT
    docker cp "$CID:/usr/local/bin/volca" dist/volca
    docker rm -f "$CID" >/dev/null
    trap - EXIT
    echo ""
    echo "Extracted: $(pwd)/dist/volca ($(du -h dist/volca | cut -f1))"
    echo ""
    file dist/volca
    if file dist/volca | grep -q "statically linked"; then
        echo "[OK] Fully static, no runtime libc dependency."
    else
        echo "[WARN] Binary does not appear statically linked:"
        ldd dist/volca || true
        exit 1
    fi
fi
