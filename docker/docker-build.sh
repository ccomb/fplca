#!/bin/bash
# Build the volca Docker image (fully-static musl binary on Alpine).
# Run from the volca directory.
#
# Usage:
#   ./docker-build.sh                          # build for the host arch, tag `volca`
#   ./docker-build.sh -t mytag                 # build with a custom tag
#   ./docker-build.sh --platform linux/arm64   # build for a specific architecture
#   ./docker-build.sh --extract                # build + copy binary to ./dist/volca
#   ./docker-build.sh -t mytag --extract       # combine flags
#
# --platform takes a Docker platform string (linux/amd64, linux/arm64); omitted,
# the image is built for the host architecture. The Dockerfile builds OpenBLAS,
# MUMPS and the GHC dependency tree from source, so building a foreign
# architecture under QEMU emulation takes hours — build on a native host of the
# target architecture instead. The script warns when it detects an emulated build.

set -e

cd "$(dirname "$0")/.."

TAG="volca"
EXTRACT=false
PLATFORM=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -t)
            [[ -n "$2" ]] || { echo "ERROR: -t requires a tag argument" >&2; exit 1; }
            TAG="$2"
            shift 2
            ;;
        --platform)
            [[ -n "$2" ]] || { echo "ERROR: --platform requires a platform argument (e.g. linux/arm64)" >&2; exit 1; }
            PLATFORM="$2"
            shift 2
            ;;
        --extract)
            EXTRACT=true
            shift
            ;;
        *)
            echo "ERROR: unknown argument: $1" >&2
            echo "Usage: $0 [-t TAG] [--platform PLATFORM] [--extract]" >&2
            exit 1
            ;;
    esac
done

# buildx is the only builder that honours --platform together with --load. It
# ships with Docker since 19.03; fail clearly rather than with a cryptic error.
if ! docker buildx version >/dev/null 2>&1; then
    echo "ERROR: 'docker buildx' is not available — install the Docker buildx plugin." >&2
    exit 1
fi

GIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
if ! git diff --quiet HEAD 2>/dev/null; then
    GIT_HASH="${GIT_HASH}-dirty"
fi
GIT_TAG=$(git describe --tags --exact-match HEAD 2>/dev/null || echo "")

# shellcheck source=../versions.env
source versions.env

# Warn when --platform targets an architecture other than the host's: the
# Dockerfile compiles OpenBLAS, MUMPS and every GHC dependency from source, so an
# emulated (QEMU) build runs for hours. Normalize `uname -m` and the platform
# string to Docker's arch names before comparing.
BUILD_ARGS=()
if [[ -n "$PLATFORM" ]]; then
    BUILD_ARGS+=(--platform "$PLATFORM")
    case "$(uname -m)" in
        x86_64|amd64)  HOST_ARCH="amd64" ;;
        aarch64|arm64) HOST_ARCH="arm64" ;;
        *)             HOST_ARCH="$(uname -m)" ;;
    esac
    TARGET_ARCH="${PLATFORM##*/}"
    if [[ "$TARGET_ARCH" != "$HOST_ARCH" ]]; then
        echo "WARNING: building '$PLATFORM' on a '$HOST_ARCH' host runs the whole" >&2
        echo "         from-source build under QEMU emulation and takes hours." >&2
        echo "         Build on a native $TARGET_ARCH host for a fast build." >&2
        echo "         (Emulation also needs QEMU binfmt handlers registered.)" >&2
    fi
fi

echo "Building Docker image: tag=$TAG hash=$GIT_HASH git-tag=${GIT_TAG:-none} alpine=$ALPINE_VERSION platform=${PLATFORM:-host}"

docker buildx build \
    "${BUILD_ARGS[@]}" \
    --load \
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
