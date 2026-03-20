#!/bin/bash
# Download and build PETSc from source.
# Shared between build.sh and Dockerfile.
#
# Required env vars (from versions.env and petsc.env):
#   PETSC_VERSION, PETSC_ARCH, PETSC_URL_BASE
#   PETSC_CONFIGURE_COMMON, PETSC_COPTFLAGS, PETSC_FOPTFLAGS
#
# Optional env vars:
#   PETSC_PLATFORM_OPTS   Platform-specific configure flags (default: Linux)
#   PETSC_DIR             Where to install (default: ./petsc)
#   PYTHON                Python interpreter (default: python3)
#   EXTRA_CONFIGURE_ARGS  Additional configure arguments

set -e

: "${PETSC_VERSION:?PETSC_VERSION is required}"
: "${PETSC_ARCH:?PETSC_ARCH is required}"
: "${PETSC_URL_BASE:?PETSC_URL_BASE is required}"
: "${PETSC_CONFIGURE_COMMON:?PETSC_CONFIGURE_COMMON is required}"

PYTHON="${PYTHON:-python3}"
PETSC_DIR="${PETSC_DIR:-$(pwd)/petsc}"
PETSC_PLATFORM_OPTS="${PETSC_PLATFORM_OPTS:-}"

PETSC_URL="${PETSC_URL_BASE}/petsc-${PETSC_VERSION}.tar.gz"

# Download
if [[ ! -f "petsc-${PETSC_VERSION}.tar.gz" ]]; then
    echo "Downloading PETSc ${PETSC_VERSION}..."
    if command -v curl &>/dev/null; then
        curl -L -o "petsc-${PETSC_VERSION}.tar.gz" "$PETSC_URL"
    else
        wget -q -O "petsc-${PETSC_VERSION}.tar.gz" "$PETSC_URL"
    fi
fi

# Extract
if [[ ! -d "petsc-${PETSC_VERSION}" ]]; then
    echo "Extracting PETSc..."
    tar xzf "petsc-${PETSC_VERSION}.tar.gz"
fi

# Set up PETSC_DIR
if [[ "$(cd petsc-${PETSC_VERSION} && pwd)" != "$(cd "$PETSC_DIR" 2>/dev/null && pwd)" ]]; then
    ln -sfn "petsc-${PETSC_VERSION}" "$PETSC_DIR" 2>/dev/null \
        || { rm -rf "$PETSC_DIR"; cp -r "petsc-${PETSC_VERSION}" "$PETSC_DIR"; }
fi

# Configure
cd "$PETSC_DIR"
echo "Configuring PETSc (optimized, with MUMPS direct solver)..."

# shellcheck disable=SC2086
$PYTHON ./configure \
    $PETSC_CONFIGURE_COMMON \
    $PETSC_PLATFORM_OPTS \
    $EXTRA_CONFIGURE_ARGS \
    COPTFLAGS="$PETSC_COPTFLAGS" \
    FOPTFLAGS="$PETSC_FOPTFLAGS" \
    PETSC_ARCH="$PETSC_ARCH"

# Build
echo "Building PETSc..."
make -j PETSC_DIR="$PETSC_DIR" PETSC_ARCH="$PETSC_ARCH" all

echo "PETSc built successfully"
