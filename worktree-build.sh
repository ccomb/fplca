#!/bin/bash
# Fast build for git worktrees.
# Reuses mumps-hs from the main repo via symlink,
# skipping dependency checks.
#
# Usage (from a worktree):
#   ./worktree-build.sh [--test] [--clean]
#
# Or specify main repo explicitly:
#   VOLCA_MAIN=/path/to/main/repo ./worktree-build.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

# Parse args
RUN_TESTS=false
CLEAN=false
for arg in "$@"; do
    case "$arg" in
        --test)  RUN_TESTS=true ;;
        --clean) CLEAN=true ;;
        *)       log_error "Unknown option: $arg (supported: --test, --clean)"; exit 1 ;;
    esac
done

# Find main repo via git worktree list
VOLCA_MAIN="${VOLCA_MAIN:-$(git worktree list --porcelain | head -1 | sed 's/^worktree //')}"

# Validate main repo has mumps-hs
[[ -d "$VOLCA_MAIN/mumps-hs" ]] || { log_error "mumps-hs not found at $VOLCA_MAIN/mumps-hs — build the main repo first"; exit 1; }

# Symlink shared deps (idempotent)
for dep in mumps-hs; do
    if [[ -d "$SCRIPT_DIR/$dep" && ! -L "$SCRIPT_DIR/$dep" ]]; then
        rmdir "$SCRIPT_DIR/$dep" 2>/dev/null || true
    fi
    ln -sfn "$VOLCA_MAIN/$dep" "$SCRIPT_DIR/$dep"
done

if [[ "$CLEAN" == "true" ]]; then
    log_info "Cleaning build artifacts..."
    rm -rf "$SCRIPT_DIR/dist-newstyle" "$SCRIPT_DIR/cabal.project.local"
fi

# Generate Version.hs and cabal.project.local
OS=$(detect_os)
BUILD_TARGET="$OS" "$SCRIPT_DIR/gen-version.sh"

MUMPS_LIB_DIR="/usr/lib/x86_64-linux-gnu" \
MUMPS_INCLUDE_DIR="/usr/include" \
LINK_MODE="dynamic" \
"$SCRIPT_DIR/gen-cabal-config.sh"

# Build
cabal build -j

if [[ "$RUN_TESTS" == "true" ]]; then
    cabal test -j
fi

log_success "Build complete"
