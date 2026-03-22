#!/bin/bash
# Fast build for git worktrees.
# Reuses PETSc and petsc-hs from the main repo via symlinks,
# skipping the ~20min PETSc build and dependency checks.
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

# Validate main repo has built PETSc and petsc-hs
for dep in petsc petsc-hs; do
    [[ -d "$VOLCA_MAIN/$dep" ]] || { log_error "$dep not found at $VOLCA_MAIN/$dep — build the main repo first"; exit 1; }
done

PETSC_ARCH=$(detect_existing_petsc_arch "$VOLCA_MAIN/petsc")
[[ -f "$VOLCA_MAIN/petsc/$PETSC_ARCH/lib/libpetsc.a" ]] || { log_error "PETSc not built at $VOLCA_MAIN/petsc/$PETSC_ARCH"; exit 1; }

# Symlink shared deps (idempotent)
# petsc-hs is a gitlink (submodule without .gitmodules), so git creates
# an empty directory in worktrees — remove it before symlinking.
for dep in petsc petsc-hs; do
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

PETSC_DIR="$(readlink -f "$SCRIPT_DIR/petsc")"
PETSC_LIB_DIR="$PETSC_DIR/$PETSC_ARCH/lib" \
PETSC_INCLUDE_DIR="$PETSC_DIR/include" \
PETSC_ARCH_INCLUDE_DIR="$PETSC_DIR/$PETSC_ARCH/include" \
LINK_MODE="static" \
"$SCRIPT_DIR/gen-cabal-config.sh"

# Build
export LD_LIBRARY_PATH="$PETSC_DIR/$PETSC_ARCH/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
cabal build -j

if [[ "$RUN_TESTS" == "true" ]]; then
    cabal test -j
fi

log_success "Build complete"
