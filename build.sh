#!/bin/bash

# =============================================================================
# fplca build script with PETSc/SLEPc
# =============================================================================
# This script builds fplca with PETSc/SLEPc integration.
# It can automatically download and build PETSc/SLEPc if not found.
#
# Usage:
#   ./build.sh [options]
#
# Options:
#   --help              Show this help message
#   --clean             Clean build artifacts before building
#   --all               Force re-download and rebuild of PETSc/SLEPc
#   --test              Run tests after building
#   --desktop           Build desktop application (Tauri bundle)
#
# Environment variables:
#   PETSC_DIR           Path to PETSc installation
#   SLEPC_DIR           Path to SLEPc installation
#   PETSC_ARCH          PETSc architecture (default: arch-linux-c-opt)
#
# Examples:
#   ./build.sh                      # Download deps if needed and build
#   ./build.sh --all                # Force re-download everything
#   ./build.sh --test               # Build and run tests
#
# =============================================================================

set -e

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source version definitions from central file
if [[ -f "$SCRIPT_DIR/versions.env" ]]; then
    # shellcheck source=versions.env
    source "$SCRIPT_DIR/versions.env"
else
    echo "ERROR: versions.env not found in $SCRIPT_DIR"
    exit 1
fi

# Detect OS
OS_TYPE="$(uname -s)"
case "$OS_TYPE" in
    Linux*)  OS="linux" ;;
    Darwin*) OS="darwin" ;;
    *)       OS="unknown" ;;
esac

# Defaults
FORCE_REBUILD=false
RUN_TESTS=false
CLEAN_BUILD=false
BUILD_DESKTOP=false

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

show_help() {
    sed -n '3,27p' "$0" | sed 's/^# //' | sed 's/^#//'
    exit 0
}

check_command() {
    local cmd="$1"
    local required="${2:-true}"

    if command -v "$cmd" &> /dev/null; then
        log_success "$cmd found: $(command -v "$cmd")"
        return 0
    else
        if [[ "$required" == "true" ]]; then
            log_error "$cmd not found"
        else
            log_warn "$cmd not found (optional)"
        fi
        return 1
    fi
}

# Check tool version against expected version (exact match)
check_version() {
    local tool="$1"
    local actual="$2"
    local expected="$3"

    if [[ "$actual" == "$expected" ]]; then
        log_success "$tool version $actual"
        return 0
    else
        log_warn "$tool version $actual (expected $expected)"
        return 1
    fi
}

# -----------------------------------------------------------------------------
# Parse arguments
# -----------------------------------------------------------------------------

while [[ $# -gt 0 ]]; do
    case $1 in
        --help|-h)
            show_help
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --all)
            FORCE_REBUILD=true
            shift
            ;;
        --test)
            RUN_TESTS=true
            shift
            ;;
        --desktop)
            BUILD_DESKTOP=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Set default PETSC_ARCH based on OS (optimized build with 64-bit indices)
# Auto-detect existing PETSC_ARCH if not set - must exist in both PETSc and SLEPc
if [[ -z "$PETSC_ARCH" ]]; then
    PETSC_DIR_CHECK=${PETSC_DIR:-"$SCRIPT_DIR/petsc"}
    SLEPC_DIR_CHECK=${SLEPC_DIR:-"$SCRIPT_DIR/slepc"}

    # Prefer opt, but only if both PETSc and SLEPc have it
    if [[ -d "$PETSC_DIR_CHECK/arch-${OS}-c-opt" && -d "$SLEPC_DIR_CHECK/arch-${OS}-c-opt" ]]; then
        PETSC_ARCH="arch-${OS}-c-opt"
    elif [[ -d "$PETSC_DIR_CHECK/arch-${OS}-c-debug" && -d "$SLEPC_DIR_CHECK/arch-${OS}-c-debug" ]]; then
        PETSC_ARCH="arch-${OS}-c-debug"
    else
        # Default for fresh builds
        PETSC_ARCH="arch-${OS}-c-opt"
    fi
fi

echo ""
log_info "Build configuration:"
log_info "  OS: $OS"
log_info "  PETSC_ARCH: $PETSC_ARCH"
echo ""

# -----------------------------------------------------------------------------
# Check dependencies
# -----------------------------------------------------------------------------

log_info "Checking dependencies..."

MISSING_DEPS=false

# Required build tools
for cmd in gcc g++ make python3 curl tar; do
    if ! check_command "$cmd"; then
        MISSING_DEPS=true
    fi
done

# Fortran compiler (needed for PETSc)
if ! check_command "gfortran"; then
    log_warn "gfortran not found - PETSc build may fail"
    log_warn "Install with: sudo apt install gfortran"
fi

# Haskell toolchain
for cmd in ghc cabal; do
    if ! check_command "$cmd"; then
        MISSING_DEPS=true
        log_warn "Install GHC and Cabal via ghcup: https://www.haskell.org/ghcup/"
    fi
done

# Node.js (needed for frontend build)
if ! check_command "npm"; then
    MISSING_DEPS=true
    log_warn "Install Node.js: https://nodejs.org/"
fi

# Rust (needed for desktop build)
if command -v rustc &> /dev/null; then
    log_success "rustc found: $(command -v rustc)"
else
    log_warn "rustc not found (optional, needed for desktop build)"
fi

# Elm (needed for frontend build)
if command -v elm &> /dev/null; then
    log_success "elm found: $(command -v elm)"
else
    log_warn "elm not found (optional, installed via npm)"
fi

if [[ "$MISSING_DEPS" == "true" ]]; then
    log_error "Missing required dependencies. Please install them and try again."
    echo ""
    if [[ "$OS" == "darwin" ]]; then
        echo "On macOS:"
        echo "  brew install gcc python3 curl node"
        echo ""
    else
        echo "On Debian/Ubuntu:"
        echo "  sudo apt install build-essential python3 curl gfortran liblapack-dev libblas-dev nodejs npm"
        echo ""
        echo "On Fedora:"
        echo "  sudo dnf install gcc gcc-c++ gcc-gfortran make python3 curl lapack-devel blas-devel nodejs npm"
        echo ""
        echo "On Arch Linux:"
        echo "  sudo pacman -S base-devel python curl gcc-fortran lapack blas nodejs npm"
        echo ""
    fi
    echo "For Haskell toolchain:"
    echo "  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
    exit 1
fi

# -----------------------------------------------------------------------------
# Check tool versions
# -----------------------------------------------------------------------------

log_info "Checking tool versions..."

# Check GHC version
GHC_ACTUAL=$(ghc --numeric-version)
check_version "GHC" "$GHC_ACTUAL" "$GHC_VERSION"

# Check Node version
if command -v node &> /dev/null; then
    NODE_ACTUAL=$(node --version | sed 's/^v//')
    check_version "Node.js" "$NODE_ACTUAL" "$NODE_VERSION"
fi

# Check Rust version (optional, for desktop build)
if command -v rustc &> /dev/null; then
    RUST_ACTUAL=$(rustc --version | awk '{print $2}')
    check_version "Rust" "$RUST_ACTUAL" "$RUST_VERSION"
fi

# Check Elm version (optional, installed via npm if missing)
if command -v elm &> /dev/null; then
    ELM_ACTUAL=$(elm --version 2>/dev/null || echo "unknown")
    if [[ "$ELM_ACTUAL" != "unknown" ]]; then
        check_version "Elm" "$ELM_ACTUAL" "$ELM_VERSION"
    fi
fi

echo ""

# -----------------------------------------------------------------------------
# Locate or download PETSc/SLEPc
# -----------------------------------------------------------------------------

# Check for system PETSc/SLEPc (Debian/Ubuntu packages)
detect_system_petsc() {
    # Debian/Ubuntu install to /usr/lib/petscdir/petsc<version>/<arch>
    local petsc_base="/usr/lib/petscdir"
    if [[ -d "$petsc_base" ]]; then
        # Find the latest version
        local latest=$(ls -d "$petsc_base"/petsc*/x86_64-linux-gnu-real 2>/dev/null | sort -V | tail -1)
        if [[ -n "$latest" && -d "$latest" ]]; then
            echo "$latest"
            return 0
        fi
    fi
    return 1
}

detect_system_slepc() {
    # Debian/Ubuntu install to /usr/lib/slepcdir/slepc<version>/<arch>
    local slepc_base="/usr/lib/slepcdir"
    if [[ -d "$slepc_base" ]]; then
        # Find the latest version
        local latest=$(ls -d "$slepc_base"/slepc*/x86_64-linux-gnu-real 2>/dev/null | sort -V | tail -1)
        if [[ -n "$latest" && -d "$latest" ]]; then
            echo "$latest"
            return 0
        fi
    fi
    return 1
}

# Check for system packages first
USE_SYSTEM_PETSC=false
SYSTEM_PETSC_DIR=""
SYSTEM_SLEPC_DIR=""

if SYSTEM_PETSC_DIR=$(detect_system_petsc) && SYSTEM_SLEPC_DIR=$(detect_system_slepc); then
    # Both system packages found
    if [[ "$FORCE_REBUILD" != "true" ]]; then
        USE_SYSTEM_PETSC=true
        log_success "Found system PETSc: $SYSTEM_PETSC_DIR"
        log_success "Found system SLEPc: $SYSTEM_SLEPC_DIR"

        # Extract version from path (e.g., petsc3.22 -> 3.22)
        SYSTEM_PETSC_VERSION=$(echo "$SYSTEM_PETSC_DIR" | grep -oP 'petsc\K[0-9.]+')
        SYSTEM_SLEPC_VERSION=$(echo "$SYSTEM_SLEPC_DIR" | grep -oP 'slepc\K[0-9.]+')
        log_info "System PETSc version: $SYSTEM_PETSC_VERSION"
        log_info "System SLEPc version: $SYSTEM_SLEPC_VERSION"
    else
        log_info "System PETSc/SLEPc found but --all forces rebuild from source"
    fi
fi

# Default paths (used when building from source)
PETSC_DIR=${PETSC_DIR:-"$SCRIPT_DIR/petsc"}
SLEPC_DIR=${SLEPC_DIR:-"$SCRIPT_DIR/slepc"}

download_and_build_petsc() {
    log_info "Downloading and building PETSc $PETSC_VERSION..."
    log_info "This will take 10-30 minutes depending on your system..."
    echo ""

    local PETSC_URL="https://web.cels.anl.gov/projects/petsc/download/release-snapshots/petsc-$PETSC_VERSION.tar.gz"

    cd "$SCRIPT_DIR"

    if [[ ! -f "petsc-$PETSC_VERSION.tar.gz" ]]; then
        log_info "Downloading PETSc from $PETSC_URL..."
        curl -L -o "petsc-$PETSC_VERSION.tar.gz" "$PETSC_URL"
    else
        log_info "Using cached PETSc tarball"
    fi

    if [[ ! -d "petsc-$PETSC_VERSION" ]]; then
        log_info "Extracting PETSc..."
        tar xzf "petsc-$PETSC_VERSION.tar.gz"
    fi

    # Create symlink
    ln -sfn "petsc-$PETSC_VERSION" petsc
    PETSC_DIR="$SCRIPT_DIR/petsc"

    cd "$PETSC_DIR"

    # Configure PETSc (optimized, no MPI for simpler build)
    # Uses system BLAS/LAPACK for performance (OpenBLAS on most Linux distros)
    log_info "Configuring PETSc (optimized, no MPI)..."

    python3 ./configure \
        --with-mpi=0 \
        --with-cxx=0 \
        --with-blaslapack-lib="-llapack -lblas" \
        --with-debugging=no \
        COPTFLAGS=-O3 \
        PETSC_ARCH="$PETSC_ARCH"

    log_info "Building PETSc..."
    make -j PETSC_DIR="$PETSC_DIR" PETSC_ARCH="$PETSC_ARCH" all

    log_success "PETSc built successfully"
    echo ""
}

download_and_build_slepc() {
    log_info "Downloading and building SLEPc $SLEPC_VERSION..."
    echo ""

    local SLEPC_URL="https://slepc.upv.es/download/distrib/slepc-$SLEPC_VERSION.tar.gz"

    cd "$SCRIPT_DIR"

    if [[ ! -f "slepc-$SLEPC_VERSION.tar.gz" ]]; then
        log_info "Downloading SLEPc from $SLEPC_URL..."
        curl -L -o "slepc-$SLEPC_VERSION.tar.gz" "$SLEPC_URL"
    else
        log_info "Using cached SLEPc tarball"
    fi

    if [[ ! -d "slepc-$SLEPC_VERSION" ]]; then
        log_info "Extracting SLEPc..."
        tar xzf "slepc-$SLEPC_VERSION.tar.gz"
    fi

    # Create symlink
    ln -sfn "slepc-$SLEPC_VERSION" slepc
    SLEPC_DIR="$SCRIPT_DIR/slepc"

    cd "$SLEPC_DIR"

    # Configure and build SLEPc
    log_info "Configuring SLEPc..."
    export PETSC_DIR
    export SLEPC_DIR
    export PETSC_ARCH

    python3 ./configure

    log_info "Building SLEPc..."
    make -j SLEPC_DIR="$SLEPC_DIR" PETSC_DIR="$PETSC_DIR" PETSC_ARCH="$PETSC_ARCH"

    log_success "SLEPc built successfully"
    echo ""
}

# Clone petsc-hs if needed
PETSC_HS_DIR="$SCRIPT_DIR/petsc-hs"
if [[ ! -d "$PETSC_HS_DIR" ]]; then
    log_info "Cloning petsc-hs..."
    git clone https://github.com/ccomb/petsc-hs.git "$PETSC_HS_DIR"
elif [[ "$FORCE_REBUILD" == "true" ]]; then
    # Don't git pull - we have local PETSc 3.24 compatibility fixes
    # Just clean the cabal build artifacts to force recompilation
    log_info "Cleaning petsc-hs build artifacts..."
    rm -rf "$PETSC_HS_DIR/dist-newstyle"
fi

# Generate TypesC2HsGen.hs if it doesn't exist
TYPES_C2HS_GEN="$PETSC_HS_DIR/src/Numerical/PETSc/Internal/C2HsGen/TypesC2HsGen.hs"
if [[ ! -f "$TYPES_C2HS_GEN" ]]; then
    log_info "Generating TypesC2HsGen.hs..."
    runhaskell "$PETSC_HS_DIR/src/Numerical/PETSc/Internal/C2HsGen/GenerateC2Hs.hs" > "$TYPES_C2HS_GEN"
fi

# Check if PETSc was built with MPI and patch petsc-hs.cabal accordingly
PETSC_HS_CABAL="$PETSC_HS_DIR/petsc-hs.cabal"
PETSC_LIB_CHECK="${PETSC_DIR:-$SCRIPT_DIR/petsc}/${PETSC_ARCH:-arch-linux-c-opt}/lib"
if [[ -f "$PETSC_LIB_CHECK/libmpi.so" ]] || [[ -f "$PETSC_LIB_CHECK/libmpich.so" ]]; then
    # PETSc was built with MPI, keep mpi dependency
    log_info "Patching petsc-hs.cabal to remove MPI dependency..."
    if grep -q "petsc, mpich, slepc" "$PETSC_HS_CABAL" 2>/dev/null; then
        # Replace mpich with mpi (the actual library name in the PETSc build)
        sed -i 's/petsc, mpich, slepc/petsc, mpi, slepc/g' "$PETSC_HS_CABAL"
    fi
elif grep -q "mpich" "$PETSC_HS_CABAL" 2>/dev/null; then
    # PETSc was built without MPI, remove mpi dependency
    log_info "Patching petsc-hs.cabal to remove MPI dependency..."
    sed -i 's/petsc, mpich, slepc/petsc, slepc/g' "$PETSC_HS_CABAL"
fi

# Use system packages or build from source
if [[ "$USE_SYSTEM_PETSC" == "true" ]]; then
    # Use system packages
    PETSC_DIR="$SYSTEM_PETSC_DIR"
    SLEPC_DIR="$SYSTEM_SLEPC_DIR"
    PETSC_ARCH=""  # System packages don't use PETSC_ARCH
    USE_SYSTEM_LIBS=true

    # Debian uses libpetsc_real.so and libslepc_real.so
    PETSC_LIB_NAME="petsc_real"
    SLEPC_LIB_NAME="slepc_real"

    # Patch petsc-hs.cabal to use system library names
    if ! grep -q "petsc_real" "$PETSC_HS_CABAL" 2>/dev/null; then
        log_info "Patching petsc-hs.cabal for system library names..."
        sed -i 's/petsc, slepc/petsc_real, slepc_real/g' "$PETSC_HS_CABAL"
    fi

    log_success "Using system PETSc: $PETSC_DIR"
    log_success "Using system SLEPc: $SLEPC_DIR"
else
    USE_SYSTEM_LIBS=false
    PETSC_LIB_NAME="petsc"
    SLEPC_LIB_NAME="slepc"

    # Download/build PETSc if needed or forced
    if [[ "$FORCE_REBUILD" == "true" ]] || [[ ! -d "$PETSC_DIR/$PETSC_ARCH" ]]; then
        if [[ "$FORCE_REBUILD" == "true" && -d "$PETSC_DIR" ]]; then
            log_info "Removing existing PETSc for rebuild..."
            rm -rf "$PETSC_DIR" petsc-*.tar.gz petsc-[0-9]*
        fi
        download_and_build_petsc
    fi

    # Download/build SLEPc if needed or forced
    if [[ "$FORCE_REBUILD" == "true" ]] || [[ ! -d "$SLEPC_DIR/$PETSC_ARCH" ]]; then
        if [[ "$FORCE_REBUILD" == "true" && -d "$SLEPC_DIR" ]]; then
            log_info "Removing existing SLEPc for rebuild..."
            rm -rf "$SLEPC_DIR" slepc-*.tar.gz slepc-[0-9]*
        fi
        download_and_build_slepc
    fi

    log_success "Using PETSc: $PETSC_DIR/$PETSC_ARCH"
    log_success "Using SLEPc: $SLEPC_DIR/$PETSC_ARCH"
fi
echo ""

# -----------------------------------------------------------------------------
# Set up environment
# -----------------------------------------------------------------------------

if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    # System packages: libs in /usr/lib, includes in the petscdir/slepcdir paths
    PETSC_LIB_DIR="/usr/lib/x86_64-linux-gnu"
    SLEPC_LIB_DIR="/usr/lib/x86_64-linux-gnu"
    PETSC_INCLUDE_DIR="$PETSC_DIR/include"
    SLEPC_INCLUDE_DIR="$SLEPC_DIR/include"
else
    # Custom build: libs in PETSC_ARCH subdirectory, includes in both main and arch dirs
    PETSC_LIB_DIR="$PETSC_DIR/$PETSC_ARCH/lib"
    SLEPC_LIB_DIR="$SLEPC_DIR/$PETSC_ARCH/lib"
    # Main headers (petscdm.h, etc.) are in $PETSC_DIR/include
    # Arch-specific headers (petscconf.h, etc.) are in $PETSC_DIR/$PETSC_ARCH/include
    PETSC_INCLUDE_DIR="$PETSC_DIR/include"
    PETSC_ARCH_INCLUDE_DIR="$PETSC_DIR/$PETSC_ARCH/include"
    SLEPC_INCLUDE_DIR="$SLEPC_DIR/include"
    SLEPC_ARCH_INCLUDE_DIR="$SLEPC_DIR/$PETSC_ARCH/include"
fi

export LD_LIBRARY_PATH="$PETSC_LIB_DIR:$SLEPC_LIB_DIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    export C_INCLUDE_PATH="$PETSC_INCLUDE_DIR:$SLEPC_INCLUDE_DIR${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
    export CPLUS_INCLUDE_PATH="$PETSC_INCLUDE_DIR:$SLEPC_INCLUDE_DIR${CPLUS_INCLUDE_PATH:+:$CPLUS_INCLUDE_PATH}"
else
    export C_INCLUDE_PATH="$PETSC_INCLUDE_DIR:$PETSC_ARCH_INCLUDE_DIR:$SLEPC_INCLUDE_DIR:$SLEPC_ARCH_INCLUDE_DIR${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
    export CPLUS_INCLUDE_PATH="$PETSC_INCLUDE_DIR:$PETSC_ARCH_INCLUDE_DIR:$SLEPC_INCLUDE_DIR:$SLEPC_ARCH_INCLUDE_DIR${CPLUS_INCLUDE_PATH:+:$CPLUS_INCLUDE_PATH}"
fi
export PETSC_DIR
export SLEPC_DIR
export PETSC_ARCH

# -----------------------------------------------------------------------------
# Clean if requested
# -----------------------------------------------------------------------------

if [[ "$CLEAN_BUILD" == "true" ]]; then
    log_info "Cleaning build artifacts..."
    cd "$SCRIPT_DIR"
    rm -rf dist-newstyle cabal.project.local
    if [[ -d "$SCRIPT_DIR/petsc-hs" ]]; then
        cd "$SCRIPT_DIR/petsc-hs"
        rm -rf dist-newstyle cabal.project.local
    fi
    log_success "Clean complete"
    echo ""
fi

# -----------------------------------------------------------------------------
# Build fplca (petsc-hs is built automatically as a dependency)
# -----------------------------------------------------------------------------

log_info "Building fplca..."
cd "$SCRIPT_DIR"

# Write cabal.project.local with library paths
if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    cat > cabal.project.local << EOF
extra-lib-dirs: $PETSC_LIB_DIR
              , $SLEPC_LIB_DIR
extra-include-dirs: $PETSC_INCLUDE_DIR
                  , $SLEPC_INCLUDE_DIR
EOF
else
    # Custom build needs both main and arch-specific include directories
    cat > cabal.project.local << EOF
extra-lib-dirs: $PETSC_LIB_DIR
              , $SLEPC_LIB_DIR
extra-include-dirs: $PETSC_INCLUDE_DIR
                  , $PETSC_ARCH_INCLUDE_DIR
                  , $SLEPC_INCLUDE_DIR
                  , $SLEPC_ARCH_INCLUDE_DIR
EOF
fi

if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    log_info "Using system libraries: $PETSC_LIB_NAME, $SLEPC_LIB_NAME"
fi

cabal build -O2

log_success "fplca built successfully"
echo ""

# -----------------------------------------------------------------------------
# Build frontend
# -----------------------------------------------------------------------------

if [[ -d "$SCRIPT_DIR/web" ]]; then
    log_info "Building frontend..."
    cd "$SCRIPT_DIR/web"
    if [[ ! -d "node_modules" ]]; then
        npm install --silent
    fi
    ./build.sh
    log_success "Frontend built successfully"
    echo ""
fi

# -----------------------------------------------------------------------------
# Run tests
# -----------------------------------------------------------------------------

if [[ "$RUN_TESTS" == "true" ]]; then
    log_info "Running tests..."
    cd "$SCRIPT_DIR"
    cabal test --test-show-details=streaming
    log_success "Tests passed"
    echo ""
fi

# -----------------------------------------------------------------------------
# Build desktop application (if requested)
# -----------------------------------------------------------------------------

if [[ "$BUILD_DESKTOP" == "true" ]]; then
    log_info "Building desktop application..."
    cd "$SCRIPT_DIR/desktop"
    ./build-desktop.sh
    log_success "Desktop application built"
    echo ""
fi

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

echo ""
echo "============================================================================="
log_success "Build completed successfully!"
echo "============================================================================="
echo ""
echo "To run fplca, first set up the library path:"
echo ""
echo "  export LD_LIBRARY_PATH=\"$PETSC_DIR/$PETSC_ARCH/lib:$SLEPC_DIR/$PETSC_ARCH/lib:\$LD_LIBRARY_PATH\""
echo ""
echo "Then run:"
echo ""
echo "  cabal run fplca -- --help"
echo ""

# Find the built executable
FPLCA_BIN=$(find "$SCRIPT_DIR/dist-newstyle" -name "fplca" -type f -executable 2>/dev/null | head -1)
if [[ -n "$FPLCA_BIN" ]]; then
    echo "Executable: $FPLCA_BIN"
    echo ""
fi

# Show desktop build instructions if not already built
if [[ "$BUILD_DESKTOP" != "true" ]]; then
    echo "To build the desktop application:"
    echo ""
    echo "  ./build.sh --desktop"
    echo ""
    echo "Or manually:"
    echo "  cd desktop && ./build-desktop.sh"
    echo ""
fi
