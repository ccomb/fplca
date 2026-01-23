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

# Versions
PETSC_VERSION="3.24.2"
SLEPC_VERSION="3.24.1"

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

# Check GHC version
GHC_VERSION=$(ghc --numeric-version)
log_info "GHC version: $GHC_VERSION"
echo ""

# -----------------------------------------------------------------------------
# Locate or download PETSc/SLEPc
# -----------------------------------------------------------------------------

# Default paths
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
        --with-blaslapack-lib="-llapack -lblas" \
        --with-debugging=no \
        COPTFLAGS=-O3 \
        CXXOPTFLAGS=-O3 \
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
echo ""

# -----------------------------------------------------------------------------
# Set up environment
# -----------------------------------------------------------------------------

export LD_LIBRARY_PATH="$PETSC_DIR/$PETSC_ARCH/lib:$SLEPC_DIR/$PETSC_ARCH/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
export C_INCLUDE_PATH="$PETSC_DIR/$PETSC_ARCH/include:$SLEPC_DIR/$PETSC_ARCH/include${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
export CPLUS_INCLUDE_PATH="$PETSC_DIR/$PETSC_ARCH/include:$SLEPC_DIR/$PETSC_ARCH/include${CPLUS_INCLUDE_PATH:+:$CPLUS_INCLUDE_PATH}"
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
cat > cabal.project.local << EOF
extra-lib-dirs: $PETSC_DIR/$PETSC_ARCH/lib
              , $SLEPC_DIR/$PETSC_ARCH/lib
extra-include-dirs: $PETSC_DIR/include
                  , $PETSC_DIR/$PETSC_ARCH/include
                  , $SLEPC_DIR/include
                  , $SLEPC_DIR/$PETSC_ARCH/include
EOF

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
