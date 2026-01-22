#!/bin/bash

# =============================================================================
# fpLCA Desktop Build Script
# =============================================================================
# Builds the desktop application using Tauri, bundling:
# - fplca Haskell backend
# - Elm frontend
# - PETSc/SLEPc libraries
#
# Prerequisites:
#   cargo install tauri-cli --locked
#
# Usage:
#   ./build-desktop.sh [--dev]
#
# Options:
#   --dev     Build for development (skips bundling)
#
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

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

# Parse arguments
DEV_MODE=false
while [[ $# -gt 0 ]]; do
    case $1 in
        --dev)
            DEV_MODE=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# -----------------------------------------------------------------------------
# Check prerequisites
# -----------------------------------------------------------------------------

log_info "Checking prerequisites..."

# Check for cargo
if ! command -v cargo &> /dev/null; then
    log_error "cargo not found. Please install Rust: https://rustup.rs/"
    exit 1
fi

# Check for tauri-cli
if ! cargo tauri --version &> /dev/null; then
    log_warn "tauri-cli not found. Installing..."
    # Use --locked to ensure compatibility with current Rust version
    cargo install tauri-cli --locked
fi

log_success "Prerequisites OK"
echo ""

# -----------------------------------------------------------------------------
# Detect version from git
# -----------------------------------------------------------------------------

log_info "Detecting version..."

# If HEAD is tagged, use the tag name; otherwise use cabal version + "-dev"
if GIT_VERSION=$(git describe --tags --exact-match HEAD 2>/dev/null); then
    VERSION="${GIT_VERSION#v}"  # Remove leading 'v' if present
    log_info "Building version: $VERSION (from tag)"
else
    CABAL_VERSION=$(grep "^version:" "$PROJECT_DIR/fplca.cabal" | awk '{print $2}')
    VERSION="${CABAL_VERSION}-dev"
    log_info "Building version: $VERSION (development)"
fi

echo ""

# -----------------------------------------------------------------------------
# Detect PETSc/SLEPc paths
# -----------------------------------------------------------------------------

# Auto-detect OS
OS_TYPE="$(uname -s)"
case "$OS_TYPE" in
    Linux*)  OS="linux" ;;
    Darwin*) OS="darwin" ;;
    *)       OS="unknown" ;;
esac

# Find PETSc/SLEPc directories
PETSC_DIR="${PETSC_DIR:-$PROJECT_DIR/petsc}"
SLEPC_DIR="${SLEPC_DIR:-$PROJECT_DIR/slepc}"

# Auto-detect PETSC_ARCH
if [[ -z "$PETSC_ARCH" ]]; then
    if [[ -d "$PETSC_DIR/arch-${OS}-c-opt" ]]; then
        PETSC_ARCH="arch-${OS}-c-opt"
    elif [[ -d "$PETSC_DIR/arch-${OS}-c-debug" ]]; then
        PETSC_ARCH="arch-${OS}-c-debug"
    else
        log_error "Could not find PETSc architecture directory"
        log_error "Please set PETSC_ARCH or build PETSc first with ../build.sh"
        exit 1
    fi
fi

log_info "Using PETSc: $PETSC_DIR/$PETSC_ARCH"
log_info "Using SLEPc: $SLEPC_DIR/$PETSC_ARCH"
echo ""

# -----------------------------------------------------------------------------
# Build backend and frontend
# -----------------------------------------------------------------------------

log_info "Building fplca backend and frontend..."
cd "$PROJECT_DIR"

# Touch source files to ensure cabal detects changes (cabal can miss changes sometimes)
touch src/Main.hs src/LCA/CLI/Types.hs src/LCA/CLI/Parser.hs 2>/dev/null || true

./build.sh

log_success "Backend and frontend built"
echo ""

# -----------------------------------------------------------------------------
# Stage resources
# -----------------------------------------------------------------------------

log_info "Staging resources..."

RESOURCES_DIR="$SCRIPT_DIR/resources"
rm -rf "$RESOURCES_DIR"
mkdir -p "$RESOURCES_DIR/lib"
mkdir -p "$RESOURCES_DIR/web"

# Copy fplca binary
FPLCA_BIN=$(find "$PROJECT_DIR/dist-newstyle" -name "fplca" -type f -executable 2>/dev/null | head -1)
if [[ -z "$FPLCA_BIN" ]]; then
    log_error "Could not find fplca binary"
    exit 1
fi
cp "$FPLCA_BIN" "$RESOURCES_DIR/fplca"
log_success "Copied fplca binary"

# Copy default config file for BYOL mode
cp "$SCRIPT_DIR/fplca.toml" "$RESOURCES_DIR/fplca.toml"
log_success "Copied default config"

# Copy web assets
if [[ -d "$PROJECT_DIR/web/dist" ]]; then
    cp -r "$PROJECT_DIR/web/dist/"* "$RESOURCES_DIR/web/"
    log_success "Copied web assets"
else
    log_warn "No web/dist directory found - frontend may not be built"
fi

# Copy PETSc/SLEPc libraries
log_info "Copying PETSc/SLEPc libraries (this may take a moment)..."

PETSC_LIB_DIR="$PETSC_DIR/$PETSC_ARCH/lib"
SLEPC_LIB_DIR="$SLEPC_DIR/$PETSC_ARCH/lib"

# Copy all shared libraries, following symlinks
for lib_dir in "$PETSC_LIB_DIR" "$SLEPC_LIB_DIR"; do
    if [[ -d "$lib_dir" ]]; then
        # Copy .so files (following symlinks to get actual files)
        find "$lib_dir" -maxdepth 1 -name "*.so*" -type f -exec cp -L {} "$RESOURCES_DIR/lib/" \; 2>/dev/null || true
        # Also handle symlinks that point to versioned libraries
        find "$lib_dir" -maxdepth 1 -name "*.so*" -type l -exec cp -L {} "$RESOURCES_DIR/lib/" \; 2>/dev/null || true
    fi
done

# Count libraries
LIB_COUNT=$(find "$RESOURCES_DIR/lib" -name "*.so*" | wc -l)
LIB_SIZE=$(du -sh "$RESOURCES_DIR/lib" 2>/dev/null | cut -f1)
log_success "Copied $LIB_COUNT libraries ($LIB_SIZE)"

echo ""

# -----------------------------------------------------------------------------
# Create placeholder icons if they don't exist
# -----------------------------------------------------------------------------

# Verify icons exist
ICONS_DIR="$SCRIPT_DIR/icons"
if [[ ! -f "$ICONS_DIR/32x32.png" ]] || [[ ! -f "$ICONS_DIR/icon.ico" ]]; then
    log_error "Missing required icons in $ICONS_DIR"
    log_error "Required: 32x32.png, 128x128.png, 128x128@2x.png, icon.ico"
    exit 1
fi

# -----------------------------------------------------------------------------
# Stage resources to target/release for direct binary testing
# -----------------------------------------------------------------------------

log_info "Staging resources to target/release..."
TARGET_DIR="$SCRIPT_DIR/target/release"
mkdir -p "$TARGET_DIR/lib" "$TARGET_DIR/web"
cp "$RESOURCES_DIR/fplca" "$TARGET_DIR/"
cp "$RESOURCES_DIR/fplca.toml" "$TARGET_DIR/" 2>/dev/null || true
cp -r "$RESOURCES_DIR/lib/"* "$TARGET_DIR/lib/" 2>/dev/null || true
cp -r "$RESOURCES_DIR/web/"* "$TARGET_DIR/web/" 2>/dev/null || true
log_success "Resources staged to target/release"

# -----------------------------------------------------------------------------
# Update tauri.conf.json version
# -----------------------------------------------------------------------------

log_info "Updating tauri.conf.json version to $VERSION..."

# Use a temporary file to ensure atomic update
TAURI_CONF="$SCRIPT_DIR/tauri.conf.json"
TAURI_CONF_TMP="$SCRIPT_DIR/tauri.conf.json.tmp"

# Update version field using sed (cross-platform compatible)
sed "s/\"version\": \"[^\"]*\"/\"version\": \"$VERSION\"/" "$TAURI_CONF" > "$TAURI_CONF_TMP"
mv "$TAURI_CONF_TMP" "$TAURI_CONF"

log_success "Updated version in tauri.conf.json"
echo ""

# -----------------------------------------------------------------------------
# Build Tauri app
# -----------------------------------------------------------------------------

cd "$SCRIPT_DIR"

if [[ "$DEV_MODE" == "true" ]]; then
    log_info "Building Tauri app (development mode)..."
    cargo tauri build --debug
else
    log_info "Building Tauri app (release mode)..."
    cargo tauri build --bundles deb
fi

log_success "Tauri build complete"
echo ""

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

echo "============================================================================="
log_success "Desktop build completed!"
echo "============================================================================="
echo ""

# Find generated bundles
if [[ "$DEV_MODE" == "true" ]]; then
    BUNDLE_DIR="$SCRIPT_DIR/target/debug/bundle"
else
    BUNDLE_DIR="$SCRIPT_DIR/target/release/bundle"
fi

if [[ -d "$BUNDLE_DIR" ]]; then
    echo "Generated bundles:"
    find "$BUNDLE_DIR" -type f \( -name "*.AppImage" -o -name "*.deb" \) -exec echo "  {}" \;
    echo ""
fi

echo "To run the desktop app in development mode:"
echo "  cd desktop && cargo tauri dev"
echo ""
