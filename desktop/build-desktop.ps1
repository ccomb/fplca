# =============================================================================
# fpLCA Desktop Build Script for Windows
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
#   .\build-desktop.ps1 [-Dev]
#
# Options:
#   -Dev     Build for development (skips bundling)
#
# =============================================================================

param(
    [switch]$Dev
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectDir = Split-Path -Parent $ScriptDir

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

function Write-Info {
    param([string]$Message)
    Write-Host "[INFO] " -ForegroundColor Blue -NoNewline
    Write-Host $Message
}

function Write-Success {
    param([string]$Message)
    Write-Host "[OK] " -ForegroundColor Green -NoNewline
    Write-Host $Message
}

function Write-Warn {
    param([string]$Message)
    Write-Host "[WARN] " -ForegroundColor Yellow -NoNewline
    Write-Host $Message
}

function Write-Error {
    param([string]$Message)
    Write-Host "[ERROR] " -ForegroundColor Red -NoNewline
    Write-Host $Message
}

# -----------------------------------------------------------------------------
# Check prerequisites
# -----------------------------------------------------------------------------

Write-Info "Checking prerequisites..."

# Check for cargo
if (-not (Get-Command cargo -ErrorAction SilentlyContinue)) {
    Write-Error "cargo not found. Please install Rust: https://rustup.rs/"
    exit 1
}

# Check for tauri-cli
$tauriVersion = & cargo tauri --version 2>$null
if (-not $tauriVersion) {
    Write-Warn "tauri-cli not found. Installing..."
    cargo install tauri-cli --locked
}

Write-Success "Prerequisites OK"
Write-Host ""

# -----------------------------------------------------------------------------
# Detect version from git
# -----------------------------------------------------------------------------

Write-Info "Detecting version..."

$gitVersion = $null
try {
    $gitVersion = git describe --tags --exact-match HEAD 2>$null
} catch {
    # No exact tag match - expected for development builds
}
if ($gitVersion) {
    $Version = $gitVersion -replace '^v', ''
    Write-Info "Building version: $Version (from tag)"
} else {
    $cabalContent = Get-Content (Join-Path $ProjectDir "fplca.cabal") | Select-String "^version:"
    $cabalVersion = ($cabalContent -split '\s+')[1]
    # MSI requires numeric-only versions, so use base version for dev builds
    $Version = $cabalVersion
    Write-Info "Building version: $Version (development)"
}

Write-Host ""

# -----------------------------------------------------------------------------
# Detect PETSc/SLEPc paths
# -----------------------------------------------------------------------------

if (-not $env:PETSC_DIR) {
    $env:PETSC_DIR = Join-Path $ProjectDir "petsc"
}
if (-not $env:SLEPC_DIR) {
    $env:SLEPC_DIR = Join-Path $ProjectDir "slepc"
}

$PetscDir = $env:PETSC_DIR
$SlepcDir = $env:SLEPC_DIR

# Auto-detect PETSC_ARCH
if (-not $env:PETSC_ARCH) {
    $archOpt = Join-Path $PetscDir "arch-mswin-c-opt"
    $archMsys2 = Join-Path $PetscDir "arch-msys2-c-opt"
    $archDebug = Join-Path $PetscDir "arch-mswin-c-debug"

    if (Test-Path $archOpt) {
        $PetscArch = "arch-mswin-c-opt"
    } elseif (Test-Path $archMsys2) {
        $PetscArch = "arch-msys2-c-opt"
    } elseif (Test-Path $archDebug) {
        $PetscArch = "arch-mswin-c-debug"
    } else {
        Write-Error "Could not find PETSc architecture directory"
        Write-Error "Please set PETSC_ARCH or build PETSc first with ..\build.ps1"
        exit 1
    }
} else {
    $PetscArch = $env:PETSC_ARCH
}

Write-Info "Using PETSc: $PetscDir\$PetscArch"
Write-Info "Using SLEPc: $SlepcDir\$PetscArch"
Write-Host ""

# -----------------------------------------------------------------------------
# Build backend and frontend
# -----------------------------------------------------------------------------

Write-Info "Building fplca backend and frontend..."
Set-Location $ProjectDir

# Touch source files to ensure cabal detects changes
$touchFiles = @("src/Main.hs", "src/LCA/CLI/Types.hs", "src/LCA/CLI/Parser.hs")
foreach ($file in $touchFiles) {
    $fullPath = Join-Path $ProjectDir $file
    if (Test-Path $fullPath) {
        (Get-Item $fullPath).LastWriteTime = Get-Date
    }
}

& "$ProjectDir\build.ps1"

Write-Success "Backend and frontend built"
Write-Host ""

# -----------------------------------------------------------------------------
# Stage resources
# -----------------------------------------------------------------------------

Write-Info "Staging resources..."

$ResourcesDir = Join-Path $ScriptDir "resources"
Remove-Item -Recurse -Force $ResourcesDir -ErrorAction SilentlyContinue
New-Item -ItemType Directory -Force -Path $ResourcesDir | Out-Null
New-Item -ItemType Directory -Force -Path (Join-Path $ResourcesDir "lib") | Out-Null
New-Item -ItemType Directory -Force -Path (Join-Path $ResourcesDir "web") | Out-Null

# Copy fplca binary
$fplcaBin = Get-ChildItem -Path (Join-Path $ProjectDir "dist-newstyle") -Recurse -Filter "fplca.exe" -ErrorAction SilentlyContinue | Select-Object -First 1
if (-not $fplcaBin) {
    Write-Error "Could not find fplca.exe binary"
    exit 1
}
# Copy as both fplca.exe (for Rust code) and fplca (for Tauri resource bundling)
Copy-Item $fplcaBin.FullName (Join-Path $ResourcesDir "fplca.exe")
Copy-Item $fplcaBin.FullName (Join-Path $ResourcesDir "fplca")
Write-Success "Copied fplca binary"

# Copy default config file for BYOL mode
Copy-Item (Join-Path $ScriptDir "fplca.toml") (Join-Path $ResourcesDir "fplca.toml")
Write-Success "Copied default config"

# Copy web assets
$webDistDir = Join-Path $ProjectDir "web\dist"
if (Test-Path $webDistDir) {
    Copy-Item -Recurse -Force "$webDistDir\*" (Join-Path $ResourcesDir "web")
    Write-Success "Copied web assets"
} else {
    Write-Warn "No web\dist directory found - frontend may not be built"
}

# Copy PETSc/SLEPc libraries (DLLs on Windows)
Write-Info "Copying PETSc/SLEPc libraries..."

$PetscLibDir = Join-Path $PetscDir "$PetscArch\lib"
$SlepcLibDir = Join-Path $SlepcDir "$PetscArch\lib"
$destLibDir = Join-Path $ResourcesDir "lib"

foreach ($libDir in @($PetscLibDir, $SlepcLibDir)) {
    if (Test-Path $libDir) {
        # Copy DLLs
        Get-ChildItem -Path $libDir -Filter "*.dll" -ErrorAction SilentlyContinue | ForEach-Object {
            Copy-Item $_.FullName $destLibDir -Force
        }
        # Also copy any static libs that might be needed
        Get-ChildItem -Path $libDir -Filter "*.lib" -ErrorAction SilentlyContinue | ForEach-Object {
            Copy-Item $_.FullName $destLibDir -Force
        }
    }
}

# Copy OpenBLAS and its dependencies from MSYS2 (required for PETSc performance)
$Msys2BinDir = "C:\msys64\ucrt64\bin"
$openBlasDlls = @(
    "libopenblas.dll",
    "libgcc_s_seh-1.dll",
    "libgfortran-5.dll",
    "libgomp-1.dll",
    "libwinpthread-1.dll",
    "libquadmath-0.dll"
)
$copiedCount = 0
foreach ($dll in $openBlasDlls) {
    $src = Join-Path $Msys2BinDir $dll
    if (Test-Path $src) {
        Copy-Item $src $destLibDir -Force
        $copiedCount++
    }
}
if ($copiedCount -gt 0) {
    Write-Success "Copied $copiedCount OpenBLAS libraries"
} else {
    Write-Warn "OpenBLAS DLLs not found in $Msys2BinDir - performance may be degraded"
}

$libCount = (Get-ChildItem -Path $destLibDir -Filter "*.dll" -ErrorAction SilentlyContinue).Count
$libSize = "{0:N2} MB" -f ((Get-ChildItem -Path $destLibDir -Recurse | Measure-Object -Property Length -Sum).Sum / 1MB)
Write-Success "Copied $libCount libraries ($libSize)"

Write-Host ""

# -----------------------------------------------------------------------------
# Create placeholder icons if they don't exist
# -----------------------------------------------------------------------------

$IconsDir = Join-Path $ScriptDir "icons"
if (-not (Test-Path (Join-Path $IconsDir "icon.ico"))) {
    Write-Info "Creating placeholder icons..."
    New-Item -ItemType Directory -Force -Path $IconsDir | Out-Null

    # Create placeholder PNG icons using Python
    $pythonScript = @'
import struct
import zlib
import os
import sys

def create_rgba_png(filename, width, height, r, g, b, a=255):
    signature = b'\x89PNG\r\n\x1a\n'
    ihdr_data = struct.pack('>IIBBBBB', width, height, 8, 6, 0, 0, 0)
    ihdr_crc = zlib.crc32(b'IHDR' + ihdr_data) & 0xffffffff
    ihdr = struct.pack('>I', 13) + b'IHDR' + ihdr_data + struct.pack('>I', ihdr_crc)
    raw_data = b''
    for y in range(height):
        raw_data += b'\x00'
        for x in range(width):
            raw_data += bytes([r, g, b, a])
    compressed = zlib.compress(raw_data, 9)
    idat_crc = zlib.crc32(b'IDAT' + compressed) & 0xffffffff
    idat = struct.pack('>I', len(compressed)) + b'IDAT' + compressed + struct.pack('>I', idat_crc)
    iend_crc = zlib.crc32(b'IEND') & 0xffffffff
    iend = struct.pack('>I', 0) + b'IEND' + struct.pack('>I', iend_crc)
    with open(filename, 'wb') as f:
        f.write(signature + ihdr + idat + iend)

icons_dir = sys.argv[1]
green = (76, 175, 80)
create_rgba_png(f'{icons_dir}/32x32.png', 32, 32, *green)
create_rgba_png(f'{icons_dir}/128x128.png', 128, 128, *green)
create_rgba_png(f'{icons_dir}/128x128@2x.png', 256, 256, *green)
'@

    $pythonScript | python - $IconsDir

    # Create a simple ICO file (Windows icon) - just copy the 32x32 PNG for now
    # Real ICO files need proper format but Tauri can work with PNG
    Write-Warn "Note: Create a proper icon.ico file for production builds"

    Write-Success "Created placeholder icons"
}

# -----------------------------------------------------------------------------
# Stage resources to target/release for direct binary testing
# -----------------------------------------------------------------------------

Write-Info "Staging resources to target\release..."
$TargetDir = Join-Path $ScriptDir "target\release"
New-Item -ItemType Directory -Force -Path $TargetDir | Out-Null
New-Item -ItemType Directory -Force -Path (Join-Path $TargetDir "lib") | Out-Null
New-Item -ItemType Directory -Force -Path (Join-Path $TargetDir "web") | Out-Null

Copy-Item (Join-Path $ResourcesDir "fplca.exe") $TargetDir -Force
Copy-Item (Join-Path $ResourcesDir "fplca") $TargetDir -Force
Copy-Item (Join-Path $ResourcesDir "fplca.toml") $TargetDir -Force -ErrorAction SilentlyContinue
Copy-Item -Recurse -Force (Join-Path $ResourcesDir "lib\*") (Join-Path $TargetDir "lib") -ErrorAction SilentlyContinue
Copy-Item -Recurse -Force (Join-Path $ResourcesDir "web\*") (Join-Path $TargetDir "web") -ErrorAction SilentlyContinue
Write-Success "Resources staged to target\release"

# -----------------------------------------------------------------------------
# Update tauri.conf.json version
# -----------------------------------------------------------------------------

Write-Info "Updating tauri.conf.json version to $Version..."

$tauriConf = Join-Path $ScriptDir "tauri.conf.json"
$content = Get-Content $tauriConf -Raw
$content = $content -replace '"version": "[^"]*"', "`"version`": `"$Version`""
Set-Content -Path $tauriConf -Value $content

Write-Success "Updated version in tauri.conf.json"
Write-Host ""

# -----------------------------------------------------------------------------
# Build Tauri app
# -----------------------------------------------------------------------------

Set-Location $ScriptDir

if ($Dev) {
    Write-Info "Building Tauri app (development mode)..."
    cargo tauri build --debug
} else {
    Write-Info "Building Tauri app (release mode)..."
    # Build MSI and NSIS installers for Windows
    cargo tauri build --bundles msi,nsis
}

if ($LASTEXITCODE -ne 0) {
    Write-Error "Tauri build failed"
    exit 1
}

Write-Success "Tauri build complete"
Write-Host ""

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

Write-Host "============================================================================="
Write-Success "Desktop build completed!"
Write-Host "============================================================================="
Write-Host ""

# Find generated bundles
if ($Dev) {
    $BundleDir = Join-Path $ScriptDir "target\debug\bundle"
} else {
    $BundleDir = Join-Path $ScriptDir "target\release\bundle"
}

if (Test-Path $BundleDir) {
    Write-Host "Generated bundles:"
    Get-ChildItem -Path $BundleDir -Recurse -Include "*.msi", "*.exe" | ForEach-Object {
        Write-Host "  $($_.FullName)"
    }
    Write-Host ""
}

Write-Host "To run the desktop app in development mode:"
Write-Host "  cd desktop; cargo tauri dev"
Write-Host ""
