# =============================================================================
# fplca build script for Windows with PETSc/SLEPc
# =============================================================================
# This script builds fplca with PETSc/SLEPc integration on Windows.
# Requires Visual Studio Build Tools, Python 3, and Haskell toolchain.
#
# Usage:
#   .\build.ps1 [options]
#
# Options:
#   -Help              Show this help message
#   -Clean             Clean build artifacts before building
#   -All               Force re-download and rebuild of PETSc/SLEPc
#   -Test              Run tests after building
#   -Desktop           Build desktop application (Tauri bundle)
#
# Environment variables:
#   PETSC_DIR          Path to PETSc installation
#   SLEPC_DIR          Path to SLEPc installation
#
# Prerequisites:
#   - Visual Studio (or Build Tools) with C++ workload
#   - Python 3.x
#   - GHC and Cabal (via ghcup)
#   - Node.js and npm
#
# =============================================================================

param(
    [switch]$Help,
    [switch]$Clean,
    [switch]$All,
    [switch]$Test,
    [switch]$Desktop
)

$ErrorActionPreference = "Stop"

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

# Versions
$PetscVersion = "3.24.2"
$SlepcVersion = "3.24.1"

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

function Show-Help {
    Get-Content $PSCommandPath | Select-Object -Skip 2 -First 24 | ForEach-Object { $_ -replace '^# ?', '' }
    exit 0
}

function Test-Command {
    param(
        [string]$Command,
        [bool]$Required = $true
    )

    $cmd = Get-Command $Command -ErrorAction SilentlyContinue
    if ($cmd) {
        Write-Success "$Command found: $($cmd.Source)"
        return $true
    } else {
        if ($Required) {
            Write-Error "$Command not found"
        } else {
            Write-Warn "$Command not found (optional)"
        }
        return $false
    }
}

# -----------------------------------------------------------------------------
# Parse arguments
# -----------------------------------------------------------------------------

if ($Help) {
    Show-Help
}

Write-Host ""
Write-Info "Build configuration:"
Write-Info "  OS: Windows"
Write-Host ""

# -----------------------------------------------------------------------------
# Check dependencies
# -----------------------------------------------------------------------------

Write-Info "Checking dependencies..."

$MissingDeps = $false

# Check for Visual Studio / MSVC (search both Program Files locations, prefer latest version)
$vcvarsall = $null
$vsPaths = @("${env:ProgramFiles}\Microsoft Visual Studio", "${env:ProgramFiles(x86)}\Microsoft Visual Studio")
$allVcvars = foreach ($vsPath in $vsPaths) {
    if (Test-Path $vsPath) {
        Get-ChildItem -Path $vsPath -Recurse -Filter "vcvarsall.bat" -ErrorAction SilentlyContinue
    }
}
# Sort by path descending to prefer newer versions (2026 > 2022 > 2019)
$vcvarsall = $allVcvars | Sort-Object -Property FullName -Descending | Select-Object -First 1
if ($vcvarsall) {
    Write-Success "Visual Studio found: $($vcvarsall.DirectoryName)"
} else {
    Write-Error "Visual Studio Build Tools not found"
    Write-Warn "Install Visual Studio with C++ workload (Desktop development with C++)"
    $MissingDeps = $true
}

# Required build tools
foreach ($cmd in @("python", "curl", "tar", "git")) {
    if (-not (Test-Command $cmd)) {
        $MissingDeps = $true
    }
}

# Haskell toolchain
foreach ($cmd in @("ghc", "cabal")) {
    if (-not (Test-Command $cmd)) {
        $MissingDeps = $true
        Write-Warn "Install GHC and Cabal via ghcup: https://www.haskell.org/ghcup/"
    }
}

# Node.js
if (-not (Test-Command "npm")) {
    $MissingDeps = $true
    Write-Warn "Install Node.js: https://nodejs.org/"
}

if ($MissingDeps) {
    Write-Error "Missing required dependencies. Please install them and try again."
    Write-Host ""
    Write-Host "Prerequisites for Windows:"
    Write-Host "  1. Visual Studio (or Build Tools) with C++ workload"
    Write-Host "  2. Python 3.x (https://www.python.org/)"
    Write-Host "  3. Git for Windows (https://git-scm.com/)"
    Write-Host "  4. Node.js (https://nodejs.org/)"
    Write-Host "  5. GHC and Cabal via ghcup (https://www.haskell.org/ghcup/)"
    Write-Host ""
    exit 1
}

# Check GHC version
$ghcVersion = & ghc --numeric-version
Write-Info "GHC version: $ghcVersion"
Write-Host ""

# -----------------------------------------------------------------------------
# Locate or set up PETSc/SLEPc paths
# -----------------------------------------------------------------------------

# Default paths
if (-not $env:PETSC_DIR) {
    $env:PETSC_DIR = Join-Path $ScriptDir "petsc"
}
if (-not $env:SLEPC_DIR) {
    $env:SLEPC_DIR = Join-Path $ScriptDir "slepc"
}

$PetscDir = $env:PETSC_DIR
$SlepcDir = $env:SLEPC_DIR

# On Windows, use a simplified arch name
$PetscArch = "arch-mswin-c-opt"
if ($env:PETSC_ARCH) {
    $PetscArch = $env:PETSC_ARCH
}

# Check if PETSc/SLEPc are already built
$PetscLibDir = Join-Path $PetscDir $PetscArch "lib"
$SlepcLibDir = Join-Path $SlepcDir $PetscArch "lib"

if (-not (Test-Path $PetscLibDir)) {
    Write-Warn "PETSc not found at: $PetscLibDir"
    Write-Host ""
    Write-Host "PETSc/SLEPc must be built separately on Windows."
    Write-Host ""
    Write-Host "Build instructions:"
    Write-Host "  1. Open 'x64 Native Tools Command Prompt for VS'"
    Write-Host "  2. Clone PETSc:"
    Write-Host "     git clone https://gitlab.com/petsc/petsc.git"
    Write-Host "     cd petsc"
    Write-Host "  3. Configure PETSc:"
    Write-Host "     python configure --with-cc=cl --with-cxx=cl --with-fc=0 \"
    Write-Host "         --download-openblas --with-mpi=0 --with-debugging=0"
    Write-Host "  4. Build PETSc (follow the instructions from configure output)"
    Write-Host "  5. Clone and build SLEPc similarly"
    Write-Host ""
    Write-Host "Set environment variables:"
    Write-Host "  `$env:PETSC_DIR = 'C:\path\to\petsc'"
    Write-Host "  `$env:SLEPC_DIR = 'C:\path\to\slepc'"
    Write-Host "  `$env:PETSC_ARCH = 'arch-mswin-c-opt'"
    Write-Host ""

    if (-not $All) {
        exit 1
    }
}

Write-Success "Using PETSc: $PetscDir\$PetscArch"
Write-Success "Using SLEPc: $SlepcDir\$PetscArch"
Write-Host ""

# -----------------------------------------------------------------------------
# Clone petsc-hs if needed
# -----------------------------------------------------------------------------

$PetscHsDir = Join-Path $ScriptDir "petsc-hs"
if (-not (Test-Path $PetscHsDir)) {
    Write-Info "Cloning petsc-hs..."
    git clone https://github.com/ccomb/petsc-hs.git $PetscHsDir
} elseif ($All) {
    Write-Info "Cleaning petsc-hs build artifacts..."
    Remove-Item -Recurse -Force (Join-Path $PetscHsDir "dist-newstyle") -ErrorAction SilentlyContinue
}

# -----------------------------------------------------------------------------
# Set up environment
# -----------------------------------------------------------------------------

$PetscIncludeDir = Join-Path $PetscDir "include"
$PetscArchIncludeDir = Join-Path $PetscDir $PetscArch "include"
$SlepcIncludeDir = Join-Path $SlepcDir "include"
$SlepcArchIncludeDir = Join-Path $SlepcDir $PetscArch "include"

# Add library paths to PATH for DLL discovery
$env:PATH = "$PetscLibDir;$SlepcLibDir;$env:PATH"

# Set include paths for C compiler
$env:INCLUDE = "$PetscIncludeDir;$PetscArchIncludeDir;$SlepcIncludeDir;$SlepcArchIncludeDir;$env:INCLUDE"

# Export for child processes
$env:PETSC_DIR = $PetscDir
$env:SLEPC_DIR = $SlepcDir
$env:PETSC_ARCH = $PetscArch

# -----------------------------------------------------------------------------
# Clean if requested
# -----------------------------------------------------------------------------

if ($Clean) {
    Write-Info "Cleaning build artifacts..."
    Set-Location $ScriptDir
    Remove-Item -Recurse -Force "dist-newstyle" -ErrorAction SilentlyContinue
    Remove-Item -Force "cabal.project.local" -ErrorAction SilentlyContinue
    if (Test-Path $PetscHsDir) {
        Set-Location $PetscHsDir
        Remove-Item -Recurse -Force "dist-newstyle" -ErrorAction SilentlyContinue
        Remove-Item -Force "cabal.project.local" -ErrorAction SilentlyContinue
    }
    Write-Success "Clean complete"
    Write-Host ""
}

# -----------------------------------------------------------------------------
# Build fplca
# -----------------------------------------------------------------------------

Write-Info "Building fplca..."
Set-Location $ScriptDir

# Write cabal.project.local with library paths
$cabalProjectLocal = @"
extra-lib-dirs: $PetscLibDir
              , $SlepcLibDir
extra-include-dirs: $PetscIncludeDir
                  , $PetscArchIncludeDir
                  , $SlepcIncludeDir
                  , $SlepcArchIncludeDir
"@

Set-Content -Path "cabal.project.local" -Value $cabalProjectLocal

cabal build -O2

if ($LASTEXITCODE -ne 0) {
    Write-Error "Build failed"
    exit 1
}

Write-Success "fplca built successfully"
Write-Host ""

# -----------------------------------------------------------------------------
# Build frontend
# -----------------------------------------------------------------------------

$webDir = Join-Path $ScriptDir "web"
if (Test-Path $webDir) {
    Write-Info "Building frontend..."
    Set-Location $webDir

    if (-not (Test-Path "node_modules")) {
        npm install --silent
    }

    # Check for build script
    if (Test-Path "build.ps1") {
        & .\build.ps1
    } elseif (Test-Path "build.sh") {
        # Try to run with bash if available (Git Bash)
        if (Get-Command bash -ErrorAction SilentlyContinue) {
            bash .\build.sh
        } else {
            # Fallback: run elm make directly
            npx elm make src/Main.elm --optimize --output=dist/app.js
        }
    }

    Write-Success "Frontend built successfully"
    Write-Host ""
}

# -----------------------------------------------------------------------------
# Run tests
# -----------------------------------------------------------------------------

if ($Test) {
    Write-Info "Running tests..."
    Set-Location $ScriptDir
    cabal test --test-show-details=streaming

    if ($LASTEXITCODE -ne 0) {
        Write-Error "Tests failed"
        exit 1
    }

    Write-Success "Tests passed"
    Write-Host ""
}

# -----------------------------------------------------------------------------
# Build desktop application (if requested)
# -----------------------------------------------------------------------------

if ($Desktop) {
    Write-Info "Building desktop application..."
    $desktopDir = Join-Path $ScriptDir "desktop"
    Set-Location $desktopDir
    & .\build-desktop.ps1
    Write-Success "Desktop application built"
    Write-Host ""
}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

Write-Host ""
Write-Host "============================================================================="
Write-Success "Build completed successfully!"
Write-Host "============================================================================="
Write-Host ""
Write-Host "To run fplca, first ensure the library path includes PETSc/SLEPc DLLs:"
Write-Host ""
Write-Host "  `$env:PATH = `"$PetscLibDir;$SlepcLibDir;`$env:PATH`""
Write-Host ""
Write-Host "Then run:"
Write-Host ""
Write-Host "  cabal run fplca -- --help"
Write-Host ""

# Find the built executable
$fplcaBin = Get-ChildItem -Path (Join-Path $ScriptDir "dist-newstyle") -Recurse -Filter "fplca.exe" -ErrorAction SilentlyContinue | Select-Object -First 1
if ($fplcaBin) {
    Write-Host "Executable: $($fplcaBin.FullName)"
    Write-Host ""
}

# Show desktop build instructions if not already built
if (-not $Desktop) {
    Write-Host "To build the desktop application:"
    Write-Host ""
    Write-Host "  .\build.ps1 -Desktop"
    Write-Host ""
    Write-Host "Or manually:"
    Write-Host "  cd desktop; .\build-desktop.ps1"
    Write-Host ""
}
