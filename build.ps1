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

# Set up MSVC environment by invoking vcvarsall.bat
function Initialize-VCEnvironment {
    param([string]$VcvarsallPath)

    Write-Info "Setting up MSVC environment..."

    # Run vcvarsall.bat and capture environment variables
    $envBefore = @{}
    Get-ChildItem env: | ForEach-Object { $envBefore[$_.Name] = $_.Value }

    # Use cmd to run vcvarsall and output environment
    $vcOutput = cmd /c "`"$VcvarsallPath`" x64 && set" 2>&1

    # Parse the output and set environment variables
    foreach ($line in $vcOutput) {
        if ($line -match '^([^=]+)=(.*)$') {
            $name = $matches[1]
            $value = $matches[2]
            if ($envBefore[$name] -ne $value) {
                [Environment]::SetEnvironmentVariable($name, $value, 'Process')
            }
        }
    }

    Write-Success "MSVC environment configured"
}

# Download and build PETSc
function Build-PETSc {
    param(
        [string]$PetscDir,
        [string]$PetscArch,
        [string]$PetscVersion,
        [string]$Msys2Bash
    )

    if (-not $Msys2Bash) {
        throw "MSYS2 is required to build PETSc. Install from https://www.msys2.org/"
    }

    Write-Info "Downloading and building PETSc $PetscVersion..."
    Write-Info "This may take 15-30 minutes..."
    Write-Host ""

    $petscUrl = "https://web.cels.anl.gov/projects/petsc/download/release-snapshots/petsc-$PetscVersion.tar.gz"
    $tarball = Join-Path $ScriptDir "petsc-$PetscVersion.tar.gz"
    $extractDir = Join-Path $ScriptDir "petsc-$PetscVersion"

    # Download if not cached
    if (-not (Test-Path $tarball)) {
        Write-Info "Downloading PETSc from $petscUrl..."
        Invoke-WebRequest -Uri $petscUrl -OutFile $tarball
    } else {
        Write-Info "Using cached PETSc tarball"
    }

    # Extract if not already extracted
    if (-not (Test-Path $extractDir)) {
        Write-Info "Extracting PETSc..."
        tar -xzf $tarball -C $ScriptDir
    }

    # Create symlink or copy
    $petscLink = Join-Path $ScriptDir "petsc"
    if (Test-Path $petscLink) {
        Remove-Item -Recurse -Force $petscLink
    }
    # Use junction on Windows (symlinks need admin)
    cmd /c mklink /J "$petscLink" "$extractDir"

    # Convert Windows path to MSYS2 path
    $petscMsysPath = $petscLink -replace '\\', '/' -replace '^([A-Za-z]):', '/$1'

    # Configure PETSc using MSYS2 bash
    Write-Info "Configuring PETSc (optimized, no MPI, no Fortran)..."
    Write-Info "Using MSYS2 bash for configure..."

    $configScript = @"
cd '$petscMsysPath'
python ./configure --with-cc=cl --with-cxx=cl --with-fc=0 --download-f2cblaslapack --with-mpi=0 --with-debugging=0 PETSC_ARCH=$PetscArch
"@

    & $Msys2Bash -l -c $configScript
    if ($LASTEXITCODE -ne 0) {
        throw "PETSc configure failed"
    }

    # Build PETSc using MSYS2 make
    Write-Info "Building PETSc..."
    $buildScript = "cd '$petscMsysPath' && make PETSC_DIR='$petscMsysPath' PETSC_ARCH=$PetscArch all"
    & $Msys2Bash -l -c $buildScript
    if ($LASTEXITCODE -ne 0) {
        throw "PETSc build failed"
    }

    Write-Success "PETSc built successfully"
    Write-Host ""
}

# Download and build SLEPc
function Build-SLEPc {
    param(
        [string]$SlepcDir,
        [string]$PetscDir,
        [string]$PetscArch,
        [string]$SlepcVersion,
        [string]$Msys2Bash
    )

    if (-not $Msys2Bash) {
        throw "MSYS2 is required to build SLEPc. Install from https://www.msys2.org/"
    }

    Write-Info "Downloading and building SLEPc $SlepcVersion..."
    Write-Host ""

    $slepcUrl = "https://slepc.upv.es/download/distrib/slepc-$SlepcVersion.tar.gz"
    $tarball = Join-Path $ScriptDir "slepc-$SlepcVersion.tar.gz"
    $extractDir = Join-Path $ScriptDir "slepc-$SlepcVersion"

    # Download if not cached
    if (-not (Test-Path $tarball)) {
        Write-Info "Downloading SLEPc from $slepcUrl..."
        Invoke-WebRequest -Uri $slepcUrl -OutFile $tarball
    } else {
        Write-Info "Using cached SLEPc tarball"
    }

    # Extract if not already extracted
    if (-not (Test-Path $extractDir)) {
        Write-Info "Extracting SLEPc..."
        tar -xzf $tarball -C $ScriptDir
    }

    # Create symlink
    $slepcLink = Join-Path $ScriptDir "slepc"
    if (Test-Path $slepcLink) {
        Remove-Item -Recurse -Force $slepcLink
    }
    cmd /c mklink /J "$slepcLink" "$extractDir"

    # Convert Windows paths to MSYS2 paths
    $slepcMsysPath = $slepcLink -replace '\\', '/' -replace '^([A-Za-z]):', '/$1'
    $petscMsysPath = $PetscDir -replace '\\', '/' -replace '^([A-Za-z]):', '/$1'

    # Configure SLEPc using MSYS2 bash
    Write-Info "Configuring SLEPc..."
    Write-Info "Using MSYS2 bash for configure..."

    $configScript = @"
export PETSC_DIR='$petscMsysPath'
export SLEPC_DIR='$slepcMsysPath'
export PETSC_ARCH=$PetscArch
cd '$slepcMsysPath'
python ./configure
"@

    & $Msys2Bash -l -c $configScript
    if ($LASTEXITCODE -ne 0) {
        throw "SLEPc configure failed"
    }

    # Build SLEPc using MSYS2 make
    Write-Info "Building SLEPc..."
    $buildScript = "cd '$slepcMsysPath' && make SLEPC_DIR='$slepcMsysPath' PETSC_DIR='$petscMsysPath' PETSC_ARCH=$PetscArch all"
    & $Msys2Bash -l -c $buildScript
    if ($LASTEXITCODE -ne 0) {
        throw "SLEPc build failed"
    }

    Write-Success "SLEPc built successfully"
    Write-Host ""
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

# Check for Visual Studio / MSVC (search known paths, prefer latest version)
$vcvarsall = $null
$vsRoots = @("${env:ProgramFiles}\Microsoft Visual Studio", "${env:ProgramFiles(x86)}\Microsoft Visual Studio")
$vsVersions = @("2026", "2025", "2024", "2023", "2022", "2019")
$vsEditions = @("Enterprise", "Professional", "Community", "BuildTools")

foreach ($vsRoot in $vsRoots) {
    if ($vcvarsall) { break }
    foreach ($vsVer in $vsVersions) {
        if ($vcvarsall) { break }
        foreach ($vsEd in $vsEditions) {
            $candidate = "$vsRoot\$vsVer\$vsEd\VC\Auxiliary\Build\vcvarsall.bat"
            if (Test-Path $candidate) {
                $vcvarsall = Get-Item $candidate
                break
            }
        }
    }
}
if ($vcvarsall) {
    Write-Success "Visual Studio found: $($vcvarsall.DirectoryName)"
} else {
    Write-Error "Visual Studio Build Tools not found"
    Write-Warn "Install Visual Studio with C++ workload (Desktop development with C++)"
    $MissingDeps = $true
}

# Required build tools
foreach ($cmd in @("curl", "tar", "git")) {
    if (-not (Test-Command $cmd)) {
        $MissingDeps = $true
    }
}

# Check for Python (try python, python3, py)
$pythonCmd = $null
foreach ($cmd in @("python", "python3", "py")) {
    if (Get-Command $cmd -ErrorAction SilentlyContinue) {
        $pythonCmd = $cmd
        Write-Success "$cmd found: $((Get-Command $cmd).Source)"
        break
    }
}
if (-not $pythonCmd) {
    Write-Error "python not found (tried: python, python3, py)"
    $MissingDeps = $true
}

# Check for MSYS2 (required for PETSc configure)
$msys2Bash = $null
$msys2Paths = @("C:\msys64\usr\bin\bash.exe", "C:\msys32\usr\bin\bash.exe", "$env:USERPROFILE\msys64\usr\bin\bash.exe")
foreach ($path in $msys2Paths) {
    if (Test-Path $path) {
        $msys2Bash = $path
        Write-Success "MSYS2 found: $path"
        break
    }
}
if (-not $msys2Bash) {
    Write-Warn "MSYS2 not found (required for building PETSc/SLEPc)"
    Write-Warn "Install from: https://www.msys2.org/"
    Write-Warn "Then run: pacman -S python make"
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
$PetscLibDir = "$PetscDir\$PetscArch\lib"
$SlepcLibDir = "$SlepcDir\$PetscArch\lib"

$needPetsc = (-not (Test-Path $PetscLibDir)) -or $All
$needSlepc = (-not (Test-Path $SlepcLibDir)) -or $All

if ($needPetsc -or $needSlepc) {
    # Set up MSVC environment before building
    Initialize-VCEnvironment -VcvarsallPath $vcvarsall.FullName
}

# Download/build PETSc if needed
if ($needPetsc) {
    if (-not $msys2Bash) {
        Write-Error "MSYS2 is required to build PETSc but was not found"
        Write-Host "Install MSYS2 from: https://www.msys2.org/"
        Write-Host "Then run in MSYS2 terminal: pacman -S python make"
        exit 1
    }
    if ($All -and (Test-Path $PetscDir)) {
        Write-Info "Removing existing PETSc for rebuild..."
        Remove-Item -Recurse -Force $PetscDir -ErrorAction SilentlyContinue
        Remove-Item -Force (Join-Path $ScriptDir "petsc-*.tar.gz") -ErrorAction SilentlyContinue
    }
    Build-PETSc -PetscDir $PetscDir -PetscArch $PetscArch -PetscVersion $PetscVersion -Msys2Bash $msys2Bash
}

# Download/build SLEPc if needed
if ($needSlepc) {
    if (-not $msys2Bash) {
        Write-Error "MSYS2 is required to build SLEPc but was not found"
        exit 1
    }
    if ($All -and (Test-Path $SlepcDir)) {
        Write-Info "Removing existing SLEPc for rebuild..."
        Remove-Item -Recurse -Force $SlepcDir -ErrorAction SilentlyContinue
        Remove-Item -Force (Join-Path $ScriptDir "slepc-*.tar.gz") -ErrorAction SilentlyContinue
    }
    Build-SLEPc -SlepcDir $SlepcDir -PetscDir $PetscDir -PetscArch $PetscArch -SlepcVersion $SlepcVersion -Msys2Bash $msys2Bash
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

$PetscIncludeDir = "$PetscDir\include"
$PetscArchIncludeDir = "$PetscDir\$PetscArch\include"
$SlepcIncludeDir = "$SlepcDir\include"
$SlepcArchIncludeDir = "$SlepcDir\$PetscArch\include"

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
