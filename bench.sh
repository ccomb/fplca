#!/bin/bash

# delete cache
rm cache/volca.*

# Script pour lancer le serveur web avec l'interface Elm

set -e

# Variables par défaut
SAMPLE_FOLDER=${SAMPLE_FOLDER:-SAMPLE.min3}
PORT=${PORT:-8080}

# Répertoires
CONFIG="volca.local.toml"
TARGETDIR=${HOME}/Prelab/ecobalyse/dbfiles/
METHOD="${TARGETDIR}/EF-v3.1/ILCD/lciamethods/01500b74-7ffb-463e-9bd4-72f17c2263ff.xml"

# Configuration PETSc - auto-detect architecture
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PETSC_DIR="${SCRIPT_DIR}/petsc"

# Prefer opt build, fallback to debug
if [[ -d "${PETSC_DIR}/arch-linux-c-opt" ]]; then
    PETSC_ARCH="arch-linux-c-opt"
elif [[ -d "${PETSC_DIR}/arch-linux-c-debug" ]]; then
    PETSC_ARCH="arch-linux-c-debug"
else
    echo "ERROR: No PETSc installation found at ${PETSC_DIR}"
    exit 1
fi

export LD_LIBRARY_PATH="${PETSC_DIR}/${PETSC_ARCH}/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

#export PETSC_OPTIONS="-pc_type lu -pc_factor_mat_solver_type mumps -ksp_rtol 1e-15 -ksp_atol 1e-15 -mumps_icntl_7 2 -mumps_icntl_29 2"
# Previous PETSC_OPTIONS - commented out due to broken pipe issues
#export PETSC_OPTIONS="-pc_type lu -pc_factor_mat_solver_type mumps -ksp_rtol 1e-5 -ksp_atol 0 -mumps_icntl_14 100 -mumps_icntl_23 15000 -mumps_icntl_28 2"
# New PETSC_OPTIONS with signal handler disabled to prevent broken pipe crashes
#export PETSC_OPTIONS="-pc_type lu -pc_factor_mat_solver_type mumps -ksp_rtol 1e-5 -ksp_atol 0 -no_signal_handler -error_output_none"
export PETSC_OPTIONS="-pc_type lu -pc_factor_mat_solver_type mumps -ksp_rtol 1e-5 -ksp_atol 0 -mumps_icntl_7 2 -mumps_icntl_14 100 -mumps_icntl_23 15000 -no_signal_handler"

# Build first (backend + frontend)
echo "Building volca..."
./build.sh

# Get the executable path (must match -O2 from build.sh)
BINARY=$(cabal list-bin -O2 volca 2>/dev/null || echo "")
if [[ -z "$BINARY" ]] || [[ ! -x "$BINARY" ]]; then
    echo "ERROR: Could not find volca executable"
    exit 1
fi

echo ""
echo "Starting VoLCA web server..."
echo "Config: $CONFIG"
echo "Method file: $METHOD"
echo "Port: $PORT"
echo "PETSc: ${PETSC_DIR}/${PETSC_ARCH}"
echo "Executable: $BINARY"
echo ""
echo "Web interface will be available at: http://localhost:$PORT/"
echo "API will be available at: http://localhost:$PORT/api/v1/"
echo ""

# Run the server using the pre-built executable
"$BINARY" \
    +RTS -N -H4G -A512M -n16m -qg0 -c -I30 -RTS \
    --config "$CONFIG" \

################################
#  Recommended Configurations  #
################################
#
#  Unified config (cold parse + server):
#
#  +RTS -N -H4G -A512M -n16m -qg0 -c -I30 -RTS
#
#  Why (benchmarked 2026-03-23 on 12-core/24-thread, 96GB RAM):
#  - -N:     Use all cores (24 capabilities)
#  - -H4G:   Pre-allocate 4GB heap (avoids expansion pauses during cold parse)
#  - -A512M: Large nursery (~21MB per capability) — reduces minor GC frequency
#  - -n16m:  Large nursery chunks — reduces allocation overhead
#  - -qg0:   Disable parallel GC for gen-0 (reduces sync overhead with many capabilities)
#  - -c:     Compacting GC to return memory to OS after loading
#  - -I30:   Idle GC after 30s (cleanup between requests)
#
#  Benchmark results (cold parse, 9 databases, 4 parallel at level 0):
#    BASELINE  -N -H512M -A16M           : 55.0s, peak ~1140% CPU
#    COLD-START -N -H2G -A64M -n8m       : 47.6s, peak ~1430% CPU
#    BIG-NURSERY -N -H2G -A256M -n8m     : 45.8s, peak ~1502% CPU
#    FEWER-CAPS -N12 -H2G -A128M -n8m    : 49.8s, peak  ~847% CPU
#    NO-PAR-GC -N -H2G -A128M -qg        : 78.0s, peak  ~612% CPU (terrible)
#    BALANCED  -N -H4G -A512M -n16m -qg0 : 44.3s, peak ~1583% CPU (winner)
#
#  Expected RSS: ~5-6GB during cold parse, drops to ~1-2GB after compaction

