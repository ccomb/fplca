#!/bin/bash
# Generate cabal.project.local for building volca with PETSc.
# Shared between build.sh and Dockerfile.
#
# Required env vars:
#   PETSC_LIB_DIR         Path to PETSc libraries
#   PETSC_INCLUDE_DIR     Path to PETSc headers
#
# Optional env vars:
#   PETSC_ARCH_INCLUDE_DIR  Path to arch-specific PETSc headers (non-system builds)
#   LINK_MODE               "dynamic" (default), "static", "system", "windows"
#   OUTPUT_DIR              Where to write cabal.project.local (default: current dir)
#
# Output: writes cabal.project.local in OUTPUT_DIR

set -e

: "${PETSC_LIB_DIR:?PETSC_LIB_DIR is required}"
: "${PETSC_INCLUDE_DIR:?PETSC_INCLUDE_DIR is required}"
LINK_MODE="${LINK_MODE:-dynamic}"
OUTPUT="${OUTPUT_DIR:-.}/cabal.project.local"

case "$LINK_MODE" in
    system)
        # Debian/Ubuntu system packages: shared linking with petsc_real + system MPI
        cat > "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $PETSC_LIB_DIR
extra-include-dirs: $PETSC_INCLUDE_DIR

-- System packages use petsc_real library name and system MPI
package petsc-hs
  ghc-options: -optl-lpetsc_real -optl-lmpi

package volca
  ghc-options: -optl-lpetsc_real -optl-lmpi
EOF
        ;;

    dynamic)
        # Shared linking: PETSc/MPI as shared libs (Docker, dev builds)
        cat > "$OUTPUT" << EOF
optimization: 2

extra-lib-dirs: $PETSC_LIB_DIR
extra-include-dirs: $PETSC_INCLUDE_DIR
                  , ${PETSC_ARCH_INCLUDE_DIR:-$PETSC_INCLUDE_DIR}

package petsc-hs
  ghc-options: -optl-L$PETSC_LIB_DIR -optl-lmpi

package volca
  ghc-options: -optl-L$PETSC_LIB_DIR -optl-lmpi
EOF
        ;;

    static)
        # Static linking: embed PETSc/MUMPS/MPI, keep gfortran/libc dynamic
        STATIC_LINK_FLAGS="-optl-L$PETSC_LIB_DIR -optl-Wl,-Bstatic -optl-Wl,--start-group -optl-lpetsc -optl-ldmumps -optl-lmumps_common -optl-lpord -optl-lscalapack -optl-lfblas -optl-lflapack -optl-lmpifort -optl-lmpi -optl-Wl,--end-group -optl-Wl,-Bdynamic -optl-lgfortran -optl-lquadmath -optl-lpthread -optl-lm -optl-ldl -optl-lrt"
        cat > "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $PETSC_LIB_DIR
extra-include-dirs: $PETSC_INCLUDE_DIR
                  , ${PETSC_ARCH_INCLUDE_DIR:-$PETSC_INCLUDE_DIR}

package petsc-hs
  extra-lib-dirs: $PETSC_LIB_DIR
  ghc-options: $STATIC_LINK_FLAGS

package volca
  ghc-options: $STATIC_LINK_FLAGS
EOF
        ;;

    windows)
        # Windows/MSYS2: complex linker flags for MinGW + OpenBLAS + MS-MPI
        : "${MSYS2_LIB_DIR:?MSYS2_LIB_DIR is required for windows mode}"
        : "${GCC_LIB_DIR:?GCC_LIB_DIR is required for windows mode}"
        : "${MPI_INCLUDE_DIR:?MPI_INCLUDE_DIR is required for windows mode}"
        cat > "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $PETSC_LIB_DIR
              , $MSYS2_LIB_DIR
extra-include-dirs: $PETSC_INCLUDE_DIR
                  , ${PETSC_ARCH_INCLUDE_DIR:-$PETSC_INCLUDE_DIR}
                  , $MPI_INCLUDE_DIR

-- Link MinGW runtime libs and OpenBLAS needed by PETSc (built with UCRT64)
package volca
  ghc-options: -optl-Wl,--allow-multiple-definition -optl-L$GCC_LIB_DIR -optl-L$MSYS2_LIB_DIR -optl-L$PETSC_LIB_DIR -optl-lscalapack -optl-ldmumps -optl-lmumps_common -optl-lpord -optl-l:libmsmpi.dll.a -optl-lopenblas -optl-lgfortran -optl-lgcc -optl-lquadmath -optl-lmingwex -optl-lpthread -optl-lmsvcrt
EOF
        ;;

    *)
        echo "ERROR: Unknown LINK_MODE: $LINK_MODE" >&2
        exit 1
        ;;
esac

echo "Generated $OUTPUT (mode=$LINK_MODE)"
