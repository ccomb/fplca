#!/bin/bash
# Generate cabal.project.local for building volca with MUMPS_SEQ.
# Shared between build.sh and Dockerfile.
#
# Optional env vars:
#   MUMPS_LIB_DIR           Path to MUMPS libraries (default: system)
#   MUMPS_INCLUDE_DIR       Path to MUMPS headers (default: /usr/include)
#   LINK_MODE               "dynamic" (default), "musl", "darwin", "windows"
#   OUTPUT_DIR              Where to write cabal.project.local (default: current dir)
#
# Output: writes cabal.project.local in OUTPUT_DIR

set -e

MUMPS_LIB_DIR="${MUMPS_LIB_DIR:-/usr/lib/x86_64-linux-gnu}"
MUMPS_INCLUDE_DIR="${MUMPS_INCLUDE_DIR:-/usr/include}"
LINK_MODE="${LINK_MODE:-dynamic}"
OUTPUT="${OUTPUT_DIR:-.}/cabal.project.local"

# Parallelism preamble shared by every LINK_MODE.
# Lives in cabal.project.local (not cabal.project) so that Docker builds —
# which copy only volca.cabal + mumps-hs/ into the build context and write a
# minimal cabal.project without `packages: .` machinery — still get jobs +
# RTS allocation area + per-module GHC parallelism.
cat > "$OUTPUT" << 'EOF'
jobs: $ncpus

program-options
  ghc-options: -j +RTS -A128m -RTS

EOF

case "$LINK_MODE" in
    dynamic)
        # Shared linking (Linux, macOS, Docker, dev builds)
        cat >> "$OUTPUT" << EOF
optimization: 2

extra-lib-dirs: $MUMPS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR
EOF
        ;;

    musl)
        # Fully static link for Linux against musl libc (Alpine). musl's
        # `dlopen` / `getaddrinfo` / NSS lookups all resolve inside a static
        # binary without dragging in the host's libc, so the resulting
        # executable is genuinely portable across Linux distros.
        #
        # Alpine packages ship LAPACK/BLAS as shared libs only (no .a), so
        # OpenBLAS — which bundles both BLAS and LAPACK in a single static
        # archive — must be built from source and pointed at via OPENBLAS_LIB_DIR.
        : "${OPENBLAS_LIB_DIR:?OPENBLAS_LIB_DIR is required for musl mode (path to libopenblas.a)}"
        case "$(uname -m)" in
            x86_64|amd64) QUADMATH_FLAG="-optl-lquadmath" ;;
            *)            QUADMATH_FLAG="" ;;
        esac
        # --gc-sections drops unreferenced sections from the final exe.
        # Effective on the C/Fortran archives that were compiled with
        # -ffunction-sections / -fdata-sections (OpenBLAS in our pipeline);
        # harmless on the others.
        MUSL_LINK_FLAGS="-optl-L$MUMPS_LIB_DIR -optl-L$OPENBLAS_LIB_DIR -optl-Wl,--gc-sections -optl-Wl,--start-group -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-lopenblas -optl-lgfortran $QUADMATH_FLAG -optl-Wl,--end-group -optl-lpthread -optl-lm"
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True
shared: False
executable-static: True

extra-lib-dirs: $MUMPS_LIB_DIR
                $OPENBLAS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package volca
  ghc-options: $MUSL_LINK_FLAGS
EOF
        ;;

    darwin)
        # macOS arm64: locally-built MUMPS (.a only) + Homebrew openblas + Homebrew gcc gfortran/quadmath.
        # ld64 picks .a from extra-lib-dirs when no .dylib is present, so no GNU -Bstatic/-Bdynamic.
        # Accelerate.framework is rejected: its LAPACK ABI does not match what build-mumps.sh emits.
        BREW_PREFIX="$(brew --prefix 2>/dev/null || echo /opt/homebrew)"
        OPENBLAS_PREFIX=$(brew --prefix openblas 2>/dev/null || echo "${BREW_PREFIX}/opt/openblas")
        # Homebrew gcc lays out libgfortran/libquadmath under lib/gcc/<major>/
        GFORTRAN_LIB_DIR=$(ls -d "${BREW_PREFIX}/Cellar/gcc/"*/lib/gcc/*/ 2>/dev/null | sort -V | tail -1)
        : "${GFORTRAN_LIB_DIR:?Could not locate Homebrew gcc libgfortran — install with: brew install gcc}"
        DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:?MACOSX_DEPLOYMENT_TARGET must be set (source versions.env)}"
        # -Wl,-dead_strip and -Wl,-dead_strip_dylibs let ld64 prune unreferenced
        # sections and unused dylib load commands. Pairs with `split-sections: True`
        # below for a meaningful (5–15 %) size win before strip even runs.
        DARWIN_LINK_FLAGS="-optl-L$MUMPS_LIB_DIR -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-L${OPENBLAS_PREFIX}/lib -optl-lopenblas -optl-L${GFORTRAN_LIB_DIR} -optl-lgfortran -optl-lquadmath -optl-lpthread -optl-lm -optl-mmacosx-version-min=${DEPLOYMENT_TARGET} -optl-Wl,-dead_strip -optl-Wl,-dead_strip_dylibs"
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $MUMPS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package mumps-hs
  extra-lib-dirs: $MUMPS_LIB_DIR
  ghc-options: $DARWIN_LINK_FLAGS

package volca
  ghc-options: $DARWIN_LINK_FLAGS
EOF
        ;;

    windows)
        # Windows/MSYS2: MinGW + OpenBLAS
        # Auto-discover MSYS2/GCC paths and convert POSIX-style MUMPS paths
        # to Windows form. Callers running under MSYS2 bash (build.sh,
        # prebuild-cabal-store.yml) used to duplicate this block; factoring
        # it here keeps the per-caller code to LINK_MODE=windows.
        if [[ -z "${MSYS2_LIB_DIR:-}" ]]; then
            MSYS2_LIB_DIR=$(cygpath -m /ucrt64/lib)
            : "${MSYS2_LIB_DIR:?cygpath -m /ucrt64/lib returned empty — is MSYS2 ucrt64 installed?}"
        fi
        if [[ -z "${GCC_LIB_DIR:-}" ]]; then
            GCC_LIB_DIR=$(find /ucrt64/lib/gcc/x86_64-w64-mingw32 -maxdepth 1 -type d 2>/dev/null | sort -V | tail -1)
            : "${GCC_LIB_DIR:?Could not locate GCC lib dir under /ucrt64/lib/gcc/x86_64-w64-mingw32 — install mingw-w64-ucrt-x86_64-gcc}"
            GCC_LIB_DIR=$(cygpath -m "$GCC_LIB_DIR")
        fi
        # Cabal + clang on Windows want forward-slash drive-letter paths
        # (`C:/foo/bar`), not the MSYS2 `/c/foo/bar` form. Convert if needed.
        win_path() { echo "$1" | sed 's|^/\([a-zA-Z]\)/|\1:/|'; }
        case "$MUMPS_LIB_DIR" in
            /[a-zA-Z]/*) MUMPS_LIB_DIR=$(win_path "$MUMPS_LIB_DIR") ;;
        esac
        case "$MUMPS_INCLUDE_DIR" in
            /[a-zA-Z]/*) MUMPS_INCLUDE_DIR=$(win_path "$MUMPS_INCLUDE_DIR") ;;
        esac
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $MUMPS_LIB_DIR
              , $MSYS2_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package volca
  ghc-options: -optl-Wl,--allow-multiple-definition -optl-L$GCC_LIB_DIR -optl-L$MSYS2_LIB_DIR -optl-L$MUMPS_LIB_DIR -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-lopenblas -optl-lgfortran -optl-lgcc -optl-lquadmath -optl-lmingwex -optl-lpthread -optl-lmsvcrt
EOF
        ;;

    *)
        echo "ERROR: Unknown LINK_MODE: $LINK_MODE" >&2
        exit 1
        ;;
esac

echo "Generated $OUTPUT (mode=$LINK_MODE)"
