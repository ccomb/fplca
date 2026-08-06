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

# Optimization level for volca's own code, as a per-package override of the
# global `optimization: 2` (which must stay 2 so the prebuilt cabal store's
# deps keep matching). Default 2 — shipped artifacts (Docker, release, local
# builds) are unaffected. CI PR/test builds export VOLCA_OPT_LEVEL=1 to halve
# the cold compile of volca's ~100 modules; -O2 buys runtime speed the smoke
# build doesn't need. Deps stay -O2 either way.
VOLCA_OPT_LEVEL="${VOLCA_OPT_LEVEL:-2}"

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

package volca
  optimization: $VOLCA_OPT_LEVEL
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
        #
        # -z stack-size=8388608: bake an 8 MB PT_GNU_STACK into the ELF.
        # musl reads this header at startup and uses it as the default
        # pthread stack size (its hardcoded fallback is 128 KB, vs glibc's
        # 8 MB picked up from RLIMIT_STACK). OpenBLAS DYNAMIC_ARCH Fortran
        # kernels have large auto-arrays that overflow 128 KB on the first
        # BLAS3 call inside MUMPS factorization (SIGSEGV / exit 139).
        # Setting it at link time covers every pthread the binary creates
        # — RTS capabilities and OpenBLAS workers alike — without patching
        # OpenBLAS source. (An earlier attempt to sed the stack size into
        # OpenBLAS's blas_server.c was a no-op: the relevant block sits
        # under #ifdef NEED_STACKATTR, which blas_server.c #undef's
        # unconditionally on Linux.)
        MUSL_LINK_FLAGS="-optl-L$MUMPS_LIB_DIR -optl-L$OPENBLAS_LIB_DIR -optl-Wl,--gc-sections -optl-Wl,-z,stack-size=8388608 -optl-Wl,--start-group -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-lopenblas -optl-lgfortran $QUADMATH_FLAG -optl-Wl,--end-group -optl-lpthread -optl-lm"
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True
shared: False
executable-static: True

extra-lib-dirs: $MUMPS_LIB_DIR
                $OPENBLAS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package volca
  optimization: $VOLCA_OPT_LEVEL
  ghc-options: $MUSL_LINK_FLAGS
EOF
        ;;

    darwin)
        # macOS arm64: locally-built MUMPS (.a only) + Homebrew openblas + Homebrew gcc gfortran/quadmath.
        # ld64 picks .a from extra-lib-dirs when no .dylib is present, so no GNU -Bstatic/-Bdynamic.
        # Accelerate.framework is rejected: its LAPACK ABI does not match what build-mumps.sh emits.
        #
        # BLAS and the Fortran runtime are linked from their .a by absolute path, never
        # via -l: Homebrew ships both .a and .dylib, ld64 has no -Bstatic to express the
        # preference, and it picks the .dylib. That produced a binary whose LC_LOAD_DYLIB
        # entries point into the build machine's Homebrew prefix, so the shipped tarball
        # aborted at dyld ("Library not loaded: .../libopenblas.0.dylib") on any Mac
        # without those formulas. Linux already links these statically (musl mode); this
        # gives macOS the same standalone binary.
        #
        # OpenBLAS comes from the same source build musl mode uses, not from Homebrew:
        # the bottled libopenblas.a is the OpenMP variant, whose __kmpc_* / omp_*
        # references only the dylib resolved on its own. Building it with USE_OPENMP=0
        # settles that instead of adding libomp — one more Homebrew dependency to keep
        # out of the shipped binary.
        : "${OPENBLAS_LIB_DIR:?OPENBLAS_LIB_DIR is required for darwin mode (path to a libopenblas.a built with USE_OPENMP=0 — see .github/actions/setup-haskell-env)}"
        BREW_PREFIX="$(brew --prefix 2>/dev/null || echo /opt/homebrew)"
        # Homebrew gcc lays out libgfortran/libquadmath under lib/gcc/<major>/
        GFORTRAN_LIB_DIR=$(ls -d "${BREW_PREFIX}/Cellar/gcc/"*/lib/gcc/*/ 2>/dev/null | sort -V | tail -1)
        : "${GFORTRAN_LIB_DIR:?Could not locate Homebrew gcc libgfortran — install with: brew install gcc}"
        GFORTRAN_LIB_DIR="${GFORTRAN_LIB_DIR%/}"
        DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:?MACOSX_DEPLOYMENT_TARGET must be set (source versions.env)}"
        # Ordered dependent-before-dependency: openblas calls into libgfortran, which
        # calls into libquadmath. libgcc.a comes last and is located by asking the
        # compiler driver rather than guessing its Cellar layout — gcc keeps it under
        # lib/gcc/<major>/gcc/<triple>/<major>/, not next to libgfortran.a. It resolves
        # the emutls/soft-arithmetic symbols libgfortran.a leaves undefined.
        DARWIN_STATIC_LIBS=(
            "${OPENBLAS_LIB_DIR}/libopenblas.a"
            "${GFORTRAN_LIB_DIR}/libgfortran.a"
            "${GFORTRAN_LIB_DIR}/libquadmath.a"
        )
        # An unanswered driver would drop libgcc.a from the link and surface as an
        # undefined ___emutls_get_address far from its cause, so an empty answer is
        # an error like a missing archive - the loop below reports it either way.
        GCC_A=$("${BREW_PREFIX}/bin/gfortran" -print-libgcc-file-name 2>/dev/null || true)
        DARWIN_STATIC_LIBS+=("${GCC_A:-<gfortran -print-libgcc-file-name answered nothing>}")
        DARWIN_STATIC_FLAGS=""
        for lib in "${DARWIN_STATIC_LIBS[@]}"; do
            if [[ ! -f "$lib" ]]; then
                echo "ERROR: static library not found: $lib" >&2
                echo "       The shipped binary must not depend on Homebrew dylibs." >&2
                echo "       Fortran runtime: brew install gcc. OpenBLAS: build it with" >&2
                echo "       NO_SHARED=1 USE_OPENMP=0 (see .github/actions/setup-haskell-env)." >&2
                exit 1
            fi
            # OpenBLAS goes in whole, the rest on demand. ld64 pulls members out
            # of an archive in one pass, driven by symbols undefined so far, and
            # OpenBLAS reaches its kernels through tables of function pointers -
            # a reference no symbol resolution can see. The member holding the
            # kernel was therefore never pulled in, the pointer stayed zero, and
            # dgemm_ jumped into a run of zero bytes on the first factorization:
            # EXC_BAD_ACCESS at 0x0, one frame below MUMPS. musl mode has the
            # same need and meets it with -Wl,--start-group; ld64's answer is to
            # force the whole archive.
            case "$lib" in
                *libopenblas.a) DARWIN_STATIC_FLAGS="$DARWIN_STATIC_FLAGS -optl-Wl,-force_load,$lib" ;;
                *)              DARWIN_STATIC_FLAGS="$DARWIN_STATIC_FLAGS -optl$lib" ;;
            esac
        done
        # -dead_strip_dylibs only drops load commands for dylibs nothing needs,
        # which is safe. Plain -dead_strip is not, now that OpenBLAS is linked
        # statically: ld64 splits sections into atoms at symbol boundaries, and
        # a local assembler label like .L2_0 is not a symbol, so hand-written
        # kernel code reached by a jump from a neighbouring atom can be dropped,
        # leaving a hole that faults when execution lands in it. Nothing here
        # proves that has happened; the few kilobytes are not worth the risk,
        # and it could not happen while OpenBLAS arrived as a dylib, whose
        # contents -dead_strip never touched.
        DARWIN_LINK_FLAGS="-optl-L$MUMPS_LIB_DIR -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq$DARWIN_STATIC_FLAGS -optl-lpthread -optl-lm -optl-mmacosx-version-min=${DEPLOYMENT_TARGET} -optl-Wl,-dead_strip_dylibs"
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $MUMPS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package mumps-hs
  extra-lib-dirs: $MUMPS_LIB_DIR
  ghc-options: $DARWIN_LINK_FLAGS

package volca
  optimization: $VOLCA_OPT_LEVEL
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
  optimization: $VOLCA_OPT_LEVEL
  ghc-options: -optl-Wl,--allow-multiple-definition -optl-L$GCC_LIB_DIR -optl-L$MSYS2_LIB_DIR -optl-L$MUMPS_LIB_DIR -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-lopenblas -optl-lgfortran -optl-lgcc -optl-lquadmath -optl-lmingwex -optl-lpthread -optl-lmsvcrt
EOF
        ;;

    *)
        echo "ERROR: Unknown LINK_MODE: $LINK_MODE" >&2
        exit 1
        ;;
esac

echo "Generated $OUTPUT (mode=$LINK_MODE)"
