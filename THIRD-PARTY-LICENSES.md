# Third-party licences

VoLCA itself is Apache-2.0 (see `LICENSE`). The distributed binary carries the
numerical libraries below. Component versions are pinned in `versions.env`.

## Linked into the binary on every platform

**MUMPS** (sparse direct solver) - CeCILL-C.
Upstream: <https://mumps-solver.org>. Licence text: <https://cecill.info>.
Built from unmodified upstream sources by `build-mumps.sh`; the build recipe
lives in that script.

**OpenBLAS** (BLAS/LAPACK) - BSD 3-Clause. Upstream:
<https://github.com/OpenMathLib/OpenBLAS>. Full text below.

**GCC runtime** (libgfortran, libquadmath, libgcc) - GPLv3 with the GCC Runtime
Library Exception. Linking this runtime into a binary compiled by GCC is exactly
what the Exception permits, so the binary imposes no further obligation.
Texts: <https://gcc.gnu.org/onlinedocs/gcc/Copying.html> and
<https://www.gnu.org/licenses/gcc-exception-3.1.html>.

## Shipped as separate files, Windows only

The Windows zip carries the MSYS2 ucrt64 runtime DLLs the binary loads
(`libgfortran-5`, `libquadmath-0`, `libgcc_s_seh-1`, `libstdc++-6`, `libgomp-1`,
`libwinpthread-1`, `libopenblas`). Their complete licence texts, as installed by
MSYS2, travel in the `licenses/` directory of that zip.

## OpenBLAS licence

Copyright (c) 2011-2014, The OpenBLAS Project
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in
   the documentation and/or other materials provided with the
   distribution.

3. Neither the name of the OpenBLAS project nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
