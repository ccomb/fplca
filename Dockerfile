# Stage 1: Build PETSc and SLEPc from source
# Configuration is centralized in config/petsc.env and versions.env
FROM debian:bookworm-slim AS petsc-builder

RUN apt-get update && apt-get install -y \
    build-essential \
    gfortran \
    python3 \
    wget \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt

# Copy configuration files
COPY versions.env /tmp/
COPY petsc.env /tmp/

# Extract versions from env files using shell
# Download and build PETSc (version from versions.env)
RUN . /tmp/versions.env && \
    wget -q https://web.cels.anl.gov/projects/petsc/download/release-snapshots/petsc-${PETSC_VERSION}.tar.gz && \
    tar xzf petsc-${PETSC_VERSION}.tar.gz && \
    rm petsc-${PETSC_VERSION}.tar.gz && \
    mv petsc-${PETSC_VERSION} petsc

WORKDIR /opt/petsc
ENV PETSC_DIR=/opt/petsc
ENV PETSC_ARCH=arch-linux-c-opt

# Configure PETSc using options from petsc.env
# Docker-specific: --download-cmake, --download-fblaslapack, --download-hdf5
RUN . /tmp/versions.env && . /tmp/petsc.env && \
    python3 ./configure \
    --with-cxx=0 \
    --download-cmake \
    --download-fblaslapack \
    --download-mpich \
    --download-mumps \
    --download-scalapack \
    --with-debugging=no \
    COPTFLAGS=-O3 \
    CXXOPTFLAGS=-O3 \
    FOPTFLAGS=-O3 \
    && make -j all

# Download and build SLEPc (version from versions.env)
WORKDIR /opt
RUN . /tmp/versions.env && \
    wget -q https://slepc.upv.es/download/distrib/slepc-${SLEPC_VERSION}.tar.gz && \
    tar xzf slepc-${SLEPC_VERSION}.tar.gz && \
    rm slepc-${SLEPC_VERSION}.tar.gz && \
    mv slepc-${SLEPC_VERSION} slepc

WORKDIR /opt/slepc
ENV SLEPC_DIR=/opt/slepc

RUN python3 ./configure && make -j

# Stage 2: Build Haskell application
# Build from fplca directory: docker build -t fplca .
FROM debian:bookworm AS haskell-builder

# Install system dependencies (no ghc/cabal - we use ghcup)
RUN apt-get update && apt-get install -y \
    curl \
    libgmp-dev \
    zlib1g-dev \
    libzstd-dev \
    pkg-config \
    nodejs \
    npm \
    git \
    build-essential \
    gfortran \
    libnuma-dev \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Copy versions.env for GHC version
COPY versions.env /tmp/

# Install ghcup and GHC (version from versions.env)
ENV GHCUP_INSTALL_BASE_PREFIX=/opt
ENV PATH="/opt/.ghcup/bin:$PATH"
RUN . /tmp/versions.env && \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC_VERSION} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=latest \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    sh

# Install c2hs via cabal
RUN cabal update && cabal install c2hs --install-method=copy --installdir=/usr/local/bin

# Copy PETSc/SLEPc from builder
COPY --from=petsc-builder /opt/petsc /opt/petsc
COPY --from=petsc-builder /opt/slepc /opt/slepc

ENV PETSC_DIR=/opt/petsc
ENV PETSC_ARCH=arch-linux-c-opt
ENV SLEPC_DIR=/opt/slepc
ENV LD_LIBRARY_PATH=/opt/petsc/${PETSC_ARCH}/lib:/opt/slepc/${PETSC_ARCH}/lib

WORKDIR /build

# Clone petsc-hs from GitHub (same as build.sh does)
# ADD fetches the branch ref from GitHub API - cache is invalidated when commit SHA changes
ADD https://api.github.com/repos/ccomb/petsc-hs/git/refs/heads/master /tmp/petsc-hs-version
RUN git clone https://github.com/ccomb/petsc-hs.git /build/petsc-hs

# Generate and process c2hs files for petsc-hs
# 1. Generate TypesC2HsGen.chs from GenerateC2Hs.hs (outputs to stdout)
# 2. Run c2hs to generate TypesC2HsGen.hs from the .chs file
RUN cd /build/petsc-hs/src/Numerical/PETSc/Internal/C2HsGen && \
    runhaskell GenerateC2Hs.hs > TypesC2HsGen.chs && \
    c2hs -C -I/opt/petsc/include \
         -C -I/opt/petsc/${PETSC_ARCH}/include \
         -C -I/opt/slepc/include \
         -C -I/opt/slepc/${PETSC_ARCH}/include \
         TypesC2HsGen.chs

# Copy ONLY dependency specification files first (for layer caching)
COPY fplca.cabal /build/fplca/

# Set up cabal.project with petsc-hs as local package
RUN echo "packages: ./fplca ./petsc-hs" > /build/cabal.project \
    && echo "allow-newer: true" >> /build/cabal.project \
    && echo "optimization: 2" > /build/cabal.project.local \
    && echo "" >> /build/cabal.project.local \
    && echo "extra-lib-dirs: /opt/petsc/${PETSC_ARCH}/lib" >> /build/cabal.project.local \
    && echo "            , /opt/slepc/${PETSC_ARCH}/lib" >> /build/cabal.project.local \
    && echo "extra-include-dirs: /opt/petsc/include" >> /build/cabal.project.local \
    && echo "                  , /opt/petsc/${PETSC_ARCH}/include" >> /build/cabal.project.local \
    && echo "                  , /opt/slepc/include" >> /build/cabal.project.local \
    && echo "                  , /opt/slepc/${PETSC_ARCH}/include" >> /build/cabal.project.local \
    && echo "" >> /build/cabal.project.local \
    && echo "-- MPI library (MPICH downloaded by PETSc)" >> /build/cabal.project.local \
    && echo "package petsc-hs" >> /build/cabal.project.local \
    && echo "  ghc-options: -optl-L/opt/petsc/${PETSC_ARCH}/lib -optl-lmpi" >> /build/cabal.project.local \
    && echo "" >> /build/cabal.project.local \
    && echo "package fplca" >> /build/cabal.project.local \
    && echo "  ghc-options: -optl-L/opt/petsc/${PETSC_ARCH}/lib -optl-lmpi" >> /build/cabal.project.local

# Update cabal and build ONLY dependencies (cached layer)
RUN cabal update \
    && cd /build && cabal build --only-dependencies fplca

# NOW copy the rest of the source
COPY . /build/fplca

# Build fplca (only recompiles app code, deps already cached)
RUN cd /build && cabal build fplca

# Build Elm frontend (uses local npm packages)
RUN cd /build/fplca/web && npm install && ./build.sh

# Find and copy the executable
RUN mkdir -p /build/output \
    && cp $(cd /build && cabal list-bin exe:fplca) /build/output/fplca

# Stage 3: Runtime image
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    libgfortran5 \
    libgomp1 \
    libgmp10 \
    zlib1g \
    libzstd1 \
    ca-certificates \
    locales \
    7zip \
    && rm -rf /var/lib/apt/lists/* \
    && sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen \
    && locale-gen

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Copy PETSc/SLEPc runtime libraries
COPY --from=petsc-builder /opt/petsc/arch-linux-c-opt/lib /opt/petsc/lib
COPY --from=petsc-builder /opt/slepc/arch-linux-c-opt/lib /opt/slepc/lib

# Copy application
COPY --from=haskell-builder /build/output/fplca /usr/local/bin/fplca
COPY --from=haskell-builder /build/fplca/web/dist /app/web/dist

ENV LD_LIBRARY_PATH=/opt/petsc/lib:/opt/slepc/lib

# PETSC_OPTIONS from config/petsc.env (MUMPS direct solver settings)
# Note: -malloc_hbw false is Docker-specific (no high-bandwidth memory)
ENV PETSC_OPTIONS="-pc_type lu -pc_factor_mat_solver_type mumps -mat_mumps_icntl_14 80 -mat_mumps_icntl_24 1 -malloc_hbw false"

WORKDIR /app

# User data directory (uploads, cache) - mount as volume for persistence
ENV FPLCA_DATA_DIR=/data
VOLUME /data

# Copy runtime config (edit fplca.docker.toml to add/remove databases)
COPY fplca.docker.toml /app/fplca.toml

EXPOSE 8080

# Copy all pre-generated cache files LAST for optimal layer caching
# When only cache files change, only this layer needs rebuilding
# Generate locally with: fplca --data /path/to/db
COPY cache/fplca.cache.*.bin.zst /app/

CMD ["fplca", "--config", "/app/fplca.toml", "server"]
