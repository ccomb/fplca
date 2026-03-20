#!/bin/bash
# Build the volca Docker image
# Run from the volca directory

set -e

cd "$(dirname "$0")"

GIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
if ! git diff --quiet HEAD 2>/dev/null; then
    GIT_HASH="${GIT_HASH}-dirty"
fi
GIT_TAG=$(git describe --tags --exact-match HEAD 2>/dev/null || echo "")

echo "Building Docker image: hash=$GIT_HASH tag=${GIT_TAG:-none}"

docker build \
    --build-arg GIT_HASH="$GIT_HASH" \
    --build-arg GIT_TAG="$GIT_TAG" \
    -t volca .
