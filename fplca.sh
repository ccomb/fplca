#!/bin/bash

# Run the fplca executable with all arguments forwarded

set -e

BINARY=$(cabal list-bin -O2 fplca 2>/dev/null || echo "")
if [[ -z "$BINARY" ]] || [[ ! -x "$BINARY" ]]; then
    echo "ERROR: Could not find fplca executable"
    exit 1
fi

"$BINARY" "$@"
