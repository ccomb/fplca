#!/bin/bash
# Compute GHC RTS flags based on available hardware.
# Usage: eval $(./rts-flags.sh)   → sets RTS_FLAGS
# Or:    source rts-flags.sh      → sets RTS_FLAGS

CORES=$(nproc 2>/dev/null || echo 4)
RAM_KB=$(grep MemTotal /proc/meminfo 2>/dev/null | awk '{print $2}')
RAM_MB=$((RAM_KB / 1024))

# Heap: ~12% of RAM, capped at 4G, minimum 512M
HEAP_MB=$((RAM_MB / 8))
[ $HEAP_MB -gt 4096 ] && HEAP_MB=4096
[ $HEAP_MB -lt 512 ] && HEAP_MB=512

# Nursery: ~16MB per core, minimum 64M
NURSERY_MB=$((CORES * 16))
[ $NURSERY_MB -lt 64 ] && NURSERY_MB=64

# Chunk size: nursery / 32, minimum 8m
CHUNK_MB=$((NURSERY_MB / 32))
[ $CHUNK_MB -lt 8 ] && CHUNK_MB=8

# Max heap: 75% of RAM, capped at 24G, minimum 2G
# Prevents OOM-killing the machine on large cold parses
MAX_MB=$((RAM_MB * 3 / 4))
[ $MAX_MB -gt 24576 ] && MAX_MB=24576
[ $MAX_MB -lt 2048 ] && MAX_MB=2048

RTS_FLAGS="+RTS -N -M${MAX_MB}M -H${HEAP_MB}M -A${NURSERY_MB}M -n${CHUNK_MB}m -qg0 -c -I30 -RTS"

echo "RTS_FLAGS=\"$RTS_FLAGS\""

# If sourced, also print to stderr for visibility
if [[ "${BASH_SOURCE[0]}" != "$0" ]]; then
    echo "RTS: ${CORES} cores, ${RAM_MB}MB RAM → ${RTS_FLAGS}" >&2
fi
