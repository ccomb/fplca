#!/bin/sh
# Refuse a data/ that changed since the last engine release under the same
# data/VERSION, and a new data/VERSION with no change behind it.
#
# The number is the bundle's identity: it names the release asset
# (volca-data-<version>.tar.gz), the data/<version>/ directory the installers
# extract into, and the dataVersion an engine reports on /api/v1/version. Two
# releases shipping different contents under one number make all three lie:
# two engines reading different data answer the same dataVersion. The number
# only has to be honest against the last release, so two pull requests both
# moving it from 2 to 3 are both right.
#
# Compares HEAD with the newest v* tag reachable from its parent; run on the
# commit being tagged, that is the release before the one being cut.
set -eu

base=$(git describe --tags --abbrev=0 --match 'v[0-9]*' HEAD^)
old=$(git show "$base:data/VERSION")
new=$(cat data/VERSION)

case "$new" in
    '' | *[!0-9]*)
        echo "data/VERSION must be an integer, got '$new'"
        exit 1
        ;;
esac

if git diff --quiet "$base" HEAD -- data ':!data/VERSION'; then
    if [ "$old" != "$new" ]; then
        echo "data/VERSION moved from $old to $new but data/ is identical to $base: keep $old"
        exit 1
    fi
elif [ "$old" = "$new" ]; then
    echo "data/ changed since $base but data/VERSION is still $old: bump it"
    exit 1
elif [ "$new" -le "$old" ]; then
    echo "data/VERSION must go up from $old, got $new"
    exit 1
fi
echo "data/VERSION $new agrees with data/ since $base"
