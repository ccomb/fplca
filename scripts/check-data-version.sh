#!/bin/sh
# Refuse a data/ that changed since the last engine release under the same
# data/VERSION, and a new data/VERSION with no change behind it.
#
# The installers keep one data/<version>/ directory per bundle and skip the
# download when it already exists, so a bundle whose contents changed under
# an unchanged number is one they never fetch. The number only has to be
# honest against the last release: two pull requests both moving it from 2
# to 3 are both right.
#
# Usage: scripts/check-data-version.sh [TAG]
#   TAG  the release being cut, so the comparison base is the release before
#        it (release.yml passes the tag being built). Without it, the base is
#        the newest v* tag reachable from the parent of HEAD.
set -eu

base=$(git describe --tags --abbrev=0 --match 'v[0-9]*' "${1:-HEAD}^")
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
