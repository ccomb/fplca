#!/bin/bash

set -e

echo "Building Elm frontend with asset hashing..."

# Create dist directory
mkdir -p dist

# Build Elm to temporary file (use local elm from node_modules)
echo "Compiling Elm..."
npx elm make src/Main.elm --output=dist/main.tmp.js --optimize

# Minify JavaScript with SWC (Elm Guide config for optimal compression)
echo "Minifying with SWC..."
BEFORE_SIZE=$(wc -c < dist/main.tmp.js)
node minify.mjs dist/main.tmp.js dist/main.min.js
mv dist/main.min.js dist/main.tmp.js
AFTER_SIZE=$(wc -c < dist/main.tmp.js)
echo "  $BEFORE_SIZE -> $AFTER_SIZE bytes ($(( (BEFORE_SIZE - AFTER_SIZE) * 100 / BEFORE_SIZE ))% reduction)"

# Calculate MD5 hash of the compiled JS
JS_HASH=$(md5sum dist/main.tmp.js | cut -d' ' -f1)
JS_FILE="$JS_HASH.js"

# Rename the temporary file to hashed name
mv dist/main.tmp.js "dist/$JS_FILE"

# Generate index.html from template
echo "Generating index.html..."
sed "s/{{JS_FILE}}/$JS_FILE/g" index.html.tmpl > dist/index.html

echo "Frontend build complete!"
echo "Generated: dist/$JS_FILE"
echo "Generated: dist/index.html"