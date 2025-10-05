#!/bin/bash

set -e

echo "Building Elm frontend with asset hashing..."

# Check if Elm is installed
if ! command -v elm &> /dev/null; then
    echo "Elm not found. Installing Elm..."
    if command -v npm &> /dev/null; then
        npm install -g elm
    else
        echo "Error: npm is required to install Elm. Please install Node.js and npm first."
        echo "On Ubuntu/Debian: sudo apt install nodejs npm"
        echo "On Arch Linux: sudo pacman -S nodejs npm"
        exit 1
    fi
fi

# Create dist directory
mkdir -p dist

# Build Elm to temporary file
echo "Compiling Elm..."
elm make src/Main.elm --output=dist/main.tmp.js --optimize

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