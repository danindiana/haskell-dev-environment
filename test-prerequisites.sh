#!/bin/bash

echo "==========================================="
echo "Haskell Prerequisites Check"
echo "==========================================="
echo ""

# Check if running as root
if [ "$EUID" -eq 0 ]; then 
   echo "❌ Please don't run as root/sudo"
   exit 1
fi

echo "Checking existing installations..."
echo ""

# Check for existing tools
if command -v ghcup &> /dev/null; then
    echo "✓ ghcup is already installed"
    ghcup --version
else
    echo "○ ghcup not found (will be installed)"
fi

if command -v ghc &> /dev/null; then
    echo "✓ GHC is already installed"
    ghc --version
else
    echo "○ GHC not found (will be installed)"
fi

if command -v cabal &> /dev/null; then
    echo "✓ Cabal is already installed"
    cabal --version | head -1
else
    echo "○ Cabal not found (will be installed)"
fi

if command -v haskell-language-server &> /dev/null; then
    echo "✓ HLS is already installed"
    haskell-language-server --version 2>&1 | head -1
else
    echo "○ HLS not found (will be installed)"
fi

echo ""
echo "Checking system dependencies..."
echo ""

MISSING_DEPS=()

for pkg in build-essential curl libffi-dev libgmp-dev libncurses-dev libtinfo5; do
    if dpkg -s $pkg &> /dev/null; then
        echo "✓ $pkg"
    else
        echo "○ $pkg (needs installation)"
        MISSING_DEPS+=($pkg)
    fi
done

echo ""
echo "==========================================="
if [ ${#MISSING_DEPS[@]} -eq 0 ]; then
    echo "✓ All system dependencies are installed!"
    echo ""
    echo "Ready to run: ./haskell-setup.sh"
else
    echo "Missing dependencies: ${MISSING_DEPS[*]}"
    echo ""
    echo "Install them with:"
    echo "sudo apt update && sudo apt install -y ${MISSING_DEPS[*]}"
fi
echo "==========================================="
