#!/bin/bash
set -e

echo "==========================================="
echo "Haskell Toolchain Setup for Ubuntu 22.04.5"
echo "==========================================="
echo ""

# Color codes for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Step 1: Installing build dependencies...${NC}"
sudo apt update
sudo apt install -y build-essential curl libffi-dev libgmp-dev libncurses-dev libtinfo5 xz-utils

echo ""
echo -e "${GREEN}✓ Build dependencies installed${NC}"
echo ""

echo -e "${YELLOW}Step 2: Installing GHCup...${NC}"
echo "This will download and run the GHCup installer."
echo "Please follow the interactive prompts."
echo ""

# Check if ghcup is already installed
if command -v ghcup &> /dev/null; then
    echo -e "${GREEN}✓ GHCup already installed${NC}"
else
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    
    # Source the environment
    if [ -f "$HOME/.ghcup/env" ]; then
        source "$HOME/.ghcup/env"
    fi
fi

echo ""
echo -e "${YELLOW}Step 3: Installing recommended GHC (9.6.7)...${NC}"
ghcup install ghc recommended
ghcup set ghc 9.6.7

echo ""
echo -e "${YELLOW}Step 4: Installing recommended Cabal...${NC}"
ghcup install cabal recommended
ghcup set cabal recommended

echo ""
echo -e "${YELLOW}Step 5: Installing recommended HLS...${NC}"
ghcup install hls recommended

echo ""
echo -e "${YELLOW}Step 6 (Optional): Installing latest GHC (9.12.2) for experiments...${NC}"
read -p "Install GHC 9.12.2? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    ghcup install ghc 9.12.2
    echo -e "${GREEN}✓ GHC 9.12.2 installed (use 'ghcup set ghc 9.12.2' to switch)${NC}"
fi

echo ""
echo -e "${YELLOW}Step 7 (Optional): Installing Stack...${NC}"
read -p "Install Stack? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    ghcup install stack
    echo -e "${GREEN}✓ Stack installed${NC}"
fi

echo ""
echo "==========================================="
echo -e "${GREEN}Installation Complete!${NC}"
echo "==========================================="
echo ""
echo "Verifying installations..."
echo ""

# Source the environment if not already sourced
if command -v ghc &> /dev/null; then
    echo "GHC version:"
    ghc --version
    echo ""
    echo "Cabal version:"
    cabal --version
    echo ""
    echo "HLS version:"
    haskell-language-server --version 2>/dev/null || echo "HLS installed but not yet in PATH (restart shell)"
    echo ""
    if command -v stack &> /dev/null; then
        echo "Stack version:"
        stack --version
    fi
else
    echo -e "${YELLOW}Please restart your shell or run:${NC}"
    echo "  source ~/.ghcup/env"
    echo ""
    echo "Then verify with:"
    echo "  ghc --version"
    echo "  cabal --version"
    echo "  haskell-language-server --version"
fi

echo ""
echo "==========================================="
echo "Quick Reference Commands:"
echo "==========================================="
echo "List installed versions:    ghcup list"
echo "Switch GHC version:         ghcup set ghc <version>"
echo "Install new GHC:            ghcup install ghc <version>"
echo "Update GHCup:               ghcup upgrade"
echo ""
echo "For multi-GHC project setup, see haskell-project-template/"
