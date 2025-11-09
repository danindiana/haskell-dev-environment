#!/bin/bash

# Python Compiler Setup for Haskell Tutorial 2
# Compiles Python 3.13 from source and creates a venv

set -e

PYTHON_VERSION="${PYTHON_VERSION:-3.13.0}"
PYTHON_MAJOR_MINOR="${PYTHON_VERSION%.*}"
INSTALL_PREFIX="$HOME/.local/python-versions/$PYTHON_VERSION"
VENV_DIR="/home/jeb/programs/haskell_install/tutorial2-venv"

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${CYAN}"
cat << "EOF"
   ╔═══════════════════════════════════════════════════════════╗
   ║  Python Compiler Setup for Haskell Tutorial 2             ║
   ║  Building Python ${PYTHON_VERSION} from source              ║
   ╚═══════════════════════════════════════════════════════════╝
EOF
echo -e "${NC}"

# Check if already installed
if [ -x "$INSTALL_PREFIX/bin/python3" ]; then
    echo -e "${GREEN}✓ Python ${PYTHON_VERSION} already compiled at ${INSTALL_PREFIX}${NC}"
else
    echo -e "${YELLOW}Installing build dependencies...${NC}"
    sudo apt-get update
    sudo apt-get install -y build-essential libssl-dev libffi-dev libsqlite3-dev \
        zlib1g-dev libbz2-dev libreadline-dev libncursesw5-dev xz-utils tk-dev \
        liblzma-dev curl

    TMPDIR=$(mktemp -d)
    trap 'rm -rf "$TMPDIR"' EXIT

    cd "$TMPDIR"
    echo -e "${YELLOW}Downloading Python ${PYTHON_VERSION}...${NC}"
    curl -sS "https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tar.xz" \
        -o "Python-${PYTHON_VERSION}.tar.xz"

    echo -e "${YELLOW}Extracting...${NC}"
    tar -xf "Python-${PYTHON_VERSION}.tar.xz"
    cd "Python-${PYTHON_VERSION}"

    echo -e "${YELLOW}Configuring (this may take a minute)...${NC}"
    ./configure --prefix="$INSTALL_PREFIX" --enable-optimizations --with-lto

    echo -e "${YELLOW}Compiling (this may take 5-10 minutes on your system)...${NC}"
    make -j "$(nproc)"

    echo -e "${YELLOW}Installing...${NC}"
    make install

    echo -e "${GREEN}✓ Python ${PYTHON_VERSION} compiled successfully!${NC}"
fi

# Create venv
if [ -d "$VENV_DIR" ]; then
    echo -e "${GREEN}✓ Venv already exists at ${VENV_DIR}${NC}"
else
    echo -e "${YELLOW}Creating Python venv at ${VENV_DIR}...${NC}"
    "$INSTALL_PREFIX/bin/python3" -m venv "$VENV_DIR"
    echo -e "${GREEN}✓ Venv created${NC}"
fi

# Upgrade pip and install packages
echo -e "${YELLOW}Upgrading pip and installing packages...${NC}"
"$VENV_DIR/bin/pip" install --upgrade pip setuptools wheel

if [ -f "/home/jeb/programs/haskell_install/py/requirements.txt" ]; then
    echo -e "${YELLOW}Installing requirements from requirements.txt...${NC}"
    "$VENV_DIR/bin/pip" install -r "/home/jeb/programs/haskell_install/py/requirements.txt"
else
    echo -e "${YELLOW}No requirements.txt found, installing common packages...${NC}"
    "$VENV_DIR/bin/pip" install requests aiohttp pandas
fi

echo ""
echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}✓ Setup Complete!${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${CYAN}To activate the venv:${NC}"
echo "  source $VENV_DIR/bin/activate"
echo ""
echo -e "${CYAN}Python is installed at:${NC}"
echo "  $INSTALL_PREFIX/bin/python3"
echo ""
echo -e "${CYAN}Venv is at:${NC}"
echo "  $VENV_DIR"
echo ""
