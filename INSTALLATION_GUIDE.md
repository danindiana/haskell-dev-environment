# Haskell Toolchain Installation Guide
## Optimized for Ubuntu 22.04.5 (AMD Ryzen 9 5950X)

### TL;DR

```bash
# Make the script executable and run it
chmod +x haskell-setup.sh
./haskell-setup.sh
```

After installation, **restart your shell** or run:
```bash
source ~/.ghcup/env
```

---

## What Gets Installed

### Recommended Toolchain (Production-Ready)

- **GHC 9.6.7** – Recommended compiler with best ecosystem support
- **Cabal 3.12.x+** – Primary build tool
- **HLS 2.10.0.0** – Haskell Language Server for IDE integration

### Optional Components

- **GHC 9.12.2** – Latest compiler for experiments
- **Stack 3.x** – Alternative build tool (optional)

---

## Manual Installation Steps

If you prefer to run commands manually:

### 1. Install System Dependencies

```bash
sudo apt update
sudo apt install -y build-essential curl libffi-dev libgmp-dev \
                    libncurses-dev libtinfo5 xz-utils
```

### 2. Install GHCup

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Follow the interactive prompts. When asked:
- **Prepend PATH?** Yes (recommended)
- **Install HLS?** Yes
- **Install stack?** Your choice (optional)

### 3. Activate GHCup Environment

Restart your terminal or run:
```bash
source ~/.ghcup/env
```

### 4. Install Recommended GHC & Tools

```bash
# Install and activate GHC 9.6.7
ghcup install ghc recommended
ghcup set ghc 9.6.7

# Install Cabal
ghcup install cabal recommended
ghcup set cabal recommended

# Install HLS
ghcup install hls recommended
```

### 5. (Optional) Install Latest GHC

```bash
ghcup install ghc 9.12.2

# Switch between versions:
# ghcup set ghc 9.12.2  # use latest
# ghcup set ghc 9.6.7   # back to recommended
```

### 6. (Optional) Install Stack

```bash
ghcup install stack
```

---

## Verification

```bash
ghc --version          # Should show 9.6.7
cabal --version        # Should show 3.12.x+
haskell-language-server --version
ghcup list            # Shows all installed tools
```

---

## Working with Multiple GHC Versions

### Switching GHC Versions

```bash
ghcup list             # See installed versions
ghcup set ghc 9.12.2   # Switch to latest
ghcup set ghc 9.6.7    # Switch back to recommended
```

### Per-Project GHC Selection

Create a `.ghcup.yaml` in your project root:

```yaml
ghc: 9.6.7
```

Or use `cabal.project`:

```cabal
with-compiler: ghc-9.6.7
```

---

## Integration with Nix

Since you already have Nix installed:

**Recommended approach:** Keep `ghcup`-managed Haskell separate from Nix.

- Use **ghcup** for system-wide/user-level Haskell development
- Use **Nix** only for projects explicitly using `flake.nix` or `shell.nix`

This avoids PATH conflicts. When working on a Nix Haskell project:
```bash
nix develop  # or nix-shell
# GHC from Nix takes precedence here
```

---

## Editor Integration

### VS Code

Install extension: `haskell.haskell`

```bash
code --install-extension haskell.haskell
```

### Vim/Neovim

Use CoC or LSP client with `haskell-language-server`.

### Emacs

Use `lsp-mode` or `eglot` with HLS.

HLS will automatically use the GHC version set by `ghcup set ghc <version>`.

---

## Quick Reference Commands

```bash
# List tools
ghcup list

# Install specific version
ghcup install ghc 9.8.4

# Set default version
ghcup set ghc 9.6.7

# Update GHCup itself
ghcup upgrade

# Remove a version
ghcup rm ghc 9.12.2

# Show GHCup configuration
ghcup config
```

---

## Project Template

A ready-to-use project template is available in `haskell-project-template/`:

```bash
cp -r haskell-project-template ~/my-new-project
cd ~/my-new-project
cabal build
cabal run my-project
```

The template includes:
- Multi-GHC support (9.6.7 + 9.12.2)
- Modern `cabal.project` with optimizations
- Library + executable + test suite structure
- HLS-friendly configuration

---

## Performance Notes for Your System

**Your AMD Ryzen 9 5950X (16c/32t) Setup:**

The `cabal.project` in the template uses:
```cabal
jobs: $ncpus
```

This will auto-detect all 32 threads for parallel builds. If you want to limit resource usage during compilation:

```cabal
jobs: 16  -- or any number you prefer
```

Expected compile times for HLS first build:
- ~10-15 minutes on your hardware (it's a large package)
- Subsequent builds are much faster thanks to caching

---

## Troubleshooting

### "ghc: command not found" after installation

```bash
source ~/.ghcup/env
# or restart your terminal
```

### HLS doesn't work with a specific GHC version

```bash
ghcup install hls --set  # Install HLS for current GHC
```

### Cabal can't find packages

```bash
cabal update  # Update package index
```

### Permission errors

Never run `ghcup` or `cabal` with `sudo`. Everything installs to `~/.ghcup` and `~/.cabal`.

---

## Next Steps

1. Try the project template:
   ```bash
   cd ~/haskell-project-template
   cabal build
   cabal run my-project
   ```

2. Explore Haskell resources:
   - [Haskell Documentation](https://www.haskell.org/documentation/)
   - [Learn You a Haskell](http://learnyouahaskell.com/)
   - [Haskell Wiki](https://wiki.haskell.org/)

3. Configure your editor for HLS integration

4. Start building! 🚀
