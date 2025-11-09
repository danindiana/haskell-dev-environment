# My Haskell Project

A sample Haskell project configured for multi-GHC support (9.6.7 + 9.12.2).

## Building

```bash
# Build the project
cabal build

# Run the executable
cabal run my-project

# Run tests
cabal test

# Start a REPL
cabal repl
```

## Switching GHC Versions

```bash
# Use recommended GHC 9.6.7
ghcup set ghc 9.6.7
cabal clean
cabal build

# Try latest GHC 9.12.2
ghcup set ghc 9.12.2
cabal clean
cabal build
```

## Development with HLS

Your editor (VS Code, Emacs, Vim, etc.) should automatically use `haskell-language-server` for:
- Type checking
- Code completion
- Inline documentation
- Refactoring tools

Make sure HLS is installed for your active GHC version:
```bash
ghcup install hls recommended
```

## Project Structure

```
my-project/
├── app/              # Executable source
│   └── Main.hs
├── src/              # Library source
│   └── MyLib.hs
├── test/             # Test suite
│   └── Main.hs
├── cabal.project     # Project-wide configuration
├── my-project.cabal  # Package definition
└── README.md
```
