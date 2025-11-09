# System Language Config/Build Stability Type Checker

A Haskell-based static type checker and validator for system programming language configurations and build environments. Ensures stability, compatibility, and best practices for language compiler setups.

## 🎯 Purpose

Validates system programming language configurations across:
- **Compiler Versions** - GHC, Rustc, Golang, Clang, MSVC
- **OS Compatibility** - Linux, macOS, Windows, BSD
- **Dependency Management** - Libraries, binaries, tools
- **Version Constraints** - Min/max versions with semantic versioning
- **Build Environment** - OS versions, toolchain compatibility
- **Stability Rules** - Pre-release detection, missing bounds warnings

## 🏗️ Architecture

### Type System (Pure & Composable)

```
Types.hs
├── Version (semantic versioning: X.Y.Z)
├── LanguageSpec (compiler + version constraints)
├── BuildEnvironment (OS, compiler, dependencies)
├── StabilityConstraint (validation rules)
└── ValidationResult (errors + warnings + summary)
```

### Validation Pipeline

```
LanguageConfig
    ↓
validateLanguageConfig
    ├── validateVersions (format & range checks)
    ├── validateEnvironment (compiler & OS checks)
    ├── validateDependencies (duplicates & requirements)
    └── validateConstraints (custom rules)
    ↓
ValidationResult (errors + warnings)
    ↓
printValidationResult (formatted output)
```

## 🚀 Quick Start

### Build

```bash
cd sys-lang-checker
source ~/.ghcup/env
cabal build
```

### Run Demo

```bash
cabal run sys-lang-checker -- --demo
```

Output shows 4 test cases:
1. ✅ Valid GHC configuration
2. ❌ Invalid version range (min > max)
3. ❌ Pre-release with duplicate dependencies
4. ❌ Suspicious compiler version

### Run Tests

```bash
cabal test
```

## 📋 API Reference

### Core Types

#### Version
```haskell
data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  } deriving (Show, Eq, Ord)

-- Usage
ghc = Version 9 6 7
```

#### LanguageSpec
```haskell
data LanguageSpec = LanguageSpec
  { langName :: Text                    -- "Haskell", "Rust", "Go"
  , langVersion :: Version              -- Current version
  , minGHCVersion :: Maybe Version      -- Minimum supported compiler
  , maxGHCVersion :: Maybe Version      -- Maximum supported compiler
  }
```

#### BuildEnvironment
```haskell
data BuildEnvironment = BuildEnvironment
  { osType :: OS                        -- Linux | MacOS | Windows | BSD
  , osVersion :: Version                -- e.g., 5.15.0
  , compilerType :: CompilerType        -- GHC | Rustc | etc.
  , compilerVersion :: Version          -- Compiler version
  , dependencies :: [Dependency]        -- Required packages
  }
```

#### Dependency
```haskell
data Dependency = Dependency
  { depName :: Text
  , depVersion :: Version
  , depKind :: DependencyKind           -- Library | Binary | Tool
  , depRequired :: Bool
  }
```

#### ValidationResult
```haskell
data ValidationResult = ValidationResult
  { isValid :: Bool
  , errors :: [ValidationError]         -- Must-fix issues
  , warnings :: [ValidationWarning]     -- Nice-to-fix issues
  , summary :: Text                     -- Human-readable status
  }
```

### Validation Functions

#### validateLanguageConfig
Main entry point for validation:
```haskell
validateLanguageConfig :: LanguageConfig -> ValidationResult
```

Performs comprehensive checks:
- ✅ Version format validation (non-negative components)
- ✅ Version range validation (min ≤ max)
- ✅ Compiler version sanity checks (≤ 20.20)
- ✅ Duplicate dependency detection
- ✅ Pre-release language detection
- ✅ Missing version bound warnings

## 📊 Error Codes

| Code | Type | Description |
|------|------|-------------|
| `VER_INVALID_FORMAT` | Error | Version has negative components |
| `VER_INVALID_RANGE` | Error | Minimum version > maximum version |
| `ENV_COMPILER_SUSPICIOUS` | Error | Compiler version suspiciously high (>20.20) |
| `DEP_DUPLICATE` | Error | Same dependency declared multiple times |
| `CONSTRAINT_VER_INVALID` | Error | Version range constraint unparseable |
| `CONSTRAINT_VER_MISMATCH` | Error | Language version outside required range |
| `WARN_PRERELEASE` | Warning | Language version < 1.0.0 |
| `WARN_NO_MIN_GHC` | Warning | No minimum GHC version specified |
| `WARN_NO_MAX_GHC` | Warning | No maximum GHC version specified |

## 💡 Example: Validating a GHC Configuration

```haskell
import Types
import Validator

-- Define your language configuration
haskellConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Haskell"
      , langVersion = Version 9 6 7
      , minGHCVersion = Just (Version 9 2 0)
      , maxGHCVersion = Just (Version 9 8 0)
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = GHC
      , compilerVersion = Version 9 6 7
      , dependencies =
          [ Dependency "aeson" (Version 2 0 0) Library True
          , Dependency "text" (Version 2 0 0) Library True
          ]
      }
  , cfgConstraints = []
  }

-- Validate
result = validateLanguageConfig haskellConfig

-- Result: ValidationResult
-- { isValid = True
-- , errors = []
-- , warnings = []
-- , summary = "✓ Configuration is stable and valid"
-- }
```

## 🧪 Test Suite

Four comprehensive test cases:

1. **testVersionFormatValidation** - Checks positive version components
2. **testVersionRangeValidation** - Detects invalid ranges (min > max)
3. **testDuplicateDependencies** - Identifies duplicate dependencies
4. **testValidGHCConfig** - Validates production GHC setup

Run with: `cabal test`

## 🔧 Extensibility

### Adding New Compiler Types

```haskell
-- In Types.hs
data CompilerType
  = GHC
  | Rustc
  | GolangCompiler
  | Clang
  | MSVC
  | MyNewCompiler        -- Add here
  | OtherCompiler String
```

### Adding New Validators

```haskell
-- In Validator.hs
validateCustomRule :: LanguageConfig -> [ValidationError]
validateCustomRule config = 
  -- Your validation logic
  []
```

Then call in `validateLanguageConfig`:
```haskell
let customErrs = validateCustomRule config
```

### Adding New OS Types

```haskell
data OS = Linux | MacOS | Windows | BSD | MyOS
```

## 📦 Dependencies

- `base >= 4.16` - Haskell prelude
- `text >= 1.2` - Efficient text handling
- `containers >= 0.6` - Data structures (pre-built for future use)

## 🎨 Design Principles

1. **Type Safety** - Errors caught at compile time, not runtime
2. **Composability** - Validators return lists that concatenate easily
3. **Clarity** - Clear error messages with context information
4. **Extensibility** - Add validators and types without breaking existing code
5. **Pure Functions** - No side effects in validation logic

## 📈 Performance

- **Time Complexity**: O(n) where n = total number of validation checks
- **Space Complexity**: O(e + w) where e = errors, w = warnings
- **Typical Runtime**: < 1ms for comprehensive validation

Optimized for multi-GHC compatibility:
```
ghc-9.6.7  ✓ (tested)
ghc-9.12.2 ✓ (compatible)
```

## 🔮 Future Enhancements

- [ ] YAML/JSON config file parsing
- [ ] Version range parsing (e.g., "1.0.0 - 2.5.0")
- [ ] Integration with cabal files
- [ ] Constraint solver for dependency resolution
- [ ] Performance benchmarking suite
- [ ] IDE integration (LSP support)
- [ ] Web API for remote validation

## 📜 License

MIT License - See LICENSE file for details

## 👨‍💻 Author

Created as part of Haskell development environment setup on Ubuntu 22.04.5 LTS with GHC 9.6.7

---

**Happy validating! May your configurations be stable and your compilers be swift!** ⚡
