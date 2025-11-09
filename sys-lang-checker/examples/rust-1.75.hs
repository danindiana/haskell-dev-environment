-- Example Rust Configuration
-- This demonstrates configuration for Rust toolchain

import Types
import Validator

rustValidConfig :: LanguageConfig
rustValidConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Rust"
      , langVersion = Version 1 75 0
      , minGHCVersion = Nothing
      , maxGHCVersion = Nothing
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = Rustc
      , compilerVersion = Version 1 75 0
      , dependencies =
          [ Dependency "cargo" (Version 1 75 0) Tool True
          , Dependency "rustlib" (Version 1 75 0) Library True
          , Dependency "libssl-dev" (Version 1 1 0) Library True
          ]
      }
  , cfgConstraints = []
  }

-- Invalid Rust config (for testing)
rustInvalidConfig :: LanguageConfig
rustInvalidConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Rust"
      , langVersion = Version 1 75 0
      , minGHCVersion = Just (Version 2 0 0)  -- Invalid for Rust!
      , maxGHCVersion = Just (Version 1 0 0)  -- min > max
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = Rustc
      , compilerVersion = Version 1 75 0
      , dependencies = []
      }
  , cfgConstraints = []
  }
