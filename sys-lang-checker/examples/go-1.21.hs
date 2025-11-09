-- Example Go Configuration
-- This demonstrates configuration for Go toolchain

import Types
import Validator

goValidConfig :: LanguageConfig
goValidConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Go"
      , langVersion = Version 1 21 0
      , minGHCVersion = Nothing
      , maxGHCVersion = Nothing
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = GolangCompiler
      , compilerVersion = Version 1 21 0
      , dependencies =
          [ Dependency "go" (Version 1 21 0) Binary True
          , Dependency "git" (Version 2 40 0) Tool True
          ]
      }
  , cfgConstraints = []
  }

-- macOS variant
goMacOSConfig :: LanguageConfig
goMacOSConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Go"
      , langVersion = Version 1 21 0
      , minGHCVersion = Nothing
      , maxGHCVersion = Nothing
      }
  , cfgEnvironment = BuildEnvironment
      { osType = MacOS
      , osVersion = Version 14 0 0
      , compilerType = GolangCompiler
      , compilerVersion = Version 1 21 0
      , dependencies =
          [ Dependency "go" (Version 1 21 0) Binary True
          ]
      }
  , cfgConstraints = []
  }
