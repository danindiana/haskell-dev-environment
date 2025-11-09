-- Example Haskell GHC Configuration
-- This demonstrates a valid and stable configuration

import Types
import Validator

ghcValidConfig :: LanguageConfig
ghcValidConfig = LanguageConfig
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
          , Dependency "containers" (Version 0 6 7) Library True
          , Dependency "cabal-install" (Version 3 8 1) Tool False
          ]
      }
  , cfgConstraints = []
  }

-- Test this configuration:
-- λ> let result = validateLanguageConfig ghcValidConfig
-- λ> isValid result
-- True
