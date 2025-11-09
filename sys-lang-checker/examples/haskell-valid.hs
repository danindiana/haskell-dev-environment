-- Example Haskell GHC Configuration (VALID)
-- Language: Haskell v9.6.7 (stable)

import Types
import Validator

haskellValidConfig :: LanguageConfig
haskellValidConfig = LanguageConfig
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
          , Dependency "base" (Version 4 16 0) Library True
          , Dependency "containers" (Version 0 6 7) Library True
          , Dependency "cabal-install" (Version 3 12 0) Tool False
          ]
      }
  , cfgConstraints = []
  }

-- Test: λ> isValid (validateLanguageConfig haskellValidConfig)
-- True
