-- Example Node.js / TypeScript Configuration (VALID)
-- Language: TypeScript v5.2.2 running on Node.js v20.9.0

import Types
import Validator

nodeJsValidConfig :: LanguageConfig
nodeJsValidConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "TypeScript"
      , langVersion = Version 5 2 2
      , minGHCVersion = Just (Version 5 0 0)
      , maxGHCVersion = Just (Version 5 3 0)
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = NodeJS
      , compilerVersion = Version 20 9 0
      , dependencies =
          [ Dependency "typescript" (Version 5 2 2) Tool True
          , Dependency "react" (Version 18 0 0) Library False
          , Dependency "express" (Version 4 18 0) Library False
          , Dependency "jest" (Version 29 0 0) Tool False
          ]
      }
  , cfgConstraints = []
  }

-- Test: λ> isValid (validateLanguageConfig nodeJsValidConfig)
-- True
