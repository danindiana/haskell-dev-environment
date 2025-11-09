-- Example Python Configuration (VALID)
-- Language: Python v3.13.0 (with standard packages)

import Types
import Validator

pythonValidConfig :: LanguageConfig
pythonValidConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Python"
      , langVersion = Version 3 13 0
      , minGHCVersion = Just (Version 3 11 0)
      , maxGHCVersion = Just (Version 3 14 0)
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = Python
      , compilerVersion = Version 3 13 0
      , dependencies =
          [ Dependency "numpy" (Version 1 24 0) Library True
          , Dependency "pandas" (Version 2 0 0) Library True
          , Dependency "requests" (Version 2 31 0) Library False
          , Dependency "pytest" (Version 7 4 0) Tool False
          ]
      }
  , cfgConstraints = []
  }

-- Test: λ> isValid (validateLanguageConfig pythonValidConfig)
-- True
