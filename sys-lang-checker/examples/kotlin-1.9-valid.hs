-- Example Kotlin Configuration (VALID)
-- Language: Kotlin v1.9.10 with JVM target

import Types
import Validator

kotlinValidConfig :: LanguageConfig
kotlinValidConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Kotlin"
      , langVersion = Version 1 9 10
      , minGHCVersion = Just (Version 1 8 0)
      , maxGHCVersion = Just (Version 2 0 0)
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = Kotlin
      , compilerVersion = Version 1 9 10
      , dependencies =
          [ Dependency "kotlinc" (Version 1 9 10) Binary True
          , Dependency "kotlin-stdlib" (Version 1 9 10) Library True
          , Dependency "junit" (Version 4 13 0) Library False
          ]
      }
  , cfgConstraints = []
  }

-- Test: λ> isValid (validateLanguageConfig kotlinValidConfig)
-- True
