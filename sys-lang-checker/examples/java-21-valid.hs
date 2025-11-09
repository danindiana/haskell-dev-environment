-- Example Java Configuration (VALID)
-- Language: Java v21 (LTS) with Maven

import Types
import Validator

javaValidConfig :: LanguageConfig
javaValidConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Java"
      , langVersion = Version 21 0 0
      , minGHCVersion = Just (Version 17 0 0)
      , maxGHCVersion = Just (Version 22 0 0)
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = JavaC
      , compilerVersion = Version 21 0 1
      , dependencies =
          [ Dependency "maven" (Version 3 9 0) Tool True
          , Dependency "junit" (Version 4 13 0) Library False
          , Dependency "log4j" (Version 2 20 0) Library True
          , Dependency "spring-boot" (Version 3 1 0) Library False
          ]
      }
  , cfgConstraints = []
  }

-- Test: λ> isValid (validateLanguageConfig javaValidConfig)
-- True
