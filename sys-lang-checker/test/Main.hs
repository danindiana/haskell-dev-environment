{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import Validator
import Data.Text (pack)

main :: IO ()
main = do
  putStrLn "\n╔════════════════════════════════════════════════════════════╗"
  putStrLn "║        System Language Checker - Unit Test Suite          ║"
  putStrLn "╚════════════════════════════════════════════════════════════╝\n"
  
  let results = 
        [ testVersionFormatValidation
        , testVersionRangeValidation
        , testDuplicateDependencies
        , testValidGHCConfig
        ]
  
  let passCount = length (filter id results)
      totalCount = length results
  
  putStrLn $ "\n✓ Passed: " ++ show passCount ++ "/" ++ show totalCount ++ " tests"
  
  if passCount == totalCount
    then putStrLn "All tests passed! ✓\n"
    else do
      putStrLn "Some tests failed! ✗\n"
      return ()

-- Test 1: Version format validation
testVersionFormatValidation :: Bool
testVersionFormatValidation = 
  let spec = LanguageSpec
        { langName = "TestLang"
        , langVersion = Version 1 2 3
        , minGHCVersion = Nothing
        , maxGHCVersion = Nothing
        }
      config = LanguageConfig spec 
        (BuildEnvironment Linux (Version 5 15 0) GHC (Version 9 6 7) [])
        []
      result = validateLanguageConfig config
  in isValid result && null (errors result)

-- Test 2: Version range validation
testVersionRangeValidation :: Bool
testVersionRangeValidation =
  let spec = LanguageSpec
        { langName = "TestLang"
        , langVersion = Version 1 0 0
        , minGHCVersion = Just (Version 9 0 0)
        , maxGHCVersion = Just (Version 8 0 0)  -- Invalid: min > max
        }
      config = LanguageConfig spec
        (BuildEnvironment Linux (Version 5 15 0) GHC (Version 9 6 7) [])
        []
      result = validateLanguageConfig config
  in not (isValid result) && not (null (errors result))

-- Test 3: Duplicate dependencies detection
testDuplicateDependencies :: Bool
testDuplicateDependencies =
  let env = BuildEnvironment
        { osType = Linux
        , osVersion = Version 5 15 0
        , compilerType = GHC
        , compilerVersion = Version 9 6 7
        , dependencies =
            [ Dependency "text" (Version 2 0 0) Library True
            , Dependency "text" (Version 2 0 0) Library True  -- Duplicate!
            ]
        }
      spec = LanguageSpec
        { langName = "Haskell"
        , langVersion = Version 9 6 7
        , minGHCVersion = Just (Version 9 0 0)
        , maxGHCVersion = Just (Version 9 8 0)
        }
      config = LanguageConfig spec env []
      result = validateLanguageConfig config
  in not (isValid result) && not (null (errors result))

-- Test 4: Valid GHC configuration
testValidGHCConfig :: Bool
testValidGHCConfig =
  let spec = LanguageSpec
        { langName = "Haskell"
        , langVersion = Version 9 6 7
        , minGHCVersion = Just (Version 9 2 0)
        , maxGHCVersion = Just (Version 9 8 0)
        }
      env = BuildEnvironment
        { osType = Linux
        , osVersion = Version 5 15 0
        , compilerType = GHC
        , compilerVersion = Version 9 6 7
        , dependencies =
            [ Dependency "aeson" (Version 2 0 0) Library True
            , Dependency "text" (Version 2 0 0) Library True
            ]
        }
      config = LanguageConfig spec env []
      result = validateLanguageConfig config
  in isValid result && null (errors result) && 
     any (\w -> warningCode w == "WARN_NO_MAX_GHC" || warningCode w == "WARN_NO_MIN_GHC") (warnings result) ||
     (isValid result && null (errors result))
