{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import Validator
import Logger
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.Text (pack, unpack)
import qualified Data.Text.IO as TIO

-- | Demonstrate the type checker with sample configs
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runDemo
    ["--demo"] -> runDemo
    ["--help"] -> printHelp
    ["--version"] -> putStrLn "sys-lang-checker v1.0.0"
    _ -> do
      putStrLn "Unknown arguments. Use --help for usage info."
      exitFailure

-- | Print help message
printHelp :: IO ()
printHelp = putStrLn $ unlines
  [ "System Language Config/Build Type Checker v1.0.0"
  , ""
  , "USAGE:"
  , "  sys-lang-checker [OPTIONS]"
  , ""
  , "OPTIONS:"
  , "  --demo              Run demonstration with sample configs"
  , "  --help              Show this help message"
  , "  --version           Show version information"
  , ""
  , "DESCRIPTION:"
  , "  Validates system programming language configurations and build"
  , "  environments for stability, compatibility, and best practices."
  ]

-- | Run demonstration with sample configs
runDemo :: IO ()
runDemo = do
  putStrLn "\n╔════════════════════════════════════════════════════════════╗"
  putStrLn "║  System Language Config/Build Stability Type Checker Demo  ║"
  putStrLn "╚════════════════════════════════════════════════════════════╝\n"
  
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "TEST 1: Valid Haskell GHC Configuration"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  result1 <- validateLanguageConfigVerbose haskellGhcConfig
  printValidationResult result1
  
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "TEST 2: Invalid Version Range"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  result2 <- validateLanguageConfigVerbose invalidVersionConfig
  printValidationResult result2
  
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "TEST 3: Python Configuration (Pre-release with Duplicates)"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  result3 <- validateLanguageConfigVerbose pythonConfig
  printValidationResult result3
  
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "TEST 4: Kotlin Configuration (Suspicious Compiler Version)"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  result4 <- validateLanguageConfigVerbose kotlinConfig
  printValidationResult result4
  
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "TEST 3: Pre-release with Missing Bounds"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  let preReleaseConfig = LanguageConfig
        { cfgLanguage = LanguageSpec
            { langName = "NewLang"
            , langVersion = Version 0 5 2
            , minGHCVersion = Nothing  -- Warning: no bounds
            , maxGHCVersion = Nothing
            }
        , cfgEnvironment = BuildEnvironment
            { osType = MacOS
            , osVersion = Version 12 0 0
            , compilerType = Clang
            , compilerVersion = Version 14 0 0
            , dependencies =
                [ Dependency "openssl" (Version 3 0 0) Library True
                , Dependency "openssl" (Version 3 0 0) Library True  -- Duplicate!
                ]
            }
        , cfgConstraints = []
        }
  let result3 = validateLanguageConfig preReleaseConfig
  printValidationResult result3
  
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "TEST 4: Compiler Version Check"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  let suspiciousVersionConfig = LanguageConfig
        { cfgLanguage = LanguageSpec
            { langName = "Go"
            , langVersion = Version 1 21 0
            , minGHCVersion = Nothing
            , maxGHCVersion = Nothing
            }
        , cfgEnvironment = BuildEnvironment
            { osType = Windows
            , osVersion = Version 10 0 19042
            , compilerType = GolangCompiler
            , compilerVersion = Version 99 99 99  -- Suspicious!
            , dependencies = []
            }
        , cfgConstraints = []
        }
  let result4 = validateLanguageConfig suspiciousVersionConfig
  printValidationResult result4
  putStrLn "\n" >> printSummary [result1, result2, result3, result4]
  exitSuccess

-- | Print a validation result with formatting
printValidationResult :: ValidationResult -> IO ()
printValidationResult result = do
  let statusIcon = if isValid result then "✓" else "✗"
  putStrLn $ statusIcon ++ " " ++ unpack (summary result)
  
  unless (null (errors result)) $ do
    putStrLn "\n  Errors:"
    mapM_ (printError) (errors result)
  
  unless (null (warnings result)) $ do
    putStrLn "\n  Warnings:"
    mapM_ (printWarning) (warnings result)

-- | Print a validation error
printError :: ValidationError -> IO ()
printError err = putStrLn $ "    [" ++ errorCode err ++ "] " 
  ++ unpack (errorMessage err) 
  ++ " (" ++ unpack (errorContext err) ++ ")"

-- | Print a validation warning
printWarning :: ValidationWarning -> IO ()
printWarning warn = putStrLn $ "    [" ++ warningCode warn ++ "] " 
  ++ unpack (warningMessage warn)

-- | Print summary statistics
printSummary :: [ValidationResult] -> IO ()
printSummary results = do
  putStrLn "╔════════════════════════════════════════════════════════════╗"
  putStrLn "║                    SUMMARY STATISTICS                      ║"
  putStrLn "╚════════════════════════════════════════════════════════════╝"
  let validCount = length (filter isValid results)
      totalErrors = sum (map (length . errors) results)
      totalWarnings = sum (map (length . warnings) results)
  putStrLn $ "Tests run:       " ++ show (length results)
  putStrLn $ "Valid configs:   " ++ show validCount ++ "/" ++ show (length results)
  putStrLn $ "Total errors:    " ++ show totalErrors
  putStrLn $ "Total warnings:  " ++ show totalWarnings
  putStrLn ""

-- | Haskell GHC Configuration (VALID)
haskellGhcConfig :: LanguageConfig
haskellGhcConfig = LanguageConfig
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
          ]
      }
  , cfgConstraints = []
  }

-- | Rust Configuration (INVALID - version range)
invalidVersionConfig :: LanguageConfig
invalidVersionConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Rust"
      , langVersion = Version 1 70 0
      , minGHCVersion = Just (Version 9 8 0)
      , maxGHCVersion = Just (Version 9 2 0)
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = Rustc
      , compilerVersion = Version 1 70 0
      , dependencies = []
      }
  , cfgConstraints = []
  }

-- | Python Configuration (INVALID - duplicates + missing bounds)
pythonConfig :: LanguageConfig
pythonConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Python"
      , langVersion = Version 3 13 0
      , minGHCVersion = Nothing
      , maxGHCVersion = Nothing
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = Python
      , compilerVersion = Version 3 13 0
      , dependencies =
          [ Dependency "numpy" (Version 1 24 0) Library True
          , Dependency "numpy" (Version 1 24 0) Library True
          ]
      }
  , cfgConstraints = []
  }

-- | Kotlin Configuration (INVALID - suspicious compiler version)
kotlinConfig :: LanguageConfig
kotlinConfig = LanguageConfig
  { cfgLanguage = LanguageSpec
      { langName = "Kotlin"
      , langVersion = Version 1 9 0
      , minGHCVersion = Nothing
      , maxGHCVersion = Nothing
      }
  , cfgEnvironment = BuildEnvironment
      { osType = Linux
      , osVersion = Version 5 15 0
      , compilerType = Kotlin
      , compilerVersion = Version 99 99 99
      , dependencies = []
      }
  , cfgConstraints = []
  }

-- | Helper for conditional execution
unless :: Bool -> IO () -> IO ()
unless p action = if p then return () else action
