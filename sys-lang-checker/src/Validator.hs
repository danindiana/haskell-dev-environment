{-# LANGUAGE OverloadedStrings #-}

module Validator where

import Types
import Logger
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import System.IO (hFlush, stdout)

-- | Main validation entry point
validateLanguageConfig :: LanguageConfig -> ValidationResult
validateLanguageConfig config =
  let
    versionErrs = validateVersions config
    envErrs = validateEnvironment config
    depErrs = validateDependencies config
    constraintErrs = validateConstraints config
    allErrs = concat [versionErrs, envErrs, depErrs, constraintErrs]
    allWarnings = generateWarnings config allErrs
    isOk = null allErrs
    summary = mkSummary isOk (length allErrs) (length allWarnings)
  in
    ValidationResult isOk allErrs allWarnings summary

-- | Verbose validation with console output
validateLanguageConfigVerbose :: LanguageConfig -> IO ValidationResult
validateLanguageConfigVerbose config = do
  let name = langName (cfgLanguage config)
  logValidationStart name
  
  -- Version validation
  logStep 1 "Checking language version format..."
  let versionErrs = validateVersions config
  mapM_ (\err -> logCheckResult "Version Format" False (errorMessage err)) versionErrs
  if null versionErrs then logCheckResult "Version Format" True "Valid" else return ()
  
  -- Environment validation
  logStep 2 "Checking build environment..."
  let envErrs = validateEnvironment config
  mapM_ (\err -> logCheckResult "Environment" False (errorMessage err)) envErrs
  if null envErrs then logCheckResult "Environment" True "Valid" else return ()
  
  -- Dependency validation
  logStep 3 "Checking dependencies..."
  let depErrs = validateDependencies config
  mapM_ (\err -> logCheckResult "Dependencies" False (errorMessage err)) depErrs
  if null depErrs then logCheckResult "Dependencies" True "No duplicates found" else return ()
  
  -- Constraint validation
  logStep 4 "Checking stability constraints..."
  let constraintErrs = validateConstraints config
  mapM_ (\err -> logCheckResult "Constraints" False (errorMessage err)) constraintErrs
  if null constraintErrs then logCheckResult "Constraints" True "All constraints satisfied" else return ()
  
  -- Generate warnings
  logStep 5 "Generating warnings..."
  let allErrs = concat [versionErrs, envErrs, depErrs, constraintErrs]
  let allWarnings = generateWarnings config allErrs
  mapM_ (\warn -> logCheckResult "Warning" True (warningMessage warn)) allWarnings
  if null allWarnings then logCheckResult "Warnings" True "None" else return ()
  
  -- Final result
  let isOk = null allErrs
  let summary = mkSummary isOk (length allErrs) (length allWarnings)
  logValidationComplete summary isOk
  logSeparator
  putStrLn ""
  
  return $ ValidationResult isOk allErrs allWarnings summary

-- | Validate version compatibility
validateVersions :: LanguageConfig -> [ValidationError]
validateVersions (LanguageConfig spec _ _) =
  catMaybes
    [ checkVersionFormat (langName spec) (langVersion spec)
    , checkVersionRange spec
    ]

-- | Check if version format is valid
checkVersionFormat :: Text -> Version -> Maybe ValidationError
checkVersionFormat langName ver =
  if major ver >= 0 && minor ver >= 0 && patch ver >= 0
    then Nothing
    else Just $ ValidationError
      { errorCode = "VER_INVALID_FORMAT"
      , errorMessage = "Version has invalid negative components"
      , errorContext = pack $ unpack langName ++ " v" ++ showVersion ver
      }

-- | Validate version range constraints
checkVersionRange :: LanguageSpec -> Maybe ValidationError
checkVersionRange spec =
  case (minGHCVersion spec, maxGHCVersion spec) of
    (Just minV, Just maxV) ->
      if minV <= maxV
        then Nothing
        else Just $ ValidationError
          { errorCode = "VER_INVALID_RANGE"
          , errorMessage = "Min GHC version exceeds max version"
          , errorContext = pack $ unpack (langName spec) ++ 
              " (min: " ++ showVersion minV ++ ", max: " ++ showVersion maxV ++ ")"
          }
    _ -> Nothing

-- | Validate build environment
validateEnvironment :: LanguageConfig -> [ValidationError]
validateEnvironment (LanguageConfig _ env _) =
  catMaybes
    [ checkCompilerVersion env
    , checkOSSupport env
    ]

-- | Check compiler version is reasonable
checkCompilerVersion :: BuildEnvironment -> Maybe ValidationError
checkCompilerVersion env =
  let v = compilerVersion env
  in if major v <= 20 && minor v <= 20  -- Sanity check for reasonable versions
       then Nothing
       else Just $ ValidationError
         { errorCode = "ENV_COMPILER_SUSPICIOUS"
         , errorMessage = "Compiler version seems unreasonably high"
         , errorContext = pack $ show (compilerType env) ++ " v" ++ showVersion v
         }

-- | Ensure OS is supported
checkOSSupport :: BuildEnvironment -> Maybe ValidationError
checkOSSupport _ = Nothing  -- All OSes in enum are valid

-- | Validate dependencies
validateDependencies :: LanguageConfig -> [ValidationError]
validateDependencies (LanguageConfig _ env _) =
  concat
    [ checkDuplicateDeps (dependencies env)
    , checkRequiredDeps (dependencies env)
    ]

-- | Check for duplicate dependency names
checkDuplicateDeps :: [Dependency] -> [ValidationError]
checkDuplicateDeps deps =
  let depNames = map depName deps
      hasDups name = length (filter (== name) depNames) > 1
      dupNames = filter hasDups (nub depNames)
      toError name = ValidationError
        { errorCode = "DEP_DUPLICATE"
        , errorMessage = "Duplicate dependency definition"
        , errorContext = name
        }
  in map toError dupNames
 where
  nub [] = []
  nub (x:xs) = x : nub (filter (/= x) xs)

-- | Check required dependencies are present
checkRequiredDeps :: [Dependency] -> [ValidationError]
checkRequiredDeps _ = []  -- Application-specific logic would go here

-- | Validate stability constraints
validateConstraints :: LanguageConfig -> [ValidationError]
validateConstraints (LanguageConfig spec _ constraints) =
  concat [validateConstraint spec c | c <- constraints]

-- | Validate individual constraint
validateConstraint :: LanguageSpec -> StabilityConstraint -> [ValidationError]
validateConstraint spec constraint =
  case constraintType constraint of
    VersionRange -> validateVersionConstraint spec constraint
    OSDependency -> validateOSConstraint spec constraint
    CompilerFeature -> validateFeatureConstraint spec constraint
    PerformanceTarget -> validatePerformanceConstraint spec constraint

-- | Validate version range constraint
validateVersionConstraint :: LanguageSpec -> StabilityConstraint -> [ValidationError]
validateVersionConstraint spec constraint =
  case parseVersionRange (constraintValue constraint) of
    Left err -> [ValidationError
      { errorCode = "CONSTRAINT_VER_INVALID"
      , errorMessage = pack err
      , errorContext = constraintValue constraint
      }]
    Right (minV, maxV) ->
      if minV <= langVersion spec && langVersion spec <= maxV
        then []
        else [ValidationError
          { errorCode = "CONSTRAINT_VER_MISMATCH"
          , errorMessage = "Language version outside required range"
          , errorContext = constraintValue constraint <> " but found " <> pack (showVersion (langVersion spec))
          }]

-- | Parse version range like "1.0.0 - 2.5.0"
parseVersionRange :: Text -> Either String (Version, Version)
parseVersionRange t = Left "Version range parsing not yet implemented"

-- | Validate OS dependency constraint
validateOSConstraint :: LanguageSpec -> StabilityConstraint -> [ValidationError]
validateOSConstraint _ _ = []

-- | Validate compiler feature constraint
validateFeatureConstraint :: LanguageSpec -> StabilityConstraint -> [ValidationError]
validateFeatureConstraint _ _ = []

-- | Validate performance target constraint
validatePerformanceConstraint :: LanguageSpec -> StabilityConstraint -> [ValidationError]
validatePerformanceConstraint _ _ = []

-- | Generate warnings based on config
generateWarnings :: LanguageConfig -> [ValidationError] -> [ValidationWarning]
generateWarnings config errs =
  catMaybes
    [ warnOldVersion (cfgLanguage config)
    , warnMissingVersionBounds (cfgLanguage config)
    ]
 where
  warnOldVersion spec =
    if major (langVersion spec) < 1
      then Just $ ValidationWarning
        { warningCode = "WARN_PRERELEASE"
        , warningMessage = "Language version is pre-release (< 1.0.0)"
        }
      else Nothing
  
  warnMissingVersionBounds spec =
    case (minGHCVersion spec, maxGHCVersion spec) of
      (Nothing, _) -> Just $ ValidationWarning
        { warningCode = "WARN_NO_MIN_GHC"
        , warningMessage = "No minimum GHC version specified"
        }
      (_, Nothing) -> Just $ ValidationWarning
        { warningCode = "WARN_NO_MAX_GHC"
        , warningMessage = "No maximum GHC version specified"
        }
      _ -> Nothing

-- | Generate summary text
mkSummary :: Bool -> Int -> Int -> Text
mkSummary isValid errCount warnCount
  | isValid && warnCount == 0 = "✓ Configuration is stable and valid"
  | isValid = "✓ Valid with " <> pack (show warnCount) <> " warning(s)"
  | otherwise = "✗ Invalid: " <> pack (show errCount) <> " error(s), " <> pack (show warnCount) <> " warning(s)"
