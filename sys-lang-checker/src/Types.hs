{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Language specification with version constraints
data LanguageSpec = LanguageSpec
  { langName :: Text
  , langVersion :: Version
  , minGHCVersion :: Maybe Version
  , maxGHCVersion :: Maybe Version
  } deriving (Show, Eq, Generic)

-- | Semantic versioning
data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  } deriving (Show, Eq, Ord, Generic)

instance Bounded Version where
  minBound = Version 0 0 0
  maxBound = Version 999 999 999

-- | Parse version from string "X.Y.Z"
parseVersion :: String -> Either String Version
parseVersion s = case break (== '.') s of
  (majStr, '.' : rest) -> case break (== '.') rest of
    (minStr, '.' : patchStr) -> do
      maj <- readInt majStr
      min' <- readInt minStr
      patch' <- readInt patchStr
      return $ Version maj min' patch'
    _ -> Left "Invalid version format: expected X.Y.Z"
  _ -> Left "Invalid version format: expected X.Y.Z"
 where
  readInt :: String -> Either String Int
  readInt s' = case reads s' of
    [(n, "")] -> Right n
    _ -> Left $ "Cannot parse integer: " ++ s'

-- | Display version as string
showVersion :: Version -> String
showVersion (Version maj min' patch') = 
  show maj ++ "." ++ show min' ++ "." ++ show patch'

-- | Comparison result with reason
data Compatibility = Compatible | Incompatible String | WarnInfo String
  deriving (Show, Eq)

-- | Build environment configuration
data BuildEnvironment = BuildEnvironment
  { osType :: OS
  , osVersion :: Version
  , compilerType :: CompilerType
  , compilerVersion :: Version
  , dependencies :: [Dependency]
  } deriving (Show, Eq, Generic)

-- | Operating system type
data OS = Linux | MacOS | Windows | BSD
  deriving (Show, Eq, Enum, Bounded, Generic)

-- | Compiler type available for system languages
data CompilerType 
  = GHC
  | Rustc
  | GolangCompiler
  | Clang
  | MSVC
  | Python
  | JavaC
  | Kotlin
  | NodeJS
  | OtherCompiler String
  deriving (Show, Eq, Generic)

-- | External dependency constraint
data Dependency = Dependency
  { depName :: Text
  , depVersion :: Version
  , depKind :: DependencyKind
  , depRequired :: Bool
  } deriving (Show, Eq, Generic)

-- | What kind of dependency
data DependencyKind = Library | Binary | Tool
  deriving (Show, Eq, Generic)

-- | Stability constraint for validation
data StabilityConstraint = StabilityConstraint
  { constraintType :: ConstraintType
  , constraintValue :: Text
  , severity :: Severity
  } deriving (Show, Eq, Generic)

-- | Type of constraint
data ConstraintType
  = VersionRange
  | OSDependency
  | CompilerFeature
  | PerformanceTarget
  deriving (Show, Eq, Generic)

-- | Severity level
data Severity = Critical | Warning | Info
  deriving (Show, Eq, Ord, Generic)

-- | Complete validation result
data ValidationResult = ValidationResult
  { isValid :: Bool
  , errors :: [ValidationError]
  , warnings :: [ValidationWarning]
  , summary :: Text
  } deriving (Show, Eq, Generic)

-- | Validation error (must-fix)
data ValidationError = ValidationError
  { errorCode :: String
  , errorMessage :: Text
  , errorContext :: Text
  } deriving (Show, Eq, Generic)

-- | Validation warning (nice-to-fix)
data ValidationWarning = ValidationWarning
  { warningCode :: String
  , warningMessage :: Text
  } deriving (Show, Eq, Generic)

-- | Config file format
data ConfigFormat = YAML | JSON | TOML
  deriving (Show, Eq)

-- | Language configuration from file
data LanguageConfig = LanguageConfig
  { cfgLanguage :: LanguageSpec
  , cfgEnvironment :: BuildEnvironment
  , cfgConstraints :: [StabilityConstraint]
  } deriving (Show, Eq, Generic)
