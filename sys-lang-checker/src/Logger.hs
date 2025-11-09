{-# LANGUAGE OverloadedStrings #-}

module Logger where

import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hFlush, stdout)

-- | Log level for filtering
data LogLevel = DEBUG | INFO | WARN | ERROR
  deriving (Show, Eq, Ord)

-- | Log entry with formatted output
logMessage :: LogLevel -> Text -> IO ()
logMessage level msg = do
  let prefix = case level of
        DEBUG -> "  [DEBUG] "
        INFO  -> "  [INFO]  "
        WARN  -> "  [WARN]  "
        ERROR -> "  [ERROR] "
  putStr prefix
  putStrLn (T.unpack msg)
  hFlush stdout

-- | Log section header
logSection :: Text -> IO ()
logSection title = do
  putStrLn ""
  putStrLn $ "┌─ " ++ T.unpack title ++ " " ++ replicate (60 - length (T.unpack title)) '-'
  hFlush stdout

-- | Log validation step
logStep :: Int -> Text -> IO ()
logStep step msg = do
  let stepNum = "[Step " ++ show step ++ "]"
  putStrLn $ "  " ++ stepNum ++ " " ++ T.unpack msg
  hFlush stdout

-- | Log check result
logCheckResult :: Text -> Bool -> Text -> IO ()
logCheckResult checkName passed msg = do
  let icon = if passed then "✓" else "✗"
  let status = if passed then "PASS" else "FAIL"
  putStrLn $ "    " ++ icon ++ " " ++ T.unpack checkName ++ " (" ++ status ++ "): " ++ T.unpack msg
  hFlush stdout

-- | Log validation starting
logValidationStart :: Text -> IO ()
logValidationStart configName = do
  logSection "VALIDATION STARTING"
  logMessage INFO $ "Validating: " <> configName
  putStrLn ""

-- | Log validation complete
logValidationComplete :: Text -> Bool -> IO ()
logValidationComplete summary isValid = do
  logSection "VALIDATION COMPLETE"
  let icon = if isValid then "✓✓" else "✗✗"
  logMessage INFO $ icon <> " " <> summary

-- | Log separator
logSeparator :: IO ()
logSeparator = putStrLn "  └─────────────────────────────────────────────────────────────"
