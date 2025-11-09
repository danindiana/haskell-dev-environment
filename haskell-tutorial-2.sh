#!/bin/bash

# Haskell Interactive Tutorial 2 - Advanced Topics
# Retro 1980s ASCII art + Choose-Your-Own-Adventure style

set -e
shopt -s lastpipe

# Colors
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# Ensure ghcup environment is loaded
if ! command -v ghc &> /dev/null; then
  if [ -f "$HOME/.ghcup/env" ]; then
    # shellcheck disable=SC1090
    source "$HOME/.ghcup/env"
  else
    echo -e "${RED}GHCup not found. Run the installer first.${NC}"
    exit 1
  fi
fi

# Where to find an optional Python venv
VENV_DIR="/home/jeb/programs/haskell_install/tutorial2-venv"

pause() {
  echo ""
  echo -e "${CYAN}Press ENTER to continue...${NC}"
  read -r
}

header() {
  clear
  cat << 'EOF'

 ███    ██  ██████  ██    ██ ███████ ███    ███  ██████  
 ████   ██ ██    ██ ██    ██ ██      ████  ████ ██       
 ██ ██  ██ ██    ██ ██    ██ █████   ██ ████ ██ ██   ███ 
 ██  ██ ██ ██    ██  ██  ██  ██      ██  ██  ██ ██    ██ 
 ██   ████  ██████    ████   ███████ ██      ██  ██████  
                                                         
   ▄████▄   ▄▄▄       ███▄ ▄███▓ ▄▄▄       ███▄    █ 
  ▒██▀ ▀█  ▒████▄    ▓██▒▀█▀ ██▒▒████▄     ██ ▀█   █ 
  ▒▓█    ▄ ▒██  ▀█▄  ▓██    ▓██░▒██  ▀█▄  ▓██  ▀█ ██▒
  ▒▓▓▄ ▄██▒░██▄▄▄▄██ ▒██    ▒██ ░██▄▄▄▄██ ▓██▒  ▐▌██▒
  ▒ ▓███▀ ░ ▓█   ▓██▒▒██▒   ░██▒ ▓█   ▓██▒▒██░   ▓██░
  ░ ░▒ ▒  ░ ▒▒   ▓▒█░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ▒░   ▒ ▒ 
    ░  ▒     ▒   ▒▒ ░░  ░      ░  ▒   ▒▒ ░░ ░░   ░ ▒░
  ░          ░   ▒   ░      ░     ░   ▒      ░   ░ ░ 
  ░ ░            ░  ░       ░         ░  ░         ░ 
  ░                                                   
EOF
  echo -e "${MAGENTA}${BOLD}             ADVANCED HASKELL - CHOOSE YOUR OWN ADVENTURE${NC}"
  echo ""
}

menu() {
  echo -e "${YELLOW}Pick a path:${NC}"
  echo "  1) Typeclasses Deep Dive (Eq, Ord, Show) + Your own typeclass"
  echo "  2) Functor → Applicative → Monad (with Maybe/Either)"
  echo "  3) JSON with Aeson + Python interop (via venv)"
  echo "  4) Concurrency with async (mapConcurrently)"
  echo "  5) Parsing with Megaparsec (arithmetic)"
  echo "  6) Property testing with QuickCheck"
  echo "  A) Run ALL sections"
  echo "  Q) Quit"
  echo ""
}

# Utility: run ghci quietly with a script
run_ghci_script() {
  local dir="$1"; shift
  local script="$1"; shift
  ( cd "$dir" && ghci < "$script" 2>&1 | grep -v "^Ok,\|^Loaded\|Leaving GHCi" )
}

# Create a fresh temp workspace per run
WORKDIR="$(mktemp -d)"
trap 'rm -rf "$WORKDIR"' EXIT

# SECTION 1: Typeclasses
section_typeclasses() {
  header
  echo -e "${GREEN}SECTION 1: Typeclasses Deep Dive${NC}"
  mkdir -p "$WORKDIR/section1"
  cat > "$WORKDIR/section1/Typeclasses.hs" <<'HS'
module Typeclasses where

-- Deriving common classes
data Color = Red | Green | Blue
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Custom Show
newtype Secret = Secret Int
instance Show Secret where
  show (Secret n) = "Secret(" ++ replicate n '*' ++ ")"

-- Your own typeclass
class Size a where
  size :: a -> Int

instance Size [a] where
  size = length

instance Size Color where
  size Red   = 1
  size Green = 2
  size Blue  = 3

-- Typeclass with default method
class Describable a where
  describe :: a -> String
  describe _ = "<mystery>"

instance Describable Color where
  describe Red = "Passion"
  describe Green = "Growth"
  describe Blue = "Calm"
HS
  cat > "$WORKDIR/section1/script.ghci" <<'GHCI'
:load Typeclasses.hs
[minBound .. maxBound] :: [Color]
Red < Blue
show (Secret 5)
size [10,20,30,40]
map describe ([Red,Green,Blue])
:quit
GHCI
  run_ghci_script "$WORKDIR/section1" "$WORKDIR/section1/script.ghci"
  pause
}

# SECTION 2: F/A/M
section_fam() {
  header
  echo -e "${GREEN}SECTION 2: Functor → Applicative → Monad${NC}"
  mkdir -p "$WORKDIR/section2"
  cat > "$WORKDIR/section2/FAM.hs" <<'HS'
module FAM where

-- Functor / Applicative / Monad with Maybe & Either
addThree :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThree a b c = do
  x <- a
  y <- b
  z <- c
  pure (x + y + z)

safeRead :: String -> Either String Int
safeRead s = case reads s of
  [(n, "")] -> Right n
  _          -> Left ("Not an Int: " ++ s)

combineEither :: String -> String -> Either String Int
combineEither s1 s2 = (+) <$> safeRead s1 <*> safeRead s2
HS
  cat > "$WORKDIR/section2/script.ghci" <<'GHCI'
:load FAM.hs
fmap (*2) (Just 21)
addThree (Just 1) (Just 2) (Just 3)
addThree (Just 1) Nothing (Just 3)
combineEither "10" "32"
combineEither "10" "oops"
:quit
GHCI
  run_ghci_script "$WORKDIR/section2" "$WORKDIR/section2/script.ghci"
  pause
}

# SECTION 3: JSON + Python interop
section_json_python() {
  header
  echo -e "${GREEN}SECTION 3: JSON with Aeson + Python Interop${NC}"
  echo "(Optional) This demo uses a Python venv at: $VENV_DIR"
  echo "If not present, run python-setup.sh first. We'll still show Aeson usage."
  mkdir -p "$WORKDIR/section3"
  cat > "$WORKDIR/section3/People.hs" <<'HS'
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module People where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON   Person

sample :: IO ()
sample = do
  let p = Person "Ada" 36
  putStrLn "Encode to JSON:"
  BL.putStrLn (encode p)
  let js = "{\"name\":\"Grace\",\"age\":47}"
  putStrLn "Decode from JSON:"
  print (decode (BL.pack js) :: Maybe Person)
HS
  cat > "$WORKDIR/section3/people.ghci" <<'GHCI'
:load People.hs
sample
:quit
GHCI
  # Try running Python generator if venv exists
  if [ -x "$VENV_DIR/bin/python" ] && [ -f "/home/jeb/programs/haskell_install/py/scripts/generate_data.py" ]; then
    echo -e "${CYAN}Running Python to generate JSON...${NC}"
    "$VENV_DIR/bin/python" \
      "/home/jeb/programs/haskell_install/py/scripts/generate_data.py" > "$WORKDIR/section3/data.json"

    cat > "$WORKDIR/section3/ReadJson.hs" <<'HS'
{-# LANGUAGE OverloadedStrings #-}
module ReadJson where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  bs <- BL.readFile "data.json"
  case eitherDecode bs :: Either String [(String, Int)] of
    Left err -> putStrLn ("Decode error: " ++ err)
    Right xs -> do
      putStrLn "Loaded name/age pairs:"
      print xs
HS
    ( cd "$WORKDIR/section3" && runghc ReadJson.hs ) || true
  fi

  run_ghci_script "$WORKDIR/section3" "$WORKDIR/section3/people.ghci"
  pause
}

# SECTION 4: Concurrency
section_concurrency() {
  header
  echo -e "${GREEN}SECTION 4: Concurrency with async${NC}"
  mkdir -p "$WORKDIR/section4"
  cat > "$WORKDIR/section4/Conc.hs" <<'HS'
module Conc where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)

work :: Int -> IO Int
work n = do
  threadDelay (300000) -- 0.3s
  pure (n * n)

demo :: IO ()
demo = do
  putStrLn "Running tasks concurrently..."
  res <- mapConcurrently work [1..6]
  print res
HS
  cat > "$WORKDIR/section4/conc.ghci" <<'GHCI'
:load Conc.hs
demo
:quit
GHCI
  run_ghci_script "$WORKDIR/section4" "$WORKDIR/section4/conc.ghci"
  pause
}

# SECTION 5: Parsing with Megaparsec
section_parse() {
  header
  echo -e "${GREEN}SECTION 5: Parsing with Megaparsec${NC}"
  mkdir -p "$WORKDIR/section5"
  cat > "$WORKDIR/section5/Parse.hs" <<'HS'
{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

pExpr :: Parser Integer
pExpr = makeExprParser pTerm table
  where
    table = [ [ InfixL (symbol "*" >> pure (*))
              , InfixL (symbol "/" >> pure div) ]
            , [ InfixL (symbol "+" >> pure (+))
              , InfixL (symbol "-" >> pure (-)) ]
            ]

pTerm :: Parser Integer
pTerm = choice
  [ between (symbol "(") (symbol ")") pExpr
  , integer
  ]

parseExpr :: String -> Either String Integer
parseExpr s = case runParser (sc *> pExpr <* eof) "expr" s of
  Left e  -> Left (errorBundlePretty e)
  Right n -> Right n
HS
  cat > "$WORKDIR/section5/parse.ghci" <<'GHCI'
:load Parse.hs
parseExpr "1 + 2 * 3"
parseExpr "(1 + 2) * 3"
parseExpr "7 * (10 - 3) / 2"
:quit
GHCI
  run_ghci_script "$WORKDIR/section5" "$WORKDIR/section5/parse.ghci"
  pause
}

# SECTION 6: Property testing with QuickCheck
section_quickcheck() {
  header
  echo -e "${GREEN}SECTION 6: Property Testing with QuickCheck${NC}"
  mkdir -p "$WORKDIR/section6"
  cat > "$WORKDIR/section6/Props.hs" <<'HS'
module Props where

import Test.QuickCheck

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

prop_sortMin :: NonEmptyList Int -> Bool
prop_sortMin (NonEmpty xs) = head (quickSort xs) == minimum xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) = quickSort [x | x <- xs, x <= p]
                ++ [p]
                ++ quickSort [x | x <- xs, x > p]
HS
  cat > "$WORKDIR/section6/props.ghci" <<'GHCI'
:module + Test.QuickCheck
:load Props.hs
quickCheck prop_reverse
quickCheck prop_sortMin
:quit
GHCI
  run_ghci_script "$WORKDIR/section6" "$WORKDIR/section6/props.ghci"
  pause
}

run_all() {
  section_typeclasses
  section_fam
  section_json_python
  section_concurrency
  section_parse
  section_quickcheck
}

# Ensure needed Haskell packages are available (for sections 3,5,6)
ensure_pkgs() {
  echo -e "${YELLOW}Checking required Haskell packages (aeson, megaparsec, QuickCheck)...${NC}"
  # Try compiling tiny one-liners to trigger cabal-install cache (using -package)
  echo "import Data.Aeson; main = putStrLn \"ok\"" | runghc -package aeson - 2>/dev/null || true
  echo "import Text.Megaparsec; main = putStrLn \"ok\"" | runghc -package megaparsec - 2>/dev/null || true
  echo "import Test.QuickCheck; main = putStrLn \"ok\"" | runghc -package QuickCheck - 2>/dev/null || true
}

main_loop() {
  header
  ensure_pkgs
  while true; do
    menu
    echo -n "Your choice: "
    read -r choice
    case "${choice^^}" in
      1) section_typeclasses ; header ; ;;
      2) section_fam ; header ; ;;
      3) section_json_python ; header ; ;;
      4) section_concurrency ; header ; ;;
      5) section_parse ; header ; ;;
      6) section_quickcheck ; header ; ;;
      A) run_all ; header ; ;;
      Q) echo -e "${GREEN}Goodbye!${NC}" ; exit 0 ;;
      *) echo -e "${RED}Invalid choice.${NC}" ;;
    esac
  done
}

if [[ "$1" == "--print-menu" ]]; then
  header
  menu
  exit 0
fi

main_loop
