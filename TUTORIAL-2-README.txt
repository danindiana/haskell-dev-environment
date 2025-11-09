╔═══════════════════════════════════════════════════════════════════╗
║   🎮 HASKELL TUTORIAL 2: ADVANCED TOPICS & CHOOSE-YOUR-OWN-ADVENTURE 🎮 ║
╚═══════════════════════════════════════════════════════════════════╝

📦 NEW FILES CREATED
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✓ haskell-tutorial-2.sh      - Main interactive tutorial (choose-your-own-adventure)
✓ START-TUTORIAL-2.sh        - Quick launcher for Tutorial 2
✓ python-setup.sh            - Compile Python 3.13 from source + create venv
✓ py/requirements.txt        - Python dependencies (requests, aiohttp, pandas)
✓ py/scripts/generate_data.py - Python script to generate JSON sample data

🎯 TUTORIAL 2 FEATURES
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✨ CHOOSE-YOUR-OWN-ADVENTURE MENU:
  [1] Typeclasses Deep Dive
      • Deriving Eq, Ord, Show
      • Custom typeclasses
      • Instance definitions

  [2] Functor → Applicative → Monad
      • Maybe and Either types
      • Monadic do-notation
      • Applicative operators (<$>, <*>)

  [3] JSON with Aeson + Python Interop
      • JSON encoding/decoding
      • Custom instances with GHC.Generics
      • Python venv integration
      • Bidirectional data exchange

  [4] Concurrency with async
      • mapConcurrently for parallel tasks
      • Threading and thread delays
      • Real-world async patterns

  [5] Parsing with Megaparsec
      • Arithmetic expression parser
      • Operator precedence & associativity
      • Error handling in parsers

  [6] Property Testing with QuickCheck
      • Generative testing
      • Property definitions
      • Automatic test case generation

  [A] Run ALL sections in sequence
  [Q] Quit

🐍 PYTHON SETUP
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

To compile Python 3.13 from source and create a venv:

    cd ~/programs/haskell_install
    ./python-setup.sh

This will:
  • Download Python 3.13.0
  • Compile with optimizations (--enable-optimizations --with-lto)
  • Install to ~/.local/python-versions/3.13.0
  • Create venv at: tutorial2-venv/
  • Install packages from requirements.txt

⏱️  Compilation takes ~5-10 minutes on your Ryzen 5950X.

After setup, activate with:
    source ~/programs/haskell_install/tutorial2-venv/bin/activate

🎮 HOW TO RUN
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Interactive menu (choose topics):
    cd ~/programs/haskell_install
    ./START-TUTORIAL-2.sh

Or directly:
    ./haskell-tutorial-2.sh

Run all sections at once (non-interactive):
    ./haskell-tutorial-2.sh
    [at menu, type: A]

Print menu only (testing):
    ./haskell-tutorial-2.sh --print-menu

🎨 80s RETRO AESTHETICS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✓ ASCII art headers for each section
✓ Cyan, magenta, yellow, green color scheme
✓ Block graphics and retro fonts
✓ "Press ENTER to continue" pacing
✓ Self-contained temporary workspace per run

🔧 TECHNICAL ARCHITECTURE
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Each section:
  1. Creates a temporary Haskell .hs file
  2. Writes GHCi script (.ghci)
  3. Runs via: ghci < script.ghci
  4. Filters output (removes noise)
  5. Displays results with color coding

JSON section also:
  • Checks if Python venv exists
  • Runs generate_data.py via venv
  • Loads/parses JSON in Haskell
  • Demonstrates FFI-like interop

Parsing section:
  • Uses Megaparsec for full expression parsing
  • Demonstrates operator precedence
  • Catches and reports errors

Concurrency section:
  • Uses async package
  • Runs tasks in parallel
  • Shows real-world speedup

QuickCheck section:
  • Generates random test data
  • Validates properties automatically
  • Tests reverse, quicksort

📚 TOPICS COVERED (NOT IN TUTORIAL 1)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Beyond Tutorial 1 basics:
✓ Typeclasses (creating your own)
✓ Functor/Applicative/Monad hierarchy
✓ Monadic do-notation
✓ Either type (error handling)
✓ JSON parsing/generation (Aeson)
✓ Concurrency (Control.Concurrent.Async)
✓ Parser combinators (Megaparsec)
✓ Property-based testing (QuickCheck)
✓ Python interoperability via venv
✓ GHC.Generics for JSON derivation

⏱️  DURATION PER SECTION
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

~1-2 minutes each
~10-15 minutes for "Run ALL"

💾 FILES GENERATED AT RUNTIME
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

All created in a temporary directory (cleaned up on exit):
  section1/Typeclasses.hs
  section2/FAM.hs
  section3/People.hs
  section3/ReadJson.hs
  section3/data.json (if Python available)
  section4/Conc.hs
  section5/Parse.hs
  section6/Props.hs

🎓 LEARNING PATH
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Recommended order:
1. Start with Tutorial 1 (basics)
2. Run Tutorial 2 section [1] (Typeclasses)
3. Run Tutorial 2 section [2] (F/A/M)
4. Run Tutorial 2 sections [5] → [6] (advanced patterns)
5. Try section [3] with Python interop
6. Explore section [4] (concurrency)

Then build your own projects!

🚀 QUICK START
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

1. Run Tutorial 2:
   cd ~/programs/haskell_install
   ./START-TUTORIAL-2.sh

2. (Optional) Set up Python:
   ./python-setup.sh

3. Explore sections interactively
4. Press ENTER to advance at your own pace
5. Choose [Q] to quit

🎉 ENJOY!
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

May your types be strong and your monads be pure! 🧲
