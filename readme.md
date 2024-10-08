# Yakka

**Yakka** is a Chess Engine written in object pascal (Delphi) that implements the Universal Chess Interface (UCI) protocol.

## Target Platform

  - Windows 11 x64 Platform
  - Command-line, Console Application
  - Written in Delphi Object Pascal
  - Will run on x86-64 CPU's which support BMI2 instruction set

## How To Use

**Yakka** implements the Universal Chess Interface (UCI) protocol that is compatible with most popular Chess GUIs. 
  
It is recommended to use a Chess GUI application such as [Arena](http://www.playwitharena.de/), [BanksiaGUI](https://banksiagui.com/) or [Cute Chess](https://cutechess.com/).
  
Alternatively Yakka can be run as a command line console using UCI commands.

## Implemented Engine Options (UCI)

  - option name Threads type spin default 1 min 1 max 16
  - option name Hash type spin default 64 min 1 max 256
  - option name Clear Hash type button
  - option name OwnBook type check default false 

## Download

You can download the precompiled Windows executable : 'Yakka v1.1 x64.exe'
This will run on x86-64 CPU's which support the BMI2 instruction set.  

## Compilation

To build the engine from source, use Delphi 12.0 Alexandra or later version.

  - Import the source files from GitHub repository
  - Set Target Platform to Windows 64-bit
  - Open Project Resources and Images..., add the following to Resource Files:
      File Name : 'Opening_Book_00.txt'
      Type : RCDATA
      Identifier : Open_Book 
  - Build 'Yakka1'

## Functional Details

* Board Representation and Move Generation

  - Bitboard based
  - Make/Unmake
  - Fully legal move generation
  - uses PEXT / PDEP instructions (requires BMI2) for sliding move generation

* Search

  - ABDADA parallel search (supports up to 16 search threads)
  - Negamax search with Alpha-Beta pruning
  - Principal Variation Search (PVS)
  - Aspiration windows
  - Iterative deepening
  - Quiescence Search (QS)
  - Transposition table (two buckets with aging)
  - Singular Extensions

* Selectivity
 
  - Reverse futility pruning
  - Recursive null-move forward pruning
  - Enhanced forward pruning
  - Enhanced Transposition Cut-off (ETC)
  - Internal Iterative Deepening (IID)
  - Late Move Reduction (LMR)
  - Late Move Pruning 
 
* Move Ordering

  - Hash move
  - Captures (MVV/LVA)
  - Static Exchange Evaluation (SEE)
  - Killer moves 
  - Counter move
  - History heuristic  

* Evaluation

  - Hand Crafted Evaluation with approx. 2540 weights (v1.1)
  - Texel tuned parameters
  - Tapered Evaluation (mg, eg)
  - Material table
  - Piece Square Tables (PST)
  - Mobility
  - King Safety
  - Material Imbalance
  - Attack / Defense Piece Bonus 
  - Pawn Structure 
  - Hash Tables (Evaluation, Pawn Structure)

* Opening Book

  - Own internal format
  - Total Book Positions = 8507 (v1.1)
  - Total Book Moves = 12184 (v1.1)  

## Version History

* Yakka v1.0 - 10th April 2024 Initial Release
* Yakka v1.1 - 15th September 2024 Bug Fixes and Improved Playing Strength

## Help & Support

Tested on desktop with Intel Core i9-9900K CPU @ 3.60GHz, Windows 11 x64, using 'Cutechess-cli' 
Please let me know of any bugs, compilation-problems or stability issues.
Any ideas, comments or suggestions for improvement are always welcome.

##  License
  
This project is licenced under the MIT Licence

## Future Endeavours (TODO)

In no particular order:
  - Much experimenting / tweaking
  - MultiPV
  - Pondering   
  - Endgame Tablebases
  - NNUE



 