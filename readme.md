# Yakka

Yakka is a Chess Engine writen in object pascal (Delphi) that implements the Universal Chess Interface (UCI) protocol.


## Target Platform

  - Windows 11 x64 Platform
  - Command-line, Console Application
  - Written in Delphi Object Pascal
  - Will run on x86-64 CPU's which support BMI2 instruction set


## How to use

  **Yakka** implements the Universal Chess Interface (UCI) protocol that is compatible with most popular Chess GUIs. 
  
  It is recommended to use a Chess GUI application such as 
  
  Alternatively Yakka can be run as a command line console using UCI commands.


## Implemented Engine Options (UCI)

  - option name Threads type spin default 6 min 1 max 16
  - option name Hash type spin default 64 min 1 max 256
  - option name Clear Hash type button
  - option name OwnBook type check default true 


## Compilation

  You can download the precompiled executable for Windows : 'Yakka v1.0 x64.exe'
  This will run on x86-64 CPU's which support the BMI2 instruction set.

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

* Selectivity
 
  - Reverse futility pruning
  - Recursive null-move forward pruning
  - Enhanced forward pruning
  - Enhanced Transposition Cut-off (ETC)
  - Internal Iterative Deepening (IID)
  - Late Move Reduction (LMR)
 
* Move Ordering

  - Hash move
  - Captures (MVV/LVA)
  - Static Exchange Evaluation (SEE)
  - Killer moves 
  - Counter move
  - History heuristic  

* Evaluation

  - Hand Crafted Evaluation with approx 2540 weights (v1.0)
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
  - Total Book Positions = 8507 (v1.0)
  - Total Book Moves = 12184 (v1.0)  


## Version History

### Yakka v1.0 

  - 10th April 2024 Initial Release


## Help & Support

  Tested on desktop with Intel Core i9-9900K CPU @ 3.60GHz, Windows 11 x64 using 'Arena' GUI

  Please let me know of any bugs, compilation-problems or stability issues.

  Any ideas, comments or suggestions for improvement are always welcome.


## Future Endevours (TODO)

  In no particular order:
  - Much experimenting / tweaking
  - MultiPV
  - Pondering   
  - Singular Extensions
  - Endgame Tablebases
  - NNUE



 