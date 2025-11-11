# Yakka

**Yakka** is a Chess Engine written in object pascal (Delphi) that implements the Universal Chess Interface (UCI) protocol.

## Target Platform

  - Windows 11 x64 Platform
  - Command-line, Console Application
  - Written in Delphi Object Pascal
  - Will run on x86-64 CPU's which support BMI2 and AVX2 instruction set

## How To Use

**Yakka** implements the Universal Chess Interface (UCI) protocol that is compatible with most popular Chess GUIs. 
  
It is recommended to use a Chess GUI application such as [Arena](http://www.playwitharena.de/), [BanksiaGUI](https://banksiagui.com/) or [Cute Chess](https://cutechess.com/).
  
Alternatively Yakka can be run as a command line console using UCI commands.

## Implemented Engine Options (UCI)

  - option name Threads type spin default 1 min 1 max 16
  - option name Hash type spin default 64 min 1 max 256
  - option name Clear Hash type button
  - option name Ponder type check default false
  - option name OwnBook type check default false  
  - option name UCI_EngineAbout type string  

## Download

You can download the precompiled Windows executable : 'Yakka v1.4 x64.exe'
This will run on x86-64 CPU's which support the BMI2 and AVX2 instruction sets.  

## Compilation

To build the engine from source, use Delphi 12.0 Alexandra or later version.

  - Import the source files from GitHub repository
  - Set Target Platform to Windows 64-bit
  - Open Project Resources and Images..., add the following to Resource Files:  
  
	  - File Name : 'Opening_Book_00.txt'
	  - Type : RCDATA
	  - Identifier : Open_Book  
	  
	  - File Name : 'NNUE 768x256_x2 31222017c gen7 early.net'
	  - Type : RCDATA
	  - Identifier : \_768x256\_x2\_gen7\_early  
	  
	  - File Name : 'NNUE 768x256_x2 85639857c gen7 mid.net'
	  - Type : RCDATA
	  - Identifier : \_768x256\_x2\_gen7\_mid  
	  
	  - File Name : 'NNUE 768x256_x2 169081011c gen7 late.net'
	  - Type : RCDATA
	  - Identifier : \_768x256\_x2\_gen7\_late  
	  
  - Build 'Yakka'

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
  - Singular Extension

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

  - NNUE net 768>256x2>1 (requires AVX2)

* Opening Book

  - Own internal format
  - Total Book Positions = 8507 (v1.1)
  - Total Book Moves = 12184 (v1.1)  

## Version History

* Yakka v1.0 - 10th April 2024       Initial release
* Yakka v1.1 - 15th September 2024   Bug fixes and improved playing strength
* Yakka v1.2 -  8th February 2025    Changed evaluation from HCE to NNUE 
* Yakka v1.3 - 10th August 2025      Bug fixes, implemented pondering and improved NNUE implementation
* Yakka v1.4 - 12th November 2025    Bug fixes and improved NNUE implementation

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
  - Endgame Tablebases




 