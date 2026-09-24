//  The MIT License (MIT)

//  Chess Engine Yakka
//  Copyright (c) 2026 Christopher Crone

//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:

//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.

//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHOR OR COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.


unit UCI;

interface
  uses
    Winapi.Windows, System.Classes, System.SysUtils, System.StrUtils, System.Character, System.Math,
    System.SyncObjs, GameDef, GameNet, Common, CPU_Info, Search, OpenBook, syzygy;

const
  CurrentName = 'Yakka';

  {$IfDef DEBUG}
  CurrentVersion = 'v1.6 debug';
  {$ELSE}
  CurrentVersion = 'v1.6';
  {$EndIf}
  VersionDate =   '24 Sep 2026';
  Author = 'Christopher Crone';

  TransTableMaxValue = 256;

  //   v1.5  net = 'NNUE 768→512x2→1 501451147c +50+ gen8 b6.net'

  type TTokenKind =
        (tok_Undefined,
         tok_WhiteSpace,
         tok_Empty,
         tok_EOL,
         tok_uci,
         tok_debug,
         tok_isready,
         tok_ucinewgame,

         tok_position,
           tok_fen,
           tok_startpos,
           tok_moves,
           tok_move,
           tok_testpos,

         tok_stop,
         tok_ponderhit,
         tok_quit,
         tok_q,

         tok_go,
           tok_searchmoves,

           tok_ponder,

           tok_wtime,
           tok_btime,
           tok_winc,
           tok_binc,
           tok_movestogo,
           tok_depth,
           tok_nodes,
           tok_mate,

           tok_movetime,
           tok_infinite,

         tok_setoption,

           tok_name,

             tok_Hash,
             tok_ClearHash,
             tok_Threads,
             tok_OwnBook,
             tok_EngineAbout,
             tok_ShowWDL,
             tok_MultiPV,

         tok_type,
         tok_default,
         tok_min,
         tok_max,
         tok_var,
         tok_value,

         tok_true,
         tok_false,

         tok_number,
         tok_string,

         tok_draw,
         tok_d,
         tok_Speedtest,

         tok_SyzygyPath);


  const  KeyWords : array[0..53] of AnsiString = (
        '',
        '',
        '',
        '',
        'uci',
        'debug',
        'isready',
        'ucinewgame',
        'position',
        'fen',
        'startpos',
        'moves',
        '',
        'testpos',
        'stop',
        'ponderhit',
        'quit',
        'q',
        'go',
        'searchmoves',
        'ponder',
        'wtime',
        'btime',
        'winc',
        'binc',
        'movestogo',
        'depth',
        'nodes',
        'mate',
        'movetime',
        'infinite',
        'setoption',
        'name',
        'hash',
        'clear',
        'threads',
        'ownbook',
        'uci_engineabout',
        'uci_showwdl',
        'multipv',
        '',
        '',
        '',
        '',
        '',
        'value',
        'true',
        'false',
        '',
        '',
        'draw',
        'd',
        'speedtest',
        'syzygypath');

type
  TTag = array[0..15] of byte;


type
  TLexer = record
    FCurChar : PAnsiChar;

    FTokenKind : TTokenKind;
    FToken : AnsiString;

    procedure Initialize(Text : PAnsiChar);
    procedure Advance;                          inline;
    function  NextChar : PAnsiChar;             inline;
    procedure SkipWhiteSpace;                   inline;

    procedure GetNumber;
    procedure GetNextToken;
    procedure GetString;
    end;

var
  SearchBoard, Board : TBoard;
  GameMoveList : TGameMoveList;

  PVS_Search : TSearch;

  Quit: Boolean;
  Lexer : TLexer;

  flag : boolean;
  OutputLock : TCriticalSection;

procedure ExecuteProgram;
procedure DisplayID;
procedure DisplayEngineAbout;
procedure CheckIsReady(var PVS_Search : TSearch);
procedure Initialize_Engine(var PVS_Search : TSearch);
procedure New_Game(var Board : TBoard; var GameMoveList : TGameMoveList; var PVS_Search : TSearch);

procedure SetupPositionFromFen(var Board : TBoard; var GameMoveList : TGameMoveList; var S : PAnsiChar; out ok : boolean);
procedure SetupPositionFromStart(var Board : TBoard; var GameMoveList : TGameMoveList);
procedure SetupTestPosition(var Board : TBoard; var GameMoveList : TGameMoveList);

function MakeMoves(var Board : TBoard; var GameMoveList : TGameMoveList; S : PAnsiChar) : boolean;
procedure GetSearchMoves(const Board : TBoard; var SearchMoves : TMoveArray; S : PAnsiChar);

procedure WriteMessage(const msg : AnsiString);

procedure DrawBoard(const Board : TBoard);
procedure HaltSearch(var PVS_Search : TSearch);
procedure ShutDownSearch(var PVS_Search : TSearch);


implementation

uses
  Bench;

procedure TLexer.Initialize(Text : PAnsiChar);
  begin
  FCurChar := Text;
  end;


procedure TLexer.Advance;
  begin
  if FCurChar^ <> AnsiChar(#0) then
    Inc(FCurChar);
  end;


function TLexer.NextChar : PAnsiChar;
  begin
  if FCurChar^ > AnsiChar(#0) then
    result := FCurChar+1
   else
    result := AnsiChar(#0);
  end;


procedure TLexer.SkipWhiteSpace;
  begin
  while (FCurChar^ <> AnsiChar(#0)) and (Char(FCurChar^).IsWhiteSpace = true) do
    Advance;
  end;


function TagMatch(const tag : TTag; const str : AnsiString) : boolean;
  var
    i : integer;

  begin
  result := true;
  if tag[0] <> length(str) then
    exit(false);

  for i := 1 to tag[0] do
    if tag[i] <> ord(str[i]) then
      exit(false);
  end;


procedure TLexer.GetNextToken;

 const

    Lookup1: array[0..31] of Integer =
      ( 0,  0,  0,  2, 11,  0,  0, 12,
        0,  4,  0, 11, 32,  0,  7,  0,
        0,  0, 17,  1,  0,  9, 12, 13,
        9,  1,  1, 13,  0,  0, 34, 39);

    Lookup2: array[0..41] of Integer =
      (32, 45, 47, 51, 28,  5, 26, 24,
       23,  9, 31, 37, 13, 30, 29, 35,
        4, 46, 34, 18, 19, 11, 17, 15,
       52, 36, 50, 53,  8, 39, 25, 22,
       27, 20, 33,  6, 21, 16, 14, 10,
       38,  7);

  var
    i, len, f1, f2, f3 : integer;
    Start : PAnsiChar;
    Tag : TTag;

  begin
  if Char(FCurChar^).IsWhiteSpace then
    SkipWhiteSpace;

  Start := FCurChar;

  if AnsiChar(FCurChar^) in ['-','0'..'9'] then
    begin
    GetNumber;
    exit;
    end;

  while AnsiChar(FCurChar^) in ['a'..'z', 'A'..'Z', '_'] do
    Advance;

  len := min(FCurChar - Start, high(Tag));       // Tag is max 15 characters = length of longest keyword
  Tag[0] :=  len;
  for i := 1 to len do
    begin
    Tag[i] := ord(Start[i-1]);
    if (Tag[i] <= 90) and (Tag[i] >= 65) then   // converts to Lowercase
      Tag[i] := Tag[i] + 32;
    end;

  FTokenKind := tok_Undefined;

  if len > 0 then
    begin
                                                                                // perfect hash to obtain tokenkind directly
    f1 := (Tag[1] - 96) xor len;
    if (f1 >= Low(Lookup1)) and (f1 <= High(Lookup1)) then
      begin
                                                    // first letter xor length
      f2 := Tag[min(len, 2)] - 96 + (Tag[max(len, 1)] - 96) shr 1 - 3;            // second letter and last letter
      f3 :=  Lookup1[f1] + f2;

      if (f3 >= Low(Lookup2)) and (f3 <= High(Lookup2)) then
        begin
        FTokenKind := TTokenKind(Lookup2[f3]);

        if (Ord(FTokenKind) > High(Keywords)) or (TagMatch(Tag, Keywords[Ord(FTokenKind)]) = false) then  // check for exact match
          FTokenKind := tok_Undefined;
        end;
      end;

    // FToken := KeyWords[Ord(FTokenKind)];

    if (FTokenKind = tok_fen) or (FTokenKind = tok_moves) then
      Advance;

    if FTokenKind = tok_d then
      FTokenKind := tok_draw
     else if FTokenKind = tok_q then
      FTokenKind := tok_quit;
    end;

  if FTokenKind <> tok_Undefined then
    exit;

  if FCurChar^ = #13 then
    begin
    FTokenKind := tok_EOL;
    FToken := FCurChar^;
    exit;
    end;

  if FCurChar^ = #0 then
    begin
    FTokenKind := tok_EOL;
    FToken := FCurChar^;
    exit;
    end;

  Advance;               // unrecognised byte — consume it so callers can't spin
  FTokenKind := tok_Undefined;
  FToken := '';
  end;


procedure TLexer.GetNumber;
  var
    TokenStart : PAnsiChar;

  begin
  TokenStart :=  FCurChar;
  Advance;

  while AnsiChar(FCurChar^) in ['0'..'9'] do
    Advance;

  FTokenKind := tok_number;
  SetString(FToken, TokenStart, FCurChar - TokenStart);
  end;


procedure TLexer.GetString;
  var
    TokenStart : PAnsiChar;

    function GetNextStr : Ansistring;
      var
        Start : PAnsiChar;

      begin
      Advance;
      Start := FCurChar;
      Advance;
      while (FCurChar^ <> Char(0)) and (FCurChar^ <> Char('"')) do
        Advance;

      SetString(result, Start, FCurChar - Start);

      if (FCurChar^ = Char('"')) and (NextChar^ = Char('"')) then
        result := result + GetNextStr;
      end;

  begin
  Advance;
  TokenStart :=  FCurChar;

  while (FCurChar^ <> Char(0)) and (FCurChar^ <> Char('"')) do
    Advance;

  FTokenKind := tok_String;
  SetString(FToken, TokenStart, FCurChar - TokenStart);

  if (FCurChar^ = Char('"')) and (NextChar^ = Char('"')) then
    FToken := FToken + GetNextStr;
  Advance;
  end;


procedure WriteMessage(const msg : AnsiString);
  begin
  OutputLock.Acquire;
    try
    writeln(output, msg);
    flush(output);
    finally
    OutputLock.Release;
    end;
  end;


procedure DisplayID;      // response to 'uci'
  var
    DisplayString : AnsiString;

  begin
  DisplayString := AnsiString('id name ' + CurrentName + ' ' + CurrentVersion) + #13#10;
  DisplayString := DisplayString + AnsiString('id author ' + Author) + #13#10;

  DisplayString := DisplayString + AnsiString(' ') + #13#10;

  DisplayString := DisplayString + AnsiString('option name Threads type spin default 1 min 1 max ' + IntToStr(TThread.ProcessorCount)) + #13#10;
  DisplayString := DisplayString + AnsiString('option name Hash type spin default 64 min 1 max ' + IntToStr(TransTableMaxValue)) + #13#10;
  DisplayString := DisplayString + AnsiString('option name Clear Hash type button') + #13#10;
  DisplayString := DisplayString + AnsiString('option name Ponder type check default false') + #13#10;
  DisplayString := DisplayString + AnsiString('option name MultiPV type spin default 1 min 1 max 32') + #13#10;
  DisplayString := DisplayString + AnsiString('option name SyzygyPath type string default <empty>') + #13#10;
   //DisplayMessage := DisplayMessage + AnsiString('option name UCI_ShowWDL type check default false') + #13#10;   TO_DO
  DisplayString := DisplayString + AnsiString('option name OwnBook type check default false') + #13#10;

  if AVX512f_Supported = true then
    DisplayString := DisplayString + AnsiString('AVX512 supported by the CPU') + #13#10;

  WriteMessage(DisplayString);

  WriteMessage(AnsiString('uciok'));
  end;


procedure DisplayEngineAbout;
  begin
  WriteMessage(AnsiString('id about ' + CurrentName + ' ' + CurrentVersion + ' by ' + Author + ' - a UCI chess engine written in Delphi'));
  end;


function ParseClamped(const tok : AnsiString; lo, hi, default : Int64) : Int64;
  begin
  if not TryStrToInt64(string(tok), result) then
    result := default;
  result := Max(lo, Min(hi, result));
  end;


procedure Initialize_Engine(var PVS_Search : TSearch);
  begin
  if assigned(PVS_Search) = false then
    begin
    PVS_Search := TSearch.Create;

    // set default values for Yakka

    PVS_Search.ThreadCount := 1;
    PVS_Search.UseOwnBook := false;
    PVS_Search.WillPonder := false;
    PVS_Search.MultiPVCount := 1;

    PVS_Search.Use_TB := false;
    PVS_Search.TB_FilesPath := '';

    if PVS_Search.TransTable.TableSize <> 64 * 65_536 then
      PVS_Search.TransTable.SetTableSize(64);

    Board.Reset;

    PVS_Search.OnMessage := WriteMessage;
    end;
  end;


procedure CheckIsReady(var PVS_Search : TSearch);
  var
    WaitMs : integer;

  begin
  if assigned(PVS_Search) = false then
    Initialize_Engine(PVS_Search);

  WaitMs := 0;
  while (PVS_Search.ReadyToSearch = false) and (WaitMs < 1000) do
    begin
    sleep(1);
    inc(WaitMs);
    end;

  WriteMessage(AnsiString('readyok'));        // response to 'isready'
  end;


procedure New_Game(var Board : TBoard; var GameMoveList : TGameMoveList; var PVS_Search : TSearch);  // response to 'ucinewgame'
  begin
  if assigned(PVS_Search) = false then
    Initialize_Engine(PVS_Search);

  PVS_Search.NewGame;
  Board.Reset;
  GameMoveList.Clear;
  end;


procedure SetupPositionFromFen(var Board : TBoard; var GameMoveList : TGameMoveList; var S : PAnsiChar; out ok : boolean);
  var
    Fen, S1 : string;
    LengthFen : integer;

  begin
  S1 := string(S).ToLower;
  LengthFen := pos('moves', S1) - 2;
  if LengthFen > 0 then
    begin
    Fen := Leftstr(string(S), LengthFen);
    S := S + LengthFen + 1;
    end
   else
    begin
    Fen := string(S);
    S := S + Length(Fen);
    end;

  ok := BoardFromFEN(Fen, Board);     // Includes a check to see if board initialized - and initializes the board if not.
                                      // Initialization should already be done when uci command sent on engine startup.
  if ok then
    GameMoveList.Clear;
  end;


procedure SetupPositionFromStart(var Board : TBoard; var GameMoveList : TGameMoveList);
  begin
  Board.Reset;
  GameMoveList.Clear;
  end;


procedure SetupTestPosition(var Board : TBoard; var GameMoveList : TGameMoveList);

// test > position fen 2r2rk1/1ppq2b1/1n6/1N1Ppp2/p7/Pn2BP2/1PQ1BP2/3RK2R w K - 0 23
// TB_test  KBNvK  fen 8/2K5/8/8/k7/8/6N1/3B4 b - - 0 1

  begin
  Board.Reset;
  BoardFromFEN('2r2rk1/1ppq2b1/1n6/1N1Ppp2/p7/Pn2BP2/1PQ1BP2/3RK2R w K - 0 23', Board);

  GameMoveList.Clear;
  end;


function TryParseUCIMove(const Board : TBoard; var S : PAnsiChar; out Move : TMove) : boolean;
  var
    fromCell, toCell, fromRank, toRank : integer;
    Piece, CapturedPiece, PromotionPiece : integer;

    function ReadSquare(out cell, rank : integer) : boolean;
      begin
      result := false;

      if not (S^ in ['a'..'h']) then
        exit;

      cell := Ord(S^) - Ord('a');
      Inc(S);

      if not (S^ in ['1'..'8']) then
        exit;

      rank := Ord(S^) - Ord('1');
      Inc(S);
      cell := 8 * (7 - rank) + cell;          // a8=0 .. h1=63, matches existing 8*(7-Rank)+FileNo
      result := true;
      end;

  begin
  result := false;
  Move := 0;

  if not ReadSquare(fromCell, fromRank) then
    exit;

  if not ReadSquare(toCell, toRank) then
    exit;

  Piece := Board.GetPiece_asm(fromCell);
  if Piece = 0 then                              // no piece on the from-square
    exit;

  CapturedPiece := Board.GetPiece_asm(toCell);
  PromotionPiece := 0;

  if (Piece = Pawn) and ((toRank = 0) or (toRank = 7)) then
    begin
      case S^ of
        'q', 'Q' : PromotionPiece := 5;
        'r', 'R' : PromotionPiece := 4;
        'b', 'B' : PromotionPiece := 3;
        'n', 'N' : PromotionPiece := 2;
       else
        exit;                                  // pawn to last rank must name a promotion piece
      end;
    Inc(S);
    end;

  if not (S^ in [' ', #9, #13, #10, #0]) then     // trailing junk, e.g. 'e2e4x'
    exit;

  if (Piece = Pawn) and (toCell mod 8 <> fromCell mod 8) and (CapturedPiece = 0) then
    CapturedPiece := Pawn;                    // en-passant

  Move := UInt64(fromCell) or (UInt64(toCell) shl 6) or (UInt64(Piece) shl 12) or (UInt64(CapturedPiece) shl 16) or (UInt64(PromotionPiece) shl 20);
  Move.SetHalfMoveCount(Board.HalfCount);
  Move.SetCastleFlags(Board.MovedPieces);

  if Board.Enpassant <> 0 then
    Move.SetEnpassantCell(GetLowBit(Board.Enpassant));

  result := true;
  end;


function MakeMoves(var Board : TBoard; var GameMoveList : TGameMoveList; S : PAnsiChar) : boolean;
  var
    Work   : TBoard;
    Parsed : array[0..1023] of TMove;
    Count, i : integer;
    Move   : TMove;

  begin
  result := true;
  Work := Board;
  Count := 0;

  while not (S^ in [#13, #10, #0]) do
    begin
    if S^ in [' ', #9] then
      begin
      Inc(S);
      Continue;
      end;

    if not TryParseUCIMove(Work, S, Move) then
      Exit(false);

    if Count > High(Parsed) then
      Exit(false);

    Parsed[Count] := Move;
    Inc(Count);
    Work.MakeMove(Move);
    end;

  for i := 0 to Count - 1 do
    begin
    GameMoveList.AddMove(Parsed[i]);
    Board.MakeMove(Parsed[i]);
    end;
  end;


procedure GetSearchMoves(const Board : TBoard; var SearchMoves : TMoveArray; S : PAnsiChar);
  var
    Move : TMove;

  begin
  SearchMoves[0] := 0;

  while not (S^ in [#13, #10, #0]) do
    begin
    if S^ in [' ', #9] then
      begin
      Inc(S);
      Continue;
      end;

    if not TryParseUCIMove(Board, S, Move) then
      begin
      SearchMoves[0] := 0;
      exit;
      end;

    if SearchMoves[0] >= High(SearchMoves) then
      exit;

    Inc(SearchMoves[0]);
    SearchMoves[SearchMoves[0]] := Move;
    end;
  end;


procedure HaltSearch(var PVS_Search : TSearch);
  var
    WaitMs : integer;

  begin
  PVS_Search.Stop := true;

  WaitMs := 0;
  while PVS_Search.Searching and (WaitMs < 2000) do
    begin
    sleep(2);
    inc(WaitMs, 2);
    end;

  if not PVS_Search.Searching then    // search over, or none was running:
    PVS_Search.Stop := false;         // don't leave Stop set for the next search
  end;


procedure ShutDownSearch(var PVS_Search : TSearch);
  begin
  if assigned(PVS_Search) = true then
    begin
    PVS_Search.Shutdown;
    PVS_Search.Free;
    end;
  end;


procedure DrawBoard(const Board : TBoard);
  var
    line : Ansistring;
    cell, RankNo, FileNo : integer;
    piece, color, t : integer;
    tempstr, Fen : Ansistring;

    DisplayString : AnsiString;

  begin
  DisplayString := DisplayString + AnsiString('   +---+---+---+---+---+---+---+---+  ') + #13#10;

  line := ' ';

  RankNo := 0;
  FileNo := 0;

  while rankNo <= 7 do
    begin
    cell := RankNo*8 + FileNo;
    piece := Board.GetPiece_asm(cell);
    color := Board.GetColor(cell);
    t := color * 7 + piece;

    if FileNo = 0 then
      line := ' ' + AnsiString(IntToStr(8 - RankNo));

      case t of
       0 : tempstr := ' ';
       1 : tempstr := 'P';
       2 : tempstr := 'N';
       3 : tempstr := 'B';
       4 : tempstr := 'R';
       5 : tempstr := 'Q';
       6 : tempstr := 'K';
       7 : tempstr := ' ';
       8 : tempstr := 'p';
       9 : tempstr := 'n';
      10 : tempstr := 'b';
      11 : tempstr := 'r';
      12 : tempstr := 'q';
      13 : tempstr := 'k';
      14 : tempstr := ' ';
      end;

    line := line + ' | ' + tempstr;
    inc(FileNo);

    if FileNo = 8 then
      begin
      line := line + ' | ';
      if (RankNo = 0) and (Board.ToPlay = Black) then
        line := line + 'B';
      if (RankNo = 7) and (Board.ToPlay = White) then
        line := line + 'W';

      DisplayString := DisplayString + line + #13#10;
      DisplayString := DisplayString + AnsiString('   +---+---+---+---+---+---+---+---+  ') + #13#10;

      FileNo := 0;
      inc(RankNo);
      line := ' ';
      end;
    end;

  DisplayString := DisplayString + AnsiString('     a   b   c   d   e   f   g   h    ') + #13#10;
  DisplayString := DisplayString + AnsiString(' ') + #13#10;

  BoardToFen(FEN, Board);

  DisplayString := DisplayString +  AnsiString(' Fen: ') + Fen + #13#10;
  DisplayString := DisplayString + AnsiString(' ') + #13#10;

  WriteMessage(DisplayString);
  end;


function ConsoleEventProc(CtrlType: DWORD): BOOL;
  begin

  // A signal that the system sends to all processes attached to a console when
  // the user closes the console (either by clicking Close on the console
  // window's window menu, or by clicking the End Task button command from Task Manager).

  result := false;

    case CtrlType of

    CTRL_C_EVENT:         begin

                          end;

    CTRL_CLOSE_EVENT:     begin
                          if assigned(PVS_Search) then
                            begin
                            PVS_Search.ShutDown;
                            FreeAndNil(PVS_Search);
                            end;

                          quit := true;
                          result := true;
                          end;

    CTRL_BREAK_EVENT:     begin

                          end;

    CTRL_LOGOFF_EVENT:    begin
                          if assigned(PVS_Search) then
                            begin
                            PVS_Search.ShutDown;
                            FreeAndNil(PVS_Search);
                            end;

                          quit := true;
                          result := true;
                          end;

    CTRL_SHUTDOWN_EVENT:  begin
                          if assigned(PVS_Search) then
                            begin
                            PVS_Search.ShutDown;
                            FreeAndNil(PVS_Search);
                            end;

                          quit := true;
                          result := true;
                          end;

    else
      result := false;
    end;
  end;


procedure ExecuteProgram;
  var
    tempToken : TTokenKind;

    TimeRemaining_White : UInt64;           // UCI:   wtime <x> white has x msec left on the clock
    TimeRemaining_Black : UInt64;           // UCI:   btime <x> black has x msec left on the clock
    TimeIncrementPerMove_White : UInt64;    // UCI:   winc <x> white time increment per move in msec if x>0
    TimeIncrementPerMove_Black : UInt64;    // UCI:   binc <x> black time increment per move in msec if x>0
    MovesToGo :  UInt64;                    // UCI:   movestogo <x> therre are x moves until the next time control if x>0 (only sent if x>0)

    InputBuff: array[0..4095] of AnsiChar;
    TempBuff: array[0..4095] of AnsiChar;
    OutputBuff: array[0..4095] of AnsiChar;

    IntParam1, IntParam2, IntParam3 : integer;
    WDLCount, DTZCount: integer;

    QuitToExit : boolean;
    DisplayString : AnsiString;
    ok : boolean;

  begin
  flag := SetConsoleCtrlHandler(@ConsoleEventProc, true);

  TTextRec(Input).BufSize := SizeOf(InputBuff);
  TTextRec(Input).BufPtr := @InputBuff;

  TTextRec(Output).BufSize := SizeOf(OutputBuff);
  TTextRec(Output).BufPtr := @OutputBuff;

  QuitToExit := false;

  DisplayString := AnsiString(CurrentName + ' ' + CurrentVersion + ' by ' + Author) + #13#10;

  if PopCnt_Supported = false then
    begin
    DisplayString := DisplayString + AnsiString('Warning : CPU doesn''t support PopCnt asm instruction') + #13#10;
    DisplayString := DisplayString + AnsiString('Yakka will not run correctly on this CPU') + #13#10;
    DisplayString := DisplayString + AnsiString(' ') + #13#10;
    QuitToExit := true;
    end;

  if BMI2_Supported = false then
    begin
    DisplayString := DisplayString + AnsiString('Warning : CPU doesn''t support BMI2 instructions') + #13#10;
    DisplayString := DisplayString + AnsiString('PEXT and PDEP asm instructions are required for move generation') + #13#10;
    DisplayString := DisplayString + AnsiString('Yakka will not run correctly on this CPU') + #13#10;
    DisplayString := DisplayString + AnsiString(' ') + #13#10;
    QuitToExit := true;
    end;

  if AVX2_Supported = false then
    begin
    DisplayString := DisplayString + AnsiString('Warning : CPU doesn''t support AVX2 instructions') + #13#10;
    DisplayString := DisplayString + AnsiString('AVX2 instructions are required for NNUE evaluation') + #13#10;
    DisplayString := DisplayString + AnsiString('Yakka will not run correctly on this CPU') + #13#10;
    DisplayString := DisplayString + AnsiString(' ') + #13#10;
    QuitToExit := true;
    end;

  if QuitToExit = true then
    begin
    DisplayString := DisplayString + AnsiString('type ''quit'' to exit') + #13#10 ;
    DisplayString := DisplayString + AnsiString(' ') + #13#10;
    end;

  WriteMessage(DisplayString);

  {if AVX2_Supported = true then
    WriteLn(output, AnsiString('AVX2 supported by the CPU'))
   else
    WriteLn(output, AnsiString('AVX2 not supported by the CPU'));

  if AVX512f_Supported = true then
    begin
    WriteLn(output, AnsiString('AVX512 supported by the CPU'));
    end; }

  if QuitToExit = false then
    begin
      try
      Quit := False;

      while not Quit do
        begin
        if EOF(Input) then     // stdin closed without 'quit' → leave, don't spin
          Break;

        ReadLn(Input, TempBuff);

        Lexer.Initialize(PAnsiChar(@TempBuff));
        Lexer.GetNextToken;

        if not Assigned(PVS_Search) then      // every handler can now assume it exists
          Initialize_Engine(PVS_Search);

        // A running search owns the engine. Per the UCI protocol the GUI must send 'stop'
        // (or 'ponderhit') and wait for 'bestmove' before anything else — ignore the rest.

        if PVS_Search.Searching and not (Lexer.FTokenKind in [tok_stop, tok_ponderhit, tok_isready, tok_quit]) then
          continue;

          try
            case
            Lexer.FTokenKind of

              tok_uci : DisplayID;

              tok_EngineAbout : DisplayEngineAbout;

              tok_isready  : CheckIsReady(PVS_Search);

              tok_ucinewgame : New_Game(Board, GameMoveList, PVS_Search);

              tok_position : begin            // [position fen <fenstring> | startpos]  moves <move1> .... <movei>
                             ok := false;
                             Lexer.GetNextToken;
                             if Lexer.FTokenKind = tok_fen then
                               begin
                               SetUpPositionFromFen(Board, GameMoveList, Lexer.FCurChar, ok);
                               PVS_Search.RepStartBoard := CopyBoard(Board);
                               end
                              else if Lexer.FTokenKind = tok_startpos then
                               begin
                               SetUpPositionFromStart(Board, GameMoveList);
                               PVS_Search.RepStartBoard := CopyBoard(Board);
                               ok := true;
                               end
                              else if Lexer.FTokenKind = tok_testpos then
                               begin
                               SetUpTestPosition(Board, GameMoveList);
                               PVS_Search.RepStartBoard := CopyBoard(Board);
                               ok := true;
                               end
                              else
                               WriteMessage(AnsiString('info string unknown command'));

                             if ok = true then
                               begin
                               Lexer.GetNextToken;
                               if Lexer.FTokenKind = tok_moves then
                                 if MakeMoves(Board, GameMoveList, Lexer.FCurChar) = false then
                                   WriteMessage(AnsiString('info string Illegal move(s)'));
                               end
                              else
                                WriteMessage(AnsiString('info string illegal fen'));
                             end;

              tok_go :  begin
                        if assigned(PVS_Search) = false then
                          Initialize_Engine(PVS_Search);

                        SearchBoard := CopyBoard(Board);

                        PVS_Search.DepthLimit := MaxSearchPly;
                        PVS_Search.TimeLimit := TimeLimit_Max;
                        PVS_Search.UseSoftTimeLimit := false;
                        PVS_Search.NodeLimit := NodeLimit_Max;

                        PVS_Search.SearchMoveList[0] := 0;
                        PVS_Search.IsPondering := false;

                          repeat
                          Lexer.GetNextToken;

                          if Lexer.FTokenKind = tok_depth then
                            begin
                            Lexer.GetNextToken;
                            if Lexer.FTokenKind = tok_number then
                              PVS_Search.DepthLimit := ParseClamped(Lexer.fToken, 1, MaxSearchPly, MaxSearchPly)
                             else
                              WriteMessage(AnsiString('info string unknown Depth'));
                            end;

                          if Lexer.FTokenKind = tok_nodes then
                            begin
                            Lexer.GetNextToken;
                            if Lexer.FTokenKind = tok_number then
                              PVS_Search.NodeLimit  := ParseClamped(Lexer.fToken, 1, NodeLimit_Max, NodeLimit_Max)
                             else
                              WriteMessage(AnsiString('info string unknown Nodes'));
                            end;

                          if Lexer.FTokenKind = tok_mate then
                            WriteMessage(AnsiString('mate'));              // Not yet implemented

                          if Lexer.FTokenKind = tok_movetime then
                            begin
                            PVS_Search.UseSoftTimeLimit := false;
                            PVS_Search.TickCarryover := 0;
                            Lexer.GetNextToken;
                            if Lexer.FTokenKind = tok_number then
                              PVS_Search.TimeLimit  := ParseClamped(Lexer.fToken, 1, TimeLimit_Max, TimeLimit_Max)
                             else
                              WriteMessage(AnsiString('info string unknown Time'));
                            end;

                          if Lexer.FTokenKind = tok_infinite then
                            begin
                            PVS_Search.DepthLimit := MaxSearchPly;
                            PVS_Search.TimeLimit := TimeLimit_Max;
                            PVS_Search.UseSoftTimeLimit := false;
                            PVS_Search.TickCarryover := 0;
                            PVS_Search.NodeLimit := NodeLimit_Max;
                            end;

                          if Lexer.FTokenKind = tok_ponder then
                            PVS_Search.IsPondering := true;

                          if Lexer.FTokenKind in [tok_wtime, tok_btime, tok_winc, tok_binc, tok_movestogo] then
                            begin
                            TimeRemaining_White := 0;
                            TimeRemaining_Black := 0;
                            TimeIncrementPerMove_White := 0;
                            TimeIncrementPerMove_Black := 0;

                            MovesToGo := 0;

                              repeat
                              tempToken := Lexer.FTokenKind;
                              Lexer.GetNextToken;
                              if Lexer.FTokenKind = tok_number then
                                begin
                                  case tempToken of
                                  tok_wtime     : TimeRemaining_White := ParseClamped(Lexer.fToken, 0, TimeLimit_Max, 0);
                                  tok_btime     : TimeRemaining_Black := ParseClamped(Lexer.fToken, 0, TimeLimit_Max, 0);
                                  tok_winc      : TimeIncrementPerMove_White := ParseClamped(Lexer.fToken, 0, TimeLimit_Max, 0);
                                  tok_binc      : TimeIncrementPerMove_Black := ParseClamped(Lexer.fToken, 0, TimeLimit_Max, 0);
                                  tok_movesToGo : begin
                                                  MovesToGo := ParseClamped(Lexer.fToken, 0, 1024, 0);
                                                  if MovesToGo > 0 then
                                                    PVS_Search.TickCarryover := 0;
                                                  end;
                                  end;

                                Lexer.GetNextToken;
                                end;

                              until Lexer.FTokenKind in [tok_EOL, tok_searchmoves];

                            PVS_Search.UseSoftTimeLimit := true;
                            PVS_Search.AllocateTimeForSearch(MovesToGo, TimeRemaining_White, TimeIncrementPerMove_White, TimeRemaining_Black, TimeIncrementPerMove_Black, Board);
                            end;

                          if Lexer.FTokenKind = tok_searchmoves then
                            begin
                            GetSearchMoves(Board, PVS_Search.SearchMoveList, Lexer.FCurChar);
                            Lexer.FTokenKind := tok_EOL;
                            end;

                          until Lexer.FTokenKind = tok_EOL;

                        if Lexer.FTokenKind = tok_EOL then
                          PVS_Search.StartInBackGround(SearchBoard, GameMoveList);

                        //if Lexer.FTokenKind = tok_Undefined then
                        //  WriteMessage(AnsiString('info string Unknown command'));
                        end;

              tok_draw : DrawBoard(Board);

              tok_setoption : begin
                              if assigned(PVS_Search) = false then
                                Initialize_Engine(PVS_Search);

                              Lexer.GetNextToken;

                              if Lexer.FTokenKind = tok_name then
                                begin
                                Lexer.GetNextToken;

                                if Lexer.FTokenKind = tok_Hash then                 // option name Hash type spin default 64 min 1 max 256
                                  begin
                                  Lexer.GetNextToken;
                                  if Lexer.FTokenKind = tok_value then
                                    begin
                                    Lexer.GetNextToken;
                                    if Lexer.FTokenKind = tok_number then
                                      PVS_Search.SetTransTableSize(ParseClamped(Lexer.fToken, 1, TransTableMaxValue, 64));
                                    end;
                                  end

                                else if Lexer.FTokenKind = tok_ClearHash then       // option name Clear Hash type button
                                  begin
                                  Lexer.GetNextToken;
                                  if Lexer.FTokenKind = tok_hash then
                                    PVS_Search.ClearTranspositionTable;
                                  end

                                else if Lexer.FTokenKind = tok_Threads then         // option name Threads type spin default 1 min 1 max count of logical cpu processors
                                  begin
                                  Lexer.GetNextToken;
                                  if Lexer.FTokenKind = tok_value then
                                    begin
                                    Lexer.GetNextToken;
                                    if Lexer.FTokenKind = tok_number then
                                      PVS_Search.SetThreadCount(ParseClamped(Lexer.fToken, 1, TThread.ProcessorCount, 1));
                                    end;
                                  end

                                else if Lexer.FTokenKind = tok_OwnBook then              // option name OwnBook type check default true
                                  begin
                                  Lexer.GetNextToken;
                                  if Lexer.FTokenKind = tok_value then
                                    begin
                                    Lexer.GetNextToken;
                                    if Lexer.FTokenKind = tok_true then
                                      begin
                                      T_OpeningBook.Prepare;
                                      PVS_Search.UseOwnBook := true;
                                      end
                                     else if Lexer.FTokenKind = tok_false then
                                      PVS_Search.UseOwnBook := false;
                                    end;
                                  end

                                else if Lexer.FTokenKind = tok_Ponder then              // option name Ponder type check default false
                                  begin
                                  Lexer.GetNextToken;
                                  if Lexer.FTokenKind = tok_value then
                                    begin
                                    Lexer.GetNextToken;
                                    if Lexer.FTokenKind = tok_true then
                                      PVS_Search.WillPonder := true
                                     else if Lexer.FTokenKind = tok_false then
                                      PVS_Search.WillPonder := false;
                                    end;
                                  end

                                else if Lexer.FTokenKind = tok_MultiPV then              // option name MultiPV type check default false
                                  begin
                                  Lexer.GetNextToken;
                                  if Lexer.FTokenKind = tok_value then
                                    begin
                                    Lexer.GetNextToken;
                                    if Lexer.FTokenKind = tok_number then
                                      PVS_Search.MultiPVCount := ParseClamped(Lexer.fToken, 1, 32, 1);
                                    end;
                                  end

                                else if Lexer.FTokenKind = tok_SyzygyPath then           // option name SyzygyPath type string default ''
                                  begin
                                  Lexer.GetNextToken;
                                  if Lexer.FTokenKind = tok_value then
                                    begin
                                    Lexer.GetString;
                                    if Lexer.FTokenKind = tok_string then
                                      begin
                                      if PVS_Search.TB_FilesPath <> AnsiString(Lexer.FToken) then
                                        begin
                                        PVS_Search.TB_FilesPath := AnsiString(Lexer.FToken);
                                        PVS_Search.Use_TB := Syzygy.TB_init(PAnsiChar(PVS_Search.TB_FilesPath));
                                        if PVS_Search.Use_TB = true then
                                          begin
                                          PVS_Search.TB_MaxPieceCount := min(Syzygy.TB_MaxCardinality[WDL], 6);
                                          Syzygy.TB_TableCount(WDLCount, DTZCount);
                                          WriteMessage(AnsiString('info string found ' + IntToStr(WDLCount) + ' WDL and ' + IntToStr(DTZCount) + ' DTZ tablebase files'));
                                          end
                                         else
                                          begin
                                          PVS_Search.TB_FilesPath := '';            // no files found so
                                          PVS_Search.TB_MaxPieceCount := -1;
                                          end;
                                        end
                                      end;
                                    end;
                                  end

                                end;
                              end;

              tok_speedtest : begin
                              IntParam1 := 1;         // threads
                              IntParam2 := 64;        // TT_Size
                              IntParam3 := 150;       // target duration

                              Lexer.GetNextToken;
                              if Lexer.FTokenKind = tok_number then
                                begin
                                IntParam1 := ParseClamped(Lexer.fToken, 1, TThread.ProcessorCount, 1);

                                Lexer.GetNextToken;
                                if Lexer.FTokenKind = tok_number then
                                  IntParam2 := ParseClamped(Lexer.fToken, 1, TransTableMaxValue, 64);

                                Lexer.GetNextToken;
                                if Lexer.FTokenKind = tok_number then
                                  IntParam3 := ParseClamped(Lexer.fToken, 1, 3600, 150);
                                end;

                              SpeedTest(IntParam1, IntParam2, IntParam3, PVS_Search);
                              end;

              tok_ponderhit : PVS_Search.IsPondering := false;

              tok_stop : HaltSearch(PVS_Search);

              tok_quit : Quit := true;

             else
              WriteMessage('info string unknown command');

            end;
          except
            on E: Exception do
              {WriteMessage(AnsiString('info string command error: ' + E.Message))};
          end;
        end;

      finally
      if assigned(PVS_Search) then
        begin
        PVS_Search.ShutDown;
        FreeAndNil(PVS_Search);
        end;
      end;
    end
   else
    begin
    Quit := False;

    while (not Quit) and (not EOF(Input)) do
      begin
      ReadLn(Input, TempBuff);

      Lexer.Initialize(PAnsiChar(@TempBuff));
      Lexer.GetNextToken;

      if Lexer.FTokenKind = tok_quit then
       Quit := true;
      end;
    end;
  end;

initialization
  OutputLock := TCriticalSection.Create;
finalization
  OutputLock.Free;

//  UCI Interface

//  GUI To Engine:

//        uci	                                       Display engine name and author name
//        isready	                                   Respond readyok
//        setoption	                                 Set engine option
//        register	                                 This engine does not require registration
//        ucinewgame	                               Reset TransTable, best move cache, killer moves, and move history
//        position	                                 Setup pieces on board
//        go wtime btime winc binc movestogo	       Determine think time, search, return best move
//        go depth nodes mate movetime infinite      Search to a specific depth (ply), node count, mate in x moves,
//                                                     for a specific duration, or until stopped via stop or quit command

//        go searchmoves	                           Search only specified moves
//        go ponder	                                 Pondering (thinking on opponent’s time)
//        stop	                                     Stop searching, return best move
//        ponderhit	                                 Pondering (thinking on opponent’s time)
//        uci_engineabout                            Display engine name, author name, and engine description
//        quit	                                     Exit program


//  Engine To GUI:

//        id name / author	                         Display engine name and author name
//        uciok	                                     Ready to receive commands
//        readyok	                                   Ready to receive position
//        bestmove	                                 Return best move
//        ponder	                                   Return opponents apparent best move

//        copyprotection	                           not implemented, Yakka is not copy-protected
//        registration	                             not implemented, Yakka does not require registration

//        info                                       Update each depth
//             multipv
//             depth  / {seldepth}                   "seldepth" is the furthest it has looked ahead in any variation
//             score cp
//             nodes
//             nps
//             hashfull                              Display TT utilization
//             time
//             wdl                                   not yet supported
//             pv


//        info string                                Display information


//  Options:
//
//        option name Threads type spin default 1 min 1 max 16
//
//                  >   Sets the number of CPU threads used for searching a position.
//                  >   For best performance, set this equal to the number of CPU cores available.
//
//        option name Hash type spin default 64 min 1 max 256
//
//                  >   Sets the size of the hash table in MB. It is recommended to set Hash after setting Threads.
//
//        option name Clear Hash type button
//
//                  >   Clears the hash table.
//
//        option name Ownbook type check default false
//
//                  >   Use imbedded opening book
//
//        option name Ponder type check default false
//
//                  >   This means that the engine is able to ponder
//                  >   When set to true, this changes the time management algorithm
//
//        option name MultiPV type spin default 1 min 1 max 32
//
//                  >   Output the N best lines (PVs) when searching. Leave at 1 for the best performance.
//                  >   ToDo, currrently not supported
//
//        option name UCI_ShowWDL type check default false   :    Not yet implemented
//
//                  >   Include wdl <win_prob> <draw_prob> <loss_prob> in the info line,
//                  >   Probabilities are in integer per thousand and always add up to 1000
//                  >   ToDo, currrently not supported
//
//        option name SyzygyPath type string default <empty>
//
//                  >   Where the Syzygy tablebase files are located.
//                  >   e.g. the folder(s) containing the .rtbw and .rtbz files.
//                  >   Multiple paths can be separated with semicolons on Windows.
//




end.
