//  The MIT License (MIT)

//  Chess Engine Yakka
//  Copyright (c) 2024 Christopher Crone

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
    System.Classes, System.SysUtils, System.StrUtils, System.Character, GameDef, Common, Search;

  type TTokenKind = (tok_Undefined,
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

                   tok_draw

                   );

type
  TLexer = record
    FCurChar : PAnsiChar;

    FTokenKind : TTokenKind;
    FToken : string;

    procedure Initialize(Text : PAnsiChar);
    procedure Advance;
    function  NextChar : PAnsiChar;
    procedure SkipWhiteSpace;

    procedure GetNumber;
    procedure GetNextToken;
    procedure GetString;
    end;

procedure DisplayID(CurrentName, CurrentVersion, Author : string);
procedure CheckIsReady(var PVS_Search : TSearch);
procedure Initialize_Engine(var PVS_Search : TSearch);
procedure New_Game(var Board : TBoard; var GameMoveList : TGameMoveList; var PVS_Search : TSearch);

procedure SetupPositionFromFen(var Board : TBoard; var GameMoveList : TGameMoveList; var S : PAnsiChar);
procedure SetupPositionFromStart(var Board : TBoard; var GameMoveList : TGameMoveList);
procedure SetupTestPosition(var Board : TBoard; var GameMoveList : TGameMoveList);
function MakeMoves(var Board : TBoard; var GameMoveList : TGameMoveList; S : PAnsiChar) : boolean;

procedure DrawBoard(Board : TBoard);
procedure HaltSearch(var PVS_Search : TSearch);
procedure ShutDownSearch(var PVS_Search : TSearch);


implementation

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

procedure TLexer.GetNextToken;
  var
    Start : PAnsiChar;
    S : AnsiString;
    Str : string;

  begin

  if Char(FCurChar^).IsWhiteSpace then
    SkipWhiteSpace;

  Start := FCurChar;

  if AnsiChar(FCurChar^) in ['0'..'9'] then
    begin
    GetNumber;
    exit;
    end;

  while AnsiChar(FCurChar^) in ['a'..'z', 'A'..'Z', '_'] do
    Advance;

  SetString(S, Start, FCurChar - Start);
  Str := ToLower(S);

  if Str = 'uci' then
    begin
    FTokenKind := tok_uci;
    FToken := 'uci';
    exit;
    end;

  if Str = 'debug' then
    begin
    FTokenKind := tok_debug;
    FToken := 'debug';
    exit;
    end;

  if Str = 'isready' then
    begin
    FTokenKind := tok_isready;
    FToken := 'isready';
    exit;
    end;

  if Str = 'ucinewgame' then
    begin
    FTokenKind := tok_ucinewgame;
    FToken := 'ucinewgame';
    exit;
    end;

  if Str = 'stop' then
    begin
    FTokenKind := tok_stop;
    FToken := 'stop';
    exit;
    end;

  if Str = 'testpos' then
    begin
    FTokenKind := tok_testpos;
    FToken := 'test';
    exit;
    end;

  if (Str = 'quit') or (S = 'q') then
    begin
    FTokenKind := tok_quit;
    FToken := 'quit';
    exit;
    end;

  if Str = 'ponderhit' then
    begin
    FTokenKind := tok_ponderhit;
    FToken := 'ponderhit';
    exit;
    end;

  if Str = 'position' then
    begin
    FTokenKind := tok_position;
    FToken := 'position';
    exit;
    end;

  if Str = 'fen' then
    begin
    FTokenKind := tok_fen;
    FToken := 'fen';
    Advance;
    exit;
    end;

  if Str = 'startpos' then
    begin
    FTokenKind := tok_startpos;
    FToken := 'startpos';
    Advance;
    exit;
    end;

  if Str = 'moves' then
    begin
    FTokenKind := tok_moves;
    FToken := 'moves';
    Advance;
    exit;
    end;

  if Str = 'go' then
    begin
    FTokenKind := tok_go;
    FToken := 'go';
    exit;
    end;

  if Str = 'searchmoves' then
    begin
    FTokenKind := tok_searchmoves;
    FToken := 'searchmoves';
    exit;
    end;

  if Str = 'wtime' then
    begin
    FTokenKind := tok_wtime;
    FToken := 'wtime';
    exit;
    end;

  if Str = 'btime' then
    begin
    FTokenKind := tok_btime;
    FToken := 'btime';
    exit;
    end;

  if Str = 'winc' then
    begin
    FTokenKind := tok_winc;
    FToken := 'winc';
    exit;
    end;

  if Str = 'binc' then
    begin
    FTokenKind := tok_binc;
    FToken := 'binc';
    exit;
    end;

  if Str = 'movestogo' then
    begin
    FTokenKind := tok_movestogo;
    FToken := 'movestogo';
    exit;
    end;

  if Str = 'depth' then
    begin
    FTokenKind := tok_depth;
    FToken := 'depth';
    exit;
    end;

  if Str = 'nodes' then
    begin
    FTokenKind := tok_nodes;
    FToken := 'nodes';
    exit;
    end;

  if Str = 'mate' then
    begin
    FTokenKind := tok_mate;
    FToken := 'mate';
    exit;
    end;

  if Str = 'movetime' then
    begin
    FTokenKind := tok_movetime;
    FToken := 'movetime';
    exit;
    end;

  if Str = 'infinite' then
    begin
    FTokenKind := tok_infinite;
    FToken := 'infinite';
    exit;
    end;

  if (Str = 'draw') or (Str = 'd') then
    begin
    FTokenKind := tok_draw;
    FToken := 'draw';
    exit;
    end;

  if Str = 'setoption' then
    begin
    FTokenKind := tok_setoption;
    FToken := 'setoption';
    exit;
    end;

  if Str = 'name' then
    begin
    FTokenKind := tok_name;
    FToken := 'name';
    exit;
    end;

  if Str = 'value' then
    begin
    FTokenKind := tok_value;
    FToken := 'value';
    exit;
    end;

  if Str = 'hash' then
    begin
    FTokenKind := tok_Hash;
    FToken := 'hash';
    exit;
    end;

  if Str = 'clear' then
    begin
    FTokenKind := tok_clearHash;
    FToken := 'clear hash';
    exit;
    end;

  if Str = 'threads' then
    begin
    FTokenKind := tok_threads;
    FToken := 'threads';
    exit;
    end;

  if Str = 'ownbook' then
    begin
    FTokenKind := tok_ownBook;
    FToken := 'ownBook';
    exit;
    end;

  if Str = 'false' then
    begin
    FTokenKind := tok_false;
    FToken := 'false';
    exit;
    end;

  if Str = 'true' then
    begin
    FTokenKind := tok_true;
    FToken := 'true';
    exit;
    end;

  if Str = 'uci_engineabout' then
    begin
    FTokenKind := tok_EngineAbout;
    FToken := 'UCI_EngineAbout';
    exit;
    end;

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


procedure DisplayID(CurrentName, CurrentVersion, Author : string);      // response to 'uci'
  begin
  WriteLn(output, AnsiString(' '));

  WriteLn(output, AnsiString('id name ' + CurrentName + ' ' + CurrentVersion));
  WriteLn(output, AnsiString('id author ' + Author));

  WriteLn(output, AnsiString(' '));

  WriteLn(output, AnsiString('option name Hash type spin default 64 min 1 max 256'));
  WriteLn(output, AnsiString('option name Clear Hash type button'));
  WriteLn(output, AnsiString('option name Threads type spin default 1 min 1 max 16'));
  WriteLn(output, AnsiString('option name OwnBook type check default false'));
  WriteLn(output, AnsiString('option name UCI_EngineAbout type string default ' + CurrentName + ' ' + CurrentVersion + ' by ' + Author));

  WriteLn(output, AnsiString(' '));

  WriteLn(output, AnsiString('uciok'));
  flush(output);
  end;


procedure Initialize_Engine(var PVS_Search : TSearch);
  begin
  if assigned(PVS_Search) = false then
    begin
    PVS_Search := TSearch.Create;

    // set default values

    PVS_Search.ThreadCount := 1;
    PVS_Search.UseOwnBook := false;
    if PVS_Search.TransTable.TableSize <> 64 * 65_536 then
      PVS_Search.TransTable.SetTableSize(64);
    end;
  end;


procedure CheckIsReady(var PVS_Search : TSearch);
  begin
  if assigned(PVS_Search) = false then
    Initialize_Engine(PVS_Search);

  WriteLn(output, AnsiString('readyok'));      // response to 'isready'
  flush(output);
  end;


procedure New_Game(var Board : TBoard; var GameMoveList : TGameMoveList; var PVS_Search : TSearch);  // response to 'ucinewgame'
  begin
  if assigned(PVS_Search) = false then
    Initialize_Engine(PVS_Search);

  PVS_Search.NewGame;
  Board.Reset;
  GameMoveList.Clear;
  end;


procedure SetupPositionFromFen(var Board : TBoard; var GameMoveList : TGameMoveList; var S : PAnsiChar);
  var
    Fen, S1 : Ansistring;
    LengthFen : integer;

  begin
  S1 := ToLower(S);
  LengthFen := pos('moves', S1) - 2;
  if LengthFen > 0 then
    begin
    Fen := Leftstr(S, LengthFen);
    S := S + LengthFen + 1;
    end
   else
    begin
    Fen := S;
    S := S + Length(Fen) + 1;
    end;

  Board.Reset;                 // Includes a check to see if board initialized - and initializes the board if not.
  BoardFromFEN(Fen, Board);    // Initialization should already be done when uci command sent on engine startup.
  GameMoveList.Clear;
  end;


procedure SetupPositionFromStart(var Board : TBoard; var GameMoveList : TGameMoveList);
  begin
  Board.Reset;
  GameMoveList.Clear;
  end;


procedure SetupTestPosition(var Board : TBoard; var GameMoveList : TGameMoveList);

// test > position fen 2r2rk1/1ppq2b1/1n6/1N1Ppp2/p7/Pn2BP2/1PQ1BP2/3RK2R w K - 0 23

  begin
  Board.Reset;
  BoardFromFEN('2r2rk1/1ppq2b1/1n6/1N1Ppp2/p7/Pn2BP2/1PQ1BP2/3RK2R w K - 0 23', Board);
  GameMoveList.Clear;
  end;


function MakeMoves(var Board : TBoard; var GameMoveList : TGameMoveList; S : PAnsiChar) : boolean;  // if sucessful then returns true, if error then returns false
  var
    Moves_text : TStringList;
    movestr : string;
    i, FileNo, Rank, source, dest : integer;
    Piece, CapturedPiece, PromotionPiece, epCell : integer;
    ValidDest, Move: UInt64;

  begin
  result := true;

  // <move1> .... <movei>

  Moves_text := TStringList.Create;

    try
    Moves_text.StrictDelimiter := false;
    Moves_text.DelimitedText := S;

    for i := 0 to Moves_Text.Count - 1 do
      begin
      movestr := Moves_text[i];

      FileNo :=  Ord(Char(movestr[1])) - Ord(Char('a'));
      Rank := StrToInt(movestr[2]) - 1;
      Source := 8*(7-Rank) + FileNo;

      FileNo :=  Ord(Char(movestr[3])) - Ord(Char('a'));
      Rank := StrToInt(movestr[4]) - 1;
      Dest := 8*(7-Rank) + FileNo;

      ValidDest := Board.GetCellValidMoves(Board.ToPlay, Source);
      if GetBit(ValidDest, Dest) = false then
        exit(false);

      Piece := Board.GetPiece_asm(Source);
      CapturedPiece := Board.GetPiece_asm(Dest);

      PromotionPiece := 0;
      if (Piece = pawn) and ((Rank = 7) or (Rank = 0)) and (Char(movestr[5]) <> ' ') then
        begin
        if (Char(movestr[5]) = 'q') or (Char(movestr[5]) = 'Q') then
          PromotionPiece := 5;
        if (Char(movestr[5]) = 'r') or (Char(movestr[5]) = 'R') then
          PromotionPiece := 4;
        if (Char(movestr[5]) = 'b') or (Char(movestr[5]) = 'B') then
          PromotionPiece := 3;
        if (Char(movestr[5]) = 'n') or (Char(movestr[5]) = 'N') then
          PromotionPiece := 2;
        end;

      if Piece = pawn then   // handle enpassant capture
        begin
        if (Dest mod 8 <> Source mod 8) and (CapturedPiece = 0) then
          CapturedPiece := Pawn;
        end;

      Move := 0;
      Move.SetSource(Source);
      Move.SetDest(Dest);
      Move.SetPiece(Piece);
      Move.SetCapturedPiece(CapturedPiece);
      Move.SetPromotionPiece(PromotionPiece);

      Move.SetHalfMoveCount(Board.HalfCount);
      Move.SetCastleFlags(Board.MovedPieces);

      if Board.Enpassant <> 0 then
        begin
        epCell := GetLowBit_Alt(Board.Enpassant);
        Move.SetEnpassantCell(epCell);
        end;

      GameMoveList.AddMove(Move);
      Board.MakeMove(Move);
      end;

    finally
    Moves_text.Free;
    end;
  end;


procedure HaltSearch(var PVS_Search : TSearch);
  begin
  PVS_Search.StopSearch;
  end;


procedure ShutDownSearch(var PVS_Search : TSearch);
  begin
  if assigned(PVS_Search) = true then
    begin
    PVS_Search.Shutdown;
    PVS_Search.Free;
    end;
  end;


procedure DrawBoard(Board : TBoard);
  var
    line : string;
    cell, RankNo, FileNo : integer;
    piece, color, t : integer;
    tempstr, Fen : string;

  begin
  WriteLn(output, ' ');
  WriteLn(output, '   +---+---+---+---+---+---+---+---+  ');

  line := ' ';

  RankNo := 0;
  FileNo := 0;

  while rankNo <= 7 do
    begin
    cell := RankNo*8 + FileNo;
    piece := Board.GetPiece_asm(cell);
    color := Board.GetColor(cell);
    if color = none then
      color := 0;
    t := color * 6 + piece;

    if FileNo = 0 then
      line := ' ' + IntToStr(8 - RankNo);

      case t of
       0 : tempstr := ' ';
       1 : tempstr := 'P';
       2 : tempstr := 'N';
       3 : tempstr := 'B';
       4 : tempstr := 'R';
       5 : tempstr := 'Q';
       6 : tempstr := 'K';
       7 : tempstr := 'p';
       8 : tempstr := 'n';
       9 : tempstr := 'b';
      10 : tempstr := 'r';
      11 : tempstr := 'q';
      12 : tempstr := 'k';
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

      WriteLn(output, line);
      WriteLn(output, '   +---+---+---+---+---+---+---+---+  ');
      FileNo := 0;
      inc(RankNo);
      line := ' ';
      end;
    end;

  WriteLn(output, '     a   b   c   d   e   f   g   h    ');
  WriteLn(output, ' ');
  BoardToFen(FEN, Board);
  WriteLn(output, ' Fen: ' + Fen);
  WriteLn(output, ' ');
  flush(output);
  end;


//  UCI Interface

//  GUI To Engine

//        uci	                                       Display engine name and author name
//        debug	                                     Display search statistics
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
//        quit	                                     Exit program


//  Engine To GUI

//        id name / author	                         Display engine name and author name
//        uciok	                                     Ready to receive commands
//        readyok	                                   Ready to receive position
//        bestmove	                                 Return best move
//        ponder	                                   Return opponents apparent best move, currently not supported

//        copyprotection	                           not implemented, Yakka is not copy-protected
//        registration	                             not implemented, Yakka does not require registration

//        info                                       Update each depth
//             multipv                               not yet supported
//             depth  / {seldepth}                   "seldepth" is the furthest it has looked ahead in any variation
//             score cp
//             nodes
//             nps
//             hashfull                              Display TT utilization
//             time
//             pv

//        info string                                Display information

//        option name ownbook                        Use imbedded ownbook true/false
//        option name uci_engineabout                Display engine name, author name, {and website link}
//        option name uci_analysemode                Analysemode true : set OwnBook to false. Analysemode false = no action

//        option name ponder                         Pondering currently not supported

//        option name nalimovpath nalimovcache       Nalimov tablebases not implemented
//        option name uci_shredderbasespath          Shredder tablebases not implemented

//        option name uci_showcurrline	             Displaying currently analyzed line not implemented
//        option name uci_showrefutations            Displaying refutations of candidate moves not implemented
//        option name uci_opponent                   Yakka does not adjust its play based on opponent
//        option name uci_setpositionvalue           Setting the value of a position (for analysis) not supported


end.
