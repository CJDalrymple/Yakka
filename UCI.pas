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
    System.Classes, System.SysUtils, System.StrUtils, System.Character, System.Math, GameDef, GameNet, Common, CPU_Info, Search;

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
         tok_d);


  const  KeyWords : array[0..51] of AnsiString = (
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
        'd');

type
  TTag = array[0..15] of byte;

type
  TLexer = record
    FCurChar : PAnsiChar;

    FTokenKind : TTokenKind;
    FToken : Ansistring;

    procedure Initialize(Text : PAnsiChar);
    procedure Advance;                          inline;
    function  NextChar : PAnsiChar;             inline;
    procedure SkipWhiteSpace;                   inline;

    procedure GetNumber;
    procedure GetNextToken;
    procedure GetString;
    end;

procedure DisplayID(CurrentName, CurrentVersion, Author : Ansistring);
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
    Lookup : array[1..128] of integer =

        ( 0,  0,  0,  0,  0,  0,  0,  0,
         25,  4, 21,  0, 46, 30,  0, 25,
          3,  1,  0,  0,  5,  0, 15,  0,
          0,  0, 25,  4,  0,  1, 18,  0,
          0,-19, 15,  9,  3,  0,  0,  0,
          0,  0, 14, 10, 25, 16, 26, 15,
          8,  0,  0, 33,  0,  2,  8,  1,
         13,  0,  0,  0,  7,  0,  0, 23,
          3,  3, 10, 10, 16,  1, 43,  0,
          3, 20,  4,  0,  0, 20,  7,  0,
          0,  3,  7, 28,  0,-16, 23,  0,
          0,  0,  0,  0,  0,  0,  0,  0,
          0,  0, 29, 32,  0,  2,  0, 14,
         23,  0,  0, 15, 23,  2,  0,  0,
          0,  0,  0,  0,  0,  0,  0, 17,
          0,  0,  0,  0,  0,  0,  0,  0);

  var
    i, len, f1, f2 : integer;
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

  len := min(FCurChar - Start, high(Tag));          // Tag is max 15 characters = length of longest keyword
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
    f1 := 3 * (Tag[1] - 96) + len;                          // perfect hash to obtain tokenkind directly
    f2 := min((5 * (Tag[max(len-1, 1)] - 96) + len), 128);

    FTokenKind := TTokenKind(Lookup[f1] + Lookup[f2]);

    if TagMatch(Tag, Keywords[Ord(FTokenKind)]) = false then       // check for exact match
      FTokenKind := tok_Undefined;

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


procedure DisplayID(CurrentName, CurrentVersion, Author : Ansistring);      // response to 'uci'
  begin
  WriteLn(output, AnsiString(' '));

  WriteLn(output, AnsiString('id name ' + CurrentName + ' ' + CurrentVersion));
  WriteLn(output, AnsiString('id author ' + Author));

  WriteLn(output, AnsiString(' '));

  WriteLn(output, AnsiString('option name Threads type spin default 1 min 1 max 16'));
  WriteLn(output, AnsiString('option name Hash type spin default 64 min 1 max 256'));
  WriteLn(output, AnsiString('option name Clear Hash type button'));
  WriteLn(output, AnsiString('option name Ponder type check default false'));

  //  WriteLn(output, AnsiString('option name MultiPV type spin default 1 min 1 max 32'));
  //  WriteLn(output, AnsiString('option name UCI_ShowWDL type check default false'));

  WriteLn(output, AnsiString('option name OwnBook type check default false'));
  WriteLn(output, AnsiString('option name UCI_EngineAbout type string default ' + CurrentName + ' ' + CurrentVersion + ' by ' + Author));

  WriteLn(output, AnsiString(' '));

  if AVX512f_Supported = true then
    begin
    WriteLn(output, AnsiString('AVX512 supported by the CPU'));
    WriteLn(output, AnsiString(' '));
    end;

  WriteLn(output, AnsiString('uciok'));
  flush(output);
  end;


procedure WriteMessage(const msg : string);
  begin
  writeln(output, AnsiString(msg));
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

    PVS_Search.OnMessage := WriteMessage;
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
  EvalHashTable.Clear;
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
    FileNo, Rank, source, dest : integer;
    Piece, CapturedPiece, PromotionPiece, epCell : integer;
    Move: TMove;

  begin
  result := false;

  // <move1> .... <movei>

    repeat
      begin
      if Char(s^) = ' ' then
        inc(s)
       else
        begin
        FileNo := Ord(Char(s^)) - Ord(Char('a'));
        inc(s);
        Rank := Ord(Char(s^)) - Ord(Char('0')) - 1;
        inc(s);
        Source := 8*(7-Rank) + FileNo;

        FileNo := Ord(Char(s^)) - Ord(Char('a'));
        inc(s);
        Rank := Ord(Char(s^)) - Ord(Char('0')) - 1;
        inc(s);
        Dest := 8*(7-Rank) + FileNo;

        Piece := Board.GetPiece_asm(Source);
        CapturedPiece := Board.GetPiece_asm(Dest);

        PromotionPiece := 0;
        if (Piece = pawn) and ((Rank = 7) or (Rank = 0)) and (Char(s^) <> ' ') then
          begin
          if (Char(s^) = 'q') or (Char(Char(s^)) = 'Q') then
            PromotionPiece := 5;
          if (Char(s^) = 'r') or (Char(s^) = 'R') then
            PromotionPiece := 4;
          if (Char(s^) = 'b') or (Char(s^) = 'B') then
            PromotionPiece := 3;
          if (Char(s^) = 'n') or (Char(s^) = 'N') then
            PromotionPiece := 2;

          inc(s);
          end;

        if Piece = pawn then   // handle enpassant capture
          begin
          if (Dest mod 8 <> Source mod 8) and (CapturedPiece = 0) then
            CapturedPiece := Pawn;
          end;

        Move := 0;
        Move := UInt64(Source) or (UInt64(Dest) shl 6) or (UInt64(Piece) shl 12) or (UInt64(CapturedPiece) shl 16) or (UInt64(PromotionPiece) shl 20);

        Move.SetHalfMoveCount(Board.HalfCount);
        Move.SetCastleFlags(Board.MovedPieces);

        if Board.Enpassant <> 0 then
          begin
          epCell := GetLowBit_Alt(Board.Enpassant);
          Move.SetEnpassantCell(epCell);
          end;

        GameMoveList.AddMove(Move);
        Board.MakeMove(Move);

        result := true;
        end;
      end
    until (s^ = #13) or (s^ = #0);
  end;


procedure HaltSearch(var PVS_Search : TSearch);
  begin
  PVS_Search.Stop := true;

    repeat
    until PVS_Search.Searching = false;
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
    line : Ansistring;
    cell, RankNo, FileNo : integer;
    piece, color, t : integer;
    tempstr, Fen : Ansistring;

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
    t := color * 7 + piece;

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
//             multipv                               not yet supported
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

//        option name UCI_EngineAbout type string default 'Yakka vX.X by Christopher Crone'
//
//                  >   Display engine name, author name, {and website link}
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
//        option name UCI_ShowWDL type check default false
//
//                  >   Include wdl <win_prob> <draw_prob> <loss_prob> in the info line,
//                  >   Probabilities are in integer per thousand and always add up to 1000
//                  >   ToDo, currrently not supported

end.
