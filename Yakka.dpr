//  The MIT License (MIT)

//  Chess Engine Yakka
//  Copyright (c) 2025 Christopher Crone

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


program Yakka;

{$APPTYPE CONSOLE}

{$R *.res}

{$R *.dres}

uses
  Winapi.Windows,
  System.SysUtils,
  System.StrUtils,
  System.Math,
  Common in 'Common.pas',
  CPU_Info in 'CPU_Info.pas',
  EndGame in 'EndGame.pas',
  GameDef in 'GameDef.pas',
  OpenBook in 'OpenBook.pas',
  Search in 'Search.pas',
  UCI in 'UCI.pas',
  GameNet in 'GameNet.pas';

const
  CurrentName = 'Yakka';
  CurrentVersion = 'v1.2';
  VersionDate =   '8 Feb 2025';
  Author = 'Christopher Crone';

var
  Board : TBoard;
  GameMoveList : TGameMoveList;

  PVS_Search : TSearch;

  Quit: Boolean;
  Lexer : TLexer;
  S: Ansistring;

  flag : boolean;


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
                            PVS_Search.Free;
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
                            PVS_Search.Free;
                            end;

                          quit := true;
                          result := true;
                          end;

    CTRL_SHUTDOWN_EVENT:  begin
                          if assigned(PVS_Search) then
                            begin
                            PVS_Search.ShutDown;
                            PVS_Search.Free;
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
    IntegerNumber : Integer;
    tempToken : TTokenKind;

    TimeRemaining_White : UInt64;           // UCI:   wtime <x> white has x msec left on the clock
    TimeRemaining_Black : UInt64;           // UCI:   btime <x> black has x msec left on the clock
    TimeIncrementPerMove_White : UInt64;    // UCI:   winc <x> white time increment per move in msec if x>0
    TimeIncrementPerMove_Black : UInt64;    // UCI:   binc <x> black time increment per move in msec if x>0
    MovesToGo :  UInt64;                    // UCI:   movestogo <x> therre are x moves until the next time control if x>0 (only sent if x>0)

    Buff: array[0..8191] of AnsiChar;

  begin
  flag := SetConsoleCtrlHandler(@ConsoleEventProc, true);

  TTextRec(Input).BufSize := SizeOf(Buff);
  TTextRec(Input).BufPtr := @Buff;

  WriteLn(output, AnsiString(CurrentName + ' ' + CurrentVersion + ' by ' + Author));
  WriteLn(output, AnsiString(' '));

  if PopCnt_Supported = false then
    begin
    WriteLn(output, AnsiString('Warning : CPU doesn''t support PopCnt asm instruction'));
    WriteLn(output, AnsiString('Yakka will not run correctly on this CPU'));
    WriteLn(output, AnsiString('type ''quit'' to exit'));
    end;

  if BMI2_Supported = false then
    begin
    WriteLn(output, AnsiString('Warning : CPU doesn''t support BMI2 instructions'));
    WriteLn(output, AnsiString('PEXT and PDEP asm instructions are required for move generation'));
    WriteLn(output, AnsiString('Yakka will not run correctly on this CPU'));
    WriteLn(output, AnsiString('type ''quit'' to exit'));
    end;

  if AVX2_Supported = false then
    begin
    WriteLn(output, AnsiString('Warning : CPU doesn''t support AVX2 instructions'));
    WriteLn(output, AnsiString('AVX2 instructions are required for NNUE evaluation'));
    WriteLn(output, AnsiString('Yakka will not run correctly on this CPU'));
    WriteLn(output, AnsiString('type ''quit'' to exit'));
    end;

  //WriteLn('To quit, type Q followed by ENTER');

  flush(output);

    try
    Quit := False;

    while not Quit do
      begin
      ReadLn(input, S);

      if s <> '' then
        begin
        Lexer.Initialize(PAnsiChar(S));
        Lexer.GetNextToken;

          case
          Lexer.FTokenKind of

            tok_uci : DisplayID(CurrentName, CurrentVersion, Author);

            tok_isready  : CheckIsReady(PVS_Search);

            tok_ucinewgame : New_Game(Board, GameMoveList, PVS_Search);

            tok_position : begin                        // [position fen <fenstring> | startpos]  moves <move1> .... <movei>
                           Lexer.GetNextToken;
                           if Lexer.FTokenKind = tok_fen then
                             SetUpPositionFromFen(Board, GameMoveList, Lexer.FCurChar)      // TODO extract fen if S contains moves

                            else if Lexer.FTokenKind = tok_startpos then
                             SetUpPositionFromStart(Board, GameMoveList)

                            else if Lexer.FTokenKind = tok_testpos then
                             SetUpTestPosition(Board, GameMoveList)

                            else
                             Writeln(AnsiString('Unknown Command'));

                           Lexer.GetNextToken;
                           if Lexer.FTokenKind = tok_moves then
                             if MakeMoves(Board, GameMoveList, Lexer.FCurChar) = false then
                               Writeln(AnsiString('Illegal Move(s)'));
                           end;

            tok_go :  begin
                      if assigned(PVS_Search) = false then
                        Initialize_Engine(PVS_Search);

                      Lexer.GetNextToken;

                      if Lexer.FTokenKind = tok_depth then
                        begin
                        Lexer.GetNextToken;
                        if Lexer.FTokenKind = tok_number then
                          begin
                          IntegerNumber := strToInt(Lexer.fToken);

                          PVS_Search.DepthLimit := IntegerNumber;
                          PVS_Search.TimeLimit := TimeLimit_Max;
                          PVS_Search.SoftTimeLimit := false;
                          PVS_Search.NodeLimit := NodeLimit_Max;

                          PVS_Search.StartInBackGround(Board, GameMoveList);
                          end
                         else
                          begin
                          Writeln(output, AnsiString('Unknown Depth'));
                          flush(output);
                          end;
                        end;

                      if Lexer.FTokenKind = tok_nodes then
                        begin
                        Lexer.GetNextToken;
                        if Lexer.FTokenKind = tok_number then
                          begin
                          IntegerNumber := strToInt(Lexer.fToken);

                          PVS_Search.DepthLimit := MaxSearchPly;
                          PVS_Search.TimeLimit := TimeLimit_Max;
                          PVS_Search.SoftTimeLimit := false;
                          PVS_Search.NodeLimit := IntegerNumber;

                          PVS_Search.StartInBackGround(Board, GameMoveList);
                          end
                         else
                          begin
                          Writeln(output, AnsiString('Unknown Nodes'));
                          flush(output);
                          end;
                        end;

                      if Lexer.FTokenKind = tok_mate then
                        Writeln(AnsiString('mate'));

                      if Lexer.FTokenKind = tok_movetime then
                        begin
                        Lexer.GetNextToken;
                        if Lexer.FTokenKind = tok_number then
                          begin
                          IntegerNumber := strToInt(Lexer.fToken);

                          PVS_Search.DepthLimit := MaxSearchPly;
                          PVS_Search.TimeLimit := IntegerNumber;
                          PVS_Search.SoftTimeLimit := false;
                          PVS_Search.NodeLimit := NodeLimit_Max;

                          PVS_Search.StartInBackGround(Board, GameMoveList);
                          end
                         else
                          begin
                          Writeln(output, AnsiString('Unknown Time'));
                          flush(output)
                          end;
                        end;

                      if Lexer.FTokenKind = tok_infinite then
                        begin

                        PVS_Search.DepthLimit := MaxSearchPly;
                        PVS_Search.TimeLimit := TimeLimit_Max;
                        PVS_Search.SoftTimeLimit := false;
                        PVS_Search.NodeLimit := NodeLimit_Max;

                        PVS_Search.StartInBackGround(Board, GameMoveList);
                        end;

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
                            IntegerNumber := strToInt(Lexer.FToken);

                              case tempToken of
                              tok_wtime     : TimeRemaining_White := IntegerNumber;
                              tok_btime     : TimeRemaining_Black := IntegerNumber;
                              tok_winc      : TimeIncrementPerMove_White := IntegerNumber;
                              tok_binc      : TimeIncrementPerMove_Black := IntegerNumber;
                              tok_movesToGo : MovesToGo := max(IntegerNumber, 0);
                              end;

                            Lexer.GetNextToken;
                            end
                           else
                            begin
                            Writeln(output, AnsiString('Error : Number expected'));
                            flush(output);
                            end;
                          until Lexer.FTokenKind = tok_EOL;

                        PVS_Search.DepthLimit := MaxSearchPly;

                        if Board.ToPlay = White then
                          begin
                          PVS_Search.TimeLimit := TimeRemaining_White;
                          PVS_Search.SoftTimeLimit := true;
                          PVS_Search.BudgetTime := AllocateTimeForSearch(MovesToGo, TimeRemaining_White, TimeIncrementPerMove_White, Board.TurnNumber);
                          end
                         else
                          begin
                          PVS_Search.TimeLimit := TimeRemaining_Black;
                          PVS_Search.SoftTimeLimit := true;
                          PVS_Search.BudgetTime := AllocateTimeForSearch(MovesToGo, TimeRemaining_Black, TimeIncrementPerMove_Black, Board.TurnNumber);
                          end;

                        PVS_Search.NodeLimit := NodeLimit_Max;

                        PVS_Search.StartInBackGround(Board, GameMoveList);
                        end;

                      if Lexer.FTokenKind = tok_ponder then
                        Writeln(output, AnsiString('ponder'));
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
                                    begin
                                    IntegerNumber := strToInt(Lexer.FToken);
                                    IntegerNumber := max(min(IntegerNumber, 256), 1);
                                    PVS_Search.SetTransTableSize(IntegerNumber);
                                    end;
                                  end

                                else if Lexer.FTokenKind = tok_ClearHash then
                                  begin
                                  PVS_Search.ClearTranspositionTable;             // option name Clear Hash type button
                                  end;
                                end;

                              if Lexer.FTokenKind = tok_Threads then              // option name Threads type spin default 4 min 1 max 16
                                begin
                                Lexer.GetNextToken;
                                if Lexer.FTokenKind = tok_value then
                                  begin
                                  Lexer.GetNextToken;
                                  if Lexer.FTokenKind = tok_number then
                                    begin
                                    IntegerNumber := strToInt(Lexer.FToken);
                                    IntegerNumber := max(min(IntegerNumber, 16), 1);

                                    PVS_Search.SetThreadCount(IntegerNumber);
                                    end;
                                  end;
                                end;

                              if Lexer.FTokenKind = tok_OwnBook then              // option name OwnBook type check default true
                                begin
                                Lexer.GetNextToken;
                                if Lexer.FTokenKind = tok_value then
                                  begin
                                  Lexer.GetNextToken;
                                  if Lexer.FTokenKind = tok_true then
                                    PVS_Search.UseOwnBook := true
                                   else if Lexer.FTokenKind = tok_false then
                                    PVS_Search.UseOwnBook := false;
                                  end;
                                end;

                              if Lexer.FTokenKind = tok_EngineAbout then          // option name UCI_EngineAbout type string default "Current Name, Current Version by Author"
                                begin
                                Writeln(output, AnsiString(' '));
                                Writeln(output, AnsiString(CurrentName + ' ' + CurrentVersion + ' by ' + Author));
                                end;

                              end;
                            end;

            tok_stop : HaltSearch(PVS_Search);

            tok_quit : Quit := true;

           else
            begin
            Writeln(output, AnsiString('Unknown Command'));
            flush(output);
            end;
          end;

        end;
      end;

    finally
      if assigned(PVS_Search) then
        begin
        PVS_Search.ShutDown;
        PVS_Search.Free;
        end;
    end;
  end;


begin
  try
    ExecuteProgram;

  except
    on E: Exception do
      begin
      WriteLn('Program terminated due to an exception');
      Writeln(E.ClassName, ': ', E.Message);
      readln;

      // Set ExitCode <> 0 to flag error condition (by convention)
      // ExitCode := 1;
      end;

  end;

end.
