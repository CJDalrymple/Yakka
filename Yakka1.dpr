//  The MIT License (MIT)

//  Chess Engine Yakka v1.0
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
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.


program Yakka1;

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
  Eval in 'Eval.pas',
  GameDef in 'GameDef.pas',
  Search in 'Search.pas',
  EndGame in 'EndGame.pas',
  UCI in 'UCI.pas',
  OpenBook in 'OpenBook.pas';

const
  CurrentName = 'Yakka';
  CurrentVersion = 'v1.0';
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
    MovesToGoFlag : boolean;
    Buff: array[0..8191] of AnsiChar;

  begin
  ReportMemoryLeaksOnShutDown := true;
  flag := SetConsoleCtrlHandler(@ConsoleEventProc, true);

  TTextRec(Input).BufSize := SizeOf(Buff);
  TTextRec(Input).BufPtr := @Buff;

    try
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

    flush(output);

    //WriteLn('To quit, type Q followed by ENTER');

    Quit := False;

    while not Quit do
      begin
      ReadLn(input, S);

      Lexer.Initialize(PAnsiChar(S));
      Lexer.GetNextToken;

        case
        Lexer.FTokenKind of

          tok_uci : DisplayID(CurrentName, CurrentVersion, Author);

          tok_isready  : Initialize_Engine(PVS_Search);

          tok_ucinewgame : New_Game(Board, GameMoveList, PVS_Search);

          tok_position : begin                        // [position fen <fenstring> | startpos]  moves <move1> .... <movei>
                         Lexer.GetNextToken;
                         if Lexer.FTokenKind = tok_fen then
                           SetUpPositionFromFen(Board, GameMoveList, Lexer.FCurChar)      // TODO extract fen if S contains moves

                          else if Lexer.FTokenKind = tok_startpos then
                           SetUpPositionFromStart(Board, GameMoveList)
                          else
                           Writeln(AnsiString('Unknown Command'));

                         Lexer.GetNextToken;
                         if Lexer.FTokenKind = tok_moves then
                           if MakeMoves(Board, GameMoveList, Lexer.FCurChar) = false then
                             Writeln(AnsiString('Illegal Move(s)'));
                         end;

          tok_go :  begin
                    if assigned(PVS_Search) = false then
                      PVS_Search := TSearch.Create;

                    Lexer.GetNextToken;

                    if Lexer.FTokenKind = tok_depth then
                      begin
                      Lexer.GetNextToken;
                      if Lexer.FTokenKind = tok_number then
                        begin
                        IntegerNumber := strToInt(Lexer.fToken);

                        PVS_Search.DepthLimit := IntegerNumber;
                        PVS_Search.TimeLimit := UInt64(18_446_744_073_709_551_615);    // UInt64.MaxValue
                        PVS_Search.NodeLimit := UInt64(18_446_744_073_709_551_615);    // UInt64.MaxValue

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
                        PVS_Search.TimeLimit := UInt64(18_446_744_073_709_551_615);    // UInt64.MaxValue
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
                        PVS_Search.NodeLimit := UInt64(18_446_744_073_709_551_615);    // UInt64.MaxValue

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
                      PVS_Search.TimeLimit := UInt64(18_446_744_073_709_551_615);    // UInt64.MaxValue
                      PVS_Search.NodeLimit := UInt64(18_446_744_073_709_551_615);    // UInt64.MaxValue

                      PVS_Search.StartInBackGround(Board, GameMoveList);
                      end;

                    if Lexer.FTokenKind in [tok_wtime, tok_btime, tok_winc, tok_binc, tok_movestogo] then
                      begin
                      MovesToGoFlag := false;

                      PVS_Search.TimeRemaining_White := 0;
                      PVS_Search.TimeRemaining_Black := 0;
                      PVS_Search.TimeIncrementPerMove_White := 0;
                      PVS_Search.TimeIncrementPerMove_Black := 0;
                      PVS_Search.MovesToGo := 0;

                        repeat
                        tempToken := Lexer.FTokenKind;
                        Lexer.GetNextToken;
                        if Lexer.FTokenKind = tok_number then
                          begin
                          IntegerNumber := strToInt(Lexer.FToken);

                            case tempToken of
                            tok_wtime     : PVS_Search.TimeRemaining_White := IntegerNumber;
                            tok_btime     : PVS_Search.TimeRemaining_Black := IntegerNumber;
                            tok_winc      : PVS_Search.TimeIncrementPerMove_White := IntegerNumber;
                            tok_binc      : PVS_Search.TimeIncrementPerMove_Black := IntegerNumber;
                            tok_movesToGo : begin
                                            PVS_Search.MovesToGo := IntegerNumber;
                                            MovesToGoFlag := true;
                                            end;
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

                      if MovesToGoFlag = false then
                        begin
                        if Board.ToPlay = White then
                          PVS_Search.TimeLimit := AllocateTimeForMove_2A(PVS_Search.TimeRemaining_White, PVS_Search.TimeIncrementPerMove_White, Board.TurnNumber, Board)
                         else
                          PVS_Search.TimeLimit := AllocateTimeForMove_2A(PVS_Search.TimeRemaining_Black, PVS_Search.TimeIncrementPerMove_Black, Board.TurnNumber, Board);
                        end
                       else
                        PVS_Search.CalcTimeLimit(Board);

                      PVS_Search.NodeLimit := UInt64(18_446_744_073_709_551_615);    // UInt64.MaxValue

                      PVS_Search.StartInBackGround(Board, GameMoveList);
                      end;

                    if Lexer.FTokenKind = tok_ponder then
                      Writeln(output, AnsiString('ponder'));
                    end;

          tok_draw : DrawBoard(Board);

          tok_setoption : begin
                          if assigned(PVS_Search) = false then
                            PVS_Search := TSearch.Create;

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

                            if Lexer.FTokenKind = tok_Threads then              // option name Threads type spin default 6 min 1 max 16
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
                              if Lexer.FTokenKind = tok_true then
                                PVS_Search.UseOwnBook := true
                               else if Lexer.FTokenKind = tok_false then
                                PVS_Search.UseOwnBook := false;
                              end;

                            if Lexer.FTokenKind = tok_EngineAbout then          // option name UCI_EngineAbout type string default 'Yakka 1.0 by Christopher Crone'
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
