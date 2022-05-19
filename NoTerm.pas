{$INCLUDE Switches.inc}
unit NoTerm;

interface

uses
  ScreenTools, Protocol, Messg, LCLIntf, LCLType, dateutils, Platform,
  SysUtils, Classes, Graphics, Controls, Forms, ButtonB, DrawDlg;

type
  TRunMode = (rmStop, rmStopped, rmRunning, rmQuit);

  TNoTermDlg = class(TDrawDlg)
    QuitBtn: TButtonB;
    GoBtn: TButtonB;
    procedure GoBtnClick(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    procedure Client(Command, Player: Integer; var Data);
  private
    Me: Integer;
    Active: Integer;
    ToldAlive: Integer;
    Round: Integer;
    LastShowYearTime: TDateTime;
    LastShowTurnChange: TDateTime;
    LastNewTurn: TDateTime;
    TurnTime: Extended;
    TotalStatTime: Extended;
    G: TNewGameData;
    Server: TServerCall;
    Shade: TBitmap;
    State: TBitmap;
    WinStat: array [0 .. nPl - 1] of Integer;
    ExtStat: array [0 .. nPl - 1] of Integer;
    AloneStat: array [0 .. nPl - 1] of Integer;
    DisallowShowActive: array [0 .. nPl - 1] of Boolean;
    TimeStat: array [0 .. nPl - 1] of Extended;
    Mode: TRunMode;
    procedure NewStat;
    procedure EndPlaying;
    procedure ShowActive(P: Integer; Active: Boolean);
    procedure ShowYear;
  end;

var
  NoTermDlg: TNoTermDlg;

procedure Client(Command, Player: Integer; var Data); stdcall;


implementation

uses
  GameServer, Log;

{$R *.lfm}

const
  UpdateInterval = 0.1; // seconds
  ShowActiveThreshold = 0.05; // seconds

  nPlOffered = 9;
  x0Brain = 109 + 48 + 23;
  y0Brain = 124 + 48 + 7 + 16;
  dxBrain = 128;
  dyBrain = 128;
  xBrain: array [0 .. nPlOffered - 1] of Integer = (x0Brain, x0Brain,
    x0Brain + dxBrain, x0Brain + dxBrain, x0Brain + dxBrain, x0Brain,
    x0Brain - dxBrain, x0Brain - dxBrain, x0Brain - dxBrain);
  yBrain: array [0 .. nPlOffered - 1] of Integer = (y0Brain, y0Brain - dyBrain,
    y0Brain - dyBrain, y0Brain, y0Brain + dyBrain, y0Brain + dyBrain,
    y0Brain + dyBrain, y0Brain, y0Brain - dyBrain);
  xActive: array [0 .. nPlOffered - 1] of Integer = (0, 0, 36, 51, 36, 0,
    -36, -51, -36);
  yActive: array [0 .. nPlOffered - 1] of Integer = (0, -51, -36, 0, 36, 51,
    36, 0, -36);

var
  FormsCreated: Boolean;

procedure TNoTermDlg.FormCreate(Sender: TObject);
begin
  Left := Screen.Width - Width - 8;
  Top := 8;
  Caption := Phrases.Lookup('AIT');
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(UniFont[ftSmall]);
  TitleHeight := 36;
  InitButtons;
  LastShowYearTime := 0;
end;

procedure TNoTermDlg.NewStat;
begin
  Round := 0;
  FillChar(WinStat, SizeOf(WinStat), 0);
  FillChar(ExtStat, SizeOf(ExtStat), 0);
  FillChar(AloneStat, SizeOf(AloneStat), 0);
  FillChar(TimeStat, SizeOf(TimeStat), 0);
  TotalStatTime := 0;
  Mode := rmStop;
end;

procedure TNoTermDlg.EndPlaying;
var
  EndCommand: Integer;
begin
  NewStat;
  if G.RO[Me].Turn > 0 then
    with MessgDlg do
    begin
      MessgText := Phrases.Lookup('ENDTOUR');
      Kind := mkYesNo;
      ShowModal;
      if ModalResult = mrIgnore then
        EndCommand := sResign
      else
        EndCommand := sBreak;
    end
  else
    EndCommand := sResign;
  Server(EndCommand, Me, 0, nil^);
end;

procedure TNoTermDlg.ShowActive(P: Integer; Active: Boolean);
begin
  if P < nPlOffered then
    Sprite(Canvas, HGrSystem, x0Brain + 28 + xActive[P],
      y0Brain + 28 + yActive[P], 8, 8, 81 + 9 * Byte(Active), 16);
end;

procedure TNoTermDlg.ShowYear;
begin
  Fill(State.Canvas, 0, 0, 192, 20, 64, 287 + 138);
  RisedTextOut(State.Canvas, 0, 0, Format(Phrases.Lookup('AIT_ROUND'), [Round])
    + ' ' + TurnToString(G.RO[Me].Turn));
  BitBltCanvas(Canvas, 64, 287 + 138, 192, 20, State.Canvas, 0, 0);
end;

procedure TNoTermDlg.Client(Command, Player: Integer; var Data);
var
  I, X, Y, P: Integer;
  ActiveDuration: Extended;
  ShipComplete: Boolean;
  R: TRect;
  nowt: TDateTime;
begin
  case Command of
    cDebugMessage:
      LogDlg.Add(Player, G.RO[0].Turn, PChar(@Data));

    cInitModule:
      begin
        Server := TInitModuleData(Data).Server;
        TInitModuleData(Data).Flags := aiThreaded;
        Shade := TBitmap.Create;
        Shade.SetSize(64, 64);
        for X := 0 to 63 do
          for Y := 0 to 63 do
            if Odd(X + Y) then
              Shade.Canvas.Pixels[X, Y] := $FFFFFF
            else
              Shade.Canvas.Pixels[X, Y] := $000000;
        State := TBitmap.Create;
        State.SetSize(192, 20);
        State.Canvas.Brush.Style := bsClear;
        State.Canvas.Font.Assign(UniFont[ftSmall]);
        NewStat;
      end;

    cReleaseModule:
      begin
        FreeAndNil(Shade);
        FreeAndNil(State);
      end;

    cNewGame, cLoadGame:
      begin
        Inc(Round);
        if Mode = rmRunning then
        begin
          Invalidate;
          Update;
        end
        else
          Show;
        G := TNewGameData(Data);
        LogDlg.mSlot.Visible := False;
        LogDlg.Host := nil;
        ToldAlive := G.RO[Me].Alive;
        Active := -1;
        FillChar(DisallowShowActive, SizeOf(DisallowShowActive), 0); // false
        LastShowTurnChange := 0;
        LastNewTurn := 0;
        TurnTime := 1.0;
      end;

    cBreakGame:
      begin
        LogDlg.List.Clear;
        if Mode <> rmRunning then
        begin
          if LogDlg.Visible then
            LogDlg.Close;
          Close;
        end;
      end;

    cTurn, cResume, cContinue:
      begin
        Me := Player;
        if Active >= 0 then
        begin
          ShowActive(Active, False);
          Active := -1;
        end; // should not happen

        nowt := NowPrecise;
        if SecondOf(nowt - LastShowYearTime) >= UpdateInterval then
        begin
          ShowYear;
          LastShowYearTime := nowt;
        end;
        TurnTime := SecondOf(nowt - LastNewTurn);
        LastNewTurn := nowt;
        if (G.RO[Me].Alive <> ToldAlive) then
        begin
          for P := 1 to nPlOffered - 1 do
            if 1 shl P and (G.RO[Me].Alive xor ToldAlive) <> 0 then
            begin
              R := Rect(xBrain[P], yBrain[P] - 16, xBrain[P] + 64,
                yBrain[P] - 16 + 64);
              InvalidateRect(Handle, @R, False);
            end;
          ToldAlive := G.RO[Me].Alive;
        end;
        Application.ProcessMessages;
        if Mode = rmQuit then
          EndPlaying
        else if G.RO[Me].Happened and phGameEnd <> 0 then
        begin // game ended, update statistics
          for P := 1 to nPlOffered - 1 do
            if Assigned(PlayersBrain[P]) then
              if 1 shl P and G.RO[Me].Alive = 0 then
                Inc(ExtStat[P]) // extinct
              else if G.RO[Me].Alive = 1 shl P then
                Inc(AloneStat[P]) // only player alive
              else
              begin // alive but not alone -- check colony ship
                ShipComplete := True;
                for I := 0 to nShipPart - 1 do
                  if G.RO[Me].Ship[P].Parts[I] < ShipNeed[I] then
                    ShipComplete := False;
                if ShipComplete then
                  Inc(WinStat[P]);
              end;
          if Mode = rmRunning then
            Server(sNextRound, Me, 0, nil^);
        end
        else if Mode = rmRunning then
          Server(sTurn, Me, 0, nil^);
        if Mode = rmStop then
        begin
          GoBtn.ButtonIndex := 22;
          Mode := rmStopped;
        end;
      end;

    cShowTurnChange:
      begin
        nowt := NowPrecise;
        if Active >= 0 then
        begin
          ActiveDuration := SecondOf(nowt - LastShowTurnChange);
          TimeStat[Active] := TimeStat[Active] + ActiveDuration;
          TotalStatTime := TotalStatTime + ActiveDuration;
          if not DisallowShowActive[Active] then
            ShowActive(Active, False);
          DisallowShowActive[Active] := (ActiveDuration < TurnTime * 0.25) and
            (ActiveDuration < ShowActiveThreshold);
        end;
        LastShowTurnChange := nowt;

        Active := Integer(Data);
        if (Active >= 0) and not DisallowShowActive[Active] then
          ShowActive(Active, True);
      end;
  end;
end;

procedure TNoTermDlg.GoBtnClick(Sender: TObject);
begin
  if Mode = rmRunning then
    Mode := rmStop
  else if Mode = rmStopped then
  begin
    Mode := rmRunning;
    GoBtn.ButtonIndex := 23;
    GoBtn.Update;
    Server(sTurn, Me, 0, nil^);
  end;
end;

procedure TNoTermDlg.QuitBtnClick(Sender: TObject);
begin
  if Mode = rmStopped then EndPlaying
    else Mode := rmQuit;
end;

procedure TNoTermDlg.FormPaint(Sender: TObject);
var
  I, TimeShare: Integer;
begin
  Fill(Canvas, 3, 3, ClientWidth - 6, ClientHeight - 6, 0, 0);
  Frame(Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, $000000, $000000);
  Frame(Canvas, 1, 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Canvas, 2, 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Corner(Canvas, 1, 1, 0, MainTexture);
  Corner(Canvas, ClientWidth - 9, 1, 1, MainTexture);
  Corner(Canvas, 1, ClientHeight - 9, 2, MainTexture);
  Corner(Canvas, ClientWidth - 9, ClientHeight - 9, 3, MainTexture);
  Canvas.Font.Assign(UniFont[ftCaption]);
  RisedTextOut(Canvas, (ClientWidth - BiColorTextWidth(Canvas, Caption)) div 2,
    7, Caption);
  Canvas.Font.Assign(UniFont[ftSmall]);
  for I := 1 to nPlOffered - 1 do
    if Assigned(PlayersBrain[I]) then
    begin
      Frame(Canvas, xBrain[I] - 24, yBrain[I] - 8 - 16, xBrain[I] - 24 + 111,
        yBrain[I] - 8 - 16 + 111, MainTexture.ColorBevelShade,
        MainTexture.ColorBevelShade);
      FrameImage(Canvas, PlayersBrain[I].Picture, xBrain[I],
        yBrain[I] - 16, 64, 64, 0, 0);
      if 1 shl I and G.RO[Me].Alive = 0 then
        BitBltCanvas(Canvas, xBrain[I], yBrain[I] - 16, 64, 64,
          Shade.Canvas, 0, 0, SRCAND);
      Sprite(Canvas, HGrSystem, xBrain[I] + 30 - 14, yBrain[I] + 53, 14,
        14, 1, 316);
      RisedTextOut(Canvas, xBrain[I] + 30 - 16 - BiColorTextWidth(Canvas,
        IntToStr(WinStat[I])), yBrain[I] + 51, IntToStr(WinStat[I]));
      Sprite(Canvas, HGrSystem, xBrain[I] + 34, yBrain[I] + 53, 14, 14,
        1 + 15, 316);
      RisedTextOut(Canvas, xBrain[I] + 34 + 16, yBrain[I] + 51,
        IntToStr(AloneStat[I]));
      Sprite(Canvas, HGrSystem, xBrain[I] + 30 - 14, yBrain[I] + 53 + 16, 14,
        14, 1 + 30, 316);
      RisedTextOut(Canvas, xBrain[I] + 30 - 16 - BiColorTextWidth(Canvas,
        IntToStr(ExtStat[I])), yBrain[I] + 51 + 16, IntToStr(ExtStat[I]));
      Sprite(Canvas, HGrSystem, xBrain[I] + 34, yBrain[I] + 53 + 16, 14, 14,
        1 + 45, 316);
      if TotalStatTime > 0 then
      begin
        TimeShare := trunc(TimeStat[I] / TotalStatTime * 100 + 0.5);
        RisedTextOut(Canvas, xBrain[I] + 34 + 16, yBrain[I] + 51 + 16,
          IntToStr(TimeShare) + '%');
      end;
      ShowActive(I, I = Active);
    end;
  Sprite(Canvas, HGrSystem2, x0Brain + 32 - 20, y0Brain + 32 - 20, 40,
    40, 115, 1);
  ShowYear;
  BtnFrame(Canvas, GoBtn.BoundsRect, MainTexture);
  BtnFrame(Canvas, QuitBtn.BoundsRect, MainTexture);
  // BtnFrame(Canvas,StatBtn.BoundsRect,MainTexture);
end;

procedure Client(Command, Player: Integer; var Data);
begin
  if not FormsCreated then
  begin
    FormsCreated := True;
    Application.CreateForm(TNoTermDlg, NoTermDlg);
  end;
  NoTermDlg.Client(Command, Player, Data);
end;

procedure TNoTermDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (char(Key) = 'M') and (ssCtrl in Shift) then
    if LogDlg.Visible then
      LogDlg.Close
    else
      LogDlg.Show;
end;

initialization

FormsCreated := False;

end.
