{$INCLUDE Switches.inc}
unit Diagram;

interface

uses
  BaseWin, LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  ButtonB, Menus;

type
  TDiaDlg = class(TFramedDlg)
    CloseBtn: TButtonB;
    ToggleBtn: TButtonB;
    Popup: TPopupMenu;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToggleBtnClick(Sender: TObject);
    procedure PlayerClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  public
    procedure OffscreenPaint; override;
    procedure ShowNewContent_Charts(NewMode: TWindowMode);
    procedure ShowNewContent_Ship(NewMode: TWindowMode; P: Integer = -1);

  private
    Kind: (dkChart, dkShip);
    Player, Mode: Integer;
  end;

var
  DiaDlg: TDiaDlg;

procedure PaintColonyShip(Canvas: TCanvas; Player, Left, Width, Top: Integer);

implementation

uses
  Protocol, ScreenTools, ClientTools, Tribes;

{$R *.lfm}

const
  Border = 24;
  RoundPixels: array [0 .. nStat - 1] of Integer = (0, 0, 0, 5, 5, 5);

  yArea = 48;
  xComp: array [0 .. 5] of Integer = (-60, -28, 4, 4, 36, 68);
  yComp: array [0 .. 5] of Integer = (-40, -40, -79, -1, -40, -40);
  xPow: array [0 .. 3] of Integer = (-116, -116, -116, -116);
  yPow: array [0 .. 3] of Integer = (-28, 0, -44, 16);
  xHab: array [0 .. 1] of Integer = (23, 23);
  yHab: array [0 .. 1] of Integer = (-81, 1);

procedure PaintColonyShip(Canvas: TCanvas; Player, Left, Width, Top: Integer);
var
  I, X, R, nComp, nPow, nHab: Integer;
begin
  Canvas.Brush.Color := $000000;
  Canvas.FillRect(Rect(Left, Top, Left + Width, Top + 200));
  Canvas.Brush.Style := bsClear;
    ScreenTools.Frame(Canvas, Left - 1, Top - 1, Left + Width, Top + 200,
      MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
    RFrame(Canvas, Left - 2, Top - 2, Left + Width + 1, Top + 200 + 1,
      MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);

    // stars
    DelphiRandSeed := Player * 11111;
    for I := 1 to Width - 16 do
    begin
      X := DelphiRandom((Width - 16) * 200);
      R := DelphiRandom(13) + 28;
      Canvas.Pixels[X div 200 + 8, X mod 200 + Top] :=
        (R * R * R * R div 10001) * $10101;
    end;

    nComp := MyRO.Ship[Player].Parts[spComp];
    nPow := MyRO.Ship[Player].Parts[spPow];
    nHab := MyRO.Ship[Player].Parts[spHab];
    if nComp > 6 then
      nComp := 6;
    if nPow > 4 then
      nPow := 4;
    if nHab > 2 then
      nHab := 2;
    for I := 0 to nHab - 1 do
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xHab[I],
        Top + 100 + yHab[I], 80, 80, 34, 1);
    for I := 0 to nComp - 1 do
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xComp[I],
        Top + 100 + yComp[I], 32, 80, 1, 1);
    if nComp > 0 then
      for I := 3 downto nPow do
        Sprite(Canvas, HGrSystem2, Left + Width div 2 + xPow[I] + 40,
          Top + 100 + yPow[I], 16, 27, 1, 82);
    for I := nPow - 1 downto 0 do
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xPow[I],
        Top + 100 + yPow[I], 56, 28, 58, 82);
    if (nComp < 3) and (nHab >= 1) then
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xComp[2] + 32 - 16,
        Top + 100 + 7 + yComp[2], 16, 27, 1, 82);
    if (nComp >= 3) and (nHab < 1) then
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xComp[2] + 32,
        Top + 100 + 7 + yComp[2], 16, 27, 18, 82);
    if (nComp < 4) and (nHab >= 2) then
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xComp[3] + 32 - 16,
        Top + 100 + 46 + yComp[3], 16, 27, 1, 82);
    if (nComp >= 4) and (nHab < 2) then
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xComp[3] + 32,
        Top + 100 + 46 + yComp[3], 16, 27, 18, 82);
    if (nComp <> 6) and (nComp <> 2) and not((nComp = 0) and (nPow < 1)) then
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xComp[nComp],
        Top + 100 + 7 + yComp[nComp], 16, 27, 18, 82);
    if (nComp <> 6) and (nComp <> 3) and not((nComp = 0) and (nPow < 2)) then
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xComp[nComp],
        Top + 100 + 46 + yComp[nComp], 16, 27, 18, 82);
    if nComp = 2 then
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xComp[3],
        Top + 100 + 7 + yComp[3], 16, 27, 18, 82);
    if nComp = 3 then
      Sprite(Canvas, HGrSystem2, Left + Width div 2 + xComp[4],
        Top + 100 + 7 + yComp[4], 16, 27, 18, 82);
end;

procedure TDiaDlg.FormCreate(Sender: TObject);
begin
  inherited;
  TitleHeight := WideFrame + 20;
  InnerHeight := Height - TitleHeight - NarrowFrame;
  CaptionRight := CloseBtn.Left;
  CaptionLeft := ToggleBtn.Left + ToggleBtn.Width;
  InitButtons;
end;

procedure TDiaDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TDiaDlg.OffscreenPaint;
var
  P, T, Max, X, Y, y0, Stop, R, RoundRange, LineStep: Integer;
  S: string;
  List: ^TChart;

  function Round(T: Integer): Integer;
  var
    N, I: Integer;
  begin
    if T < RoundRange then
      N := T
    else
      N := RoundRange;
    Result := 0;
    for I := T - N to T do
      Inc(Result, List[I]);
    Result := Result div (N + 1);
  end;

  procedure ShareBar(X, Y: Integer; Cap: string; val0, val1: Integer);
  begin
    LoweredTextOut(Offscreen.Canvas, -1, MainTexture, X - 2, Y, Cap);
    DLine(Offscreen.Canvas, X - 2, X + 169, Y + 16, MainTexture.ColorTextShade,
      MainTexture.ColorTextLight);
    if val0 > 0 then
      S := Format(Phrases.Lookup('SHARE'), [val0, val1])
    else
      S := '0';
    RisedTextOut(Offscreen.Canvas,
      X + 170 - BiColorTextWidth(Offscreen.Canvas, S), Y, S);
  end;

begin
  inherited;
  if Kind = dkChart then
    with Offscreen.Canvas do
    begin
      Font.Assign(UniFont[ftTiny]);
      Font.Color := $808080;

      RoundRange := RoundPixels[Mode] * (MyRO.Turn - 1)
        div (InnerWidth - 2 * Border);

      GetMem(List, 4 * (MyRO.Turn + 2));
      if Mode = stExplore then
        Max := G.lx * G.ly
      else
      begin
        Max := -1;
        for P := 0 to nPl - 1 do
          if (G.Difficulty[P] > 0) and
            (Server(sGetChart + Mode shl 4, Me, P, List^) >= rExecuted) then
            for T := 0 to MyRO.Turn - 1 do
            begin
              R := Round(T);
              if R > Max then
                Max := R;
            end;
      end;

      Brush.Color := $000000;
      FillRect(Rect(0, 0, InnerWidth, InnerHeight));
      Brush.Style := bsClear;
      Pen.Color := $606060;
      MoveTo(Border, InnerHeight - Border);
      LineTo(InnerWidth - Border, InnerHeight - Border);
      if MyRO.Turn >= 800 then
        LineStep := 200
      else if MyRO.Turn >= 400 then
        LineStep := 100
      else
        LineStep := 50;
      for T := 0 to (MyRO.Turn - 1) div LineStep do
      begin
        X := Border + (InnerWidth - 2 * Border) * T *
          LineStep div (MyRO.Turn - 1);
        MoveTo(X, Border);
        LineTo(X, InnerHeight - Border);
        S := IntToStr(abs(TurnToYear(T * LineStep)));
        Textout(X - TextWidth(S) div 2, Border - 16, S);
      end;

      y0 := 0;
      if Max > 0 then
      begin
        for P := 0 to nPl - 1 do
          if (G.Difficulty[P] > 0) and
            (Server(sGetChart + Mode shl 4, Me, P, List^) >= rExecuted) then
          begin
            Pen.Color := Tribe[P].Color;
            Stop := MyRO.Turn - 1;
            while (Stop > 0) and (List[Stop] = 0) do
              Dec(Stop);
            for T := 0 to Stop do
            begin
              R := Round(T);
              X := Border + (InnerWidth - 2 * Border) * T div (MyRO.Turn - 1);
              Y := InnerHeight - Border - (InnerHeight - 2 * Border) *
                R div Max;
              if T = 0 then
                MoveTo(X, Y)
                // else if Mode=stTerritory then
                // begin LineTo(x,y0); LineTo(x,y) end
              else if RoundPixels[Mode] = 0 then
              begin
                if (Y <> y0) or (T = Stop) then
                  LineTo(X, Y)
              end
              else
                LineTo(X, Y);
              y0 := Y;
            end;
          end;
      end;
      FreeMem(List);
    end
  else
    with Offscreen.Canvas do
    begin
      Font.Assign(UniFont[ftSmall]);
      FillOffscreen(0, 0, InnerWidth, InnerHeight);

      PaintColonyShip(Offscreen.Canvas, Player, 8, InnerWidth - 16, yArea);

      ShareBar(InnerWidth div 2 - 85, InnerHeight - 62,
        Phrases.Lookup('SHIPHAB'), MyRO.Ship[Player].Parts[spHab], 2);
      ShareBar(InnerWidth div 2 - 85, InnerHeight - 43,
        Phrases.Lookup('SHIPPOW'), MyRO.Ship[Player].Parts[spPow], 4);
      ShareBar(InnerWidth div 2 - 85, InnerHeight - 24,
        Phrases.Lookup('SHIPCOMP'), MyRO.Ship[Player].Parts[spComp], 6);
    end;
  MarkUsedOffscreen(InnerWidth, InnerHeight);
end;

procedure TDiaDlg.FormPaint(Sender: TObject);
var
  S: string;
begin
  inherited;
  Canvas.Font.Assign(UniFont[ftNormal]);
  if Kind = dkChart then
    S := Phrases.Lookup('DIAGRAM', Mode)
  else
    S := Tribe[Player].TPhrase('SHORTNAME');
  LoweredTextOut(Canvas, -1, MainTexture,
    (ClientWidth - BiColorTextWidth(Canvas, S)) div 2, 31, S);
end;

procedure TDiaDlg.FormShow(Sender: TObject);
begin
  if WindowMode = wmModal then
  begin { center on screen }
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
  end;
  OffscreenPaint;
end;

procedure TDiaDlg.ShowNewContent_Charts(NewMode: TWindowMode);
begin
  Kind := dkChart;
  Mode := stPop;
  ToggleBtn.ButtonIndex := 15;
  ToggleBtn.Hint := Phrases.Lookup('BTN_PAGE');
  Caption := Phrases.Lookup('TITLE_DIAGRAMS');
  inherited ShowNewContent(NewMode);
end;

procedure TDiaDlg.ShowNewContent_Ship(NewMode: TWindowMode; P: Integer);
begin
  Kind := dkShip;
  if P < 0 then
  begin
    Player := Me;
    while MyRO.Ship[Player].Parts[spComp] + MyRO.Ship[Player].Parts[spPow] +
      MyRO.Ship[Player].Parts[spHab] = 0 do
      Player := (Player + 1) mod nPl;
  end
  else
    Player := P;
  ToggleBtn.ButtonIndex := 28;
  ToggleBtn.Hint := Phrases.Lookup('BTN_SELECT');
  Caption := Phrases.Lookup('TITLE_SHIPS');
  inherited ShowNewContent(NewMode);
end;

procedure TDiaDlg.ToggleBtnClick(Sender: TObject);
var
  p1: Integer;
  M: TMenuItem;
begin
  if Kind = dkChart then
  begin
    Mode := (Mode + 1) mod nStat;
    OffscreenPaint;
    Invalidate;
  end
  else
  begin
    EmptyMenu(Popup.Items);
    for p1 := 0 to nPl - 1 do
      if MyRO.Ship[p1].Parts[spComp] + MyRO.Ship[p1].Parts[spPow] +
        MyRO.Ship[p1].Parts[spHab] > 0 then
      begin
        M := TMenuItem.Create(Popup);
        M.RadioItem := True;
        M.Caption := Tribe[p1].TPhrase('SHORTNAME');
        M.Tag := p1;
        M.OnClick := PlayerClick;
        if p1 = Player then
          M.Checked := True;
        Popup.Items.Add(M);
      end;
    Popup.Popup(Left + ToggleBtn.Left, Top + ToggleBtn.Top + ToggleBtn.Height);
  end;
end;

procedure TDiaDlg.PlayerClick(Sender: TObject);
begin
  ShowNewContent_Ship(FWindowMode, TComponent(Sender).Tag);
end;

procedure TDiaDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F6) and (Kind = dkChart) then // my key
    ToggleBtnClick(nil)
  else if (Key = VK_F8) and (Kind = dkShip) then // my other key
  else
    inherited;
end;

end.
