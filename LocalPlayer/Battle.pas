{$INCLUDE Switches.inc}
unit Battle;

interface

uses
  ScreenTools, Protocol, ButtonBase, ButtonA, Types, LCLIntf, LCLType,
  SysUtils, Classes, Graphics, Controls, Forms, DrawDlg, IsoEngine;

type

  { TBattleDlg }

  TBattleDlg = class(TDrawDlg)
    OKBtn: TButtonA;
    CancelBtn: TButtonA;
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure PaintBattleOutcome(ca: TCanvas; xm, ym, uix, ToLoc: Integer;
      Forecast: TBattleForecastEx);
  private
    IsoMap: TIsoMap;
  public
    uix, ToLoc: Integer;
    Forecast: TBattleForecastEx;
    IsSuicideQuery: Boolean;
  end;

var
  BattleDlg: TBattleDlg;


implementation

uses
  Term, ClientTools;

{$R *.lfm}

const
  Border = 3;
  MessageLineSpacing = 20;

  DamageColor = $0000E0;
  FanaticColor = $800080;
  FirstStrikeColor = $A0A0A0;

procedure TBattleDlg.PaintBattleOutcome(ca: TCanvas; xm, ym, uix, ToLoc: Integer;
  Forecast: TBattleForecastEx);
var
  euix, ADamage, DDamage, StrMax, DamageMax, MaxBar, LAStr, LDStr, LADamage,
    LDDamage, LABaseDamage, LAAvoidedDamage, LDBaseDamage: Integer;
  // TerrType: Cardinal;
  UnitInfo: TUnitInfo;
  TextSize: TSize;
  LabelText: string;
  FirstStrike: Boolean;
begin
  MaxBar := 65;

  // TerrType:=MyMap[ToLoc] and fTerrain;
  GetUnitInfo(ToLoc, euix, UnitInfo);

  FirstStrike := (MyModel[MyUn[uix].mix].Cap[mcFirst] > 0) and
    (Forecast.DBaseDamage >= UnitInfo.Health);
  ADamage := MyUn[uix].Health - Forecast.EndHealthAtt;
  if FirstStrike then
    ADamage := ADamage + Forecast.ABaseDamage div 2;
  DDamage := UnitInfo.Health - Forecast.EndHealthDef;
  if Forecast.AStr > Forecast.DStr then
    StrMax := Forecast.AStr
  else
    StrMax := Forecast.DStr;
  if ADamage > DDamage then
    DamageMax := ADamage
  else
    DamageMax := DDamage;
  if Forecast.ABaseDamage > Forecast.DBaseDamage then
    StrMax := StrMax * DamageMax div Forecast.ABaseDamage
  else
    StrMax := StrMax * DamageMax div Forecast.DBaseDamage;

  LAStr := Forecast.AStr * MaxBar div StrMax;
  LDStr := Forecast.DStr * MaxBar div StrMax;
  LADamage := ADamage * MaxBar div DamageMax;
  LABaseDamage := Forecast.ABaseDamage * MaxBar div DamageMax;
  if FirstStrike then
    LAAvoidedDamage := LABaseDamage div 2
  else
    LAAvoidedDamage := 0;
  LDDamage := DDamage * MaxBar div DamageMax;
  LDBaseDamage := Forecast.DBaseDamage * MaxBar div DamageMax;

  DarkGradient(ca, xm - 8 - LAStr, ym - 8, LAStr, 2);
  VDarkGradient(ca, xm - 8, ym - 8 - LDStr, LDStr, 2);
  LightGradient(ca, xm + 8, ym - 8, LDBaseDamage, DamageColor);
  if LDDamage > LDBaseDamage then
    LightGradient(ca, xm + 8 + LDBaseDamage, ym - 8, LDDamage - LDBaseDamage,
      FanaticColor);
  if LAAvoidedDamage > 0 then
    VLightGradient(ca, xm - 8, ym + 8, LAAvoidedDamage, FirstStrikeColor);
  VLightGradient(ca, xm - 8, ym + 8 + LAAvoidedDamage,
    LABaseDamage - LAAvoidedDamage, DamageColor);
  if LADamage > LABaseDamage then
    VLightGradient(ca, xm - 8, ym + 8 + LABaseDamage, LADamage - LABaseDamage,
      FanaticColor);
  BitBltCanvas(ca, xm - 12, ym - 12, 24, 24,
    HGrSystem.Mask.Canvas, 26, 146, SRCAND);
  BitBltCanvas(ca, xm - 12, ym - 12, 24, 24,
    HGrSystem.Data.Canvas, 26, 146, SRCPAINT);

  LabelText := Format('%d', [Forecast.AStr]);
  TextSize := ca.TextExtent(LabelText);
  if TextSize.cx div 2 + 2 > LAStr div 2 then
    RisedTextOut(ca, xm - 10 - TextSize.cx, ym - (TextSize.cy + 1) div 2,
      LabelText)
  else
    RisedTextOut(ca, xm - 8 - (LAStr + TextSize.cx) div 2,
      ym - (TextSize.cy + 1) div 2, LabelText);

  LabelText := Format('%d', [Forecast.DStr]);
  TextSize := ca.TextExtent(LabelText);
  if TextSize.cy div 2 > LDStr div 2 then
    RisedTextOut(ca, xm - (TextSize.cx + 1) div 2, ym - 8 - TextSize.cy,
      LabelText)
  else
    RisedTextOut(ca, xm - (TextSize.cx + 1) div 2,
      ym - 8 - (LDStr + TextSize.cy) div 2, LabelText);

  if Forecast.EndHealthDef <= 0 then
  begin
    BitBltCanvas(ca, xm + 9 + LDDamage - 7, ym - 6, 14, 17,
      HGrSystem.Mask.Canvas, 51, 153, SRCAND);
    BitBltCanvas(ca, xm + 8 + LDDamage - 7, ym - 7, 14, 17,
      HGrSystem.Mask.Canvas, 51, 153, SRCAND);
    BitBltCanvas(ca, xm + 8 + LDDamage - 7, ym - 7, 14, 17,
      HGrSystem.Data.Canvas, 51, 153, SRCPAINT);
  end;
  LabelText := Format('%d', [DDamage]);
  TextSize := ca.TextExtent(LabelText);
  if TextSize.cx div 2 + 2 > LDDamage div 2 then
  begin
    if Forecast.EndHealthDef > 0 then
      RisedTextOut(ca, xm + 10, ym - (TextSize.cy + 1) div 2, LabelText);
  end
  else
    RisedTextOut(ca, xm + 8 + (LDDamage - TextSize.cx) div 2,
      ym - (TextSize.cy + 1) div 2, LabelText);

  if Forecast.EndHealthAtt <= 0 then
  begin
    BitBltCanvas(ca, xm - 6, ym + 9 + LADamage - 7, 14, 17,
      HGrSystem.Mask.Canvas, 51, 153, SRCAND);
    BitBltCanvas(ca, xm - 7, ym + 8 + LADamage - 7, 14, 17,
      HGrSystem.Mask.Canvas, 51, 153, SRCAND);
    BitBltCanvas(ca, xm - 7, ym + 8 + LADamage - 7, 14, 17,
      HGrSystem.Data.Canvas, 51, 153, SRCPAINT);
  end;
  LabelText := Format('%d', [MyUn[uix].Health - Forecast.EndHealthAtt]);
  TextSize := ca.TextExtent(LabelText);
  if TextSize.cy div 2 > (LADamage - LAAvoidedDamage) div 2 + LAAvoidedDamage
  then
  begin
    if Forecast.EndHealthAtt > 0 then
      RisedTextOut(ca, xm - (TextSize.cx + 1) div 2, ym + 8 + LAAvoidedDamage,
        LabelText);
  end
  else
    RisedTextOut(ca, xm - (TextSize.cx + 1) div 2, ym + 8 + LAAvoidedDamage +
      (LADamage - LAAvoidedDamage - TextSize.cy) div 2, LabelText);

  IsoMap.SetOutput(Buffer);
  BitBltCanvas(Buffer.Canvas, 0, 0, 66, 48, ca, xm + 8 + 4,
    ym - 8 - 12 - 48);
  { if TerrType<fForest then
    Sprite(Buffer,HGrTerrain,0,16,66,32,1+TerrType*(xxt*2+1),1+yyt)
    else
    begin
    Sprite(Buffer,HGrTerrain,0,16,66,32,1+2*(xxt*2+1),1+yyt+2*(yyt*3+1));
    if (TerrType=fForest) and IsJungle(ToLoc div G.lx) then
    Sprite(Buffer,HGrTerrain,0,16,66,32,1+7*(xxt*2+1),1+yyt+19*(yyt*3+1))
    else Sprite(Buffer,HGrTerrain,0,16,66,32,1+7*(xxt*2+1),1+yyt+2*(2+TerrType-fForest)*(yyt*3+1));
    end; }
  IsoMap.PaintUnit(1, 0, UnitInfo, 0);
  BitBltCanvas(ca, xm + 8 + 4, ym - 8 - 12 - 48, 66, 48, Buffer.Canvas,
    0, 0);

  BitBltCanvas(Buffer.Canvas, 0, 0, 66, 48, ca, xm - 8 - 4 - 66,
    ym + 8 + 12);
  MakeUnitInfo(Me, MyUn[uix], UnitInfo);
  UnitInfo.Flags := UnitInfo.Flags and not unFortified;
  IsoMap.PaintUnit(1, 0, UnitInfo, 0);
  BitBltCanvas(ca, xm - 8 - 4 - 66, ym + 8 + 12, 66, 48, Buffer.Canvas, 0, 0);
end;

procedure TBattleDlg.FormCreate(Sender: TObject);
begin
  IsoMap := TIsoMap.Create;
  OKBtn.Caption := Phrases.Lookup('BTN_YES');
  CancelBtn.Caption := Phrases.Lookup('BTN_NO');
  InitButtons;
end;

procedure TBattleDlg.FormShow(Sender: TObject);
begin
  if IsSuicideQuery then
  begin
    ClientWidth := 300;
    ClientHeight := 288;
    OKBtn.Visible := True;
    CancelBtn.Visible := True;
    Left := (Screen.Width - ClientWidth) div 2; // center on screen
    Top := (Screen.Height - ClientHeight) div 2;
  end
  else
  begin
    ClientWidth := 178;
    ClientHeight := 178;
    OKBtn.Visible := False;
    CancelBtn.Visible := False;
  end;
end;

procedure TBattleDlg.FormPaint(Sender: TObject);
var
  ym, cix, P: Integer;
  S, s1: string;
begin
  with Canvas do
  begin
    Brush.Color := 0;
    FillRect(Rect(0, 0, ClientWidth, ClientHeight));
    Brush.Style := bsClear;
    PaintBackground(self, 3 + Border, 3 + Border,
      ClientWidth - (6 + 2 * Border), ClientHeight - (6 + 2 * Border));
  end;
  Frame(Canvas, Border + 1, Border + 1, ClientWidth - (2 + Border),
    ClientHeight - (2 + Border), MainTexture.ColorBevelLight,
    MainTexture.ColorBevelShade);
  Frame(Canvas, 2 + Border, 2 + Border, ClientWidth - (3 + Border),
    ClientHeight - (3 + Border), MainTexture.ColorBevelLight,
    MainTexture.ColorBevelShade);

  if IsSuicideQuery then
  begin
    Canvas.Font.Assign(UniFont[ftCaption]);
    S := Phrases.Lookup('TITLE_SUICIDE');
    RisedTextOut(Canvas, (ClientWidth - BiColorTextWidth(Canvas, S)) div 2,
      7 + Border, S);
    Canvas.Font.Assign(UniFont[ftNormal]);
    S := Phrases.Lookup('SUICIDE');
    P := Pos('\', S);
    if P = 0 then
      RisedTextOut(Canvas, (ClientWidth - BiColorTextWidth(Canvas, S))
        div 2, 205, S)
    else
    begin
      s1 := Copy(S, 1, P - 1);
      RisedTextOut(Canvas, (ClientWidth - BiColorTextWidth(Canvas, s1)) div 2,
        205 - MessageLineSpacing div 2, s1);
      s1 := Copy(S, P + 1, 255);
      RisedTextOut(Canvas, (ClientWidth - BiColorTextWidth(Canvas, s1)) div 2,
        205 + (MessageLineSpacing - MessageLineSpacing div 2), s1);
    end;
    ym := 110;
  end
  else
    ym := ClientHeight div 2;
  Canvas.Font.Assign(UniFont[ftSmall]);
  PaintBattleOutcome(Canvas, ClientWidth div 2, ym, uix, ToLoc, Forecast);

  for cix := 0 to ControlCount - 1 do
    if (Controls[cix].Visible) and (Controls[cix] is TButtonBase) then
      BtnFrame(Canvas, Controls[cix].BoundsRect, MainTexture);
end;

procedure TBattleDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(IsoMap);
end;

procedure TBattleDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not IsSuicideQuery then
    Close;
end;

procedure TBattleDlg.FormDeactivate(Sender: TObject);
begin
  if not IsSuicideQuery then
    Close;
end;

procedure TBattleDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not IsSuicideQuery and (Key <> VK_SHIFT) then
  begin
    Close;
    MainScreen.Update;
    if Key <> VK_ESCAPE then
      MainScreen.FormKeyDown(Sender, Key, Shift);
  end;
end;

procedure TBattleDlg.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TBattleDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
