{$INCLUDE Switches.inc}
unit CityType;

interface

uses
  Protocol, ClientTools, Term, ScreenTools, BaseWin, LCLIntf, LCLType,
  SysUtils, Classes, Graphics, Controls, Forms,
  ButtonB, ExtCtrls;

type
  TCityTypeDlg = class(TFramedDlg)
    CloseBtn: TButtonB;
    DeleteBtn: TButtonB;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DeleteBtnClick(Sender: TObject);
  public
    procedure ShowNewContent(NewMode: TWindowMode);
  protected
    procedure OffscreenPaint; override;
  private
    nPool, dragiix, ctype: Integer;
    Pooliix: array [0 .. nImp - 1] of Integer;
    listed: Set of 0 .. nImp;
    Changed: Boolean;
    procedure LoadType(NewType: Integer);
    procedure SaveType;
  end;

var
  CityTypeDlg: TCityTypeDlg;

implementation

uses Help;

{$R *.lfm}

const
  xList = 7;
  yList = 0;
  nListRow = 4;
  nListCol = 10;
  xPool = 7;
  yPool = 220;
  nPoolRow = 4;
  nPoolCol = 10;
  xSwitch = 7;
  ySwitch = 150;
  xView = 226;
  yView = 130;

procedure TCityTypeDlg.FormCreate(Sender: TObject);
begin
  inherited;
  CaptionRight := CloseBtn.Left;
  InitButtons;
  HelpContext := 'MACRO';
  Caption := Phrases.Lookup('TITLE_CITYTYPES');
  DeleteBtn.Hint := Phrases.Lookup('BTN_DELETE');
end;

procedure TCityTypeDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TCityTypeDlg.FormPaint(Sender: TObject);
begin
  inherited;
  BtnFrame(Canvas, DeleteBtn.BoundsRect, MainTexture);
end;

procedure TCityTypeDlg.OffscreenPaint;
var
  I, iix: Integer;
  S: string;
begin
  inherited;
  Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
  FillOffscreen(xList - 7, yList, 42 * nListCol + 14, 32 * nListRow);
  FillOffscreen(xPool - 7, yPool, 42 * nPoolCol + 14, 32 * nPoolRow);
  FillOffscreen(0, yList + 32 * nListRow, 42 * nPoolCol + 14,
    yPool - yList - 32 * nListRow);

  Frame(Offscreen.Canvas, 0, yList + 32 * nListRow, InnerWidth - 255,
    yPool - 23, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Offscreen.Canvas, InnerWidth - 254, yList + 32 * nListRow,
    InnerWidth - 89, yPool - 23, MainTexture.ColorBevelLight,
    MainTexture.ColorBevelShade);
  Frame(Offscreen.Canvas, InnerWidth - 88, yList + 32 * nListRow,
    InnerWidth - 1, yPool - 23, MainTexture.ColorBevelLight,
    MainTexture.ColorBevelShade);
  Frame(Offscreen.Canvas, 0, yPool - 22, InnerWidth - 1, yPool - 1,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  for I := 0 to nCityType - 1 do
  begin
    RFrame(Offscreen.Canvas, xSwitch + I * 42, ySwitch, xSwitch + 39 + I * 42,
      ySwitch + 23, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
    if I = ctype then
      Frame(Offscreen.Canvas, xSwitch + 1 + I * 42, ySwitch + 1,
        xSwitch + 38 + I * 42, ySwitch + 22, MainTexture.ColorBevelShade,
        MainTexture.ColorBevelLight)
    else
      Frame(Offscreen.Canvas, xSwitch + 1 + I * 42, ySwitch + 1,
        xSwitch + 38 + I * 42, ySwitch + 22, MainTexture.ColorBevelLight,
        MainTexture.ColorBevelShade);
    BitBltCanvas(Offscreen.Canvas, xSwitch + 2 + I * 42, ySwitch + 2,
      xSizeSmall, ySizeSmall, SmallImp.Canvas, (I + 3) * xSizeSmall, 0);
  end;
  RisedTextOut(Offscreen.Canvas, 8, yList + 32 * nListRow + 2,
    Phrases.Lookup('BUILDORDER'));
  RisedTextOut(Offscreen.Canvas, 8, ySwitch + 26,
    Phrases.Lookup('CITYTYPE', ctype));
  S := Phrases.Lookup('BUILDREST');
  RisedTextOut(Offscreen.Canvas,
    (InnerWidth - BiColorTextWidth(Offscreen.Canvas, S)) div 2,
    yList + 72 + 32 * nListRow, S);

  with Offscreen.Canvas do
  begin
    for I := 1 to nListRow - 1 do
      DLine(Offscreen.Canvas, xList - 5, xList + 4 + 42 * nListCol,
        yList - 1 + 32 * I, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
    for I := 0 to nListCol * nListRow - 1 do
    begin
      S := IntToStr(I + 1);
      Font.Color := MainTexture.ColorTextLight;
      Textout(xList + 20 + I mod nListCol * 42 - TextWidth(S) div 2,
        yList + 15 + I div nListCol * 32 - TextHeight(S) div 2, S);
    end;
  end;

  I := 0;
  while MyData.ImpOrder[ctype, I] >= 0 do
  begin
    RFrame(Offscreen.Canvas, xList + 20 - xSizeSmall div 2 + I mod nListCol *
      42, yList + 15 - ySizeSmall div 2 + I div nListCol * 32,
      xList + 21 + xSizeSmall div 2 + I mod nListCol * 42,
      yList + 16 + ySizeSmall div 2 + I div nListCol * 32,
      MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
    BitBltCanvas(Offscreen.Canvas, xList + 21 - xSizeSmall div 2 +
      I mod nListCol * 42, yList + 16 - ySizeSmall div 2 + I div nListCol * 32,
      xSizeSmall, ySizeSmall, SmallImp.Canvas,
      MyData.ImpOrder[ctype, I] mod 7 * xSizeSmall,
      (MyData.ImpOrder[ctype, I] + SystemIconLines * 7) div 7 *
      ySizeSmall);
    Inc(I);
  end;

  nPool := 0;
  for iix := nWonder to nImp - 1 do
    if not(iix in listed) and (Imp[iix].Kind = ikCommon) and (iix <> imTrGoods)
      and (Imp[iix].Preq <> preNA) and
      ((Imp[iix].Preq = preNone) or (MyRO.Tech[Imp[iix].Preq] >= tsApplicable))
    then
    begin
      Pooliix[nPool] := iix;
      RFrame(Offscreen.Canvas, xPool + 20 - xSizeSmall div 2 +
        nPool mod nPoolCol * 42, yPool + 15 - ySizeSmall div 2 +
        nPool div nPoolCol * 32, xPool + 21 + xSizeSmall div 2 +
        nPool mod nPoolCol * 42, yPool + 16 + ySizeSmall div 2 +
        nPool div nPoolCol * 32, MainTexture.ColorBevelLight,
        MainTexture.ColorBevelShade);
      BitBltCanvas(Offscreen.Canvas, xPool + 21 - xSizeSmall div 2 +
        nPool mod nPoolCol * 42, yPool + 16 - ySizeSmall div 2 +
        nPool div nPoolCol * 32, xSizeSmall, ySizeSmall, SmallImp.Canvas,
        iix mod 7 * xSizeSmall, (iix + SystemIconLines * 7) div 7 *
        ySizeSmall);
      Inc(nPool);
    end;
  DeleteBtn.Visible := MyData.ImpOrder[ctype, 0] >= 0;

  if dragiix >= 0 then
  begin
    ImpImage(Offscreen.Canvas, xView + 9, yView + 5, dragiix);
    S := Phrases.Lookup('IMPROVEMENTS', dragiix);
    RisedTextOut(Offscreen.Canvas,
      xView + 36 - BiColorTextWidth(Offscreen.Canvas, S) div 2,
      ySwitch + 26, S);
  end;
  MarkUsedOffscreen(InnerWidth, InnerHeight);
end;

procedure TCityTypeDlg.LoadType(NewType: Integer);
var
  I: Integer;
begin
  ctype := NewType;
  listed := [];
  I := 0;
  while MyData.ImpOrder[ctype, I] >= 0 do
  begin
    Include(listed, MyData.ImpOrder[ctype, I]);
    Inc(I);
  end;
  Changed := False;
end;

procedure TCityTypeDlg.SaveType;
var
  cix: Integer;
begin
  if Changed then
  begin
    for cix := 0 to MyRO.nCity - 1 do
      if (MyCity[cix].Loc >= 0) and (MyCity[cix].Status and 7 = ctype + 1) then
        AutoBuild(cix, MyData.ImpOrder[ctype]);
    Changed := False;
  end;
end;

procedure TCityTypeDlg.FormShow(Sender: TObject);
begin
  LoadType(0);
  dragiix := -1;
  OffscreenPaint;
end;

procedure TCityTypeDlg.ShowNewContent(NewMode: TWindowMode);
begin
  inherited ShowNewContent(NewMode);
end;

procedure TCityTypeDlg.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  X := X - SideFrame;
  Y := Y - WideFrame;
  I := (X - xList) div 42 + (Y - yList) div 32 * nListCol;
  if (I < nImp) and (MyData.ImpOrder[ctype, I] >= 0) and
    (X > xList + 2 + I mod nListCol * 42) and
    (Y > yList + 5 + I div nListCol * 32) and
    (X < xList + 3 + 36 + I mod nListCol * 42) and
    (Y < yList + 6 + 20 + I div nListCol * 32) then
  begin
    if ssShift in Shift then
      HelpDlg.ShowNewContent(WindowModeMakePersistent(FWindowMode), hkImp,
        MyData.ImpOrder[ctype, I])
    else
    begin
      dragiix := MyData.ImpOrder[ctype, I];
      Screen.Cursor := crImpDrag;
      SmartUpdateContent;
    end;
    Exit;
  end;
  I := (X - xPool) div 42 + (Y - yPool) div 32 * nPoolCol;
  if (I < nPool) and (X > xPool + 2 + I mod nPoolCol * 42) and
    (Y > yPool + 5 + I div nPoolCol * 32) and
    (X < xPool + 3 + 36 + I mod nPoolCol * 42) and
    (Y < yPool + 6 + 20 + I div nPoolCol * 32) then
  begin
    if ssShift in Shift then
      HelpDlg.ShowNewContent(WindowModeMakePersistent(FWindowMode), hkImp, Pooliix[I])
    else
    begin
      dragiix := Pooliix[I];
      Screen.Cursor := crImpDrag;
      SmartUpdateContent;
    end;
    Exit;
  end;
  I := (X - xSwitch) div 42;
  if (I < nCityType) and (X > xSwitch + 2 + I * 42) and
    (X < xSwitch + 3 + 36 + I * 42) and (Y >= ySwitch + 2) and (Y < ySwitch + 22)
  then
  begin
    SaveType;
    LoadType(I);
    SmartUpdateContent;
  end;
end;

procedure TCityTypeDlg.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  procedure UnList(iix: Integer);
  var
    I: Integer;
  begin
    I := 0;
    while (MyData.ImpOrder[ctype, I] >= 0) and
      (MyData.ImpOrder[ctype, I] <> iix) do
      Inc(I);
    Assert(MyData.ImpOrder[ctype, I] = iix);
    Move(MyData.ImpOrder[ctype, I + 1], MyData.ImpOrder[ctype, I], nImp - I);
    Exclude(listed, iix);
  end;

var
  I: Integer;
begin
  X := X - SideFrame;
  Y := Y - WideFrame;
  if dragiix >= 0 then
  begin
    if (X >= xList) and (X < xList + nListCol * 42) and (Y >= yList) and
      (Y < yList + nListRow * 32) then
    begin
      if dragiix in listed then
        UnList(dragiix);
      I := (X - xList) div 42 + (Y - yList) div 32 * nListCol;
      while (I > 0) and (MyData.ImpOrder[ctype, I - 1] < 0) do
        Dec(I);
      Move(MyData.ImpOrder[ctype, I], MyData.ImpOrder[ctype, I + 1],
        nImp - I - 1);
      MyData.ImpOrder[ctype, I] := dragiix;
      Include(listed, dragiix);
      Changed := True;
    end
    else if (dragiix in listed) and (X >= xPool) and (X < xPool + nPoolCol * 42)
      and (Y >= yPool) and (Y < yPool + nPoolRow * 32) then
    begin
      UnList(dragiix);
      Changed := True;
    end;
    dragiix := -1;
    SmartUpdateContent;
  end;
  Screen.Cursor := crDefault;
end;

procedure TCityTypeDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveType;
  inherited;
end;

procedure TCityTypeDlg.DeleteBtnClick(Sender: TObject);
begin
  FillChar(MyData.ImpOrder[ctype], SizeOf(MyData.ImpOrder[ctype]), Byte(-1));
  listed := [];
  Changed := True;
  SmartUpdateContent;
end;

end.
