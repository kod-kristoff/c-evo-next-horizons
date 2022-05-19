{$INCLUDE Switches.inc}
unit MessgEx;

interface

uses
  Messg, Protocol, ScreenTools, Platform, DateUtils, LCLIntf, LCLType, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, ButtonA, ButtonB, StdCtrls,
  DrawDlg;

type
  TMessageIconKind = (mikNone, mikImp, mikModel, mikTribe, mikBook, mikAge,
    mikPureIcon, mikMyArmy, mikEnemyArmy, mikFullControl, mikShip, mikBigIcon,
    mikEnemyShipComplete);

  { TMessgExDlg }

  TMessgExDlg = class(TBaseMessgDlg)
    Button1: TButtonA;
    Button2: TButtonA;
    Button3: TButtonA;
    RemoveBtn: TButtonB;
    EInput: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RemoveBtnClick(Sender: TObject);
  public
    Kind: TMessageKind;
    IconIndex: Integer;
    HelpKind: Integer;
    HelpNo: Integer;
    CenterTo: Integer;
    IconKind: TMessageIconKind;
    OpenSound: string;
    function ShowModal: Integer; override;
    procedure CancelMovie;
  private
    MovieCancelled: Boolean;
    procedure PaintBook(ca: TCanvas; X, Y, clPage, clCover: Integer);
    procedure PaintMyArmy;
    procedure PaintEnemyArmy;
    procedure OnPlaySound(var Msg: TMessage); message WM_PLAYSOUND;
  end;

var
  MessgExDlg: TMessgExDlg;

procedure SoundMessageEx(SimpleText, SoundItem: string);
procedure TribeMessage(P: Integer; SimpleText, SoundItem: string);
function SimpleQuery(QueryKind: TMessageKind; SimpleText, SoundItem: string)
  : Integer;
procedure ContextMessage(SimpleText, SoundItem: string;
  ContextKind, ContextNo: Integer);


implementation

uses
  ClientTools, BaseWin, Term, Help, UnitStat, Tribes, UPixelPointer,
  Diagram, Sound;

{$R *.lfm}

const
  LostUnitsPerLine = 6;

procedure TMessgExDlg.FormCreate(Sender: TObject);
begin
  inherited;
  IconKind := mikNone;
  CenterTo := 0;
  OpenSound := '';
end;

procedure TMessgExDlg.FormShow(Sender: TObject);
var
  I: Integer;
begin
  if IconKind = mikEnemyArmy then
    InitAllEnemyModels;

  Button1.Visible := GameMode <> cMovie;
  Button2.Visible := (GameMode <> cMovie) and (Kind <> mkOk);
  Button3.Visible := (GameMode <> cMovie) and (Kind = mkYesNoCancel);
  RemoveBtn.Visible := (GameMode <> cMovie) and (Kind = mkOkCancelRemove);
  EInput.Visible := (GameMode <> cMovie) and (Kind = mkModel);
  if Button3.Visible then
  begin
    Button1.Left := 43;
    Button2.Left := 159;
  end
  else if Button2.Visible then
  begin
    Button1.Left := 101;
    Button2.Left := 217;
  end
  else
    Button1.Left := 159;
  RemoveBtn.Left := ClientWidth - 38;
  case Kind of
    mkYesNo, mkYesNoCancel:
      begin
        Button1.Caption := Phrases.Lookup('BTN_YES');
        Button2.Caption := Phrases.Lookup('BTN_NO')
      end;
    mkOKCancel, mkOkCancelRemove:
      begin
        Button1.Caption := Phrases.Lookup('BTN_OK');
        Button2.Caption := Phrases.Lookup('BTN_CANCEL');
      end;
  else
    begin
      Button1.Caption := Phrases.Lookup('BTN_OK');
      Button2.Caption := Phrases.Lookup('BTN_INFO');
    end;
  end;
  Button3.Caption := Phrases.Lookup('BTN_CANCEL');
  RemoveBtn.Hint := Phrases.Lookup('BTN_DELGAME');

  case IconKind of
    mikImp, mikModel, mikAge, mikPureIcon:
      TopSpace := 56;
    mikBigIcon:
      TopSpace := 152;
    mikEnemyShipComplete:
      TopSpace := 136;
    mikBook:
      if IconIndex >= 0 then
        TopSpace := 84
      else
        TopSpace := 47;
    mikTribe:
      begin
        Tribe[IconIndex].InitAge(GetAge(IconIndex));
        if Assigned(Tribe[IconIndex].faceHGr) then
          TopSpace := 64;
      end;
    mikFullControl:
      TopSpace := 80;
    mikShip:
      TopSpace := 240;
  else
    TopSpace := 0;
  end;

  SplitText(True);
  ClientHeight := 72 + Border + TopSpace + Lines * MessageLineSpacing;
  if GameMode = cMovie then
    ClientHeight := ClientHeight - 32;
  if Kind = mkModel then
    ClientHeight := ClientHeight + 36;
  if IconKind in [mikMyArmy, mikEnemyArmy] then
  begin
    if nLostArmy > LostUnitsPerLine * 6 then
      ClientHeight := ClientHeight + 6 * 48
    else
      ClientHeight := ClientHeight + ((nLostArmy - 1) div LostUnitsPerLine
        + 1) * 48;
  end;
  case CenterTo of
    0:
      begin
        Left := (Screen.Width - ClientWidth) div 2;
        Top := (Screen.Height - ClientHeight) div 2 - MapCenterUp;
      end;
    1:
      begin
        Left := (Screen.Width - ClientWidth) div 4;
        Top := (Screen.Height - ClientHeight) * 2 div 3 - MapCenterUp;
      end;
    -1:
      begin
        Left := (Screen.Width - ClientWidth) div 4;
        Top := (Screen.Height - ClientHeight) div 3 - MapCenterUp;
      end;
  end;
  for I := 0 to ControlCount - 1 do
    Controls[I].Top := ClientHeight - (34 + Border);
  if Kind = mkModel then
    EInput.Top := ClientHeight - (76 + Border);
end;

function TMessgExDlg.ShowModal: Integer;
var
  Ticks0: TDateTime;
  Ticks: TDateTime;
begin
  Caption := Phrases.Lookup('TITLE_MESSAGE');
  if GameMode = cMovie then
  begin
    if not((GameMode = cMovie) and (MovieSpeed = 4)) then
    begin
      MovieCancelled := False;
      Show;
      Ticks0 := NowPrecise;
      repeat
        Application.ProcessMessages;
        Sleep(1);
        Ticks := NowPrecise;
      until MovieCancelled or (Round((Ticks - Ticks0) / OneMillisecond) >= 1500);
      Hide;
    end;
    Result := mrOk;
  end
  else
    Result := inherited;
  Gtk2Fix;
end;

procedure TMessgExDlg.CancelMovie;
begin
  MovieCancelled := True;
end;

procedure TMessgExDlg.PaintBook(ca: TCanvas; X, Y, clPage, clCover: Integer);
const
  xScrewed = 77;
  yScrewed = 10;
  wScrewed = 43;
  hScrewed = 27;
type
  TScrewed = array [0 .. wScrewed - 1, 0 .. hScrewed - 1, 0 .. 3] of Single;
var
  ix, iy, xDst, yDst, dx, dy, xIcon, yIcon: Integer;
  BookRect: TRect;
  x1, xR, yR, share: Single;
  Screwed: TScrewed;
  SrcPtr: TPixelPointer;
  Width: Integer;
  Height: Integer;
begin
  Width := 56;
  Height := 40;
  if IconIndex >= 0 then begin
    xIcon := IconIndex mod 7 * xSizeBig;
    yIcon := (IconIndex + SystemIconLines * 7) div 7 * ySizeBig;
    // prepare screwed icon
    Screwed := Default(TScrewed);
    BigImp.BeginUpdate;
    SrcPtr := PixelPointer(BigImp, ScaleToNative(xIcon), ScaleToNative(yIcon));
    for iy := 0 to ScaleToNative(Height) - 1 do begin
      for ix := 0 to ScaleToNative(Width) - 1 do begin
        xR := ScaleFromNative(ix) * (37 + ScaleFromNative(iy) * 5 / Height) / Width;
        xDst := Trunc(xR);
        xR := Frac(xR);
        x1 := (120 - ScaleFromNative(ix)) * (120 - ScaleFromNative(ix)) - 10000;
        yR := ScaleFromNative(iy) * 18 / Height + x1 * x1 / 4000000;
        yDst := Trunc(yR);
        yR := Frac(yR);
        for dx := 0 to 1 do
          for dy := 0 to 1 do begin
            if dx = 0 then
              share := 1 - xR
            else
              share := xR;
            if dy = 0 then
              share := share * (1 - yR)
            else
              share := share * yR;
            Screwed[xDst + dx, yDst + dy, 0] := Screwed[xDst + dx, yDst + dy, 0]
              + share * SrcPtr.Pixel^.B;
            Screwed[xDst + dx, yDst + dy, 1] := Screwed[xDst + dx, yDst + dy, 1]
              + share * SrcPtr.Pixel^.G;
            Screwed[xDst + dx, yDst + dy, 2] := Screwed[xDst + dx, yDst + dy, 2]
              + share * SrcPtr.Pixel^.R;
            Screwed[xDst + dx, yDst + dy, 3] := Screwed[xDst + dx, yDst + dy,
              3] + share;
        end;
        SrcPtr.NextPixel;
      end;
      SrcPtr.NextLine;
    end;
    BigImp.EndUpdate;
    BookRect := BigBook.BoundsRect;
  end
  else
  begin
    BookRect := SmallBook.BoundsRect;
  end;
  X := X - BookRect.Width div 2;

  // paint
  UnshareBitmap(LogoBuffer);
  BitBltCanvas(LogoBuffer.Canvas, 0, 0, BookRect.Width, BookRect.Height, ca, X, Y);

  if IconIndex >= 0 then
    for iy := 0 to hScrewed - 1 do
      for ix := 0 to wScrewed - 1 do
        if Screwed[ix, iy, 3] > 0.01 then
          LogoBuffer.Canvas.Pixels[xScrewed + ix, yScrewed + iy] :=
            Trunc(Screwed[ix, iy, 2] / Screwed[ix, iy, 3]) +
            Trunc(Screwed[ix, iy, 1] / Screwed[ix, iy, 3]) shl 8 +
            Trunc(Screwed[ix, iy, 0] / Screwed[ix, iy, 3]) shl 16;

  ImageOp_BCC(LogoBuffer, Templates.Data, Point(0, 0), BookRect, clCover, clPage);

  BitBltCanvas(ca, X, Y, BookRect.Width, BookRect.Height, LogoBuffer.Canvas, 0, 0);
end;

procedure TMessgExDlg.PaintMyArmy;
begin
end;

procedure TMessgExDlg.PaintEnemyArmy;
var
  emix, ix, iy, X, Y, count, UnitsInLine: Integer;
begin
  ix := 0;
  iy := 0;
  if nLostArmy > LostUnitsPerLine then
    UnitsInLine := LostUnitsPerLine
  else
    UnitsInLine := nLostArmy;
  for emix := 0 to MyRO.nEnemyModel - 1 do
    for count := 0 to LostArmy[emix] - 1 do
    begin
      X := ClientWidth div 2 + ix * 64 - UnitsInLine * 32;
      Y := 26 + Border + TopSpace + Lines * MessageLineSpacing + iy * 48;
      with MyRO.EnemyModel[emix], Tribe[Owner].ModelPicture[mix] do
      begin
        BitBltCanvas(Canvas, X, Y, 64, 48, HGr.Mask.Canvas,
          pix mod 10 * 65 + 1, pix div 10 * 49 + 1, SRCAND);
        BitBltCanvas(Canvas, X, Y, 64, 48, HGr.Data.Canvas,
          pix mod 10 * 65 + 1, pix div 10 * 49 + 1, SRCPAINT);
      end;

      // next position
      Inc(ix);
      if ix = LostUnitsPerLine then
      begin // next line
        ix := 0;
        Inc(iy);
        if iy = 6 then
          Exit;
        UnitsInLine := nLostArmy - LostUnitsPerLine * iy;
        if UnitsInLine > LostUnitsPerLine then
          UnitsInLine := LostUnitsPerLine;
      end;
    end;
end;

procedure TMessgExDlg.FormPaint(Sender: TObject);
var
  p1, clSaveTextLight, clSaveTextShade: Integer;
begin
  if (IconKind = mikImp) and (IconIndex = 27) then
  begin // "YOU WIN" message
    clSaveTextLight := MainTexture.ColorTextLight;
    clSaveTextShade := MainTexture.ColorTextShade;
    MainTexture.ColorTextLight := $000000; // gold
    MainTexture.ColorTextShade := $0FDBFF;
    inherited;
    MainTexture.ColorTextLight := clSaveTextLight;
    MainTexture.ColorTextShade := clSaveTextShade;
  end
  else
    inherited;

  case IconKind of
    mikImp:
      if Imp[IconIndex].Kind = ikWonder then
      begin
        p1 := MyRO.Wonder[IconIndex].EffectiveOwner;
        UnshareBitmap(Buffer);
        BitBltCanvas(Buffer.Canvas, 0, 0, xSizeBig + 2 * GlowRange,
          ySizeBig + 2 * GlowRange, Canvas,
          ClientWidth div 2 - (28 + GlowRange), 24 - GlowRange);
        BitBltCanvas(Buffer.Canvas, GlowRange, GlowRange, xSizeBig, ySizeBig,
          BigImp.Canvas, IconIndex mod 7 * xSizeBig,
          (IconIndex + SystemIconLines * 7) div 7 * ySizeBig);
        if p1 < 0 then
          GlowFrame(Buffer, GlowRange, GlowRange, xSizeBig, ySizeBig, $000000)
        else
          GlowFrame(Buffer, GlowRange, GlowRange, xSizeBig, ySizeBig,
            Tribe[p1].Color);
        BitBltCanvas(Canvas, ClientWidth div 2 - (28 + GlowRange),
          24 - GlowRange, xSizeBig + 2 * GlowRange, ySizeBig + 2 * GlowRange,
          Buffer.Canvas, 0, 0);
      end
      else
        ImpImage(Canvas, ClientWidth div 2 - 28, 24, IconIndex);
    mikAge:
      begin
        if IconIndex = 0 then
          ImpImage(Canvas, ClientWidth div 2 - 28, 24, -7)
        else
          ImpImage(Canvas, ClientWidth div 2 - 28, 24, 24 + IconIndex)
      end;
    mikModel:
      with Tribe[Me].ModelPicture[IconIndex] do
      begin
        FrameImage(Canvas, BigImp, ClientWidth div 2 - 28, 24, xSizeBig,
          ySizeBig, 0, 0);
        BitBltCanvas(Canvas, ClientWidth div 2 - 32, 20, 64, 44,
          HGr.Mask.Canvas, pix mod 10 * 65 + 1,
          pix div 10 * 49 + 1, SRCAND);
        BitBltCanvas(Canvas, ClientWidth div 2 - 32, 20, 64, 44,
          HGr.Data.Canvas, pix mod 10 * 65 + 1,
          pix div 10 * 49 + 1, SRCPAINT);
      end;
    mikBook:
      PaintBook(Canvas, ClientWidth div 2, 24, MainTexture.ColorPage,
        MainTexture.ColorCover);
    mikTribe:
      if Assigned(Tribe[IconIndex].faceHGr) then
      begin
        Frame(Canvas, ClientWidth div 2 - 32 - 1, 24 - 1,
          ClientWidth div 2 + 32, 24 + 48, $000000, $000000);
        BitBltCanvas(Canvas, ClientWidth div 2 - 32, 24, 64, 48,
          Tribe[IconIndex].faceHGr.Data.Canvas,
          1 + Tribe[IconIndex].facepix mod 10 * 65,
          1 + Tribe[IconIndex].facepix div 10 * 49)
      end;
    mikPureIcon:
      FrameImage(Canvas, BigImp, ClientWidth div 2 - 28, 24, xSizeBig, ySizeBig,
        IconIndex mod 7 * xSizeBig, IconIndex div 7 * ySizeBig);
    mikBigIcon:
      FrameImage(Canvas, BigImp, ClientWidth div 2 - 3 * 28, 32, xSizeBig * 3,
        ySizeBig * 3, IconIndex mod 2 * 3 * xSizeBig,
        IconIndex div 2 * 3 * ySizeBig);
    mikEnemyShipComplete:
      begin
        BitBltCanvas(Buffer.Canvas, 0, 0, 140, 120, Canvas,
          (ClientWidth - 140) div 2, 24);
        ImageOp_BCC(Buffer, Templates.Data, Point(0, 0), StarshipDeparted.BoundsRect, 0, $FFFFFF);
        BitBltCanvas(Canvas, (ClientWidth - 140) div 2, 24, 140, 120,
          Buffer.Canvas, 0, 0);
      end;
    mikMyArmy:
      PaintMyArmy;
    mikEnemyArmy:
      PaintEnemyArmy;
    mikFullControl:
      Sprite(Canvas, HGrSystem2, ClientWidth div 2 - 31, 24, 63, 63, 1, 281);
    mikShip:
      PaintColonyShip(Canvas, IconIndex, 17, ClientWidth - 34, 38);
  end;

  if EInput.Visible then
    EditFrame(Canvas, EInput.BoundsRect, MainTexture);

  if OpenSound <> '' then
    PostMessage(Handle, WM_PLAYSOUND, 0, 0);
end;

procedure TMessgExDlg.Button1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TMessgExDlg.Button2Click(Sender: TObject);
begin
  if Kind = mkOkHelp then
    HelpDlg.ShowNewContent(wmSubmodal, HelpKind, HelpNo)
  else if Kind = mkModel then
    UnitStatDlg.ShowNewContent_OwnModel(wmSubmodal, IconIndex)
  else
    ModalResult := mrIgnore;
end;

procedure TMessgExDlg.Button3Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TMessgExDlg.RemoveBtnClick(Sender: TObject);
begin
  ModalResult := mrNo;
end;

procedure TMessgExDlg.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ModalResult := mrOk
  else if (Key = #27) then
    if Button3.Visible then
      ModalResult := mrCancel
    else if Button2.Visible then
      ModalResult := mrIgnore;
end;

procedure SoundMessageEx(SimpleText, SoundItem: string);
// because Messg.SoundMessage not capable of movie mode
begin
  with MessgExDlg do
  begin
    MessgText := SimpleText;
    OpenSound := SoundItem;
    Kind := mkOk;
    ShowModal;
  end;
end;

procedure TribeMessage(P: Integer; SimpleText, SoundItem: string);
begin
  with MessgExDlg do
  begin
    OpenSound := SoundItem;
    MessgText := SimpleText;
    Kind := mkOk;
    IconKind := mikTribe;
    IconIndex := P;
    ShowModal;
  end;
end;

function SimpleQuery(QueryKind: TMessageKind; SimpleText, SoundItem: string)
  : Integer;
begin
  with MessgExDlg do
  begin
    MessgText := SimpleText;
    OpenSound := SoundItem;
    Kind := QueryKind;
    ShowModal;
    Result := ModalResult;
  end;
end;

procedure ContextMessage(SimpleText, SoundItem: string;
  ContextKind, ContextNo: Integer);
begin
  with MessgExDlg do
  begin
    MessgText := SimpleText;
    OpenSound := SoundItem;
    Kind := mkOkHelp;
    HelpKind := ContextKind;
    HelpNo := ContextNo;
    ShowModal;
  end;
end;

procedure TMessgExDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IconKind := mikNone;
  CenterTo := 0;
end;

procedure TMessgExDlg.OnPlaySound(var Msg: TMessage);
begin
  Play(OpenSound);
  OpenSound := '';
end;

end.
