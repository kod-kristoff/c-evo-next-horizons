{$INCLUDE Switches.inc}
unit Wonders;

interface

uses
  ScreenTools, BaseWin, Protocol, LCLIntf, LCLType, SysUtils, Classes, Graphics,
  Controls, Forms, ButtonB;

type

  { TWondersDlg }

  TWondersDlg = class(TBufferedDrawDlg)
    CloseBtn: TButtonB;
    procedure FormCreate(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    Selection: Integer;
    Center: TPoint;
    procedure DarkIcon(i: Integer);
    procedure Glow(i, GlowColor: Integer);
    procedure PaintBackgroundShape;
  public
    procedure OffscreenPaint; override;
    procedure ShowNewContent(NewMode: Integer);
  end;

var
  WondersDlg: TWondersDlg;


implementation

uses
  Term, ClientTools, Help, Tribes, UPixelPointer;

{$R *.lfm}

const
  RingPosition: array [0 .. 20] of TPoint = (
    (X: -80; Y: -32), // Pyramids
    (X: 80; Y: -32), // Zeus
    (X: 0; Y: -64), // Gardens
    (X: 0; Y: 0), // Colossus
    (X: 0; Y: 64), // Lighthouse
    (X: -80; Y: 32), // GrLibrary
    (X: -90; Y: 114), // Oracle
    (X: 80; Y: 32), // Sun
    (X: 90; Y: -114), // Leo
    (X: -180; Y: 0), // Magellan
    (X: 90; Y: 114), // Mich
    (X: 0; Y: 0), // {11;}
    (X: 180; Y: 0), // Newton
    (X: -90; Y: -114), // Bach
    (X: 0; Y: 0), // {14;}
    (X: -160; Y: -64), // Liberty
    (X: 0; Y: 128), // Eiffel
    (X: 160; Y: -64), // Hoover
    (X: -160; Y: 64), // Shinkansen
    (X: 0; Y: -128), // Manhattan
    (X: 160; Y: 64) // Mir
  );

procedure TWondersDlg.FormCreate(Sender: TObject);
begin
  Canvas.Font.Assign(UniFont[ftNormal]);
  Canvas.Brush.Style := bsClear;
  InitButtons;
end;

procedure TWondersDlg.FormShow(Sender: TObject);
begin
  Selection := -1;
  OffscreenPaint;
end;

procedure TWondersDlg.ShowNewContent(NewMode: Integer);
begin
  inherited ShowNewContent(NewMode);
end;

procedure TWondersDlg.PaintBackgroundShape;
const
  Darken = 24;
  // space=pi/120;
  amax0 = 15734; // 1 shl 16*tan(pi/12-space);
  amin1 = 19413; // 1 shl 16*tan(pi/12+space);
  amax1 = 62191; // 1 shl 16*tan(pi/4-space);
  amin2 = 69061; // 1 shl 16*tan(pi/4+space);
  amax2 = 221246; // 1 shl 16*tan(5*pi/12-space);
  amin3 = 272977; // 1 shl 16*tan(5*pi/12+space);
var
  X: Integer;
  Y: Integer;
  ax: Integer;
  R: Integer;
  C: Integer;
  Ch: Integer;
  Line: array [0..3] of TPixelPointer;
  Width: Integer;
  Height: Integer;
begin
  Width := ScaleToNative(180);
  Height := ScaleToNative(128);
  Offscreen.BeginUpdate;
  Line[0] := PixelPointer(Offscreen, ScaleToNative(Center.X), ScaleToNative(Center.Y));
  Line[1] := PixelPointer(Offscreen, ScaleToNative(Center.X), ScaleToNative(Center.Y) - 1);
  Line[2] := PixelPointer(Offscreen, ScaleToNative(Center.X) - 1, ScaleToNative(Center.Y));
  Line[3] := PixelPointer(Offscreen, ScaleToNative(Center.X) - 1, ScaleToNative(Center.Y) - 1);
  for Y := 0 to Height - 1 do begin
    for X := 0 to Width - 1 do begin
      r := X * X * ((Height div 4) * (Height div 4)) + Y * Y * ((Width div 4) * (Width div 4));
      ax := ((1 shl 16 div (Height div 4)) * (Width div 4)) * Y;
      if (r < ScaleToNative(8) * Height * Width * Width) and
        ((r >= (Height div 4) * (Height div 2) * (Width div 2) * (Width div 2)) and (ax < amax2 * X) and
        ((ax < amax0 * X) or (ax > amin2 * X)) or (ax > amin1 * X) and
        ((ax < amax1 * X) or (ax > amin3 * X))) then begin
        for ch := 0 to 2 do begin
          c := Line[0].Pixel^.Planes[ch] - Darken;
          if c < 0 then Line[0].Pixel^.Planes[ch] := 0
            else Line[0].Pixel^.Planes[ch] := c;
          c := Line[1].Pixel^.Planes[ch] - Darken;
          if c < 0 then Line[1].Pixel^.Planes[ch] := 0
            else Line[1].Pixel^.Planes[ch] := c;
          c := Line[2].Pixel^.Planes[ch] - Darken;
          if c < 0 then Line[2].Pixel^.Planes[ch] := 0
            else Line[2].Pixel^.Planes[ch] := c;
          c := Line[3].Pixel^.Planes[ch] - Darken;
          if c < 0 then Line[3].Pixel^.Planes[ch] := 0
            else Line[3].Pixel^.Planes[ch] := c;
        end;
      end;
      Line[0].NextPixel;
      Line[1].NextPixel;
      Line[2].PreviousPixel;
      Line[3].PreviousPixel;
    end;
    Line[0].NextLine;
    Line[1].PreviousLine;
    Line[2].NextLine;
    Line[3].PreviousLine;
  end;
  Offscreen.EndUpdate;
end;

procedure TWondersDlg.DarkIcon(i: Integer);
var
  X, Y, ch, x0Dst, y0Dst, x0Src, y0Src, darken, c: Integer;
  Src, Dst: TPixelPointer;
begin
  Offscreen.BeginUpdate;
  x0Dst := ClientWidth div 2 - xSizeBig div 2 + RingPosition[i].X;
  y0Dst := ClientHeight div 2 - ySizeBig div 2 + RingPosition[i].Y;
  x0Src := (i mod 7) * xSizeBig;
  y0Src := (i div 7 + SystemIconLines) * ySizeBig;
  Src := PixelPointer(BigImp, ScaleToNative(x0Src), ScaleToNative(y0Src));
  Dst := PixelPointer(Offscreen, ScaleToNative(x0Dst), ScaleToNative(y0Dst));
  for Y := 0 to ScaleToNative(ySizeBig) - 1 do begin
    for X := 0 to ScaleToNative(xSizeBig) - 1 do begin
      Darken := ((255 - Src.Pixel^.B) * 3 + (255 - Src.Pixel^.G) *
        15 + (255 - Src.Pixel^.R) * 9) div 128;
      for ch := 0 to 2 do begin
        c := Dst.Pixel^.Planes[ch] - Darken;
        if c < 0 then Dst.Pixel^.Planes[ch] := 0
          else Dst.Pixel^.Planes[ch] := c;
      end;
      Src.NextPixel;
      Dst.NextPixel;
    end;
    Src.NextLine;
    Dst.NextLine;
  end;
  Offscreen.EndUpdate;
end;

procedure TWondersDlg.Glow(i, GlowColor: Integer);
begin
  GlowFrame(Offscreen,
    ClientWidth div 2 - xSizeBig div 2 + RingPosition[i].X,
    ClientHeight div 2 - ySizeBig div 2 + RingPosition[i].Y,
    xSizeBig, ySizeBig, GlowColor);
end;

procedure TWondersDlg.OffscreenPaint;
var
  I: Integer;
  HaveWonder: Boolean;
  S: string;
begin
  if (OffscreenUser <> nil) and (OffscreenUser <> Self) then
    OffscreenUser.Update;
  // complete working with old owner to prevent rebound
  OffscreenUser := Self;

  Fill(Offscreen.Canvas, 3, 3, ClientWidth - 6, ClientHeight - 6,
    (Maintexture.Width - ClientWidth) div 2, (Maintexture.Height - ClientHeight) div 2);
  Frame(Offscreen.Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);
  Frame(Offscreen.Canvas, 1, 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Offscreen.Canvas, 2, 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Corner(Offscreen.Canvas, 1, 1, 0, MainTexture);
  Corner(Offscreen.Canvas, ClientWidth - 9, 1, 1, MainTexture);
  Corner(Offscreen.Canvas, 1, ClientHeight - 9, 2, MainTexture);
  Corner(Offscreen.Canvas, ClientWidth - 9, ClientHeight - 9, 3, MainTexture);

  BtnFrame(Offscreen.Canvas, CloseBtn.BoundsRect, MainTexture);

  Offscreen.Canvas.Font.Assign(UniFont[ftCaption]);
  S := Phrases.Lookup('TITLE_WONDERS');
  RisedTextOut(Offscreen.Canvas,
    (ClientWidth - BiColorTextWidth(Offscreen.Canvas, S)) div 2 - 1, 7, S);
  Offscreen.Canvas.Font.Assign(UniFont[ftNormal]);

  Center := Point(ClientWidth div 2, ClientHeight div 2);

  PaintBackgroundShape;

  // Draw all bitmaps first
  HaveWonder := False;
  for I := 0 to 20 do begin
    if Imp[I].Preq <> preNA then begin
      case MyRO.Wonder[I].CityID of
        WonderNotBuiltYet: begin
          Fill(Offscreen.Canvas, Center.X - xSizeBig div 2 + RingPosition[I].X - 3,
            Center.Y - ySizeBig div 2 + RingPosition[I].Y - 3, xSizeBig + 6,
            ySizeBig + 6, (Maintexture.Width - ClientWidth) div 2,
            (Maintexture.Height - ClientHeight) div 2);
        end;
        WonderDestroyed: begin
          HaveWonder := True;
          BitBltCanvas(Offscreen.Canvas,
            Center.X - xSizeBig div 2 + RingPosition[I].X,
            Center.Y - ySizeBig div 2 + RingPosition[I].Y, xSizeBig,
            ySizeBig, BigImp.Canvas, 0, (SystemIconLines + 3) *
            ySizeBig);
        end;
        else begin
          HaveWonder := True;
          BitBltCanvas(Offscreen.Canvas,
            Center.X - xSizeBig div 2 + RingPosition[I].X,
            Center.Y - ySizeBig div 2 + RingPosition[I].Y, xSizeBig, ySizeBig,
            BigImp.Canvas, (I mod 7) * xSizeBig,
            (I div 7 + SystemIconLines) * ySizeBig);
        end;
      end;
    end;
  end;

  // Do direct pixel postprocessing separately to avoid bitmap copying in memory
  Offscreen.Canvas.FillRect(0, 0, 0, 0);
  Offscreen.BeginUpdate;
  for I := 0 to 20 do begin
    if Imp[I].Preq <> preNA then begin
      case MyRO.Wonder[I].CityID of
        WonderNotBuiltYet: DarkIcon(I);
        WonderDestroyed: Glow(I, $000000);
        else begin
          if MyRO.Wonder[I].EffectiveOwner >= 0 then
            Glow(I, Tribe[MyRO.Wonder[I].EffectiveOwner].Color)
          else Glow(I, $000000);
        end;
      end;
    end;
  end;
  Offscreen.EndUpdate;

  if not HaveWonder then
  begin
    S := Phrases.Lookup('NOWONDER');
    RisedTextOut(Offscreen.Canvas,
      Center.X - BiColorTextWidth(Offscreen.Canvas, S) div 2,
      Center.Y - Offscreen.Canvas.TextHeight(S) div 2, S);
  end;

  MarkUsedOffscreen(ClientWidth, ClientHeight);
end; { OffscreenPaint }

procedure TWondersDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TWondersDlg.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  I, OldSelection: Integer;
  S: string;
begin
  OldSelection := Selection;
  Selection := -1;
  for I := 0 to 20 do
    if (Imp[I].Preq <> preNA) and
      (X >= Center.X - xSizeBig div 2 + RingPosition[I].X) and
      (X < Center.X + xSizeBig div 2 + RingPosition[I].X) and
      (Y >= Center.Y - ySizeBig div 2 + RingPosition[I].Y) and
      (Y < Center.Y + ySizeBig div 2 + RingPosition[I].Y) then
    begin
      Selection := I;
      Break;
    end;
  if Selection <> OldSelection then
  begin
    Fill(Canvas, 9, ClientHeight - 3 - 46, ClientWidth - 18, 44,
      (Maintexture.Width - ClientWidth) div 2, (Maintexture.Height - ClientHeight) div 2);
    if Selection >= 0 then
    begin
      if MyRO.Wonder[Selection].CityID = WonderNotBuiltYet then
      begin // not built yet
        { S:=Phrases.Lookup('IMPROVEMENTS',Selection);
          Canvas.Font.Color:=$000000;
          Canvas.TextOut(
          (ClientWidth-BiColorTextWidth(Canvas,S)) div 2+1,
          ClientHeight-3-36+1, S);
          Canvas.Font.Color:=MainTexture.ColorBevelLight;
          Canvas.TextOut(
          (ClientWidth-BiColorTextWidth(Canvas,S)) div 2,
          ClientHeight-3-36, S); }
      end
      else
      begin
        S := Phrases.Lookup('IMPROVEMENTS', Selection);
        if MyRO.Wonder[Selection].CityID <> WonderDestroyed then
          S := Format(Phrases.Lookup('WONDEROF'),
            [S, CityName(MyRO.Wonder[Selection].CityID)]);
        LoweredTextOut(Canvas, -1, MainTexture,
          (ClientWidth - BiColorTextWidth(Canvas, S)) div 2,
          ClientHeight - 3 - 36 - 10, S);
        if MyRO.Wonder[Selection].CityID = WonderDestroyed then
          S := Phrases.Lookup('DESTROYED')
        else if MyRO.Wonder[Selection].EffectiveOwner < 0 then
          S := Phrases.Lookup('EXPIRED')
        else
          S := Tribe[MyRO.Wonder[Selection].EffectiveOwner].TPhrase('WONDEROWNER');
        LoweredTextOut(Canvas, -1, MainTexture,
          (ClientWidth - BiColorTextWidth(Canvas, S)) div 2,
          ClientHeight - 3 - 36 + 10, S);
      end;
    end;
  end;
end;

procedure TWondersDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Selection >= 0 then
    HelpDlg.ShowNewContent(FWindowMode or wmPersistent, hkImp, Selection);
end;

end.
