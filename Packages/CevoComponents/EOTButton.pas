unit EOTButton;

interface

uses
  ButtonBase, Classes, SysUtils, Graphics, LCLIntf, LCLType;

const
  eotBlinkOff = -1;
  eotCancel = 0;
  eotGray = 1;
  eotBlinkOn = 2;
  eotBackToNego = 3;

type
  TEOTButton = class(TButtonBase)
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetButtonIndexFast(x: integer);
    procedure SetBack(ca: TCanvas; x, y: integer);
  private
    FTemplate: TBitmap;
    FIndex: integer;
    procedure SetIndex(x: integer);
  public
    property Template: TBitmap read FTemplate write FTemplate;
  published
    property Visible;
    property ButtonIndex: integer read FIndex write SetIndex;
    property OnClick;
  protected
    Buffer, Back: TBitmap;
    procedure Paint; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TEOTButton]);
end;

procedure ImageOp_CBC(Dst, Src: TBitmap; xDst, yDst, xSrc, ySrc, w, h, Color0,
  Color2: integer);
// Src is template
// B channel = Color0 amp
// G channel = background amp (old Dst content), 128=original brightness
// R channel = Color2 amp
type
  TPixel = array [0 .. 2] of Byte;
var
  ix, iy, amp0, amp1, trans, Value: integer;
  SrcLine, DstLine: ^TPixel;
begin
  Src.BeginUpdate;
  Dst.BeginUpdate;
  for iy := 0 to h - 1 do
  begin
    SrcLine := Src.ScanLine[ySrc + iy] + xSrc * (Src.RawImage.Description.BitsPerPixel shr 3);
    DstLine := Dst.ScanLine[yDst + iy] + xDst * (Dst.RawImage.Description.BitsPerPixel shr 3);
    for ix := 0 to w - 1 do
    begin
      trans := SrcLine[0] * 2; // green channel = transparency
      amp0 := SrcLine[1] * 2;
      amp1 := SrcLine[2] * 2;
      if trans <> $FF then
      begin
        Value := (DstLine[0] * trans + (Color2 shr 16 and $FF) * amp1
          + (Color0 shr 16 and $FF) * amp0) div $FF;
        if Value < 256 then
          DstLine[0] := Value
        else
          DstLine[0] := 255;
        Value := (DstLine[1] * trans + (Color2 shr 8 and $FF) * amp1
          + (Color0 shr 8 and $FF) * amp0) div $FF;
        if Value < 256 then
          DstLine[1] := Value
        else
          DstLine[1] := 255;
        Value := (DstLine[2] * trans + (Color2 and $FF) * amp1 +
          (Color0 and $FF) * amp0) div $FF;
        if Value < 256 then
          DstLine[2] := Value
        else
          DstLine[2] := 255;
      end;
      SrcLine := Pointer(SrcLine) + (Src.RawImage.Description.BitsPerPixel shr 3);
      DstLine := Pointer(DstLine) + (Dst.RawImage.Description.BitsPerPixel shr 3);
    end;
  end;
  Src.EndUpdate;
  Dst.EndUpdate;
end;

constructor TEOTButton.Create;
begin
  inherited;
  Buffer := TBitmap.Create;
  Buffer.PixelFormat := pf24bit;
  Buffer.SetSize(48, 48);
  Buffer.Canvas.FillRect(0, 0, Buffer.Width, Buffer.Height);
  Back := TBitmap.Create;
  Back.PixelFormat := pf24bit;
  Back.SetSize(48, 48);
  Back.Canvas.FillRect(0, 0, Back.Width, Back.Height);
  ShowHint := true;
  SetBounds(0, 0, 48, 48);
end;

destructor TEOTButton.Destroy;
begin
  FreeAndNil(Buffer);
  FreeAndNil(Back);
  inherited;
end;

procedure TEOTButton.Paint;
begin
  with Canvas do
    if FGraphic <> nil then
    begin
      // TODO: For some reason BitBlt is not working with gray background here
      //BitBlt(Buffer.Canvas.Handle, 0, 0, 48, 48, Back.Canvas.Handle, 0,
      //  0, SRCCOPY);
      Buffer.Canvas.Draw(0, 0, Back);
      ImageOp_CBC(Buffer, Template, 0, 0, 133, 149 + 48 * Byte(FDown), 48, 48,
        $000000, $FFFFFF);
      if FIndex >= 0 then
        ImageOp_CBC(Buffer, Template, 8, 8, 1 + 32 * Byte(FIndex), 246, 32, 32,
          $000000, $FFFFFF);
      BitBlt(Canvas.Handle, 0, 0, 48, 48, Buffer.Canvas.Handle, 0, 0, SRCCOPY);
    end
    else
    begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 48, 48))
    end
end;

procedure TEOTButton.SetIndex(x: integer);
begin
  if x <> FIndex then
  begin
    FIndex := x;
    Invalidate
  end
end;

procedure TEOTButton.SetButtonIndexFast(x: integer);
begin
  if Visible and (x <> FIndex) then
  begin
    FIndex := x;
    try
      Paint
    except
    end
  end
end;

procedure TEOTButton.SetBack(ca: TCanvas; x, y: integer);
begin
  BitBlt(Back.Canvas.Handle, 0, 0, 48, 48, ca.Handle, x, y, SRCCOPY);
end;

end.
