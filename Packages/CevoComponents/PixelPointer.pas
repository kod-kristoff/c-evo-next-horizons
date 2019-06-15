unit PixelPointer;

interface

uses
  Classes, SysUtils, Graphics;

type
  TColor32 = type Cardinal;
  TColor32Component = (ccBlue, ccGreen, ccRed, ccAlpha);
  TPixel32 = packed record
    case Integer of
      0: (B, G, R, A: Byte);
      1: (ARGB: TColor32);
      2: (Planes: array[0..3] of Byte);
      3: (Components: array[TColor32Component] of Byte);
  end;
  PPixel32 = ^TPixel32;

  { TPixelPointer }

  TPixelPointer = record
    Base: PPixel32;
    Pixel: PPixel32;
    Line: PPixel32;
    RelLine: PPixel32;
    BytesPerPixel: Integer;
    BytesPerLine: Integer;
    procedure NextLine; inline; // Move pointer to start of new base line
    procedure NextPixel; inline; // Move pointer to next pixel
    procedure SetXY(X, Y: Integer); inline; // Set pixel position relative to base
    procedure SetX(X: Integer); inline; // Set horizontal pixel position relative to base
    procedure Init(Bitmap: TRasterImage; BaseX: Integer = 0; BaseY: Integer = 0); inline;
  end;
  PPixelPointer = ^TPixelPointer;


implementation

{ TPixelPointer }

procedure TPixelPointer.NextLine; inline;
begin
  Line := Pointer(Line) + BytesPerLine;
  Pixel := Line;
end;

procedure TPixelPointer.NextPixel; inline;
begin
  Pixel := Pointer(Pixel) + BytesPerPixel;
end;

procedure TPixelPointer.SetXY(X, Y: Integer); inline;
begin
  Line := Pointer(Base) + Y * BytesPerLine;
  SetX(X);
end;

procedure TPixelPointer.SetX(X: Integer); inline;
begin
  Pixel := Pointer(Line) + X * BytesPerPixel;
end;

procedure TPixelPointer.Init(Bitmap: TRasterImage; BaseX: Integer = 0;
  BaseY: Integer = 0); inline;
begin
  BytesPerLine := Bitmap.RawImage.Description.BytesPerLine;
  BytesPerPixel := Bitmap.RawImage.Description.BitsPerPixel shr 3;
  Base := PPixel32(Bitmap.RawImage.Data + BaseX * BytesPerPixel + BaseY * BytesPerLine);
  SetXY(0, 0);
end;


end.

