unit UTexture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  { TTexture }

  TTexture = class
  private
    FAge: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetAge(AValue: Integer);
  public
    Image: TBitmap;
    ColorBevelLight: TColor;
    ColorBevelShade: TColor;
    ColorTextLight: TColor;
    ColorTextShade: TColor;
    ColorLitText: TColor;
    ColorMark: TColor;
    ColorPage: TColor;
    ColorCover: TColor;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TTexture);
    property Age: Integer read FAge write SetAge;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;


implementation

uses
  ScreenTools, Directories;

{ TTexture }

procedure TTexture.SetAge(AValue: Integer);
begin
  if FAge = AValue then Exit;
  FAge := AValue;
  LoadGraphicFile(Image, GetGraphicsDir + DirectorySeparator +
    'Texture' + IntToStr(Age + 1) + '.jpg');
  ColorBevelLight := Colors.Canvas.Pixels[clkAge0 + Age, cliBevelLight];
  ColorBevelShade := Colors.Canvas.Pixels[clkAge0 + Age, cliBevelShade];
  ColorTextLight := Colors.Canvas.Pixels[clkAge0 + Age, cliTextLight];
  ColorTextShade := Colors.Canvas.Pixels[clkAge0 + Age, cliTextShade];
  ColorLitText := Colors.Canvas.Pixels[clkAge0 + Age, cliLitText];
  ColorMark := Colors.Canvas.Pixels[clkAge0 + Age, cliMark];
  ColorPage := Colors.Canvas.Pixels[clkAge0 + Age, cliPage];
  ColorCover := Colors.Canvas.Pixels[clkAge0 + Age, cliCover];
end;

function TTexture.GetHeight: Integer;
begin
  Result := Image.Height;
end;

function TTexture.GetWidth: Integer;
begin
  Result := Image.Width;
end;

constructor TTexture.Create;
begin
  Image := TBitmap.Create;
  FAge := -2;
end;

destructor TTexture.Destroy;
begin
  FreeAndNil(Image);
  inherited;
end;

procedure TTexture.Assign(Source: TTexture);
begin
  FAge := Source.FAge;
  Image.Assign(Image);
  ColorBevelLight := Source.ColorBevelLight;
  ColorBevelShade := Source.ColorBevelShade;
  ColorTextLight := Source.ColorTextLight;
  ColorTextShade := Source.ColorTextShade;
  ColorLitText := Source.ColorLitText;
  ColorMark := Source.ColorMark;
  ColorPage := Source.ColorPage;
  ColorCover := Source.ColorCover;
end;

end.


