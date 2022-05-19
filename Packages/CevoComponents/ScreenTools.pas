unit ScreenTools;

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  StringTables, LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Math,
  Forms, Menus, GraphType, UGraphicSet, LazFileUtils, UTexture;

type
  TLoadGraphicFileOption = (gfNoError, gfNoGamma);
  TLoadGraphicFileOptions = set of TLoadGraphicFileOption;

  TFontType = (ftNormal, ftSmall, ftTiny, ftCaption, ftButton);

{$IFDEF WINDOWS}
function ChangeResolution(X, Y, bpp, freq: Integer): Boolean;
{$ENDIF}
procedure RestoreResolution;
procedure EmptyMenu(MenuItems: TMenuItem; Keep: Integer = 0);
function TurnToYear(Turn: Integer): Integer;
function TurnToString(Turn: Integer): string;
function MovementToString(Movement: Integer): string;
procedure BtnFrame(ca: TCanvas; P: TRect; T: TTexture);
procedure EditFrame(ca: TCanvas; P: TRect; T: TTexture);
function HexStringToColor(S: string): Integer;
function ExtractFileNameWithoutExt(const Filename: string): string;
function LoadGraphicFile(Bmp: TBitmap; FileName: string; Options: TLoadGraphicFileOptions = []): Boolean;
function LoadGraphicSet(const Name: string; Transparency: Boolean = True): TGraphicSet;
procedure Dump(dst: TBitmap; HGr: TGraphicSet; xDst, yDst, Width, Height, xGr, yGr: Integer);
procedure BitmapReplaceColor(Dst: TBitmap; X, Y, Width, Height: Integer; OldColor, NewColor: TColor);
procedure Sprite(Canvas: TCanvas; HGr: TGraphicSet; xDst, yDst, Width, Height, xGr, yGr: Integer);
  overload;
procedure Sprite(dst: TBitmap; HGr: TGraphicSet; xDst, yDst, Width, Height, xGr, yGr: Integer);
  overload;
procedure MakeBlue(Dst: TBitmap; X, Y, Width, Height: Integer);
procedure MakeRed(Dst: TBitmap; X, Y, Width, Height: Integer);
procedure ImageOp_B(dst, Src: TBitmap; xDst, yDst, xSrc, ySrc, Width, Height: Integer);
procedure ImageOp_BCC(Dst, Src: TBitmap;
  xDst, yDst, xSrc, ySrc, Width, Height, Color1, Color2: Integer); overload;
procedure ImageOp_BCC(Dst, Src: TBitmap;
  DstPos: TPoint; SrcRect: TRect; Color1, Color2: Integer); overload;
procedure ImageOp_CBC(Dst, Src: TBitmap; xDst, yDst, xSrc, ySrc, Width, Height,
  Color0, Color2: Integer);
procedure ImageOp_CCC(bmp: TBitmap; X, Y, Width, Height, Color0, Color1, Color2: Integer);
function BitBltCanvas(DestCanvas: TCanvas; X, Y, Width, Height: Integer;
  SrcCanvas: TCanvas; XSrc, YSrc: Integer; Rop: DWORD = SRCCOPY): Boolean; overload;
function BitBltCanvas(Dest: TCanvas; DestRect: TRect;
  Src: TCanvas; SrcPos: TPoint; Rop: DWORD = SRCCOPY): Boolean; overload;
function BitBltBitmap(Dest: TBitmap; X, Y, Width, Height: Integer;
  Src: TBitmap; XSrc, YSrc: Integer; Rop: DWORD = SRCCOPY): Boolean; overload;
function BitBltBitmap(Dest: TBitmap; DestRect: TRect;
  Src: TBitmap; SrcPos: TPoint; Rop: DWORD = SRCCOPY): Boolean; overload;
procedure SLine(ca: TCanvas; x0, x1, Y: Integer; cl: TColor);
procedure DLine(ca: TCanvas; x0, x1, Y: Integer; cl0, cl1: TColor);
procedure Frame(ca: TCanvas; x0, y0, x1, y1: Integer; cl0, cl1: TColor);
procedure RFrame(ca: TCanvas; x0, y0, x1, y1: Integer; cl0, cl1: TColor);
procedure CFrame(ca: TCanvas; x0, y0, x1, y1, Corner: Integer; cl: TColor);
procedure FrameImage(ca: TCanvas; Src: TBitmap;
  X, Y, Width, Height, xSrc, ySrc: Integer; IsControl: Boolean = False);
procedure GlowFrame(Dst: TBitmap; x0, y0, Width, Height: Integer; cl: TColor);
procedure InitOrnament;
procedure InitCityMark(T: TTexture);
procedure Fill(ca: TCanvas; Left, Top, Width, Height, xOffset, yOffset: Integer); overload;
procedure Fill(Canvas: TCanvas; Rect: TRect; Offset: TPoint); overload;
procedure FillLarge(ca: TCanvas; x0, y0, x1, y1, xm: Integer);
procedure FillSeamless(ca: TCanvas; Left, Top, Width, Height, xOffset, yOffset: Integer;
  const Texture: TBitmap);
procedure FillRectSeamless(ca: TCanvas; x0, y0, x1, y1, xOffset, yOffset: Integer;
  const Texture: TBitmap);
procedure PaintBackground(Form: TForm; Left, Top, Width, Height: Integer);
procedure Corner(ca: TCanvas; X, Y, Kind: Integer; T: TTexture);
procedure BiColorTextOut(ca: TCanvas; clMain, clBack: TColor; X, Y: Integer; S: string);
procedure LoweredTextOut(ca: TCanvas; cl: TColor; T: TTexture;
  X, Y: Integer; S: string);
function BiColorTextWidth(ca: TCanvas; S: string): Integer;
procedure RisedTextOut(ca: TCanvas; X, Y: Integer; S: string);
procedure LightGradient(ca: TCanvas; X, Y, Width, Color: Integer);
procedure DarkGradient(ca: TCanvas; X, Y, Width, Kind: Integer);
procedure VLightGradient(ca: TCanvas; X, Y, Height, Color: Integer);
procedure VDarkGradient(ca: TCanvas; X, Y, Height, Kind: Integer);
procedure UnderlinedTitleValue(Canvas: TCanvas; Title, Value: string; X, Y, Width: Integer);
procedure NumberBar(dst: TBitmap; X, Y: Integer; Cap: string; val: Integer;
  T: TTexture);
procedure CountBar(dst: TBitmap; X, Y, W: Integer; Kind: Integer;
  Cap: string; val: Integer; T: TTexture);
procedure PaintProgressBar(ca: TCanvas; Kind, X, Y, Pos, Growth, Max: Integer;
  T: TTexture);
procedure PaintRelativeProgressBar(ca: TCanvas;
  Kind, X, Y, size, Pos, Growth, Max: Integer; IndicateComplete: Boolean;
  T: TTexture);
procedure PaintLogo(Canvas: TCanvas; X, Y, LightColor, ShadeColor: Integer);
procedure LoadPhrases;
procedure Texturize(Dest, Texture: TBitmap; TransparentColor: Cardinal);
procedure DarkenImage(Bitmap: TBitmap; Change: Integer);
function ScaleToNative(Value: Integer): Integer;
function ScaleFromNative(Value: Integer): Integer;
procedure UnshareBitmap(Bitmap: TBitmap);
procedure Gtk2Fix;

const
  TransparentColor1 = $FF00FF;
  TransparentColor2 = $7F007F;

  // template positions in Templates.png
  xNation = 1;
  yNation = 25;
  xCoal = 1;
  yCoal = 148;

  // Icons.bmp structure
  xSizeBig = 56;
  ySizeBig = 40;

  GlowRange = 8;

  EmptySpaceColor = $101010;

  // color matrix
  clkAge0 = 1;
  cliTexture = 0;
  cliBevelLight = cliTexture + 1;
  cliBevelShade = cliTexture + 2;
  cliTextLight = cliTexture + 3;
  cliTextShade = cliTexture + 4;
  cliLitText = cliTexture + 5;
  cliMark = cliTexture + 6;
  cliDimmedText = cliTexture + 7;
  cliRoad = 8;
  cliHouse = cliRoad + 1;
  cliImp = cliRoad + 2;
  cliImpProject = cliRoad + 3;
  cliPage = 13;
  cliCover = cliPage + 1;
  clkMisc = 5;
  cliPaper = 0;
  cliPaperText = 1;
  cliPaperCaption = 2;
  clkCity = 6;
  cliPlains = 0;
  cliPrairie = 1;
  cliHills = 2;
  cliTundra = 3;
  cliWater = 4;

var
  Phrases: TStringTable;
  Phrases2: TStringTable;
  GrExt: TGraphicSets;

  HGrSystem: TGraphicSet;
  CityMark1: TGraphicSetItem;
  CityMark2: TGraphicSetItem;

  HGrSystem2: TGraphicSet;
  Ornament: TGraphicSetItem;
  GBrainNoTerm: TGraphicSetItem;
  GBrainSuperVirtual: TGraphicSetItem;
  GBrainTerm: TGraphicSetItem;
  GBrainRandom: TGraphicSetItem;

  Templates: TGraphicSet;
  Logo: TGraphicSetItem;
  BigBook: TGraphicSetItem;
  SmallBook: TGraphicSetItem;
  MenuLogo: TGraphicSetItem;
  LinkArrows: TGraphicSetItem;
  ScienceNationDot: TGraphicSetItem;
  ResearchIcon: TGraphicSetItem;
  ChangeIcon: TGraphicSetItem;
  TreasuryIcon: TGraphicSetItem;
  StarshipDeparted: TGraphicSetItem;
  WeightOn: TGraphicSetItem;
  WeightOff: TGraphicSetItem;

  ClickFrameColor: Integer;
  MainTexture: TTexture;
  Colors: TBitmap;
  Paper: TBitmap;
  BigImp: TBitmap;
  LogoBuffer: TBitmap;
  FullScreen: Boolean;
  GenerateNames: Boolean;
  InitOrnamentDone: Boolean;
  Phrases2FallenBackToEnglish: Boolean;

  UniFont: array [TFontType] of TFont;
  Gamma: Integer; // global gamma correction (cent)

procedure LoadAssets;
procedure UnitInit;
procedure UnitDone;
procedure InitGammaLookupTable;


implementation

uses
  Directories, Sound, UPixelPointer;

var
  {$IFDEF WINDOWS}
  StartResolution: TDeviceMode;
  ResolutionChanged: Boolean;
  {$ENDIF}

  GammaLookupTable: array [0..255] of Byte;

{$IFDEF WINDOWS}
function ChangeResolution(X, Y, bpp, freq: Integer): Boolean;
var
  DevMode: TDeviceMode;
begin
  EnumDisplaySettings(nil, 0, DevMode);
  DevMode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or
    DM_DISPLAYFREQUENCY;
  DevMode.dmPelsWidth := X;
  DevMode.dmPelsHeight := Y;
  DevMode.dmBitsPerPel := bpp;
  DevMode.dmDisplayFrequency := freq;
  Result := ChangeDisplaySettings(DevMode, 0) = DISP_CHANGE_SUCCESSFUL;
  if Result then
    ResolutionChanged := True;
end;

{$ENDIF}

procedure RestoreResolution;
begin
  {$IFDEF WINDOWS}
  if ResolutionChanged then
    ChangeDisplaySettings(StartResolution, 0);
  ResolutionChanged := False;
  {$ENDIF}
end;

procedure EmptyMenu(MenuItems: TMenuItem; Keep: Integer = 0);
var
  MenuItem: TMenuItem;
begin
  while MenuItems.Count > Keep do begin
    MenuItem := MenuItems[MenuItems.Count - 1];
    MenuItems.Delete(MenuItems.Count - 1);
    FreeAndNil(MenuItem);
  end;
end;

function TurnToYear(Turn: Integer): Integer;
begin
  Result := -4000;
  if Turn <= 0 then Exit;

  // Year -4000..-1000, Turn 0..60
  Inc(Result, Min(60, Turn) * 50);
  Dec(Turn, Min(60, Turn));
  if Turn = 0 then Exit;

  // Year -1000..0, Turn 60..100
  Inc(Result, Min(40, Turn) * 25);
  Dec(Turn, Min(40, Turn));
  if Turn = 0 then Exit;

  // Year 0..1500, Turn 100..175
  Inc(Result, Min(75, Turn) * 20);
  Dec(Turn, Min(75, Turn));
  if Turn = 0 then Exit;

  // Year 1500..1750, Turn 175..200
  Inc(Result, Min(25, Turn) * 10);
  Dec(Turn, Min(25, Turn));
  if Turn = 0 then Exit;

  // Year 1750..1850, Turn 200..250
  Inc(Result, Min(50, Turn) * 2);
  Dec(Turn, Min(50, Turn));
  if Turn = 0 then Exit;

  // Year 1850.., Turn 250..
  Inc(Result, Turn);
end;

function TurnToString(Turn: Integer): string;
var
  Year: Integer;
begin
  if GenerateNames then
  begin
    Year := TurnToYear(Turn);
    if Year < 0 then
      Result := Format(Phrases.Lookup('BC'), [-Year])
    else
      Result := Format(Phrases.Lookup('AD'), [Year]);
  end
  else
    Result := IntToStr(Turn);
end;

function MovementToString(Movement: Integer): string;
begin
  if Movement >= 1000 then
  begin
    Result := Char(48 + Movement div 1000);
    Movement := Movement mod 1000;
  end
  else
    Result := '';
  Result := Result + Char(48 + Movement div 100);
  Movement := Movement mod 100;
  if Movement > 0 then
  begin
    Result := Result + '.' + Char(48 + Movement div 10);
    Movement := Movement mod 10;
    if Movement > 0 then
      Result := Result + Char(48 + Movement);
  end;
end;

procedure BtnFrame(ca: TCanvas; P: TRect; T: TTexture);
begin
  RFrame(ca, P.Left - 1, P.Top - 1, P.Right, P.Bottom, T.ColorBevelShade,
    T.ColorBevelLight);
end;

procedure EditFrame(ca: TCanvas; P: TRect; T: TTexture);
begin
  Frame(ca, P.Left - 1, P.Top - 1, P.Right, P.Bottom, $000000, $000000);
  Frame(ca, P.Left - 2, P.Top - 2, P.Right + 1, P.Bottom + 1, $000000, $000000);
  Frame(ca, P.Left - 3, P.Top - 3, P.Right + 2, P.Bottom + 1, $000000, $000000);
  RFrame(ca, P.Left - 4, P.Top - 4, P.Right + 3, P.Bottom + 2, T.ColorBevelShade,
    T.ColorBevelLight);
end;

function HexCharToInt(X: Char): Integer;
begin
  case X of
    '0' .. '9': Result := Ord(X) - Ord('0');
    'A' .. 'F': Result := Ord(X) - Ord('A') + 10;
    'a' .. 'f': Result := Ord(X) - Ord('a') + 10;
    else Result := 0
  end;
end;

function HexStringToColor(S: string): Integer;
begin
  while (Length(S) > 0) and (S[1] = ' ') do
    Delete(S, 1, 1);
  S := S + '000000';
  if Gamma = 100 then
    Result := $10 * HexCharToInt(S[1]) + $1 * HexCharToInt(S[2]) +
      $1000 * HexCharToInt(S[3]) + $100 * HexCharToInt(S[4]) +
      $100000 * HexCharToInt(S[5]) + $10000 * HexCharToInt(S[6])
  else
    Result := GammaLookupTable[$10 * HexCharToInt(S[1]) + HexCharToInt(S[2])] +
      $100 * GammaLookupTable[$10 * HexCharToInt(S[3]) + HexCharToInt(S[4])] +
      $10000 * GammaLookupTable[$10 * HexCharToInt(S[5]) + HexCharToInt(S[6])];
end;

function ApplyGammaToPixel(Pixel: TPixel32): TPixel32;
begin
  Result.R := GammaLookupTable[Pixel.R];
  Result.G := GammaLookupTable[Pixel.G];
  Result.B := GammaLookupTable[Pixel.B];
end;

procedure ApplyGammaToBitmap(Bitmap: TBitmap);
var
  PixelPtr: TPixelPointer;
  X, Y: Integer;
begin
  Bitmap.BeginUpdate;
  PixelPtr := PixelPointer(Bitmap);
  for Y := 0 to ScaleToNative(Bitmap.Height) - 1 do begin
    for X := 0 to ScaleToNative(Bitmap.Width) - 1 do begin
      PixelPtr.Pixel^ := ApplyGammaToPixel(PixelPtr.Pixel^);
      PixelPtr.NextPixel;
    end;
    PixelPtr.NextLine;
  end;
  Bitmap.EndUpdate;
end;

procedure CopyGray8BitTo24bitBitmap(Dst, Src: TRasterImage);
var
  SrcPtr, DstPtr: TPixelPointer;
  X, Y: Integer;
begin
  //Dst.SetSize(Src.Width, Src.Height);
  SrcPtr := PixelPointer(Src);
  DstPtr := PixelPointer(Dst);
  for Y := 0 to ScaleToNative(Src.Height - 1) do begin
    for X := 0 to ScaleToNative(Src.Width - 1) do begin
      DstPtr.Pixel^.B := SrcPtr.Pixel^.B;
      DstPtr.Pixel^.G := SrcPtr.Pixel^.B;
      DstPtr.Pixel^.R := SrcPtr.Pixel^.B;
      SrcPtr.NextPixel;
      DstPtr.NextPixel;
    end;
    SrcPtr.NextLine;
    DstPtr.NextLine;
  end;
end;

function LoadGraphicFile(Bmp: TBitmap; FileName: string; Options:
  TLoadGraphicFileOptions = []): Boolean;
var
  Jpeg: TJpegImage;
  Png: TPortableNetworkGraphic;
begin
  Result := False;
  if ExtractFileExt(FileName) = '' then
    FileName := FileName + '.png';

  if FileExists(FileName) then begin
    if ExtractFileExt(FileName) = '.jpg' then begin
      Jpeg := TJpegImage.Create;
      try
        Jpeg.LoadFromFile(FileName);
        if not (gfNoGamma in Options) then
          Bmp.PixelFormat := pf24bit;
        Bmp.SetSize(Jpeg.Width, Jpeg.Height);
        Bmp.Canvas.Draw(0, 0, Jpeg);
        Result := True;
      except
        Result := False;
      end;
      FreeAndNil(Jpeg);
    end else
    if ExtractFileExt(FileName) = '.png' then begin
      Png := TPortableNetworkGraphic.Create;
      try
        Png.PixelFormat := Bmp.PixelFormat;
        Png.LoadFromFile(FileName);
        if not (gfNoGamma in Options) then
          Bmp.PixelFormat := pf24bit;
        Bmp.SetSize(Png.Width, Png.Height);
        if (Png.RawImage.Description.Format = ricfGray) then
        begin
          // LCL doesn't support 8-bit colors properly. Use 24-bit instead.
          Bmp.PixelFormat := pf24bit;
          CopyGray8BitTo24bitBitmap(Bmp, Png);
        end
        else
          Bmp.Canvas.Draw(0, 0, Png);
        Result := True;
      except
        Result := False;
      end;
      FreeAndNil(Png);
    end else
    if ExtractFileExt(FileName) = '.bmp' then begin
      try
        Bmp.LoadFromFile(FileName);
        if not (gfNoGamma in Options) then
          Bmp.PixelFormat := pf24bit;
        Result := True;
      except
        Result := False;
      end;
    end else
      raise Exception.Create('Unsupported image file type ' + ExtractFileExt(FileName));
  end;

  if not Result then begin
    if not (gfNoError in Options) then
      raise Exception.Create(Format(Phrases.Lookup('FILENOTFOUND'), [FileName]));
  end;

  if (not (gfNoGamma in Options)) and (Gamma <> 100) then
    ApplyGammaToBitmap(Bmp);
end;

function ExtractFileNameWithoutExt(const Filename: string): string;
var
  P: Integer;
begin
  Result := Filename;
  P := Length(Result);
  while P > 0 do begin
    case Result[P] of
      PathDelim: Exit;
      {$ifdef windows}
      '/': if ('/' in AllowDirectorySeparators) then Exit;
      {$endif}
      '.': Exit(Copy(Result, 1, P - 1));
    end;
    Dec(P);
  end;
end;

function LoadGraphicSet(const Name: string; Transparency: Boolean = True): TGraphicSet;
var
  X: Integer;
  Y: Integer;
  OriginalColor: Integer;
  FileName: string;
  DataPixel: TPixelPointer;
  MaskPixel: TPixelPointer;
begin
  Result := GrExt.SearchByName(Name);
  if not Assigned(Result) then begin
    Result := GrExt.AddNew(Name);
    FileName := GetGraphicsDir + DirectorySeparator + Name;
    // Do not apply gamma during file load as it would affect also transparency colors
    if not LoadGraphicFile(Result.Data, FileName, [gfNoGamma]) then begin
      Result := nil;
      Exit;
    end;

    FileName := ExtractFileNameWithoutExt(FileName) + GraphicSetFileExt;
    if FileExists(FileName) then
      Result.LoadFromFile(FileName);

    Result.ResetPixUsed;

    if Transparency then begin
      Result.Mask.SetSize(Result.Data.Width, Result.Data.Height);

      Result.Data.BeginUpdate;
      Result.Mask.BeginUpdate;
      DataPixel := PixelPointer(Result.Data);
      MaskPixel := PixelPointer(Result.Mask);
      for Y := 0 to ScaleToNative(Result.Data.Height) - 1 do begin
        for X := 0 to ScaleToNative(Result.Data.Width) - 1 do begin
          OriginalColor := DataPixel.Pixel^.ARGB and $FFFFFF;
          if (OriginalColor = TransparentColor1) or (OriginalColor = TransparentColor2) then begin
            MaskPixel.Pixel^.R := $FF;
            MaskPixel.Pixel^.G := $FF;
            MaskPixel.Pixel^.B := $FF;
            DataPixel.Pixel^.R := 0;
            DataPixel.Pixel^.G := 0;
            DataPixel.Pixel^.B := 0;
          end else begin
            MaskPixel.Pixel^.R := $00;
            MaskPixel.Pixel^.G := $00;
            MaskPixel.Pixel^.B := $00;
          end;
          DataPixel.NextPixel;
          MaskPixel.NextPixel;
        end;
        DataPixel.NextLine;
        MaskPixel.NextLine;
      end;
      Result.Data.EndUpdate;
      Result.Mask.EndUpdate;

      if Gamma <> 100 then
        ApplyGammaToBitmap(Result.Data);
    end;
  end;
end;

procedure Dump(dst: TBitmap; HGr: TGraphicSet; xDst, yDst, Width, Height, xGr, yGr: Integer);
begin
  BitBltCanvas(dst.Canvas, xDst, yDst, Width, Height,
    HGr.Data.Canvas, xGr, yGr);
end;

procedure BitmapReplaceColor(Dst: TBitmap; X, Y, Width, Height: Integer; OldColor, NewColor: TColor);
var
  XX, YY: Integer;
  PixelPtr: TPixelPointer;
begin
  Dst.BeginUpdate;
  PixelPtr := PixelPointer(Dst, ScaleToNative(X), ScaleToNative(Y));
  for YY := 0 to ScaleToNative(Height) - 1 do begin
    for XX := 0 to ScaleToNative(Width) - 1 do begin
      if PixelPtr.Pixel^.RGB = SwapRedBlue(OldColor) then begin
        PixelPtr.Pixel^.RGB := SwapRedBlue(NewColor);
      end;
      PixelPtr.NextPixel;
    end;
    PixelPtr.NextLine;
  end;
  Dst.EndUpdate;
end;

procedure MakeBlue(Dst: TBitmap; X, Y, Width, Height: Integer);
var
  XX, YY: Integer;
  PixelPtr: TPixelPointer;
begin
  Dst.BeginUpdate;
  PixelPtr := PixelPointer(Dst, ScaleToNative(X), ScaleToNative(Y));
  for yy := 0 to ScaleToNative(Height) - 1 do begin
    for xx := 0 to ScaleToNative(Width) - 1 do begin
      PixelPtr.Pixel^.B := PixelPtr.Pixel^.B div 2;
      PixelPtr.Pixel^.G := PixelPtr.Pixel^.G div 2;
      PixelPtr.Pixel^.R := PixelPtr.Pixel^.R div 2;
      PixelPtr.NextPixel;
    end;
    PixelPtr.NextLine;
  end;
  Dst.EndUpdate;
end;

procedure MakeRed(Dst: TBitmap; X, Y, Width, Height: Integer);
var
  XX, YY: Integer;
  Gray: Integer;
  PixelPtr: TPixelPointer;
begin
  Dst.BeginUpdate;
  PixelPtr := PixelPointer(Dst, ScaleToNative(X), ScaleToNative(Y));
  for YY := 0 to ScaleToNative(Height) - 1 do begin
    for XX := 0 to ScaleToNative(Width) - 1 do begin
      Gray := (Integer(PixelPtr.Pixel^.B) + Integer(PixelPtr.Pixel^.G) +
        Integer(PixelPtr.Pixel^.R)) * 85 shr 8;
      PixelPtr.Pixel^.B := 0;
      PixelPtr.Pixel^.G := 0;
      PixelPtr.Pixel^.R := Gray; // 255-(255-gray) div 2;
      PixelPtr.NextPixel;
    end;
    PixelPtr.NextLine;
  end;
  Dst.EndUpdate;
end;

procedure ImageOp_B(dst, Src: TBitmap; xDst, yDst, xSrc, ySrc, Width, Height: Integer);
// Src is template
// X channel = background amp (old Dst content), 128=original brightness
var
  X, Y: Integer;
  Brightness, Test: Integer;
  PixelSrc: TPixelPointer;
  PixelDst: TPixelPointer;
begin
  xDst := ScaleToNative(xDst);
  yDst := ScaleToNative(yDst);
  xSrc := ScaleToNative(xSrc);
  ySrc := ScaleToNative(ySrc);
  Width := ScaleToNative(Width);
  Height := ScaleToNative(Height);
  //Assert(Src.PixelFormat = pf8bit);
  Assert(dst.PixelFormat = pf24bit);
  if xDst < 0 then begin
    Width := Width + xDst;
    xSrc := xSrc - xDst;
    xDst := 0;
  end;
  if yDst < 0 then begin
    Height := Height + yDst;
    ySrc := ySrc - yDst;
    yDst := 0;
  end;
  if xDst + Width > ScaleToNative(dst.Width) then
    Width := ScaleToNative(dst.Width) - xDst;
  if yDst + Height > ScaleToNative(dst.Height) then
    Height := ScaleToNative(dst.Height) - yDst;
  if (Width < 0) or (Height < 0) then
    Exit;

  dst.BeginUpdate;
  Src.BeginUpdate;
  PixelDst := PixelPointer(Dst, xDst, yDst);
  PixelSrc := PixelPointer(Src, xSrc, ySrc);
  for Y := 0 to Height - 1 do begin
    for X := 0 to Width - 1 do  begin
      Brightness := PixelSrc.Pixel^.B; // One byte for 8-bit color
      Test := (PixelDst.Pixel^.R * Brightness) shr 7;
      if Test >= 256 then
        PixelDst.Pixel^.R := 255
      else
        PixelDst.Pixel^.R := Test; // Red
      Test := (PixelDst.Pixel^.G * Brightness) shr 7;
      if Test >= 256 then
        PixelDst.Pixel^.G := 255
      else
        PixelDst.Pixel^.G := Test; // Green
      Test := (PixelDst.Pixel^.B * Brightness) shr 7;
      if Test >= 256 then
        PixelDst.Pixel^.R := 255
      else
        PixelDst.Pixel^.B := Test; // Blue
      PixelDst.NextPixel;
      PixelSrc.NextPixel;
    end;
    PixelDst.NextLine;
    PixelSrc.NextLine;
  end;
  src.EndUpdate;
  dst.EndUpdate;
end;

procedure ImageOp_BCC(dst, Src: TBitmap; xDst, yDst, xSrc, ySrc, Width, Height,
  Color1, Color2: Integer);
// Src is template
// B channel = background amp (old Dst content), 128=original brightness
// G channel = Color1 amp, 128=original brightness
// R channel = Color2 amp, 128=original brightness
var
  ix, iy, amp1, amp2, trans, Value: Integer;
  SrcPixel: TPixelPointer;
  DstPixel: TPixelPointer;
begin
  xDst := ScaleToNative(xDst);
  yDst := ScaleToNative(yDst);
  xSrc := ScaleToNative(xSrc);
  ySrc := ScaleToNative(ySrc);
  Width := ScaleToNative(Width);
  Height := ScaleToNative(Height);
  if xDst < 0 then begin
    Width := Width + xDst;
    xSrc := xSrc - xDst;
    xDst := 0;
  end;
  if yDst < 0 then begin
    Height := Height + yDst;
    ySrc := ySrc - yDst;
    yDst := 0;
  end;
  if xDst + Width > ScaleToNative(dst.Width) then
    Width := ScaleToNative(dst.Width) - xDst;
  if yDst + Height > ScaleToNative(dst.Height) then
    Height := ScaleToNative(dst.Height) - yDst;
  if (Width < 0) or (Height < 0) then
    Exit;

  Src.BeginUpdate;
  dst.BeginUpdate;
  SrcPixel := PixelPointer(Src, xSrc, ySrc);
  DstPixel := PixelPointer(Dst, xDst, yDst);
  for iy := 0 to Height - 1 do begin
    for ix := 0 to Width - 1 do begin
      trans := SrcPixel.Pixel^.B * 2; // green channel = transparency
      amp1 := SrcPixel.Pixel^.G * 2;
      amp2 := SrcPixel.Pixel^.R * 2;
      if trans <> $FF then begin
        Value := (DstPixel.Pixel^.B * trans + ((Color2 shr 16) and $FF) *
          amp2 + ((Color1 shr 16) and $FF) * amp1) div $FF;
        DstPixel.Pixel^.B := Min(Value, 255);

        Value := (DstPixel.Pixel^.G * trans + ((Color2 shr 8) and $FF) *
          amp2 + ((Color1 shr 8) and $FF) * amp1) div $FF;
        DstPixel.Pixel^.G := Min(Value, 255);

        Value := (DstPixel.Pixel^.R * trans + (Color2 and $FF) *
          amp2 + (Color1 and $FF) * amp1) div $FF;
        DstPixel.Pixel^.R := Min(Value, 255);
      end;

      SrcPixel.NextPixel;
      DstPixel.NextPixel;
    end;
    SrcPixel.NextLine;
    DstPixel.NextLine;
  end;
  Src.EndUpdate;
  dst.EndUpdate;
end;

procedure ImageOp_BCC(Dst, Src: TBitmap; DstPos: TPoint; SrcRect: TRect;
  Color1, Color2: Integer);
begin
  ImageOp_BCC(Dst, Src, DstPos.X, DstPos.Y, SrcRect.Left, SrcRect.Top,
    SrcRect.Width, SrcRect.Height, Color1, Color2);
end;

procedure ImageOp_CBC(Dst, Src: TBitmap; xDst, yDst, xSrc, ySrc, Width, Height,
  Color0, Color2: Integer);
// Src is template
// B channel = Color0 amp
// G channel = background amp (old Dst content), 128=original brightness
// R channel = Color2 amp
var
  ix, iy, amp0, amp1, trans, Value: Integer;
  SrcPixel: TPixelPointer;
  DstPixel: TPixelPointer;
begin
  xDst := ScaleToNative(xDst);
  yDst := ScaleToNative(yDst);
  xSrc := ScaleToNative(xSrc);
  ySrc := ScaleToNative(ySrc);
  Width := ScaleToNative(Width);
  Height := ScaleToNative(Height);
  Src.BeginUpdate;
  Dst.BeginUpdate;
  SrcPixel := PixelPointer(Src, xSrc, ySrc);
  DstPixel := PixelPointer(Dst, xDst, yDst);
  for iy := 0 to Height - 1 do begin
    for ix := 0 to Width - 1 do begin
      trans := SrcPixel.Pixel^.B * 2; // green channel = transparency
      amp0 := SrcPixel.Pixel^.G * 2;
      amp1 := SrcPixel.Pixel^.R * 2;
      if trans <> $FF then begin
        Value := (DstPixel.Pixel^.B * trans + (Color2 shr 16 and $FF) * amp1 +
          (Color0 shr 16 and $FF) * amp0) div $FF;
        DstPixel.Pixel^.B := Min(Value, 255);

        Value := (DstPixel.Pixel^.G * trans + (Color2 shr 8 and $FF) * amp1 +
          (Color0 shr 8 and $FF) * amp0) div $FF;
        DstPixel.Pixel^.G := Min(Value, 255);

        Value := (DstPixel.Pixel^.R * trans + (Color2 and $FF) * amp1 +
          (Color0 and $FF) * amp0) div $FF;
        DstPixel.Pixel^.R := Min(Value, 255);
      end;
      SrcPixel.NextPixel;
      DstPixel.NextPixel;
    end;
    SrcPixel.NextLine;
    DstPixel.NextLine;
  end;
  Src.EndUpdate;
  Dst.EndUpdate;
end;

procedure ImageOp_CCC(bmp: TBitmap; X, Y, Width, Height, Color0, Color1, Color2: Integer);
// Bmp is template
// B channel = Color0 amp, 128=original brightness
// G channel = Color1 amp, 128=original brightness
// R channel = Color2 amp, 128=original brightness
var
  I, Red, Green: Integer;
  PixelPtr: TPixelPointer;
begin
  X := ScaleToNative(X);
  Y := ScaleToNative(Y);
  Width := ScaleToNative(Width);
  Height := ScaleToNative(Height);
  bmp.BeginUpdate;
  Assert(bmp.PixelFormat = pf24bit);
  Height := Y + Height;
  PixelPtr := PixelPointer(Bmp, X, Y);
  while Y < Height do begin
    for I := 0 to Width - 1 do begin
      Red := ((PixelPtr.Pixel^.B * (Color0 and $0000FF) + PixelPtr.Pixel^.G *
        (Color1 and $0000FF) + PixelPtr.Pixel^.R * (Color2 and $0000FF)) shr 8) and $ff;
      Green := ((PixelPtr.Pixel^.B * ((Color0 shr 8) and $0000FF) +
        PixelPtr.Pixel^.G * ((Color1 shr 8) and $0000FF) + PixelPtr.Pixel^.R *
        ((Color2 shr 8) and $0000FF)) shr 8) and $ff;
      PixelPtr.Pixel^.B := ((PixelPtr.Pixel^.B * ((Color0 shr 16) and $0000FF) +
        PixelPtr.Pixel^.G * ((Color1 shr 16) and $0000FF) + PixelPtr.Pixel^.R *
        ((Color2 shr 16) and $0000FF)) shr 8) and $ff; // Blue
      PixelPtr.Pixel^.G := Green;
      PixelPtr.Pixel^.R := Red;
      PixelPtr.NextPixel;
    end;
    Inc(Y);
    PixelPtr.NextLine;
  end;
  bmp.EndUpdate;
end;

procedure Sprite(Canvas: TCanvas; HGr: TGraphicSet; xDst, yDst, Width, Height, xGr, yGr: Integer);
begin
  BitBltCanvas(Canvas, xDst, yDst, Width, Height,
    HGr.Mask.Canvas, xGr, yGr, SRCAND);
  BitBltCanvas(Canvas, xDst, yDst, Width, Height,
    HGr.Data.Canvas, xGr, yGr, SRCPAINT);
end;

procedure Sprite(dst: TBitmap; HGr: TGraphicSet; xDst, yDst, Width, Height, xGr, yGr: Integer);
begin
  BitBltCanvas(dst.Canvas, xDst, yDst, Width, Height,
    HGr.Mask.Canvas, xGr, yGr, SRCAND);
  BitBltCanvas(dst.Canvas, xDst, yDst, Width, Height,
    HGr.Data.Canvas, xGr, yGr, SRCPAINT);
end;

function BitBltCanvas(DestCanvas: TCanvas; X, Y, Width, Height: Integer;
  SrcCanvas: TCanvas; XSrc, YSrc: Integer; Rop: DWORD = SRCCOPY): Boolean;
begin
  {$IFDEF WINDOWS}
  // LCLIntf.BitBlt is slower than direct Windows BitBlt
  Result := Windows.BitBlt(DestCanvas.Handle, X, Y, Width, Height, SrcCanvas.Handle, XSrc, YSrc, Rop);
  {$ELSE}
  Result := BitBlt(DestCanvas.Handle, X, Y, Width, Height, SrcCanvas.Handle, XSrc, YSrc, Rop);
  {$ENDIF}
end;

function BitBltCanvas(Dest: TCanvas; DestRect: TRect; Src: TCanvas;
  SrcPos: TPoint; Rop: DWORD): Boolean;
begin
  Result := BitBltCanvas(Dest, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height,
    Src, SrcPos.X, SrcPos.Y, Rop);
end;

function BitBltBitmap(Dest: TBitmap; X, Y, Width, Height: Integer;
  Src: TBitmap; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := BitBltCanvas(Dest.Canvas, X, Y, Width, Height, Src.Canvas, XSrc, YSrc, Rop);
end;

function BitBltBitmap(Dest: TBitmap; DestRect: TRect; Src: TBitmap;
  SrcPos: TPoint; Rop: DWORD): Boolean;
begin
  Result := BitBltCanvas(Dest.Canvas, DestRect, Src.Canvas, SrcPos, Rop);
end;

procedure SLine(ca: TCanvas; x0, x1, Y: Integer; cl: TColor);
begin
  with ca do begin
    Pen.Color := cl;
    MoveTo(x0, Y);
    LineTo(x1 + 1, Y);
  end;
end;

procedure DLine(ca: TCanvas; x0, x1, Y: Integer; cl0, cl1: TColor);
begin
  with ca do begin
    Pen.Color := cl0;
    MoveTo(x0, Y);
    LineTo(x1, Y);
    Pen.Color := cl1;
    MoveTo(x0 + 1, Y + 1);
    LineTo(x1 + 1, Y + 1);
    Pixels[x0, Y + 1] := cl0;
    Pixels[x1, Y] := cl1;
  end;
end;

procedure Frame(ca: TCanvas; x0, y0, x1, y1: Integer; cl0, cl1: TColor);
begin
  with ca do begin
    MoveTo(x0, y1);
    Pen.Color := cl0;
    LineTo(x0, y0);
    LineTo(x1, y0);
    Pen.Color := cl1;
    LineTo(x1, y1);
    LineTo(x0, y1);
  end;
end;

procedure RFrame(ca: TCanvas; x0, y0, x1, y1: Integer; cl0, cl1: TColor);
begin
  with ca do begin
    Pen.Color := cl0;
    MoveTo(x0, y0 + 1);
    LineTo(x0, y1);
    MoveTo(x0 + 1, y0);
    LineTo(x1, y0);
    Pen.Color := cl1;
    MoveTo(x1, y0 + 1);
    LineTo(x1, y1);
    MoveTo(x0 + 1, y1);
    LineTo(x1, y1);
  end;
end;

procedure CFrame(ca: TCanvas; x0, y0, x1, y1, Corner: Integer; cl: TColor);
begin
  with ca do begin
    Pen.Color := cl;
    MoveTo(x0, y0 + Corner - 1);
    LineTo(x0, y0);
    LineTo(x0 + Corner, y0);
    MoveTo(x1, y0 + Corner - 1);
    LineTo(x1, y0);
    LineTo(x1 - Corner, y0);
    MoveTo(x1, y1 - Corner + 1);
    LineTo(x1, y1);
    LineTo(x1 - Corner, y1);
    MoveTo(x0, y1 - Corner + 1);
    LineTo(x0, y1);
    LineTo(x0 + Corner, y1);
  end;
end;

procedure FrameImage(ca: TCanvas; Src: TBitmap;
  X, Y, Width, Height, xSrc, ySrc: Integer; IsControl: Boolean = False);
begin
  if IsControl then begin
    Frame(ca, X - 1, Y - 1, X + Width, Y + Height, $B0B0B0, $FFFFFF);
    RFrame(ca, X - 2, Y - 2, X + Width + 1, Y + Height + 1, $FFFFFF, $B0B0B0);
  end else
    Frame(ca, X - 1, Y - 1, X + Width, Y + Height, $000000, $000000);
  BitBltCanvas(ca, X, Y, Width, Height, Src.Canvas, xSrc, ySrc);
end;

procedure GlowFrame(Dst: TBitmap; x0, y0, Width, Height: Integer; cl: TColor);
var
  X, Y, ch, R: Integer;
  DstPtr: TPixelPointer;
  DpiGlowRange: Integer;
begin
  DpiGlowRange := ScaleToNative(GlowRange);
  X0 := ScaleToNative(X0);
  Y0 := ScaleToNative(Y0);
  Width := ScaleToNative(Width);
  Height := ScaleToNative(Height);
  Dst.BeginUpdate;
  DstPtr := PixelPointer(Dst, x0 - DpiGlowRange + 1, y0 - DpiGlowRange + 1);
  for Y := -DpiGlowRange + 1 to Height - 1 + DpiGlowRange - 1 do begin
    for X := -DpiGlowRange + 1 to Width - 1 + DpiGlowRange - 1 do begin
      if X < 0 then
        if Y < 0 then
          R := round(sqrt(sqr(X) + sqr(Y)))
        else if Y >= Height then
          R := round(sqrt(sqr(X) + sqr(Y - (Height - 1))))
        else
          R := -X
      else if X >= Width then
        if Y < 0 then
          R := round(sqrt(sqr(X - (Width - 1)) + sqr(Y)))
        else if Y >= Height then
          R := round(sqrt(sqr(X - (Width - 1)) + sqr(Y - (Height - 1))))
        else
          R := X - (Width - 1)
      else if Y < 0 then
        R := -Y
      else if Y >= Height then
        R := Y - (Height - 1)
      else begin
        DstPtr.NextPixel;
        continue;
      end;
      if R = 0 then
        R := 1;
      if R < DpiGlowRange then
        for ch := 0 to 2 do
          DstPtr.Pixel^.Planes[2 - ch] :=
            (DstPtr.Pixel^.Planes[2 - ch] * (R - 1) + (cl shr (8 * ch) and $FF) *
            (DpiGlowRange - R)) div (DpiGlowRange - 1);
      DstPtr.NextPixel;
    end;
    DstPtr.NextLine;
  end;
  Dst.EndUpdate;
end;

procedure InitOrnament;
var
  P: TColor;
  X, Y: Integer;
  Light, Shade: TColor32;
  PixelPtr: TPixelPointer;
begin
  if InitOrnamentDone then Exit;
  Light := ColorToColor32(MainTexture.ColorBevelLight);
  // and $FCFCFC shr 2*3+MainTexture.ColorBevelShade and $FCFCFC shr 2;
  Shade := ColorToColor32(MainTexture.ColorBevelShade and $FCFCFC shr 2 * 3 +
    MainTexture.ColorBevelLight and $FCFCFC shr 2);
  HGrSystem2.Data.BeginUpdate;
  PixelPtr := PixelPointer(HGrSystem2.Data, ScaleToNative(Ornament.Left),
    ScaleToNative(Ornament.Top));
  if PixelPtr.BytesPerPixel = 3 then begin
    for Y := 0 to ScaleToNative(Ornament.Height) - 1 do begin
      for X := 0 to ScaleToNative(Ornament.Width) - 1 do begin
        P := Color32ToColor(PixelPtr.Pixel^.RGB);
        if P = $0000FF then PixelPtr.Pixel^.RGB := Light
        else if P = $FF0000 then PixelPtr.Pixel^.RGB := Shade;
        PixelPtr.NextPixel;
      end;
      PixelPtr.NextLine;
    end;
  end else begin
    for Y := 0 to ScaleToNative(Ornament.Height) - 1 do begin
      for X := 0 to ScaleToNative(Ornament.Width) - 1 do begin
        P := Color32ToColor(PixelPtr.Pixel^.ARGB);
        if P = $0000FF then PixelPtr.Pixel^.ARGB := Light
        else if P = $FF0000 then PixelPtr.Pixel^.ARGB := Shade;
        PixelPtr.NextPixel;
      end;
      PixelPtr.NextLine;
    end;
  end;
  InitOrnamentDone := True;
  HGrSystem2.Data.EndUpdate;
end;

procedure InitCityMark(T: TTexture);
var
  X: Integer;
  Y: Integer;
  Intensity: Integer;
begin
  for X := 0 to CityMark1.Width - 1 do begin
    for Y := 0 to CityMark1.Height - 1 do begin
      if HGrSystem.Mask.Canvas.Pixels[CityMark1.Left + X, CityMark1.Top + Y] = 0 then
      begin
        Intensity := HGrSystem.Data.Canvas.Pixels[CityMark1.Left +
          X, CityMark1.Top + Y] and $FF;
        HGrSystem.Data.Canvas.Pixels[CityMark2.Left + X, CityMark2.Top + Y] :=
          T.ColorMark and $FF * Intensity div $FF + T.ColorMark shr 8 and
          $FF * Intensity div $FF shl 8 + T.ColorMark shr 16 and
          $FF * Intensity div $FF shl 16;
      end;
    end;
  end;
  BitBltCanvas(HGrSystem.Mask.Canvas, CityMark2.Left, CityMark2.Top, CityMark1.Width, CityMark1.Width,
    HGrSystem.Mask.Canvas, CityMark1.Left, CityMark1.Top);
end;

procedure Fill(ca: TCanvas; Left, Top, Width, Height, xOffset, yOffset: Integer);
begin
  Assert((Left + xOffset >= 0) and (Left + xOffset + Width <= MainTexture.Width) and
    (Top + yOffset >= 0) and (Top + yOffset + Height <= MainTexture.Height));
  BitBltCanvas(ca, Left, Top, Width, Height, MainTexture.Image.Canvas,
    Left + xOffset, Top + yOffset);
end;

procedure Fill(Canvas: TCanvas; Rect: TRect; Offset: TPoint);
begin
  Fill(Canvas, Rect.Left, Rect.Top, Rect.Width, Rect.Height, Offset.X, Offset.Y);
end;

procedure FillLarge(ca: TCanvas; x0, y0, x1, y1, xm: Integer);

  function Band(I: Integer): Integer;
  var
    N: Integer;
  begin
    N := ((MainTexture.Height div 2) div (y1 - y0)) * 2;
    while MainTexture.Height div 2 + (I + 1) * (y1 - y0) > MainTexture.Height do
      Dec(I, N);
    while MainTexture.Height div 2 + I * (y1 - y0) < 0 do
      Inc(I, N);
    Result := I;
  end;

var
  I: Integer;
begin
  for I := 0 to (x1 - xm) div MainTexture.Width - 1 do
    BitBltCanvas(ca, xm + I * MainTexture.Width, y0, MainTexture.Width, y1 - y0,
      MainTexture.Image.Canvas, 0, MainTexture.Height div 2 + Band(I) *
      (y1 - y0));
  BitBltCanvas(ca, xm + ((x1 - xm) div MainTexture.Width) * MainTexture.Width, y0,
    x1 - (xm + ((x1 - xm) div MainTexture.Width) * MainTexture.Width), y1 - y0,
    MainTexture.Image.Canvas, 0, MainTexture.Height div 2 + Band(
    (x1 - xm) div MainTexture.Width) * (y1 - y0));
  for I := 0 to (xm - x0) div MainTexture.Width - 1 do
    BitBltCanvas(ca, xm - (I + 1) * MainTexture.Width, y0, MainTexture.Width, y1 - y0,
      MainTexture.Image.Canvas, 0, MainTexture.Height div 2 +
      Band(-I - 1) * (y1 - y0));
  BitBltCanvas(ca, x0, y0, xm - ((xm - x0) div MainTexture.Width) *
    MainTexture.Width - x0, y1 - y0, MainTexture.Image.Canvas,
    ((xm - x0) div MainTexture.Width + 1) * MainTexture.Width - (xm - x0),
    MainTexture.Height div 2 + Band(-(xm - x0) div MainTexture.Width - 1) * (y1 - y0));
end;

procedure FillSeamless(ca: TCanvas; Left, Top, Width, Height, xOffset, yOffset: Integer;
  const Texture: TBitmap);
var
  X, Y, x0cut, y0cut, x1cut, y1cut: Integer;
begin
  while xOffset < 0 do
    Inc(xOffset, Texture.Width);
  while yOffset < 0 do
    Inc(yOffset, Texture.Height);
  for Y := (Top + yOffset) div Texture.Height to (Top + yOffset + Height - 1) div
    Texture.Height do
  begin
    y0cut := Top + yOffset - Y * Texture.Height;
    if y0cut < 0 then
      y0cut := 0;
    y1cut := (Y + 1) * Texture.Height - (Top + yOffset + Height);
    if y1cut < 0 then
      y1cut := 0;
    for X := (Left + xOffset) div Texture.Width to (Left + xOffset + Width - 1) div
      Texture.Width do
    begin
      x0cut := Left + xOffset - X * Texture.Width;
      if x0cut < 0 then
        x0cut := 0;
      x1cut := (X + 1) * Texture.Width - (Left + xOffset + Width);
      if x1cut < 0 then
        x1cut := 0;
      BitBltCanvas(ca, X * Texture.Width + x0cut - xOffset,
        Y * Texture.Height + y0cut - yOffset, Texture.Width - x0cut - x1cut,
        Texture.Height - y0cut - y1cut, Texture.Canvas, x0cut, y0cut);
    end;
  end;
end;

procedure FillRectSeamless(ca: TCanvas; x0, y0, x1, y1, xOffset, yOffset: Integer;
  const Texture: TBitmap);
begin
  FillSeamless(ca, x0, y0, x1 - x0, y1 - y0, xOffset, yOffset, Texture);
end;

procedure PaintBackground(Form: TForm; Left, Top, Width, Height: Integer);
begin
  Fill(Form.Canvas, Left, Top, Width, Height, (MainTexture.Width - Form.ClientWidth) div
    2, (MainTexture.Height - Form.ClientHeight) div 2);
end;

procedure Corner(ca: TCanvas; X, Y, Kind: Integer; T: TTexture);
begin
  { BitBltCanvas(ca,x,y,8,8,T.HGr.Mask.Canvas,
    T.xGr+29+Kind*9,T.yGr+89,SRCAND);
    BitBltCanvas(ca,X,Y,8,8,T.HGr.Data.Canvas,
    T.xGr+29+Kind*9,T.yGr+89,SRCPAINT); }
end;

procedure BiColorTextOut(ca: TCanvas; clMain, clBack: TColor; X, Y: Integer; S: string);

  procedure PaintIcon(X, Y, Kind: Integer);
  begin
    BitBltCanvas(ca, X, Y + 6, 10, 10, HGrSystem.Mask.Canvas,
      66 + Kind mod 11 * 11, 115 + Kind div 11 * 11, SRCAND);
    BitBltCanvas(ca, X, Y + 6, 10, 10, HGrSystem.Data.Canvas,
      66 + Kind mod 11 * 11, 115 + Kind div 11 * 11, SRCPAINT);
  end;

var
  P, xp: Integer;
  sp: string;
  shadow: Boolean;
  Text: string;
begin
  Inc(X);
  Inc(Y);
  for shadow := True downto False do
    with ca do
      if not shadow or (clBack <> $7F007F) then
      begin
        if shadow then
          Font.Color := clBack
        else
          Font.Color := clMain;
        sp := S;
        xp := X;
        repeat
          P := Pos('%', sp);
          if (P = 0) or (P + 1 > Length(sp)) or not
            (sp[P + 1] in ['c', 'f', 'l', 'm', 'n', 'o', 'p', 'r', 't', 'w']) then
          begin
            ca.Textout(xp, Y, sp);
            Break;
          end
          else
          begin
            Text := Copy(sp, 1, P - 1);
            Textout(xp, Y, Text);
            Inc(xp, ca.TextWidth(Text));
            if not shadow then
              case sp[P + 1] of
                'c': PaintIcon(xp + 1, Y, 6);
                'f': PaintIcon(xp + 1, Y, 0);
                'l': PaintIcon(xp + 1, Y, 8);
                'm': PaintIcon(xp + 1, Y, 17);
                'n': PaintIcon(xp + 1, Y, 7);
                'o': PaintIcon(xp + 1, Y, 16);
                'p': PaintIcon(xp + 1, Y, 2);
                'r': PaintIcon(xp + 1, Y, 12);
                't': PaintIcon(xp + 1, Y, 4);
                'w': PaintIcon(xp + 1, Y, 13);
              end;
            Inc(xp, 10);
            Delete(sp, 1, P + 1);
          end;
        until False;
        Dec(X);
        Dec(Y);
      end;
end;

function BiColorTextWidth(ca: TCanvas; S: string): Integer;
var
  P: Integer;
begin
  Result := 1;
  repeat
    P := Pos('%', S);
    if (P = 0) or (P = Length(S)) then
    begin
      Inc(Result, ca.TextWidth(S));
      Break;
    end
    else
    begin
      if not (S[P + 1] in ['c', 'f', 'l', 'm', 'n', 'o', 'p', 'r', 't', 'w'])
      then
        Inc(Result, ca.TextWidth(Copy(S, 1, P + 1)))
      else
        Inc(Result, ca.TextWidth(Copy(S, 1, P - 1)) + 10);
      Delete(S, 1, P + 1);
    end;
  until False;
end;

procedure LoweredTextOut(ca: TCanvas; cl: TColor; T: TTexture;
  X, Y: Integer; S: string);
begin
  if cl = -2 then
    BiColorTextOut(ca, (T.ColorBevelShade and $FEFEFE) shr 1,
      T.ColorBevelLight, X, Y, S)
  else if cl < 0 then
    BiColorTextOut(ca, T.ColorTextShade, T.ColorTextLight, X, Y, S)
  else
    BiColorTextOut(ca, cl, T.ColorTextLight, X, Y, S);
end;

procedure RisedTextOut(ca: TCanvas; X, Y: Integer; S: string);
begin
  BiColorTextOut(ca, $FFFFFF, $000000, X, Y, S);
end;

procedure Gradient(ca: TCanvas; X, Y, dx, dy, Width, Height, Color: Integer;
  Brightness: array of Integer);
var
  I, R, G, B: Integer;
begin
  for I := 0 to Length(Brightness) - 1 do begin // gradient
    R := Color and $FF + Brightness[I];
    if R < 0 then
      R := 0
    else if R >= 256 then
      R := 255;
    G := Color shr 8 and $FF + Brightness[I];
    if G < 0 then
      G := 0
    else if G >= 256 then
      G := 255;
    B := Color shr 16 and $FF + Brightness[I];
    if B < 0 then
      B := 0
    else if B >= 256 then
      B := 255;
    ca.Pen.Color := R + G shl 8 + B shl 16;
    ca.MoveTo(X + dx * I, Y + dy * I);
    ca.LineTo(X + dx * I + Width, Y + dy * I + Height);
  end;
  ca.Pen.Color := $000000;
  ca.MoveTo(X + 1, Y + 16 * dy + Height);
  ca.LineTo(X + 16 * dx + Width, Y + 16 * dy + Height);
  ca.LineTo(X + 16 * dx + Width, Y);
end;

procedure LightGradient(ca: TCanvas; X, Y, Width, Color: Integer);
const
  Brightness: array [0 .. 15] of Integer =
    (16, 12, 8, 4, 0, -4, -8, -12, -16, -20, -24, -28, -32, -36, -40, -44);
begin
  Gradient(ca, X, Y, 0, 1, Width, 0, Color, Brightness);
end;

procedure DarkGradient(ca: TCanvas; X, Y, Width, Kind: Integer);
const
  Brightness: array [0 .. 15] of Integer =
    (16, 12, 8, 4, 0, -4, -8, -12 - 24, -16 + 16, -20, -24, -28, -32, -36, -40, -44);
begin
  Gradient(ca, X, Y, 0, 1, Width, 0, HGrSystem.Data.Canvas.Pixels
    [187, 137 + Kind], Brightness);
end;

procedure VLightGradient(ca: TCanvas; X, Y, Height, Color: Integer);
const
  Brightness: array [0 .. 15] of Integer =
    (16, 12, 8, 4, 0, -4, -8, -12, -16, -20, -24, -28, -32, -36, -40, -44);
begin
  Gradient(ca, X, Y, 1, 0, 0, Height, Color, Brightness);
end;

procedure VDarkGradient(ca: TCanvas; X, Y, Height, Kind: Integer);
const
  Brightness: array [0 .. 15] of Integer =
    (16, 12, 8, 4, 0, -4, -8, -12 - 24, -16 + 16, -20, -24, -28, -32, -36, -40, -44);
begin
  Gradient(ca, X, Y, 1, 0, 0, Height,
    HGrSystem.Data.Canvas.Pixels[187, 137 + Kind], Brightness);
end;

procedure UnderlinedTitleValue(Canvas: TCanvas; Title, Value: string; X, Y, Width: Integer);
begin
  DLine(Canvas, X, X + Width, Y + 19, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  RisedTextOut(Canvas, X, Y, Title);
  RisedTextOut(Canvas, X + Width - BiColorTextWidth(Canvas, Value), Y, Value);
end;

procedure NumberBar(dst: TBitmap; X, Y: Integer; Cap: string;
  val: Integer; T: TTexture);
var
  S: string;
begin
  if val > 0 then
  begin
    DLine(dst.Canvas, X - 2, X + 170, Y + 16, T.ColorBevelShade,
      T.ColorBevelLight);
    LoweredTextOut(dst.Canvas, -1, T, X - 2, Y, Cap);
    S := IntToStr(val);
    RisedTextOut(dst.Canvas, X + 170 - BiColorTextWidth(dst.Canvas,
      S), Y, S);
  end;
end;

procedure CountBar(dst: TBitmap; X, Y, W: Integer; Kind: Integer;
  Cap: string; val: Integer; T: TTexture);
var
  I, sd, ld, cl, xIcon, yIcon: Integer;
  S: string;
begin
  // val:=random(40); //!!!
  if val = 0 then
    Exit;
  Assert(Kind >= 0);
  with dst.Canvas do
  begin
    // xIcon:=x+100;
    // yIcon:=y;
    // DLine(dst.Canvas,x-2,x+170+32,y+16,T.ColorBevelShade,T.ColorBevelLight);

    xIcon := X - 5;
    yIcon := Y + 15;
    DLine(dst.Canvas, X - 2, xIcon + W + 2, yIcon + 16, T.ColorBevelShade,
      T.ColorBevelLight);

    S := IntToStr(val);
    if val < 0 then
      cl := $0000FF
    else
      cl := -1;
    LoweredTextOut(dst.Canvas, cl, T, X - 2, Y, Cap);
    LoweredTextOut(dst.Canvas, cl, T,
      xIcon + W + 2 - BiColorTextWidth(dst.Canvas, S), yIcon, S);

    if (Kind = 12) and (val >= 100) then
    begin // science with symbol for 100
      val := val div 10;
      sd := 14 * (val div 10 + val mod 10 - 1);
      if sd = 0 then
        sd := 1;
      if sd < W - 44 then
        ld := sd
      else
        ld := W - 44;
      for I := 0 to val mod 10 - 1 do
      begin
        BitBltCanvas(dst.Canvas, xIcon + 4 + I * (14 * ld div sd), yIcon + 2 + 1, 14,
          14, HGrSystem.Mask.Canvas, 67 + Kind mod 8 * 15,
          70 + Kind div 8 * 15, SRCAND);
        Sprite(dst, HGrSystem, xIcon + 3 + I * (14 * ld div sd), yIcon + 2,
          14, 14, 67 + Kind mod 8 * 15, 70 + Kind div 8 * 15);
      end;
      for I := 0 to val div 10 - 1 do
      begin
        BitBltCanvas(dst.Canvas, xIcon + 4 + (val mod 10) *
          (14 * ld div sd) + I * (14 * ld div sd), yIcon + 3, 14, 14,
          HGrSystem.Mask.Canvas, 67 + 7 mod 8 * 15,
          70 + 7 div 8 * 15, SRCAND);
        Sprite(dst, HGrSystem, xIcon + 3 + (val mod 10) *
          (14 * ld div sd) + I * (14 * ld div sd), yIcon + 2, 14,
          14, 67 + 7 mod 8 * 15,
          70 + 7 div 8 * 15);
      end;
    end
    else
    begin
      val := abs(val);
      if val mod 10 = 0 then
        sd := 14 * (val div 10 - 1)
      else
        sd := 10 * (val mod 10 - 1) + 14 * (val div 10);
      if sd = 0 then
        sd := 1;
      if sd < W - 44 then
        ld := sd
      else
        ld := W - 44;
      for I := 0 to val div 10 - 1 do
      begin
        BitBltCanvas(dst.Canvas, xIcon + 4 + I * (14 * ld div sd), yIcon + 3, 14, 14,
          HGrSystem.Mask.Canvas, 67 + Kind mod 8 * 15,
          70 + Kind div 8 * 15, SRCAND);
        Sprite(dst, HGrSystem, xIcon + 3 + I * (14 * ld div sd), yIcon + 2,
          14, 14, 67 + Kind mod 8 * 15, 70 + Kind div 8 * 15);
      end;
      for I := 0 to val mod 10 - 1 do
      begin
        BitBltCanvas(dst.Canvas, xIcon + 4 + (val div 10) *
          (14 * ld div sd) + I * (10 * ld div sd), yIcon + 7, 10, 10,
          HGrSystem.Mask.Canvas, 66 + Kind mod 11 * 11,
          115 + Kind div 11 * 11, SRCAND);
        Sprite(dst, HGrSystem, xIcon + 3 + (val div 10) *
          (14 * ld div sd) + I * (10 * ld div sd), yIcon + 6, 10,
          10, 66 + Kind mod 11 * 11,
          115 + Kind div 11 * 11);
      end;
    end;
  end;
end;

procedure PaintProgressBar(ca: TCanvas; Kind, X, Y, Pos, Growth, Max: Integer;
  T: TTexture);
var
  I: Integer;
begin
  if Pos > Max then
    Pos := Max;
  if Growth < 0 then
  begin
    Pos := Pos + Growth;
    if Pos < 0 then
    begin
      Growth := Growth - Pos;
      Pos := 0;
    end;
  end
  else if Pos + Growth > Max then
    Growth := Max - Pos;
  Frame(ca, X - 1, Y - 1, X + Max, Y + 7, $000000, $000000);
  RFrame(ca, X - 2, Y - 2, X + Max + 1, Y + 8, T.ColorBevelShade,
    T.ColorBevelLight);
  with ca do
  begin
    for I := 0 to Pos div 8 - 1 do
      BitBltCanvas(ca, X + I * 8, Y, 8, 7,
        HGrSystem.Data.Canvas, 104, 9 + 8 * Kind);
    BitBltCanvas(ca, X + 8 * (Pos div 8), Y, Pos - 8 * (Pos div 8), 7,
      HGrSystem.Data.Canvas, 104, 9 + 8 * Kind);
    if Growth > 0 then
    begin
      for I := 0 to Growth div 8 - 1 do
        BitBltCanvas(ca, X + Pos + I * 8, Y, 8, 7,
          HGrSystem.Data.Canvas, 112, 9 + 8 * Kind);
      BitBltCanvas(ca, X + Pos + 8 * (Growth div 8), Y,
        Growth - 8 * (Growth div 8), 7, HGrSystem.Data.Canvas,
        112, 9 + 8 * Kind);
    end
    else if Growth < 0 then
    begin
      for I := 0 to -Growth div 8 - 1 do
        BitBltCanvas(ca, X + Pos + I * 8, Y, 8, 7,
          HGrSystem.Data.Canvas, 104, 1);
      BitBltCanvas(ca, X + Pos + 8 * (-Growth div 8), Y, -Growth -
        8 * (-Growth div 8), 7,
        HGrSystem.Data.Canvas, 104, 1);
    end;
    Brush.Color := $000000;
    FillRect(Rect(X + Pos + abs(Growth), Y, X + Max, Y + 7));
    Brush.Style := bsClear;
  end;
end;

// pos and growth are relative to max, set size independent
procedure PaintRelativeProgressBar(ca: TCanvas;
  Kind, X, Y, size, Pos, Growth, Max: Integer; IndicateComplete: Boolean;
  T: TTexture);
begin
  if Growth > 0 then
    PaintProgressBar(ca, Kind, X, Y, Pos * size div Max,
      (Growth * size + Max div 2) div Max, size, T)
  else
    PaintProgressBar(ca, Kind, X, Y, Pos * size div Max,
      (Growth * size - Max div 2) div Max, size, T);
  if IndicateComplete and (Pos + Growth >= Max) then
    Sprite(ca, HGrSystem, X + size - 10, Y - 7, 23, 16, 1, 129);
end;

procedure PaintLogo(Canvas: TCanvas; X, Y, LightColor, ShadeColor: Integer);
begin
  UnshareBitmap(LogoBuffer);
  BitBltCanvas(LogoBuffer.Canvas, 0, 0, Logo.Width, Logo.Height, Canvas, X, Y);
  ImageOp_BCC(LogoBuffer, Templates.Data, Point(0, 0), Logo.BoundsRect,
    LightColor, ShadeColor);
  BitBltCanvas(Canvas, X, Y, Logo.Width, Logo.Height, LogoBuffer.Canvas, 0, 0);
end;

procedure LoadPhrases;
begin
  if Phrases = nil then Phrases := TStringTable.Create;
  if Phrases2 = nil then Phrases2 := TStringTable.Create;
  Phrases2FallenBackToEnglish := False;
  if FileExists(LocalizedFilePath('Language.txt')) then
  begin
    Phrases.LoadFromFile(LocalizedFilePath('Language.txt'));
    if FileExists(LocalizedFilePath('Language2.txt')) then
      Phrases2.LoadFromFile(LocalizedFilePath('Language2.txt'))
    else
    begin
      Phrases2.LoadFromFile(HomeDir + 'Language2.txt');
      Phrases2FallenBackToEnglish := True;
    end;
  end
  else
  begin
    Phrases.LoadFromFile(HomeDir + 'Language.txt');
    Phrases2.LoadFromFile(HomeDir + 'Language2.txt');
  end;

  if Sounds = nil then Sounds := TStringTable.Create;
  if not Sounds.LoadFromFile(GetSoundsDir + DirectorySeparator + 'sound.txt') then
  begin
    FreeAndNil(Sounds);
  end;
end;

procedure Texturize(Dest, Texture: TBitmap; TransparentColor: Cardinal);
var
  SrcPixel, DstPixel: TPixelPointer;
  X, Y: Integer;
  TexWidth, TexHeight: Integer;
begin
  // texturize background
  Dest.BeginUpdate;
  TexWidth := Texture.Width;
  TexHeight := Texture.Height;
  DstPixel := PixelPointer(Dest);
  SrcPixel := PixelPointer(Texture);
  for Y := 0 to ScaleToNative(Dest.Height) - 1 do begin
    for X := 0 to ScaleToNative(Dest.Width) - 1 do begin
      if (DstPixel.Pixel^.ARGB and $FFFFFF) = TransparentColor then begin
        SrcPixel.SetXY(X mod TexWidth, Y mod TexHeight);
        DstPixel.Pixel^.B := SrcPixel.Pixel^.B;
        DstPixel.Pixel^.G := SrcPixel.Pixel^.G;
        DstPixel.Pixel^.R := SrcPixel.Pixel^.R;
      end;
      DstPixel.NextPixel;
    end;
    DstPixel.NextLine;
  end;
  Dest.EndUpdate;
end;

procedure DarkenImage(Bitmap: TBitmap; Change: Integer);
var
  X, Y: Integer;
  PicturePixel: TPixelPointer;
begin
  Bitmap.BeginUpdate;
  PicturePixel := PixelPointer(Bitmap);
  for Y := 0 to ScaleToNative(Bitmap.Height) - 1 do begin
    for X := 0 to ScaleToNative(Bitmap.Width) - 1 do begin
      PicturePixel.Pixel^.B := Max(PicturePixel.Pixel^.B - Change, 0);
      PicturePixel.Pixel^.G := Max(PicturePixel.Pixel^.G - Change, 0);
      PicturePixel.Pixel^.R := Max(PicturePixel.Pixel^.R - Change, 0);
      PicturePixel.NextPixel;
    end;
    PicturePixel.NextLine;
  end;
  Bitmap.EndUpdate;
end;

function ScaleToNative(Value: Integer): Integer;
begin
  Result := Value;
end;

function ScaleFromNative(Value: Integer): Integer;
begin
  Result := Value;
end;

procedure UnshareBitmap(Bitmap: TBitmap);
begin
  // FillRect cause image data to be freed so subsequent BitBlt can access valid image data
  Bitmap.Canvas.FillRect(0, 0, 0, 0);
end;

procedure Gtk2Fix;
{$IFDEF UNIX}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF UNIX}
  // Wait and process messages little bit to avoid crash or force repaint under Gtk2
  for I := 0 to 10 do begin
    Sleep(1);
    Application.ProcessMessages;
  end;
  {$ENDIF}
end;

procedure LoadFonts;
var
  Section: TFontType;
  FontScript: TextFile;
  Size: Integer;
  S: string;
  I: Integer;
  P: Integer;
begin
  Section := ftNormal;
  AssignFile(FontScript, LocalizedFilePath('Fonts.txt'));
  try
    Reset(FontScript);
    while not Eof(FontScript) do begin
      ReadLn(FontScript, S);
      if S <> '' then
        if S[1] = '#' then begin
          S := TrimRight(S);
          if S = '#SMALL' then Section := ftSmall
          else if S = '#TINY' then Section := ftTiny
          else if S = '#CAPTION' then Section := ftCaption
          else if S = '#BUTTON' then Section := ftButton
          else Section := ftNormal;
        end else begin
          P := Pos(',', S);
          if P > 0 then begin
            UniFont[section].Name := Trim(Copy(S, 1, P - 1));
            Size := 0;
            for I := P + 1 to Length(S) do
              case S[I] of
                '0' .. '9':
                  Size := Size * 10 + Byte(S[I]) - 48;
                'B', 'b':
                  UniFont[section].Style := UniFont[section].Style + [fsBold];
                'I', 'i':
                  UniFont[section].Style := UniFont[section].Style + [fsItalic];
              end;
            UniFont[section].Size := Round(Size * 72 / UniFont[section].PixelsPerInch);
          end;
        end;
    end;
    CloseFile(FontScript);
  except
  end;
end;

procedure ReleaseFonts;
var
  Section: TFontType;
begin
  for Section := Low(TFontType) to High(TFontType) do
    FreeAndNil(UniFont[section]);
end;

procedure InitGammaLookupTable;
var
  I: Integer;
  P: Integer;
begin
  GammaLookupTable[0] := 0;
  for I := 1 to 255 do begin
    P := Round(255.0 * Exp(Ln(I / 255.0) * 100.0 / Gamma));
    Assert((P >= 0) and (P < 256));
    GammaLookupTable[I] := P;
  end;
end;

procedure LoadAssets;
begin
  LoadPhrases;
  LoadFonts;
  Templates := LoadGraphicSet('Templates.png', False);
  with Templates do begin
    Logo := GetItem('Logo');
    BigBook := GetItem('BigBook');
    SmallBook := GetItem('SmallBook');
    MenuLogo := GetItem('MenuLogo');
    LinkArrows := GetItem('LinkArrows');
    ScienceNationDot := GetItem('ScienceNationDot');
    ResearchIcon := GetItem('Research');
    ChangeIcon := GetItem('Change');
    TreasuryIcon := GetItem('Treasury');
    StarshipDeparted := GetItem('StarshipDeparted');
    WeightOn := GetItem('WeightOn');
    WeightOff := GetItem('WeightOff');
  end;

  LoadGraphicFile(Colors, GetGraphicsDir + DirectorySeparator + 'Colors.png');
  LoadGraphicFile(Paper, GetGraphicsDir + DirectorySeparator + 'Paper.jpg');
  LoadGraphicFile(BigImp, GetGraphicsDir + DirectorySeparator + 'Icons.png');
end;

procedure UnitInit;
var
  Section: TFontType;
begin
  Gamma := 100;
  InitGammaLookupTable;

  {$IFDEF WINDOWS}
  EnumDisplaySettings(nil, $FFFFFFFF, StartResolution);
  ResolutionChanged := False;
  {$ENDIF}

  for Section := Low(TFontType) to High(TFontType) do
    UniFont[Section] := TFont.Create;

  GrExt := TGraphicSets.Create;

  HGrSystem := LoadGraphicSet('System.png');
  with HGrSystem do begin
    CityMark1 := GetItem('CityMark1');
    CityMark2 := GetItem('CityMark2');
  end;

  HGrSystem2 := LoadGraphicSet('System2.png');
  with HGrSystem2 do begin
    Ornament := GetItem('Ornament');
    GBrainNoTerm := GetItem('BrainNoTerm');
    GBrainSuperVirtual := GetItem('BrainSuperVirtual');
    GBrainTerm := GetItem('BrainTerm');
    GBrainRandom := GetItem('BrainRandom');
  end;

  Colors := TBitmap.Create;
  Colors.PixelFormat := pf24bit;
  Paper := TBitmap.Create;
  Paper.PixelFormat := pf24bit;
  BigImp := TBitmap.Create;
  BigImp.PixelFormat := pf24bit;
  MainTexture := TTexture.Create;
  ClickFrameColor := HGrSystem.Data.Canvas.Pixels[187, 175];
  InitOrnamentDone := False;
  GenerateNames := True;

  LoadAssets;

  LogoBuffer := TBitmap.Create;
  LogoBuffer.PixelFormat := pf24bit;
  LogoBuffer.SetSize(BigBook.Width, BigBook.Height);
end;

procedure UnitDone;
begin
  RestoreResolution;
  FreeAndNil(GrExt);
  ReleaseFonts;
  FreeAndNil(Phrases);
  FreeAndNil(Phrases2);
  FreeAndNil(LogoBuffer);
  FreeAndNil(BigImp);
  FreeAndNil(Paper);
  FreeAndNil(Colors);
  FreeAndNil(MainTexture);
end;

end.
