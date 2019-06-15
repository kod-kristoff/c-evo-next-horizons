unit ScreenTools;

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  StringTables, LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls,
  Forms, Menus, GraphType;

type
  TTexture = record
    Image: TBitmap;
    clBevelLight, clBevelShade, clTextLight, clTextShade, clLitText, clMark,
    clPage, clCover: TColor;
  end;

{$IFDEF WINDOWS}
function ChangeResolution(x, y, bpp, freq: integer): boolean;
{$ENDIF}
procedure RestoreResolution;
function Play(Item: string; Index: integer = -1): boolean;
procedure PreparePlay(Item: string; Index: integer = -1);
procedure EmptyMenu(MenuItems: TMenuItem; Keep: Integer = 0);
function TurnToYear(Turn: integer): integer;
function TurnToString(Turn: integer): string;
function MovementToString(Movement: integer): string;
procedure BtnFrame(ca: TCanvas; p: TRect; const T: TTexture);
procedure EditFrame(ca: TCanvas; p: TRect; const T: TTexture);
function HexStringToColor(S: string): integer;
function LoadGraphicFile(bmp: TBitmap; Path: string; Options: integer = 0): boolean;
function LoadGraphicSet(const Name: string): integer;
procedure Dump(dst: TBitmap; HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
procedure Sprite(Canvas: TCanvas; HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
  overload;
procedure Sprite(dst: TBitmap; HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
  overload;
procedure MakeBlue(dst: TBitmap; x, y, w, h: integer);
procedure ImageOp_B(dst, Src: TBitmap; xDst, yDst, xSrc, ySrc, w, h: integer);
procedure ImageOp_BCC(dst, Src: TBitmap;
  xDst, yDst, xSrc, ySrc, w, h, Color1, Color2: integer);
procedure ImageOp_CCC(bmp: TBitmap; x, y, w, h, Color0, Color1, Color2: integer);
function BitBltCanvas(DestCanvas: TCanvas; X, Y, Width, Height: integer;
  SrcCanvas: TCanvas; XSrc, YSrc: integer; Rop: DWORD): boolean;
procedure SLine(ca: TCanvas; x0, x1, y: integer; cl: TColor);
procedure DLine(ca: TCanvas; x0, x1, y: integer; cl0, cl1: TColor);
procedure Frame(ca: TCanvas; x0, y0, x1, y1: integer; cl0, cl1: TColor);
procedure RFrame(ca: TCanvas; x0, y0, x1, y1: integer; cl0, cl1: TColor);
procedure CFrame(ca: TCanvas; x0, y0, x1, y1, Corner: integer; cl: TColor);
procedure FrameImage(ca: TCanvas; Src: TBitmap;
  x, y, Width, Height, xSrc, ySrc: integer; IsControl: boolean = False);
procedure GlowFrame(dst: TBitmap; x0, y0, Width, Height: integer; cl: TColor);
procedure InitOrnament;
procedure InitCityMark(const T: TTexture);
procedure Fill(ca: TCanvas; Left, Top, Width, Height, xOffset, yOffset: integer);
procedure FillLarge(ca: TCanvas; x0, y0, x1, y1, xm: integer);
procedure FillSeamless(ca: TCanvas; Left, Top, Width, Height, xOffset, yOffset: integer;
  const Texture: TBitmap);
procedure FillRectSeamless(ca: TCanvas; x0, y0, x1, y1, xOffset, yOffset: integer;
  const Texture: TBitmap);
procedure PaintBackground(Form: TForm; Left, Top, Width, Height: integer);
procedure Corner(ca: TCanvas; x, y, Kind: integer; const T: TTexture);
procedure BiColorTextOut(ca: TCanvas; clMain, clBack: TColor; x, y: integer; s: string);
procedure LoweredTextOut(ca: TCanvas; cl: TColor; const T: TTexture;
  x, y: integer; s: string);
function BiColorTextWidth(ca: TCanvas; s: string): integer;
procedure RisedTextOut(ca: TCanvas; x, y: integer; s: string);
procedure LightGradient(ca: TCanvas; x, y, Width, Color: integer);
procedure DarkGradient(ca: TCanvas; x, y, Width, Kind: integer);
procedure VLightGradient(ca: TCanvas; x, y, Height, Color: integer);
procedure VDarkGradient(ca: TCanvas; x, y, Height, Kind: integer);
procedure NumberBar(dst: TBitmap; x, y: integer; Cap: string; val: integer;
  const T: TTexture);
procedure CountBar(dst: TBitmap; x, y, w: integer; Kind: integer;
  Cap: string; val: integer; const T: TTexture);
procedure PaintProgressBar(ca: TCanvas; Kind, x, y, pos, Growth, max: integer;
  const T: TTexture);
procedure PaintRelativeProgressBar(ca: TCanvas;
  Kind, x, y, size, pos, Growth, max: integer; IndicateComplete: boolean;
  const T: TTexture);
procedure PaintLogo(ca: TCanvas; x, y, clLight, clShade: integer);
function SetMainTextureByAge(Age: integer): boolean;
procedure LoadPhrases;

const
  nGrExtmax = 64;
  wMainTexture = 640;
  hMainTexture = 480;

  // template positions in Template.bmp
  xLogo = 1;
  yLogo = 1;
  wLogo = 122;
  hLogo = 23; // logo
  xBBook = 1;
  yBBook = 74;
  wBBook = 143;
  hBBook = 73; // big book
  xSBook = 72;
  ySBook = 37;
  wSBook = 72;
  hSBook = 36; // small book
  xNation = 1;
  yNation = 25;
  xCoal = 1;
  yCoal = 148;

  // Icons.bmp structure
  xSizeBig = 56;
  ySizeBig = 40;

  GlowRange = 8;

  EmptySpaceColor = $101010;

  // template positions in System2.bmp
  xOrna = 156;
  yOrna = 1;
  wOrna = 27;
  hOrna = 26; // ornament

  // sound modes
  smOff = 0;
  smOn = 1;
  smOnAlt = 2;

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

  // LoadGraphicFile options
  gfNoError = $01;
  gfNoGamma = $02;

type
  TGrExtDescr = record { don't use dynamic strings here! }
    Name: string[31];
    Data, Mask: TBitmap;
    pixUsed: array [byte] of byte;
  end;

  TGrExtDescrSize = record { for size calculation only - must be the same as
      TGrExtDescr, but without pixUsed }
    Name: string[31];
    Data, Mask: TBitmap;
  end;

  TFontType = (ftNormal, ftSmall, ftTiny, ftCaption, ftButton);

var
  Phrases, Phrases2, Sounds: TStringTable;
  nGrExt: integer;
  GrExt: array [0 .. nGrExtmax - 1] of ^TGrExtDescr;
  HGrSystem, HGrSystem2, ClickFrameColor, SoundMode, MainTextureAge: integer;
  MainTexture: TTexture;
  Templates, Colors, Paper, BigImp, LogoBuffer: TBitmap;
  FullScreen, GenerateNames, InitOrnamentDone, Phrases2FallenBackToEnglish: Boolean;

  UniFont: array [TFontType] of TFont;
  AppRegistryKey: string = '\SOFTWARE\C-evo';

procedure UnitInit;
procedure UnitDone;

implementation

uses
  Directories, Sound, Registry, PixelPointer;

var
  {$IFDEF WINDOWS}
  StartResolution: TDeviceMode;
  ResolutionChanged: boolean;
  {$ENDIF}

  Gamma: Integer; // global gamma correction (cent)
  GammaLookupTable: array [0 .. 255] of Byte;

{$IFDEF WINDOWS}
function ChangeResolution(x, y, bpp, freq: integer): boolean;
var
  DevMode: TDeviceMode;
begin
  EnumDisplaySettings(nil, 0, DevMode);
  DevMode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or
    DM_DISPLAYFREQUENCY;
  DevMode.dmPelsWidth := x;
  DevMode.dmPelsHeight := y;
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

function Play(Item: string; Index: integer = -1): boolean;
{$IFNDEF DEBUG}
var
  WavFileName: string;
{$ENDIF}
begin
  Result := False;
{$IFNDEF DEBUG}
  if (Sounds = nil) or (SoundMode = smOff) or (Item = '') then
  begin
    Result := True;
    Exit;
  end;
  WavFileName := Sounds.Lookup(Item, Index);
  Assert(WavFileName[1] <> '[');
  Result := (WavFileName <> '') and (WavFileName[1] <> '[') and (WavFileName <> '*');
  if Result then
    // SndPlaySound(pchar(HomeDir+'Sounds' +DirectorySeparator+WavFileName+'.wav'),SND_ASYNC)
    PlaySound(HomeDir + 'Sounds' + DirectorySeparator + WavFileName);
{$ENDIF}
end;

procedure PreparePlay(Item: string; Index: Integer = -1);
{$IFNDEF DEBUG}
var
  WavFileName: string;
{$ENDIF}
begin
{$IFNDEF DEBUG}
  if (Sounds = nil) or (SoundMode = smOff) or (Item = '') then
    Exit;
  WavFileName := Sounds.Lookup(Item, Index);
  Assert(WavFileName[1] <> '[');
  if (WavFileName <> '') and (WavFileName[1] <> '[') and (WavFileName <> '*') then
    PrepareSound(HomeDir + 'Sounds' + DirectorySeparator + WavFileName);
{$ENDIF}
end;

procedure EmptyMenu(MenuItems: TMenuItem; Keep: Integer = 0);
var
  MenuItem: TMenuItem;
begin
  while MenuItems.Count > Keep do begin
    MenuItem := MenuItems[MenuItems.Count - 1];
    MenuItems.Delete(MenuItems.Count - 1);
    MenuItem.Free;
  end;
end;

function TurnToYear(Turn: Integer): Integer;
var
  I: Integer;
begin
  Result := -4000;
  for I := 1 to Turn do
    if Result < -1000 then Inc(Result, 50) // 0..60
    else if Result < 0 then Inc(Result, 25) // 60..100
    else if Result < 1500 then Inc(Result, 20) // 100..175
    else if Result < 1750 then Inc(Result, 10) // 175..200
    else if Result < 1850 then Inc(Result, 2) // 200..250
    else Inc(Result);
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

procedure BtnFrame(ca: TCanvas; p: TRect; const T: TTexture);
begin
  RFrame(ca, p.Left - 1, p.Top - 1, p.Right, p.Bottom, T.clBevelShade,
    T.clBevelLight);
end;

procedure EditFrame(ca: TCanvas; p: TRect; const T: TTexture);
begin
  Frame(ca, p.Left - 1, p.Top - 1, p.Right, p.Bottom, $000000, $000000);
  Frame(ca, p.Left - 2, p.Top - 2, p.Right + 1, p.Bottom + 1, $000000, $000000);
  Frame(ca, p.Left - 3, p.Top - 3, p.Right + 2, p.Bottom + 1, $000000, $000000);
  RFrame(ca, p.Left - 4, p.Top - 4, p.Right + 3, p.Bottom + 2, T.clBevelShade,
    T.clBevelLight);
end;

function HexCharToInt(X: Char): Integer;
begin
  case x of
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
  PixelPtr.Init(Bitmap);
  for Y := 0 to Bitmap.Height - 1 do begin
    for X := 0 to Bitmap.Width - 1 do begin
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
  SrcPtr.Init(Src);
  DstPtr.Init(Dst);
  for Y := 0 to Src.Height - 1 do begin
    for X := 0 to Src.Width - 1 do begin
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

function LoadGraphicFile(bmp: TBitmap; Path: string; Options: Integer): Boolean;
var
  jtex: TJpegImage;
  Png: TPortableNetworkGraphic;
begin
  Result := True;
  if ExtractFileExt(Path) = '' then
    Path := Path + '.png';
  if ExtractFileExt(Path) = '.jpg' then begin
    jtex := tjpegimage.Create;
    try
      jtex.LoadFromFile(Path);
    except
      Result := False;
    end;
    if Result then
    begin
      if Options and gfNoGamma = 0 then
        bmp.PixelFormat := pf24bit;
      Bmp.SetSize(jtex.Width, jtex.Height);
      Bmp.Canvas.Draw(0, 0, jtex);
    end;
    jtex.Free;
  end
  else
  if ExtractFileExt(Path) = '.png' then begin
    Png := TPortableNetworkGraphic.Create;
    Png.PixelFormat := Bmp.PixelFormat;
    try
      Png.LoadFromFile(Path);
    except
      Result := False;
    end;
    if Result then
    begin
      if Options and gfNoGamma = 0 then
        bmp.PixelFormat := pf24bit;
      bmp.SetSize(Png.Width, Png.Height);
      if (Png.RawImage.Description.Format = ricfGray) then
      begin
        // LCL doesn't support 8-bit colors properly. Use 24-bit instead.
        Bmp.PixelFormat := pf24bit;
        CopyGray8BitTo24bitBitmap(Bmp, Png);
      end
      else
        Bmp.Canvas.draw(0, 0, Png);
    end;
    Png.Free;
  end
  else
  if ExtractFileExt(Path) = '.bmp' then begin
    try
      bmp.LoadFromFile(Path);
    except
      Result := False;
    end;
    if Result then begin
      if Options and gfNoGamma = 0 then
        bmp.PixelFormat := pf24bit;
    end;
  end
  else
    raise Exception.Create('Unsupported image file type ' + ExtractFileExt(Path));

  if not Result then begin
    if Options and gfNoError = 0 then
      raise Exception.Create(Format(Phrases.Lookup('FILENOTFOUND'), [Path]));
  end;

  if (Options and gfNoGamma = 0) and (Gamma <> 100) then
    ApplyGammaToBitmap(Bmp);
end;

function LoadGraphicSet(const Name: string): Integer;
var
  I, x, y, xmax, OriginalColor: Integer;
  FileName: string;
  Source: TBitmap;
  DataPixel, MaskPixel: TPixelPointer;
begin
  I := 0;
  while (I < nGrExt) and (GrExt[i].Name <> Name) do
    Inc(I);
  Result := I;
  if I = nGrExt then begin
    Source := TBitmap.Create;
    Source.PixelFormat := pf24bit;
    FileName := HomeDir + 'Graphics' + DirectorySeparator + Name;
    if not LoadGraphicFile(Source, FileName) then begin
      Result := -1;
      Exit;
    end;

    GetMem(GrExt[nGrExt], SizeOf(TGrExtDescrSize) + Source.Height div 49 * 10);
    GrExt[nGrExt].Name := Name;

    xmax := Source.Width - 1; // allows 4-byte access even for last pixel
    if xmax > 970 then
      xmax := 970;

    GrExt[nGrExt].Data := Source;
    GrExt[nGrExt].Data.PixelFormat := pf24bit;
    GrExt[nGrExt].Mask := TBitmap.Create;
    GrExt[nGrExt].Mask.PixelFormat := pf24bit;
    GrExt[nGrExt].Mask.SetSize(Source.Width, Source.Height);

    GrExt[nGrExt].Data.BeginUpdate;
    GrExt[nGrExt].Mask.BeginUpdate;
    DataPixel.Init(GrExt[nGrExt].Data);
    MaskPixel.Init(GrExt[nGrExt].Mask);
    for y := 0 to Source.Height - 1 do begin
      for x := 0 to xmax - 1 do begin
        OriginalColor := DataPixel.Pixel^.ARGB and $FFFFFF;
        if (OriginalColor = $FF00FF) or (OriginalColor = $7F007F) then
        begin // transparent
          MaskPixel.Pixel^.ARGB := $FFFFFF;
          DataPixel.Pixel^.ARGB := DataPixel.Pixel^.ARGB and $FF000000;
        end
        else begin
          MaskPixel.Pixel^.ARGB := $000000; // non-transparent
          if Gamma <> 100 then
            DataPixel.Pixel^ := ApplyGammaToPixel(DataPixel.Pixel^);
        end;
        DataPixel.NextPixel;
        MaskPixel.NextPixel;
      end;
      DataPixel.NextLine;
      MaskPixel.NextLine;
    end;
    GrExt[nGrExt].Data.EndUpdate;
    GrExt[nGrExt].Mask.EndUpdate;

    FillChar(GrExt[nGrExt].pixUsed, GrExt[nGrExt].Data.Height div 49 * 10, 0);
    Inc(nGrExt);
  end;
end;

procedure Dump(dst: TBitmap; HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
begin
  BitBlt(dst.Canvas.Handle, xDst, yDst, Width, Height,
    GrExt[HGr].Data.Canvas.Handle, xGr, yGr, SRCCOPY);
end;

procedure MakeBlue(dst: TBitmap; x, y, w, h: integer);
var
  XX, YY: integer;
  PixelPtr: TPixelPointer;
begin
  Dst.BeginUpdate;
  PixelPtr.Init(Dst, X, Y);
  for yy := 0 to h - 1 do begin
    for xx := 0 to w - 1 do begin
      PixelPtr.Pixel^.B := PixelPtr.Pixel^.B div 2;
      PixelPtr.Pixel^.G := PixelPtr.Pixel^.G div 2;
      PixelPtr.Pixel^.R := PixelPtr.Pixel^.R div 2;
      PixelPtr.NextPixel;
    end;
    PixelPtr.NextLine;
  end;
  Dst.EndUpdate;
end;

procedure ImageOp_B(dst, Src: TBitmap; xDst, yDst, xSrc, ySrc, w, h: Integer);
// Src is template
// X channel = background amp (old Dst content), 128=original brightness
var
  X, Y: Integer;
  Brightness, Test: Integer;
  PixelSrc: TPixelPointer;
  PixelDst: TPixelPointer;
begin
  //Assert(Src.PixelFormat = pf8bit);
  Assert(dst.PixelFormat = pf24bit);
  if xDst < 0 then begin
    w := w + xDst;
    xSrc := xSrc - xDst;
    xDst := 0;
  end;
  if yDst < 0 then begin
    h := h + yDst;
    ySrc := ySrc - yDst;
    yDst := 0;
  end;
  if xDst + w > dst.Width then
    w := dst.Width - xDst;
  if yDst + h > dst.Height then
    h := dst.Height - yDst;
  if (w < 0) or (h < 0) then
    exit;

  dst.BeginUpdate;
  Src.BeginUpdate;
  PixelDst.Init(Dst, xDst, yDst);
  PixelSrc.Init(Src, xSrc, ySrc);
  for Y := 0 to h - 1 do begin
    for X := 0 to w - 1 do  begin
      Brightness := PixelSrc.Pixel^.B; // One byte for 8-bit color
      test := (PixelDst.Pixel^.R * Brightness) shr 7;
      if test >= 256 then
        PixelDst.Pixel^.R := 255
      else
        PixelDst.Pixel^.R := test; // Red
      test := (PixelDst.Pixel^.G * Brightness) shr 7;
      if test >= 256 then
        PixelDst.Pixel^.G := 255
      else
        PixelDst.Pixel^.G := test; // Green
      test := (PixelDst.Pixel^.B * Brightness) shr 7;
      if test >= 256 then
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

procedure ImageOp_BCC(dst, Src: TBitmap;
  xDst, yDst, xSrc, ySrc, w, h, Color1, Color2: integer);
// Src is template
// B channel = background amp (old Dst content), 128=original brightness
// G channel = Color1 amp, 128=original brightness
// R channel = Color2 amp, 128=original brightness
var
  ix, iy, amp1, amp2, trans, Value: integer;
  SrcPixel, DstPixel: TPixelPointer;
begin
  if xDst < 0 then begin
    w := w + xDst;
    xSrc := xSrc - xDst;
    xDst := 0;
  end;
  if yDst < 0 then begin
    h := h + yDst;
    ySrc := ySrc - yDst;
    yDst := 0;
  end;
  if xDst + w > dst.Width then
    w := dst.Width - xDst;
  if yDst + h > dst.Height then
    h := dst.Height - yDst;
  if (w < 0) or (h < 0) then
    exit;

  Src.BeginUpdate;
  dst.BeginUpdate;
  SrcPixel.Init(Src, xSrc, ySrc);
  DstPixel.Init(Dst, xDst, yDst);
  for iy := 0 to h - 1 do begin
    for ix := 0 to w - 1 do begin
      trans := SrcPixel.Pixel^.B * 2; // green channel = transparency
      amp1 := SrcPixel.Pixel^.G * 2;
      amp2 := SrcPixel.Pixel^.R * 2;
      if trans <> $FF then begin
        Value := (DstPixel.Pixel^.B * trans + ((Color2 shr 16) and $FF) *
          amp2 + ((Color1 shr 16) and $FF) * amp1) div $FF;
        if Value < 256 then
          DstPixel.Pixel^.B := Value
        else
          DstPixel.Pixel^.B := 255;
        Value := (DstPixel.Pixel^.G * trans + ((Color2 shr 8) and $FF) *
          amp2 + ((Color1 shr 8) and $FF) * amp1) div $FF;
        if Value < 256 then
          DstPixel.Pixel^.G := Value
        else
          DstPixel.Pixel^.G := 255;
        Value := (DstPixel.Pixel^.R * trans + (Color2 and $FF) *
          amp2 + (Color1 and $FF) * amp1) div $FF;
        if Value < 256 then
          DstPixel.Pixel^.R := Value
        else
          DstPixel.Pixel^.R := 255;
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

procedure ImageOp_CCC(bmp: TBitmap; x, y, w, h, Color0, Color1, Color2: Integer);
// Bmp is template
// B channel = Color0 amp, 128=original brightness
// G channel = Color1 amp, 128=original brightness
// R channel = Color2 amp, 128=original brightness
var
  i, Red, Green: Integer;
  PixelPtr: TPixelPointer;
begin
  bmp.BeginUpdate;
  assert(bmp.PixelFormat = pf24bit);
  h := y + h;
  PixelPtr.Init(Bmp, x, y);
  while y < h do begin
    for i := 0 to w - 1 do begin
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
    Inc(y);
    PixelPtr.NextLine;
  end;
  bmp.EndUpdate;
end;

procedure Sprite(Canvas: TCanvas; HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
begin
  BitBlt(Canvas.Handle, xDst, yDst, Width, Height,
    GrExt[HGr].Mask.Canvas.Handle, xGr, yGr, SRCAND);
  BitBlt(Canvas.Handle, xDst, yDst, Width, Height,
    GrExt[HGr].Data.Canvas.Handle, xGr, yGr, SRCPAINT);
end;

procedure Sprite(dst: TBitmap; HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
begin
  BitBlt(dst.Canvas.Handle, xDst, yDst, Width, Height,
    GrExt[HGr].Mask.Canvas.Handle, xGr, yGr, SRCAND);
  BitBlt(dst.Canvas.Handle, xDst, yDst, Width, Height,
    GrExt[HGr].Data.Canvas.Handle, xGr, yGr, SRCPAINT);
end;

function BitBltCanvas(DestCanvas: TCanvas; X, Y, Width, Height: integer;
  SrcCanvas: TCanvas; XSrc, YSrc: integer; Rop: DWORD): boolean;
begin
  Assert(Rop = SRCCOPY);
  DestCanvas.CopyRect(Rect(X, Y, X + Width, Y + Height), SrcCanvas,
    Rect(XSrc, YSrc, XSrc + Width, YSrc + Height));
  Result := True;
end;

procedure SLine(ca: TCanvas; x0, x1, y: integer; cl: TColor);
begin
  with ca do begin
    Pen.Color := cl;
    MoveTo(x0, y);
    LineTo(x1 + 1, y);
  end;
end;

procedure DLine(ca: TCanvas; x0, x1, y: integer; cl0, cl1: TColor);
begin
  with ca do begin
    Pen.Color := cl0;
    MoveTo(x0, y);
    LineTo(x1, y);
    Pen.Color := cl1;
    MoveTo(x0 + 1, y + 1);
    LineTo(x1 + 1, y + 1);
    Pixels[x0, y + 1] := cl0;
    Pixels[x1, y] := cl1;
  end;
end;

procedure Frame(ca: TCanvas; x0, y0, x1, y1: integer; cl0, cl1: TColor);
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

procedure RFrame(ca: TCanvas; x0, y0, x1, y1: integer; cl0, cl1: TColor);
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

procedure CFrame(ca: TCanvas; x0, y0, x1, y1, Corner: integer; cl: TColor);
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
  x, y, Width, Height, xSrc, ySrc: integer; IsControl: boolean = False);
begin
  if IsControl then begin
    Frame(ca, x - 1, y - 1, x + Width, y + Height, $B0B0B0, $FFFFFF);
    RFrame(ca, x - 2, y - 2, x + Width + 1, y + Height + 1, $FFFFFF, $B0B0B0);
  end else
    Frame(ca, x - 1, y - 1, x + Width, y + Height, $000000, $000000);
  BitBlt(ca.Handle, x, y, Width, Height, Src.Canvas.Handle, xSrc, ySrc,
    SRCCOPY);
end;

procedure GlowFrame(dst: TBitmap; x0, y0, Width, Height: Integer; cl: TColor);
var
  x, y, ch, r: Integer;
  DstPtr: TPixelPointer;
begin
  dst.BeginUpdate;
  DstPtr.Init(dst, x0, y0);
  for y := -GlowRange + 1 to Height - 1 + GlowRange - 1 do begin
    for x := -GlowRange + 1 to Width - 1 + GlowRange - 1 do begin
      DstPtr.SetXY(x, y);
      if x < 0 then
        if y < 0 then
          r := round(sqrt(sqr(x) + sqr(y)))
        else if y >= Height then
          r := round(sqrt(sqr(x) + sqr(y - (Height - 1))))
        else
          r := -x
      else if x >= Width then
        if y < 0 then
          r := round(sqrt(sqr(x - (Width - 1)) + sqr(y)))
        else if y >= Height then
          r := round(sqrt(sqr(x - (Width - 1)) + sqr(y - (Height - 1))))
        else
          r := x - (Width - 1)
      else if y < 0 then
        r := -y
      else if y >= Height then
        r := y - (Height - 1)
      else
        continue;
      if r = 0 then
        r := 1;
      if r < GlowRange then
        for ch := 0 to 2 do
          DstPtr.Pixel^.Planes[2 - ch] :=
            (DstPtr.Pixel^.Planes[2 - ch] * (r - 1) + (cl shr (8 * ch) and $FF) *
            (GlowRange - r)) div (GlowRange - 1);
    end;
  end;
  dst.EndUpdate;
end;

procedure InitOrnament;
var
  x, y, p, Light, Shade: Integer;
begin
  if InitOrnamentDone then Exit;
  Light := MainTexture.clBevelLight;
  // and $FCFCFC shr 2*3+MainTexture.clBevelShade and $FCFCFC shr 2;
  Shade := MainTexture.clBevelShade and $FCFCFC shr 2 * 3 +
    MainTexture.clBevelLight and $FCFCFC shr 2;
  for x := 0 to wOrna - 1 do
    for y := 0 to hOrna - 1 do begin
      p := GrExt[HGrSystem2].Data.Canvas.Pixels[xOrna + x, yOrna + y];
      if p = $0000FF then
        GrExt[HGrSystem2].Data.Canvas.Pixels[xOrna + x, yOrna + y] := Light
      else if p = $FF0000 then
        GrExt[HGrSystem2].Data.Canvas.Pixels[xOrna + x, yOrna + y] := Shade;
    end;
  InitOrnamentDone := True;
end;

procedure InitCityMark(const T: TTexture);
var
  x, y, intensity: Integer;
begin
  for x := 0 to 9 do
    for y := 0 to 9 do
      if GrExt[HGrSystem].Mask.Canvas.Pixels[66 + x, 47 + y] = 0 then
      begin
        intensity := GrExt[HGrSystem].Data.Canvas.Pixels[66 +
          x, 47 + y] and $FF;
        GrExt[HGrSystem].Data.Canvas.Pixels[77 + x, 47 + y] :=
          T.clMark and $FF * intensity div $FF + T.clMark shr 8 and
          $FF * intensity div $FF shl 8 + T.clMark shr 16 and
          $FF * intensity div $FF shl 16;
      end;
  BitBlt(GrExt[HGrSystem].Mask.Canvas.Handle, 77, 47, 10, 10,
    GrExt[HGrSystem].Mask.Canvas.Handle, 66, 47, SRCCOPY);
end;

procedure Fill(ca: TCanvas; Left, Top, Width, Height, xOffset, yOffset: Integer);
begin
  Assert((Left + xOffset >= 0) and (Left + xOffset + Width <= wMainTexture) and
    (Top + yOffset >= 0) and (Top + yOffset + Height <= hMainTexture));
  BitBlt(ca.Handle, Left, Top, Width, Height, MainTexture.Image.Canvas.Handle,
    Left + xOffset, Top + yOffset, SRCCOPY);
end;

procedure FillLarge(ca: TCanvas; x0, y0, x1, y1, xm: Integer);

  function Band(I: Integer): Integer;
  var
    n: integer;
  begin
    n := ((hMainTexture div 2) div (y1 - y0)) * 2;
    while hMainTexture div 2 + (I + 1) * (y1 - y0) > hMainTexture do
      Dec(I, n);
    while hMainTexture div 2 + I * (y1 - y0) < 0 do
      Inc(I, n);
    Result := I;
  end;

var
  I: Integer;
begin
  for I := 0 to (x1 - xm) div wMainTexture - 1 do
    BitBlt(ca.Handle, xm + I * wMainTexture, y0, wMainTexture, y1 - y0,
      MainTexture.Image.Canvas.Handle, 0, hMainTexture div 2 + Band(I) *
      (y1 - y0), SRCCOPY);
  BitBlt(ca.Handle, xm + ((x1 - xm) div wMainTexture) * wMainTexture, y0,
    x1 - (xm + ((x1 - xm) div wMainTexture) * wMainTexture), y1 - y0,
    MainTexture.Image.Canvas.Handle, 0, hMainTexture div 2 + Band(
    (x1 - xm) div wMainTexture) * (y1 - y0), SRCCOPY);
  for I := 0 to (xm - x0) div wMainTexture - 1 do
    BitBlt(ca.Handle, xm - (I + 1) * wMainTexture, y0, wMainTexture, y1 - y0,
      MainTexture.Image.Canvas.Handle, 0, hMainTexture div 2 +
      Band(-I - 1) * (y1 - y0), SRCCOPY);
  BitBlt(ca.Handle, x0, y0, xm - ((xm - x0) div wMainTexture) *
    wMainTexture - x0, y1 - y0, MainTexture.Image.Canvas.Handle,
    ((xm - x0) div wMainTexture + 1) * wMainTexture - (xm - x0),
    hMainTexture div 2 + Band(-(xm - x0) div wMainTexture - 1) * (y1 - y0), SRCCOPY);
end;

procedure FillSeamless(ca: TCanvas; Left, Top, Width, Height, xOffset, yOffset: Integer;
  const Texture: TBitmap);
var
  x, y, x0cut, y0cut, x1cut, y1cut: Integer;
begin
  while xOffset < 0 do
    Inc(xOffset, Texture.Width);
  while yOffset < 0 do
    Inc(yOffset, Texture.Height);
  for y := (Top + yOffset) div Texture.Height to (Top + yOffset + Height - 1) div
    Texture.Height do
  begin
    y0cut := Top + yOffset - y * Texture.Height;
    if y0cut < 0 then
      y0cut := 0;
    y1cut := (y + 1) * Texture.Height - (Top + yOffset + Height);
    if y1cut < 0 then
      y1cut := 0;
    for x := (Left + xOffset) div Texture.Width to (Left + xOffset + Width - 1) div
      Texture.Width do
    begin
      x0cut := Left + xOffset - x * Texture.Width;
      if x0cut < 0 then
        x0cut := 0;
      x1cut := (x + 1) * Texture.Width - (Left + xOffset + Width);
      if x1cut < 0 then
        x1cut := 0;
      BitBlt(ca.Handle, x * Texture.Width + x0cut - xOffset,
        y * Texture.Height + y0cut - yOffset, Texture.Width - x0cut - x1cut,
        Texture.Height - y0cut - y1cut, Texture.Canvas.Handle, x0cut,
        y0cut, SRCCOPY);
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
  Fill(Form.Canvas, Left, Top, Width, Height, (wMainTexture - Form.ClientWidth) div
    2, (hMainTexture - Form.ClientHeight) div 2);
end;

procedure Corner(ca: TCanvas; x, y, Kind: Integer; const T: TTexture);
begin
  { BitBlt(ca.Handle,x,y,8,8,GrExt[T.HGr].Mask.Canvas.Handle,
    T.xGr+29+Kind*9,T.yGr+89,SRCAND);
    BitBlt(ca.Handle,x,y,8,8,GrExt[T.HGr].Data.Canvas.Handle,
    T.xGr+29+Kind*9,T.yGr+89,SRCPAINT); }
end;

procedure BiColorTextOut(ca: TCanvas; clMain, clBack: TColor; x, y: Integer; s: string);

  procedure PaintIcon(x, y, Kind: Integer);
  begin
    BitBlt(ca.Handle, x, y + 6, 10, 10, GrExt[HGrSystem].Mask.Canvas.Handle,
      66 + Kind mod 11 * 11, 115 + Kind div 11 * 11, SRCAND);
    BitBlt(ca.Handle, x, y + 6, 10, 10, GrExt[HGrSystem].Data.Canvas.Handle,
      66 + Kind mod 11 * 11, 115 + Kind div 11 * 11, SRCPAINT);
  end;

var
  p, xp: Integer;
  sp: string;
  shadow: Boolean;
begin
  Inc(x);
  Inc(y);
  for shadow := True downto False do
    with ca do
      if not shadow or (clBack <> $7F007F) then
      begin
        if shadow then
          Font.Color := clBack
        else
          Font.Color := clMain;
        sp := s;
        xp := x;
        repeat
          p := pos('%', sp);
          if (p = 0) or (p + 1 > Length(sp)) or not
            (sp[p + 1] in ['c', 'f', 'l', 'm', 'n', 'o', 'p', 'r', 't', 'w']) then
          begin
            ca.Textout(xp, y, sp);
            Break;
          end
          else
          begin
            Textout(xp, y, copy(sp, 1, p - 1));
            Inc(xp, ca.TextWidth(copy(sp, 1, p - 1)));
            if not shadow then
              case sp[p + 1] of
                'c': PaintIcon(xp + 1, y, 6);
                'f': PaintIcon(xp + 1, y, 0);
                'l': PaintIcon(xp + 1, y, 8);
                'm': PaintIcon(xp + 1, y, 17);
                'n': PaintIcon(xp + 1, y, 7);
                'o': PaintIcon(xp + 1, y, 16);
                'p': PaintIcon(xp + 1, y, 2);
                'r': PaintIcon(xp + 1, y, 12);
                't': PaintIcon(xp + 1, y, 4);
                'w': PaintIcon(xp + 1, y, 13);
              end;
            Inc(xp, 10);
            Delete(sp, 1, p + 1);
          end;
        until False;
        Dec(x);
        Dec(y);
      end;
end;

function BiColorTextWidth(ca: TCanvas; s: string): Integer;
var
  P: Integer;
begin
  Result := 1;
  repeat
    P := Pos('%', s);
    if (P = 0) or (P = Length(s)) then
    begin
      Inc(Result, ca.TextWidth(s));
      Break;
    end
    else
    begin
      if not (s[P + 1] in ['c', 'f', 'l', 'm', 'n', 'o', 'p', 'r', 't', 'w'])
      then
        Inc(Result, ca.TextWidth(copy(s, 1, P + 1)))
      else
        Inc(Result, ca.TextWidth(copy(s, 1, P - 1)) + 10);
      Delete(s, 1, P + 1);
    end;
  until False;
end;

procedure LoweredTextOut(ca: TCanvas; cl: TColor; const T: TTexture;
  x, y: Integer; s: string);
begin
  if cl = -2 then
    BiColorTextOut(ca, (T.clBevelShade and $FEFEFE) shr 1,
      T.clBevelLight, x, y, s)
  else if cl < 0 then
    BiColorTextOut(ca, T.clTextShade, T.clTextLight, x, y, s)
  else
    BiColorTextOut(ca, cl, T.clTextLight, x, y, s);
end;

procedure RisedTextOut(ca: TCanvas; x, y: integer; s: string);
begin
  BiColorTextOut(ca, $FFFFFF, $000000, x, y, s);
end;

procedure Gradient(ca: TCanvas; x, y, dx, dy, Width, Height, Color: Integer;
  Brightness: array of integer);
var
  i, r, g, b: Integer;
begin
  begin
    for i := 0 to 15 do
    begin // gradient
      r := Color and $FF + Brightness[i];
      if r < 0 then
        r := 0
      else if r >= 256 then
        r := 255;
      g := Color shr 8 and $FF + Brightness[i];
      if g < 0 then
        g := 0
      else if g >= 256 then
        g := 255;
      b := Color shr 16 and $FF + Brightness[i];
      if b < 0 then
        b := 0
      else if b >= 256 then
        b := 255;
      ca.Pen.Color := r + g shl 8 + b shl 16;
      ca.MoveTo(x + dx * i, y + dy * i);
      ca.LineTo(x + dx * i + Width, y + dy * i + Height);
    end;
    ca.Pen.Color := $000000;
    ca.MoveTo(x + 1, y + 16 * dy + Height);
    ca.LineTo(x + 16 * dx + Width, y + 16 * dy + Height);
    ca.LineTo(x + 16 * dx + Width, y);
  end;
end;

procedure LightGradient(ca: TCanvas; x, y, Width, Color: Integer);
const
  Brightness: array [0 .. 15] of integer =
    (16, 12, 8, 4, 0, -4, -8, -12, -16, -20, -24, -28, -32, -36, -40, -44);
begin
  Gradient(ca, x, y, 0, 1, Width, 0, Color, Brightness);
end;

procedure DarkGradient(ca: TCanvas; x, y, Width, Kind: Integer);
const
  Brightness: array [0 .. 15] of integer =
    (16, 12, 8, 4, 0, -4, -8, -12 - 24, -16 + 16, -20, -24, -28, -32, -36, -40, -44);
begin
  Gradient(ca, x, y, 0, 1, Width, 0, GrExt[HGrSystem].Data.Canvas.Pixels
    [187, 137 + Kind], Brightness);
end;

procedure VLightGradient(ca: TCanvas; x, y, Height, Color: Integer);
const
  Brightness: array [0 .. 15] of integer =
    (16, 12, 8, 4, 0, -4, -8, -12, -16, -20, -24, -28, -32, -36, -40, -44);
begin
  Gradient(ca, x, y, 1, 0, 0, Height, Color, Brightness);
end;

procedure VDarkGradient(ca: TCanvas; x, y, Height, Kind: Integer);
const
  Brightness: array [0 .. 15] of integer =
    (16, 12, 8, 4, 0, -4, -8, -12 - 24, -16 + 16, -20, -24, -28, -32, -36, -40, -44);
begin
  Gradient(ca, x, y, 1, 0, 0, Height,
    GrExt[HGrSystem].Data.Canvas.Pixels[187, 137 + Kind], Brightness);
end;

procedure NumberBar(dst: TBitmap; x, y: integer; Cap: string;
  val: Integer; const T: TTexture);
var
  s: string;
begin
  if val > 0 then
  begin
    DLine(dst.Canvas, x - 2, x + 170, y + 16, T.clBevelShade,
      T.clBevelLight);
    LoweredTextOut(dst.Canvas, -1, T, x - 2, y, Cap);
    s := IntToStr(val);
    RisedTextOut(dst.Canvas, x + 170 - BiColorTextWidth(dst.Canvas,
      s), y, s);
  end;
end;

procedure CountBar(dst: TBitmap; x, y, w: Integer; Kind: Integer;
  Cap: string; val: Integer; const T: TTexture);
var
  i, sd, ld, cl, xIcon, yIcon: Integer;
  s: string;
begin
  // val:=random(40); //!!!
  if val = 0 then
    Exit;
  Assert(Kind >= 0);
  with dst.Canvas do
  begin
    // xIcon:=x+100;
    // yIcon:=y;
    // DLine(dst.Canvas,x-2,x+170+32,y+16,T.clBevelShade,T.clBevelLight);

    xIcon := x - 5;
    yIcon := y + 15;
    DLine(dst.Canvas, x - 2, xIcon + w + 2, yIcon + 16, T.clBevelShade,
      T.clBevelLight);

    s := IntToStr(val);
    if val < 0 then
      cl := $0000FF
    else
      cl := -1;
    LoweredTextOut(dst.Canvas, cl, T, x - 2, y, Cap);
    LoweredTextOut(dst.Canvas, cl, T,
      xIcon + w + 2 - BiColorTextWidth(dst.Canvas, s), yIcon, s);

    if (Kind = 12) and (val >= 100) then
    begin // science with symbol for 100
      val := val div 10;
      sd := 14 * (val div 10 + val mod 10 - 1);
      if sd = 0 then
        sd := 1;
      if sd < w - 44 then
        ld := sd
      else
        ld := w - 44;
      for i := 0 to val mod 10 - 1 do
      begin
        BitBlt(Handle, xIcon + 4 + i * (14 * ld div sd), yIcon + 2 + 1, 14,
          14, GrExt[HGrSystem].Mask.Canvas.Handle, 67 + Kind mod 8 * 15,
          70 + Kind div 8 * 15, SRCAND);
        Sprite(dst, HGrSystem, xIcon + 3 + i * (14 * ld div sd), yIcon + 2,
          14, 14, 67 + Kind mod 8 * 15, 70 + Kind div 8 * 15);
      end;
      for i := 0 to val div 10 - 1 do
      begin
        BitBlt(dst.Canvas.Handle, xIcon + 4 + (val mod 10) *
          (14 * ld div sd) + i * (14 * ld div sd), yIcon + 3, 14, 14,
          GrExt[HGrSystem].Mask.Canvas.Handle, 67 + 7 mod 8 * 15,
          70 + 7 div 8 * 15, SRCAND);
        Sprite(dst, HGrSystem, xIcon + 3 + (val mod 10) *
          (14 * ld div sd) + i * (14 * ld div sd), yIcon + 2, 14,
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
      if sd < w - 44 then
        ld := sd
      else
        ld := w - 44;
      for i := 0 to val div 10 - 1 do
      begin
        BitBlt(Handle, xIcon + 4 + i * (14 * ld div sd), yIcon + 3, 14, 14,
          GrExt[HGrSystem].Mask.Canvas.Handle, 67 + Kind mod 8 * 15,
          70 + Kind div 8 * 15, SRCAND);
        Sprite(dst, HGrSystem, xIcon + 3 + i * (14 * ld div sd), yIcon + 2,
          14, 14, 67 + Kind mod 8 * 15, 70 + Kind div 8 * 15);
      end;
      for i := 0 to val mod 10 - 1 do
      begin
        BitBlt(dst.Canvas.Handle, xIcon + 4 + (val div 10) *
          (14 * ld div sd) + i * (10 * ld div sd), yIcon + 7, 10, 10,
          GrExt[HGrSystem].Mask.Canvas.Handle, 66 + Kind mod 11 * 11,
          115 + Kind div 11 * 11, SRCAND);
        Sprite(dst, HGrSystem, xIcon + 3 + (val div 10) *
          (14 * ld div sd) + i * (10 * ld div sd), yIcon + 6, 10,
          10, 66 + Kind mod 11 * 11,
          115 + Kind div 11 * 11);
      end;
    end;
  end;
end;

procedure PaintProgressBar(ca: TCanvas; Kind, x, y, pos, Growth, max: Integer;
  const T: TTexture);
var
  i: Integer;
begin
  if pos > max then
    pos := max;
  if Growth < 0 then
  begin
    pos := pos + Growth;
    if pos < 0 then
    begin
      Growth := Growth - pos;
      pos := 0;
    end;
  end
  else if pos + Growth > max then
    Growth := max - pos;
  Frame(ca, x - 1, y - 1, x + max, y + 7, $000000, $000000);
  RFrame(ca, x - 2, y - 2, x + max + 1, y + 8, T.clBevelShade,
    T.clBevelLight);
  with ca do
  begin
    for i := 0 to pos div 8 - 1 do
      BitBlt(Handle, x + i * 8, y, 8, 7,
        GrExt[HGrSystem].Data.Canvas.Handle, 104, 9 + 8 * Kind, SRCCOPY);
    BitBlt(Handle, x + 8 * (pos div 8), y, pos - 8 * (pos div 8), 7,
      GrExt[HGrSystem].Data.Canvas.Handle, 104, 9 + 8 * Kind, SRCCOPY);
    if Growth > 0 then
    begin
      for i := 0 to Growth div 8 - 1 do
        BitBlt(Handle, x + pos + i * 8, y, 8, 7,
          GrExt[HGrSystem].Data.Canvas.Handle, 112, 9 + 8 * Kind, SRCCOPY);
      BitBlt(Handle, x + pos + 8 * (Growth div 8), y,
        Growth - 8 * (Growth div 8), 7, GrExt[HGrSystem].Data.Canvas.Handle,
        112, 9 + 8 * Kind, SRCCOPY);
    end
    else if Growth < 0 then
    begin
      for i := 0 to -Growth div 8 - 1 do
        BitBlt(Handle, x + pos + i * 8, y, 8, 7,
          GrExt[HGrSystem].Data.Canvas.Handle, 104, 1, SRCCOPY);
      BitBlt(Handle, x + pos + 8 * (-Growth div 8), y, -Growth -
        8 * (-Growth div 8), 7,
        GrExt[HGrSystem].Data.Canvas.Handle, 104, 1, SRCCOPY);
    end;
    Brush.Color := $000000;
    FillRect(Rect(x + pos + abs(Growth), y, x + max, y + 7));
    Brush.Style := bsClear;
  end;
end;

// pos and growth are relative to max, set size independent
procedure PaintRelativeProgressBar(ca: TCanvas;
  Kind, x, y, size, pos, Growth, max: Integer; IndicateComplete: Boolean;
  const T: TTexture);
begin
  if Growth > 0 then
    PaintProgressBar(ca, Kind, x, y, pos * size div max,
      (Growth * size + max div 2) div max, size, T)
  else
    PaintProgressBar(ca, Kind, x, y, pos * size div max,
      (Growth * size - max div 2) div max, size, T);
  if IndicateComplete and (pos + Growth >= max) then
    Sprite(ca, HGrSystem, x + size - 10, y - 7, 23, 16, 1, 129);
end;

procedure PaintLogo(ca: TCanvas; x, y, clLight, clShade: Integer);
begin
  BitBltCanvas(LogoBuffer.Canvas, 0, 0, wLogo, hLogo, ca, x,
    y, SRCCOPY);
  ImageOp_BCC(LogoBuffer, Templates, 0, 0, 1, 1, wLogo, hLogo,
    clLight, clShade);
  BitBlt(ca.Handle, x, y, wLogo, hLogo, LogoBuffer.Canvas.Handle, 0,
    0, SRCCOPY);
end;

function SetMainTextureByAge(Age: Integer): Boolean;
begin
  if Age <> MainTextureAge then
    with MainTexture do begin
      MainTextureAge := Age;
      LoadGraphicFile(Image, HomeDir + 'Graphics' + DirectorySeparator +
        'Texture' + IntToStr(Age + 1) + '.jpg');
      clBevelLight := Colors.Canvas.Pixels[clkAge0 + Age, cliBevelLight];
      clBevelShade := Colors.Canvas.Pixels[clkAge0 + Age, cliBevelShade];
      clTextLight := Colors.Canvas.Pixels[clkAge0 + Age, cliTextLight];
      clTextShade := Colors.Canvas.Pixels[clkAge0 + Age, cliTextShade];
      clLitText := Colors.Canvas.Pixels[clkAge0 + Age, cliLitText];
      clMark := Colors.Canvas.Pixels[clkAge0 + Age, cliMark];
      clPage := Colors.Canvas.Pixels[clkAge0 + Age, cliPage];
      clCover := Colors.Canvas.Pixels[clkAge0 + Age, cliCover];
      Result := True;
    end
  else
    Result := False;
end;

procedure LoadPhrases;
begin
  if Phrases = nil then
    Phrases := TStringTable.Create;
  if Phrases2 = nil then
    Phrases2 := TStringTable.Create;
  Phrases2FallenBackToEnglish := False;
  if FileExists(LocalizedFilePath('Language.txt')) then
  begin
    Phrases.loadfromfile(LocalizedFilePath('Language.txt'));
    if FileExists(LocalizedFilePath('Language2.txt')) then
      Phrases2.loadfromfile(LocalizedFilePath('Language2.txt'))
    else
    begin
      Phrases2.loadfromfile(HomeDir + 'Language2.txt');
      Phrases2FallenBackToEnglish := True;
    end;
  end
  else
  begin
    Phrases.loadfromfile(HomeDir + 'Language.txt');
    Phrases2.loadfromfile(HomeDir + 'Language2.txt');
  end;

  if Sounds = nil then
    Sounds := TStringTable.Create;
  if not Sounds.loadfromfile(HomeDir + 'Sounds' + DirectorySeparator + 'sound.txt') then
  begin
    FreeAndNil(Sounds);
  end;
end;

procedure LoadFonts;
var
  Section: TFontType;
  FontScript: TextFile;
  Size: integer;
  S: string;
  I: integer;
  P: integer;
begin
  for Section := Low(TFontType) to High(TFontType) do
    UniFont[Section] := TFont.Create;

  Section := ftNormal;
  AssignFile(FontScript, LocalizedFilePath('Fonts.txt'));
  try
    Reset(fontscript);
    while not EOF(FontScript) do begin
      ReadLn(FontScript, s);
      if s <> '' then
        if s[1] = '#' then begin
          s := TrimRight(s);
          if s = '#SMALL' then
            Section := ftSmall
          else if s = '#TINY' then
            Section := ftTiny
          else if s = '#CAPTION' then
            Section := ftCaption
          else if s = '#BUTTON' then
            Section := ftButton
          else
            Section := ftNormal;
        end else begin
          p := Pos(',', s);
          if p > 0 then begin
            UniFont[section].Name := Trim(Copy(s, 1, p - 1));
            Size := 0;
            for i := p + 1 to Length(s) do
              case s[i] of
                '0' .. '9':
                  Size := Size * 10 + Byte(s[i]) - 48;
                'B', 'b':
                  UniFont[section].Style := UniFont[section].Style + [fsBold];
                'I', 'i':
                  UniFont[section].Style := UniFont[section].Style + [fsItalic];
              end;
            // 0.8 constant is compensation for Lazarus as size of fonts against Delphi differs
            UniFont[section].Size :=
              Round(size * Screen.PixelsPerInch / UniFont[section].PixelsPerInch * 0.8);
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

procedure UnitInit;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  with Reg do
    try
      OpenKey(AppRegistryKey, True);
      if ValueExists('Gamma') then
        Gamma := ReadInteger('Gamma')
      else
      begin
        Gamma := 100;
        WriteInteger('Gamma', Gamma);
      end;
      if ValueExists('Locale') then
        LocaleCode := ReadString('Locale')
      else
        LocaleCode := '';
    finally
      Free;
    end;

  if Gamma <> 100 then InitGammaLookupTable;

  {$IFDEF WINDOWS}
  EnumDisplaySettings(nil, $FFFFFFFF, StartResolution);
  ResolutionChanged := False;
  {$ENDIF}

  LoadPhrases;

  LogoBuffer := TBitmap.Create;
  LogoBuffer.PixelFormat := pf24bit;
  LogoBuffer.SetSize(wBBook, hBBook);

  LoadFonts;

  nGrExt := 0;
  HGrSystem := LoadGraphicSet('System.png');
  HGrSystem2 := LoadGraphicSet('System2.png');
  Templates := TBitmap.Create;
  Templates.PixelFormat := pf24bit;
  LoadGraphicFile(Templates, HomeDir + 'Graphics' + DirectorySeparator +
    'Templates.png', gfNoGamma);
  Colors := TBitmap.Create;
  Colors.PixelFormat := pf24bit;
  LoadGraphicFile(Colors, HomeDir + 'Graphics' + DirectorySeparator + 'Colors.png');
  Paper := TBitmap.Create;
  Paper.PixelFormat := pf24bit;
  LoadGraphicFile(Paper, HomeDir + 'Graphics' + DirectorySeparator + 'Paper.jpg');
  BigImp := TBitmap.Create;
  BigImp.PixelFormat := pf24bit;
  LoadGraphicFile(BigImp, HomeDir + 'Graphics' + DirectorySeparator + 'Icons.png');
  MainTexture.Image := TBitmap.Create;
  MainTextureAge := -2;
  ClickFrameColor := GrExt[HGrSystem].Data.Canvas.Pixels[187, 175];
  InitOrnamentDone := False;
  GenerateNames := True;
end;

procedure UnitDone;
var
  Reg: TRegistry;
  I: integer;
begin
  Reg := TRegistry.Create;
  with Reg do
    try
      OpenKey(AppRegistryKey, True);
      WriteString('Locale', LocaleCode);
      WriteInteger('Gamma', Gamma);
      if FullScreen then WriteInteger('ScreenMode', 1)
        else WriteInteger('ScreenMode', 0);
    finally
      Free;
    end;

  RestoreResolution;
  for I := 0 to nGrExt - 1 do begin
    GrExt[I].Data.Free;
    GrExt[I].Mask.Free;
    FreeMem(GrExt[I]);
  end;

  ReleaseFonts;

  FreeAndNil(Phrases);
  FreeAndNil(Phrases2);
  if Sounds <> nil then
    FreeAndNil(Sounds);
  FreeAndNil(LogoBuffer);
  FreeAndNil(BigImp);
  FreeAndNil(Paper);
  FreeAndNil(Templates);
  FreeAndNil(Colors);
  FreeAndNil(MainTexture.Image);
end;

end.
