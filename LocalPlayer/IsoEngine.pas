{$INCLUDE Switches.inc}
unit IsoEngine;

interface

uses
  Protocol, ClientTools, ScreenTools, Tribes, {$IFNDEF SCR}Term, {$ENDIF}
  LCLIntf, LCLType, SysUtils, Classes, Graphics, UPixelPointer;

type
  TInitEnemyModelEvent = function(emix: integer): boolean;

  { TIsoMap }

  TIsoMap = class
    constructor Create;
    procedure SetOutput(Output: TBitmap);
    procedure SetPaintBounds(Left, Top, Right, Bottom: integer);
    procedure Paint(x, y, Loc, nx, ny, CityLoc, CityOwner: integer;
      UseBlink: boolean = false; CityAllowClick: boolean = false);
    procedure PaintUnit(x, y: integer; const UnitInfo: TUnitInfo;
      Status: integer);
    procedure PaintCity(x, y: integer; const CityInfo: TCityInfo;
      accessory: boolean = true);
    procedure BitBltBitmap(Src: TBitmap; x, y, Width, Height, xSrc, ySrc,
      Rop: integer);

    procedure AttackBegin(const ShowMove: TShowMove);
    procedure AttackEffect(const ShowMove: TShowMove);
    procedure AttackEnd;

  private
    procedure CityGrid(xm, ym: integer; CityAllowClick: Boolean);
    function IsShoreTile(Loc: integer): boolean;
    procedure MakeDark(Line: PPixelPointer; Length: Integer);
    procedure ShadeOutside(x0, y0, x1, y1, xm, ym: integer);
  protected
    FOutput: TBitmap;
    FLeft, FTop, FRight, FBottom, RealTop, RealBottom, AttLoc, DefLoc,
      DefHealth, FAdviceLoc: integer;
    DataCanvas: TCanvas;
    MaskCanvas: TCanvas;
    function Connection4(Loc, Mask, Value: integer): integer;
    function Connection8(Loc, Mask: integer): integer;
    function OceanConnection(Loc: integer): integer;
    procedure PaintShore(x, y, Loc: integer);
    procedure PaintTileExtraTerrain(x, y, Loc: integer);
    procedure PaintTileObjects(x, y, Loc, CityLoc, CityOwner: integer;
      UseBlink: boolean);
    procedure PaintGrid(x, y, nx, ny: integer);
    procedure FillRect(x, y, Width, Height, Color: integer);
    procedure Textout(x, y, Color: integer; const s: string);
    procedure Sprite(HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
    procedure TSprite(xDst, yDst, grix: integer; PureBlack: boolean = false);

  public
    property AdviceLoc: integer read FAdviceLoc write FAdviceLoc;
  end;

const
  // options switched by buttons
  moPolitical = 0;
  moCityNames = 1;
  moGreatWall = 4;
  moGrid = 5;
  moBareTerrain = 6;

  // other options
  moEditMode = 16;
  moLocCodes = 17;

var
  NoMap: TIsoMap;
  Options: integer;
  pDebugMap: integer; // -1 for off

function IsJungle(y: integer): boolean;
procedure Init(InitEnemyModelHandler: TInitEnemyModelEvent);
function ApplyTileSize(xxtNew, yytNew: integer): boolean;
procedure Done;
procedure Reset;

implementation

const
  ShoreDither = fGrass;
  TerrainIconLines = 21;
  TerrainIconCols = 9;

  // sprites indexes
  spRow2 = 2 * TerrainIconCols + 6;
  spBlink1 = 1 * TerrainIconCols + 8;
  spBlink2 = 2 * TerrainIconCols + 8;
  spPrefStartPos = 1 * TerrainIconCols;
  spStartPos = 2 * TerrainIconCols;
  spPlain = 2 * TerrainIconCols + 7;
  spForest = 3 * TerrainIconCols;
  spRoad = 9 * TerrainIconCols;
  spRailRoad = 10 * TerrainIconCols;
  spCanal = 11 * TerrainIconCols;
  spIrrigation = 12 * TerrainIconCols;
  spFarmLand = 12 * TerrainIconCols + 1;
  spMine = 12 * TerrainIconCols + 2;
  spFortFront = 12 * TerrainIconCols + 3;
  spBase = 12 * TerrainIconCols + 4;
  spSpacePort = 12 * TerrainIconCols + 5;
  spPollution = 12 * TerrainIconCols + 6;
  spFortBack = 12 * TerrainIconCols + 7;
  spMinerals = 12 * TerrainIconCols + 8;
  spRiver = 13 * TerrainIconCols;
  spRiverMouths = 15 * TerrainIconCols;
  spGrid = 15 * TerrainIconCols + 6;
  spJungle = 18 * TerrainIconCols;
  spCanalMouths = 20 * TerrainIconCols;

var
  BordersOK: integer;
  OnInitEnemyModel: TInitEnemyModelEvent;
  LandPatch, OceanPatch, Borders: TBitmap;
  TSpriteSize: array [0 .. TerrainIconLines * 9 - 1] of TRect;
  DebugMap: ^TTileList;
  CitiesPictures: array [2 .. 3, 0 .. 3] of TCityPicture;
  FoW, ShowLoc, ShowCityNames, ShowObjects, ShowBorder, ShowMyBorder,
    ShowGrWall, ShowDebug: boolean;

function IsJungle(y: integer): boolean;
begin
  result := (y > (G.ly - 2) div 4) and (G.ly - 1 - y > (G.ly - 2) div 4)
end;

procedure Init(InitEnemyModelHandler: TInitEnemyModelEvent);
begin
  OnInitEnemyModel := InitEnemyModelHandler;
  if NoMap <> nil then
    NoMap.Free;
  NoMap := TIsoMap.Create;
end;

function ApplyTileSize(xxtNew, yytNew: integer): boolean;
var
  i, x, y, xSrc, ySrc, HGrTerrainNew, HGrCitiesNew, age, size: integer;
  LandMore, OceanMore, DitherMask, Mask24: TBitmap;
  MaskLine: array [0 .. 50 * 3 - 1] of TPixelPointer; // 32 = assumed maximum for yyt
  Border: boolean;
begin
  result := false;
  HGrTerrainNew := LoadGraphicSet(Format('Terrain%dx%d.png',
    [xxtNew * 2, yytNew * 2]));
  if HGrTerrainNew < 0 then
    exit;
  HGrCitiesNew := LoadGraphicSet(Format('Cities%dx%d.png',
    [xxtNew * 2, yytNew * 2]));
  if HGrCitiesNew < 0 then
    exit;
  xxt := xxtNew;
  yyt := yytNew;
  HGrTerrain := HGrTerrainNew;
  HGrCities := HGrCitiesNew;
  result := true;

  // prepare age 2+3 cities
  for age := 2 to 3 do
    for size := 0 to 3 do
      with CitiesPictures[age, size] do
        FindPosition(HGrCities, size * (xxt * 2 + 1), (age - 2) * (yyt * 3 + 1),
          xxt * 2 - 1, yyt * 3 - 1, $00FFFF, xShield, yShield);

  { prepare dithered ground tiles }
  if LandPatch <> nil then
    LandPatch.Free;
  LandPatch := TBitmap.Create;
  LandPatch.PixelFormat := pf24bit;
  LandPatch.Canvas.Brush.Color := 0;
  LandPatch.SetSize(xxt * 18, yyt * 9);
  LandPatch.Canvas.FillRect(0, 0, LandPatch.Width, LandPatch.Height);
  if OceanPatch <> nil then
    OceanPatch.Free;
  OceanPatch := TBitmap.Create;
  OceanPatch.PixelFormat := pf24bit;
  OceanPatch.Canvas.Brush.Color := 0;
  OceanPatch.SetSize(xxt * 8, yyt * 4);
  OceanPatch.Canvas.FillRect(0, 0, OceanPatch.Width, OceanPatch.Height);
  LandMore := TBitmap.Create;
  LandMore.PixelFormat := pf24bit;
  LandMore.Canvas.Brush.Color := 0;
  LandMore.SetSize(xxt * 18, yyt * 9);
  LandMore.Canvas.FillRect(0, 0, LandMore.Width, LandMore.Height);
  OceanMore := TBitmap.Create;
  OceanMore.PixelFormat := pf24bit;
  OceanMore.Canvas.Brush.Color := 0;
  OceanMore.SetSize(xxt * 8, yyt * 4);
  OceanMore.Canvas.FillRect(0, 0, OceanMore.Width, OceanMore.Height);
  DitherMask := TBitmap.Create;
  DitherMask.PixelFormat := pf24bit;
  DitherMask.SetSize(xxt * 2, yyt * 2);
  DitherMask.Canvas.FillRect(0, 0, DitherMask.Width, DitherMask.Height);
  BitBltCanvas(DitherMask.Canvas, 0, 0, xxt * 2, yyt * 2,
    GrExt[HGrTerrain].Mask.Canvas, 1 + 7 * (xxt * 2 + 1),
    1 + yyt + 15 * (yyt * 3 + 1), SRCAND);

  for x := -1 to 6 do
  begin
    if x = -1 then
    begin
      xSrc := ShoreDither * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt
    end
    else if x = 6 then
    begin
      xSrc := 1 + (xxt * 2 + 1) * 2;
      ySrc := 1 + yyt + (yyt * 3 + 1) * 2
    end
    else
    begin
      xSrc := (x + 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt
    end;
    for y := -1 to 6 do
      BitBltCanvas(LandPatch.Canvas, (x + 2) * (xxt * 2), (y + 2) * yyt,
        xxt * 2, yyt, GrExt[HGrTerrain].Data.Canvas, xSrc, ySrc);
    for y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (x + 2) * (xxt * 2), (y + 2) * yyt, xxt,
        yyt, GrExt[HGrTerrain].Data.Canvas, xSrc + xxt, ySrc + yyt,
        SRCPAINT);
    for y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (x + 2) * (xxt * 2) + xxt, (y + 2) * yyt,
        xxt, yyt, GrExt[HGrTerrain].Data.Canvas, xSrc, ySrc + yyt,
        SRCPAINT);
    for y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (x + 2) * (xxt * 2), (y + 2) * yyt, xxt,
        yyt, DitherMask.Canvas, xxt, yyt, SRCAND);
    for y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (x + 2) * (xxt * 2) + xxt, (y + 2) * yyt,
        xxt, yyt, DitherMask.Canvas, 0, yyt, SRCAND);
  end;

  for y := -1 to 6 do
  begin
    if y = -1 then
    begin
      xSrc := ShoreDither * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt
    end
    else if y = 6 then
    begin
      xSrc := 1 + 2 * (xxt * 2 + 1);
      ySrc := 1 + yyt + 2 * (yyt * 3 + 1)
    end
    else
    begin
      xSrc := (y + 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt
    end;
    for x := -2 to 6 do
      BitBltCanvas(LandMore.Canvas, (x + 2) * (xxt * 2), (y + 2) * yyt,
        xxt * 2, yyt, GrExt[HGrTerrain].Data.Canvas, xSrc, ySrc);
    BitBltCanvas(LandMore.Canvas, xxt * 2, (y + 2) * yyt, xxt, yyt,
      GrExt[HGrTerrain].Data.Canvas, xSrc + xxt, ySrc + yyt, SRCPAINT);
    for x := 0 to 7 do
      BitBltCanvas(LandMore.Canvas, (x + 2) * (xxt * 2) - xxt, (y + 2) * yyt,
        xxt * 2, yyt, GrExt[HGrTerrain].Data.Canvas, xSrc, ySrc + yyt,
        SRCPAINT);
    for x := -2 to 6 do
      BitBltCanvas(LandMore.Canvas, (x + 2) * (xxt * 2), (y + 2) * yyt,
        xxt * 2, yyt, DitherMask.Canvas, 0, 0, SRCAND);
  end;

  for x := 0 to 3 do
    for y := 0 to 3 do
    begin
      if (x = 1) and (y = 1) then
        xSrc := 1
      else
        xSrc := (x mod 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
      if (x >= 1) = (y >= 2) then
        BitBltCanvas(OceanPatch.Canvas, x * (xxt * 2), y * yyt, xxt * 2, yyt,
          GrExt[HGrTerrain].Data.Canvas, xSrc, ySrc);
      if (x >= 1) and ((y < 2) or (x >= 2)) then
      begin
        BitBltCanvas(OceanPatch.Canvas, x * (xxt * 2), y * yyt, xxt, yyt,
          GrExt[HGrTerrain].Data.Canvas, xSrc + xxt, ySrc + yyt,
          SRCPAINT);
        BitBltCanvas(OceanPatch.Canvas, x * (xxt * 2) + xxt, y * yyt, xxt, yyt,
          GrExt[HGrTerrain].Data.Canvas, xSrc, ySrc + yyt, SRCPAINT);
      end;
      BitBltCanvas(OceanPatch.Canvas, x * (xxt * 2), y * yyt, xxt, yyt,
        DitherMask.Canvas, xxt, yyt, SRCAND);
      BitBltCanvas(OceanPatch.Canvas, x * (xxt * 2) + xxt, y * yyt, xxt, yyt,
        DitherMask.Canvas, 0, yyt, SRCAND);
    end;

  for y := 0 to 3 do
    for x := 0 to 3 do
    begin
      if (x = 1) and (y = 1) then
        xSrc := 1
      else
        xSrc := (y mod 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
      if (x < 1) or (y >= 2) then
        BitBltCanvas(OceanMore.Canvas, x * (xxt * 2), y * yyt, xxt * 2, yyt,
          GrExt[HGrTerrain].Data.Canvas, xSrc, ySrc);
      if (x = 1) and (y < 2) or (x >= 2) and (y >= 1) then
      begin
        BitBltCanvas(OceanMore.Canvas, x * (xxt * 2), y * yyt, xxt, yyt,
          GrExt[HGrTerrain].Data.Canvas, xSrc + xxt, ySrc + yyt,
          SRCPAINT);
        BitBltCanvas(OceanMore.Canvas, x * (xxt * 2) + xxt, y * yyt, xxt, yyt,
          GrExt[HGrTerrain].Data.Canvas, xSrc, ySrc + yyt, SRCPAINT);
      end;
      BitBltCanvas(OceanMore.Canvas, x * (xxt * 2), y * yyt, xxt * 2, yyt,
        DitherMask.Canvas, 0, 0, SRCAND);
    end;

  BitBltCanvas(DitherMask.Canvas, 0, 0, xxt * 2, yyt * 2,
    DitherMask.Canvas, 0, 0, DSTINVERT); { invert dither mask }
  BitBltCanvas(DitherMask.Canvas, 0, 0, xxt * 2, yyt * 2,
    GrExt[HGrTerrain].Mask.Canvas, 1, 1 + yyt, SRCPAINT);

  for x := -1 to 6 do
    for y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (x + 2) * (xxt * 2), (y + 2) * yyt,
        xxt * 2, yyt, DitherMask.Canvas, 0, 0, SRCAND);

  for y := -1 to 6 do
    for x := -2 to 7 do
      BitBltCanvas(LandMore.Canvas, (x + 2) * (xxt * 2) - xxt, (y + 2) * yyt,
        xxt * 2, yyt, DitherMask.Canvas, 0, yyt, SRCAND);

  BitBltCanvas(LandPatch.Canvas, 0, 0, (xxt * 2) * 9, yyt * 9,
    LandMore.Canvas, 0, 0, SRCPAINT);

  for x := 0 to 3 do
    for y := 0 to 3 do
      BitBltCanvas(OceanPatch.Canvas, x * (xxt * 2), y * yyt, xxt * 2, yyt,
        DitherMask.Canvas, 0, 0, SRCAND);

  for y := 0 to 3 do
    for x := 0 to 4 do
      BitBltCanvas(OceanMore.Canvas, x * (xxt * 2) - xxt, y * yyt, xxt * 2,
        yyt, DitherMask.Canvas, 0, yyt, SRCAND);

  BitBltCanvas(OceanPatch.Canvas, 0, 0, (xxt * 2) * 4, yyt * 4,
    OceanMore.Canvas, 0, 0, SRCPAINT);

  with DitherMask.Canvas do
  begin
    Brush.Color := $FFFFFF;
    FillRect(Rect(0, 0, xxt * 2, yyt));
  end;
  BitBltCanvas(DitherMask.Canvas, 0, 0, xxt * 2, yyt,
    GrExt[HGrTerrain].Mask.Canvas, 1, 1 + yyt);

  for x := 0 to 6 do
    BitBltCanvas(LandPatch.Canvas, (x + 2) * (xxt * 2), yyt, xxt * 2, yyt,
      DitherMask.Canvas, 0, 0, SRCAND);
  BitBltCanvas(DitherMask.Canvas, 0, 0, xxt * 2, yyt, DitherMask.Canvas,
    0, 0, DSTINVERT);

  for y := 0 to 6 do
    BitBltCanvas(LandPatch.Canvas, xxt * 2, (y + 2) * yyt, xxt * 2, yyt,
      DitherMask.Canvas, 0, 0, SRCAND);

  LandMore.Free;
  OceanMore.Free;
  DitherMask.Free;

  // reduce size of terrain icons
  Mask24 := TBitmap.Create;
  Mask24.Assign(GrExt[HGrTerrain].Mask);
  Mask24.PixelFormat := pf24bit;
  Mask24.BeginUpdate;
  for ySrc := 0 to TerrainIconLines - 1 do
  begin
    for i := 0 to yyt * 3 - 1 do
      MaskLine[i] := PixelPointer(Mask24, 0, 1 + ySrc * (yyt * 3 + 1) + i);
    for xSrc := 0 to 9 - 1 do
    begin
      i := ySrc * 9 + xSrc;
      TSpriteSize[i].Left := 0;
      repeat
        Border := true;
        for y := 0 to yyt * 3 - 1 do begin
          MaskLine[y].SetX(1 + xSrc * (xxt * 2 + 1) + TSpriteSize[i].Left);
          if MaskLine[y].Pixel^.B = 0 then Border := false;
        end;
        if Border then Inc(TSpriteSize[i].Left);
      until not Border or (TSpriteSize[i].Left = xxt * 2 - 1);
      TSpriteSize[i].Top := 0;
      repeat
        Border := true;
        for x := 0 to xxt * 2 - 1 do begin
          MaskLine[TSpriteSize[i].Top].SetX(1 + xSrc * (xxt * 2 + 1) + x);
          if MaskLine[TSpriteSize[i].Top].Pixel^.B = 0 then Border := false;
        end;
        if Border then inc(TSpriteSize[i].Top);
      until not Border or (TSpriteSize[i].Top = yyt * 3 - 1);
      TSpriteSize[i].Right := xxt * 2;
      repeat
        Border := true;
        for y := 0 to yyt * 3 - 1 do begin
          MaskLine[y].SetX(xSrc * (xxt * 2 + 1) + TSpriteSize[i].Right);
          if MaskLine[y].Pixel^.B = 0 then Border := false;
        end;
        if Border then Dec(TSpriteSize[i].Right);
      until not Border or (TSpriteSize[i].Right = TSpriteSize[i].Left);
      TSpriteSize[i].Bottom := yyt * 3;
      repeat
        Border := true;
        for x := 0 to xxt * 2 - 1 do begin
          MaskLine[TSpriteSize[i].Bottom - 1].SetX(1 + xSrc * (xxt * 2 + 1) + x);
          if MaskLine[TSpriteSize[i].Bottom - 1].Pixel^.B = 0 then Border := false;
        end;
        if Border then Dec(TSpriteSize[i].Bottom);
      until not Border or (TSpriteSize[i].Bottom = TSpriteSize[i].Top);
    end
  end;
  Mask24.EndUpdate;
  Mask24.Free;

  if Borders <> nil then
    Borders.Free;
  Borders := TBitmap.Create;
  Borders.PixelFormat := pf24bit;
  Borders.SetSize(xxt * 2, (yyt * 2) * nPl);
  Borders.Canvas.FillRect(0, 0, Borders.Width, Borders.Height);
  BordersOK := 0;
end;

procedure Done;
begin
  FreeAndNil(NoMap);
  FreeAndNil(LandPatch);
  FreeAndNil(OceanPatch);
  FreeAndNil(Borders);
end;

procedure Reset;
begin
  BordersOK := 0;
end;

constructor TIsoMap.Create;
begin
  inherited;
  FLeft := 0;
  FTop := 0;
  FRight := 0;
  FBottom := 0;
  AttLoc := -1;
  DefLoc := -1;
  FAdviceLoc := -1;
end;

procedure TIsoMap.SetOutput(Output: TBitmap);
begin
  FOutput := Output;
  FLeft := 0;
  FTop := 0;
  FRight := FOutput.Width;
  FBottom := FOutput.Height;
end;

procedure TIsoMap.SetPaintBounds(Left, Top, Right, Bottom: integer);
begin
  FLeft := Left;
  FTop := Top;
  FRight := Right;
  FBottom := Bottom;
end;

procedure TIsoMap.FillRect(x, y, Width, Height, Color: integer);
begin
  if x < FLeft then
  begin
    Width := Width - (FLeft - x);
    x := FLeft;
  end;
  if y < FTop then
  begin
    Height := Height - (FTop - y);
    y := FTop;
  end;
  if x + Width >= FRight then
    Width := FRight - x;
  if y + Height >= FBottom then
    Height := FBottom - y;
  if (Width <= 0) or (Height <= 0) then
    exit;

  FOutput.Canvas.Brush.Color := Color;
  FOutput.Canvas.FillRect(Rect(x, y, x + Width, y + Height));
  FOutput.Canvas.Brush.Style := bsClear;
end;

procedure TIsoMap.Textout(x, y, Color: integer; const s: string);
begin
  FOutput.Canvas.Font.Color := Color;
  FOutput.Canvas.TextRect(Rect(FLeft, FTop, FRight, FBottom), x, y, s)
end;

procedure TIsoMap.BitBltBitmap(Src: TBitmap; x, y, Width, Height, xSrc, ySrc,
  Rop: integer);
begin
  if x < FLeft then
  begin
    Width := Width - (FLeft - x);
    xSrc := xSrc + (FLeft - x);
    x := FLeft;
  end;
  if y < FTop then
  begin
    Height := Height - (FTop - y);
    ySrc := ySrc + (FTop - y);
    y := FTop;
  end;
  if x + Width >= FRight then
    Width := FRight - x;
  if y + Height >= FBottom then
    Height := FBottom - y;
  if (Width <= 0) or (Height <= 0) then
    exit;

  BitBltCanvas(FOutput.Canvas, x, y, Width, Height, Src.Canvas, xSrc, ySrc, Rop);
end;

procedure TIsoMap.Sprite(HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
begin
  BitBltBitmap(GrExt[HGr].Mask, xDst, yDst, Width, Height, xGr, yGr, SRCAND);
  BitBltBitmap(GrExt[HGr].Data, xDst, yDst, Width, Height, xGr, yGr, SRCPAINT);
end;

procedure TIsoMap.TSprite(xDst, yDst, grix: integer;
  PureBlack: boolean = false);
var
  Width, Height, xSrc, ySrc: integer;
begin
  Width := TSpriteSize[grix].Right - TSpriteSize[grix].Left;
  Height := TSpriteSize[grix].Bottom - TSpriteSize[grix].Top;
  xSrc := 1 + grix mod 9 * (xxt * 2 + 1) + TSpriteSize[grix].Left;
  ySrc := 1 + grix div 9 * (yyt * 3 + 1) + TSpriteSize[grix].Top;
  xDst := xDst + TSpriteSize[grix].Left;
  yDst := yDst - yyt + TSpriteSize[grix].Top;
  if xDst < FLeft then
  begin
    Width := Width - (FLeft - xDst);
    xSrc := xSrc + (FLeft - xDst);
    xDst := FLeft
  end;
  if yDst < FTop then
  begin
    Height := Height - (FTop - yDst);
    ySrc := ySrc + (FTop - yDst);
    yDst := FTop
  end;
  if xDst + Width >= FRight then
    Width := FRight - xDst;
  if yDst + Height >= FBottom then
    Height := FBottom - yDst;
  if (Width <= 0) or (Height <= 0) then
    exit;

  BitBltCanvas(FOutput.Canvas, xDst, yDst, Width, Height, MaskCanvas, xSrc, ySrc, SRCAND);
  if not PureBlack then
    BitBltCanvas(FOutput.Canvas, xDst, yDst, Width, Height, DataCanvas, xSrc, ySrc, SRCPAINT);
end;

procedure TIsoMap.PaintUnit(x, y: integer; const UnitInfo: TUnitInfo;
  Status: integer);
var
  xsh, ysh, xGr, yGr, j, mixShow: integer;
begin
  with UnitInfo do
    if (Owner = me) or (emix <> $FFFF) then
    begin
      if Job = jCity then
        mixShow := -1 // building site
      else
        mixShow := mix;
      if (Tribe[Owner].ModelPicture[mixShow].HGr = 0) and
        (@OnInitEnemyModel <> nil) then
        if not OnInitEnemyModel(emix) then
          exit;
      xsh := Tribe[Owner].ModelPicture[mixShow].xShield;
      ysh := Tribe[Owner].ModelPicture[mixShow].yShield;
{$IFNDEF SCR} if Status and usStay <> 0 then
        j := 19
      else if Status and usRecover <> 0 then
        j := 16
      else if Status and (usGoto or usEnhance) = usGoto or usEnhance then
        j := 18
      else if Status and usEnhance <> 0 then
        j := 17
      else if Status and usGoto <> 0 then
        j := 20
      else {$ENDIF} if Job = jCity then
          j := jNone
        else
          j := Job;
      if Flags and unMulti <> 0 then
        Sprite(Tribe[Owner].symHGr, x + xsh - 1 + 4, y + ysh - 2, 14, 12,
          33 + Tribe[Owner].sympix mod 10 * 65,
          1 + Tribe[Owner].sympix div 10 * 49);
      Sprite(Tribe[Owner].symHGr, x + xsh - 1, y + ysh - 2, 14, 12,
        18 + Tribe[Owner].sympix mod 10 * 65,
        1 + Tribe[Owner].sympix div 10 * 49);
      FillRect(x + xsh, y + ysh + 5, 1 + Health * 11 div 100, 3,
        ColorOfHealth(Health));
      if j > 0 then
      begin
        xGr := 121 + j mod 7 * 9;
        yGr := 1 + j div 7 * 9;
        BitBltBitmap(GrExt[HGrSystem].Mask, x + xsh + 3, y + ysh + 9, 8, 8, xGr,
          yGr, SRCAND);
        Sprite(HGrSystem, x + xsh + 2, y + ysh + 8, 8, 8, xGr, yGr);
      end;
      with Tribe[Owner].ModelPicture[mixShow] do
        Sprite(HGr, x, y, 64, 48, pix mod 10 * 65 + 1, pix div 10 * 49 + 1);
      if Flags and unFortified <> 0 then
      begin
        { DataCanvas:=GrExt[HGrTerrain].Data.Canvas;
          MaskCanvas:=GrExt[HGrTerrain].Mask.Canvas;
          TSprite(x,y+16,12*9+7); }
        Sprite(HGrStdUnits, x, y, xxu * 2, yyu * 2, 1 + 6 * (xxu * 2 + 1), 1);
      end;
    end;
end; { PaintUnit }

procedure TIsoMap.PaintCity(x, y: integer; const CityInfo: TCityInfo;
  accessory: boolean);
var
  age, cHGr, cpix, xGr, xShield, yShield, LabelTextColor, LabelLength: integer;
  cpic: TCityPicture;
  s: string;
begin
  age := GetAge(CityInfo.Owner);
  if CityInfo.size < 5 then
    xGr := 0
  else if CityInfo.size < 9 then
    xGr := 1
  else if CityInfo.size < 13 then
    xGr := 2
  else
    xGr := 3;
  Tribe[CityInfo.Owner].InitAge(age);
  if age < 2 then
  begin
    cHGr := Tribe[CityInfo.Owner].cHGr;
    cpix := Tribe[CityInfo.Owner].cpix;
    if (ciWalled and CityInfo.Flags = 0) or
      (GrExt[cHGr].Data.Canvas.Pixels[(xGr + 4) * 65, cpix * 49 + 48] = $00FFFF)
    then
      Sprite(cHGr, x - xxc, y - 2 * yyc, xxc * 2, yyc * 3,
        xGr * (xxc * 2 + 1) + 1, 1 + cpix * (yyc * 3 + 1));
    if ciWalled and CityInfo.Flags <> 0 then
      Sprite(cHGr, x - xxc, y - 2 * yyc, xxc * 2, yyc * 3,
        (xGr + 4) * (xxc * 2 + 1) + 1, 1 + cpix * (yyc * 3 + 1));
  end
  else
  begin
    if ciWalled and CityInfo.Flags <> 0 then
      Sprite(HGrCities, x - xxt, y - 2 * yyt, 2 * xxt, 3 * yyt,
        (xGr + 4) * (2 * xxt + 1) + 1, 1 + (age - 2) * (3 * yyt + 1))
    else
      Sprite(HGrCities, x - xxt, y - 2 * yyt, 2 * xxt, 3 * yyt,
        xGr * (2 * xxt + 1) + 1, 1 + (age - 2) * (3 * yyt + 1));
  end;

  if not accessory then
    exit;

  { if ciCapital and CityInfo.Flags<>0 then
    Sprite(Tribe[CityInfo.Owner].symHGr,x+cpic.xf,y-13+cpic.yf,13,14,
    1+Tribe[CityInfo.Owner].sympix mod 10 *65,
    1+Tribe[CityInfo.Owner].sympix div 10 *49); {capital -- paint flag }

  if MyMap[CityInfo.Loc] and fObserved <> 0 then
  begin
    if age < 2 then
    begin
      cpic := Tribe[CityInfo.Owner].CityPicture[xGr];
      xShield := x - xxc + cpic.xShield;
      yShield := y - 2 * yyc + cpic.yShield;
    end
    else
    begin
      cpic := CitiesPictures[age, xGr];
      xShield := x - xxt + cpic.xShield;
      yShield := y - 2 * yyt + cpic.yShield;
    end;
    s := IntToStr(CityInfo.size);
    LabelLength := FOutput.Canvas.TextWidth(s);
    FillRect(xShield, yShield, LabelLength + 4, 16, $000000);
    if MyMap[CityInfo.Loc] and (fUnit or fObserved) = fObserved then
      // empty city
      LabelTextColor := Tribe[CityInfo.Owner].Color
    else
    begin
      FillRect(xShield + 1, yShield + 1, LabelLength + 2, 14,
        Tribe[CityInfo.Owner].Color);
      LabelTextColor := $000000;
    end;
    Textout(xShield + 2, yShield - 1, LabelTextColor, s);
  end
end; { PaintCity }

function PoleTile(Loc: integer): integer;
begin { virtual pole tile }
  result := fUNKNOWN;
  if Loc < -2 * G.lx then
  else if Loc < -G.lx then
  begin
    if (MyMap[dLoc(Loc, 0, 2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, -2, 2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 2, 2)] and fTerrain <> fUNKNOWN) then
      result := fArctic;
    if (MyMap[dLoc(Loc, 0, 2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, -2, 2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 2, 2)] and fObserved <> 0) then
      result := result or fObserved
  end
  else if Loc < 0 then
  begin
    if (MyMap[dLoc(Loc, -1, 1)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 1, 1)] and fTerrain <> fUNKNOWN) then
      result := fArctic;
    if (MyMap[dLoc(Loc, -1, 1)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 1, 1)] and fObserved <> 0) then
      result := result or fObserved
  end
  else if Loc < G.lx * (G.ly + 1) then
  begin
    if (MyMap[dLoc(Loc, -1, -1)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 1, -1)] and fTerrain <> fUNKNOWN) then
      result := fArctic;
    if (MyMap[dLoc(Loc, -1, -1)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 1, -1)] and fObserved <> 0) then
      result := result or fObserved
  end
  else if Loc < G.lx * (G.ly + 2) then
  begin
    if (MyMap[dLoc(Loc, 0, -2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, -2, -2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 2, -2)] and fTerrain <> fUNKNOWN) then
      result := fArctic;
    if (MyMap[dLoc(Loc, 0, -2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, -2, -2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 2, -2)] and fObserved <> 0) then
      result := result or fObserved
  end
end;

const
  Dirx: array [0 .. 7] of integer = (1, 2, 1, 0, -1, -2, -1, 0);
  Diry: array [0 .. 7] of integer = (-1, 0, 1, 2, 1, 0, -1, -2);

function TIsoMap.Connection4(Loc, Mask, Value: integer): integer;
begin
  result := 0;
  if dLoc(Loc, 1, -1) >= 0 then
  begin
    if MyMap[dLoc(Loc, 1, -1)] and Mask = Cardinal(Value) then
      inc(result, 1);
    if MyMap[dLoc(Loc, -1, -1)] and Mask = Cardinal(Value) then
      inc(result, 8);
  end;
  if dLoc(Loc, 1, 1) < G.lx * G.ly then
  begin
    if MyMap[dLoc(Loc, 1, 1)] and Mask = Cardinal(Value) then
      inc(result, 2);
    if MyMap[dLoc(Loc, -1, 1)] and Mask = Cardinal(Value) then
      inc(result, 4);
  end
end;

function TIsoMap.Connection8(Loc, Mask: integer): integer;
var
  Dir, ConnLoc: integer;
begin
  result := 0;
  for Dir := 0 to 7 do
  begin
    ConnLoc := dLoc(Loc, Dirx[Dir], Diry[Dir]);
    if (ConnLoc >= 0) and (ConnLoc < G.lx * G.ly) and
      (MyMap[ConnLoc] and Mask <> 0) then
      inc(result, 1 shl Dir);
  end
end;

function TIsoMap.OceanConnection(Loc: integer): integer;
var
  Dir, ConnLoc: integer;
begin
  result := 0;
  for Dir := 0 to 7 do
  begin
    ConnLoc := dLoc(Loc, Dirx[Dir], Diry[Dir]);
    if (ConnLoc < 0) or (ConnLoc >= G.lx * G.ly) or
      ((MyMap[ConnLoc] - 2) and fTerrain < 13) then
      inc(result, 1 shl Dir);
  end
end;

procedure TIsoMap.PaintShore(x, y, Loc: integer);
var
  Conn, Tile: integer;
begin
  if (y <= FTop - yyt * 2) or (y > FBottom) or (x <= FLeft - xxt * 2) or
    (x > FRight) then
    exit;
  if (Loc < 0) or (Loc >= G.lx * G.ly) then
    exit;
  Tile := MyMap[Loc];
  if Tile and fTerrain >= fGrass then
    exit;
  Conn := OceanConnection(Loc);
  if Conn = 0 then
    exit;

  BitBltBitmap(GrExt[HGrTerrain].Data, x + xxt div 2, y, xxt, yyt,
    1 + (Conn shr 6 + Conn and 1 shl 2) * (xxt * 2 + 1),
    1 + yyt + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  BitBltBitmap(GrExt[HGrTerrain].Data, x + xxt, y + yyt div 2, xxt, yyt,
    1 + (Conn and 7) * (xxt * 2 + 1) + xxt,
    1 + yyt * 2 + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  BitBltBitmap(GrExt[HGrTerrain].Data, x + xxt div 2, y + yyt, xxt, yyt,
    1 + (Conn shr 2 and 7) * (xxt * 2 + 1) + xxt,
    1 + yyt + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  BitBltBitmap(GrExt[HGrTerrain].Data, x, y + yyt div 2, xxt, yyt,
    1 + (Conn shr 4 and 7) * (xxt * 2 + 1),
    1 + yyt * 2 + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  Conn := Connection4(Loc, fTerrain, fUNKNOWN); { dither to black }
  if Conn and 1 <> 0 then
    BitBltBitmap(GrExt[HGrTerrain].Mask, x + xxt, y, xxt, yyt, 1 + 7 * (xxt * 2 + 1) +
      xxt, 1 + yyt + 15 * (yyt * 3 + 1), SRCAND);
  if Conn and 2 <> 0 then
    BitBltBitmap(GrExt[HGrTerrain].Mask, x + xxt, y + yyt, xxt, yyt,
      1 + 7 * (xxt * 2 + 1) + xxt, 1 + yyt * 2 + 15 * (yyt * 3 + 1), SRCAND);
  if Conn and 4 <> 0 then
    BitBltBitmap(GrExt[HGrTerrain].Mask, x, y + yyt, xxt, yyt, 1 + 7 * (xxt * 2 + 1),
      1 + yyt * 2 + 15 * (yyt * 3 + 1), SRCAND);
  if Conn and 8 <> 0 then
    BitBltBitmap(GrExt[HGrTerrain].Mask, x, y, xxt, yyt, 1 + 7 * (xxt * 2 + 1),
      1 + yyt + 15 * (yyt * 3 + 1), SRCAND);
end;

procedure TIsoMap.PaintTileExtraTerrain(x, y, Loc: integer);
var
  Dir, Conn, RRConn, yGr, Tile, yLoc: integer;
begin
  if (Loc < 0) or (Loc >= G.lx * G.ly) or (y <= -yyt * 2) or
    (y > FOutput.Height) or (x <= -xxt * 2) or (x > FOutput.Width) then
    exit;
  Tile := MyMap[Loc];
  if Tile and fTerrain = fForest then
  begin
    yLoc := Loc div G.lx;
    if IsJungle(yLoc) then
      yGr := spJungle
    else
      yGr := spForest;
    Conn := Connection4(Loc, fTerrain, Tile and fTerrain);
    if (yLoc = (G.ly - 2) div 4) or (G.ly - 1 - yLoc = (G.ly + 2) div 4) then
      Conn := Conn and not 6 // no connection to south
    else if (yLoc = (G.ly + 2) div 4) or (G.ly - 1 - yLoc = (G.ly - 2) div 4)
    then
      Conn := Conn and not 9; // no connection to north
    TSprite(x, y, yGr + Conn mod 8 + (Conn div 8) * TerrainIconCols);
  end
  else if Tile and fTerrain in [fHills, fMountains, fForest] then
  begin
    yGr := 3 + 2 * (Tile and fTerrain - fForest);
    Conn := Connection4(Loc, fTerrain, Tile and fTerrain);
    TSprite(x, y, Conn mod 8 + (yGr + Conn div 8) * TerrainIconCols);
  end
  else if Tile and fDeadLands <> 0 then
    TSprite(x, y, spRow2);

  if ShowObjects then
  begin
    if Tile and fTerImp = tiFarm then
      TSprite(x, y, spFarmLand)
    else if Tile and fTerImp = tiIrrigation then
      TSprite(x, y, spIrrigation);
  end;
  if Tile and fRiver <> 0 then
  begin
    Conn := Connection4(Loc, fRiver, fRiver) or
      Connection4(Loc, fTerrain, fShore) or Connection4(Loc, fTerrain,
      fUNKNOWN);
    TSprite(x, y, spRiver + Conn mod 8 + (Conn div 8) * TerrainIconCols);
  end;

  if Tile and fTerrain < fGrass then
  begin
    Conn := Connection4(Loc, fRiver, fRiver);
    for Dir := 0 to 3 do
      if Conn and (1 shl Dir) <> 0 then { river mouths }
        TSprite(x, y, spRiverMouths + Dir);
    if ShowObjects then
    begin
      Conn := Connection8(Loc, fCanal);
      for Dir := 0 to 7 do
        if Conn and (1 shl Dir) <> 0 then { canal mouths }
          TSprite(x, y, spCanalMouths + 1 + Dir);
    end
  end;

  if ShowObjects then
  begin
    if (Tile and fCanal <> 0) or (Tile and fCity <> 0) then
    begin // paint canal connections
      Conn := Connection8(Loc, fCanal or fCity);
      if Tile and fCanal <> 0 then
        Conn := Conn or ($FF - OceanConnection(Loc));
      if Conn = 0 then
      begin
        if Tile and fCanal <> 0 then
          TSprite(x, y, spCanal)
      end
      else
        for Dir := 0 to 7 do
          if (1 shl Dir) and Conn <> 0 then
            TSprite(x, y, spCanal + 1 + Dir);
    end;
    if Tile and (fRR or fCity) <> 0 then
      RRConn := Connection8(Loc, fRR or fCity)
    else
      RRConn := 0;
    if Tile and (fRoad or fRR or fCity) <> 0 then
    begin // paint road connections
      Conn := Connection8(Loc, fRoad or fRR or fCity) and not RRConn;
      if (Conn = 0) and (Tile and (fRR or fCity) = 0) then
        TSprite(x, y, spRoad)
      else if Conn > 0 then
        for Dir := 0 to 7 do
          if (1 shl Dir) and Conn <> 0 then
            TSprite(x, y, spRoad + 1 + Dir);
    end;
    // paint railroad connections
    if (Tile and fRR <> 0) and (RRConn = 0) then
      TSprite(x, y, spRailRoad)
    else if RRConn > 0 then
      for Dir := 0 to 7 do
        if (1 shl Dir) and RRConn <> 0 then
          TSprite(x, y, spRailRoad + 1 + Dir);
  end;
end;

// (x,y) is top left pixel of (2*xxt,3*yyt) rectangle
procedure TIsoMap.PaintTileObjects(x, y, Loc, CityLoc, CityOwner: integer;
  UseBlink: boolean);
var
  p1, p2, uix, cix, dy, Loc1, Tile, Multi, Destination: integer;
  CityInfo: TCityInfo;
  UnitInfo: TUnitInfo;
  fog: boolean;
  SpecialRow: Integer;
  SpecialCol: Integer;

  procedure NameCity;
  var
    cix, xs, w: integer;
    BehindCityInfo: TCityInfo;
    s: string;
    IsCapital: boolean;
  begin
    BehindCityInfo.Loc := Loc - 2 * G.lx;
    if ShowCityNames and (Options and (1 shl moEditMode) = 0) and
      (BehindCityInfo.Loc >= 0) and (BehindCityInfo.Loc < G.lx * G.ly) and
      (MyMap[BehindCityInfo.Loc] and fCity <> 0) then
    begin
      GetCityInfo(BehindCityInfo.Loc, cix, BehindCityInfo);
      IsCapital := BehindCityInfo.Flags and ciCapital <> 0;
      { if Showuix and (cix>=0) then s:=IntToStr(cix)
        else } s := CityName(BehindCityInfo.ID);
      w := FOutput.Canvas.TextWidth(s);
      xs := x + xxt - (w + 1) div 2;
      if IsCapital then
        FOutput.Canvas.Font.Style := FOutput.Canvas.Font.Style + [fsUnderline];
      Textout(xs + 1, y - 9, $000000, s);
      Textout(xs, y - 10, $FFFFFF, s);
      if IsCapital then
        FOutput.Canvas.Font.Style := FOutput.Canvas.Font.Style - [fsUnderline];
    end;
  end;

  procedure ShowSpacePort;
  begin
    if ShowObjects and (Options and (1 shl moEditMode) = 0) and
      (Tile and fCity <> 0) and (CityInfo.Flags and ciSpacePort <> 0) then
      TSprite(x + xxt, y - 6, spSpacePort);
  end;

  procedure PaintBorder;
  var
    dx, dy: integer;
    PixelPtr: TPixelPointer;
  begin
    if ShowBorder and (Loc >= 0) and (Loc < G.lx * G.ly) and
      (Tile and fTerrain <> fUNKNOWN) then
    begin
      p1 := MyRO.Territory[Loc];
      if (p1 >= 0) and (ShowMyBorder or (p1 <> me)) then
      begin
        if BordersOK and (1 shl p1) = 0 then
        begin
          // Clearing before BitBltBitmap SRCCOPY shouldn't be neccesary but for some
          // reason without it code works different then under Delphi
          Borders.Canvas.FillRect(Bounds(0, p1 * (yyt * 2), xxt * 2, yyt * 2));

          BitBltCanvas(Borders.Canvas, 0, p1 * (yyt * 2), xxt * 2,
            yyt * 2, GrExt[HGrTerrain].Data.Canvas,
            1 + 8 * (xxt * 2 + 1), 1 + yyt + 16 * (yyt * 3 + 1));
          Borders.BeginUpdate;
          for dy := 0 to yyt * 2 - 1 do
          begin
            PixelPtr := PixelPointer(Borders, 0, p1 * (yyt * 2) + dy);
            for dx := 0 to xxt * 2 - 1 do begin
              if PixelPtr.Pixel^.B = 99 then begin
                PixelPtr.Pixel^.B := Tribe[p1].Color shr 16 and $FF;
                PixelPtr.Pixel^.G := Tribe[p1].Color shr 8 and $FF;
                PixelPtr.Pixel^.R := Tribe[p1].Color and $FF;
              end;
              PixelPtr.NextPixel;
            end;
          end;
          Borders.EndUpdate;
          BordersOK := BordersOK or 1 shl p1;
        end;
        for dy := 0 to 1 do
          for dx := 0 to 1 do
          begin
            Loc1 := dLoc(Loc, dx * 2 - 1, dy * 2 - 1);
            begin
              if (Loc1 < 0) or (Loc1 >= G.lx * G.ly) then
                p2 := -1
              else if MyMap[Loc1] and fTerrain = fUNKNOWN then
                p2 := p1
              else
                p2 := MyRO.Territory[Loc1];
              if p2 <> p1 then
              begin
                BitBltBitmap(GrExt[HGrTerrain].Mask, x + dx * xxt, y + dy * yyt, xxt,
                  yyt, 1 + 8 * (xxt * 2 + 1) + dx * xxt,
                  1 + yyt + 16 * (yyt * 3 + 1) + dy * yyt, SRCAND);
                BitBltBitmap(Borders, x + dx * xxt, y + dy * yyt, xxt, yyt, dx * xxt,
                  p1 * (yyt * 2) + dy * yyt, SRCPAINT);
              end
            end;
          end
      end
    end;
  end;

begin
  if (Loc < 0) or (Loc >= G.lx * G.ly) then
    Tile := PoleTile(Loc)
  else
    Tile := MyMap[Loc];
  if ShowObjects and (Options and (1 shl moEditMode) = 0) and
    (Tile and fCity <> 0) then
    GetCityInfo(Loc, cix, CityInfo);
  if (y <= FTop - yyt * 2) or (y > FBottom) or (x <= FLeft - xxt * 2) or
    (x > FRight) then
  begin
    NameCity;
    ShowSpacePort;
    exit;
  end;
  if Tile and fTerrain = fUNKNOWN then
  begin
    NameCity;
    ShowSpacePort;
    exit
  end; { square not discovered }

  if not(FoW and (Tile and fObserved = 0)) then
    PaintBorder;

  if (Loc >= 0) and (Loc < G.lx * G.ly) and (Loc = FAdviceLoc) then
    TSprite(x, y, spPlain);

  if (Loc >= 0) and (Loc < G.lx * G.ly) and (Tile and fSpecial <> 0)
  then { special resources }
  begin
    dy := Loc div G.lx;
    SpecialCol := Tile and fTerrain;
    SpecialRow := Tile and fSpecial shr 5;
    if SpecialCol < fForest then
      TSprite(x, y, SpecialCol + SpecialRow * TerrainIconCols)
    else if (SpecialCol = fForest) and IsJungle(dy) then
      TSprite(x, y, spJungle - 1 + SpecialRow * TerrainIconCols)
    else
      TSprite(x, y, spForest - 1 + ((SpecialCol - fForest) * 2 + SpecialRow) * TerrainIconCols);
  end;

  if ShowObjects then
  begin
    if Tile and fTerImp = tiMine then
      TSprite(x, y, spMine);
    if Tile and fTerImp = tiBase then
      TSprite(x, y, spBase);
    if Tile and fPoll <> 0 then
      TSprite(x, y, spPollution);
    if Tile and fTerImp = tiFort then
    begin
      TSprite(x, y, spFortBack);
      if Tile and fObserved = 0 then
        TSprite(x, y, spFortFront);
    end;
  end;
  if (Tile and fDeadLands) <> 0 then
    TSprite(x, y, spMinerals + (Tile shr 25 and 3) * TerrainIconCols);

  if Options and (1 shl moEditMode) <> 0 then
    fog := (Loc < 0) or (Loc >= G.lx * G.ly)
    // else if CityLoc>=0 then
    // fog:= (Loc<0) or (Loc>=G.lx*G.ly) or (Distance(Loc,CityLoc)>5)
  else if ShowGrWall then
    fog := Tile and fGrWall = 0
  else
    fog := FoW and (Tile and fObserved = 0);
  if fog and ShowObjects then
    if Loc < -G.lx then
      Sprite(HGrTerrain, x, y + yyt, xxt * 2, yyt, 1 + 6 * (xxt * 2 + 1),
        1 + yyt * 2 + 15 * (yyt * 3 + 1))
    else if Loc >= G.lx * (G.ly + 1) then
      Sprite(HGrTerrain, x, y, xxt * 2, yyt, 1 + 6 * (xxt * 2 + 1),
        1 + yyt + 15 * (yyt * 3 + 1))
    else
      TSprite(x, y, spGrid, xxt <> 33);

  if FoW and (Tile and fObserved = 0) then
    PaintBorder;

{$IFNDEF SCR}
  // paint goto destination mark
  if DestinationMarkON and (CityOwner < 0) and (UnFocus >= 0) and
    (MyUn[UnFocus].Status and usGoto <> 0) then
  begin
    Destination := MyUn[UnFocus].Status shr 16;
    if (Destination = Loc) and (Destination <> MyUn[UnFocus].Loc) then
      if not UseBlink or BlinkOn then
        TSprite(x, y, spBlink1)
      else
        TSprite(x, y, spBlink2)
  end;
{$ENDIF}
  if Options and (1 shl moEditMode) <> 0 then
  begin
    if Tile and fPrefStartPos <> 0 then
      TSprite(x, y, spPrefStartPos)
    else if Tile and fStartPos <> 0 then
      TSprite(x, y, spStartPos);
  end
  else if ShowObjects then
  begin
    { if (CityLoc<0) and (UnFocus>=0) and (Loc=MyUn[UnFocus].Loc) then
      if BlinkOn then TSprite(x,y,8+9*0)
      else TSprite(x,y,8+9*1); }

    NameCity;
    ShowSpacePort;
    if Tile and fCity <> 0 then
      PaintCity(x + xxt, y + yyt, CityInfo, CityOwner < 0);

    if (Tile and fUnit <> 0) and (Loc <> AttLoc) and
      ((Loc <> DefLoc) or (DefHealth <> 0))
{$IFNDEF SCR} and ((CityOwner >= 0) or (UnFocus < 0) or not UseBlink or
      BlinkOn or (Loc <> MyUn[UnFocus].Loc)){$ENDIF}
      and ((Tile and fCity <> fCity) or (Loc = DefLoc)
{$IFNDEF SCR} or (not UseBlink or BlinkOn) and (UnFocus >= 0) and
      (Loc = MyUn[UnFocus].Loc){$ENDIF}) then
    begin { unit }
      GetUnitInfo(Loc, uix, UnitInfo);
      if (Loc = DefLoc) and (DefHealth >= 0) then
        UnitInfo.Health := DefHealth;
      if (UnitInfo.Owner <> CityOwner) and
        not((CityOwner = me) and (MyRO.Treaty[UnitInfo.Owner] = trAlliance))
      then
{$IFNDEF SCR} if (UnFocus >= 0) and (Loc = MyUn[UnFocus].Loc) then { active unit }
        begin
          Multi := UnitInfo.Flags and unMulti;
          MakeUnitInfo(me, MyUn[UnFocus], UnitInfo);
          UnitInfo.Flags := UnitInfo.Flags or Multi;
          PaintUnit(x + (xxt - xxu), y + (yyt - yyu_anchor), UnitInfo,
            MyUn[UnFocus].Status);
        end
        else if UnitInfo.Owner = me then
        begin
          if ClientMode = cMovieTurn then
            PaintUnit(x + (xxt - xxu), y + (yyt - yyu_anchor), UnitInfo, 0)
            // status is not set with precise timing during loading
          else
            PaintUnit(x + (xxt - xxu), y + (yyt - yyu_anchor), UnitInfo,
              MyUn[uix].Status);
          // if Showuix then Textout(x+16,y+5,$80FF00,IntToStr(uix));
        end
        else {$ENDIF} PaintUnit(x + (xxt - xxu), y + (yyt - yyu_anchor), UnitInfo, 0);
    end
    else if Tile and fHiddenUnit <> 0 then
      Sprite(HGrStdUnits, x + (xxt - xxu), y + (yyt - yyu_anchor), xxu * 2,
        yyu * 2, 1 + 5 * (xxu * 2 + 1), 1)
    else if Tile and fStealthUnit <> 0 then
      Sprite(HGrStdUnits, x + (xxt - xxu), y + (yyt - yyu_anchor), xxu * 2,
        yyu * 2, 1 + 5 * (xxu * 2 + 1), 1 + 1 * (yyu * 2 + 1))
  end;

  if ShowObjects and (Tile and fTerImp = tiFort) and (Tile and fObserved <> 0)
  then
    TSprite(x, y, spFortFront);

  if (Loc >= 0) and (Loc < G.lx * G.ly) then
    if ShowLoc then
      Textout(x + xxt - 16, y + yyt - 9, $FFFF00, IntToStr(Loc))
    else if ShowDebug and (DebugMap <> nil) and (Loc >= 0) and
      (Loc < G.lx * G.ly) and (DebugMap[Loc] <> 0) then
      Textout(x + xxt - 16, y + yyt - 9, $00E0FF,
        IntToStr(integer(DebugMap[Loc])))
end; { PaintTileObjects }

procedure TIsoMap.PaintGrid(x, y, nx, ny: integer);

  procedure ClippedLine(dx0, dy0: integer; mirror: boolean);
  var
    x0, x1, dxmin, dymin, dxmax, dymax, n: integer;
  begin
    with FOutput.Canvas do
    begin
      dxmin := (FLeft - x) div xxt;
      dymin := (RealTop - y) div yyt;
      dxmax := (FRight - x - 1) div xxt + 1;
      dymax := (RealBottom - y - 1) div yyt + 1;
      n := dymax - dy0;
      if mirror then
      begin
        if dx0 - dxmin < n then
          n := dx0 - dxmin;
        if dx0 > dxmax then
        begin
          n := n - (dx0 - dxmax);
          dy0 := dy0 + (dx0 - dxmax);
          dx0 := dxmax
        end;
        if dy0 < dymin then
        begin
          n := n - (dymin - dy0);
          dx0 := dx0 - (dymin - dy0);
          dy0 := dymin
        end;
      end
      else
      begin
        if dxmax - dx0 < n then
          n := dxmax - dx0;
        if dx0 < dxmin then
        begin
          n := n - (dxmin - dx0);
          dy0 := dy0 + (dxmin - dx0);
          dx0 := dxmin
        end;
        if dy0 < dymin then
        begin
          n := n - (dymin - dy0);
          dx0 := dx0 + (dymin - dy0);
          dy0 := dymin
        end;
      end;
      if n <= 0 then
        exit;
      if mirror then
      begin
        x0 := x + dx0 * xxt - 1;
        x1 := x + (dx0 - n) * xxt - 1;
      end
      else
      begin
        x0 := x + dx0 * xxt;
        x1 := x + (dx0 + n) * xxt;
      end;
      moveto(x0, y + dy0 * yyt);
      lineto(x1, y + (dy0 + n) * yyt);
    end
  end;

var
  i: integer;
begin
  FOutput.Canvas.pen.Color := $000000; // $FF shl (8*random(3));
  for i := 0 to nx div 2 do
    ClippedLine(i * 2, 0, false);
  for i := 1 to (nx + 1) div 2 do
    ClippedLine(i * 2, 0, true);
  for i := 0 to ny div 2 do
  begin
    ClippedLine(0, 2 * i + 2, false);
    ClippedLine(nx + 1, 2 * i + 1 + nx and 1, true);
  end;
end;

function TIsoMap.IsShoreTile(Loc: integer): boolean;
const
  Dirx: array [0 .. 7] of integer = (1, 2, 1, 0, -1, -2, -1, 0);
  Diry: array [0 .. 7] of integer = (-1, 0, 1, 2, 1, 0, -1, -2);
var
  Dir, ConnLoc: integer;
begin
  result := false;
  for Dir := 0 to 7 do
  begin
    ConnLoc := dLoc(Loc, Dirx[Dir], Diry[Dir]);
    if (ConnLoc < 0) or (ConnLoc >= G.lx * G.ly) or
      ((MyMap[ConnLoc] - 2) and fTerrain < 13) then
      result := true
  end
end;

procedure TIsoMap.MakeDark(Line: PPixelPointer; Length: Integer);
var
  I: Integer;
begin
  for I := 0 to Length - 1 do begin
    Line^.Pixel^.B := (Line^.Pixel^.B shr 1) and $7f;
    Line^.Pixel^.G := (Line^.Pixel^.G shr 1) and $7f;
    Line^.Pixel^.R := (Line^.Pixel^.R shr 1) and $7f;
    Line^.NextPixel;
  end;
end;

procedure TIsoMap.ShadeOutside(x0, y0, x1, y1, xm, ym: integer);
const
  rShade = 3.75;
var
  y, wBright: integer;
  y_n, w_n: single;
  Line: TPixelPointer;
begin
  FOutput.BeginUpdate;
  for y := y0 to y1 - 1 do begin
    Line := PixelPointer(FOutput, 0, y);
    y_n := (y - ym) / yyt;
    if abs(y_n) < rShade then begin
      // Darken left and right parts of elipsis
      w_n := sqrt(sqr(rShade) - sqr(y_n));
      wBright := trunc(w_n * xxt + 0.5);
      Line.SetX(x0);
      MakeDark(@Line, xm - x0 - wBright);
      Line.SetX(xm + wBright);
      MakeDark(@Line, x1 - xm - wBright);
    end else begin
      // Darken entire line
      Line.SetX(x0);
      MakeDark(@Line, x1 - x0);
    end;
  end;
  FOutput.EndUpdate;
end;

procedure TIsoMap.CityGrid(xm, ym: integer; CityAllowClick: Boolean);
var
  i: integer;
begin
  with FOutput.Canvas do
  begin
    if CityAllowClick then
      pen.Color := $FFFFFF
    else
      pen.Color := $000000;
    pen.Width := 1;
    for i := 0 to 3 do
    begin
      moveto(xm - xxt * (4 - i), ym + yyt * (1 + i));
      lineto(xm + xxt * (1 + i), ym - yyt * (4 - i));
      moveto(xm - xxt * (4 - i), ym - yyt * (1 + i));
      lineto(xm + xxt * (1 + i), ym + yyt * (4 - i));
    end;
    moveto(xm - xxt * 4, ym + yyt * 1);
    lineto(xm - xxt * 1, ym + yyt * 4);
    moveto(xm + xxt * 1, ym + yyt * 4);
    lineto(xm + xxt * 4, ym + yyt * 1);
    moveto(xm - xxt * 4, ym - yyt * 1);
    lineto(xm - xxt * 1, ym - yyt * 4);
    moveto(xm + xxt * 1, ym - yyt * 4);
    lineto(xm + xxt * 4, ym - yyt * 1);
    pen.Width := 1;
  end
end;

procedure TIsoMap.Paint(x, y, Loc, nx, ny, CityLoc, CityOwner: integer;
  UseBlink: boolean; CityAllowClick: boolean);
var
  dx, dy, xm, ym, ALoc, BLoc, ATer, BTer, Aix, bix: integer;
begin
  FoW := true;
  ShowLoc := Options and (1 shl moLocCodes) <> 0;
  ShowDebug := pDebugMap >= 0;
  ShowObjects := (CityOwner >= 0) or (Options and (1 shl moBareTerrain) = 0);
  ShowCityNames := ShowObjects and (CityOwner < 0) and
    (Options and (1 shl moCityNames) <> 0);
  ShowBorder := true;
  ShowMyBorder := CityOwner < 0;
  ShowGrWall := (CityOwner < 0) and (Options and (1 shl moGreatWall) <> 0);
  if ShowDebug then
    Server(sGetDebugMap, me, pDebugMap, DebugMap)
  else
    DebugMap := nil;
  with FOutput.Canvas do
  begin
    RealTop := y - ((Loc + 12345 * G.lx) div G.lx - 12345) * yyt;
    RealBottom := y + (G.ly - ((Loc + 12345 * G.lx) div G.lx - 12345) +
      3) * yyt;
    Brush.Color := EmptySpaceColor;
    if RealTop > FTop then
      FillRect(Rect(FLeft, FTop, FRight, RealTop))
    else
      RealTop := FTop;
    if RealBottom < FBottom then
      FillRect(Rect(FLeft, RealBottom, FRight, FBottom))
    else
      RealBottom := FBottom;
    Brush.Color := $000000;
    FillRect(Rect(FLeft, RealTop, FRight, RealBottom));
    Brush.Style := bsClear;
  end;

  for dy := 0 to ny + 1 do
    if (Loc + dy * G.lx >= 0) and (Loc + (dy - 3) * G.lx < G.lx * G.ly) then
      for dx := 0 to nx do
      begin
        ALoc := dLoc(Loc, dx - (dy + dx) and 1, dy - 2);
        BLoc := dLoc(Loc, dx - (dy + dx + 1) and 1, dy - 1);
        if (ALoc < 0) or (ALoc >= G.lx * G.ly) then
          ATer := PoleTile(ALoc) and fTerrain
        else
          ATer := MyMap[ALoc] and fTerrain;
        if (BLoc < 0) or (BLoc >= G.lx * G.ly) then
          BTer := PoleTile(BLoc) and fTerrain
        else
          BTer := MyMap[BLoc] and fTerrain;

        if (ATer <> fUNKNOWN) or (BTer <> fUNKNOWN) then
          if ((ATer < fGrass) or (ATer = fUNKNOWN)) and
            ((BTer < fGrass) or (BTer = fUNKNOWN)) then
          begin
            if ATer = fUNKNOWN then
              Aix := 0
            else if IsShoreTile(ALoc) then
              if ATer = fOcean then
                Aix := -1
              else
                Aix := 1
            else
              Aix := ATer + 2;
            if BTer = fUNKNOWN then
              bix := 0
            else if IsShoreTile(BLoc) then
              if BTer = fOcean then
                bix := -1
              else
                bix := 1
            else
              bix := BTer + 2;
            if (Aix > 1) or (bix > 1) then
            begin
              if Aix = -1 then
                if bix = fOcean + 2 then
                begin
                  Aix := 0;
                  bix := 0
                end
                else
                begin
                  Aix := 0;
                  bix := 1
                end
              else if bix = -1 then
                if Aix = fOcean + 2 then
                begin
                  Aix := 1;
                  bix := 1
                end
                else
                begin
                  Aix := 1;
                  bix := 0
                end;
              BitBltBitmap(OceanPatch, x + dx * xxt, y + dy * yyt, xxt, yyt,
                Aix * (xxt * 2) + (dx + dy + 1) and 1 * xxt, bix * yyt, SRCCOPY)
            end
          end
          else
          begin
            if ATer = fUNKNOWN then
              Aix := 0
            else if (ALoc >= 0) and (ALoc < G.lx * G.ly) and
              (MyMap[ALoc] and fDeadLands <> 0) then
              Aix := -2
            else if ATer = fOcean then
              Aix := -1
            else if ATer = fShore then
              Aix := 1
            else if ATer >= fForest then
              Aix := 8
            else
              Aix := ATer;
            if BTer = fUNKNOWN then
              bix := 0
            else if (BLoc >= 0) and (BLoc < G.lx * G.ly) and
              (MyMap[BLoc] and fDeadLands <> 0) then
              bix := -2
            else if BTer = fOcean then
              bix := -1
            else if BTer = fShore then
              bix := 1
            else if BTer >= fForest then
              bix := 8
            else
              bix := BTer;
            if (Aix = -2) and (bix = -2) then
            begin
              Aix := fDesert;
              bix := fDesert
            end
            else if Aix = -2 then
              if bix < 2 then
                Aix := 8
              else
                Aix := bix
            else if bix = -2 then
              if Aix < 2 then
                bix := 8
              else
                bix := Aix;
            if Aix = -1 then
              BitBltBitmap(GrExt[HGrTerrain].Data, x + dx * xxt, y + dy * yyt, xxt,
                yyt, 1 + 6 * (xxt * 2 + 1) + (dx + dy + 1) and 1 * xxt, 1 + yyt,
                SRCCOPY) // arctic <-> ocean
            else if bix = -1 then
              BitBltBitmap(GrExt[HGrTerrain].Data, x + dx * xxt, y + dy * yyt, xxt,
                yyt, 1 + 6 * (xxt * 2 + 1) + xxt - (dx + dy + 1) and 1 * xxt,
                1 + yyt * 2, SRCCOPY) // arctic <-> ocean
            else
              BitBltBitmap(LandPatch, x + dx * xxt, y + dy * yyt, xxt, yyt,
                Aix * (xxt * 2) + (dx + dy + 1) and 1 * xxt, bix * yyt, SRCCOPY)
          end
      end;

  DataCanvas := GrExt[HGrTerrain].Data.Canvas;
  MaskCanvas := GrExt[HGrTerrain].Mask.Canvas;
  for dy := -2 to ny + 1 do
    for dx := -1 to nx do
      if (dx + dy) and 1 = 0 then
        PaintShore(x + xxt * dx, y + yyt + yyt * dy, dLoc(Loc, dx, dy));
  for dy := -2 to ny + 1 do
    for dx := -1 to nx do
      if (dx + dy) and 1 = 0 then
        PaintTileExtraTerrain(x + xxt * dx, y + yyt + yyt * dy,
          dLoc(Loc, dx, dy));
  if CityOwner >= 0 then
  begin
    for dy := -2 to ny + 1 do
      for dx := -2 to nx + 1 do
        if (dx + dy) and 1 = 0 then
        begin
          ALoc := dLoc(Loc, dx, dy);
          if Distance(ALoc, CityLoc) > 5 then
            PaintTileObjects(x + xxt * dx, y + yyt + yyt * dy, ALoc, CityLoc,
              CityOwner, UseBlink);
        end;
    dx := ((CityLoc mod G.lx * 2 + CityLoc div G.lx and 1) -
      ((Loc + 666 * G.lx) mod G.lx * 2 + (Loc + 666 * G.lx) div G.lx and 1) + 3
      * G.lx) mod (2 * G.lx) - G.lx;
    dy := CityLoc div G.lx - (Loc + 666 * G.lx) div G.lx + 666;
    xm := x + (dx + 1) * xxt;
    ym := y + (dy + 1) * yyt + yyt;
    ShadeOutside(FLeft, FTop, FRight, FBottom, xm, ym);
    CityGrid(xm, ym, CityAllowClick);
    for dy := -2 to ny + 1 do
      for dx := -2 to nx + 1 do
        if (dx + dy) and 1 = 0 then
        begin
          ALoc := dLoc(Loc, dx, dy);
          if Distance(ALoc, CityLoc) <= 5 then
            PaintTileObjects(x + xxt * dx, y + yyt + yyt * dy, ALoc, CityLoc,
              CityOwner, UseBlink);
        end;
  end
  else
  begin
    if ShowLoc or (Options and (1 shl moEditMode) <> 0) or
      (Options and (1 shl moGrid) <> 0) then
      PaintGrid(x, y, nx, ny);
    for dy := -2 to ny + 1 do
      for dx := -2 to nx + 1 do
        if (dx + dy) and 1 = 0 then
          PaintTileObjects(x + xxt * dx, y + yyt + yyt * dy, dLoc(Loc, dx, dy),
            CityLoc, CityOwner, UseBlink);
  end;

  // frame(FOutput.Canvas,x+1,y+1,x+nx*33+33-2,y+ny*16+32-2,$FFFF,$FFFF);
end; { Paint }

procedure TIsoMap.AttackBegin(const ShowMove: TShowMove);
begin
  AttLoc := ShowMove.FromLoc;
  DefLoc := dLoc(AttLoc, ShowMove.dx, ShowMove.dy);
  DefHealth := -1;
end;

procedure TIsoMap.AttackEffect(const ShowMove: TShowMove);
begin
  DefHealth := ShowMove.EndHealthDef;
end;

procedure TIsoMap.AttackEnd;
begin
  AttLoc := -1;
  DefLoc := -1;
end;

initialization

NoMap := nil;
LandPatch := nil;
OceanPatch := nil;
Borders := nil;

end.
