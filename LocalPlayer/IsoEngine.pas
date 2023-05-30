{$INCLUDE Switches.inc}
unit IsoEngine;

interface

uses
  Protocol, ClientTools, ScreenTools, Tribes, LCLIntf, LCLType, SysUtils,
  Classes, Graphics, PixelPointer, GraphicSet;

const
  TerrainIconLines = 21;
  TerrainIconCols = 9;

type
  TInitEnemyModelEvent = function(emix: Integer): Boolean;
  TTileSize = (tsSmall, tsMedium, tsBig);

  TTerrainSpriteSize = array of TRect;

  { TCitiesPictures }

  TCitiesPictures = class
    Pictures: array [2..3, 0..3] of TCityPicture;
    procedure Prepare(HGrCities: TGraphicSet; xxt, yyt: Integer);
  end;

  { TIsoMap }

  TIsoMap = class
  private
    FTileSize: TTileSize;
    const
      Dirx: array [0..7] of Integer = (1, 2, 1, 0, -1, -2, -1, 0);
      Diry: array [0..7] of Integer = (-1, 0, 1, 2, 1, 0, -1, -2);
    procedure CityGrid(xm, ym: Integer; CityAllowClick: Boolean);
    function IsShoreTile(Loc: Integer): Boolean;
    procedure MakeDark(Line: PPixelPointer; Length: Integer);
    procedure SetTileSize(AValue: TTileSize);
    procedure ShadeOutside(x0, y0, Width, Height, xm, ym: Integer);
  protected
    FOutput: TBitmap;
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    RealTop: Integer;
    RealBottom: Integer;
    AttLoc: Integer;
    DefLoc: Integer;
    DefHealth: Integer;
    FAdviceLoc: Integer;
    DataCanvas: TCanvas;
    MaskCanvas: TCanvas;
    LandPatch: TBitmap;
    OceanPatch: TBitmap;
    Borders: TBitmap;
    BordersOK: PInteger;
    CitiesPictures: TCitiesPictures;
    ShowLoc: Boolean;
    ShowCityNames: Boolean;
    ShowObjects: Boolean;
    ShowBorder: Boolean;
    ShowMyBorder: Boolean;
    ShowGrWall: Boolean;
    ShowDebug: Boolean;
    FoW: Boolean;
    function Connection4(Loc, Mask, Value: Integer): Integer;
    function Connection8(Loc, Mask: Integer): Integer;
    function OceanConnection(Loc: Integer): Integer;
    procedure PaintShore(X, Y, Loc: Integer);
    procedure PaintTileExtraTerrain(X, Y, Loc: Integer);
    procedure PaintTileObjects(X, Y, Loc, CityLoc, CityOwner: Integer;
      UseBlink: Boolean);
    procedure PaintGrid(X, Y, nx, ny: Integer);
    procedure FillRect(X, Y, Width, Height, Color: Integer);
    procedure Textout(X, Y, Color: Integer; const S: string);
    procedure Sprite(HGr: TGraphicSet; xDst, yDst, Width, Height, xGr, yGr: Integer);
    procedure TSprite(xDst, yDst, grix: Integer; PureBlack: Boolean = False);
    procedure ApplyTileSize(ATileSize: TTileSize);
  public
    xxt: Integer; // half of tile size x/y
    yyt: Integer; // half of tile size x/y
    TSpriteSize: TTerrainSpriteSize;
    HGrTerrain: TGraphicSet;
    HGrCities: TGraphicSet;
    pDebugMap: Integer; // -1 for off
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure SetOutput(Output: TBitmap);
    procedure SetPaintBounds(Left, Top, Right, Bottom: Integer);
    procedure Paint(X, Y, Loc, nx, ny, CityLoc, CityOwner: Integer;
      UseBlink: Boolean = False; CityAllowClick: Boolean = False);
    procedure PaintUnit(X, Y: Integer; const UnitInfo: TUnitInfo;
      Status: Integer);
    procedure PaintCity(X, Y: Integer; const CityInfo: TCityInfo;
      accessory: Boolean = True);
    procedure BitBltBitmap(Src: TBitmap; X, Y, Width, Height, xSrc, ySrc,
      Rop: Integer);
    procedure AttackBegin(const ShowMove: TShowMove);
    procedure AttackEffect(const ShowMove: TShowMove);
    procedure AttackEnd;
    procedure ReduceTerrainIconsSize;
    property AdviceLoc: Integer read FAdviceLoc write FAdviceLoc;
    property TileSize: TTileSize read FTileSize write SetTileSize;
  end;

  { TIsoMapCache }

  TIsoMapCache = class
    LandPatch: TBitmap;
    OceanPatch: TBitmap;
    Borders: TBitmap;
    BordersOk: Integer;
    TSpriteSize: TTerrainSpriteSize;
    HGrTerrain: TGraphicSet;
    HGrCities: TGraphicSet;
    CitiesPictures: TCitiesPictures;
    procedure AssignToIsoMap(IsoMap: TIsoMap);
    constructor Create;
    destructor Destroy; override;
  end;

const
  DefaultTileSize: TTileSize = tsMedium;
  TileSizes: array [TTileSize] of TPoint = ((X: 33; Y: 16), (X: 48; Y: 24),
    (X: 72; Y: 36));

function IsJungle(Y: Integer): Boolean;
procedure Init(InitEnemyModelHandler: TInitEnemyModelEvent);

var
  MapOptions: TMapOptions;


implementation

uses
  Term;

const
  ShoreDither = fGrass;

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
  OnInitEnemyModel: TInitEnemyModelEvent;
  DebugMap: ^TTileList;
  IsoMapCache: array[TTileSize] of TIsoMapCache;

function IsJungle(Y: Integer): Boolean;
begin
  Result := (Y > (G.ly - 2) div 4) and (G.ly - 1 - Y > (G.ly - 2) div 4)
end;

procedure Init(InitEnemyModelHandler: TInitEnemyModelEvent);
begin
  OnInitEnemyModel := InitEnemyModelHandler;
end;

{ TCitiesPictures }

procedure TCitiesPictures.Prepare(HGrCities: TGraphicSet; xxt, yyt: Integer);
var
  Age: Integer;
  Size: Integer;
begin
  // prepare age 2+3 cities
  for age := 2 to 3 do
    for size := 0 to 3 do
      with Pictures[Age, Size] do
        FindPosition(HGrCities, Size * (xxt * 2 + 1), (Age - 2) * (yyt * 3 + 1),
          xxt * 2 - 1, yyt * 3 - 1, $00FFFF, xShield, yShield);
end;

{ TIsoMapCache }

procedure TIsoMapCache.AssignToIsoMap(IsoMap: TIsoMap);
begin
  IsoMap.HGrTerrain := HGrTerrain;
  IsoMap.HGrCities := HGrCities;
  IsoMap.Borders := Borders;
  IsoMap.BordersOK := @BordersOk;
  IsoMap.LandPatch := LandPatch;
  IsoMap.OceanPatch := OceanPatch;
  IsoMap.TSpriteSize := TSpriteSize;
  IsoMap.CitiesPictures := CitiesPictures;
end;

constructor TIsoMapCache.Create;
begin
  LandPatch := TBitmap.Create;
  LandPatch.PixelFormat := pf24bit;
  OceanPatch := TBitmap.Create;
  OceanPatch.PixelFormat := pf24bit;
  Borders := TBitmap.Create;
  Borders.PixelFormat := pf24bit;
  HGrTerrain := nil;
  HGrCities := nil;
  SetLength(TSpriteSize, TerrainIconLines * TerrainIconCols);
  CitiesPictures := TCitiesPictures.Create;
end;

destructor TIsoMapCache.Destroy;
begin
  FreeAndNil(CitiesPictures);
  FreeAndNil(LandPatch);
  FreeAndNil(OceanPatch);
  FreeAndNil(Borders);
  inherited;
end;

procedure TIsoMap.ReduceTerrainIconsSize;
var
  MaskLine: array of TPixelPointer;
  Mask24: TBitmap;
  xSrc: Integer;
  ySrc: Integer;
  I: Integer;
  X: Integer;
  Y: Integer;
  Border: Boolean;
begin
  SetLength(MaskLine, yyt * 3);

  // reduce size of terrain icons
  Mask24 := TBitmap.Create;
  Mask24.Assign(HGrTerrain.Mask);
  Mask24.PixelFormat := pf24bit;
  Mask24.BeginUpdate;
  for ySrc := 0 to TerrainIconLines - 1 do begin
    for I := 0 to yyt * 3 - 1 do
      MaskLine[I] := TPixelPointer.Create(Mask24, ScaleToNative(0),
        ScaleToNative(1 + ySrc * (yyt * 3 + 1) + I));
    for xSrc := 0 to TerrainIconCols - 1 do begin
      I := ySrc * 9 + xSrc;
      TSpriteSize[I].Left := 0;
      repeat
        Border := True;
        for Y := 0 to yyt * 3 - 1 do begin
          MaskLine[Y].SetX(ScaleToNative(1 + xSrc * (xxt * 2 + 1) + TSpriteSize[I].Left));
          if MaskLine[Y].Pixel^.B = 0 then Border := False;
        end;
        if Border then Inc(TSpriteSize[I].Left);
      until not Border or (TSpriteSize[I].Left = xxt * 2 - 1);
      TSpriteSize[I].Top := 0;
      repeat
        Border := True;
        for X := 0 to xxt * 2 - 1 do begin
          MaskLine[TSpriteSize[I].Top].SetX(ScaleToNative(1 + xSrc * (xxt * 2 + 1) + X));
          if MaskLine[TSpriteSize[I].Top].Pixel^.B = 0 then Border := False;
        end;
        if Border then Inc(TSpriteSize[I].Top);
      until not Border or (TSpriteSize[I].Top = yyt * 3 - 1);
      TSpriteSize[I].Right := xxt * 2;
      repeat
        Border := True;
        for Y := 0 to yyt * 3 - 1 do begin
          MaskLine[Y].SetX(ScaleToNative(xSrc * (xxt * 2 + 1) + TSpriteSize[I].Right));
          if MaskLine[Y].Pixel^.B = 0 then Border := False;
        end;
        if Border then Dec(TSpriteSize[I].Right);
      until not Border or (TSpriteSize[I].Right = TSpriteSize[I].Left);
      TSpriteSize[I].Bottom := yyt * 3;
      repeat
        Border := True;
        for X := 0 to xxt * 2 - 1 do begin
          MaskLine[TSpriteSize[I].Bottom - 1].SetX(ScaleToNative(1 + xSrc * (xxt * 2 + 1) + X));
          if MaskLine[TSpriteSize[I].Bottom - 1].Pixel^.B = 0 then Border := False;
        end;
        if Border then Dec(TSpriteSize[I].Bottom);
      until not Border or (TSpriteSize[I].Bottom = TSpriteSize[I].Top);
    end;
  end;
  Mask24.EndUpdate;
  FreeAndNil(Mask24);
end;

procedure TIsoMap.ApplyTileSize(ATileSize: TTileSize);
var
  X: Integer;
  Y: Integer;
  xSrc: Integer;
  ySrc: Integer;
  LandMore: TBitmap;
  OceanMore: TBitmap;
  DitherMask: TBitmap;
  FileName: string;
begin
  FTileSize := ATileSize;
  xxt := TileSizes[ATileSize].X;
  yyt := TileSizes[ATileSize].Y;

  if Assigned(IsoMapCache[ATileSize]) then begin
    IsoMapCache[ATileSize].AssignToIsoMap(Self);
    Exit;
  end;
  IsoMapCache[ATileSize] := TIsoMapCache.Create;

  FileName := Format('Terrain%dx%d.png', [xxt * 2, yyt * 2]);
  IsoMapCache[ATileSize].HGrTerrain := LoadGraphicSet(FileName);
  if not Assigned(IsoMapCache[ATileSize].HGrTerrain) then
    raise Exception.Create(FileName + ' not found.');

  FileName := Format('Cities%dx%d.png', [xxt * 2, yyt * 2]);
  IsoMapCache[ATileSize].HGrCities := LoadGraphicSet(FileName);
  if not Assigned(IsoMapCache[ATileSize].HGrCities) then
    raise Exception.Create(FileName + ' not found.');

  IsoMapCache[ATileSize].AssignToIsoMap(Self);

  CitiesPictures.Prepare(HGrCities, xxt, yyt);

  { prepare dithered ground tiles }
  LandPatch.Canvas.Brush.Color := 0;
  LandPatch.SetSize(xxt * 18, yyt * 9);
  LandPatch.Canvas.FillRect(0, 0, LandPatch.Width, LandPatch.Height);
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
    HGrTerrain.Mask.Canvas, 1 + 7 * (xxt * 2 + 1),
    1 + yyt + 15 * (yyt * 3 + 1), SRCAND);

  for X := -1 to 6 do begin
    if X = -1 then begin
      xSrc := ShoreDither * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
    end
    else if X = 6 then begin
      xSrc := 1 + (xxt * 2 + 1) * 2;
      ySrc := 1 + yyt + (yyt * 3 + 1) * 2;
    end else begin
      xSrc := (X + 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
    end;
    for Y := -1 to 6 do
      BitBltCanvas(LandPatch.Canvas, (X + 2) * (xxt * 2), (Y + 2) * yyt,
        xxt * 2, yyt, HGrTerrain.Data.Canvas, xSrc, ySrc);
    for Y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (X + 2) * (xxt * 2), (Y + 2) * yyt, xxt,
        yyt, HGrTerrain.Data.Canvas, xSrc + xxt, ySrc + yyt,
        SRCPAINT);
    for Y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (X + 2) * (xxt * 2) + xxt, (Y + 2) * yyt,
        xxt, yyt, HGrTerrain.Data.Canvas, xSrc, ySrc + yyt,
        SRCPAINT);
    for Y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (X + 2) * (xxt * 2), (Y + 2) * yyt, xxt,
        yyt, DitherMask.Canvas, xxt, yyt, SRCAND);
    for Y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (X + 2) * (xxt * 2) + xxt, (Y + 2) * yyt,
        xxt, yyt, DitherMask.Canvas, 0, yyt, SRCAND);
  end;

  for Y := -1 to 6 do begin
    if Y = -1 then begin
      xSrc := ShoreDither * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
    end
    else if Y = 6 then begin
      xSrc := 1 + 2 * (xxt * 2 + 1);
      ySrc := 1 + yyt + 2 * (yyt * 3 + 1);
    end else begin
      xSrc := (Y + 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
    end;
    for X := -2 to 6 do
      BitBltCanvas(LandMore.Canvas, (X + 2) * (xxt * 2), (Y + 2) * yyt,
        xxt * 2, yyt, HGrTerrain.Data.Canvas, xSrc, ySrc);
    BitBltCanvas(LandMore.Canvas, xxt * 2, (Y + 2) * yyt, xxt, yyt,
      HGrTerrain.Data.Canvas, xSrc + xxt, ySrc + yyt, SRCPAINT);
    for X := 0 to 7 do
      BitBltCanvas(LandMore.Canvas, (X + 2) * (xxt * 2) - xxt, (Y + 2) * yyt,
        xxt * 2, yyt, HGrTerrain.Data.Canvas, xSrc, ySrc + yyt,
        SRCPAINT);
    for X := -2 to 6 do
      BitBltCanvas(LandMore.Canvas, (X + 2) * (xxt * 2), (Y + 2) * yyt,
        xxt * 2, yyt, DitherMask.Canvas, 0, 0, SRCAND);
  end;

  for X := 0 to 3 do begin
    for Y := 0 to 3 do begin
      if (X = 1) and (Y = 1) then xSrc := 1
      else
        xSrc := (X mod 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
      if (X >= 1) = (Y >= 2) then
        BitBltCanvas(OceanPatch.Canvas, X * (xxt * 2), Y * yyt, xxt * 2, yyt,
          HGrTerrain.Data.Canvas, xSrc, ySrc);
      if (X >= 1) and ((Y < 2) or (X >= 2)) then
      begin
        BitBltCanvas(OceanPatch.Canvas, X * (xxt * 2), Y * yyt, xxt, yyt,
          HGrTerrain.Data.Canvas, xSrc + xxt, ySrc + yyt,
          SRCPAINT);
        BitBltCanvas(OceanPatch.Canvas, X * (xxt * 2) + xxt, Y * yyt, xxt, yyt,
          HGrTerrain.Data.Canvas, xSrc, ySrc + yyt, SRCPAINT);
      end;
      BitBltCanvas(OceanPatch.Canvas, X * (xxt * 2), Y * yyt, xxt, yyt,
        DitherMask.Canvas, xxt, yyt, SRCAND);
      BitBltCanvas(OceanPatch.Canvas, X * (xxt * 2) + xxt, Y * yyt, xxt, yyt,
        DitherMask.Canvas, 0, yyt, SRCAND);
    end;
  end;

  for Y := 0 to 3 do begin
    for X := 0 to 3 do begin
      if (X = 1) and (Y = 1) then xSrc := 1
      else
        xSrc := (Y mod 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
      if (X < 1) or (Y >= 2) then
        BitBltCanvas(OceanMore.Canvas, X * (xxt * 2), Y * yyt, xxt * 2, yyt,
          HGrTerrain.Data.Canvas, xSrc, ySrc);
      if (X = 1) and (Y < 2) or (X >= 2) and (Y >= 1) then
      begin
        BitBltCanvas(OceanMore.Canvas, X * (xxt * 2), Y * yyt, xxt, yyt,
          HGrTerrain.Data.Canvas, xSrc + xxt, ySrc + yyt,
          SRCPAINT);
        BitBltCanvas(OceanMore.Canvas, X * (xxt * 2) + xxt, Y * yyt, xxt, yyt,
          HGrTerrain.Data.Canvas, xSrc, ySrc + yyt, SRCPAINT);
      end;
      BitBltCanvas(OceanMore.Canvas, X * (xxt * 2), Y * yyt, xxt * 2, yyt,
        DitherMask.Canvas, 0, 0, SRCAND);
    end;
  end;

  BitBltCanvas(DitherMask.Canvas, 0, 0, xxt * 2, yyt * 2,
    DitherMask.Canvas, 0, 0, DSTINVERT); { invert dither mask }
  BitBltCanvas(DitherMask.Canvas, 0, 0, xxt * 2, yyt * 2,
    HGrTerrain.Mask.Canvas, 1, 1 + yyt, SRCPAINT);

  for X := -1 to 6 do
    for Y := -2 to 6 do
      BitBltCanvas(LandPatch.Canvas, (X + 2) * (xxt * 2), (Y + 2) * yyt,
        xxt * 2, yyt, DitherMask.Canvas, 0, 0, SRCAND);

  for Y := -1 to 6 do
    for X := -2 to 7 do
      BitBltCanvas(LandMore.Canvas, (X + 2) * (xxt * 2) - xxt, (Y + 2) * yyt,
        xxt * 2, yyt, DitherMask.Canvas, 0, yyt, SRCAND);

  BitBltCanvas(LandPatch.Canvas, 0, 0, (xxt * 2) * 9, yyt * 9,
    LandMore.Canvas, 0, 0, SRCPAINT);

  for X := 0 to 3 do
    for Y := 0 to 3 do
      BitBltCanvas(OceanPatch.Canvas, X * (xxt * 2), Y * yyt, xxt * 2, yyt,
        DitherMask.Canvas, 0, 0, SRCAND);

  for Y := 0 to 3 do
    for X := 0 to 4 do
      BitBltCanvas(OceanMore.Canvas, X * (xxt * 2) - xxt, Y * yyt, xxt * 2,
        yyt, DitherMask.Canvas, 0, yyt, SRCAND);

  BitBltCanvas(OceanPatch.Canvas, 0, 0, (xxt * 2) * 4, yyt * 4,
    OceanMore.Canvas, 0, 0, SRCPAINT);

  with DitherMask.Canvas do begin
    Brush.Color := $FFFFFF;
    FillRect(Rect(0, 0, xxt * 2, yyt));
  end;
  BitBltCanvas(DitherMask.Canvas, 0, 0, xxt * 2, yyt,
    HGrTerrain.Mask.Canvas, 1, 1 + yyt);

  for X := 0 to 6 do
    BitBltCanvas(LandPatch.Canvas, (X + 2) * (xxt * 2), yyt, xxt * 2, yyt,
      DitherMask.Canvas, 0, 0, SRCAND);
  BitBltCanvas(DitherMask.Canvas, 0, 0, xxt * 2, yyt, DitherMask.Canvas,
    0, 0, DSTINVERT);

  for Y := 0 to 6 do
    BitBltCanvas(LandPatch.Canvas, xxt * 2, (Y + 2) * yyt, xxt * 2, yyt,
      DitherMask.Canvas, 0, 0, SRCAND);

  FreeAndNil(LandMore);
  FreeAndNil(OceanMore);
  FreeAndNil(DitherMask);

  ReduceTerrainIconsSize;

  Borders.SetSize(xxt * 2, (yyt * 2) * nPl);
  Borders.Canvas.FillRect(0, 0, Borders.Width, Borders.Height);
  BordersOK^ := 0;
end;

procedure TIsoMap.Reset;
begin
  BordersOK^ := 0;
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
  TileSize := DefaultTileSize;
end;

destructor TIsoMap.Destroy;
begin
  inherited;
end;

procedure TIsoMap.SetOutput(Output: TBitmap);
begin
  FOutput := Output;
  FLeft := 0;
  FTop := 0;
  FRight := FOutput.Width;
  FBottom := FOutput.Height;
end;

procedure TIsoMap.SetPaintBounds(Left, Top, Right, Bottom: Integer);
begin
  FLeft := Left;
  FTop := Top;
  FRight := Right;
  FBottom := Bottom;
end;

procedure TIsoMap.FillRect(X, Y, Width, Height, Color: Integer);
begin
  if X < FLeft then
  begin
    Width := Width - (FLeft - X);
    X := FLeft;
  end;
  if Y < FTop then
  begin
    Height := Height - (FTop - Y);
    Y := FTop;
  end;
  if X + Width >= FRight then
    Width := FRight - X;
  if Y + Height >= FBottom then
    Height := FBottom - Y;
  if (Width <= 0) or (Height <= 0) then
    Exit;

  FOutput.Canvas.Brush.Color := Color;
  FOutput.Canvas.FillRect(Rect(X, Y, X + Width, Y + Height));
  FOutput.Canvas.Brush.Style := bsClear;
end;

procedure TIsoMap.Textout(X, Y, Color: Integer; const S: string);
begin
  FOutput.Canvas.Font.Color := Color;
  FOutput.Canvas.TextRect(Rect(FLeft, FTop, FRight, FBottom), X, Y, S)
end;

procedure TIsoMap.BitBltBitmap(Src: TBitmap; X, Y, Width, Height, xSrc, ySrc,
  Rop: Integer);
begin
  if X < FLeft then
  begin
    Width := Width - (FLeft - X);
    xSrc := xSrc + (FLeft - X);
    X := FLeft;
  end;
  if Y < FTop then
  begin
    Height := Height - (FTop - Y);
    ySrc := ySrc + (FTop - Y);
    Y := FTop;
  end;
  if X + Width >= FRight then
    Width := FRight - X;
  if Y + Height >= FBottom then
    Height := FBottom - Y;
  if (Width <= 0) or (Height <= 0) then
    Exit;

  BitBltCanvas(FOutput.Canvas, X, Y, Width, Height, Src.Canvas, xSrc, ySrc, Rop);
end;

procedure TIsoMap.Sprite(HGr: TGraphicSet; xDst, yDst, Width, Height, xGr, yGr: Integer);
begin
  BitBltBitmap(HGr.Mask, xDst, yDst, Width, Height, xGr, yGr, SRCAND);
  BitBltBitmap(HGr.Data, xDst, yDst, Width, Height, xGr, yGr, SRCPAINT);
end;

procedure TIsoMap.TSprite(xDst, yDst, grix: Integer;
  PureBlack: Boolean = False);
var
  Width: Integer;
  Height: Integer;
  xSrc: Integer;
  ySrc: Integer;
begin
  Width := TSpriteSize[grix].Right - TSpriteSize[grix].Left;
  Height := TSpriteSize[grix].Bottom - TSpriteSize[grix].Top;
  xSrc := 1 + grix mod 9 * (xxt * 2 + 1) + TSpriteSize[grix].Left;
  ySrc := 1 + grix div 9 * (yyt * 3 + 1) + TSpriteSize[grix].Top;
  xDst := xDst + TSpriteSize[grix].Left;
  yDst := yDst - yyt + TSpriteSize[grix].Top;
  if xDst < FLeft then begin
    Width := Width - (FLeft - xDst);
    xSrc := xSrc + (FLeft - xDst);
    xDst := FLeft;
  end;
  if yDst < FTop then begin
    Height := Height - (FTop - yDst);
    ySrc := ySrc + (FTop - yDst);
    yDst := FTop;
  end;
  if xDst + Width >= FRight then
    Width := FRight - xDst;
  if yDst + Height >= FBottom then
    Height := FBottom - yDst;
  if (Width <= 0) or (Height <= 0) then
    Exit;

  BitBltCanvas(FOutput.Canvas, xDst, yDst, Width, Height, MaskCanvas, xSrc, ySrc, SRCAND);
  if not PureBlack then
    BitBltCanvas(FOutput.Canvas, xDst, yDst, Width, Height, DataCanvas, xSrc, ySrc, SRCPAINT);
end;

procedure TIsoMap.PaintUnit(X, Y: Integer; const UnitInfo: TUnitInfo;
  Status: Integer);
var
  xsh, ysh, xGr, yGr, J, mixShow: Integer;
begin
  with UnitInfo do
    if (Owner = Me) or (emix <> $FFFF) then
    begin
      if Job = jCity then
        mixShow := -1 // building site
      else
        mixShow := mix;
      if (not Assigned(Tribe[Owner].ModelPicture[mixShow].HGr)) and
        (@OnInitEnemyModel <> nil) then
        if not OnInitEnemyModel(emix) then
          Exit;
      xsh := Tribe[Owner].ModelPicture[mixShow].xShield;
      ysh := Tribe[Owner].ModelPicture[mixShow].yShield;
{$IFNDEF SCR} if Status and usStay <> 0 then
        J := 19
      else if Status and usRecover <> 0 then
        J := 16
      else if Status and (usGoto or usEnhance) = usGoto or usEnhance then
        J := 18
      else if Status and usEnhance <> 0 then
        J := 17
      else if Status and usGoto <> 0 then
        J := 20
      else {$ENDIF} if Job = jCity then
          J := jNone
        else
          J := Job;
      if Flags and unMulti <> 0 then
        Sprite(Tribe[Owner].symHGr, X + xsh - 1 + 4, Y + ysh - 2, 14, 12,
          33 + Tribe[Owner].sympix mod 10 * 65,
          1 + Tribe[Owner].sympix div 10 * 49);
      Sprite(Tribe[Owner].symHGr, X + xsh - 1, Y + ysh - 2, 14, 12,
        18 + Tribe[Owner].sympix mod 10 * 65,
        1 + Tribe[Owner].sympix div 10 * 49);
      FillRect(X + xsh, Y + ysh + 5, 1 + Health * 11 div 100, 3,
        ColorOfHealth(Health));
      if J > 0 then
      begin
        xGr := 121 + J mod 7 * 9;
        yGr := 1 + J div 7 * 9;
        BitBltBitmap(HGrSystem.Mask, X + xsh + 3, Y + ysh + 9, 8, 8, xGr,
          yGr, SRCAND);
        Sprite(HGrSystem, X + xsh + 2, Y + ysh + 8, 8, 8, xGr, yGr);
      end;
      with Tribe[Owner].ModelPicture[mixShow] do
        Sprite(HGr, X, Y, 64, 48, pix mod 10 * 65 + 1, pix div 10 * 49 + 1);
      if Flags and unFortified <> 0 then
      begin
        { DataCanvas:=HGrTerrain.Data.Canvas;
          MaskCanvas:=HGrTerrain.Mask.Canvas;
          TSprite(X,Y+16,12*9+7); }
        Sprite(HGrStdUnits, X, Y, xxu * 2, yyu * 2, 1 + 6 * (xxu * 2 + 1), 1);
      end;
    end;
end;

procedure TIsoMap.PaintCity(X, Y: Integer; const CityInfo: TCityInfo;
  accessory: Boolean);
var
  age: Integer;
  cHGr: TGraphicSet;
  cpix: Integer;
  xGr: Integer;
  xShield: Integer;
  yShield: Integer;
  LabelTextColor: Integer;
  LabelLength: Integer;
  cpic: TCityPicture;
  S: string;
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
      (cHGr.Data.Canvas.Pixels[(xGr + 4) * 65, cpix * 49 + 48] = $00FFFF)
    then
      Sprite(cHGr, X - xxc, Y - 2 * yyc, xxc * 2, yyc * 3,
        xGr * (xxc * 2 + 1) + 1, 1 + cpix * (yyc * 3 + 1));
    if ciWalled and CityInfo.Flags <> 0 then
      Sprite(cHGr, X - xxc, Y - 2 * yyc, xxc * 2, yyc * 3,
        (xGr + 4) * (xxc * 2 + 1) + 1, 1 + cpix * (yyc * 3 + 1));
  end
  else
  begin
    if ciWalled and CityInfo.Flags <> 0 then
      Sprite(HGrCities, X - xxt, Y - 2 * yyt, 2 * xxt, 3 * yyt,
        (xGr + 4) * (2 * xxt + 1) + 1, 1 + (age - 2) * (3 * yyt + 1))
    else
      Sprite(HGrCities, X - xxt, Y - 2 * yyt, 2 * xxt, 3 * yyt,
        xGr * (2 * xxt + 1) + 1, 1 + (age - 2) * (3 * yyt + 1));
  end;

  if not accessory then
    Exit;

  { if ciCapital and CityInfo.Flags<>0 then
    Sprite(Tribe[CityInfo.Owner].symHGr,X+cpic.xf,Y-13+cpic.yf,13,14,
    1+Tribe[CityInfo.Owner].sympix mod 10 *65,
    1+Tribe[CityInfo.Owner].sympix div 10 *49); {capital -- paint flag }

  if MyMap[CityInfo.Loc] and fObserved <> 0 then
  begin
    if age < 2 then
    begin
      cpic := Tribe[CityInfo.Owner].CityPicture[xGr];
      xShield := X - xxc + cpic.xShield;
      yShield := Y - 2 * yyc + cpic.yShield;
    end
    else
    begin
      cpic := CitiesPictures.Pictures[age, xGr];
      xShield := X - xxt + cpic.xShield;
      yShield := Y - 2 * yyt + cpic.yShield;
    end;
    S := IntToStr(CityInfo.size);
    LabelLength := FOutput.Canvas.TextWidth(S);
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
    Textout(xShield + 2, yShield - 1, LabelTextColor, S);
  end;
end;

function PoleTile(Loc: Integer): Integer;
begin { virtual pole tile }
  Result := fUNKNOWN;
  if Loc < -2 * G.lx then
  else if Loc < -G.lx then
  begin
    if (MyMap[dLoc(Loc, 0, 2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, -2, 2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 2, 2)] and fTerrain <> fUNKNOWN) then
      Result := fArctic;
    if (MyMap[dLoc(Loc, 0, 2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, -2, 2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 2, 2)] and fObserved <> 0) then
      Result := Result or fObserved;
  end
  else if Loc < 0 then
  begin
    if (MyMap[dLoc(Loc, -1, 1)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 1, 1)] and fTerrain <> fUNKNOWN) then
      Result := fArctic;
    if (MyMap[dLoc(Loc, -1, 1)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 1, 1)] and fObserved <> 0) then
      Result := Result or fObserved;
  end
  else if Loc < G.lx * (G.ly + 1) then
  begin
    if (MyMap[dLoc(Loc, -1, -1)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 1, -1)] and fTerrain <> fUNKNOWN) then
      Result := fArctic;
    if (MyMap[dLoc(Loc, -1, -1)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 1, -1)] and fObserved <> 0) then
      Result := Result or fObserved;
  end
  else if Loc < G.lx * (G.ly + 2) then
  begin
    if (MyMap[dLoc(Loc, 0, -2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, -2, -2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 2, -2)] and fTerrain <> fUNKNOWN) then
      Result := fArctic;
    if (MyMap[dLoc(Loc, 0, -2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, -2, -2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 2, -2)] and fObserved <> 0) then
      Result := Result or fObserved;
  end;
end;

function TIsoMap.Connection4(Loc, Mask, Value: Integer): Integer;
begin
  Result := 0;
  if dLoc(Loc, 1, -1) >= 0 then
  begin
    if MyMap[dLoc(Loc, 1, -1)] and Mask = Cardinal(Value) then
      Inc(Result, 1);
    if MyMap[dLoc(Loc, -1, -1)] and Mask = Cardinal(Value) then
      Inc(Result, 8);
  end;
  if dLoc(Loc, 1, 1) < G.lx * G.ly then
  begin
    if MyMap[dLoc(Loc, 1, 1)] and Mask = Cardinal(Value) then
      Inc(Result, 2);
    if MyMap[dLoc(Loc, -1, 1)] and Mask = Cardinal(Value) then
      Inc(Result, 4);
  end;
end;

function TIsoMap.Connection8(Loc, Mask: Integer): Integer;
var
  Dir: Integer;
  ConnLoc: Integer;
begin
  Result := 0;
  for Dir := 0 to 7 do
  begin
    ConnLoc := dLoc(Loc, Dirx[Dir], Diry[Dir]);
    if (ConnLoc >= 0) and (ConnLoc < G.lx * G.ly) and
      (MyMap[ConnLoc] and Mask <> 0) then
      Inc(Result, 1 shl Dir);
  end;
end;

function TIsoMap.OceanConnection(Loc: Integer): Integer;
var
  Dir: Integer;
  ConnLoc: Integer;
begin
  Result := 0;
  for Dir := 0 to 7 do
  begin
    ConnLoc := dLoc(Loc, Dirx[Dir], Diry[Dir]);
    if (ConnLoc < 0) or (ConnLoc >= G.lx * G.ly) or
      ((MyMap[ConnLoc] - 2) and fTerrain < 13) then
      Inc(Result, 1 shl Dir);
  end;
end;

procedure TIsoMap.PaintShore(X, Y, Loc: Integer);
var
  Conn: Integer;
  Tile: Integer;
begin
  if (Y <= FTop - yyt * 2) or (Y > FBottom) or (X <= FLeft - xxt * 2) or
    (X > FRight) then
    Exit;
  if (Loc < 0) or (Loc >= G.lx * G.ly) then
    Exit;
  Tile := MyMap[Loc];
  if Tile and fTerrain >= fGrass then
    Exit;
  Conn := OceanConnection(Loc);
  if Conn = 0 then
    Exit;

  BitBltBitmap(HGrTerrain.Data, X + xxt div 2, Y, xxt, yyt,
    1 + (Conn shr 6 + Conn and 1 shl 2) * (xxt * 2 + 1),
    1 + yyt + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  BitBltBitmap(HGrTerrain.Data, X + xxt, Y + yyt div 2, xxt, yyt,
    1 + (Conn and 7) * (xxt * 2 + 1) + xxt,
    1 + yyt * 2 + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  BitBltBitmap(HGrTerrain.Data, X + xxt div 2, Y + yyt, xxt, yyt,
    1 + (Conn shr 2 and 7) * (xxt * 2 + 1) + xxt,
    1 + yyt + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  BitBltBitmap(HGrTerrain.Data, X, Y + yyt div 2, xxt, yyt,
    1 + (Conn shr 4 and 7) * (xxt * 2 + 1),
    1 + yyt * 2 + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  Conn := Connection4(Loc, fTerrain, fUNKNOWN); { dither to black }
  if Conn and 1 <> 0 then
    BitBltBitmap(HGrTerrain.Mask, X + xxt, Y, xxt, yyt, 1 + 7 * (xxt * 2 + 1) +
      xxt, 1 + yyt + 15 * (yyt * 3 + 1), SRCAND);
  if Conn and 2 <> 0 then
    BitBltBitmap(HGrTerrain.Mask, X + xxt, Y + yyt, xxt, yyt,
      1 + 7 * (xxt * 2 + 1) + xxt, 1 + yyt * 2 + 15 * (yyt * 3 + 1), SRCAND);
  if Conn and 4 <> 0 then
    BitBltBitmap(HGrTerrain.Mask, X, Y + yyt, xxt, yyt, 1 + 7 * (xxt * 2 + 1),
      1 + yyt * 2 + 15 * (yyt * 3 + 1), SRCAND);
  if Conn and 8 <> 0 then
    BitBltBitmap(HGrTerrain.Mask, X, Y, xxt, yyt, 1 + 7 * (xxt * 2 + 1),
      1 + yyt + 15 * (yyt * 3 + 1), SRCAND);
end;

procedure TIsoMap.PaintTileExtraTerrain(X, Y, Loc: Integer);
var
  Dir, Conn, RRConn, yGr, Tile, yLoc: Integer;
begin
  if (Loc < 0) or (Loc >= G.lx * G.ly) or (Y <= -yyt * 2) or
    (Y > FOutput.Height) or (X <= -xxt * 2) or (X > FOutput.Width) then
    Exit;
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
    TSprite(X, Y, yGr + Conn mod 8 + (Conn div 8) * TerrainIconCols);
  end
  else if Tile and fTerrain in [fHills, fMountains, fForest] then
  begin
    yGr := 3 + 2 * (Tile and fTerrain - fForest);
    Conn := Connection4(Loc, fTerrain, Tile and fTerrain);
    TSprite(X, Y, Conn mod 8 + (yGr + Conn div 8) * TerrainIconCols);
  end
  else if Tile and fDeadLands <> 0 then
    TSprite(X, Y, spRow2);

  if ShowObjects then
  begin
    if Tile and fTerImp = tiFarm then
      TSprite(X, Y, spFarmLand)
    else if Tile and fTerImp = tiIrrigation then
      TSprite(X, Y, spIrrigation);
  end;
  if Tile and fRiver <> 0 then
  begin
    Conn := Connection4(Loc, fRiver, fRiver) or
      Connection4(Loc, fTerrain, fShore) or Connection4(Loc, fTerrain,
      fUNKNOWN);
    TSprite(X, Y, spRiver + Conn mod 8 + (Conn div 8) * TerrainIconCols);
  end;

  if Tile and fTerrain < fGrass then
  begin
    Conn := Connection4(Loc, fRiver, fRiver);
    for Dir := 0 to 3 do
      if Conn and (1 shl Dir) <> 0 then { river mouths }
        TSprite(X, Y, spRiverMouths + Dir);
    if ShowObjects then
    begin
      Conn := Connection8(Loc, fCanal);
      for Dir := 0 to 7 do
        if Conn and (1 shl Dir) <> 0 then { canal mouths }
          TSprite(X, Y, spCanalMouths + 1 + Dir);
    end;
  end;

  if ShowObjects then begin
    // Paint canal connections
    if (Tile and fCanal <> 0) or (Tile and fCity <> 0) then begin
      Conn := Connection8(Loc, fCanal or fCity);
      if Tile and fCanal <> 0 then
        Conn := Conn or ($FF - OceanConnection(Loc));
      if Conn = 0 then begin
        if Tile and fCanal <> 0 then
          TSprite(X, Y, spCanal);
      end
      else
        for Dir := 0 to 7 do
          if (1 shl Dir) and Conn <> 0 then
            TSprite(X, Y, spCanal + 1 + Dir);
    end;

    if Tile and (fRR or fCity) <> 0 then
      RRConn := Connection8(Loc, fRR or fCity)
    else
      RRConn := 0;

    // Paint road connections
    if Tile and (fRoad or fRR or fCity) <> 0 then begin
      Conn := Connection8(Loc, fRoad or fRR or fCity) and not RRConn;
      if (Conn = 0) and (Tile and (fRR or fCity) = 0) then
        TSprite(X, Y, spRoad)
      else if Conn > 0 then
        for Dir := 0 to 7 do
          if (1 shl Dir) and Conn <> 0 then
            TSprite(X, Y, spRoad + 1 + Dir);
    end;

    // Paint railroad connections
    if (Tile and fRR <> 0) and (RRConn = 0) then
      TSprite(X, Y, spRailRoad)
    else if RRConn > 0 then begin
      for Dir := 0 to 7 do
        if (1 shl Dir) and RRConn <> 0 then
          TSprite(X, Y, spRailRoad + 1 + Dir);
    end;
  end;
end;

// (x,y) is top left pixel of (2*xxt,3*yyt) rectangle
procedure TIsoMap.PaintTileObjects(X, Y, Loc, CityLoc, CityOwner: Integer;
  UseBlink: Boolean);
var
  p1, p2, uix, cix, dy, Loc1, Tile, Multi, Destination: Integer;
  CityInfo: TCityInfo;
  UnitInfo: TUnitInfo;
  fog: Boolean;
  SpecialRow: Integer;
  SpecialCol: Integer;

  procedure NameCity;
  var
    cix, xs, W: Integer;
    BehindCityInfo: TCityInfo;
    S: string;
    IsCapital: Boolean;
  begin
    BehindCityInfo.Loc := Loc - 2 * G.lx;
    if ShowCityNames and not (moEditMode in MapOptions) and
      (BehindCityInfo.Loc >= 0) and (BehindCityInfo.Loc < G.lx * G.ly) and
      (MyMap[BehindCityInfo.Loc] and fCity <> 0) then
    begin
      GetCityInfo(BehindCityInfo.Loc, cix, BehindCityInfo);
      IsCapital := BehindCityInfo.Flags and ciCapital <> 0;
      { if Showuix and (cix>=0) then s:=IntToStr(cix)
        else } S := CityName(BehindCityInfo.ID);
      W := FOutput.Canvas.TextWidth(S);
      xs := X + xxt - (W + 1) div 2;
      if IsCapital then
        FOutput.Canvas.Font.Style := FOutput.Canvas.Font.Style + [fsUnderline];
      Textout(xs + 1, Y - 9, $000000, S);
      Textout(xs, Y - 10, $FFFFFF, S);
      if IsCapital then
        FOutput.Canvas.Font.Style := FOutput.Canvas.Font.Style - [fsUnderline];
    end;
  end;

  procedure ShowSpacePort;
  begin
    if ShowObjects and not (moEditMode in MapOptions) and
      (Tile and fCity <> 0) and (CityInfo.Flags and ciSpacePort <> 0) then
      TSprite(X + xxt, Y - 6, spSpacePort);
  end;

  procedure PaintBorder;
  var
    dx, dy: Integer;
  begin
    if ShowBorder and (Loc >= 0) and (Loc < G.lx * G.ly) and
      (Tile and fTerrain <> fUNKNOWN) then begin
      p1 := MyRO.Territory[Loc];
      if (p1 >= 0) and (ShowMyBorder or (p1 <> Me)) then begin
        if BordersOK^ and (1 shl p1) = 0 then begin
          UnshareBitmap(Borders);
          BitBltCanvas(Borders.Canvas, 0, p1 * (yyt * 2), xxt * 2,
            yyt * 2, HGrTerrain.Data.Canvas,
            1 + 8 * (xxt * 2 + 1), 1 + yyt + 16 * (yyt * 3 + 1));
          BitmapReplaceColor(Borders, 0, p1 * (yyt * 2), xxt * 2, yyt * 2, $636363, Tribe[p1].Color);
          BordersOK^ := BordersOK^ or 1 shl p1;
        end;
        for dy := 0 to 1 do
          for dx := 0 to 1 do begin
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
                BitBltBitmap(HGrTerrain.Mask, X + dx * xxt, Y + dy * yyt, xxt,
                  yyt, 1 + 8 * (xxt * 2 + 1) + dx * xxt,
                  1 + yyt + 16 * (yyt * 3 + 1) + dy * yyt, SRCAND);
                BitBltBitmap(Borders, X + dx * xxt, Y + dy * yyt, xxt, yyt, dx * xxt,
                  p1 * (yyt * 2) + dy * yyt, SRCPAINT);
              end;
            end;
          end;
      end;
    end;
  end;

begin
  if (Loc < 0) or (Loc >= G.lx * G.ly) then
    Tile := PoleTile(Loc)
  else
    Tile := MyMap[Loc];
  if ShowObjects and not (moEditMode in MapOptions) and
    (Tile and fCity <> 0) then
    GetCityInfo(Loc, cix, CityInfo);
  if (Y <= FTop - yyt * 2) or (Y > FBottom) or (X <= FLeft - xxt * 2) or
    (X > FRight) then
  begin
    NameCity;
    ShowSpacePort;
    Exit;
  end;
  if Tile and fTerrain = fUNKNOWN then
  begin
    NameCity;
    ShowSpacePort;
    Exit;
  end; { square not discovered }

  if not(FoW and (Tile and fObserved = 0)) then
    PaintBorder;

  if (Loc >= 0) and (Loc < G.lx * G.ly) and (Loc = FAdviceLoc) then
    TSprite(X, Y, spPlain);

  if (Loc >= 0) and (Loc < G.lx * G.ly) and (Tile and fSpecial <> 0)
  then { special resources }
  begin
    dy := Loc div G.lx;
    SpecialCol := Tile and fTerrain;
    SpecialRow := Tile and fSpecial shr 5;
    if SpecialCol < fForest then
      TSprite(X, Y, SpecialCol + SpecialRow * TerrainIconCols)
    else if (SpecialCol = fForest) and IsJungle(dy) then
      TSprite(X, Y, spJungle - 1 + SpecialRow * TerrainIconCols)
    else
      TSprite(X, Y, spForest - 1 + ((SpecialCol - fForest) * 2 + SpecialRow) * TerrainIconCols);
  end;

  if ShowObjects then
  begin
    if Tile and fTerImp = tiMine then
      TSprite(X, Y, spMine);
    if Tile and fTerImp = tiBase then
      TSprite(X, Y, spBase);
    if Tile and fPoll <> 0 then
      TSprite(X, Y, spPollution);
    if Tile and fTerImp = tiFort then
    begin
      TSprite(X, Y, spFortBack);
      if Tile and fObserved = 0 then
        TSprite(X, Y, spFortFront);
    end;
  end;
  if (Tile and fDeadLands) <> 0 then
    TSprite(X, Y, spMinerals + (Tile shr 25 and 3) * TerrainIconCols);

  if moEditMode in MapOptions then
    fog := (Loc < 0) or (Loc >= G.lx * G.ly)
    // else if CityLoc>=0 then
    // fog:= (Loc<0) or (Loc>=G.lx*G.ly) or (Distance(Loc,CityLoc)>5)
  else if ShowGrWall then
    fog := Tile and fGrWall = 0
  else
    fog := FoW and (Tile and fObserved = 0);
  if fog and ShowObjects then
    if Loc < -G.lx then
      Sprite(HGrTerrain, X, Y + yyt, xxt * 2, yyt, 1 + 6 * (xxt * 2 + 1),
        1 + yyt * 2 + 15 * (yyt * 3 + 1))
    else if Loc >= G.lx * (G.ly + 1) then
      Sprite(HGrTerrain, X, Y, xxt * 2, yyt, 1 + 6 * (xxt * 2 + 1),
        1 + yyt + 15 * (yyt * 3 + 1))
    else
      TSprite(X, Y, spGrid, xxt <> 33);

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
        TSprite(X, Y, spBlink1)
      else
        TSprite(X, Y, spBlink2)
  end;
{$ENDIF}
  if moEditMode in MapOptions then
  begin
    if Tile and fPrefStartPos <> 0 then
      TSprite(X, Y, spPrefStartPos)
    else if Tile and fStartPos <> 0 then
      TSprite(X, Y, spStartPos);
  end
  else if ShowObjects then
  begin
    { if (CityLoc<0) and (UnFocus>=0) and (Loc=MyUn[UnFocus].Loc) then
      if BlinkOn then TSprite(X,Y,8+9*0)
      else TSprite(X,Y,8+9*1); }

    NameCity;
    ShowSpacePort;
    if Tile and fCity <> 0 then
      PaintCity(X + xxt, Y + yyt, CityInfo, CityOwner < 0);

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
        not((CityOwner = Me) and (MyRO.Treaty[UnitInfo.Owner] = trAlliance))
      then
{$IFNDEF SCR} if (UnFocus >= 0) and (Loc = MyUn[UnFocus].Loc) then { active unit }
        begin
          Multi := UnitInfo.Flags and unMulti;
          MakeUnitInfo(Me, MyUn[UnFocus], UnitInfo);
          UnitInfo.Flags := UnitInfo.Flags or Multi;
          PaintUnit(X + (xxt - xxu), Y + (yyt - yyu_anchor), UnitInfo,
            MyUn[UnFocus].Status);
        end
        else if UnitInfo.Owner = Me then
        begin
          if ClientMode = cMovieTurn then
            PaintUnit(X + (xxt - xxu), Y + (yyt - yyu_anchor), UnitInfo, 0)
            // status is not set with precise timing during loading
          else
            PaintUnit(X + (xxt - xxu), Y + (yyt - yyu_anchor), UnitInfo,
              MyUn[uix].Status);
          // if Showuix then Textout(x+16,y+5,$80FF00,IntToStr(uix));
        end
        else {$ENDIF} PaintUnit(X + (xxt - xxu), Y + (yyt - yyu_anchor), UnitInfo, 0);
    end
    else if Tile and fHiddenUnit <> 0 then
      Sprite(HGrStdUnits, X + (xxt - xxu), Y + (yyt - yyu_anchor), xxu * 2,
        yyu * 2, 1 + 5 * (xxu * 2 + 1), 1)
    else if Tile and fStealthUnit <> 0 then
      Sprite(HGrStdUnits, X + (xxt - xxu), Y + (yyt - yyu_anchor), xxu * 2,
        yyu * 2, 1 + 5 * (xxu * 2 + 1), 1 + 1 * (yyu * 2 + 1))
  end;

  if ShowObjects and (Tile and fTerImp = tiFort) and (Tile and fObserved <> 0)
  then
    TSprite(X, Y, spFortFront);

  if (Loc >= 0) and (Loc < G.lx * G.ly) then
    if ShowLoc then
      Textout(X + xxt - 16, Y + yyt - 9, $FFFF00, IntToStr(Loc))
    else if ShowDebug and (DebugMap <> nil) and (Loc >= 0) and
      (Loc < G.lx * G.ly) and (DebugMap[Loc] <> 0) then
      Textout(X + xxt - 16, Y + yyt - 9, $00E0FF,
        IntToStr(Integer(DebugMap[Loc])))
end;

procedure TIsoMap.PaintGrid(X, Y, nx, ny: Integer);

  procedure ClippedLine(dx0, dy0: Integer; mirror: Boolean);
  var
    x0, x1, dxmin, dymin, dxmax, dymax, N: Integer;
  begin
    with FOutput.Canvas do
    begin
      dxmin := (FLeft - X) div xxt;
      dymin := (RealTop - Y) div yyt;
      dxmax := (FRight - X - 1) div xxt + 1;
      dymax := (RealBottom - Y - 1) div yyt + 1;
      N := dymax - dy0;
      if mirror then
      begin
        if dx0 - dxmin < N then
          N := dx0 - dxmin;
        if dx0 > dxmax then
        begin
          N := N - (dx0 - dxmax);
          dy0 := dy0 + (dx0 - dxmax);
          dx0 := dxmax
        end;
        if dy0 < dymin then
        begin
          N := N - (dymin - dy0);
          dx0 := dx0 - (dymin - dy0);
          dy0 := dymin
        end;
      end
      else
      begin
        if dxmax - dx0 < N then
          N := dxmax - dx0;
        if dx0 < dxmin then
        begin
          N := N - (dxmin - dx0);
          dy0 := dy0 + (dxmin - dx0);
          dx0 := dxmin
        end;
        if dy0 < dymin then
        begin
          N := N - (dymin - dy0);
          dx0 := dx0 + (dymin - dy0);
          dy0 := dymin
        end;
      end;
      if N <= 0 then
        Exit;
      if mirror then
      begin
        x0 := X + dx0 * xxt - 1;
        x1 := X + (dx0 - N) * xxt - 1;
      end
      else
      begin
        x0 := X + dx0 * xxt;
        x1 := X + (dx0 + N) * xxt;
      end;
      moveto(x0, Y + dy0 * yyt);
      lineto(x1, Y + (dy0 + N) * yyt);
    end;
  end;

var
  I: Integer;
begin
  FOutput.Canvas.pen.Color := $000000; // $FF shl (8*random(3));
  for I := 0 to nx div 2 do
    ClippedLine(I * 2, 0, False);
  for I := 1 to (nx + 1) div 2 do
    ClippedLine(I * 2, 0, True);
  for I := 0 to ny div 2 do
  begin
    ClippedLine(0, 2 * I + 2, False);
    ClippedLine(nx + 1, 2 * I + 1 + nx and 1, True);
  end;
end;

function TIsoMap.IsShoreTile(Loc: Integer): Boolean;
var
  Dir: Integer;
  ConnLoc: Integer;
begin
  Result := False;
  for Dir := 0 to 7 do
  begin
    ConnLoc := dLoc(Loc, Dirx[Dir], Diry[Dir]);
    if (ConnLoc < 0) or (ConnLoc >= G.lx * G.ly) or
      ((MyMap[ConnLoc] - 2) and fTerrain < 13) then
      Result := True;
  end;
end;

procedure TIsoMap.MakeDark(Line: PPixelPointer; Length: Integer);
var
  I: Integer;
begin
  for I := 0 to Length - 1 do begin
    Line^.Pixel^.B := (Line^.Pixel^.B shr 1) and $7F;
    Line^.Pixel^.G := (Line^.Pixel^.G shr 1) and $7F;
    Line^.Pixel^.R := (Line^.Pixel^.R shr 1) and $7F;
    Line^.NextPixel;
  end;
end;

procedure TIsoMap.SetTileSize(AValue: TTileSize);
begin
  if FTileSize = AValue then Exit;
  FTileSize := AValue;
  ApplyTileSize(AValue);
end;

procedure TIsoMap.ShadeOutside(x0, y0, Width, Height, xm, ym: Integer);
const
  rShade = 3.75;
var
  Y, wBright: Integer;
  y_n, w_n: Single;
  Line: TPixelPointer;
begin
  FOutput.BeginUpdate;
  Line := TPixelPointer.Create(FOutput, ScaleToNative(x0), ScaleToNative(y0));
  for Y := 0 to ScaleToNative(Height) - 1 do begin
    y_n := (ScaleFromNative(Y) + y0 - ym) / yyt;
    if abs(y_n) < rShade then begin
      // Darken left and right parts of elipsis
      w_n := sqrt(sqr(rShade) - sqr(y_n));
      wBright := trunc(w_n * xxt + 0.5);
      Line.SetX(0);
      MakeDark(@Line, ScaleToNative(xm - wBright));
      Line.SetX(ScaleToNative(xm + wBright));
      MakeDark(@Line, ScaleToNative(Width - xm - wBright));
    end else begin
      // Darken entire line
      Line.SetX(0);
      MakeDark(@Line, ScaleToNative(Width));
    end;
    Line.NextLine;
  end;
  FOutput.EndUpdate;
end;

procedure TIsoMap.CityGrid(xm, ym: Integer; CityAllowClick: Boolean);
var
  I: Integer;
begin
  with FOutput.Canvas do
  begin
    if CityAllowClick then
      pen.Color := $FFFFFF
    else
      pen.Color := $000000;
    pen.Width := 1;
    for I := 0 to 3 do
    begin
      moveto(xm - xxt * (4 - I), ym + yyt * (1 + I));
      lineto(xm + xxt * (1 + I), ym - yyt * (4 - I));
      moveto(xm - xxt * (4 - I), ym - yyt * (1 + I));
      lineto(xm + xxt * (1 + I), ym + yyt * (4 - I));
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
  end;
end;

procedure TIsoMap.Paint(X, Y, Loc, nx, ny, CityLoc, CityOwner: Integer;
  UseBlink: Boolean; CityAllowClick: Boolean);
var
  dx, dy, xm, ym, ALoc, BLoc, ATer, BTer, Aix, bix: Integer;
begin
  FoW := True;
  ShowLoc := moLocCodes in MapOptions;
  ShowDebug := pDebugMap >= 0;
  ShowObjects := (CityOwner >= 0) or not (moBareTerrain in MapOptions);
  ShowCityNames := ShowObjects and (CityOwner < 0) and
    (moCityNames in MapOptions);
  ShowBorder := True;
  ShowMyBorder := CityOwner < 0;
  ShowGrWall := (CityOwner < 0) and (moGreatWall in MapOptions);
  if ShowDebug then
    Server(sGetDebugMap, Me, pDebugMap, DebugMap)
  else
    DebugMap := nil;
  with FOutput.Canvas do
  begin
    RealTop := Y - ((Loc + 12345 * G.lx) div G.lx - 12345) * yyt;
    RealBottom := Y + (G.ly - ((Loc + 12345 * G.lx) div G.lx - 12345) +
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
                  bix := 0;
                end
                else
                begin
                  Aix := 0;
                  bix := 1;
                end
              else if bix = -1 then
                if Aix = fOcean + 2 then
                begin
                  Aix := 1;
                  bix := 1;
                end
                else
                begin
                  Aix := 1;
                  bix := 0;
                end;
              BitBltBitmap(OceanPatch, X + dx * xxt, Y + dy * yyt, xxt, yyt,
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
              bix := fDesert;
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
              BitBltBitmap(HGrTerrain.Data, X + dx * xxt, Y + dy * yyt, xxt,
                yyt, 1 + 6 * (xxt * 2 + 1) + (dx + dy + 1) and 1 * xxt, 1 + yyt,
                SRCCOPY) // arctic <-> ocean
            else if bix = -1 then
              BitBltBitmap(HGrTerrain.Data, X + dx * xxt, Y + dy * yyt, xxt,
                yyt, 1 + 6 * (xxt * 2 + 1) + xxt - (dx + dy + 1) and 1 * xxt,
                1 + yyt * 2, SRCCOPY) // arctic <-> ocean
            else
              BitBltBitmap(LandPatch, X + dx * xxt, Y + dy * yyt, xxt, yyt,
                Aix * (xxt * 2) + (dx + dy + 1) and 1 * xxt, bix * yyt, SRCCOPY)
          end;
      end;

  DataCanvas := HGrTerrain.Data.Canvas;
  MaskCanvas := HGrTerrain.Mask.Canvas;
  for dy := -2 to ny + 1 do
    for dx := -1 to nx do
      if (dx + dy) and 1 = 0 then
        PaintShore(X + xxt * dx, Y + yyt + yyt * dy, dLoc(Loc, dx, dy));
  for dy := -2 to ny + 1 do
    for dx := -1 to nx do
      if (dx + dy) and 1 = 0 then
        PaintTileExtraTerrain(X + xxt * dx, Y + yyt + yyt * dy,
          dLoc(Loc, dx, dy));
  if CityOwner >= 0 then
  begin
    for dy := -2 to ny + 1 do
      for dx := -2 to nx + 1 do
        if (dx + dy) and 1 = 0 then
        begin
          ALoc := dLoc(Loc, dx, dy);
          if Distance(ALoc, CityLoc) > 5 then
            PaintTileObjects(X + xxt * dx, Y + yyt + yyt * dy, ALoc, CityLoc,
              CityOwner, UseBlink);
        end;
    dx := ((CityLoc mod G.lx * 2 + CityLoc div G.lx and 1) -
      ((Loc + 666 * G.lx) mod G.lx * 2 + (Loc + 666 * G.lx) div G.lx and 1) + 3
      * G.lx) mod (2 * G.lx) - G.lx;
    dy := CityLoc div G.lx - (Loc + 666 * G.lx) div G.lx + 666;
    xm := X + (dx + 1) * xxt;
    ym := Y + (dy + 1) * yyt + yyt;
    ShadeOutside(FLeft, FTop, FRight - FLeft, FBottom - FTop, xm, ym);
    CityGrid(xm, ym, CityAllowClick);
    for dy := -2 to ny + 1 do
      for dx := -2 to nx + 1 do
        if (dx + dy) and 1 = 0 then
        begin
          ALoc := dLoc(Loc, dx, dy);
          if Distance(ALoc, CityLoc) <= 5 then
            PaintTileObjects(X + xxt * dx, Y + yyt + yyt * dy, ALoc, CityLoc,
              CityOwner, UseBlink);
        end;
  end
  else
  begin
    if ShowLoc or (moEditMode in MapOptions) or
      (moGrid in MapOptions) then
      PaintGrid(X, Y, nx, ny);
    for dy := -2 to ny + 1 do
      for dx := -2 to nx + 1 do
        if (dx + dy) and 1 = 0 then
          PaintTileObjects(X + xxt * dx, Y + yyt + yyt * dy, dLoc(Loc, dx, dy),
            CityLoc, CityOwner, UseBlink);
  end;

  // frame(FOutput.Canvas,x+1,y+1,x+nx*33+33-2,y+ny*16+32-2,$FFFF,$FFFF);
end;

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

procedure IsoEngineDone;
var
  I: TTileSize;
begin
  for I := Low(IsoMapCache) to High(IsoMapCache) do
    FreeAndNil(IsoMapCache[I]);
end;

finalization

IsoEngineDone;

end.
