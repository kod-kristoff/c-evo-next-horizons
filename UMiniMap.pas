{$INCLUDE Switches.inc}
unit UMiniMap;

interface

uses
  Classes, SysUtils, Graphics, Protocol, ClientTools;

type
  TMiniMode = (mmNone, mmPicture, mmMultiPlayer);
  TMapArray = array[0 .. lxmax * lymax - 1] of Byte;

  { TMiniMap }

  TMiniMap = class
  const
    MaxWidthMapLogo = 96;
    MaxHeightMapLogo = 96;
  var
    Bitmap: TBitmap; { game world sample preview }
    Size: TPoint;
    Colors: array [0 .. 11, 0 .. 1] of TColor;
    Mode: TMiniMode;
    MapOptions: TMapOptions;
    procedure LoadFromLogFile(FileName: string; var LastTurn: Integer; DefaultSize: TPoint);
    procedure LoadFromMapFile(FileName: string; var nMapLandTiles, nMapStartPositions: Integer);
    procedure PaintRandom(Brightness, StartLandMass: Integer; WorldSize: TPoint);
    procedure PaintFile(SaveMap: TMapArray);
    procedure Paint(MyMap: PTileList; MapWidth: Integer; ClientMode: Integer;
      xxt, xwMini: Integer);
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  ScreenTools, UPixelPointer, Global, GameServer, IsoEngine, Tribes;

const
  // save map tile flags
  smOwned = $20;
  smUnit = $40;
  smCity = $80;

{ TMiniMap }

constructor TMiniMap.Create;
var
  X, Y: Integer;
begin
  Bitmap := TBitmap.Create;

  for X := 0 to 11 do
    for Y := 0 to 1 do
      Colors[x, y] := HGrSystem.Data.Canvas.Pixels[66 + x, 67 + y];
end;

destructor TMiniMap.Destroy;
begin
  FreeAndNil(Bitmap);
  inherited;
end;

procedure TMiniMap.LoadFromLogFile(FileName: string; var LastTurn: Integer; DefaultSize: TPoint);
var
  SaveMap: TMapArray;
  y: Integer;
  Dummy: Integer;
  FileLandMass: integer;
  LogFile: file;
  s: string[255];
  MapRow: array [0 .. lxmax - 1] of Cardinal;
begin
  AssignFile(LogFile, FileName);
  try
    Reset(LogFile, 4);
    BlockRead(LogFile, s[1], 2); { file id }
    BlockRead(LogFile, Dummy, 1); { format id }
    if Dummy >= $000E01 then
      BlockRead(LogFile, Dummy, 1); { item stored since 0.14.1 }
    BlockRead(LogFile, Size.X, 1);
    BlockRead(LogFile, Size.Y, 1);
    BlockRead(LogFile, FileLandMass, 1);
    if FileLandMass = 0 then
      for y := 0 to Size.Y - 1 do
        BlockRead(LogFile, MapRow, Size.X);
    BlockRead(LogFile, Dummy, 1);
    BlockRead(LogFile, Dummy, 1);
    BlockRead(LogFile, LastTurn, 1);
    BlockRead(LogFile, SaveMap, 1);
    if SaveMap[0] = $80 then
      Mode := mmMultiPlayer
    else
      Mode := mmPicture;
    if Mode = mmPicture then
      BlockRead(LogFile, SaveMap[4], (Size.X * Size.Y - 1) div 4);
    CloseFile(LogFile);
  except
    CloseFile(LogFile);
    LastTurn := 0;
    Size := DefaultSize;
    Mode := mmNone;
  end;
  PaintFile(SaveMap);
end;

procedure TMiniMap.LoadFromMapFile(FileName: string; var nMapLandTiles, nMapStartPositions: Integer);
var
  x, y, lxFile, lyFile: integer;
  MapFile: file;
  s: string[255];
  MapRow: array [0 .. lxmax - 1] of Cardinal;
  ImageFileName: string;
begin
  ImageFileName := Copy(FileName, 1, Length(FileName) - Length(CevoMapExt)) + CevoMapPictureExt;
  Mode := mmPicture;
  if LoadGraphicFile(Bitmap, ImageFileName, [gfNoError]) then
  begin
    if Bitmap.width div 2 > MaxWidthMapLogo then
      Bitmap.width := MaxWidthMapLogo * 2;
    if Bitmap.height > MaxHeightMapLogo then
      Bitmap.height := MaxHeightMapLogo;
    Size.X := Bitmap.width div 2;
    Size.Y := Bitmap.height;
  end else begin
    Mode := mmNone;
    Size.X := MaxWidthMapLogo;
    Size.Y := MaxHeightMapLogo;
  end;

  AssignFile(MapFile, FileName);
  try
    Reset(MapFile, 4);
    BlockRead(MapFile, s[1], 2); { file id }
    BlockRead(MapFile, x, 1); { format id }
    BlockRead(MapFile, x, 1); // MaxTurn
    BlockRead(MapFile, lxFile, 1);
    BlockRead(MapFile, lyFile, 1);
    nMapLandTiles := 0;
    nMapStartPositions := 0;
    for y := 0 to lyFile - 1 do begin
      BlockRead(MapFile, MapRow, lxFile);
      for x := 0 to lxFile - 1 do
      begin
        if (MapRow[x] and fTerrain) in [fGrass, fPrairie, fTundra, fSwamp,
          fForest, fHills] then
          inc(nMapLandTiles);
        if MapRow[x] and (fPrefStartPos or fStartPos) <> 0 then
          inc(nMapStartPositions);
      end
    end;
    if nMapStartPositions > nPl then
      nMapStartPositions := nPl;
    CloseFile(MapFile);
  except
    CloseFile(MapFile);
  end;
end;

procedure TMiniMap.PaintRandom(Brightness, StartLandMass: Integer; WorldSize: TPoint);
var
  i, x, y, xm, cm: Integer;
  MiniPixel: TPixelPointer;
  Map: ^TTileList;
begin
  Map := PreviewMap(StartLandMass);
  Size := WorldSize;

  Bitmap.PixelFormat := pf24bit;
  Bitmap.SetSize(Size.X * 2, Size.Y);
  Bitmap.BeginUpdate;
  MiniPixel := PixelPointer(Bitmap);
  for y := 0 to ScaleToNative(Size.Y) - 1 do begin
    for x := 0 to ScaleToNative(Size.X) - 1 do begin
      for i := 0 to 1 do begin
        xm := (x * 2 + i + y and 1) mod (ScaleToNative(Size.X) * 2);
        MiniPixel.SetX(xm);
        cm := Colors[Map[ScaleFromNative(x) * lxmax div Size.X + lxmax *
          ((ScaleFromNative(y) * (lymax - 1) + Size.Y div 2) div (Size.Y - 1))] and
          fTerrain, i];
        MiniPixel.Pixel^.B := ((cm shr 16) and $FF) * Brightness div 3;
        MiniPixel.Pixel^.G := ((cm shr 8) and $FF) * Brightness div 3;
        MiniPixel.Pixel^.R := ((cm shr 0) and $FF) * Brightness div 3;
      end;
    end;
    MiniPixel.NextLine;
  end;
  Bitmap.EndUpdate;
end;

procedure TMiniMap.PaintFile(SaveMap: TMapArray);
var
  i, x, y, xm, cm, Tile, OwnColor, EnemyColor: integer;
  MiniPixel: TPixelPointer;
  PrevMiniPixel: TPixelPointer;
begin
  OwnColor := HGrSystem.Data.Canvas.Pixels[95, 67];
  EnemyColor := HGrSystem.Data.Canvas.Pixels[96, 67];
  Bitmap.PixelFormat := pf24bit;
  Bitmap.SetSize(Size.X * 2, Size.Y);
  if Mode = mmPicture then begin
    Bitmap.BeginUpdate;
    MiniPixel := PixelPointer(Bitmap);
    PrevMiniPixel := PixelPointer(Bitmap, 0, -1);
    for y := 0 to ScaleToNative(Size.Y) - 1 do begin
      for x := 0 to ScaleToNative(Size.X) - 1 do begin
        for i := 0 to 1 do begin
          xm := (x * 2 + i + y and 1) mod (ScaleToNative(Size.X) * 2);
          MiniPixel.SetX(xm);
          Tile := SaveMap[ScaleFromNative(x) + Size.X * ScaleFromNative(y)];
          if Tile and fTerrain = fUNKNOWN then
            cm := $000000
          else if Tile and smCity <> 0 then begin
            if Tile and smOwned <> 0 then cm := OwnColor
              else cm := EnemyColor;
            if y > 0 then begin
              // 2x2 city dot covers two lines
              PrevMiniPixel.SetX(xm);
              PrevMiniPixel.Pixel^.B := cm shr 16;
              PrevMiniPixel.Pixel^.G:= cm shr 8 and $FF;
              PrevMiniPixel.Pixel^.R := cm and $FF;
            end;
          end
          else if (i = 0) and (Tile and smUnit <> 0) then begin
            if Tile and smOwned <> 0 then cm := OwnColor
              else cm := EnemyColor;
          end else
            cm := Colors[Tile and fTerrain, i];
          MiniPixel.Pixel^.B := (cm shr 16) and $ff;
          MiniPixel.Pixel^.G := (cm shr 8) and $ff;
          MiniPixel.Pixel^.R := (cm shr 0) and $ff;
        end;
      end;
      MiniPixel.NextLine;
      PrevMiniPixel.NextLine;
    end;
    Bitmap.EndUpdate;
  end;
end;

procedure TMiniMap.Paint(MyMap: PTileList; MapWidth: Integer; ClientMode: Integer;
  xxt, xwMini: Integer);
var
  x, y, Loc, i, hw, xm, cm, cmPolOcean, cmPolNone: integer;
  PrevMiniPixel: TPixelPointer;
  MiniPixel: TPixelPointer;
  TerrainTile: Cardinal;
  MyCity: PCity;
  EnemyCity: PCityInfo;
  MyUnit: PUn;
  EnemyUnit: PUnitInfo;
begin
  if not Assigned(MyMap) then Exit;
  cmPolOcean := HGrSystem.Data.Canvas.Pixels[101, 67];
  cmPolNone := HGrSystem.Data.Canvas.Pixels[102, 67];
  hw := MapWidth div (xxt * 2);
  with Bitmap.Canvas do begin
    Brush.Color := $000000;
    FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
  end;
  Bitmap.PixelFormat := pf24bit;
  Bitmap.SetSize(Size.X * 2, Size.Y);
  Bitmap.BeginUpdate;
  MiniPixel := PixelPointer(Bitmap);
  PrevMiniPixel := PixelPointer(Bitmap, 0, -1);
  for y := 0 to ScaleToNative(Size.Y) - 1 do begin
    for x := 0 to ScaleToNative(Size.X) - 1 do begin
      Loc := ScaleFromNative(x) + Size.X * ScaleFromNative(y);
      if (MyMap[Loc] and fTerrain) <> fUNKNOWN then begin
        for i := 0 to 1 do begin
          xm := ((x - ScaleToNative(xwMini)) * 2 + i + y and 1 - ScaleToNative(hw) +
            ScaleToNative(Size.X) * 5) mod (ScaleToNative(Size.X) * 2);
          MiniPixel.SetX(xm);
          TerrainTile := MyMap[Loc] and fTerrain;
          if TerrainTile > 11 then TerrainTile := 0;
          cm := Colors[TerrainTile, i];
          if ClientMode = cEditMap then begin
            if MyMap[Loc] and (fPrefStartPos or fStartPos) <> 0 then
              cm := $FFFFFF;
          end
          else if MyMap[Loc] and fCity <> 0 then begin
            // City
            MyCity := GetMyCityByLoc(Loc);
            if Assigned(MyCity) then cm := Tribe[me].Color
            else begin
              EnemyCity := GetEnemyCityByLoc(Loc);
              if Assigned(EnemyCity) then
                cm := Tribe[EnemyCity^.Owner].Color;
            end;
            cm := $808080 or cm shr 1; { increase brightness }
            if y > 0 then begin
              // 2x2 city dot covers two lines
              PrevMiniPixel.SetX(xm);
              PrevMiniPixel.Pixel^.B := (cm shr 16) and $ff;
              PrevMiniPixel.Pixel^.G := (cm shr 8) and $ff;
              PrevMiniPixel.Pixel^.R := (cm shr 0) and $ff;
            end;
          end
          else if (i = 0) and (MyMap[Loc] and fUnit <> 0) then begin
            // Unit
            MyUnit := GetMyUnitByLoc(Loc);
            if Assigned(MyUnit) then cm := Tribe[me].Color
            else begin
              EnemyUnit := GetEnemyUnitByLoc(Loc);
              if Assigned(EnemyUnit) then
                cm := Tribe[EnemyUnit.Owner].Color;
            end;
            cm := $808080 or cm shr 1; { increase brightness }
          end
          else if moPolitical in MapOptions then begin
            // Political
            if MyMap[Loc] and fTerrain < fGrass then cm := cmPolOcean
            else if MyRO.Territory[Loc] < 0 then cm := cmPolNone
            else cm := Tribe[MyRO.Territory[Loc]].Color;
          end;
          MiniPixel.Pixel^.B := (cm shr 16) and $ff;
          MiniPixel.Pixel^.G := (cm shr 8) and $ff;
          MiniPixel.Pixel^.R := (cm shr 0) and $ff;
        end;
      end;
    end;
    MiniPixel.NextLine;
    PrevMiniPixel.NextLine;
  end;
  Bitmap.EndUpdate;
end;


end.

