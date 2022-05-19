{$INCLUDE Switches.inc}
// {$DEFINE TEXTLOG}
// {$DEFINE LOADPERF}
unit Database;

interface

uses
  SysUtils, Protocol, CmdList;

const
  // additional test flags
  //{$DEFINE FastContact} { extra small world with railroad everywhere }

  neumax = 4096;
  necmax = 1024;
  nemmax = 1024;

  lNoObserve = 0;
  lObserveUnhidden = 1;
  lObserveAll = 2;
  lObserveSuper = 3; // observe levels

  TerrType_Canalable = [fGrass, fDesert, fPrairie, fTundra, fSwamp,
    fForest, fHills];

  nStartUn = 1;
  StartUn: array [0 .. nStartUn - 1] of Integer = (0); // mix of start units

  CityOwnTile = 13;

type
  TGameMode = (moLoading_Fast, moLoading, moMovie, moPlaying);

var
  GAlive: Integer; { players alive; bitset of 1 shl p }
  GWatching: Integer;
  GInitialized: Integer;
  GAI: Integer;
  RND: Integer; { world map randseed }
  lx: Integer;
  ly: Integer;
  MapSize: Integer; // = lx*ly
  LandMass: Integer;
{$IFOPT O-}InvalidTreatyMap, {$ENDIF}
  SaveMapCenterLoc: Integer;
  PeaceEnded: Integer;
  GTurn: Integer; { current turn }
  GTestFlags: Integer;
  Mode: TGameMode;
  GWonder: array [0 .. nWonder - 1] of TWonderInfo;
  ServerVersion: array [0 .. nPl - 1] of Integer;
  ProcessClientData: array [0 .. nPl - 1] of Boolean;
  CL: TCmdList;
{$IFDEF TEXTLOG}CmdInfo: string;
  TextLog: TextFile; {$ENDIF}
{$IFDEF LOADPERF}time_total, time_total0, time_x0, time_x1, time_a, time_b, time_c: int64; {$ENDIF}
  // map data
  RealMap: array [0 .. lxmax * lymax - 1] of Cardinal;
  Continent: array [0 .. lxmax * lymax - 1] of Integer;
  { continent id for each tile }
  Occupant: array [0 .. lxmax * lymax - 1] of ShortInt;
  { occupying player for each tile }
  ZoCMap: array [0 .. lxmax * lymax - 1] of ShortInt;
  ObserveLevel: array [0 .. lxmax * lymax - 1] of Cardinal;
  { Observe Level of player p in bits 2*p and 2*p+1 }
  UsedByCity: array [0 .. lxmax * lymax - 1] of Integer;
  { location of exploiting city for
    each tile, =-1 if not exploited }

  // player data
  RW: array [0 .. nPl - 1] of TPlayerContext; { player data }
  Difficulty: array [0 .. nPl - 1] of Integer;
  GShip: array [0 .. nPl - 1] of TShipInfo;
  ResourceMask: array [0 .. nPl - 1] of Cardinal;
  Founded: array [0 .. nPl - 1] of Integer; { number of cities founded }
  TerritoryCount: array [0 .. nPl] of Integer;
  LastValidStat, Researched, Discovered, // number of tiles discovered
  GrWallContinent: array [0 .. nPl - 1] of Integer;
  RWemix: array [0 .. nPl - 1, 0 .. nPl - 1, 0 .. nmmax - 1] of SmallInt;
  // [p1,p2,mix] -> index of p2's model mix in p1's enemy model list
  Destroyed: array [0 .. nPl - 1, 0 .. nPl - 1, 0 .. nmmax - 1] of SmallInt;
  // [p1,p2,mix] -> number of p2's units with model mix that p1 has destroyed
  nTech: array [0 .. nPl - 1] of Integer; { number of known techs }
  // NewContact: array[0..nPl-1,0..nPl-1] of boolean;

type
  TVicinity8Loc = array [0 .. 7] of Integer;
  TVicinity21Loc = array [0 .. 27] of Integer;

procedure MaskD(var X: array of Cardinal; Count, Mask: Cardinal);
procedure IntServer(Command, Player, Subject: Integer; var Data);
procedure CompactLists(P: Integer);
procedure ClearTestFlags(ClearFlags: Integer);
procedure SetTestFlags(P, SetFlags: Integer);

// Tech Related Functions
function TechBaseCost(nTech, diff: Integer): Integer;
function TechCost(P: Integer): Integer;
procedure CalculateModel(var M: TModel);
procedure CheckSpecialModels(P, pre: Integer);
procedure EnableDevModel(P: Integer);
procedure SeeTech(P, ad: Integer);
procedure DiscoverTech(P, ad: Integer);
procedure CheckExpiration(Wonder: Integer);

// Location Navigation
function dLoc(Loc, dx, dy: Integer): Integer;
procedure dxdy(Loc0, Loc1: Integer; var dx, dy: Integer);
function Distance(Loc0, Loc1: Integer): Integer;
procedure V8_to_Loc(Loc0: Integer; var VicinityLoc: TVicinity8Loc);
procedure V21_to_Loc(Loc0: Integer; var VicinityLoc: TVicinity21Loc);

// Game Initialization
procedure InitRandomGame;
procedure InitMapGame(Human: Integer);
procedure ReleaseGame;

// Map Editor
function MapGeneratorAvailable: Boolean;
procedure CreateElevation;
procedure CreateMap(preview: Boolean);
procedure InitMapEditor;
procedure ReleaseMapEditor;
procedure EditTile(Loc, NewTile: Integer);

// Map Revealing
function GetTileInfo(P, cix, Loc: Integer; var Info: TTileInfo): Integer;
procedure Strongest(Loc: Integer; var uix, Strength, Bonus, Cnt: Integer);
function UnitSpeed(P, mix, Health: Integer): Integer;
procedure GetUnitReport(P, uix: Integer; var UnitReport: TUnitReport);
procedure SearchCity(Loc: Integer; var P, cix: Integer);
procedure TellAboutModel(P, taOwner, tamix: Integer);
function emixSafe(P, taOwner, tamix: Integer): Integer;
function Discover9(Loc, P, Level: Integer;
  TellAllied, EnableContact: Boolean): Boolean;
function Discover21(Loc, P, AdjacentLevel: Integer;
  TellAllied, EnableContact: Boolean): Boolean;
procedure DiscoverAll(P, Level: Integer);
procedure DiscoverViewAreas(P: Integer);
function GetUnitStack(P, Loc: Integer): Integer;
procedure UpdateUnitMap(Loc: Integer; CityChange: Boolean = False);
procedure RecalcV8ZoC(P, Loc: Integer);
procedure RecalcMapZoC(P: Integer);
procedure RecalcPeaceMap(P: Integer);

// Territory Calculation
procedure CheckBorders(OriginLoc: Integer; PlayerLosingCity: Integer = -1);
procedure LogCheckBorders(P, cix: Integer; PlayerLosingCity: Integer = -1);

// Map Processing
procedure CreateUnit(P, mix: Integer);
procedure FreeUnit(P, uix: Integer);
procedure PlaceUnit(P, uix: Integer);
procedure RemoveUnit(P, uix: Integer; Enemy: Integer = -1);
procedure RemoveUnit_UpdateMap(P, uix: Integer);
procedure RemoveAllUnits(P, Loc: Integer; Enemy: Integer = -1);
procedure RemoveDomainUnits(D, P, Loc: Integer);
procedure FoundCity(P, FoundLoc: Integer);
procedure DestroyCity(P, cix: Integer; SaveUnits: Boolean);
procedure ChangeCityOwner(pOld, cixOld, pNew: Integer);
procedure CompleteJob(P, Loc, Job: Integer);

// Diplomacy
procedure IntroduceEnemy(p1, p2: Integer);
procedure GiveCivilReport(P, pAbout: Integer);
procedure GiveMilReport(P, pAbout: Integer);
procedure ShowPrice(pSender, pTarget, Price: Integer);
function PayPrice(pSender, pTarget, Price: Integer; execute: Boolean): Boolean;
procedure CancelTreaty(P, pWith: Integer; DecreaseCredibility: Boolean = True);
function DoSpyMission(P, pCity, cix, Mission: Integer): Cardinal;


implementation

uses
{$IFDEF LOADPERF}SysUtils, Windows, {$ENDIF}
{$IFDEF TEXTLOG}SysUtils, {$ENDIF}
  IPQ;

var
  UnBuilt: array [0 .. nPl - 1] of Integer; { number of units built }

procedure MaskD(var X: array of Cardinal; Count, Mask: Cardinal);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    X[I] := X[I] and Mask;
end;

procedure CompactLists(P: Integer);
var
  uix, uix1, cix: Integer;
{$IFOPT O-}V21: Integer;
  Radius: TVicinity21Loc; {$ENDIF}
begin
  with RW[P] do
  begin
    // compact unit list
    uix := 0;
    while uix < nUn do
      if Un[uix].Loc < 0 then
      begin
        Dec(nUn);
        Un[uix] := Un[nUn]; { replace removed unit by last }
        if (Un[uix].TroopLoad > 0) or (Un[uix].AirLoad > 0) then
          for uix1 := 0 to nUn - 1 do
            if Un[uix1].Master = nUn then
              Un[uix1].Master := uix;
        // index of last unit changes
      end
      else
        Inc(uix);

    // compact city list
    cix := 0;
    while cix < nCity do
      if City[cix].Loc < 0 then
      begin
        Dec(nCity);
        City[cix] := City[nCity]; { replace city by last }
        for uix1 := 0 to nUn - 1 do
          if Un[uix1].Home = nCity then
            Un[uix1].Home := cix;
        { index of last city changes }
      end
      else
        Inc(cix);

    // compact enemy city list
    cix := 0;
    while cix < nEnemyCity do
      if EnemyCity[cix].Loc < 0 then
      begin
        Dec(nEnemyCity);
        EnemyCity[cix] := EnemyCity[nEnemyCity]; { replace city by last }
      end
      else
        Inc(cix);

{$IFOPT O-}
    for cix := 0 to nCity - 1 do
      with City[cix] do
      begin
        V21_to_Loc(Loc, Radius);
        for V21 := 1 to 26 do
          if Tiles and (1 shl V21) <> 0 then
            Assert(UsedByCity[Radius[V21]] = Loc);
      end;
{$ENDIF}
  end;
end;

{
  Tech Related Functions
  ____________________________________________________________________
}
function TechBaseCost(nTech, diff: Integer): Integer;
var
  c0: Single;
begin
  c0 := TechFormula_M[diff] * (nTech + 4) *
    exp((nTech + 4) / TechFormula_D[diff]);
  if c0 >= $10000000 then
    Result := $10000000
  else
    Result := trunc(c0);
end;

function TechCost(P: Integer): Integer;
begin
  with RW[P] do
  begin
    Result := TechBaseCost(nTech[P], Difficulty[P]);
    if ResearchTech >= 0 then
      if (ResearchTech = adMilitary) or (Tech[ResearchTech] = tsSeen) then
        Result := Result shr 1
      else if ResearchTech in FutureTech then
        if Government = gFuture then
          Result := Result * 2
        else
          Result := Result * 4;
  end;
end;

procedure SetModelFlags(var M: TModel);
begin
  M.Flags := 0;
  if (M.Domain = dGround) and (M.Kind <> mkDiplomat) then
    M.Flags := M.Flags or mdZOC;
  if (M.Kind = mkDiplomat) or (M.Attack + M.Cap[mcBombs] = 0) then
    M.Flags := M.Flags or mdCivil;
  if (M.Cap[mcOver] > 0) or (M.Domain = dSea) and (M.Weight >= 6) then
    M.Flags := M.Flags or mdDoubleSupport;
end;

procedure CalculateModel(var M: TModel);
{ calculate attack, defense, cost... of a model by features }
var
  I: Integer;
begin
  with M do
  begin
    Attack := (Cap[mcOffense] + Cap[mcOver]) * MStrength;
    Defense := (Cap[mcDefense] + Cap[mcOver]) * MStrength;
    case Domain of
      dGround:
        Speed := 150 + Cap[mcMob] * 50;
      dSea:
        begin
          Speed := 350 + 200 * Cap[mcNP] + 200 * Cap[mcTurbines];
          if Cap[mcNP] = 0 then
            Inc(Speed, 100 * Cap[mcSE]);
        end;
      dAir:
        Speed := 850 + 400 * Cap[mcJet];
    end;
    Cost := 0;
    for I := 0 to nFeature - 1 do
      if 1 shl Domain and Feature[I].Domains <> 0 then
        Inc(Cost, Cap[I] * Feature[I].Cost);
    Cost := Cost * MCost;
    Weight := 0;
    for I := 0 to nFeature - 1 do
      if 1 shl Domain and Feature[I].Domains <> 0 then
        if (Domain = dGround) and (I = mcDefense) then
          Inc(Weight, Cap[I] * 2)
        else
          Inc(Weight, Cap[I] * Feature[I].Weight);
  end;
  SetModelFlags(M);
end;

procedure CheckSpecialModels(P, pre: Integer);
var
  I, mix1: Integer;
  HasAlready: Boolean;
begin
  for I := 0 to nSpecialModel -
    1 do { check whether new special model available }
    if (SpecialModelPreq[I] = pre) and (RW[P].nModel < nmmax) then
    begin
      HasAlready := False;
      for mix1 := 0 to RW[P].nModel - 1 do
        if (RW[P].Model[mix1].Kind = SpecialModel[I].Kind) and
          (RW[P].Model[mix1].Attack = SpecialModel[I].Attack) and
          (RW[P].Model[mix1].Speed = SpecialModel[I].Speed) then
          HasAlready := True;
      if not HasAlready then
      begin
        RW[P].Model[RW[P].nModel] := SpecialModel[I];
        SetModelFlags(RW[P].Model[RW[P].nModel]);
        with RW[P].Model[RW[P].nModel] do
        begin
          Status := 0;
          SavedStatus := 0;
          IntroTurn := GTurn;
          Built := 0;
          Lost := 0;
          ID := P shl 12 + RW[P].nModel;
          if (Kind = mkSpecial_Boat) and (ServerVersion[P] < $000EF0) then
            Speed := 350; // old longboat
        end;
        Inc(RW[P].nModel);
      end;
    end;
end;

procedure EnableDevModel(P: Integer);
begin
  with RW[P] do
    if nModel < nmmax then
    begin
      Model[nModel] := DevModel;
      with Model[nModel] do
      begin
        Status := 0;
        SavedStatus := 0;
        IntroTurn := GTurn;
        Built := 0;
        Lost := 0;
        ID := P shl 12 + nModel;
      end;
      Inc(nModel);
      Inc(Researched[P]);
    end;
end;

procedure SeeTech(P, ad: Integer);
begin
{$IFDEF TEXTLOG}CmdInfo := CmdInfo + Format(' P%d:A%d', [P, ad]); {$ENDIF}
  RW[P].Tech[ad] := tsSeen;
  // inc(nTech[p]);
  Inc(Researched[P]);
end;

procedure FreeSlaves;
var
  p1, uix: Integer;
begin
  for p1 := 0 to nPl - 1 do
    if (GAlive and (1 shl p1) <> 0) then
      for uix := 0 to RW[p1].nUn - 1 do
        if RW[p1].Model[RW[p1].Un[uix].mix].Kind = mkSlaves then
          RW[p1].Un[uix].Job := jNone;
end;

procedure DiscoverTech(P, ad: Integer);

  procedure TellAboutKeyTech(P, Source: Integer);
  var
    I, p1: Integer;
  begin
    for I := 1 to 3 do
      if ad = AgePreq[I] then
        for p1 := 0 to nPl - 1 do
          if (p1 <> P) and ((GAlive or GWatching) and (1 shl p1) <> 0) then
            RW[p1].EnemyReport[P].Tech[ad] := Source;
  end;

var
  I: Integer;
begin
  if ad in FutureTech then
  begin
    if RW[P].Tech[ad] < tsApplicable then
      RW[P].Tech[ad] := 1
    else
      Inc(RW[P].Tech[ad]);
    if ad <> futResearchTechnology then
      Inc(nTech[P], 2);
    Inc(Researched[P], 8);
    Exit;
  end;

  if RW[P].Tech[ad] = tsSeen then
  begin
    Inc(nTech[P]);
    Inc(Researched[P]);
  end
  else
  begin
    Inc(nTech[P], 2);
    Inc(Researched[P], 2);
  end;
  RW[P].Tech[ad] := tsResearched;
  TellAboutKeyTech(P, tsResearched);
  CheckSpecialModels(P, ad);
  if ad = adScience then
    ResourceMask[P] := ResourceMask[P] or fSpecial2;
  if ad = adMassProduction then
    ResourceMask[P] := ResourceMask[P] or fModern;

  for I := 0 to nWonder - 1 do { check whether wonders expired }
    if (GWonder[I].EffectiveOwner <> GWonder[woEiffel].EffectiveOwner) and
      (Imp[I].Expiration = ad) then
    begin
      GWonder[I].EffectiveOwner := -1;
      if I = woPyramids then
        FreeSlaves;
    end;
end;

procedure CheckExpiration(Wonder: Integer);
// GWonder[Wonder].EffectiveOwner must be set before!
var
  P: Integer;
begin
  if (Imp[Wonder].Expiration >= 0) and
    (GWonder[woEiffel].EffectiveOwner <> GWonder[Wonder].EffectiveOwner) then
    for P := 0 to nPl - 1 do // check if already expired
      if (1 shl P and GAlive <> 0) and
        (RW[P].Tech[Imp[Wonder].Expiration] >= tsApplicable) then
      begin
        GWonder[Wonder].EffectiveOwner := -1;
        if Wonder = woPyramids then
          FreeSlaves;
      end;
end;

{
  Location Navigation
  ____________________________________________________________________
}
function dLoc(Loc, dx, dy: Integer): Integer;
{ relative location, dx in hor and dy in ver direction from Loc }
var
  y0: Integer;
begin
  if not (Loc >= 0) and (Loc < MapSize) and (dx + lx >= 0) then
    raise Exception.Create('Relative location error');
  Assert((Loc >= 0) and (Loc < MapSize) and (dx + lx >= 0));
  y0 := Loc div lx;
  Result := (Loc + (dx + y0 and 1 + lx + lx) shr 1) mod lx + lx * (y0 + dy);
  if (Result < 0) or (Result >= MapSize) then
    Result := -1;
end;

procedure dxdy(Loc0, Loc1: Integer; var dx, dy: Integer);
begin
  dx := ((Loc1 mod lx * 2 + Loc1 div lx and 1) -
    (Loc0 mod lx * 2 + Loc0 div lx and 1) + 3 * lx) mod (2 * lx) - lx;
  dy := Loc1 div lx - Loc0 div lx;
end;

function Distance(Loc0, Loc1: Integer): Integer;
var
  dx, dy: Integer;
begin
  dxdy(Loc0, Loc1, dx, dy);
  dx := abs(dx);
  dy := abs(dy);
  Result := dx + dy + abs(dx - dy) shr 1;
end;

procedure V8_to_Loc(Loc0: Integer; var VicinityLoc: TVicinity8Loc);
var
  x0, y0, lx0: Integer;
begin
  lx0 := lx; // put in register!
  y0 := Loc0 div lx0;
  x0 := Loc0 - y0 * lx0; // Loc0 mod lx;
  y0 := y0 and 1;
  VicinityLoc[1] := Loc0 + lx0 * 2;
  VicinityLoc[3] := Loc0 - 1;
  VicinityLoc[5] := Loc0 - lx0 * 2;
  VicinityLoc[7] := Loc0 + 1;
  Inc(Loc0, y0);
  VicinityLoc[0] := Loc0 + lx0;
  VicinityLoc[2] := Loc0 + lx0 - 1;
  VicinityLoc[4] := Loc0 - lx0 - 1;
  VicinityLoc[6] := Loc0 - lx0;

  // world is round!
  if x0 < lx0 - 1 then
  begin
    if x0 = 0 then
    begin
      Inc(VicinityLoc[3], lx0);
      if y0 = 0 then
      begin
        Inc(VicinityLoc[2], lx0);
        Inc(VicinityLoc[4], lx0);
      end;
    end;
  end
  else
  begin
    Dec(VicinityLoc[7], lx0);
    if y0 = 1 then
    begin
      Dec(VicinityLoc[0], lx0);
      Dec(VicinityLoc[6], lx0);
    end;
  end;
end;

procedure V21_to_Loc(Loc0: Integer; var VicinityLoc: TVicinity21Loc);
var
  dx, dy, bit, y0, xComp, yComp, xComp0, xCompSwitch: Integer;
  dst: ^Integer;
begin
  y0 := Loc0 div lx;
  xComp0 := Loc0 - y0 * lx - 1; // Loc0 mod lx -1
  xCompSwitch := xComp0 - 1 + y0 and 1;
  if xComp0 < 0 then
    Inc(xComp0, lx);
  if xCompSwitch < 0 then
    Inc(xCompSwitch, lx);
  xCompSwitch := xCompSwitch xor xComp0;
  yComp := lx * (y0 - 3);
  dst := @VicinityLoc;
  bit := 1;
  for dy := 0 to 6 do
  begin
    xComp0 := xComp0 xor xCompSwitch;
    xComp := xComp0;
    for dx := 0 to 3 do
    begin
      if bit and $67F7F76 <> 0 then
        dst^ := xComp + yComp
      else
        dst^ := -1;
      Inc(xComp);
      if xComp >= lx then
        Dec(xComp, lx);
      Inc(dst);
      bit := bit shl 1;
    end;
    Inc(yComp, lx);
  end;
end;

{
  Map Creation
  ____________________________________________________________________
}
var
  primitive: Integer;
  StartLoc, StartLoc2: array [0 .. nPl - 1] of Integer; { starting coordinates }
  Elevation: array [0 .. lxmax * lymax - 1] of Byte; { map elevation }
  ElCount: array [Byte] of Integer; { count of elevation occurance }

procedure CalculatePrimitive;
var
  I, J: Integer;
begin
  primitive := 1;
  I := 2;
  while I * I <= MapSize + 1 do // test whether prime
  begin
    if (MapSize + 1) mod I = 0 then
      primitive := 0;
    Inc(I);
  end;

  if primitive > 0 then
    repeat
      Inc(primitive);
      I := 1;
      J := 0;
      repeat
        Inc(J);
        I := I * primitive mod (MapSize + 1);
      until (I = 1) or (J = MapSize + 1);
    until J = MapSize;
end;

function MapGeneratorAvailable: Boolean;
begin
  Result := (primitive > 0) and (lx >= 20) and (ly >= 40);
end;

procedure CreateElevation;
const
  D = 64;
  Smooth = 0.049; { causes low amplitude of short waves }
  Detail = 0.095; { causes short period of short waves }
  Merge = 5; { elevation merging range at the connection line of the
    round world,in relation to lx }

var
  sa, ca, f1, f2: array [1 .. D] of Single;
  imerge, X, Y: Integer;
  V, maxv: Single;

  function Value(X, Y: Integer): Single; { elevation formula }
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to D do
      Result := Result + sin(f1[I] * ((X * 2 + Y and 1) * sa[I] + Y * 1.5 *
        ca[I])) * f2[I];
    { x values effectively multiplied with 2 to get 2 horizantal periods
      of the prime waves }
  end;

begin
  for X := 1 to D do { prepare formula parameters }
  begin
{$IFNDEF SCR} if X = 1 then
      V := pi / 2 { first wave goes horizontal }
    else {$ENDIF} V := DelphiRandom * 2 * pi;
    sa[X] := sin(V) / lx;
    ca[X] := cos(V) / ly;
    f1[X] := 2 * pi * exp(Detail * (X - 1));
    f2[X] := exp(-X * Smooth);
  end;

  imerge := 2 * lx div Merge;
  FillChar(ElCount, SizeOf(ElCount), 0);
  maxv := 0;
  for X := 0 to lx - 1 do
    for Y := 0 to ly - 1 do
    begin
      V := Value(X, Y);
      if X * 2 < imerge then
        V := (X * 2 * V + (imerge - X * 2) * Value(X + lx, Y)) / imerge;
      V := V - sqr(sqr(2 * Y / ly - 1)); { soft cut at poles }
      if V > maxv then
        maxv := V;

      if V < -4 then
        Elevation[X + lx * Y] := 0
      else if V > 8.75 then
        Elevation[X + lx * Y] := 255
      else
        Elevation[X + lx * Y] := Round((V + 4) * 20);
      Inc(ElCount[Elevation[X + lx * Y]]);
    end;
end;

procedure FindContinents;

  procedure ReplaceCont(A, B, Stop: Integer);
  { replace continent name a by b }
  // make sure always continent[loc]<=loc
  var
    I: Integer;
  begin
    if A < B then
    begin
      I := A;
      A := B;
      B := I
    end;
    if A > B then
      for I := A to Stop do
        if Continent[I] = A then
          Continent[I] := B;
  end;

var
  X, Y, Loc, Wrong: Integer;
begin
  for Y := 1 to ly - 2 do
    for X := 0 to lx - 1 do
    begin
      Loc := X + lx * Y;
      Continent[Loc] := -1;
      if RealMap[Loc] and fTerrain >= fGrass then
      begin
        if (Y - 2 >= 1) and (RealMap[Loc - 2 * lx] and fTerrain >= fGrass) then
          Continent[Loc] := Continent[Loc - 2 * lx];
        if (X - 1 + Y and 1 >= 0) and (Y - 1 >= 1) and
          (RealMap[Loc - 1 + Y and 1 - lx] and fTerrain >= fGrass) then
          Continent[Loc] := Continent[Loc - 1 + Y and 1 - lx];
        if (X + Y and 1 < lx) and (Y - 1 >= 1) and
          (RealMap[Loc + Y and 1 - lx] and fTerrain >= fGrass) then
          Continent[Loc] := Continent[Loc + Y and 1 - lx];
        if (X - 1 >= 0) and (RealMap[Loc - 1] and fTerrain >= fGrass) then
          if Continent[Loc] = -1 then
            Continent[Loc] := Continent[Loc - 1]
          else
            ReplaceCont(Continent[Loc - 1], Continent[Loc], Loc);
        if Continent[Loc] = -1 then
          Continent[Loc] := Loc;
      end;
    end;

  { connect continents due to round earth }
  for Y := 1 to ly - 2 do
    if RealMap[lx * Y] and fTerrain >= fGrass then
    begin
      Wrong := -1;
      if RealMap[lx - 1 + lx * Y] and fTerrain >= fGrass then
        Wrong := Continent[lx - 1 + lx * Y];
      if (Y and 1 = 0) and (Y - 1 >= 1) and
        (RealMap[lx - 1 + lx * (Y - 1)] and fTerrain >= fGrass) then
        Wrong := Continent[lx - 1 + lx * (Y - 1)];
      if (Y and 1 = 0) and (Y + 1 < ly - 1) and
        (RealMap[lx - 1 + lx * (Y + 1)] and fTerrain >= fGrass) then
        Wrong := Continent[lx - 1 + lx * (Y + 1)];
      if Wrong >= 0 then
        ReplaceCont(Wrong, Continent[lx * Y], MapSize - 1);
    end;
end;

procedure RarePositions;
// distribute rare resources
// must be done after FindContinents
var
  I, J, Cnt, X, Y, dx, dy, Loc0, Loc1, xworst, yworst, totalrare, RareMaxWater,
    RareType, iBest, jbest, MinDist, xBlock, yBlock, V8: Integer;
  AreaCount, RareByArea, RareAdjacent: array [0 .. 7, 0 .. 4] of Integer;
  RareLoc: array [0 .. 11] of Integer;
  Dist: array [0 .. 11, 0 .. 11] of Integer;
  Adjacent: TVicinity8Loc;
begin
  RareMaxWater := 0;
  repeat
    FillChar(AreaCount, SizeOf(AreaCount), 0);
    for Y := 1 to ly - 2 do
    begin
      yBlock := Y * 5 div ly;
      if yBlock = (Y + 1) * 5 div ly then
        for X := 0 to lx - 1 do
        begin
          xBlock := X * 8 div lx;
          if xBlock = (X + 1) * 8 div lx then
          begin
            Loc0 := X + lx * Y;
            if RealMap[Loc0] and fTerrain >= fGrass then
            begin
              Cnt := 0;
              V8_to_Loc(Loc0, Adjacent);
              for V8 := 0 to 7 do
              begin
                Loc1 := Adjacent[V8];
                if (Loc1 >= 0) and (Loc1 < MapSize) and
                  (RealMap[Loc1] and fTerrain < fGrass) then
                  Inc(Cnt); // count adjacent water
              end;
              if Cnt <= RareMaxWater then // inner land
              begin
                Inc(AreaCount[xBlock, yBlock]);
                if DelphiRandom(AreaCount[xBlock, yBlock]) = 0 then
                  RareByArea[xBlock, yBlock] := Loc0;
              end;
            end;
          end;
        end;
    end;
    totalrare := 0;
    for X := 0 to 7 do
      for Y := 0 to 4 do
        if AreaCount[X, Y] > 0 then
          Inc(totalrare);
    Inc(RareMaxWater);
  until totalrare >= 12;

  while totalrare > 12 do
  begin // remove rarebyarea resources too close to each other
    FillChar(RareAdjacent, SizeOf(RareAdjacent), 0);
    for X := 0 to 7 do
      for Y := 0 to 4 do
        if AreaCount[X, Y] > 0 then
        begin
          if (AreaCount[(X + 1) mod 8, Y] > 0) and
            (Continent[RareByArea[X, Y]] = Continent
            [RareByArea[(X + 1) mod 8, Y]]) then
          begin
            Inc(RareAdjacent[X, Y]);
            Inc(RareAdjacent[(X + 1) mod 8, Y]);
          end;
          if Y < 4 then
          begin
            if (AreaCount[X, Y + 1] > 0) and
              (Continent[RareByArea[X, Y]] = Continent[RareByArea[X, Y + 1]])
            then
            begin
              Inc(RareAdjacent[X, Y]);
              Inc(RareAdjacent[X, Y + 1]);
            end;
            if (AreaCount[(X + 1) mod 8, Y + 1] > 0) and
              (Continent[RareByArea[X, Y]] = Continent[RareByArea[(X + 1) mod 8,
              Y + 1]]) then
            begin
              Inc(RareAdjacent[X, Y]);
              Inc(RareAdjacent[(X + 1) mod 8, Y + 1]);
            end;
            if (AreaCount[(X + 7) mod 8, Y + 1] > 0) and
              (Continent[RareByArea[X, Y]] = Continent[RareByArea[(X + 7) mod 8,
              Y + 1]]) then
            begin
              Inc(RareAdjacent[X, Y]);
              Inc(RareAdjacent[(X + 7) mod 8, Y + 1]);
            end;
          end;
        end;
    xworst := 0;
    yworst := 0;
    Cnt := 0;
    for X := 0 to 7 do
      for Y := 0 to 4 do
        if AreaCount[X, Y] > 0 then
        begin
          if (Cnt = 0) or (RareAdjacent[X, Y] > RareAdjacent[xworst, yworst])
          then
          begin
            xworst := X;
            yworst := Y;
            Cnt := 1;
          end
          else if (RareAdjacent[X, Y] = RareAdjacent[xworst, yworst]) then
          begin
            Inc(Cnt);
            if DelphiRandom(Cnt) = 0 then
            begin
              xworst := X;
              yworst := Y;
            end;
          end;
        end;
    AreaCount[xworst, yworst] := 0;
    Dec(totalrare);
  end;

  Cnt := 0;
  for X := 0 to 7 do
    for Y := 0 to 4 do
      if AreaCount[X, Y] > 0 then
      begin
        RareLoc[Cnt] := RareByArea[X, Y];
        Inc(Cnt);
      end;
  for I := 0 to 11 do
  begin
    RealMap[RareLoc[I]] := RealMap[RareLoc[I]] and not(fTerrain or fSpecial) or
      (fDesert or fDeadLands);
    for dy := -1 to 1 do
      for dx := -1 to 1 do
        if (dx + dy) and 1 = 0 then
        begin
          Loc1 := dLoc(RareLoc[I], dx, dy);
          if (Loc1 >= 0) and (RealMap[Loc1] and fTerrain = fMountains) then
            RealMap[Loc1] := RealMap[Loc1] and not fTerrain or fHills;
        end;
  end;
  for I := 0 to 11 do
    for J := 0 to 11 do
      Dist[I, J] := Distance(RareLoc[I], RareLoc[J]);

  ibest := 0;
  jbest := 0;
  MinDist := Distance(0, MapSize - lx shr 1) shr 1;
  for RareType := 1 to 3 do
  begin
    Cnt := 0;
    for I := 0 to 11 do
      if RareLoc[I] >= 0 then
        for J := 0 to 11 do
          if RareLoc[J] >= 0 then
            if (Cnt > 0) and (Dist[iBest, jbest] >= MinDist) then
            begin
              if Dist[I, J] >= MinDist then
              begin
                Inc(Cnt);
                if DelphiRandom(Cnt) = 0 then
                begin
                  iBest := I;
                  jbest := J;
                end;
              end;
            end
            else if (Cnt = 0) or (Dist[I, J] > Dist[iBest, jbest]) then
            begin
              iBest := I;
              jbest := J;
              Cnt := 1;
            end;
    RealMap[RareLoc[iBest]] := RealMap[RareLoc[iBest]] or
      Cardinal(RareType) shl 25;
    RealMap[RareLoc[jbest]] := RealMap[RareLoc[jbest]] or
      Cardinal(RareType) shl 25;
    RareLoc[iBest] := -1;
    RareLoc[jbest] := -1;
  end;
end;

function CheckShore(Loc: Integer): Boolean;
var
  Loc1, OldTile, V21: Integer;
  Radius: TVicinity21Loc;
begin
  Result := False;
  OldTile := RealMap[Loc];
  if OldTile and fTerrain < fGrass then
  begin
    RealMap[Loc] := RealMap[Loc] and not fTerrain or fOcean;
    V21_to_Loc(Loc, Radius);
    for V21 := 1 to 26 do
    begin
      Loc1 := Radius[V21];
      if (Loc1 >= 0) and (Loc1 < MapSize) and
        (RealMap[Loc1] and fTerrain >= fGrass) and
        (RealMap[Loc1] and fTerrain <> fArctic) then
        RealMap[Loc] := RealMap[Loc] and not fTerrain or fShore;
    end;
    if (RealMap[Loc] xor Cardinal(OldTile)) and fTerrain <> 0 then
      Result := True;
  end;
end;

function ActualSpecialTile(Loc: Integer): Cardinal;
begin
  Result := SpecialTile(Loc, RealMap[Loc] and fTerrain, lx);
end;

procedure CreateMap(preview: Boolean);
const
  ShHiHills = 6; { of land }
  ShMountains = 6; { of land }
  ShRandHills = 12; { of land }
  ShTestRiver = 40;
  ShSwamp = 25; { of grassland }
  MinRivLen = 3;
  unification = 70;
  hotunification = 50; // min. 25

  Zone: array [0 .. 3, 2 .. 9] of Single = { terrain distribution }
    ((0.25, 0, 0, 0.4, 0, 0, 0, 0.35), (0.55, 0, 0.1, 0, 0, 0, 0, 0.35),
    (0.4, 0, 0.35, 0, 0, 0, 0, 0.25), (0, 0.7, 0, 0, 0, 0, 0, 0.3));
  { Grs  Dst  Pra  Tun  - - - For }

  function RndLow(Y: Integer): Cardinal;
  { random lowland appropriate to climate }
  var
    z0, I: Integer;
    P, p0, ZPlus: Single;
  begin
    if ly - 1 - Y > Y then
    begin
      z0 := 6 * Y div ly;
      ZPlus := 6 * Y / ly - z0;
    end
    else
    begin
      z0 := 6 * (ly - 1 - Y) div ly;
      ZPlus := 6 * (ly - 1 - Y) / ly - z0;
    end;
    p0 := 1;
    for I := 2 to 9 do
    begin
      P := Zone[z0, I] * (1 - ZPlus) + Zone[z0 + 1, I] * ZPlus;
      { weight between zones z0 and z0+1 }
      if DelphiRandom * p0 < P then
      begin
        RndLow := I;
        Break;
      end;
      p0 := p0 - P;
    end;
  end;

  function RunRiver(Loc0: Integer): Integer;
  { runs river from start point Loc0; return value: length }
  var
    Dir, T, Loc, Loc1, Cost: Integer;
    Q: TIPQ;
    From: array [0 .. lxmax * lymax - 1] of Integer;
    Time: array [0 .. lxmax * lymax - 1] of Integer;
    OneTileLake: Boolean;
  begin
    FillChar(Time, SizeOf(Time), 255); { -1 }
    Q := TIPQ.Create(MapSize);
    Q.Put(Loc0, 0);
    while Q.Get(Loc, T) and (RealMap[Loc] and fRiver = 0) do
    begin
      if (RealMap[Loc] and fTerrain < fGrass) then
      begin
        OneTileLake := True;
        for Dir := 0 to 3 do
        begin
          Loc1 := dLoc(Loc, Dir and 1 * 2 - 1, Dir shr 1 * 2 - 1);
          if (Loc1 >= 0) and (RealMap[Loc1] and fTerrain < fGrass) then
            OneTileLake := False;
        end;
        if not OneTileLake then
          Break;
      end;
      Time[Loc] := T;
      for Dir := 0 to 3 do
      begin
        Loc1 := dLoc(Loc, Dir and 1 * 2 - 1, Dir shr 1 * 2 - 1);
        if (Loc1 >= lx) and (Loc1 < lx * (ly - 1)) and (Time[Loc1] < 0) then
        begin
          if RealMap[Loc1] and fRiver = 0 then
          begin
            Cost := Elevation[Loc1] - Elevation[Loc];
            if Cost < 0 then
              Cost := 0;
          end
          else
            Cost := 0;
          if Q.Put(Loc1, T + Cost shl 8 + 1) then
            From[Loc1] := Loc;
        end;
      end;
    end;
    Loc1 := Loc;
    Result := 0;
    while Loc <> Loc0 do
    begin
      Loc := From[Loc];
      Inc(Result);
    end;
    if (Result > 1) and ((Result >= MinRivLen) or
      (RealMap[Loc1] and fTerrain >= fGrass)) then
    begin
      Loc := Loc1;
      while Loc <> Loc0 do
      begin
        Loc := From[Loc];
        if RealMap[Loc] and fTerrain in [fHills, fMountains] then
          RealMap[Loc] := fGrass or fRiver
        else if RealMap[Loc] and fTerrain >= fGrass then
          RealMap[Loc] := RealMap[Loc] or fRiver;
      end;
    end
    else
      Result := 0;
    FreeAndNil(Q);
  end;

var
  X, Y, N, Dir, plus, Count, Loc0, Loc1, bLand, bHills, bMountains, V8: Integer;
  CopyFrom: array [0 .. lxmax * lymax - 1] of Integer;
  Adjacent: TVicinity8Loc;

begin
  FillChar(RealMap, MapSize * SizeOf(Cardinal), 0);
  plus := 0;
  bMountains := 256;
  while plus < MapSize * LandMass * ShMountains div 10000 do
  begin
    Dec(bMountains);
    Inc(plus, ElCount[bMountains]);
  end;
  Count := plus;
  plus := 0;
  bHills := bMountains;
  while plus < MapSize * LandMass * ShHiHills div 10000 do
  begin
    Dec(bHills);
    Inc(plus, ElCount[bHills]);
  end;
  Inc(Count, plus);
  bLand := bHills;
  while Count < MapSize * LandMass div 100 do
  begin
    Dec(bLand);
    Inc(Count, ElCount[bLand]);
  end;

  for Loc0 := lx to lx * (ly - 1) - 1 do
    if Elevation[Loc0] >= bMountains then
      RealMap[Loc0] := fMountains
    else if Elevation[Loc0] >= bHills then
      RealMap[Loc0] := fHills
    else if Elevation[Loc0] >= bLand then
      RealMap[Loc0] := fGrass;

  // remove one-tile islands
  for Loc0 := 0 to MapSize - 1 do
    if RealMap[Loc0] >= fGrass then
    begin
      Count := 0;
      V8_to_Loc(Loc0, Adjacent);
      for V8 := 0 to 7 do
      begin
        Loc1 := Adjacent[V8];
        if (Loc1 < 0) or (Loc1 >= MapSize) or
          (RealMap[Loc1] and fTerrain < fGrass) or
          (RealMap[Loc1] and fTerrain = fArctic) then
          Inc(Count); // count adjacent water
      end;
      if Count = 8 then
        RealMap[Loc0] := fOcean;
    end;

  if not preview then
  begin
    plus := 36 * 56 * 20 * ShTestRiver div (LandMass * 100);
    if plus > MapSize then
      plus := MapSize;
    Loc0 := DelphiRandom(MapSize);
    for N := 0 to plus - 1 do
    begin
      if (RealMap[Loc0] and fTerrain >= fGrass) and (Loc0 >= lx) and
        (Loc0 < MapSize - lx) then
        RunRiver(Loc0);
      Loc0 := (Loc0 + 1) * primitive mod (MapSize + 1) - 1;
    end;
  end;

  for Loc0 := 0 to MapSize - 1 do
    if (RealMap[Loc0] = fGrass) and (DelphiRandom(100) < ShRandHills) then
      RealMap[Loc0] := RealMap[Loc0] or fHills;

  // make terrain types coherent
  for Loc0 := 0 to MapSize - 1 do
    CopyFrom[Loc0] := Loc0;

  for N := 0 to unification * MapSize div 100 do
  begin
    Y := DelphiRandom(ly);
    if abs(Y - (ly shr 1)) > ly div 4 + DelphiRandom(ly * hotunification div 100) then
      if Y < ly shr 1 then
        Y := ly shr 1 - Y
      else
        Y := 3 * ly shr 1 - Y;
    Loc0 := lx * Y + DelphiRandom(lx);
    if RealMap[Loc0] and fTerrain = fGrass then
    begin
      Dir := DelphiRandom(4);
      Loc1 := dLoc(Loc0, Dir and 1 * 2 - 1, Dir shr 1 * 2 - 1);
      if (Loc1 >= 0) and (RealMap[Loc1] and fTerrain = fGrass) then
      begin
        while CopyFrom[Loc0] <> Loc0 do
          Loc0 := CopyFrom[Loc0];
        while CopyFrom[Loc1] <> Loc1 do
          Loc1 := CopyFrom[Loc1];
        if Loc1 < Loc0 then
          CopyFrom[Loc0] := Loc1
        else
          CopyFrom[Loc1] := Loc0;
      end;
    end;
  end;

  for Loc0 := 0 to MapSize - 1 do
    if (RealMap[Loc0] and fTerrain = fGrass) and (CopyFrom[Loc0] = Loc0) then
      RealMap[Loc0] := RealMap[Loc0] and not fTerrain or RndLow(Loc0 div lx);

  for Loc0 := 0 to MapSize - 1 do
    if RealMap[Loc0] and fTerrain = fGrass then
    begin
      Loc1 := Loc0;
      while CopyFrom[Loc1] <> Loc1 do
        Loc1 := CopyFrom[Loc1];
      RealMap[Loc0] := RealMap[Loc0] and not fTerrain or
        RealMap[Loc1] and fTerrain;
    end;

  for Loc0 := 0 to MapSize - 1 do
    if RealMap[Loc0] and fTerrain = fGrass then
    begin // change grassland to swamp
      if DelphiRandom(100) < ShSwamp then
        RealMap[Loc0] := RealMap[Loc0] and not fTerrain or fSwamp;
    end;

  for Loc0 := 0 to MapSize - 1 do // change desert to prairie 1
    if RealMap[Loc0] and fTerrain = fDesert then
    begin
      if RealMap[Loc0] and fRiver <> 0 then
        Count := 5
      else
      begin
        Count := 0;
        for Dir := 0 to 3 do
        begin
          Loc1 := dLoc(Loc0, Dir and 1 * 2 - 1, Dir shr 1 * 2 - 1);
          if Loc1 >= 0 then
            if RealMap[Loc1] and fTerrain < fGrass then
              Inc(Count, 2);
        end;
      end;
      if Count >= 4 then
        RealMap[Loc0] := RealMap[Loc0] and not fTerrain or fPrairie;
    end;

  for Loc0 := 0 to MapSize - 1 do // change desert to prairie 2
    if RealMap[Loc0] and fTerrain = fDesert then
    begin
      Count := 0;
      for Dir := 0 to 3 do
      begin
        Loc1 := dLoc(Loc0, Dir and 1 * 2 - 1, Dir shr 1 * 2 - 1);
        if Loc1 >= 0 then
          if RealMap[Loc1] and fTerrain <> fDesert then
            Inc(Count);
      end;
      if Count >= 4 then
        RealMap[Loc0] := RealMap[Loc0] and not fTerrain or fPrairie;
    end;

  for Loc0 := 0 to MapSize - 1 do
    CheckShore(Loc0); // change ocean to shore
  for X := 0 to lx - 1 do
  begin
    RealMap[X + lx * 0] := fArctic;
    if RealMap[X + lx * 1] >= fGrass then
      RealMap[X + lx * 1] := RealMap[X + lx * 1] and not fTerrain or fTundra;
    if RealMap[X + lx * (ly - 2)] >= fGrass then
      RealMap[X + lx * (ly - 2)] := RealMap[X + lx * (ly - 2)] and
        not fTerrain or fTundra;
    RealMap[X + lx * (ly - 1)] := fArctic;
  end;

  for Loc0 := 0 to MapSize - 1 do // define special terrain tiles
    RealMap[Loc0] := RealMap[Loc0] or ActualSpecialTile(Loc0) shl 5 or
      ($F shl 27);

  if not preview then
  begin
    FindContinents;
    RarePositions;
  end;
end;

procedure StartPositions;
// define nation start positions
// must be done after FindContinents

var
  CountGood: (cgBest, cgFlat, cgLand);

  function IsGoodTile(Loc: Integer): Boolean;
  var
    xLoc, yLoc: Integer;
  begin
    xLoc := Loc mod lx;
    yLoc := Loc div lx;
    if RealMap[Loc] and fDeadLands <> 0 then
      Result := False
    else
      case CountGood of
        cgBest:
          Result := (RealMap[Loc] and fTerrain in [fGrass, fPrairie, fTundra,
            fSwamp, fForest]) and Odd((lymax + xLoc - yLoc shr 1) shr 1 + xLoc +
            (yLoc + 1) shr 1);
        cgFlat:
          Result := (RealMap[Loc] and fTerrain in [fGrass, fPrairie, fTundra,
            fSwamp, fForest]);
        cgLand:
          Result := RealMap[Loc] and fTerrain >= fGrass;
      end;
  end;

const
  MaxCityLoc = 64;

var
  p1, p2, nAlive, C, Loc, Loc1, CntGood, CntGoodGrass, MinDist, I, J, N,
    nsc, V21, V8, BestDist, TestDist, MinGood, nIrrLoc,
    FineDistSQR, nRest: Integer;
  ccount: array [0 .. lxmax * lymax - 1] of Word;
  sc, StartLoc0, sccount: array [1 .. nPl] of Integer;
  TestStartLoc: array [0 .. nPl - 1] of Integer;
  CityLoc: array [1 .. nPl, 0 .. MaxCityLoc - 1] of Integer;
  nCityLoc: array [1 .. nPl] of Integer;
  RestLoc: array [0 .. MaxCityLoc - 1] of Integer;
  IrrLoc: array [0 .. 20] of Integer;
  Radius: TVicinity21Loc;
  Adjacent: TVicinity8Loc;
  ok: Boolean;

begin
  nAlive := 0;
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and GAlive <> 0 then
      Inc(nAlive);
  if nAlive = 0 then
    Exit;

  { count good tiles }
  FillChar(ccount, MapSize * 2, 0);
  for Loc := 0 to MapSize - 1 do
    if RealMap[Loc] and fTerrain = fGrass then
      if ActualSpecialTile(Loc) = 1 then
        Inc(ccount[Continent[Loc]], 3)
      else
        Inc(ccount[Continent[Loc]], 2)
    else if RealMap[Loc] and fTerrain in [fPrairie, fSwamp, fForest, fHills]
    then
      Inc(ccount[Continent[Loc]]);

  Loc := 0;
  while ccount[Loc] > 0 do
    Inc(Loc);
  for I := 1 to nAlive do
  begin
    sc[I] := Loc;
    sccount[I] := 1
  end;
  { init with zero size start continents, then search bigger ones }
  for Loc := 0 to MapSize - 1 do
    if ccount[Loc] > 0 then
    begin // search biggest continents
      p1 := nAlive + 1;
      while (p1 > 1) and (ccount[Loc] > ccount[sc[p1 - 1]]) do
      begin
        if p1 < nAlive + 1 then
          sc[p1] := sc[p1 - 1];
        Dec(p1);
      end;
      if p1 < nAlive + 1 then
        sc[p1] := Loc;
    end;
  nsc := nAlive;
  repeat
    C := 1; // search least crowded continent after smallest
    for I := 2 to nsc - 1 do
      if ccount[sc[I]] * (2 * sccount[C] + 1) > ccount[sc[C]] *
        (2 * sccount[I] + 1) then
        C := I;
    if ccount[sc[nsc]] * (2 * sccount[C] + 1) > ccount[sc[C]] then
      Break; // even least crowded continent is more crowded than smallest
    Inc(sccount[C]);
    Dec(nsc);
  until sccount[nsc] > 1;

  MinGood := 7;
  CountGood := cgBest;
  repeat
    Dec(MinGood);
    if (MinGood = 3) and (CountGood < cgLand) then // too demanding!
    begin
      Inc(CountGood);
      MinGood := 6;
    end;
    FillChar(nCityLoc, SizeOf(nCityLoc), 0);
    Loc := DelphiRandom(MapSize);
    for I := 0 to MapSize - 1 do
    begin
      if ((Loc >= 4 * lx) and (Loc < MapSize - 4 * lx) or (CountGood >= cgLand))
        and IsGoodTile(Loc) then
      begin
        C := nsc;
        while (C > 0) and (Continent[Loc] <> sc[C]) do
          Dec(C);
        if (C > 0) and (nCityLoc[C] < MaxCityLoc) then
        begin
          CntGood := 1;
          V21_to_Loc(Loc, Radius);
          for V21 := 1 to 26 do
            if V21 <> CityOwnTile then
            begin
              Loc1 := Radius[V21];
              if (Loc1 >= 0) and (Loc1 < MapSize) and IsGoodTile(Loc1) then
                Inc(CntGood);
            end;
          if CntGood >= MinGood then
          begin
            CityLoc[C, nCityLoc[C]] := Loc;
            Inc(nCityLoc[C]);
          end;
        end;
      end;
      Loc := (Loc + 1) * primitive mod (MapSize + 1) - 1;
    end;

    ok := True;
    for C := 1 to nsc do
      if nCityLoc[C] < sccount[C] * (8 - MinGood) div (7 - MinGood) then
        ok := False;
  until ok;

  FineDistSQR := MapSize * LandMass * 9 div (nAlive * 100);
  p1 := 1;
  for C := 1 to nsc do
  begin // for all start continents
    if sccount[C] = 1 then
      StartLoc0[p1] := CityLoc[C, DelphiRandom(nCityLoc[C])]
    else
    begin
      BestDist := 0;
      N := 1 shl sccount[C] * 32; // number of tries to find good distribution
      if N > 1 shl 12 then
        N := 1 shl 12;
      while (N > 0) and (BestDist * BestDist < FineDistSQR) do
      begin
        MinDist := MaxInt;
        nRest := nCityLoc[C];
        for I := 0 to nRest - 1 do
          RestLoc[I] := CityLoc[C, I];
        for I := 0 to sccount[C] - 1 do
        begin
          if nRest = 0 then
            Break;
          J := DelphiRandom(nRest);
          TestStartLoc[I] := RestLoc[J];
          RestLoc[J] := RestLoc[nRest - 1];
          Dec(nRest);
          for J := 0 to I - 1 do
          begin
            TestDist := Distance(TestStartLoc[I], TestStartLoc[J]);
            if TestDist < MinDist then
              MinDist := TestDist;
          end;
          if I = sccount[C] - 1 then
          begin
            Assert(MinDist > BestDist);
            BestDist := MinDist;
            for J := 0 to sccount[C] - 1 do
              StartLoc0[p1 + J] := TestStartLoc[J];
          end
          else if BestDist > 0 then
          begin
            J := 0;
            while J < nRest do
            begin // remove all locs from rest which have too little distance to this one
              TestDist := Distance(TestStartLoc[I], RestLoc[J]);
              if TestDist <= BestDist then
              begin
                RestLoc[J] := RestLoc[nRest - 1];
                Dec(nRest);
              end
              else
                Inc(J);
            end;
          end;
        end;
        Dec(N)
      end;
    end;
    p1 := p1 + sccount[C]
  end;

  // make start locs fertile
  for p1 := 1 to nAlive do
  begin
    RealMap[StartLoc0[p1]] := RealMap[StartLoc0[p1]] and
      not(fTerrain or fSpecial) or fGrass or fSpecial1;
    CntGood := 1;
    CntGoodGrass := 1;
    V21_to_Loc(StartLoc0[p1], Radius);
    for V21 := 1 to 26 do
      if V21 <> CityOwnTile then
      begin
        Loc1 := Radius[V21];
        if (Loc1 >= 0) and (Loc1 < MapSize) and IsGoodTile(Loc1) then
          if RealMap[Loc1] and fTerrain = fGrass then
            Inc(CntGoodGrass)
          else
            Inc(CntGood);
      end;
    for V21 := 1 to 26 do
      if V21 <> CityOwnTile then
      begin
        Loc1 := Radius[V21];
        if (Loc1 >= 0) and (Loc1 < MapSize) and
          (RealMap[Loc1] and fDeadLands = 0) then
          if IsGoodTile(Loc1) and (DelphiRandom(CntGood) < MinGood - CntGoodGrass + 1)
          then
          begin
            RealMap[Loc1] := RealMap[Loc1] and not(fTerrain or fSpecial)
              or fGrass;
            RealMap[Loc1] := RealMap[Loc1] or ActualSpecialTile(Loc1) shl 5;
          end
          else if RealMap[Loc1] and fTerrain = fDesert then
            RealMap[Loc1] := RealMap[Loc1] and not fTerrain or fPrairie
          else if (RealMap[Loc1] and fTerrain in [fPrairie, fTundra, fSwamp])
            and (DelphiRandom(2) = 0) then
            RealMap[Loc1] := RealMap[Loc1] and not fTerrain or fForest;
      end;

    // first irrigation
    nIrrLoc := 0;
    for V21 := 1 to 26 do
      if V21 <> CityOwnTile then
      begin
        Loc1 := Radius[V21];
        if (Loc1 >= 0) and (Loc1 < MapSize) and
          (RealMap[Loc1] and (fTerrain or fSpecial) = fGrass or fSpecial1) then
        begin
          IrrLoc[nIrrLoc] := Loc1;
          Inc(nIrrLoc);
        end;
      end;
    I := 2;
    if I > nIrrLoc then
      I := nIrrLoc;
    while I > 0 do
    begin
      J := DelphiRandom(nIrrLoc);
      RealMap[IrrLoc[J]] := RealMap[IrrLoc[J]] or tiIrrigation;
      IrrLoc[J] := IrrLoc[nIrrLoc - 1];
      Dec(nIrrLoc);
      Dec(I);
    end;
  end;

  StartLoc[0] := 0;
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and GAlive <> 0 then
    begin
      repeat
        I := DelphiRandom(nAlive) + 1
      until StartLoc0[I] >= 0;
      StartLoc[p1] := StartLoc0[I];
      StartLoc0[I] := -1
    end;
  SaveMapCenterLoc := StartLoc[0];

  // second unit starting position
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and GAlive <> 0 then
    begin
      StartLoc2[p1] := StartLoc[p1];
      V8_to_Loc(StartLoc[p1], Adjacent);
      for V8 := 0 to 7 do
      begin
        Loc1 := Adjacent[V8];
        for p2 := 0 to nPl - 1 do
          if (1 shl p2 and GAlive <> 0) and (StartLoc[p2] = Loc1) then
            Loc1 := -1;
        for p2 := 0 to p1 - 1 do
          if (1 shl p2 and GAlive <> 0) and (StartLoc2[p2] = Loc1) then
            Loc1 := -1;
        if (Loc1 < 0) or (Loc1 >= MapSize) or
          (RealMap[Loc1] and fTerrain in [fOcean, fShore, fDesert, fArctic,
          fMountains]) or (RealMap[Loc1] and fDeadLands <> 0) then
          TestDist := -1
        else if RealMap[Loc1] and fTerrain = fGrass then
          TestDist := 2
        else if Terrain[RealMap[Loc1] and fTerrain].IrrEff > 0 then
          TestDist := 1
        else
          TestDist := 0;
        if (StartLoc2[p1] = StartLoc[p1]) or (TestDist > BestDist) then
        begin
          StartLoc2[p1] := Loc1;
          BestDist := TestDist;
          N := 1;
        end
        else if TestDist = BestDist then
        begin
          Inc(N);
          if DelphiRandom(N) = 0 then
            StartLoc2[p1] := Loc1;
        end;
      end;
    end;
end;

procedure PredefinedStartPositions(Human: Integer);
// use predefined nation start positions
var
  I, p1, Loc1, nAlive, nStartLoc0, nPrefStartLoc0, imax: Integer;
  StartLoc0: array [0 .. lxmax * lymax - 1] of Integer;
  ishuman: Boolean;
begin
  nAlive := 0;
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and GAlive <> 0 then
      Inc(nAlive);
  if nAlive = 0 then
    Exit;

  for I := 0 to Length(StartLoc0) - 1 do
    StartLoc0[I] := 0;

  // calculate starting positions
  nStartLoc0 := 0;
  nPrefStartLoc0 := 0;
  for Loc1 := 0 to MapSize - 1 do
    if RealMap[Loc1] and fPrefStartPos <> 0 then
    begin
      StartLoc0[nStartLoc0] := StartLoc0[nPrefStartLoc0];
      StartLoc0[nPrefStartLoc0] := Loc1;
      Inc(nPrefStartLoc0);
      Inc(nStartLoc0);
      RealMap[Loc1] := RealMap[Loc1] and not fPrefStartPos;
    end
    else if RealMap[Loc1] and fStartPos <> 0 then
    begin
      StartLoc0[nStartLoc0] := Loc1;
      Inc(nStartLoc0);
      RealMap[Loc1] := RealMap[Loc1] and not fStartPos;
    end;
  Assert(nStartLoc0 >= nAlive);

  StartLoc[0] := 0;
  for ishuman := True downto False do
    for p1 := 0 to nPl - 1 do
      if (1 shl p1 and GAlive <> 0) and ((1 shl p1 and Human <> 0) = ishuman)
      then
      begin
        Dec(nStartLoc0);
        imax := nStartLoc0;
        if nPrefStartLoc0 > 0 then
        begin
          Dec(nPrefStartLoc0);
          imax := nPrefStartLoc0;
        end;
        I := DelphiRandom(imax + 1);
        StartLoc[p1] := StartLoc0[I];
        StartLoc2[p1] := StartLoc0[I];
        StartLoc0[I] := StartLoc0[imax];
        StartLoc0[imax] := StartLoc0[nStartLoc0];
      end;
  SaveMapCenterLoc := StartLoc[0];
end;

procedure InitGame;
var
  I, P, p1, uix, Loc1: Integer;
begin
  {$IFDEF FastContact}
    { Railroad everywhere }
    for Loc1 := 0 to MapSize - 1 do
      if RealMap[Loc1] and fTerrain >= fGrass then
        RealMap[Loc1] := RealMap[Loc1] or fRR;
  {$ENDIF}

  { !!!for Loc1:=0 to MapSize-1 do
    if RealMap[Loc1] and fterrain>=fGrass then
    if Delphirandom(3)=0 then RealMap[Loc1]:=RealMap[Loc1] or fRoad
    else if Delphirandom(3)=0 then RealMap[Loc1]:=RealMap[Loc1] or fRR;
    {random Road and Railroad }
  { !!!for Loc1:=0 to MapSize-1 do
    if (RealMap[Loc1] and fterrain>=fGrass) and (Delphirandom(20)=0) then
    RealMap[Loc1]:=RealMap[Loc1] or fPoll; }

  FillChar(Occupant, MapSize, Byte(-1));
  FillChar(ZoCMap, MapSize, 0);
  FillChar(ObserveLevel, MapSize * 4, 0);
  FillChar(UsedByCity, MapSize * 4, Byte(-1));
  GTestFlags := 0;
  GInitialized := GAlive or GWatching;
  for P := 0 to nPl - 1 do
    if 1 shl P and GInitialized <> 0 then
      with RW[P] do
      begin
        Researched[P] := 0;
        Discovered[P] := 0;
        TerritoryCount[P] := 0;
        nTech[P] := 0;
        if Difficulty[P] = 0 then
          ResourceMask[P] := $FFFFFFFF
        else
          ResourceMask[P] := $FFFFFFFF and not(fSpecial2 or fModern);
        GrWallContinent[P] := -1;

        GetMem(Map, 4 * MapSize);
        GetMem(MapObservedLast, 2 * MapSize);
        FillChar(MapObservedLast^, 2 * MapSize, Byte(-1));
        GetMem(Territory, MapSize);
        FillChar(Territory^, MapSize, $FF);
        GetMem(Un, numax * SizeOf(TUn));
        GetMem(Model, (nmmax + 1) * SizeOf(TModel));
        // draft needs one model behind last
        GetMem(City, ncmax * SizeOf(TCity));
        GetMem(EnemyUn, neumax * SizeOf(TUnitInfo));
        GetMem(EnemyCity, necmax * SizeOf(TCityInfo));
        GetMem(EnemyModel, nemmax * SizeOf(TModelInfo));
        for p1 := 0 to nPl - 1 do
        begin
          if 1 shl p1 and GInitialized <> 0 then
          begin
            FillChar(RWemix[P, p1], SizeOf(RWemix[P, p1]), 255); { -1 }
            FillChar(Destroyed[P, p1], SizeOf(Destroyed[P, p1]), 0);
          end;
          Attitude[p1] := atNeutral;
          Treaty[p1] := trNoContact;
          LastCancelTreaty[p1] := -CancelTreatyTurns - 1;
          EvaStart[p1] := -PeaceEvaTurns - 1;
          Tribute[p1] := 0;
          TributePaid[p1] := 0;
          if (p1 <> P) and (1 shl p1 and GAlive <> 0) then
          begin // initialize enemy report
            GetMem(EnemyReport[p1], SizeOf(TEnemyReport) - 2 *
              (INFIN + 1 - nmmax));
            FillChar(EnemyReport[p1].Tech, nAdv, Byte(tsNA));
            EnemyReport[p1].TurnOfContact := -1;
            EnemyReport[p1].TurnOfCivilReport := -1;
            EnemyReport[p1].TurnOfMilReport := -1;
            EnemyReport[p1].Attitude := atNeutral;
            EnemyReport[p1].Government := gDespotism;
            if 1 shl P and GAlive = 0 then
              Treaty[p1] := trNone // supervisor
          end
          else
            EnemyReport[p1] := nil;
        end;
        TestFlags := GTestFlags;
        Credibility := InitialCredibility;
        MaxCredibility := 100;
        nUn := 0;
        nModel := 0;
        nCity := 0;
        nEnemyUn := 0;
        nEnemyCity := 0;
        nEnemyModel := 0;
        for Loc1 := 0 to MapSize - 1 do
          Map[Loc1] := fUNKNOWN;
        FillChar(Tech, nAdv, Byte(tsNA));
        FillChar(NatBuilt, SizeOf(NatBuilt), 0);
      end;

  // create initial models and units
  for P := 0 to nPl - 1 do
    if (1 shl P and GAlive <> 0) then
      with RW[P] do
      begin
        nModel := 0;
        for I := 0 to nSpecialModel - 1 do
          if SpecialModelPreq[I] = preNone then
          begin
            Model[nModel] := SpecialModel[I];
            Model[nModel].Status := 0;
            Model[nModel].IntroTurn := 0;
            Model[nModel].Built := 0;
            Model[nModel].Lost := 0;
            Model[nModel].ID := P shl 12 + nModel;
            SetModelFlags(Model[nModel]);
            Inc(nModel);
          end;
        nUn := 0;
        UnBuilt[P] := 0;
        for uix := 0 to nStartUn - 1 do
        begin
          CreateUnit(P, StartUn[uix]);
          Dec(Model[StartUn[uix]].Built);
          Un[uix].Loc := StartLoc2[P];
          PlaceUnit(P, uix);
        end;
        FoundCity(P, StartLoc[P]); // capital
        Founded[P] := 1;
        with City[0] do
        begin
          ID := P shl 12;
          Flags := chFounded;
        end;
      end;

  TerritoryCount[nPl] := MapSize;
  // fillchar(NewContact, sizeof(NewContact), false);
end;

procedure InitRandomGame;
begin
  DelphiRandSeed := RND;
  CalculatePrimitive;
  CreateElevation;
  CreateMap(False);
  StartPositions;
  InitGame;
end;

procedure InitMapGame(Human: Integer);
begin
  DelphiRandSeed := RND;
  FindContinents;
  PredefinedStartPositions(Human);
  InitGame;
end;

procedure ReleaseGame;
var
  p1, p2: Integer;
begin
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and GInitialized <> 0 then
    begin
      for p2 := 0 to nPl - 1 do
        if RW[p1].EnemyReport[p2] <> nil then
          FreeMem(RW[p1].EnemyReport[p2]);
      FreeMem(RW[p1].EnemyUn);
      FreeMem(RW[p1].EnemyCity);
      FreeMem(RW[p1].EnemyModel);
      FreeMem(RW[p1].Un);
      FreeMem(RW[p1].City);
      FreeMem(RW[p1].Model);
      FreeMem(RW[p1].Territory);
      FreeMem(RW[p1].MapObservedLast);
      FreeMem(RW[p1].Map);
    end;
end;

procedure InitMapEditor;
var
  p1: Integer;
begin
  CalculatePrimitive;
  FillChar(Occupant, MapSize, Byte(-1));
  FillChar(ObserveLevel, MapSize * 4, 0);
  with RW[0] do
  begin
    ResourceMask[0] := $FFFFFFFF;
    GetMem(Map, 4 * MapSize);
    GetMem(MapObservedLast, 2 * MapSize);
    FillChar(MapObservedLast^, 2 * MapSize, Byte(-1));
    GetMem(Territory, MapSize);
    FillChar(Territory^, MapSize, $FF);
    Un := nil;
    Model := nil;
    City := nil;
    EnemyUn := nil;
    EnemyCity := nil;
    EnemyModel := nil;
    for p1 := 0 to nPl - 1 do
      EnemyReport[p1] := nil;
    nUn := 0;
    nModel := 0;
    nCity := 0;
    nEnemyUn := 0;
    nEnemyCity := 0;
    nEnemyModel := 0;
  end;
end;

procedure ReleaseMapEditor;
begin
  FreeMem(RW[0].Territory);
  FreeMem(RW[0].MapObservedLast);
  FreeMem(RW[0].Map);
end;

procedure EditTile(Loc, NewTile: Integer);
var
  Loc1, V21: Integer;
  Radius: TVicinity21Loc;
begin
  if NewTile and fDeadLands <> 0 then
    NewTile := NewTile and (fDeadLands or fModern or fRiver) or fDesert;
  case NewTile and fTerrain of
    fOcean, fShore:
      NewTile := NewTile and (fTerrain or fSpecial);
    fMountains, fArctic:
      NewTile := NewTile and not fRiver;
  end;
  with Terrain[NewTile and fTerrain] do
    if (ClearTerrain >= 0) or (AfforestTerrain >= 0) or (TransTerrain >= 0) then
      NewTile := NewTile or fSpecial;
  // only automatic special resources for transformable tiles
  if NewTile and fRR <> 0 then
    NewTile := NewTile and not fRoad;
  if not((NewTile and fTerrain) in TerrType_Canalable) then
    NewTile := NewTile and not fCanal;
  if Terrain[NewTile and fTerrain].IrrEff = 0 then
  begin
    NewTile := NewTile and not(fPrefStartPos or fStartPos);
    if (NewTile and fTerImp = tiIrrigation) or (NewTile and fTerImp = tiFarm)
    then
      NewTile := NewTile and not fTerImp;
  end;
  if (Terrain[NewTile and fTerrain].MineEff = 0) and
    (NewTile and fTerImp = tiMine) then
    NewTile := NewTile and not fTerImp;

  RealMap[Loc] := NewTile;
  if NewTile and fSpecial = fSpecial then
  // standard special resource distribution
    RealMap[Loc] := RealMap[Loc] and not fSpecial or
      ActualSpecialTile(Loc) shl 5;

  // automatic shore tiles
  V21_to_Loc(Loc, Radius);
  for V21 := 1 to 26 do
  begin
    Loc1 := Radius[V21];
    if (Loc1 >= 0) and (Loc1 < MapSize) then
    begin
      if CheckShore(Loc1) then
        RealMap[Loc1] := RealMap[Loc1] and not fSpecial or
          ActualSpecialTile(Loc1) shl 5;
      RealMap[Loc1] := RealMap[Loc1] or ($F shl 27);
      RW[0].Map[Loc1] := RealMap[Loc1] and $07FFFFFF or fObserved;
    end;
  end;
  // RealMap[Loc]:=RealMap[Loc] and not fSpecial;
  // RW[0].Map[Loc]:=RealMap[Loc] or fObserved;
end;

{
  Map Revealing
  ____________________________________________________________________
}
function GetTileInfo(P, cix, Loc: Integer; var Info: TTileInfo): Integer;
// cix>=0 - known city index of player p -- only core internal!
// cix=-1 - search city, player unknown, only if permission for p
// cix=-2 - don't search city, don't calculate city benefits, just government of player p
var
  p0, Tile, special: Integer;
begin
  with Info do
  begin
    p0 := P;
    if cix >= 0 then
      Tile := RealMap[Loc]
    else
    begin
      Tile := RW[P].Map[Loc];
      if Tile and fTerrain = fUNKNOWN then
      begin
        Result := eNoPreq;
        Exit;
      end;
    end;

    if (cix = -1) and (UsedByCity[Loc] >= 0) then
    begin // search exploiting player and city
      SearchCity(UsedByCity[Loc], P, cix);
      if not((P = p0) or (ObserveLevel[UsedByCity[Loc]] shr (2 * p0) and
        3 = lObserveSuper)) then
        cix := -1
    end;
    if cix = -1 then
    begin
      Result := eInvalid;
      Exit;
    end; // no city found here

    special := Tile and fSpecial and ResourceMask[P] shr 5;
    with Terrain[Tile and fTerrain] do
    begin
      Food := FoodRes[special];
      Prod := ProdRes[special];
      Trade := TradeRes[special];
      if (special > 0) and (Tile and fTerrain <> fGrass) and
        (RW[P].NatBuilt[imSpacePort] > 0) then
      begin // GeoSat effect
        Food := 2 * Food - FoodRes[0];
        Prod := 2 * Prod - ProdRes[0];
        Trade := 2 * Trade - TradeRes[0];
      end;

      if (Tile and fTerImp = tiIrrigation) or (Tile and fTerImp = tiFarm) or
        (Tile and fCity <> 0) then
        Inc(Food, IrrEff); { irrigation effect }
      if Tile and fTerImp = tiMine then
        Inc(Prod, MineEff); { mining effect }
      if (Tile and fRiver <> 0) and (RW[P].Tech[adMapMaking] >= tsApplicable)
      then
        Inc(Trade); { river effect }
      if (Tile and (fRoad or fRR) <> 0) and (MoveCost = 1) and
        (RW[P].Tech[adWheel] >= tsApplicable) then
        Inc(Trade); { road effect }
      if (Tile and (fRR or fCity) <> 0) and
        (RW[P].Tech[adRailroad] >= tsApplicable) then
        Inc(Prod, Prod shr 1); { railroad effect }

      ExplCity := -1;
      if (cix >= 0) and (P = p0) then
        ExplCity := cix;
      if cix >= 0 then
        if Tile and fTerrain >= fGrass then
        begin
          if ((Tile and fTerImp = tiFarm) or (Tile and fCity <> 0)) and
            (RW[P].City[cix].Built[imSupermarket] > 0) then
            Inc(Food, Food shr 1); { farmland effect }
          if (Tile and (fRoad or fRR) <> 0) and (MoveCost = 1) and
            (RW[P].City[cix].Built[imHighways] > 0) then
            Inc(Trade, 1); { superhighway effect }
        end
        else
        begin
          if RW[P].City[cix].Built[imHarbor] > 0 then
            Inc(Food); { harbour effect }
          if RW[P].City[cix].Built[imPlatform] > 0 then
            Inc(Prod); { oil platform effect }
          if GWonder[woLighthouse].EffectiveOwner = P then
            Inc(Prod);
        end;
    end;

    { good government influence }
    if (RW[P].Government in [gRepublic, gDemocracy, gFuture]) and (Trade > 0)
    then
      Inc(Trade);
    if (RW[P].Government = gCommunism) and (Prod > 1) then
      Inc(Prod);

    if RW[P].Government in [gAnarchy, gDespotism] then
    begin { bad government influence }
      if Food > 3 then
        Food := 3;
      if Prod > 2 then
        Prod := 2;
      if Trade > 2 then
        Trade := 2;
    end;

    if Tile and (fTerrain or fPoll) > fPoll then
    begin { pollution - decrease ressources }
      Dec(Food, Food shr 1);
      Dec(Prod, Prod shr 1);
      Dec(Trade, Trade shr 1);
    end;

    if Tile and fCity <> 0 then
      Trade := 0
    else if (cix >= 0) and (RW[P].City[cix].Built[imCourt] + RW[P].City[cix]
      .Built[imPalace] = 0) then
      if RW[P].City[cix].Built[imTownHall] = 0 then
        Trade := 0
      else if Trade > 3 then
        Trade := 3;
  end;
  Result := eOK;
end;

procedure Strongest(Loc: Integer; var uix, Strength, Bonus, Cnt: Integer);
{ find strongest defender at Loc }
var
  Defender, uix1, Det, Cost, TestStrength, TestBonus, TestDet, TestCost,
    Domain: Integer;
  PUn: ^TUn;
  PModel: ^TModel;
begin
  Defender := Occupant[Loc];
  Cost := 0;
  Cnt := 0;
  Det := -1;
  for uix1 := 0 to RW[Defender].nUn - 1 do
  begin
    PUn := @RW[Defender].Un[uix1];
    PModel := @RW[Defender].Model[PUn.mix];
    if PModel.Kind = mkSpecial_Glider then
      Domain := dGround
    else
      Domain := PModel.Domain;
    if PUn.Loc = Loc then
    begin
      Inc(Cnt);
      if PUn.Master < 0 then
      begin
        if Domain < dSea then
        begin
          TestBonus := Terrain[RealMap[Loc] and fTerrain].Defense;
          if RealMap[Loc] and fTerImp = tiFort then
            Inc(TestBonus, 4);
          if PUn.Flags and unFortified <> 0 then
            Inc(TestBonus, 2);
          if (PModel.Kind = mkSpecial_TownGuard) and
            (RealMap[Loc] and fCity <> 0) then
            Inc(TestBonus, 4);
        end
        else
          TestBonus := 4;
        Inc(TestBonus, PUn.exp div ExpCost);
        TestStrength := PModel.Defense * TestBonus * PUn.Health;
        if (Domain = dAir) and ((RealMap[Loc] and fCity <> 0) or
          (RealMap[Loc] and fTerImp = tiBase)) then
          TestStrength := 0;
        if (Domain = dSea) and (RealMap[Loc] and fTerrain >= fGrass) then
          TestStrength := TestStrength shr 1;
        TestDet := TestStrength;
        if PModel.Cap[mcStealth] > 0 then
        else if PModel.Cap[mcSub] > 0 then
          Inc(TestDet, 1 shl 28)
        else if (Domain = dGround) and (PModel.Cap[mcFanatic] > 0) and
          not(RW[Defender].Government in [gRepublic, gDemocracy, gFuture]) then
          Inc(TestDet, 4 shl 28) // fanatic ground units always defend
        else if PModel.Flags and mdZOC <> 0 then
          Inc(TestDet, 3 shl 28)
        else
          Inc(TestDet, 2 shl 28);
        TestCost := RW[Defender].Model[PUn.mix].Cost;
        if (TestDet > Det) or (TestDet = Det) and (TestCost < Cost) then
        begin
          uix := uix1;
          Strength := TestStrength;
          Bonus := TestBonus;
          Det := TestDet;
          Cost := TestCost;
        end;
      end;
    end;
  end;
end;

function UnitSpeed(P, mix, Health: Integer): Integer;
begin
  with RW[P].Model[mix] do
  begin
    Result := Speed;
    if Domain = dSea then
    begin
      if GWonder[woMagellan].EffectiveOwner = P then
        Inc(Result, 200);
      if Health < 100 then
        Result := ((Result - 250) * Health div 5000) * 50 + 250;
    end;
  end;
end;

procedure GetUnitReport(P, uix: Integer; var UnitReport: TUnitReport);
var
  TerrOwner: Integer;
  PModel: ^TModel;
begin
  UnitReport.FoodSupport := 0;
  UnitReport.ProdSupport := 0;
  UnitReport.ReportFlags := 0;
  if RW[P].Government <> gAnarchy then
    with RW[P].Un[uix] do
    begin
      PModel := @RW[P].Model[mix];
      if (PModel.Kind = mkSettler)
      { and (GWonder[woFreeSettlers].EffectiveOwner<>p) } then
        UnitReport.FoodSupport := SettlerFood[RW[P].Government]
      else if Flags and unConscripts <> 0 then
        UnitReport.FoodSupport := 1;

      if RW[P].Government <> gFundamentalism then
      begin
        if GTestFlags and tfImmImprove = 0 then
        begin
          if PModel.Flags and mdDoubleSupport <> 0 then
            UnitReport.ProdSupport := 2
          else
            UnitReport.ProdSupport := 1;
          if PModel.Kind = mkSpecial_TownGuard then
            UnitReport.ReportFlags := UnitReport.ReportFlags or
              urfAlwaysSupport;
        end;
        if PModel.Flags and mdCivil = 0 then
        begin
          TerrOwner := RealMap[Loc] shr 27;
          case RW[P].Government of
            gRepublic, gFuture:
              if (TerrOwner <> P) and (TerrOwner < nPl) and
                (RW[P].Treaty[TerrOwner] < trAlliance) then
                UnitReport.ReportFlags := UnitReport.ReportFlags or urfDeployed;
            gDemocracy:
              if (TerrOwner >= nPl) or (TerrOwner <> P) and
                (RW[P].Treaty[TerrOwner] < trAlliance) then
                UnitReport.ReportFlags := UnitReport.ReportFlags or urfDeployed;
          end;
        end;
      end;
    end;
end;

procedure SearchCity(Loc: Integer; var P, cix: Integer);
// set p to supposed owner before call
var
  I: Integer;
begin
  if RealMap[Loc] < nPl shl 27 then
    P := RealMap[Loc] shr 27;
  for I := 0 to nPl - 1 do
  begin
    if 1 shl P and GAlive <> 0 then
      with RW[P] do
      begin
        cix := nCity - 1;
        while (cix >= 0) and (City[cix].Loc <> Loc) do
          Dec(cix);
        if cix >= 0 then
          Exit;
      end;
    Assert(I < nPl - 1);
    P := (P + 1) mod nPl;
  end;
end;

procedure MakeCityInfo(P, cix: Integer; var ci: TCityInfo);
begin
  Assert((P >= 0) and (P < nPl));
  Assert((cix >= 0) and (cix < RW[P].nCity));
  with RW[P].City[cix] do
  begin
    ci.Loc := Loc;
    ci.ID := ID;
    ci.Owner := P;
    ci.Size := Size;
    ci.Flags := 0;
    if Built[imPalace] > 0 then
      Inc(ci.Flags, ciCapital);
    if (Built[imWalls] > 0) or (Continent[Loc] = GrWallContinent[P]) then
      Inc(ci.Flags, ciWalled);
    if Built[imCoastalFort] > 0 then
      Inc(ci.Flags, ciCoastalFort);
    if Built[imMissileBat] > 0 then
      Inc(ci.Flags, ciMissileBat);
    if Built[imBunker] > 0 then
      Inc(ci.Flags, ciBunker);
    if Built[imSpacePort] > 0 then
      Inc(ci.Flags, ciSpacePort);
  end;
end;

procedure TellAboutModel(P, taOwner, tamix: Integer);
var
  I: Integer;
begin
  if (P = taOwner) or (Mode < moPlaying) then
    Exit;
  I := 0;
  while (I < RW[P].nEnemyModel) and ((RW[P].EnemyModel[I].Owner <> taOwner) or
    (RW[P].EnemyModel[I].mix <> tamix)) do
    Inc(I);
  if I = RW[P].nEnemyModel then
    IntServer(sIntTellAboutModel + P shl 4, taOwner, tamix, nil^);
end;

function emixSafe(P, taOwner, tamix: Integer): Integer;
begin
  Result := RWemix[P, taOwner, tamix];
  if Result < 0 then
  begin // sIntTellAboutModel comes too late
    Assert(Mode = moMovie);
    Result := $FFFF;
  end;
end;

procedure IntroduceEnemy(p1, p2: Integer);
begin
  RW[p1].Treaty[p2] := trNone;
  RW[p2].Treaty[p1] := trNone;
end;

function DiscoverTile(Loc, P, pTell, Level: Integer; EnableContact: Boolean;
  euix: Integer = -2): Boolean;
// euix = -2: full discover
// euix = -1: unit and city only, append units in EnemyUn
// euix >= 0: unit and city only, replace EnemyUn[euix]

  procedure SetContact(p1, p2: Integer);
  begin
    if (Mode < moPlaying) or (p1 = p2) or (RW[p1].Treaty[p2] > trNoContact) then
      Exit;
    IntServer(sIntTellAboutNation, p1, p2, nil^);
    // NewContact[p1,p2]:=true
  end;

var
  I, uix, cix, TerrOwner, TerrOwnerTreaty, Strength, Bonus, Cnt, pFoundCity,
    cixFoundCity, MinLevel, Loc1, V8: Integer;
  Tile, AddFlags: Cardinal;
  Adjacent: TVicinity8Loc;
  unx: ^TUn;
  mox: ^TModel;
begin
  Result := False;
  with RW[pTell] do
  begin
    Tile := RealMap[Loc] and ResourceMask[pTell];
    if Mode = moLoading_Fast then
      AddFlags := 0 // don't discover units
    else
    begin
      AddFlags := Map[Loc] and fInEnemyZoC // always preserve this flag!
        or fObserved;
      if Level = lObserveSuper then
        AddFlags := AddFlags or fSpiedOut;
      if (GrWallContinent[pTell] >= 0) and
        (Continent[Loc] = GrWallContinent[pTell]) then
        AddFlags := AddFlags or fGrWall;
      if (Mode = moPlaying) and ((Tile and (nPl shl 27) <> nPl shl 27) and
        (pTell = P)) then
      begin // set fPeace flag?
        TerrOwner := Tile shr 27;
        if TerrOwner <> pTell then
        begin
          TerrOwnerTreaty := RW[pTell].Treaty[TerrOwner];
          if 1 shl TerrOwnerTreaty and
            (1 shl trPeace or 1 shl TrFriendlyContact) <> 0 then
            AddFlags := AddFlags or fPeace;
        end;
      end;

      if Occupant[Loc] >= 0 then
        if Occupant[Loc] = pTell then
        begin
          AddFlags := AddFlags or (fOwned or fUnit);
          if ZoCMap[Loc] > 0 then
            AddFlags := AddFlags or fOwnZoCUnit;
          // Level:=lObserveSuper // always see own units
        end
        else if Map[Loc] and fUnit <> 0 then
          AddFlags := AddFlags or fUnit
        else
        begin
          Strongest(Loc, uix, Strength, Bonus, Cnt);
          unx := @RW[Occupant[Loc]].Un[uix];
          mox := @RW[Occupant[Loc]].Model[unx.mix];
          Assert((ZoCMap[Loc] <> 0) = (mox.Flags and mdZOC <> 0));
          if (mox.Cap[mcStealth] > 0) and (Tile and fCity = 0) and
            (Tile and fTerImp <> tiBase) then
            MinLevel := lObserveSuper
          else if (mox.Cap[mcSub] > 0) and (Tile and fTerrain < fGrass) then
            MinLevel := lObserveAll
          else
            MinLevel := lObserveUnhidden;
          if Level >= MinLevel then
          begin
            AddFlags := AddFlags or fUnit;
            if euix >= 0 then
              uix := euix
            else
            begin
              uix := nEnemyUn;
              Inc(nEnemyUn);
              Assert(nEnemyUn < neumax);
            end;
            MakeUnitInfo(Occupant[Loc], unx^, EnemyUn[uix]);
            if Cnt > 1 then
              EnemyUn[uix].Flags := EnemyUn[uix].Flags or unMulti;
            if (mox.Flags and mdZOC <> 0) and (pTell = P) and
              (Treaty[Occupant[Loc]] < trAlliance) then
            begin // set fInEnemyZoC flags of surrounding tiles
              V8_to_Loc(Loc, Adjacent);
              for V8 := 0 to 7 do
              begin
                Loc1 := Adjacent[V8];
                if (Loc1 >= 0) and (Loc1 < MapSize) then
                  Map[Loc1] := Map[Loc1] or fInEnemyZoC
              end;
            end;
            if EnableContact and (mox.Domain = dGround) then
              SetContact(pTell, Occupant[Loc]);
            if Mode >= moMovie then
            begin
              TellAboutModel(pTell, Occupant[Loc], unx.mix);
              EnemyUn[uix].emix := emixSafe(pTell, Occupant[Loc], unx.mix);
            end;
            // Level:=lObserveSuper; // don't discover unit twice
            if (pTell = P) and
              ((Tile and fCity = 0) or (1 shl pTell and GAI <> 0)) then
              Result := True;
          end
          else
            AddFlags := AddFlags or Map[Loc] and (fStealthUnit or fHiddenUnit);
        end;
    end; // if Mode>moLoading_Fast

    if Tile and fCity <> 0 then
      if ObserveLevel[Loc] shr (2 * pTell) and 3 > 0 then
        AddFlags := AddFlags or Map[Loc] and fOwned
      else
      begin
        pFoundCity := Tile shr 27;
        if pFoundCity = pTell then
          AddFlags := AddFlags or fOwned
        else
        begin
          if EnableContact then
            SetContact(pTell, pFoundCity);
          cixFoundCity := RW[pFoundCity].nCity - 1;
          while (cixFoundCity >= 0) and
            (RW[pFoundCity].City[cixFoundCity].Loc <> Loc) do
            Dec(cixFoundCity);
          Assert(cixFoundCity >= 0);
          I := 0;
          while (I < nEnemyCity) and (EnemyCity[I].Loc <> Loc) do
            Inc(I);
          if I = nEnemyCity then
          begin
            Inc(nEnemyCity);
            Assert(nEnemyCity < necmax);
            EnemyCity[I].Status := 0;
            EnemyCity[I].SavedStatus := 0;
            if pTell = P then
              Result := True;
          end;
          MakeCityInfo(pFoundCity, cixFoundCity, EnemyCity[I]);
        end;
      end
    else if Map[Loc] and fCity <> 0 then // remove enemycity
      for cix := 0 to nEnemyCity - 1 do
        if EnemyCity[cix].Loc = Loc then
          EnemyCity[cix].Loc := -1;

    if Map[Loc] and fTerrain = fUNKNOWN then
      Inc(Discovered[pTell]);
    if euix >= -1 then
      Map[Loc] := Map[Loc] and not(fUnit or fCity or fOwned or fOwnZoCUnit) or
        (Tile and $07FFFFFF or AddFlags) and
        (fUnit or fCity or fOwned or fOwnZoCUnit)
    else
    begin
      Map[Loc] := Tile and $07FFFFFF or AddFlags;
      if Tile and $78000000 = $78000000 then
        Territory[Loc] := -1
      else
        Territory[Loc] := Tile shr 27;
      MapObservedLast[Loc] := GTurn
    end;
    ObserveLevel[Loc] := ObserveLevel[Loc] and not(3 shl (2 * pTell)) or
      Cardinal(Level) shl (2 * pTell);
  end;
end;

function Discover9(Loc, P, Level: Integer;
  TellAllied, EnableContact: Boolean): Boolean;
var
  V9, Loc1, pTell, OldLevel: Integer;
  Radius: TVicinity8Loc;
begin
  Assert((Mode > moLoading_Fast) or (RW[P].nEnemyUn = 0));
  Result := False;
  V8_to_Loc(Loc, Radius);
  for V9 := 0 to 8 do
  begin
    if V9 = 8 then
      Loc1 := Loc
    else
      Loc1 := Radius[V9];
    if (Loc1 >= 0) and (Loc1 < MapSize) then
      if TellAllied then
      begin
        for pTell := 0 to nPl - 1 do
          if (pTell = P) or (1 shl pTell and GAlive <> 0) and
            (RW[P].Treaty[pTell] = trAlliance) then
          begin
            OldLevel := ObserveLevel[Loc1] shr (2 * pTell) and 3;
            if Level > OldLevel then
              Result := DiscoverTile(Loc1, P, pTell, Level, EnableContact)
                or Result;
          end;
      end
      else
      begin
        OldLevel := ObserveLevel[Loc1] shr (2 * P) and 3;
        if Level > OldLevel then
          Result := DiscoverTile(Loc1, P, P, Level, EnableContact) or Result;
      end;
  end;
end;

function Discover21(Loc, P, AdjacentLevel: Integer;
  TellAllied, EnableContact: Boolean): Boolean;
var
  V21, Loc1, pTell, Level, OldLevel, AdjacentFlags: Integer;
  Radius: TVicinity21Loc;
begin
  Assert((Mode > moLoading_Fast) or (RW[P].nEnemyUn = 0));
  Result := False;
  AdjacentFlags := $00267620 shr 1;
  V21_to_Loc(Loc, Radius);
  for V21 := 1 to 26 do
  begin
    Loc1 := Radius[V21];
    if (Loc1 >= 0) and (Loc1 < MapSize) then
    begin
      if AdjacentFlags and 1 <> 0 then
        Level := AdjacentLevel
      else
        Level := lObserveUnhidden;
      if TellAllied then
      begin
        for pTell := 0 to nPl - 1 do
          if (pTell = P) or (1 shl pTell and GAlive <> 0) and
            (RW[P].Treaty[pTell] = trAlliance) then
          begin
            OldLevel := ObserveLevel[Loc1] shr (2 * pTell) and 3;
            if Level > OldLevel then
              Result := DiscoverTile(Loc1, P, pTell, Level, EnableContact)
                or Result;
          end;
      end
      else
      begin
        OldLevel := ObserveLevel[Loc1] shr (2 * P) and 3;
        if Level > OldLevel then
          Result := DiscoverTile(Loc1, P, P, Level, EnableContact) or Result;
      end;
    end;
    AdjacentFlags := AdjacentFlags shr 1;
  end;
end;

procedure DiscoverAll(P, Level: Integer);
{ player p discovers complete playground (for supervisor) }
var
  Loc, OldLevel: Integer;
begin
  Assert((Mode > moLoading_Fast) or (RW[P].nEnemyUn = 0));
  for Loc := 0 to MapSize - 1 do
  begin
    OldLevel := ObserveLevel[Loc] shr (2 * P) and 3;
    if Level > OldLevel then
      DiscoverTile(Loc, P, P, Level, False);
  end;
end;

procedure DiscoverViewAreas(P: Integer);
var
  pTell, uix, cix, ecix, Loc, RealOwner: Integer;
  PModel: ^TModel;
begin // discover unit and city view areas
  for pTell := 0 to nPl - 1 do
    if (pTell = P) or (RW[P].Treaty[pTell] = trAlliance) then
    begin
      for uix := 0 to RW[pTell].nUn - 1 do
        with RW[pTell].Un[uix] do
          if (Loc >= 0) and (Master < 0) and (RealMap[Loc] and fCity = 0) then
          begin
            PModel := @RW[pTell].Model[mix];
            if (PModel.Kind = mkDiplomat) or (PModel.Cap[mcSpy] > 0) then
              Discover21(Loc, P, lObserveSuper, False, True)
            else if (PModel.Cap[mcRadar] + PModel.Cap[mcCarrier] > 0) or
              (PModel.Domain = dAir) then
              Discover21(Loc, P, lObserveAll, False, False)
            else if (RealMap[Loc] and fTerrain = fMountains) or
              (RealMap[Loc] and fTerImp = tiFort) or
              (RealMap[Loc] and fTerImp = tiBase) or (PModel.Cap[mcAcademy] > 0)
            then
              Discover21(Loc, P, lObserveUnhidden, False,
                PModel.Domain = dGround)
            else
              Discover9(Loc, P, lObserveUnhidden, False,
                PModel.Domain = dGround);
          end;
      for cix := 0 to RW[pTell].nCity - 1 do
        if RW[pTell].City[cix].Loc >= 0 then
          Discover21(RW[pTell].City[cix].Loc, P, lObserveUnhidden, False, True);
      for ecix := 0 to RW[pTell].nEnemyCity - 1 do
      begin // players know territory, so no use in hiding city owner
        Loc := RW[pTell].EnemyCity[ecix].Loc;
        if Loc >= 0 then
        begin
          RealOwner := (RealMap[Loc] shr 27) and $F;
          if RealOwner < nPl then
            RW[pTell].EnemyCity[ecix].Owner := RealOwner
          else
          begin
            RW[pTell].EnemyCity[ecix].Loc := -1;
            RW[pTell].Map[Loc] := RW[pTell].Map[Loc] and not fCity;
          end;
        end;
      end;
    end;
end;

function GetUnitStack(P, Loc: Integer): Integer;
var
  uix: Integer;
  unx: ^TUn;
begin
  Result := 0;
  if Occupant[Loc] < 0 then
    Exit;
  for uix := 0 to RW[Occupant[Loc]].nUn - 1 do
  begin
    unx := @RW[Occupant[Loc]].Un[uix];
    if unx.Loc = Loc then
    begin
      MakeUnitInfo(Occupant[Loc], unx^, RW[P].EnemyUn[RW[P].nEnemyUn + Result]);
      TellAboutModel(P, Occupant[Loc], unx.mix);
      RW[P].EnemyUn[RW[P].nEnemyUn + Result].emix :=
        RWemix[P, Occupant[Loc], unx.mix];
      Inc(Result);
    end;
  end;
end;

procedure UpdateUnitMap(Loc: Integer; CityChange: Boolean = False);
// update maps and enemy units of all players after unit change
var
  P, euix, OldLevel: Integer;
  AddFlags, ClearFlags: Cardinal;
begin
  if (Mode = moLoading_Fast) and not CityChange then
    Exit;
  for P := 0 to nPl - 1 do
    if 1 shl P and (GAlive or GWatching) <> 0 then
    begin
      OldLevel := ObserveLevel[Loc] shr (2 * P) and 3;
      if OldLevel > lNoObserve then
      begin
        if RW[P].Map[Loc] and (fUnit or fOwned) = fUnit then
        begin
          // replace unit located here in EnemyUn
          // do not just set loc:=-1 because total number would be unlimited
          euix := RW[P].nEnemyUn - 1;
          while euix >= 0 do
          begin
            if RW[P].EnemyUn[euix].Loc = Loc then
            begin
              RW[P].EnemyUn[euix].Loc := -1;
              Break;
            end;
            Dec(euix);
          end;
          RW[P].Map[Loc] := RW[P].Map[Loc] and not fUnit
        end
        else
        begin // look for empty slot in EnemyUn
          euix := RW[P].nEnemyUn - 1;
          while (euix >= 0) and (RW[P].EnemyUn[euix].Loc >= 0) do
            Dec(euix);
        end;
        if (Occupant[Loc] < 0) and not CityChange then
        begin // calling DiscoverTile not necessary, only clear map flags
          ClearFlags := fUnit or fHiddenUnit or fStealthUnit or fOwnZoCUnit;
          if RealMap[Loc] and fCity = 0 then
            ClearFlags := ClearFlags or fOwned;
          RW[P].Map[Loc] := RW[P].Map[Loc] and not ClearFlags;
        end
        else if (Occupant[Loc] <> P) or CityChange then
        begin // city or enemy unit update necessary, call DiscoverTile
          ObserveLevel[Loc] := ObserveLevel[Loc] and not(3 shl (2 * P));
          DiscoverTile(Loc, P, P, OldLevel, False, euix);
        end
        else { if (Occupant[Loc]=p) and not CityChange then }
        begin // calling DiscoverTile not necessary, only set map flags
          ClearFlags := 0;
          AddFlags := fUnit or fOwned;
          if ZoCMap[Loc] > 0 then
            AddFlags := AddFlags or fOwnZoCUnit
          else
            ClearFlags := ClearFlags or fOwnZoCUnit;
          RW[P].Map[Loc] := RW[P].Map[Loc] and not ClearFlags or AddFlags;
        end;
      end;
    end;
end;

procedure RecalcV8ZoC(P, Loc: Integer);
// recalculate fInEnemyZoC flags around single tile
var
  V8, V8V8, Loc1, Loc2, p1, ObserveMask: Integer;
  Tile1: ^Cardinal;
  Adjacent, AdjacentAdjacent: TVicinity8Loc;
begin
  if Mode = moLoading_Fast then
    Exit;
  ObserveMask := 3 shl (2 * P);
  V8_to_Loc(Loc, Adjacent);
  for V8 := 0 to 7 do
  begin
    Loc1 := Adjacent[V8];
    if (Loc1 >= 0) and (Loc1 < MapSize) then
    begin
      Tile1 := @RW[P].Map[Loc1];
      Tile1^ := Tile1^ and not fInEnemyZoC;
      V8_to_Loc(Loc1, AdjacentAdjacent);
      for V8V8 := 0 to 7 do
      begin
        Loc2 := AdjacentAdjacent[V8V8];
        if (Loc2 >= 0) and (Loc2 < MapSize) and (ZoCMap[Loc2] > 0) and
          (ObserveLevel[Loc2] and ObserveMask <> 0) then
        begin
          p1 := Occupant[Loc2];
          Assert(p1 <> nPl);
          if (p1 <> P) and (RW[P].Treaty[p1] < trAlliance) then
          begin
            Tile1^ := Tile1^ or fInEnemyZoC;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure RecalcMapZoC(P: Integer);
// recalculate fInEnemyZoC flags for the whole map
var
  Loc, Loc1, V8, p1, ObserveMask: Integer;
  Adjacent: TVicinity8Loc;
begin
  if Mode = moLoading_Fast then
    Exit;
  MaskD(RW[P].Map^, MapSize, Cardinal(not Cardinal(fInEnemyZoC)));
  ObserveMask := 3 shl (2 * P);
  for Loc := 0 to MapSize - 1 do
    if (ZoCMap[Loc] > 0) and (ObserveLevel[Loc] and ObserveMask <> 0) then
    begin
      p1 := Occupant[Loc];
      Assert(p1 <> nPl);
      if (p1 <> P) and (RW[P].Treaty[p1] < trAlliance) then
      begin // this non-allied enemy ZoC unit is known to this player -- set flags!
        V8_to_Loc(Loc, Adjacent);
        for V8 := 0 to 7 do
        begin
          Loc1 := Adjacent[V8];
          if (Loc1 >= 0) and (Loc1 < MapSize) then
            RW[P].Map[Loc1] := RW[P].Map[Loc1] or fInEnemyZoC;
        end;
      end;
    end;
end;

procedure RecalcPeaceMap(P: Integer);
// recalculate fPeace flags for the whole map
var
  Loc, p1: Integer;
  PeacePlayer: array [-1 .. nPl - 1] of Boolean;
begin
  if Mode <> moPlaying then
    Exit;
  MaskD(RW[P].Map^, MapSize, Cardinal(not Cardinal(fPeace)));
  for p1 := -1 to nPl - 1 do
    PeacePlayer[p1] := (p1 >= 0) and (p1 <> P) and (1 shl p1 and GAlive <> 0)
      and (RW[P].Treaty[p1] in [trPeace, TrFriendlyContact]);
  for Loc := 0 to MapSize - 1 do
    if PeacePlayer[RW[P].Territory[Loc]] then
      RW[P].Map[Loc] := RW[P].Map[Loc] or fPeace;
end;

{
  Territory Calculation
  ____________________________________________________________________
}
var
  BorderChanges: array [0 .. sIntExpandTerritory and $F - 1] of Cardinal;

procedure ChangeTerritory(Loc, P: Integer);
var
  p1: Integer;
begin
  Assert(P >= 0); // no player's territory indicated by p=nPl
  Dec(TerritoryCount[RealMap[Loc] shr 27]);
  Inc(TerritoryCount[P]);
  RealMap[Loc] := RealMap[Loc] and not($F shl 27) or Cardinal(P) shl 27;
  if P = $F then
    P := -1;
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and (GAlive or GWatching) <> 0 then
      if RW[p1].Map[Loc] and fTerrain <> fUNKNOWN then
      begin
        RW[p1].Territory[Loc] := P;
        if (P < nPl) and (P <> p1) and (1 shl P and GAlive <> 0) and
          (RW[p1].Treaty[P] in [trPeace, TrFriendlyContact]) then
          RW[p1].Map[Loc] := RW[p1].Map[Loc] or fPeace
        else
          RW[p1].Map[Loc] := RW[p1].Map[Loc] and not fPeace;
      end;
end;

procedure ExpandTerritory(OriginLoc: Integer);
var
  I, dx, dy, dxMax, dyMax, Loc, NewOwner: Integer;
begin
  if OriginLoc = -1 then
    raise Exception.Create('Location error');
  I := 0;
  dyMax := 0;
  while (dyMax + 1) + (dyMax + 1) shr 1 <= CountryRadius do
    Inc(dyMax);
  for dy := -dyMax to dyMax do
  begin
    dxMax := dy and 1;
    while abs(dy) + (dxMax + 2) + abs(abs(dy) - (dxMax + 2)) shr 1 <=
      CountryRadius do
      Inc(dxMax, 2);
    for dx := -dxMax to dxMax do
      if (dy + dx) and 1 = 0 then
      begin
        NewOwner := BorderChanges[I div 8] shr (I mod 8 * 4) and $F;
        Loc := dLoc(OriginLoc, dx, dy);
        if (Loc >= 0) and (Cardinal(NewOwner) <> RealMap[Loc] shr 27) then
          ChangeTerritory(Loc, NewOwner);
        Inc(I);
      end;
  end;
end;

procedure CheckBorders(OriginLoc, PlayerLosingCity: Integer);
// OriginLoc: only changes in CountryRadius around this location possible,
// -1 for complete map, -2 for double-check (no more changes allowed)
// PlayerLosingCity: do nothing but remove tiles no longer in reach from this
// player's territory, -1 for full border recalculation
var
  I, R, Loc, Loc1, dx, dy, p1, p2, cix, NewDist, dxMax, dyMax, OldOwner, V8: Integer;
  NewOwner: Cardinal;
  Adjacent: TVicinity8Loc;
  AtPeace: array [0 .. nPl, 0 .. nPl] of Boolean;
  Country, FormerCountry, { to who's country a tile belongs }
  Dist, FormerDist, StolenDist: array [0 .. lxmax * lymax - 1] of ShortInt;
begin
  if PlayerLosingCity >= 0 then
  begin
    for Loc := 0 to MapSize - 1 do
      StolenDist[Loc] := CountryRadius + 1;
    for cix := 0 to RW[PlayerLosingCity].nCity - 1 do
      if RW[PlayerLosingCity].City[cix].Loc >= 0 then
        StolenDist[RW[PlayerLosingCity].City[cix].Loc] := 0;

    for R := 1 to CountryRadius shr 1 do
    begin
      Move(StolenDist, FormerDist, MapSize);
      for Loc := 0 to MapSize - 1 do
        if (FormerDist[Loc] <= CountryRadius - 2)
        // use same conditions as below!
          and ((1 shl (RealMap[Loc] and fTerrain)) and
          (1 shl fShore + 1 shl fMountains + 1 shl fArctic) = 0) then
        begin
          V8_to_Loc(Loc, Adjacent);
          for V8 := 0 to 7 do
          begin
            Loc1 := Adjacent[V8];
            NewDist := FormerDist[Loc] + 2 + V8 and 1;
            if (Loc1 >= 0) and (Loc1 < MapSize) and (NewDist < StolenDist[Loc1])
            then
              StolenDist[Loc1] := NewDist;
          end;
        end;
    end;
  end;

  FillChar(Country, MapSize, Byte(-1));
  for Loc := 0 to MapSize - 1 do
    Dist[Loc] := CountryRadius + 1;
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and GAlive <> 0 then
      for cix := 0 to RW[p1].nCity - 1 do
        if RW[p1].City[cix].Loc >= 0 then
        begin
          Country[RW[p1].City[cix].Loc] := p1;
          Dist[RW[p1].City[cix].Loc] := 0;
        end;

  for R := 1 to CountryRadius shr 1 do
  begin
    Move(Country, FormerCountry, MapSize);
    Move(Dist, FormerDist, MapSize);
    for Loc := 0 to MapSize - 1 do
      if (FormerDist[Loc] <= CountryRadius - 2) // use same conditions as above!
        and ((1 shl (RealMap[Loc] and fTerrain)) and
        (1 shl fShore + 1 shl fMountains + 1 shl fArctic) = 0) then
      begin
        Assert(FormerCountry[Loc] >= 0);
        V8_to_Loc(Loc, Adjacent);
        for V8 := 0 to 7 do
        begin
          Loc1 := Adjacent[V8];
          NewDist := FormerDist[Loc] + 2 + V8 and 1;
          if (Loc1 >= 0) and (Loc1 < MapSize) and (NewDist < Dist[Loc1]) then
          begin
            Country[Loc1] := FormerCountry[Loc];
            Dist[Loc1] := NewDist;
          end;
        end;
      end;
  end;

  FillChar(AtPeace, SizeOf(AtPeace), False);
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and GAlive <> 0 then
      for p2 := 0 to nPl - 1 do
        if (p2 <> p1) and (1 shl p2 and GAlive <> 0) and
          (RW[p1].Treaty[p2] >= trPeace) then
          AtPeace[p1, p2] := True;

  if OriginLoc >= 0 then
  begin // update area only
    I := 0;
    FillChar(BorderChanges, SizeOf(BorderChanges), 0);
    dyMax := 0;
    while (dyMax + 1) + (dyMax + 1) shr 1 <= CountryRadius do
      Inc(dyMax);
    for dy := -dyMax to dyMax do
    begin
      dxMax := dy and 1;
      while abs(dy) + (dxMax + 2) + abs(abs(dy) - (dxMax + 2)) shr 1 <=
        CountryRadius do
        Inc(dxMax, 2);
      for dx := -dxMax to dxMax do
        if (dy + dx) and 1 = 0 then
        begin
          Loc := dLoc(OriginLoc, dx, dy);
          if Loc >= 0 then
          begin
            OldOwner := RealMap[Loc] shr 27;
            NewOwner := Country[Loc] and $F;
            if NewOwner <> OldOwner then
              if AtPeace[NewOwner, OldOwner] and
                not((OldOwner = PlayerLosingCity) and
                (StolenDist[Loc] > CountryRadius)) then
                NewOwner := OldOwner // peace fixes borders
              else
                ChangeTerritory(Loc, NewOwner);
            BorderChanges[I shr 3] := BorderChanges[I shr 3] or
              ((NewOwner shl ((I and 7) * 4)) and $ffffffff);
          end;
          Inc(I);
        end;
    end;
  end
  else
    for Loc := 0 to MapSize - 1 do // update complete map
    begin
      OldOwner := RealMap[Loc] shr 27;
      NewOwner := Country[Loc] and $F;
      if (NewOwner <> OldOwner) and (not AtPeace[NewOwner, OldOwner] or
        ((OldOwner = PlayerLosingCity) and (StolenDist[Loc] > CountryRadius)))
      then
      begin
        Assert(OriginLoc <> -2); // test if border saving works
        ChangeTerritory(Loc, NewOwner);
      end;
    end;

{$IFOPT O-} if OriginLoc <> -2 then
    CheckBorders(-2); {$ENDIF} // check: single pass should do!
end;

procedure LogCheckBorders(P, cix, PlayerLosingCity: Integer);
begin
  CheckBorders(RW[P].City[cix].Loc, PlayerLosingCity);
  IntServer(sIntExpandTerritory, P, cix, BorderChanges);
end;

{
  Map Processing
  ____________________________________________________________________
}

procedure CreateUnit(P, mix: Integer);
begin
  with RW[P] do
  begin
    Un[nUn].mix := mix;
    with Un[nUn] do
    begin
      ID := UnBuilt[P];
      Inc(UnBuilt[P]);
      Status := 0;
      SavedStatus := 0;
      Inc(Model[mix].Built);
      Home := -1;
      Health := 100;
      Flags := 0;
      Movement := 0;
      if Model[mix].Domain = dAir then
      begin
        Fuel := Model[mix].Cap[mcFuel];
        Flags := Flags or unBombsLoaded;
      end;
      Job := jNone;
      exp := ExpCost shr 1;
      TroopLoad := 0;
      AirLoad := 0;
      Master := -1;
    end;
    Inc(nUn);
  end
end;

procedure FreeUnit(P, uix: Integer);
// loc or master should be set after call
// implementation is critical for loading performance, change carefully
var
  Loc0, uix1: Integer;
  Occ, ZoC: Boolean;
begin
  with RW[P].Un[uix] do
  begin
    Job := jNone;
    Flags := Flags and not(unFortified or unMountainDelay);
    Loc0 := Loc;
  end;
  if Occupant[Loc0] >= 0 then
  begin
    Assert(Occupant[Loc0] = P);
    Occ := False;
    ZoC := False;
    for uix1 := 0 to RW[P].nUn - 1 do
      with RW[P].Un[uix1] do
        if (Loc = Loc0) and (Master < 0) and (uix1 <> uix) then
        begin
          Occ := True;
          if RW[P].Model[mix].Flags and mdZOC <> 0 then
          begin
            ZoC := True;
            Break;
          end;
        end;
    if not Occ then
      Occupant[Loc0] := -1;
    if not ZoC then
      ZoCMap[Loc0] := 0;
  end;
end;

procedure PlaceUnit(P, uix: Integer);
begin
  with RW[P].Un[uix] do
  begin
    Occupant[Loc] := P;
    if RW[P].Model[mix].Flags and mdZOC <> 0 then
      ZoCMap[Loc] := 1;
  end;
end;

procedure CountLost(P, mix, Enemy: Integer);
begin
  Inc(RW[P].Model[mix].Lost);
  TellAboutModel(Enemy, P, mix);
  Inc(Destroyed[Enemy, P, mix]);
end;

procedure RemoveUnit(P, uix: Integer; Enemy: Integer = -1);
// use enemy only from inside sMoveUnit if attack
var
  uix1: Integer;
begin
  with RW[P].Un[uix] do
  begin
    Assert((Loc >= 0) or (RW[P].Model[mix].Kind = mkDiplomat));
    // already freed when spy mission
    if Loc >= 0 then
      FreeUnit(P, uix);
    if Master >= 0 then
      if RW[P].Model[mix].Domain = dAir then
        Dec(RW[P].Un[Master].AirLoad)
      else
        Dec(RW[P].Un[Master].TroopLoad);
    if (TroopLoad > 0) or (AirLoad > 0) then
      for uix1 := 0 to RW[P].nUn - 1 do
        if (RW[P].Un[uix1].Loc >= 0) and (RW[P].Un[uix1].Master = uix) then
        { unit mastered by removed unit -- remove too }
        begin
          RW[P].Un[uix1].Loc := -1;
          if Enemy >= 0 then
            CountLost(P, RW[P].Un[uix1].mix, Enemy);
        end;
    Loc := -1;
    if Enemy >= 0 then
      CountLost(P, mix, Enemy);
  end;
end;

procedure RemoveUnit_UpdateMap(P, uix: Integer);
var
  Loc0: Integer;
begin
  Loc0 := RW[P].Un[uix].Loc;
  RemoveUnit(P, uix);
  if Mode > moLoading_Fast then
    UpdateUnitMap(Loc0);
end;

procedure RemoveAllUnits(P, Loc: Integer; Enemy: Integer = -1);
var
  uix: Integer;
begin
  for uix := 0 to RW[P].nUn - 1 do
    if RW[P].Un[uix].Loc = Loc then
    begin
      if Enemy >= 0 then
        CountLost(P, RW[P].Un[uix].mix, Enemy);
      RW[P].Un[uix].Loc := -1;
    end;
  Occupant[Loc] := -1;
  ZoCMap[Loc] := 0;
end;

procedure RemoveDomainUnits(D, P, Loc: Integer);
var
  uix: Integer;
begin
  for uix := 0 to RW[P].nUn - 1 do
    if (RW[P].Model[RW[P].Un[uix].mix].Domain = D) and (RW[P].Un[uix].Loc = Loc)
    then
      RemoveUnit(P, uix);
end;

procedure FoundCity(P, FoundLoc: Integer);
var
  p1, cix1, V21, dx, dy: Integer;
begin
  if RW[P].nCity = ncmax then
    Exit;
  Inc(RW[P].nCity);
  with RW[P].City[RW[P].nCity - 1] do
  begin
    Size := 2;
    Status := 0;
    SavedStatus := 0;
    FillChar(Built, SizeOf(Built), 0);
    Food := 0;
    Project := cpImp + imTrGoods;
    Prod := 0;
    Project0 := Project;
    Prod0 := 0;
    Pollution := 0;
    N1 := 0;
    Loc := FoundLoc;
    if UsedByCity[FoundLoc] >= 0 then
    begin { central tile is exploited - toggle in exploiting city }
      p1 := P;
      SearchCity(UsedByCity[FoundLoc], p1, cix1);
      dxdy(UsedByCity[FoundLoc], FoundLoc, dx, dy);
      V21 := (dy + 3) shl 2 + (dx + 3) shr 1;
      RW[p1].City[cix1].Tiles := RW[p1].City[cix1].Tiles and not(1 shl V21);
    end;
    Tiles := 1 shl 13; { exploit central tile }
    UsedByCity[FoundLoc] := FoundLoc;
    RealMap[FoundLoc] := RealMap[FoundLoc] and
      (fTerrain or fSpecial or fRiver or nPl shl 27) or fCity;

    ChangeTerritory(Loc, P);
  end;
end;

procedure StealCity(P, cix: Integer; SaveUnits: Boolean);
var
  I, J, uix1, cix1, nearest: Integer;
begin
  for I := 0 to nWonder - 1 do
    if RW[P].City[cix].Built[I] = 1 then
    begin
      GWonder[I].EffectiveOwner := -1;
      if I = woPyramids then
        FreeSlaves;
      if I = woEiffel then // deactivate expired wonders
        for J := 0 to nWonder - 1 do
          if GWonder[J].EffectiveOwner = P then
            CheckExpiration(J);
    end;
  for I := nWonder to nImp - 1 do
    if (Imp[I].Kind <> ikCommon) and (RW[P].City[cix].Built[I] > 0) then
    begin { destroy national projects }
      RW[P].NatBuilt[I] := 0;
      if I = imGrWall then
        GrWallContinent[P] := -1;
    end;

  for uix1 := 0 to RW[P].nUn - 1 do
    with RW[P].Un[uix1] do
      if (Loc >= 0) and (Home = cix) then
        if SaveUnits then
        begin // support units by nearest other city
          nearest := -1;
          for cix1 := 0 to RW[P].nCity - 1 do
            if (cix1 <> cix) and (RW[P].City[cix1].Loc >= 0) and
              ((nearest < 0) or (Distance(RW[P].City[cix1].Loc, Loc) <
              Distance(RW[P].City[nearest].Loc, Loc))) then
              nearest := cix1;
          Home := nearest;
        end
        else
          RemoveUnit(P, uix1); // destroy supported units
end;

procedure DestroyCity(P, cix: Integer; SaveUnits: Boolean);
var
  I, V21: Integer;
  Radius: TVicinity21Loc;
begin
  StealCity(P, cix, SaveUnits);
  with RW[P].City[cix] do begin
    for I := 0 to nWonder - 1 do
      if Built[I] > 0 then
        GWonder[I].CityID := WonderDestroyed;
    V21_to_Loc(Loc, Radius);
    for V21 := 1 to 26 do
      if 1 shl V21 and Tiles <> 0 then
        UsedByCity[Radius[V21]] := -1;
    RealMap[Loc] := RealMap[Loc] and not fCity;
    Loc := -1;
  end;
end;

procedure ChangeCityOwner(pOld, cixOld, pNew: Integer);
var
  I, J, cix1, Loc1, V21: Integer;
  Radius: TVicinity21Loc;
begin
  Inc(RW[pNew].nCity);
  RW[pNew].City[RW[pNew].nCity - 1] := RW[pOld].City[cixOld];
  StealCity(pOld, cixOld, False);
  RW[pOld].City[cixOld].Loc := -1;
  with RW[pNew].City[(RW[pNew].nCity - 1)] do
  begin
    Food := 0;
    Project := cpImp + imTrGoods;
    Prod := 0;
    Project0 := Project;
    Prod0 := 0;
    Status := 0;
    SavedStatus := 0;
    N1 := 0;

    // check for siege
    V21_to_Loc(Loc, Radius);
    for V21 := 1 to 26 do
      if Tiles and (1 shl V21) and not(1 shl CityOwnTile) <> 0 then
      begin
        Loc1 := Radius[V21];
        Assert((Loc1 >= 0) and (Loc1 < MapSize) and (UsedByCity[Loc1] = Loc));
        if (ZoCMap[Loc1] > 0) and (Occupant[Loc1] <> pNew) and
          (RW[pNew].Treaty[Occupant[Loc1]] < trAlliance) then
        begin // tile can't remain exploited
          Tiles := Tiles and not(1 shl V21);
          UsedByCity[Loc1] := -1;
        end;
        // don't check for siege by peace territory here, because territory
        // might not be up to date -- done in turn beginning anyway
      end;
    Built[imTownHall] := 0;
    Built[imCourt] := 0;
    for I := nWonder to nImp - 1 do
      if Imp[I].Kind <> ikCommon then
        Built[I] := 0; { destroy national projects }
    for I := 0 to nWonder - 1 do
      if Built[I] = 1 then
      begin // new wonder owner!
        GWonder[I].EffectiveOwner := pNew;
        if I = woEiffel then // reactivate expired wonders
        begin
          for J := 0 to nWonder - 1 do
            if Imp[J].Expiration >= 0 then
              for cix1 := 0 to (RW[pNew].nCity - 1) do
                if RW[pNew].City[cix1].Built[J] = 1 then
                  GWonder[J].EffectiveOwner := pNew;
        end
        else
          CheckExpiration(I);
        case I of
          woLighthouse:
            CheckSpecialModels(pNew, preLighthouse);
          woLeo:
            CheckSpecialModels(pNew, preLeo);
          woPyramids:
            CheckSpecialModels(pNew, preBuilder);
        end;
      end;

    // remove city from enemy cities
    // not done by Discover, because fCity still set!
    cix1 := RW[pNew].nEnemyCity - 1;
    while (cix1 >= 0) and (RW[pNew].EnemyCity[cix1].Loc <> Loc) do
      Dec(cix1);
    Assert(cix1 >= 0);
    RW[pNew].EnemyCity[cix1].Loc := -1;

    ChangeTerritory(Loc, pNew);
  end;
end;

procedure CompleteJob(P, Loc, Job: Integer);
var
  ChangedTerrain, p1: Integer;
begin
  Assert(Job <> jCity);
  ChangedTerrain := -1;
  case Job of
    jRoad:
      RealMap[Loc] := RealMap[Loc] or fRoad;
    jRR:
      RealMap[Loc] := RealMap[Loc] and not fRoad or fRR;
    jClear:
      begin
        ChangedTerrain := Terrain[RealMap[Loc] and fTerrain].ClearTerrain;
        RealMap[Loc] := RealMap[Loc] and not fTerrain or
          Cardinal(ChangedTerrain);
        RealMap[Loc] := RealMap[Loc] and not(3 shl 5) or
          ActualSpecialTile(Loc) shl 5;
      end;
    jIrr:
      RealMap[Loc] := RealMap[Loc] and not fTerImp or tiIrrigation;
    jFarm:
      RealMap[Loc] := RealMap[Loc] and not fTerImp or tiFarm;
    jAfforest:
      begin
        ChangedTerrain := Terrain[RealMap[Loc] and fTerrain].AfforestTerrain;
        RealMap[Loc] := RealMap[Loc] and not fTerrain or
          Cardinal(ChangedTerrain);
        RealMap[Loc] := RealMap[Loc] and not(3 shl 5) or
          ActualSpecialTile(Loc) shl 5;
      end;
    jMine:
      RealMap[Loc] := RealMap[Loc] and not fTerImp or tiMine;
    jFort:
      RealMap[Loc] := RealMap[Loc] and not fTerImp or tiFort;
    jCanal:
      RealMap[Loc] := RealMap[Loc] or fCanal;
    jTrans:
      begin
        ChangedTerrain := Terrain[RealMap[Loc] and fTerrain].TransTerrain;
        RealMap[Loc] := RealMap[Loc] and not fTerrain or
          Cardinal(ChangedTerrain);
        RealMap[Loc] := RealMap[Loc] and not(3 shl 5) or
          ActualSpecialTile(Loc) shl 5;
        if not(RealMap[Loc] and fTerrain in TerrType_Canalable) then
        begin
          RemoveDomainUnits(dSea, P, Loc);
          RealMap[Loc] := RealMap[Loc] and not fCanal;
        end;
      end;
    jPoll:
      RealMap[Loc] := RealMap[Loc] and not fPoll;
    jBase:
      RealMap[Loc] := RealMap[Loc] and not fTerImp or tiBase;
    jPillage:
      if RealMap[Loc] and fTerImp <> 0 then
      begin
        if RealMap[Loc] and fTerImp = tiBase then
          RemoveDomainUnits(dAir, P, Loc);
        RealMap[Loc] := RealMap[Loc] and not fTerImp
      end
      else if RealMap[Loc] and fCanal <> 0 then
      begin
        RemoveDomainUnits(dSea, P, Loc);
        RealMap[Loc] := RealMap[Loc] and not fCanal
      end
      else if RealMap[Loc] and fRR <> 0 then
        RealMap[Loc] := RealMap[Loc] and not fRR or fRoad
      else if RealMap[Loc] and fRoad <> 0 then
        RealMap[Loc] := RealMap[Loc] and not fRoad;
  end;
  if ChangedTerrain >= 0 then
  begin // remove terrain improvements if not possible on new terrain
    if ((RealMap[Loc] and fTerImp = tiIrrigation) or
      (RealMap[Loc] and fTerImp = tiFarm)) and
      ((Terrain[ChangedTerrain].IrrClearWork = 0) or
      (Terrain[ChangedTerrain].ClearTerrain >= 0)) then
      RealMap[Loc] := RealMap[Loc] and not fTerImp;
    if (RealMap[Loc] and fTerImp = tiMine) and
      ((Terrain[ChangedTerrain].MineAfforestWork = 0) or
      (Terrain[ChangedTerrain].AfforestTerrain >= 0)) then
      RealMap[Loc] := RealMap[Loc] and not fTerImp;
  end;

  // update map of all observing players
  if Mode > moLoading_Fast then
    for p1 := 0 to nPl - 1 do
      if (1 shl p1 and (GAlive or GWatching) <> 0) and
        (ObserveLevel[Loc] shr (2 * p1) and 3 > lNoObserve) then
        RW[p1].Map[Loc] := RW[p1].Map[Loc] and
          not(fTerrain or fSpecial or fTerImp or fRoad or fRR or fCanal or
          fPoll) or RealMap[Loc] and (fTerrain or fSpecial or fTerImp or
          fRoad or fRR or fCanal or fPoll);
end;

{
  Diplomacy
  ____________________________________________________________________
}
procedure GiveCivilReport(P, pAbout: Integer);
begin
  with RW[P].EnemyReport[pAbout]^ do
  begin
    // general info
    TurnOfCivilReport := LastValidStat[pAbout];
    Move(RW[pAbout].Treaty, Treaty, SizeOf(Treaty));
    Government := RW[pAbout].Government;
    Money := RW[pAbout].Money;

    // tech info
    ResearchTech := RW[pAbout].ResearchTech;
    ResearchDone := RW[pAbout].Research * 100 div TechCost(pAbout);
    if ResearchDone > 100 then
      ResearchDone := 100;
    Move(RW[pAbout].Tech, Tech, nAdv);
  end;
end;

procedure GiveMilReport(P, pAbout: Integer);
var
  uix, mix: Integer;
begin
  with RW[P].EnemyReport[pAbout]^ do
  begin
    TurnOfMilReport := LastValidStat[pAbout];
    nModelCounted := RW[pAbout].nModel;
    for mix := 0 to RW[pAbout].nModel - 1 do
    begin
      TellAboutModel(P, pAbout, mix);
      UnCount[mix] := 0
    end;
    for uix := 0 to RW[pAbout].nUn - 1 do
      if RW[pAbout].Un[uix].Loc >= 0 then
        Inc(UnCount[RW[pAbout].Un[uix].mix]);
  end;
end;

procedure ShowPrice(pSender, pTarget, Price: Integer);
begin
  case Price and opMask of
    opTech: // + advance
      with RW[pTarget].EnemyReport[pSender]^ do
        if Tech[Price - opTech] < tsApplicable then
          Tech[Price - opTech] := tsApplicable;
    opModel: // + model index
      TellAboutModel(pTarget, pSender, Price - opModel);
    { opCity: // + city ID
      begin
      end; }
  end;
end;

function CopyCivilReport(pSender, pTarget, pAbout: Integer): Boolean;
var
  I: Integer;
  rSender, rTarget: ^TEnemyReport;
begin // copy third nation civil report
  Result := False;
  if RW[pTarget].Treaty[pAbout] = trNoContact then
    IntroduceEnemy(pTarget, pAbout);
  rSender := Pointer(RW[pSender].EnemyReport[pAbout]);
  rTarget := Pointer(RW[pTarget].EnemyReport[pAbout]);
  if rSender.TurnOfCivilReport > rTarget.TurnOfCivilReport then
  begin // only if newer than current information
    rTarget.TurnOfCivilReport := rSender.TurnOfCivilReport;
    rTarget.Treaty := rSender.Treaty;
    rTarget.Government := rSender.Government;
    rTarget.Money := rSender.Money;
    rTarget.ResearchTech := rSender.ResearchTech;
    rTarget.ResearchDone := rSender.ResearchDone;
    Result := True;
  end;
  for I := 0 to nAdv - 1 do
    if rTarget.Tech[I] < rSender.Tech[I] then
    begin
      rTarget.Tech[I] := rSender.Tech[I];
      Result := True;
    end;
end;

function CopyMilReport(pSender, pTarget, pAbout: Integer): Boolean;
var
  mix: Integer;
  rSender, rTarget: ^TEnemyReport;
begin // copy third nation military report
  Result := False;
  if RW[pTarget].Treaty[pAbout] = trNoContact then
    IntroduceEnemy(pTarget, pAbout);
  rSender := Pointer(RW[pSender].EnemyReport[pAbout]);
  rTarget := Pointer(RW[pTarget].EnemyReport[pAbout]);
  if rSender.TurnOfMilReport > rTarget.TurnOfMilReport then
  begin // only if newer than current information
    rTarget.TurnOfMilReport := rSender.TurnOfMilReport;
    rTarget.nModelCounted := rSender.nModelCounted;
    Move(rSender.UnCount, rTarget.UnCount, 2 * rSender.nModelCounted);
    for mix := 0 to rTarget.nModelCounted - 1 do
      TellAboutModel(pTarget, pAbout, mix);
    Result := True;
  end;
end;

procedure CopyModel(pSender, pTarget, mix: Integer);
var
  I: Integer;
  miSender, miTarget: TModelInfo;
  ok: Boolean;
begin
  // only if target doesn't already have a model like this
  ok := RW[pTarget].nModel < nmmax;
  MakeModelInfo(pSender, mix, RW[pSender].Model[mix], miSender);
  for I := 0 to RW[pTarget].nModel - 1 do
  begin
    MakeModelInfo(pTarget, I, RW[pTarget].Model[I], miTarget);
    if IsSameModel(miSender, miTarget) then
      ok := False;
  end;
  if ok then
  begin
    RW[pTarget].Model[RW[pTarget].nModel] := RW[pSender].Model[mix];
    with RW[pTarget].Model[RW[pTarget].nModel] do
    begin
      IntroTurn := GTurn;
      if Kind = mkSelfDeveloped then
        Kind := mkEnemyDeveloped;
      Status := 0;
      SavedStatus := 0;
      Built := 0;
      Lost := 0;
    end;
    Inc(RW[pTarget].nModel);
    Inc(Researched[pTarget]);
    TellAboutModel(pSender, pTarget, RW[pTarget].nModel - 1);
  end;
end;

procedure CopyMap(pSender, pTarget: Integer);
var
  Loc, I, cix: Integer;
  Tile: Cardinal;
begin
  for Loc := 0 to MapSize - 1 do
    if (RW[pSender].MapObservedLast[Loc] > RW[pTarget].MapObservedLast[Loc])
    then
    begin
      Tile := RW[pSender].Map[Loc];
      if Tile and fCity <> 0 then
      begin
        I := 0;
        while (I < RW[pTarget].nEnemyCity) and
          (RW[pTarget].EnemyCity[I].Loc <> Loc) do
          Inc(I);
        if I = RW[pTarget].nEnemyCity then
        begin
          Inc(RW[pTarget].nEnemyCity);
          Assert(RW[pTarget].nEnemyCity < necmax);
          RW[pTarget].EnemyCity[I].Status := 0;
          RW[pTarget].EnemyCity[I].SavedStatus := 0;
        end;
        if Tile and fOwned <> 0 then
        begin // city owned by sender -- create new info
          cix := RW[pSender].nCity - 1;
          while (cix >= 0) and (RW[pSender].City[cix].Loc <> Loc) do
            Dec(cix);
          MakeCityInfo(pSender, cix, RW[pTarget].EnemyCity[I]);
        end
        else // city not owned by sender -- copy old info
        begin
          cix := RW[pSender].nEnemyCity - 1;
          while (cix >= 0) and (RW[pSender].EnemyCity[cix].Loc <> Loc) do
            Dec(cix);
          RW[pTarget].EnemyCity[I] := RW[pSender].EnemyCity[cix];
        end;
      end
      else if RW[pTarget].Map[Loc] and fCity <> 0 then // remove enemycity
        for cix := 0 to RW[pTarget].nEnemyCity - 1 do
          if RW[pTarget].EnemyCity[cix].Loc = Loc then
            RW[pTarget].EnemyCity[cix].Loc := -1;

      Tile := Tile and (not(fSpecial or fModern) or ResourceMask[pTarget]);
      Tile := Tile or (RW[pTarget].Map[Loc] and fModern);
      if (Tile and fTerrain = RW[pTarget].Map[Loc] and fTerrain) then
        Tile := Tile or (RW[pTarget].Map[Loc] and fSpecial);

      if RW[pTarget].Map[Loc] and fTerrain = fUNKNOWN then
        Inc(Discovered[pTarget]);
      RW[pTarget].Map[Loc] := RW[pTarget].Map[Loc] and fInEnemyZoC
      // always preserve this flag!
        or Tile and not(fUnit or fHiddenUnit or fStealthUnit or fObserved or
        fSpiedOut or fOwned or fInEnemyZoC or fOwnZoCUnit or fPeace or fGrWall);
      if RW[pSender].Territory[Loc] <> RW[pTarget].Territory[Loc] then
      begin
        RW[pTarget].Territory[Loc] := RW[pSender].Territory[Loc];
        { if RW[pTarget].BorderHelper<>nil then
          RW[pTarget].BorderHelper[Loc]:=0; }
      end;
      RW[pTarget].Territory[Loc] := RW[pSender].Territory[Loc];
      RW[pTarget].MapObservedLast[Loc] := RW[pSender].MapObservedLast[Loc];
    end;
end;

function PayPrice(pSender, pTarget, Price: Integer; execute: Boolean): Boolean;
var
  pSubject, I, N, NewTreaty: Integer;
begin
  Result := True;
  case Price and opMask of
    opCivilReport: // + turn + concerned player shl 16
      begin
        pSubject := Price shr 16 and $F;
        if pTarget = pSubject then
          Result := False
        else if pSender = pSubject then
        begin
          if execute then
            GiveCivilReport(pTarget, pSender);
        end
        else if RW[pSender].EnemyReport[pSubject].TurnOfCivilReport < 0 then
          Result := False
        else if execute then
          CopyCivilReport(pSender, pTarget, pSubject);
      end;
    opMilReport: // + turn + concerned player shl 16
      begin
        pSubject := Price shr 16 and $F;
        if pTarget = pSubject then
          Result := False
        else if pSender = pSubject then
        begin
          if execute then
            GiveMilReport(pTarget, pSender);
        end
        else if RW[pSender].EnemyReport[pSubject].TurnOfMilReport < 0 then
          Result := False
        else if execute then
          CopyMilReport(pSender, pTarget, pSubject);
      end;
    opMap:
      if execute then
      begin
        CopyMap(pSender, pTarget);
        RecalcPeaceMap(pTarget);
      end;
    opTreaty .. opTreaty + trAlliance: // + nation treaty
      begin
        if Price - opTreaty = RW[pSender].Treaty[pTarget] - 1 then
        begin // agreed treaty end
          if execute then
            CancelTreaty(pSender, pTarget, False);
        end
        else
        begin
          NewTreaty := -1;
          if Price - opTreaty = RW[pSender].Treaty[pTarget] + 1 then
            NewTreaty := Price - opTreaty
          else if (RW[pSender].Treaty[pTarget] = trNone) and
            (Price - opTreaty = trPeace) then
            NewTreaty := trPeace;
          if NewTreaty < 0 then
            Result := False
          else if execute then
          begin
            Assert(NewTreaty > RW[pSender].Treaty[pTarget]);
            RW[pSender].Treaty[pTarget] := NewTreaty;
            RW[pTarget].Treaty[pSender] := NewTreaty;
            if NewTreaty >= TrFriendlyContact then
            begin
              GiveCivilReport(pTarget, pSender);
              GiveCivilReport(pSender, pTarget);
            end;
            if NewTreaty = trAlliance then
            begin
              GiveMilReport(pTarget, pSender);
              GiveMilReport(pSender, pTarget);
              CopyMap(pSender, pTarget);
              CopyMap(pTarget, pSender);
              RecalcMapZoC(pSender);
              RecalcMapZoC(pTarget);
            end;
            if not(NewTreaty in [trPeace, TrFriendlyContact]) then
            begin
              RW[pSender].EvaStart[pTarget] := -PeaceEvaTurns - 1;
              RW[pTarget].EvaStart[pSender] := -PeaceEvaTurns - 1;
            end;
            RecalcPeaceMap(pSender);
            RecalcPeaceMap(pTarget);
          end;
        end;
      end;
    opShipParts: // + number + part type shl 16
      begin
        N := Price and $FFFF; // number
        I := Price shr 16 and $F; // type
        if (I < nShipPart) and (GShip[pSender].Parts[I] >= N) then
        begin
          if execute then
          begin
            Dec(GShip[pSender].Parts[I], N);
            RW[pSender].Ship[pSender].Parts[I] := GShip[pSender].Parts[I];
            RW[pTarget].Ship[pSender].Parts[I] := GShip[pSender].Parts[I];
            if RW[pTarget].NatBuilt[imSpacePort] > 0 then
            begin // space ship control requires space port
              Inc(GShip[pTarget].Parts[I], N);
              RW[pSender].Ship[pTarget].Parts[I] := GShip[pTarget].Parts[I];
              RW[pTarget].Ship[pTarget].Parts[I] := GShip[pTarget].Parts[I];
            end;
          end;
        end
        else
          Result := False;
      end;
    opMoney: // + value
      if (Price - opMoney <= MaxMoneyPrice) and
        (RW[pSender].Money >= Price - opMoney) then
      begin
        if execute then
        begin
          Dec(RW[pSender].Money, Price - opMoney);
          Inc(RW[pTarget].Money, Price - opMoney);
        end;
      end
      else
        Result := False;
    opTribute: // + value
      if execute then
      begin
      end;
    opTech: // + advance
      if RW[pSender].Tech[Price - opTech] >= tsApplicable then
      begin
        if execute and (RW[pTarget].Tech[Price - opTech] = tsNA) then
        begin
          SeeTech(pTarget, Price - opTech);
          RW[pSender].EnemyReport[pTarget].Tech[Price - opTech] := tsSeen;
        end;
      end
      else
        Result := False;
    opAllTech:
      if execute then
        for I := 0 to nAdv - 1 do
          if (RW[pSender].Tech[I] >= tsApplicable) and
            (RW[pTarget].Tech[I] = tsNA) then
          begin
            SeeTech(pTarget, I);
            RW[pSender].EnemyReport[pTarget].Tech[I] := tsSeen;
            RW[pTarget].EnemyReport[pSender].Tech[I] := tsApplicable;
          end;
    opModel: // + model index
      if Price - opModel < RW[pSender].nModel then
      begin
        if execute then
          CopyModel(pSender, pTarget, Price - opModel);
      end
      else
        Result := False;
    opAllModel:
      if execute then
        for I := 0 to RW[pSender].nModel - 1 do
        begin
          TellAboutModel(pTarget, pSender, I);
          CopyModel(pSender, pTarget, I);
        end;
    { opCity: // + city ID
      begin
      Result:=False
      end; }
  end;
end;

procedure CancelTreaty(P, pWith: Integer; DecreaseCredibility: Boolean);
// side effect: PeaceEnded := bitarray of players with which peace treaty was canceled
var
  p1, OldTreaty: Integer;
begin
  OldTreaty := RW[P].Treaty[pWith];
  PeaceEnded := 0;
  if OldTreaty >= trPeace then
    RW[P].LastCancelTreaty[pWith] := GTurn;
  if DecreaseCredibility then
  begin
    case OldTreaty of
      trPeace:
        begin
          RW[P].Credibility := RW[P].Credibility shr 1;
          if RW[P].MaxCredibility > 0 then
            Dec(RW[P].MaxCredibility, 10);
          if RW[P].Credibility > RW[P].MaxCredibility then
            RW[P].Credibility := RW[P].MaxCredibility;
        end;
      trAlliance:
        RW[P].Credibility := RW[P].Credibility * 3 div 4;
    end;
    RW[pWith].EnemyReport[P].Credibility := RW[P].Credibility;
  end;

  if OldTreaty = trPeace then
  begin
    for p1 := 0 to nPl - 1 do
      if (p1 = pWith) or DecreaseCredibility and (p1 <> P) and
        (RW[pWith].Treaty[p1] = trAlliance) and (RW[P].Treaty[p1] >= trPeace)
      then
      begin
        RW[P].Treaty[p1] := trNone;
        RW[p1].Treaty[P] := trNone;
        RW[P].EvaStart[p1] := -PeaceEvaTurns - 1;
        RW[p1].EvaStart[P] := -PeaceEvaTurns - 1;
        Inc(PeaceEnded, 1 shl p1);
      end;
    CheckBorders(-1);
    if (Mode > moLoading_Fast) and (PeaceEnded > 0) then
      RecalcMapZoC(P);
  end
  else
  begin
    RW[P].Treaty[pWith] := OldTreaty - 1;
    RW[pWith].Treaty[P] := OldTreaty - 1;
    if OldTreaty = TrFriendlyContact then
    begin // necessary for loading
      GiveCivilReport(P, pWith);
      GiveCivilReport(pWith, P);
    end
    else if OldTreaty = trAlliance then
    begin // necessary for loading
      GiveMilReport(P, pWith);
      GiveMilReport(pWith, P);
    end;
    if (Mode > moLoading_Fast) and (OldTreaty = trAlliance) then
    begin
      RecalcMapZoC(P);
      RecalcMapZoC(pWith);
    end;
  end;
  if OldTreaty in [trPeace, trAlliance] then
  begin
    RecalcPeaceMap(P);
    RecalcPeaceMap(pWith);
  end;
end;

function DoSpyMission(P, pCity, cix, Mission: Integer): Cardinal;
var
  p1: Integer;
begin
  Result := 0;
  case Mission of
    smSabotageProd:
      RW[pCity].City[cix].Flags := RW[pCity].City[cix].Flags or
        chProductionSabotaged;
    smStealMap:
      begin
        CopyMap(pCity, P);
        RecalcPeaceMap(P);
      end;
    smStealCivilReport:
      begin
        if RW[P].Treaty[pCity] = trNoContact then
          IntroduceEnemy(P, pCity);
        GiveCivilReport(P, pCity);
      end;
    smStealMilReport:
      begin
        if RW[P].Treaty[pCity] = trNoContact then
          IntroduceEnemy(P, pCity);
        GiveMilReport(P, pCity);
      end;
    smStealForeignReports:
      begin
        for p1 := 0 to nPl - 1 do
          if (p1 <> P) and (p1 <> pCity) and (RW[pCity].EnemyReport[p1] <> nil)
          then
          begin
            if RW[pCity].EnemyReport[p1].TurnOfCivilReport >= 0 then
              if CopyCivilReport(pCity, P, p1) then
                Result := Result or (1 shl (2 * p1));
            if RW[pCity].EnemyReport[p1].TurnOfMilReport >= 0 then
              if CopyMilReport(pCity, P, p1) then
                Result := Result or (2 shl (2 * p1));
          end;
      end;
  end;
end;

{
  Test Flags
  ____________________________________________________________________
}
procedure ClearTestFlags(ClearFlags: Integer);
var
  p1: Integer;
begin
  GTestFlags := GTestFlags and (not ClearFlags or tfTested or tfAllTechs or
    tfAllContact);
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and (GAlive or GWatching) <> 0 then
      RW[p1].TestFlags := GTestFlags;
end;

procedure SetTestFlags(P, SetFlags: Integer);
var
  I, p1, p2, MoreFlags: Integer;
begin
  MoreFlags := SetFlags and not GTestFlags;
  GTestFlags := GTestFlags or (SetFlags and $7FF);
  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and (GAlive or GWatching) <> 0 then
      RW[p1].TestFlags := GTestFlags;

  if MoreFlags and (tfUncover or tfAllContact) <> 0 then
    for p1 := 0 to nPl - 2 do
      if 1 shl p1 and GAlive <> 0 then
        for p2 := p1 + 1 to nPl - 1 do
          if 1 shl p2 and GAlive <> 0 then
          begin // make p1 and p2 know each other
            if RW[p1].Treaty[p2] = trNoContact then
              IntroduceEnemy(p1, p2);
          end;

  if MoreFlags and tfAllTechs <> 0 then
    for p1 := 0 to nPl - 1 do
    begin
      ResourceMask[p1] := $FFFFFFFF;
      if 1 shl p1 and GAlive <> 0 then
      begin
        for I := 0 to nAdv - 1 do // give all techs to player p1
          if not(I in FutureTech) and (RW[p1].Tech[I] < tsApplicable) then
          begin
            RW[p1].Tech[I] := tsCheat;
            CheckSpecialModels(p1, I);
          end;
        for p2 := 0 to nPl - 1 do
          if (p2 <> p1) and (1 shl p2 and (GAlive or GWatching) <> 0) then
            for I := 1 to 3 do
              if RW[p2].EnemyReport[p1].Tech[AgePreq[I]] < tsApplicable then
                RW[p2].EnemyReport[p1].Tech[AgePreq[I]] := tsCheat;
      end;
    end;

  if MoreFlags and tfUncover <> 0 then
  begin
    DiscoverAll(P, lObserveSuper);
    for p1 := 0 to nPl - 1 do
      if 1 shl p1 and GAlive <> 0 then
      begin
        ResourceMask[p1] := $FFFFFFFF;
        if p1 <> P then
        begin
          GiveCivilReport(P, p1);
          GiveMilReport(P, p1);
        end;
      end;
  end;
end;

{
  Internal Command Processing
  ____________________________________________________________________
}
procedure IntServer(Command, Player, Subject: Integer; var Data);
var
  I, p1: Integer;
begin
  if Mode = moPlaying then
    CL.Put(Command, Player, Subject, @Data);

  case Command of

    sIntTellAboutNation:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntTellAboutNation P%d+P%d', [Player, Subject]); {$ENDIF}
        Assert((Player >= 0) and (Player < nPl) and (Subject >= 0) and
          (Subject < nPl));
        IntroduceEnemy(Player, Subject);
      end;

    sIntHaveContact:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntHaveContact P%d+P%d', [Player, Subject]); {$ENDIF}
        Assert(RW[Player].Treaty[Subject] > trNoContact);
        RW[Player].EnemyReport[Subject].TurnOfContact := GTurn;
        RW[Subject].EnemyReport[Player].TurnOfContact := GTurn;
      end;

    sIntCancelTreaty:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntCancelTreaty P%d with P%d', [Player, Subject]); {$ENDIF}
        CancelTreaty(Player, Subject);
      end;

    (* sIntChoosePeace:
      begin
      {$IFDEF TEXTLOG}CmdInfo:=Format('IntChoosePeace P%d+P%d', [Player,Subject]);{$ENDIF}
      RW[Player].Treaty[Subject]:=trPeace;
      RW[Subject].Treaty[Player]:=trPeace;
      end; *)

    sIntTellAboutModel .. sIntTellAboutModel + (nPl - 1) shl 4:
      begin
        p1 := (Command - sIntTellAboutModel) shr 4; // told player
{$IFDEF TEXTLOG}CmdInfo := Format('IntTellAboutModel P%d about P%d Mod%d', [p1, Player, Subject]); {$ENDIF}
        Assert((Player >= 0) and (Player < nPl));
        Assert((Subject >= 0) and (Subject < RW[Player].nModel));
        MakeModelInfo(Player, Subject, RW[Player].Model[Subject],
          RW[p1].EnemyModel[RW[p1].nEnemyModel]);
        RWemix[p1, Player, Subject] := RW[p1].nEnemyModel;
        Inc(RW[p1].nEnemyModel);
        Assert(RW[p1].nEnemyModel < nemmax);
      end;

    sIntDiscoverZOC:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntDiscoverZOC P%d Loc%d', [Player, Integer(Data)]); {$ENDIF}
        Discover9(Integer(Data), Player, lObserveUnhidden, True, False);
      end;

    sIntExpandTerritory:
      if Mode < moPlaying then
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntExpandTerritory P%d Loc%d', [Player, RW[Player].City[Subject].Loc]); {$ENDIF}
        Move(Data, BorderChanges, SizeOf(BorderChanges));
        ExpandTerritory(RW[Player].City[Subject].Loc);
      end;

    sIntBuyMaterial:
      with RW[Player].City[Subject] do
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntBuyMaterial P%d Loc%d Cost%d', [Player, Loc, Integer(Data)]); {$ENDIF}
        Dec(RW[Player].Money, Integer(Data));
        if (GWonder[woMich].EffectiveOwner = Player) and (Project and cpImp <> 0)
        then
          Inc(Prod, Integer(Data) div 2)
        else
          Inc(Prod, Integer(Data) div 4);
        if Project0 and not cpAuto <> Project and not cpAuto then
          Project0 := Project;
        Prod0 := Prod;
      end;

    sIntPayPrices .. sIntPayPrices + 12:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntPayPrices P%d+P%d', [Player, Subject]); {$ENDIF}
        for I := 0 to TOffer(Data).nDeliver - 1 do
          PayPrice(Player, Subject, TOffer(Data).Price[I], True);
        for I := 0 to TOffer(Data).nCost - 1 do
          PayPrice(Subject, Player, TOffer(Data).Price[TOffer(Data).nDeliver
            + I], True);
        for I := 0 to TOffer(Data).nDeliver + TOffer(Data).nCost - 1 do
          if TOffer(Data).Price[I] = opTreaty + trAlliance then
          begin // add view area of allied player
            DiscoverViewAreas(Player);
            DiscoverViewAreas(Subject);
            Break;
          end;
      end;

    sIntSetDevModel:
      if Mode < moPlaying then
        Move(Data, RW[Player].DevModel.Kind, sIntSetDevModel and $F * 4);

    sIntSetModelStatus:
      if ProcessClientData[Player] then
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntSetModelStatus P%d', [Player]);
        {$ENDIF}
        RW[Player].Model[Subject].Status := Integer(Data);
      end;

    sIntSetUnitStatus:
      if ProcessClientData[Player] then
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntSetUnitStatus P%d', [Player]);
        {$ENDIF}
        RW[Player].Un[Subject].Status := Integer(Data);
      end;

    sIntSetCityStatus:
      if ProcessClientData[Player] then
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntSetCityStatus P%d', [Player]);
        {$ENDIF}
        RW[Player].City[Subject].Status := Integer(Data);
      end;

    sIntSetECityStatus:
      if ProcessClientData[Player] then
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('IntSetECityStatus P%d', [Player]);
        {$ENDIF}
        RW[Player].EnemyCity[Subject].Status := Integer(Data);
      end;
  end;
end;

end.
