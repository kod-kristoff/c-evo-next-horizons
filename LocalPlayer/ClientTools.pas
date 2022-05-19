{$INCLUDE Switches.inc}
unit ClientTools;

interface

uses
  Protocol;

const
  nOfferedResourceWeights = 6;
  OfferedResourceWeights: array [0 .. nOfferedResourceWeights - 1] of Cardinal =
    (rwOff, rwMaxScience, rwForceScience, rwMaxGrowth, rwForceProd, rwMaxProd);

type
  TImpOrder = array [0 .. (nImp + 4) div 4 * 4 - 1] of ShortInt;
  TEnhancementJobs = array [0 .. 11, 0 .. 7] of Byte;
  JobResultSet = set of 0 .. 39;

  TMapOption = (
    // options switched by buttons
    moPolitical = 0, moCityNames = 1, moGreatWall = 4, moGrid = 5, moBareTerrain = 6,
    // other options
    moEditMode = 16, moLocCodes = 17
  );
  TMapOptions = set of TMapOption;

  TSaveOption = (soAlEffectiveMovesOnly = 0, soEnMoves = 1, soEnAttacks = 2,
    soEnNoMoves = 3, soWaitTurn = 4, soEffectiveMovesOnly = 5, soEnFastMoves = 6,
    soSlowMoves = 7, soFastMoves = 8, soVeryFastMoves = 9, soNames = 10,
    soRepList = 11, soRepScreens = 12, soSoundOff = 13, soSoundOn = 14,
    soSoundOnAlt = 15, soScrollSlow = 16, soScrollFast = 17, soScrollOff = 18,
    soAlSlowMoves = 19, soAlFastMoves = 20, somAlNoMoves = 21, soTellAI = 30);
  TSaveOptions = set of TSaveOption;

var
  Server: TServerCall;
  G: TNewGameData;
  Me: Integer;
  MyRO: ^TPlayerContext;
  MyMap: ^TTileList;
  MyUn: ^TUnList;
  MyCity: ^TCityList;
  MyModel: ^TModelList;

  AdvValue: array [0 .. nAdv - 1] of Integer;

function dLoc(Loc, dx, dy: Integer): Integer;
function Distance(Loc0, Loc1: Integer): Integer;
function UnrestAtLoc(uix, Loc: Integer): Boolean;
function GetMoveAdvice(uix, ToLoc: Integer;
  var MoveAdviceData: TMoveAdviceData): Integer;
function ColorOfHealth(Health: Integer): Integer;
function IsMultiPlayerGame: Boolean;
procedure ItsMeAgain(P: Integer);
function GetAge(P: Integer): Integer;
function IsCivilReportNew(Enemy: Integer): Boolean;
function IsMilReportNew(Enemy: Integer): Boolean;
function CutCityFoodSurplus(FoodSurplus: Integer; IsCityAlive: Boolean;
  gov, size: Integer): Integer;
function CityTaxBalance(cix: Integer; const CityReport: TCityReportNew): Integer;
procedure SumCities(var TaxSum, ScienceSum: Integer);
function JobTest(uix, Job: Integer; IgnoreResults: JobResultSet = []): Boolean;
procedure GetUnitInfo(Loc: Integer; var uix: Integer; var UnitInfo: TUnitInfo);
procedure GetCityInfo(Loc: Integer; var cix: Integer; var CityInfo: TCityInfo);
function UnitExhausted(uix: Integer): Boolean;
function ModelHash(const ModelInfo: TModelInfo): Integer;
function ProcessEnhancement(uix: Integer; const Jobs: TEnhancementJobs): Integer;
function AutoBuild(cix: Integer; const ImpOrder: TImpOrder): Boolean;
procedure DebugMessage(Level: Integer; Text: string);
procedure CityOptimizer_BeginOfTurn;
procedure CityOptimizer_CityChange(cix: Integer);
procedure CityOptimizer_TileBecomesAvailable(Loc: Integer);
procedure CityOptimizer_ReleaseCityTiles(cix, ReleasedTiles: Integer);
procedure CityOptimizer_BeforeRemoveUnit(uix: Integer);
procedure CityOptimizer_AfterRemoveUnit;
procedure CityOptimizer_EndOfTurn;
function GetMyCityByLoc(Loc: Integer): PCity;
function GetEnemyCityByLoc(Loc: Integer): PCityInfo;
function GetMyUnitByLoc(Loc: Integer): PUn;
function GetEnemyUnitByLoc(Loc: Integer): PUnitInfo;


implementation

var
  CityNeedsOptimize: array [0 .. ncmax - 1] of Boolean;

function dLoc(Loc, dx, dy: Integer): Integer;
var
  y0: Integer;
begin
  y0 := (Loc + G.lx * 1024) div G.lx - 1024;
  Result := (Loc + (dx + y0 and 1 + G.lx * 1024) shr 1) mod G.lx + G.lx * (y0 + dy);
end;

function Distance(Loc0, Loc1: Integer): Integer;
var
  dx, dy: Integer;
begin
  Inc(Loc0, G.lx * 1024);
  Inc(Loc1, G.lx * 1024);
  dx := abs(((Loc1 mod G.lx * 2 + Loc1 div G.lx and 1) -
    (Loc0 mod G.lx * 2 + Loc0 div G.lx and 1) + 3 * G.lx) mod (2 * G.lx) - G.lx);
  dy := abs(Loc1 div G.lx - Loc0 div G.lx);
  Result := dx + dy + abs(dx - dy) shr 1;
end;

function UnrestAtLoc(uix, Loc: Integer): Boolean;
var
  uix1: Integer;
begin
  Result := False;
  if MyModel[MyUn[uix].mix].Flags and mdCivil = 0 then
    case MyRO.Government of
      gRepublic, gFuture:
        Result := (MyRO.Territory[Loc] >= 0) and (MyRO.Territory[Loc] <> Me) and
          (MyRO.Treaty[MyRO.Territory[Loc]] < trAlliance);
      gDemocracy:
        Result := (MyRO.Territory[Loc] < 0) or (MyRO.Territory[Loc] <> Me) and
          (MyRO.Treaty[MyRO.Territory[Loc]] < trAlliance);
    end;
  with MyModel[MyUn[uix].mix] do
    if Cap[mcSeaTrans] + Cap[mcAirTrans] + Cap[mcCarrier] > 0 then
      for uix1 := 0 to MyRO.nUn - 1 do // check transported units too
        if (MyUn[uix1].Loc >= 0) and (MyUn[uix1].Master = uix) then
          Result := Result or UnrestAtLoc(uix1, Loc);
end;

function GetMoveAdvice(uix, ToLoc: Integer;
  var MoveAdviceData: TMoveAdviceData): Integer;
var
  MinEndHealth: Integer;
begin
  if MyModel[MyUn[uix].mix].Domain = dGround then
    MinEndHealth := 100
  else
    MinEndHealth := 1; // resistent to hostile terrain -- don't consider
  repeat
    if MyUn[uix].Health >= MinEndHealth then
    begin
      MoveAdviceData.ToLoc := ToLoc;
      MoveAdviceData.MoreTurns := 999;
      MoveAdviceData.MaxHostile_MovementLeft := MyUn[uix].Health - MinEndHealth;
      Result := Server(sGetMoveAdvice, Me, uix, MoveAdviceData);
      if (MinEndHealth <= 1) or (Result <> eNoWay) then
        Exit;
    end;
    case MinEndHealth of
      100:
        MinEndHealth := 50;
      50:
        MinEndHealth := 25;
      25:
        MinEndHealth := 12;
      else
        MinEndHealth := 1
    end;
  until False;
end;

function ColorOfHealth(Health: Integer): Integer;
var
  Red, Green: Integer;
begin
  Green := 400 * Health div 100;
  if Green > 200 then
    Green := 200;
  Red := 510 * (100 - Health) div 100;
  if Red > 255 then
    Red := 255;
  Result := Green shl 8 + Red;
end;

function IsMultiPlayerGame: Boolean;
var
  p1: Integer;
begin
  Result := False;
  for p1 := 1 to nPl - 1 do
    if G.RO[p1] <> nil then
      Result := True;
end;

procedure ItsMeAgain(P: Integer);
begin
  if G.RO[P] <> nil then
    MyRO := Pointer(G.RO[P])
  else if G.SuperVisorRO[P] <> nil then
    MyRO := Pointer(G.SuperVisorRO[P])
  else
    Exit;
  Me := P;
  MyMap := Pointer(MyRO.Map);
  MyUn := Pointer(MyRO.Un);
  MyCity := Pointer(MyRO.City);
  MyModel := Pointer(MyRO.Model);
end;

function GetAge(P: Integer): Integer;
var
  I: Integer;
begin
  if P = Me then begin
    Result := 0;
    for I := 1 to 3 do
      if MyRO.Tech[AgePreq[I]] >= tsApplicable then
        Result := I;
  end else begin
    Result := 0;
    for I := 1 to 3 do
      if MyRO.EnemyReport[P].Tech[AgePreq[I]] >= tsApplicable then
        Result := I;
  end;
end;

function IsCivilReportNew(Enemy: Integer): Boolean;
var
  I: Integer;
begin
  Assert(Enemy <> Me);
  I := MyRO.EnemyReport[Enemy].TurnOfCivilReport;
  Result := (I = MyRO.Turn) or (I = MyRO.Turn - 1) and (Enemy > Me);
end;

function IsMilReportNew(Enemy: Integer): Boolean;
var
  I: Integer;
begin
  Assert(Enemy <> Me);
  I := MyRO.EnemyReport[Enemy].TurnOfMilReport;
  Result := (I = MyRO.Turn) or (I = MyRO.Turn - 1) and (Enemy > Me);
end;

function CutCityFoodSurplus(FoodSurplus: Integer; IsCityAlive: Boolean;
  gov, size: Integer): Integer;
begin
  Result := FoodSurplus;
  if not IsCityAlive or (Result > 0) and ((gov = gFuture) or
    (size >= NeedAqueductSize) and (Result < 2)) then
    Result := 0; { no growth }
end;

function CityTaxBalance(cix: Integer; const CityReport: TCityReportNew): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (CityReport.HappinessBalance >= 0) { no disorder } and
    (MyCity[cix].Flags and chCaptured = 0) then // not captured
  begin
    Inc(Result, CityReport.Tax);
    if (MyCity[cix].Project and (cpImp + cpIndex) = cpImp + imTrGoods) and
      (CityReport.Production > 0) then
      Inc(Result, CityReport.Production);
    if ((MyRO.Government = gFuture) or (MyCity[cix].size >=
      NeedAqueductSize) and (CityReport.FoodSurplus < 2)) and
      (CityReport.FoodSurplus > 0) then
      Inc(Result, CityReport.FoodSurplus);
  end;
  for I := nWonder to nImp - 1 do
    if MyCity[cix].Built[I] > 0 then
      Dec(Result, Imp[I].Maint);
end;

procedure SumCities(var TaxSum, ScienceSum: Integer);
var
  cix: Integer;
  CityReport: TCityReportNew;
begin
  TaxSum := MyRO.OracleIncome;
  ScienceSum := 0;
  if MyRO.Government = gAnarchy then
    Exit;
  for cix := 0 to MyRO.nCity - 1 do
    if MyCity[cix].Loc >= 0 then
    begin
      CityReport.HypoTiles := -1;
      CityReport.HypoTaxRate := -1;
      CityReport.HypoLuxuryRate := -1;
      Server(sGetCityReportNew, Me, cix, CityReport);
      if (CityReport.HappinessBalance >= 0) { no disorder } and
        (MyCity[cix].Flags and chCaptured = 0) then // not captured
        ScienceSum := ScienceSum + CityReport.Science;
      TaxSum := TaxSum + CityTaxBalance(cix, CityReport);
    end;
end;

function JobTest(uix, Job: Integer; IgnoreResults: JobResultSet): Boolean;
var
  Test: Integer;
begin
  Test := Server(sStartJob + Job shl 4 - sExecute, Me, uix, nil^);
  Result := (Test >= rExecuted) or (Test in IgnoreResults);
end;

procedure GetUnitInfo(Loc: Integer; var uix: Integer; var UnitInfo: TUnitInfo);
var
  I, Cnt: Integer;
begin
  if MyMap[Loc] and fOwned <> 0 then
  begin
    Server(sGetDefender, Me, Loc, uix);
    Cnt := 0;
    for I := 0 to MyRO.nUn - 1 do
      if MyUn[I].Loc = Loc then
        Inc(Cnt);
    MakeUnitInfo(Me, MyUn[uix], UnitInfo);
    if Cnt > 1 then
      UnitInfo.Flags := UnitInfo.Flags or unMulti;
  end
  else
  begin
    uix := MyRO.nEnemyUn - 1;
    while (uix >= 0) and (MyRO.EnemyUn[uix].Loc <> Loc) do
      Dec(uix);
    UnitInfo := MyRO.EnemyUn[uix];
  end;
end;

procedure GetCityInfo(Loc: Integer; var cix: Integer; var CityInfo: TCityInfo);
begin
  if MyMap[Loc] and fOwned <> 0 then
  begin
    CityInfo.Loc := Loc;
    cix := MyRO.nCity - 1;
    while (cix >= 0) and (MyCity[cix].Loc <> Loc) do
      Dec(cix);
    with CityInfo do
    begin
      Owner := Me;
      ID := MyCity[cix].ID;
      size := MyCity[cix].size;
      Flags := 0;
      if MyCity[cix].Built[imPalace] > 0 then
        Inc(Flags, ciCapital);
      if (MyCity[cix].Built[imWalls] > 0) or
        (MyMap[MyCity[cix].Loc] and fGrWall <> 0) then
        Inc(Flags, ciWalled);
      if MyCity[cix].Built[imCoastalFort] > 0 then
        Inc(Flags, ciCoastalFort);
      if MyCity[cix].Built[imMissileBat] > 0 then
        Inc(Flags, ciMissileBat);
      if MyCity[cix].Built[imBunker] > 0 then
        Inc(Flags, ciBunker);
      if MyCity[cix].Built[imSpacePort] > 0 then
        Inc(Flags, ciSpacePort);
    end;
  end
  else
  begin
    cix := MyRO.nEnemyCity - 1;
    while (cix >= 0) and (MyRO.EnemyCity[cix].Loc <> Loc) do
      Dec(cix);
    CityInfo := MyRO.EnemyCity[cix];
  end;
end;

function UnitExhausted(uix: Integer): Boolean;
  // check if another move of this unit is still possible
var
  dx, dy: Integer;
begin
  Result := True;
  if (MyUn[uix].Movement > 0) or
    (MyRO.Wonder[woShinkansen].EffectiveOwner = Me) then
    if (MyUn[uix].Movement >= 100) or
      ((MyModel[MyUn[uix].mix].Kind = mkCaravan) and
      (MyMap[MyUn[uix].Loc] and fCity <> 0)) then
      Result := False
    else
      for dx := -2 to 2 do
        for dy := -2 to 2 do
          if abs(dx) + abs(dy) = 2 then
            if Server(sMoveUnit - sExecute + dx and 7 shl 4 + dy and
              7 shl 7, Me, uix, nil^) >= rExecuted then
              Result := False;
end;

function ModelHash(const ModelInfo: TModelInfo): Integer;
var
  I, FeatureCode, Hash1, Hash2, Hash2r, D: Cardinal;
begin
  with ModelInfo do
    if Kind > mkEnemyDeveloped then
      Result := Integer($C0000000 + Speed div 50 + Kind shl 8)
    else
    begin
      FeatureCode := 0;
      for I := mcFirstNonCap to nFeature - 1 do
        if 1 shl Domain and Feature[I].Domains <> 0 then
        begin
          FeatureCode := FeatureCode * 2;
          if 1 shl (I - mcFirstNonCap) <> 0 then
            Inc(FeatureCode);
        end;
      case Domain of
        dGround:
        begin
          Assert(FeatureCode < 1 shl 8);
          Assert(Attack < 5113);
          Assert(Defense < 2273);
          Assert(Cost < 1611);
          Hash1 := (Attack * 2273 + Defense) * 9 + (Speed - 150) div 50;
          Hash2 := FeatureCode * 1611 + Cost;
        end;
        dSea:
        begin
          Assert(FeatureCode < 1 shl 9);
          Assert(Attack < 12193);
          Assert(Defense < 6097);
          Assert(Cost < 4381);
          Hash1 := ((Attack * 6097 + Defense) * 5 +
            (Speed - 350) div 100) * 2;
          if Weight >= 6 then
            Inc(Hash1);
          Hash2 := ((TTrans * 17 + ATrans_Fuel) shl 9 + FeatureCode) *
            4381 + Cost;
        end;
        dAir:
        begin
          Assert(FeatureCode < 1 shl 5);
          Assert(Attack < 2407);
          Assert(Defense < 1605);
          Assert(Bombs < 4813);
          Assert(Cost < 2089);
          Hash1 := (Attack * 1605 + Defense) shl 5 + FeatureCode;
          Hash2 := ((Bombs * 7 + ATrans_Fuel) * 4 + TTrans) * 2089 + Cost;
        end;
      end;
      Hash2r := 0;
      for I := 0 to 7 do
      begin
        Hash2r := Hash2r * 13;
        D := Hash2 div 13;
        Inc(Hash2r, Hash2 - D * 13);
        Hash2 := D;
      end;
      Result := Integer(Domain shl 30 + Hash1 xor Hash2r);
    end;
end;

function ProcessEnhancement(uix: Integer; const Jobs: TEnhancementJobs): Integer;
  { return values:
    eJobDone - all applicable jobs done
    eOK - enhancement not complete
    eDied - job done and died (thurst) }
var
  stage, NextJob, Tile: Integer;
  Done: set of jNone .. jPoll;
begin
  Done := [];
  Tile := MyMap[MyUn[uix].Loc];
  if Tile and fRoad <> 0 then
    Include(Done, jRoad);
  if Tile and fRR <> 0 then
    Include(Done, jRR);
  if (Tile and fTerImp = tiIrrigation) or (Tile and fTerImp = tiFarm) then
    Include(Done, jIrr);
  if Tile and fTerImp = tiFarm then
    Include(Done, jFarm);
  if Tile and fTerImp = tiMine then
    Include(Done, jMine);
  if Tile and fPoll = 0 then
    Include(Done, jPoll);

  if MyUn[uix].Job = jNone then
    Result := eJobDone
  else
    Result := eOK;
  while (Result <> eOK) and (Result <> eDied) do
  begin
    stage := -1;
    repeat
      if stage = -1 then
        NextJob := jPoll
      else
        NextJob := Jobs[Tile and fTerrain, stage];
      if (NextJob = jNone) or not (NextJob in Done) then
        Break;
      Inc(stage);
    until stage = 5;
    if (stage = 5) or (NextJob = jNone) then
    begin
      Result := eJobDone;
      Break;
    end; // tile enhancement complete
    Result := Server(sStartJob + NextJob shl 4, Me, uix, nil^);
    Include(Done, NextJob);
  end;
end;

function AutoBuild(cix: Integer; const ImpOrder: TImpOrder): Boolean;
var
  I, NewProject: Integer;
begin
  Result := False;
  if (MyCity[cix].Project and (cpImp + cpIndex) = cpImp + imTrGoods) or
    (MyCity[cix].Flags and chProduction <> 0) then
  begin
    I := 0;
    repeat
      while (ImpOrder[I] >= 0) and (MyCity[cix].Built[ImpOrder[I]] > 0) do
        Inc(I);
      if ImpOrder[I] < 0 then
        Break;
      Assert(I < nImp);
      NewProject := cpImp + ImpOrder[I];
      if Server(sSetCityProject, Me, cix, NewProject) >= rExecuted then
      begin
        Result := True;
        CityOptimizer_CityChange(cix);
        Break;
      end;
      Inc(I);
    until False;
  end;
end;

procedure CalculateAdvValues;
var
  I, J: Integer;
  known: array [0 .. nAdv - 1] of Integer;

  procedure MarkPreqs(I: Integer);
  begin
    if known[I] = 0 then
    begin
      known[I] := 1;
      if (I <> adScience) and (I <> adMassProduction) then
      begin
        if (AdvPreq[I, 0] >= 0) then
          MarkPreqs(AdvPreq[I, 0]);
        if (AdvPreq[I, 1] >= 0) then
          MarkPreqs(AdvPreq[I, 1]);
      end;
    end;
  end;

begin
  FillChar(AdvValue, SizeOf(AdvValue), 0);
  for I := 0 to nAdv - 1 do
  begin
    FillChar(known, SizeOf(known), 0);
    MarkPreqs(I);
    for J := 0 to nAdv - 1 do
      if known[J] > 0 then
        Inc(AdvValue[I]);
    if I in FutureTech then
      Inc(AdvValue[I], 3000)
    else if known[adMassProduction] > 0 then
      Inc(AdvValue[I], 2000)
    else if known[adScience] > 0 then
      Inc(AdvValue[I], 1000);
  end;
end;

procedure DebugMessage(Level: Integer; Text: string);
begin
  Server(sMessage, Me, Level, PChar(Text)^);
end;

function MarkCitiesAround(Loc, cixExcept: Integer): Boolean;
  // return whether a city was marked
var
  cix: Integer;
begin
  Result := False;
  for cix := 0 to MyRO.nCity - 1 do
    if (cix <> cixExcept) and (MyCity[cix].Loc >= 0) and
      (MyCity[cix].Flags and chCaptured = 0) and
      (Distance(MyCity[cix].Loc, Loc) <= 5) then
    begin
      CityNeedsOptimize[cix] := True;
      Result := True;
    end;
end;

procedure OptimizeCities(CheckOnly: Boolean);
var
  cix, fix, dx, dy, Loc1, OptiType: Integer;
  Done: Boolean;
  Advice: TCityTileAdviceData;
begin
  repeat
    Done := True;
    for cix := 0 to MyRO.nCity - 1 do
      if CityNeedsOptimize[cix] then
      begin
        OptiType := (MyCity[cix].Status shr 4) and $0F;
        if OptiType <> 0 then
        begin
          Advice.ResourceWeights := OfferedResourceWeights[OptiType];
          Server(sGetCityTileAdvice, Me, cix, Advice);
          if Advice.Tiles <> MyCity[cix].Tiles then
            if CheckOnly then
            begin
              // TODO: What is this assert for?
              // Need to optimize city tiles but CheckOnly true?
              //assert(false)
            end
            else
            begin
              for fix := 1 to 26 do
                if MyCity[cix].Tiles and not Advice.Tiles and
                  (1 shl fix) <> 0 then
                begin // tile no longer used by this city -- check using it by another
                  dy := fix shr 2 - 3;
                  dx := fix and 3 shl 1 - 3 + (dy + 3) and 1;
                  Loc1 := dLoc(MyCity[cix].Loc, dx, dy);
                  if MarkCitiesAround(Loc1, cix) then
                    Done := False;
                end;
              Server(sSetCityTiles, Me, cix, Advice.Tiles);
            end;
        end;
        CityNeedsOptimize[cix] := False;
      end;
  until Done;
end;

procedure CityOptimizer_BeginOfTurn;
var
  cix: Integer;
begin
  FillChar(CityNeedsOptimize, MyRO.nCity - 1, 0); // false
  if MyRO.Government <> gAnarchy then
  begin
    for cix := 0 to MyRO.nCity - 1 do
      if (MyCity[cix].Loc >= 0) and (MyCity[cix].Flags and chCaptured = 0)
      then
        CityNeedsOptimize[cix] := True;
    OptimizeCities(False); // optimize all cities
  end;
end;

procedure CityOptimizer_CityChange(cix: Integer);
begin
  if (MyRO.Government <> gAnarchy) and (cix <> -1) and (MyCity[cix].Flags and
    chCaptured = 0) then
  begin
    CityNeedsOptimize[cix] := True;
    OptimizeCities(False);
  end;
end;

procedure CityOptimizer_TileBecomesAvailable(Loc: Integer);
begin
  if (MyRO.Government <> gAnarchy) and MarkCitiesAround(Loc, -1) then
    OptimizeCities(False);
end;

procedure CityOptimizer_ReleaseCityTiles(cix, ReleasedTiles: Integer);
var
  fix, dx, dy, Loc1: Integer;
  Done: Boolean;
begin
  if (MyRO.Government <> gAnarchy) and (ReleasedTiles <> 0) then
  begin
    Done := True;
    for fix := 1 to 26 do
      if ReleasedTiles and (1 shl fix) <> 0 then
      begin
        dy := fix shr 2 - 3;
        dx := fix and 3 shl 1 - 3 + (dy + 3) and 1;
        Loc1 := dLoc(MyCity[cix].Loc, dx, dy);
        if MarkCitiesAround(Loc1, cix) then
          Done := False;
      end;
    if not Done then
      OptimizeCities(False);
  end;
end;

procedure CityOptimizer_BeforeRemoveUnit(uix: Integer);
var
  uix1: Integer;
begin
  if MyRO.Government <> gAnarchy then
  begin
    if MyUn[uix].Home >= 0 then
      CityNeedsOptimize[MyUn[uix].Home] := True;

    // transported units are also removed
    for uix1 := 0 to MyRO.nUn - 1 do
      if (MyUn[uix1].Loc >= 0) and (MyUn[uix1].Master = uix) and
        (MyUn[uix1].Home >= 0) then
        CityNeedsOptimize[MyUn[uix1].Home] := True;
  end;
end;

procedure CityOptimizer_AfterRemoveUnit;
begin
  if MyRO.Government <> gAnarchy then
    OptimizeCities(False);
end;

procedure CityOptimizer_EndOfTurn;
// all cities should already be optimized here -- only check this
var
  cix: Integer;
begin
{$IFOPT O-}
  if MyRO.Government <> gAnarchy then
  begin
    FillChar(CityNeedsOptimize, MyRO.nCity - 1, 0); // false
    for cix := 0 to MyRO.nCity - 1 do
      if (MyCity[cix].Loc >= 0) and (MyCity[cix].Flags and chCaptured = 0)
      then
        CityNeedsOptimize[cix] := True;
    OptimizeCities(True); // check all cities
  end;
{$ENDIF}
end;

function GetMyCityByLoc(Loc: Integer): PCity;
var
  I: Integer;
begin
  I := MyRO.nCity - 1;
  while (I >= 0) and (MyCity[I].Loc <> Loc) do Dec(I);
  if I >= 0 then Result := @MyCity[I]
    else Result := nil;
end;

function GetEnemyCityByLoc(Loc: Integer): PCityInfo;
var
  I: Integer;
begin
  I := MyRO.nEnemyCity - 1;
  while (I >= 0) and (MyRo.EnemyCity[I].Loc <> Loc) do Dec(I);
  if I >= 0 then Result := @MyRo.EnemyCity[I]
    else Result := nil;
end;

function GetMyUnitByLoc(Loc: Integer): PUn;
var
  I: Integer;
begin
  I := MyRO.nUn - 1;
  while (I >= 0) and (MyUn[I].Loc <> Loc) do Dec(I);
  if I >= 0 then Result := @MyUn[I]
    else Result := nil;
end;

function GetEnemyUnitByLoc(Loc: Integer): PUnitInfo;
var
  I: Integer;
begin
  I := MyRO.nEnemyUn - 1;
  while (I >= 0) and (MyRO.EnemyUn[I].Loc <> Loc) do Dec(I);
  if I >= 0 then Result := @MyRO.EnemyUn[I]
    else Result := nil;
end;


initialization

Assert(nImp < 128);
CalculateAdvValues;

end.
