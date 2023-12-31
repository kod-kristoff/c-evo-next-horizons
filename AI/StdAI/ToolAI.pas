{$INCLUDE Switches.inc}
unit ToolAI;

interface

uses
  SysUtils, Math,
{$IFDEF DEBUG}Names,{$ENDIF}
  Protocol, CustomAI;

type
  TGroupTransportPlan = record
    LoadLoc, uixTransport, nLoad, TurnsEmpty, TurnsLoaded: Integer;
    uixLoad: array[0..15] of Integer;
  end;


  TToolAI = class(TCustomAI)
  protected
  {$IFDEF DEBUG}DebugMap: array[0..lxmax * lymax - 1] of Integer;{$ENDIF}

    function CenterOfEmpire: Integer;
    // tile that is in the middle of all own cities

    function CityTaxBalance(cix: Integer; const CityReport: TCityReport): Integer;
    // calculates exact difference of income and maintenance cost for a single city
    // positive result = income higher than maintenance
    // negative result = income lower than maintenance
    // respects production and food converted to gold
    // CityReport must have been prepared before
    procedure SumCities(TaxRate: Integer; var TaxSum, ScienceSum: Integer);
    // calculates exact total tax and science income
    // tax is reduced by maintenance (so might be negative)
    // luxury not supported

    procedure OptimizeCityTiles;
    // obsolete; use City_OptimizeTiles instead

    procedure GetCityProdPotential;
    // calculates potential collected production resources of a city
    // result: list for all cities in CityResult
    procedure GetCityTradePotential;
    // calculates potential collected trade resources of a city
    // result: list for all cities in CityResult

    procedure JobAssignment_Initialize;
    // initialization, must be called first of the JobAssignment functions
    procedure JobAssignment_AddJob(Loc, Job, Score: Integer);
    // add job for settlers with certain score
    // jobs include founding cities!
    procedure JobAssignment_AddUnit(uix: Integer);
    // add a settler unit to do jobs
    procedure JobAssignment_Go;
    // to be called after all jobs and the settlers for them have been added
    // assigns each job to one settler, moves the settlers and makes them work
    // settlers prefer jobs which are closer to their current location and jobs with a higher score
    // starting a job one turn earlier counts the same as 4 points of score
    // function does not cancel jobs that are already started
    function JobAssignment_GotJob(uix: Integer): Boolean;
    // can be called after JobAssignment_Go to find out whether
    // a certain settler has been assigned a job to

    procedure AnalyzeMap;
    // calculates formations and districts

    function CheckStep(MoveStyle, TimeBeforeStep, CrossCorner: Integer;
      var TimeAfterStep, RecoverTurns: Integer; FromTile, ToTile: Integer;
      IsCapture: Boolean): Integer;
    // forecast single unit move between adjacent tiles
    // format of TimeBeforeStep and TimeAfterStep: $1000*number of turns + $800-MP left
    // RecoverTurns: number of turns needed to rest outside city in order to
    //   recover from damage taken in this move (rounded up)
    // FromTile and ToTile must be Map[FromLoc] and Map[ToLoc], no location codes
    // CrossCorner=1 for long moves that cross the tile corner, =0 for short ones that don't

    function GetMyMoveStyle(mix, Health: Integer): Integer;

    function Unit_MoveEx(uix, ToLoc: Integer; Options: Integer = 0): Integer;

    procedure SeaTransport_BeginInitialize;
    procedure SeaTransport_EndInitialize;
    // sea transport, obligatory call order:
    // 1. BeginInitialize
    // [2. AddLoad/AddTransport/AddDestination]
    // 3. EndInitialize
    // [4. MakeGroupPlan, MakeGroupPlan, MakeGroupPlan...]
    // don't use Pile between BeginInitialize and EndInitialize
    // sea transport only works well if
    // - all transports have same speed
    // - all transports have same capacity
    // - no transport is damaged
    procedure SeaTransport_AddLoad(uix: Integer);
    procedure SeaTransport_AddTransport(uix: Integer);
    procedure SeaTransport_AddDestination(Loc: Integer);
    function SeaTransport_MakeGroupPlan(var TransportPlan: TGroupTransportPlan): Boolean;
    // make plan for group of units to transport from a single loading location by a single transport
    // the plan optimizes:
    // - time for the units to move to the loading location
    // - time for the transport to move to the loading location
    // - time for the transport to move to one of the destination locations
    // after the plan is made, units and transport are removed from the pool, so that
    //   subsequent calls to MakeGroupPlan result in plans that may be executed parallel
    // function returns false if no more transports are possible

    function CurrentMStrength(Domain: Integer): Integer;
  end;

const
  // no-formations
  nfUndiscovered = -1;
  nfPole = -2;
  nfPeace = -3;

  // return codes of CheckStep
  csOk = 0;
  // step is valid
  // TimeAfterMove has been calculated
  csForbiddenTile = 1;
  // unit can not move onto this tile
  // TimeAfterMove not calculated
  csForbiddenStep = 2;
  // (ZoC unit only) unit can not do this step because of ZoC violation
  // maybe tile can be reached using another way
  // TimeAfterMove not calculated
  csCheckTerritory = 3;
  // move within other nations's territory shortly after making peace
  // step is only possible if RO.Territory is the same for both tiles
  // TimeAfterMove has been calculated

  // Unit_MoveEx
  mxAdjacent = $00000001;

var
  nContinent, nOcean, nDistrict: Integer;
  Formation: array[0..lxmax * lymax - 1] of Integer;
  // water: ocean index, land: continent index, sorted by size
  // territory unpassable due to peace treaty divides a continent
  District: array[0..lxmax * lymax - 1] of Integer;
  // index of coherent own territory, sorted by size
  CityResult: array[0..nCmax - 1] of Integer;

  Advancedness: array[0..nAdv - 1] of Integer;
// total number of prerequisites for each advance

implementation

uses
  Pile;

type
  pinteger = ^Integer;

var
  // for JobAssignment
  MaxScore: Integer;
  TileJob, TileJobScore: array[0..lxmax * lymax - 1] of Byte;
  JobLocOfSettler: array[0..nUmax - 1] of Integer; // ToAssign = find job

  // for Transport
  TransportMoveStyle, TransportCapacity, nTransportLoad: Integer;
  InitComplete, HaveDestinations: Boolean;
  uixTransportLoad, TransportAvailable: array[0..nUmax - 1] of Integer;
  TurnsAfterLoad: array[0..lxmax * lymax - 1] of ShortInt;

procedure ReplaceD(Start, Stop: pinteger; Raider, Twix: Integer);
begin
  while Start <> Stop do
  begin
    if Start^ = Raider then
      Start^ := Twix;
    Inc(Start);
  end;
end;

function NextZero(Start, Stop: pinteger; Mask: Cardinal): pinteger;
begin
  while (Start <> Stop) and (Start^ and Mask <> 0) do
    Inc(Start);
  Result := Start;
end;

function TToolAI.CenterOfEmpire: Integer;
var
  cix, Loc, X, Y, sy, N: Integer;
  A, su, sv: Double;
begin
  N := 0;
  sy := 0;
  su := 0;
  sv := 0;
  for cix := 0 to RO.nCity - 1 do
  begin
    Loc := MyCity[cix].Loc;
    if Loc >= 0 then
    begin
      Y := Loc div G.lx;
      X := Loc - Y * G.lx;
      Inc(sy, Y);
      A := 2 * pi * X / G.lx;
      su := su + cos(A);
      sv := sv + sin(A);
      Inc(N);
    end;
  end;
  A := arctan2(sv, su);
  X := round(G.lx * A / (2 * pi));
  while X >= G.lx do
    Dec(X, G.lx);
  while X < 0 do
    Inc(X, G.lx);
  Result := ((2 * sy + N) div (2 * N)) * G.lx + X;
end;

function TToolAI.CityTaxBalance(cix: Integer; const CityReport: TCityReport): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (CityReport.Working - CityReport.Happy <= MyCity[cix].Size shr 1) {no disorder} and
    (MyCity[cix].Flags and chCaptured = 0) then // not captured
  begin
    Inc(Result, CityReport.Tax);
    if (MyCity[cix].Project and (cpImp + cpIndex) = cpImp + imTrGoods) and
      (CityReport.ProdRep > CityReport.Support) then
      Inc(Result, CityReport.ProdRep - CityReport.Support);
    if ((RO.Government = gLybertarianism) or (MyCity[cix].Size >=
      NeedAqueductSize) and (CityReport.FoodRep < CityReport.Eaten + 2)) and
      (CityReport.FoodRep > CityReport.Eaten) then
      Inc(Result, CityReport.FoodRep - CityReport.Eaten);
  end;
  for I := nWonder to nImp - 1 do
    if MyCity[cix].Built[I] > 0 then
      Dec(Result, Imp[I].Maint);
end;

procedure TToolAI.SumCities(TaxRate: Integer; var TaxSum, ScienceSum: Integer);
var
  cix, p1: Integer;
  CityReport: TCityReport;
begin
  TaxSum := 0;
  ScienceSum := 0;
  if RO.Government = gAnarchy then
    Exit;
  for p1 := 0 to nPl - 1 do
    if RO.Tribute[p1] <= RO.TributePaid[p1] then
      // don't rely on tribute from bankrupt nations
      TaxSum := TaxSum + RO.Tribute[p1];
  for cix := 0 to RO.nCity - 1 do
    if MyCity[cix].Loc >= 0 then
    begin
      City_GetHypoReport(cix, -1, TaxRate, 0, CityReport);
      if (CityReport.Working - CityReport.Happy <= MyCity[cix].Size shr
        1) {no disorder} and (MyCity[cix].Flags and chCaptured = 0) then // not captured
        ScienceSum := ScienceSum + CityReport.Science;
      TaxSum := TaxSum + CityTaxBalance(cix, CityReport);
    end;
end;

//------------------------------------------------------------------------------
// City Tiles Processing

const
  pctOptimize = 0;
  pctGetProdPotential = 1;
  pctGetTradePotential = 2;

procedure TToolAI.OptimizeCityTiles;
var
  cix: Integer;
begin
  for cix := 0 to RO.nCity - 1 do
    with MyCity[cix] do
      if Loc >= 0 then
        City_OptimizeTiles(cix);
end;

procedure TToolAI.GetCityProdPotential;
var
  cix: Integer;
  Advice: TCityTileAdviceData;
begin
  for cix := 0 to RO.nCity - 1 do
    with MyCity[cix] do
      if Loc >= 0 then
      begin
        Advice.ResourceWeights := rwMaxProd;
        Server(sGetCityTileAdvice, Me, cix, Advice);
        CityResult[cix] := Advice.CityReport.ProdRep; // considers factory, but shouldn't
      end;
end;

procedure TToolAI.GetCityTradePotential;
var
  cix: Integer;
  Advice: TCityTileAdviceData;
begin
  for cix := 0 to RO.nCity - 1 do
    with MyCity[cix] do
      if Loc >= 0 then
      begin
        Advice.ResourceWeights := rwMaxScience;
        Server(sGetCityTileAdvice, Me, cix, Advice);
        CityResult[cix] := Advice.CityReport.Trade;
      end;
end;

//------------------------------------------------------------------------------
// JobAssignment

const
  ToAssign = lxmax * lymax;

procedure TToolAI.JobAssignment_Initialize;
begin
  FillChar(JobLocOfSettler, RO.nUn * SizeOf(Integer), $FF); // -1
  FillChar(TileJob, MapSize, jNone);
  FillChar(TileJobScore, MapSize, 0);
  MaxScore := 0;
end;

procedure TToolAI.JobAssignment_AddJob(Loc, Job, Score: Integer);
begin
  if Score > 255 then
    Score := 255;
  if Score > TileJobScore[Loc] then
  begin
    TileJob[Loc] := Job;
    TileJobScore[Loc] := Score;
    if Score > MaxScore then
      MaxScore := Score;
  end;
end;

procedure TToolAI.JobAssignment_AddUnit(uix: Integer);
begin
  Assert(MyModel[MyUnit[uix].mix].Kind in [mkSettler, mkSlaves]);
  JobLocOfSettler[uix] := ToAssign;
end;

function TToolAI.JobAssignment_GotJob(uix: Integer): Boolean;
begin
  Result := JobLocOfSettler[uix] >= 0;
end;

procedure TToolAI.JobAssignment_Go;
const
  DistanceScore = 4;
  StepSizeByTerrain: array[0..11] of Integer =
    (0, 0, 1, 2, 1, 1, 0, 1, 0, 1, 1, 2);
  //Oc-Sh-Gr-De-Pr-Tu-Ar-Sw-XX-Fo-Hi-Mo
var
  uix, BestScore, BestCount, BestLoc, BestJob, BestDistance, TestLoc,
  NextLoc, TestDistance, V8, TestScore, StepSize, MoveResult: Integer;
  UnitsToAssign: Boolean;
  Adjacent: TVicinity8Loc;
  SettlerOfJobLoc, DistToLoc: array[0..lxmax * lymax - 1] of smallint;
  // DistToLoc is only defined where SettlerOfJobLoc>=0
  TileChecked: array[0..lxmax * lymax - 1] of Boolean;
begin
  FillChar(SettlerOfJobLoc, MapSize * 2, $FF); // -1

  // keep up jobs that are already started
  for uix := 0 to RO.nUn - 1 do
    if (MyUnit[uix].Loc >= 0) and (MyUnit[uix].Job > jNone) then
    begin
      JobLocOfSettler[uix] := MyUnit[uix].Loc;
      SettlerOfJobLoc[MyUnit[uix].Loc] := uix;
      DistToLoc[MyUnit[uix].Loc] := 0;
    end;

  // assign remaining jobs to remaining settlers
  UnitsToAssign := True;
  while UnitsToAssign do
  begin
    UnitsToAssign := False;
    for uix := 0 to RO.nUn - 1 do
      if JobLocOfSettler[uix] = ToAssign then
      begin
        BestJob := jNone;
        BestScore := -999999;
        FillChar(TileChecked, MapSize * SizeOf(Boolean), False);
        Pile.Create(MapSize);
        Pile.Put(MyUnit[uix].Loc, 0); // start search for new job at current location
        while Pile.Get(TestLoc, TestDistance) do
        begin
          // add surrounding tiles to queue, but only if there's a chance to beat BestScore
          if MaxScore - DistanceScore * (TestDistance + 1) >= BestScore then
          begin
            V8_to_Loc(TestLoc, Adjacent);
            for V8 := 0 to 7 do
            begin
              NextLoc := Adjacent[V8];
              if (NextLoc >= 0) and not TileChecked[NextLoc] and
                (Map[NextLoc] and fTerrain <> fUNKNOWN) then
              begin
                StepSize := StepSizeByTerrain[Map[NextLoc] and fTerrain];
                if (StepSize > 0) // no water or arctic tile
                  and (Map[NextLoc] and (fUnit or fOwned) <> fUnit) // no foreign unit
                  and ((RO.Territory[NextLoc] < 0) or
                  (RO.Territory[NextLoc] = Me)) // no foreign territory
                  and (Map[TestLoc] and Map[NextLoc] and fInEnemyZoC = 0) then
                  // move not prevented by ZoC
                  Pile.Put(NextLoc, TestDistance + StepSize);
                // simplification, only optimal for 150 mp units in land with no roads
              end;
            end;
          end;

          // check tile for job
          if (TileJob[TestLoc] > jNone) and
            ((MyModel[MyUnit[uix].mix].Kind <> mkSlaves) or
            (TileJob[TestLoc] <> jCity)) and
            ((SettlerOfJobLoc[TestLoc] < 0) or (DistToLoc[TestLoc] > TestDistance)) then
          begin
            TestScore := Integer(TileJobScore[TestLoc]) - DistanceScore * TestDistance;
            if TestScore > BestScore then
              BestCount := 0;
            if TestScore >= BestScore then
            begin
              Inc(BestCount);
              if random(BestCount) = 0 then
              begin
                BestScore := TestScore;
                BestLoc := TestLoc;
                BestJob := TileJob[TestLoc];
                BestDistance := TestDistance;
              end;
            end;
          end;
          TileChecked[TestLoc] := True;
        end;
        Pile.Free;

        if BestJob > jNone then
        begin // new job found for this unit
          if SettlerOfJobLoc[BestLoc] >= 0 then
          begin // another unit was already assigned to this job, but is not as close -- reassign that unit!
            JobLocOfSettler[SettlerOfJobLoc[BestLoc]] := ToAssign;
            UnitsToAssign := True;
          end;
          JobLocOfSettler[uix] := BestLoc;
          SettlerOfJobLoc[BestLoc] := uix;
          DistToLoc[BestLoc] := BestDistance;
        end
        else
          JobLocOfSettler[uix] := -1; // no jobs for this settler
      end; // for uix
  end;

  // move settlers and start new jobs
  for uix := 0 to RO.nUn - 1 do
    with MyUnit[uix] do
      if (Loc >= 0) and (Job = jNone) and (JobLocOfSettler[uix] >= 0) then
      begin
        if Loc <> JobLocOfSettler[uix] then
          repeat
            MoveResult := Unit_Move(uix, JobLocOfSettler[uix])
          until (MoveResult < rExecuted) or (MoveResult and
              (rLocationReached or rMoreTurns or rUnitRemoved) <> 0);
        if (Loc = JobLocOfSettler[uix]) and (Movement >= 100) then
          Unit_StartJob(uix, TileJob[JobLocOfSettler[uix]]);
      end;
end;

//------------------------------------------------------------------------------
// Map Analysis

procedure TToolAI.AnalyzeMap;
var
  I, J, Loc, Loc1, V8, Count, Kind, MostIndex: Integer;
  Adjacent: TVicinity8Loc;
  IndexOfID: array[0..lxmax * lymax - 1] of smallint;
  IDOfIndex: array[0..lxmax * lymax div 2 - 1] of smallint;
begin
  FillChar(District, MapSize * 4, $FF);
  for Loc := 0 to MapSize - 1 do
    if Map[Loc] and fTerrain = fUNKNOWN then
      Formation[Loc] := nfUndiscovered
    else if Map[Loc] and fTerrain = fArctic then
      Formation[Loc] := nfPole
    else if Map[Loc] and fPeace <> 0 then
      Formation[Loc] := nfPeace
    else
    begin
      Formation[Loc] := Loc;
      V8_to_Loc(Loc, Adjacent);
      for V8 := 0 to 7 do
      begin
        Loc1 := Adjacent[V8];
        if (Loc1 < Loc) and (Loc1 >= 0) and (Formation[Loc1] >= 0) and
          ((Map[Loc1] and fTerrain >= fGrass) = (Map[Loc] and fTerrain >= fGrass)) then
          if Formation[Loc] = Loc then
            Formation[Loc] := Formation[Loc1]
          else if Formation[Loc] < Formation[Loc1] then
            ReplaceD(@Formation[Formation[Loc1]], @Formation[Loc + 1],
              Formation[Loc1], Formation[Loc])
          else if Formation[Loc] > Formation[Loc1] then
            ReplaceD(@Formation[Formation[Loc]], @Formation[Loc + 1],
              Formation[Loc], Formation[Loc1]);
      end;
      if (RO.Territory[Loc] = Me) and (Map[Loc] and fTerrain >= fGrass) then
      begin
        District[Loc] := Loc;
        for V8 := 0 to 7 do
        begin
          Loc1 := Adjacent[V8];
          if (Loc1 < Loc) and (Loc1 >= 0) and (District[Loc1] >= 0) then
            if District[Loc] = Loc then
              District[Loc] := District[Loc1]
            else if District[Loc] < District[Loc1] then
              ReplaceD(@District[District[Loc1]], @District[Loc + 1],
                District[Loc1], District[Loc])
            else if District[Loc] > District[Loc1] then
              ReplaceD(@District[District[Loc]], @District[Loc + 1],
                District[Loc], District[Loc1]);
        end;
      end;
    end;

  // sort continents, oceans and districts by size
  for Kind := 0 to 2 do
  begin
    FillChar(IndexOfID, MapSize * 2, 0);
    case Kind of
      0: // continents
        for Loc := 0 to MapSize - 1 do
          if (Formation[Loc] >= 0) and (Map[Loc] and fTerrain >= fGrass) then
            Inc(IndexOfID[Formation[Loc]]);
      1: // oceans
        for Loc := 0 to MapSize - 1 do
          if (Formation[Loc] >= 0) and (Map[Loc] and fTerrain < fGrass) then
            Inc(IndexOfID[Formation[Loc]]);
      2: // districts
        for Loc := 0 to MapSize - 1 do
          if District[Loc] >= 0 then
            Inc(IndexOfID[District[Loc]]);
    end;

    Count := 0;
    for Loc := 0 to MapSize - 1 do
      if IndexOfID[Loc] > 0 then
      begin
        IDOfIndex[Count] := Loc;
        Inc(Count);
      end;
    for I := 0 to Count - 2 do
    begin
      MostIndex := I;
      for J := I + 1 to Count - 1 do
        if IndexOfID[IDOfIndex[J]] > IndexOfID[IDOfIndex[MostIndex]] then
          MostIndex := J;
      if MostIndex <> I then
      begin
        J := IDOfIndex[I];
        IDOfIndex[I] := IDOfIndex[MostIndex];
        IDOfIndex[MostIndex] := J;
      end;
    end;
    for I := 0 to Count - 1 do
      IndexOfID[IDOfIndex[I]] := I;

    case Kind of
      0: // continents
      begin
        nContinent := Count;
        for Loc := 0 to MapSize - 1 do
          if (Formation[Loc] >= 0) and (Map[Loc] and fTerrain >= fGrass) then
            Formation[Loc] := IndexOfID[Formation[Loc]];
      end;
      1: // oceans
      begin
        nOcean := Count;
        for Loc := 0 to MapSize - 1 do
          if (Formation[Loc] >= 0) and (Map[Loc] and fTerrain < fGrass) then
            Formation[Loc] := IndexOfID[Formation[Loc]];
      end;
      2: // districts
      begin
        nDistrict := Count;
        for Loc := 0 to MapSize - 1 do
          if District[Loc] >= 0 then
            District[Loc] := IndexOfID[District[Loc]];
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Path Finding

const
  // basic move styles
  msGround = $00000000;
  msNoGround = $10000000;
  msAlpine = $20000000;
  msOver = $40000000;
  msSpy = $50000000;

  // other
  msHostile = $08000000;

// bits:   |31|30|29|28|27|26 .. 16|15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
// ground: |   Basic   |Ho| Speed  |       HeavyCost       |        RailCost       |
// other:  |   Basic   | 0| Speed  |              X X X             | MaxTerrType  |

function TToolAI.GetMyMoveStyle(mix, Health: Integer): Integer;
begin
  with MyModel[mix] do
  begin
    Result := Speed shl 16;
    case Domain of
      dGround:
      begin
        Inc(Result, (50 + (Speed - 150) * 13 shr 7) shl 8); //HeavyCost
        if RO.Wonder[woShinkansen].EffectiveOwner <> Me then
          Inc(Result, Speed * (4 * 1311) shr 17); // RailCost
        if (RO.Wonder[woGardens].EffectiveOwner <> Me) or
          (Kind = mkSettler) and (Speed >= 200) then
          Inc(Result, msHostile);
        if Kind = mkDiplomat then
          Inc(Result, msSpy)
        else if Cap[mcOver] > 0 then
          Inc(Result, msOver)
        else if Cap[mcAlpine] > 0 then
          Inc(Result, msAlpine)
        else
          Inc(Result, msGround);
      end;
      dSea:
      begin
        Result := Speed;
        if RO.Wonder[woMagellan].EffectiveOwner = Me then
          Inc(Result, 200);
        if Health < 100 then
          Result := ((Result - 250) * Health div 5000) * 50 + 250;
        Result := Result shl 16;
        Inc(Result, msNoGround);
        if Cap[mcNav] > 0 then
          Inc(Result);
      end;
      dAir:
        Inc(Result, msNoGround + fUNKNOWN xor 1 - 1);
    end;
  end;
end;

function TToolAI.CheckStep(MoveStyle, TimeBeforeStep, CrossCorner: Integer;
  var TimeAfterStep, RecoverTurns: Integer; FromTile, ToTile: Integer;
  IsCapture: Boolean): Integer;
var
  MoveCost, RecoverCost: Integer;
begin
  //IsCapture:=true;
  Assert(((FromTile and fTerrain <= fMountains) or (FromTile and
    fTerrain = fUNKNOWN)) and ((ToTile and fTerrain <= fMountains) or
    (ToTile and fTerrain = fUNKNOWN)));
  // do not pass location codes for FromTile and ToTile!
  RecoverTurns := 0;
  if MoveStyle < msGround + $10000000 then
  begin // common ground units
    if (ToTile + 1) and fTerrain < fGrass + 1 then
      Result := csForbiddenTile
    else if (ToTile and not FromTile and fPeace = 0) and
      (ToTile and (fUnit or fOwned) <> fUnit) and
      (IsCapture or (ToTile and (fCity or fOwned) <> fCity)) then
      if (FromTile and fCity <> 0) or (ToTile and (fCity or fOwned) = fCity or fOwned) or
        (ToTile and FromTile and (fInEnemyZoc or fOwnZoCUnit) <> fInEnemyZoc) then
      begin // ZoC is ok
        if (ToTile and (fRR or fCity) = 0) or (FromTile and (fRR or fCity) = 0) then
        begin // no railroad
          if (ToTile and (fRoad or fRR or fCity) <> 0) and
            (FromTile and (fRoad or fRR or fCity) <> 0) or
            (FromTile and ToTile and (fRiver or fCanal) <> 0) then
            MoveCost := 20 //move along road, river or canal
          else
          begin
            case Terrain[ToTile and fTerrain].MoveCost of
              1: MoveCost := 50; // plain terrain
              2: MoveCost := MoveStyle shr 8 and $FF; // heavy terrain
              else // mountains
              begin
                if TimeBeforeStep and $FFF + MoveStyle shr 16 and $7FF <= $800 then
                  TimeAfterStep := TimeBeforeStep and $7FFFF000 + $1800
                else
                  TimeAfterStep := TimeBeforeStep and $7FFFF000 + $2800;
                // must wait for next turn
                if (MoveStyle and msHostile <> 0) and
                  ((FromTile and (fTerrain or fSpecial1) = fDesert) or
                  (FromTile and fTerrain = fArctic)) and
                  (FromTile and (fCity or fRiver or fCanal) = 0) then
                begin
                  RecoverCost := ($800 - TimeBeforeStep and $FFF) * 5 shr 1;
                  while RecoverCost > 0 do
                  begin
                    Inc(RecoverTurns);
                    Dec(RecoverCost, MoveStyle shr 16 and $7FF);
                  end;
                end;
                Result := csOk;
                if ToTile and fPeace <> 0 then
                  Result := csCheckTerritory;
                Exit;
              end;
            end;
          end;
        end
        else
          MoveCost := MoveStyle and $FF; //move along railroad

        Inc(MoveCost, MoveCost shl CrossCorner);
        if (MoveStyle and msHostile = 0) or
          (ToTile and (fTerrain or fSpecial1) <> fDesert) and
          (ToTile and fTerrain <> fArctic) or (ToTile and
          (fCity or fRiver or fCanal) <> 0) or (ToTile and fTerImp = tiBase) then
          RecoverCost := 0
        else
          RecoverCost := (MoveCost * 5) shr 1;
        // damage from movement: MoveCost*DesertThurst/NoCityRecovery
        if (TimeBeforeStep and $FFF + MoveCost <= $800) and
          (TimeBeforeStep and $FFF < $800) then
          TimeAfterStep := TimeBeforeStep + MoveCost
        else
        begin
          TimeAfterStep := TimeBeforeStep and $7FFFF000 + $1800 -
            MoveStyle shr 16 and $7FF + MoveCost; // must wait for next turn
          if (MoveStyle and msHostile <> 0) and
            ((FromTile and (fTerrain or fSpecial1) = fDesert) or
            (FromTile and fTerrain = fArctic)) and
            (FromTile and (fCity or fRiver or fCanal) = 0) and
            (FromTile and fTerImp <> tiBase) then
            Inc(RecoverCost, ($800 - TimeBeforeStep and $FFF) * 5 shr 1);
        end;
        while RecoverCost > 0 do
        begin
          Inc(RecoverTurns);
          Dec(RecoverCost, MoveStyle shr 16 and $7FF);
        end;
        Result := csOk;
        if ToTile and fPeace <> 0 then
          Result := csCheckTerritory;
      end
      else
        Result := csForbiddenStep // ZoC violation
    else
      Result := csForbiddenTile;
  end

  else if MoveStyle < msNoGround + $10000000 then
  begin // ships and aircraft
    if ((ToTile and fTerrain xor 1 > MoveStyle and fTerrain) and
      (ToTile and (fCity or fCanal) = 0)) or (ToTile and not FromTile and fPeace <> 0) or
      (ToTile and (fUnit or fOwned) = fUnit) or (ToTile and
      (fCity or fOwned) = fCity) then
      Result := csForbiddenTile
    else
    begin
      MoveCost := 50 shl CrossCorner + 50;
      if TimeBeforeStep and $FFF + MoveCost <= $800 then
        TimeAfterStep := TimeBeforeStep + MoveCost
      else
        TimeAfterStep := TimeBeforeStep and $7FFFF000 + $1800 -
          MoveStyle shr 16 and $7FF + MoveCost;
      // must wait for next turn
      Result := csOk;
      if ToTile and fPeace <> 0 then
        Result := csCheckTerritory;
    end;
  end

  else if MoveStyle < msAlpine + $10000000 then
  begin // alpine
    if (ToTile + 1) and fTerrain < fGrass + 1 then
      Result := csForbiddenTile
    else if (ToTile and not FromTile and fPeace = 0) and
      (ToTile and (fUnit or fOwned) <> fUnit) and
      (IsCapture or (ToTile and (fCity or fOwned) <> fCity)) then
      if (FromTile and fCity <> 0) or (ToTile and (fCity or fOwned) = fCity or fOwned) or
        (ToTile and FromTile and (fInEnemyZoc or fOwnZoCUnit) <> fInEnemyZoc) then
      begin
        if (ToTile and (fRR or fCity) = 0) or (FromTile and (fRR or fCity) = 0) then
          MoveCost := 20 // no railroad
        else
          MoveCost := MoveStyle and $FF; //move along railroad
        Inc(MoveCost, MoveCost shl CrossCorner);
        if (TimeBeforeStep and $FFF + MoveCost <= $800) and
          (TimeBeforeStep and $FFF < $800) then
          TimeAfterStep := TimeBeforeStep + MoveCost
        else
          TimeAfterStep := TimeBeforeStep and $7FFFF000 + $1800 -
            MoveStyle shr 16 and $7FF + MoveCost;
        // must wait for next turn
        Result := csOk;
        if ToTile and fPeace <> 0 then
          Result := csCheckTerritory;
      end
      else
        Result := csForbiddenStep // ZoC violation
    else
      Result := csForbiddenTile;
  end

  else if MoveStyle < msOver + $10000000 then
  begin // overweight
    if (ToTile + 1) and fTerrain < fGrass + 1 then
      Result := csForbiddenTile
    else if (ToTile and not FromTile and fPeace = 0) and
      (ToTile and (fUnit or fOwned) <> fUnit) and
      (IsCapture or (ToTile and (fCity or fOwned) <> fCity)) then
      if (FromTile and fCity <> 0) or (ToTile and (fCity or fOwned) = fCity or fOwned) or
        (ToTile and FromTile and (fInEnemyZoc or fOwnZoCUnit) <> fInEnemyZoc) then
      begin
        if (ToTile and (fRR or fCity) = 0) or (FromTile and (fRR or fCity) = 0) then
        begin // no railroad
          if (ToTile and (fRoad or fRR or fCity) <> 0) and
            (FromTile and (fRoad or fRR or fCity) <> 0) or
            (FromTile and ToTile and (fRiver or fCanal) <> 0) then
            MoveCost := 40 //move along road, river or canal
          else
          begin
            Result := csForbiddenTile;
            Exit;
          end;
        end
        else
          MoveCost := MoveStyle and $FF; //move along railroad
        Inc(MoveCost, MoveCost shl CrossCorner);
        if (TimeBeforeStep and $FFF + MoveCost <= $800) and
          (TimeBeforeStep and $FFF < $800) then
          TimeAfterStep := TimeBeforeStep + MoveCost
        else
          TimeAfterStep := TimeBeforeStep and $7FFFF000 + $1800 -
            MoveStyle shr 16 and $7FF + MoveCost;
        // must wait for next turn
        Result := csOk;
        if ToTile and fPeace <> 0 then
          Result := csCheckTerritory;
      end
      else
        Result := csForbiddenStep // ZoC violation
    else
      Result := csForbiddenTile;
  end

  else {if MoveStyle<msSpy+$10000000 then}
  begin // spies
    if (ToTile + 1) and fTerrain < fGrass + 1 then
      Result := csForbiddenTile
    else if (ToTile and (fUnit or fOwned) <> fUnit) and
      (IsCapture or (ToTile and (fCity or fOwned) <> fCity)) then
    begin
      if (ToTile and (fRR or fCity) = 0) or (FromTile and (fRR or fCity) = 0) then
      begin // no railroad
        if (ToTile and (fRoad or fRR or fCity) <> 0) and
          (FromTile and (fRoad or fRR or fCity) <> 0) or
          (FromTile and ToTile and (fRiver or fCanal) <> 0) then
          MoveCost := 20 //move along road, river or canal
        else
        begin
          case Terrain[ToTile and fTerrain].MoveCost of
            1: MoveCost := 50; // plain terrain
            2: MoveCost := MoveStyle shr 8 and $FF; // heavy terrain
            else // mountains
            begin
              if TimeBeforeStep and $FFF + MoveStyle shr 16 and $7FF <= $800 then
                TimeAfterStep := TimeBeforeStep and $7FFFF000 + $1800
              else
                TimeAfterStep := TimeBeforeStep and $7FFFF000 + $2800;
              // must wait for next turn
              Result := csOk;
              Exit;
            end;
          end;
        end;
      end
      else
        MoveCost := MoveStyle and $FF; //move along railroad
      Inc(MoveCost, MoveCost shl CrossCorner);
      if (TimeBeforeStep and $FFF + MoveCost <= $800) and
        (TimeBeforeStep and $FFF < $800) then
        TimeAfterStep := TimeBeforeStep + MoveCost
      else
        TimeAfterStep := TimeBeforeStep and $7FFFF000 + $1800 -
          MoveStyle shr 16 and $7FF + MoveCost;
      // must wait for next turn
      Result := csOk;
    end
    else
      Result := csForbiddenTile;
  end;
end;

(*
-------- Pathfinding Reference Implementation --------
var
MoveStyle,V8,Loc,Time,NextLoc,NextTime,RecoverTurns: Integer;
Adjacent: TVicinity8Loc;
Reached: array[0..lxmax*lymax-1] of Boolean;
begin
FillChar(Reached, MapSize, False);
MoveStyle:=GetMyMoveStyle(MyUnit[uix].mix, MyUnit[uix].Health);
Pile.Create(MapSize);
Pile.Put(MyUnit[uix].Loc, $800-MyUnit[uix].Movement);
while Pile.Get(Loc, Time) do
  begin
  // todo: check exit condition, e.g. whether destination reached

  Reached[Loc]:=True;
  V8_to_Loc(Loc, Adjacent);
  for V8:=0 to 7 do
    begin
    NextLoc:=Adjacent[V8];
    if (NextLoc>=0) and not Reached[NextLoc] then
      case CheckStep(MoveStyle, Time, V8 and 1, NextTime, RecoverTurns, Map[Loc], Map[NextLoc]) of
        csOk:
          Pile.Put(NextLoc, NextTime+RecoverTurns*$1000);
        csForbiddenTile:
          Reached[NextLoc]:=True; // don't check moving there again
        csCheckTerritory:
          if RO.Territory[NextLoc]=RO.Territory[Loc] then
            Pile.Put(NextLoc, NextTime+RecoverTurns*$1000);
        end
    end;
  end;
Pile.Free;
end;
*)

function TToolAI.Unit_MoveEx(uix, ToLoc: Integer; Options: Integer): Integer;
var
  Loc, NextLoc, Temp, FromLoc, EndLoc, Time, V8, MoveResult, RecoverTurns,
  NextTime, MoveStyle: Integer;
  Adjacent: TVicinity8Loc;
  PreLoc: array[0..lxmax * lymax - 1] of Integer;
  Reached: array[0..lxmax * lymax - 1] of Boolean;
begin
  Result := eOk;
  FromLoc := MyUnit[uix].Loc;
  if FromLoc = ToLoc then
    Exit;

  FillChar(Reached, MapSize, False);
  MoveStyle := GetMyMoveStyle(MyUnit[uix].mix, MyUnit[uix].Health);
  EndLoc := -1;
  Pile.Create(MapSize);
  Pile.Put(FromLoc, $800 - MyUnit[uix].Movement);
  while Pile.Get(Loc, Time) do
  begin
    if (Loc = ToLoc) or (ToLoc = maNextCity) and (Map[Loc] and fCity <> 0) and
      (Map[Loc] and fOwned <> 0) then
    begin
      EndLoc := Loc;
      Break;
    end;
    Reached[Loc] := True;
    V8_to_Loc(Loc, Adjacent);
    for V8 := 0 to 7 do
    begin
      NextLoc := Adjacent[V8];
      if NextLoc >= 0 then
        if (NextLoc = ToLoc) and (Options and mxAdjacent <> 0) then
        begin
          EndLoc := Loc;
          Break;
        end
        else if not Reached[NextLoc] then
        begin
          case CheckStep(MoveStyle, Time, V8 and 1, NextTime, RecoverTurns,
              Map[Loc], Map[NextLoc], NextLoc = ToLoc) of
            csOk:
              if Pile.Put(NextLoc, NextTime + RecoverTurns * $1000) then
                PreLoc[NextLoc] := Loc;
            csForbiddenTile:
              Reached[NextLoc] := True; // don't check moving there again
            csCheckTerritory:
              if RO.Territory[NextLoc] = RO.Territory[Loc] then
                if Pile.Put(NextLoc, NextTime + RecoverTurns * $1000) then
                  PreLoc[NextLoc] := Loc;
          end;
        end;
    end;
    if EndLoc >= 0 then
      Break;
  end;
  Pile.Free;

  if EndLoc >= 0 then
  begin
    Loc := EndLoc;
    NextLoc := PreLoc[Loc];
    while Loc <> FromLoc do
    begin // invert meaning of PreLoc
      Temp := Loc;
      Loc := NextLoc;
      NextLoc := PreLoc[Loc];
      PreLoc[Loc] := Temp;
    end;
    while Loc <> EndLoc do
    begin
      Loc := PreLoc[Loc];
      MoveResult := Unit_Step(uix, Loc);
      if (MoveResult <> eOK) and (MoveResult <> eLoaded) then
      begin
        Result := MoveResult;
        Break;
      end;
    end;
  end
  else
    Result := eNoWay;
end;

//------------------------------------------------------------------------------
// Oversea Transport

procedure TToolAI.SeaTransport_BeginInitialize;
begin
  FillChar(TransportAvailable, RO.nUn * SizeOf(Integer), $FF); // -1
  InitComplete := False;
  HaveDestinations := False;
  nTransportLoad := 0;
  TransportMoveStyle := 0;
  TransportCapacity := $100;
  Pile.Create(MapSize);
end;

procedure TToolAI.SeaTransport_AddLoad(uix: Integer);
var
  I: Integer;
begin
  Assert(not InitComplete); // call order violation!
  if Map[MyUnit[uix].Loc] and fTerrain < fGrass then
    Exit;
  for I := 0 to nTransportLoad - 1 do
    if uix = uixTransportLoad[I] then
      Exit;
  uixTransportLoad[nTransportLoad] := uix;
  Inc(nTransportLoad);
end;

procedure TToolAI.SeaTransport_AddTransport(uix: Integer);
var
  MoveStyle: Integer;
begin
  Assert(not InitComplete); // call order violation!
  Assert(MyModel[MyUnit[uix].mix].Cap[mcSeaTrans] > 0);
  TransportAvailable[uix] := 1;
  with MyModel[MyUnit[uix].mix] do
  begin
    if MTrans * Cap[mcSeaTrans] < TransportCapacity then
      TransportCapacity := MTrans * Cap[mcSeaTrans];
    MoveStyle := GetMyMoveStyle(MyUnit[uix].mix, 100);
    if (TransportMoveStyle = 0) or (MoveStyle < TransportMoveStyle) and
      (MoveStyle and not TransportMoveStyle and 1 = 0) or
      (not MoveStyle and TransportMoveStyle and 1 <> 0) then
      TransportMoveStyle := MoveStyle;
  end;
end;

procedure TToolAI.SeaTransport_AddDestination(Loc: Integer);
begin
  Assert(not InitComplete); // call order violation!
  Pile.Put(Loc, $800);
  HaveDestinations := True;
end;

procedure TToolAI.SeaTransport_EndInitialize;
var
  Loc0, Time0, V8, Loc1, ArriveTime, RecoverTurns: Integer;
  Adjacent: TVicinity8Loc;
begin
  Assert(not InitComplete); // call order violation!
  InitComplete := True;
  if HaveDestinations then
  begin // calculate TurnsAfterLoad from destination locs
    FillChar(TurnsAfterLoad, MapSize, $FF); // -1
    while Pile.Get(Loc0, Time0) do
    begin // search backward
      if Time0 = $800 then
        TurnsAfterLoad[Loc0] := 1
      else
        TurnsAfterLoad[Loc0] := Time0 shr 12;
      V8_to_Loc(Loc0, Adjacent);
      for V8 := 0 to 7 do
      begin
        Loc1 := Adjacent[V8];
        if (Loc1 >= 0) and (TurnsAfterLoad[Loc1] = -1) then
        begin
          case CheckStep(TransportMoveStyle, Time0, V8 and 1, ArriveTime,
              RecoverTurns, Map[Loc0], Map[Loc1], False) of
            csOk: Pile.Put(Loc1, ArriveTime);
            csForbiddenStep: TurnsAfterLoad[Loc1] := -2;
          end;
        end;
      end;
    end;
  end;
  Pile.Free;
end;

function TToolAI.SeaTransport_MakeGroupPlan(
  var TransportPlan: TGroupTransportPlan): Boolean;
var
  V8, I, J, iPicked, uix, Loc0, Time0, Loc1, RecoverTurns, MoveStyle,
  TurnsLoaded, TurnCount, tuix, tuix1, ArriveTime, TotalDelay,
  BestTotalDelay, GroupCount, BestGroupCount, BestLoadLoc, FullMovementLoc,
  nSelectedLoad, F, OriginContinent, A, B: Integer;
  CompleteFlag, NotReachedFlag, ContinueUnit: Cardinal;
  IsComplete, ok, IsFirstLoc: Boolean;
  StartLocPtr, ArrivedEnd: pinteger;
  Adjacent: TVicinity8Loc;
  uixSelectedLoad: array[0..15] of Integer;
  tuixSelectedLoad: array[0..15] of Integer;
  Arrived: array[0..lxmax * lymax] of Cardinal;
  ResponsibleTransport: array[0..lxmax * lymax - 1] of smallint;
  TurnsBeforeLoad: array[0..lxmax * lymax - 1] of ShortInt;
  GroupComplete: array[0..lxmax * lymax - 1] of Boolean;
begin
  Assert(InitComplete); // call order violation!

  if HaveDestinations and (nTransportLoad > 0) then
  begin // transport and units already adjacent?
    for uix := 0 to RO.nUn - 1 do
      if (TransportAvailable[uix] > 0) and (Map[MyUnit[uix].Loc] and
        fTerrain = fShore) then
      begin
        GroupCount := 0;
        for tuix := 0 to nTransportLoad - 1 do
        begin
          Loc_to_ab(MyUnit[uix].Loc, MyUnit[uixTransportLoad[tuix]].Loc, A, B);
          if (abs(A) <= 1) and (abs(B) <= 1) then
          begin
            Assert((A <> 0) or (B <> 0));
            Inc(GroupCount);
          end;
        end;
        if (GroupCount = nTransportLoad) or (GroupCount >= TransportCapacity) then
        begin
          TransportPlan.LoadLoc := MyUnit[uix].Loc;
          TransportPlan.uixTransport := uix;
          TransportAvailable[uix] := 0;
          TransportPlan.TurnsEmpty := 0;
          TransportPlan.TurnsLoaded := TurnsAfterLoad[TransportPlan.LoadLoc];
          TransportPlan.nLoad := 0;
          for tuix := nTransportLoad - 1 downto 0 do
          begin
            Loc_to_ab(TransportPlan.LoadLoc, MyUnit[uixTransportLoad[tuix]].Loc, A, B);
            if (abs(A) <= 1) and (abs(B) <= 1) then
            begin
              TransportPlan.uixLoad[TransportPlan.nLoad] := uixTransportLoad[tuix];
              uixTransportLoad[tuix] := uixTransportLoad[nTransportLoad - 1];
              Dec(nTransportLoad);
              Inc(TransportPlan.nLoad);
              if TransportPlan.nLoad = TransportCapacity then
                Break;
            end;
          end;
          Result := True;
          Exit;
        end;
      end;
  end;

  while HaveDestinations and (nTransportLoad > 0) do
  begin
    // select units from same continent
    FillChar(Arrived, 4 * nContinent, 0); // misuse Arrived as counter
    for tuix := 0 to nTransportLoad - 1 do
    begin
      Assert(Map[MyUnit[uixTransportLoad[tuix]].Loc] and fTerrain >= fGrass);
      F := Formation[MyUnit[uixTransportLoad[tuix]].Loc];
      if F >= 0 then
        Inc(Arrived[F]);
    end;
    OriginContinent := 0;
    for F := 1 to nContinent - 1 do
      if Arrived[F] > Arrived[OriginContinent] then
        OriginContinent := F;
    nSelectedLoad := 0;
    for tuix := 0 to nTransportLoad - 1 do
      if Formation[MyUnit[uixTransportLoad[tuix]].Loc] = OriginContinent then
      begin
        tuixSelectedLoad[nSelectedLoad] := tuix;
        uixSelectedLoad[nSelectedLoad] := uixTransportLoad[tuix];
        Inc(nSelectedLoad);
        if nSelectedLoad = 16 then
          Break;
      end;

    Pile.Create(MapSize);
    FillChar(ResponsibleTransport, MapSize * 2, $FF); // -1
    FillChar(TurnsBeforeLoad, MapSize, $FF); // -1
    ok := False;
    for uix := 0 to RO.nUn - 1 do
      if TransportAvailable[uix] > 0 then
      begin
        ok := True;
        Pile.Put(MyUnit[uix].Loc, ($800 - MyUnit[uix].Movement) shl 12 + uix);
      end;
    if not ok then // no transports
    begin
      TransportPlan.LoadLoc := -1;
      Result := False;
      Pile.Free;
      Exit;
    end;
    while Pile.Get(Loc0, Time0) do
    begin
      uix := Time0 and $FFF;
      Time0 := Time0 shr 12;
      ResponsibleTransport[Loc0] := uix;
      TurnsBeforeLoad[Loc0] := Time0 shr 12;
      V8_to_Loc(Loc0, Adjacent);
      for V8 := 0 to 7 do
      begin
        Loc1 := Adjacent[V8];
        if (Loc1 >= 0) and (ResponsibleTransport[Loc1] < 0) then
          case CheckStep(GetMyMoveStyle(MyUnit[uix].mix, MyUnit[uix].Health),
              Time0, V8 and 1, ArriveTime, RecoverTurns, Map[Loc0], Map[Loc1], False) of
            csOk: Pile.Put(Loc1, ArriveTime shl 12 + uix);
            csForbiddenTile: ResponsibleTransport[Loc1] := RO.nUn; // don't check again
          end;
      end;
    end;

    FillChar(Arrived, MapSize * 4, $55); // set NotReachedFlag for all tiles
    FillChar(GroupComplete, MapSize, False);
    BestLoadLoc := -1;

    // check direct loading
    for tuix := 0 to nSelectedLoad - 1 do
    begin
      uix := uixSelectedLoad[tuix];
      if MyUnit[uix].Movement = Integer(MyModel[MyUnit[uix].mix].Speed) then
      begin
        NotReachedFlag := 1 shl (2 * tuix);
        CompleteFlag := NotReachedFlag shl 1;
        V8_to_Loc(MyUnit[uix].Loc, Adjacent);
        for V8 := 0 to 7 do
        begin
          Loc1 := Adjacent[V8];
          if (Loc1 >= 0) and (Map[Loc1] and fTerrain < fGrass) and
            not GroupComplete[Loc1] then
          begin // possible transport start location
            Arrived[Loc1] := (Arrived[Loc1] or CompleteFlag) and not NotReachedFlag;
            if (TurnsBeforeLoad[Loc1] >= 0) and (TurnsAfterLoad[Loc1] >= 0) then
            begin
              I := 1;
              GroupCount := 0;
              for tuix1 := 0 to nSelectedLoad - 1 do
              begin
                if Arrived[loc1] and I = 0 then
                  Inc(GroupCount);
                I := I shl 2;
              end;
              Assert(GroupCount <= TransportCapacity);
              if (GroupCount = TransportCapacity) or (GroupCount = nSelectedLoad) then
                GroupComplete[loc1] := True;
              TotalDelay := TurnsBeforeLoad[Loc1] + TurnsAfterLoad[Loc1];
              if (BestLoadLoc < 0) or (GroupCount shl 16 -
                TotalDelay > BestGroupCount shl 16 - BestTotalDelay) then
              begin
                BestLoadLoc := Loc1;
                BestGroupCount := GroupCount;
                BestTotalDelay := TotalDelay;
              end;
            end;
          end;
        end;
      end;
    end;

    TurnCount := 0;
    ArrivedEnd := @Arrived[MapSize];

    // check moving+loading
    ContinueUnit := 1 shl nSelectedLoad - 1;
    while (ContinueUnit > 0) and ((BestLoadLoc < 0) or
        (TurnCount < BestTotalDelay - 2)) do
    begin
      for tuix := 0 to nSelectedLoad - 1 do
        if 1 shl tuix and ContinueUnit <> 0 then
        begin
          uix := uixSelectedLoad[tuix];
          MoveStyle := GetMyMoveStyle(MyUnit[uix].mix, MyUnit[uix].Health);
          NotReachedFlag := 1 shl (2 * tuix);
          CompleteFlag := NotReachedFlag shl 1;
          FullMovementLoc := -1;

          Pile.Empty;
          if TurnCount = 0 then
          begin
            Pile.Put(MyUnit[uix].Loc, $1800 - MyUnit[uix].Movement);
            if MyUnit[uix].Movement = Integer(MyModel[MyUnit[uix].mix].Speed) then
              FullMovementLoc := MyUnit[uix].Loc;
            // surrounding tiles can be loaded immediately
            StartLocPtr := ArrivedEnd;
          end
          else
            StartLocPtr := @Arrived;
          IsFirstLoc := True;

          repeat
            if StartLocPtr <> ArrivedEnd then
              // search next movement start location for this turn
              StartLocPtr := NextZero(StartLocPtr, ArrivedEnd,
                CompleteFlag or NotReachedFlag);
            if StartLocPtr <> ArrivedEnd then
            begin
              Loc0 := (Integer(StartLocPtr) - Integer(@Arrived)) shr 2;
              Inc(StartLocPtr);
              Time0 := $800;
            end
            else if not Pile.Get(Loc0, Time0) then
            begin
              if IsFirstLoc then
                ContinueUnit := ContinueUnit and not (1 shl tuix);
              Break;
            end;
            IsFirstLoc := False;

            Arrived[Loc0] := Arrived[Loc0] and not NotReachedFlag;
            if not GroupComplete[Loc0] and (Map[Loc0] and fTerrain <> fMountains) then
            begin // check whether group complete -- no mountains because complete flag might be faked there
              I := 1;
              GroupCount := 0;
              for tuix1 := 0 to nSelectedLoad - 1 do
              begin
                if Arrived[Loc0] and I = 0 then
                  Inc(GroupCount);
                I := I shl 2;
              end;
              Assert(GroupCount <= TransportCapacity);
              if (GroupCount = TransportCapacity) or (GroupCount = nSelectedLoad) then
                GroupComplete[Loc0] := True;
            end;

            V8_to_Loc(Loc0, Adjacent);
            IsComplete := True;
            for V8 := 0 to 7 do
            begin
              Loc1 := Adjacent[V8];
              if (Loc1 < G.ly) or (Loc1 >= MapSize - G.ly) then
                Adjacent[V8] := -1 // pole, don't consider moving here
              else if Arrived[Loc1] and NotReachedFlag = 0 then
                Adjacent[V8] := -1 // unit has already arrived this tile
              else if GroupComplete[Loc1] then
                Adjacent[V8] := -1 // already other group complete
              else if Map[Loc1] and fTerrain < fGrass then
              begin // possible transport start location
                Arrived[Loc1] := (Arrived[Loc1] or CompleteFlag) and not NotReachedFlag;
                Adjacent[V8] := -1;
                if (TurnsBeforeLoad[Loc1] >= 0) and (TurnsAfterLoad[Loc1] >= 0) then
                begin
                  I := 1;
                  GroupCount := 0;
                  for tuix1 := 0 to nSelectedLoad - 1 do
                  begin
                    if Arrived[loc1] and I = 0 then
                      Inc(GroupCount);
                    I := I shl 2;
                  end;
                  Assert(GroupCount <= TransportCapacity);
                  if (GroupCount = TransportCapacity) or
                    (GroupCount = nSelectedLoad) then
                    GroupComplete[loc1] := True;
                  if TurnsBeforeLoad[Loc1] > TurnCount + 1 then
                    TotalDelay := TurnsBeforeLoad[Loc1] + TurnsAfterLoad[Loc1]
                  else
                    TotalDelay := TurnCount + 1 + TurnsAfterLoad[Loc1];
                  if (BestLoadLoc < 0) or (GroupCount shl
                    16 - TotalDelay > BestGroupCount shl 16 - BestTotalDelay) then
                  begin
                    BestLoadLoc := Loc1;
                    BestGroupCount := GroupCount;
                    BestTotalDelay := TotalDelay;
                  end;
                end;
              end
              else if (Map[Loc1] and fTerrain = fMountains) and
                ((Map[Loc0] and (fRoad or fRR or fCity) = 0) or
                (Map[Loc1] and (fRoad or fRR or fCity) = 0)) and
                (Map[Loc0] and Map[Loc1] and (fRiver or fCanal) = 0) then
              begin // mountain delay too complicated for this algorithm
                Arrived[Loc1] := (Arrived[Loc1] or CompleteFlag) and not NotReachedFlag;
                Adjacent[V8] := -1;
              end
              else
                IsComplete := False;
            end;
            if IsComplete then
            begin
              Arrived[Loc0] := (Arrived[Loc0] or CompleteFlag) and not NotReachedFlag;
              continue;
            end;
            IsComplete := True;
            for V8 := 0 to 7 do
            begin
              Loc1 := Adjacent[V8];
              if Loc1 >= 0 then
              begin
                ok := False;
                case CheckStep(MoveStyle, Time0, V8 and 1, ArriveTime,
                    RecoverTurns, Map[Loc0], Map[Loc1], False) of
                  csOk: ok := True;
                  csForbiddenTile:
                    ;// !!! don't check moving there again
                  csCheckTerritory:
                    ok := RO.Territory[Loc1] = RO.Territory[Loc0];
                end;
                if ok and Pile.TestPut(Loc1, ArriveTime) then
                  if ArriveTime < $2000 then
                    Pile.Put(Loc1, ArriveTime)
                  else
                    IsComplete := False;
              end;
            end;
            if IsComplete then
              Arrived[Loc0] := (Arrived[Loc0] or CompleteFlag) and not NotReachedFlag;
          until False;
        end;

      Inc(TurnCount);
    end;
    Pile.Free;

    if BestLoadLoc >= 0 then
    begin
      TransportPlan.LoadLoc := BestLoadLoc;
      TransportPlan.uixTransport := ResponsibleTransport[BestLoadLoc];
      TransportAvailable[TransportPlan.uixTransport] := 0;
      TransportPlan.TurnsEmpty := BestTotalDelay - TurnsAfterLoad[BestLoadLoc];
      TransportPlan.TurnsLoaded := TurnsAfterLoad[BestLoadLoc];
      TransportPlan.nLoad := 0;
      for tuix := nSelectedLoad - 1 downto 0 do
        if 1 shl (2 * tuix) and Arrived[BestLoadLoc] = 0 then
        begin
          Assert(uixTransportLoad[tuixSelectedLoad[tuix]] = uixSelectedLoad[tuix]);
          TransportPlan.uixLoad[TransportPlan.nLoad] := uixSelectedLoad[tuix];
          uixTransportLoad[tuixSelectedLoad[tuix]] :=
            uixTransportLoad[nTransportLoad - 1];
          Dec(nTransportLoad);
          Inc(TransportPlan.nLoad);
        end;
      Result := True;
      Exit;
    end;

    // no loading location for a single of these units -- remove all
    // should be pretty rare case
    for tuix := nSelectedLoad - 1 downto 0 do
    begin
      Assert(uixTransportLoad[tuixSelectedLoad[tuix]] = uixSelectedLoad[tuix]);
      uixTransportLoad[tuixSelectedLoad[tuix]] :=
        uixTransportLoad[nTransportLoad - 1];
      Dec(nTransportLoad);
    end;
  end;
  TransportPlan.LoadLoc := -1;
  Result := False;
end;

//------------------------------------------------------------------------------
// Misc

function TToolAI.CurrentMStrength(Domain: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to nUpgrade - 1 do
    with upgrade[Domain, I] do
      if (Preq = preNone) or (Preq >= 0) and
        ((RO.Tech[Preq] >= tsApplicable) or (Preq in FutureTech) and
        (RO.Tech[Preq] >= 0)) then
      begin
        if Preq in FutureTech then
          Inc(Result, RO.Tech[Preq] * Strength)
        else
          Inc(Result, Strength);
      end;
end;

//------------------------------------------------------------------------------

procedure SetAdvancedness;
var
  ad, J, Reduction, AgeThreshold: Integer;
  known: array[0..nAdv - 1] of Integer;

  procedure MarkPreqs(ad: Integer);
  var
    I: Integer;
  begin
    if known[ad] = 0 then
    begin
      known[ad] := 1;
      for I := 0 to 2 do
        if AdvPreq[ad, I] >= 0 then
          MarkPreqs(AdvPreq[ad, I]);
    end;
  end;

begin
  FillChar(Advancedness, SizeOf(Advancedness), 0);
  for ad := 0 to nAdv - 1 do
  begin
    FillChar(known, SizeOf(known), 0);
    MarkPreqs(ad);
    for J := 0 to nAdv - 1 do
      if known[J] > 0 then
        Inc(Advancedness[ad]);
  end;
  AgeThreshold := Advancedness[adScience];
  Reduction := Advancedness[adScience] div 3;
  for ad := 0 to nAdv - 5 do
    if Advancedness[ad] >= AgeThreshold then
      Dec(Advancedness[ad], Reduction);
  AgeThreshold := Advancedness[adMassProduction];
  Reduction := (Advancedness[adMassProduction] - Advancedness[adScience]) div 3;
  for ad := 0 to nAdv - 5 do
    if Advancedness[ad] >= AgeThreshold then
      Dec(Advancedness[ad], Reduction);
end;


initialization
  SetAdvancedness;

end.
