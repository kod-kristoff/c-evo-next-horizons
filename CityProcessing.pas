{$INCLUDE Switches.inc}
unit CityProcessing;

interface

uses
  Protocol, Database;

// Reporting
procedure GetCityAreaInfo(P, Loc: Integer; var CityAreaInfo: TCityAreaInfo);
function CanCityGrow(P, cix: Integer): Boolean;
function GetCityReport(P, cix: Integer; var CityReport: TCityReport): Integer;
function GetCityReportNew(P, cix: Integer;
  var CityReportNew: TCityReportNew): Integer;

// Internal Tile Picking
function AddBestCityTile(P, cix: Integer): Boolean;
procedure CityGrowth(P, cix: Integer);
procedure CityShrink(P, cix: Integer);
procedure Pollute(P, cix: Integer);

// Turn Processing
procedure PayCityMaintenance(P, cix: Integer);
procedure CollectCityResources(P, cix: Integer);
function CityTurn(P, cix: Integer): Boolean;

// Tile Access
function SetCityTiles(P, cix, NewTiles: Integer;
  TestOnly: Boolean = False): Integer;
procedure GetCityTileAdvice(P, cix: Integer; var Advice: TCityTileAdviceData);

// Start/End Game
procedure InitGame;
procedure ReleaseGame;


implementation

type
  TTradeProcessing = record
    TaxBonus: Integer;
    LuxBonus: Integer;
    ScienceBonus: Integer;
    FutResBonus: Integer;
    ScienceDoubling: Integer;
    HappyBase: Integer;
    RelCorr: Single;
    FlexibleLuxury: Boolean;
  end;

  TProdProcessing = record
    ProdBonus: Integer;
    PollBonus: Integer;
    FutProdBonus: Integer;
    PollThreshold: Integer;
  end;

  PCityReportEx = ^TCityReportEx;

  TCityReportEx = record
    BaseHappiness: Integer;
    BaseControl: Integer;
    Material: Integer;
    ProdProcessing: TProdProcessing;
    TradeProcessing: TTradeProcessing;
  end;

var
  MaxDist: Integer;

{
  Reporting
  ____________________________________________________________________
}
procedure GetCityAreaInfo(P, Loc: Integer; var CityAreaInfo: TCityAreaInfo);
var
  V21, Loc1, p1: Integer;
  Radius: TVicinity21Loc;
begin
{$IFOPT O-}Assert(1 shl P and InvalidTreatyMap = 0); {$ENDIF}
  with CityAreaInfo do
  begin
    V21_to_Loc(Loc, Radius);
    for V21 := 0 to 26 do
    begin
      Loc1 := Radius[V21];
      if (Loc1 < 0) or (Loc1 >= MapSize) then
        Available[V21] := faInvalid
      else
      begin
        p1 := RealMap[Loc1] shr 27;
        if (p1 < nPl) and (p1 <> P) and (RW[P].Treaty[p1] >= trPeace) then
          Available[V21] := faTreaty
        else if (ZoCMap[Loc1] > 0) and (Occupant[Loc1] <> P) and
          (RW[P].Treaty[Occupant[Loc1]] < trAlliance) then
          Available[V21] := faSiege
        else if (UsedByCity[Loc1] <> -1) and (UsedByCity[Loc1] <> Loc) then
          Available[V21] := faNotAvailable
        else
          Available[V21] := faAvailable;
      end;
    end;
  end;
end;

function CanCityGrow(P, cix: Integer): Boolean;
begin
  with RW[P].City[cix] do
    Result := (Size < MaxCitySize) and
      ((Size < NeedAqueductSize) or (Built[imAqueduct] = 1) and
      (Size < NeedSewerSize) or (Built[imSewer] = 1));
end;

procedure DetermineCityProdProcessing(P, cix: Integer;
  var ProdProcessing: TProdProcessing);
begin
  with RW[P].City[cix], ProdProcessing do
  begin
    ProdBonus := 0;
    PollBonus := 0;
    if Built[imFactory] = 1 then
      Inc(ProdBonus);
    if Built[imMfgPlant] = 1 then
      Inc(ProdBonus);
    if (Built[imPower] = 1) or (Built[imHydro] = 1) or (Built[imNuclear] = 1) or
      (GWonder[woHoover].EffectiveOwner = P) then
      ProdBonus := ProdBonus * 2;
    if Built[imFactory] = 1 then
      Inc(PollBonus);
    if Built[imMfgPlant] = 1 then
      Inc(PollBonus);
    if (Built[imFactory] + Built[imMfgPlant] > 0) then
      if (Built[imHydro] > 0) or (GWonder[woHoover].EffectiveOwner = P) then
        Dec(PollBonus)
      else if (Built[imNuclear] = 0) and (Built[imPower] = 1) then
        Inc(PollBonus);
    if (RW[P].Government <= gDespotism) or (Built[imRecycling] = 1) then
      PollBonus := -2; // no pollution
    PollThreshold := Size;
    FutProdBonus := 0;
    if RW[P].Tech[futProductionTechnology] > 0 then
    begin // future tech benefits
      if Built[imFactory] = 1 then
        Inc(FutProdBonus, FactoryFutureBonus * RW[P].Tech
          [futProductionTechnology]);
      if Built[imMfgPlant] = 1 then
        Inc(FutProdBonus, MfgPlantFutureBonus * RW[P].Tech
          [futProductionTechnology]);
    end;
  end;
end;

procedure BoostProd(BaseProd: Integer; ProdProcessing: TProdProcessing;
  var Prod, Poll: Integer);
begin
  Poll := BaseProd * (2 + ProdProcessing.PollBonus) shr 1;
  if Poll <= ProdProcessing.PollThreshold then
    Poll := 0
  else
    Dec(Poll, ProdProcessing.PollThreshold);
  if ProdProcessing.FutProdBonus > 0 then
    Prod := BaseProd * (100 + ProdProcessing.ProdBonus * 50 +
      ProdProcessing.FutProdBonus) div 100
  else
    Prod := BaseProd * (2 + ProdProcessing.ProdBonus) shr 1;
end;

procedure DetermineCityTradeProcessing(P, cix, HappinessBeforeLux: Integer;
  var TradeProcessing: TTradeProcessing);
var
  I, Dist: Integer;
begin
  with RW[P].City[cix], TradeProcessing do
  begin
    TaxBonus := 0;
    ScienceBonus := 0;
    if Built[imMarket] = 1 then
      Inc(TaxBonus, 2);
    if Built[imBank] = 1 then
    begin
      Inc(TaxBonus, 3);
      if RW[P].NatBuilt[imStockEx] = 1 then
        Inc(TaxBonus, 3);
    end;
    LuxBonus := TaxBonus;
    if Built[imLibrary] = 1 then
      Inc(ScienceBonus, 2);
    if Built[imUniversity] = 1 then
      Inc(ScienceBonus, 3);
    if Built[imResLab] = 1 then
      Inc(ScienceBonus, 3);
    ScienceDoubling := 0;
    if Built[imNatObs] > 0 then
      Inc(ScienceDoubling);
    if RW[P].Government = gFundamentalism then
      Dec(ScienceDoubling)
    else if (GWonder[woNewton].EffectiveOwner = P) and
      (RW[P].Government = gMonarchy) then
      Inc(ScienceDoubling);
    FlexibleLuxury := ((ServerVersion[P] >= $0100F1) and
      (GWonder[woLiberty].EffectiveOwner = P) or (ServerVersion[P] < $0100F1)
      and (GWonder[woMich].EffectiveOwner = P)) and
      (RW[P].Government <> gAnarchy);
    FutResBonus := 0;
    if RW[P].Tech[futResearchTechnology] > 0 then
    begin // future tech benefits
      if Built[imUniversity] = 1 then
        Inc(FutResBonus, UniversityFutureBonus * RW[P].Tech
          [futResearchTechnology]);
      if Built[imResLab] = 1 then
        Inc(FutResBonus, ResLabFutureBonus * RW[P].Tech[futResearchTechnology]);
    end;
    if (RW[P].NatBuilt[imPalace] > 0) or (ServerVersion[P] < $010000) then
    begin // calculate corruption
      Dist := MaxDist;
      for I := 0 to RW[P].nCity - 1 do
        if (RW[P].City[I].Loc >= 0) and (RW[P].City[I].Built[imPalace] = 1) then
          Dist := Distance(Loc, RW[P].City[I].Loc);
      if (Dist = 0) or (CorrLevel[RW[P].Government] = 0) then
        RelCorr := 0.0
      else
      begin
        RelCorr := Dist / MaxDist;
        if CorrLevel[RW[P].Government] > 1 then
          RelCorr := Exp(ln(RelCorr) / CorrLevel[RW[P].Government]);
        if Built[imCourt] = 1 then
          RelCorr := RelCorr / 2;
        // !!! floating point calculation always deterministic???
      end
    end
    else if Built[imCourt] = 1 then
      RelCorr := 0.5
    else
      RelCorr := 1.0;
    HappyBase := Size + HappinessBeforeLux;
  end;
end;

procedure SplitTrade(Trade, TaxRate, LuxRate, Working: Integer;
  TradeProcessing: TTradeProcessing; var Corruption, Tax, Lux,
  Science: Integer);
var
  plus: Integer;
begin
  Corruption := Trunc(Trade * TradeProcessing.RelCorr);
  Tax := (TaxRate * (Trade - Corruption) + 50) div 100;
  if TradeProcessing.FlexibleLuxury then
  begin
    plus := Working * 2 - TradeProcessing.HappyBase;
    // required additional luxury
    if plus > 0 then
    begin
      Lux := (4 * plus + 3 + TradeProcessing.LuxBonus)
        div (4 + TradeProcessing.LuxBonus);
      if Lux > Trade - Corruption then
        Lux := Trade - Corruption;
      if Tax > Trade - Corruption - Lux then
        Tax := Trade - Corruption - Lux;
    end
    else
      Lux := 0;
  end
  else if (LuxRate = 0) or (TaxRate = 100) then
    Lux := 0
  else
    Lux := (LuxRate * (Trade - Corruption) + 49) div 100;
  Science := Trade - Corruption - Lux - Tax;
  Tax := Tax * (4 + TradeProcessing.TaxBonus) shr 2;
  Lux := Lux * (4 + TradeProcessing.LuxBonus) shr 2;
  if TradeProcessing.FutResBonus > 0 then
    Science := Science * (100 + TradeProcessing.ScienceBonus * 25 +
      TradeProcessing.FutResBonus) div 100
  else
    Science := Science * (4 + TradeProcessing.ScienceBonus) shr 2;
  Science := Science shl 2 shr (2 - TradeProcessing.ScienceDoubling);
end;

function GetProjectCost(P, cix: Integer): Integer;
var
  I: Integer;
begin
  with RW[P].City[cix] do
  begin
    if Project and cpImp = 0 then
    begin
      Result := RW[P].Model[Project and cpIndex].Cost; { unit project }
      if Project and cpConscripts <> 0 then
      begin
        I := RW[P].Model[Project and cpIndex].MCost;
        Result := Result - 3 * I;
        if Result <= 0 then
          Result := I;
      end
      else if RW[P].Model[Project and cpIndex].Cap[mcLine] > 0 then
        if Project0 and (not cpAuto or cpRepeat) = Project and not cpAuto or cpRepeat
        then
          Result := Result shr 1
        else
          Result := Result * 2;
    end
    else
    begin { improvement project }
      Result := Imp[Project and cpIndex].Cost;
      if (Project and cpIndex < nWonder) and (GWonder[woColossus].EffectiveOwner = P)
      then
        Result := Result * ColossusEffect div 100;
    end;
    Result := Result * BuildCostMod[Difficulty[P]] div 12;
  end;
end;

function GetSmallCityReport(P, cix: Integer; var CityReport: TCityReport;
  PCityReportEx: PCityReportEx = nil): Integer;
var
  I, uix, V21, Loc1, ForcedSupport, BaseHappiness, Control: Integer;
  ProdProcessing: TProdProcessing;
  TradeProcessing: TTradeProcessing;
  Radius: TVicinity21Loc;
  UnitReport: TUnitReport;
  RareOK: array [0 .. 3] of Integer;
  TileInfo: TTileInfo;
begin
  with RW[P].City[cix], CityReport do
  begin
    if HypoTiles <= 0 then
      HypoTiles := Tiles;
    if HypoTax < 0 then
      HypoTax := RW[P].TaxRate;
    if HypoLux < 0 then
      HypoLux := RW[P].LuxRate;

    if (Flags and chCaptured <> 0) or (RW[P].Government = gAnarchy) then
    begin
      Working := 0;
      for V21 := 1 to 26 do
        if HypoTiles and (1 shl V21) <> 0 then
          Inc(Working); // for backward compatibility

      if RW[P].Government = gFundamentalism then
      begin
        Happy := Size;
        Control := Size;
      end // !!! old bug, kept for compatibility
      else
      begin
        Happy := 0;
        Control := 0;
      end;

      BaseHappiness := BasicHappy * 2;
      Support := 0;
      Deployed := 0;
      Eaten := Size * 2;
      FoodRep := Size * 2;
      ProdRep := 0;
      Trade := 0;
      PollRep := 0;
      Corruption := 0;
      Tax := 0;
      Lux := 0;
      Science := 0;

      if PCityReportEx <> nil then
      begin
        PCityReportEx.Material := ProdRep;
        PCityReportEx.BaseHappiness := BaseHappiness;
        PCityReportEx.BaseControl := Control;
      end;
    end
    else // not captured, no anarchy
    begin
      Control := 0;
      BaseHappiness := BasicHappy * 2;
      Happy := BasicHappy;
      if (Built[imColosseum] > 0) then
      begin
        if (Happy < (Size + 1) shr 1) then
          Happy := (Size + 1) shr 1;
        if Size > 4 then
          BaseHappiness := Size;
      end;
      for I := 0 to nWonder - 1 do
        if Built[I] = 1 then
        begin
          Inc(Happy);
          Inc(BaseHappiness, 2);
        end;
      if Built[imTemple] = 1 then
      begin
        Inc(Happy);
        Inc(BaseHappiness, 2);
      end;
      if Built[imCathedral] = 1 then
      begin
        Inc(Happy, 2);
        Inc(BaseHappiness, 4);
        if GWonder[woBach].EffectiveOwner = P then
        begin
          Inc(Happy);
          Inc(BaseHappiness, 2);
        end;
      end;
      if Built[imTheater] > 0 then
      begin
        Inc(Happy, 2);
        Inc(BaseHappiness, 4);
      end;

      // calculate unit support
{$IFOPT O-}Assert(InvalidTreatyMap = 0); {$ENDIF}
      Support := 0;
      ForcedSupport := 0;
      Eaten := Size * 2;
      Deployed := 0;
      for uix := 0 to RW[P].nUn - 1 do
        with RW[P].Un[uix] do
          if (Loc >= 0) and (Home = cix) then
          begin
            GetUnitReport(P, uix, UnitReport);
            Inc(Eaten, UnitReport.FoodSupport);
            if UnitReport.ReportFlags and urfAlwaysSupport <> 0 then
              Inc(ForcedSupport, UnitReport.ProdSupport)
            else
              Inc(Support, UnitReport.ProdSupport);
            if UnitReport.ReportFlags and urfDeployed <> 0 then
              Inc(Deployed);
          end;
      if Deployed >= Happy then
        Happy := 0
      else
        Dec(Happy, Deployed);
      Dec(Support, Size * SupportFree[RW[P].Government] shr 1);
      if Support < 0 then
        Support := 0;
      Inc(Support, ForcedSupport);

      { control }
      case RW[P].Government of
        gDespotism:
          for uix := 0 to RW[P].nUn - 1 do
            if (RW[P].Un[uix].Loc = Loc) and
              (RW[P].Model[RW[P].Un[uix].mix].Kind = mkSpecial_TownGuard) then
            begin
              Inc(Happy);
              Inc(Control, 2);
            end;
        gFundamentalism:
          begin
            BaseHappiness := 0; // done by control
            Happy := Size;
            Control := Size;
          end;
      end;

      // collect processing parameters
      DetermineCityProdProcessing(P, cix, ProdProcessing);
      DetermineCityTradeProcessing(P, cix, BaseHappiness + Control - 2 *
        Deployed, TradeProcessing);

      // collect resources
      Working := 0;
      FoodRep := 0;
      ProdRep := 0;
      Trade := 0;
      FillChar(RareOK, SizeOf(RareOK), 0);
      V21_to_Loc(Loc, Radius);
      for V21 := 1 to 26 do
        if HypoTiles and (1 shl V21) <> 0 then
        begin { sum resources of exploited tiles }
          Loc1 := Radius[V21];
          if (Loc1 < 0) or (Loc1 >= MapSize) then
          // HypoTiles go beyond map border!
          begin
            Result := eInvalid;
            Exit;
          end;
          GetTileInfo(P, cix, Loc1, TileInfo);
          Inc(FoodRep, TileInfo.Food);
          Inc(ProdRep, TileInfo.Prod);
          Inc(Trade, TileInfo.Trade);
          if (RealMap[Loc1] and fModern <> 0) and
            (RW[P].Tech[adMassProduction] >= tsApplicable) then
            Inc(RareOK[RealMap[Loc1] shr 25 and 3]);
          Inc(Working);
        end;
      if Built[imAlgae] = 1 then
        Inc(FoodRep, 12);

      if PCityReportEx <> nil then
      begin
        PCityReportEx.Material := ProdRep;
        PCityReportEx.BaseHappiness := BaseHappiness;
        PCityReportEx.BaseControl := Control;
        PCityReportEx.ProdProcessing := ProdProcessing;
        PCityReportEx.TradeProcessing := TradeProcessing;
      end;

      BoostProd(ProdRep, ProdProcessing, ProdRep, PollRep);
      SplitTrade(Trade, HypoTax, HypoLux, Working, TradeProcessing, Corruption,
        Tax, Lux, Science);
      Happy := Happy + (Lux + Size and 1) shr 1;
      // new style disorder requires 1 lux less for cities with odd size

      // check if rare resource available
      if (GTestFlags and tfNoRareNeed = 0) and (ProdRep > Support) and
        (Project and cpImp <> 0) and ((Project and cpIndex = imShipComp) and
        (RareOK[1] = 0) or (Project and cpIndex = imShipPow) and (RareOK[2] = 0)
        or (Project and cpIndex = imShipHab) and (RareOK[3] = 0)) then
        ProdRep := Support;
    end;
  end;
  Result := eOk;
end;

function GetCityReport(P, cix: Integer; var CityReport: TCityReport): Integer;
begin
  Result := GetSmallCityReport(P, cix, CityReport);
  CityReport.Storage := StorageSize[Difficulty[P]];
  CityReport.ProdCost := GetProjectCost(P, cix);
end;

function GetCityReportNew(P, cix: Integer;
  var CityReportNew: TCityReportNew): Integer;
var
  CityReport: TCityReport;
  CityReportEx: TCityReportEx;
begin
  with CityReportNew do
  begin
    CityReport.HypoTiles := HypoTiles;
    CityReport.HypoTax := HypoTaxRate;
    CityReport.HypoLux := HypoLuxuryRate;
    Result := GetSmallCityReport(P, cix, CityReport, @CityReportEx);
    FoodSupport := CityReport.Eaten - 2 * RW[P].City[cix].Size;
    MaterialSupport := CityReport.Support;
    ProjectCost := GetProjectCost(P, cix);
    Storage := StorageSize[Difficulty[P]];
    Deployed := CityReport.Deployed;
    Morale := CityReportEx.BaseHappiness;
    CollectedControl := CityReportEx.BaseControl +
      (RW[P].City[cix].Size - CityReport.Working) * 2;
    CollectedFood := CityReport.FoodRep;
    CollectedMaterial := CityReportEx.Material;
    CollectedTrade := CityReport.Trade;
    Working := CityReport.Working;
    Production := CityReport.ProdRep - CityReport.Support;
    AddPollution := CityReport.PollRep;
    Corruption := CityReport.Corruption;
    Tax := CityReport.Tax;
    Science := CityReport.Science;
    Luxury := CityReport.Lux;
    FoodSurplus := CityReport.FoodRep - CityReport.Eaten;
    HappinessBalance := Morale + Luxury + CollectedControl - RW[P].City[cix]
      .Size - 2 * Deployed;
  end;
end;

{
  Internal Tile Picking
  ____________________________________________________________________
}
procedure NextBest(P, cix: Integer; var SelectedLoc, SelectedV21: Integer);
{ best tile unused but available by city cix }
var
  Resources, Most, Loc1, p1, V21: Integer;
  TileInfo: TTileInfo;
  Radius: TVicinity21Loc;
begin
{$IFOPT O-}Assert(1 shl P and InvalidTreatyMap = 0); {$ENDIF}
  Most := 0;
  SelectedLoc := -1;
  SelectedV21 := -1;
  with RW[P].City[cix] do
  begin
    V21_to_Loc(Loc, Radius);
    for V21 := 1 to 26 do
    begin
      Loc1 := Radius[V21];
      if (Loc1 >= 0) and (Loc1 < MapSize) and (UsedByCity[Loc1] = -1) then
      begin
        p1 := RealMap[Loc1] shr 27;
        if ((p1 = nPl) or (p1 = P) or (RW[P].Treaty[p1] < trPeace)) and
          ((ZoCMap[Loc1] = 0) or (Occupant[Loc1] = P) or
          (RW[P].Treaty[Occupant[Loc1]] = trAlliance)) then
        begin
          GetTileInfo(P, cix, Loc1, TileInfo);
          Resources := TileInfo.Food shl 16 + TileInfo.Prod shl 8 +
            TileInfo.Trade;
          { priority: 1.food - 2.prod - 3.trade }
          if Resources > Most then
          begin
            SelectedLoc := Loc1;
            SelectedV21 := V21;
            Most := Resources;
          end;
        end;
      end;
    end;
  end;
end;

procedure NextWorst(P, cix: Integer; var SelectedLoc, SelectedV21: Integer);
{ worst tile used by city cix }
var
  Resources, Least, Loc1, V21: Integer;
  Radius: TVicinity21Loc;
  TileInfo: TTileInfo;
begin
  Least := MaxInt;
  SelectedLoc := -1;
  SelectedV21 := -1;
  with RW[P].City[cix] do
  begin
    V21_to_Loc(Loc, Radius);
    for V21 := 1 to 26 do
      if V21 <> CityOwnTile then
      begin
        Loc1 := Radius[V21];
        if (Loc1 >= 0) and (Loc1 < MapSize) and (1 shl V21 and Tiles <> 0) then
        begin
          GetTileInfo(P, cix, Loc1, TileInfo);
          Resources := TileInfo.Food shl 16 + TileInfo.Prod shl 8 +
            TileInfo.Trade;
          { priority: 1.food - 2.prod - 3.trade }
          if Resources < Least then
          begin
            SelectedLoc := Loc1;
            SelectedV21 := V21;
            Least := Resources;
          end;
        end;
      end;
  end;
end;

function NextPoll(P, cix: Integer): Integer;
var
  Resources, Best, dx, dy, Loc1, Dist, BestDist, V21, pTerr: Integer;
  Radius: TVicinity21Loc;
  TileInfo: TTileInfo;
begin
  BestDist := MaxInt;
{$IFOPT O-}Assert(1 shl P and InvalidTreatyMap = 0); {$ENDIF}
  Best := 0;
  Result := -1;
  with RW[P].City[cix] do
  begin
    V21_to_Loc(Loc, Radius);
    for V21 := 1 to 26 do
      if V21 <> CityOwnTile then
      begin
        Loc1 := Radius[V21];
        if (Loc1 >= 0) and (Loc1 < MapSize) and
          (RealMap[Loc1] and fTerrain >= fGrass) and
          (RealMap[Loc1] and (fPoll or fDeadLands or fCity) = 0) then
        begin
          pTerr := RealMap[Loc1] shr 27;
          if (pTerr = nPl) or (pTerr = P) or (RW[P].Treaty[pTerr] < trPeace)
          then
          begin
            GetTileInfo(P, cix, Loc1, TileInfo);
            Resources := TileInfo.Prod shl 16 + TileInfo.Trade shl 8 +
              TileInfo.Food;
            { priority: 1.prod - 2.trade - 3.food }
            dy := V21 shr 2 - 3;
            dx := V21 and 3 shl 1 - 3 + (dy + 3) and 1;
            Dist := abs(dx) + abs(dy) + abs(abs(dx) - abs(dy)) shr 1;
            if (Resources > Best) or (Resources = Best) and (Dist < BestDist)
            then
            begin
              Result := Loc1;
              Best := Resources;
              BestDist := Dist;
            end;
          end;
        end;
      end;
  end;
end;

function AddBestCityTile(P, cix: Integer): Boolean;
var
  TileLoc, V21: Integer;
begin
  NextBest(P, cix, TileLoc, V21);
  Result := TileLoc >= 0;
  if Result then
    with RW[P].City[cix] do
    begin
      Assert(1 shl V21 and Tiles = 0);
      Tiles := Tiles or (1 shl V21);
      UsedByCity[TileLoc] := Loc;
    end;
end;

procedure CityGrowth(P, cix: Integer);
var
  TileLoc, V21: Integer;
  AltCityReport: TCityReport;
begin
  with RW[P].City[cix] do
  begin
    Inc(Size);
    NextBest(P, cix, TileLoc, V21);
    if TileLoc >= 0 then
    begin { test whether exploitation of tile would lead to disorder }
      AltCityReport.HypoTiles := Tiles + 1 shl V21;
      AltCityReport.HypoTax := -1;
      AltCityReport.HypoLux := -1;
      GetSmallCityReport(P, cix, AltCityReport);
      if AltCityReport.Working - AltCityReport.Happy <= Size shr 1 then
      // !!! change to new style disorder
      begin { no disorder -- exploit tile }
        Assert(1 shl V21 and Tiles = 0);
        Tiles := Tiles or (1 shl V21);
        UsedByCity[TileLoc] := Loc;
      end;
    end;
  end;
end;

procedure CityShrink(P, cix: Integer);
var
  TileLoc, V21, Working: Integer;
  AltCityReport: TCityReport;
begin
  with RW[P].City[cix] do
  begin
    Working := 0;
    for V21 := 1 to 26 do
      if Tiles and (1 shl V21) <> 0 then
        Inc(Working);
    Dec(Size);
    if Food > StorageSize[Difficulty[P]] then
      Food := StorageSize[Difficulty[P]];
    NextWorst(P, cix, TileLoc, V21);
    if Working > Size then
    begin { all citizens were working -- worst tile no longer exploited }
      Assert(1 shl V21 and Tiles <> 0);
      Tiles := Tiles and not(1 shl V21);
      UsedByCity[TileLoc] := -1;
    end
    else { test whether exploitation of tile would lead to disorder }
    begin
      AltCityReport.HypoTiles := -1;
      AltCityReport.HypoTax := -1;
      AltCityReport.HypoLux := -1;
      GetSmallCityReport(P, cix, AltCityReport);
      if AltCityReport.Working - AltCityReport.Happy > Size shr 1 then
      // !!! change to new style disorder
      begin { disorder -- don't exploit tile }
        Assert(1 shl V21 and Tiles <> 0);
        Tiles := Tiles and not(1 shl V21);
        UsedByCity[TileLoc] := -1;
      end;
    end;
  end;
end;

procedure Pollute(P, cix: Integer);
var
  PollutionLoc: Integer;
begin
  with RW[P].City[cix] do
  begin
    Pollution := Pollution - MaxPollution;
    PollutionLoc := NextPoll(P, cix);
    if PollutionLoc >= 0 then
    begin
      Inc(Flags, chPollution);
      RealMap[PollutionLoc] := RealMap[PollutionLoc] or fPoll;
    end;
  end;
end;

{
  Turn Processing
  ____________________________________________________________________
}
procedure PayCityMaintenance(P, cix: Integer);
var
  I: Integer;
begin
  with RW[P], City[cix] do
    for I := nWonder to nImp - 1 do
      if (Built[I] > 0) and (Project0 and (cpImp or cpIndex) <> (cpImp or I))
      then // don't pay maintenance when just completed
      begin
        Dec(Money, Imp[I].Maint);
        if Money < 0 then
        begin { out of money - sell improvement }
          Inc(Money, Imp[I].Cost * BuildCostMod[Difficulty[P]] div 12);
          Built[I] := 0;
          if Imp[I].Kind <> ikCommon then
          begin
            Assert(I <> imSpacePort);
            // never sell automatically! (solution: no maintenance)
            NatBuilt[I] := 0;
            if I = imGrWall then
              GrWallContinent[P] := -1;
          end;
          Inc(Flags, chImprovementLost);
        end;
      end;
end;

procedure CollectCityResources(P, cix: Integer);
var
  CityStorage, CityProjectCost: Integer;
  CityReport: TCityReportNew;
  Disorder: Boolean;
begin
  with RW[P], City[cix], CityReport do
    if Flags and chCaptured <> 0 then
    begin
      Flags := Flags and not chDisorder;
      Dec(Flags, $10000);
      if Flags and chCaptured = 0 then
        Flags := Flags or chAfterCapture;
    end
    else if Government = gAnarchy then
      Flags := Flags and not chDisorder
    else
    begin
      HypoTiles := -1;
      HypoTaxRate := -1;
      HypoLuxuryRate := -1;
      GetCityReportNew(P, cix, CityReport);
      CityStorage := StorageSize[Difficulty[P]];
      CityProjectCost := GetProjectCost(P, cix);

      Disorder := (HappinessBalance < 0);
      if Disorder and (Flags and chDisorder <> 0) then
        CollectedMaterial := 0; // second turn disorder
      if Disorder then
        Flags := Flags or chDisorder
      else
        Flags := Flags and not chDisorder;

      if not Disorder and ((Government = gFuture) or (Size >= NeedAqueductSize)
        and (FoodSurplus < 2)) and (FoodSurplus > 0) then
        Inc(Money, FoodSurplus)
      else if not(Disorder and (FoodSurplus > 0)) then
      begin { calculate new food storage }
        Food := Food + FoodSurplus;
        if ((GTestFlags and tfImmGrow <> 0) or (Food >= CityStorage) and
          (Food - FoodSurplus < CityStorage)) // only warn once
          and (Size < MaxCitySize) and
          (Project and (cpImp + cpIndex) <> cpImp + imAqueduct) and
          (Project and (cpImp + cpIndex) <> cpImp + imSewer) and
          not CanCityGrow(P, cix) then
          Inc(Flags, chNoGrowthWarning);
      end;

      if Prod > CityProjectCost then
      begin
        Inc(Money, Prod - CityProjectCost);
        Prod := CityProjectCost;
      end;
      if Production < 0 then
        Flags := Flags or chUnitLost
      else if not Disorder and (Flags and chProductionSabotaged = 0) then
        if Project and (cpImp + cpIndex) = cpImp + imTrGoods then
          Inc(Money, Production)
        else
          Inc(Prod, Production);

      if not Disorder then
      begin
        { sum research points and taxes }
        Inc(Research, Science);
        Inc(Money, Tax);
        Pollution := Pollution + AddPollution;
      end;
    end;
end;

function CityTurn(P, cix: Integer): Boolean;
// return value: whether city keeps existing
var
  I, uix, cix2, p1, SizeMod, CityStorage, CityProjectCost, NewImp, Det,
    TestDet: Integer;
  LackOfMaterial, CheckGrow, DoProd, IsActive: Boolean;
begin
  with RW[P], City[cix] do
  begin
    SizeMod := 0;
    CityStorage := StorageSize[Difficulty[P]];
    CityProjectCost := GetProjectCost(P, cix);

    LackOfMaterial := Flags and chUnitLost <> 0;
    Flags := Flags and not chUnitLost;

    IsActive := (Government <> gAnarchy) and (Flags and chCaptured = 0);
    CheckGrow := (Flags and chDisorder = 0) and IsActive and
      (Government <> gFuture);
    if CheckGrow and (GTestFlags and tfImmGrow <> 0) then { fast growth }
    begin
      if CanCityGrow(P, cix) then
        Inc(SizeMod);
    end
    else if CheckGrow and (Food >= CityStorage) then { normal growth }
    begin
      if CanCityGrow(P, cix) then
      begin
        if Built[imGranary] = 1 then
          Dec(Food, CityStorage shr 1)
        else
          Dec(Food, CityStorage);
        Inc(SizeMod);
      end;
    end
    else if Food < 0 then { famine }
    begin
      Food := 0;
      // check if settlers or conscripts there to disband
      uix := -1;
      for I := 0 to nUn - 1 do
        if (Un[I].Loc >= 0) and (Un[I].Home = cix) and
          ((Model[Un[I].mix].Kind = mkSettler)
          { and (GWonder[woFreeSettlers].EffectiveOwner<>p) }
          or (Un[I].Flags and unConscripts <> 0)) and
          ((uix = -1) or (Model[Un[I].mix].Cost < Model[Un[uix].mix].Cost) or
          (Model[Un[I].mix].Cost = Model[Un[uix].mix].Cost) and
          (Un[I].Exp < Un[uix].Exp)) then
          uix := I;

      if uix >= 0 then
      begin
        RemoveUnit_UpdateMap(P, uix);
        Inc(Flags, chUnitLost);
      end
      else
      begin
        Dec(SizeMod);
        Inc(Flags, chPopDecrease);
      end
    end;
    if Food > CityStorage then
      Food := CityStorage;

    if LackOfMaterial then
    begin
      if Flags and chUnitLost = 0 then
      begin { one unit lost }
        uix := -1;
        Det := MaxInt;
        for I := 0 to nUn - 1 do
          if (Un[I].Loc >= 0) and (Un[I].Home = cix) then
            with Model[Un[I].mix] do
            begin
              if Kind = mkSpecial_TownGuard then
                TestDet := Un[I].Health + Un[I].Exp shl 8
                // disband townguards first
              else
              begin
                TestDet := Un[I].Health + Un[I].Exp shl 8 + Cost shl 16;
                // value of unit
                if Flags and mdDoubleSupport <> 0 then
                  TestDet := TestDet shr 1;
                // double support, tend to disband first
              end;
              if TestDet < Det then
              begin
                uix := I;
                Det := TestDet;
              end;
            end;
        if uix >= 0 then
        begin
          RemoveUnit_UpdateMap(P, uix);
          Inc(Flags, chUnitLost);
        end;
      end;
    end;

    if GTestFlags and tfImmImprove <> 0 then
      Prod := CityProjectCost;
    DoProd := (Project and (cpImp + cpIndex) <> cpImp + imTrGoods) and
      (Prod >= CityProjectCost);

    // check if wonder already built
    if (Project and cpImp <> 0) and (Project and cpIndex < nWonder) and
      (GWonder[Project and cpIndex].CityID <> WonderNotBuiltYet) then
    begin
      Inc(Flags, chOldWonder);
      DoProd := False;
    end;

    // check if producing settlers would disband city
    if DoProd and (Project and (cpImp or cpDisbandCity) = 0) and
      ((Size + SizeMod - 2 < 2) and
      (Model[Project and cpIndex].Kind = mkSettler) or (Size + SizeMod - 1 < 2)
      and ((Model[Project and cpIndex].Kind = mkSlaves) or
      (Project and cpConscripts <> 0))) then
    begin
      Inc(Flags, chNoSettlerProd);
      DoProd := False;
    end;

    if DoProd then
    begin { project complete }
      Dec(Prod, CityProjectCost);
      if Project and cpImp = 0 then { produce unit }
      begin
        if nUn < numax then
        begin
          CreateUnit(P, Project and cpIndex);
          Un[nUn - 1].Loc := Loc;
          with Un[nUn - 1] do
          begin
            Home := cix;
            if (Model[mix].Domain < dSea) and (Built[imElite] = 1) then
              Exp := ExpCost * (nExp - 1) { elite }
            else if (Model[mix].Domain < dSea) and (Built[imBarracks] = 1) or
              (Model[mix].Domain = dSea) and (Built[imDockyard] = 1) or
              (Model[mix].Domain = dAir) and (Built[imAirport] = 1) then
              Exp := ExpCost * 2; { vet }
            if Project and cpConscripts <> 0 then
              Flags := Flags or unConscripts;
          end;
          PlaceUnit(P, nUn - 1);
          UpdateUnitMap(Loc);
          if Model[Project and cpIndex].Kind = mkSettler then
            Dec(SizeMod, 2) { settler produced - city shrink }
          else if (Model[Project and cpIndex].Kind = mkSlaves) or
            (Project and cpConscripts <> 0) then
            Dec(SizeMod); { slaves/conscripts produced - city shrink }
        end;
        Project0 := Project or cpRepeat or cpCompleted;
      end
      else if Imp[Project and cpIndex].Kind = ikShipPart then
      begin { produce ship parts }
        Inc(GShip[P].Parts[Project and cpIndex - imShipComp]);
        Project0 := Project or cpCompleted;
      end
      else { produce improvement }
      begin
        NewImp := Project and cpIndex;
        Inc(Money, Prod); { change rest to money }
        Project0 := Project or cpCompleted;
        Project := cpImp + imTrGoods;
        Prod := 0;

        if Imp[NewImp].Kind in [ikNatLocal, ikNatGlobal] then
        begin // nat. project
          for I := 0 to nCity - 1 do
            if (City[I].Loc >= 0) and (City[I].Built[NewImp] = 1) then
            begin { allowed only once }
              Inc(Money, Imp[NewImp].Cost * BuildCostMod[Difficulty[P]] div 12);
              City[I].Built[NewImp] := 0;
            end;
          NatBuilt[NewImp] := 1;

          // immediate nat. project effects
          case NewImp of
            imGrWall:
              GrWallContinent[P] := Continent[Loc];
          end;
        end;

        if NewImp < nWonder then
        begin // wonder
          GWonder[NewImp].CityID := ID;
          GWonder[NewImp].EffectiveOwner := P;
          CheckExpiration(NewImp);

          // immediate wonder effects
          case NewImp of
            woEiffel:
              begin // reactivate wonders
                for I := 0 to nWonder - 1 do
                  if Imp[I].Expiration >= 0 then
                    for cix2 := 0 to nCity - 1 do
                      if (City[cix2].Loc >= 0) and (City[cix2].Built[I] = 1)
                      then
                        GWonder[I].EffectiveOwner := P;
              end;
            woLighthouse:
              CheckSpecialModels(P, preLighthouse);
            woLeo:
              begin
                Inc(Research, TechBaseCost(nTech[P], Difficulty[P]) +
                  TechBaseCost(nTech[P] + 2, Difficulty[P]));
                CheckSpecialModels(P, preLeo);
              end;
            woPyramids:
              CheckSpecialModels(P, preBuilder);
            woMir:
              begin
                for p1 := 0 to nPl - 1 do
                  if (p1 <> P) and (1 shl p1 and GAlive <> 0) then
                  begin
                    if RW[P].Treaty[p1] = trNoContact then
                      IntroduceEnemy(P, p1);
                    GiveCivilReport(P, p1);
                    GiveMilReport(P, p1);
                  end;
              end;
          end;
        end;

        for I := 0 to nImpReplacement - 1 do // sell obsolete buildings
          if (ImpReplacement[I].NewImp = NewImp) and
            (Built[ImpReplacement[I].OldImp] > 0) then
          begin
            Inc(RW[P].Money, Imp[ImpReplacement[I].OldImp].Cost * BuildCostMod
              [Difficulty[P]] div 12);
            Built[ImpReplacement[I].OldImp] := 0;
          end;

        if NewImp in [imPower, imHydro, imNuclear] then
          for I := 0 to nImp - 1 do
            if (I <> NewImp) and (I in [imPower, imHydro, imNuclear]) and
              (Built[I] > 0) then
            begin // sell obsolete power plant
              Inc(RW[P].Money, Imp[I].Cost * BuildCostMod[Difficulty[P]
                ] div 12);
              Built[I] := 0;
            end;

        Built[NewImp] := 1;
      end;
      Prod0 := Prod;
      Inc(Flags, chProduction);
    end
    else
    begin
      Project0 := Project0 and not cpCompleted;
      if Project0 and not cpAuto <> Project and not cpAuto then
        Project0 := Project;
      Prod0 := Prod;
    end;

    if SizeMod > 0 then
    begin
      CityGrowth(P, cix);
      Inc(Flags, chPopIncrease);
    end;
    Result := Size + SizeMod >= 2;
    if Result then
      while SizeMod < 0 do
      begin
        CityShrink(P, cix);
        Inc(SizeMod);
      end;
  end;
end;

{
  Tile Access
  ____________________________________________________________________
}
function SetCityTiles(P, cix, NewTiles: Integer;
  TestOnly: Boolean = False): Integer;
var
  V21, Working, ChangeTiles, AddTiles, Loc1: Integer;
  CityAreaInfo: TCityAreaInfo;
  Radius: TVicinity21Loc;
begin
  with RW[P].City[cix] do
  begin
    ChangeTiles := NewTiles xor Integer(Tiles);
    AddTiles := NewTiles and not Tiles;
    if Mode = moPlaying then
    begin // do all checks
      if NewTiles and not $67F7F76 <> 0 then
      begin
        Result := eInvalid;
        Exit
      end; // invalid tile index included
      if NewTiles and (1 shl 13) = 0 then
      begin
        Result := eViolation;
        Exit
      end; // city tile must be exploited
      if ChangeTiles = 0 then
      begin
        Result := eNotChanged;
        Exit
      end;
      if AddTiles <> 0 then
      begin
        // check if new tiles possible
        GetCityAreaInfo(P, Loc, CityAreaInfo);
        for V21 := 1 to 26 do
          if AddTiles and (1 shl V21) <> 0 then
            if CityAreaInfo.Available[V21] <> faAvailable then
            begin
              Result := eTileNotAvailable;
              Exit;
            end;
        // not more tiles than inhabitants
        Working := 0;
        for V21 := 1 to 26 do
          if NewTiles and (1 shl V21) <> 0 then
            Inc(Working);
        if Working > Size then
        begin
          Result := eNoWorkerAvailable;
          Exit;
        end;
      end;
    end;
    Result := eOk;
    if not TestOnly then
    begin
      V21_to_Loc(Loc, Radius);
      for V21 := 1 to 26 do
        if ChangeTiles and (1 shl V21) <> 0 then
        begin
          Loc1 := Radius[V21];
          Assert((Loc1 >= 0) and (Loc1 < MapSize));
          if NewTiles and (1 shl V21) <> 0 then
            UsedByCity[Loc1] := Loc // employ tile
          else if UsedByCity[Loc1] <> Loc then
            Assert(Mode < moPlaying)
            // should only happen during loading, because of wrong sSetCityTiles command order
          else
            UsedByCity[Loc1] := -1; // unemploy tile
        end;
      Tiles := NewTiles;
    end;
  end;
end;

procedure GetCityTileAdvice(P, cix: Integer; var Advice: TCityTileAdviceData);
const
  oFood = 0;
  oProd = 1;
  oTax = 2;
  oScience = 3;
type
  TTileData = record
    Food: Integer;
    Prod: Integer;
    Trade: Integer;
    SubValue: Integer;
    V21: Integer;
  end;
var
  I, V21, Loc1, nHierarchy, iH, iT, iH_Switch, MinWorking, MaxWorking,
    WantedProd, MinFood, MinProd, count, Take, MaxTake, AreaSize, FormulaCode,
    NeedRare, RareTiles, cix1, dx, dy, BestTiles, ProdBeforeBoost, TestTiles,
    SubPlus, SuperPlus: Integer;
  SuperValue, BestSuperValue, SubValue, BestSubValue: Integer;
  Value, BestValue, ValuePlus: Extended;
  ValueFormula_Weight: array [oFood .. oScience] of Extended;
  ValueFormula_Multiply: array [oFood .. oScience] of Boolean;
  Output: array [oFood .. oScience] of Integer;
  TileInfo, BaseTileInfo: TTileInfo;
  Radius, Radius1: TVicinity21Loc;
  TestReport: TCityReport;
  CityReportEx: TCityReportEx;
  CityAreaInfo: TCityAreaInfo;
  Hierarchy: array [0 .. 20, 0 .. 31] of TTileData;
  nTile, nSelection: array [0 .. 20] of Integer;
  SubCriterion: array [0 .. 27] of Integer;
  FoodWasted, FoodToTax, ProdToTax, RareOK, NeedStep2, IsBest: Boolean;
begin
  if (RW[P].Government = gAnarchy) or (RW[P].City[cix].Flags and chCaptured <> 0)
  then
  begin
    FillChar(Advice.CityReport, SizeOf(Advice.CityReport), 0);
    Advice.Tiles := 1 shl CityOwnTile;
    Advice.CityReport.HypoTiles := 1 shl CityOwnTile;
    Exit;
  end;

  for I := oFood to oScience do
  begin // decode evaluation formula from weights parameter
    FormulaCode := Advice.ResourceWeights shr (24 - 8 * I) and $FF;
    ValueFormula_Multiply[I] := FormulaCode and $80 <> 0;
    if FormulaCode and $40 <> 0 then
      ValueFormula_Weight[I] := (FormulaCode and $0F) *
        (1 shl (FormulaCode and $30 shr 4)) / 16
    else
      ValueFormula_Weight[I] := (FormulaCode and $0F) *
        (1 shl (FormulaCode and $30 shr 4));
  end;

  TestReport.HypoTiles := 1 shl CityOwnTile;
  TestReport.HypoTax := -1;
  TestReport.HypoLux := -1;
  GetSmallCityReport(P, cix, TestReport, @CityReportEx);
  with RW[P].City[cix] do
  begin
    V21_to_Loc(Loc, Radius);
    FoodToTax := RW[P].Government = gFuture;
    ProdToTax := Project and (cpImp + cpIndex) = cpImp + imTrGoods;
    FoodWasted := not FoodToTax and (Food = StorageSize[Difficulty[P]]) and
      not CanCityGrow(P, cix);

    // sub criteria
    for V21 := 1 to 26 do
    begin
      Loc1 := Radius[V21];
      if Loc1 >= 0 then
        SubCriterion[V21] := 3360 - (Distance(Loc, Loc1) - 1) * 32 -
          V21 xor $15;
    end;
    for cix1 := 0 to RW[P].nCity - 1 do
      if cix1 <> cix then
      begin
        Loc1 := RW[P].City[cix1].Loc;
        if Loc1 >= 0 then
        begin
          if Distance(Loc, Loc1) <= 10 then
          begin // cities overlap -- prefer tiles outside common range
            V21_to_Loc(Loc1, Radius1);
            for V21 := 1 to 26 do
            begin
              Loc1 := Radius1[V21];
              if (Loc1 >= 0) and (Loc1 < MapSize) and (Distance(Loc, Loc1) <= 5)
              then
              begin
                dxdy(Loc, Loc1, dx, dy);
                Dec(SubCriterion[(dy + 3) shl 2 + (dx + 3) shr 1], 160);
              end;
            end;
          end;
        end;
      end;

    GetCityAreaInfo(P, Loc, CityAreaInfo);
    AreaSize := 0;
    for V21 := 1 to 26 do
      if CityAreaInfo.Available[V21] = faAvailable then
        Inc(AreaSize);

    if RW[P].Government = gFundamentalism then
    begin
      MinWorking := Size;
      MaxWorking := Size;
    end
    else
    begin
      MinWorking := CityReportEx.TradeProcessing.HappyBase shr 1;
      if MinWorking > Size then
        MinWorking := Size;
      if (RW[P].LuxRate = 0) and not CityReportEx.TradeProcessing.FlexibleLuxury
      then
        MaxWorking := MinWorking
      else
        MaxWorking := Size;
    end;
    if MaxWorking > AreaSize then
    begin
      MaxWorking := AreaSize;
      if MinWorking > AreaSize then
        MinWorking := AreaSize;
    end;
    if TestReport.Support = 0 then
      WantedProd := 0
    else
      WantedProd := 1 + (TestReport.Support * 100 - 1)
        div (100 + CityReportEx.ProdProcessing.ProdBonus * 50 +
        CityReportEx.ProdProcessing.FutProdBonus);

    // consider resources for ship parts
    NeedRare := 0;
    if (GTestFlags and tfNoRareNeed = 0) and (Project and cpImp <> 0) then
      case Project and cpIndex of
        imShipComp:
          NeedRare := fCobalt;
        imShipPow:
          NeedRare := fUranium;
        imShipHab:
          NeedRare := fMercury;
      end;
    if NeedRare > 0 then
    begin
      RareTiles := 0;
      for V21 := 1 to 26 do
      begin
        Loc1 := Radius[V21];
        if (Loc1 >= 0) and (Loc1 < MapSize) and
          (RealMap[Loc1] and fModern = Cardinal(NeedRare)) then
          RareTiles := RareTiles or (1 shl V21);
      end;
    end;

    // step 1: sort tiles to hierarchies
    nHierarchy := 0;
    for V21 := 1 to 26 do // non-rare tiles
      if (CityAreaInfo.Available[V21] = faAvailable) and
        ((NeedRare = 0) or (1 shl V21 and RareTiles = 0)) then
      begin
        Loc1 := Radius[V21];
        Assert((Loc1 >= 0) and (Loc1 < MapSize));
        GetTileInfo(P, cix, Loc1, TileInfo);
        if V21 = CityOwnTile then
          BaseTileInfo := TileInfo
        else
        begin
          iH := 0;
          while iH < nHierarchy do
          begin
            iT := 0;
            while (iT < nTile[iH]) and (TileInfo.Food <= Hierarchy[iH, iT].Food)
              and (TileInfo.Prod <= Hierarchy[iH, iT].Prod) and
              (TileInfo.Trade <= Hierarchy[iH, iT].Trade) and
              not((TileInfo.Food = Hierarchy[iH, iT].Food) and
              (TileInfo.Prod = Hierarchy[iH, iT].Prod) and
              (TileInfo.Trade = Hierarchy[iH, iT].Trade) and
              (SubCriterion[V21] >= SubCriterion[Hierarchy[iH, iT].V21])) do
              Inc(iT);
            if (iT = nTile[iH]) // new worst tile in this hierarchy
              or ((TileInfo.Food >= Hierarchy[iH, iT].Food)
              // new middle tile in this hierarchy
              and (TileInfo.Prod >= Hierarchy[iH, iT].Prod) and
              (TileInfo.Trade >= Hierarchy[iH, iT].Trade)) then
              Break; // insert position found!
            Inc(iH);
          end;
          if iH = nHierarchy then
          begin // need to start new hierarchy
            nTile[iH] := 0;
            Inc(nHierarchy);
            iT := 0;
          end;
          Move(Hierarchy[iH, iT], Hierarchy[iH, iT + 1],
            (nTile[iH] - iT) * SizeOf(TTileData));
          Inc(nTile[iH]);
          Hierarchy[iH, iT].V21 := V21;
          Hierarchy[iH, iT].Food := TileInfo.Food;
          Hierarchy[iH, iT].Prod := TileInfo.Prod;
          Hierarchy[iH, iT].Trade := TileInfo.Trade;
          Hierarchy[iH, iT].SubValue := SubCriterion[V21];
        end;
      end;
    if NeedRare <> 0 then
    begin // rare tiles need own hierarchy
      iH := nHierarchy;
      for V21 := 1 to 26 do
        if (CityAreaInfo.Available[V21] = faAvailable) and
          (1 shl V21 and RareTiles <> 0) then
        begin
          Loc1 := Radius[V21];
          Assert((V21 <> CityOwnTile) and (Loc1 >= 0) and (Loc1 < MapSize));
          GetTileInfo(P, cix, Loc1, TileInfo);
          if iH = nHierarchy then
          begin // need to start new hierarchy
            nTile[iH] := 0;
            Inc(nHierarchy);
            iT := 0;
          end
          else
            iT := nTile[iH];
          Inc(nTile[iH]);
          Hierarchy[iH, iT].V21 := V21;
          Hierarchy[iH, iT].Food := TileInfo.Food; // = 0
          Hierarchy[iH, iT].Prod := TileInfo.Prod; // = 1
          Hierarchy[iH, iT].Trade := TileInfo.Trade; // = 0
          Hierarchy[iH, iT].SubValue := SubCriterion[V21];
        end;
    end;
    if Built[imAlgae] > 0 then
      Inc(BaseTileInfo.Food, 12);

    // step 2: summarize resources
    for iH := 0 to nHierarchy - 1 do
    begin
      Move(Hierarchy[iH, 0], Hierarchy[iH, 1], nTile[iH] * SizeOf(TTileData));
      Hierarchy[iH, 0].Food := 0;
      Hierarchy[iH, 0].Prod := 0;
      Hierarchy[iH, 0].Trade := 0;
      Hierarchy[iH, 0].SubValue := 0;
      Hierarchy[iH, 0].V21 := 0;
      for iT := 1 to nTile[iH] do
      begin
        Inc(Hierarchy[iH, iT].Food, Hierarchy[iH, iT - 1].Food);
        Inc(Hierarchy[iH, iT].Prod, Hierarchy[iH, iT - 1].Prod);
        Inc(Hierarchy[iH, iT].Trade, Hierarchy[iH, iT - 1].Trade);
        Inc(Hierarchy[iH, iT].SubValue, Hierarchy[iH, iT - 1].SubValue);
        Hierarchy[iH, iT].V21 := 1 shl Hierarchy[iH, iT].V21 +
          Hierarchy[iH, iT - 1].V21;
      end;
    end;

    // step 3: try all combinations
    BestValue := 0.0;
    BestSuperValue := 0;
    BestSubValue := 0;
    BestTiles := 0;
    FillChar(nSelection, SizeOf(nSelection), 0);
    TestReport.FoodRep := BaseTileInfo.Food;
    ProdBeforeBoost := BaseTileInfo.Prod;
    TestReport.Trade := BaseTileInfo.Trade;
    TestReport.Working := 1;
    MinFood := 0;
    MinProd := 0;
    iH_Switch := nHierarchy;
    count := 0;
    repeat
      // ensure minima
      iH := 0;
      while (TestReport.Working < MaxWorking) and (iH < iH_Switch) and
        ((TestReport.Working < MinWorking) or
        (TestReport.FoodRep < TestReport.Eaten) or
        (ProdBeforeBoost < WantedProd)) do
      begin
        Assert(nSelection[iH] = 0);
        Take := MinWorking - TestReport.Working;
        if Take > nTile[iH] then
          Take := nTile[iH]
        else
        begin
          if Take < 0 then
            Take := 0;
          MaxTake := nTile[iH];
          if TestReport.Working + MaxTake > MaxWorking then
            MaxTake := MaxWorking - TestReport.Working;
          while (Take < MaxTake) and
            (TestReport.FoodRep + Hierarchy[iH, Take].Food < MinFood) do
            Inc(Take);
          while (Take < MaxTake) and
            (ProdBeforeBoost + Hierarchy[iH, Take].Prod < MinProd) do
            Inc(Take);
        end;
        nSelection[iH] := Take;
        Inc(TestReport.Working, Take);
        with Hierarchy[iH, Take] do
        begin
          Inc(TestReport.FoodRep, Food);
          Inc(ProdBeforeBoost, Prod);
          Inc(TestReport.Trade, Trade);
        end;
        Inc(iH);
      end;

      Assert((TestReport.Working >= MinWorking) and
        (TestReport.Working <= MaxWorking));
      if (TestReport.FoodRep >= MinFood) and (ProdBeforeBoost >= MinProd) then
      begin
        SplitTrade(TestReport.Trade, RW[P].TaxRate, RW[P].LuxRate,
          TestReport.Working, CityReportEx.TradeProcessing,
          TestReport.Corruption, TestReport.Tax, TestReport.Lux,
          TestReport.Science);

        if CityReportEx.BaseHappiness + CityReportEx.BaseControl +
          TestReport.Lux + 2 * (Size - TestReport.Working) - 2 *
          TestReport.Deployed >= Size then
        begin // city is not in disorder -- evaluate combination
          Inc(count);
          if (MinProd < WantedProd) and (ProdBeforeBoost > MinProd) then
          begin // no combination reached wanted prod yet
            MinProd := ProdBeforeBoost;
            if MinProd > WantedProd then
              MinProd := WantedProd
          end;
          if MinProd = WantedProd then
          // do not care for food before prod is ensured
            if (MinFood < TestReport.Eaten) and (TestReport.FoodRep > MinFood)
            then
            begin // no combination reached wanted food yet
              MinFood := TestReport.FoodRep;
              if MinFood > TestReport.Eaten then
                MinFood := TestReport.Eaten
            end;
          BoostProd(ProdBeforeBoost, CityReportEx.ProdProcessing,
            TestReport.ProdRep, TestReport.PollRep);
          SuperValue := 0;

          // super-criterion A: unit support granted?
          if TestReport.ProdRep >= TestReport.Support then
            SuperValue := SuperValue or 1 shl 30;

          // super-criterion B: food demand granted?
          if TestReport.FoodRep >= TestReport.Eaten then
            SuperValue := SuperValue or 63 shl 24
          else if TestReport.FoodRep > TestReport.Eaten - 63 then
            SuperValue := SuperValue or
              (63 - (TestReport.Eaten - TestReport.FoodRep)) shl 24;

          SuperPlus := SuperValue - BestSuperValue;
          if SuperPlus >= 0 then
          begin
            Output[oTax] := TestReport.Tax;
            Output[oScience] := TestReport.Science;

            if TestReport.FoodRep < TestReport.Eaten then
              Output[oFood] := TestReport.FoodRep
              // appreciate what we have, combination will have bad supervalue anyway
            else if FoodWasted then
              Output[oFood] := 0
            else
            begin
              Output[oFood] := TestReport.FoodRep - TestReport.Eaten;
              if FoodToTax or (Size >= NeedAqueductSize) and (Output[oFood] = 1)
              then
              begin
                Inc(Output[oTax], Output[oFood]);
                Output[oFood] := 0;
              end;
            end;

            if TestReport.ProdRep < TestReport.Support then
              Output[oProd] := TestReport.ProdRep
              // appreciate what we have, combination will have bad supervalue anyway
            else
            begin
              if NeedRare > 0 then
              begin
                RareOK := False;
                for iH := 0 to nHierarchy - 1 do
                  if Hierarchy[iH, nSelection[iH]].V21 and RareTiles <> 0 then
                    RareOK := True;
                if not RareOK then
                  TestReport.ProdRep := TestReport.Support;
              end;
              Output[oProd] := TestReport.ProdRep - TestReport.Support;
              if ProdToTax then
              begin
                Inc(Output[oTax], Output[oProd]);
                Output[oProd] := 0;
              end;
            end;

            NeedStep2 := False;
            Value := 0;
            for I := oFood to oScience do
              if ValueFormula_Multiply[I] then
                NeedStep2 := True
              else
                Value := Value + ValueFormula_Weight[I] * Output[I];
            if NeedStep2 then
            begin
              if Value > 0 then
                Value := ln(Value) + 123;
              for I := oFood to oScience do
                if ValueFormula_Multiply[I] and (Output[I] > 0) then
                  Value := Value + ValueFormula_Weight[I] *
                    (ln(Output[I]) + 123);
            end;

            ValuePlus := Value - BestValue;
            if (SuperPlus > 0) or (ValuePlus >= 0.0) then
            begin
              SubValue := (TestReport.FoodRep + ProdBeforeBoost +
                TestReport.Trade) shl 18;
              TestTiles := 1 shl CityOwnTile;
              for iH := 0 to nHierarchy - 1 do
              begin
                Inc(TestTiles, Hierarchy[iH, nSelection[iH]].V21);
                Inc(SubValue, Hierarchy[iH, nSelection[iH]].SubValue);
              end;
              IsBest := True;
              if (SuperPlus = 0) and (ValuePlus = 0.0) then
              begin
                SubPlus := SubValue - BestSubValue;
                if SubPlus < 0 then
                  IsBest := False
                else if SubPlus = 0 then
                begin
                  Assert(TestTiles <> BestTiles);
                  IsBest := TestTiles > BestTiles
                end
              end;
              if IsBest then
              begin
                BestSuperValue := SuperValue;
                BestValue := Value;
                BestSubValue := SubValue;
                BestTiles := TestTiles;
                TestReport.Happy :=
                  (CityReportEx.TradeProcessing.HappyBase - Size) div 2 +
                  TestReport.Lux shr 1;
                Advice.CityReport := TestReport;
              end;
            end; // if (SuperPlus>0) or (ValuePlus>=0.0)
          end; // if SuperPlus>=0
        end;
      end;

      // calculate next combination
      iH_Switch := 0;
      repeat
        with Hierarchy[iH_Switch, nSelection[iH_Switch]] do
        begin
          Dec(TestReport.FoodRep, Food);
          Dec(ProdBeforeBoost, Prod);
          Dec(TestReport.Trade, Trade);
        end;
        Inc(nSelection[iH_Switch]);
        Inc(TestReport.Working);
        if (nSelection[iH_Switch] <= nTile[iH_Switch]) and
          (TestReport.Working <= MaxWorking) then
        begin
          with Hierarchy[iH_Switch, nSelection[iH_Switch]] do
          begin
            Inc(TestReport.FoodRep, Food);
            Inc(ProdBeforeBoost, Prod);
            Inc(TestReport.Trade, Trade);
          end;
          Break;
        end;
        Dec(TestReport.Working, nSelection[iH_Switch]);
        nSelection[iH_Switch] := 0;
        Inc(iH_Switch);
      until iH_Switch = nHierarchy;
    until iH_Switch = nHierarchy; // everything tested -- done
  end;
  Assert(BestSuperValue > 0); // advice should always be possible
  Advice.Tiles := BestTiles;
  Advice.CityReport.HypoTiles := BestTiles;
end;

{
  Start/End Game
  ____________________________________________________________________
}
procedure InitGame;
var
  P, I, mixTownGuard: Integer;
begin
  MaxDist := Distance(0, MapSize - lx shr 1);
  for P := 0 to nPl - 1 do
    if (1 shl P and GAlive <> 0) then
      with RW[P] do
      begin // initialize capital
        mixTownGuard := 0;
        while Model[mixTownGuard].Kind <> mkSpecial_TownGuard do
          Inc(mixTownGuard);
        with City[0] do
        begin
          Built[imPalace] := 1;
          Size := 4;
          for I := 2 to Size do
            AddBestCityTile(P, 0);
          Project := mixTownGuard;
        end;
        NatBuilt[imPalace] := 1;
      end;
end;

procedure ReleaseGame;
begin
end;

end.
