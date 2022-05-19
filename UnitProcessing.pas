{$INCLUDE Switches.inc}
unit UnitProcessing;

interface

uses
  SysUtils, Protocol, Database;

type
  TMoveType = (mtInvalid, mtMove, mtCapture, mtSpyMission, mtAttack,
    mtBombard, mtExpel);

  TMoveInfo = record
    MoveType: TMoveType;
    Cost, ToMaster, EndHealth, Defender, Dcix, Duix, EndHealthDef: Integer;
    MountainDelay: Boolean;
  end;

var
  uixSelectedTransport: Integer;
  Worked: array [0 .. nPl - 1] of Integer; { settler work statistics }

  // Moving/Combat
function HostileDamage(P, mix, Loc, MP: Integer): Integer;
function CalculateMove(P, uix, ToLoc, MoveLength: Integer; TestOnly: Boolean;
  var MoveInfo: TMoveInfo): Integer;
function GetBattleForecast(Loc: Integer; var BattleForecast: TBattleForecast;
  var Duix, Dcix, AStr, DStr, ABaseDamage, DBaseDamage: Integer): Integer;
function LoadUnit(P, uix: Integer; TestOnly: Boolean): Integer;
function UnloadUnit(P, uix: Integer; TestOnly: Boolean): Integer;
procedure Recover(P, uix: Integer);
function GetMoveAdvice(P, uix: Integer; var A: TMoveAdviceData): Integer;
function CanPlaneReturn(P, uix: Integer;
  PlaneReturnData: TPlaneReturnData): Boolean;

// Terrain Improvement
function StartJob(P, uix, NewJob: Integer; TestOnly: Boolean): Integer;
function Work(P, uix: Integer): Boolean;
function GetJobProgress(P, Loc: Integer;
  var JobProgressData: TJobProgressData): Integer;

// Start/End Game
procedure InitGame;
procedure ReleaseGame;


implementation

uses
  IPQ;

const
  eMountains = $6000FFFF; // additional return code for server internal use

  // tile control flags
  coKnown = $02;
  coTrue = $04;

  ContraJobs: array [0 .. nJob - 1] of Set of 0 .. nJob - 1 = ([], // jNone
    [jCity], // jRoad
    [jCity], // jRR
    [jCity, jTrans], // jClear
    [jCity, jFarm, jAfforest, jMine, jBase, jFort], // jIrr
    [jCity, jIrr, jAfforest, jMine, jBase, jFort], // jFarm
    [jCity, jIrr, jFarm, jTrans], // jAfforest
    [jCity, jTrans, jIrr, jFarm, jBase, jFort], // jMine
    [jCity, jTrans], // jCanal
    [jCity, jClear, jAfforest, jMine, jCanal], // jTrans
    [jCity, jIrr, jFarm, jMine, jBase], // jFort
    [jCity], // jPoll
    [jCity, jIrr, jFarm, jMine, jFort], // jBase
    [jCity], // jPillage
    [jRoad .. jPillage]); // jCity

type
  TToWorkList = array [0 .. INFIN, 0 .. nJob - 1] of Word;

var
  ToWork: ^TToWorkList; { work left for each tile and job }

  {
    Moving/Combat
    ____________________________________________________________________
  }
function HostileDamage(P, mix, Loc, MP: Integer): Integer;
var
  Tile: Integer;
begin
  Tile := RealMap[Loc];
  if (RW[P].Model[mix].Domain >= dSea) or (RW[P].Model[mix].Kind = mkSettler)
    and (RW[P].Model[mix].Speed >= 200) or
    (Tile and (fCity or fRiver or fCanal) <> 0) or (Tile and fTerImp = tiBase)
    or (GWonder[woGardens].EffectiveOwner = P) then
    Result := 0
  else if (Tile and fTerrain = fDesert) and
    (Tile and fSpecial <> fSpecial1 { Oasis } ) then
  begin
    Assert((Tile and fTerImp <> tiIrrigation) and (Tile and fTerImp <> tiFarm));
    Result := (DesertThurst * MP - 1) div RW[P].Model[mix].Speed + 1
  end
  else if Tile and fTerrain = fArctic then
  begin
    Assert((Tile and fTerImp <> tiIrrigation) and (Tile and fTerImp <> tiFarm));
    Result := (ArcticThurst * MP - 1) div RW[P].Model[mix].Speed + 1
  end
  else
    Result := 0;
end;

function Controlled(P, Loc: Integer; IsDest: Boolean): Integer;
{ whether tile at Loc is in control zone of enemy unit
  returns combination of tile control flags }
var
  Loc1, V8: Integer;
  Adjacent: TVicinity8Loc;
begin
  Result := 0;
  if IsDest and (Occupant[Loc] = P) and (ZoCMap[Loc] > 0) then
    Exit;
  // destination tile, not controlled if already occupied

  if (RealMap[Loc] and fCity = 0) or (Integer(RealMap[Loc] shr 27) <> P) and
    (ServerVersion[P] >= $000EF0) then
  begin // not own city
    V8_to_Loc(Loc, Adjacent);
    for V8 := 0 to 7 do
    begin
      Loc1 := Adjacent[V8];
      if (Loc1 >= 0) and (Loc1 < MapSize) and (ZoCMap[Loc1] > 0) and
        (Occupant[Loc1] >= 0) and (Occupant[Loc1] <> P) and
        (RW[P].Treaty[Occupant[Loc1]] < trAlliance) then
        if ObserveLevel[Loc1] and (3 shl (P * 2)) > 0 then
        begin // p observes tile
          Result := coKnown or coTrue;
          Exit;
        end
        else
          Result := coTrue; // p does not observe tile
    end;
  end;
end;

function GetMoveCost(P, mix, FromLoc, ToLoc, MoveLength: Integer;
  var MoveCost: Integer): Integer;
// MoveLength - 2 for short move, 3 for long move
var
  FromTile, ToTile: Integer;
begin
  Result := eOK;
  FromTile := RealMap[FromLoc];
  ToTile := RealMap[ToLoc];
  with RW[P].Model[mix] do
  begin
    case Domain of
      dGround:
        if (ToTile and fTerrain >= fGrass) then { domain ok }
        // if (Flags and mdCivil<>0) and (ToTile and fDeadLands<>0) then result:=eEerie
        // else
        begin { valid move }
          if (FromTile and (fRR or fCity) <> 0) and
            (ToTile and (fRR or fCity) <> 0) then
            if GWonder[woShinkansen].EffectiveOwner = P then
              MoveCost := 0
            else
              MoveCost := Speed * (4 * 1311) shr 17 // move along railroad
          else if (FromTile and (fRoad or fRR or fCity) <> 0) and
            (ToTile and (fRoad or fRR or fCity) <> 0) or
            (FromTile and ToTile and (fRiver or fCanal) <> 0) or
            (Cap[mcAlpine] > 0) then
            // move along road, river or canal
            if Cap[mcOver] > 0 then
              MoveCost := 40
            else
              MoveCost := 20
          else if Cap[mcOver] > 0 then
            Result := eNoRoad
          else
            case Terrain[ToTile and fTerrain].MoveCost of
              1:
                MoveCost := 50; // plain terrain
              2:
                begin
                  Assert(Speed - 150 <= 600);
                  MoveCost := 50 + (Speed - 150) * 13 shr 7; // heavy terrain
                end;
              3:
                begin
                  MoveCost := Speed;
                  Result := eMountains;
                  Exit;
                end;
            end;
          MoveCost := MoveCost * MoveLength;
        end
        else
          Result := eDomainMismatch;

      dSea:
        if (ToTile and (fCity or fCanal) <> 0) or (ToTile and fTerrain < fGrass)
        then { domain ok }
          if (ToTile and fTerrain <> fOcean) or (Cap[mcNav] > 0) then
            MoveCost := 50 * MoveLength { valid move }
          else
            Result := eNoNav { navigation required for open sea }
        else
          Result := eDomainMismatch;

      dAir:
        MoveCost := 50 * MoveLength; { always valid move }
    end;
  end;
end;

function CalculateMove(P, uix, ToLoc, MoveLength: Integer; TestOnly: Boolean;
  var MoveInfo: TMoveInfo): Integer;
var
  uix1, p1, FromLoc, DestControlled, AStr, DStr, ABaseDamage,
    DBaseDamage: Integer;
  PModel: ^TModel;
  BattleForecast: TBattleForecast;
begin
  with RW[P], Un[uix] do
  begin
    PModel := @Model[mix];
    FromLoc := Loc;

    BattleForecast.pAtt := P;
    BattleForecast.mixAtt := mix;
    BattleForecast.HealthAtt := Health;
    BattleForecast.ExpAtt := Exp;
    BattleForecast.FlagsAtt := Flags;
    BattleForecast.Movement := Movement;
    Result := GetBattleForecast(ToLoc, BattleForecast, MoveInfo.Duix,
      MoveInfo.Dcix, AStr, DStr, ABaseDamage, DBaseDamage);

    if Result = eHiddenUnit then
      if TestOnly then
        Result := eOK // behave just like unit was moving
      else if Mode > moLoading_Fast then
        Map[ToLoc] := Map[ToLoc] or fHiddenUnit;
    if Result = eStealthUnit then
      if TestOnly then
        Result := eOK // behave just like unit was moving
      else if Mode > moLoading_Fast then
        Map[ToLoc] := Map[ToLoc] or fStealthUnit;
    if Result < rExecuted then
      Exit;

    case Result of
      eOK:
        MoveInfo.MoveType := mtMove;
      eExpelled:
        MoveInfo.MoveType := mtExpel;
    else
      MoveInfo.MoveType := mtAttack;
    end;

    if MoveInfo.MoveType = mtMove then
    begin
      if Mode = moPlaying then
      begin
        p1 := RealMap[ToLoc] shr 27;
        if (p1 < nPl) and (p1 <> P) and
          ((RealMap[Loc] shr 27 <> Cardinal(p1)) and (PModel.Kind <> mkDiplomat)
          and (Treaty[p1] >= trPeace) and (Treaty[p1] < trAlliance) or
          (RealMap[ToLoc] and fCity <> 0) and (Treaty[p1] >= trPeace)) then
        begin
          Result := eTreaty;
          Exit;
        end; // keep peace treaty!
      end;
      if (RealMap[ToLoc] and fCity <> 0) and
        (RealMap[ToLoc] shr 27 <> Cardinal(P)) then // empty enemy city
        if PModel.Kind = mkDiplomat then
        begin
          MoveInfo.MoveType := mtSpyMission;
        end
        else if PModel.Domain = dGround then
        begin
          if PModel.Flags and mdCivil <> 0 then
          begin
            Result := eNoCapturer;
            Exit;
          end;
          MoveInfo.MoveType := mtCapture;
        end
        else
        begin
          if (PModel.Domain = dSea) and (PModel.Cap[mcArtillery] = 0) then
          begin
            Result := eDomainMismatch;
            Exit;
          end
          else if (PModel.Attack = 0) and
            not((PModel.Cap[mcBombs] > 0) and (Flags and unBombsLoaded <> 0))
          then
          begin
            Result := eNoBombarder;
            Exit;
          end
          else if Movement < 100 then
          begin
            Result := eNoTime_Bombard;
            Exit;
          end;
          MoveInfo.MoveType := mtBombard;
          Result := eBombarded;
        end;
    end;

    MoveInfo.MountainDelay := False;
    if MoveInfo.MoveType in [mtAttack, mtBombard, mtExpel] then
    begin
      if (Master >= 0) or (PModel.Domain = dSea) and
        (RealMap[Loc] and fTerrain >= fGrass) or (PModel.Domain = dAir) and
        ((RealMap[Loc] and fCity <> 0) or (RealMap[Loc] and fTerImp = tiBase))
      then
      begin
        Result := eViolation;
        Exit;
      end;
      if MoveInfo.MoveType = mtBombard then
      begin
        MoveInfo.EndHealth := Health;
        MoveInfo.EndHealthDef := -1;
      end
      else
      begin
        MoveInfo.EndHealth := BattleForecast.EndHealthAtt;
        MoveInfo.EndHealthDef := BattleForecast.EndHealthDef;
      end;
    end
    else // if MoveInfo.MoveType in [mtMove,mtCapture,mtSpyMission] then
    begin
      if (Master >= 0) and (PModel.Domain < dSea) then
      begin // transport unload
        MoveInfo.Cost := PModel.Speed;
        if RealMap[ToLoc] and fTerrain < fGrass then
          Result := eDomainMismatch;
      end
      else
      begin
        Result := GetMoveCost(P, mix, FromLoc, ToLoc, MoveLength,
          MoveInfo.Cost);
        if Result = eMountains then
        begin
          Result := eOK;
          MoveInfo.MountainDelay := True
        end;
      end;
      if (Result >= rExecuted) and (MoveInfo.MoveType = mtSpyMission) then
        Result := eMissionDone;

      MoveInfo.ToMaster := -1;
      if (Result = eDomainMismatch) and (PModel.Domain < dSea) and
        (PModel.Cap[mcOver] = 0) then
      begin
        for uix1 := 0 to nUn - 1 do
          with Un[uix1] do // check load to transport
            if (Loc = ToLoc) and
              (TroopLoad < Model[mix].MTrans * Model[mix].Cap[mcSeaTrans]) then
            begin
              Result := eLoaded;
              MoveInfo.Cost := PModel.Speed;
              MoveInfo.ToMaster := uix1;
              if (uixSelectedTransport >= 0) and (uix1 = uixSelectedTransport)
              then
                Break;
            end;
      end
      else if (PModel.Domain = dAir) and (PModel.Cap[mcAirTrans] = 0) and
        (RealMap[ToLoc] and fCity = 0) and (RealMap[ToLoc] and fTerImp <> tiBase)
      then
      begin
        for uix1 := 0 to nUn - 1 do
          with Un[uix1] do
            if (Loc = ToLoc) and
              (AirLoad < Model[mix].MTrans * Model[mix].Cap[mcCarrier]) then
            begin // load plane to ship
              Result := eLoaded;
              MoveInfo.ToMaster := uix1;
              if (uixSelectedTransport >= 0) and (uix1 = uixSelectedTransport)
              then
                Break;
            end;
      end;
      if Result < rExecuted then
        Exit;

      if (Master < 0) and (MoveInfo.ToMaster < 0) then
        MoveInfo.EndHealth := Health - HostileDamage(P, mix, ToLoc,
          MoveInfo.Cost)
      else
        MoveInfo.EndHealth := Health;

      if (Mode = moPlaying) and (PModel.Flags and mdZOC <> 0) and (Master < 0)
        and (MoveInfo.ToMaster < 0) and (Controlled(P, FromLoc, False) >= coTrue)
      then
      begin
        DestControlled := Controlled(P, ToLoc, True);
        if DestControlled >= coTrue + coKnown then
        begin
          Result := eZOC;
          Exit;
        end
        else if not TestOnly and (DestControlled >= coTrue) then
        begin
          Result := eZOC_EnemySpotted;
          Exit;
        end;
      end;
      if (Movement = 0) and (ServerVersion[P] >= $0100F1) or
        (MoveInfo.Cost > Movement) then
        if (Master >= 0) or (MoveInfo.ToMaster >= 0) then
        begin
          Result := eNoTime_Load;
          Exit;
        end
        else
        begin
          Result := eNoTime_Move;
          Exit;
        end;
      if (MoveInfo.EndHealth <= 0) or (MoveInfo.MoveType = mtSpyMission) then
        Result := Result or rUnitRemoved;
      // spy mission or victim of HostileDamage

    end; // if MoveInfo.MoveType in [mtMove,mtCapture,mtSpyMission]

    if MoveInfo.MoveType in [mtAttack, mtExpel] then
      MoveInfo.Defender := Occupant[ToLoc]
    else if RealMap[ToLoc] and fCity <> 0 then
    begin // MoveInfo.Dcix not set yet
      MoveInfo.Defender := RealMap[ToLoc] shr 27;
      SearchCity(ToLoc, MoveInfo.Defender, MoveInfo.Dcix);
    end;
  end;
end;

function GetBattleForecast(Loc: Integer; var BattleForecast: TBattleForecast;
  var Duix, Dcix, AStr, DStr, ABaseDamage, DBaseDamage: Integer): Integer;
var
  Time, Defender, ABon, DBon, DCnt, MultiDamage: Integer;
  PModel, DModel: ^TModel;
begin
  with BattleForecast do
  begin
    Defender := Occupant[Loc];
    if (Defender < 0) or (Defender = pAtt) then
    begin
      Result := eOK;
      Exit;
    end; // no attack, simple move

    PModel := @RW[pAtt].Model[mixAtt];
    Strongest(Loc, Duix, DStr, DBon, DCnt); { get defense strength and bonus }
    if (PModel.Kind = mkDiplomat) and (RealMap[Loc] and fCity <> 0) then
    begin // spy mission -- return as if move was possible
      EndHealthAtt := HealthAtt;
      EndHealthDef := RW[Defender].Un[Duix].Health;
      Result := eOK;
      Exit;
    end;

    DModel := @RW[Defender].Model[RW[Defender].Un[Duix].mix];
    if (RealMap[Loc] and fCity = 0) and (RealMap[Loc] and fTerImp <> tiBase)
    then
    begin
      if (DModel.Cap[mcSub] > 0) and (RealMap[Loc] and fTerrain < fGrass) and
        (ObserveLevel[Loc] shr (2 * pAtt) and 3 < lObserveAll) then
      begin
        Result := eHiddenUnit;
        Exit;
      end; // attacking submarine not allowed
      if (DModel.Cap[mcStealth] > 0) and
        (ObserveLevel[Loc] shr (2 * pAtt) and 3 <> lObserveSuper) then
      begin
        Result := eStealthUnit;
        Exit;
      end; // attacking stealth aircraft not allowed
      if (DModel.Domain = dAir) and (DModel.Kind <> mkSpecial_Glider) and
        (PModel.Domain <> dAir) then
      begin
        Result := eDomainMismatch;
        Exit;
      end; // can't attack plane
    end;
    if ((PModel.Cap[mcArtillery] = 0) or ((ServerVersion[pAtt] >= $010200) and
      (RealMap[Loc] and fTerrain < fGrass) and (DModel.Cap[mcSub] > 0)))
    // ground units can't attack submarines
      and ((PModel.Domain = dGround) and (RealMap[Loc] and fTerrain < fGrass) or
      (PModel.Domain = dSea) and (RealMap[Loc] and fTerrain >= fGrass)) then
    begin
      Result := eDomainMismatch;
      Exit;
    end;
    if (PModel.Attack = 0) and not((PModel.Cap[mcBombs] > 0) and
      (FlagsAtt and unBombsLoaded <> 0) and (DModel.Domain < dAir)) then
    begin
      Result := eInvalid;
      Exit;
    end;

    if Movement = 0 then
    begin
      Result := eNoTime_Attack;
      Exit;
    end;

{$IFOPT O-}Assert(InvalidTreatyMap = 0); {$ENDIF}
    if RW[pAtt].Treaty[Defender] >= trPeace then
    begin
      if (PModel.Domain <> dAir) and (PModel.Attack > 0) and
        (Integer(RealMap[Loc] shr 27) = pAtt) then
        if Movement >= 100 then
        begin // expel friendly unit
          EndHealthDef := RW[Defender].Un[Duix].Health;
          EndHealthAtt := HealthAtt;
          Result := eExpelled
        end
        else
          Result := eNoTime_Expel
      else
        Result := eTreaty;
      Exit;
    end;

    // calculate defender strength
    if RealMap[Loc] and fCity <> 0 then
    begin // consider city improvements
      SearchCity(Loc, Defender, Dcix);
      if (PModel.Domain < dSea) and (PModel.Cap[mcArtillery] = 0) and
        ((RW[Defender].City[Dcix].Built[imWalls] = 1) or
        (Continent[RW[Defender].City[Dcix].Loc] = GrWallContinent[Defender]))
      then
        Inc(DBon, 8)
      else if (PModel.Domain = dSea) and
        (RW[Defender].City[Dcix].Built[imCoastalFort] = 1) then
        Inc(DBon, 4)
      else if (PModel.Domain = dAir) and
        (RW[Defender].City[Dcix].Built[imMissileBat] = 1) then
        Inc(DBon, 4);
      if RW[Defender].City[Dcix].Built[imBunker] = 1 then
        Inc(DBon, 4)
    end;
    if (PModel.Domain = dAir) and (DModel.Cap[mcAirDef] > 0) then
      Inc(DBon, 4);
    DStr := DModel.Defense * DBon * 100;
    if (DModel.Domain = dAir) and ((RealMap[Loc] and fCity <> 0) or
      (RealMap[Loc] and fTerImp = tiBase)) then
      DStr := 0;
    if (DModel.Domain = dSea) and (RealMap[Loc] and fTerrain >= fGrass) then
      DStr := DStr shr 1;

    // calculate attacker strength
    if PModel.Cap[mcWill] > 0 then
      Time := 100
    else
    begin
      Time := Movement;
      if Time > 100 then
        Time := 100;
    end;
    ABon := 4 + ExpAtt div ExpCost;
    AStr := PModel.Attack;
    if (FlagsAtt and unBombsLoaded <> 0) and (DModel.Domain < dAir) then
    // use bombs
      AStr := AStr + PModel.Cap[mcBombs] * PModel.MStrength * 2;
    AStr := Time * AStr * ABon;

    // calculate base damage for defender
    if DStr = 0 then
      DBaseDamage := RW[Defender].Un[Duix].Health
    else
    begin
      DBaseDamage := HealthAtt * AStr div DStr;
      if DBaseDamage = 0 then
        DBaseDamage := 1;
      if DBaseDamage > RW[Defender].Un[Duix].Health then
        DBaseDamage := RW[Defender].Un[Duix].Health;
    end;

    // calculate base damage for attacker
    if AStr = 0 then
      ABaseDamage := HealthAtt
    else
    begin
      ABaseDamage := RW[Defender].Un[Duix].Health * DStr div AStr;
      if ABaseDamage = 0 then
        ABaseDamage := 1;
      if ABaseDamage > HealthAtt then
        ABaseDamage := HealthAtt;
    end;

    // calculate final damage for defender
    MultiDamage := 2;
    if (ABaseDamage = HealthAtt) and (PModel.Cap[mcFanatic] > 0) and
      not(RW[pAtt].Government in [gRepublic, gDemocracy, gFuture]) then
      MultiDamage := MultiDamage * 2; // fanatic attacker died
    EndHealthDef := RW[Defender].Un[Duix].Health - MultiDamage *
      DBaseDamage div 2;
    if EndHealthDef < 0 then
      EndHealthDef := 0;

    // calculate final damage for attacker
    MultiDamage := 2;
    if DBaseDamage = RW[Defender].Un[Duix].Health then
    begin
      if (DModel.Cap[mcFanatic] > 0) and
        not(RW[Defender].Government in [gRepublic, gDemocracy, gFuture]) then
        MultiDamage := MultiDamage * 2; // fanatic defender died
      if PModel.Cap[mcFirst] > 0 then
        MultiDamage := MultiDamage shr 1; // first strike unit wins
    end;
    Time := Movement;
    if Time > 100 then
      Time := 100;
    EndHealthAtt := HealthAtt - MultiDamage * ABaseDamage div 2 -
      HostileDamage(pAtt, mixAtt, Loc, Time);
    if EndHealthAtt < 0 then
      EndHealthAtt := 0;

    if EndHealthDef > 0 then
      Result := eLost
    else if EndHealthAtt > 0 then
      Result := eWon
    else
      Result := eBloody;
  end;
end;

function LoadUnit(P, uix: Integer; TestOnly: Boolean): Integer;
var
  uix1, D, Cost, ToMaster: Integer;
begin
  Result := eOK;
  with RW[P].Un[uix] do
  begin
    D := RW[P].Model[mix].Domain;
    if (Master >= 0) or (D = dSea) or
      (RW[P].Model[mix].Cap[mcAirTrans] + RW[P].Model[mix].Cap[mcOver] > 0) then
      Result := eViolation
    else
    begin
      ToMaster := -1;
      for uix1 := 0 to RW[P].nUn - 1 do
        if RW[P].Un[uix1].Loc = Loc then
          with RW[P].Un[uix1], RW[P].Model[mix] do
            if (D < dSea) and
              (TroopLoad < MTrans * (Cap[mcSeaTrans] + Cap[mcAirTrans])) or
              (D = dAir) and (AirLoad < MTrans * Cap[mcCarrier]) then
            begin { load onto unit uix1 }
              if (uixSelectedTransport < 0) or (uix1 = uixSelectedTransport)
              then
              begin
                ToMaster := uix1;
                Break;
              end
              else if ToMaster < 0 then
                ToMaster := uix1;
            end;
      if ToMaster < 0 then
        Result := eNoLoadCapacity
      else
      begin
        if D = dAir then
          Cost := 100
        else
          Cost := RW[P].Model[mix].Speed;
        if Movement < Cost then
          Result := eNoTime_Load
        else if not TestOnly then
        begin
          FreeUnit(P, uix);
          Dec(Movement, Cost);
          if D = dAir then
            Inc(RW[P].Un[ToMaster].AirLoad)
          else
            Inc(RW[P].Un[ToMaster].TroopLoad);
          Master := ToMaster;
          UpdateUnitMap(Loc);
        end;
      end;
    end;
  end;
end;

function UnloadUnit(P, uix: Integer; TestOnly: Boolean): Integer;
var
  Cost: Integer;
begin
  Result := eOK;
  with RW[P].Un[uix] do
    if Master < 0 then
      Result := eNotChanged
    else if (RW[P].Model[mix].Domain < dSea) and
      (RealMap[Loc] and fTerrain < fGrass) then
      Result := eDomainMismatch
      // else if (RW[p].Model[mix].Domain<dSea)
      // and (RW[p].Model[mix].Flags and mdCivil<>0)
      // and (RealMap[Loc] and fDeadLands<>0) then result:=eEerie
    else
    begin
      if RW[P].Model[mix].Domain = dAir then
        Cost := 100
      else
        Cost := RW[P].Model[mix].Speed;
      if Movement < Cost then
        Result := eNoTime_Load
      else if not TestOnly then
      begin
        Dec(Movement, Cost);
        if RW[P].Model[mix].Domain = dAir then
          Dec(RW[P].Un[Master].AirLoad)
        else
        begin
          Dec(RW[P].Un[Master].TroopLoad);
          // Movement:=0 // no more movement after unload
        end;
        Master := -1;
        PlaceUnit(P, uix);
        UpdateUnitMap(Loc);
      end;
    end;
end;

procedure Recover(P, uix: Integer);
var
  cix, Recovery: Integer;
begin
  with RW[P], Un[uix] do
  begin
    if (Master >= 0) and (Model[Un[Master].mix].Cap[mcSupplyShip] > 0) then
      Recovery := FastRecovery { hospital ship }
    else if RealMap[Loc] and fTerImp = tiBase then
      Recovery := CityRecovery
    else if RealMap[Loc] and fCity <> 0 then
    begin { unit in city }
      cix := nCity - 1;
      while (cix >= 0) and (City[cix].Loc <> Loc) do
        Dec(cix);
      if City[cix].Flags and chDisorder <> 0 then
        Recovery := NoCityRecovery
      else if (Model[mix].Domain = dGround) and
        (City[cix].Built[imBarracks] + City[cix].Built[imElite] > 0) or
        (Model[mix].Domain = dSea) and (City[cix].Built[imDockyard] = 1) or
        (Model[mix].Domain = dAir) and (City[cix].Built[imAirport] = 1) then
        Recovery := FastRecovery { city has baracks/shipyard/airport }
      else
        Recovery := CityRecovery
    end
    else if (RealMap[Loc] and fTerrain >= fGrass) and (Model[mix].Domain <> dAir)
    then
      Recovery := NoCityRecovery
    else
      Recovery := 0;

    Recovery := Recovery * Movement div Model[mix].Speed;
    { recovery depends on movement unused }
    if Recovery > Health then
      Recovery := Health; // health max. doubled each turn
    if Recovery > 100 - Health then
      Recovery := 100 - Health;
    Inc(Health, Recovery);
  end;
end;

function GetMoveAdvice(P, uix: Integer; var A: TMoveAdviceData): Integer;
const
  // domains
  gmaAir = 0;
  gmaSea = 1;
  gmaGround_NoZoC = 2;
  gmaGround_ZoC = 3;
  // flags
  gmaNav = 4;
  gmaOver = 4;
  gmaAlpine = 8;
var
  I, FromLoc, EndLoc, T, T1, maxmov, initmov, Loc, Loc1, FromTile, ToTile, V8,
    MoveInfo, HeavyCost, RailCost, MoveCost, AddDamage, MaxDamage,
    MovementLeft: Integer;
  Map: ^TTileList;
  Q: TIPQ;
  Adjacent: TVicinity8Loc;
  From: array [0 .. lxmax * lymax - 1] of Integer;
  Time: array [0 .. lxmax * lymax - 1] of Integer;
  Damage: array [0 .. lxmax * lymax - 1] of Integer;
  MountainDelay, Resistant: Boolean;
  // tt,tt0: int64;
begin
  // QueryPerformanceCounter(tt0);

  MaxDamage := RW[P].Un[uix].Health - 1;
  if MaxDamage > A.MaxHostile_MovementLeft then
    if A.MaxHostile_MovementLeft >= 0 then
      MaxDamage := A.MaxHostile_MovementLeft
    else
      MaxDamage := 0;

  Map := @(RW[P].Map^);
  if (A.ToLoc <> maNextCity) and ((A.ToLoc < 0) or (A.ToLoc >= MapSize)) then
  begin
    Result := eInvalid;
    Exit;
  end;
  if (A.ToLoc <> maNextCity) and (Map[A.ToLoc] and fTerrain = fUNKNOWN) then
  begin
    Result := eNoWay;
    Exit;
  end;

  with RW[P].Model[RW[P].Un[uix].mix] do
    case Domain of
      dGround:
        if (A.ToLoc <> maNextCity) and (Map[A.ToLoc] and fTerrain = fOcean) then
        begin
          Result := eDomainMismatch;
          Exit;
        end
        else
        begin
          if Flags and mdZOC <> 0 then
            MoveInfo := gmaGround_ZoC
          else
            MoveInfo := gmaGround_NoZoC;
          if Cap[mcOver] > 0 then
            Inc(MoveInfo, gmaOver);
          if Cap[mcAlpine] > 0 then
            Inc(MoveInfo, gmaAlpine);
          HeavyCost := 50 + (Speed - 150) * 13 shr 7;
          if GWonder[woShinkansen].EffectiveOwner = P then
            RailCost := 0
          else
            RailCost := Speed * (4 * 1311) shr 17;
          maxmov := Speed;
          initmov := 0;
          Resistant := (GWonder[woGardens].EffectiveOwner = P) or
            (Kind = mkSettler) and (Speed >= 200);
        end;
      dSea:
        if (A.ToLoc <> maNextCity) and (Map[A.ToLoc] and fTerrain >= fGrass) and
          (Map[A.ToLoc] and (fCity or fUnit or fCanal) = 0) then
        begin
          Result := eDomainMismatch;
          Exit;
        end
        else
        begin
          MoveInfo := gmaSea;
          if Cap[mcNav] > 0 then
            Inc(MoveInfo, gmaNav);
          maxmov := UnitSpeed(P, RW[P].Un[uix].mix, 100);
          initmov := maxmov - UnitSpeed(P, RW[P].Un[uix].mix,
            RW[P].Un[uix].Health);
        end;
      dAir:
        begin
          MoveInfo := gmaAir;
          maxmov := Speed;
          initmov := 0;
        end;
    end;

  FromLoc := RW[P].Un[uix].Loc;
  FillChar(Time, SizeOf(Time), 255); { -1 }
  Damage[FromLoc] := 0;
  Q := TIPQ.Create(MapSize);
  Q.Put(FromLoc, (maxmov - RW[P].Un[uix].Movement) shl 8);
  while Q.Get(Loc, T) do
  begin
    Time[Loc] := T;
    if T >= (A.MoreTurns + 1) shl 20 then
    begin
      Loc := -1;
      Break;
    end;
    FromTile := Map[Loc];
    if (Loc = A.ToLoc) or (A.ToLoc = maNextCity) and (FromTile and fCity <> 0)
    then
      Break;
    if T and $FFF00 = $FFF00 then
      Inc(T, $100000); // indicates mountain delay
    V8_to_Loc(Loc, Adjacent);
    for V8 := 0 to 7 do
    begin
      Loc1 := Adjacent[V8];
      if (Loc1 >= 0) and (Loc1 < MapSize) and (Time[Loc1] < 0) then
      begin
        ToTile := Map[Loc1];
        if (Loc1 = A.ToLoc) and (ToTile and (fUnit or fOwned) = fUnit) and
          not((MoveInfo and 3 = gmaSea) and (FromTile and fTerrain >= fGrass))
          and not((MoveInfo and 3 = gmaAir) and ((FromTile and fCity <> 0) or
          (FromTile and fTerImp = tiBase))) then
        begin // attack position found
          if Q.Put(Loc1, T + 1) then
            From[Loc1] := Loc;
        end
        else if (ToTile and fTerrain <> fUNKNOWN) and
          ((Loc1 = A.ToLoc) or (ToTile and (fCity or fOwned) <> fCity))
        // don't move through enemy cities
          and ((Loc1 = A.ToLoc) or (ToTile and (fUnit or fOwned) <> fUnit))
        // way is blocked
          and (ToTile and not FromTile and fPeace = 0) and
          ((MoveInfo and 3 < gmaGround_ZoC) or (ToTile and FromTile and
          fInEnemyZoc = 0) or (ToTile and fOwnZoCUnit <> 0) or
          (FromTile and fCity <> 0) or (ToTile and (fCity or fOwned) = fCity or
          fOwned)) then
        begin
          // calculate move cost, must be identic to GetMoveCost function
          AddDamage := 0;
          MountainDelay := False;
          case MoveInfo of

            gmaAir:
              MoveCost := 50; { always valid move }

            gmaSea:
              if (ToTile and (fCity or fCanal) <> 0) or
                (ToTile and fTerrain = fShore) then { domain ok }
                MoveCost := 50 { valid move }
              else
                MoveCost := -1;

            gmaSea + gmaNav:
              if (ToTile and (fCity or fCanal) <> 0) or
                (ToTile and fTerrain < fGrass) then { domain ok }
                MoveCost := 50 { valid move }
              else
                MoveCost := -1;

          else // ground unit
            if (ToTile and fTerrain >= fGrass) then { domain ok }
            begin { valid move }
              if (FromTile and (fRR or fCity) <> 0) and
                (ToTile and (fRR or fCity) <> 0) then
                MoveCost := RailCost // move along railroad
              else if (FromTile and (fRoad or fRR or fCity) <> 0) and
                (ToTile and (fRoad or fRR or fCity) <> 0) or
                (FromTile and ToTile and (fRiver or fCanal) <> 0) or
                (MoveInfo and gmaAlpine <> 0) then
                // move along road, river or canal
                if MoveInfo and gmaOver <> 0 then
                  MoveCost := 40
                else
                  MoveCost := 20
              else if MoveInfo and gmaOver <> 0 then
                MoveCost := -1
              else
                case Terrain[ToTile and fTerrain].MoveCost of
                  1:
                    MoveCost := 50; // plain terrain
                  2:
                    MoveCost := HeavyCost; // heavy terrain
                  3:
                    begin
                      MoveCost := maxmov;
                      MountainDelay := True;
                    end;
                end;

              // calculate HostileDamage
              if not Resistant and (ToTile and fTerImp <> tiBase) then
                if ToTile and (fTerrain or fCity or fRiver or fCanal or
                  fSpecial1 { Oasis } ) = fDesert then
                begin
                  if V8 and 1 <> 0 then
                    AddDamage := ((DesertThurst * 3) * MoveCost - 1)
                      div maxmov + 1
                  else
                    AddDamage := ((DesertThurst * 2) * MoveCost - 1)
                      div maxmov + 1
                end
                else if ToTile and (fTerrain or fCity or fRiver or fCanal) = fArctic
                then
                begin
                  if V8 and 1 <> 0 then
                    AddDamage := ((ArcticThurst * 3) * MoveCost - 1)
                      div maxmov + 1
                  else
                    AddDamage := ((ArcticThurst * 2) * MoveCost - 1)
                      div maxmov + 1
                end;
            end
            else
              MoveCost := -1;

          end;

          if (MoveCost > 0) and not MountainDelay then
            if V8 and 1 <> 0 then
              Inc(MoveCost, MoveCost * 2)
            else
              Inc(MoveCost, MoveCost);

          if (MoveInfo and 2 <> 0) // ground unit, check transport load/unload
            and ((MoveCost < 0) and (ToTile and (fUnit or fOwned) = fUnit or
            fOwned) // assume ship/airplane is transport -- load!
            or (MoveCost >= 0) and (FromTile and fTerrain < fGrass)) then
            MoveCost := maxmov; // transport load or unload

          if MoveCost >= 0 then
          begin { valid move }
            MovementLeft := maxmov - T shr 8 and $FFF - MoveCost;
            if (MovementLeft < 0) or ((MoveCost = 0) and (MovementLeft = 0))
            then
            begin // must wait for next turn
              // calculate HostileDamage
              if (MoveInfo and 2 <> 0) { ground unit }
                and not Resistant and (FromTile and fTerImp <> tiBase) then
                if FromTile and (fTerrain or fCity or fRiver or fCanal or
                  fSpecial1 { Oasis } ) = fDesert then
                  Inc(AddDamage, (DesertThurst * (maxmov - T shr 8 and $FFF) -
                    1) div maxmov + 1)
                else if FromTile and (fTerrain or fCity or fRiver or fCanal) = fArctic
                then
                  Inc(AddDamage, (ArcticThurst * (maxmov - T shr 8 and $FFF) -
                    1) div maxmov + 1);

              T1 := T and $7FF000FF + $100000 + (initmov + MoveCost) shl 8;
            end
            else
              T1 := T + MoveCost shl 8 + 1;
            if MountainDelay then
              T1 := T1 or $FFF00;
            if (Damage[Loc] + AddDamage <= MaxDamage) and (T1 and $FF < $FF)
            then
              if Q.Put(Loc1, T1) then
              begin
                From[Loc1] := Loc;
                Damage[Loc1] := Damage[Loc] + AddDamage;
              end;
          end;
        end;
      end;
    end;
  end;
  FreeAndNil(Q);
  if (Loc = A.ToLoc) or (A.ToLoc = maNextCity) and (Loc >= 0) and
    (Map[Loc] and fCity <> 0) then
  begin
    A.MoreTurns := T shr 20;
    EndLoc := Loc;
    A.nStep := 0;
    while Loc <> FromLoc do
    begin
      if Time[Loc] < $100000 then
        Inc(A.nStep);
      Loc := From[Loc];
    end;
    Loc := EndLoc;
    I := A.nStep;
    while Loc <> FromLoc do
    begin
      if Time[Loc] < $100000 then
      begin
        Dec(I);
        if I < 25 then
        begin
          A.dx[I] := ((Loc mod lx * 2 + Loc div lx and 1) -
            (From[Loc] mod lx * 2 + From[Loc] div lx and 1) + 3 * lx)
            mod (2 * lx) - lx;
          A.dy[I] := Loc div lx - From[Loc] div lx;
        end
      end;
      Loc := From[Loc];
    end;
    A.MaxHostile_MovementLeft := maxmov - Time[EndLoc] shr 8 and $FFF;
    if A.nStep > 25 then
      A.nStep := 25;
    Result := eOK
  end
  else
    Result := eNoWay;

  // QueryPerformanceCounter(tt);{time in s is: (tt-tt0)/PerfFreq}
end;

function CanPlaneReturn(P, uix: Integer;
  PlaneReturnData: TPlaneReturnData): Boolean;
const
  mfEnd = 1;
  mfReached = 2;
var
  uix1, T, T1, Loc, Loc1, FromTile, ToTile, V8, MoveCost, maxmov: Integer;
  Map: ^TTileList;
  Q: TIPQ;
  Adjacent: TVicinity8Loc;
  MapFlags: array [0 .. lxmax * lymax - 1] of Byte;
begin
  Map := @(RW[P].Map^);

  // calculate possible return points
  FillChar(MapFlags, SizeOf(MapFlags), 0);
  if RW[P].Model[RW[P].Un[uix].mix].Kind = mkSpecial_Glider then
  begin
    for Loc := 0 to MapSize - 1 do
      if Map[Loc] and fTerrain >= fGrass then
        MapFlags[Loc] := MapFlags[Loc] or mfEnd;
  end
  else
  begin
    for Loc := 0 to MapSize - 1 do
      if (Map[Loc] and (fCity or fOwned) = fCity or fOwned) or
        (Map[Loc] and fTerImp = tiBase) and (Map[Loc] and fObserved <> 0) and
        (Map[Loc] and (fUnit or fOwned) <> fUnit) then
        MapFlags[Loc] := MapFlags[Loc] or mfEnd;
    if RW[P].Model[RW[P].Un[uix].mix].Cap[mcAirTrans] = 0 then
    // plane can land on carriers
      for uix1 := 0 to RW[P].nUn - 1 do
        with RW[P].Un[uix1], RW[P].Model[mix] do
          if AirLoad < MTrans * Cap[mcCarrier] then
            MapFlags[Loc] := MapFlags[Loc] or mfEnd;
  end;

  with RW[P].Un[uix] do
  begin
    if Master >= 0 then // can return to same carrier, even if full now
      MapFlags[Loc] := MapFlags[Loc] or mfEnd;
    maxmov := RW[P].Model[mix].Speed;
  end;

  Result := False;
  Q := TIPQ.Create(MapSize);
  Q.Put(PlaneReturnData.Loc, (maxmov - PlaneReturnData.Movement) shl 8);
  while Q.Get(Loc, T) do
  begin
    MapFlags[Loc] := MapFlags[Loc] or mfReached;
    if T >= (PlaneReturnData.Fuel + 1) shl 20 then
    begin
      Result := False;
      Break;
    end;
    if MapFlags[Loc] and mfEnd <> 0 then
    begin
      Result := True;
      Break;
    end;
    FromTile := Map[Loc];
    V8_to_Loc(Loc, Adjacent);
    for V8 := 0 to 7 do
    begin
      Loc1 := Adjacent[V8];
      if (Loc1 >= 0) and (Loc1 < MapSize) and (MapFlags[Loc1] and mfReached = 0)
      then
      begin
        ToTile := Map[Loc1];
        if (ToTile and fTerrain <> fUNKNOWN) and
          (ToTile and (fCity or fOwned) <> fCity)
        // don't move through enemy cities
          and (ToTile and (fUnit or fOwned) <> fUnit) // way is blocked
          and (ToTile and not FromTile and fPeace = 0) then
        begin
          if V8 and 1 <> 0 then
            MoveCost := 150
          else
            MoveCost := 100;
          if MoveCost + T shr 8 and $FFF > maxmov then
          // must wait for next turn
            T1 := T and $7FF000FF + $100000 + MoveCost shl 8
          else
            T1 := T + MoveCost shl 8;
          Q.Put(Loc1, T1);
        end;
      end;
    end;
  end;
  FreeAndNil(Q);
end;

{
  Terrain Improvement
  ____________________________________________________________________
}
function CalculateJobWork(P, Loc, Job: Integer; var JobWork: Integer): Integer;
var
  TerrType: Integer;
begin
  Result := eOK;
  TerrType := RealMap[Loc] and fTerrain;
  with Terrain[TerrType] do
    case Job of
      jCity:
        if RealMap[Loc] and fCity <> 0 then
          Result := eInvalid
        else if IrrEff = 0 then
          Result := eNoCityTerrain
        else
          JobWork := CityWork;
      jRoad:
        if RealMap[Loc] and (fRoad or fRR) = 0 then
        begin
          JobWork := MoveCost * RoadWork;
          if RealMap[Loc] and fRiver <> 0 then
            if RW[P].Tech[adBridgeBuilding] >= tsApplicable then
              Inc(JobWork, RoadBridgeWork) { across river }
            else
              Result := eNoBridgeBuilding;
        end
        else
          Result := eInvalid;
      jRR:
        if RealMap[Loc] and fRoad = 0 then
          Result := eNoPreq
        else if RealMap[Loc] and fRR <> 0 then
          Result := eInvalid
        else
        begin
          JobWork := MoveCost * RRWork;
          if RealMap[Loc] and fRiver <> 0 then
            Inc(JobWork, RRBridgeWork); { across river }
        end;
      jClear:
        if (TerrType = fDesert) and (GWonder[woGardens].EffectiveOwner <> P)
        then
          Result := eInvalid
        else if ClearTerrain >= 0 then
          JobWork := IrrClearWork
        else
          Result := eInvalid;
      jIrr:
        begin
          JobWork := IrrClearWork;
          if (IrrEff = 0) or (RealMap[Loc] and fTerImp = tiIrrigation) or
            (RealMap[Loc] and fTerImp = tiFarm) then
            Result := eInvalid;
        end;
      jFarm:
        if RealMap[Loc] and fTerImp <> tiIrrigation then
          Result := eNoPreq
        else
        begin
          JobWork := IrrClearWork * FarmWork;
          if (JobWork <= 0) or (RealMap[Loc] and fTerImp = tiFarm) then
            Result := eInvalid;
        end;
      jAfforest:
        if AfforestTerrain >= 0 then
          JobWork := MineAfforestWork
        else
          Result := eInvalid;
      jMine:
        begin
          JobWork := MineAfforestWork;
          if (MineEff = 0) or (RealMap[Loc] and fTerImp = tiMine) then
            Result := eInvalid;
        end;
      jFort:
        if RealMap[Loc] and fTerImp <> tiFort then
          JobWork := MoveCost * FortWork
        else
          Result := eInvalid;
      jCanal:
        if (RealMap[Loc] and fCanal = 0) and (TerrType in TerrType_Canalable)
        then
          JobWork := CanalWork
        else
          Result := eInvalid;
      jTrans:
        begin
          JobWork := TransWork;
          if JobWork <= 0 then
            Result := eInvalid;
        end;
      jPoll:
        if RealMap[Loc] and fPoll <> 0 then
          JobWork := PollWork
        else
          Result := eInvalid;
      jBase:
        if RealMap[Loc] and fTerImp <> tiBase then
          JobWork := MoveCost * BaseWork
        else
          Result := eInvalid;
      jPillage:
        if RealMap[Loc] and (fRoad or fRR or fCanal or fTerImp) <> 0 then
          JobWork := PillageWork
        else
          Result := eInvalid;
    end;
end;

function StartJob(P, uix, NewJob: Integer; TestOnly: Boolean): Integer;
var
  JobWork, Loc0, p1, uix1, TerrType: Integer;
begin
{$IFOPT O-}Assert(1 shl P and InvalidTreatyMap = 0); {$ENDIF}
  Result := eOK;
  with RW[P].Un[uix] do
  begin
    if NewJob = Job then
    begin
      Result := eNotChanged;
      Exit;
    end;
    if NewJob = jNone then
    begin
      if not TestOnly then
        Job := jNone;
      Exit;
    end;
    Loc0 := Loc;
    if (RealMap[Loc0] and fDeadLands <> 0) and (NewJob <> jRoad) and
      (NewJob <> jRR) then
    begin
      Result := eDeadLands;
      Exit;
    end;
    TerrType := RealMap[Loc0] and fTerrain;
    if (RealMap[Loc0] and fCity <> 0) or (TerrType < fGrass) or (Master >= 0) or
      not((NewJob = jPillage) and (RW[P].Model[mix].Domain = dGround) or
      (RW[P].Model[mix].Kind = mkSettler) or (NewJob <> jCity) and
      (RW[P].Model[mix].Kind = mkSlaves) and (GWonder[woPyramids].EffectiveOwner
      >= 0)) then
    begin
      Result := eInvalid;
      Exit;
    end;
    if (JobPreq[NewJob] <> preNone) and
      (RW[P].Tech[JobPreq[NewJob]] < tsApplicable) then
    begin
      Result := eNoPreq;
      Exit;
    end;

    Result := CalculateJobWork(P, Loc0, NewJob, JobWork);
    if (Mode = moPlaying) and (Result = eOK) and (NewJob <> jPoll) then
    begin // not allowed in territory of friendly nation
      p1 := RealMap[Loc0] shr 27; // owner of territory
      if (p1 < nPl) and (p1 <> P) and (RW[P].Treaty[p1] >= trPeace) then
        Result := eTreaty; // keep peace treaty!
    end;
    if TestOnly or (Result < rExecuted) then
      Exit;

    if (ToWork[Loc0, NewJob] = 0) or (ToWork[Loc0, NewJob] > JobWork) then
      ToWork[Loc0, NewJob] := JobWork;
    Job := NewJob;
    Flags := Flags and not unFortified;
    for uix1 := 0 to RW[P].nUn - 1 do
      if (RW[P].Un[uix1].Loc = Loc) and
        (RW[P].Un[uix1].Job in ContraJobs[NewJob]) then
        RW[P].Un[uix1].Job := jNone; // stop contradictive jobs
    if ServerVersion[P] < $000EF0 then
      if Work(P, uix) then
        Result := eJobDone;
    if (NewJob = jCity) and (Result = eJobDone) then
    begin
      RemoveUnit_UpdateMap(P, uix);
      Result := eCity;
    end
    else if Health <= 0 then
    begin // victim of HostileDamage
      RemoveUnit_UpdateMap(P, uix);
      Result := Result or rUnitRemoved;
    end;
    if Mode > moLoading_Fast then
    begin
      if Result = eCity then
      begin
        ObserveLevel[Loc0] := ObserveLevel[Loc0] and not(3 shl (2 * P));
        Discover21(Loc0, P, lObserveUnhidden, True, True);
        // CheckContact;
      end;
    end;
  end;
end;

function Work(P, uix: Integer): Boolean;
var
  uix1, j0: Integer;
begin
  Result := False;
  with RW[P].Un[uix] do
    if Movement >= 100 then
    begin
      Assert(ToWork[Loc, Job] < $FFFF); // should have been set by StartJob
      if Job >= jRoad then
        if Integer(Movement) >= Integer(ToWork[Loc, Job]) then { work complete }
        begin
          Result := True;
          if Job <> jIrr then
            Health := Health - HostileDamage(P, mix, Loc, ToWork[Loc, Job]);
          Dec(Movement, ToWork[Loc, Job]);
          if not(Job in [jCity, jPillage, jPoll]) then
            Inc(Worked[P], ToWork[Loc, Job]);
          if Job = jCity then
          begin // found new city
            FoundCity(P, Loc);
            Inc(Founded[P]);
            with RW[P].City[RW[P].nCity - 1] do
            begin
              ID := P shl 12 + Founded[P] - 1;
              Flags := chFounded;
            end;
            if Mode = moPlaying then
            begin
              LogCheckBorders(P, RW[P].nCity - 1);
              RecalcPeaceMap(P);
            end;
{$IFOPT O-} if Mode < moPlaying then
              InvalidTreatyMap := not(1 shl P); {$ENDIF}
            // territory should not be considered for the rest of the command
            // execution, because during loading a game it's incorrect before
            // subsequent sIntExpandTerritory is processed
            RW[P].Un[uix].Health := 0; // causes unit to be removed later
          end
          else
            CompleteJob(P, Loc, Job);
          ToWork[Loc, Job] := 0;
          j0 := Job;
          for uix1 := 0 to RW[P].nUn - 1 do
            if (RW[P].Un[uix1].Loc = Loc) and (RW[P].Un[uix1].Job = j0) then
              RW[P].Un[uix1].Job := jNone
        end
        else
        begin
          Dec(ToWork[Loc, Job], Movement);
          if not(Job in [jCity, jPillage, jPoll]) then
            Inc(Worked[P], Movement);
          Health := Health - HostileDamage(P, mix, Loc, Movement);
          Movement := 0;
        end
    end
end;

function GetJobProgress(P, Loc: Integer;
  var JobProgressData: TJobProgressData): Integer;
var
  Job, JobResult, uix: Integer;
begin
  for Job := 0 to nJob - 1 do
  begin
    JobResult := CalculateJobWork(P, Loc, Job, JobProgressData[Job].Required);
    if JobResult = eOK then
    begin
      if ToWork[Loc, Job] = $FFFF then // not calculated yet
        JobProgressData[Job].Done := 0
      else
        JobProgressData[Job].Done := JobProgressData[Job].Required -
          ToWork[Loc, Job];
    end
    else
    begin
      JobProgressData[Job].Required := 0;
      JobProgressData[Job].Done := 0;
    end;
    JobProgressData[Job].NextTurnPlus := 0;
  end;
  for uix := 0 to RW[P].nUn - 1 do
    if (RW[P].Un[uix].Loc = Loc) and (RW[P].Un[uix].Movement >= 100) then
      Inc(JobProgressData[RW[P].Un[uix].Job].NextTurnPlus,
        RW[P].Un[uix].Movement);
  Result := eOK;
end;

{
  Start/End Game
  ____________________________________________________________________
}
procedure InitGame;
begin
  GetMem(ToWork, 2 * MapSize * nJob);
  FillChar(ToWork^, 2 * MapSize * nJob, $FF);
end;

procedure ReleaseGame;
begin
  FreeMem(ToWork);
end;

end.
