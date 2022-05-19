{$INCLUDE Switches.inc}
unit CustomAI;

interface

uses
{$IFDEF DEBUG}SysUtils,{$ENDIF} // necessary for debug exceptions
  Protocol;

type
  TNegoTime = (BeginOfTurn, EndOfTurn, EnemyCalled);

  TCustomAI = class
  public
    procedure Process(Command: Integer; var Data);

    // overridables
    constructor Create(Nation: Integer); virtual;
    destructor Destroy; override;
    procedure SetDataDefaults; virtual;
    procedure SetDataRandom; virtual;
    procedure OnBeforeEnemyAttack(UnitInfo: TUnitInfo;
      ToLoc, EndHealth, EndHealthDef: Integer); virtual;
    procedure OnBeforeEnemyCapture(UnitInfo: TUnitInfo; ToLoc: Integer); virtual;
    procedure OnAfterEnemyAttack; virtual;
    procedure OnAfterEnemyCapture; virtual;

  protected
    Me: Integer; // index of the controlled nation
    RO: ^TPlayerContext;
    Map: ^TTileList;
    MyUnit: ^TUnList;
    MyCity: ^TCityList;
    MyModel: ^TModelList;

    cixStateImp: array[imPalace..imSpacePort] of Integer;

    // negotiation
    Opponent: Integer; // nation i'm in negotiation with, -1 indicates no-negotiation mode
    MyAction, MyLastAction, OppoAction: Integer;
    MyOffer, MyLastOffer, OppoOffer: TOffer;

    // overridables
    procedure DoTurn; virtual;
    procedure DoNegotiation; virtual;
    function ChooseResearchAdvance: Integer; virtual;
    function ChooseStealAdvance: Integer; virtual;
    function ChooseGovernment: Integer; virtual;
    function WantNegotiation(Nation: Integer; NegoTime: TNegoTime): Boolean; virtual;
    function OnNegoRejected_CancelTreaty: Boolean; virtual;

    // general functions
    function IsResearched(Advance: Integer): Boolean;
    function ResearchCost: Integer;
    function ChangeAttitude(Nation, Attitude: Integer): Integer;
    function Revolution: Integer;
    function ChangeRates(Tax, Lux: Integer): Integer;
    function PrepareNewModel(Domain: Integer): Integer;
    function SetNewModelFeature(F, Count: Integer): Integer;
    function AdvanceResearchable(Advance: Integer): Boolean;
    function AdvanceStealable(Advance: Integer): Boolean;
    function GetJobProgress(Loc: Integer; var JobProgress: TJobProgressData): Boolean;
    function DebugMessage(Level: Integer; Text: string): Boolean;
    function SetDebugMap(var DebugMap): Boolean;

    // unit functions
    procedure Unit_FindMyDefender(Loc: Integer; var uix: Integer);
    procedure Unit_FindEnemyDefender(Loc: Integer; var euix: Integer);
    function Unit_Move(uix, ToLoc: Integer): Integer;
    function Unit_Step(uix, ToLoc: Integer): Integer;
    function Unit_Attack(uix, ToLoc: Integer): Integer;
    function Unit_DoMission(uix, MissionType, ToLoc: Integer): Integer;
    function Unit_MoveForecast(uix, ToLoc: Integer;
      var RemainingMovement: Integer): Boolean;
    function Unit_AttackForecast(uix, ToLoc, AttackMovement: Integer;
      var RemainingHealth: Integer): Boolean;
    function Unit_DefenseForecast(euix, ToLoc: Integer;
      var RemainingHealth: Integer): Boolean;
    function Unit_Disband(uix: Integer): Integer;
    function Unit_StartJob(uix, NewJob: Integer): Integer;
    function Unit_SetHomeHere(uix: Integer): Integer;
    function Unit_Load(uix: Integer): Integer;
    function Unit_Unload(uix: Integer): Integer;
    function Unit_SelectTransport(uix: Integer): Integer;
    function Unit_AddToCity(uix: Integer): Integer;

    // city functions
    procedure City_FindMyCity(Loc: Integer; var cix: Integer);
    procedure City_FindEnemyCity(Loc: Integer; var ecix: Integer);
    function City_HasProject(cix: Integer): Boolean;
    function City_CurrentImprovementProject(cix: Integer): Integer;
    function City_CurrentUnitProject(cix: Integer): Integer;
    function City_GetTileInfo(cix, TileLoc: Integer; var TileInfo: TTileInfo): Integer;
    function City_GetReport(cix: Integer; var Report: TCityReport): Integer;
    function City_GetHypoReport(cix, HypoTiles, HypoTax, HypoLux: Integer;
      var Report: TCityReport): Integer;
    function City_GetReportNew(cix: Integer; var Report: TCityReportNew): Integer;
    function City_GetHypoReportNew(cix, HypoTiles, HypoTaxRate, HypoLuxuryRate: Integer;
      var Report: TCityReportNew): Integer;
    function City_GetAreaInfo(cix: Integer; var AreaInfo: TCityAreaInfo): Integer;
    function City_StartUnitProduction(cix, mix: Integer): Integer;
    function City_StartEmigration(cix, mix: Integer;
      AllowDisbandCity, AsConscripts: Boolean): Integer;
    function City_StartImprovement(cix, iix: Integer): Integer;
    function City_Improvable(cix, iix: Integer): Boolean;
    function City_StopProduction(cix: Integer): Integer;
    function City_BuyProject(cix: Integer): Integer;
    function City_SellImprovement(cix, iix: Integer): Integer;
    function City_RebuildImprovement(cix, iix: Integer): Integer;
    function City_SetTiles(cix, NewTiles: Integer): Integer;
    procedure City_OptimizeTiles(cix: Integer; ResourceWeights: Cardinal = rwMaxGrowth);

    // negotiation
    function Nego_CheckMyAction: Integer;

  private
    HaveTurned: Boolean;
    UnwantedNego: set of 0..nPl - 1;
    Contacted: set of 0..nPl - 1;
    procedure StealAdvance;
  end;


var
  Server: TServerCall;
  G: TNewGameData;
  RWDataSize, MapSize: Integer;
  decompose24: Cardinal;
  nodata: Pointer;

const
  CityOwnTile = 13; // = ab_to_V21(0,0)

  // additional return codes
  rLocationReached = $00010000;
  // Unit_Move: move was not interrupted, location reached
  rMoreTurns = $00020000;
// Unit_Move: move was not interrupted, location not reached yet

type
  TVicinity8Loc = array[0..7] of Integer;
  TVicinity21Loc = array[0..27] of Integer;


procedure Init(NewGameData: TNewGameData);

procedure ab_to_Loc(Loc0, A, B: Integer; var Loc: Integer);
procedure Loc_to_ab(Loc0, Loc: Integer; var A, B: Integer);
procedure ab_to_V8(A, B: Integer; var V8: Integer);
procedure V8_to_ab(V8: Integer; var A, B: Integer);
procedure ab_to_V21(A, B: Integer; var V21: Integer);
procedure V21_to_ab(V21: Integer; var A, B: Integer);
procedure V8_to_Loc(Loc0: Integer; var VicinityLoc: TVicinity8Loc);
procedure V21_to_Loc(Loc0: Integer; var VicinityLoc: TVicinity21Loc);
function Distance(Loc0, Loc1: Integer): Integer;


implementation

const
  ab_v8: array[-4..4] of Integer = (5, 6, 7, 4, -1, 0, 3, 2, 1);
  v8_a: array[0..7] of Integer = (1, 1, 0, -1, -1, -1, 0, 1);
  v8_b: array[0..7] of Integer = (0, 1, 1, 1, 0, -1, -1, -1);


procedure ab_to_Loc(Loc0, A, B: Integer; var Loc: Integer);
{relative location from Loc0}
var
  y0: Integer;
begin
  Assert((Loc0 >= 0) and (Loc0 < MapSize) and (A - B + G.lx >= 0));
  y0 := Cardinal(Loc0) * decompose24 shr 24;
  Loc := (Loc0 + (A - B + y0 and 1 + G.lx + G.lx) shr 1) mod G.lx + G.lx * (y0 + A + B);
  if Loc >= MapSize then
    Loc := -$1000;
end;

procedure Loc_to_ab(Loc0, Loc: Integer; var A, B: Integer);
{$IFDEF FPC}// freepascal
var
  dx, dy: Integer;
begin
  dx := ((Loc mod G.lx * 2 + Loc div G.lx and 1) - (Loc0 mod G.lx * 2 + Loc0 div
    G.lx and 1) + 3 * G.lx) mod (2 * G.lx) - G.lx;
  dy := Loc div G.lx - Loc0 div G.lx;
  A := (dx + dy) div 2;
  B := (dy - dx) div 2;
end;

{$ELSE}// delphi
register;
asm
push ebx

// calculate
push ecx
div Byte ptr [G]
xor ebx,ebx
mov bl,ah  // ebx:=Loc0 mod G.lx
mov ecx,eax
and ecx,$000000FF // ecx:=Loc0 div G.lx
mov eax,edx
div Byte ptr [G]
xor edx,edx
mov dl,ah // edx:=Loc mod G.lx
and eax,$000000FF // eax:=Loc div G.lx
sub edx,ebx // edx:=Loc mod G.lx-Loc0 mod G.lx
mov ebx,eax
sub ebx,ecx // ebx:=dy
and eax,1
and ecx,1
add edx,edx
add eax,edx
sub eax,ecx // eax:=dx, not normalized
pop ecx

// normalize
mov edx,dword ptr [G]
cmp eax,edx
jl @A
  sub eax,edx
  sub eax,edx
  jmp @ok
@A:
neg edx
cmp eax,edx
jnl @ok
  sub eax,edx
  sub eax,edx

// return results
@ok:
mov edx,ebx
sub edx,eax
add eax,ebx
sar edx,1 // edx:=b
mov ebx,[B]
mov [ebx],edx
sar eax,1 // eax:=a
mov [A],eax

pop ebx
end;
{$ENDIF}

procedure ab_to_V8(A, B: Integer; var V8: Integer);
begin
  Assert((abs(A) <= 1) and (abs(B) <= 1) and ((A <> 0) or (B <> 0)));
  V8 := ab_v8[2 * B + B + A];
end;

procedure V8_to_ab(V8: Integer; var A, B: Integer);
begin
  A := v8_a[V8];
  B := V8_b[V8];
end;

procedure ab_to_V21(A, B: Integer; var V21: Integer);
begin
  V21 := (A + B + 3) shl 2 + (A - B + 3) shr 1;
end;

procedure V21_to_ab(V21: Integer; var A, B: Integer);
var
  dx, dy: Integer;
begin
  dy := V21 shr 2 - 3;
  dx := V21 and 3 shl 1 - 3 + (dy + 3) and 1;
  A := (dx + dy) div 2;
  B := (dy - dx) div 2;
end;

procedure V8_to_Loc(Loc0: Integer; var VicinityLoc: TVicinity8Loc);
var
  x0, y0, lx: Integer;
begin
  lx := G.lx;
  y0 := Cardinal(Loc0) * decompose24 shr 24;
  x0 := Loc0 - y0 * lx; // Loc0 mod lx;
  VicinityLoc[1] := Loc0 + lx * 2;
  VicinityLoc[3] := Loc0 - 1;
  VicinityLoc[5] := Loc0 - lx * 2;
  VicinityLoc[7] := Loc0 + 1;
  Inc(Loc0, y0 and 1);
  VicinityLoc[0] := Loc0 + lx;
  VicinityLoc[2] := Loc0 + lx - 1;
  VicinityLoc[4] := Loc0 - lx - 1;
  VicinityLoc[6] := Loc0 - lx;

  // world is round!
  if x0 < lx - 1 then
  begin
    if x0 = 0 then
    begin
      Inc(VicinityLoc[3], lx);
      if y0 and 1 = 0 then
      begin
        Inc(VicinityLoc[2], lx);
        Inc(VicinityLoc[4], lx);
      end;
    end;
  end
  else
  begin
    Dec(VicinityLoc[7], lx);
    if y0 and 1 = 1 then
    begin
      Dec(VicinityLoc[0], lx);
      Dec(VicinityLoc[6], lx);
    end;
  end;

  // check south pole
  case G.ly - y0 of
    1:
    begin
      VicinityLoc[0] := -$1000;
      VicinityLoc[1] := -$1000;
      VicinityLoc[2] := -$1000;
    end;
    2: VicinityLoc[1] := -$1000;
  end;
end;

procedure V21_to_Loc(Loc0: Integer; var VicinityLoc: TVicinity21Loc);
var
  dx, dy, bit, y0, xComp, yComp, xComp0, xCompSwitch: Integer;
  dst: ^Integer;
begin
  y0 := Cardinal(Loc0) * decompose24 shr 24;
  xComp0 := Loc0 - y0 * G.lx - 1; // Loc0 mod G.lx -1
  xCompSwitch := xComp0 - 1 + y0 and 1;
  if xComp0 < 0 then
    Inc(xComp0, G.lx);
  if xCompSwitch < 0 then
    Inc(xCompSwitch, G.lx);
  xCompSwitch := xCompSwitch xor xComp0;
  yComp := G.lx * (y0 - 3);
  dst := @VicinityLoc;
  bit := 1;
  for dy := 0 to 6 do
    if yComp < MapSize then
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
        if xComp >= G.lx then
          Dec(xComp, G.lx);
        Inc(dst);
        bit := bit shl 1;
      end;
      Inc(yComp, G.lx);
    end
    else
    begin
      for dx := 0 to 3 do
      begin
        dst^ := -$1000;
        Inc(dst);
      end;
    end;
end;

function Distance(Loc0, Loc1: Integer): Integer;
var
  A, B, dx, dy: Integer;
begin
  Loc_to_ab(Loc0, Loc1, A, B);
  dx := abs(A - B);
  dy := abs(A + B);
  Result := dx + dy + abs(dx - dy) shr 1;
end;


procedure Init(NewGameData: TNewGameData);
{$IFDEF DEBUG}var
  Loc: Integer;
{$ENDIF}
begin
  G := NewGameData;
  MapSize := G.lx * G.ly;
  decompose24 := (1 shl 24 - 1) div G.lx + 1;
{$IFDEF DEBUG}
  for Loc := 0 to MapSize - 1 do
    Assert(Cardinal(Loc) * decompose24 shr 24 = Cardinal(Loc div G.lx));
{$ENDIF}
end;


constructor TCustomAI.Create(Nation: Integer);
begin
  inherited Create;
  Me := Nation;
  RO := Pointer(G.RO[Nation]);
  Map := Pointer(RO.Map);
  MyUnit := Pointer(RO.Un);
  MyCity := Pointer(RO.City);
  MyModel := Pointer(RO.Model);
  Opponent := -1;
end;

destructor TCustomAI.Destroy;
begin
  Server(sSetDebugMap, Me, 0, nodata^);
end;


procedure TCustomAI.Process(Command: Integer; var Data);
var
  Nation, NewResearch, NewGov, Count, ad, cix, iix: Integer;
  NegoTime: TNegoTime;
begin
  case Command of
    cTurn, cContinue:
    begin
      if RO.Alive and (1 shl Me) = 0 then
      begin // I'm dead, huhu
        Server(sTurn, Me, 0, nodata^);
        Exit;
      end;
      if Command = cTurn then
      begin
        FillChar(cixStateImp, SizeOf(cixStateImp), $FF);
        for cix := 0 to RO.nCity - 1 do
          if MyCity[cix].Loc >= 0 then
            for iix := imPalace to imSpacePort do
              if MyCity[cix].Built[iix] > 0 then
                cixStateImp[iix] := cix;
        if RO.Happened and phChangeGov <> 0 then
        begin
          NewGov := ChooseGovernment;
          if NewGov > gAnarchy then
            Server(sSetGovernment, Me, NewGov, nodata^);
        end;
        HaveTurned := False;
        Contacted := [];
      end;
      if (Command = cContinue) and (MyAction = scContact) then
      begin
        if OnNegoRejected_CancelTreaty then
          if RO.Treaty[Opponent] >= trPeace then
            if Server(sCancelTreaty, Me, 0, nodata^) < rExecuted then
              Assert(False);
      end
      else
        UnwantedNego := [];
      Opponent := -1;
      repeat
        if HaveTurned then
          NegoTime := EndOfTurn
        else
          NegoTime := BeginOfTurn;
        if RO.Government <> gAnarchy then
          for Nation := 0 to nPl - 1 do
            if (Nation <> Me) and (1 shl Nation and RO.Alive <> 0) and
              (RO.Treaty[Nation] >= trNone) and not (Nation in Contacted) and not
              (Nation in UnwantedNego) and
              (Server(scContact - sExecute + Nation shl 4, Me, 0, nodata^) >= rExecuted) then
              if WantNegotiation(Nation, NegoTime) then
              begin
                if Server(scContact + Nation shl 4, Me, 0, nodata^) >= rExecuted then
                begin
                  Include(Contacted, Nation);
                  Opponent := Nation;
                  MyAction := scContact;
                  Exit;
                end;
              end
              else
                Include(UnwantedNego, Nation);
        if NegoTime = BeginOfTurn then
        begin
          DoTurn;
          HaveTurned := True;
          Contacted := [];
          UnwantedNego := [];
        end
        else
          Break;
      until False;
      if RO.Happened and phTech <> 0 then
      begin
        NewResearch := ChooseResearchAdvance;
        if NewResearch < 0 then
        begin // choose random research
          Count := 0;
          for ad := 0 to nAdv - 1 do
            if AdvanceResearchable(ad) then
            begin
              Inc(Count);
              if random(Count) = 0 then
                NewResearch := ad;
            end;
        end;
        Server(sSetResearch, Me, NewResearch, nodata^);
      end;
      if Server(sTurn, Me, 0, nodata^) < rExecuted then
        Assert(False);
    end;
    scContact:
      if WantNegotiation(Integer(Data), EnemyCalled) then
      begin
        if Server(scDipStart, Me, 0, nodata^) < rExecuted then
          Assert(False);
        Opponent := Integer(Data);
        MyAction := scDipStart;
      end
      else
      begin
        if Server(scReject, Me, 0, nodata^) < rExecuted then
          Assert(False);
      end;
    scDipStart, scDipNotice, scDipAccept, scDipCancelTreaty, scDipOffer, scDipBreak:
    begin
      OppoAction := Command;
      if Command = scDipOffer then
        OppoOffer := TOffer(Data);
      if Command = scDipStart then
        MyLastAction := scContact
      else
      begin
        MyLastAction := MyAction;
        MyLastOffer := MyOffer;
      end;
      if (OppoAction = scDipCancelTreaty) or (OppoAction = scDipBreak) then
        MyAction := scDipNotice
      else
      begin
        MyAction := scDipOffer;
        MyOffer.nDeliver := 0;
        MyOffer.nCost := 0;
      end;
      DoNegotiation;
      Assert((MyAction = scDipNotice) or (MyAction = scDipAccept) or
        (MyAction = scDipCancelTreaty) or (MyAction = scDipOffer) or (MyAction = scDipBreak));
      if MyAction = scDipOffer then
        Server(MyAction, Me, 0, MyOffer)
      else
        Server(MyAction, Me, 0, nodata^);
    end;
    cShowEndContact:
      Opponent := -1;
  end;
end;

{$HINTS OFF}
procedure TCustomAI.SetDataDefaults;
begin
end;

procedure TCustomAI.SetDataRandom;
begin
end;

procedure TCustomAI.DoTurn;
begin
end;

procedure TCustomAI.DoNegotiation;
begin
end;

procedure TCustomAI.OnBeforeEnemyAttack(UnitInfo: TUnitInfo;
  ToLoc, EndHealth, EndHealthDef: Integer);
begin
end;

procedure TCustomAI.OnBeforeEnemyCapture(UnitInfo: TUnitInfo; ToLoc: Integer);
begin
end;

procedure TCustomAI.OnAfterEnemyAttack;
begin
end;

procedure TCustomAI.OnAfterEnemyCapture;
begin
end;

function TCustomAI.ChooseResearchAdvance: Integer;
begin
  Result := -1;
end;

function TCustomAI.ChooseStealAdvance: Integer;
begin
  Result := -1;
end;

function TCustomAI.ChooseGovernment: Integer;
begin
  Result := gDespotism;
end;

function TCustomAI.WantNegotiation(Nation: Integer; NegoTime: TNegoTime): Boolean;
begin
  Result := False;
end;

function TCustomAI.OnNegoRejected_CancelTreaty: Boolean;
begin
  Result := False;
end;

{$HINTS ON}

procedure TCustomAI.StealAdvance;
var
  Steal, ad, Count: Integer;
begin
  Steal := ChooseStealAdvance;
  if Steal < 0 then
  begin // choose random advance
    Count := 0;
    for ad := 0 to nAdv - 1 do
      if AdvanceStealable(ad) then
      begin
        Inc(Count);
        if random(Count) = 0 then
          Steal := ad;
      end;
  end;
  if Steal >= 0 then
    Server(sStealTech, Me, Steal, nodata^);
  RO.Happened := RO.Happened and not phStealTech;
end;

function TCustomAI.IsResearched(Advance: Integer): Boolean;
begin
  Result := (Advance = preNone) or (Advance <> preNA) and (RO.Tech[Advance] >= tsApplicable);
end;

function TCustomAI.ResearchCost: Integer;
begin
  Server(sGetTechCost, Me, 0, Result);
end;

function TCustomAI.ChangeAttitude(Nation, Attitude: Integer): Integer;
begin
  Result := Server(sSetAttitude + Nation shl 4, Me, Attitude, nodata^);
end;

function TCustomAI.Revolution: Integer;
begin
  Result := Server(sRevolution, Me, 0, nodata^);
end;

function TCustomAI.ChangeRates(Tax, Lux: Integer): Integer;
begin
  Result := Server(sSetRates, Me, Tax div 10 and $F + Lux div 10 and $F shl 4, nodata^);
end;

function TCustomAI.PrepareNewModel(Domain: Integer): Integer;
begin
  Result := Server(sCreateDevModel, Me, Domain, nodata^);
end;

function TCustomAI.SetNewModelFeature(F, Count: Integer): Integer;
begin
  Result := Server(sSetDevModelCap + Count shl 4, Me, F, nodata^);
end;

function TCustomAI.AdvanceResearchable(Advance: Integer): Boolean;
begin
  Result := Server(sSetResearch - sExecute, Me, Advance, nodata^) >= rExecuted;
end;

function TCustomAI.AdvanceStealable(Advance: Integer): Boolean;
begin
  Result := Server(sStealTech - sExecute, Me, Advance, nodata^) >= rExecuted;
end;

function TCustomAI.GetJobProgress(Loc: Integer;
  var JobProgress: TJobProgressData): Boolean;
begin
  Result := Server(sGetJobProgress, Me, Loc, JobProgress) >= rExecuted;
end;

function TCustomAI.DebugMessage(Level: Integer; Text: string): Boolean;
begin
  Text := Copy('P' + char(48 + Me) + ' ' + Text, 1, 254);
  Server(sMessage, Me, Level, PChar(Text)^);

  Result := True;
  // always returns true so that it can be used like
  // "assert(DebugMessage(...));" -> not compiled in release build
end;

function TCustomAI.SetDebugMap(var DebugMap): Boolean;
begin
  Server(sSetDebugMap, Me, 0, DebugMap);

  Result := True;
  // always returns true so that it can be used like
  // "assert(SetDebugMap(...));" -> not compiled in release build
end;

procedure TCustomAI.Unit_FindMyDefender(Loc: Integer; var uix: Integer);
begin
  if Server(sGetDefender, Me, Loc, uix) < rExecuted then
    uix := -1;
end;

procedure TCustomAI.Unit_FindEnemyDefender(Loc: Integer; var euix: Integer);
begin
  euix := RO.nEnemyUn - 1;
  while (euix >= 0) and (RO.EnemyUn[euix].Loc <> Loc) do
    Dec(euix);
end;

function TCustomAI.Unit_Move(uix, ToLoc: Integer): Integer;
var
  Step: Integer;
  DestinationReached: Boolean;
  Advice: TMoveAdviceData;
begin
  Assert((uix >= 0) and (uix < RO.nUn) and (MyUnit[uix].Loc >= 0)); // is a unit
{Loc_to_ab(MyUnit[uix].Loc,ToLoc,a,b);
Assert((A<>0) or (B<>0));
if (A>=-1) and (A<=1) and (B>=-1) and (B<=1) then
  begin // move to adjacent tile
  !!!problem: if Move is invalid, return codes are not consistent with other branch (eNoWay)
  Advice.nStep:=1;
  Advice.dx[0]:=A-B;
  Advice.dy[0]:=A+B;
  Advice.MoreTurns:=0;
  Advice.MaxHostile_MovementLeft:=MyUnit[uix].Movement;
  Result:=eOK;
  end
else}
  begin // move to non-adjacent tile, find shortest path
    Advice.ToLoc := ToLoc;
    Advice.MoreTurns := 9999;
    Advice.MaxHostile_MovementLeft := 100;
    Result := Server(sGetMoveAdvice, Me, uix, Advice);
  end;
  if Result = eOk then
  begin
    DestinationReached := False;
    Step := 0;
    repeat
      if Result and (rExecuted or rUnitRemoved) = rExecuted then
        // check if destination reached
        if (ToLoc >= 0) and (Advice.MoreTurns = 0) and (Step = Advice.nStep - 1) and
          ((Map[ToLoc] and (fUnit or fOwned) = fUnit) // attack
          or (Map[ToLoc] and (fCity or fOwned) = fCity) and
          ((MyModel[MyUnit[uix].mix].Domain <> dGround) // bombardment
          or (MyModel[MyUnit[uix].mix].Flags and mdCivil <> 0))) then // can't capture
        begin
          DestinationReached := True;
          Break;
        end // stop next to destination
        else if Step = Advice.nStep then
          DestinationReached := True; // normal move -- stop at destination

      if (Step = Advice.nStep) or (Result <> eOK) and (Result <> eLoaded) then
        Break;

      Result := Server(sMoveUnit + (Advice.dx[Step] and 7) shl 4 +
        (Advice.dy[Step] and 7) shl 7, Me, uix, nodata^);
      Inc(Step);
      if RO.Happened and phStealTech <> 0 then
        StealAdvance;
    until False;
    if DestinationReached then
      if Advice.nStep = 25 then
        Result := Unit_Move(uix, ToLoc) // Shinkansen
      else if Advice.MoreTurns = 0 then
        Result := Result or rLocationReached
      else
        Result := Result or rMoreTurns;
  end;
end;

function TCustomAI.Unit_Step(uix, ToLoc: Integer): Integer;
var
  A, B: Integer;
begin
  Loc_to_ab(MyUnit[uix].Loc, ToLoc, A, B);
  Assert(((A <> 0) or (B <> 0)) and (A >= -1) and (A <= 1) and (B >= -1) and (B <= 1));
  Result := Server(sMoveUnit + ((A - B) and 7) shl 4 + ((A + B) and 7) shl 7, Me, uix, nodata^);
  if RO.Happened and phStealTech <> 0 then
    StealAdvance;
end;

function TCustomAI.Unit_Attack(uix, ToLoc: Integer): Integer;
var
  A, B: Integer;
begin
  Assert((uix >= 0) and (uix < RO.nUn) and (MyUnit[uix].Loc >= 0) // is a unit
    and ((Map[ToLoc] and (fUnit or fOwned) = fUnit) // is an attack
    or (Map[ToLoc] and (fCity or fOwned) = fCity) and
    (MyModel[MyUnit[uix].mix].Domain <> dGround))); // is a bombardment
  Loc_to_ab(MyUnit[uix].Loc, ToLoc, A, B);
  Assert(((A <> 0) or (B <> 0)) and (A >= -1) and (A <= 1) and (B >= -1) and (B <= 1));
  // attack to adjacent tile
  Result := Server(sMoveUnit + (A - B) and 7 shl 4 + (A + B) and 7 shl 7, Me, uix, nodata^);
end;

function TCustomAI.Unit_DoMission(uix, MissionType, ToLoc: Integer): Integer;
var
  A, B: Integer;
begin
  Result := Server(sSetSpyMission + MissionType shl 4, Me, 0, nodata^);
  if Result >= rExecuted then
  begin
    Assert((uix >= 0) and (uix < RO.nUn) and (MyUnit[uix].Loc >= 0) // is a unit
      and (MyModel[MyUnit[uix].mix].Kind = mkDiplomat)); // is a commando
    Loc_to_ab(MyUnit[uix].Loc, ToLoc, A, B);
    Assert(((A <> 0) or (B <> 0)) and (A >= -1) and (A <= 1) and (B >= -1) and (B <= 1));
    // city must be adjacent
    Result := Server(sMoveUnit - sExecute + (A - B) and 7 shl 4 + (A + B) and 7 shl 7, Me, uix, nodata^);
    if Result = eMissionDone then
      Result := Server(sMoveUnit + (A - B) and 7 shl 4 + (A + B) and 7 shl 7, Me, uix, nodata^)
    else if (Result <> eNoTime_Move) and (Result <> eTreaty) and (Result <> eNoTurn) then
      Result := eInvalid; // not a special commando mission!
  end;
end;

function TCustomAI.Unit_MoveForecast(uix, ToLoc: Integer;
  var RemainingMovement: Integer): Boolean;
var
  Advice: TMoveAdviceData;
begin
  Assert((uix >= 0) and (uix < RO.nUn) and (MyUnit[uix].Loc >= 0)); // is a unit
  Advice.ToLoc := ToLoc;
  Advice.MoreTurns := 0;
  Advice.MaxHostile_MovementLeft := 100;
  if Server(sGetMoveAdvice, Me, uix, Advice) = eOk then
  begin
    RemainingMovement := Advice.MaxHostile_MovementLeft;
    Result := True;
  end
  else
  begin
    RemainingMovement := -1;
    Result := False;
  end;
end;

// negative RemainingHealth is remaining helth of defender if lost
function TCustomAI.Unit_AttackForecast(uix, ToLoc, AttackMovement: Integer;
  var RemainingHealth: Integer): Boolean;
var
  BattleForecast: TBattleForecast;
begin
  Assert((uix >= 0) and (uix < RO.nUn) and (MyUnit[uix].Loc >= 0) // is a unit
    and (Map[ToLoc] and (fUnit or fOwned) = fUnit)); // is an attack
  RemainingHealth := -$100;
  Result := False;
  if AttackMovement >= 0 then
    with MyUnit[uix] do
    begin
      BattleForecast.pAtt := Me;
      BattleForecast.mixAtt := mix;
      BattleForecast.HealthAtt := Health;
      BattleForecast.ExpAtt := Exp;
      BattleForecast.FlagsAtt := Flags;
      BattleForecast.Movement := AttackMovement;
      if Server(sGetBattleForecast, Me, ToLoc, BattleForecast) >= rExecuted then
      begin
        if BattleForecast.EndHealthAtt > 0 then
          RemainingHealth := BattleForecast.EndHealthAtt
        else
          RemainingHealth := -BattleForecast.EndHealthDef;
        Result := True;
      end;
    end;
end;

function TCustomAI.Unit_DefenseForecast(euix, ToLoc: Integer;
  var RemainingHealth: Integer): Boolean;
var
  BattleForecast: TBattleForecast;
begin
  Assert((euix >= 0) and (euix < RO.nEnemyUn) and (RO.EnemyUn[euix].Loc >= 0) // is an enemy unit
    and (Map[ToLoc] and (fUnit or fOwned) = (fUnit or fOwned))); // is an attack
  RemainingHealth := $100;
  Result := False;
  with RO.EnemyUn[euix] do
  begin
    BattleForecast.pAtt := Owner;
    BattleForecast.mixAtt := mix;
    BattleForecast.HealthAtt := Health;
    BattleForecast.ExpAtt := Exp;
    BattleForecast.FlagsAtt := Flags;
    BattleForecast.Movement := 100;
    if Server(sGetBattleForecast, Me, ToLoc, BattleForecast) >= rExecuted then
    begin
      if BattleForecast.EndHealthDef > 0 then
        RemainingHealth := BattleForecast.EndHealthDef
      else
        RemainingHealth := -BattleForecast.EndHealthAtt;
      Result := True;
    end;
  end;
end;

function TCustomAI.Unit_Disband(uix: Integer): Integer;
begin
  Result := Server(sRemoveUnit, Me, uix, nodata^);
end;

function TCustomAI.Unit_StartJob(uix, NewJob: Integer): Integer;
begin
  Result := Server(sStartJob + NewJob shl 4, Me, uix, nodata^);
end;

function TCustomAI.Unit_SetHomeHere(uix: Integer): Integer;
begin
  Result := Server(sSetUnitHome, Me, uix, nodata^);
end;

function TCustomAI.Unit_Load(uix: Integer): Integer;
begin
  Result := Server(sLoadUnit, Me, uix, nodata^);
end;

function TCustomAI.Unit_Unload(uix: Integer): Integer;
begin
  Result := Server(sUnloadUnit, Me, uix, nodata^);
end;

function TCustomAI.Unit_AddToCity(uix: Integer): Integer;
begin
  Result := Server(sAddToCity, Me, uix, nodata^);
end;

function TCustomAI.Unit_SelectTransport(uix: Integer): Integer;
begin
  Result := Server(sSelectTransport, Me, uix, nodata^);
end;


procedure TCustomAI.City_FindMyCity(Loc: Integer; var cix: Integer);
begin
  if Map[Loc] and (fCity or fOwned) <> fCity or fOwned then
    cix := -1
  else
  begin
    cix := RO.nCity - 1;
    while (cix >= 0) and (MyCity[cix].Loc <> Loc) do
      Dec(cix);
  end;
end;

procedure TCustomAI.City_FindEnemyCity(Loc: Integer; var ecix: Integer);
begin
  if Map[Loc] and (fCity or fOwned) <> fCity then
    ecix := -1
  else
  begin
    ecix := RO.nEnemyCity - 1;
    while (ecix >= 0) and (RO.EnemyCity[ecix].Loc <> Loc) do
      Dec(ecix);
  end;
end;

function TCustomAI.City_HasProject(cix: Integer): Boolean;
begin
  Result := MyCity[cix].Project and (cpImp + cpIndex) <> cpImp + imTrGoods;
end;

function TCustomAI.City_CurrentImprovementProject(cix: Integer): Integer;
begin
  if MyCity[cix].Project and cpImp = 0 then
    Result := -1
  else
  begin
    Result := MyCity[cix].Project and cpIndex;
    if Result = imTrGoods then
      Result := -1;
  end;
end;

function TCustomAI.City_CurrentUnitProject(cix: Integer): Integer;
begin
  if MyCity[cix].Project and cpImp <> 0 then
    Result := -1
  else
    Result := MyCity[cix].Project and cpIndex;
end;

function TCustomAI.City_GetTileInfo(cix, TileLoc: Integer;
  var TileInfo: TTileInfo): Integer;
begin
  TileInfo.ExplCity := cix;
  Result := Server(sGetHypoCityTileInfo, Me, TileLoc, TileInfo);
end;

function TCustomAI.City_GetReport(cix: Integer; var Report: TCityReport): Integer;
begin
  Report.HypoTiles := -1;
  Report.HypoTax := -1;
  Report.HypoLux := -1;
  Result := Server(sGetCityReport, Me, cix, Report);
end;

function TCustomAI.City_GetHypoReport(cix, HypoTiles, HypoTax, HypoLux: Integer;
  var Report: TCityReport): Integer;
begin
  Report.HypoTiles := HypoTiles;
  Report.HypoTax := HypoTax;
  Report.HypoLux := HypoLux;
  Result := Server(sGetCityReport, Me, cix, Report);
end;

function TCustomAI.City_GetReportNew(cix: Integer; var Report: TCityReportNew): Integer;
begin
  Report.HypoTiles := -1;
  Report.HypoTaxRate := -1;
  Report.HypoLuxuryRate := -1;
  Result := Server(sGetCityReportNew, Me, cix, Report);
end;

function TCustomAI.City_GetHypoReportNew(cix, HypoTiles, HypoTaxRate,
  HypoLuxuryRate: Integer; var Report: TCityReportNew): Integer;
begin
  Report.HypoTiles := HypoTiles;
  Report.HypoTaxRate := HypoTaxRate;
  Report.HypoLuxuryRate := HypoLuxuryRate;
  Result := Server(sGetCityReportNew, Me, cix, Report);
end;

function TCustomAI.City_GetAreaInfo(cix: Integer; var AreaInfo: TCityAreaInfo): Integer;
begin
  Result := Server(sGetCityAreaInfo, Me, cix, AreaInfo);
end;

function TCustomAI.City_StartUnitProduction(cix, mix: Integer): Integer;
begin
  if (MyCity[cix].Project and (cpImp + cpIndex) <> mix) then
    // not already producing that
    Result := Server(sSetCityProject, Me, cix, mix);
end;

function TCustomAI.City_StartEmigration(cix, mix: Integer;
  AllowDisbandCity, AsConscripts: Boolean): Integer;
var
  NewProject: Integer;
begin
  NewProject := mix;
  if AllowDisbandCity then
    NewProject := NewProject or cpDisbandCity;
  if AsConscripts then
    NewProject := NewProject or cpConscripts;
  Result := Server(sSetCityProject, Me, cix, NewProject);
end;

function TCustomAI.City_StartImprovement(cix, iix: Integer): Integer;
var
  NewProject: Integer;
begin
  NewProject := iix + cpImp;
  if (MyCity[cix].Project and (cpImp + cpIndex) <> NewProject) then
    // not already producing that
    Result := Server(sSetCityProject, Me, cix, NewProject);
end;

function TCustomAI.City_Improvable(cix, iix: Integer): Boolean;
var
  NewProject: Integer;
begin
  NewProject := iix + cpImp;
  Result := Server(sSetCityProject - sExecute, Me, cix, NewProject) >= rExecuted;
end;

function TCustomAI.City_StopProduction(cix: Integer): Integer;
var
  NewProject: Integer;
begin
  NewProject := imTrGoods + cpImp;
  Result := Server(sSetCityProject, Me, cix, NewProject);
end;

function TCustomAI.City_BuyProject(cix: Integer): Integer;
begin
  Result := Server(sBuyCityProject, Me, cix, nodata^);
end;

function TCustomAI.City_SellImprovement(cix, iix: Integer): Integer;
begin
  Result := Server(sSellCityImprovement, Me, cix, iix);
end;

function TCustomAI.City_RebuildImprovement(cix, iix: Integer): Integer;
begin
  Result := Server(sRebuildCityImprovement, Me, cix, iix);
end;

function TCustomAI.City_SetTiles(cix, NewTiles: Integer): Integer;
begin
  Result := Server(sSetCityTiles, Me, cix, NewTiles);
end;

procedure TCustomAI.City_OptimizeTiles(cix: Integer; ResourceWeights: Cardinal);
var
  Advice: TCityTileAdviceData;
begin
  Advice.ResourceWeights := ResourceWeights;
  Server(sGetCityTileAdvice, Me, cix, Advice);
  City_SetTiles(cix, Advice.Tiles);
end;


// negotiation
function TCustomAI.Nego_CheckMyAction: Integer;
begin
  Assert(Opponent >= 0); // only allowed in negotiation mode
  Assert((MyAction = scDipNotice) or (MyAction = scDipAccept) or
    (MyAction = scDipCancelTreaty) or (MyAction = scDipOffer) or (MyAction = scDipBreak));
  if MyAction = scDipOffer then
    Result := Server(MyAction - sExecute, Me, 0, MyOffer)
  else
    Result := Server(MyAction - sExecute, Me, 0, nodata^);
end;


initialization
  nodata := Pointer(0);
  RWDataSize := 0;

end.
