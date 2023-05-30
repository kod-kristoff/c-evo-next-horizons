{$INCLUDE Switches.inc}
// {$DEFINE TEXTLOG}
// {$DEFINE LOADPERF}
unit GameServer;

interface

uses
  Protocol, Database, dynlibs, Platform, dateutils, LazFileUtils, Graphics,
  Brain, Global;

const
  FirstAICompatibleVersion = $000D00;
  FirstBookCompatibleVersion = $010103;

  maxBrain = 255;

type
  // notifications
  TNotify = (
    ntCreateWorld,
    ntInitModule,
    ntInitLocalHuman,
    ntDLLError,
    ntAIError,
    ntClientError,
    ntInitPlayers,
    ntDeactivationMissing,
    ntSetAIName,
    ntException,
    ntLoadBegin,
    ntLoadState,
    ntEndInfo,
    ntBackOn,
    ntBackOff,
    ntLoadError,
    ntStartDone,
    ntStartGo,
    ntStartGoRefresh,
    ntStartGoRefreshMaps,
    ntChangeClient,
    ntNextPlayer,
    ntDeinitModule
  );

  TNotifyFunction = procedure(ID: TNotify; Index: Integer = 0);

var
  // PARAMETERS
  PlayersBrain: TBrains; { brain of the players view }
  Difficulty: array [0 .. nPl - 1] of Integer absolute Database.Difficulty;
  { difficulty }

  // READ ONLY
  DotNetClient: TClientCall;
  Brains: TBrains; // { available brains }
  NotifyMessage: string;

  BrainNoTerm: TBrain;
  BrainSuperVirtual: TBrain;
  BrainTerm: TBrain;
  BrainRandom: TBrain;
  BrainNetworkClient: TBrain;
  BrainNetworkServer: TBrain;

  NetworkEnabled: Boolean;

procedure Init(NotifyFunction: TNotifyFunction);
procedure Done;

procedure StartNewGame(const Path, FileName, Map: string;
  Newlx, Newly, NewLandMass, NewMaxTurn: Integer);
function LoadGame(const Path, FileName: string; Turn: Integer;
  MovieMode: Boolean): Boolean;
procedure EditMap(const Map: string; Newlx, Newly, NewLandMass: Integer);
procedure DirectHelp(Command: Integer);

procedure ChangeClient;
procedure NextPlayer;
function PreviewMap(lm: Integer): Pointer;


implementation

uses
  Directories, CityProcessing, UnitProcessing, CmdList, LCLIntf, LCLType,
  LMessages, Classes, SysUtils;

resourcestring
  SNoAiFound = 'No AI libraries found in directory %s';

var
  MaxTurn: Integer;
  LoadTurn: Integer; { turn where to stop loading }
  nLogOpened: Integer; { nLog of opened book }
{$IFOPT O-}nHandoverStack, {$ENDIF}
  LastEndClientCommand: Integer;
  pContacted: Integer; // player contacted for negotiation
  pDipActive: Integer; // player who's to speak in a negotiation
  pTurn: Integer; { player who's turn it is }
  GWinner: Integer;
  GColdWarStart: Integer;
  GStealFrom: Integer;
  SpyMission: Integer;
  ZOCTile: Integer;
  CCCommand: Integer;
  CCPlayer: Integer;
  DebugMap: array [0 .. nPl - 1] of Pointer;
  ExeInfo: TSearchRec;
  Stat: array [0 .. nStat - 1, 0 .. nPl - 1] of ^TChart;
  AutoSaveState: TCmdListState;
  MapField: ^Cardinal; // predefined map
  LastOffer: TOffer;
  CCData: array [0 .. 14] of Integer;
  bix: TBrains; { brain of the players }
  DevModelTurn: array [0 .. nPl - 1] of Integer; { turn of last call to sResetModel }
  OriginalDataVersion: array [0 .. nPl - 1] of Integer;
  SavedTiles { , SavedResourceWeights } : array [0 .. ncmax - 1] of Cardinal;
  SavedData: array [0 .. nPl - 1] of Pointer;
  LogFileName: string;
  SavePath: string; { name of file for saving the current game }
  MapFileName: string; // name of map to use, empty for random
  AICredits: string;
  AIInfo: array [0 .. nPl - 1] of string;
  Notify: TNotifyFunction;
  LastClientTime: TDateTime;
{$IFOPT O-}HandoverStack: array [0 .. 31] of Cardinal; {$ENDIF}
  AutoSaveExists: Boolean;
  LoadOK: Boolean;
  WinOnAlone: Boolean;
  PreviewElevation: Boolean;
  MovieStopped: Boolean;

const
  PreviewRND = 41601260; { randseed for preview map }

function Server(Command, Player, Subject: Integer; var Data): Integer;
  stdcall; forward;

procedure CallPlayer(Command, P: Integer; var Data);
begin
  if ((Mode <> moMovie) or (P = 0)) then
  begin
{$IFOPT O-}
    HandoverStack[nHandoverStack] := P;
    HandoverStack[nHandoverStack + 1] := Command;
    Inc(nHandoverStack, 2);
    bix[P].Client(Command, P, Data);
    Dec(nHandoverStack, 2);
{$ELSE}
    try
      bix[P].Client(Command, P, Data);
    except
      Notify(ntException + bix[P]);
    end;
{$ENDIF}
  end;
end;

procedure CallAllPlayers(Command: Integer; var Data);
var
  I: Integer;
begin
  for I := 0 to nPl - 1 do
    if Assigned(bix[I]) then
      CallPlayer(Command, I, Data);
end;

procedure CallClient(bix, Command: Integer; var Data);
begin
  if ((Mode <> moMovie) or (bix = Brains.IndexOf(GameServer.bix[0]))) then
  begin
{$IFOPT O-}
    HandoverStack[nHandoverStack] := bix;
    HandoverStack[nHandoverStack + 1] := Command;
    Inc(nHandoverStack, 2);
    Brains[bix].Client(Command, -1, Data);
    Dec(nHandoverStack, 2);
{$ELSE}
    try
      Brain[bix].Client(Command, -1, Data);
    except
      Notify(ntException + bix);
    end;
{$ENDIF}
  end
end;

procedure Init(NotifyFunction: TNotifyFunction);
var
  F: TSearchRec;
  BasePath: string;
  NewBrain: TBrain;
  I: Integer;
begin
  Notify := NotifyFunction;
  PreviewElevation := False;
  PlayersBrain := TBrains.Create(False);
  PlayersBrain.Count := nPl;
  for I := 0 to nPl - 1 do
    PlayersBrain[I] := nil;

  bix := TBrains.Create(False);
  bix.Count := nPl;
  for I := 0 to nPl - 1 do
    bix[I] := nil;

  { get available brains }
  Brains := TBrains.Create;
  BrainNoTerm := Brains.AddNew;
  BrainNoTerm.FileName := ':AIT';
  BrainNoTerm.Flags := 0;
  BrainNoTerm.Initialized := False;
  BrainNoTerm.Kind := btNoTerm;
  BrainSuperVirtual := Brains.AddNew;
  BrainSuperVirtual.FileName := ':Supervisor';
  BrainSuperVirtual.Flags := 0;
  BrainSuperVirtual.Initialized := False;
  BrainSuperVirtual.Kind := btSuperVirtual;
  if NetworkEnabled then begin
    BrainNetworkClient := Brains.AddNew;
    BrainNetworkClient.FileName := ':NetworkClient';
    BrainNetworkClient.Flags := fMultiple;
    BrainNetworkClient.Initialized := False;
    BrainNetworkClient.ServerVersion := CevoVersion;
    BrainNetworkClient.Kind := btNetworkClient;
  end;
  BrainTerm := Brains.AddNew;
  BrainTerm.FileName := ':StdIntf';
  BrainTerm.Flags := fMultiple;
  BrainTerm.Initialized := False;
  BrainTerm.ServerVersion := CevoVersion;
  BrainTerm.Kind := btTerm;
  BrainRandom := Brains.AddNew;
  BrainRandom.FileName := ':Random';
  BrainRandom.Flags := fMultiple;
  BrainRandom.Initialized := False;
  BrainRandom.Kind := btRandom;
  if NetworkEnabled then begin
    BrainNetworkServer := Brains.AddNew;
    BrainNetworkServer.FileName := ':NetworkServer';
    BrainNetworkServer.Flags := fMultiple;
    BrainNetworkServer.Initialized := False;
    BrainNetworkServer.ServerVersion := CevoVersion;
    BrainNetworkServer.Kind := btNetworkServer;
  end;

  if FindFirst(GetAiDir + DirectorySeparator + '*', faDirectory or faArchive or faReadOnly, F) = 0 then
  repeat
    BasePath := GetAiDir + DirectorySeparator + F.Name;
    if (F.Name <> '.') and (F.Name <> '..') and DirectoryExists(BasePath) then begin
      NewBrain := Brains.AddNew;
      NewBrain.Kind := btAI;
      NewBrain.LoadFromFile(BasePath + DirectorySeparator + F.Name + '.ai.txt');
      if (NewBrain.ServerVersion >= FirstAICompatibleVersion) and
        (NewBrain.ServerVersion <= CevoVersion) and
        ((NewBrain.Flags and fDotNet = 0) or (@DotNetClient <> nil)) then begin
        end else Brains.Delete(Brains.Count - 1);
    end;
  until FindNext(F) <> 0;
  FindClose(F);

  if Brains.GetKindCount(btAI) = 0 then
    raise Exception.Create(Format(SNoAiFound, [GetAiDir]));
end;

procedure Done;
var
  I: Integer;
begin
  for I := 0 to Brains.Count - 1 do
  with Brains[I] do
    if Initialized then begin
      CallClient(I, cReleaseModule, nil^);
      if (Kind = btAI) and ((Flags and fDotNet) = 0) then
        FreeLibrary(hm);
    end;
  FreeAndNil(PlayersBrain);
  FreeAndNil(bix);
  FreeAndNil(Brains);
end;

function PreviewMap(lm: Integer): Pointer;
begin
  lx := lxmax;
  ly := lymax;
  MapSize := lx * ly;
  LandMass := lm;
  DelphiRandSeed := PreviewRND;
  if not PreviewElevation then
  begin
    CreateElevation;
    PreviewElevation := True;
  end;
  CreateMap(True);
  Result := @RealMap;
end;

procedure ChangeClientWhenDone(Command, Player: Integer; var Data;
  DataSize: Integer);
begin
  CCCommand := Command;
  CCPlayer := Player;
  if DataSize > 0 then
    Move(Data, CCData, DataSize);
  Notify(ntChangeClient);
end;

procedure PutMessage(Level: Integer; Text: string);
begin
  bix[0].Client(cDebugMessage, Level, PChar(Text)^);
end;

procedure ForceClientDeactivation;
var
  NullOffer: TOffer;
begin
  if pDipActive < 0 then
    Server(sTurn, pTurn, 0, nil^) // no nego mode
  else
    case LastEndClientCommand of // nego mode
      scContact:
        Server(scReject, pDipActive, 0, nil^);
      scDipCancelTreaty, scDipBreak:
        Server(scDipNotice, pDipActive, 0, nil^);
    else
      begin // make null offer
        NullOffer.nDeliver := 0;
        NullOffer.nCost := 0;
        Server(scDipOffer, pDipActive, 0, NullOffer);
      end
    end
end;

procedure ChangeClient;
// hand over control to other client (as specified by CC...)
var
  P: Integer;
  T: TDateTime;
begin
  T := NowPrecise;
  PutMessage(1 shl 16 + 2, Format('CLIENT: took %.1f ms',
    [(T - LastClientTime) / OneMillisecond]));
  LastClientTime := T;
  PutMessage(1 shl 16 + 2, Format('CLIENT: calling %d (%s)',
    [CCPlayer, bix[CCPlayer].Name]));
  if CCCommand = cTurn then
    for P := 0 to nPl - 1 do
      if (P <> CCPlayer) and (1 shl P and GWatching <> 0) then
        CallPlayer(cShowTurnChange, P, CCPlayer);

  P := CCPlayer;
  CCPlayer := -1;
  CallPlayer(CCCommand, P, CCData);
  if (Mode = moPlaying) and (bix[P].Flags and aiThreaded = 0) and
    (CCPlayer < 0) then
  begin
    Notify(ntDeactivationMissing, P);
    ForceClientDeactivation;
  end;
end;

procedure Inform(P: Integer);
var
  I, p1: Integer;
begin
  RW[P].Turn := GTurn;
  if (GTurn = MaxTurn) and (P = pTurn) and (P = 0) then
    RW[P].Happened := RW[P].Happened or phTimeUp;
  if (GWinner > 0) and (P = pTurn) and (P = 0) then
    RW[P].Happened := RW[P].Happened or phShipComplete;
  RW[P].Alive := GAlive;
  Move(GWonder, RW[P].Wonder, SizeOf(GWonder));
  Move(GShip, RW[P].Ship, SizeOf(GShip));
  for p1 := 0 to nPl - 1 do
    if (p1 <> P) and Assigned(bix[p1]) and (Difficulty[p1] > 0) then
      RW[P].EnemyReport[p1].Credibility := RW[p1].Credibility;
  for p1 := 0 to nPl - 1 do
    if (p1 <> P) and (1 shl p1 and GAlive <> 0) then
    begin
      if (GTestFlags and tfUncover <> 0) or (Difficulty[P] = 0) or
        (RW[P].Treaty[p1] >= trFriendlyContact) then
        GiveCivilReport(P, p1);
      if (GTestFlags and tfUncover <> 0) or (Difficulty[P] = 0) or
        (RW[P].Treaty[p1] = trAlliance) then
        GiveMilReport(P, p1)
    end;
  for I := 0 to RW[P].nEnemyModel - 1 do
    with RW[P].EnemyModel[I] do
      Lost := Destroyed[P, Owner, mix];
end;

procedure LogChanges;
var
  P, ix: Integer;
begin
  for P := 0 to nPl - 1 do
    if (1 shl P and GWatching <> 0) and ProcessClientData[P] then
    begin
      // log unit status changes
      for ix := 0 to RW[P].nUn - 1 do
        with RW[P].Un[ix] do
          if (Loc >= 0) and (SavedStatus <> Status) then
          begin
            CL.Put(sIntSetUnitStatus, P, ix, @Status);
            SavedStatus := Status;
          end;
      // log city status changes
      for ix := 0 to RW[P].nCity - 1 do
        with RW[P].City[ix] do
          if (Loc >= 0) and (SavedStatus <> Status) then
          begin
            CL.Put(sIntSetCityStatus, P, ix, @Status);
            SavedStatus := Status;
          end;
      // log model status changes
      for ix := 0 to RW[P].nModel - 1 do
        with RW[P].Model[ix] do
          if SavedStatus <> Status then
          begin
            CL.Put(sIntSetModelStatus, P, ix, @Status);
            SavedStatus := Status;
          end;
      // log enemy city status changes
      for ix := 0 to RW[P].nEnemyCity - 1 do
        with RW[P].EnemyCity[ix] do
          if (Loc >= 0) and (SavedStatus <> Status) then
          begin
            CL.Put(sIntSetECityStatus, P, ix, @Status);
            SavedStatus := Status;
          end;
      // log data changes
      if bix[P].DataSize > 0 then
      begin
        CL.PutDataChanges(sIntDataChange, P, SavedData[P], RW[P].Data,
          bix[P].DataSize);
        Move(RW[P].Data^, SavedData[P]^, bix[P].DataSize * 4);
      end;
    end;
end;

procedure NoLogChanges;
var
  P, ix: Integer;
begin
  for P := 0 to nPl - 1 do
    if (1 shl P and GWatching <> 0) and ProcessClientData[P] then
    begin
      for ix := 0 to RW[P].nUn - 1 do
        with RW[P].Un[ix] do
          SavedStatus := Status;
      for ix := 0 to RW[P].nCity - 1 do
        with RW[P].City[ix] do
          SavedStatus := Status;
      for ix := 0 to RW[P].nModel - 1 do
        with RW[P].Model[ix] do
          SavedStatus := Status;
      for ix := 0 to RW[P].nEnemyCity - 1 do
        with RW[P].EnemyCity[ix] do
          SavedStatus := Status;
      if bix[P].DataSize > 0 then
        Move(RW[P].Data^, SavedData[P]^, bix[P].DataSize * 4);
    end;
end;

function HasChanges(P: Integer): Boolean;
type
  TDWordList = array [0 .. INFIN] of Cardinal;
  PDWortList = ^TDWordList;
var
  ix: Integer;
begin
  Result := False;
  for ix := 0 to RW[P].nUn - 1 do
    with RW[P].Un[ix] do
      if (Loc >= 0) and (SavedStatus <> Status) then
        Result := True;
  for ix := 0 to RW[P].nCity - 1 do
    with RW[P].City[ix] do
      if (Loc >= 0) and (SavedStatus <> Status) then
        Result := True;
  for ix := 0 to RW[P].nModel - 1 do
    with RW[P].Model[ix] do
      if SavedStatus <> Status then
        Result := True;
  for ix := 0 to RW[P].nEnemyCity - 1 do
    with RW[P].EnemyCity[ix] do
      if (Loc >= 0) and (SavedStatus <> Status) then
        Result := True;
  if RW[P].Data <> nil then
    for ix := 0 to bix[P].DataSize - 1 do
      if PDWortList(SavedData[P])[ix] <> PDWortList(RW[P].Data)[ix] then
        Result := True;
end;

procedure InitBrain(bix: TBrain);
var
  InitModuleData: TInitModuleData;
begin
  Assert(bix.Kind <> btSuperVirtual);
  with bix do begin
    if Initialized then
      Exit;
    if Kind = btAI then
    begin { get client function }
      Notify(ntInitModule, Brains.IndexOf(bix));
      if Flags and fDotNet > 0 then
        Client := DotNetClient
      else
      begin
        hm := LoadLibrary(PChar(DLLName));
        if hm = 0 then
        begin
          Client := nil;
          Notify(ntDLLError, Brains.IndexOf(bix));
        end
        else
        begin
          Client := GetProcAddress(hm, 'client');
          if @Client = nil then
            Notify(ntClientError, Brains.IndexOf(bix));
        end;
      end;
    end;
    if @Client <> nil then
    begin
      Initialized := True;
      InitModuleData.Server := @Server;
      InitModuleData.DataVersion := 0;
      InitModuleData.DataSize := 0;
      InitModuleData.Flags := 0;
      CallClient(Brains.IndexOf(bix), cInitModule, InitModuleData);
      DataVersion := InitModuleData.DataVersion;
      DataSize := (InitModuleData.DataSize + 3) div 4;
      if DataSize > MaxDataSize then
        DataSize := 0;
      Flags := Flags or InitModuleData.Flags;
    end;
  end;
end;

procedure SaveMap(FileName: string);
var
  I: Integer;
  MapFile: TFileStream;
  S: string[255];
begin
  MapFile := TFileStream.Create(GetMapsDir + DirectorySeparator + FileName,
    fmCreate or fmShareExclusive);
  try
    MapFile.Position := 0;
    S := 'cEvoMap'#0;
    MapFile.write(S[1], 8); { file id }
    I := 0;
    MapFile.write(I, 4); { format id }
    MapFile.write(MaxTurn, 4);
    MapFile.write(lx, 4);
    MapFile.write(ly, 4);
    MapFile.write(RealMap, MapSize * 4);
  finally
    FreeAndNil(MapFile);
  end;
end;

function LoadMap(FileName: string): Boolean;
var
  I, Loc1: Integer;
  MapFile: TFileStream;
  S: string[255];
begin
  Result := False;
  MapFile := nil;
  try
    MapFile := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
    MapFile.Position := 0;
    MapFile.read(S[1], 8); { file id }
    MapFile.read(I, 4); { format id }
    if I = 0 then
    begin
      MapFile.read(I, 4); // MaxTurn
      MapFile.read(lx, 4);
      MapFile.read(ly, 4);
      ly := ly and not 1;
      if lx > lxmax then
        lx := lxmax;
      if ly > lymax then
        ly := lymax;
      MapSize := lx * ly;
      MapFile.read(RealMap, MapSize * 4);
      for Loc1 := 0 to MapSize - 1 do
      begin
        RealMap[Loc1] := RealMap[Loc1] and
          ($7F01FFFF or fPrefStartPos or fStartPos) or ($F shl 27);
        if RealMap[Loc1] and (fTerrain or fSpecial) = fSwamp or fSpecial2 then
          RealMap[Loc1] := RealMap[Loc1] and not(fTerrain or fSpecial) or
            (fSwamp or fSpecial1);
        if (RealMap[Loc1] and fDeadLands <> 0) and
          (RealMap[Loc1] and fTerrain <> fArctic) then
          RealMap[Loc1] := RealMap[Loc1] and not(fTerrain or fSpecial)
            or fDesert;
      end;
      Result := True;
    end;
    FreeAndNil(MapFile);
  except
    if MapFile <> nil then
      FreeAndNil(MapFile);
  end;
end;

procedure SaveGame(FileName: string; auto: Boolean);
var
  X, Y, I, zero, Tile, nLocal: Integer;
  LogFile: TFileStream;
  S: string[255];
  SaveMap: array [0 .. lxmax * lymax - 1] of Byte;
begin
  nLocal := 0;
  for I := 0 to nPl - 1 do
    if Assigned(bix[I]) and (bix[I].Kind = btTerm) then
      Inc(nLocal);
  if Difficulty[0] = 0 then
    nLocal := 0;
  if nLocal <= 1 then
    for Y := 0 to ly - 1 do
      for X := 0 to lx - 1 do
      begin
        Tile := RW[0].Map[(X + SaveMapCenterLoc + lx shr 1) mod lx + lx * Y];
        SaveMap[X + lx * Y] := Tile and fTerrain + Tile and
          (fCity or fUnit or fOwned) shr 16;
      end;

  if auto and AutoSaveExists then // append to existing file
    LogFile := TFileStream.Create(SavePath + FileName, fmOpenReadWrite or
      fmShareExclusive)
  else // create new file
    LogFile := TFileStream.Create(SavePath + FileName,
      fmCreate or fmShareExclusive);

  zero := 0;
  LogFile.Position := 0;
  S := 'cEvoBook';
  LogFile.write(S[1], 8); { file id }
  I := CevoVersion;
  LogFile.write(I, 4); { c-evo version }
  LogFile.write(ExeInfo.Time, 4);
  LogFile.write(lx, 4);
  LogFile.write(ly, 4);
  LogFile.write(LandMass, 4);
  if LandMass = 0 then
    LogFile.write(MapField^, MapSize * 4);

  LogFile.write(MaxTurn, 4);
  LogFile.write(RND, 4);
  LogFile.write(GTurn, 4);
  if nLocal > 1 then // multiplayer game -- no quick view
  begin
    I := $80;
    LogFile.write(I, 4);
  end
  else
    LogFile.write(SaveMap, ((MapSize - 1) div 4 + 1) * 4);
  for I := 0 to nPl - 1 do
    if not Assigned(bix[I]) then
      LogFile.write(zero, 4)
    else
    begin
      if PlayersBrain[I].Kind in [btRandom, btAI] then
        S := bix[I].FileName
      else
        S := PlayersBrain[I].FileName;
      Move(zero, S[Length(S) + 1], 4);
      LogFile.write(S, (Length(S) div 4 + 1) * 4);
      LogFile.write(OriginalDataVersion[I], 4);
      S := ''; { behavior }
      Move(zero, S[Length(S) + 1], 4);
      LogFile.write(S, (Length(S) div 4 + 1) * 4);
      LogFile.write(Difficulty[I], 4);
    end;

  if auto and AutoSaveExists then
    CL.AppendToFile(LogFile, AutoSaveState)
  else
    CL.SaveToFile(LogFile);
  FreeAndNil(LogFile);
  if auto then
  begin
    AutoSaveState := CL.State;
    AutoSaveExists := True;
  end
end;

procedure StartGame;
var
  I, P, p1, Human, nAlive, bixUni: Integer;
  Game: TNewGameData;
  // GameEx: TNewGameExData;
  Path: shortstring;
  BrainUsed: Set of 0 .. 254; { used brains }
  AIBrains: TBrains;
begin
  for p1 := 0 to nPl - 1 do begin
    if Assigned(PlayersBrain[p1]) and (PlayersBrain[p1].Kind = btSuperVirtual) then
      bix[p1] := BrainTerm // supervisor and local human use same module
    else if Assigned(PlayersBrain[p1]) and (PlayersBrain[p1].Kind = btRandom) then
      if Brains.GetKindCount(btAI) = 0 then
        bix[p1] := nil
      else begin
        AIBrains := TBrains.Create(False);
        Brains.GetByKind(btAI, AIBrains);
        bix[p1] := AIBrains[DelphiRandom(AIBrains.Count)];
        FreeAndNil(AIBrains);
      end
    else
      bix[p1] := PlayersBrain[p1];
    if not Assigned(PlayersBrain[p1]) then
      Difficulty[p1] := -1;
  end;

  if bix[0].Kind <> btNoTerm then
    Notify(ntInitLocalHuman);

  BrainUsed := [];
  for P := 0 to nPl - 1 do
    if Assigned(bix[P]) and ((Mode <> moMovie) or (P = 0)) then
    begin { initiate selected control module }
      AIInfo[P] := bix[P].Name + #0;
      InitBrain(bix[P]);
      if Mode = moPlaying then
      begin // new game, this data version is original
        OriginalDataVersion[P] := bix[P].DataVersion;
        ProcessClientData[P] := True;
      end
      else // loading game, compare with data version read from file
        ProcessClientData[P] := ProcessClientData[P] and
          (OriginalDataVersion[P] = bix[P].DataVersion);
      if @bix[P].Client = nil then // client function not found
        if bix[0].Kind = btNoTerm then
          bix[P] := nil
        else
        begin
          bix[P] := BrainTerm;
          OriginalDataVersion[P] := -1;
          ProcessClientData[P] := False;
        end;
      if Assigned(bix[P]) then
        Include(BrainUsed, Brains.IndexOf(bix[P]));
    end;

  Notify(ntCreateWorld);
  nAlive := 0;
  GAlive := 0;
  if Mode = moMovie then
    GWatching := 1
  else
    GWatching := 0;
  GAI := 0;
  for p1 := 0 to nPl - 1 do
    if Assigned(bix[p1]) then
    begin
      if Mode <> moMovie then
        Inc(GWatching, 1 shl p1);
      if bix[p1].Kind = btAI then
        Inc(GAI, 1 shl p1);
      if Difficulty[p1] > 0 then
      begin
        Inc(GAlive, 1 shl p1);
        Inc(nAlive);
      end;
      ServerVersion[p1] := bix[p1].ServerVersion;
    end;
  WinOnAlone := (bix[0].Kind = btNoTerm) and (nAlive > 1);
  GWinner := 0;
  GColdWarStart := -ColdWarTurns - 1;
  uixSelectedTransport := -1;
  SpyMission := smSabotageProd;
  for p1 := 0 to nPl - 1 do
    DebugMap[p1] := nil;

  GTurn := 0;
  for I := 0 to nWonder - 1 do
    with GWonder[I] do
    begin
      CityID := -1;
      EffectiveOwner := -1
    end;
  FillChar(GShip, SizeOf(GShip), 0);

  for P := 0 to nPl - 1 do
    if 1 shl P and (GAlive or GWatching) <> 0 then
      with RW[P] do
      begin
        Government := gDespotism;
        Money := StartMoney;
        TaxRate := 30;
        LuxRate := 0;
        Research := 0;
        ResearchTech := -2;
        AnarchyStart := -AnarchyTurns - 1;
        Happened := 0;
        LastValidStat[P] := -1;
        Worked[P] := 0;
        Founded[P] := 0;
        DevModelTurn[P] := -1;
        OracleIncome := 0;

        if bix[P].DataSize > 0 then
        begin
          GetMem(SavedData[P], bix[P].DataSize * 4);
          GetMem(Data, bix[P].DataSize * 4);
          FillChar(SavedData[P]^, bix[P].DataSize * 4, 0);
          FillChar(Data^, bix[P].DataSize * 4, 0);
        end
        else
        begin
          Data := nil;
          SavedData[P] := nil;
        end;
        nBattleHistory := 0;
        BattleHistory := nil;
        { if bix[p]=bixTerm then
          begin
          GetMem(BorderHelper,MapSize);
          FillChar(BorderHelper^,MapSize,0);
          end
          else } BorderHelper := nil;
        for I := 0 to nStat - 1 do
          GetMem(Stat[I, P], 4 * (MaxTurn + 1));
        if bix[P].Flags and fDotNet <> 0 then
        begin
          GetMem(RW[P].DefaultDebugMap, MapSize * 4);
          FillChar(RW[P].DefaultDebugMap^, MapSize * 4, 0);
          DebugMap[P] := RW[P].DefaultDebugMap;
        end
        else
          RW[P].DefaultDebugMap := nil;

        { !!!for i:=0 to nShipPart-1 do GShip[p].Parts[i]:=Delphirandom((3-i)*2); }
      end;

  if LandMass > 0 then
  begin // random map
    InitRandomGame;
    PreviewElevation := False;
    MapField := nil;
  end
  else
  begin // predefined map
    if Mode = moPlaying then
      LoadMap(MapFileName); // new game -- load map from file
    GetMem(MapField, MapSize * 4);
    Move(RealMap, MapField^, MapSize * 4);
    Human := 0;
    for p1 := 0 to nPl - 1 do
      if Assigned(bix[p1]) and (bix[p1].Kind = btTerm) then
        Inc(Human, 1 shl p1);
    InitMapGame(Human);
  end;
  CityProcessing.InitGame;
  UnitProcessing.InitGame;
  for P := 0 to nPl - 1 do
    if 1 shl P and (GAlive or GWatching) <> 0 then
      Inform(P);

  pTurn := -1;
  if bix[0].Kind <> btNoTerm then
    Notify(ntInitLocalHuman);
  Game.lx := lx;
  Game.ly := ly;
  Game.LandMass := LandMass;
  Game.MaxTurn := MaxTurn;
  Move(Difficulty, Game.Difficulty, SizeOf(Difficulty));
  // GameEx.lx:=lx; GameEx.ly:=ly; GameEx.LandMass:=LandMass;
  // GameEx.MaxTurn:=MaxTurn; GameEx.RND:=RND;
  // move(Difficulty,GameEx.Difficulty,SizeOf(Difficulty));
  AICredits := '';
  for I := 0 to Brains.Count - 1 do
  with Brains[I] do begin
    if Initialized then
      if I in BrainUsed then
      begin
        if Kind = btAI then
          Notify(ntInitPlayers);
        for P := 0 to nPl - 1 do
        begin
          if Brains.IndexOf(bix[P]) = I then
            Game.RO[P] := @RW[P]
          else
            Game.RO[P] := nil;
          if (Kind = btTerm) and (Difficulty[0] = 0) and Assigned(bix[P]) then
            Game.SuperVisorRO[P] := @RW[P]
          else
            Game.SuperVisorRO[P] := nil;
        end;
        if Flags and fDotNet > 0 then
        begin
          Path := DLLName;
          Move(Path[1], Game.AssemblyPath, Length(Path));
          Game.AssemblyPath[Length(Path)] := #0;
        end
        else
          Game.AssemblyPath[0] := #0;
        case Mode of
          moLoading, moLoading_Fast:
            CallClient(I, cLoadGame, Game);
          moMovie:
            CallClient(I, cMovie, Game);
          moPlaying:
            CallClient(I, cNewGame, Game);
        end;
        if (Kind = btAI) and (Credits <> '') then
          if AICredits = '' then
            AICredits := Credits
          else
            AICredits := AICredits + '\' + Credits;
      end
      else
      begin { module no longer used -- unload }
        CallClient(I, cReleaseModule, nil^);
        if Kind = btAI then
        begin
          if Flags and fDotNet = 0 then
            FreeLibrary(hm);
          Client := nil;
        end;
        Initialized := False;
      end;
  end;
  AICredits := AICredits + #0;

  if bix[0].Kind <> btNoTerm then
  begin
    // uni ai?
    bixUni := -1;
    for p1 := 0 to nPl - 1 do
      if Assigned(bix[p1]) and (bix[p1].Kind = btAI) then
        if bixUni = -1 then
          bixUni := Brains.IndexOf(bix[p1])
        else if bixUni <> Brains.IndexOf(bix[p1]) then
          bixUni := -2;
    for p1 := 0 to nPl - 1 do
      if Assigned(bix[p1]) and (bix[p1].Kind = btAI) then
      begin
        if bixUni = -2 then
          NotifyMessage := bix[p1].FileName
        else
          NotifyMessage := '';
        Notify(ntSetAIName, p1);
      end;
  end;

  CheckBorders(-1);
{$IFOPT O-}InvalidTreatyMap := 0; {$ENDIF}
  AutoSaveExists := False;
  pDipActive := -1;
  pTurn := 0;

  if Mode >= moMovie then
    Notify(ntEndInfo);
end;

procedure EndGame;
var
  I, p1: Integer;
begin
  if LandMass = 0 then
    FreeMem(MapField);
  for p1 := 0 to nPl - 1 do
    if Assigned(bix[p1]) then
    begin
      for I := 0 to nStat - 1 do
        FreeMem(Stat[I, p1]);
      if RW[p1].BattleHistory <> nil then
        FreeMem(RW[p1].BattleHistory);
      { if RW[p1].BorderHelper<>nil then FreeMem(RW[p1].BorderHelper); }
      FreeMem(RW[p1].Data);
      FreeMem(SavedData[p1]);
      if RW[p1].DefaultDebugMap <> nil then
        FreeMem(RW[p1].DefaultDebugMap);
    end;
  UnitProcessing.ReleaseGame;
  CityProcessing.ReleaseGame;
  Database.ReleaseGame;
  FreeAndNil(CL);
end;

procedure GenerateStat(P: Integer);
var
  cix, uix: Integer;
begin
  if Difficulty[P] > 0 then
    with RW[P] do
    begin
      Stat[stPop, P, GTurn] := 0;
      for cix := 0 to nCity - 1 do
        if City[cix].Loc >= 0 then
          Inc(Stat[stPop, P, GTurn], City[cix].Size);
      Stat[stScience, P, GTurn] := Researched[P] * 50;
      if (RW[P].ResearchTech >= 0) and (RW[P].ResearchTech <> adMilitary) then
        Inc(Stat[stScience, P, GTurn], Research * 100 div TechBaseCost(nTech[P],
          Difficulty[P]));
      Stat[stMil, P, GTurn] := 0;
      for uix := 0 to nUn - 1 do
        if Un[uix].Loc >= 0 then
          with Model[Un[uix].mix] do
          begin
            if (Kind <= mkEnemyDeveloped) and (Un[uix].mix <> 1) then
              Inc(Stat[stMil, P, GTurn], Weight * MStrength *
                Un[uix].Health div 100)
            else if Domain = dGround then
              Inc(Stat[stMil, P, GTurn], (Attack + 2 * Defense) *
                Un[uix].Health div 100)
            else
              Inc(Stat[stMil, P, GTurn], (Attack + Defense) *
                Un[uix].Health div 100);
            case Kind of
              mkSlaves:
                Inc(Stat[stPop, P, GTurn]);
              mkSettler:
                Inc(Stat[stPop, P, GTurn], 2);
            end;
          end;
      Stat[stMil, P, GTurn] := Stat[stMil, P, GTurn] div 16;
      Stat[stExplore, P, GTurn] := Discovered[P];
      Stat[stTerritory, P, GTurn] := TerritoryCount[P];
      Stat[stWork, P, GTurn] := Worked[P];
      LastValidStat[P] := GTurn;
    end;
end;

procedure LogCityTileChanges;
var
  cix: Integer;
begin
  for cix := 0 to RW[pTurn].nCity - 1 do
    with RW[pTurn].City[cix] do
      if Loc >= 0 then
      begin
        { if SavedResourceWeights[cix]<>ResourceWeights then
          begin // log city resource weight changes
          CL.Put(sSetCityResourceWeights, pTurn, cix, @ResourceWeights);
          SavedResourceWeights[cix]:=ResourceWeights;
          end; }
        if SavedTiles[cix] <> Tiles then
        begin // log city tile changes
          CL.Put(sSetCityTiles, pTurn, cix, @Tiles);
          SavedTiles[cix] := Tiles;
        end;
      end;
end;

procedure NoLogCityTileChanges;
var
  cix: Integer;
begin
  for cix := 0 to RW[pTurn].nCity - 1 do
    with RW[pTurn].City[cix] do
      if Loc >= 0 then
      begin
        // SavedResourceWeights[cix]:=ResourceWeights;
        SavedTiles[cix] := Tiles;
      end;
end;

function HasCityTileChanges: Boolean;
var
  cix: Integer;
begin
  Result := False;
  for cix := 0 to RW[pTurn].nCity - 1 do
    with RW[pTurn].City[cix] do
      if Loc >= 0 then
      begin
        // if SavedResourceWeights[cix]<>ResourceWeights then result:=true;
        if SavedTiles[cix] <> Tiles then
          Result := True;
      end;
end;

procedure BeforeTurn0;
var
  p1, uix: Integer;
begin
  for uix := 0 to RW[pTurn].nUn - 1 do { init movement points for first turn }
    with RW[pTurn].Un[uix] do
      Movement := RW[pTurn].Model[mix].Speed;

  if Difficulty[pTurn] > 0 then
    DiscoverViewAreas(pTurn)
  else { supervisor }
  begin
    DiscoverAll(pTurn, lObserveSuper);
    for p1 := 1 to nPl - 1 do
      if 1 shl p1 and GAlive <> 0 then
      begin
        GiveCivilReport(pTurn, p1);
        GiveMilReport(pTurn, p1);
      end;
  end;
  // CheckContact;
end;

function LoadGame(const Path, FileName: string; Turn: Integer;
  MovieMode: Boolean): Boolean;
var
  J: TBrain;
  I, ix, D, p1, Command, Subject: Integer;
  K: Integer;
{$IFDEF TEXTLOG}LoadPos0: Integer; {$ENDIF}
  Data: Pointer;
  LogFile: TFileStream;
  FormerCLState: TCmdListState;
  S: string[255];
  SaveMap: array [0 .. lxmax * lymax - 1] of Byte;
  Started, StatRequest: Boolean;
begin
  SavePath := Path;
  LogFileName := FileName;
  LoadTurn := Turn;
  LogFile := TFileStream.Create(SavePath + LogFileName, fmOpenRead or
    fmShareExclusive);
  LogFile.Position := 0;
  LogFile.Read(S[1], 8); { file id }
  LogFile.Read(I, 4); { c-evo version }
  LogFile.Read(J, 4); { exe time }

  if (I >= FirstBookCompatibleVersion) and (I <= CevoVersion) then
  begin
    Result := True;
    LogFile.Read(lx, 4);
    LogFile.Read(ly, 4);
    MapSize := lx * ly;
    LogFile.Read(LandMass, 4);
    if LandMass = 0 then
      LogFile.Read(RealMap, MapSize * 4); // use predefined map
    LogFile.Read(MaxTurn, 4);
    LogFile.Read(RND, 4);
    LogFile.Read(GTurn, 4);
    LogFile.Read(SaveMap, 4);
    if SaveMap[0] <> $80 then
      LogFile.read(SaveMap[4], ((MapSize - 1) div 4 + 1) * 4 - 4);
    for p1 := 0 to nPl - 1 do
    begin
      LogFile.Read(S[0], 4);
      if S[0] = #0 then
        PlayersBrain[p1] := nil
      else
      begin
        LogFile.Read(S[4], Byte(S[0]) div 4 * 4);
        LogFile.Read(OriginalDataVersion[p1], 4);
        LogFile.Read(D, 4); { behavior }
        LogFile.Read(Difficulty[p1], 4);
        J := Brains.Last;
        while Assigned(J) and (AnsiCompareFileName(J.FileName, S) <> 0) do begin
          K := Brains.IndexOf(J) - 1;
          if K >= 0 then J := Brains[K]
            else J := nil;
        end;
        if not Assigned(J) then
        begin // ai not found -- replace by local player
          ProcessClientData[p1] := False;
          NotifyMessage := S;
          Notify(ntAIError);
          J := BrainTerm;
        end
        else
          ProcessClientData[p1] := True;
        if J.Kind = btNoTerm then
          J := BrainSuperVirtual;
        // crashed tournament -- load as supervisor
        PlayersBrain[p1] := J;
      end;
    end;
  end
  else
    Result := False;

  if Result then begin
    CL := TCmdList.Create;
    CL.LoadFromFile(LogFile);
  end;
  FreeAndNil(LogFile);
  if not Result then
    Exit;

  Notify(ntStartDone);
  if LoadTurn < 0 then
    LoadTurn := GTurn;
  if MovieMode then
    Mode := moMovie
  else if LoadTurn = 0 then
    Mode := moLoading
  else
    Mode := moLoading_Fast;
{$IFDEF TEXTLOG}AssignFile(TextLog, SavePath + LogFileName + '.txt');
  Rewrite(TextLog); {$ENDIF}
  LoadOK := True;
  StartGame;
  if MovieMode then
  begin
    bix[0].Client(cShowGame, 0, nil^);
    Notify(ntBackOff);
  end
  else
    Notify(ntLoadBegin);

  started := False;
  StatRequest := False;
  MovieStopped := False;
{$IFDEF LOADPERF}QueryPerformanceCounter(time_total0);
  time_a := 0;
  time_b := 0;
  time_c := 0; {$ENDIF}
  while not MovieStopped and (CL.Progress < 1000) do
  begin
    FormerCLState := CL.State;
    CL.Get(Command, p1, Subject, Data);
    if p1 < 0 then
      p1 := pTurn;
    if StatRequest and (Command and (sctMask or sExecute) <> sctInternal or
      sExecute) then
    begin
      GenerateStat(pTurn);
      StatRequest := False;
    end;
    // complete all internal commands following an sTurn before generating statistics
    if (Command = sTurn) and not started then
    begin
{$IFDEF TEXTLOG}WriteLn(TextLog, '---Turn 0 P0---'); {$ENDIF}
      for p1 := 0 to nPl - 1 do
        if Assigned(bix[p1]) and ((Mode <> moMovie) or (p1 = 0)) then
          CallPlayer(cReplay, p1, nil^);
      BeforeTurn0;
      if MovieMode then
      begin
        Inform(pTurn);
        CallPlayer(cMovieTurn, 0, nil^);
      end;
      StatRequest := True;
      started := True;
    end
    else if (Command = sTurn) and (pTurn = 0) and (GTurn = LoadTurn) then
    begin
      Assert(CL.State.LoadPos = FormerCLState.LoadPos + 4); // size of sTurn
      CL.State := FormerCLState;
      CL.Cut;
      Break;
    end
    else if Command = sIntDataChange then
    begin
{$IFDEF TEXTLOG}LoadPos0 := CL.State.LoadPos; {$ENDIF}
      if ProcessClientData[p1] then
        CL.GetDataChanges(RW[p1].Data, bix[p1].DataSize)
      else
        CL.GetDataChanges(nil, 0);
{$IFDEF TEXTLOG}WriteLn(TextLog, Format('Data Changes P%d (%d Bytes)', [p1, CL.State.LoadPos - LoadPos0])); {$ENDIF}
    end
    else
    begin
{$IFDEF TEXTLOG}CmdInfo := Format('Command %x', [Command]); {$ENDIF}
      if Command and (sctMask or sExecute) = sctInternal or sExecute then
        IntServer(Command, p1, Subject, Data^) // internal command
      else
      begin
        StatRequest := Command = sTurn;
        Server(Command, p1, Subject, Data^);
      end;
{$IFDEF TEXTLOG}WriteLn(TextLog, CmdInfo); {$ENDIF}
    end;
    if not MovieMode then
      Notify(ntLoadState, CL.Progress * 128 div 1000);
  end;

  if MovieMode then
  begin
    Notify(ntBackOn);
    bix[0].Client(cBreakGame, -1, nil^);
    EndGame;
    Notify(ntStartGo);
    Result := False;
    Exit;
  end;

  if StatRequest then
    GenerateStat(pTurn);
  Assert(started);
{$IFDEF TEXTLOG}CloseFile(TextLog); {$ENDIF}
{$IFDEF LOADPERF}QueryPerformanceCounter(time_total); { time in s is: (time_total-time_total0)/PerfFreq }{$ENDIF}
  NoLogChanges;
  NoLogCityTileChanges;
  if LogFileName[1] = '~' then
  begin
    Delete(LogFileName, 1, 1);
    nLogOpened := -1;
  end
  else
    nLogOpened := CL.State.nLog;

  Mode := moPlaying;
  LastEndClientCommand := -1;
  if (GTestFlags and tfUncover <> 0) or (Difficulty[pTurn] = 0) then
    DiscoverAll(pTurn, lObserveSuper) { supervisor - all tiles visible }
  else
    DiscoverViewAreas(pTurn);

  for p1 := 0 to nPl - 1 do
    if 1 shl p1 and (GAlive or GWatching) <> 0 then
    begin
      RecalcPeaceMap(p1);
      for ix := 0 to RW[p1].nEnemyUn - 1 do
        with RW[p1].EnemyUn[ix] do
          emix := RWemix[p1, Owner, mix];
      Inform(p1);
    end;
{$IFOPT O-}CheckBorders(-2); {$ENDIF} // for testing only
  Notify(ntEndInfo);
  if not LoadOK then
  begin
    NotifyMessage := SavePath + LogFileName;
    Notify(ntLoadError);
  end;
  bix[0].Client(cShowGame, 0, nil^);
  Notify(ntBackOff);
  Inform(pTurn);
  ChangeClientWhenDone(cResume, 0, nil^, 0);
end;

procedure InsertTerritoryUpdateCommands;
var
  p1, Command, Subject: Integer;
  Data: Pointer;
  FormerCLState: TCmdListState;
begin
  while CL.Progress < 1000 do
  begin
    FormerCLState := CL.State;
    CL.Get(Command, p1, Subject, Data);
    if (Command = sIntExpandTerritory) and (p1 = pTurn) then
    begin
      IntServer(Command, p1, Subject, Data^);
{$IFDEF TEXTLOG}WriteLn(TextLog, 'AfterTurn - ExpandTerritory'); {$ENDIF}
    end
    else
    begin
      CL.State := FormerCLState;
      Break;
    end;
  end;
{$IFOPT O-}InvalidTreatyMap := 0; {$ENDIF}
end;

procedure StartNewGame(const Path, FileName, Map: string;
  Newlx, Newly, NewLandMass, NewMaxTurn: Integer);
var
  I: Integer;
begin
  Notify(ntStartDone);
  SavePath := Path;
  LogFileName := FileName;
  MapFileName := Map;
  {$IFDEF FastContact}
    lx := 24;
    ly := 42;
  {$ELSE}
    lx := Newlx;
    ly := Newly;
  {$ENDIF}
  MapSize := lx * ly;
  if MapFileName <> '' then
    LandMass := 0
  else
    LandMass := NewLandMass;
  MaxTurn := NewMaxTurn;
  DelphiRandomize;
  RND := DelphiRandSeed;
  Mode := moPlaying;
  CL := TCmdList.Create;
  StartGame;
  NoLogChanges;
  CallAllPlayers(cGetReady, nil^);
  LogChanges;
  CL.Put(sTurn, 0, 0, nil);
  BeforeTurn0;
  NoLogCityTileChanges;
  GenerateStat(pTurn);
  nLogOpened := -1;
  LastEndClientCommand := -1;
  CallPlayer(cShowGame, 0, nil^);
  for I := 0 to nPl - 1 do
    if Assigned(bix[I]) and (bix[I].Kind = btNetworkServer) then
      CallPlayer(cShowGame, I, nil^);
  Notify(ntBackOff);
  Inform(pTurn);
  ChangeClientWhenDone(cTurn, 0, nil^, 0)
end;

procedure DirectHelp(Command: Integer);
begin
  InitBrain(BrainTerm);
  BrainTerm.Client(Command, -1, nil^);
  AICredits := #0;
end;

procedure EditMap(const Map: string; Newlx, Newly, NewLandMass: Integer);
var
  p1, Loc1: Integer;
  Game: TNewGameData;
begin
  Notify(ntStartDone);
  Notify(ntInitLocalHuman);
  MapFileName := Map;
  lx := Newlx;
  ly := Newly;
  MapSize := lx * ly;
  LandMass := NewLandMass;
  bix[0] := BrainTerm;
  Difficulty[0] := 0;
  InitBrain(BrainTerm);

  DelphiRandomize;
  GAlive := 0;
  GWatching := 1;
  if not LoadMap(MapFileName) then
    for Loc1 := 0 to MapSize - 1 do
      RealMap[Loc1] := fOcean or ($F shl 27);
  CL := nil;
  InitMapEditor;
  RW[0].Data := nil;
  RW[0].BorderHelper := nil;
  RW[0].Alive := 0;
  Game.lx := lx;
  Game.ly := ly;
  Game.RO[0] := @RW[0];
  Game.Difficulty[0] := 0;
  for p1 := 1 to nPl - 1 do begin
    Game.RO[p1] := nil;
    Game.Difficulty[p1] := -1;
  end;
  BrainTerm.Client(cNewMap, -1, Game);

  DiscoverAll(0, lObserveSuper);
  Notify(ntEndInfo);
  bix[0].Client(cShowGame, 0, nil^);
  Notify(ntBackOff);
  ChangeClientWhenDone(cEditMap, 0, nil^, 0);
end;

procedure DestroySpacePort_TellPlayers(P, pCapturer: Integer);
var
  cix, I, p1: Integer;
  ShowShipChange: TShowShipChange;
begin
  // stop ship part production
  for cix := 0 to RW[P].nCity - 1 do
    with RW[P].City[cix] do
      if (Loc >= 0) and (Project and cpImp <> 0) and
        ((Project and cpIndex = woMIR) or
        (Imp[Project and cpIndex].Kind = ikShipPart)) then
      begin
        Inc(RW[P].Money, Prod0);
        Prod := 0;
        Prod0 := 0;
        Project := cpImp + imTrGoods;
        Project0 := cpImp + imTrGoods;
      end;

  // destroy ship
  with GShip[P] do
    if Parts[0] + Parts[1] + Parts[2] > 0 then
    begin
      for I := 0 to nShipPart - 1 do
      begin
        ShowShipChange.Ship1Change[I] := -Parts[I];
        if pCapturer >= 0 then
        begin
          ShowShipChange.Ship2Change[I] := Parts[I];
          Inc(GShip[pCapturer].Parts[I], Parts[I]);
        end;
        Parts[I] := 0;
      end;
      if Mode >= moMovie then
      begin
        if pCapturer >= 0 then
          ShowShipChange.Reason := scrCapture
        else
          ShowShipChange.Reason := scrDestruction;
        ShowShipChange.Ship1Owner := P;
        ShowShipChange.Ship2Owner := pCapturer;
        for p1 := 0 to nPl - 1 do
          if 1 shl p1 and (GAlive or GWatching) <> 0 then
          begin
            Move(GShip, RW[p1].Ship, SizeOf(GShip));
            if 1 shl p1 and GWatching <> 0 then
              CallPlayer(cShowShipChange, p1, ShowShipChange);
          end;
      end;
    end;
end;

procedure DestroyCity_TellPlayers(P, cix: Integer; SaveUnits: Boolean);
begin
  if RW[P].City[cix].built[imSpacePort] > 0 then
    DestroySpacePort_TellPlayers(P, -1);
  DestroyCity(P, cix, SaveUnits);
end;

procedure ChangeCityOwner_TellPlayers(pOld, cixOld, pNew: Integer);
begin
  if RW[pOld].City[cixOld].built[imSpacePort] > 0 then
    if RW[pNew].NatBuilt[imSpacePort] > 0 then
      DestroySpacePort_TellPlayers(pOld, pNew)
    else
      DestroySpacePort_TellPlayers(pOld, -1);
  ChangeCityOwner(pOld, cixOld, pNew);
end;

procedure CheckWin(P: Integer);
var
  I: Integer;
  ShipComplete: Boolean;
begin
  ShipComplete := True;
  for I := 0 to nShipPart - 1 do
    if GShip[P].Parts[I] < ShipNeed[I] then
      ShipComplete := False;
  if ShipComplete then
    GWinner := GWinner or 1 shl P; // game won!
end;

procedure BeforeTurn;
var
  I, p1, uix, cix, V21, Loc1, Cost, Job0, nAlive, nAppliers, ad, OldLoc,
    SiegedTiles, nUpdateLoc: Integer;
  UpdateLoc: array [0 .. numax - 1] of Integer;
  Radius: TVicinity21Loc;
  ShowShipChange: TShowShipChange;
  TribeExtinct, JobDone, MirBuilt: Boolean;
begin
{$IFOPT O-}Assert(1 shl pTurn and InvalidTreatyMap = 0); {$ENDIF}
  Assert(1 shl pTurn and (GAlive or GWatching) <> 0);
  if (1 shl pTurn and GAlive = 0) and (Difficulty[pTurn] > 0) then
    Exit;

  if (GWonder[woGrLibrary].EffectiveOwner = pTurn) and (GWinner = 0) then
  begin // check great library effect
    nAlive := 0;
    for p1 := 0 to nPl - 1 do
      if 1 shl p1 and GAlive <> 0 then
        Inc(nAlive);
    ad := 0;
    while ad <= (nAdv - 5) do begin
      if RW[pTurn].Tech[ad] < tsSeen then
      begin
        nAppliers := 0;
        for p1 := 0 to nPl - 1 do
          if (p1 <> pTurn) and (1 shl p1 and GAlive <> 0) and
            (RW[p1].Tech[ad] >= tsApplicable) then
            Inc(nAppliers);
        if nAppliers * 2 > nAlive then
        begin
          SeeTech(pTurn, ad);
          Inc(nTech[pTurn]);
          if Mode >= moMovie then
            CallPlayer(cShowGreatLibTech, pTurn, ad);
          // do not call CallPlayer(pTurn) while map is invalid
        end;
      end;
      Inc(ad);
    end;
  end;

  MaskD(ObserveLevel, MapSize, not Cardinal(3 shl (2 * pTurn)));
  if Mode > moLoading_Fast then
    MaskD(RW[pTurn].Map^, MapSize, Cardinal(not Cardinal(fUnit or fHiddenUnit or
      fStealthUnit or fObserved or fSpiedOut or fOwned or fOwnZoCUnit or
      fInEnemyZoC)));
  RW[pTurn].nEnemyUn := 0;

  MirBuilt := False;
  if (Difficulty[pTurn] > 0) and (GWinner = 0) then
    with RW[pTurn] do
    begin
      if nCity > 0 then
        for p1 := 0 to nPl - 1 do
          if GTurn = EvaStart[p1] + PeaceEvaTurns then
          begin // peace contract -- remove all units from p1's territory
            Loc1 := City[0].Loc; // search destination for homeless units
            for cix := 1 to nCity - 1 do
              if (City[cix].Loc >= 0) and
                ((Loc1 < 0) or (City[cix].built[imPalace] > 0)) then
                Loc1 := City[cix].Loc;
            for uix := 0 to nUn - 1 do
              with Un[uix] do
                if (Loc >= 0) and (Model[mix].Kind <> mkDiplomat) and
                  ((Home >= 0) or (Loc1 >= 0)) and
                  (RealMap[Loc] shr 27 = Cardinal(p1)) then
                begin
                  OldLoc := Loc;
                  if Master >= 0 then
                  begin // transport unload
                    if Model[mix].Domain = dAir then
                      Dec(Un[Master].AirLoad)
                    else
                      Dec(Un[Master].TroopLoad);
                    Master := -1;
                  end
                  else
                    FreeUnit(pTurn, uix);

                  if Home >= 0 then
                    Loc := City[Home].Loc
                  else
                    Loc := Loc1;
                  PlaceUnit(pTurn, uix);
                  UpdateUnitMap(OldLoc);
                  UpdateUnitMap(Loc);
                  Flags := Flags or unWithdrawn;
                  Happened := Happened or phPeaceEvacuation;
                end;
          end;

      if Mode >= moMovie then
        FillChar(ShowShipChange, SizeOf(ShowShipChange), 0);
      TribeExtinct := True;
      nUpdateLoc := 0;
      for cix := 0 to nCity - 1 do
        with City[cix] do
          if Loc >= 0 then
          begin { next turn for all cities - city loop 1 }
            // if ServerVersion[pTurn]>=$000EF0 then
            // Flags:=Flags and (chFounded or chCaptured or chProductionSabotaged or chDisorder)
            // else Flags:=Flags and (chCaptured or chProductionSabotaged or chDisorder);
            // check for siege
            SiegedTiles := 0;
            V21_to_Loc(Loc, Radius);
            for V21 := 1 to 26 do
              if Tiles and (1 shl V21) and not(1 shl CityOwnTile) <> 0 then
              begin
                Loc1 := Radius[V21];
                Assert((Loc1 >= 0) and (Loc1 < MapSize) and
                  (UsedByCity[Loc1] = Loc));
                p1 := RealMap[Loc1] shr 27;
                if (RealMap[Loc1] and fCity <> 0) or (p1 < nPl) and
                  (p1 <> pTurn) and (RW[pTurn].Treaty[p1] >= trPeace) or
                  (ZoCMap[Loc1] > 0) and (Occupant[Loc1] <> pTurn) and
                  (Treaty[Occupant[Loc1]] < trPeace) then
                begin
                  Tiles := Tiles and not(1 shl V21);
                  UsedByCity[Loc1] := -1;
                  Flags := Flags or chSiege;
                  Inc(SiegedTiles);
                end;
              end;
            while SiegedTiles > 0 do // replace sieged tiles
            begin
              if not AddBestCityTile(pTurn, cix) then
                Break;
              Dec(SiegedTiles);
            end;

            if Flags and chFounded = 0 then
            begin
              // CollectCityResources(pTurn,cix); // old style

              if CityTurn(pTurn, cix) then
                TribeExtinct := False
              else
              begin // city is erased
                RemoveDomainUnits(dSea, pTurn, Loc);
                RemoveDomainUnits(dAir, pTurn, Loc);
                Map[Loc] := Map[Loc] and not fCity; // !!! do this in inner core
                UpdateLoc[nUpdateLoc] := Loc;
                Inc(nUpdateLoc);
                DestroyCity_TellPlayers(pTurn, cix, True);
              end;

              if (Flags and chProduction <> 0) and (Project0 and cpImp <> 0)
              then
              begin
                if Project0 and cpIndex = woMIR then // MIR completed
                  MirBuilt := True
                else if Project0 and cpIndex = woManhattan then
                  GColdWarStart := GTurn
                else if Imp[Project0 and cpIndex].Kind = ikShipPart
                then { ship parts produced }
                  Inc(ShowShipChange.Ship1Change[Project0 and cpIndex -
                    imShipComp]);
              end;
            end;
          end; { city loop 1 }
      if nUpdateLoc > 0 then
      begin
        CheckBorders(-1, pTurn);
        for I := 0 to nUpdateLoc - 1 do
          UpdateUnitMap(UpdateLoc[I], True);
        if Mode >= moMovie then
          for p1 := 0 to nPl - 1 do
            if (1 shl p1 and GWatching <> 0) and (p1 <> pTurn) then
              for I := 0 to nUpdateLoc - 1 do
                if ObserveLevel[UpdateLoc[I]] shr (2 * p1) and 3 >= lObserveUnhidden
                then
                  CallPlayer(cShowCityChanged, p1, UpdateLoc[I]);
      end;

      for uix := 0 to nUn - 1 do
        with Un[uix] do
          if Loc >= 0 then
          begin // unit loop 2
            if Health < 100 then
              Recover(pTurn, uix);

            if Flags and unMountainDelay <> 0 then
            begin
              Movement := 0;
              Flags := Flags and not unMountainDelay;
            end
            else
              Movement := UnitSpeed(pTurn, mix, Health); { refresh movement }

            Assert(Loc >= 0);
            if Model[mix].Kind <> mkDiplomat then
            begin // check treaty violation
              p1 := RealMap[Loc] shr 27;
              if (p1 < nPl) and (p1 <> pTurn) and (Treaty[p1] >= trPeace) then
              begin
                if (Job in [jCity, jPillage, jClear, jAfforest, jTrans]) or
                  (Job in [jIrr, jMine, jFort, jBase]) and
                  (RealMap[Loc] and fTerImp <> 0) then
                  Job := jNone;
                if (GTurn > EvaStart[p1] + PeaceEvaTurns) and
                  (Treaty[p1] <> trAlliance) then
                begin
                  EvaStart[p1] := GTurn;
                  Happened := Happened or phPeaceViolation;
                  if Mode >= moMovie then
                    CallPlayer(cShowPeaceViolation, p1, pTurn);
                end;
              end;
            end;

            if ServerVersion[pTurn] >= $000EF0 then
            begin
              if (Health <= 0) or TribeExtinct then
                RemoveUnit_UpdateMap(pTurn, uix);
            end;
          end;

      if ServerVersion[pTurn] < $000EF0 then
        for uix := 0 to nUn - 1 do
          with Un[uix] do
            if Loc >= 0 then
            begin // unit loop 3
              Loc1 := Loc;
              Job0 := Job;
              if Job <> jNone then
                JobDone := Work(pTurn, uix);
              { settlers do terrain improvement jobs }
              if (Health <= 0) or TribeExtinct then
                RemoveUnit_UpdateMap(pTurn, uix);

              if (Job0 = jCity) and JobDone then // new city
              begin
                AddBestCityTile(pTurn, RW[pTurn].nCity - 1);
                UpdateUnitMap(Loc1, True);
                if Mode >= moMovie then // tell enemies
                  for p1 := 0 to nPl - 1 do
                    if (1 shl p1 and GWatching <> 0) and (p1 <> pTurn) and
                      (ObserveLevel[Loc1] and (3 shl (2 * p1)) > 0) then
                      CallPlayer(cShowCityChanged, p1, Loc1);
              end;
            end;

      { pollution - city loop 3 }
      for cix := 0 to nCity - 1 do
        with City[cix] do
          if (Loc >= 0) and (Pollution >= MaxPollution) then
            Pollute(pTurn, cix);

      CompactLists(pTurn);
      if (nUn = 0) and (nCity = 0) then
      begin // nation made extinct
        Happened := Happened or phExtinct;
        GAlive := GAlive and not(1 shl pTurn);
        Stat[stPop, pTurn, GTurn] := 0;
        Stat[stMil, pTurn, GTurn] := 0;
        Stat[stScience, pTurn, GTurn] := 0;
        Stat[stExplore, pTurn, GTurn] := 0;
        Stat[stTerritory, pTurn, GTurn] := 0;
        Stat[stWork, pTurn, GTurn] := 0;
        for p1 := 0 to nPl - 1 do
          if 1 shl p1 and (GAlive or GWatching) <> 0 then
          begin
            if p1 <> pTurn then
            begin
              GiveCivilReport(p1, pTurn);
              if (GTestFlags and tfUncover <> 0) or (Difficulty[p1] = 0) or
                (RW[p1].Treaty[pTurn] = trAlliance) then
                GiveMilReport(p1, pTurn);
            end;
            with RW[p1] do
            begin
              Alive := GAlive;
              for Loc1 := 0 to MapSize - 1 do
                if Territory[Loc1] = pTurn then
                // remove territory of extinct nation from player maps
                begin
                  Territory[Loc1] := -1;
                  Map[Loc1] := Map[Loc1] and not fPeace;
                end;
            end;
          end;
        Exit;
      end;

      // check research
      Cost := TechCost(pTurn);
      if GTestFlags and tfImmAdvance <> 0 then
        Research := Cost;
      if (Happened and phTech = 0) and (Research >= Cost) then
      begin
        if ResearchTech = adMilitary then
          EnableDevModel(pTurn) { new Unit class initiated }
        else if ResearchTech >= 0 then
          DiscoverTech(pTurn, ResearchTech);

        Dec(Research, Cost);
        Happened := Happened or phTech;
        ResearchTech := -1;
      end
      else if (ResearchTech = -2) and (nCity > 0) then
      begin
        Happened := Happened or phTech;
        ResearchTech := -1;
      end;

      if Credibility < MaxCredibility then
        for p1 := 0 to nPl - 1 do
          if (p1 <> pTurn) and (1 shl p1 and GAlive <> 0) and
            (Treaty[p1] >= trPeace) then
          begin
            Inc(Credibility);
            Break;
          end;

      if GWinner = 0 then
        CheckWin(pTurn);
      if (Mode >= moMovie) and (GWinner = 0) and
        ((ShowShipChange.Ship1Change[0] > 0) or
        (ShowShipChange.Ship1Change[1] > 0) or
        (ShowShipChange.Ship1Change[2] > 0)) then
      begin
        ShowShipChange.Reason := scrProduction;
        ShowShipChange.Ship1Owner := pTurn;
        ShowShipChange.Ship2Owner := -1;
        for p1 := 0 to nPl - 1 do
          if (p1 <> pTurn) and (1 shl p1 and (GAlive or GWatching) <> 0) then
          begin
            Move(GShip, RW[p1].Ship, SizeOf(GShip));
            if 1 shl p1 and GWatching <> 0 then
              CallPlayer(cShowShipChange, p1, ShowShipChange);
          end;
      end;
      if WinOnAlone and (GAlive and not(1 shl pTurn or 1) = 0) then
        GWinner := 1 shl pTurn; // break if only one nation left

      if GTurn = AnarchyStart + AnarchyTurns then
      begin
        AnarchyStart := -AnarchyTurns - 1;
        Government := gDespotism;
        for p1 := 0 to nPl - 1 do
          if (p1 <> pTurn) and ((GAlive or GWatching) and (1 shl p1) <> 0) then
            RW[p1].EnemyReport[pTurn].Government := gDespotism;
        Inc(Happened, phChangeGov);
      end;
    end; // if Difficulty[pTurn]>0

  if (pTurn = 0) and (GWinner > 0) then
  begin // game over, give world map and all reports to player 0
    DiscoverAll(pTurn, lObserveSuper);
    for p1 := 1 to nPl - 1 do
      if 1 shl p1 and GAlive <> 0 then
      begin
        if RW[pTurn].Treaty[p1] < trNone then
        begin
          RW[pTurn].Treaty[p1] := trNone;
          RW[p1].Treaty[pTurn] := trNone;
        end;
        GiveCivilReport(pTurn, p1);
        GiveMilReport(pTurn, p1);
      end;
  end
  else
  begin
    // show observed areas
    if (GTestFlags and tfUncover <> 0) or (Difficulty[pTurn] = 0)
    then { supervisor - all tiles visible }
    begin
      if (bix[pTurn].Kind <> btNoTerm) and
        ((Difficulty[pTurn] > 0) or (Mode > moLoading_Fast)) then
        DiscoverAll(pTurn, lObserveSuper);
    end
    else
    begin
      DiscoverViewAreas(pTurn);
      if MirBuilt then
        DiscoverAll(pTurn, lObserveUnhidden);
    end;
  end;
  // CheckContact;
end;

procedure AfterTurn;
var
  cix, uix, p1, Loc1, Job0: Integer;
  JobDone: Boolean;
begin
  with RW[pTurn] do
  begin
    for cix := 0 to nCity - 1 do
      if City[cix].Loc >= 0 then
      begin
        // City[cix].Flags:=City[cix].Flags and not chProductionSabotaged;
        City[cix].Flags := City[cix].Flags and (chCaptured or chDisorder);
        CollectCityResources(pTurn, cix); // new style
      end;

    Inc(Money, OracleIncome);
    OracleIncome := 0;
    if GWonder[woOracle].EffectiveOwner = pTurn then
    begin
      for p1 := 0 to nPl - 1 do
        if (1 shl p1 and GAlive <> 0) and
          ((p1 = pTurn) or (RW[pTurn].Treaty[p1] > trNoContact)) then
          for cix := 0 to RW[p1].nCity - 1 do
            if (RW[p1].City[cix].Loc >= 0) and
              (RW[p1].City[cix].built[imTemple] > 0) then
              Inc(OracleIncome);
    end;

    if (GTestFlags and tfImmImprove = 0) and (Government <> gAnarchy) then
      for cix := 0 to nCity - 1 do
        if (City[cix].Loc >= 0) and (City[cix].Flags and chCaptured = 0) then
          PayCityMaintenance(pTurn, cix);

    if ServerVersion[pTurn] >= $000EF0 then
    begin // let settlers work
      for cix := 0 to nCity - 1 do
        City[cix].Flags := City[cix].Flags and not chFounded;
      for uix := 0 to nUn - 1 do
        with Un[uix] do
          if Loc >= 0 then
          begin
            Loc1 := Loc;
            Job0 := Job;
            if Job <> jNone then
              JobDone := Work(pTurn, uix);
            { settlers do terrain improvement jobs }
            if Health <= 0 then
              RemoveUnit_UpdateMap(pTurn, uix);

            if (Job0 = jCity) and JobDone then // new city
            begin
              AddBestCityTile(pTurn, RW[pTurn].nCity - 1);
              UpdateUnitMap(Loc1, True);
              if Mode >= moMovie then // tell enemies
                for p1 := 0 to nPl - 1 do
                  if (1 shl p1 and GWatching <> 0) and (p1 <> pTurn) and
                    (ObserveLevel[Loc1] and (3 shl (2 * p1)) > 0) then
                    CallPlayer(cShowCityChanged, p1, Loc1);
            end;
          end;
    end;

    for uix := 0 to nUn - 1 do
      with Un[uix] do
        if Loc >= 0 then
        begin { next turn for all units }
          if Model[mix].Domain = dAir then
            if (Master >= 0) or (RealMap[Loc] and fCity <> 0) or
              (RealMap[Loc] and fTerImp = tiBase) then
            begin
              Fuel := Model[mix].Cap[mcFuel];
              Flags := Flags or unBombsLoaded;
            end
            else if Model[mix].Kind = mkSpecial_Glider then { glider }
            begin
              if RealMap[Loc] and fTerrain < fGrass then
              begin
                RemoveUnit_UpdateMap(pTurn, uix); // unit lost
                Happened := Happened or phGliderLost;
              end;
            end
            else
            begin
              Dec(Fuel);
              if Fuel < 0 then
              begin
                RemoveUnit_UpdateMap(pTurn, uix); // unit lost
                Happened := Happened or phPlaneLost;
              end
            end
          else if (Master < 0) and (Movement > 0) then // check HostileDamage
          begin
            Health := Health - HostileDamage(pTurn, mix, Loc, Movement);
            if Health < 0 then
              RemoveUnit_UpdateMap(pTurn, uix);
          end;
        end; { unit loop 1 }

    for uix := 0 to nUn - 1 do
      with Un[uix] do
      begin
        Flags := Flags and not unWithdrawn;
        if (Loc >= 0) and (Model[mix].Domain = dGround) and (Master < 0) and
          ((Integer(Movement) = Model[mix].Speed) or
          (Model[mix].Cap[mcAcademy] > 0) and (Movement * 2 >= Model[mix].Speed))
        then
          Flags := Flags or unFortified; // fortify unmoved units
      end;

    if (GTestFlags and tfUncover = 0) and (Difficulty[pTurn] > 0) then
    begin // restrict view area to current positions
      MaskD(ObserveLevel, MapSize, not Cardinal(3 shl (2 * pTurn)));
      if Mode > moLoading_Fast then
        MaskD(RW[pTurn].Map^, MapSize, Cardinal(not Cardinal(fUnit or fHiddenUnit or
          fStealthUnit or fObserved or fSpiedOut or fOwned or fOwnZoCUnit or
          fInEnemyZoC)));
      RW[pTurn].nEnemyUn := 0;
      DiscoverViewAreas(pTurn);
    end;

    if GWinner = 0 then
      for p1 := 0 to nPl - 1 do
        if 1 shl p1 and GAlive <> 0 then
          CheckWin(p1);
  end;
end;

procedure NextPlayer;
begin
  if GTurn = 0 then
    BeforeTurn0
  else
    BeforeTurn;
  NoLogCityTileChanges;
  GenerateStat(pTurn);
  Inform(pTurn);
  ChangeClient;
end;

function ExecuteMove(P, uix, ToLoc: Integer; var MoveInfo: TMoveInfo;
  ShowMove: TShowMove): Integer;
var
  I, p1, FromLoc, uix1, nUpdateLoc: Integer;
  MinLevel, MissionResult: Cardinal;
  PModel: ^TModel;
  UpdateLoc: array [0 .. numax - 1] of Integer;
  SeeFrom, SeeTo, ExtDiscover: Boolean;
begin
  Result := 0;
  with RW[P], Un[uix] do
  begin
    PModel := @Model[mix];
    FromLoc := Loc;

    if Master < 0 then
      FreeUnit(P, uix);
    if (MoveInfo.MoveType in [mtMove, mtCapture]) and MoveInfo.MountainDelay
    then
    begin
      Flags := Flags or unMountainDelay;
    end;
    Loc := -2;
    if TroopLoad + AirLoad > 0 then
      for I := 0 to nUn - 1 do
        if (Un[I].Loc >= 0) and (Un[I].Master = uix) then
          Un[I].Loc := -2;
    UpdateUnitMap(FromLoc);

    if Mode >= moMovie then { show move in interface modules }
    begin
      ShowMove.EndHealth := MoveInfo.EndHealth;
      ShowMove.EndHealthDef := -1;
      if Master >= 0 then
        if Model[Un[Master].mix].Domain = dAir then
          ShowMove.Flags := ShowMove.Flags or umPlaneUnloading
        else
          ShowMove.Flags := ShowMove.Flags or umShipUnloading;
      if MoveInfo.ToMaster >= 0 then
        if Model[Un[MoveInfo.ToMaster].mix].Domain = dAir then
          ShowMove.Flags := ShowMove.Flags or umPlaneLoading
        else
          ShowMove.Flags := ShowMove.Flags or umShipLoading;
      for p1 := 0 to nPl - 1 do
        if (1 shl p1 and GWatching <> 0) and ((p1 <> P) or (bix[p1].Kind = btTerm))
        then
        begin
          if PModel.Cap[mcStealth] > 0 then
            MinLevel := lObserveSuper
          else if PModel.Cap[mcSub] > 0 then
            MinLevel := lObserveAll
          else
            MinLevel := lObserveUnhidden;
          SeeFrom := (p1 = P) or (ObserveLevel[FromLoc] shr (2 * p1) and
            3 >= MinLevel);
          SeeTo := (p1 = P) or (ObserveLevel[ToLoc] shr (2 * p1) and
            3 >= MinLevel);
          if SeeFrom and SeeTo then
          begin
            TellAboutModel(p1, P, mix);
            if p1 = P then
              ShowMove.emix := -1
            else
              ShowMove.emix := emixSafe(p1, P, mix);
            if MoveInfo.MoveType = mtCapture then
              CallPlayer(cShowCapturing, p1, ShowMove)
            else
              CallPlayer(cShowMoving, p1, ShowMove);
          end
          else if SeeFrom then
            CallPlayer(cShowUnitChanged, p1, FromLoc);
        end;
    end;

    if MoveInfo.MoveType <> mtSpyMission then
      Loc := ToLoc;
    if TroopLoad + AirLoad > 0 then
      for I := 0 to nUn - 1 do
        if Un[I].Loc = -2 then
          Un[I].Loc := ToLoc;

    ExtDiscover := False;
    nUpdateLoc := 0;
    if MoveInfo.MoveType = mtCapture then
    begin
      Assert(Occupant[ToLoc] < 0);
      for uix1 := 0 to RW[MoveInfo.Defender].nUn - 1 do
        with RW[MoveInfo.Defender].Un[uix1] do
          if (Loc >= 0) and (Home = MoveInfo.Dcix) then
          begin
            UpdateLoc[nUpdateLoc] := Loc;
            Inc(nUpdateLoc);
          end;
      // unit will be removed -- remember position and update for all players

      if (RW[MoveInfo.Defender].City[MoveInfo.Dcix].Size > 2) and (nCity < ncmax)
      then
      begin // city captured
        ChangeCityOwner_TellPlayers(MoveInfo.Defender, MoveInfo.Dcix, P);
        City[nCity - 1].Flags := CaptureTurns shl 16;
        CityShrink(P, nCity - 1);
        if Mode = moPlaying then
          with RW[P].City[nCity - 1] do
          begin
            // SavedResourceWeights[nCity-1]:=ResourceWeights;
            SavedTiles[nCity - 1] := Tiles;
          end;
        ExtDiscover := True;

        // Temple of Zeus effect
        if GWonder[woZeus].EffectiveOwner = P then
        begin
          GiveCivilReport(P, MoveInfo.Defender);
          for I := 0 to nAdv - 1 do
            if not(I in FutureTech) and (RW[P].Tech[I] < tsSeen) and
              (RW[MoveInfo.Defender].Tech[I] >= tsApplicable) then
            begin
              Happened := Happened or phStealTech;
              GStealFrom := MoveInfo.Defender;
              Break;
            end;
        end;
        if Mode = moPlaying then
          LogCheckBorders(P, nCity - 1, MoveInfo.Defender);
{$IFOPT O-} if Mode < moPlaying then
          InvalidTreatyMap := not(1 shl P); {$ENDIF}
        // territory should not be considered for the rest of the command
        // execution, because during loading a game it's incorrect before
        // subsequent sIntExpandTerritory is processed
      end
      else // city destroyed
      begin
        DestroyCity_TellPlayers(MoveInfo.Defender, MoveInfo.Dcix, False);
        CheckBorders(ToLoc, MoveInfo.Defender);
      end;
      RecalcPeaceMap(P);
      if Mode >= moMovie then
        Move(GWonder, Wonder, SizeOf(GWonder));
    end; { if MoveInfo.MoveType=mtCapture }

    if MoveInfo.MoveType = mtSpyMission then
    begin
      MissionResult := DoSpyMission(P, MoveInfo.Defender, MoveInfo.Dcix,
        SpyMission);
      if (Mode = moPlaying) and (SpyMission = smStealForeignReports) then
        CallPlayer(cShowMissionResult, P, MissionResult);
    end;

    Health := MoveInfo.EndHealth;
    Dec(Movement, MoveInfo.Cost);
    // transport unload
    if Master >= 0 then
    begin
      if PModel.Domain = dAir then
        Dec(Un[Master].AirLoad)
      else
      begin
        Dec(Un[Master].TroopLoad);
        Assert(Movement <= 0);
      end;
      Master := -1;
    end;

    if (Health <= 0) or (MoveInfo.MoveType = mtSpyMission) then
      RemoveUnit(P, uix) // spy mission or victim of HostileDamage
    else
    begin // transport load
      Master := MoveInfo.ToMaster;
      if MoveInfo.ToMaster >= 0 then
      begin
        if PModel.Domain = dAir then
          Inc(Un[MoveInfo.ToMaster].AirLoad)
        else
          Inc(Un[MoveInfo.ToMaster].TroopLoad);
      end
      else
        PlaceUnit(P, uix);
    end;

    if (MoveInfo.MoveType = mtCapture) and (nUpdateLoc > 0) then
      RecalcMapZoC(P);
    UpdateUnitMap(ToLoc, MoveInfo.MoveType = mtCapture);
    for I := 0 to nUpdateLoc - 1 do
      UpdateUnitMap(UpdateLoc[I]);
    // tell about lost units of defender

    if (MoveInfo.MoveType <> mtSpyMission) and (Master < 0) then
    begin
      if (PModel.Kind = mkDiplomat) or (PModel.Domain = dAir) or
        (PModel.Cap[mcRadar] + PModel.Cap[mcCarrier] + PModel.Cap[mcAcademy] >
        0) or (RealMap[ToLoc] and fTerrain = fMountains) or
        (RealMap[ToLoc] and fTerImp = tiFort) or
        (RealMap[ToLoc] and fTerImp = tiBase) then
        ExtDiscover := True;
      if (PModel.Kind = mkDiplomat) or (PModel.Cap[mcSpy] > 0) then
        I := lObserveSuper
      else if (PModel.Domain = dAir) or
        (PModel.Cap[mcRadar] + PModel.Cap[mcCarrier] > 0) then
        I := lObserveAll
      else
        I := lObserveUnhidden;
      if ExtDiscover then
      begin
        if Discover21(ToLoc, P, I, True, PModel.Domain = dGround) then
          Result := Result or rEnemySpotted;
      end
      else
      begin
        if Discover9(ToLoc, P, I, True, PModel.Domain = dGround) then
          Result := Result or rEnemySpotted;
      end;
    end;

    if Mode >= moMovie then { show after-move in interface modules }
      for p1 := 0 to nPl - 1 do
        if (1 shl p1 and GWatching <> 0) and ((p1 <> P) or (bix[p1].Kind = btTerm))
        then
        begin
          if PModel.Cap[mcStealth] > 0 then
            MinLevel := lObserveSuper
          else if PModel.Cap[mcSub] > 0 then
            MinLevel := lObserveAll
          else
            MinLevel := lObserveUnhidden;
          SeeFrom := (p1 = P) or (ObserveLevel[FromLoc] shr (2 * p1) and
            3 >= MinLevel);
          SeeTo := (p1 = P) or (ObserveLevel[ToLoc] shr (2 * p1) and
            3 >= MinLevel);
          if SeeTo and (MoveInfo.MoveType = mtCapture) then
            CallPlayer(cShowCityChanged, p1, ToLoc);
          if SeeFrom and SeeTo then
            CallPlayer(cShowAfterMove, p1, ToLoc)
          else if (MoveInfo.MoveType <> mtSpyMission) and SeeTo then
            CallPlayer(cShowUnitChanged, p1, ToLoc);
          for I := 0 to nUpdateLoc - 1 do
            if ObserveLevel[UpdateLoc[I]] shr (2 * p1) and 3 >= lObserveUnhidden
            then
              CallPlayer(cShowUnitChanged, p1, UpdateLoc[I]);
        end;
  end;
end;

function ExecuteAttack(P, uix, ToLoc: Integer; var MoveInfo: TMoveInfo;
  ShowMove: TShowMove): Integer;

  procedure WriteBattleHistory(ToLoc, FromLoc, Attacker, Defender, mixAttacker,
    mixDefender: Integer; AttackerLost, DefenderLost: Boolean);
  var
    AttackerBattle, DefenderBattle: ^TBattle;
  begin
    with RW[Attacker] do
    begin
      if nBattleHistory = 0 then
        ReallocMem(BattleHistory, 16 * SizeOf(TBattle))
      else if (nBattleHistory >= 16) and
        (nBattleHistory and (nBattleHistory - 1) = 0) then
        ReallocMem(BattleHistory, nBattleHistory * (2 * SizeOf(TBattle)));
      AttackerBattle := @BattleHistory[nBattleHistory];
      Inc(nBattleHistory);
    end;
    with RW[Defender] do
    begin
      if nBattleHistory = 0 then
        ReallocMem(BattleHistory, 16 * SizeOf(TBattle))
      else if (nBattleHistory >= 16) and
        (nBattleHistory and (nBattleHistory - 1) = 0) then
        ReallocMem(BattleHistory, nBattleHistory * (2 * SizeOf(TBattle)));
      DefenderBattle := @BattleHistory[nBattleHistory];
      Inc(nBattleHistory);
    end;
    AttackerBattle.Enemy := Defender;
    AttackerBattle.Flags := 0;
    AttackerBattle.Turn := GTurn;
    AttackerBattle.mix := mixAttacker;
    AttackerBattle.mixEnemy := mixDefender;
    AttackerBattle.ToLoc := ToLoc;
    AttackerBattle.FromLoc := FromLoc;
    DefenderBattle.Enemy := Attacker;
    DefenderBattle.Flags := bhEnemyAttack;
    DefenderBattle.Turn := GTurn;
    DefenderBattle.mix := mixDefender;
    DefenderBattle.mixEnemy := mixAttacker;
    DefenderBattle.ToLoc := ToLoc;
    DefenderBattle.FromLoc := FromLoc;
    if AttackerLost then
    begin
      AttackerBattle.Flags := AttackerBattle.Flags or bhMyUnitLost;
      DefenderBattle.Flags := DefenderBattle.Flags or bhEnemyUnitLost;
    end;
    if DefenderLost then
    begin
      AttackerBattle.Flags := AttackerBattle.Flags or bhEnemyUnitLost;
      DefenderBattle.Flags := DefenderBattle.Flags or bhMyUnitLost;
    end;
  end;

var
  I, p1, FromLoc, uix1, nUpdateLoc, ExpGain, ExpelToLoc, cix1: Integer;
  PModel: ^TModel;
  UpdateLoc: array [0 .. numax - 1] of Integer;
  LoseCityPop, CityDestroyed, SeeFrom, SeeTo, ZoCDefenderDestroyed: Boolean;
begin
  Result := 0;
  with RW[P].Un[uix] do
  begin
    PModel := @RW[P].Model[mix];
    FromLoc := Loc;

    ShowMove.EndHealth := MoveInfo.EndHealth;
    ShowMove.EndHealthDef := MoveInfo.EndHealthDef;
    if MoveInfo.MoveType = mtAttack then
      WriteBattleHistory(ToLoc, FromLoc, P, MoveInfo.Defender, mix,
        RW[MoveInfo.Defender].Un[MoveInfo.Duix].mix, MoveInfo.EndHealth <= 0,
        MoveInfo.EndHealthDef <= 0);

    { if RW[p].Treaty[MoveInfo.Defender]=trCeaseFire then
      begin
      if Mode>=moMovie then
      CallPlayer(cShowCancelTreaty,MoveInfo.Defender,P);
      CancelTreaty(P,MoveInfo.Defender)
      end; }
    if Mode >= moMovie then { show attack in interface modules }
      for p1 := 0 to nPl - 1 do
        if (1 shl p1 and GWatching <> 0) and ((p1 <> P) or (bix[p1].Kind = btTerm))
        then
        begin
          SeeFrom := ObserveLevel[FromLoc] shr (2 * p1) and
            3 >= lObserveUnhidden;
          SeeTo := ObserveLevel[ToLoc] shr (2 * p1) and 3 >= lObserveUnhidden;
          if SeeFrom and SeeTo then
          begin
            TellAboutModel(p1, P, mix);
            if p1 = P then
              ShowMove.emix := -1
            else
              ShowMove.emix := emixSafe(p1, P, mix);
            CallPlayer(cShowAttacking, p1, ShowMove);
          end;
        end;

    LoseCityPop := False;
    if (RealMap[ToLoc] and fCity <> 0) and
      ((MoveInfo.MoveType = mtAttack) and (MoveInfo.EndHealthDef <= 0) or
      (MoveInfo.MoveType = mtBombard) and (BombardmentDestroysCity or
      (RW[MoveInfo.Defender].City[MoveInfo.Dcix].Size > 2))) then
      case PModel.Domain of
        dGround:
          LoseCityPop := (PModel.Cap[mcArtillery] > 0) or
            (RW[MoveInfo.Defender].City[MoveInfo.Dcix].built[imWalls] = 0) and
            (Continent[ToLoc] <> GrWallContinent[MoveInfo.Defender]);
        dSea:
          LoseCityPop := RW[MoveInfo.Defender].City[MoveInfo.Dcix].built
            [imCoastalFort] = 0;
        dAir:
          LoseCityPop := RW[MoveInfo.Defender].City[MoveInfo.Dcix].built
            [imMissileBat] = 0;
      end;
    CityDestroyed := LoseCityPop and
      (RW[MoveInfo.Defender].City[MoveInfo.Dcix].Size <= 2);

    if MoveInfo.MoveType = mtBombard then
    begin
      Assert(Movement >= 100);
      if PModel.Attack = 0 then
        Flags := Flags and not unBombsLoaded;
      Dec(Movement, 100);
    end
    else if MoveInfo.MoveType = mtExpel then
    begin
      Assert(Movement >= 100);
      Job := jNone;
      Flags := Flags and not unFortified;
      Dec(Movement, 100);
    end
    else
    begin
      Assert(MoveInfo.MoveType = mtAttack);
      if MoveInfo.EndHealth = 0 then
        RemoveUnit(P, uix, MoveInfo.Defender) // destroy attacker
      else
      begin // update attacker
        ExpGain := (Health - MoveInfo.EndHealth + 1) shr 1;
        if Exp + ExpGain > (nExp - 1) * ExpCost then
          Exp := (nExp - 1) * ExpCost
        else
          Inc(Exp, ExpGain);
        Health := MoveInfo.EndHealth;
        Job := jNone;
        if RW[MoveInfo.Defender].Model[RW[MoveInfo.Defender].Un[MoveInfo.Duix]
          .mix].Domain < dAir then
          Flags := Flags and not unBombsLoaded;
        Flags := Flags and not unFortified;
        if Movement > 100 then
          Dec(Movement, 100)
        else
          Movement := 0;
      end;
    end;

    ZoCDefenderDestroyed := False;
    nUpdateLoc := 0;
    if MoveInfo.MoveType = mtExpel then
      with RW[MoveInfo.Defender], Un[MoveInfo.Duix] do
      begin // expel friendly unit
        if Home >= 0 then
          ExpelToLoc := City[Home].Loc
        else
        begin
          ExpelToLoc := City[0].Loc; // search destination for homeless units
          for cix1 := 1 to nCity - 1 do
            if (City[cix1].Loc >= 0) and
              ((ExpelToLoc < 0) or (City[cix1].built[imPalace] > 0)) then
              ExpelToLoc := City[cix1].Loc;
        end;
        if ExpelToLoc >= 0 then
        begin
          FreeUnit(MoveInfo.Defender, MoveInfo.Duix);
          Loc := ExpelToLoc;
          PlaceUnit(MoveInfo.Defender, MoveInfo.Duix);
          UpdateLoc[nUpdateLoc] := Loc;
          Inc(nUpdateLoc);
          Flags := Flags or unWithdrawn;
        end;
      end
    else if (MoveInfo.MoveType = mtAttack) and (MoveInfo.EndHealthDef > 0) then
      with RW[MoveInfo.Defender].Un[MoveInfo.Duix] do
      begin // update defender
        ExpGain := (Health - MoveInfo.EndHealthDef + 1) shr 1;
        if Exp + ExpGain > (nExp - 1) * ExpCost then
          Exp := (nExp - 1) * ExpCost
        else
          Inc(Exp, ExpGain);
        Health := MoveInfo.EndHealthDef;
      end
    else
    begin // destroy defenders
      if MoveInfo.MoveType <> mtBombard then
      begin
        ZoCDefenderDestroyed := RW[MoveInfo.Defender].Model
          [RW[MoveInfo.Defender].Un[MoveInfo.Duix].mix].Flags and mdZOC <> 0;
        if ((RealMap[ToLoc] and fCity = 0) and
          (RealMap[ToLoc] and fTerImp <> tiBase) and
          (RealMap[ToLoc] and fTerImp <> tiFort)) or LoseCityPop and
          (RW[MoveInfo.Defender].City[MoveInfo.Dcix].Size = 2) then
          RemoveAllUnits(MoveInfo.Defender, ToLoc, P)
          { no city, base or fortress }
        else
          RemoveUnit(MoveInfo.Defender, MoveInfo.Duix, P);
      end;

      if LoseCityPop then // city defender defeated -- shrink city
        if not CityDestroyed then
          CityShrink(MoveInfo.Defender, MoveInfo.Dcix)
        else
        begin
          for uix1 := 0 to RW[MoveInfo.Defender].nUn - 1 do
            with RW[MoveInfo.Defender].Un[uix1] do
              if (Loc >= 0) and (Home = MoveInfo.Dcix) then
              begin
                UpdateLoc[nUpdateLoc] := Loc;
                Inc(nUpdateLoc);
              end;
          // unit will be removed -- remember position and update for all players
          DestroyCity_TellPlayers(MoveInfo.Defender, MoveInfo.Dcix, False);
          CheckBorders(ToLoc, MoveInfo.Defender);
          RecalcPeaceMap(P);
        end;
    end;

    if CityDestroyed and (nUpdateLoc > 0) then
      RecalcMapZoC(P)
    else if ZoCDefenderDestroyed then
      RecalcV8ZoC(P, ToLoc);
    UpdateUnitMap(FromLoc);
    UpdateUnitMap(ToLoc, LoseCityPop);
    for I := 0 to nUpdateLoc - 1 do
      UpdateUnitMap(UpdateLoc[I]);
    // tell about lost units of defender

    if Mode >= moMovie then
    begin
      for I := 0 to RW[P].nEnemyModel - 1 do
        with RW[P].EnemyModel[I] do
          Lost := Destroyed[P, Owner, mix];
      for p1 := 0 to nPl - 1 do { show after-attack in interface modules }
        if (1 shl p1 and GWatching <> 0) and ((p1 <> P) or (bix[p1].Kind = btTerm))
        then
        begin
          SeeFrom := ObserveLevel[FromLoc] shr (2 * p1) and
            3 >= lObserveUnhidden;
          SeeTo := ObserveLevel[ToLoc] shr (2 * p1) and 3 >= lObserveUnhidden;
          if SeeTo and CityDestroyed then
            CallPlayer(cShowCityChanged, p1, ToLoc); // city was destroyed
          if SeeFrom and SeeTo then
          begin
            CallPlayer(cShowAfterAttack, p1, ToLoc);
            CallPlayer(cShowAfterAttack, p1, FromLoc);
          end
          else
          begin
            if SeeTo then
              CallPlayer(cShowUnitChanged, p1, ToLoc);
            if SeeFrom then
              CallPlayer(cShowUnitChanged, p1, FromLoc);
          end;
          if SeeTo and (MoveInfo.MoveType = mtExpel) and (ExpelToLoc >= 0) then
            CallPlayer(cShowUnitChanged, p1, ExpelToLoc);
        end;
    end;
  end;
end;

function MoveUnit(P, uix, dx, dy: Integer; TestOnly: Boolean): Integer;
var
  ToLoc: Integer;
  MoveInfo: TMoveInfo;
  ShowMove: TShowMove;
begin
{$IFOPT O-}Assert(1 shl P and InvalidTreatyMap = 0); {$ENDIF}
  with RW[P].Un[uix] do
  begin
    ToLoc := dLoc(Loc, dx, dy);
    if (ToLoc < 0) or (ToLoc >= MapSize) then
    begin
      Result := eInvalid;
      Exit;
    end;
    Result := CalculateMove(P, uix, ToLoc, 3 - dy and 1, TestOnly, MoveInfo);
    if Result = eZOC_EnemySpotted then
      ZOCTile := ToLoc;
    if (Result >= rExecuted) and not TestOnly then
    begin
      ShowMove.dx := dx;
      ShowMove.dy := dy;
      ShowMove.FromLoc := Loc;
      ShowMove.mix := mix;
      ShowMove.Health := Health;
      ShowMove.Fuel := Fuel;
      ShowMove.Exp := Exp;
      ShowMove.Load := TroopLoad + AirLoad;
      ShowMove.Owner := P;
      if (TroopLoad > 0) or (AirLoad > 0) then
        ShowMove.Flags := unMulti
      else
        ShowMove.Flags := 0;
      case MoveInfo.MoveType of
        mtCapture:
          ShowMove.Flags := ShowMove.Flags or umCapturing;
        mtSpyMission:
          ShowMove.Flags := ShowMove.Flags or umSpyMission;
        mtBombard:
          ShowMove.Flags := ShowMove.Flags or umBombarding;
        mtExpel:
          ShowMove.Flags := ShowMove.Flags or umExpelling;
      end;
      case MoveInfo.MoveType of
        mtMove, mtCapture, mtSpyMission:
          Result := ExecuteMove(P, uix, ToLoc, MoveInfo, ShowMove) or Result;
        mtAttack, mtBombard, mtExpel:
          Result := ExecuteAttack(P, uix, ToLoc, MoveInfo, ShowMove) or Result;
      end;
    end;
  end;
end;

function Server(Command, Player, Subject: Integer; var Data): Integer; stdcall;

  function CountPrice(const Offer: TOffer; PriceType: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Offer.nDeliver + Offer.nCost - 1 do
      if Offer.Price[I] and $FFFF0000 = Cardinal(PriceType) then
        Inc(Result);
  end;

{ procedure UpdateBorderHelper;
  var
  X, Y, Loc, Loc1, dx, dy, ObserveMask: Integer;
  begin
  ObserveMask:=3 shl (2*pTurn);
  for X:=0 to lx-1 do for Y:=0 to ly shr 1-1 do
  begin
  Loc:=lx*(Y*2)+X;
  if ObserveLevel[Loc] and ObserveMask<>0 then
  begin
  for dy:=0 to 1 do for dx:=0 to 1 do
  begin
  Loc1:=(Loc+dx-1+lx) mod lx +lx*((Y+dy)*2-1);
  if (Loc1>=0) and (Loc1<MapSize)
  and (ObserveLevel[Loc1] and ObserveMask<>0) then
  if RealMap[Loc1] and $78000000=RealMap[Loc] and $78000000 then
  begin
  RW[pTurn].BorderHelper[Loc]:=RW[pTurn].BorderHelper[Loc] and not (1 shl (dy*2+dx));
  RW[pTurn].BorderHelper[Loc1]:=RW[pTurn].BorderHelper[Loc1] and not (8 shr (dy*2+dx))
  end
  else
  begin
  RW[pTurn].BorderHelper[Loc]:=RW[pTurn].BorderHelper[Loc] or (1 shl (dy*2+dx));
  RW[pTurn].BorderHelper[Loc1]:=RW[pTurn].BorderHelper[Loc1] or (8 shr (dy*2+dx));
  end
  end
  end
  end
  end; }

const
  ptSelect = 0;
  ptTrGoods = 1;
  ptUn = 2;
  ptCaravan = 3;
  ptImp = 4;
  ptWonder = 6;
  ptShip = 7;
  ptInvalid = 8;

  function ProjectType(Project: Integer): Integer;
  begin
    if Project and cpCompleted <> 0 then
      Result := ptSelect
    else if Project and (cpImp + cpIndex) = cpImp + imTrGoods then
      Result := ptTrGoods
    else if Project and cpImp = 0 then
      if RW[Player].Model[Project and cpIndex].Kind = mkCaravan then
        Result := ptCaravan
      else
        Result := ptUn
    else if Project and cpIndex >= nImp then
      Result := ptInvalid
    else if Imp[Project and cpIndex].Kind = ikWonder then
      Result := ptWonder
    else if Imp[Project and cpIndex].Kind = ikShipPart then
      Result := ptShip
    else
      Result := ptImp;
  end;

var
  D, I, J, p1, p2, pt0, pt1, uix1, cix1, Loc0, Loc1, dx, dy, NewCap, MinCap,
    MaxCap, CapWeight, Cost, NextProd, Preq, TotalFood, TotalProd, CheckSum,
    StopTurn, FutureMCost, NewProject, OldImp, mix, V8, V21, AStr, DStr,
    ABaseDamage, DBaseDamage: Integer;
  CityReport: TCityReport;
  FormerCLState: TCmdListState;
  Adjacent: TVicinity8Loc;
  Radius: TVicinity21Loc;
  ShowShipChange: TShowShipChange;
  ShowNegoData: TShowNegoData;
  logged, ok, HasShipChanged, AllHumansDead, OfferFullySupported: Boolean;
begin
  if Command = sTurn then
  begin
    p2 := -1;
    for p1 := 0 to nPl - 1 do
      if (p1 <> Player) and (1 shl p1 and GWatching <> 0) then
        CallPlayer(cShowTurnChange, p1, p2);
  end;

  Assert(MapSize = lx * ly);
  Assert(Command and (sctMask or sExecute) <> sctInternal or sExecute);
  // not for internal commands
  if (Command < 0) or (Command >= $10000) then
  begin
    Result := eUnknown;
    Exit;
  end;

  if (Player < 0) or (Player >= nPl) or
    ((Command and (sctMask or sExecute) <> sctInfo) and
    ((Subject < 0) or (Subject >= $1000))) then
  begin
    Result := eInvalid;
    Exit;
  end;

  if (1 shl Player and (GAlive or GWatching) = 0) and
    not((Command = sTurn) or (Command = sBreak) or (Command = sResign) or
    (Command = sGetAIInfo) or (Command = sGetAICredits) or
    (Command = sGetVersion) or (Command and $FF0F = sGetChart)) then
  begin
    PutMessage(1 shl 16 + 1, Format('NOT Alive: %d', [Player]));
    Result := eNoTurn;
    Exit;
  end;

  Result := eOK;

  // check if command allowed now
  if (Mode = moPlaying) and not((Command >= cClientEx) or (Command = sMessage)
    or (Command = sSetDebugMap) or (Command = sGetDebugMap) or
    (Command = sGetAIInfo) or (Command = sGetAICredits) or
    (Command = sGetVersion) or (Command = sGetTechCost) or
    (Command = sGetDefender) or (Command = sGetUnitReport) or
    (Command = sGetCityReport) or (Command = sGetCityTileInfo) or
    (Command = sGetCity) or (Command = sGetEnemyCityReport) or
    (Command = sGetEnemyCityAreaInfo) or (Command = sGetCityReportNew) or
    (Command and $FF0F = sGetChart) or (Command and $FF0F = sSetAttitude))
  // commands always allowed
    and not((Player = pTurn) and (Command < $1000))
  // info request always allowed for pTurn
    and ((pDipActive < 0) and (Player <> pTurn) // not his turn
    or (pDipActive >= 0) and (Player <> pDipActive)
    // not active in negotiation mode
    or (pDipActive >= 0) and (Command and sctMask <> sctEndClient)) then
  // no nego command
  begin
    PutMessage(1 shl 16 + 1, Format('No Turn: %d calls %x',
      [Player, Command shr 4]));
    Result := eNoTurn;
    Exit;
  end;

  // do not use EXIT hereafter!

{$IFOPT O-}
  HandoverStack[nHandoverStack] := Player + $1000;
  HandoverStack[nHandoverStack + 1] := Command;
  Inc(nHandoverStack, 2);

  InvalidTreatyMap := 0;
  // new command, sIntExpandTerritory of previous command was processed
{$ENDIF}
  if (Mode = moPlaying) and (Command >= sExecute) and
    ((Command and sctMask <> sctEndClient) or (Command = sTurn)) and
    (Command and sctMask <> sctModel) and (Command <> sCancelTreaty) and
    (Command <> sSetCityTiles) and (Command <> sBuyCityProject) and
    ((Command < cClientEx) or ProcessClientData[Player]) then
  begin { log command }
    FormerCLState := CL.State;
    CL.Put(Command, Player, Subject, @Data);
    logged := True;
  end
  else
    logged := False;

  case Command of

    {
      Info Request Commands
      ____________________________________________________________________
    }
    sMessage:
      bix[0].Client(cDebugMessage, Subject, Data);

    sSetDebugMap:
      DebugMap[Player] := @Data;

    sGetDebugMap:
      Pointer(Data) := DebugMap[Subject];

    { sChangeSuperView:
      if Difficulty[Player]=0 then
      begin
      for I:=0 to nBrain-1 do if Brain[I].Initialized then
      CallClient(I, cShowSuperView, Subject)
      end
      else Result:=eInvalid; }

    sRefreshDebugMap:
      bix[0].Client(cRefreshDebugMap, -1, Player);

    sGetChart .. sGetChart + (nStat - 1) shl 4:
      if (Subject >= 0) and (Subject < nPl) and Assigned(bix[Subject]) then
      begin
        StopTurn := 0;
        if (Difficulty[Player] = 0) or (GTestFlags and tfUncover <> 0)
        // supervisor
          or (Subject = Player) // own chart
          or (GWinner > 0) // game end chart
          or (1 shl Subject and GAlive = 0) then // chart of extinct nation
          if Subject > Player then
            StopTurn := GTurn
          else
            StopTurn := GTurn + 1
        else if RW[Player].Treaty[Subject] > trNoContact then
          if Command shr 4 and $F = stMil then
            StopTurn := RW[Player].EnemyReport[Subject].TurnOfMilReport + 1
          else
            StopTurn := RW[Player].EnemyReport[Subject].TurnOfCivilReport + 1;
        Move(Stat[Command shr 4 and $F, Subject]^, Data,
          StopTurn * SizeOf(Integer));
        FillChar(TChart(Data)[StopTurn], (GTurn - StopTurn) *
          SizeOf(Integer), 0);
      end
      else
        Result := eInvalid;

    sGetTechCost:
      Integer(Data) := TechCost(Player);

    sGetAIInfo:
      if AIInfo[Subject] = '' then
        PChar(Data) := nil
      else
        PChar(Data) := @AIInfo[Subject][1];

    sGetAICredits:
      if AICredits = '' then
        PChar(Data) := nil
      else
        PChar(Data) := @AICredits[1];

    sGetVersion:
      Integer(Data) := CevoVersion;

    sGetGameChanged:
      if Player <> 0 then
        Result := eInvalid
      else if (CL <> nil) and (CL.State.nLog = nLogOpened) and
        (CL.State.MoveCode = 0) and not HasCityTileChanges and
        not HasChanges(Player) then
        Result := eNotChanged;

    sGetTileInfo:
      if (Subject >= 0) and (Subject < MapSize) then
        Result := GetTileInfo(Player, -2, Subject, TTileInfo(Data))
      else
        Result := eInvalid;

    sGetCityTileInfo:
      if (Subject >= 0) and (Subject < MapSize) then
        Result := GetTileInfo(Player, -1, Subject, TTileInfo(Data))
      else
        Result := eInvalid;

    sGetHypoCityTileInfo:
      if (Subject >= 0) and (Subject < MapSize) then
      begin
        if (TTileInfo(Data).ExplCity < 0) or
          (TTileInfo(Data).ExplCity >= RW[Player].nCity) then
          Result := eInvalid
        else if ObserveLevel[Subject] shr (Player * 2) and 3 = 0 then
          Result := eNoPreq
        else
          Result := GetTileInfo(Player, TTileInfo(Data).ExplCity, Subject,
            TTileInfo(Data));
      end
      else
        Result := eInvalid;

    sGetJobProgress:
      if (Subject >= 0) and (Subject < MapSize) then
      begin
        if ObserveLevel[Subject] shr (Player * 2) and 3 = 0 then
          Result := eNoPreq
        else
          Result := GetJobProgress(Player, Subject, TJobProgressData(Data));
      end
      else
        Result := eInvalid;

    sGetModels:
      if (GTestFlags and tfUncover <> 0) or (Difficulty[Player] = 0)
      then { supervisor only command }
      begin
        for p1 := 0 to nPl - 1 do
          if (p1 <> Player) and (1 shl p1 and GAlive <> 0) then
            for mix := 0 to RW[p1].nModel - 1 do
              TellAboutModel(Player, p1, mix);
      end
      else
        Result := eInvalid;

    sGetUnits:
      if (Subject >= 0) and (Subject < MapSize) and
        (ObserveLevel[Subject] shr (Player * 2) and 3 = lObserveSuper) then
        Integer(Data) := GetUnitStack(Player, Subject)
      else
        Result := eNoPreq;

    sGetDefender:
      if (Subject >= 0) and (Subject < MapSize) and (Occupant[Subject] = Player)
      then
        Strongest(Subject, Integer(Data), D, I, J)
      else
        Result := eInvalid;

    sGetBattleForecast, sGetBattleForecastEx:
      if (Subject >= 0) and (Subject < MapSize) and
        (ObserveLevel[Subject] and (3 shl (Player * 2)) > 0) then
        with TBattleForecast(Data) do
          if (1 shl pAtt and GAlive <> 0) and (mixAtt >= 0) and
            (mixAtt < RW[pAtt].nModel) and
            ((pAtt = Player) or (RWemix[Player, pAtt, mixAtt] >= 0)) then
          begin
            Result := GetBattleForecast(Subject, TBattleForecast(Data), uix1,
              cix1, AStr, DStr, ABaseDamage, DBaseDamage);
            if Command = sGetBattleForecastEx then
            begin
              TBattleForecastEx(Data).AStr := (AStr + 200) div 400;
              TBattleForecastEx(Data).DStr := (DStr + 200) div 400;
              TBattleForecastEx(Data).ABaseDamage := ABaseDamage;
              TBattleForecastEx(Data).DBaseDamage := DBaseDamage;
            end;
            if Result = eOK then
              Result := eInvalid; // no enemy unit there!
          end
          else
            Result := eInvalid
      else
        Result := eInvalid;

    sGetUnitReport:
      if (Subject < 0) or (Subject >= RW[Player].nUn) or
        (RW[Player].Un[Subject].Loc < 0) then
        Result := eInvalid
      else
        GetUnitReport(Player, Subject, TUnitReport(Data));

    sGetMoveAdvice:
      if (Subject < 0) or (Subject >= RW[Player].nUn) or
        (RW[Player].Un[Subject].Loc < 0) then
        Result := eInvalid
      else
        Result := GetMoveAdvice(Player, Subject, TMoveAdviceData(Data));

    sGetPlaneReturn:
      if (Subject < 0) or (Subject >= RW[Player].nUn) or
        (RW[Player].Un[Subject].Loc < 0) or
        (RW[Player].Model[RW[Player].Un[Subject].mix].Domain <> dAir) then
        Result := eInvalid
      else
      begin
        if CanPlaneReturn(Player, Subject, TPlaneReturnData(Data)) then
          Result := eOK
        else
          Result := eNoWay;
      end;

    sGetCity:
      if (Subject >= 0) and (Subject < MapSize) and
        (ObserveLevel[Subject] shr (Player * 2) and 3 = lObserveSuper) and
        (RealMap[Subject] and fCity <> 0) then
        with TGetCityData(Data) do
        begin
          Owner := Player;
          SearchCity(Subject, Owner, cix1);
          C := RW[Owner].City[cix1];
          if (Owner <> Player) and (C.Project and cpImp = 0) then
            TellAboutModel(Player, Owner, C.Project and cpIndex);
        end
      else
        Result := eInvalid;

    sGetCityReport:
      if (Subject < 0) or (Subject >= RW[Player].nCity) or
        (RW[Player].City[Subject].Loc < 0) then
        Result := eInvalid
      else
        Result := GetCityReport(Player, Subject, TCityReport(Data));

    sGetCityReportNew:
      if (Subject < 0) or (Subject >= RW[Player].nCity) or
        (RW[Player].City[Subject].Loc < 0) then
        Result := eInvalid
      else
        GetCityReportNew(Player, Subject, TCityReportNew(Data));

    sGetCityAreaInfo:
      if (Subject < 0) or (Subject >= RW[Player].nCity) or
        (RW[Player].City[Subject].Loc < 0) then
        Result := eInvalid
      else
        GetCityAreaInfo(Player, RW[Player].City[Subject].Loc,
          TCityAreaInfo(Data));

    sGetEnemyCityReport:
      if (Subject >= 0) and (Subject < MapSize) and
        (ObserveLevel[Subject] shr (Player * 2) and 3 = lObserveSuper) and
        (RealMap[Subject] and fCity <> 0) then
      begin
        p1 := Occupant[Subject];
        if p1 < 0 then
          p1 := 1;
        SearchCity(Subject, p1, cix1);
        TCityReport(Data).HypoTiles := -1;
        TCityReport(Data).HypoTax := -1;
        TCityReport(Data).HypoLux := -1;
        GetCityReport(p1, cix1, TCityReport(Data));
      end
      else
        Result := eInvalid;

    sGetEnemyCityReportNew:
      if (Subject >= 0) and (Subject < MapSize) and
        (ObserveLevel[Subject] shr (Player * 2) and 3 = lObserveSuper) and
        (RealMap[Subject] and fCity <> 0) then
      begin
        p1 := Occupant[Subject];
        if p1 < 0 then
          p1 := 1;
        SearchCity(Subject, p1, cix1);
        TCityReport(Data).HypoTiles := -1;
        TCityReport(Data).HypoTax := -1;
        TCityReport(Data).HypoLux := -1;
        GetCityReportNew(p1, cix1, TCityReportNew(Data));
      end
      else
        Result := eInvalid;

    sGetEnemyCityAreaInfo:
      if (Subject >= 0) and (Subject < MapSize) and
        (ObserveLevel[Subject] shr (Player * 2) and 3 = lObserveSuper) and
        (RealMap[Subject] and fCity <> 0) then
      begin
        p1 := Occupant[Subject];
        if p1 < 0 then
          p1 := 1;
        SearchCity(Subject, p1, cix1);
        GetCityAreaInfo(p1, Subject, TCityAreaInfo(Data));
      end
      else
        Result := eInvalid;

    sGetCityTileAdvice:
      if (Subject < 0) or (Subject >= RW[Player].nCity) or
        (RW[Player].City[Subject].Loc < 0) then
        Result := eInvalid
      else
        GetCityTileAdvice(Player, Subject, TCityTileAdviceData(Data));

    {
      Map Editor Commands
      ____________________________________________________________________
    }
    sEditTile:
      if Player = 0 then
        with TEditTileData(Data) do
          EditTile(Loc, NewTile)
      else
        Result := eInvalid;

    sRandomMap:
      if (Player = 0) and MapGeneratorAvailable then
      begin
        CreateElevation;
        PreviewElevation := False;
        CreateMap(False);
        FillChar(ObserveLevel, MapSize * 4, 0);
        DiscoverAll(Player, lObserveSuper);
      end
      else
        Result := eInvalid;

    sMapGeneratorRequest:
      if not MapGeneratorAvailable then
        Result := eInvalid;

    {
      Client Deactivation Commands
      ____________________________________________________________________
    }
    sTurn, sTurn - sExecute:
      begin
        AllHumansDead := True;
        for p1 := 0 to nPl - 1 do
          if (1 shl p1 and GAlive <> 0) and (bix[p1].Kind = btTerm) then
            AllHumansDead := False;
        if (pDipActive >= 0) // still in negotiation mode
          or (pTurn = 0) and ((GWinner > 0) or (GTurn = MaxTurn) or
          (Difficulty[0] > 0) and AllHumansDead) then // game end reached
          Result := eViolation
        else if Command >= sExecute then
        begin
          if Mode = moPlaying then
          begin
            CL.State := FormerCLState;
            LogCityTileChanges;
{$IFNDEF SCR}
            if pTurn = 0 then
            begin
              LogChanges;
              SaveGame('~' + LogFileName, True);
            end;
{$ENDIF}
          end
          else if (Mode = moMovie) and (pTurn = 0) then
            CallPlayer(cMovieEndTurn, 0, nil^);
          GWatching := GWatching and GAlive or 1;
          RW[pTurn].Happened := 0;
          uixSelectedTransport := -1;
          SpyMission := smSabotageProd;
          if 1 shl pTurn and GAlive <> 0 then
          begin
            // calculate checksum
            TotalFood := 0;
            TotalProd := 0;
            for I := 0 to RW[pTurn].nCity - 1 do
              if RW[pTurn].City[I].Loc >= 0 then
              begin
                Inc(TotalFood, RW[pTurn].City[I].Food);
                Inc(TotalProd, RW[pTurn].City[I].Prod);
              end;
            CheckSum := TotalFood and 7 + TotalProd and 7 shl 3 +
              RW[pTurn].Money and 7 shl 6 + Worked[pTurn] div 100 and 7 shl 9;
          end
          else
            CheckSum := 0;

          if Mode < moPlaying then // check checksum
          begin
            if CheckSum <> Subject then
              LoadOK := False;
          end
          else // save checksum
            CL.Put(Command, Player, CheckSum, @Data);
{$IFDEF TEXTLOG}
          CmdInfo := '';
          if CheckSum and 7 <> Subject and 7 then
            CmdInfo := Format('***ERROR (Food %d) ',
              [(CheckSum and 7 - Subject and 7 + 12) mod 8 - 4]) + CmdInfo;
          if CheckSum shr 3 and 7 <> Subject shr 3 and 7 then
            CmdInfo := '***ERROR (Prod) ' + CmdInfo;
          if CheckSum shr 6 and 7 <> Subject shr 6 and 7 then
            CmdInfo := '***ERROR (Research) ' + CmdInfo;
          if CheckSum shr 9 and 7 <> Subject shr 9 and 7 then
            CmdInfo := '***ERROR (Work) ' + CmdInfo;
{$ENDIF}
          if 1 shl pTurn and GAlive <> 0 then
          begin
            AfterTurn;
            if Mode < moPlaying then
              InsertTerritoryUpdateCommands;
            // if bix[pTurn]=bixTerm then UpdateBorderHelper;
          end;

          repeat
            pTurn := (pTurn + 1) mod nPl;
            if pTurn = 0 then
              Inc(GTurn);
            if Assigned(bix[pTurn]) and ((1 shl pTurn) and GAlive = 0) then
            begin // already made extinct -- continue statistics
              Stat[stExplore, pTurn, GTurn] := 0;
              Stat[stPop, pTurn, GTurn] := 0;
              Stat[stTerritory, pTurn, GTurn] := 0;
              Stat[stScience, pTurn, GTurn] := 0;
              Stat[stWork, pTurn, GTurn] := 0;
              Stat[stMil, pTurn, GTurn] := 0;
            end;
          until (pTurn = 0) or ((1 shl pTurn and (GAlive or GWatching) <> 0) and
            (GWinner = 0));
          if (Mode = moLoading_Fast) and
            ((GTurn = LoadTurn) or (GTurn = LoadTurn - 1) and (pTurn > 0)) then
            Mode := moLoading;
          if Mode = moPlaying then
          begin
            CCCommand := cTurn;
            CCPlayer := pTurn;
            Notify(ntNextPlayer);
          end
          else
          begin
            if GTurn = 0 then
              BeforeTurn0
            else
              BeforeTurn;
            if (Mode = moMovie) and (pTurn = 0) then
            begin
              Inform(pTurn);
              CallPlayer(cMovieTurn, 0, nil^);
            end;
          end;
{$IFDEF TEXTLOG}CmdInfo := CmdInfo + Format('---Turn %d P%d---', [GTurn, pTurn]); {$ENDIF}
        end;
      end; // sTurn

    sBreak, sResign, sNextRound, sReload:
      if Mode = moMovie then
        MovieStopped := True
      else
      begin
        if Command = sReload then
        begin
          ok := (Difficulty[0] = 0) and (bix[0].Kind <> btNoTerm) and
            (Integer(Data) >= 0) and (Integer(Data) < GTurn);
          for p1 := 1 to nPl - 1 do
            if bix[p1].Kind = btTerm then
              ok := False;
          // allow reload in AI-only games only
        end
        else
          ok := Player = 0;
        if ok then
        begin
          if (Command = sBreak) or (Command = sResign) then
            Notify(ntBackOn);
          for I := 0 to Brains.Count - 1 do
            if Brains[I].Initialized then
            begin
              if Brains[I].Kind = btAI then
                Notify(ntDeinitModule, I);
              CallClient(I, cBreakGame, nil^);
            end;
          Notify(ntEndInfo);
          if (Command = sBreak) or (Command = sReload) then
          begin
            LogCityTileChanges;
            LogChanges;
            SaveGame(LogFileName, False);
          end;
          DeleteFile(SavePath + '~' + LogFileName);
          EndGame;
          case Command of
            sBreak:
              Notify(ntStartGoRefresh);
            sResign:
              Notify(ntStartGo);
            sNextRound:
              StartNewGame(SavePath, LogFileName, MapFileName, lx, ly,
                LandMass, MaxTurn);
            sReload:
              LoadGame(SavePath, LogFileName, Integer(Data), False);
          end;
        end
        else
          Result := eInvalid;
      end;

    sAbandonMap, sSaveMap:
      if Player = 0 then
      begin
        if Command = sSaveMap then
          SaveMap(MapFileName);
        Notify(ntBackOn);
        BrainTerm.Client(cBreakGame, -1, nil^);
        ReleaseMapEditor;
        if Command = sSaveMap then
          Notify(ntStartGoRefreshMaps)
        else
          Notify(ntStartGo);
      end
      else
        Result := eInvalid;

    scContact .. scContact + (nPl - 1) shl 4, scContact - sExecute .. scContact
      - sExecute + (nPl - 1) shl 4:
      if (pDipActive >= 0) or (1 shl (Command shr 4 and $F) and GAlive = 0) then
        Result := eInvalid
      else if GWinner > 0 then
        Result := eViolation // game end reached
      else if RW[Player].Treaty[Command shr 4 and $F] = trNoContact then
        Result := eNoPreq
      else if GTurn < GColdWarStart + ColdWarTurns then
        Result := eColdWar
      else if RW[Player].Government = gAnarchy then
        Result := eAnarchy
      else if RW[Command shr 4 and $F].Government = gAnarchy then
      begin
        Result := eAnarchy;
        LastEndClientCommand := scReject; // enable cancel treaty
        pContacted := Command shr 4 and $F;
      end
      else if Command >= sExecute then
      begin // contact request
        pContacted := Command shr 4 and $F;
        pDipActive := pContacted;
        Assert(Mode = moPlaying);
        Inform(pDipActive);
        ChangeClientWhenDone(scContact, pDipActive, pTurn, 4);
      end;

    scReject, scReject - sExecute:
      if LastEndClientCommand and $FF0F = scContact then
      begin
        if Command >= sExecute then
        begin // contact requested and not accepted yet
          pDipActive := -1;
          Assert(Mode = moPlaying);
          ChangeClientWhenDone(cContinue, pTurn, nil^, 0);
        end;
      end
      else
        Result := eInvalid;

    scDipStart, scDipStart - sExecute:
      if LastEndClientCommand and $FF0F = scContact then
      begin
        if Command >= sExecute then
        begin // accept contact
          pContacted := pDipActive;
          RW[pContacted].EnemyReport[pTurn].Credibility :=
            RW[pTurn].Credibility;
          pDipActive := pTurn;
          Assert(Mode = moPlaying);
          IntServer(sIntHaveContact, pTurn, pContacted, nil^);
          ChangeClientWhenDone(scDipStart, pDipActive, nil^, 0);
        end;
      end
      else
        Result := eInvalid;

    scDipNotice, scDipAccept, scDipCancelTreaty, scDipBreak,
      scDipNotice - sExecute, scDipAccept - sExecute,
      scDipCancelTreaty - sExecute, scDipBreak - sExecute:
      if pDipActive >= 0 then
      begin
        Assert(Mode = moPlaying);
        if pDipActive = pTurn then
          p1 := pContacted
        else
          p1 := pTurn;
        if (Command and not sExecute = scDipBreak and not sExecute) and
          (LastEndClientCommand <> scDipBreak) then // ok
        else if (Command and not sExecute = scDipNotice and not sExecute) and
          ((LastEndClientCommand = scDipCancelTreaty) or
          (LastEndClientCommand = scDipBreak)) then // ok
        else if (Command and not sExecute = scDipAccept and not sExecute) and
          (LastEndClientCommand = scDipOffer) then
          with LastOffer do
          begin
            // check if offer can be accepted
            if nDeliver + nCost = 0 then
              Result := eOfferNotAcceptable;
            for I := 0 to nDeliver + nCost - 1 do
              if Price[I] = opChoose then
                Result := eOfferNotAcceptable;
            for I := 0 to nCost - 1 do
              if not PayPrice(pDipActive, p1, Price[nDeliver + I], False) then
                Result := eOfferNotAcceptable;
            if (Command >= sExecute) and (Result >= rExecuted) then
            begin
              IntServer(sIntPayPrices + nDeliver + nCost, p1, pDipActive,
                LastOffer);
              // CheckContact;

              // tell other players about ship part trades
              HasShipChanged := False;
              FillChar(ShowShipChange, SizeOf(ShowShipChange), 0);
              for I := 0 to nDeliver + nCost - 1 do
                if Price[I] and opMask = opShipParts then
                begin
                  HasShipChanged := True;
                  if I >= nDeliver then
                  begin // p1 has demanded from pDipActive
                    ShowShipChange.Ship1Change[Price[I] shr 16 and 3] :=
                      +Integer(Price[I] and $FFFF);
                    ShowShipChange.Ship2Change[Price[I] shr 16 and 3] :=
                      -Integer(Price[I] and $FFFF);
                  end
                  else
                  begin // p1 has delivered to pDipActive
                    ShowShipChange.Ship1Change[Price[I] shr 16 and 3] :=
                      -Integer(Price[I] and $FFFF);
                    ShowShipChange.Ship2Change[Price[I] shr 16 and 3] :=
                      +Integer(Price[I] and $FFFF);
                  end;
                end;
              if HasShipChanged then
              begin
                ShowShipChange.Reason := scrTrade;
                ShowShipChange.Ship1Owner := p1;
                ShowShipChange.Ship2Owner := pDipActive;
                for p2 := 0 to nPl - 1 do
                  if (p2 <> p1) and (p2 <> pDipActive) and
                    (1 shl p2 and (GAlive or GWatching) <> 0) then
                  begin
                    Move(GShip, RW[p2].Ship, SizeOf(GShip));
                    if 1 shl p2 and GWatching <> 0 then
                      CallPlayer(cShowShipChange, p2, ShowShipChange);
                  end;
              end;
            end;
          end
        else if (Command and not sExecute = scDipCancelTreaty and not sExecute)
          and (RW[pDipActive].Treaty[p1] >= trPeace) then
        begin
          if (ServerVersion[pDipActive] >= $010100) and
            (GTurn < RW[pDipActive].LastCancelTreaty[p1] + CancelTreatyTurns)
          then
            Result := eCancelTreatyRush
          else if Command >= sExecute then
          begin
            IntServer(sIntCancelTreaty, pDipActive, p1, nil^);
            for p2 := 0 to nPl - 1 do
              if (p2 <> p1) and (1 shl p2 and PeaceEnded <> 0) then
              begin
                I := p1 shl 4 + pDipActive;
                CallPlayer(cShowSupportAllianceAgainst, p2, I);
              end;
            for p2 := 0 to nPl - 1 do
              if (p2 <> p1) and (1 shl p2 and PeaceEnded <> 0) then
              begin
                I := p2;
                CallPlayer(cShowCancelTreatyByAlliance, pDipActive, I);
              end;
          end;
        end
        else
          Result := eInvalid;
        if (Command >= sExecute) and (Result >= rExecuted) then
          if LastEndClientCommand = scDipBreak then
          begin // break negotiation
            pDipActive := -1;
            CallPlayer(cShowEndContact, pContacted, nil^);
            ChangeClientWhenDone(cContinue, pTurn, nil^, 0);
          end
          else
          begin
            if (GTestFlags and tfUncover <> 0) or (Difficulty[0] = 0) then
              with ShowNegoData do
              begin // display negotiation in log window
                pSender := pDipActive;
                pTarget := p1;
                Action := Command;
                bix[0].Client(cShowNego, 1 shl 16 + 3, ShowNegoData);
              end;
            pDipActive := p1;
            ChangeClientWhenDone(Command, pDipActive, nil^, 0);
          end;
      end
      else
        Result := eInvalid;

    scDipOffer, scDipOffer - sExecute:
      if (pDipActive >= 0) and (LastEndClientCommand <> scDipCancelTreaty) and
        (LastEndClientCommand <> scDipBreak) then
        if (LastEndClientCommand = scDipOffer) and
          (LastOffer.nDeliver + LastOffer.nCost + TOffer(Data).nDeliver +
          TOffer(Data).nCost = 0) then
        begin
          if Command >= sExecute then
          begin // agreed discussion end
            pDipActive := -1;
            CallPlayer(cShowEndContact, pContacted, nil^);
            Assert(Mode = moPlaying);
            ChangeClientWhenDone(cContinue, pTurn, nil^, 0);
          end;
        end
        else
        begin
          // check if offer can be made
          if pDipActive = pTurn then
            p1 := pContacted
          else
            p1 := pTurn;
          if RW[pDipActive].Treaty[p1] < trPeace then
          begin // no tribute allowed!
            for I := 0 to TOffer(Data).nDeliver + TOffer(Data).nCost - 1 do
              if (TOffer(Data).Price[I] and opMask = opTribute) then
                Result := eInvalidOffer;
            for I := 0 to TOffer(Data).nDeliver + TOffer(Data).nCost - 1 do
              if (TOffer(Data).Price[I] = opTreaty + trPeace) then
                Result := eOK;
          end;
          for I := 0 to TOffer(Data).nDeliver - 1 do
            if (TOffer(Data).Price[I] <> opChoose) and
              not PayPrice(pDipActive, p1, TOffer(Data).Price[I], False) then
              Result := eInvalidOffer;
          if CountPrice(TOffer(Data), opTreaty) > 1 then
            Result := eInvalidOffer;
          for I := 0 to nShipPart - 1 do
            if CountPrice(TOffer(Data), opShipParts + I shl 16) > 1 then
              Result := eInvalidOffer;
          if CountPrice(TOffer(Data), opMoney) > 1 then
            Result := eInvalidOffer;
          if CountPrice(TOffer(Data), opTribute) > 1 then
            Result := eInvalidOffer;
          case CountPrice(TOffer(Data), opChoose) of
            0:
              ;
            1:
              if (TOffer(Data).nCost = 0) or (TOffer(Data).nDeliver = 0) then
                Result := eInvalidOffer;
          else
            Result := eInvalidOffer;
          end;

          // !!! check here if cost can be demanded

          if (Command >= sExecute) and (Result >= rExecuted) then
          begin
            OfferFullySupported := (TOffer(Data).nDeliver <= 2) and
              (TOffer(Data).nCost <= 2); // >2 no more allowed
            for I := 0 to TOffer(Data).nDeliver + TOffer(Data).nCost - 1 do
            begin
              if TOffer(Data).Price[I] and opMask = opTribute then
                OfferFullySupported := False;
              // tribute no more part of the game
              if (TOffer(Data).Price[I] and opMask = opTreaty) and
                (TOffer(Data).Price[I] - opTreaty <= RW[pDipActive].Treaty[p1])
              then
                OfferFullySupported := False;
              // agreed treaty end no more part of the game
              if TOffer(Data).Price[I] = opTreaty + trCeaseFire then
                OfferFullySupported := False;
              // ceasefire no more part of the game
            end;
            if not OfferFullySupported then
            begin
              // some elements have been removed from the game -
              // automatically respond will null-offer
              LastOffer.nDeliver := 0;
              LastOffer.nCost := 0;
              ChangeClientWhenDone(scDipOffer, pDipActive, LastOffer,
                SizeOf(LastOffer));
            end
            else
            begin
              if (GTestFlags and tfUncover <> 0) or (Difficulty[0] = 0) then
                with ShowNegoData do
                begin // display negotiation in log window
                  pSender := pDipActive;
                  pTarget := p1;
                  Action := Command;
                  Offer := TOffer(Data);
                  bix[0].Client(cShowNego, 1 shl 16 + 3, ShowNegoData);
                end;
              LastOffer := TOffer(Data);
              // show offered things to receiver
              for I := 0 to LastOffer.nDeliver - 1 do
                ShowPrice(pDipActive, p1, LastOffer.Price[I]);
              pDipActive := p1;
              Assert(Mode = moPlaying);
              ChangeClientWhenDone(scDipOffer, pDipActive, LastOffer,
                SizeOf(LastOffer));
            end;
          end;
        end
      else
        Result := eInvalid;

    {
      General Commands
      ____________________________________________________________________
    }
    sClearTestFlag:
      if Player = 0 then
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('ClearTestFlag %x', [Subject]); {$ENDIF}
        ClearTestFlags(Subject);
      end
      else
        Result := eInvalid;

    sSetTestFlag:
      if Player = 0 then
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('SetTestFlag %x', [Subject]); {$ENDIF}
        SetTestFlags(Player, Subject);
        // CheckContact;
      end
      else
        Result := eInvalid;

    sSetGovernment, sSetGovernment - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('SetGovernment P%d: %d', [Player, Subject]); {$ENDIF}
        if RW[Player].Happened and phChangeGov = 0 then
          Result := eViolation
        else if RW[Player].Government = Subject then
          Result := eNotChanged
        else if (Subject >= nGov) then
          Result := eInvalid
        else if (Subject >= gMonarchy) and
          (RW[Player].Tech[GovPreq[Subject]] < tsApplicable) then
          Result := eNoPreq
        else if Command >= sExecute then
        begin
          RW[Player].Government := Subject;
          for p1 := 0 to nPl - 1 do
            if (p1 <> Player) and ((GAlive or GWatching) and (1 shl p1) <> 0)
            then
              RW[p1].EnemyReport[Player].Government := Subject;
        end;
      end;

    sSetRates, sSetRates - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('SetRates P%d: %d/%d', [Player, Subject and $F * 10, Subject shr 4 * 10]); {$ENDIF}
        if Subject and $F + Subject shr 4 > 10 then
          Result := eInvalid
        else if (RW[Player].TaxRate = Subject and $F * 10) and
          (RW[Player].LuxRate = Subject shr 4 * 10) then
          Result := eNotChanged
        else if Command >= sExecute then
        begin
          RW[Player].TaxRate := Subject and $F * 10;
          RW[Player].LuxRate := Subject shr 4 * 10;
        end;
      end;

    sRevolution:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('Revolution P%d', [Player]); {$ENDIF}
        if RW[Player].Government = gAnarchy then
          Result := eInvalid
        else
        begin
          RW[Player].Government := gAnarchy;
          for p1 := 0 to nPl - 1 do
            if (p1 <> Player) and ((GAlive or GWatching) and (1 shl p1) <> 0)
            then
              RW[p1].EnemyReport[Player].Government := gAnarchy;
          RW[Player].AnarchyStart := GTurn;
        end;
      end;

    sSetResearch, sSetResearch - sExecute:
      with RW[Player] do
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('SetResearch P%d: %d', [Player, Subject]);
        {$ENDIF}
        if (Happened and phTech <> 0) and
          ((Subject < nAdv) or (Subject = adMilitary)) then
        begin
          if (Mode = moPlaying) and (Subject = adMilitary) and
            (DevModelTurn[Player] <> GTurn) then
            Result := eNoModel
          else if Subject <> adMilitary then
          begin
            if Subject = futComputingTechnology then
            begin
              if Tech[Subject] >= MaxFutureTech_Computing then
                Result := eInvalid;
            end
            else if Subject in FutureTech then
            begin
              if Tech[Subject] >= MaxFutureTech then
                Result := eInvalid;
            end
            else if Tech[Subject] >= tsApplicable then
              Result := eInvalid; // already discovered
            if Tech[Subject] <> tsSeen then // look if preqs met
              if AdvPreq[Subject, 2] <> preNone then
              begin // 2 of 3 required
                I := 0;
                for J := 0 to 2 do
                  if Tech[AdvPreq[Subject, J]] >= tsApplicable then
                    Inc(I);
                if I < 2 then
                  Result := eNoPreq;
              end
              else if (AdvPreq[Subject, 0] <> preNone) and
                (Tech[AdvPreq[Subject, 0]] < tsApplicable) or
                (AdvPreq[Subject, 1] <> preNone) and
                (Tech[AdvPreq[Subject, 1]] < tsApplicable) then
                Result := eNoPreq;
          end;
          if (Result = eOK) and (Command >= sExecute) then
          begin
            if (Mode = moPlaying) and (Subject = adMilitary) then
              IntServer(sIntSetDevModel, Player, 0, DevModel.Kind);
            // save DevModel, because sctModel commands are not logged
            ResearchTech := Subject;
          end;
        end
        else
          Result := eViolation;
      end;

    sStealTech, sStealTech - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('StealTech P%d: %d', [Player, Subject]);
        {$ENDIF}
        if RW[Player].Happened and phStealTech = 0 then
          Result := eInvalid
        else if (Subject >= nAdv) or (Subject in FutureTech) or
          (RW[Player].Tech[Subject] >= tsSeen) or
          (RW[GStealFrom].Tech[Subject] < tsApplicable) then
          Result := eInvalid
        else if Command >= sExecute then
        begin
          SeeTech(Player, Subject);
          Dec(RW[Player].Happened, phStealTech);
        end;
      end;

    sSetAttitude .. sSetAttitude + (nPl - 1) shl 4,
      sSetAttitude - sExecute .. sSetAttitude - sExecute + (nPl - 1) shl 4:
      begin
        p1 := Command shr 4 and $F;
{$IFDEF TEXTLOG}CmdInfo := Format('SetAttitude P%d to P%d: %d', [Player, p1, Subject]); {$ENDIF}
        if (Subject >= nAttitude) or (p1 >= nPl) or
          (RW[Player].EnemyReport[p1] = nil) then
          Result := eInvalid
        else if RW[Player].Treaty[p1] = trNoContact then
          Result := eNoPreq
        else if RW[Player].Attitude[p1] = Subject then
          Result := eNotChanged
        else if Command >= sExecute then
        begin
          RW[Player].Attitude[p1] := Subject;
          RW[p1].EnemyReport[Player].Attitude := Subject;
        end;
      end;

    sCancelTreaty, sCancelTreaty - sExecute:
      if (LastEndClientCommand <> scReject) or
        (RW[Player].Treaty[pContacted] < trPeace) then
        Result := eInvalid
      else if (ServerVersion[Player] >= $010100) and
        (GTurn < RW[Player].LastCancelTreaty[pContacted] + CancelTreatyTurns)
      then
        Result := eCancelTreatyRush
      else if Command >= sExecute then
      begin
        CallPlayer(cShowCancelTreaty, pContacted, Player);
        IntServer(sIntCancelTreaty, Player, pContacted, nil^);
        for p2 := 0 to nPl - 1 do
          if (p2 <> pContacted) and (1 shl p2 and PeaceEnded <> 0) then
          begin
            I := pContacted shl 4 + Player;
            CallPlayer(cShowSupportAllianceAgainst, p2, I);
          end;
        for p2 := 0 to nPl - 1 do
          if (p2 <> pContacted) and (1 shl p2 and PeaceEnded <> 0) then
          begin
            I := p2;
            CallPlayer(cShowCancelTreatyByAlliance, Player, I);
          end;
        LastEndClientCommand := sTurn;
      end;

    {
      Model Related Commands
      ____________________________________________________________________
    }
    sCreateDevModel, sCreateDevModel - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('CreateDevModel P%d', [Player]); {$ENDIF}
        if Subject >= 4 then
          Result := eInvalid
        else if (upgrade[Subject, 0].Preq <> preNone) and
          (RW[Player].Tech[upgrade[Subject, 0].Preq] < tsApplicable) then
          Result := eNoPreq
        else if Command >= sExecute then
        begin
          with RW[Player].DevModel do
          begin
            Domain := Subject;
            MStrength := 0;
            MTrans := 0;
            MCost := 0;
            Upgrades := 0;
            FutureMCost := 0;
            for I := 0 to nUpgrade - 1 do
              with upgrade[Domain, I] do
                if (Preq = preNone) or (Preq >= 0) and
                  ((RW[Player].Tech[Preq] >= tsApplicable) or
                  (Preq in FutureTech) and (RW[Player].Tech[Preq] >= 0)) then
                begin
                  if Preq in FutureTech then
                  begin
                    J := RW[Player].Tech[Preq];
                    Inc(FutureMCost, J * Cost);
                  end
                  else
                  begin
                    J := 1;
                    if Cost > MCost then
                      MCost := Cost;
                  end;
                  Inc(Upgrades, 1 shl I);
                  Inc(MStrength, J * Strength);
                  Inc(MTrans, J * Trans);
                end;
            Inc(MCost, FutureMCost);
            FillChar(Cap, SizeOf(Cap), 0);
            Cap[mcOffense] := 2;
            Cap[mcDefense] := 1;
            for I := 0 to nFeature - 1 do
              with Feature[I] do
                if (1 shl Domain and Domains <> 0) and
                  ((Preq = preNone) or (Preq = preSun) and
                  (GWonder[woSun].EffectiveOwner = Player) or (Preq >= 0) and
                  (RW[Player].Tech[Preq] >= tsApplicable)) and (I in AutoFeature)
                then
                  Cap[I] := 1;
            MaxWeight := 5;
            if (WeightPreq7[Domain] <> preNA) and
              (RW[Player].Tech[WeightPreq7[Domain]] >= tsApplicable) then
              MaxWeight := 7;
            if (WeightPreq10[Domain] <> preNA) and
              (RW[Player].Tech[WeightPreq10[Domain]] >= tsApplicable) then
              if Domain = dSea then
                MaxWeight := 9
              else
                MaxWeight := 10;
          end;
          CalculateModel(RW[Player].DevModel);
          DevModelTurn[Player] := GTurn;
        end
      end;

    sSetDevModelCap .. sSetDevModelCap + $3F0,
      sSetDevModelCap - sExecute .. sSetDevModelCap - sExecute + $3F0:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('SetDevModelCap P%d', [Player]); {$ENDIF}
        if Subject >= nFeature then
          Result := eInvalid
        else if DevModelTurn[Player] = GTurn then
        begin
          NewCap := Command shr 4 and $3F; { new value }
          with RW[Player].DevModel do
            if 1 shl Domain and Feature[Subject].Domains = 0 then
              Result := eDomainMismatch
            else if not((Feature[Subject].Preq = preNone) or
              (Feature[Subject].Preq = preSun) and
              (GWonder[woSun].EffectiveOwner = Player) or
              (Feature[Subject].Preq >= 0) and
              (RW[Player].Tech[Feature[Subject].Preq] >= tsApplicable)) then
              Result := eNoPreq
            else
            begin
              if (Subject in AutoFeature) or (Subject = mcDefense) then
                MinCap := 1
              else
                MinCap := 0; { MinCap - minimum use of feature }
              if Subject >= mcFirstNonCap then
                MaxCap := 1
              else if Subject = mcDefense then
              begin
                if Domain = dGround then
                  MaxCap := 2
                else
                  MaxCap := 3;
                if RW[Player].Tech[adSteel] >= tsApplicable then
                  Inc(MaxCap);
              end
              else
                MaxCap := 8; { MaxCap - maximum use of this feature }
              if (Domain = dGround) and (Subject = mcDefense) then
                CapWeight := 2
              else
                CapWeight := Feature[Subject].Weight;
              if (NewCap < MinCap) or (NewCap > MaxCap) or
                (Weight + (NewCap - Cap[Subject]) * CapWeight > MaxWeight) then
                Result := eViolation
              else if Command >= sExecute then
              begin
                Cap[Subject] := NewCap;

                // mutual feature exclusion
                case Subject of
                  mcSub:
                    begin
                      if ServerVersion[Player] >= $010103 then
                        Cap[mcSeaTrans] := 0;
                      Cap[mcArtillery] := 0;
                      Cap[mcCarrier] := 0;
                      if Cap[mcDefense] > 2 then
                        Cap[mcDefense] := 2;
                    end;
                  mcSeaTrans:
                    begin
                      if ServerVersion[Player] >= $010103 then
                        Cap[mcSub] := 0;
                    end;
                  mcCarrier:
                    Cap[mcSub] := 0;
                  mcArtillery:
                    Cap[mcSub] := 0;
                  mcAlpine:
                    begin
                      Cap[mcOver] := 0;
                      Cap[mcMob] := 0;
                    end;
                  mcOver:
                    Cap[mcAlpine] := 0;
                  mcMob:
                    begin
                      Cap[mcAlpine] := 0;
                    end;
                end;

                CalculateModel(RW[Player].DevModel);
              end;
            end;
        end
        else
          Result := eNoModel;
      end;

    {
      Unit Related Commands
      ____________________________________________________________________
    }
    sRemoveUnit, sRemoveUnit - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('RemoveUnit P%d Mod%d Loc%d', [Player, RW[Player].Un[Subject].mix, RW[Player].Un[Subject].Loc]); {$ENDIF}
        if (Subject >= RW[Player].nUn) or (RW[Player].Un[Subject].Loc < 0) then
          Result := eInvalid
        else
        begin
          Result := eRemoved;
          Loc0 := RW[Player].Un[Subject].Loc;
          if RealMap[Loc0] and fCity <> 0 then { check utilize }
          begin
            SearchCity(Loc0, Player, cix1);
            with RW[Player].City[cix1] do
            begin
              if (RW[Player].Model[RW[Player].Un[Subject].mix].Kind = mkCaravan)
                and ((Project and cpImp = 0) or
                (Imp[Project and cpIndex].Kind <> ikShipPart)) or
                (Project and cpImp = 0) and
                (RW[Player].Model[Project and cpIndex].Kind <> mkCaravan) then
                Result := eUtilized;
              if Command >= sExecute then
              begin
                if Result = eUtilized then
                begin
                  with RW[Player].Un[Subject] do
                  begin
                    Cost := Integer(RW[Player].Model[mix].Cost) * Health *
                      BuildCostMod[Difficulty[Player]] div 1200;
                    if RW[Player].Model[mix].Cap[mcLine] > 0 then
                      Cost := Cost div 2;
                  end;
                  if Project and (cpImp + cpIndex) = cpImp + imTrGoods then
                    Inc(RW[Player].Money, Cost)
                  else
                  begin
                    Inc(Prod, Cost * 2 div 3);
                    Project0 := Project0 and not cpCompleted;
                    if Project0 and not cpAuto <> Project and not cpAuto then
                      Project0 := Project;
                    Prod0 := Prod;
                  end
                end;
                RemoveUnit_UpdateMap(Player, Subject);
              end;
            end;
          end
          else if Command >= sExecute then
            RemoveUnit_UpdateMap(Player, Subject);
        end
      end;

    sSetUnitHome, sSetUnitHome - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('SetUnitHome P%d Mod%d Loc%d', [Player, RW[Player].Un[Subject].mix, RW[Player].Un[Subject].Loc]); {$ENDIF}
        if (Subject >= RW[Player].nUn) or (RW[Player].Un[Subject].Loc < 0) then
          Result := eInvalid
        else
        begin
          Loc0 := RW[Player].Un[Subject].Loc;
          if RealMap[Loc0] and fCity = 0 then
            Result := eInvalid
          else
          begin
            SearchCity(Loc0, Player, cix1);
            if RW[Player].City[cix1].Flags and chCaptured <> 0 then
              Result := eViolation
            else if Command >= sExecute then
              RW[Player].Un[Subject].Home := cix1;
          end;
        end;
      end;

    sSetSpyMission .. sSetSpyMission + (nSpyMission - 1) shl 4,
      sSetSpyMission - sExecute .. sSetSpyMission - sExecute +
      (nSpyMission - 1) shl 4:
      if Command >= sExecute then
        SpyMission := Command shr 4 and $F;

    sLoadUnit, sLoadUnit - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('LoadUnit P%d Mod%d Loc%d', [Player, RW[Player].Un[Subject].mix, RW[Player].Un[Subject].Loc]); {$ENDIF}
        if (Subject >= RW[Player].nUn) or (RW[Player].Un[Subject].Loc < 0) then
          Result := eInvalid
        else
          Result := LoadUnit(Player, Subject, Command < sExecute);
      end;

    sUnloadUnit, sUnloadUnit - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('UnloadUnit P%d Mod%d Loc%d', [Player, RW[Player].Un[Subject].mix, RW[Player].Un[Subject].Loc]); {$ENDIF}
        if (Subject >= RW[Player].nUn) or (RW[Player].Un[Subject].Loc < 0) then
          Result := eInvalid
        else
          Result := UnloadUnit(Player, Subject, Command < sExecute);
      end;

    sSelectTransport, sSelectTransport - sExecute:
      if (Subject >= RW[Player].nUn) or (RW[Player].Un[Subject].Loc < 0) then
        Result := eInvalid
      else
        with RW[Player].Model[RW[Player].Un[Subject].mix] do
        begin
          if Cap[mcSeaTrans] + Cap[mcAirTrans] + Cap[mcCarrier] = 0 then
            Result := eInvalid
          else if Command >= sExecute then
            uixSelectedTransport := Subject;
        end;

    sCreateUnit .. sCreateUnit + (nPl - 1) shl 4,
      sCreateUnit - sExecute .. sCreateUnit - sExecute + (nPl - 1) shl 4:
      if (GTestFlags and tfUncover <> 0) or (Difficulty[Player] = 0)
      then { supervisor only command }
      begin
        p1 := Command shr 4 and $F;
        Loc1 := Integer(Data);
        if (Occupant[Loc1] >= 0) and (p1 <> Occupant[Loc1]) or
          (RealMap[Loc1] and fCity <> 0) and
          (RealMap[Loc1] shr 27 <> Cardinal(p1)) or
          (RW[p1].Model[Subject].Domain < dAir) and
          ((RW[p1].Model[Subject].Domain = dSea) <> (RealMap[Integer(Data)] and
          fTerrain < fGrass)) then
          Result := eViolation
        else if Command >= sExecute then
        begin
          CreateUnit(p1, Subject);
          RW[p1].Un[RW[p1].nUn - 1].Loc := Integer(Data);
          PlaceUnit(p1, RW[p1].nUn - 1);
          UpdateUnitMap(Integer(Data));
        end;
      end
      else
        Result := eInvalid;

    sMoveUnit + (0 + 6 * 8) * 16, sMoveUnit + (1 + 7 * 8) * 16,
      sMoveUnit + (2 + 0 * 8) * 16, sMoveUnit + (1 + 1 * 8) * 16,
      sMoveUnit + (0 + 2 * 8) * 16, sMoveUnit + (7 + 1 * 8) * 16,
      sMoveUnit + (6 + 0 * 8) * 16, sMoveUnit + (7 + 7 * 8) * 16,
      sMoveUnit - sExecute + (0 + 6 * 8) * 16, sMoveUnit - sExecute +
      (1 + 7 * 8) * 16, sMoveUnit - sExecute + (2 + 0 * 8) * 16,
      sMoveUnit - sExecute + (1 + 1 * 8) * 16, sMoveUnit - sExecute +
      (0 + 2 * 8) * 16, sMoveUnit - sExecute + (7 + 1 * 8) * 16,
      sMoveUnit - sExecute + (6 + 0 * 8) * 16, sMoveUnit - sExecute +
      (7 + 7 * 8) * 16:
      begin
        dx := (Command shr 4 + 4) and 7 - 4;
        dy := (Command shr 7 + 4) and 7 - 4;
{$IFDEF TEXTLOG}CmdInfo := Format('MoveUnit P%d I%d Mod%d Loc%d (%d,%d)', [Player, Subject, RW[Player].Un[Subject].mix, RW[Player].Un[Subject].Loc, dx, dy]); {$ENDIF}
        if (Subject >= RW[Player].nUn) or (RW[Player].Un[Subject].Loc < 0) then
          Result := eInvalid
        else
          Result := MoveUnit(Player, Subject, dx, dy, Command < sExecute);
      end;

    {
      Settlers Related Commands
      ____________________________________________________________________
    }
    sAddToCity, sAddToCity - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('AddToCity P%d Mod%d Loc%d', [Player, RW[Player].Un[Subject].mix, RW[Player].Un[Subject].Loc]); {$ENDIF}
        if (Subject >= RW[Player].nUn) or (RW[Player].Un[Subject].Loc < 0) then
          Result := eInvalid
        else if not(RW[Player].Model[RW[Player].Un[Subject].mix].Kind
          in [mkSettler, mkSlaves]) and
          (RW[Player].Un[Subject].Flags and unConscripts = 0) then
          Result := eViolation
        else
        begin
          Loc0 := RW[Player].Un[Subject].Loc;
          if RealMap[Loc0] and fCity = 0 then
            Result := eInvalid
          else
          begin
            SearchCity(Loc0, Player, cix1);
            with RW[Player].City[cix1] do
              if not CanCityGrow(Player, cix1) then
                Result := eMaxSize
              else if Command >= sExecute then
              begin { add to city }
                if Mode = moPlaying then
                  SavedTiles[cix1] := 0; // save in every case
                if CanCityGrow(Player, cix1) then
                  CityGrowth(Player, cix1);
                if (RW[Player].Model[RW[Player].Un[Subject].mix]
                  .Kind = mkSettler) and CanCityGrow(Player, cix1) then
                  CityGrowth(Player, cix1);
                RemoveUnit_UpdateMap(Player, Subject);
              end;
          end;
        end;
      end;

    sStartJob .. sStartJob + $3F0, sStartJob - sExecute .. sStartJob + $3F0
      - sExecute:
      begin
        Loc0 := RW[Player].Un[Subject].Loc;
        I := Command shr 4 and $3F; // new job
{$IFDEF TEXTLOG}CmdInfo := Format('StartJob P%d Mod%d Loc%d: %d', [Player, RW[Player].Un[Subject].mix, Loc0, I]); {$ENDIF}
        if (Subject >= RW[Player].nUn) or (Loc0 < 0) then
          Result := eInvalid
        else if I >= nJob then
          Result := eInvalid
        else
        begin
          Result := StartJob(Player, Subject, I, Command < sExecute);
          if Result = eCity then
          begin // new city
            cix1 := RW[Player].nCity - 1;
            AddBestCityTile(Player, cix1);
            if Mode = moPlaying then
              with RW[Player].City[cix1] do
              begin
                // SavedResourceWeights[cix1]:=ResourceWeights;
                SavedTiles[cix1] := 0; // save in every case
              end;
            if Mode >= moMovie then { show new city in interface modules }
              for p1 := 0 to nPl - 1 do
                if (1 shl p1 and GWatching <> 0) and (p1 <> Player) and
                  (ObserveLevel[Loc0] and (3 shl (2 * p1)) > 0) then
                  CallPlayer(cShowCityChanged, p1, Loc0);
          end;
        end;
      end;

    {
      City Related Commands
      ____________________________________________________________________
    }
    sSetCityProject, sSetCityProject - sExecute:
      begin
        NewProject := Integer(Data) and not cpAuto;
{$IFDEF TEXTLOG}CmdInfo := Format('SetCityProject P%d Loc%d: %d', [Player, RW[Player].City[Subject].Loc, NewProject]); {$ENDIF}
        if (Subject >= RW[Player].nCity) or (RW[Player].City[Subject].Loc < 0)
        then
          Result := eInvalid
        else
          with RW[Player].City[Subject] do
          begin
            if NewProject = Project then
              Result := eNotChanged
            else
            begin
              pt0 := ProjectType(Project0);
              pt1 := ProjectType(NewProject);
              if NewProject and cpImp = 0 then
              begin
                if NewProject and cpIndex >= RW[Player].nModel then
                  Result := eInvalid
                else if (NewProject and cpConscripts <> 0) and
                  not((RW[Player].Tech[adConscription] >= tsApplicable) and
                  (RW[Player].Model[NewProject and cpIndex].Domain = dGround)
                  and (RW[Player].Model[NewProject and cpIndex].Kind < mkScout))
                then
                  Result := eViolation
                  // else if (RW[Player].Model[NewProject and cpIndex].Kind=mkSlaves)
                  // and (GWonder[woPyramids].EffectiveOwner<>Player) then
                  // result:=eNoPreq
              end
              else if NewProject and cpIndex >= nImp then
                Result := eInvalid
              else
              begin
                Preq := Imp[NewProject and cpIndex].Preq;
                for I := 0 to nImpReplacement - 1 do
                  if (ImpReplacement[I].OldImp = NewProject and cpIndex) and
                    (built[ImpReplacement[I].NewImp] > 0) then
                    Result := eObsolete;
                if Result = eObsolete then
                else if Preq = preNA then
                  Result := eInvalid
                else if (Preq >= 0) and (RW[Player].Tech[Preq] < tsApplicable)
                then
                  Result := eNoPreq
                else if built[NewProject and cpIndex] > 0 then
                  Result := eInvalid
                else if (NewProject and cpIndex < nWonder) and
                  (GWonder[NewProject and cpIndex].CityID <> WonderNotBuiltYet) then
                  Result := eViolation // wonder already exists
                else if (NewProject and cpIndex = imSpacePort) and
                  (RW[Player].NatBuilt[imSpacePort] > 0) then
                  Result := eViolation // space port already exists
                else if (NewProject = cpImp + imBank) and (built[imMarket] = 0)
                  or (NewProject = cpImp + imUniversity) and
                  (built[imLibrary] = 0) or (NewProject = cpImp + imResLab) and
                  (built[imUniversity] = 0) or (NewProject = cpImp + imMfgPlant)
                  and (built[imFactory] = 0) then
                  Result := eNoPreq;
                case NewProject - cpImp of
                  woLighthouse, woMagellan, imCoastalFort, imHarbor, imPlatform:
                    begin { city at ocean? }
                      Preq := 0;
                      V8_to_Loc(Loc, Adjacent);
                      for V8 := 0 to 7 do
                      begin
                        Loc1 := Adjacent[V8];
                        if (Loc1 >= 0) and (Loc1 < MapSize) and
                          (RealMap[Loc1] and fTerrain = fShore) then
                          Inc(Preq);
                      end;
                      if Preq = 0 then
                        Result := eNoPreq;
                    end;
                  woHoover, imHydro:
                    begin { city at river or mountains? }
                      Preq := 0;
                      V8_to_Loc(Loc, Adjacent);
                      for V8 := 0 to 7 do
                      begin
                        Loc1 := Adjacent[V8];
                        if (Loc1 >= 0) and (Loc1 < MapSize) and
                          ((RealMap[Loc1] and fTerrain = fMountains) or
                          (RealMap[Loc1] and fRiver <> 0)) then
                          Inc(Preq);
                      end;
                      if Preq = 0 then
                        Result := eNoPreq;
                    end;
                  woMIR, imShipComp, imShipPow, imShipHab:
                    if RW[Player].NatBuilt[imSpacePort] = 0 then
                      Result := eNoPreq;
                end;
                if (GTestFlags and tfNoRareNeed = 0) and
                  (Imp[NewProject and cpIndex].Kind = ikShipPart) then
                  if RW[Player].Tech[adMassProduction] < tsApplicable then
                    Result := eNoPreq
                  else
                  begin // check for rare resources
                    if NewProject and cpIndex = imShipComp then
                      J := 1
                    else if NewProject and cpIndex = imShipPow then
                      J := 2
                    else { if NewProject and cpIndex=imShipHab then }
                      J := 3;
                    // j = rare resource required
                    Preq := 0;
                    V21_to_Loc(Loc, Radius);
                    for V21 := 1 to 26 do
                    begin
                      Loc1 := Radius[V21];
                      if (Loc1 >= 0) and (Loc1 < MapSize) and
                        (RealMap[Loc1] shr 25 and 3 = Cardinal(J)) then
                        Inc(Preq);
                    end;
                    if Preq = 0 then
                      Result := eNoPreq;
                  end;
              end;

              if (Command >= sExecute) and (Result >= rExecuted) then
              begin
                if pt0 <> ptSelect then
                  if NewProject and (cpImp or cpIndex) = Project0 and
                    (cpImp or cpIndex) then
                    Prod := Prod0
                  else if (pt1 = ptTrGoods) or (pt1 = ptShip) or (pt1 <> pt0)
                    and (pt0 <> ptCaravan) then
                  begin
                    Inc(RW[Player].Money, Prod0);
                    Prod := 0;
                    Prod0 := 0;
                    Project0 := cpImp + imTrGoods;
                  end
                  else
                    Prod := Prod0 * 2 div 3;
                Project := NewProject;
              end;
            end;
          end;
      end;

    sBuyCityProject, sBuyCityProject - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('BuyCityProject P%d Loc%d', [Player, RW[Player].City[Subject].Loc]); {$ENDIF}
        if (Subject >= RW[Player].nCity) or (RW[Player].City[Subject].Loc < 0)
        then
          Result := eInvalid
        else
          with RW[Player].City[Subject] do
            if (RW[Player].Government = gAnarchy) or (Flags and chCaptured <> 0)
            then
              Result := eOutOfControl
            else if (Project and cpImp <> 0) and
              ((Project and cpIndex = imTrGoods) or
              (Imp[Project and cpIndex].Kind = ikShipPart)) then
              Result := eInvalid // don't buy colony ship
            else
            begin
              CityReport.HypoTiles := -1;
              CityReport.HypoTax := -1;
              CityReport.HypoLux := -1;
              GetCityReport(Player, Subject, CityReport);
              Cost := CityReport.ProdCost;
              NextProd := CityReport.ProdRep - CityReport.Support;
              if (CityReport.Working - CityReport.Happy > Size shr 1) or
                (NextProd < 0) then // !!! change to new style disorder
                NextProd := 0;
              Cost := Cost - Prod - NextProd;
              if (GWonder[woMich].EffectiveOwner = Player) and
                (Project and cpImp <> 0) then
                Cost := Cost * 2
              else
                Cost := Cost * 4;
              if Cost <= 0 then
                Result := eNotChanged
              else if Cost > RW[Player].Money then
                Result := eViolation
              else if Command >= sExecute then
                IntServer(sIntBuyMaterial, Player, Subject, Cost);
              // need to save material/cost because city tiles are not correct
              // when loading
            end;
      end;

    sSellCityProject, sSellCityProject - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('SellCityProject P%d Loc%d', [Player, RW[Player].City[Subject].Loc]); {$ENDIF}
        if (Subject >= RW[Player].nCity) or (RW[Player].City[Subject].Loc < 0)
        then
          Result := eInvalid
        else if Command >= sExecute then
          with RW[Player].City[Subject] do
          begin
            Inc(RW[Player].Money, Prod0);
            Prod := 0;
            Prod0 := 0;
          end;
      end;

    sSellCityImprovement, sSellCityImprovement - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('SellCityImprovement P%d Loc%d: %d', [Player, RW[Player].City[Subject].Loc, Integer(Data)]); {$ENDIF}
        if (Subject >= RW[Player].nCity) or (RW[Player].City[Subject].Loc < 0)
        then
          Result := eInvalid
        else
          with RW[Player].City[Subject] do
            if built[Integer(Data)] = 0 then
              Result := eInvalid
            else if (RW[Player].Government = gAnarchy) or
              (Flags and chCaptured <> 0) then
              Result := eOutOfControl
            else if Flags and chImprovementSold <> 0 then
              Result := eOnlyOnce
            else if Command >= sExecute then
            begin
              Inc(RW[Player].Money, Imp[Integer(Data)].Cost * BuildCostMod
                [Difficulty[Player]] div 12);
              built[Integer(Data)] := 0;
              if Imp[Integer(Data)].Kind in [ikNatLocal, ikNatGlobal] then
              begin
                RW[Player].NatBuilt[Integer(Data)] := 0;
                case Integer(Data) of
                  imGrWall:
                    GrWallContinent[Player] := -1;
                  imSpacePort:
                    DestroySpacePort_TellPlayers(Player, -1);
                end;
              end;
              Inc(Flags, chImprovementSold);
            end;
      end;

    sRebuildCityImprovement, sRebuildCityImprovement - sExecute:
      begin
        OldImp := Integer(Data);
{$IFDEF TEXTLOG}CmdInfo := Format('RebuildCityImprovement P%d Loc%d: %d', [Player, RW[Player].City[Subject].Loc, OldImp]); {$ENDIF}
        if (Subject >= RW[Player].nCity) or (RW[Player].City[Subject].Loc < 0)
        then
          Result := eInvalid
        else
        begin
          if (OldImp < 0) or (OldImp >= nImp) or
            not(Imp[OldImp].Kind in [ikCommon, ikNatLocal, ikNatGlobal]) then
            Result := eInvalid
          else
            with RW[Player].City[Subject] do
              if (built[OldImp] = 0) or (Project and cpImp = 0) or
                not(Imp[Project and cpIndex].Kind in [ikCommon, ikNatLocal,
                ikNatGlobal]) then
                Result := eInvalid
              else if (RW[Player].Government = gAnarchy) or
                (Flags and chCaptured <> 0) then
                Result := eOutOfControl
              else if Flags and chImprovementSold <> 0 then
                Result := eOnlyOnce
              else if Command >= sExecute then
              begin
                Inc(Prod, Imp[OldImp].Cost * BuildCostMod[Difficulty[Player]]
                  div 12 * 2 div 3);
                Project0 := Project0 and not cpCompleted;
                if Project0 and not cpAuto <> Project and not cpAuto then
                  Project0 := Project;
                Prod0 := Prod;
                built[OldImp] := 0;
                if Imp[OldImp].Kind in [ikNatLocal, ikNatGlobal] then
                begin // nat. project lost
                  RW[Player].NatBuilt[OldImp] := 0;
                  case OldImp of
                    imGrWall:
                      GrWallContinent[Player] := -1;
                    imSpacePort:
                      DestroySpacePort_TellPlayers(Player, -1);
                  end;
                end;
                Inc(Flags, chImprovementSold);
              end;
        end;
      end;

    sSetCityTiles, sSetCityTiles - sExecute:
      begin
{$IFDEF TEXTLOG}CmdInfo := Format('SetCityTiles P%d Loc%d: %x', [Player, RW[Player].City[Subject].Loc, Integer(Data)]); {$ENDIF}
        if (Subject >= RW[Player].nCity) or (RW[Player].City[Subject].Loc < 0)
        then
          Result := eInvalid
        else
          Result := SetCityTiles(Player, Subject, Integer(Data),
            Command < sExecute);
      end;

    {
      Client Exclusive Commands
      ____________________________________________________________________
    }
  else
    if Command >= cClientEx then
    begin
{$IFDEF TEXTLOG}CmdInfo := Format('ClientEx%x P%d', [Command, Player]);
      {$ENDIF}
      if ProcessClientData[Player] or (Mode = moPlaying) then
        CallPlayer(Command, Player, Data)
    end
    else
      Result := eUnknown;
  end; { case command }

  // do not log invalid and non-relevant commands
  if Result = eZOC_EnemySpotted then
  begin
    Assert(Mode = moPlaying);
    CL.State := FormerCLState;
    IntServer(sIntDiscoverZOC, Player, 0, ZOCTile);
  end
  else if Result and rEffective = 0 then
    if Mode < moPlaying then
    begin
{$IFDEF TEXTLOG}CmdInfo := Format('***ERROR (%x) ', [Result]) + CmdInfo;
      {$ENDIF}
      LoadOK := False;
    end
    else
    begin
      if logged then
        CL.State := FormerCLState;
      if (Result < rExecuted) and (Command >= sExecute) then
        PutMessage(1 shl 16 + 1, Format('INVALID: %d calls %x (%d)',
          [Player, Command, Subject]));
    end;

  if (Command and (cClientEx or sExecute or sctMask) = sExecute or sctEndClient)
    and (Result >= rExecuted) then
    LastEndClientCommand := Command;
{$IFOPT O-}Dec(nHandoverStack, 2); {$ENDIF}
end;


initialization

FindFirst(ParamStr(0), $21, ExeInfo);
FindClose(ExeInfo);

{$IFOPT O-}nHandoverStack := 0; {$ENDIF}

end.
