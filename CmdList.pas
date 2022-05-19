{$INCLUDE Switches.inc}
unit CmdList;

interface

uses
  Classes, SysUtils, Math;

const
  MaxDataSize = 1024;
  CommandDataElementSize = 4;
  CommandDataElementCountMask = $F;
  CommandDataMaxSize = CommandDataElementSize * CommandDataElementCountMask;

type
  TLogData = array [0 .. 999999999] of Byte;

  TCmdListState = record
    nLog: Integer; { used size of LogData in bytes }
    LoadPos: Integer; { position in LogData when loading a game }
    LastMovingUnit: Integer;
    MoveCode: Cardinal;
    LoadMoveCode: Cardinal;
  end;

  TCmdList = class
    constructor Create;
    destructor Destroy; override;
    procedure Get(var Command, Player, Subject: Integer; var Data: Pointer);
    procedure GetDataChanges(Data: Pointer; DataSize: Integer);
    procedure Put(Command, Player, Subject: Integer; Data: Pointer);
    procedure PutDataChanges(Command, Player: Integer;
      OldData, NewData: Pointer; DataSize: Integer);
    procedure LoadFromFile(const F: TFileStream);
    procedure SaveToFile(const F: TFileStream);
    procedure AppendToFile(const F: TFileStream; const OldState: TCmdListState);
    procedure Cut;
    function Progress: Integer;
  private
    LogAlloc: Integer; { allocated size of LogData in bytes }
    LogData: ^TLogData;
    FState: TCmdListState;
    procedure PutData(Data: Pointer; Length: Integer);
    procedure CompleteMoveCode;
  public
    property State: TCmdListState read FState write FState;
  end;

  function CommandWithData(Command: Integer; DataSize: Byte): Integer;

resourcestring
  SCommandDataSizeError = 'Command data size %d out of range (0-%d).';


implementation

uses
  Protocol;

const
  LogGrow = 1 shl 18;

type
  TData = array [0 .. MaxDataSize - 1] of Cardinal;
  PData = ^TData;

function CommandWithData(Command: Integer; DataSize: Byte): Integer;
var
  DataElementCount: Byte;
begin
  if DataSize > CommandDataMaxSize then
    raise Exception.Create(Format(SCommandDataSizeError, [DataSize, CommandDataMaxSize]));
  DataElementCount := Ceil(DataSize / CommandDataElementSize);
  Result := Command or (DataElementCount and CommandDataElementCountMask);
end;

constructor TCmdList.Create;
begin
  inherited;
  FState.nLog := 0;
  LogAlloc := 0;
  LogData := nil;
  FState.LastMovingUnit := -1;
  FState.MoveCode := 0;
  FState.LoadMoveCode := 0;
end;

destructor TCmdList.Destroy;
begin
  ReallocMem(LogData, 0);
  inherited;
end;

procedure TCmdList.Get(var Command, Player, Subject: Integer; var Data: Pointer);
var
  DirCode: Cardinal;
  Code: Cardinal;
begin
  if FState.LoadMoveCode > 0 then
  begin
    Player := -1;
    if FState.LoadMoveCode and 1 = 1 then
    begin // FM
      DirCode := FState.LoadMoveCode shr 1 and 7;
      Subject := FState.LastMovingUnit;
      FState.LoadMoveCode := FState.LoadMoveCode shr 4;
    end
    else
    begin // M
      DirCode := FState.LoadMoveCode shr 3 and 7;
      Subject := FState.LoadMoveCode shr 6 and $FFF;
      FState.LoadMoveCode := FState.LoadMoveCode shr 18;
      FState.LastMovingUnit := Subject
    end;
    case DirCode of
      0: Command := sMoveUnit + $090;
      1: Command := sMoveUnit + $0F0;
      2: Command := sMoveUnit + $390;
      3: Command := sMoveUnit + $3F0;
      4: Command := sMoveUnit + $020;
      5: Command := sMoveUnit + $060;
      6: Command := sMoveUnit + $100;
      7: Command := sMoveUnit + $300;
    end;
    Data := nil;
  end
  else
  begin
    Code := Cardinal((@LogData[FState.LoadPos])^);
    if Code and 3 = 0 then
    begin // non-clientex command
      Command := Code shr 2 and $3FFF + sExecute;
      Player := Code shr 16 and $F;
      Subject := Code shr 20 and $FFF;
      Inc(FState.LoadPos, 4);
    end
    else if Code and 7 = 2 then
    begin // clientex command
      Command := Code shr 3 and $FFFF;
      Player := Code shr 19 and $F;
      Subject := 0;
      Inc(FState.LoadPos, 3);
    end
    else
    begin // move command shortcut
      if (Code and 1 = 1) and (Code and (7 shl 4) <> 6 shl 4) then
      begin
        FState.LoadMoveCode := Code and $FF;
        Inc(FState.LoadPos);
      end
      else
      begin
        FState.LoadMoveCode := Code and $FFFFFF;
        Inc(FState.LoadPos, 3);
      end;
      Get(Command, Player, Subject, Data);
      Exit;
    end;

    if Command and CommandDataElementCountMask = 0 then
      Data := nil
    else
    begin
      Data := @LogData[FState.LoadPos];
      Inc(FState.LoadPos, Command and CommandDataElementCountMask * CommandDataElementSize);
    end;
  end;
end;

procedure TCmdList.GetDataChanges(Data: Pointer; DataSize: Integer);
var
  b0, b1: Integer;
  Map0, Map1: Cardinal;
begin
  Map0 := Cardinal((@LogData[FState.LoadPos])^);
  Inc(FState.LoadPos, 4);
  b0 := 0;
  while Map0 > 0 do begin
    if Map0 and 1 <> 0 then begin
      Map1 := Cardinal((@LogData[FState.LoadPos])^);
      Inc(FState.LoadPos, 4);
      for b1 := 0 to 31 do
        if 1 shl b1 and Map1 <> 0 then begin
          if b0 * 32 + b1 < DataSize then
            PData(Data)[b0 * 32 + b1] := Cardinal((@LogData[FState.LoadPos])^);
          Inc(FState.LoadPos, 4);
        end;
    end;
    Inc(b0);
    Map0 := Map0 shr 1;
  end;
end;

procedure TCmdList.Put(Command, Player, Subject: Integer; Data: Pointer);
var
  DirCode, Code: Cardinal;
begin
  if Command and $FC00 = sMoveUnit then
  begin // move command shortcut
    case Command of
      sMoveUnit + $090: DirCode := 0;
      sMoveUnit + $0F0: DirCode := 1;
      sMoveUnit + $390: DirCode := 2;
      sMoveUnit + $3F0: DirCode := 3;
      sMoveUnit + $020: DirCode := 4;
      sMoveUnit + $060: DirCode := 5;
      sMoveUnit + $100: DirCode := 6;
      sMoveUnit + $300: DirCode := 7;
    end;
    if Subject = FState.LastMovingUnit then
      Code := 1 + DirCode shl 1
    else
      Code := 6 + DirCode shl 3 + Cardinal(Subject) shl 6;
    if FState.MoveCode = 0 then
      FState.MoveCode := Code
    else if FState.MoveCode and 1 = 1 then
    begin // FM + this
      FState.MoveCode := FState.MoveCode + Code shl 4;
      if Code and 1 = 1 then
        PutData(@FState.MoveCode, 1) // FM + FM
      else
        PutData(@FState.MoveCode, 3); // FM + M
      FState.MoveCode := 0;
    end
    else if Code and 1 = 1 then
    begin // M + FM
      FState.MoveCode := FState.MoveCode + Code shl 18;
      PutData(@FState.MoveCode, 3);
      FState.MoveCode := 0;
    end
    else // M + M
    begin
      PutData(@FState.MoveCode, 3);
      FState.MoveCode := Code;
    end;
    FState.LastMovingUnit := Subject;
  end
  else
  begin
    CompleteMoveCode;
    if Command >= cClientEx then
    begin
      Code := 2 + Command shl 3 + Player shl 19;
      PutData(@Code, 3);
    end
    else
    begin
      Code := Cardinal(Command - sExecute) shl 2 + Cardinal(Player) shl 16 +
        Cardinal(Subject) shl 20;
      PutData(@Code, 4);
    end;
  end;
  if Command and CommandDataElementCountMask <> 0 then
    PutData(Data, Command and CommandDataElementCountMask * CommandDataElementSize);
end;

procedure TCmdList.PutDataChanges(Command, Player: Integer;
  OldData, NewData: Pointer; DataSize: Integer);
var
  MapPos, LogPos, b0, b1, RowEnd: Integer;
  Map0, Map1, Code: Cardinal;
begin
  if DataSize <= 0 then
    Exit;
  if DataSize > MaxDataSize then
    DataSize := MaxDataSize;
  CompleteMoveCode;
  MapPos := FState.nLog + 8;
  LogPos := MapPos + 4;
  Map0 := 0;
  for b0 := 0 to (DataSize - 1) div 32 do
  begin
    if LogPos + 4 * 32 > LogAlloc then
    begin
      Inc(LogAlloc, LogGrow);
      ReallocMem(LogData, LogAlloc);
    end;
    Map0 := Map0 shr 1;
    Map1 := 0;
    RowEnd := DataSize - 1;
    if RowEnd > b0 * 32 + 31 then
      RowEnd := b0 * 32 + 31;
    for b1 := b0 * 32 to RowEnd do
    begin
      Map1 := Map1 shr 1;
      if PData(NewData)[b1] <> PData(OldData)[b1] then
      begin
        Cardinal((@LogData[LogPos])^) := PData(NewData)[b1];
        Inc(LogPos, 4);
        Inc(Map1, $80000000);
      end;
    end;
    if Map1 > 0 then
    begin
      Map1 := Map1 shr (b0 * 32 + 31 - RowEnd);
      Cardinal((@LogData[MapPos])^) := Map1;
      MapPos := LogPos;
      Inc(LogPos, 4);
      Inc(Map0, $80000000);
    end;
  end;
  if Map0 = 0 then
    Exit; // no changes

  Map0 := Map0 shr (31 - (DataSize - 1) div 32);
  Cardinal((@LogData[FState.nLog + 4])^) := Map0;
  Code := Cardinal(Command - sExecute) shl 2 + Cardinal(Player) shl 16;
  Cardinal((@LogData[FState.nLog])^) := Code;
  FState.nLog := MapPos;
end;

procedure TCmdList.PutData(Data: Pointer; Length: Integer);
begin
  if FState.nLog + Length > LogAlloc then
  begin
    Inc(LogAlloc, LogGrow);
    ReallocMem(LogData, LogAlloc);
  end;
  Move(Data^, LogData[FState.nLog], Length);
  Inc(FState.nLog, Length);
end;

procedure TCmdList.CompleteMoveCode;
begin
  if FState.MoveCode > 0 then
  begin
    if FState.MoveCode and 1 = 1 then
      PutData(@FState.MoveCode, 1) // Single FM
    else
      PutData(@FState.MoveCode, 3); // Single M
    FState.MoveCode := 0;
  end;
end;

procedure TCmdList.LoadFromFile(const F: TFileStream);
begin
  F.read(FState.nLog, 4);
  LogData := nil;
  LogAlloc := ((FState.nLog + 2) div LogGrow + 1) * LogGrow;
  ReallocMem(LogData, LogAlloc);
  F.read(LogData^, FState.nLog);
  FState.LoadPos := 0;
end;

procedure TCmdList.SaveToFile(const F: TFileStream);
begin
  CompleteMoveCode;
  F.write(FState.nLog, 4);
  F.write(LogData^, FState.nLog);
end;

procedure TCmdList.AppendToFile(const F: TFileStream;
  const OldState: TCmdListState);
begin
  CompleteMoveCode;
  F.write(FState.nLog, 4);
  F.Position := F.Position + OldState.nLog;
  F.write(LogData[OldState.nLog], FState.nLog - OldState.nLog);
end;

procedure TCmdList.Cut;
begin
  FState.nLog := FState.LoadPos;
end;

function TCmdList.Progress: Integer;
begin
  if (FState.LoadPos = FState.nLog) and (FState.LoadMoveCode = 0) then
    Result := 1000 // loading complete
  else if FState.nLog > 1 shl 20 then
    Result := (FState.LoadPos shr 8) * 999 div (FState.nLog shr 8)
  else
    Result := FState.LoadPos * 999 div FState.nLog;
end;

{ Format Specification:

  Non-ClientEx-Command:
  Byte3    Byte2    Byte1    Byte0
  ssssssss sssspppp cccccccc cccccc00
  (C = Command-sExecute, P = Player, S = Subject)

  ClientEx-Command:
  Byte2    Byte1    Byte0
  0ppppccc cccccccc ccccc010
  (C = Command, P = Player)

  Single Move:
  Byte2    Byte1    Byte0
  000000ss ssssssss ssaaa110
  (A = Direction, S = Subject)

  Move + Follow Move:
  Byte2    Byte1    Byte0
  00bbb1ss ssssssss ssaaa110
  (A = Direction 1, S = Subject 1, B = Direction 2)

  Follow Move + Move:
  Byte2    Byte1    Byte0
  00ssssss ssssssbb b110aaa1
  (A = Direction 1, B = Direction 2, S = Subject 2)

  Single Follow Move:
  Byte0
  0000aaa1
  (A = Direction)

  Double Follow Move:
  Byte0
  bbb1aaa1
  (A = Direction 1, B = Direction 2)
}

end.
