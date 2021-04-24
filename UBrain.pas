unit UBrain;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, Graphics, Protocol, LazFileUtils, dynlibs;

const
  // module flags
  fMultiple = $10000000;
  fDotNet = $20000000;
  fUsed = $40000000;

type
  TBrainType = (btNoTerm, btSuperVirtual, btTerm, btRandom, btAI, btNetworkServer,
    btNetworkClient);

  { TBrain }

  TBrain = class
    FileName: string;
    DLLName: string;
    Name: string;
    Credits: string; { filename and full name }
    hm: TLibHandle; { module handle }
    Flags: Integer;
    ServerVersion: Integer;
    DataVersion: Integer;
    DataSize: Integer;
    Client: TClientCall; { client function address }
    Initialized: Boolean;
    Kind: TBrainType;
    Picture: TBitmap;
    Beginner: Boolean;
    procedure LoadFromFile(AIFileName: string);
    constructor Create;
    destructor Destroy; override;
  end;

  { TBrains }

  TBrains = class(TFPGObjectList<TBrain>)
    function AddNew: TBrain;
    function GetKindCount(Kind: TBrainType): Integer;
    procedure GetByKind(Kind: TBrainType; Brains: TBrains);
    function GetBeginner: TBrain;
  end;


implementation

uses
  ScreenTools;

{ TBrain }

procedure TBrain.LoadFromFile(AIFileName: string);
var
  T: Text;
  Key: string;
  Value: string;
  S: string;
  BasePath: string;
  I: Integer;
begin
  BasePath := ExtractFileDir(AIFileName);
  FileName := ExtractFileName(ExtractFileNameWithoutExt(ExtractFileNameWithoutExt(AIFileName)));
  Name := FileName;
  DLLName := BasePath + DirectorySeparator + Name + '.dll';
  Credits := '';
  Flags := fMultiple;
  Client := nil;
  Initialized := false;
  ServerVersion := 0;
  if not FileExists(AIFileName) then
    raise Exception.Create(Format('AI specification file %s not found', [AIFileName]));
  AssignFile(T, AIFileName);
  Reset(T);
  while not EOF(T) do
  begin
    ReadLn(T, s);
    s := trim(s);
    if Pos(' ', S) > 0 then begin
      Key := Copy(S, 1, Pos(' ', S) - 1);
      Value := Trim(Copy(S, Pos(' ', S) + 1, Length(S)));
    end else begin
      Key := S;
      Value := '';
    end;
    if Key = '#NAME' then
      Name := Value
    else if Key = '#.NET' then
      Flags := Flags or fDotNet
    else if Key = '#BEGINNER' then
      Beginner := True
    else if Key = '#PATH' then
      DLLName := BasePath + DirectorySeparator + Value
    {$IFDEF WINDOWS}{$IFDEF CPU32}
    else if Key = '#PATH_WIN32' then
      DLLName := BasePath + DirectorySeparator + Value
    {$ENDIF}{$ENDIF}
    {$IFDEF WINDOWS}{$IFDEF CPU64}
    else if Key = '#PATH_WIN64' then
      DLLName := BasePath + DirectorySeparator + Value
    {$ENDIF}{$ENDIF}
    {$IFDEF LINUX}{$IFDEF CPU32}
    else if Key = '#PATH_LINUX32' then
      DLLName := BasePath + DirectorySeparator + Value
    {$ENDIF}{$ENDIF}
    {$IFDEF LINUX}{$IFDEF CPU64}
    else if Key = '#PATH_LINUX64' then
      DLLName := BasePath + DirectorySeparator + Value
    {$ENDIF}{$ENDIF}
    else if Key = '#GAMEVERSION' then
      for i := 1 to Length(Value) do
        case Value[i] of
          '0' .. '9':
            ServerVersion := ServerVersion and $FFFF00 + ServerVersion and
              $FF * 10 + ord(Value[i]) - 48;
          '.':
          ServerVersion := ServerVersion shl 8;
      end
    else if Key = '#CREDITS' then
      Credits := Value;
  end;
  CloseFile(T);
end;

constructor TBrain.Create;
begin
  Picture := TBitmap.Create;
  Picture.SetSize(64, 64);
end;

destructor TBrain.Destroy;
begin
  FreeAndNil(Picture);
  inherited;
end;

{ TBrains }

function TBrains.AddNew: TBrain;
begin
  Result := TBrain.Create;
  Add(Result);
end;

function TBrains.GetKindCount(Kind: TBrainType): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Kind = Kind then Inc(Result);
end;

procedure TBrains.GetByKind(Kind: TBrainType; Brains: TBrains);
var
  I: Integer;
begin
  Brains.Clear;
  for I := 0 to Count - 1 do
    if Items[I].Kind = Kind then Brains.Add(Items[I]);
end;

function TBrains.GetBeginner: TBrain;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and not Items[I].Beginner do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

end.

