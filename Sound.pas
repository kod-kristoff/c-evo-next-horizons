unit Sound;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, fgl
  {$IFDEF WINDOWS}, MMSystem, Windows{$ENDIF};

function PrepareSound(FileName: string): integer;
procedure PlaySound(FileName: string);

type
  TSoundPlayer = class(TForm)
  private
    {$IFDEF WINDOWS}
    procedure OnMCI(var m: TMessage); message MM_MCINOTIFY;
    {$ENDIF}
  end;

implementation

{$R *.lfm}

type
  TSound = class
  public
    FDeviceID: word;
    FFileName: string;
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Play(HWND: DWORD);
    procedure Stop;
    procedure Reset;
  end;

constructor TSound.Create(const FileName: string);
{$IFDEF WINDOWS}
var
  OpenParm: TMCI_Open_Parms;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  FDeviceID := 0;
  FFileName := FileName;
  if FileExists(FFileName) then
  begin
    OpenParm.dwCallback := 0;
    OpenParm.lpstrDeviceType := 'WaveAudio';
    OpenParm.lpstrElementName := PChar(FFileName);
    mciSendCommand(0, MCI_Open, MCI_WAIT or MCI_OPEN_ELEMENT or
      MCI_OPEN_SHAREABLE, integer(@OpenParm));
    FDeviceID := OpenParm.wDeviceID;
  end
  {$ENDIF}
end;

destructor TSound.Destroy;
begin
  {$IFDEF WINDOWS}
  if FDeviceID <> 0 then
    mciSendCommand(FDeviceID, MCI_CLOSE, MCI_WAIT, 0);
  {$ENDIF}
  inherited Destroy;
end;

procedure TSound.Play(HWND: DWORD);
{$IFDEF WINDOWS}
var
  PlayParm: TMCI_Play_Parms;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if FDeviceID <> 0 then
  begin
    PlayParm.dwCallback := HWND;
    mciSendCommand(FDeviceID, MCI_PLAY, MCI_NOTIFY, DWORD_PTR(@PlayParm));
  end
  {$ENDIF}
end;

procedure TSound.Stop;
begin
  {$IFDEF WINDOWS}
  mciSendCommand(FDeviceID, MCI_STOP, 0, 0);
  {$ENDIF}
end;

procedure TSound.Reset;
begin
  {$IFDEF WINDOWS}
  mciSendCommand(FDeviceID, MCI_SEEK, MCI_SEEK_TO_START, 0);
  {$ENDIF}
end;


var
  SoundPlayer: TSoundPlayer;
  SoundList: TFPGObjectList<TSound>;
  PlayingSound: TSound;

{$IFDEF WINDOWS}
procedure TSoundPlayer.OnMCI(var m: TMessage);
begin
  if (m.wParam = MCI_Notify_Successful) and (PlayingSound <> nil) then
  begin
    PlayingSound.Reset;
    PlayingSound := nil;
  end;
end;
{$ENDIF}

function PrepareSound(FileName: string): integer;
begin
  Result := 0;
  while (result < SoundList.Count) and (SoundList[result].FFileName <> FileName) do
    inc(result);
  if result = SoundList.Count then begin
    // first time this sound is played
    SoundList.Add(TSound.Create(FileName));
    Result := SoundList.Count - 1;
  end;
end;

procedure PlaySound(FileName: string);
begin
  if PlayingSound <> nil then
    exit;
  if SoundPlayer = nil then
    Application.CreateForm(TSoundPlayer, SoundPlayer);
  PlayingSound := SoundList[PrepareSound(FileName)];
  if PlayingSound.FDeviceID = 0 then
    PlayingSound := nil
  else
    PlayingSound.Play(SoundPlayer.Handle);
end;

var
  i: integer;

procedure UnitInit;
begin
  SoundList := TFPGObjectList<TSound>.Create;
  PlayingSound := nil;
  SoundPlayer := nil;
end;

procedure UnitDone;
begin
  if PlayingSound <> nil then begin
    PlayingSound.Stop;
    Sleep(222);
  end;
  FreeAndNil(SoundList);
end;

initialization

UnitInit;

finalization

UnitDone;

end.
