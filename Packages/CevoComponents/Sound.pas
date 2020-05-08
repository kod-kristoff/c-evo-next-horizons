unit Sound;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, fgl, FileUtil,
  StringTables, Directories
  {$IFDEF WINDOWS}, MMSystem, Windows{$ENDIF}
  {$IFDEF LINUX}, Messages, Process, AsyncProcess{$ENDIF};

type
  TPlayStyle = (psAsync, psSync);

  { TSoundPlayer }

  TSoundPlayer = class(TForm)
  private
    {$IFDEF WINDOWS}
    PrevWndProc: WNDPROC;
    procedure OnMCI(var m: TMessage); message MM_MCINOTIFY;
  public
    constructor Create(AOwner: TComponent); override;
    {$ENDIF}
  end;

  { TSound }

  TSound = class
  private
    {$IFDEF LINUX}
    PlayCommand: string;
    SoundPlayerAsyncProcess: TAsyncProcess;
    SoundPlayerSyncProcess: TProcess;
    {$ENDIF}
    function GetNonWindowsPlayCommand: string;
  public
    FDeviceID: Word;
    FFileName: string;
    PlayStyle: TPlayStyle;
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Play(HWND: DWORD);
    procedure Stop;
    procedure Reset;
  end;

function PrepareSound(FileName: string): Integer;
procedure PlaySound(FileName: string);
function Play(Item: string; Index: Integer = -1): Boolean;
procedure PreparePlay(Item: string; Index: Integer = -1);

const
  // sound modes
  smOff = 0;
  smOn = 1;
  smOnAlt = 2;

var
  Sounds: TStringTable;
  SoundMode: Integer;
  SoundPlayer: TSoundPlayer;
  SoundList: TFPGObjectList<TSound>;
  PlayingSound: TSound;


implementation

{$R *.lfm}

resourcestring
  SUnableToPlay = 'PlayStyle=%s: Unable to play %s Message:%s';
  SPlayCommandNotWork = 'The play command %s does not work on your system';

constructor TSound.Create(const FileName: string);
{$IFDEF WINDOWS}
var
  OpenParm: TMCI_Open_Parms;
{$ENDIF}
begin
  PlayStyle := psAsync;
  FFileName := FileName;
  {$IFDEF WINDOWS}
  FDeviceID := 0;
  if FileExists(FFileName) then begin
    OpenParm.dwCallback := 0;
    OpenParm.lpstrDeviceType := 'WaveAudio';
    OpenParm.lpstrElementName := PChar(FFileName);
    mciSendCommand(0, MCI_Open, MCI_WAIT or MCI_OPEN_ELEMENT or
      MCI_OPEN_SHAREABLE, DWORD_PTR(@OpenParm));
    FDeviceID := OpenParm.wDeviceID;
  end
  {$ENDIF}
  {$IFDEF LINUX}
  PlayCommand := GetNonWindowsPlayCommand;
  FDeviceID := 1;
  {$ENDIF}
end;

destructor TSound.Destroy;
begin
  {$IFDEF WINDOWS}
  if FDeviceID <> 0 then
    mciSendCommand(FDeviceID, MCI_CLOSE, MCI_WAIT, 0);
  {$ENDIF}
  {$IFDEF LINUX}
  FreeAndNil(SoundPlayerSyncProcess);
  FreeAndNil(SoundPlayerAsyncProcess);
  {$ENDIF}
  inherited Destroy;
end;

function TSound.GetNonWindowsPlayCommand: string;
begin
  Result := '';
  // Try play
  if (FindDefaultExecutablePath('play') <> '') then
    Result := 'play';
  // Try aplay
  if (result = '') then
    if (FindDefaultExecutablePath('aplay') <> '') then
      Result := 'aplay -q';
  // Try paplay
  if (Result = '') then
    if (FindDefaultExecutablePath('paplay') <> '') then
      Result := 'paplay';
  // Try mplayer
  if (Result = '') then
    if (FindDefaultExecutablePath('mplayer') <> '') then
      Result := 'mplayer -really-quiet';
  // Try CMus
  if (Result = '') then
    if (FindDefaultExecutablePath('CMus') <> '') then
      Result := 'CMus';
  // Try pacat
  if (Result = '') then
    if (FindDefaultExecutablePath('pacat') <> '') then
      Result := 'pacat -p';
  // Try ffplay
  if (Result = '') then
    if (FindDefaultExecutablePath('ffplay') <> '') then
      result := 'ffplay -autoexit -nodisp';
  // Try cvlc
  if (Result = '') then
    if (FindDefaultExecutablePath('cvlc') <> '') then
      result := 'cvlc -q --play-and-exit';
  // Try canberra-gtk-play
  if (Result = '') then
    if (FindDefaultExecutablePath('canberra-gtk-play') <> '') then
      Result := 'canberra-gtk-play -c never -f';
  // Try Macintosh command?
  if (Result = '') then
    if (FindDefaultExecutablePath('afplay') <> '') then
      Result := 'afplay';
end;


procedure TSound.Play(HWND: DWORD);
{$IFDEF WINDOWS}
var
  PlayParm: TMCI_Play_Parms;
{$ENDIF}
{$IFDEF LINUX}
var
  L: TStringList;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if FDeviceID <> 0 then
  begin
    PlayParm.dwCallback := HWND;
    mciSendCommand(FDeviceID, MCI_PLAY, MCI_NOTIFY, DWORD_PTR(@PlayParm));
  end
  {$ENDIF}
  {$IFDEF LINUX}
  // How to play in Linux? Use generic Linux commands
  // Use asyncprocess to play sound as SND_ASYNC
  // proceed if we managed to find a valid command
  if PlayCommand <> '' then begin
    L := TStringList.Create;
    try
      L.Delimiter := ' ';
      L.DelimitedText := PlayCommand;
      if PlayStyle = psASync then begin
        if SoundPlayerAsyncProcess = nil then
          SoundPlayerAsyncProcess := TAsyncProcess.Create(nil);
        SoundPlayerAsyncProcess.CurrentDirectory := ExtractFileDir(FFilename);
        SoundPlayerAsyncProcess.Executable := FindDefaultExecutablePath(L[0]);
        SoundPlayerAsyncProcess.Parameters.Clear;
        for I := 1 to L.Count - 1 do
          SoundPlayerAsyncProcess.Parameters.Add(L[I]);
        SoundPlayerAsyncProcess.Parameters.Add(FFilename);
        try
          SoundPlayerAsyncProcess.Execute;
        except
          On E: Exception do
            E.CreateFmt(SUnableToPlay, ['paASync', FFilename, E.Message]);
        end;
        PlayingSound := nil;
      end else begin
        if SoundPlayerSyncProcess = nil then
          SoundPlayerSyncProcess := TProcess.Create(nil);
        SoundPlayerSyncProcess.CurrentDirectory := ExtractFileDir(FFilename);
        SoundPlayerSyncProcess.Executable := FindDefaultExecutablePath(L[0]);
        SoundPlayersyncProcess.Parameters.Clear;
        for I := 1 to L.Count - 1 do
          SoundPlayerSyncProcess.Parameters.Add(L[I]);
        SoundPlayerSyncProcess.Parameters.Add(FFilename);
        try
          SoundPlayerSyncProcess.Execute;
          SoundPlayersyncProcess.WaitOnExit;
        except
          On E: Exception do
            E.CreateFmt(SUnableToPlay, ['paSync', FFilename, E.Message]);
        end;
        PlayingSound := nil;
      end;
    finally
      L.Free;
    end;
  end
  else
    raise Exception.CreateFmt(SPlayCommandNotWork, [PlayCommand]);
  {$ENDIF}
end;

procedure TSound.Stop;
begin
  {$IFDEF WINDOWS}
  mciSendCommand(FDeviceID, MCI_STOP, 0, 0);
  {$ENDIF}
  {$IFDEF LINUX}
  if SoundPlayerSyncProcess <> nil then SoundPlayerSyncProcess.Terminate(1);
  if SoundPlayerAsyncProcess <> nil then SoundPlayerAsyncProcess.Terminate(1);
  {$ENDIF}
end;

procedure TSound.Reset;
begin
  {$IFDEF WINDOWS}
  mciSendCommand(FDeviceID, MCI_SEEK, MCI_SEEK_TO_START, 0);
  {$ENDIF}
end;

{$IFDEF WINDOWS}
function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
var
  Message: TMessage;
begin
  if (uMsg = MM_MCINOTIFY) then begin
    Message.msg := uMsg;
    Message.wParam := wParam;
    Message.lParam := lParam;
    SoundPlayer.OnMCI(Message);
  end;
  Result := CallWindowProc(SoundPlayer.PrevWndProc, Ahwnd, uMsg, WParam, LParam);
end;

procedure TSoundPlayer.OnMCI(var m: TMessage);
begin
  if (m.wParam = MCI_NOTIFY_SUCCESSFUL) and (PlayingSound <> nil) then
  begin
    PlayingSound.Reset;
    PlayingSound := nil;
  end;
end;

constructor TSoundPlayer.Create(AOwner: TComponent);
begin
  inherited;
  // MM_MCINOTIFY is not handled by LCL, fallback to low lever handling
  // https://wiki.lazarus.freepascal.org/Win32/64_Interface#Processing_non-user_messages_in_your_window
  PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback)));
end;
{$ENDIF}

function PrepareSound(FileName: string): Integer;
begin
  Result := 0;
  while (Result < SoundList.Count) and (SoundList[result].FFileName <> FileName) do
    Inc(Result);
  if Result = SoundList.Count then begin
    // First time this sound is played
    SoundList.Add(TSound.Create(FileName));
    Result := SoundList.Count - 1;
  end;
end;

procedure PlaySound(FileName: string);
begin
  if PlayingSound <> nil then Exit;
  if SoundPlayer = nil then
    Application.CreateForm(TSoundPlayer, SoundPlayer);
  PlayingSound := SoundList[PrepareSound(FileName)];
  if PlayingSound.FDeviceID = 0 then
    PlayingSound := nil
  else
    PlayingSound.Play(SoundPlayer.Handle);
end;

function Play(Item: string; Index: Integer = -1): Boolean;
var
  WavFileName: string;
begin
  Result := False;
  if (Sounds = nil) or (SoundMode = smOff) or (Item = '') then
  begin
    Result := True;
    Exit;
  end;
  WavFileName := Sounds.Lookup(Item, Index);
  Assert(WavFileName[1] <> '[');
  Result := (WavFileName <> '') and (WavFileName[1] <> '[') and (WavFileName <> '*');
  if Result then
    // SndPlaySound(pchar(GetSoundsDir + DirectorySeparator + WavFileName + '.wav'), SND_ASYNC)
    PlaySound(GetSoundsDir + DirectorySeparator + WavFileName);
end;

procedure PreparePlay(Item: string; Index: Integer = -1);
var
  WavFileName: string;
begin
  if (Sounds = nil) or (SoundMode = smOff) or (Item = '') then
    Exit;
  WavFileName := Sounds.Lookup(Item, Index);
  Assert(WavFileName[1] <> '[');
  if (WavFileName <> '') and (WavFileName[1] <> '[') and (WavFileName <> '*') then
    PrepareSound(GetSoundsDir + DirectorySeparator + WavFileName);
end;

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
  if Sounds <> nil then
    FreeAndNil(Sounds);
end;

initialization

UnitInit;

finalization

UnitDone;

end.
