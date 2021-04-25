unit UNetworkClient;

{$mode delphi}

interface

uses
  Classes, SysUtils{$IFDEF LINUX}, fpAsync, fpsock{$ENDIF}, Protocol;

{$IFDEF LINUX}
type
  { TTCPClientThread }

  TTCPClientThread = class(TThread)
    TCPClient: TTCPClient;
    procedure Execute; override;
  end;

  { TNetworkClient }

  TNetworkClient = class
  private
    DataAvailableHandle: Pointer;
    TCPClientThread: TTCPClientThread;
    ReceiveBuffer: TMemoryStream;
    procedure DataAvailableExecute(Sender: TObject);
    procedure ConnectionStateChangeExecute(Sender: TClientConnectionSocket;
      OldState, NewState: TConnectionState);
    procedure DataAvailableSync;
  public
    AuxServer: TServerCall;
    LocalClient: TClientCall;
    TCPClient: TTCPClient;
    ClientEventLoop: TEventLoop;
    function Server(Command: TCommand; Player, Subject: Integer; var Data): Integer;
    constructor Create;
    destructor Destroy; override;
  end;

var
  NetworkClient: TNetworkClient;
{$ENDIF}

procedure Client(Command, Player: Integer; var Data); stdcall;


implementation

uses
  LocalPlayer{$IFDEF LINUX}, Global, UNetworkCommon{$ENDIF};

procedure Client(Command, Player: Integer; var Data);
{$IFDEF LINUX}
var
  Cmd: TCommand;
{$ENDIF}
begin
  {$IFDEF LINUX}
  Cmd := TCommand(Command);
  case Cmd of
    cmInitModule: begin
      if not Assigned(NetworkClient) then
        NetworkClient := TNetworkClient.Create;
      NetworkClient.AuxServer := TInitModuleData(Data).Server;
      TInitModuleData(Data).Flags := aiThreaded;
      NetworkClient.LocalClient := LocalPlayer.Client;
    end;
    cmReleaseModule: begin
      FreeAndNil(NetworkClient);
    end;
  end;
  {$ENDIF}
end;

{$IFDEF LINUX}
function LocalServer(Command, Player, Subject: Integer; var Data): Integer; stdcall;
begin
  if Assigned(NetworkClient) then
    Result := NetworkClient.Server(TCommand(Command), Player, Subject, Data);
end;

{ TTCPClientThread }

procedure TTCPClientThread.Execute;
begin
  TCPClient.EventLoop.Run;
end;

{ TNetworkClient }

procedure TNetworkClient.DataAvailableExecute(Sender: TObject);
begin
  TCPClientThread.Synchronize(TCPClientThread, DataAvailableSync);

  ClientEventLoop.ClearDataAvailableNotify(DataAvailableHandle);
  DataAvailableHandle := ClientEventLoop.SetDataAvailableNotify(TCPClient.Stream.Handle, DataAvailableExecute, nil);
end;

procedure TNetworkClient.ConnectionStateChangeExecute(
  Sender: TClientConnectionSocket; OldState, NewState: TConnectionState);
begin
  if NewState = connConnected then
    DataAvailableHandle := ClientEventLoop.SetDataAvailableNotify(TCPClient.Stream.Handle, DataAvailableExecute, nil);
end;

procedure TNetworkClient.DataAvailableSync;
var
  Command: Integer;
  ReadCount: Integer;
  Player: Integer;
  Data: array of Byte;
begin
  StreamAppend(ReceiveBuffer, TCPClient.Stream);
  while ReceiveBuffer.Size >= 2 * SizeOf(Integer) do begin
    ReceiveBuffer.Position := 0;
    Command := Integer(ReceiveBuffer.ReadDWord);
    Player := Integer(ReceiveBuffer.ReadDWord);
    SetLength(Data, GetCommandDataSize(TCommand(Command)));
    if Length(Data) > 0 then begin
      ReadCount := ReceiveBuffer.Read(Data[0], Length(Data));
      SetLength(Data, ReadCount);
    end;

    // Rewrite server address received from network by local handler
    if Command = cInitModule then begin
      PInitModuleData(@Data[0])^.Server := LocalServer;
    end;

    if Length(Data) > 0 then
      LocalClient(Command, Player, Data[0])
      else LocalClient(Command, Player, nil^);

    StreamRemoveRead(ReceiveBuffer);
  end;
end;

function TNetworkClient.Server(Command: TCommand; Player, Subject: Integer;
  var Data): Integer;
begin
  with TCPClient.Stream do begin
    WriteDWord(DWord(Command));
    WriteDWord(Player);
    WriteDWord(Subject);
    if GetCommandDataSize(Command) > 0 then
      Write(Data, GetCommandDataSize(Command));
    Result := ReadDWord;
  end;
end;

constructor TNetworkClient.Create;
begin
  ReceiveBuffer := TMemoryStream.Create;
  ClientEventLoop := TEventLoop.Create;
  TCPClient := TTCPClient.Create(nil);
  with TCPClient do begin
    EventLoop := ClientEventLoop;
    Host := '127.0.0.1';
    Port := CevoNetworkPort;
    OnConnectionStateChange := ConnectionStateChangeExecute;
    Active := True;
  end;
  TCPClientThread := TTCPClientThread.Create(True);
  TCPClientThread.TCPClient := TCPClient;
  TCPClientThread.Start;
end;

destructor TNetworkClient.Destroy;
begin
  ClientEventLoop.Break;
  FreeAndNil(TCPClientThread);
  FreeAndNil(TCPClient);
  FreeAndNil(ClientEventLoop);
  FreeAndNil(ReceiveBuffer);
  inherited;
end;
{$ENDIF}

end.

