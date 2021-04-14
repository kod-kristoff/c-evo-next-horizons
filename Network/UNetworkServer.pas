unit UNetworkServer;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl{$IFDEF LINUX}, fpAsync, fpsock{$ENDIF}, Protocol, fphttpclient;

{$IFDEF LINUX}
type
  TNetworkServer = class;
  TNetworkServerPlayer = class;

  { TTCPServerThread }

  TTCPServerThread = class(TThread)
    TCPServer: TTCPServer;
    procedure Execute; override;
  end;

  { TNetworkServerConnection }

  TNetworkServerConnection = class
  private
    DataAvailableHandle: Pointer;
    ReceiveBuffer: TMemoryStream;
    procedure DisconnectExecute(Sender: TObject);
    procedure DataAvailableSync;
    procedure DataAvailableExecute(Sender: TObject);
  public
    NetworkServer: TNetworkServer;
    Socket: TSocketStream;
    ServerEventLoop: TEventLoop;
    Player: TNetworkServerPlayer;
    Connected: Boolean;
    procedure Run;
    constructor Create;
    destructor Destroy; override;
  end;

  { TNetworkServerPlayer }

  TNetworkServerPlayer = class
  private
    Buffer: TMemoryStream;
  public
    NetworkServer: TNetworkServer;
    Id: Integer;
    Connection: TNetworkServerConnection;
    constructor Create;
    destructor Destroy; override;
    procedure Client(Command: TCommand; Player: Integer; var Data);
    function Server(Command: TCommand; Player, Subject: Integer; var Data): Integer;
  end;

  { TNetworkServerPlayers }

  TNetworkServerPlayers = class(TFPGObjectList<TNetworkServerPlayer>)
    function SearchById(Id: Integer): TNetworkServerPlayer;
  end;

  { TNetworkServer }

  TNetworkServer = class
  private
    Players: TNetworkServerPlayers;
    Connections: TFPGObjectList<TNetworkServerConnection>;
    TCPServerThread: TTCPServerThread;
    ServerEventLoop: TEventLoop;
    procedure ConnectExecute(Sender: TConnectionBasedSocket; AStream: TSocketStream);
    procedure Client(Command: TCommand; Player: integer; var Data);
  public
    TCPServer: TTCPServer;
    Server: TServerCall;
    constructor Create;
    destructor Destroy; override;
  end;

var
  NetworkServer: TNetworkServer;
{$ENDIF}

procedure Client(Command, Player: integer; var Data); stdcall;


implementation

uses
  Global, UNetworkCommon;

procedure Client(Command, Player: integer; var Data);
begin
  {$IFDEF LINUX}
  if not Assigned(NetworkServer) then begin
    NetworkServer := TNetworkServer.Create;
  end;
  case TCommand(Command) of
    cmReleaseModule: begin
      FreeAndNil(NetworkServer);
    end;
  end;
  NetworkServer.Client(TCommand(Command), Player, Data);
  {$ENDIF}
end;

{$IFDEF LINUX}

{ TNetworkServerPlayers }

function TNetworkServerPlayers.SearchById(Id: Integer): TNetworkServerPlayer;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].Id <> Id) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

{ TNetworkServerPlayer }

constructor TNetworkServerPlayer.Create;
begin
  Buffer := TMemoryStream.Create;
end;

destructor TNetworkServerPlayer.Destroy;
begin
  if Assigned(Connection) then Connection.Player := nil;
  FreeAndNil(Buffer);
  inherited;
end;

procedure TNetworkServerPlayer.Client(Command: TCommand; Player: Integer; var Data);
begin
  if Assigned(Connection) then begin
    with Connection.Socket do begin
      if Buffer.Size > 0 then begin
        Buffer.Position := 0;
        CopyFrom(Buffer, Buffer.Size);
        Buffer.Clear;
      end;
      WriteDWord(DWord(Command));
      WriteDWord(DWord(Player));
      if GetCommandDataSize(Command) > 0 then
        Write(Data, GetCommandDataSize(Command));
    end;
  end else begin
    with Buffer do begin
      WriteDWord(DWord(Command));
      WriteDWord(DWord(Player));
      if GetCommandDataSize(Command) > 0 then
        Write(Data, GetCommandDataSize(Command));
    end;
  end;
end;

function TNetworkServerPlayer.Server(Command: TCommand; Player,
  Subject: Integer; var Data): Integer;
begin
  NetworkServer.Server(Integer(Command), Player, Subject, Data);
end;

{ TNetworkServerConnection }

procedure TNetworkServerConnection.DisconnectExecute(Sender: TObject);
begin
  {Connected := False;
  if Assigned(Player) then begin
    Player.Connection := nil;
    Player := nil;
  end;
  }
end;

procedure TNetworkServerConnection.DataAvailableSync;
var
  Data: array of Byte;
  ReadCount: Integer;
  PlayerIndex: Integer;
  Subject: Integer;
  Command: TCommand;
begin
  StreamAppend(ReceiveBuffer, Socket);
  while ReceiveBuffer.Size >= 3 * SizeOf(Integer) do begin
    ReceiveBuffer.Position := 0;

    Command := TCommand(ReceiveBuffer.ReadDWord);
    PlayerIndex := ReceiveBuffer.ReadDWord;
    Subject := ReceiveBuffer.ReadDWord;
    SetLength(Data, GetCommandDataSize(TCommand(Command)));
    if Length(Data) > 0 then begin
      ReadCount := ReceiveBuffer.Read(Data[0], Length(Data));
      SetLength(Data, ReadCount);
    end;
    if Assigned(Player) then begin
      if Length(Data) > 0 then
        Player.Server(Command, PlayerIndex, Subject, Data[0])
        else Player.Server(Command, PlayerIndex, Subject, nil^);
    end;
    StreamRemoveRead(ReceiveBuffer);
  end;
end;

procedure TNetworkServerConnection.DataAvailableExecute(Sender: TObject);
begin
  NetworkServer.TCPServerThread.Synchronize(NetworkServer.TCPServerThread, DataAvailableSync);
  Sleep(10); // TODO: How to reset this event
end;

procedure TNetworkServerConnection.Run;
begin
  Socket.OnDisconnect := DisconnectExecute;
  DataAvailableHandle := NetworkServer.TCPServer.EventLoop.SetDataAvailableNotify(Socket.Handle, DataAvailableExecute, nil);
  Connected := True;
end;

constructor TNetworkServerConnection.Create;
begin
  ReceiveBuffer := TMemoryStream.Create;
end;

destructor TNetworkServerConnection.Destroy;
begin
  if Assigned(Player) then Player.Connection := nil;
  if Assigned(DataAvailableHandle) then
    NetworkServer.TCPServer.EventLoop.ClearDataAvailableNotify(DataAvailableHandle);
  FreeAndNil(Socket);
  NetworkServer.Connections.Remove(Self);
  FreeAndNil(ReceiveBuffer);
  inherited;
end;

{ TNetworkServer }

procedure TNetworkServer.ConnectExecute(Sender: TConnectionBasedSocket;
  AStream: TSocketStream);
var
  NewConnection: TNetworkServerConnection;
  Player: TNetworkServerPlayer;
  I: Integer;
  InitModuleData: TInitModuleData;
begin
  NewConnection := TNetworkServerConnection.Create;
  NewConnection.Socket := AStream;
  NewConnection.NetworkServer := Self;
  Connections.Add(NewConnection);
  NewConnection.Run;

  // Search for player without connection
  Player := nil;
  for I := 0 to Players.Count - 1 do begin
    if not Assigned(Players[I].Connection) then begin
      Player := Players[I];
      Break;
    end;
  end;
  if Assigned(Player) then begin
    NewConnection.Player := Player;
    Player.Connection := NewConnection;
    Player.Client(cmInitModule, Player.Id, InitModuleData);
  end else AStream.Free;
end;

procedure TNetworkServer.Client(Command: TCommand; Player: integer; var Data);
var
  Cmd: TCommand;
  NewPlayer: TNetworkServerPlayer;
  I: Integer;
  ServerPlayer: TNetworkServerPlayer;
begin
  if Player <> -1 then begin
    NewPlayer := TNetworkServerPlayer.Create;
    NewPlayer.Id := Player;
    NewPlayer.NetworkServer := Self;
    Players.Add(NewPlayer);
  end;

  Cmd := TCommand(Command);
  case Cmd of
    cmInitModule: begin
      TInitModuleData(Data).Flags := aiThreaded;
      Server := TInitModuleData(Data).Server;
    end;
    else begin
      if Player = -1 then begin
        // Send to all
        for I := 0 to NetworkServer.Players.Count - 1 do
          NetworkServer.Players[I].Client(Cmd, Player, Data);
      end else begin
        ServerPlayer := NetworkServer.Players.SearchById(Player);
        if Assigned(ServerPlayer) then
          ServerPlayer.Client(Cmd, Player, Data);
      end;
    end;
  end;
end;

constructor TNetworkServer.Create;
begin
  Players := TNetworkServerPlayers.Create;
  ServerEventLoop := TEventLoop.Create;
  Connections := TFPGObjectList<TNetworkServerConnection>.Create;
  TCPServer := TTCPServer.Create(nil);
  with TCPServer do begin
    EventLoop := ServerEventLoop;
    Port := CevoNetworkPort;
    OnConnect := ConnectExecute;
    Active := True;
  end;
  TCPServerThread := TTCPServerThread.Create(True);
  TCPServerThread.TCPServer := TCPServer;
  TCPServerThread.Start;
end;

destructor TNetworkServer.Destroy;
begin
  ServerEventLoop.Break;
  FreeAndNil(TCPServerThread);
  FreeAndNil(ServerEventLoop);
  FreeAndNil(TCPServer);
  FreeAndNil(Connections);
  FreeAndNil(Players);
  inherited;
end;

{ TTCPServerThread }

procedure TTCPServerThread.Execute;
begin
  TCPServer.EventLoop.Run;
end;

{$ENDIF}

end.

