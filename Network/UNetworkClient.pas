unit UNetworkClient;

{$mode delphi}

interface

uses
  Classes, SysUtils, fpsock, fpAsync, Protocol;


procedure Client(Command, Player: Integer; var Data); stdcall;


implementation

uses
  LocalPlayer, Global;

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
    procedure AppendStream(Stream: TStream; SourceStream: TStream);
    procedure DataAvailableExecute(Sender: TObject);
    procedure ConnectionStateChangeExecute(Sender: TClientConnectionSocket;
      OldState, NewState: TConnectionState);
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

procedure Client(Command, Player: Integer; var Data);
var
  Cmd: TCommand;
begin
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
end;

{ TTCPClientThread }

procedure TTCPClientThread.Execute;
begin
  TCPClient.EventLoop.Run;
end;

{ TNetworkClient }

procedure TNetworkClient.AppendStream(Stream: TStream; SourceStream: TStream);
var
  Buffer: array of Byte;
  ReadCount: Integer;
  Base: Integer;
const
  ChunkSize = 4096;
begin
  SetLength(Buffer, 0);
  Base := 0;
  repeat
    SetLength(Buffer, Length(Buffer) + ChunkSize);
    ReadCount := SourceStream.Read(Buffer[Base], ChunkSize);
    Inc(Base, ReadCount);
    SetLength(Buffer, Base);
  until ReadCount < ChunkSize;

  if Length(Buffer) > 0 then begin
    Stream.Position := Stream.Size;
    Stream.Write(Buffer[0], Length(Buffer));
  end;
end;

procedure TNetworkClient.DataAvailableExecute(Sender: TObject);
var
  Command: Integer;
  Player: Integer;
  Data: array of Byte;
begin
  AppendStream(ReceiveBuffer, TCPClient.Stream);
  ReceiveBuffer.Position := 0;
  Command := Integer(ReceiveBuffer.ReadDWord);
  Player := Integer(ReceiveBuffer.ReadDWord);
  SetLength(Data, GetCommandDataSize(TCommand(Command)));
  if Length(Data) > 0 then
    LocalClient(Command, Player, Data[0])
    else LocalClient(Command, Player, nil^);

  // Remove already read data from start of memory stream
  Move(PByte(ReceiveBuffer.Memory + ReceiveBuffer.Position)^, ReceiveBuffer.Memory^, ReceiveBuffer.Size - ReceiveBuffer.Position);
  ReceiveBuffer.SetSize(ReceiveBuffer.Size - ReceiveBuffer.Position);

  ClientEventLoop.ClearDataAvailableNotify(DataAvailableHandle);
  DataAvailableHandle := ClientEventLoop.SetDataAvailableNotify(TCPClient.Stream.Handle, DataAvailableExecute, nil);
end;

procedure TNetworkClient.ConnectionStateChangeExecute(
  Sender: TClientConnectionSocket; OldState, NewState: TConnectionState);
begin
  if NewState = connConnected then
    DataAvailableHandle := ClientEventLoop.SetDataAvailableNotify(TCPClient.Stream.Handle, DataAvailableExecute, nil);
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

end.

