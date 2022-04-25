unit UNetworkCommon;

interface

uses
  Classes, SysUtils;

procedure StreamAppend(Stream: TStream; SourceStream: TStream);
procedure StreamRemoveRead(Stream: TMemoryStream);


implementation

procedure StreamAppend(Stream: TStream; SourceStream: TStream);
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

procedure StreamRemoveRead(Stream: TMemoryStream);
begin
  // Remove already read data from start of memory stream
  Move(PByte(Stream.Memory + Stream.Position)^, Stream.Memory^, Stream.Size - Stream.Position);
  Stream.SetSize(Stream.Size - Stream.Position);
end;

end.

