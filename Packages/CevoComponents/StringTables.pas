unit StringTables;

interface

uses
  Classes;

type
  { TStringTable }

  TStringTable = class
  private
    Lines: TStringList;
    function IsLabel(Text, Name: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(const FileName: String): Boolean;
    function GetHandle(const Item: string): Integer;
    function LookupByHandle(Handle: Integer; Index: Integer = -1): string;
    function Lookup(const Item: string; Index: Integer = -1): string;
    function Search(const Content: string; var Handle, Index: Integer): Boolean;
  end;


implementation

uses
  SysUtils;

function TStringTable.IsLabel(Text, Name: string): Boolean;
begin
  Result := ((Copy(Text, 1, 1) = '#') and (Copy(Text, 2, Length(Name)) = Name))
    and ((Length(Text) = (Length(Name) + 1)) or (Copy(Text, Length(Name) + 2, 1) = ' '));
end;

constructor TStringTable.Create;
begin
  Lines := TStringList.Create;
end;

destructor TStringTable.Destroy;
begin
  FreeAndNil(Lines);
end;

function TStringTable.LoadFromFile(const FileName: String): Boolean;
begin
  Result := True;
  Lines.Clear;
  try
    Lines.LoadFromFile(FileName);
  except
    Result := False;
  end;
end;

function TStringTable.GetHandle(const Item: string): Integer;
var
  I: Integer;
begin
  I := 0;
  while (I < Lines.Count) and not IsLabel(Lines[I], Item) do
    Inc(I);
  if I < Lines.Count then Result := I
    else Result := -1;
end;

function TStringTable.LookupByHandle(Handle: Integer; Index: Integer): string;
var
  S: string;
begin
  if Index < 0 then begin
    if Handle < 0 then begin
      Result := '';
      Exit;
    end else begin
      if Pos(' ', Lines[Handle]) = 0 then S := ''
        else S := Copy(Lines[Handle], Pos(' ', Lines[Handle]) + 1, MaxInt);
      while ((Handle + 1) < Lines.Count) and (Copy(Lines[Handle + 1], 1, 1) <> '#') do begin
        Inc(Handle);
        if (Length(Lines[Handle]) > 0) and (Lines[Handle][1] <> '''') then begin
          if (S <> '') and (S[Length(S)] <> '\') then
            S := S + ' ';
          S := S + Lines[Handle];
        end;
      end;
      Result := S;
    end;
  end else
  if (Handle + Index + 1) >= Lines.Count then begin
    Result := '';
    Exit;
  end else Result := Lines[Handle + Index + 1];
  while (Result <> '') and ((Result[1] = ' ') or (Result[1] = #9)) do
    Delete(Result, 1, 1);
  while (Result <> '') and ((Result[Length(Result)] = ' ') or
    (Result[Length(Result)] = #9)) do
    Delete(Result, Length(Result), 1);
  if Result = '' then Result := '*';
end;

function TStringTable.Lookup(const Item: string; Index: Integer): string;
var
  Handle: Integer;
begin
  Handle := GetHandle(Item);
  if Handle >= 0 then Result := LookupByHandle(Handle, Index)
    else Result := '';
  if Result = '' then begin
    if Index < 0 then Result := Format('[%s]', [Item])
      else Result := Format('[%s %d]', [Item, Index]);
  end;
end;

{ might become necessary for 1.3

  function TStringTable.Lookup(const Fallback: TStringTable; const Item: string; Index: Integer): string;
  var
  Handle: Integer;
  begin
  Handle:=Gethandle(Item);
  if Handle>=0 then Result:=LookupByHandle(Handle, Index)
  else Result:='';
  if Result='' then
  Result:=Fallback.Lookup(Item, Index);
  end;

  function TStringTable.TryLookup(const Item: string; Index: Integer): string;
  var
  Handle: Integer;
  begin
  Handle:=Gethandle(Item);
  if Handle>=0 then Result:=LookupByHandle(Handle, Index)
  else Result:='';
  end; }

function TStringTable.Search(const Content: string;
  var Handle, Index: Integer): Boolean;
var
  H, I: Integer;
  UContent: string;
begin
  UContent := UpperCase(Content);
  H := Handle;
  if H < 0 then
    I := 0
  else
    I := Index + 1;
  repeat
    if H + I + 1 >= Lines.Count then
    begin
      Result := False;
      Exit;
    end;
    if Copy(Lines[H + I + 1], 1, 1) = '#' then
    begin
      H := H + I + 1;
      I := -1;
    end;
    if (H >= 0) and not ((Length(Lines[H + I + 1]) > 0) and (Lines[H + I + 1][1] in ['#', ':', ';'])) and
      (Pos(UContent, UpperCase(Lines[H + I + 1])) > 0) then
    begin
      Index := I;
      Handle := H;
      Result := True;
      Exit;
    end;
    Inc(I);
  until False;
end;

end.
