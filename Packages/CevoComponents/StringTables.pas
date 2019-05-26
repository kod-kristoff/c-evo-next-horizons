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
    function LoadFromFile(const FileName: String): boolean;
    function GetHandle(const Item: AnsiString): integer;
    function LookupByHandle(Handle: integer; Index: integer = -1): string;
    function Lookup(const Item: string; Index: integer = -1): string;
    function Search(const Content: string; var Handle, Index: integer): boolean;
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

function TStringTable.LoadFromFile(const FileName: String): boolean;
begin
  Result := True;
  Lines.Clear;
  try
    Lines.LoadFromFile(FileName);
  except
    Result := False;
  end;
end;

function TStringTable.GetHandle(const Item: AnsiString): integer;
var
  I: Integer;
begin
  I := 0;
  while (I < Lines.Count) and not IsLabel(Lines[I], Item) do
    Inc(I);
  if I < Lines.Count then Result := I
    else Result := -1;
end;

function TStringTable.LookupByHandle(Handle: integer; Index: integer): string;
var
  s: string;
begin
  if Index < 0 then
    if Handle < 0 then begin
      Result := '';
      Exit;
    end else begin
      if Pos(' ', Lines[Handle]) = 0 then S := ''
        else s := Copy(Lines[Handle], Pos(' ', Lines[Handle]) + 1, MaxInt);
      while ((Handle + 1) < Lines.Count) and (Copy(Lines[Handle + 1], 1, 1) <> '#') do begin
        Inc(Handle);
        if (Length(Lines[Handle]) > 0) and (Lines[Handle][1] <> '''') then begin
          if (s <> '') and (s[Length(s)] <> '\') then
            s := s + ' ';
          s := s + Lines[Handle];
        end;
      end;
      Result := S;
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

function TStringTable.Lookup(const Item: string; Index: integer): string;
var
  Handle: integer;
begin
  Handle := GetHandle(Item);
  if Handle >= 0 then
    result := LookupByHandle(Handle, Index)
  else
    result := '';
  if result = '' then
    if Index < 0 then
      result := Format('[%s]', [Item])
    else
      result := Format('[%s %d]', [Item, Index])
end;

{ might become necessary for 1.3

  function TStringTable.Lookup(const Fallback: TStringTable; const Item: string; Index: integer): string;
  var
  Handle: integer;
  begin
  Handle:=Gethandle(Item);
  if Handle>=0 then result:=LookupByHandle(Handle, Index)
  else result:='';
  if result='' then
  result:=Fallback.Lookup(Item, Index);
  end;

  function TStringTable.TryLookup(const Item: string; Index: integer): string;
  var
  Handle: integer;
  begin
  Handle:=Gethandle(Item);
  if Handle>=0 then result:=LookupByHandle(Handle, Index)
  else result:='';
  end; }

function TStringTable.Search(const Content: string;
  var Handle, Index: integer): boolean;
var
  h, i: integer;
  UContent: string;
begin
  UContent := UpperCase(Content);
  h := Handle;
  if h < 0 then
    i := 0
  else
    i := Index + 1;
  repeat
    if h + i + 1 >= Lines.Count then
    begin
      result := false;
      exit
    end;
    if Copy(Lines[h + i + 1], 1, 1) = '#' then
    begin
      h := h + i + 1;
      i := -1
    end;
    if (h >= 0) and not ((Length(Lines[h + i + 1]) > 0) and (Lines[h + i + 1][1] in ['#', ':', ';'])) and
      (Pos(UContent, UpperCase(Lines[h + i + 1])) > 0) then
    begin
      Index := i;
      Handle := h;
      Result := True;
      Exit;
    end;
    Inc(I);
  until False;
end;

end.
