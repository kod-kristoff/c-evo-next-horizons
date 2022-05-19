{single instance priority queue
main parts contributed by Rassim Eminli}

{$INCLUDE Switches.inc}

unit Pile;

interface

procedure Create(Size: Integer);
procedure Free;
procedure Empty;
function Put(Item, Value: Integer): Boolean;
function TestPut(Item, Value: Integer): Boolean;
function Get(var Item, Value: Integer): Boolean;


implementation

const
  MaxSize = 9600;

type
  TheapItem = record
    Item: Integer;
    Value: Integer;
  end;

var
  bh: array[0..MaxSize - 1] of TheapItem;
  Ix: array[0..MaxSize - 1] of Integer;
  N, CurrentSize: Integer;
{$IFDEF DEBUG}InUse: Boolean;{$ENDIF}


procedure Create(Size: Integer);
begin
  {$IFDEF DEBUG}
  Assert(not InUse, 'Pile is a single instance class, ' +
    'no multiple usage possible. Always call Pile.Free after use.');
{$ENDIF}
  Assert(Size <= MaxSize);
  if (N <> 0) or (Size > CurrentSize) then
  begin
    FillChar(Ix, Size * sizeOf(Integer), 255);
    N := 0;
  end;
  CurrentSize := Size;
        {$IFDEF DEBUG}
  InUse := True;
{$ENDIF}
end;

procedure Free;
begin
        {$IFDEF DEBUG}
  Assert(InUse);
  InUse := False;
{$ENDIF}
end;

procedure Empty;
begin
  if N <> 0 then
  begin
    FillChar(Ix, CurrentSize * sizeOf(Integer), 255);
    N := 0;
  end;
end;

//Parent(i) = (i-1)/2.
function Put(Item, Value: Integer): Boolean; //O(lg(n))
var
  I, J: Integer;
begin
  Assert(Item < CurrentSize);
  I := Ix[Item];
  if I >= 0 then
  begin
    if bh[I].Value <= Value then
    begin
      Result := False;
      Exit;
    end;
  end
  else
  begin
    I := N;
    Inc(N);
  end;

  while I > 0 do
  begin
    J := (I - 1) shr 1;  //Parent(i) = (i-1)/2
    if Value >= bh[J].Value then
      Break;
    bh[I] := bh[J];
    Ix[bh[I].Item] := I;
    I := J;
  end;
  //  Insert the new Item at the insertion point found.
  bh[I].Value := Value;
  bh[I].Item := Item;
  Ix[bh[I].Item] := I;
  Result := True;
end;

function TestPut(Item, Value: Integer): Boolean;
var
  I: Integer;
begin
  Assert(Item < CurrentSize);
  I := Ix[Item];
  Result := (I < 0) or (bh[I].Value > Value);
end;

//Left(i) = 2*i+1.
//Right(i) = 2*i+2 => Left(i)+1
function Get(var Item, Value: Integer): Boolean; //O(lg(n))
var
  I, J: Integer;
  Last: TheapItem;
begin
  if N = 0 then
  begin
    Result := False;
    Exit;
  end;

  Item := bh[0].Item;
  Value := bh[0].Value;

  Ix[Item] := -1;

  Dec(N);
  if N > 0 then
  begin
    Last := bh[N];
    I := 0;
    J := 1;
    while J < N do
    begin
      //  Right(i) = Left(i)+1
      if (J < N - 1) and (bh[J].Value > bh[J + 1].Value) then
        Inc(J);
      if Last.Value <= bh[J].Value then
        Break;

      bh[I] := bh[J];
      Ix[bh[I].Item] := I;
      I := J;
      J := J shl 1 + 1;  //Left(j) = 2*j+1
    end;

    // Insert the root in the correct place in the heap.
    bh[I] := Last;
    Ix[Last.Item] := I;
  end;
  Result := True;
end;

initialization
  N := 0;
  CurrentSize := 0;
        {$IFDEF DEBUG}
  InUse := False;
{$ENDIF}
end.
