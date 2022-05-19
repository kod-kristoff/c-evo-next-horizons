{ binary heap priority queue
  Code contributed by Rassim Eminli }

{$INCLUDE Switches.inc}
unit IPQ;

interface

type
  TIntegerArray = array [0 .. $40000000 div SizeOf(Integer)] of Integer;
  PIntegerArray = ^TIntegerArray;

  TheapItem = record
    Item: Integer;
    Value: Integer;
  end;

  TItemArray = array [0 .. $40000000 div SizeOf(TheapItem)] of TheapItem;
  PItemArray = ^TItemArray;

  TIPQ = class
    constructor Create(Max: Integer);
    destructor Destroy; override;
    procedure Empty;
    function Put(Item, Value: Integer): Boolean;
    function TestPut(Item, Value: Integer): Boolean;
    function Get(var Item, Value: Integer): Boolean;
  private
    // n - is the size of the heap.
    // fmax - is the max size of the heap.
    N, fmax: Integer;

    // bh - stores (Value, Item) pairs of the heap.
    // Ix - stores the positions of pairs in the heap bh.
    bh: PItemArray;
    Ix: PIntegerArray;
  end;

implementation

constructor TIPQ.Create(Max: Integer);
begin
  inherited Create;
  fmax := Max;
  GetMem(bh, fmax * SizeOf(TheapItem));
  GetMem(Ix, fmax * SizeOf(Integer));
  N := -1;
  Empty;
end;

destructor TIPQ.Destroy;
begin
  FreeMem(bh);
  FreeMem(Ix);
  inherited;
end;

procedure TIPQ.Empty;
begin
  if N <> 0 then
  begin
    FillChar(Ix^, fmax * SizeOf(Integer), 255);
    N := 0;
  end;
end;

// Parent(i) = (i-1)/2.
function TIPQ.Put(Item, Value: Integer): Boolean; // O(lg(n))
var
  I, J: Integer;
  lbh: PItemArray;
  lIx: PIntegerArray;
begin
  lIx := Ix;
  lbh := bh;
  I := lIx[Item];
  if I >= 0 then
  begin
    if lbh[I].Value <= Value then
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
    J := (I - 1) shr 1; // Parent(i) = (i-1)/2
    if Value >= lbh[J].Value then
      Break;
    lbh[I] := lbh[J];
    lIx[lbh[I].Item] := I;
    I := J;
  end;
  // Insert the new Item at the insertion point found.
  lbh[I].Value := Value;
  lbh[I].Item := Item;
  lIx[lbh[I].Item] := I;
  Result := True;
end;

function TIPQ.TestPut(Item, Value: Integer): Boolean;
var
  I: Integer;
begin
  I := Ix[Item];
  Result := (I < 0) or (bh[I].Value > Value);
end;

// Left(i) = 2*i+1.
// Right(i) = 2*i+2 => Left(i)+1
function TIPQ.Get(var Item, Value: Integer): Boolean; // O(lg(n))
var
  I, J: Integer;
  Last: TheapItem;
  lbh: PItemArray;
begin
  if N = 0 then
  begin
    Result := False;
    Exit;
  end;

  lbh := bh;
  Item := lbh[0].Item;
  Value := lbh[0].Value;

  Ix[Item] := -1;

  Dec(N);
  if N > 0 then
  begin
    Last := lbh[N];
    I := 0;
    J := 1;
    while J < N do
    begin
      // Right(i) = Left(i)+1
      if (J < N - 1) and (lbh[J].Value > lbh[J + 1].Value) then
        Inc(J);
      if Last.Value <= lbh[J].Value then
        Break;

      lbh[I] := lbh[J];
      Ix[lbh[I].Item] := I;
      I := J;
      J := J shl 1 + 1; // Left(j) = 2*j+1
    end;

    // Insert the root in the correct place in the heap.
    lbh[I] := Last;
    Ix[Last.Item] := I;
  end;
  Result := True;
end;

end.
