unit UKeyBindings;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, LCLProc, LCLType, Menus;

type
  TKeyBinding = class
    ShortName: string;
    FullName: string;
    ShortCut: TShortCut;
  end;

  { TKeyBindings }

  TKeyBindings = class(TFPGObjectList<TKeyBinding>)
    function AddItem(const ShortName, FullName: string; ShortCut: TShortCut): TKeyBinding; overload;
    function AddItem(const ShortName, FullName: string; ShortCutText: string): TKeyBinding; overload;
    function Search(ShortName: string): TKeyBinding;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
  end;

var
  KeyBindings: TKeyBindings;
  BEndTurn: TKeyBinding;
  BHelp: TKeyBinding;
  BUnitStat: TKeyBinding;
  BCityStat: TKeyBinding;
  BScienceStat: TKeyBinding;
  BEUnitStat: TKeyBinding;
  BDiagram: TKeyBinding;
  BWonders: TKeyBinding;
  BShips: TKeyBinding;
  BNations: TKeyBinding;
  BEmpire: TKeyBinding;
  BResign: TKeyBinding;
  BRandomMap: TKeyBinding;
  BDisbandUnit: TKeyBinding;
  BFortify: TKeyBinding;
  BCenterUnit: TKeyBinding;
  BStay: TKeyBinding;
  BNoOrders: TKeyBinding;
  BTechTree: TKeyBinding;
  BWait: TKeyBinding;
  BJump: TKeyBinding;


implementation

{ TKeyBindings }

function TKeyBindings.AddItem(const ShortName, FullName: string; ShortCut: TShortCut
  ): TKeyBinding;
begin
  Result := TKeyBinding.Create;
  Result.ShortName := ShortName;
  Result.FullName := FullName;
  Result.ShortCut := ShortCut;
  Add(Result);
end;

function TKeyBindings.AddItem(const ShortName, FullName: string;
  ShortCutText: string): TKeyBinding;
begin
  Result := AddItem(ShortName, FullName, TextToShortCut(ShortCutText));
end;

function TKeyBindings.Search(ShortName: string): TKeyBinding;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].ShortName <> ShortName) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

procedure TKeyBindings.LoadFromFile(FileName: string);
var
  Lines: TStringList;
  I: Integer;
  KeyBinding: TKeyBinding;
begin
  Lines := TStringList.Create;
  Lines.NameValueSeparator := '=';
  try
    Lines.LoadFromFile(FileName);
    for I := 0 to Lines.Count - 1 do begin
      KeyBinding := Search(Lines.Names[I]);
      if Assigned(KeyBinding) then begin
        KeyBinding.ShortCut := TextToShortCut(Lines.ValueFromIndex[I]);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TKeyBindings.SaveToFile(FileName: string);
var
  Lines: TStringList;
  I: Integer;
begin
  Lines := TStringList.Create;
  Lines.NameValueSeparator := '=';
  try
    for I := 0 to Count - 1 do begin
      Lines.Add(TKeyBinding(Items[I]).ShortName + '=' + ShortCutToText(TKeyBinding(Items[I]).ShortCut));
    end;
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;


initialization

KeyBindings := TKeyBindings.Create;
with KeyBindings do begin
  BEndTurn := AddItem('EndTurn', 'End turn', 'NumPlus');
  BHelp := AddItem('Help', 'Help', 'F1');
  BUnitStat := AddItem('UnitStat', 'Unit stat', 'F2');
  BCityStat := AddItem('CityStat', 'City stat', 'F3');
  BScienceStat := AddItem('ScienceStat', 'Science research', 'F4');
  BEUnitStat := AddItem('EnemyUnitStat', 'Enemy unit stat', 'F5');
  BDiagram := AddItem('Diagram', 'Charts', 'F6');
  BWonders := AddItem('Wonders', 'World wonders', 'F7');
  BShips := AddItem('Ships', 'Colonization ships', 'F8');
  BNations := AddItem('Nations', 'Nations', 'F9');
  BEmpire := AddItem('Empire', 'Empire', 'F10');
  BResign := AddItem('Resign', 'Resign', 'Ctrl+Q');
  BRandomMap := AddItem('RandomMap', 'Random map', 'Ctrl+R');
  BDisbandUnit := AddItem('DisbandUnit', 'Disband unit', 'Ctrl+D');
  BFortify := AddItem('Fortify', 'Fortify', 'F');
  BCenterUnit := AddItem('CenterUnit', 'Center', 'C');
  BStay := AddItem('Stay', 'Stay', 'S');
  BNoOrders := AddItem('NoOrders', 'No orders', 'Space');
  BTechTree := AddItem('TechTree', 'Tech tree', 'T');
  BWait := AddItem('Wait', 'Wait', 'W');
  BJump := AddItem('Jump', 'Jump 20 turns', 'Ctrl+J');
end;


finalization

FreeAndNil(KeyBindings);

end.

