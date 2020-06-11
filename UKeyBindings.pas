unit UKeyBindings;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, LCLProc;

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
  N: Integer;
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
BEndTurn := KeyBindings.AddItem('EndTurn', 'End turn', 'NumPlus');
BHelp := KeyBindings.AddItem('Help', 'Help', 'F1');
BUnitStat := KeyBindings.AddItem('UnitStat', 'Unit stat', 'F2');
BCityStat := KeyBindings.AddItem('CityStat', 'City stat', 'F3');
BScienceStat := KeyBindings.AddItem('ScienceStat', 'Science research', 'F4');
BEUnitStat := KeyBindings.AddItem('EnemyUnitStat', 'Enemy unit stat', 'F5');
BDiagram := KeyBindings.AddItem('Diagram', 'Charts', 'F6');
BWonders := KeyBindings.AddItem('Wonders', 'World wonders', 'F7');
BShips := KeyBindings.AddItem('Ships', 'Colonization ships', 'F8');
BNations := KeyBindings.AddItem('Nations', 'Nations', 'F9');
BEmpire := KeyBindings.AddItem('Empire', 'Empire', 'F10');


finalization

FreeAndNil(KeyBindings);

end.

