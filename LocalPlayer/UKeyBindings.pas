unit UKeyBindings;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, LCLProc, LCLType, Menus, Registry;

type

  { TKeyBinding }

  TKeyBinding = class
    ShortName: string;
    FullName: string;
    ShortCut: TShortCut;
    ShortCut2: TShortCut;
    function Test(AShortCut: TShortCut): Boolean;
  end;

  { TKeyBindings }

  TKeyBindings = class(TFPGObjectList<TKeyBinding>)
  public
    function AddItem(const ShortName, FullName: string; ShortCut: TShortCut; ShortCut2: TShortCut = 0): TKeyBinding; overload;
    function AddItem(const ShortName, FullName: string; ShortCutText: string; ShortCutText2: string = ''): TKeyBinding; overload;
    function Search(ShortName: string): TKeyBinding;
    procedure LoadFromRegistry(RootKey: HKEY; Key: string);
    procedure SaveToRegistry(RootKey: HKEY; Key: string);
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
  BCancel: TKeyBinding;
  BPillage: TKeyBinding;
  BSelectTransport: TKeyBinding;
  BTechTree: TKeyBinding;
  BWait: TKeyBinding;
  BJump: TKeyBinding;
  BMapBtn0: TKeyBinding;
  BMapBtn1: TKeyBinding;
  BMapBtn4: TKeyBinding;
  BMapBtn5: TKeyBinding;
  BMapBtn6: TKeyBinding;
  BSetDebugMap0: TKeyBinding;
  BSetDebugMap1: TKeyBinding;
  BSetDebugMap2: TKeyBinding;
  BSetDebugMap3: TKeyBinding;
  BSetDebugMap4: TKeyBinding;
  BSetDebugMap5: TKeyBinding;
  BSetDebugMap6: TKeyBinding;
  BSetDebugMap7: TKeyBinding;
  BSetDebugMap8: TKeyBinding;
  BSetDebugMap9: TKeyBinding;
  BDebugMap: TKeyBinding;
  BLocCodes: TKeyBinding;
  BLogDlg: TKeyBinding;
  BNames: TKeyBinding;
  BRun: TKeyBinding;
  BTestMapRepaint: TKeyBinding;
  BSetViewpoint0: TKeyBinding;
  BSetViewpoint1: TKeyBinding;
  BSetViewpoint2: TKeyBinding;
  BSetViewpoint3: TKeyBinding;
  BSetViewpoint4: TKeyBinding;
  BSetViewpoint5: TKeyBinding;
  BSetViewpoint6: TKeyBinding;
  BSetViewpoint7: TKeyBinding;
  BSetViewpoint8: TKeyBinding;
  BSetViewpoint9: TKeyBinding;
  BAirBase: TKeyBinding;
  BBuildCity: TKeyBinding;
  BEnhance: TKeyBinding;
  BGoOn: TKeyBinding;
  BHome: TKeyBinding;
  BFarmClearIrrigation: TKeyBinding;
  BLoad: TKeyBinding;
  BAfforestMine: TKeyBinding;
  BCanal: TKeyBinding;
  BTrans: TKeyBinding;
  BPollution: TKeyBinding;
  BRailRoad: TKeyBinding;
  BUnload: TKeyBinding;
  BRecover: TKeyBinding;
  BUtilize: TKeyBinding;
  BMoveLeftDown: TKeyBinding;
  BMoveDown: TKeyBinding;
  BMoveRightDown: TKeyBinding;
  BMoveRight: TKeyBinding;
  BMoveRightUp: TKeyBinding;
  BMoveUp: TKeyBinding;
  BMoveLeftUp: TKeyBinding;
  BMoveLeft: TKeyBinding;


implementation

{ TKeyBinding }

function TKeyBinding.Test(AShortCut: TShortCut): Boolean;
begin
  Result := (AShortCut = ShortCut) or (AShortCut = ShortCut2);
end;

{ TKeyBindings }

function TKeyBindings.AddItem(const ShortName, FullName: string; ShortCut: TShortCut;
  ShortCut2: TShortCut = 0): TKeyBinding;
begin
  Result := TKeyBinding.Create;
  Result.ShortName := ShortName;
  Result.FullName := FullName;
  Result.ShortCut := ShortCut;
  Result.ShortCut2 := ShortCut2;
  Add(Result);
end;

function TKeyBindings.AddItem(const ShortName, FullName: string;
  ShortCutText: string; ShortCutText2: string = ''): TKeyBinding;
begin
  Result := AddItem(ShortName, FullName, TextToShortCut(ShortCutText),
    TextToShortCut(ShortCutText2));
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

procedure TKeyBindings.LoadFromRegistry(RootKey: HKEY; Key: string);
var
  I: Integer;
  Registry: TRegistry;
  Text: string;
begin
  Registry := TRegistry.Create;
  Registry.RootKey := RootKey;
  with Registry do
  try
    OpenKey(Key, True);
    for I := 0 to Count - 1 do begin
      Text := '';
      if ValueExists(Items[I].ShortName) then begin
        Text := ReadString(Items[I].ShortName);
        if Pos(',', Text) > 0 then begin
          Items[I].ShortCut2 := TextToShortCut(Copy(Text, Pos(',', Text) + 1, MaxInt));
          Items[I].ShortCut := TextToShortCut(Copy(Text, 1, Pos(',', Text) - 1));
        end else begin
          Items[I].ShortCut := TextToShortCut(Text);
          Items[I].ShortCut2 := 0;
        end;
      end else begin
        Text := ShortCutToText(Items[I].ShortCut);
        if Items[I].ShortCut2 <> 0 then Text := Text + ',' + ShortCutToText(Items[I].ShortCut2);
        WriteString(Items[I].ShortName, Text);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TKeyBindings.SaveToRegistry(RootKey: HKEY; Key: string);
var
  I: Integer;
  Registry: TRegistry;
  Text: string;
begin
  Registry := TRegistry.Create;
  Registry.RootKey := RootKey;
  with Registry do
  try
    OpenKey(Key, True);
    for I := 0 to Count - 1 do begin
      Text := ShortCutToText(Items[I].ShortCut);
      if Items[I].ShortCut2 <> 0 then Text := Text + ',' + ShortCutToText(Items[I].ShortCut2);
      WriteString(Items[I].ShortName, Text);
    end;
  finally
    Free;
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
  BCancel := AddItem('Cancel', 'Cancel', 'Ctrl+C');
  BPillage := AddItem('Pillage', 'Pillage', 'Ctrl+P');
  BSelectTransport := AddItem('SelectTransport', 'Select transport', 'Ctrl+T');
  BTechTree := AddItem('TechTree', 'Tech tree', 'T');
  BWait := AddItem('Wait', 'Wait', 'W');
  BJump := AddItem('Jump', 'Jump 20 turns', 'Ctrl+J');
  BMapBtn0 := AddItem('MapButton1', 'Map button 1', '1');
  BMapBtn1 := AddItem('MapButton2', 'Map button 2', '2');
  BMapBtn4 := AddItem('MapButton3', 'Map button 3', '3');
  BMapBtn5 := AddItem('MapButton4', 'Map button 4', '4');
  BMapBtn6 := AddItem('MapButton5', 'Map button 5', '5');
  BSetDebugMap0 := AddItem('SetDebugMap0', 'Set debug map 0', '0');
  BSetDebugMap1 := AddItem('SetDebugMap1', 'Set debug map 1', '1');
  BSetDebugMap2 := AddItem('SetDebugMap2', 'Set debug map 2', '2');
  BSetDebugMap3 := AddItem('SetDebugMap3', 'Set debug map 3', '3');
  BSetDebugMap4 := AddItem('SetDebugMap4', 'Set debug map 4', '4');
  BSetDebugMap5 := AddItem('SetDebugMap5', 'Set debug map 5', '5');
  BSetDebugMap6 := AddItem('SetDebugMap6', 'Set debug map 6', '6');
  BSetDebugMap7 := AddItem('SetDebugMap7', 'Set debug map 7', '7');
  BSetDebugMap8 := AddItem('SetDebugMap8', 'Set debug map 8', '8');
  BSetDebugMap9 := AddItem('SetDebugMap9', 'Set debug map 9', '9');
  BDebugMap := AddItem('DebugMap', 'Debug map', 'Ctrl+K');
  BLocCodes := AddItem('LocCodes', 'Location codes', 'Ctrl+L');
  BLogDlg := AddItem('LogDlg', 'Log dialog', 'Ctrl+M');
  BNames := AddItem('ShowNames', 'Show names', 'Ctrl+N');
  BRun := AddItem('Run', 'Run', 'Ctrl+R');
  BTestMapRepaint := AddItem('TestMapRepaint', 'Test map reapaint', 'Ctrl+Space');
  BSetViewpoint0 := AddItem('SetViewpoint0', 'Set viewpoint 0', 'Ctrl+0');
  BSetViewpoint1 := AddItem('SetViewpoint1', 'Set viewpoint 1', 'Ctrl+1');
  BSetViewpoint2 := AddItem('SetViewpoint2', 'Set viewpoint 2', 'Ctrl+2');
  BSetViewpoint3 := AddItem('SetViewpoint3', 'Set viewpoint 3', 'Ctrl+3');
  BSetViewpoint4 := AddItem('SetViewpoint4', 'Set viewpoint', 'Ctrl+4');
  BSetViewpoint5 := AddItem('SetViewpoint0', 'Set viewpoint', 'Ctrl+5');
  BSetViewpoint6 := AddItem('SetViewpoint1', 'Set viewpoint', 'Ctrl+6');
  BSetViewpoint7 := AddItem('SetViewpoint2', 'Set viewpoint', 'Ctrl+7');
  BSetViewpoint8 := AddItem('SetViewpoint3', 'Set viewpoint', 'Ctrl+8');
  BSetViewpoint9 := AddItem('SetViewpoint4', 'Set viewpoint', 'Ctrl+9');
  BAirBase := AddItem('AirBase', 'Air base', 'A');
  BBuildCity := AddItem('BuildCity', 'Build city', 'B');
  BEnhance := AddItem('Enhance', 'Enhance', 'E');
  BGoOn := AddItem('GoOn', 'Go on', 'G');
  BHome := AddItem('Home', 'Home', 'H');
  BFarmClearIrrigation := AddItem('FarmClearIrrigation', 'Farm/Clear/Irrigation', 'I');
  BLoad := AddItem('Load', 'Load', 'L');
  BAfforestMine := AddItem('AfforestMine', 'Afforest/Mine', 'M');
  BCanal := AddItem('Canal', 'Canal', 'N');
  BTrans := AddItem('Trans', 'Trans', 'O');
  BPollution := AddItem('Pollution', 'Pollution', 'P');
  BRailRoad := AddItem('RailRoad', 'Rails/Road', 'R');
  BUnload := AddItem('Unload', 'Unload', 'U');
  BRecover := AddItem('Recover', 'Recover', 'V');
  BUtilize := AddItem('Utilize', 'Utilize', 'Z');
  BMoveLeftDown := AddItem('MoveLeftDown', 'Move unit left-down', 'Num1', 'End');
  BMoveDown := AddItem('MoveDown', 'Move unit down', 'Num2', 'Down');
  BMoveRightDown := AddItem('MoveRightDown', 'Move unit right-down', 'Num3', 'PgDn');
  BMoveRight := AddItem('MoveRight', 'Move unit right', 'Num6', 'Right');
  BMoveRightUp := AddItem('MoveRightUp', 'Move unit right-up', 'Num9', 'PgUp');
  BMoveUp := AddItem('MoveUp', 'Move unit up', 'Num8', 'Up');
  BMoveLeftUp := AddItem('MoveLeftUp', 'Move unit left-up', 'Num7', 'Home');
  BMoveLeft := AddItem('MoveLeft', 'Move unit left', 'Num4', 'Left');
end;


finalization

FreeAndNil(KeyBindings);

end.

