{$INCLUDE Switches.inc}
unit Start;

interface

uses
  GameServer, Messg, ButtonBase, ButtonA, ButtonC, ButtonB, Area,
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Menus, Registry, DrawDlg, fgl, Protocol;

type
  TPlayerSlot = class
    DiffUpBtn: TButtonC;
    DiffDownBtn: TButtonC;
    MultiBtn: TButtonC;
    OfferMultiple: Boolean;
    Rect: TRect;
  end;

  TPlayerSlots = class(TFPGObjectList<TPlayerSlot>)
  end;

  TStartPage = (
    pgStartRandom,
    pgStartMap,
    pgNoLoad,
    pgLoad,
    pgEditRandom,
    pgEditMap,
    pgMain
  );

  TStartTab = (tbMain, tbMap, tbNew, tbPrevious);
  TMainAction = (maConfig, maManual, maCredits, maAIDev, maWeb, maNone);
  TMainActionSet = set of TMainAction;

  TMapArray = array[0 .. lxmax * lymax - 1] of Byte;

  TMiniMode = (mmNone, mmPicture, mmMultiPlayer);

  { TMiniMap }

  TMiniMap = class
  const
    MaxWidthMapLogo = 96;
    MaxHeightMapLogo = 96;
  var
    Bitmap: TBitmap; { game world sample preview }
    Size: TPoint;
    Colors: array [0 .. 11, 0 .. 1] of TColor;
    Mode: TMiniMode;
    procedure LoadFromLogFile(FileName: string; var LastTurn: Integer);
    procedure LoadFromMapFile(FileName: string; var nMapLandTiles, nMapStartPositions: Integer);
    procedure PaintRandom(Brightness, StartLandMass, WorldSize: Integer);
    procedure PaintFile(SaveMap: TMapArray);
    constructor Create;
    destructor Destroy; override;
  end;

  { TStartDlg }

  TStartDlg = class(TDrawDlg)
    PopupMenu1: TPopupMenu;
    StartBtn: TButtonA;
    Down1Btn: TButtonC;
    Up1Btn: TButtonC;
    List: TListBox;
    RenameBtn: TButtonB;
    DeleteBtn: TButtonB;
    Down2Btn: TButtonC;
    Up2Btn: TButtonC;
    QuitBtn: TButtonB;
    CustomizeBtn: TButtonC;
    AutoDiffUpBtn: TButtonC;
    AutoDiffDownBtn: TButtonC;
    AutoEnemyUpBtn: TButtonC;
    AutoEnemyDownBtn: TButtonC;
    ReplayBtn: TButtonB;
    procedure StartBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BrainClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; x, y: integer);
    procedure Up1BtnClick(Sender: TObject);
    procedure Down1BtnClick(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure RenameBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DiffBtnClick(Sender: TObject);
    procedure MultiBtnClick(Sender: TObject);
    procedure Up2BtnClick(Sender: TObject);
    procedure Down2BtnClick(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure CustomizeBtnClick(Sender: TObject);
    procedure AutoDiffUpBtnClick(Sender: TObject);
    procedure AutoDiffDownBtnClick(Sender: TObject);
    procedure AutoEnemyUpBtnClick(Sender: TObject);
    procedure AutoEnemyDownBtnClick(Sender: TObject);
    procedure ReplayBtnClick(Sender: TObject);
  public
    EmptyPicture: TBitmap;
    procedure UpdateFormerGames;
    procedure UpdateMaps;
  private
    WorldSize: Integer;
    StartLandMass: Integer;
    MaxTurn: Integer;
    AutoEnemies: Integer;
    AutoDiff: Integer;
    MultiControl: Integer;
    Page: TStartPage;
    ShowTab: TStartTab;
    Tab: TStartTab;
    Diff0: Integer;
    BrainDefault: TBrain;
    nMapLandTiles: Integer;
    nMapStartPositions: Integer;
    LoadTurn: Integer;
    LastTurn: Integer;
    { last turn of selected former game }
    SlotAvailable: Integer;
    PlayerPopupIndex: Integer; { brain concerned by brain context menu }
    ListIndex: array [TStartTab] of Integer;
    MapFileName: string;
    FormerGames: TStringList;
    Maps: TStringList;
    LogoBuffer: TBitmap;
    // BookDate: string;
    PlayerSlots: TPlayerSlots;
    ActionsOffered: TMainActionSet;
    SelectedAction: TMainAction;
    TurnValid: Boolean;
    Tracking: Boolean;
    DefaultAI: string;
    MiniMap: TMiniMap;
    procedure DrawAction(y, IconIndex: integer; HeaderItem, TextItem: string);
    procedure InitPopup(PlayerIndex: Integer);
    procedure OfferBrain(Brain: TBrain; FixedLines: Integer);
    procedure PaintInfo;
    procedure ChangePage(NewPage: TStartPage);
    procedure ChangeTab(NewTab: TStartTab);
    procedure UnlistBackupFile(FileName: string);
    procedure SmartInvalidate(x0, y0, x1, y1: integer;
      invalidateTab0: boolean = false); overload;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure LoadAiBrainsPictures;
    procedure UpdateInterface;
  end;

var
  StartDlg: TStartDlg;


implementation

uses
  Global, Directories, Direct, ScreenTools, Inp, Back, Settings, UPixelPointer,
  UKeyBindings;

{$R *.lfm}

const
  // predefined world size
  // attention: lx*ly+1 must be prime!
  { MaxWorldSize=8;
    lxpre: array[0..nWorldSize-1] of integer =(30,40,50,60,70,90,110,130);
    lypre: array[0..nWorldSize-1] of integer =(46,52,60,70,84,94,110,130);
    DefaultWorldTiles=4200; }
  MaxWorldSize = 6;
  WorldSizes: array [0 .. MaxWorldSize - 1] of TPoint = ((X: 30; Y: 46),
    (X: 40; Y: 52), (X: 50; Y: 60), (X: 60; Y: 70), (X: 75; Y: 82),
    (X: 100; Y: 96));
  DefaultWorldTiles = 4150;
  DefaultWorldSize = 3;
  DefaultLandMass = 30;

  nPlOffered = 9;
  yMain = 14;
  xActionIcon = 55;
  xAction = 111;
  yAction = 60;
  ActionPitch = 56;
  ActionSideBorder = 24;
  ActionBottomBorder = 10;
  wBuffer = 91;
  x0Mini = 437;
  y0Mini = 178;
  xTurnSlider = 346;
  yTurnSlider = 262;
  wTurnSlider = 168;
  yLogo = 74;
  xDefault = 234;
  yDefault = 148;
  x0Brain = 146;
  y0Brain = 148;
  dxBrain = 104;
  dyBrain = 80;
  xBrain: array [0 .. nPlOffered - 1] of integer = (x0Brain, x0Brain,
    x0Brain + dxBrain, x0Brain + dxBrain, x0Brain + dxBrain, x0Brain,
    x0Brain - dxBrain, x0Brain - dxBrain, x0Brain - dxBrain);
  yBrain: array [0 .. nPlOffered - 1] of integer = (y0Brain, y0Brain - dyBrain,
    y0Brain - dyBrain, y0Brain, y0Brain + dyBrain, y0Brain + dyBrain,
    y0Brain + dyBrain, y0Brain, y0Brain - dyBrain);
  TabOffset = -115;
  TabSize = 159;
  TabHeight = 40;

  InitAlive: array [1 .. nPl] of integer = (1, 1 + 2, 1 + 2 + 32,
    1 + 2 + 8 + 128, 1 + 2 + 8 + 32 + 128, 1 + 2 + 8 + 16 + 64 + 128,
    1 + 2 + 4 + 16 + 32 + 64 + 256, 511 - 32, 511, 511 - 32, 511, 511 - 32, 511,
    511 - 32, 511);
  InitMulti: array [nPlOffered + 1 .. nPl] of integer = (256, 256, 256 + 128,
    256 + 128, 256 + 128 + 64, 256 + 128 + 64);

  PlayerAutoDiff: array [1 .. 5] of integer = (1, 1, 2, 2, 3);
  EnemyAutoDiff: array [1 .. 5] of integer = (4, 3, 2, 1, 1);
  KeyBindingsFileName = 'KeyBindings.txt';

{ TMiniMap }

constructor TMiniMap.Create;
var
  X, Y: Integer;
begin
  Bitmap := TBitmap.Create;

  for X := 0 to 11 do
    for Y := 0 to 1 do
      Colors[x, y] := GrExt[HGrSystem].Data.Canvas.Pixels[66 + x, 67 + y];
end;

destructor TMiniMap.Destroy;
begin
  FreeAndNil(Bitmap);
  inherited Destroy;
end;

procedure TMiniMap.LoadFromLogFile(FileName: string; var LastTurn: Integer);
var
  SaveMap: TMapArray;
  y: Integer;
  Dummy: Integer;
  FileLandMass: integer;
  LogFile: file;
  s: string[255];
  MapRow: array [0 .. lxmax - 1] of Cardinal;
begin
  AssignFile(LogFile, FileName);
  try
    Reset(LogFile, 4);
    BlockRead(LogFile, s[1], 2); { file id }
    BlockRead(LogFile, Dummy, 1); { format id }
    if Dummy >= $000E01 then
      BlockRead(LogFile, Dummy, 1); { item stored since 0.14.1 }
    BlockRead(LogFile, Size.X, 1);
    BlockRead(LogFile, Size.Y, 1);
    BlockRead(LogFile, FileLandMass, 1);
    if FileLandMass = 0 then
      for y := 0 to Size.Y - 1 do
        BlockRead(LogFile, MapRow, Size.X);
    BlockRead(LogFile, Dummy, 1);
    BlockRead(LogFile, Dummy, 1);
    BlockRead(LogFile, LastTurn, 1);
    BlockRead(LogFile, SaveMap, 1);
    if SaveMap[0] = $80 then
      Mode := mmMultiPlayer
    else
      Mode := mmPicture;
    if Mode = mmPicture then
      BlockRead(LogFile, SaveMap[4], (Size.X * Size.Y - 1) div 4);
    CloseFile(LogFile);
  except
    CloseFile(LogFile);
    LastTurn := 0;
    Size := WorldSizes[DefaultWorldSize];
    Mode := mmNone;
  end;
  PaintFile(SaveMap);
end;

procedure TMiniMap.LoadFromMapFile(FileName: string; var nMapLandTiles, nMapStartPositions: Integer);
var
  x, y, lxFile, lyFile: integer;
  MapFile: file;
  s: string[255];
  MapRow: array [0 .. lxmax - 1] of Cardinal;
  ImageFileName: string;
begin
  ImageFileName := Copy(FileName, 1, Length(FileName) - Length(CevoMapExt)) + '.png';
  Mode := mmPicture;
  if LoadGraphicFile(Bitmap, ImageFileName, gfNoError) then
  begin
    if Bitmap.width div 2 > MaxWidthMapLogo then
      Bitmap.width := MaxWidthMapLogo * 2;
    if Bitmap.height > MaxHeightMapLogo then
      Bitmap.height := MaxHeightMapLogo;
    Size.X := Bitmap.width div 2;
    Size.Y := Bitmap.height;
  end
  else
  begin
    Mode := mmNone;
    Size.X := MaxWidthMapLogo;
    Size.Y := MaxHeightMapLogo;
  end;

  AssignFile(MapFile, FileName);
  try
    Reset(MapFile, 4);
    BlockRead(MapFile, s[1], 2); { file id }
    BlockRead(MapFile, x, 1); { format id }
    BlockRead(MapFile, x, 1); // MaxTurn
    BlockRead(MapFile, lxFile, 1);
    BlockRead(MapFile, lyFile, 1);
    nMapLandTiles := 0;
    nMapStartPositions := 0;
    for y := 0 to lyFile - 1 do begin
      BlockRead(MapFile, MapRow, lxFile);
      for x := 0 to lxFile - 1 do
      begin
        if (MapRow[x] and fTerrain) in [fGrass, fPrairie, fTundra, fSwamp,
          fForest, fHills] then
          inc(nMapLandTiles);
        if MapRow[x] and (fPrefStartPos or fStartPos) <> 0 then
          inc(nMapStartPositions);
      end
    end;
    if nMapStartPositions > nPl then
      nMapStartPositions := nPl;
    CloseFile(MapFile);
  except
    CloseFile(MapFile);
  end;
end;

procedure TMiniMap.PaintRandom(Brightness, StartLandMass, WorldSize: Integer);
var
  i, x, y, xm, cm: Integer;
  MiniPixel: TPixelPointer;
  Map: ^TTileList;
begin
  Map := PreviewMap(StartLandMass);
  Size := WorldSizes[WorldSize];

  Bitmap.PixelFormat := pf24bit;
  Bitmap.SetSize(Size.X * 2, Size.Y);
  Bitmap.BeginUpdate;
  MiniPixel := PixelPointer(Bitmap);
  for y := 0 to ScaleToNative(Size.Y) - 1 do begin
    for x := 0 to ScaleToNative(Size.X) - 1 do begin
      for i := 0 to 1 do begin
        xm := (x * 2 + i + y and 1) mod (ScaleToNative(Size.X) * 2);
        MiniPixel.SetX(xm);
        cm := Colors
          [Map[ScaleFromNative(x) * lxmax div Size.X + lxmax *
          ((ScaleFromNative(y) * (lymax - 1) + Size.Y div 2) div (Size.Y - 1))] and
          fTerrain, i];
        MiniPixel.Pixel^.B := ((cm shr 16) and $FF) * Brightness div 3;
        MiniPixel.Pixel^.G := ((cm shr 8) and $FF) * Brightness div 3;
        MiniPixel.Pixel^.R := ((cm shr 0) and $FF) * Brightness div 3;
      end;
    end;
    MiniPixel.NextLine;
  end;
  Bitmap.EndUpdate;
end;

procedure TMiniMap.PaintFile(SaveMap: TMapArray);
var
  i, x, y, xm, cm, Tile, OwnColor, EnemyColor: integer;
  MiniPixel: TPixelPointer;
  PrevMiniPixel: TPixelPointer;
begin
  OwnColor := GrExt[HGrSystem].Data.Canvas.Pixels[95, 67];
  EnemyColor := GrExt[HGrSystem].Data.Canvas.Pixels[96, 67];
  Bitmap.PixelFormat := pf24bit;
  Bitmap.SetSize(Size.X * 2, Size.Y);
  if Mode = mmPicture then begin
    Bitmap.BeginUpdate;
    MiniPixel := PixelPointer(Bitmap);
    PrevMiniPixel := PixelPointer(Bitmap, 0, -1);
    for y := 0 to ScaleToNative(Size.Y) - 1 do begin
      for x := 0 to ScaleToNative(Size.X) - 1 do begin
        for i := 0 to 1 do begin
          xm := (x * 2 + i + y and 1) mod (ScaleToNative(Size.X) * 2);
          MiniPixel.SetX(xm);
          Tile := SaveMap[ScaleFromNative(x) + Size.X * ScaleFromNative(y)];
          if Tile and fTerrain = fUNKNOWN then
            cm := $000000
          else if Tile and smCity <> 0 then
          begin
            if Tile and smOwned <> 0 then
              cm := OwnColor
            else
              cm := EnemyColor;
            if y > 0 then begin
              // 2x2 city dot covers two lines
              PrevMiniPixel.SetX(xm);
              PrevMiniPixel.Pixel^.B := cm shr 16;
              PrevMiniPixel.Pixel^.G:= cm shr 8 and $FF;
              PrevMiniPixel.Pixel^.R := cm and $FF;
            end;
          end
          else if (i = 0) and (Tile and smUnit <> 0) then
            if Tile and smOwned <> 0 then
              cm := OwnColor
            else cm := EnemyColor
          else
            cm := Colors[Tile and fTerrain, i];
          MiniPixel.Pixel^.B := (cm shr 16) and $ff;
          MiniPixel.Pixel^.G := (cm shr 8) and $ff;
          MiniPixel.Pixel^.R := (cm shr 0) and $ff;
        end;
      end;
      MiniPixel.NextLine;
      PrevMiniPixel.NextLine;
    end;
    Bitmap.EndUpdate;
  end;
end;

{ TStartDlg }

procedure TStartDlg.FormCreate(Sender: TObject);
var
  x, i: Integer;
  PlayerSlot: TPlayerSlot;
  AIBrains: TBrains;
  KeyBindingsAbsoluteFileName: string;
begin
  PlayerSlots := TPlayerSlots.Create;
  PlayerSlots.Count := nPlOffered;
  for I := 0 to PlayerSlots.Count - 1 do begin
    PlayerSlot := TPlayerSlot.Create;
    PlayerSlot.Rect := Bounds(xBrain[I], YBrain[I], 0, 0);
    PlayerSlots[I] := PlayerSlot;
  end;
  LoadConfig;
  LoadAssets;
  KeyBindingsAbsoluteFileName := DataDir + DirectorySeparator + KeyBindingsFileName;
  if FileExists(KeyBindingsAbsoluteFileName) then KeyBindings.LoadFromFile(KeyBindingsAbsoluteFileName)
    else begin
      ForceDirectories(ExtractFileDir(KeyBindingsAbsoluteFileName));
      KeyBindings.SaveToFile(KeyBindingsAbsoluteFileName);
    end;

  ActionsOffered := [maConfig, maManual, maCredits, maWeb];
  if FileExists(HomeDir + AITemplateFileName) then
    Include(ActionsOffered, maAIDev);

  BrainDefault := nil;
  for i := Brains.IndexOf(BrainRandom) to Brains.Count - 1 do
    if AnsiCompareFileName(DefaultAI, Brains[i].FileName) = 0 then
      BrainDefault := Brains[i];
  if (BrainDefault = BrainRandom) and (Brains.GetKindCount(btAI) < 2) then
    BrainDefault := nil;
  if (not Assigned(BrainDefault)) and (Brains.GetKindCount(btAI) > 0) then
    begin
      AIBrains := TBrains.Create(False);
      Brains.GetByKind(btAI, AIBrains);
      BrainDefault := Brains[0];
      AIBrains.Free;
    end; // default AI not found, use any

  DirectDlg.Left := (Screen.Width - DirectDlg.Width) div 2;
  DirectDlg.Top := (Screen.Height - DirectDlg.Height) div 2;

  UpdateInterface;

  Canvas.Font.Assign(UniFont[ftNormal]);
  Canvas.Brush.Style := bsClear;

  QuitBtn.Hint := Phrases.Lookup('STARTCONTROLS', 0);
  ReplayBtn.Hint := Phrases.Lookup('BTN_REPLAY');
  PlayerSlots.Count := nPlOffered;
  for i := 0 to PlayerSlots.Count - 1 do
  with PlayerSlots[i] do begin
    DiffUpBtn := TButtonC.Create(self);
    DiffUpBtn.Graphic := GrExt[HGrSystem].Data;
    DiffUpBtn.left := xBrain[i] - 18;
    DiffUpBtn.top := yBrain[i] + 39;
    DiffUpBtn.ButtonIndex := 1;
    DiffUpBtn.Parent := self;
    DiffUpBtn.OnClick := DiffBtnClick;
    DiffDownBtn := TButtonC.Create(self);
    DiffDownBtn.Graphic := GrExt[HGrSystem].Data;
    DiffDownBtn.left := xBrain[i] - 18;
    DiffDownBtn.top := yBrain[i] + 51;
    DiffDownBtn.ButtonIndex := 0;
    DiffDownBtn.Parent := self;
    DiffDownBtn.OnClick := DiffBtnClick;
  end;
  for i := 6 to 8 do
  with PlayerSlots[i] do begin
    MultiBtn := TButtonC.Create(self);
    MultiBtn.Graphic := GrExt[HGrSystem].Data;
    MultiBtn.left := xBrain[i] - 18;
    MultiBtn.top := yBrain[i];
    MultiBtn.Parent := self;
    MultiBtn.OnClick := MultiBtnClick;
    OfferMultiple := True;
  end;

  x := BiColorTextWidth(Canvas, Phrases.Lookup('STARTCONTROLS', 7)) div 2;
  CustomizeBtn.left := x0Brain + 32 - 16 - x;
  if AutoDiff < 0 then
    CustomizeBtn.ButtonIndex := 3
  else
    CustomizeBtn.ButtonIndex := 2;

  BitBltBitmap(BrainNoTerm.Picture, 0, 0, 64, 64, GrExt[HGrSystem2].Data, 1, 111);
  BitBltBitmap(BrainSuperVirtual.Picture, 0, 0, 64, 64, GrExt[HGrSystem2].Data, 66, 111);
  BitBltBitmap(BrainTerm.Picture, 0, 0, 64, 64, GrExt[HGrSystem2].Data, 131, 111);
  BitBltBitmap(BrainRandom.Picture, 0, 0, 64, 64, GrExt[HGrSystem2].Data, 131, 46);
  LoadAiBrainsPictures;

  EmptyPicture := TBitmap.Create;
  EmptyPicture.PixelFormat := pf24bit;
  EmptyPicture.SetSize(64, 64);
  EmptyPicture.Canvas.FillRect(0, 0, EmptyPicture.Width, EmptyPicture.Height);
  LogoBuffer := TBitmap.Create;
  LogoBuffer.PixelFormat := pf24bit;
  LogoBuffer.SetSize(wBuffer, 56);
  LogoBuffer.Canvas.FillRect(0, 0, LogoBuffer.Width, LogoBuffer.Height);

  MiniMap := TMiniMap.Create;
  InitButtons;

  PlayersBrain[0] := BrainTerm;
  SlotAvailable := -1;
  Tab := tbNew;
  Diff0 := 2;
  TurnValid := False;
  Tracking := False;
  FormerGames := TStringList.Create;
  UpdateFormerGames;
  ShowTab := tbNew; // always start with new book page
  MapFileName := '';
  Maps := TStringList.Create;
  UpdateMaps;
end;

procedure TStartDlg.FormDestroy(Sender: TObject);
begin
  SaveConfig;
  FreeAndNil(FormerGames);
  FreeAndNil(Maps);
  FreeAndNil(EmptyPicture);
  FreeAndNil(LogoBuffer);
  FreeAndNil(PlayerSlots);
  FreeAndNil(MiniMap);
end;

procedure TStartDlg.SmartInvalidate(x0, y0, x1, y1: integer;
  invalidateTab0: boolean);
var
  i: integer;
  r0, r1: HRgn;
begin
  r0 := CreateRectRgn(x0, y0, x1, y1);
  for i := 0 to ControlCount - 1 do
    if not (Controls[i] is TArea) and Controls[i].Visible then
    begin
      with Controls[i].BoundsRect do
        r1 := CreateRectRgn(left, top, Right, Bottom);
      CombineRgn(r0, r0, r1, RGN_DIFF);
      DeleteObject(r1);
    end;
  if not invalidateTab0 then begin
    r1 := CreateRectRgn(0, 0, 6 + 36, 3 + 38); // tab 0 icon
    CombineRgn(r0, r0, r1, RGN_DIFF);
    DeleteObject(r1);
  end;
  InvalidateRgn(Handle, r0, false);
  DeleteObject(r0);
end;

procedure TStartDlg.LoadConfig;
var
  Reg: TRegistry;
  I: Integer;
  S: string;
  ResolutionX, ResolutionY, ResolutionBPP, ResolutionFreq: Integer;
  ScreenMode: Integer;
begin
  Reg := TRegistry.Create;
  with Reg do try
    // Initialize AI assignment
    OpenKey(AppRegistryKey + '\AI', True);
    for I := 0 to nPlOffered - 1 do begin
      if I = 0 then S := ':StdIntf'
        else S := 'StdAI';
      if not ValueExists('Control' + IntToStr(I)) then
        WriteString('Control' + IntToStr(I), S);
      if not ValueExists('Diff' + IntToStr(I)) then
        WriteInteger('Diff' + IntToStr(I), 2);
    end;

    OpenKey(AppRegistryKey, True);
    if ValueExists('Gamma') then Gamma := ReadInteger('Gamma')
      else Gamma := 100;
    if Gamma <> 100 then InitGammaLookupTable;
    if ValueExists('Locale') then LocaleCode := ReadString('Locale')
      else LocaleCode := '';
    if ValueExists('WorldSize') then WorldSize := Reg.ReadInteger('WorldSize')
      else WorldSize := DefaultWorldSize;
    if ValueExists('LandMass') then StartLandMass := Reg.ReadInteger('LandMass')
      else StartLandMass := DefaultLandMass;
    if ValueExists('MaxTurn') then MaxTurn := Reg.ReadInteger('MaxTurn')
      else MaxTurn := 800;
    if ValueExists('DefaultAI') then DefaultAI := Reg.ReadString('DefaultAI')
      else DefaultAI := 'StdAI';
    if ValueExists('AutoEnemies') then AutoEnemies := Reg.ReadInteger('AutoEnemies')
      else AutoEnemies := 8;
    if ValueExists('AutoDiff') then AutoDiff := Reg.ReadInteger('AutoDiff')
      else AutoDiff := 1;

    if ValueExists('ScreenMode') then
      ScreenMode := ReadInteger('ScreenMode')
      else ScreenMode := 1;
    FullScreen := ScreenMode > 0;
    if ValueExists('ResolutionX') then
      ResolutionX := ReadInteger('ResolutionX');
    if ValueExists('ResolutionY') then
      ResolutionY := ReadInteger('ResolutionY');
    if ValueExists('ResolutionBPP') then
      ResolutionBPP := ReadInteger('ResolutionBPP');
    if ValueExists('ResolutionFreq') then
      ResolutionFreq := ReadInteger('ResolutionFreq');
    if ValueExists('MultiControl') then
      MultiControl := ReadInteger('MultiControl')
      else MultiControl := 0;
    {$IFDEF WINDOWS}
    if ScreenMode = 2 then
      ChangeResolution(ResolutionX, ResolutionY, ResolutionBPP,
        ResolutionFreq);
    {$ENDIF}
  finally
    Free;
  end;
end;

procedure TStartDlg.SaveConfig;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  with Reg do try
    OpenKey(AppRegistryKey, True);
    WriteInteger('WorldSize', WorldSize);
    WriteInteger('LandMass', StartLandMass);
    WriteString('Locale', LocaleCode);
    WriteInteger('Gamma', Gamma);
    if FullScreen then WriteInteger('ScreenMode', 1)
      else WriteInteger('ScreenMode', 0);
    WriteInteger('MultiControl', MultiControl);
  finally
    Free;
  end;
end;

procedure TStartDlg.LoadAiBrainsPictures;
var
  AIBrains: TBrains;
  I: Integer;
begin
  AIBrains := TBrains.Create(False);
  Brains.GetByKind(btAI, AIBrains);
  for i := 0 to AIBrains.Count - 1 do
  with AIBrains[I] do begin
    if not LoadGraphicFile(AIBrains[i].Picture, GetAiDir + DirectorySeparator +
      FileName + DirectorySeparator + FileName + '.png', gfNoError) then begin
      with AIBrains[i].Picture.Canvas do begin
        Brush.Color := $904830;
        FillRect(Rect(0, 0, 64, 64));
        Font.Assign(UniFont[ftTiny]);
        Font.Style := [];
        Font.Color := $5FDBFF;
        Textout(32 - TextWidth(FileName) div 2,
          32 - TextHeight(FileName) div 2, FileName);
      end;
    end;
  end;
  AIBrains.Free;
end;

procedure TStartDlg.UpdateInterface;
var
  r0, r1: HRgn;
  Location: TPoint;
begin
  if FullScreen then begin
    Location := Point((Screen.Width - 800) * 3 div 8,
      Screen.Height - Height - (Screen.Height - 600) div 3);
    BoundsRect := Bounds(Location.X, Location.Y, Width, Height);

    r0 := CreateRectRgn(0, 0, Width, Height);
    r1 := CreateRectRgn(TabOffset + 4 * TabSize + 2, 0, Width, TabHeight);
    CombineRgn(r0, r0, r1, RGN_DIFF);
    DeleteObject(r1);
    r1 := CreateRectRgn(QuitBtn.Left, QuitBtn.Top, QuitBtn.Left + QuitBtn.Width,
      QuitBtn.top + QuitBtn.Height);
    CombineRgn(r0, r0, r1, RGN_OR);
    DeleteObject(r1);
    SetWindowRgn(Handle, r0, False);
    DeleteObject(r0); // causes crash with Windows 95
  end else begin
    BoundsRect := Bounds((Screen.Width - Width) div 2,
      (Screen.Height - Height) div 2, Width, Height)
  end;
end;

procedure TStartDlg.DrawAction(y, IconIndex: integer; HeaderItem, TextItem: string);
begin
  Canvas.Font.Assign(UniFont[ftCaption]);
  Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
  RisedTextOut(Canvas, xAction, y - 3, Phrases2.Lookup(HeaderItem));
  Canvas.Font.Assign(UniFont[ftNormal]);
  BiColorTextOut(Canvas, Colors.Canvas.Pixels[clkAge0 - 1, cliDimmedText],
    $000000, xAction, y + 21, Phrases2.Lookup(TextItem));

  // TODO: Explicitly clear background to black but in fact BitBlt SRCCOPY should do it
  LogoBuffer.Canvas.FillRect(0, 0, LogoBuffer.Width, LogoBuffer.Height);
  BitBltCanvas(LogoBuffer.Canvas, 0, 0, 50, 50, Canvas,
    xActionIcon - 2, y - 2);
  GlowFrame(LogoBuffer, 8, 8, 34, 34, $202020);
  BitBltCanvas(Canvas, xActionIcon - 2, y - 2, 50, 50,
    LogoBuffer.Canvas, 0, 0);
  BitBltCanvas(Canvas, xActionIcon, y, 40, 40, BigImp.Canvas,
    (IconIndex mod 7) * xSizeBig + 8, (IconIndex div 7) * ySizeBig);
  RFrame(Canvas, xActionIcon - 1, y - 1, xActionIcon + 40, y + 40,
    $000000, $000000);
end;

procedure TStartDlg.FormPaint(Sender: TObject);
const
  TabNames: array[TStartTab] of Integer = (0, 11, 3, 4);
var
  i, w, h, xMini, yMini, y: integer;
  s: string;
  Tab2: TStartTab;
  MainAction: TMainAction;
begin
  PaintBackground(self, 3, 3, TabOffset + 4 * TabSize - 4, TabHeight - 3);
  PaintBackground(self, 3, TabHeight + 3, ClientWidth - 6,
    ClientHeight - TabHeight - 6);
  with Canvas do
  begin
    Brush.Color := $000000;
    FillRect(Rect(0, 1, ClientWidth, 3));
    FillRect(Rect(TabOffset + 4 * TabSize + 2, 0, ClientWidth, TabHeight));
    Brush.Style := bsClear;
  end;
  if Page in [pgStartRandom, pgStartMap] then
  begin
    Frame(Canvas, 328, yMain + 112 - 15, ClientWidth, Up2Btn.top + 38,
      MainTexture.clBevelShade, MainTexture.clBevelLight);
    if AutoDiff > 0 then
    begin
      Frame(Canvas, -1 { x0Brain-dxBrain } ,
        yMain + 112 - 15 { Up1Btn.Top-12 }{ y0Brain-dyBrain } ,
        x0Brain + dxBrain + 64, Up2Btn.top + 38 { y0Brain+dyBrain+64 } ,
        MainTexture.clBevelShade, MainTexture.clBevelLight);
    end;
  end
  else if Page <> pgMain then
    Frame(Canvas, 328, Up1Btn.top - 15, ClientWidth, Up2Btn.top + 38,
      MainTexture.clBevelShade, MainTexture.clBevelLight);
  Frame(Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);

  // draw tabs
  Frame(Canvas, 2, 2 + 2 * integer(Tab <> tbMain), TabOffset + (0 + 1) * TabSize - 1,
    TabHeight, MainTexture.clBevelLight, MainTexture.clBevelShade);
  Frame(Canvas, 1, 1 + 2 * integer(Tab <> tbMain), TabOffset + (0 + 1) * TabSize,
    TabHeight, MainTexture.clBevelLight, MainTexture.clBevelShade);
  Canvas.Pixels[1, 1 + 2 * integer(Tab <> tbMain)] := MainTexture.clBevelShade;
  for Tab2 := tbMap to tbPrevious do
  begin
    Frame(Canvas, TabOffset + Integer(Tab2) * TabSize + 2, 2 + 2 * integer(Tab <> Tab2),
      TabOffset + (Integer(Tab2) + 1) * TabSize - 1, TabHeight, MainTexture.clBevelLight,
      MainTexture.clBevelShade);
    Frame(Canvas, TabOffset + Integer(Tab2) * TabSize + 1, 1 + 2 * integer(Tab <> Tab2),
      TabOffset + (Integer(Tab2) + 1) * TabSize, TabHeight, MainTexture.clBevelLight,
      MainTexture.clBevelShade);
    Canvas.Pixels[TabOffset + Integer(Tab2) * TabSize + 1, 1 + 2 * integer(Tab <> Tab2)] :=
      MainTexture.clBevelShade;
  end;
  Canvas.Font.Assign(UniFont[ftNormal]);
  for Tab2 := tbMap to tbPrevious do
  begin
    s := Phrases.Lookup('STARTCONTROLS', TabNames[Tab2]);
    RisedTextOut(Canvas, TabOffset + Integer(Tab2) * TabSize + 1 +
      (TabSize - BiColorTextWidth(Canvas, s)) div 2,
      10 + 2 * integer(Tab <> Tab2), s);
  end;
  Frame(Canvas, TabOffset + 4 * TabSize + 1, -1, ClientWidth, TabHeight,
    $000000, $000000);
  Frame(Canvas, 1, TabHeight + 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.clBevelLight, MainTexture.clBevelShade);
  Frame(Canvas, 2, TabHeight + 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.clBevelLight, MainTexture.clBevelShade);
  if Tab = tbMain then
  begin
    PaintBackground(self, 3, TabHeight - 1, TabSize - 4 - 3 + TabOffset + 3, 4);
    Canvas.Pixels[2, TabHeight] := MainTexture.clBevelLight;
  end
  else
  begin
    PaintBackground(self, TabOffset + 3 + Integer(Tab) * TabSize, TabHeight - 1,
      TabSize - 4, 4);
    Canvas.Pixels[TabOffset + Integer(Tab) * TabSize + 2, TabHeight] :=
      MainTexture.clBevelLight;
  end;
  Canvas.Pixels[TabOffset + (Integer(Tab) + 1) * TabSize - 1, TabHeight + 1] :=
    MainTexture.clBevelShade;
  if Tab < tbPrevious then
    Frame(Canvas, TabOffset + (Integer(Tab) + 1) * TabSize + 1, 3,
      TabOffset + (Integer(Tab) + 1) * TabSize + 2, TabHeight, MainTexture.clBevelShade,
      MainTexture.clBevelShade); // Tab shadow
  // TODO: Explicitly clear background to black but in fact BitBlt SRCCOPY should do it
  LogoBuffer.Canvas.FillRect(0, 0, LogoBuffer.Width, LogoBuffer.Height);
  BitBltCanvas(LogoBuffer.Canvas, 0, 0, 36, 36, Canvas, 6,
    3 + 2 * integer(Tab <> tbMain));

  ImageOp_BCC(LogoBuffer, Templates, 0, 0, 145, 38, 36, 27, $BFBF20, $4040DF);
  // logo part 1
  ImageOp_BCC(LogoBuffer, Templates, 10, 27, 155, 38 + 27, 26, 9, $BFBF20,
    $4040DF); // logo part 2
  BitBltCanvas(Canvas, 6, 3 + 2 * integer(Tab <> tbMain), 36, 36,
    LogoBuffer.Canvas, 0, 0);

  if Page = pgMain then begin
    if SelectedAction <> maNone then // mark selected action
      for i := 0 to (ClientWidth - 2 * ActionSideBorder) div wBuffer + 1 do
      begin
        w := ClientWidth - 2 * ActionSideBorder - i * wBuffer;
        if w > wBuffer then
          w := wBuffer;
        h := ActionPitch;
        if yAction + Integer(SelectedAction) * ActionPitch - 8 + h > ClientHeight - ActionBottomBorder
        then
          h := ClientHeight - ActionBottomBorder -
            (yAction + Integer(SelectedAction) * ActionPitch - 8);
        // TODO: Explicitly clear background to black but in fact BitBlt SRCCOPY should do it
        LogoBuffer.Canvas.FillRect(0, 0, LogoBuffer.Width, LogoBuffer.Height);
        BitBltCanvas(LogoBuffer.Canvas, 0, 0, w, h, Canvas,
          ActionSideBorder + i * wBuffer, yAction + Integer(SelectedAction) * ActionPitch
          - 8);
        MakeBlue(LogoBuffer, 0, 0, w, h);
        BitBltCanvas(Canvas, ActionSideBorder + i * wBuffer,
          yAction + Integer(SelectedAction) * ActionPitch - 8, w, h,
          LogoBuffer.Canvas, 0, 0);
      end;
    y := yAction;
    for MainAction := Low(TMainActionSet) to High(TMainActionSet) do
    begin
      if MainAction in ActionsOffered then
        case MainAction of
          maConfig: DrawAction(y, 25, 'ACTIONHEADER_CONFIG', 'ACTION_CONFIG');
          maManual: DrawAction(y, 19, 'ACTIONHEADER_MANUAL', 'ACTION_MANUAL');
          maCredits: DrawAction(y, 22, 'ACTIONHEADER_CREDITS', 'ACTION_CREDITS');
          maAIDev: DrawAction(y, 24, 'ACTIONHEADER_AIDEV', 'ACTION_AIDEV');
          maWeb:
            begin
              Canvas.Font.Assign(UniFont[ftCaption]);
              // Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline];
              RisedTextOut(Canvas, xActionIcon + 99, y,
                Phrases2.Lookup('ACTIONHEADER_WEB'));
              Canvas.Font.Assign(UniFont[ftNormal]);
              // TODO: Explicitly clear background to black but in fact BitBlt SRCCOPY should do it
              LogoBuffer.Canvas.FillRect(0, 0, LogoBuffer.Width, LogoBuffer.Height);
              BitBltCanvas(LogoBuffer.Canvas, 0, 0, 91, 25, Canvas,
                xActionIcon, y + 2);
              ImageOp_BCC(LogoBuffer, Templates, 0, 0, 1, 400, 91, 25, 0,
                Colors.Canvas.Pixels[clkAge0 - 1, cliDimmedText]);
              BitBltCanvas(Canvas, xActionIcon, y + 2, 91, 25,
                LogoBuffer.Canvas, 0, 0);
            end;
        end;
      Inc(y, ActionPitch);
    end;
  end
  else if Page in [pgStartRandom, pgStartMap] then
  begin
    DLine(Canvas, 344, 514, y0Mini + 61 + 19, MainTexture.clBevelLight,
      MainTexture.clBevelShade);
    RisedTextOut(Canvas, 344, y0Mini + 61, Phrases.Lookup('STARTCONTROLS', 10));
    s := TurnToString(MaxTurn);
    RisedTextOut(Canvas, 514 - BiColorTextWidth(Canvas, s), y0Mini + 61, s);
    s := Phrases.Lookup('STARTCONTROLS', 7);
    w := Canvas.TextWidth(s);
    LoweredTextOut(Canvas, -2, MainTexture, x0Brain + 32 - w div 2,
      y0Brain + dyBrain + 69, s);

    InitOrnament;
    if AutoDiff < 0 then
    begin
      for i := 12 to 19 do
        if (i < 13) or (i > 17) then
        begin
          BitBltCanvas(Canvas, 9 + i * 27, yLogo - 2, wOrna, hOrna,
            GrExt[HGrSystem2].Mask.Canvas, xOrna, yOrna, SRCAND);
          BitBltCanvas(Canvas, 9 + i * 27, yLogo - 2, wOrna, hOrna,
            GrExt[HGrSystem2].Data.Canvas, xOrna, yOrna, SRCPAINT);
        end;
      PaintLogo(Canvas, 69 + 11 * 27, yLogo, MainTexture.clBevelLight,
        MainTexture.clBevelShade);

      for i := 0 to nPlOffered - 1 do
        if 1 shl i and SlotAvailable <> 0 then
        begin
          if Assigned(PlayersBrain[i]) then
            FrameImage(Canvas, PlayersBrain[i].Picture, xBrain[i], yBrain[i],
              64, 64, 0, 0, true)
          else
            FrameImage(Canvas, EmptyPicture, xBrain[i], yBrain[i], 64, 64,
              0, 0, true);
          if Assigned(PlayersBrain[I]) and (PlayersBrain[i].Kind in [btTerm, btRandom, btAI]) then
          begin
            BitBltCanvas(Canvas, xBrain[i] - 18, yBrain[i] + 19, 12, 14,
              GrExt[HGrSystem].Data.Canvas, 134 + (Difficulty[i] - 1) *
              13, 28);
            Frame(Canvas, xBrain[i] - 19, yBrain[i] + 18, xBrain[i] - 18 + 12,
              yBrain[i] + (19 + 14), $000000, $000000);
            RFrame(Canvas, PlayerSlots[i].DiffUpBtn.left - 1, PlayerSlots[i].DiffUpBtn.top - 1,
              PlayerSlots[i].DiffUpBtn.left + 12, PlayerSlots[i].DiffUpBtn.top + 24,
              MainTexture.clBevelShade, MainTexture.clBevelLight);
            with Canvas do
            begin
              Brush.Color := $000000;
              FillRect(Rect(xBrain[i] - 5, yBrain[i] + 25, xBrain[i] - 2,
                yBrain[i] + 27));
              Brush.Style := bsClear;
            end;
            if PlayerSlots[I].OfferMultiple then
            begin
              RFrame(Canvas, PlayerSlots[I].MultiBtn.left - 1, PlayerSlots[I].MultiBtn.top - 1,
                PlayerSlots[I].MultiBtn.left + 12, PlayerSlots[I].MultiBtn.top + 12,
                MainTexture.clBevelShade, MainTexture.clBevelLight);
              BitBltCanvas(Canvas, xBrain[i] - 31, yBrain[i], 13, 12,
                GrExt[HGrSystem].Data.Canvas, 88, 47);
            end;
          end;
          if Assigned(PlayersBrain[i]) then
          begin
            PlayerSlots[i].DiffUpBtn.Hint := Format(Phrases.Lookup('STARTCONTROLS', 9),
              [PlayersBrain[i].Name]);
            PlayerSlots[i].DiffDownBtn.Hint := PlayerSlots[i].DiffUpBtn.Hint;
          end;
        end;
    end
    else
    begin
      DLine(Canvas, 24, 198, yMain + 140 + 19, MainTexture.clBevelLight,
        MainTexture.clBevelShade);
      RisedTextOut(Canvas, 24 { x0Brain+32-BiColorTextWidth(Canvas,s) div 2 } ,
        yMain + 140 { y0Mini-77 } , Phrases.Lookup('STARTCONTROLS', 15));
      if Page = pgStartRandom then
        s := IntToStr(AutoEnemies)
      else if nMapStartPositions = 0 then
        s := '0'
      else
        s := IntToStr(nMapStartPositions - 1);
      RisedTextOut(Canvas, 198 - BiColorTextWidth(Canvas, s), yMain + 140, s);
      DLine(Canvas, 24, xDefault - 6, yMain + 164 + 19,
        MainTexture.clBevelLight, MainTexture.clBevelShade);
      RisedTextOut(Canvas, 24 { x0Brain+32-BiColorTextWidth(Canvas,s) div 2 } ,
        yMain + 164 { y0Mini-77 } , Phrases.Lookup('STARTCONTROLS', 16));
      if AutoDiff = 1 then
        FrameImage(Canvas, BrainBeginner.Picture, xDefault, yDefault, 64,
          64, 0, 0, false)
      else
        FrameImage(Canvas, BrainDefault.Picture, xDefault, yDefault, 64, 64,
          0, 0, true);
      DLine(Canvas, 56, 272, y0Mini + 61 + 19, MainTexture.clBevelLight,
        MainTexture.clBevelShade);
      RisedTextOut(Canvas, 56, y0Mini + 61,
        Phrases.Lookup('STARTCONTROLS', 14));
      s := Phrases.Lookup('AUTODIFF', AutoDiff - 1);
      RisedTextOut(Canvas, 272 - BiColorTextWidth(Canvas, s), y0Mini + 61, s);

      for i := 0 to 19 do
        if (i < 2) or (i > 6) then
        begin
          BitBltCanvas(Canvas, 9 + i * 27, yLogo - 2, wOrna, hOrna,
            GrExt[HGrSystem2].Mask.Canvas, xOrna, yOrna, SRCAND);
          BitBltCanvas(Canvas, 9 + i * 27, yLogo - 2, wOrna, hOrna,
            GrExt[HGrSystem2].Data.Canvas, xOrna, yOrna, SRCPAINT);
        end;
      PaintLogo(Canvas, 69, yLogo, MainTexture.clBevelLight,
        MainTexture.clBevelShade);
    end;
  end
  else if Page = pgLoad then
  begin
    // RisedTextOut(Canvas,x0Mini+2-BiColorTextWidth(Canvas,BookDate) div 2,y0Mini-73,BookDate);
    if LastTurn > 0 then
    begin
      PaintProgressBar(Canvas, 6, xTurnSlider, yTurnSlider, 0,
        LoadTurn * wTurnSlider div LastTurn, wTurnSlider, MainTexture);
      Frame(Canvas, xTurnSlider - 2, yTurnSlider - 2, xTurnSlider + wTurnSlider
        + 1, yTurnSlider + 8, $B0B0B0, $FFFFFF);
      RFrame(Canvas, xTurnSlider - 3, yTurnSlider - 3, xTurnSlider + wTurnSlider
        + 2, yTurnSlider + 9, $FFFFFF, $B0B0B0);
    end
    else
      DLine(Canvas, 344, 514, y0Mini + 61 + 19, MainTexture.clBevelLight,
        MainTexture.clBevelShade);
    RisedTextOut(Canvas, 344, y0Mini + 61, Phrases.Lookup('STARTCONTROLS', 8));
    s := TurnToString(LoadTurn);
    RisedTextOut(Canvas, 514 - BiColorTextWidth(Canvas, s), y0Mini + 61, s);
  end
  else if Page = pgEditRandom then
  begin
    DLine(Canvas, 344, 514, y0Mini - 77 + 19, MainTexture.clBevelLight,
      MainTexture.clBevelShade);
    RisedTextOut(Canvas, 344, y0Mini - 77, Phrases.Lookup('STARTCONTROLS', 5));
    s := IntToStr((WorldSizes[WorldSize].X * WorldSizes[WorldSize].Y * 20 +
      DefaultWorldTiles div 2) div DefaultWorldTiles * 5) + '%';
    RisedTextOut(Canvas, 514 - BiColorTextWidth(Canvas, s), y0Mini - 77, s);
    DLine(Canvas, 344, 514, y0Mini + 61 + 19, MainTexture.clBevelLight,
      MainTexture.clBevelShade);
    RisedTextOut(Canvas, 344, y0Mini + 61, Phrases.Lookup('STARTCONTROLS', 6));
    s := IntToStr(StartLandMass) + '%';
    RisedTextOut(Canvas, 514 - BiColorTextWidth(Canvas, s), y0Mini + 61, s);
  end
  else if Page = pgEditMap then
  begin
    // DLine(Canvas,344,514,y0Mini+61+19,MainTexture.clBevelLight,MainTexture.clBevelShade);
    s := Format(Phrases2.Lookup('MAPPROP'),
      [(nMapLandTiles * 100 + 556) div 1112,
      // 1112 is typical for world with 100% size and default land mass
      nMapStartPositions]);
    RisedTextOut(Canvas, x0Mini - BiColorTextWidth(Canvas, s) div 2,
      y0Mini + 61, s);
  end;

  if StartBtn.Visible then
    BtnFrame(Canvas, StartBtn.BoundsRect, MainTexture);
  if Up2Btn.Visible then
    RFrame(Canvas, Up2Btn.left - 1, Up2Btn.top - 1, Up2Btn.left + 12,
      Up2Btn.top + 24, MainTexture.clBevelShade, MainTexture.clBevelLight);
  if Up1Btn.Visible then
    RFrame(Canvas, Up1Btn.left - 1, Up1Btn.top - 1, Up1Btn.left + 12,
      Up1Btn.top + 24, MainTexture.clBevelShade, MainTexture.clBevelLight);
  if AutoDiffUpBtn.Visible then
    RFrame(Canvas, AutoDiffUpBtn.left - 1, AutoDiffUpBtn.top - 1,
      AutoDiffUpBtn.left + 12, AutoDiffUpBtn.top + 24, MainTexture.clBevelShade,
      MainTexture.clBevelLight);
  if AutoEnemyUpBtn.Visible then
    RFrame(Canvas, AutoEnemyUpBtn.left - 1, AutoEnemyUpBtn.top - 1,
      AutoEnemyUpBtn.left + 12, AutoEnemyUpBtn.top + 24,
      MainTexture.clBevelShade, MainTexture.clBevelLight);
  if CustomizeBtn.Visible then
    RFrame(Canvas, CustomizeBtn.left - 1, CustomizeBtn.top - 1,
      CustomizeBtn.left + 12, CustomizeBtn.top + 12, MainTexture.clBevelShade,
      MainTexture.clBevelLight);
  if List.Visible then
    EditFrame(Canvas, List.BoundsRect, MainTexture);
  if RenameBtn.Visible then
    BtnFrame(Canvas, RenameBtn.BoundsRect, MainTexture);
  if DeleteBtn.Visible then
    BtnFrame(Canvas, DeleteBtn.BoundsRect, MainTexture);
  if Page = pgLoad then
    BtnFrame(Canvas, ReplayBtn.BoundsRect, MainTexture);

  if not (Page in [pgMain, pgNoLoad]) then
  begin
    xMini := x0Mini - MiniMap.Size.X;
    yMini := y0Mini - MiniMap.Size.Y div 2;
    Frame(Canvas, xMini, yMini, xMini + 3 + MiniMap.Size.X * 2,
      yMini + 3 + MiniMap.Size.Y, MainTexture.clBevelLight,
      MainTexture.clBevelShade);
    Frame(Canvas, xMini + 1, yMini + 1, xMini + 2 + MiniMap.Size.X * 2,
      yMini + 2 + MiniMap.Size.Y, MainTexture.clBevelShade,
      MainTexture.clBevelLight);

    s := '';
    if MiniMap.Mode = mmPicture then
    begin
      BitBltCanvas(Canvas, xMini + 2, yMini + 2, MiniMap.Size.X * 2, MiniMap.Size.Y,
        MiniMap.Bitmap.Canvas, 0, 0);
      if Page = pgStartRandom then
        s := Phrases.Lookup('RANMAP')
    end
    else if MiniMap.Mode = mmMultiPlayer then
      s := Phrases.Lookup('MPMAP')
    else if Page = pgStartMap then
      s := Copy(MapFileName, 1, Length(MapFileName) - 9)
    else if Page = pgEditMap then
      s := List.Items[List.ItemIndex]
    else if Page = pgNoLoad then
      s := Phrases.Lookup('NOGAMES');
    if s <> '' then
      RisedTextOut(Canvas, x0Mini + 2 - BiColorTextWidth(Canvas, s) div 2,
        y0Mini - 8, s);
  end;
end;

procedure TStartDlg.FormShow(Sender: TObject);
begin
  SetMainTextureByAge(-1);
  List.Font.Color := MainTexture.clMark;

  Fill(EmptyPicture.Canvas, Bounds(0, 0, 64, 64),
    Point((wMaintexture - 64) div 2, (hMaintexture - 64) div 2));

  DarkenImage(EmptyPicture, 28);

  Difficulty[0] := Diff0;

  SelectedAction := maNone;
  if ShowTab = tbPrevious then
    PreviewMap(StartLandMass); // avoid delay on first TabX change
  ChangeTab(ShowTab);
  Background.Enabled := False;
end;

procedure TStartDlg.UnlistBackupFile(FileName: string);
var
  I: Integer;
begin
  if FileName[1] <> '~' then
    FileName := '~' + FileName;
  I := FormerGames.Count - 1;
  while (I >= 0) and (AnsiCompareFileName(FormerGames[I], FileName) <> 0) do
    Dec(I);
  if I >= 0 then
  begin
    FormerGames.Delete(I);
    if ListIndex[tbNew] = I then
      ListIndex[tbNew] := 0
  end;
end;

procedure TStartDlg.StartBtnClick(Sender: TObject);
var
  I, GameCount, MapCount: Integer;
  FileName: string;
  Reg: TRegistry;
begin
  case Page of
    pgLoad:
      begin // load
        FileName := List.Items[List.ItemIndex];
        if LoadGame(GetSavedDir + DirectorySeparator, FileName + CevoExt, LoadTurn, false)
        then
          UnlistBackupFile(FileName)
        else
          SimpleMessage(Phrases.Lookup('LOADERR'));
        SlotAvailable := -1;
      end;
    pgStartRandom, pgStartMap:
      if Assigned(PlayersBrain[0]) then
      begin
        if (Page = pgStartMap) and (nMapStartPositions = 0) and (AutoDiff > 0)
        then
        begin
          SimpleMessage(Phrases.Lookup('NOSTARTPOS'));
          Exit;
        end;

        Reg := TRegistry.Create;
        with Reg do
        try
          OpenKey(AppRegistryKey, true);
          if ValueExists('GameCount') then GameCount := ReadInteger('GameCount')
            else GameCount := 0;

          if (AutoDiff < 0) and (PlayersBrain[0].Kind = btNoTerm) then
            FileName := 'Round' + IntToStr(GetProcessID())
          else begin
            Inc(GameCount);
            FileName := Format(Phrases.Lookup('GAME'), [GameCount]);
          end;

          // Save settings and AI assignment
          if Page = pgStartRandom then begin
            SaveConfig;
            OpenKey(AppRegistryKey + '\AI', True);
            if AutoDiff < 0 then
              for I := 0 to nPlOffered - 1 do begin
                if not Assigned(PlayersBrain[I]) then
                  Reg.WriteString('Control' + IntToStr(I), '')
                else Reg.WriteString('Control' + IntToStr(I),
                  PlayersBrain[I].FileName);
                WriteInteger('Diff' + IntToStr(I), Difficulty[I]);
              end;
          end;

          OpenKey(AppRegistryKey, True);
          if AutoDiff > 0 then
          begin
            WriteString('DefaultAI', BrainDefault.FileName);
            SlotAvailable := 0; // PlayerSlot will be invalid hereafter
            PlayersBrain[0] := BrainTerm;
            Difficulty[0] := PlayerAutoDiff[AutoDiff];
            for I := 1 to nPl - 1 do
              if (Page = pgStartRandom) and (I <= AutoEnemies) or
                (Page = pgStartMap) and (I < nMapStartPositions) then begin
                if AutoDiff = 1 then PlayersBrain[I] := BrainBeginner
                  else PlayersBrain[I] := BrainDefault;
                Difficulty[I] := EnemyAutoDiff[AutoDiff];
              end  else PlayersBrain[I] := nil;
          end else begin
            for I := 6 to 8 do
              if (PlayersBrain[0].Kind <> btNoTerm) and (MultiControl and (1 shl I) <> 0)
              then begin
                PlayersBrain[I + 3] := PlayersBrain[I];
                Difficulty[I + 3] := Difficulty[I];
                PlayersBrain[I + 6] := PlayersBrain[I];
                Difficulty[I + 6] := Difficulty[I];
              end else begin
                PlayersBrain[I + 3] := nil;
                PlayersBrain[I + 6] := nil;
              end;
          end;

          WriteInteger('AutoDiff', AutoDiff);
          WriteInteger('AutoEnemies', AutoEnemies);
          WriteInteger('MaxTurn', MaxTurn);
          WriteInteger('GameCount', GameCount);
        finally
          Free;
        end;

        StartNewGame(GetSavedDir + DirectorySeparator, FileName + CevoExt, MapFileName,
          WorldSizes[WorldSize].X, WorldSizes[WorldSize].Y, StartLandMass, MaxTurn);
        UnlistBackupFile(FileName);
      end;
    pgEditMap:
      EditMap(MapFileName, lxmax, lymax, StartLandMass);
    pgEditRandom: // new map
      begin
        Reg := TRegistry.Create;
        with Reg do
        try
          OpenKey(AppRegistryKey, True);
          if ValueExists('MapCount') then MapCount := ReadInteger('MapCount')
            else MapCount := 0;
          Inc(MapCount);
          WriteInteger('MapCount', MapCount);
        finally
          Free;
        end;
        MapFileName := Format(Phrases.Lookup('MAP'), [MapCount]) + CevoMapExt;
        EditMap(MapFileName, WorldSizes[WorldSize].X, WorldSizes[WorldSize].Y, StartLandMass);
      end;
  end;
end;

procedure TStartDlg.PaintInfo;
begin
  case Page of
    pgStartRandom: begin
      MiniMap.Mode := mmPicture;
      MiniMap.PaintRandom(3, StartLandMass, WorldSize);
    end;
    pgNoLoad: begin
      MiniMap.Mode := mmNone;
      MiniMap.Size := WorldSizes[DefaultWorldSize];
    end;
    pgLoad: begin
        MiniMap.LoadFromLogFile(GetSavedDir + DirectorySeparator +
          List.Items[List.ItemIndex] + CevoExt, LastTurn);
        // BookDate:=DateToStr(FileDateToDateTime(FileAge(FileName)));
        if not TurnValid then  begin
          LoadTurn := LastTurn;
          SmartInvalidate(xTurnSlider - 2, y0Mini + 61,
            xTurnSlider + wTurnSlider + 2, yTurnSlider + 9);
        end;
        TurnValid := True;
      end;
    pgEditRandom: begin
      MapFileName := '';
      MiniMap.Mode := mmPicture;
      MiniMap.PaintRandom(4, StartLandMass, WorldSize);
    end;
    pgStartMap, pgEditMap:
      begin
        if Page = pgEditMap then
          MapFileName := List.Items[List.ItemIndex] + CevoMapExt;
        MiniMap.LoadFromMapFile(GetMapsDir + DirectorySeparator + MapFileName, nMapLandTiles, nMapStartPositions);
        if Page = pgEditMap then
          SmartInvalidate(x0Mini - 112, y0Mini + 61, x0Mini + 112, y0Mini + 91);
      end;
  end;
  SmartInvalidate(x0Mini - lxmax, y0Mini - lymax div 2,
    x0Mini - lxmax + 2 * lxmax + 4, y0Mini - lymax div 2 + lymax + 4);
end;

procedure TStartDlg.BrainClick(Sender: TObject);
var
  I: Integer;
begin
  // Play('BUTTON_UP');
  if PlayerPopupIndex < 0 then
  begin // change default AI
    BrainDefault := Brains[TMenuItem(Sender).Tag];
    SmartInvalidate(xDefault, yDefault, xDefault + 64, yDefault + 64);
  end
  else
  begin
    if Assigned(PlayersBrain[PlayerPopupIndex]) then
      PlayersBrain[PlayerPopupIndex].Flags := PlayersBrain[PlayerPopupIndex].Flags and not fUsed;
    if TMenuItem(Sender).Tag = -1 then begin
      PlayersBrain[PlayerPopupIndex] := nil;
      PlayerSlots[PlayerPopupIndex].DiffUpBtn.Visible := False;
      PlayerSlots[PlayerPopupIndex].DiffDownBtn.Visible := False;
      if PlayerSlots[PlayerPopupIndex].OfferMultiple then begin
        PlayerSlots[PlayerPopupIndex].MultiBtn.Visible := False;
        PlayerSlots[PlayerPopupIndex].MultiBtn.ButtonIndex := 2 + (MultiControl shr PlayerPopupIndex) and 1;
      end;
      MultiControl := MultiControl and not (1 shl PlayerPopupIndex);
    end else begin
      PlayersBrain[PlayerPopupIndex] := Brains[TMenuItem(Sender).Tag];
      PlayerSlots[PlayerPopupIndex].DiffUpBtn.Visible := PlayersBrain[PlayerPopupIndex].Kind in [btTerm, btRandom, btAI];
      PlayerSlots[PlayerPopupIndex].DiffDownBtn.Visible := PlayersBrain[PlayerPopupIndex].Kind in [btTerm, btRandom, btAI];
      if PlayerSlots[PlayerPopupIndex].OfferMultiple then begin
        PlayerSlots[PlayerPopupIndex].MultiBtn.Visible := PlayersBrain[PlayerPopupIndex].Kind in [btTerm, btRandom, btAI];
        PlayerSlots[PlayerPopupIndex].MultiBtn.ButtonIndex := 2 + (MultiControl shr PlayerPopupIndex) and 1;
      end;
      PlayersBrain[PlayerPopupIndex].Flags := PlayersBrain[PlayerPopupIndex].Flags or fUsed;
      if PlayersBrain[PlayerPopupIndex].Kind in [btNoTerm, btSuperVirtual] then
        Difficulty[PlayerPopupIndex] := 0 { supervisor }
      else
        Difficulty[PlayerPopupIndex] := 2;
      if (Page = pgStartRandom) and (PlayerSlots[PlayerPopupIndex].OfferMultiple) and
        (not Assigned(PlayersBrain[PlayerPopupIndex])) then
        MultiControl := MultiControl and not (1 shl PlayerPopupIndex);
      if (PlayerPopupIndex = 0) and (MapFileName <> '') then
        ChangePage(Page);
      if PlayersBrain[PlayerPopupIndex].Kind = btNoTerm then
      begin // turn all local players off
        for I := 1 to PlayerSlots.Count - 1 do
          if PlayersBrain[I].Kind = btTerm then begin
            PlayersBrain[I] := nil;
            PlayerSlots[I].DiffUpBtn.Visible := false;
            PlayerSlots[I].DiffUpBtn.Tag := 0;
            PlayerSlots[I].DiffDownBtn.Visible := false;
            PlayerSlots[I].DiffDownBtn.Tag := 0;
            if PlayerSlots[I].OfferMultiple then begin
              PlayerSlots[I].MultiBtn.Visible := false;
              PlayerSlots[I].MultiBtn.Tag := 0;
            end;
            SmartInvalidate(xBrain[I] - 31, yBrain[I] - 1, xBrain[I] + 64,
              PlayerSlots[I].DiffUpBtn.top + 25);
          end;
        BrainTerm.Flags := BrainTerm.Flags and not fUsed;
      end;
    end;
    SmartInvalidate(xBrain[PlayerPopupIndex] - 31, yBrain[PlayerPopupIndex] - 1,
      xBrain[PlayerPopupIndex] + 64, PlayerSlots[PlayerPopupIndex].DiffUpBtn.top + 25);
  end;
end;

procedure TStartDlg.OfferBrain(Brain: TBrain; FixedLines: Integer);
var
  J: Integer;
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(PopupMenu1);
  if not Assigned(Brain) then MenuItem.Caption := Phrases.Lookup('NOMOD')
    else MenuItem.Caption := Brain.Name;
  MenuItem.Tag := Brains.IndexOf(Brain);
  MenuItem.OnClick := BrainClick;
  J := FixedLines;
  while (J < PopupMenu1.Items.Count) and
    (StrIComp(pchar(MenuItem.Caption), pchar(PopupMenu1.Items[J].Caption)) > 0) do
    Inc(J);
  MenuItem.RadioItem := True;
  if (PlayerPopupIndex < 0) then MenuItem.Checked := BrainDefault = Brain
    else MenuItem.Checked := PlayersBrain[PlayerPopupIndex] = Brain;
  PopupMenu1.Items.Insert(J, MenuItem);
end;

procedure TStartDlg.InitPopup(PlayerIndex: Integer);
var
  I: Integer;
  FixedLines: integer;
  MenuItem: TMenuItem;
  AIBrains: TBrains;
begin
  PlayerPopupIndex := PlayerIndex;
  EmptyMenu(PopupMenu1.Items);
  if PlayerPopupIndex < 0 then begin // select default AI
    FixedLines := 0;
    if Brains.GetKindCount(btAI) >= 2 then begin
      OfferBrain(BrainRandom, FixedLines);
      Inc(FixedLines);
    end;
    AIBrains := TBrains.Create(False);
    Brains.GetByKind(btAI, AIBrains);
    for I := 0 to AIBrains.Count - 1 do // offer available AIs
      if AIBrains[I].Flags and fMultiple <> 0 then
        OfferBrain(AIBrains[I], FixedLines);
    AIBrains.Free;
  end else begin
    FixedLines := 0;
    if PlayerPopupIndex > 0 then begin
      OfferBrain(nil, FixedLines);
      Inc(FixedLines);
    end;
    for I := Brains.IndexOf(BrainTerm) downto 0 do // offer game interfaces
      if (PlayerPopupIndex = 0) or (Brains[i].Kind = btTerm) and
        (PlayersBrain[0].Kind <> btNoTerm) then begin
        OfferBrain(Brains[I], FixedLines);
        Inc(FixedLines);
      end;
    if PlayerPopupIndex > 0 then begin
      MenuItem := TMenuItem.Create(PopupMenu1);
      MenuItem.Caption := '-';
      PopupMenu1.Items.Add(MenuItem);
      Inc(FixedLines);
      if Brains.GetKindCount(btAI) >= 2 then begin
        OfferBrain(BrainRandom, FixedLines);
        Inc(FixedLines);
      end;
      AIBrains := TBrains.Create(False);
      Brains.GetByKind(btAI, AIBrains);
      for I := 0 to AIBrains.Count - 1 do // offer available AIs
        if (AIBrains[I].Flags and fMultiple <> 0) or (AIBrains[I].Flags and fUsed = 0)
          or (Brains[I] = PlayersBrain[PlayerPopupIndex]) then
          OfferBrain(AIBrains[i], FixedLines);
      AIBrains.Free;
    end;
  end;
end;

procedure TStartDlg.UpdateFormerGames;
var
  I: Integer;
  F: TSearchRec;
begin
  FormerGames.Clear;
  if FindFirst(GetSavedDir + DirectorySeparator + '*' + CevoExt, $21, F) = 0 then
    repeat
      I := FormerGames.Count;
      while (I > 0) and (F.Time < integer(FormerGames.Objects[I - 1])) do
        Dec(I);
      FormerGames.InsertObject(I, Copy(F.Name, 1, Length(F.Name) - 5),
        TObject(F.Time));
    until FindNext(F) <> 0;
  FindClose(F);
  ListIndex[tbNew] := FormerGames.Count - 1;
  if (ShowTab = tbNew) and (FormerGames.Count > 0) then
    ShowTab := tbPrevious;
  TurnValid := False;
end;

procedure TStartDlg.UpdateMaps;
var
  f: TSearchRec;
begin
  Maps.Clear;
  if FindFirst(GetMapsDir + DirectorySeparator + '*' + CevoMapExt, $21, f) = 0 then
    repeat
      Maps.Add(Copy(f.Name, 1, Length(f.Name) - 9));
    until FindNext(f) <> 0;
  FindClose(F);
  Maps.Sort;
  Maps.Insert(0, Phrases.Lookup('RANMAP'));
  ListIndex[tbMain] := Maps.IndexOf(Copy(MapFileName, 1, Length(MapFileName) - 9));
  if ListIndex[tbMain] < 0 then
    ListIndex[tbMain] := 0;
end;

procedure TStartDlg.ChangePage(NewPage: TStartPage);
var
  i, j, p1: integer;
  s: string;
  Reg: TRegistry;
  InvalidateTab0: boolean;
begin
  InvalidateTab0 := (Page = pgMain) or (NewPage = pgMain);
  Page := NewPage;
  case Page of
    pgStartRandom, pgStartMap:
      begin
        StartBtn.Caption := Phrases.Lookup('STARTCONTROLS', 1);
        if Page = pgStartRandom then
          i := nPlOffered
        else
        begin
          i := nMapStartPositions;
          if i = 0 then
          begin
            PlayersBrain[0] := BrainSuperVirtual;
            Difficulty[0] := 0
          end;
          if PlayersBrain[0].Kind in [btNoTerm, btSuperVirtual] then
            inc(i);
          if i > nPl then
            i := nPl;
          if i <= nPlOffered then
            MultiControl := 0
          else
            MultiControl := InitMulti[i];
        end;
        if InitAlive[i] <> SlotAvailable then
          if Page = pgStartRandom then
          begin // restore AI assignment of last start
            Reg := TRegistry.Create;
            with Reg do
            try
              OpenKey(AppRegistryKey + '\AI', True);
              for p1 := 0 to nPlOffered - 1 do begin
                PlayersBrain[p1] := nil;
                s := ReadString('Control' + IntToStr(p1));
                Difficulty[p1] := ReadInteger('Diff' + IntToStr(p1));
                if s <> '' then
                  for j := 0 to Brains.Count - 1 do
                    if AnsiCompareFileName(s, Brains[j].FileName) = 0 then
                      PlayersBrain[p1] := Brains[j];
              end;
            finally
              Free;
            end;
          end
          else
            for p1 := 1 to nPl - 1 do
              if 1 shl p1 and InitAlive[i] <> 0 then
              begin
                PlayersBrain[p1] := BrainDefault;
                Difficulty[p1] := 2;
              end
              else
                PlayersBrain[p1] := nil;
        SlotAvailable := InitAlive[i];
        for i := 0 to nPlOffered - 1 do
          if (AutoDiff < 0) and Assigned(PlayersBrain[i]) and
            (PlayersBrain[i].Kind in [btTerm, btRandom, btAI]) then
          begin
            PlayerSlots[i].DiffUpBtn.Tag := 768;
            PlayerSlots[i].DiffDownBtn.Tag := 768;
          end
          else
          begin
            PlayerSlots[i].DiffUpBtn.Tag := 0;
            PlayerSlots[i].DiffDownBtn.Tag := 0;
          end;
        for i := 6 to 8 do
          if (AutoDiff < 0) and Assigned(PlayersBrain[i]) and
            (PlayersBrain[i].Kind in [btTerm, btRandom, btAI]) then
          begin
            PlayerSlots[i].MultiBtn.Tag := 768;
            PlayerSlots[i].MultiBtn.ButtonIndex := 2 + (MultiControl shr i) and 1;
            PlayerSlots[i].MultiBtn.Enabled := Page = pgStartRandom
          end
          else
            PlayerSlots[i].MultiBtn.Tag := 0;
        if (AutoDiff > 0) and (Page <> pgStartMap) then
        begin
          AutoEnemyUpBtn.Tag := 768;
          AutoEnemyDownBtn.Tag := 768;
        end
        else
        begin
          AutoEnemyUpBtn.Tag := 0;
          AutoEnemyDownBtn.Tag := 0;
        end;
        if AutoDiff > 0 then
        begin
          AutoDiffUpBtn.Tag := 768;
          AutoDiffDownBtn.Tag := 768;
        end
        else
        begin
          AutoDiffUpBtn.Tag := 0;
          AutoDiffDownBtn.Tag := 0;
        end;
      end;

    pgNoLoad, pgLoad:
      begin
        StartBtn.Caption := Phrases.Lookup('STARTCONTROLS', 2);
        RenameBtn.Hint := Phrases.Lookup('BTN_RENGAME');
        DeleteBtn.Hint := Phrases.Lookup('BTN_DELGAME');
      end;

    pgEditRandom, pgEditMap:
      begin
        StartBtn.Caption := Phrases.Lookup('STARTCONTROLS', 12);
        RenameBtn.Hint := Phrases.Lookup('BTN_RENMAP');
        DeleteBtn.Hint := Phrases.Lookup('BTN_DELMAP');
      end;
  end;

  PaintInfo;
  for i := 0 to ControlCount - 1 do
    Controls[i].Visible := Controls[i].Tag and (256 shl Integer(Page)) <> 0;
  if Page = pgLoad then
    ReplayBtn.Visible := MiniMap.Mode <> mmMultiPlayer;
  List.Invalidate;
  SmartInvalidate(0, 0, ClientWidth, ClientHeight, invalidateTab0);
end;

procedure TStartDlg.ChangeTab(NewTab: TStartTab);
begin
  Tab := NewTab;
  case Tab of
    tbMap:
      List.Items.Assign(Maps);
    tbPrevious:
      List.Items.Assign(FormerGames);
  end;
  if Tab <> tbNew then
    if List.Count > 0 then begin
      if (ListIndex[Tab] < List.Count) and (ListIndex[Tab] >= 0) then
        List.ItemIndex := ListIndex[Tab]
        else List.ItemIndex := 0;
    end else List.ItemIndex := -1;
  case Tab of
    tbMain:
      ChangePage(pgMain);
    tbMap:
      if List.ItemIndex = 0 then
        ChangePage(pgEditRandom)
      else
        ChangePage(pgEditMap);
    tbNew:
      if MapFileName = '' then
        ChangePage(pgStartRandom)
      else
        ChangePage(pgStartMap);
    tbPrevious:
      if FormerGames.Count = 0 then
        ChangePage(pgNoLoad)
      else
        ChangePage(pgLoad);
  end;
end;

procedure TStartDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
var
  I: Integer;
begin
  if (y < TabHeight + 1) and (x - TabOffset < TabSize * 4) and
    ((x - TabOffset) div TabSize <> Integer(Tab)) then
  begin
    // Play('BUTTON_DOWN');
    ListIndex[Tab] := List.ItemIndex;
    ChangeTab(TStartTab((x - TabOffset) div TabSize));
  end
  else if Page = pgMain then
  begin
    case SelectedAction of
      maConfig:
        begin
          SettingsDlg := TSettingsDlg.Create(nil);
          if SettingsDlg.ShowModal = mrOk then begin
            LoadAssets;
            Invalidate;
            UpdateInterface;
            Background.UpdateInterface;
          end;
          FreeAndNil(SettingsDlg);
        end;
      maManual:
        DirectHelp(cStartHelp);
      maCredits:
        DirectHelp(cStartCredits);
      maAIDev:
        OpenDocument(HomeDir + AITemplateFileName);
      maWeb:
        OpenURL(CevoHomepage);
    end;
  end
  else if (AutoDiff < 0) and ((Page = pgStartRandom) or (Page = pgStartMap) and
    (nMapStartPositions > 0)) then
  begin
    for I := 0 to nPlOffered - 1 do
      if (1 shl I and SlotAvailable <> 0) and (x >= xBrain[I]) and
        (y >= yBrain[I]) and (x < xBrain[I] + 64) and (y < yBrain[I] + 64) then
      begin
        InitPopup(I);
        if yBrain[I] > y0Brain then
          PopupMenu1.Popup(left + xBrain[I] + 4, top + yBrain[I] + 60)
        else
          PopupMenu1.Popup(left + xBrain[I] + 4, top + yBrain[I] + 4);
      end
  end
  else if (AutoDiff > 1) and ((Page = pgStartRandom) or (Page = pgStartMap)) and
    (x >= xDefault) and (y >= yDefault) and (x < xDefault + 64) and
    (y < yDefault + 64) then
    if Brains.GetKindCount(btAI) < 2 then
      SimpleMessage(Phrases.Lookup('NOALTAI'))
    else
    begin
      InitPopup(-1);
      PopupMenu1.Popup(left + xDefault + 4, top + yDefault + 4);
    end
  else if (Page = pgLoad) and (LastTurn > 0) and (y >= yTurnSlider) and
    (y < yTurnSlider + 7) and (x >= xTurnSlider) and
    (x <= xTurnSlider + wTurnSlider) then
  begin
    LoadTurn := LastTurn * (x - xTurnSlider) div wTurnSlider;
    SmartInvalidate(xTurnSlider - 2, y0Mini + 61, xTurnSlider + wTurnSlider + 2,
      yTurnSlider + 9);
    Tracking := True;
  end;
end;

procedure TStartDlg.Up2BtnClick(Sender: TObject);
begin
  case Page of
    pgStartRandom, pgStartMap:
      if MaxTurn < 1400 then
      begin
        inc(MaxTurn, 200);
        SmartInvalidate(344, y0Mini + 61, 514, y0Mini + 82);
      end;
    pgLoad:
      if LoadTurn < LastTurn then
      begin
        inc(LoadTurn);
        SmartInvalidate(xTurnSlider - 2, y0Mini + 61, xTurnSlider + wTurnSlider
          + 2, yTurnSlider + 9);
      end;
    pgEditRandom:
      if StartLandMass < 96 then
      begin
        inc(StartLandMass, 5);
        PaintInfo;
        SmartInvalidate(344, y0Mini + 61, 514, y0Mini + 61 + 21);
      end;
  end;
end;

procedure TStartDlg.Down2BtnClick(Sender: TObject);
begin
  case Page of
    pgStartRandom, pgStartMap:
      if MaxTurn > 400 then
      begin
        dec(MaxTurn, 200);
        SmartInvalidate(344, y0Mini + 61, 514, y0Mini + 82);
      end;
    pgLoad:
      if LoadTurn > 0 then
      begin
        dec(LoadTurn);
        SmartInvalidate(xTurnSlider - 2, y0Mini + 61, xTurnSlider + wTurnSlider
          + 2, yTurnSlider + 9);
      end;
    pgEditRandom:
      if StartLandMass > 10 then
      begin
        dec(StartLandMass, 5);
        PaintInfo;
        SmartInvalidate(344, y0Mini + 61, 514, y0Mini + 61 + 21);
      end;
  end;
end;

procedure TStartDlg.Up1BtnClick(Sender: TObject);
begin
  if WorldSize < MaxWorldSize - 1 then
  begin
    Inc(WorldSize);
    PaintInfo;
    SmartInvalidate(344, y0Mini - 77, 510, y0Mini - 77 + 21);
  end;
end;

procedure TStartDlg.Down1BtnClick(Sender: TObject);
begin
  if WorldSize > 0 then
  begin
    Dec(WorldSize);
    PaintInfo;
    SmartInvalidate(344, y0Mini - 77, 510, y0Mini - 77 + 21);
  end;
end;

procedure TStartDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DirectDlg.Close;
end;

procedure TStartDlg.ListClick(Sender: TObject);
var
  I: Integer;
begin
  if (Tab = tbMap) and ((List.ItemIndex = 0) <> (Page = pgEditRandom)) then
  begin
    if List.ItemIndex = 0 then
      Page := pgEditRandom
    else
      Page := pgEditMap;
    for I := 0 to ControlCount - 1 do
      Controls[I].Visible := Controls[I].Tag and (256 shl Integer(Page)) <> 0;
    SmartInvalidate(328, Up1Btn.top - 12, ClientWidth, Up2Btn.top + 35);
  end;
  if Page = pgLoad then
    TurnValid := False;
  PaintInfo;
  if Page = pgLoad then
    ReplayBtn.Visible := MiniMap.Mode <> mmMultiPlayer;
end;

procedure TStartDlg.RenameBtnClick(Sender: TObject);
var
  i: integer;
  NewName: string;
  f: file;
  ok: boolean;
begin
  if List.ItemIndex >= 0 then
  begin
    if Page = pgLoad then
      InputDlg.Caption := Phrases.Lookup('TITLE_BOOKNAME')
    else
      InputDlg.Caption := Phrases.Lookup('TITLE_MAPNAME');
    InputDlg.EInput.Text := List.Items[List.ItemIndex];
    InputDlg.CenterToRect(BoundsRect);
    InputDlg.ShowModal;
    NewName := InputDlg.EInput.Text;
    while (NewName <> '') and (NewName[1] = '~') do
      Delete(NewName, 1, 1);
    if (InputDlg.ModalResult = mrOK) and (NewName <> '') and
      (NewName <> List.Items[List.ItemIndex]) then
    begin
      for i := 1 to Length(NewName) do
        if NewName[i] in ['\', '/', ':', '*', '?', '"', '<', '>', '|'] then
        begin
          SimpleMessage(Format(Phrases.Lookup('NOFILENAME'), [NewName[i]]));
          exit
        end;
      if Page = pgLoad then
        AssignFile(f, GetSavedDir + DirectorySeparator + List.Items[List.ItemIndex] + CevoExt)
      else
        AssignFile(f, GetMapsDir + DirectorySeparator + List.Items[List.ItemIndex] +
          CevoMapExt);
      ok := true;
      try
        if Page = pgLoad then
          Rename(f, GetSavedDir + DirectorySeparator + NewName + CevoExt)
        else
          Rename(f, GetMapsDir + DirectorySeparator + NewName + CevoMapExt);
      except
        // Play('INVALID');
        ok := false
      end;
      if Page <> pgLoad then
        try // rename map picture
          AssignFile(f, GetMapsDir + DirectorySeparator + List.Items[List.ItemIndex]
            + '.png');
          Rename(f, GetMapsDir + DirectorySeparator + NewName + '.png');
        except
        end;
      if ok then
      begin
        if Page = pgLoad then
          FormerGames[List.ItemIndex] := NewName
        else
          Maps[List.ItemIndex] := NewName;
        List.Items[List.ItemIndex] := NewName;
        if Page = pgEditMap then
          PaintInfo;
        List.Invalidate;
      end;
    end;
  end;
end;

procedure TStartDlg.DeleteBtnClick(Sender: TObject);
var
  iDel: integer;
  f: file;
begin
  if List.ItemIndex >= 0 then
  begin
    if Page = pgLoad then
      MessgDlg.MessgText := Phrases.Lookup('DELETEQUERY')
    else
      MessgDlg.MessgText := Phrases.Lookup('MAPDELETEQUERY');
    MessgDlg.Kind := mkOKCancel;
    MessgDlg.ShowModal;
    if MessgDlg.ModalResult = mrOK then
    begin
      if Page = pgLoad then
        AssignFile(f, GetSavedDir + DirectorySeparator + List.Items[List.ItemIndex] + CevoExt)
      else
        AssignFile(f, GetMapsDir + DirectorySeparator + List.Items[List.ItemIndex] +
          CevoMapExt);
      Erase(f);
      iDel := List.ItemIndex;
      if Page = pgLoad then
        FormerGames.Delete(iDel)
      else
        Maps.Delete(iDel);
      List.Items.Delete(iDel);
      if List.Items.Count = 0 then
        ChangePage(pgNoLoad)
      else
      begin
        if iDel = 0 then
          List.ItemIndex := 0
        else
          List.ItemIndex := iDel - 1;
        if (Page = pgEditMap) and (List.ItemIndex = 0) then
          ChangePage(pgEditRandom)
        else
        begin
          List.Invalidate;
          if Page = pgLoad then
            TurnValid := false;
          PaintInfo;
          if Page = pgLoad then
            ReplayBtn.Visible := MiniMap.Mode <> mmMultiPlayer;
        end;
      end;
    end;
  end;
end;

procedure TStartDlg.DiffBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to nPlOffered - 1 do
    if (Sender = PlayerSlots[I].DiffUpBtn) and (Difficulty[I] < 3) or
      (Sender = PlayerSlots[I].DiffDownBtn) and (Difficulty[I] > 1) then
    begin
      if Sender = PlayerSlots[I].DiffUpBtn then
        Inc(Difficulty[I])
      else
        Dec(Difficulty[I]);
      SmartInvalidate(xBrain[I] - 18, yBrain[I] + 19, xBrain[I] - 18 + 12,
        yBrain[I] + (19 + 14));
    end;
end;

procedure TStartDlg.MultiBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 6 to 8 do
    if Sender = PlayerSlots[I].MultiBtn then
    begin
      MultiControl := MultiControl xor (1 shl I);
      TButtonC(Sender).ButtonIndex := 2 + (MultiControl shr I) and 1;
    end;
end;

procedure TStartDlg.FormHide(Sender: TObject);
begin
  Diff0 := Difficulty[0];
  ListIndex[Tab] := List.ItemIndex;
  ShowTab := Tab;
  Background.Enabled := True;
end;

procedure TStartDlg.QuitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TStartDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if KeyToShortCut(Key, Shift) = BHelp.ShortCut then
    DirectHelp(cStartHelp);
end;

procedure TStartDlg.CustomizeBtnClick(Sender: TObject);
begin
  AutoDiff := -AutoDiff;
  CustomizeBtn.ButtonIndex := CustomizeBtn.ButtonIndex xor 1;
  ChangePage(Page);
end;

procedure TStartDlg.AutoDiffUpBtnClick(Sender: TObject);
begin
  if AutoDiff < 5 then
  begin
    Inc(AutoDiff);
    SmartInvalidate(120, y0Mini + 61, 272, y0Mini + 61 + 21);
    SmartInvalidate(xDefault - 2, yDefault - 2, xDefault + 64 + 2,
      yDefault + 64 + 2);
  end;
end;

procedure TStartDlg.AutoDiffDownBtnClick(Sender: TObject);
begin
  if AutoDiff > 1 then
  begin
    Dec(AutoDiff);
    SmartInvalidate(120, y0Mini + 61, 272, y0Mini + 61 + 21);
    SmartInvalidate(xDefault - 2, yDefault - 2, xDefault + 64 + 2,
      yDefault + 64 + 2);
  end;
end;

procedure TStartDlg.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
begin
  Tracking := False;
end;

procedure TStartDlg.FormMouseMove(Sender: TObject; Shift: TShiftState;
  x, y: integer);
var
  OldLoadTurn: Integer;
  NewSelectedAction: TMainAction;
begin
  if Tracking then
  begin
    x := x - xTurnSlider;
    if x < 0 then
      x := 0
    else if x > wTurnSlider then
      x := wTurnSlider;
    OldLoadTurn := LoadTurn;
    LoadTurn := LastTurn * x div wTurnSlider;
    if LoadTurn < OldLoadTurn then
    begin
      SmartInvalidate(xTurnSlider + LoadTurn * wTurnSlider div LastTurn,
        yTurnSlider, xTurnSlider + OldLoadTurn * wTurnSlider div LastTurn + 1,
        yTurnSlider + 7);
      SmartInvalidate(344, y0Mini + 61, 514, y0Mini + 82);
    end
    else if LoadTurn > OldLoadTurn then
    begin
      SmartInvalidate(xTurnSlider + OldLoadTurn * wTurnSlider div LastTurn,
        yTurnSlider, xTurnSlider + LoadTurn * wTurnSlider div LastTurn + 1,
        yTurnSlider + 7);
      SmartInvalidate(344, y0Mini + 61, 514, y0Mini + 82);
    end;
  end
  else if Page = pgMain then
  begin
    if (x >= ActionSideBorder) and (x < ClientWidth - ActionSideBorder) and
      (y >= yAction - 8) and (y < ClientHeight - ActionBottomBorder) then
    begin
      NewSelectedAction := TMainAction((y - (yAction - 8)) div ActionPitch);
      if not (NewSelectedAction in ActionsOffered) then
        NewSelectedAction := maNone;
    end
    else
      NewSelectedAction := maNone;
    if NewSelectedAction <> SelectedAction then
    begin
      if SelectedAction <> maNone then
        SmartInvalidate(ActionSideBorder, yAction + Integer(SelectedAction) * ActionPitch
          - 8, ClientWidth - ActionSideBorder, yAction + (Integer(SelectedAction) + 1) *
          ActionPitch - 8);
      SelectedAction := NewSelectedAction;
      if SelectedAction <> maNone then
        SmartInvalidate(ActionSideBorder, yAction + Integer(SelectedAction) * ActionPitch
          - 8, ClientWidth - ActionSideBorder, yAction + (Integer(SelectedAction) + 1) *
          ActionPitch - 8);
    end;
  end;
end;

procedure TStartDlg.AutoEnemyUpBtnClick(Sender: TObject);
begin
  if AutoEnemies < nPl - 1 then
  begin
    Inc(AutoEnemies);
    SmartInvalidate(160, yMain + 140, 198, yMain + 140 + 21);
  end;
end;

procedure TStartDlg.AutoEnemyDownBtnClick(Sender: TObject);
begin
  if AutoEnemies > 0 then
  begin
    Dec(AutoEnemies);
    SmartInvalidate(160, yMain + 140, 198, yMain + 140 + 21);
  end;
end;

procedure TStartDlg.ReplayBtnClick(Sender: TObject);
begin
  LoadGame(GetSavedDir + DirectorySeparator, List.Items[List.ItemIndex] + CevoExt,
    LastTurn, True);
  SlotAvailable := -1;
end;


end.
