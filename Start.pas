{$INCLUDE Switches.inc}
unit Start;

interface

uses
  GameServer, Messg, ButtonBase, ButtonA, ButtonC, ButtonB, Area, Types,
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Menus, Registry, DrawDlg, Generics.Collections, Protocol, MiniMap, Brain,
  Translator;

type
  { TPlayerSlot }

  TPlayerSlot = class
    DiffUpBtn: TButtonC;
    DiffDownBtn: TButtonC;
    MultiBtn: TButtonC;
    OfferMultiple: Boolean;
    Rect: TRect;
  end;

  TPlayerSlots = class(TObjectList<TPlayerSlot>)
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
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListKeyPress(Sender: TObject; var Key: char);
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
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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
    LastGame: string;
    procedure DrawAction(Y, IconIndex: Integer; HeaderItem, TextItem: string);
    procedure InitPopup(PlayerIndex: Integer);
    procedure OfferBrain(Brain: TBrain; FixedLines: Integer);
    procedure PaintInfo;
    procedure ChangePage(NewPage: TStartPage);
    procedure ChangeTab(NewTab: TStartTab);
    procedure UnlistBackupFile(FileName: string);
    procedure SmartInvalidate(x0, y0, x1, y1: Integer;
      invalidateTab0: Boolean = False); overload;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure LoadLanguages;
    procedure LoadAiBrainsPictures;
    procedure UpdateInterface;
    procedure ShowSettings;
  public
    EmptyPicture: TBitmap;
    Translator: TTranslator;
    procedure UpdateFormerGames;
    procedure UpdateMaps;
  end;

var
  StartDlg: TStartDlg;


implementation

uses
  Global, Directories, Direct, ScreenTools, Inp, Back, Settings, KeyBindings,
  Languages;

{$R *.lfm}

const
  // predefined world size
  // attention: lx*ly+1 must be prime!
  { MaxWorldSize=8;
    lxpre: array[0..nWorldSize-1] of Integer =(30,40,50,60,70,90,110,130);
    lypre: array[0..nWorldSize-1] of Integer =(46,52,60,70,84,94,110,130);
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
  xBrain: array [0 .. nPlOffered - 1] of Integer = (x0Brain, x0Brain,
    x0Brain + dxBrain, x0Brain + dxBrain, x0Brain + dxBrain, x0Brain,
    x0Brain - dxBrain, x0Brain - dxBrain, x0Brain - dxBrain);
  yBrain: array [0 .. nPlOffered - 1] of Integer = (y0Brain, y0Brain - dyBrain,
    y0Brain - dyBrain, y0Brain, y0Brain + dyBrain, y0Brain + dyBrain,
    y0Brain + dyBrain, y0Brain, y0Brain - dyBrain);
  TabOffset = -115;
  TabSize = 159;
  TabHeight = 40;

  InitAlive: array [1 .. nPl] of Integer = (1, 1 + 2, 1 + 2 + 32,
    1 + 2 + 8 + 128, 1 + 2 + 8 + 32 + 128, 1 + 2 + 8 + 16 + 64 + 128,
    1 + 2 + 4 + 16 + 32 + 64 + 256, 511 - 32, 511, 511 - 32, 511, 511 - 32, 511,
    511 - 32, 511);
  InitMulti: array [nPlOffered + 1 .. nPl] of Integer = (256, 256, 256 + 128,
    256 + 128, 256 + 128 + 64, 256 + 128 + 64);

  PlayerAutoDiff: array [1 .. 5] of Integer = (1, 1, 2, 2, 3);
  EnemyAutoDiff: array [1 .. 5] of Integer = (4, 3, 2, 1, 1);

{ TStartDlg }

procedure TStartDlg.FormCreate(Sender: TObject);
var
  X, I: Integer;
  PlayerSlot: TPlayerSlot;
  AIBrains: TBrains;
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
  LoadLanguages;

  ActionsOffered := [maConfig, maManual, maCredits, maWeb];
  if FileExists(HomeDir + AITemplateFileName) then
    Include(ActionsOffered, maAIDev);

  BrainDefault := nil;
  for I := Brains.IndexOf(BrainRandom) to Brains.Count - 1 do
    if AnsiCompareFileName(DefaultAI, Brains[I].FileName) = 0 then
      BrainDefault := Brains[I];
  if (BrainDefault = BrainRandom) and (Brains.GetKindCount(btAI) < 2) then
    BrainDefault := nil;
  if (not Assigned(BrainDefault)) and (Brains.GetKindCount(btAI) > 0) then
    begin
      AIBrains := TBrains.Create(False);
      Brains.GetByKind(btAI, AIBrains);
      BrainDefault := Brains[0];
      FreeAndNil(AIBrains);
    end; // default AI not found, use any

  DirectDlg.Left := (Screen.Width - DirectDlg.Width) div 2;
  DirectDlg.Top := (Screen.Height - DirectDlg.Height) div 2;

  UpdateInterface;

  Canvas.Font.Assign(UniFont[ftNormal]);
  Canvas.Brush.Style := bsClear;

  QuitBtn.Hint := Phrases.Lookup('STARTCONTROLS', 0);
  ReplayBtn.Hint := Phrases.Lookup('BTN_REPLAY');
  PlayerSlots.Count := nPlOffered;
  for I := 0 to PlayerSlots.Count - 1 do
  with PlayerSlots[I] do begin
    DiffUpBtn := TButtonC.Create(self);
    DiffUpBtn.Graphic := HGrSystem.Data;
    DiffUpBtn.left := xBrain[I] - 18;
    DiffUpBtn.top := yBrain[I] + 39;
    DiffUpBtn.ButtonIndex := 1;
    DiffUpBtn.Parent := self;
    DiffUpBtn.OnClick := DiffBtnClick;
    DiffDownBtn := TButtonC.Create(self);
    DiffDownBtn.Graphic := HGrSystem.Data;
    DiffDownBtn.left := xBrain[I] - 18;
    DiffDownBtn.top := yBrain[I] + 51;
    DiffDownBtn.ButtonIndex := 0;
    DiffDownBtn.Parent := self;
    DiffDownBtn.OnClick := DiffBtnClick;
  end;
  for I := 6 to 8 do
  with PlayerSlots[I] do begin
    MultiBtn := TButtonC.Create(self);
    MultiBtn.Graphic := HGrSystem.Data;
    MultiBtn.left := xBrain[I] - 18;
    MultiBtn.top := yBrain[I];
    MultiBtn.Parent := self;
    MultiBtn.OnClick := MultiBtnClick;
    OfferMultiple := True;
  end;

  X := BiColorTextWidth(Canvas, Phrases.Lookup('STARTCONTROLS', 7)) div 2;
  CustomizeBtn.left := x0Brain + 32 - 16 - X;
  if AutoDiff < 0 then
    CustomizeBtn.ButtonIndex := 3
  else
    CustomizeBtn.ButtonIndex := 2;

  BitBltBitmap(BrainNoTerm.Picture, 0, 0, 64, 64, HGrSystem2.Data, GBrainNoTerm.Left, GBrainNoTerm.Top);
  BitBltBitmap(BrainSuperVirtual.Picture, 0, 0, 64, 64, HGrSystem2.Data, GBrainSuperVirtual.Left, GBrainSuperVirtual.Top);
  BitBltBitmap(BrainTerm.Picture, 0, 0, 64, 64, HGrSystem2.Data, GBrainTerm.Left, GBrainTerm.Top);
  BitBltBitmap(BrainRandom.Picture, 0, 0, 64, 64, HGrSystem2.Data, GBrainRandom.Left, GBrainRandom.Top);

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
  MapFileName := '';
  Maps := TStringList.Create;
  UpdateMaps;
end;

procedure TStartDlg.FormDestroy(Sender: TObject);
begin
  SaveConfig;
  FreeAndNil(Translator);
  FreeAndNil(FormerGames);
  FreeAndNil(Maps);
  FreeAndNil(EmptyPicture);
  FreeAndNil(LogoBuffer);
  FreeAndNil(PlayerSlots);
  FreeAndNil(MiniMap);
end;

procedure TStartDlg.SmartInvalidate(x0, y0, x1, y1: Integer;
  invalidateTab0: Boolean);
var
  I: Integer;
  r0, r1: HRgn;
begin
  r0 := CreateRectRgn(x0, y0, x1, y1);
  for I := 0 to ControlCount - 1 do
    if not (Controls[I] is TArea) and Controls[I].Visible then
    begin
      with Controls[I].BoundsRect do
        r1 := CreateRectRgn(left, top, Right, Bottom);
      CombineRgn(r0, r0, r1, RGN_DIFF);
      DeleteObject(r1);
    end;
  if not invalidateTab0 then begin
    r1 := CreateRectRgn(0, 0, 6 + 36, 3 + 38); // tab 0 icon
    CombineRgn(r0, r0, r1, RGN_DIFF);
    DeleteObject(r1);
  end;
  InvalidateRgn(Handle, r0, False);
  DeleteObject(r0);
end;

procedure TStartDlg.LoadConfig;
var
  Reg: TRegistry;
  I: Integer;
  S: string;
  {$IFDEF WINDOWS}
  ResolutionX, ResolutionY, ResolutionBPP, ResolutionFreq: Integer;
  {$ENDIF}
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
    if ValueExists('StartTab') then ShowTab := TStartTab(Reg.ReadInteger('StartTab'))
       else ShowTab := tbNew;
    if ValueExists('LastGame') then LastGame := Reg.ReadString('LastGame')
       else LastGame := '';
    if ValueExists('NetworkEnabled') then NetworkEnabled := Reg.ReadBool('NetworkEnabled')
       else NetworkEnabled := False;

    if ValueExists('ScreenMode') then
      ScreenMode := ReadInteger('ScreenMode')
      else ScreenMode := 1;
    FullScreen := ScreenMode > 0;
    if ValueExists('MultiControl') then
      MultiControl := ReadInteger('MultiControl')
      else MultiControl := 0;
    {$IFDEF WINDOWS}
    if ValueExists('ResolutionX') then
      ResolutionX := ReadInteger('ResolutionX');
    if ValueExists('ResolutionY') then
      ResolutionY := ReadInteger('ResolutionY');
    if ValueExists('ResolutionBPP') then
      ResolutionBPP := ReadInteger('ResolutionBPP');
    if ValueExists('ResolutionFreq') then
      ResolutionFreq := ReadInteger('ResolutionFreq');
    if ScreenMode = 2 then
      ChangeResolution(ResolutionX, ResolutionY, ResolutionBPP,
        ResolutionFreq);
    {$ENDIF}
  finally
    Free;
  end;

  KeyBindings.KeyBindings.LoadFromRegistry(HKEY_CURRENT_USER, AppRegistryKey + '\KeyBindings');
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
    WriteInteger('StartTab', Integer(ShowTab));
    WriteString('LastGame', LastGame);
    WriteBool('NetworkEnabled', NetworkEnabled);
  finally
    Free;
  end;

  KeyBindings.KeyBindings.SaveToRegistry(HKEY_CURRENT_USER, AppRegistryKey + '\KeyBindings');
end;

procedure TStartDlg.LoadLanguages;
var
  I: Integer;
begin
  Translator := TTranslator.Create(nil);
  with Translator, Languages do begin
    AddNew('zh-Hant', 'Traditional Chinese');
    AddNew('zh-Hans', 'Simplified Chinese');
    SearchByCode('').Available := True;

    for I := 1 to Languages.Count - 1 do
    with Languages[I] do begin
      Available := DirectoryExists(HomeDir + 'Localization' + DirectorySeparator + Code) or (Code = 'en');
    end;
  end;
end;

procedure TStartDlg.LoadAiBrainsPictures;
var
  AiBrains: TBrains;
begin
  AiBrains := TBrains.Create(False);
  try
    Brains.GetByKind(btAI, AiBrains);
    AiBrains.LoadPictures;
  finally
    FreeAndNil(AiBrains);
  end;
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

procedure TStartDlg.ShowSettings;
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

procedure TStartDlg.DrawAction(Y, IconIndex: Integer; HeaderItem, TextItem: string);
begin
  Canvas.Font.Assign(UniFont[ftCaption]);
  Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
  RisedTextOut(Canvas, xAction, Y - 3, Phrases2.Lookup(HeaderItem));
  Canvas.Font.Assign(UniFont[ftNormal]);
  BiColorTextOut(Canvas, Colors.Canvas.Pixels[clkAge0 - 1, cliDimmedText],
    $000000, xAction, Y + 21, Phrases2.Lookup(TextItem));

  UnshareBitmap(LogoBuffer);
  BitBltCanvas(LogoBuffer.Canvas, 0, 0, 50, 50, Canvas,
    xActionIcon - 2, Y - 2);
  GlowFrame(LogoBuffer, 8, 8, 34, 34, $202020);
  BitBltCanvas(Canvas, xActionIcon - 2, Y - 2, 50, 50,
    LogoBuffer.Canvas, 0, 0);
  BitBltCanvas(Canvas, xActionIcon, Y, 40, 40, BigImp.Canvas,
    (IconIndex mod 7) * xSizeBig + 8, (IconIndex div 7) * ySizeBig);
  RFrame(Canvas, xActionIcon - 1, Y - 1, xActionIcon + 40, Y + 40,
    $000000, $000000);
end;

procedure TStartDlg.FormPaint(Sender: TObject);
const
  TabNames: array[TStartTab] of Integer = (0, 11, 3, 4);
var
  I, W, H, xMini, yMini, Y: Integer;
  S: string;
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
      MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
    if AutoDiff > 0 then
    begin
      Frame(Canvas, -1 { x0Brain-dxBrain } ,
        yMain + 112 - 15 { Up1Btn.Top-12 }{ y0Brain-dyBrain } ,
        x0Brain + dxBrain + 64, Up2Btn.top + 38 { y0Brain+dyBrain+64 } ,
        MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
    end;
  end
  else if Page <> pgMain then
    Frame(Canvas, 328, Up1Btn.top - 15, ClientWidth, Up2Btn.top + 38,
      MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
  Frame(Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);

  // draw tabs
  Frame(Canvas, 2, 2 + 2 * Integer(Tab <> tbMain), TabOffset + (0 + 1) * TabSize - 1,
    TabHeight, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Canvas, 1, 1 + 2 * Integer(Tab <> tbMain), TabOffset + (0 + 1) * TabSize,
    TabHeight, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Canvas.Pixels[1, 1 + 2 * Integer(Tab <> tbMain)] := MainTexture.ColorBevelShade;
  for Tab2 := tbMap to tbPrevious do
  begin
    Frame(Canvas, TabOffset + Integer(Tab2) * TabSize + 2, 2 + 2 * Integer(Tab <> Tab2),
      TabOffset + (Integer(Tab2) + 1) * TabSize - 1, TabHeight, MainTexture.ColorBevelLight,
      MainTexture.ColorBevelShade);
    Frame(Canvas, TabOffset + Integer(Tab2) * TabSize + 1, 1 + 2 * Integer(Tab <> Tab2),
      TabOffset + (Integer(Tab2) + 1) * TabSize, TabHeight, MainTexture.ColorBevelLight,
      MainTexture.ColorBevelShade);
    Canvas.Pixels[TabOffset + Integer(Tab2) * TabSize + 1, 1 + 2 * Integer(Tab <> Tab2)] :=
      MainTexture.ColorBevelShade;
  end;
  Canvas.Font.Assign(UniFont[ftNormal]);
  for Tab2 := tbMap to tbPrevious do
  begin
    S := Phrases.Lookup('STARTCONTROLS', TabNames[Tab2]);
    RisedTextOut(Canvas, TabOffset + Integer(Tab2) * TabSize + 1 +
      (TabSize - BiColorTextWidth(Canvas, S)) div 2,
      10 + 2 * Integer(Tab <> Tab2), S);
  end;
  Frame(Canvas, TabOffset + 4 * TabSize + 1, -1, ClientWidth, TabHeight,
    $000000, $000000);
  Frame(Canvas, 1, TabHeight + 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Canvas, 2, TabHeight + 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  if Tab = tbMain then
  begin
    PaintBackground(self, 3, TabHeight - 1, TabSize - 4 - 3 + TabOffset + 3, 4);
    Canvas.Pixels[2, TabHeight] := MainTexture.ColorBevelLight;
  end
  else
  begin
    PaintBackground(self, TabOffset + 3 + Integer(Tab) * TabSize, TabHeight - 1,
      TabSize - 4, 4);
    Canvas.Pixels[TabOffset + Integer(Tab) * TabSize + 2, TabHeight] :=
      MainTexture.ColorBevelLight;
  end;
  Canvas.Pixels[TabOffset + (Integer(Tab) + 1) * TabSize - 1, TabHeight + 1] :=
    MainTexture.ColorBevelShade;
  if Tab < tbPrevious then
    Frame(Canvas, TabOffset + (Integer(Tab) + 1) * TabSize + 1, 3,
      TabOffset + (Integer(Tab) + 1) * TabSize + 2, TabHeight, MainTexture.ColorBevelShade,
      MainTexture.ColorBevelShade); // Tab shadow

  // Paint menu logo
  UnshareBitmap(LogoBuffer);
  BitBltCanvas(LogoBuffer.Canvas, 0, 0, MenuLogo.Width, MenuLogo.Height, Canvas, 6,
    3 + 2 * Integer(Tab <> tbMain));

  ImageOp_BCC(LogoBuffer, Templates.Data, 0, 0, MenuLogo.Left, MenuLogo.Top,
    MenuLogo.Width, MenuLogo.Height - 9, $BFBF20, $4040DF); // logo part 1
  ImageOp_BCC(LogoBuffer, Templates.Data, 10, 27, MenuLogo.Left + 10,
    MenuLogo.Top + 27, MenuLogo.Width - 10, 9, $BFBF20, $4040DF); // logo part 2
  BitBltCanvas(Canvas, 6, 3 + 2 * Integer(Tab <> tbMain), MenuLogo.Width, MenuLogo.Height,
    LogoBuffer.Canvas, 0, 0);

  if Page = pgMain then begin
    if SelectedAction <> maNone then // mark selected action
      for I := 0 to (ClientWidth - 2 * ActionSideBorder) div wBuffer + 1 do
      begin
        W := ClientWidth - 2 * ActionSideBorder - I * wBuffer;
        if W > wBuffer then
          W := wBuffer;
        H := ActionPitch;
        if yAction + Integer(SelectedAction) * ActionPitch - 8 + H > ClientHeight - ActionBottomBorder
        then
          H := ClientHeight - ActionBottomBorder -
            (yAction + Integer(SelectedAction) * ActionPitch - 8);

        UnshareBitmap(LogoBuffer);
        BitBltCanvas(LogoBuffer.Canvas, 0, 0, W, H, Canvas,
          ActionSideBorder + I * wBuffer, yAction + Integer(SelectedAction) * ActionPitch
          - 8);
        MakeBlue(LogoBuffer, 0, 0, W, H);
        BitBltCanvas(Canvas, ActionSideBorder + I * wBuffer,
          yAction + Integer(SelectedAction) * ActionPitch - 8, W, H,
          LogoBuffer.Canvas, 0, 0);
      end;
    Y := yAction;
    for MainAction := Low(TMainActionSet) to High(TMainActionSet) do
    begin
      if MainAction in ActionsOffered then
        case MainAction of
          maConfig: DrawAction(Y, 25, 'ACTIONHEADER_CONFIG', 'ACTION_CONFIG');
          maManual: DrawAction(Y, 19, 'ACTIONHEADER_MANUAL', 'ACTION_MANUAL');
          maCredits: DrawAction(Y, 22, 'ACTIONHEADER_CREDITS', 'ACTION_CREDITS');
          maAIDev: DrawAction(Y, 24, 'ACTIONHEADER_AIDEV', 'ACTION_AIDEV');
          maWeb:
            begin
              Canvas.Font.Assign(UniFont[ftCaption]);
              // Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline];
              RisedTextOut(Canvas, xActionIcon + 99, Y,
                Format(Phrases2.Lookup('ACTIONHEADER_WEB'), [CevoHomepageShort]));
              Canvas.Font.Assign(UniFont[ftNormal]);

              UnshareBitmap(LogoBuffer);
              BitBltCanvas(LogoBuffer.Canvas, 0, 0, LinkArrows.Width, LinkArrows.Height, Canvas,
                xActionIcon, Y + 2);
              ImageOp_BCC(LogoBuffer, Templates.Data, Point(0, 0), LinkArrows.BoundsRect, 0,
                Colors.Canvas.Pixels[clkAge0 - 1, cliDimmedText]);
              BitBltCanvas(Canvas, xActionIcon, Y + 2, LinkArrows.Width, LinkArrows.Height,
                LogoBuffer.Canvas, 0, 0);
            end;
        end;
      Inc(Y, ActionPitch);
    end;
  end
  else if Page in [pgStartRandom, pgStartMap] then
  begin
    UnderlinedTitleValue(Canvas, Phrases.Lookup('STARTCONTROLS', 10),
      TurnToString(MaxTurn), 344, y0Mini + 61, 170);

    S := Phrases.Lookup('STARTCONTROLS', 7);
    W := Canvas.TextWidth(S);
    LoweredTextOut(Canvas, -2, MainTexture, x0Brain + 32 - W div 2,
      y0Brain + dyBrain + 69, S);

    InitOrnament;
    if AutoDiff < 0 then
    begin
      for I := 12 to 19 do
        if (I < 13) or (I > 17) then begin
          BitBltCanvas(Canvas, 9 + I * 27, yLogo - 2, Ornament.Width, Ornament.Height,
            HGrSystem2.Mask.Canvas, Ornament.Left, Ornament.Top, SRCAND);
          BitBltCanvas(Canvas, 9 + I * 27, yLogo - 2, Ornament.Width, Ornament.Height,
            HGrSystem2.Data.Canvas, Ornament.Left, Ornament.Top, SRCPAINT);
        end;
      PaintLogo(Canvas, 69 + 11 * 27, yLogo, MainTexture.ColorBevelLight,
        MainTexture.ColorBevelShade);

      for I := 0 to nPlOffered - 1 do
        if 1 shl I and SlotAvailable <> 0 then
        begin
          if Assigned(PlayersBrain[I]) then
            FrameImage(Canvas, PlayersBrain[I].Picture, xBrain[I], yBrain[I],
              64, 64, 0, 0, True)
          else
            FrameImage(Canvas, EmptyPicture, xBrain[I], yBrain[I], 64, 64,
              0, 0, True);
          if Assigned(PlayersBrain[I]) and (PlayersBrain[I].Kind in [btTerm, btRandom, btAI]) then
          begin
            BitBltCanvas(Canvas, xBrain[I] - 18, yBrain[I] + 19, 12, 14,
              HGrSystem.Data.Canvas, 134 + (Difficulty[I] - 1) *
              13, 28);
            Frame(Canvas, xBrain[I] - 19, yBrain[I] + 18, xBrain[I] - 18 + 12,
              yBrain[I] + (19 + 14), $000000, $000000);
            RFrame(Canvas, PlayerSlots[I].DiffUpBtn.left - 1, PlayerSlots[I].DiffUpBtn.top - 1,
              PlayerSlots[I].DiffUpBtn.left + 12, PlayerSlots[I].DiffUpBtn.top + 24,
              MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
            with Canvas do
            begin
              Brush.Color := $000000;
              FillRect(Rect(xBrain[I] - 5, yBrain[I] + 25, xBrain[I] - 2,
                yBrain[I] + 27));
              Brush.Style := bsClear;
            end;
            if PlayerSlots[I].OfferMultiple then
            begin
              RFrame(Canvas, PlayerSlots[I].MultiBtn.left - 1, PlayerSlots[I].MultiBtn.top - 1,
                PlayerSlots[I].MultiBtn.left + 12, PlayerSlots[I].MultiBtn.top + 12,
                MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
              BitBltCanvas(Canvas, xBrain[I] - 31, yBrain[I], 13, 12,
                HGrSystem.Data.Canvas, 88, 47);
            end;
          end;
          if Assigned(PlayersBrain[I]) then
          begin
            PlayerSlots[I].DiffUpBtn.Hint := Format(Phrases.Lookup('STARTCONTROLS', 9),
              [PlayersBrain[I].Name]);
            PlayerSlots[I].DiffDownBtn.Hint := PlayerSlots[I].DiffUpBtn.Hint;
          end;
        end;
    end
    else
    begin
      DLine(Canvas, 24, 198, yMain + 140 + 19, MainTexture.ColorBevelLight,
        MainTexture.ColorBevelShade);
      RisedTextOut(Canvas, 24 { x0Brain+32-BiColorTextWidth(Canvas,s) div 2 } ,
        yMain + 140 { y0Mini-77 } , Phrases.Lookup('STARTCONTROLS', 15));
      if Page = pgStartRandom then
        S := IntToStr(AutoEnemies)
      else if nMapStartPositions = 0 then
        S := '0'
      else
        S := IntToStr(nMapStartPositions - 1);
      RisedTextOut(Canvas, 198 - BiColorTextWidth(Canvas, S), yMain + 140, S);

      DLine(Canvas, 24, xDefault - 6, yMain + 164 + 19,
        MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
      RisedTextOut(Canvas, 24 { x0Brain+32-BiColorTextWidth(Canvas,s) div 2 } ,
        yMain + 164 { y0Mini-77 } , Phrases.Lookup('STARTCONTROLS', 16));
      if AutoDiff = 1 then
        FrameImage(Canvas, Brains.GetBeginner.Picture, xDefault, yDefault, 64,
          64, 0, 0, False)
      else
        FrameImage(Canvas, BrainDefault.Picture, xDefault, yDefault, 64, 64,
          0, 0, True);
      DLine(Canvas, 56, 272, y0Mini + 61 + 19, MainTexture.ColorBevelLight,
        MainTexture.ColorBevelShade);

      RisedTextOut(Canvas, 56, y0Mini + 61,
        Phrases.Lookup('STARTCONTROLS', 14));
      S := Phrases.Lookup('AUTODIFF', AutoDiff - 1);
      RisedTextOut(Canvas, 272 - BiColorTextWidth(Canvas, S), y0Mini + 61, S);

      for I := 0 to 19 do
        if (I < 2) or (I > 6) then begin
          BitBltCanvas(Canvas, 9 + I * 27, yLogo - 2, Ornament.Width, Ornament.Height,
            HGrSystem2.Mask.Canvas, Ornament.Left, Ornament.Top, SRCAND);
          BitBltCanvas(Canvas, 9 + I * 27, yLogo - 2, Ornament.Width, Ornament.Height,
            HGrSystem2.Data.Canvas, Ornament.Left, Ornament.Top, SRCPAINT);
        end;
      PaintLogo(Canvas, 69, yLogo, MainTexture.ColorBevelLight,
        MainTexture.ColorBevelShade);
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
      DLine(Canvas, 344, 514, y0Mini + 61 + 19, MainTexture.ColorBevelLight,
        MainTexture.ColorBevelShade);
    RisedTextOut(Canvas, 344, y0Mini + 61, Phrases.Lookup('STARTCONTROLS', 8));
    S := TurnToString(LoadTurn);
    RisedTextOut(Canvas, 514 - BiColorTextWidth(Canvas, S), y0Mini + 61, S);
  end
  else if Page = pgEditRandom then
  begin
    UnderlinedTitleValue(Canvas, Phrases.Lookup('STARTCONTROLS', 5),
      IntToStr((WorldSizes[WorldSize].X * WorldSizes[WorldSize].Y * 20 +
      DefaultWorldTiles div 2) div DefaultWorldTiles * 5) + '%',
      344, y0Mini - 77, 170);
    UnderlinedTitleValue(Canvas, Phrases.Lookup('STARTCONTROLS', 6),
      IntToStr(StartLandMass) + '%', 344, y0Mini + 61, 170);
  end
  else if Page = pgEditMap then
  begin
    // DLine(Canvas,344,514,y0Mini+61+19,MainTexture.ColorBevelLight,MainTexture.ColorBevelShade);
    S := Format(Phrases2.Lookup('MAPPROP'),
      [(nMapLandTiles * 100 + 556) div 1112,
      // 1112 is typical for world with 100% size and default land mass
      nMapStartPositions]);
    RisedTextOut(Canvas, x0Mini - BiColorTextWidth(Canvas, S) div 2,
      y0Mini + 61, S);
  end;

  if StartBtn.Visible then
    BtnFrame(Canvas, StartBtn.BoundsRect, MainTexture);
  if Up2Btn.Visible then
    RFrame(Canvas, Up2Btn.left - 1, Up2Btn.top - 1, Up2Btn.left + 12,
      Up2Btn.top + 24, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
  if Up1Btn.Visible then
    RFrame(Canvas, Up1Btn.left - 1, Up1Btn.top - 1, Up1Btn.left + 12,
      Up1Btn.top + 24, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
  if AutoDiffUpBtn.Visible then
    RFrame(Canvas, AutoDiffUpBtn.left - 1, AutoDiffUpBtn.top - 1,
      AutoDiffUpBtn.left + 12, AutoDiffUpBtn.top + 24, MainTexture.ColorBevelShade,
      MainTexture.ColorBevelLight);
  if AutoEnemyUpBtn.Visible then
    RFrame(Canvas, AutoEnemyUpBtn.left - 1, AutoEnemyUpBtn.top - 1,
      AutoEnemyUpBtn.left + 12, AutoEnemyUpBtn.top + 24,
      MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
  if CustomizeBtn.Visible then
    RFrame(Canvas, CustomizeBtn.left - 1, CustomizeBtn.top - 1,
      CustomizeBtn.left + 12, CustomizeBtn.top + 12, MainTexture.ColorBevelShade,
      MainTexture.ColorBevelLight);
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
      yMini + 3 + MiniMap.Size.Y, MainTexture.ColorBevelLight,
      MainTexture.ColorBevelShade);
    Frame(Canvas, xMini + 1, yMini + 1, xMini + 2 + MiniMap.Size.X * 2,
      yMini + 2 + MiniMap.Size.Y, MainTexture.ColorBevelShade,
      MainTexture.ColorBevelLight);

    S := '';
    if MiniMap.Mode = mmPicture then
    begin
      BitBltCanvas(Canvas, xMini + 2, yMini + 2, MiniMap.Size.X * 2, MiniMap.Size.Y,
        MiniMap.Bitmap.Canvas, 0, 0);
      if Page = pgStartRandom then
        S := Phrases.Lookup('RANMAP')
    end
    else if MiniMap.Mode = mmMultiPlayer then
      S := Phrases.Lookup('MPMAP')
    else if Page = pgStartMap then
      S := Copy(MapFileName, 1, Length(MapFileName) - Length(CevoMapExt))
    else if Page = pgEditMap then
      S := List.Items[List.ItemIndex]
    else if Page = pgNoLoad then
      S := Phrases.Lookup('NOGAMES');
    if S <> '' then
      RisedTextOut(Canvas, x0Mini + 2 - BiColorTextWidth(Canvas, S) div 2,
        y0Mini - 8, S);
  end;
end;

procedure TStartDlg.FormShow(Sender: TObject);
begin
  {$IFDEF UNIX}
  ShowInTaskBar := stAlways;
  {$ENDIF}
  MainTexture.Age := -1;
  List.Font.Color := MainTexture.ColorMark;

  Fill(EmptyPicture.Canvas, Bounds(0, 0, 64, 64),
    Point((Maintexture.Width - 64) div 2, (Maintexture.Height - 64) div 2));

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
      ListIndex[tbNew] := 0;
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
        if LoadGame(GetSavedDir + DirectorySeparator, FileName + CevoExt, LoadTurn, False)
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
          OpenKey(AppRegistryKey, True);
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
          if BrainDefault.Kind <> btNetworkClient then begin
            if AutoDiff > 0 then begin
              WriteString('DefaultAI', BrainDefault.FileName);
              SlotAvailable := 0; // PlayerSlot will be invalid hereafter
              PlayersBrain[0] := BrainTerm;
              Difficulty[0] := PlayerAutoDiff[AutoDiff];
              for I := 1 to nPl - 1 do
                if (Page = pgStartRandom) and (I <= AutoEnemies) or
                  (Page = pgStartMap) and (I < nMapStartPositions) then begin
                  if AutoDiff = 1 then PlayersBrain[I] := Brains.GetBeginner
                    else PlayersBrain[I] := BrainDefault;
                  Difficulty[I] := EnemyAutoDiff[AutoDiff];
                end else PlayersBrain[I] := nil;
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
      EditMap(GetMapsDir + DirectorySeparator + MapFileName, lxmax, lymax, StartLandMass);
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
        EditMap(GetMapsDir + DirectorySeparator + MapFileName,
          WorldSizes[WorldSize].X, WorldSizes[WorldSize].Y, StartLandMass);
      end;
  end;
end;

procedure TStartDlg.ListKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then StartBtnClick(Sender);
end;

procedure TStartDlg.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  DelKey = 46;
begin
  if Key = DelKey then DeleteBtnClick(Sender)
    else FormKeyDown(Sender, Key, Shift);
end;

procedure TStartDlg.PaintInfo;
begin
  case Page of
    pgStartRandom: begin
      MiniMap.Mode := mmPicture;
      MiniMap.PaintRandom(3, StartLandMass, WorldSizes[WorldSize]);
    end;
    pgNoLoad: begin
      MiniMap.Mode := mmNone;
      MiniMap.Size := WorldSizes[DefaultWorldSize];
    end;
    pgLoad: begin
        MiniMap.LoadFromLogFile(GetSavedDir + DirectorySeparator +
          List.Items[List.ItemIndex] + CevoExt, LastTurn, WorldSizes[DefaultWorldSize]);
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
      MiniMap.PaintRandom(4, StartLandMass, WorldSizes[WorldSize]);
    end;
    pgStartMap, pgEditMap:
      begin
        if Page = pgEditMap then
          MapFileName := List.Items[List.ItemIndex] + CevoMapExt;
        MiniMap.LoadFromMapFile(GetMapsDir + DirectorySeparator + MapFileName,
          nMapLandTiles, nMapStartPositions);
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
          if Assigned(PlayersBrain[I]) and (PlayersBrain[I].Kind = btTerm) then begin
            PlayersBrain[I] := nil;
            PlayerSlots[I].DiffUpBtn.Visible := False;
            PlayerSlots[I].DiffUpBtn.Tag := 0;
            PlayerSlots[I].DiffDownBtn.Visible := False;
            PlayerSlots[I].DiffDownBtn.Tag := 0;
            if PlayerSlots[I].OfferMultiple then begin
              PlayerSlots[I].MultiBtn.Visible := False;
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
    (StrIComp(PChar(MenuItem.Caption), PChar(PopupMenu1.Items[J].Caption)) > 0) do
    Inc(J);
  MenuItem.RadioItem := True;
  if (PlayerPopupIndex < 0) then MenuItem.Checked := BrainDefault = Brain
    else MenuItem.Checked := PlayersBrain[PlayerPopupIndex] = Brain;
  PopupMenu1.Items.Insert(J, MenuItem);
end;

procedure TStartDlg.InitPopup(PlayerIndex: Integer);
var
  I: Integer;
  FixedLines: Integer;
  MenuItem: TMenuItem;
  AIBrains: TBrains;
begin
  FixedLines := 0;
  PlayerPopupIndex := PlayerIndex;
  EmptyMenu(PopupMenu1.Items);
  if PlayerPopupIndex < 0 then begin // select default AI
    if NetworkEnabled then begin
      OfferBrain(BrainNetworkClient, FixedLines);
      Inc(FixedLines);
    end;

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
      if AIBrains[I].Flags and fMultiple <> 0 then
        OfferBrain(AIBrains[I], FixedLines);
    FreeAndNil(AIBrains);
  end else begin
    if PlayerPopupIndex > 0 then begin
      OfferBrain(nil, FixedLines);
      Inc(FixedLines);
    end;
    for I := Brains.IndexOf(BrainTerm) downto 0 do // offer game interfaces
      if (PlayerPopupIndex = 0) or (Brains[I].Kind = btTerm) and
        (PlayersBrain[0].Kind <> btNoTerm) then begin
        OfferBrain(Brains[I], FixedLines);
        Inc(FixedLines);
      end;
    if PlayerPopupIndex > 0 then begin
      if NetworkEnabled then begin
        OfferBrain(BrainNetworkServer, FixedLines);
        Inc(FixedLines);
      end;

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
          OfferBrain(AIBrains[I], FixedLines);
      FreeAndNil(AIBrains);
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
      while (I > 0) and (F.Time < Integer(FormerGames.Objects[I - 1])) do
        Dec(I);
      FormerGames.InsertObject(I, Copy(F.Name, 1, Length(F.Name) - 5),
        TObject(F.Time));
    until FindNext(F) <> 0;
  FindClose(F);
  I := FormerGames.IndexOf(LastGame);
  if I >= 0 then ListIndex[tbPrevious] := I
    else ListIndex[tbPrevious] := FormerGames.Count - 1;
  TurnValid := False;
end;

procedure TStartDlg.UpdateMaps;
var
  F: TSearchRec;
begin
  Maps.Clear;
  if FindFirst(GetMapsDir + DirectorySeparator + '*' + CevoMapExt, $21, F) = 0 then
    repeat
      Maps.Add(Copy(F.Name, 1, Length(F.Name) - Length(CevoMapExt)));
    until FindNext(F) <> 0;
  FindClose(F);
  Maps.Sort;
  Maps.Insert(0, Phrases.Lookup('RANMAP'));
  ListIndex[tbMain] := Maps.IndexOf(Copy(MapFileName, 1, Length(MapFileName) - Length(CevoMapExt)));
  if ListIndex[tbMain] < 0 then
    ListIndex[tbMain] := 0;
end;

procedure TStartDlg.ChangePage(NewPage: TStartPage);
var
  I, J, p1: Integer;
  S: string;
  Reg: TRegistry;
  InvalidateTab0: Boolean;
begin
  InvalidateTab0 := (Page = pgMain) or (NewPage = pgMain);
  Page := NewPage;
  case Page of
    pgStartRandom, pgStartMap:
      begin
        StartBtn.Caption := Phrases.Lookup('STARTCONTROLS', 1);
        if Page = pgStartRandom then
          I := nPlOffered
        else
        begin
          I := nMapStartPositions;
          if I = 0 then
          begin
            PlayersBrain[0] := BrainSuperVirtual;
            Difficulty[0] := 0;
          end;
          if PlayersBrain[0].Kind in [btNoTerm, btSuperVirtual] then
            Inc(I);
          if I > nPl then
            I := nPl;
          if I <= nPlOffered then
            MultiControl := 0
          else
            MultiControl := InitMulti[I];
        end;
        if InitAlive[I] <> SlotAvailable then
          if Page = pgStartRandom then
          begin // restore AI assignment of last start
            Reg := TRegistry.Create;
            with Reg do
            try
              OpenKey(AppRegistryKey + '\AI', True);
              for p1 := 0 to nPlOffered - 1 do begin
                PlayersBrain[p1] := nil;
                S := ReadString('Control' + IntToStr(p1));
                Difficulty[p1] := ReadInteger('Diff' + IntToStr(p1));
                if S <> '' then
                  for J := 0 to Brains.Count - 1 do
                    if AnsiCompareFileName(S, Brains[J].FileName) = 0 then
                      PlayersBrain[p1] := Brains[J];
              end;
            finally
              Free;
            end;
          end
          else
            for p1 := 1 to nPl - 1 do
              if 1 shl p1 and InitAlive[I] <> 0 then
              begin
                PlayersBrain[p1] := BrainDefault;
                Difficulty[p1] := 2;
              end
              else
                PlayersBrain[p1] := nil;
        SlotAvailable := InitAlive[I];
        for I := 0 to nPlOffered - 1 do
          if (AutoDiff < 0) and Assigned(PlayersBrain[I]) and
            (PlayersBrain[I].Kind in [btTerm, btRandom, btAI]) then
          begin
            PlayerSlots[I].DiffUpBtn.Tag := 768;
            PlayerSlots[I].DiffDownBtn.Tag := 768;
          end
          else
          begin
            PlayerSlots[I].DiffUpBtn.Tag := 0;
            PlayerSlots[I].DiffDownBtn.Tag := 0;
          end;
        for I := 6 to 8 do
          if (AutoDiff < 0) and Assigned(PlayersBrain[I]) and
            (PlayersBrain[I].Kind in [btTerm, btRandom, btAI]) then
          begin
            PlayerSlots[I].MultiBtn.Tag := 768;
            PlayerSlots[I].MultiBtn.ButtonIndex := 2 + (MultiControl shr I) and 1;
            PlayerSlots[I].MultiBtn.Enabled := Page = pgStartRandom
          end
          else
            PlayerSlots[I].MultiBtn.Tag := 0;
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
  for I := 0 to ControlCount - 1 do
    Controls[I].Visible := Controls[I].Tag and (256 shl Integer(Page)) <> 0;
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
      if (ListIndex[Tab] < List.Count) and (ListIndex[Tab] >= 0) then begin
        List.ItemIndex := ListIndex[Tab];
      end else List.ItemIndex := 0;
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
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  if (Y < TabHeight + 1) and (X - TabOffset < TabSize * 4) and
    ((X - TabOffset) div TabSize <> Integer(Tab)) then
  begin
    // Play('BUTTON_DOWN');
    ListIndex[Tab] := List.ItemIndex;
    ChangeTab(TStartTab((X - TabOffset) div TabSize));
  end
  else if Page = pgMain then begin
    case SelectedAction of
      maConfig: ShowSettings;
      maManual: DirectHelp(cStartHelp);
      maCredits: DirectHelp(cStartCredits);
      maAIDev: OpenDocument(HomeDir + AITemplateFileName);
      maWeb: OpenURL(CevoHomepage);
    end;
  end
  else if (AutoDiff < 0) and ((Page = pgStartRandom) or (Page = pgStartMap) and
    (nMapStartPositions > 0)) then
  begin
    for I := 0 to nPlOffered - 1 do
      if (1 shl I and SlotAvailable <> 0) and (X >= xBrain[I]) and
        (Y >= yBrain[I]) and (X < xBrain[I] + 64) and (Y < yBrain[I] + 64) then
      begin
        InitPopup(I);
        if yBrain[I] > y0Brain then
          PopupMenu1.Popup(left + xBrain[I] + 4, top + yBrain[I] + 60)
        else
          PopupMenu1.Popup(left + xBrain[I] + 4, top + yBrain[I] + 4);
      end;
  end
  else if (AutoDiff > 1) and ((Page = pgStartRandom) or (Page = pgStartMap)) and
    (X >= xDefault) and (Y >= yDefault) and (X < xDefault + 64) and
    (Y < yDefault + 64) then
    if Brains.GetKindCount(btAI) < 2 then
      SimpleMessage(Phrases.Lookup('NOALTAI'))
    else
    begin
      InitPopup(-1);
      PopupMenu1.Popup(left + xDefault + 4, top + yDefault + 4);
    end
  else if (Page = pgLoad) and (LastTurn > 0) and (Y >= yTurnSlider) and
    (Y < yTurnSlider + 7) and (X >= xTurnSlider) and
    (X <= xTurnSlider + wTurnSlider) then
  begin
    LoadTurn := LastTurn * (X - xTurnSlider) div wTurnSlider;
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
        Inc(MaxTurn, 200);
        SmartInvalidate(344, y0Mini + 61, 514, y0Mini + 82);
      end;
    pgLoad:
      if LoadTurn < LastTurn then
      begin
        Inc(LoadTurn);
        SmartInvalidate(xTurnSlider - 2, y0Mini + 61, xTurnSlider + wTurnSlider
          + 2, yTurnSlider + 9);
      end;
    pgEditRandom:
      if StartLandMass < 96 then
      begin
        Inc(StartLandMass, 5);
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
        Dec(MaxTurn, 200);
        SmartInvalidate(344, y0Mini + 61, 514, y0Mini + 82);
      end;
    pgLoad:
      if LoadTurn > 0 then
      begin
        Dec(LoadTurn);
        SmartInvalidate(xTurnSlider - 2, y0Mini + 61, xTurnSlider + wTurnSlider
          + 2, yTurnSlider + 9);
      end;
    pgEditRandom:
      if StartLandMass > 10 then
      begin
        Dec(StartLandMass, 5);
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
  I: Integer;
  NewName: string;
  F: file;
  ok: Boolean;
  MapPictureFileName: string;
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
      for I := 1 to Length(NewName) do
        if NewName[I] in ['\', '/', ':', '*', '?', '"', '<', '>', '|'] then
        begin
          SimpleMessage(Format(Phrases.Lookup('NOFILENAME'), [NewName[I]]));
          Exit;
        end;
      if Page = pgLoad then
        AssignFile(F, GetSavedDir + DirectorySeparator + List.Items[List.ItemIndex] + CevoExt)
      else
        AssignFile(F, GetMapsDir + DirectorySeparator + List.Items[List.ItemIndex] +
          CevoMapExt);
      ok := True;
      try
        if Page = pgLoad then
          Rename(F, GetSavedDir + DirectorySeparator + NewName + CevoExt)
        else
          Rename(F, GetMapsDir + DirectorySeparator + NewName + CevoMapExt);
      except
        // Play('INVALID');
        ok := False;
      end;
      if Page <> pgLoad then begin
        // Rename map picture
        MapPictureFileName := GetMapsDir + DirectorySeparator +
          List.Items[List.ItemIndex] + CevoMapPictureExt;
        if FileExists(MapPictureFileName) then
        try
          AssignFile(F, GetMapsDir + DirectorySeparator + List.Items[List.ItemIndex]
            + CevoMapPictureExt);
          Rename(F, GetMapsDir + DirectorySeparator + NewName + CevoMapPictureExt);
        except
        end;
      end;
      if ok then begin
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
  iDel: Integer;
  F: file;
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
        AssignFile(F, GetSavedDir + DirectorySeparator + List.Items[List.ItemIndex] + CevoExt)
      else
        AssignFile(F, GetMapsDir + DirectorySeparator + List.Items[List.ItemIndex] +
          CevoMapExt);
      Erase(F);
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
            TurnValid := False;
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
  LastGame := FormerGames[ListIndex[tbPrevious]];
end;

procedure TStartDlg.QuitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TStartDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ShortCut: TShortCut;
begin
  ShortCut := KeyToShortCut(Key, Shift);
  if BFullScreen.Test(ShortCut) then begin
    FullScreen := not FullScreen;
    UpdateInterface;
    Background.UpdateInterface;
    SetFocus;
  end else
  if BHelp.Test(ShortCut) then
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
  Shift: TShiftState; X, Y: Integer);
begin
  Tracking := False;
end;

procedure TStartDlg.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  OldLoadTurn: Integer;
  NewSelectedAction: TMainAction;
begin
  if Tracking then
  begin
    X := X - xTurnSlider;
    if X < 0 then
      X := 0
    else if X > wTurnSlider then
      X := wTurnSlider;
    OldLoadTurn := LoadTurn;
    LoadTurn := LastTurn * X div wTurnSlider;
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
    if (X >= ActionSideBorder) and (X < ClientWidth - ActionSideBorder) and
      (Y >= yAction - 8) and (Y < ClientHeight - ActionBottomBorder) then
    begin
      NewSelectedAction := TMainAction((Y - (yAction - 8)) div ActionPitch);
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
