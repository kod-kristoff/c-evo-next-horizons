{$INCLUDE Switches.inc}
unit Start;

interface

uses
  GameServer, Messg, ButtonBase, ButtonA, ButtonC, ButtonB, Area, Math,
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Menus, Registry, DrawDlg, fgl;

const
  // main actions
  nMainActions = 5;
  maConfig = 0;
  maManual = 1;
  maCredits = 2;
  maAIDev = 3;
  maWeb = 4;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BrainClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure Up1BtnClick(Sender: TObject);
    procedure Down1BtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListClick(Sender: TObject);
    procedure RenameBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DiffBtnClick(Sender: TObject);
    procedure MultiBtnClick(Sender: TObject);
    procedure Up2BtnClick(Sender: TObject);
    procedure Down2BtnClick(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CustomizeBtnClick(Sender: TObject);
    procedure AutoDiffUpBtnClick(Sender: TObject);
    procedure AutoDiffDownBtnClick(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; x, y: integer);
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
    MiniWidth: Integer;
    MiniHeight: Integer;
    SelectedAction: Integer;
    Page: Integer;
    ShowTab: Integer;
    Tab: Integer;
    Diff0: Integer;
    BrainDefault: TBrain;
    nMapLandTiles: Integer;
    nMapStartPositions: Integer;
    LoadTurn: Integer;
    LastTurn: Integer;
    { last turn of selected former game }
    SlotAvailable: Integer;
    PlayerPopupIndex: Integer; { brain concerned by brain context menu }
    ListIndex: array [0 .. 3] of integer;
    MapFileName: string;
    FormerGames, Maps: TStringList;
    LogoBuffer, Mini: TBitmap; { game world sample preview }
    MiniColors: array [0 .. 11, 0 .. 1] of TColor;
    // BookDate: string;
    PlayerSlots: TPlayerSlots;
    MiniMode: (mmNone, mmPicture, mmMultiPlayer);
    ActionsOffered: set of 0 .. nMainActions - 1;
    TurnValid, Tracking: boolean;
    DefaultAI: string;
    procedure InitPopup(PlayerIndex: Integer);
    procedure PaintInfo;
    procedure ChangePage(NewPage: integer);
    procedure ChangeTab(NewTab: integer);
    procedure UnlistBackupFile(FileName: string);
    procedure SmartInvalidate(x0, y0, x1, y1: integer;
      invalidateTab0: boolean = false); overload;
    procedure LoadConfig;
  end;

var
  StartDlg: TStartDlg;

implementation

uses
  Directories, Protocol, Direct, ScreenTools, Inp, Back, Locale;

{$R *.lfm}

const
  // predefined world size
  // attention: lx*ly+1 must be prime!
  { nWorldSize=8;
    lxpre: array[0..nWorldSize-1] of integer =(30,40,50,60,70,90,110,130);
    lypre: array[0..nWorldSize-1] of integer =(46,52,60,70,84,94,110,130);
    DefaultWorldTiles=4200; }
  nWorldSize = 6;
  lxpre: array [0 .. nWorldSize - 1] of integer = (30, 40, 50, 60, 75, 100);
  lypre: array [0 .. nWorldSize - 1] of integer = (46, 52, 60, 70, 82, 96);
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

  MaxWidthMapLogo = 96;
  MaxHeightMapLogo = 96;

  InitAlive: array [1 .. nPl] of integer = (1, 1 + 2, 1 + 2 + 32,
    1 + 2 + 8 + 128, 1 + 2 + 8 + 32 + 128, 1 + 2 + 8 + 16 + 64 + 128,
    1 + 2 + 4 + 16 + 32 + 64 + 256, 511 - 32, 511, 511 - 32, 511, 511 - 32, 511,
    511 - 32, 511);
  InitMulti: array [nPlOffered + 1 .. nPl] of integer = (256, 256, 256 + 128,
    256 + 128, 256 + 128 + 64, 256 + 128 + 64);

  pgStartRandom = 0;
  pgStartMap = 1;
  pgNoLoad = 2;
  pgLoad = 3;
  pgEditRandom = 4;
  pgEditMap = 5;
  pgMain = 6;

  PlayerAutoDiff: array [1 .. 5] of integer = (1, 1, 2, 2, 3);
  EnemyAutoDiff: array [1 .. 5] of integer = (4, 3, 2, 1, 1);

procedure TStartDlg.FormCreate(Sender: TObject);
var
  x, y, i: Integer;
  r0, r1: HRgn;
  Location: TPoint;
  AIBrains: TBrains;
  PlayerSlot: TPlayerSlot;
begin
  PlayerSlots := TPlayerSlots.Create;
  PlayerSlots.Count := nPlOffered;
  for I := 0 to PlayerSlots.Count - 1 do begin
    PlayerSlot := TPlayerSlot.Create;
    PlayerSlot.Rect := Bounds(xBrain[I], YBrain[I], 0, 0);
    PlayerSlots[I] := PlayerSlot;
  end;
  LoadConfig;

  ActionsOffered := [maManual, maCredits, maWeb];
  Include(ActionsOffered, maConfig);
  if FileExists(HomeDir + 'AI Template' + DirectorySeparator + 'AI development manual.html') then
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
  DirectDlg.Top := (screen.Height - DirectDlg.Height) div 2;

  if FullScreen then
  begin
    Location := Point((Screen.Width - 800) * 3 div 8,
      Screen.Height - Height - (Screen.Height - 600) div 3);
    Left := Location.X;
    Top := Location.Y;

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
  end
  else
  begin
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
  end;

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

  Brains[0].Picture := TBitmap.Create;
  Brains[0].Picture.SetSize(64, 64);
  BitBlt(Brains[0].Picture.Canvas.Handle, 0, 0, 64, 64,
    GrExt[HGrSystem2].Data.Canvas.Handle, 1, 111, SRCCOPY);
  Brains[1].Picture := TBitmap.Create;
  Brains[1].Picture.SetSize(64, 64);
  BitBlt(Brains[1].Picture.Canvas.Handle, 0, 0, 64, 64,
    GrExt[HGrSystem2].Data.Canvas.Handle, 66, 111, SRCCOPY);
  Brains[2].Picture := TBitmap.Create;
  Brains[2].Picture.SetSize(64, 64);
  BitBlt(Brains[2].Picture.Canvas.Handle, 0, 0, 64, 64,
    GrExt[HGrSystem2].Data.Canvas.Handle, 131, 111, SRCCOPY);
  Brains[3].Picture := TBitmap.Create;
  Brains[3].Picture.SetSize(64, 64);
  BitBlt(Brains[3].Picture.Canvas.Handle, 0, 0, 64, 64,
    GrExt[HGrSystem2].Data.Canvas.Handle, 131, 46, SRCCOPY);

  AIBrains := TBrains.Create(False);
  Brains.GetByKind(btAI, AIBrains);
  for i := 0 to AIBrains.Count - 1 do
  with AIBrains[I] do
  begin
    AIBrains[i].Picture := TBitmap.Create;
    if not LoadGraphicFile(AIBrains[i].Picture, HomeDir + 'AI' + DirectorySeparator +
      FileName + DirectorySeparator + FileName + '.png', gfNoError) then begin
      AIBrains[i].Picture.SetSize(64, 64);
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

  EmptyPicture := TBitmap.Create;
  EmptyPicture.PixelFormat := pf24bit;
  EmptyPicture.SetSize(64, 64);
  EmptyPicture.Canvas.FillRect(0, 0, EmptyPicture.Width, EmptyPicture.Height);
  LogoBuffer := TBitmap.Create;
  LogoBuffer.PixelFormat := pf24bit;
  LogoBuffer.SetSize(wBuffer, 56);
  LogoBuffer.Canvas.FillRect(0, 0, LogoBuffer.Width, LogoBuffer.Height);

  Mini := TBitmap.Create;
  for x := 0 to 11 do
    for y := 0 to 1 do
      MiniColors[x, y] := GrExt[HGrSystem].Data.Canvas.Pixels[66 + x, 67 + y];
  InitButtons;

  PlayersBrain[0] := BrainTerm;
  SlotAvailable := -1;
  Tab := 2;
  Diff0 := 2;
  TurnValid := false;
  Tracking := false;
  FormerGames := TStringList.Create;
  UpdateFormerGames;
  ShowTab := 2; // always start with new book page
  MapFileName := '';
  Maps := TStringList.Create;
  UpdateMaps;
end;

procedure TStartDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FormerGames);
  FreeAndNil(Maps);
  FreeAndNil(Mini);
  FreeAndNil(EmptyPicture);
  FreeAndNil(LogoBuffer);
  FreeAndNil(PlayerSlots);
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
  if not invalidateTab0 then
  begin
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
    WriteInteger('MultiControl', 0);

    OpenKey(AppRegistryKey, True);
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
    {$IFDEF WINDOWS}
    if ScreenMode = 2 then
      ChangeResolution(ResolutionX, ResolutionY, ResolutionBPP,
        ResolutionFreq);
    {$ENDIF}
  finally
    Free;
  end;
end;

procedure TStartDlg.FormPaint(Sender: TObject);
const
  TabNames: array [0 .. 3] of integer = (0, 11, 3, 4);

  procedure DrawAction(y, IconIndex: integer; HeaderItem, TextItem: string);
  begin
    Canvas.Font.Assign(UniFont[ftCaption]);
    Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
    RisedTextOut(Canvas, xAction, y - 3, Phrases2.Lookup(HeaderItem));
    Canvas.Font.Assign(UniFont[ftNormal]);
    BiColorTextOut(Canvas, Colors.Canvas.Pixels[clkAge0 - 1, cliDimmedText],
      $000000, xAction, y + 21, Phrases2.Lookup(TextItem));
    BitBltCanvas(LogoBuffer.Canvas, 0, 0, 50, 50, Canvas,
      xActionIcon - 2, y - 2, SRCCOPY);
    GlowFrame(LogoBuffer, 8, 8, 34, 34, $202020);
    BitBlt(Canvas.Handle, xActionIcon - 2, y - 2, 50, 50,
      LogoBuffer.Canvas.Handle, 0, 0, SRCCOPY);
    BitBlt(Canvas.Handle, xActionIcon, y, 40, 40, BigImp.Canvas.Handle,
      (IconIndex mod 7) * xSizeBig + 8, (IconIndex div 7) * ySizeBig, SRCCOPY);
    RFrame(Canvas, xActionIcon - 1, y - 1, xActionIcon + 40, y + 40,
      $000000, $000000);
  end;

var
  i, w, h, xMini, yMini, y: integer;
  s: string;
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
    end
  end
  else if Page <> pgMain then
    Frame(Canvas, 328, Up1Btn.top - 15, ClientWidth, Up2Btn.top + 38,
      MainTexture.clBevelShade, MainTexture.clBevelLight);
  Frame(Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);

  // draw tabs
  Frame(Canvas, 2, 2 + 2 * integer(Tab <> 0), TabOffset + (0 + 1) * TabSize - 1,
    TabHeight, MainTexture.clBevelLight, MainTexture.clBevelShade);
  Frame(Canvas, 1, 1 + 2 * integer(Tab <> 0), TabOffset + (0 + 1) * TabSize,
    TabHeight, MainTexture.clBevelLight, MainTexture.clBevelShade);
  Canvas.Pixels[1, 1 + 2 * integer(Tab <> 0)] := MainTexture.clBevelShade;
  for i := 1 to 3 do
  begin
    Frame(Canvas, TabOffset + i * TabSize + 2, 2 + 2 * integer(Tab <> i),
      TabOffset + (i + 1) * TabSize - 1, TabHeight, MainTexture.clBevelLight,
      MainTexture.clBevelShade);
    Frame(Canvas, TabOffset + i * TabSize + 1, 1 + 2 * integer(Tab <> i),
      TabOffset + (i + 1) * TabSize, TabHeight, MainTexture.clBevelLight,
      MainTexture.clBevelShade);
    Canvas.Pixels[TabOffset + i * TabSize + 1, 1 + 2 * integer(Tab <> i)] :=
      MainTexture.clBevelShade;
  end;
  Canvas.Font.Assign(UniFont[ftNormal]);
  for i := 1 to 3 do
  begin
    s := Phrases.Lookup('STARTCONTROLS', TabNames[i]);
    RisedTextOut(Canvas, TabOffset + i * TabSize + 1 +
      (TabSize - BiColorTextWidth(Canvas, s)) div 2,
      10 + 2 * integer(Tab <> i), s);
  end;
  Frame(Canvas, TabOffset + 4 * TabSize + 1, -1, ClientWidth, TabHeight,
    $000000, $000000);
  Frame(Canvas, 1, TabHeight + 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.clBevelLight, MainTexture.clBevelShade);
  Frame(Canvas, 2, TabHeight + 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.clBevelLight, MainTexture.clBevelShade);
  if Tab = 0 then
  begin
    PaintBackground(self, 3, TabHeight - 1, TabSize - 4 - 3 + TabOffset + 3, 4);
    Canvas.Pixels[2, TabHeight] := MainTexture.clBevelLight;
  end
  else
  begin
    PaintBackground(self, TabOffset + 3 + Tab * TabSize, TabHeight - 1,
      TabSize - 4, 4);
    Canvas.Pixels[TabOffset + Tab * TabSize + 2, TabHeight] :=
      MainTexture.clBevelLight;
  end;
  Canvas.Pixels[TabOffset + (Tab + 1) * TabSize - 1, TabHeight + 1] :=
    MainTexture.clBevelShade;
  if Tab < 3 then
    Frame(Canvas, TabOffset + (Tab + 1) * TabSize + 1, 3,
      TabOffset + (Tab + 1) * TabSize + 2, TabHeight, MainTexture.clBevelShade,
      MainTexture.clBevelShade); // Tab shadow
  BitBltCanvas(LogoBuffer.Canvas, 0, 0, 36, 36, Canvas, 6,
    3 + 2 * integer(Tab <> 0), SRCCOPY);


  ImageOp_BCC(LogoBuffer, Templates, 0, 0, 145, 38, 36, 27, $BFBF20, $4040DF);
  // logo part 1
  ImageOp_BCC(LogoBuffer, Templates, 10, 27, 155, 38 + 27, 26, 9, $BFBF20,
    $4040DF); // logo part 2
  BitBlt(Canvas.Handle, 6, 3 + 2 * integer(Tab <> 0), 36, 36,
    LogoBuffer.Canvas.Handle, 0, 0, SRCCOPY);

  if Page = pgMain then
  begin
    if SelectedAction >= 0 then // mark selected action
      for i := 0 to (ClientWidth - 2 * ActionSideBorder) div wBuffer + 1 do
      begin
        w := ClientWidth - 2 * ActionSideBorder - i * wBuffer;
        if w > wBuffer then
          w := wBuffer;
        h := ActionPitch;
        if yAction + SelectedAction * ActionPitch - 8 + h > ClientHeight - ActionBottomBorder
        then
          h := ClientHeight - ActionBottomBorder -
            (yAction + SelectedAction * ActionPitch - 8);
        BitBltCanvas(LogoBuffer.Canvas, 0, 0, w, h, Canvas,
          ActionSideBorder + i * wBuffer, yAction + SelectedAction * ActionPitch
          - 8, SRCCOPY);
        MakeBlue(LogoBuffer, 0, 0, w, h);
        BitBlt(Canvas.Handle, ActionSideBorder + i * wBuffer,
          yAction + SelectedAction * ActionPitch - 8, w, h,
          LogoBuffer.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    y := yAction;
    for i := 0 to nMainActions - 1 do
    begin
      if i in ActionsOffered then
        case i of
          maConfig:
            DrawAction(y, 25, 'ACTIONHEADER_CONFIG', 'ACTION_CONFIG');
          maManual:
            DrawAction(y, 19, 'ACTIONHEADER_MANUAL', 'ACTION_MANUAL');
          maCredits:
            DrawAction(y, 22, 'ACTIONHEADER_CREDITS', 'ACTION_CREDITS');
          maAIDev:
            DrawAction(y, 24, 'ACTIONHEADER_AIDEV', 'ACTION_AIDEV');
          maWeb:
            begin
              Canvas.Font.Assign(UniFont[ftCaption]);
              // Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline];
              RisedTextOut(Canvas, xActionIcon + 99, y,
                Phrases2.Lookup('ACTIONHEADER_WEB'));
              Canvas.Font.Assign(UniFont[ftNormal]);
              BitBltCanvas(LogoBuffer.Canvas, 0, 0, 91, 25, Canvas,
                xActionIcon, y + 2, SRCCOPY);
              ImageOp_BCC(LogoBuffer, Templates, 0, 0, 1, 400, 91, 25, 0,
                Colors.Canvas.Pixels[clkAge0 - 1, cliDimmedText]);
              BitBlt(Canvas.Handle, xActionIcon, y + 2, 91, 25,
                LogoBuffer.Canvas.Handle, 0, 0, SRCCOPY);
            end;
        end;
      inc(y, ActionPitch);
    end
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
          BitBlt(Canvas.Handle, 9 + i * 27, yLogo - 2, wOrna, hOrna,
            GrExt[HGrSystem2].Mask.Canvas.Handle, xOrna, yOrna, SRCAND);
          BitBlt(Canvas.Handle, 9 + i * 27, yLogo - 2, wOrna, hOrna,
            GrExt[HGrSystem2].Data.Canvas.Handle, xOrna, yOrna, SRCPAINT);
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
            BitBlt(Canvas.Handle, xBrain[i] - 18, yBrain[i] + 19, 12, 14,
              GrExt[HGrSystem].Data.Canvas.Handle, 134 + (Difficulty[i] - 1) *
              13, 28, SRCCOPY);
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
              BitBlt(Canvas.Handle, xBrain[i] - 31, yBrain[i], 13, 12,
                GrExt[HGrSystem].Data.Canvas.Handle, 88, 47, SRCCOPY);
            end
          end;
          if Assigned(PlayersBrain[i]) then
          begin
            PlayerSlots[i].DiffUpBtn.Hint := Format(Phrases.Lookup('STARTCONTROLS', 9),
              [PlayersBrain[i].Name]);
            PlayerSlots[i].DiffDownBtn.Hint := PlayerSlots[i].DiffUpBtn.Hint;
          end
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
          BitBlt(Canvas.Handle, 9 + i * 27, yLogo - 2, wOrna, hOrna,
            GrExt[HGrSystem2].Mask.Canvas.Handle, xOrna, yOrna, SRCAND);
          BitBlt(Canvas.Handle, 9 + i * 27, yLogo - 2, wOrna, hOrna,
            GrExt[HGrSystem2].Data.Canvas.Handle, xOrna, yOrna, SRCPAINT);
        end;
      PaintLogo(Canvas, 69, yLogo, MainTexture.clBevelLight,
        MainTexture.clBevelShade);
    end
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
    s := IntToStr((lxpre[WorldSize] * lypre[WorldSize] * 20 +
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

  if not(Page in [pgMain, pgNoLoad]) then
  begin
    xMini := x0Mini - MiniWidth;
    yMini := y0Mini - MiniHeight div 2;
    Frame(Canvas, xMini, yMini, xMini + 3 + MiniWidth * 2,
      yMini + 3 + MiniHeight, MainTexture.clBevelLight,
      MainTexture.clBevelShade);
    Frame(Canvas, xMini + 1, yMini + 1, xMini + 2 + MiniWidth * 2,
      yMini + 2 + MiniHeight, MainTexture.clBevelShade,
      MainTexture.clBevelLight);
  end;
  s := '';
  if MiniMode = mmPicture then
  begin
    BitBlt(Canvas.Handle, xMini + 2, yMini + 2, MiniWidth * 2, MiniHeight,
      Mini.Canvas.Handle, 0, 0, SRCCOPY);
    if Page = pgStartRandom then
      s := Phrases.Lookup('RANMAP')
  end
  else if MiniMode = mmMultiPlayer then
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

procedure TStartDlg.FormShow(Sender: TObject);
var
  x, y: integer;
  PicturePixel: TPixelPointer;
begin
  SetMainTextureByAge(-1);
  List.Font.Color := MainTexture.clMark;
  EmptyPicture.BeginUpdate;

  Fill(EmptyPicture.Canvas, 0, 0, 64, 64, (wMaintexture - 64) div 2,
    (hMaintexture - 64) div 2);
  // darken texture for empty slot
  PicturePixel.Init(EmptyPicture);
  for y := 0 to 63 do begin
    for x := 0 to 64 - 1 do begin
      PicturePixel.Pixel^.B := Max(PicturePixel.Pixel^.B - 28, 0);
      PicturePixel.Pixel^.G := Max(PicturePixel.Pixel^.G - 28, 0);
      PicturePixel.Pixel^.R := Max(PicturePixel.Pixel^.R - 28, 0);
      PicturePixel.NextPixel;
    end;
    PicturePixel.NextLine;
  end;
  EmptyPicture.EndUpdate;

  Difficulty[0] := Diff0;

  SelectedAction := -1;
  if ShowTab = 3 then
    PreviewMap(StartLandMass); // avoid delay on first TabX change
  ChangeTab(ShowTab);
  Background.Enabled := false;
end;

procedure TStartDlg.UnlistBackupFile(FileName: string);
var
  i: integer;
begin
  if FileName[1] <> '~' then
    FileName := '~' + FileName;
  i := FormerGames.Count - 1;
  while (i >= 0) and (AnsiCompareFileName(FormerGames[i], FileName) <> 0) do
    dec(i);
  if i >= 0 then
  begin
    FormerGames.Delete(i);
    if ListIndex[2] = i then
      ListIndex[2] := 0
  end
end;

procedure TStartDlg.StartBtnClick(Sender: TObject);
var
  i, GameCount, MapCount: integer;
  FileName: string;
  Reg: TRegistry;
begin
  case Page of
    pgLoad:
      begin // load
        FileName := List.Items[List.ItemIndex];
        if LoadGame(DataDir + 'Saved' + DirectorySeparator, FileName + '.cevo', LoadTurn, false)
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
          exit
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
            inc(GameCount);
            FileName := Format(Phrases.Lookup('GAME'), [GameCount]);
          end;

          // save settings and AI assignment
          if Page = pgStartRandom then begin
            WriteInteger('WorldSize', WorldSize);
            WriteInteger('LandMass', StartLandMass);

            OpenKey(AppRegistryKey + '\AI', True);
            if AutoDiff < 0 then
              for i := 0 to nPlOffered - 1 do begin
                if not Assigned(PlayersBrain[i]) then
                  Reg.WriteString('Control' + IntToStr(i), '')
                else Reg.WriteString('Control' + IntToStr(i),
                  PlayersBrain[i].FileName);
                WriteInteger('Diff' + IntToStr(i), Difficulty[i]);
              end;
            WriteInteger('MultiControl', MultiControl);
          end;

          OpenKey(AppRegistryKey, True);
          if AutoDiff > 0 then
          begin
            WriteString('DefaultAI', BrainDefault.FileName);
            SlotAvailable := 0; // bixView will be invalid hereafter
            PlayersBrain[0] := BrainTerm;
            Difficulty[0] := PlayerAutoDiff[AutoDiff];
            for i := 1 to nPl - 1 do
              if (Page = pgStartRandom) and (i <= AutoEnemies) or
                (Page = pgStartMap) and (i < nMapStartPositions) then begin
                if AutoDiff = 1 then PlayersBrain[i] := BrainBeginner
                  else PlayersBrain[i] := BrainDefault;
                Difficulty[i] := EnemyAutoDiff[AutoDiff];
              end  else PlayersBrain[i] := nil;
          end else begin
            for i := 6 to 8 do
              if (PlayersBrain[0].Kind <> btNoTerm) and (MultiControl and (1 shl i) <> 0)
              then begin
                PlayersBrain[i + 3] := PlayersBrain[i];
                Difficulty[i + 3] := Difficulty[i];
                PlayersBrain[i + 6] := PlayersBrain[i];
                Difficulty[i + 6] := Difficulty[i];
              end else begin
                PlayersBrain[i + 3] := nil;
                PlayersBrain[i + 6] := nil;
              end
          end;

          WriteInteger('AutoDiff', AutoDiff);
          WriteInteger('AutoEnemies', AutoEnemies);
          WriteInteger('MaxTurn', MaxTurn);
          WriteInteger('GameCount', GameCount);
        finally
          Free;
        end;

        StartNewGame(DataDir + 'Saved' + DirectorySeparator, FileName + '.cevo', MapFileName,
          lxpre[WorldSize], lypre[WorldSize], StartLandMass, MaxTurn);
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
        MapFileName := Format(Phrases.Lookup('MAP'), [MapCount]) + '.cevo map';
        EditMap(MapFileName, lxpre[WorldSize], lypre[WorldSize], StartLandMass);
      end
  end
end;

procedure TStartDlg.PaintInfo;

  procedure PaintRandomMini(Brightness: integer);
  var
    i, x, y, xm, cm: integer;
    MiniPixel: TPixelPointer;
    Map: ^TTileList;
  begin
    Map := PreviewMap(StartLandMass);
    MiniWidth := lxpre[WorldSize];
    MiniHeight := lypre[WorldSize];

    Mini.PixelFormat := pf24bit;
    Mini.SetSize(MiniWidth * 2, MiniHeight);
    Mini.BeginUpdate;
    MiniPixel.Init(Mini);
    for y := 0 to MiniHeight - 1 do begin
      for x := 0 to MiniWidth - 1 do begin
        for i := 0 to 1 do begin
          xm := (x * 2 + i + y and 1) mod (MiniWidth * 2);
          MiniPixel.SetX(xm);
          cm := MiniColors
            [Map[x * lxmax div MiniWidth + lxmax *
            ((y * (lymax - 1) + MiniHeight div 2) div (MiniHeight - 1))] and
            fTerrain, i];
          MiniPixel.Pixel^.B := ((cm shr 16) and $FF) * Brightness div 3;
          MiniPixel.Pixel^.G := ((cm shr 8) and $FF) * Brightness div 3;
          MiniPixel.Pixel^.R := ((cm shr 0) and $FF) * Brightness div 3;
        end;
      end;
      MiniPixel.NextLine;
    end;
    Mini.EndUpdate;
  end;

var
  SaveMap: array [0 .. lxmax * lymax - 1] of Byte;

  procedure PaintFileMini;
  var
    i, x, y, xm, cm, Tile, OwnColor, EnemyColor: integer;
    MiniPixel, PrevMiniPixel: TPixelPointer;
  begin
    OwnColor := GrExt[HGrSystem].Data.Canvas.Pixels[95, 67];
    EnemyColor := GrExt[HGrSystem].Data.Canvas.Pixels[96, 67];
    Mini.PixelFormat := pf24bit;
    Mini.SetSize(MiniWidth * 2, MiniHeight);
    if MiniMode = mmPicture then
    begin
      Mini.BeginUpdate;
      MiniPixel.Init(Mini);
      PrevMiniPixel.Init(Mini, 0, -1);
      for y := 0 to MiniHeight - 1 do begin
        for x := 0 to MiniWidth - 1 do begin
          for i := 0 to 1 do begin
            xm := (x * 2 + i + y and 1) mod (MiniWidth * 2);
            MiniPixel.SetX(xm);
            Tile := SaveMap[x + MiniWidth * y];
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
              end
            end
            else if (i = 0) and (Tile and smUnit <> 0) then
              if Tile and smOwned <> 0 then
                cm := OwnColor
              else
                cm := EnemyColor
            else
              cm := MiniColors[Tile and fTerrain, i];
            MiniPixel.Pixel^.B := cm shr 16;
            MiniPixel.Pixel^.G:= cm shr 8 and $FF;
            MiniPixel.Pixel^.R := cm and $FF;
          end;
        end;
        MiniPixel.NextLine;
        PrevMiniPixel.NextLine;
      end;
      Mini.EndUpdate;
    end;
  end;

var
  x, y, dummy, FileLandMass, lxFile, lyFile: integer;
  LogFile, MapFile: file;
  s: string[255];
  MapRow: array [0 .. lxmax - 1] of Cardinal;

begin
  case Page of
    pgStartRandom:
      begin
        MiniMode := mmPicture;
        PaintRandomMini(3);
      end;

    pgNoLoad:
      begin
        MiniWidth := lxpre[DefaultWorldSize];
        MiniHeight := lypre[DefaultWorldSize];
        MiniMode := mmNone;
      end;

    pgLoad:
      begin
        AssignFile(LogFile, DataDir + 'Saved' + DirectorySeparator + List.Items[List.ItemIndex]
          + '.cevo');
        try
          Reset(LogFile, 4);
          BlockRead(LogFile, s[1], 2); { file id }
          BlockRead(LogFile, dummy, 1); { format id }
          if dummy >= $000E01 then
            BlockRead(LogFile, dummy, 1); { item stored since 0.14.1 }
          BlockRead(LogFile, MiniWidth, 1);
          BlockRead(LogFile, MiniHeight, 1);
          BlockRead(LogFile, FileLandMass, 1);
          if FileLandMass = 0 then
            for y := 0 to MiniHeight - 1 do
              BlockRead(LogFile, MapRow, MiniWidth);
          BlockRead(LogFile, dummy, 1);
          BlockRead(LogFile, dummy, 1);
          BlockRead(LogFile, LastTurn, 1);
          BlockRead(LogFile, SaveMap, 1);
          if SaveMap[0] = $80 then
            MiniMode := mmMultiPlayer
          else
            MiniMode := mmPicture;
          if MiniMode = mmPicture then
            BlockRead(LogFile, SaveMap[4], (MiniWidth * MiniHeight - 1) div 4);
          CloseFile(LogFile);
        except
          CloseFile(LogFile);
          LastTurn := 0;
          MiniWidth := lxpre[DefaultWorldSize];
          MiniHeight := lypre[DefaultWorldSize];
          MiniMode := mmNone;
        end;
        // BookDate:=DateToStr(FileDateToDateTime(FileAge(FileName)));
        PaintFileMini;
        if not TurnValid then
        begin
          LoadTurn := LastTurn;
          SmartInvalidate(xTurnSlider - 2, y0Mini + 61,
            xTurnSlider + wTurnSlider + 2, yTurnSlider + 9);
        end;
        TurnValid := true;
      end;

    pgEditRandom:
      begin
        MapFileName := '';
        MiniMode := mmPicture;
        PaintRandomMini(4);
      end;

    pgStartMap, pgEditMap:
      begin
        MiniMode := mmPicture;
        if Page = pgEditMap then
          MapFileName := List.Items[List.ItemIndex] + '.cevo map';
        if LoadGraphicFile(Mini, DataDir + 'Maps' + DirectorySeparator + Copy(MapFileName, 1,
          Length(MapFileName) - 9) + '.png', gfNoError) then
        begin
          if Mini.width div 2 > MaxWidthMapLogo then
            Mini.width := MaxWidthMapLogo * 2;
          if Mini.height > MaxHeightMapLogo then
            Mini.height := MaxHeightMapLogo;
          MiniWidth := Mini.width div 2;
          MiniHeight := Mini.height;
        end
        else
        begin
          MiniMode := mmNone;
          MiniWidth := MaxWidthMapLogo;
          MiniHeight := MaxHeightMapLogo;
        end;

        AssignFile(MapFile, DataDir + 'Maps' + DirectorySeparator + MapFileName);
        try
          Reset(MapFile, 4);
          BlockRead(MapFile, s[1], 2); { file id }
          BlockRead(MapFile, x, 1); { format id }
          BlockRead(MapFile, x, 1); // MaxTurn
          BlockRead(MapFile, lxFile, 1);
          BlockRead(MapFile, lyFile, 1);
          nMapLandTiles := 0;
          nMapStartPositions := 0;
          for y := 0 to lyFile - 1 do
          begin
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
        if Page = pgEditMap then
          SmartInvalidate(x0Mini - 112, y0Mini + 61, x0Mini + 112, y0Mini + 91);
      end
  end;
  SmartInvalidate(x0Mini - lxmax, y0Mini - lymax div 2,
    x0Mini - lxmax + 2 * lxmax + 4, y0Mini - lymax div 2 + lymax + 4);
end;

procedure TStartDlg.BrainClick(Sender: TObject);
var
  i: integer;
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
            PlayersBrain[i] := nil;
            PlayerSlots[i].DiffUpBtn.Visible := false;
            PlayerSlots[i].DiffUpBtn.Tag := 0;
            PlayerSlots[i].DiffDownBtn.Visible := false;
            PlayerSlots[i].DiffDownBtn.Tag := 0;
            if PlayerSlots[i].OfferMultiple then begin
              PlayerSlots[i].MultiBtn.Visible := false;
              PlayerSlots[i].MultiBtn.Tag := 0;
            end;
            SmartInvalidate(xBrain[i] - 31, yBrain[i] - 1, xBrain[i] + 64,
              PlayerSlots[i].DiffUpBtn.top + 25);
          end;
        BrainTerm.Flags := BrainTerm.Flags and not fUsed;
      end;
    end;
    SmartInvalidate(xBrain[PlayerPopupIndex] - 31, yBrain[PlayerPopupIndex] - 1,
      xBrain[PlayerPopupIndex] + 64, PlayerSlots[PlayerPopupIndex].DiffUpBtn.top + 25);
  end
end;

procedure TStartDlg.InitPopup(PlayerIndex: Integer);
var
  I: Integer;
  FixedLines: integer;
  MenuItem: TMenuItem;
  AIBrains: TBrains;

  procedure OfferBrain(Brain: TBrain);
  var
    J: Integer;
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

begin
  PlayerPopupIndex := PlayerIndex;
  EmptyMenu(PopupMenu1.Items);
  if PlayerPopupIndex < 0 then begin // select default AI
    FixedLines := 0;
    if Brains.GetKindCount(btAI) >= 2 then begin
      OfferBrain(BrainRandom);
      Inc(FixedLines);
    end;
    AIBrains := TBrains.Create(False);
    Brains.GetByKind(btAI, AIBrains);
    for I := 0 to AIBrains.Count - 1 do // offer available AIs
      if AIBrains[I].Flags and fMultiple <> 0 then
        OfferBrain(AIBrains[I]);
    AIBrains.Free;
  end else begin
    FixedLines := 0;
    if PlayerPopupIndex > 0 then begin
      OfferBrain(nil);
      Inc(FixedLines);
    end;
    for I := Brains.IndexOf(BrainTerm) downto 0 do // offer game interfaces
      if (PlayerPopupIndex = 0) or (Brains[i].Kind = btTerm) and
        (PlayersBrain[0].Kind <> btNoTerm) then begin
        OfferBrain(Brains[I]);
        Inc(FixedLines);
      end;
    if PlayerPopupIndex > 0 then begin
      MenuItem := TMenuItem.Create(PopupMenu1);
      MenuItem.Caption := '-';
      PopupMenu1.Items.Add(MenuItem);
      Inc(FixedLines);
      if Brains.GetKindCount(btAI) >= 2 then begin
        OfferBrain(BrainRandom);
        Inc(FixedLines);
      end;
      AIBrains := TBrains.Create(False);
      Brains.GetByKind(btAI, AIBrains);
      for I := 0 to AIBrains.Count - 1 do // offer available AIs
        if (AIBrains[I].Flags and fMultiple <> 0) or (AIBrains[I].Flags and fUsed = 0)
          or (Brains[I] = PlayersBrain[PlayerPopupIndex]) then
          OfferBrain(AIBrains[i]);
      AIBrains.Free;
    end;
  end;
end;

procedure TStartDlg.UpdateFormerGames;
var
  i: integer;
  f: TSearchRec;
begin
  FormerGames.Clear;
  if FindFirst(DataDir + 'Saved' + DirectorySeparator + '*.cevo', $21, f) = 0 then
    repeat
      i := FormerGames.Count;
      while (i > 0) and (f.Time < integer(FormerGames.Objects[i - 1])) do
        dec(i);
      FormerGames.InsertObject(i, Copy(f.Name, 1, Length(f.Name) - 5),
        TObject(f.Time));
    until FindNext(f) <> 0;
  FindClose(f);
  ListIndex[2] := FormerGames.Count - 1;
  if (ShowTab = 2) and (FormerGames.Count > 0) then
    ShowTab := 3;
  TurnValid := false;
end;

procedure TStartDlg.UpdateMaps;
var
  f: TSearchRec;
begin
  Maps.Clear;
  if FindFirst(DataDir + 'Maps' + DirectorySeparator + '*.cevo map', $21, f) = 0 then
    repeat
      Maps.Add(Copy(f.Name, 1, Length(f.Name) - 9));
    until FindNext(f) <> 0;
  FindClose(F);
  Maps.Sort;
  Maps.Insert(0, Phrases.Lookup('RANMAP'));
  ListIndex[0] := Maps.IndexOf(Copy(MapFileName, 1, Length(MapFileName) - 9));
  if ListIndex[0] < 0 then
    ListIndex[0] := 0;
end;

procedure TStartDlg.ChangePage(NewPage: integer);
var
  i, j, p1: integer;
  s: string;
  Reg: TRegistry;
  invalidateTab0: boolean;
begin
  invalidateTab0 := (Page = pgMain) or (NewPage = pgMain);
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
              MultiControl := Reg.ReadInteger('MultiControl');
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
    Controls[i].Visible := Controls[i].Tag and (256 shl Page) <> 0;
  if Page = pgLoad then
    ReplayBtn.Visible := MiniMode <> mmMultiPlayer;
  List.Invalidate;
  SmartInvalidate(0, 0, ClientWidth, ClientHeight, invalidateTab0);
end;

procedure TStartDlg.ChangeTab(NewTab: integer);
begin
  Tab := NewTab;
  case Tab of
    1:
      List.Items.Assign(Maps);
    3:
      List.Items.Assign(FormerGames);
  end;
  if Tab <> 2 then
    if List.Count > 0 then begin
      if (List.Count > ListIndex[Tab]) then
        List.ItemIndex := ListIndex[Tab]
        else List.ItemIndex := 0;
    end else List.ItemIndex := -1;
  case Tab of
    0:
      ChangePage(pgMain);
    1:
      if List.ItemIndex = 0 then
        ChangePage(pgEditRandom)
      else
        ChangePage(pgEditMap);
    2:
      if MapFileName = '' then
        ChangePage(pgStartRandom)
      else
        ChangePage(pgStartMap);
    3:
      if FormerGames.Count = 0 then
        ChangePage(pgNoLoad)
      else
        ChangePage(pgLoad);
  end;
end;

procedure TStartDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
var
  i: integer;
begin
  if (y < TabHeight + 1) and (x - TabOffset < TabSize * 4) and
    ((x - TabOffset) div TabSize <> Tab) then
  begin
    // Play('BUTTON_DOWN');
    ListIndex[Tab] := List.ItemIndex;
    ChangeTab((x - TabOffset) div TabSize);
  end
  else if Page = pgMain then
  begin
    case SelectedAction of
      maConfig:
        begin
          LocaleDlg := TLocaleDlg.Create(nil);
          if LocaleDlg.ShowModal = mrOk then begin
            LoadPhrases;
            Invalidate;
          end;
          FreeAndNil(LocaleDlg);
        end;
      maManual:
        DirectHelp(cStartHelp);
      maCredits:
        DirectHelp(cStartCredits);
      maAIDev:
        OpenDocument(pchar(HomeDir + 'AI Template' + DirectorySeparator + 'AI development manual.html'));
      maWeb:
        OpenURL('http://c-evo.org')
    end;
  end
  else if (AutoDiff < 0) and ((Page = pgStartRandom) or (Page = pgStartMap) and
    (nMapStartPositions > 0)) then
  begin
    for i := 0 to nPlOffered - 1 do
      if (1 shl i and SlotAvailable <> 0) and (x >= xBrain[i]) and
        (y >= yBrain[i]) and (x < xBrain[i] + 64) and (y < yBrain[i] + 64) then
      begin
        InitPopup(i);
        if yBrain[i] > y0Brain then
          PopupMenu1.Popup(left + xBrain[i] + 4, top + yBrain[i] + 60)
        else
          PopupMenu1.Popup(left + xBrain[i] + 4, top + yBrain[i] + 4);
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
    Tracking := true
  end
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
  end
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
      end
  end
end;

procedure TStartDlg.Up1BtnClick(Sender: TObject);
begin
  if WorldSize < nWorldSize - 1 then
  begin
    inc(WorldSize);
    PaintInfo;
    SmartInvalidate(344, y0Mini - 77, 510, y0Mini - 77 + 21);
  end
end;

procedure TStartDlg.Down1BtnClick(Sender: TObject);
begin
  if WorldSize > 0 then
  begin
    dec(WorldSize);
    PaintInfo;
    SmartInvalidate(344, y0Mini - 77, 510, y0Mini - 77 + 21);
  end
end;

procedure TStartDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DirectDlg.Close
end;

procedure TStartDlg.ListClick(Sender: TObject);
var
  i: integer;
begin
  if (Tab = 1) and ((List.ItemIndex = 0) <> (Page = pgEditRandom)) then
  begin
    if List.ItemIndex = 0 then
      Page := pgEditRandom
    else
      Page := pgEditMap;
    for i := 0 to ControlCount - 1 do
      Controls[i].Visible := Controls[i].Tag and (256 shl Page) <> 0;
    SmartInvalidate(328, Up1Btn.top - 12, ClientWidth, Up2Btn.top + 35);
  end;
  if Page = pgLoad then
    TurnValid := false;
  PaintInfo;
  if Page = pgLoad then
    ReplayBtn.Visible := MiniMode <> mmMultiPlayer;
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
        AssignFile(f, DataDir + 'Saved' + DirectorySeparator + List.Items[List.ItemIndex] + '.cevo')
      else
        AssignFile(f, DataDir + 'Maps'+ DirectorySeparator + List.Items[List.ItemIndex] +
          '.cevo map');
      ok := true;
      try
        if Page = pgLoad then
          Rename(f, DataDir + 'Saved'+ DirectorySeparator + NewName + '.cevo')
        else
          Rename(f, DataDir + 'Maps'+ DirectorySeparator + NewName + '.cevo map');
      except
        // Play('INVALID');
        ok := false
      end;
      if Page <> pgLoad then
        try // rename map picture
          AssignFile(f, DataDir + 'Maps'+ DirectorySeparator + List.Items[List.ItemIndex]
            + '.png');
          Rename(f, DataDir + 'Maps'+ DirectorySeparator + NewName + '.png');
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
      end
    end
  end
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
        AssignFile(f, DataDir + 'Saved' + DirectorySeparator + List.Items[List.ItemIndex] + '.cevo')
      else
        AssignFile(f, DataDir + 'Maps' + DirectorySeparator + List.Items[List.ItemIndex] +
          '.cevo map');
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
            ReplayBtn.Visible := MiniMode <> mmMultiPlayer;
        end;
      end
    end
  end
end;

procedure TStartDlg.DiffBtnClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to nPlOffered - 1 do
    if (Sender = PlayerSlots[i].DiffUpBtn) and (Difficulty[i] < 3) or
      (Sender = PlayerSlots[i].DiffDownBtn) and (Difficulty[i] > 1) then
    begin
      if Sender = PlayerSlots[i].DiffUpBtn then
        inc(Difficulty[i])
      else
        dec(Difficulty[i]);
      SmartInvalidate(xBrain[i] - 18, yBrain[i] + 19, xBrain[i] - 18 + 12,
        yBrain[i] + (19 + 14));
    end
end;

procedure TStartDlg.MultiBtnClick(Sender: TObject);
var
  i: integer;
begin
  for i := 6 to 8 do
    if Sender = PlayerSlots[i].MultiBtn then
    begin
      MultiControl := MultiControl xor (1 shl i);
      TButtonC(Sender).ButtonIndex := 2 + (MultiControl shr i) and 1;
    end
end;

procedure TStartDlg.FormHide(Sender: TObject);
begin
  Diff0 := Difficulty[0];
  ListIndex[Tab] := List.ItemIndex;
  ShowTab := Tab;
  Background.Enabled := true;
end;

procedure TStartDlg.QuitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TStartDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = []) and (Key = VK_F1) then
    DirectHelp(cStartHelp);
end;

procedure TStartDlg.CustomizeBtnClick(Sender: TObject);
begin
  AutoDiff := -AutoDiff;
  CustomizeBtn.ButtonIndex := CustomizeBtn.ButtonIndex xor 1;
  ChangePage(Page)
end;

procedure TStartDlg.AutoDiffUpBtnClick(Sender: TObject);
begin
  if AutoDiff < 5 then
  begin
    inc(AutoDiff);
    SmartInvalidate(120, y0Mini + 61, 272, y0Mini + 61 + 21);
    SmartInvalidate(xDefault - 2, yDefault - 2, xDefault + 64 + 2,
      yDefault + 64 + 2);
  end
end;

procedure TStartDlg.AutoDiffDownBtnClick(Sender: TObject);
begin
  if AutoDiff > 1 then
  begin
    dec(AutoDiff);
    SmartInvalidate(120, y0Mini + 61, 272, y0Mini + 61 + 21);
    SmartInvalidate(xDefault - 2, yDefault - 2, xDefault + 64 + 2,
      yDefault + 64 + 2);
  end
end;

procedure TStartDlg.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
begin
  Tracking := false;
end;

procedure TStartDlg.FormMouseMove(Sender: TObject; Shift: TShiftState;
  x, y: integer);
var
  OldLoadTurn, NewSelectedAction: integer;
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
      NewSelectedAction := (y - (yAction - 8)) div ActionPitch;
      if not(NewSelectedAction in ActionsOffered) then
        NewSelectedAction := -1;
    end
    else
      NewSelectedAction := -1;
    if NewSelectedAction <> SelectedAction then
    begin
      if SelectedAction >= 0 then
        SmartInvalidate(ActionSideBorder, yAction + SelectedAction * ActionPitch
          - 8, ClientWidth - ActionSideBorder, yAction + (SelectedAction + 1) *
          ActionPitch - 8);
      SelectedAction := NewSelectedAction;
      if SelectedAction >= 0 then
        SmartInvalidate(ActionSideBorder, yAction + SelectedAction * ActionPitch
          - 8, ClientWidth - ActionSideBorder, yAction + (SelectedAction + 1) *
          ActionPitch - 8);
    end
  end
end;

procedure TStartDlg.AutoEnemyUpBtnClick(Sender: TObject);
begin
  if AutoEnemies < nPl - 1 then
  begin
    inc(AutoEnemies);
    SmartInvalidate(160, yMain + 140, 198, yMain + 140 + 21);
  end
end;

procedure TStartDlg.AutoEnemyDownBtnClick(Sender: TObject);
begin
  if AutoEnemies > 0 then
  begin
    dec(AutoEnemies);
    SmartInvalidate(160, yMain + 140, 198, yMain + 140 + 21);
  end
end;

procedure TStartDlg.ReplayBtnClick(Sender: TObject);
begin
  LoadGame(DataDir + 'Saved' + DirectorySeparator, List.Items[List.ItemIndex] + '.cevo',
    LastTurn, true);
  SlotAvailable := -1;
end;

end.
