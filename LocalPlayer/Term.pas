{$INCLUDE Switches.inc}
unit Term;

interface

uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
{$IFDEF UNIX}
  LMessages, Messages,
{$ENDIF}
  Protocol, Tribes, PVSB, ClientTools, ScreenTools, BaseWin, Messg, ButtonBase,
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, DrawDlg, Types,
  Forms, Menus, ExtCtrls, dateutils, Platform, ButtonB, ButtonC, EOTButton, Area,
  UGraphicSet, UMiniMap, IsoEngine;

const
  WM_EOT = WM_USER;

type
  TPaintLocTempStyle = (pltsNormal, pltsBlink);

  TSoundBlock = (sbStart, sbWonder, sbScience, sbContact, sbTurn);
  TSoundBlocks = set of TSoundBlock;

  { TMainScreen }

  TMainScreen = class(TDrawDlg)
    mBigTiles: TMenuItem;
    mNextUnit: TMenuItem;
    N13: TMenuItem;
    mPrevUnit: TMenuItem;
    Timer1: TTimer;
    GamePopup: TPopupMenu;
    UnitPopup: TPopupMenu;
    mIrrigation: TMenuItem;
    mCity: TMenuItem;
    mRoad: TMenuItem;
    mMine: TMenuItem;
    mPollution: TMenuItem;
    mHome: TMenuItem;
    mStay: TMenuItem;
    mDisband: TMenuItem;
    mWait: TMenuItem;
    mNoOrders: TMenuItem;
    MTrans: TMenuItem;
    UnitBtn: TButtonB;
    mResign: TMenuItem;
    mOptions: TMenuItem;
    mEnMoves: TMenuItem;
    mWaitTurn: TMenuItem;
    mRep: TMenuItem;
    mFort: TMenuItem;
    mCentre: TMenuItem;
    N1: TMenuItem;
    mAirBase: TMenuItem;
    N5: TMenuItem;
    mCityTypes: TMenuItem;
    mHelp: TMenuItem;
    mCanal: TMenuItem;
    mTest: TMenuItem;
    mLocCodes: TMenuItem;
    mLoad: TMenuItem;
    StatPopup: TPopupMenu;
    mCityStat: TMenuItem;
    mUnitStat: TMenuItem;
    mWonders: TMenuItem;
    mScienceStat: TMenuItem;
    mRailRoad: TMenuItem;
    mClear: TMenuItem;
    mFarm: TMenuItem;
    mAfforest: TMenuItem;
    mRep0: TMenuItem;
    mRep1: TMenuItem;
    mRep2: TMenuItem;
    mRep3: TMenuItem;
    mRep4: TMenuItem;
    mRep5: TMenuItem;
    mRep7: TMenuItem;
    mRep8: TMenuItem;
    mRep9: TMenuItem;
    mRep15: TMenuItem;
    mCancel: TMenuItem;
    mLog: TMenuItem;
    mEUnitStat: TMenuItem;
    mRep10: TMenuItem;
    mEnAttacks: TMenuItem;
    mEnNoMoves: TMenuItem;
    mDiagram: TMenuItem;
    mJump: TMenuItem;
    mNations: TMenuItem;
    mManip: TMenuItem;
    mManip0: TMenuItem;
    mManip1: TMenuItem;
    mManip2: TMenuItem;
    mManip3: TMenuItem;
    mManip4: TMenuItem;
    mManip5: TMenuItem;
    mEnhanceDef: TMenuItem;
    mEnhance: TMenuItem;
    mShips: TMenuItem;
    mMacro: TMenuItem;
    mRun: TMenuItem;
    N10: TMenuItem;
    mRepList: TMenuItem;
    mRepScreens: TMenuItem;
    mRep11: TMenuItem;
    mNames: TMenuItem;
    mManip6: TMenuItem;
    mRep12: TMenuItem;
    mRandomMap: TMenuItem;
    mUnload: TMenuItem;
    mRecover: TMenuItem;
    MapBtn0: TButtonC;
    MapBtn1: TButtonC;
    MapBtn4: TButtonC;
    MapBtn5: TButtonC;
    EditPopup: TPopupMenu;
    mCreateUnit: TMenuItem;
    MapBtn6: TButtonC;
    mDebugMap: TMenuItem;
    mUtilize: TMenuItem;
    mRep6: TMenuItem;
    mEnemyMovement: TMenuItem;
    mEnFastMoves: TMenuItem;
    mOwnMovement: TMenuItem;
    mSlowMoves: TMenuItem;
    mFastMoves: TMenuItem;
    mVeryFastMoves: TMenuItem;
    mGoOn: TMenuItem;
    mSound: TMenuItem;
    mSoundOn: TMenuItem;
    mSoundOnAlt: TMenuItem;
    mSoundOff: TMenuItem;
    N6: TMenuItem;
    TerrainBtn: TButtonB;
    TerrainPopup: TPopupMenu;
    mScrolling: TMenuItem;
    mScrollSlow: TMenuItem;
    mScrollFast: TMenuItem;
    mScrollOff: TMenuItem;
    mPillage: TMenuItem;
    mSelectTransport: TMenuItem;
    mEmpire: TMenuItem;
    N4: TMenuItem;
    N2: TMenuItem;
    mWebsite: TMenuItem;
    N3: TMenuItem;
    mRevolution: TMenuItem;
    mRep13: TMenuItem;
    UnitInfoBtn: TButtonB;
    EOT: TEOTButton;
    mAllyMovement: TMenuItem;
    mAlSlowMoves: TMenuItem;
    mAlFastMoves: TMenuItem;
    N7: TMenuItem;
    mEffectiveMovesOnly: TMenuItem;
    N8: TMenuItem;
    mAlEffectiveMovesOnly: TMenuItem;
    mAlNoMoves: TMenuItem;
    N9: TMenuItem;
    mViewpoint: TMenuItem;
    mTileSize: TMenuItem;
    mNormalTiles: TMenuItem;
    mSmallTiles: TMenuItem;
    N11: TMenuItem;
    MenuArea: TArea;
    TreasuryArea: TArea;
    ResearchArea: TArea;
    ManagementArea: TArea;
    mTechTree: TMenuItem;
    MovieSpeed1Btn: TButtonB;
    MovieSpeed2Btn: TButtonB;
    MovieSpeed3Btn: TButtonB;
    MovieSpeed4Btn: TButtonB;
    N12: TMenuItem;
    mRep14: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure mAfforestClick(Sender: TObject);
    procedure mAirBaseClick(Sender: TObject);
    procedure mCanalClick(Sender: TObject);
    procedure mCancelClick(Sender: TObject);
    procedure mCentreClick(Sender: TObject);
    procedure mcityClick(Sender: TObject);
    procedure mCityStatClick(Sender: TObject);
    procedure mCityTypesClick(Sender: TObject);
    procedure mClearClick(Sender: TObject);
    procedure mDiagramClick(Sender: TObject);
    procedure mEmpireClick(Sender: TObject);
    procedure mEnhanceClick(Sender: TObject);
    procedure mEnhanceDefClick(Sender: TObject);
    procedure mEUnitStatClick(Sender: TObject);
    procedure mFarmClick(Sender: TObject);
    procedure mfortClick(Sender: TObject);
    procedure mGoOnClick(Sender: TObject);
    procedure mHelpClick(Sender: TObject);
    procedure mhomeClick(Sender: TObject);
    procedure mirrigationClick(Sender: TObject);
    procedure mirrigationDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure mJumpClick(Sender: TObject);
    procedure mLoadClick(Sender: TObject);
    procedure mmineClick(Sender: TObject);
    procedure mNationsClick(Sender: TObject);
    procedure mNextUnitClick(Sender: TObject);
    procedure mnoordersClick(Sender: TObject);
    procedure mPillageClick(Sender: TObject);
    procedure mpollutionClick(Sender: TObject);
    procedure mPrevUnitClick(Sender: TObject);
    procedure mRandomMapClick(Sender: TObject);
    procedure mRecoverClick(Sender: TObject);
    procedure mResignClick(Sender: TObject);
    procedure mRevolutionClick(Sender: TObject);
    procedure mroadClick(Sender: TObject);
    procedure mRailRoadClick(Sender: TObject);
    procedure mRunClick(Sender: TObject);
    procedure mScienceStatClick(Sender: TObject);
    procedure mSelectTransportClick(Sender: TObject);
    procedure mShipsClick(Sender: TObject);
    procedure mstayClick(Sender: TObject);
    procedure mTechTreeClick(Sender: TObject);
    procedure mtransClick(Sender: TObject);
    procedure mUnitStatClick(Sender: TObject);
    procedure mUnloadClick(Sender: TObject);
    procedure mwaitClick(Sender: TObject);
    procedure mWebsiteClick(Sender: TObject);
    procedure mWondersClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MapBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EOTClick(Sender: TObject);
    procedure PanelBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mDisbandOrUtilizeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PanelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Toggle(Sender: TObject);
    procedure PanelBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PanelBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure mShowClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure mRepClicked(Sender: TObject);
    procedure mLogClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Radio(Sender: TObject);
    procedure mManipClick(Sender: TObject);
    procedure mNamesClick(Sender: TObject);
    procedure MapBtnClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CreateUnitClick(Sender: TObject);
    procedure mSoundOffClick(Sender: TObject);
    procedure mSoundOnClick(Sender: TObject);
    procedure mSoundOnAltClick(Sender: TObject);
    procedure UnitInfoBtnClick(Sender: TObject);
    procedure ViewpointClick(Sender: TObject);
    procedure DebugMapClick(Sender: TObject);
    procedure mSmallTilesClick(Sender: TObject);
    procedure mNormalTilesClick(Sender: TObject);
    procedure mBigTilesClick(Sender: TObject);
    procedure GrWallBtnDownChanged(Sender: TObject);
    procedure BareBtnDownChanged(Sender: TObject);
    procedure MovieSpeedBtnClick(Sender: TObject);
  private
    xw: Integer; // Base map x
    yw: Integer; // Base map y
    xwd: Integer;
    ywd: Integer;
    xwMini: Integer;
    ywMini: Integer;
    xMidPanel: Integer;
    xRightPanel: Integer;
    xTroop: Integer;
    xTerrain: Integer;
    xMini: Integer;
    yMini: Integer;
    ywmax: Integer;
    ywcenter: Integer;
    TroopLoc: Integer;
    TrCnt: Integer;
    TrRow: Integer;
    TrPitch: Integer;
    MapWidth: Integer;
    MapOffset: Integer;
    MapHeight: Integer;
    BlinkTime: Integer;
    BrushLoc: Integer;
    EditLoc: Integer;
    xMouse: Integer;
    yMouse: Integer;
    BrushType: Cardinal;
    trix: array [0 .. 63] of Integer;
    AILogo: array [0 .. nPl - 1] of TBitmap;
    MiniMap: TMiniMap;
    Panel: TBitmap;
    TopBar: TBitmap;
    sb: TPVScrollbar;
    Closable: Boolean;
    RepaintOnResize: Boolean;
    Tracking: Boolean;
    TurnComplete: Boolean;
    Edited: Boolean;
    GoOnPhase: Boolean;
    HaveStrategyAdvice: Boolean;
    FirstMovieTurn: Boolean;
    PrevWindowState: TWindowState;
    CurrentWindowState: TWindowState;
    MainMap: TIsoMap;
    NoMap: TIsoMap;
    NoMapPanel: TIsoMap;
    function ChooseUnusedTribe: Integer;
    function DoJob(j0: Integer): Integer;
    procedure GetTribeList;
    procedure InitModule;
    procedure DoneModule;
    procedure InitTurn(NewPlayer: Integer);
    procedure SaveMenuItemsState;
    procedure ScrollBarUpdate(Sender: TObject);
    procedure ArrangeMidPanel;
    procedure MainOffscreenPaint;
    procedure MiniMapPaint;
    procedure PaintAll;
    procedure PaintAllMaps;
    procedure CopyMiniToPanel;
    procedure PanelPaint;
    procedure FocusNextUnit(Dir: Integer = 1);
    procedure NextUnit(NearLoc: Integer; AutoTurn: Boolean);
    procedure Scroll(dx, dy: Integer);
    procedure SetMapPos(Loc: Integer; MapPos: TPoint);
    procedure Centre(Loc: Integer);
    procedure SetTroopLoc(Loc: Integer);
    procedure ProcessRect(x0, y0, nx, ny, Options: Integer);
    procedure PaintLoc(Loc: Integer; Radius: Integer = 0);
    procedure PaintLoc_BeforeMove(FromLoc: Integer);
    procedure PaintLocTemp(Loc: Integer; Style: TPaintLocTempStyle = pltsNormal);
    procedure PaintBufferToScreen(xMap, yMap, width, height: Integer);
    procedure PaintDestination;
    procedure SetUnFocus(uix: Integer);
    function MoveUnit(dx, dy: Integer; Options: Integer = 0): Integer;
    procedure MoveToLoc(Loc: Integer; CheckSuicide: Boolean);
    procedure MoveOnScreen(ShowMove: TShowMove; Step0, Step1, nStep: Integer;
      Restore: Boolean = True);
    procedure FocusOnLoc(Loc: Integer; Options: Integer = 0);
    function EndTurn(WasSkipped: Boolean = False): Boolean;
    procedure EndNego;
    function IsPanelPixel(X, Y: Integer): Boolean;
    procedure InitPopup(Popup: TPopupMenu);
    procedure SetMapOptions;
    procedure CheckMovieSpeedBtnState;
    procedure CheckTerrainBtnVisible;
    procedure RememberPeaceViolation;
    procedure SetDebugMap(P: Integer);
    procedure SetViewpoint(P: Integer);
    function LocationOfScreenPixel(X, Y: Integer): Integer;
    function GetCenterLoc: Integer;
    procedure SetTileSizeCenter(TileSize: TTileSize);
    procedure SetTileSize(TileSize: TTileSize; Loc: Integer; MapPos: TPoint);
    procedure RectInvalidate(Left, Top, Rigth, Bottom: Integer);
    procedure ShowEnemyShipChange(ShowShipChange: TShowShipChange);
    procedure SmartRectInvalidate(Left, Top, Rigth, Bottom: Integer);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure OnScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure OnEOT(var Msg: TMessage); message WM_EOT;
    procedure SoundPreload(Check: TSoundBlocks);
    procedure UpdateKeyShortcuts;
    procedure SetFullScreen(Active: Boolean);
    procedure PaintZoomedTile(dst: TBitmap; X, Y, Loc: Integer);
  public
    UsedOffscreenWidth: Integer;
    UsedOffscreenHeight: Integer;
    Offscreen: TBitmap;
    OffscreenUser: TForm;
    procedure Client(Command, NewPlayer: Integer; var Data);
    procedure SetAIName(P: Integer; Name: string);
    function ZoomToCity(Loc: Integer; NextUnitOnClose: Boolean = False;
      ShowEvent: Integer = 0): Boolean;
    procedure CityClosed(Activateuix: Integer; StepFocus: Boolean = False;
      SelectFocus: Boolean = False);
    function DipCall(Command: Integer): Integer;
    function OfferCall(var Offer: TOffer): Integer;
    procedure UpdateViews(UpdateCityScreen: Boolean = False);
    function ContactRefused(P: Integer; Item: String): Boolean;
  end;

var
  MainScreen: TMainScreen;

type

  { TTribeInfo }

  TTribeInfo = record
    trix: Integer;
    FileName: ShortString;
    function GetCommandDataSize: Byte;
  end;

  { TCityNameInfo }

  TCityNameInfo = record
    ID: Integer;
    NewName: ShortString;
    function GetCommandDataSize: Byte;
  end;

  { TModelNameInfo }

  TModelNameInfo = record
    mix: Integer;
    NewName: ShortString;
    function GetCommandDataSize: Byte;
  end;

  TPriceSet = set of $00 .. $FF;
  TFormAction = (faClose, faEnable, faDisable, faUpdate, faSmartUpdateContent);

const
  crImpDrag = 2;
  crFlatHand = 3;

  xxu = 32;
  yyu = 24; // half of unit slot size x/y
  yyu_anchor = 32;
  xxc = 32;
  yyc = 16; // 1/2 of city slot size in x, 1/2 of ground tile size in y (=1/3 of slot)

  // layout
  TopBarHeight = 41;
  PanelHeight = 168;
  MidPanelHeight = 120;
  // TopBarHeight+MidPanelHeight should be same as BaseWin.yUnused
  MapCenterUp = (MidPanelHeight - TopBarHeight) div 2;

  nCityType = 4;

  { client exclusive commands: }
  cSetTribe = $9000;
  cSetNewModelPicture = $9100;
  cSetModelName = $9110;
  cSetModelPicture = $9120;
  cSetSlaveIndex = $9131;
  cSetCityName = $9200;

  // city status flags
  csTypeMask = $0007;
  csToldDelay = $0008;
  csResourceWeightsMask = $00F0;
  csToldBombard = $0100;

  { unit status flags }
  usStay = $01;
  usWaiting = $02;
  usGoto = $04;
  usEnhance = $08;
  usRecover = $10;
  usToldNoReturn = $100;
  usPersistent = usStay or usGoto or usEnhance or usRecover or
    Integer($FFFF0000);

  { model status flags }
  msObsolete = $1;
  msAllowConscripts = $2;

  { additional city happened flags }
  chTypeDel = $8000;
  chAllImpsMade = $4000;

  adNone = $801;
  adFar = $802;
  adNexus = $803;

  SpecialModelPictureCode: array [0 .. nSpecialModel - 1] of Integer = (10,
    11, 40, 41, 21, 30, { 50,51, } 64, 74, { 71, } 73);

  pixSlaves = 0;
  pixNoSlaves = 1; // index of slaves in StdUnits

  // icons.bmp properties
  xSizeSmall = 36;
  ySizeSmall = 20;
  SystemIconLines = 2;
  // lines of system icons in icons.bmp before improvements

  nCityEventPriority = 16;
  CityEventPriority: array [0 .. nCityEventPriority - 1] of Integer =
    (chDisorder, chImprovementLost, chUnitLost, chAllImpsMade, chProduction,
    chOldWonder, chNoSettlerProd, chPopDecrease, chProductionSabotaged,
    chNoGrowthWarning, chPollution, chTypeDel, chFounded, chSiege,
    chAfterCapture, chPopIncrease);

  CityEventSoundItem: array [0 .. 15] of string = ('CITY_DISORDER', '',
    'CITY_POPPLUS', 'CITY_POPMINUS', 'CITY_UNITLOST', 'CITY_IMPLOST',
    'CITY_SABOTAGE', 'CITY_GROWTHNEEDSIMP', 'CITY_POLLUTION', 'CITY_SIEGE',
    'CITY_WONDEREX', 'CITY_EMDELAY', 'CITY_FOUNDED', 'CITY_FOUNDED', '',
    'CITY_INVALIDTYPE');

type
  TPersistentData = record
    FarTech: Integer;
    ToldAge: Integer;
    ToldModels: Integer;
    ToldAlive: Integer;
    ToldContact: Integer;
    ToldOwnCredibility: Integer;
    ColdWarStart: Integer;
    PeaceEvaHappened: Integer;
    EnhancementJobs: TEnhancementJobs;
    ImpOrder: array [0 .. nCityType - 1] of TImpOrder;
    ToldWonders: array [0 .. nWonder - 1] of TWonderInfo;
    ToldTech: array [0 .. nAdv - 1] of ShortInt;
  end;

  TDipMem = record
    pContact: Integer;
    SentCommand: Integer;
    FormerTreaty: Integer;
    SentOffer: TOffer;
    DeliveredPrices: TPriceSet;
    ReceivedPrices: TPriceSet;
  end;

  TCurrentMoveInfo = record
    AfterMovePaintRadius: Integer;
    AfterAttackExpeller: Integer;
    DoShow: Boolean;
    IsAlly: Boolean;
  end;

var
  MyData: ^TPersistentData;
  AdvIcon: array [0 .. nAdv - 1] of Integer;
  { icons displayed with the technologies }
  GameMode: Integer;
  ClientMode: Integer;
  Age: Integer;
  UnFocus: Integer;
  OptionChecked: TSaveOptions;
  MapOptionChecked: TMapOptions;
  nLostArmy: Integer;
  ScienceSum: Integer;
  TaxSum: Integer;
  SoundPreloadDone: TSoundBlocks;
  MarkCityLoc: Integer;
  MovieSpeed: Integer;
  CityRepMask: Cardinal;
  ReceivedOffer: TOffer;
  Buffer: TBitmap;
  SmallImp: TBitmap;
  BlinkON: Boolean;
  DestinationMarkON: Boolean;
  StartRunning: Boolean;
  StayOnTop_Ensured: Boolean;
  Supervising: Boolean;
  UnusedTribeFiles: TStringList;
  TribeNames: TStringList;
  TribeOriginal: array [0 .. nPl - 1] of Boolean;
  LostArmy: array [0 .. nPl * nMmax - 1] of Integer;
  DipMem: array [0 .. nPl - 1] of TDipMem;

function CityEventName(I: Integer): string;
function RoughCredibility(Credibility: Integer): Integer;

function InitEnemyModel(emix: Integer): Boolean;
procedure InitAllEnemyModels;
procedure InitMyModel(mix: Integer; final: Boolean);

procedure ImpImage(ca: TCanvas; X, Y, iix: Integer; Government: Integer = -1;
  IsControl: Boolean = False);
procedure HelpOnTerrain(Loc: Integer; NewMode: TWindowMode);
function AlignUp(Value, Alignment: Integer): Integer;


implementation

uses
  Directories, CityScreen, Draft, MessgEx, Select, CityType, Help,
  UnitStat, Log, Diagram, NatStat, Wonders, Enhance, Nego, UPixelPointer, Sound,
  Battle, Rates, TechTree, Registry, Global, UKeyBindings, CmdList;

{$R *.lfm}

const
  lxmax_xxx = 130;
  LeftPanelWidth = 70;
  overlap = PanelHeight - MidPanelHeight;
  yTroop = PanelHeight - 83;
  xPalace = 66;
  yPalace = 24; // 120;
{  xAdvisor = 108;
  yAdvisor = 48;}
  xUnitText = 80;
  BlinkOnTime = 12;
  BlinkOffTime = 6;
  MoveTime = 300; // {time for moving a unit in ms}
  WaitAfterShowMove = 32;
  FastScrolling = False; // causes problems with overlapping windows

  nBrushTypes = 26;
  BrushTypes: array [0 .. nBrushTypes - 1] of Cardinal = (fPrefStartPos,
    fStartPos, fShore, fGrass, fTundra, fPrairie, fDesert, fSwamp, fForest,
    fHills, fMountains, fArctic, fDeadLands, fDeadLands or fCobalt,
    fDeadLands or fUranium, fDeadLands or fMercury, fRiver, fRoad, fRR, fCanal,
    tiIrrigation, tiFarm, tiMine, fPoll, tiFort, tiBase);

  // MoveUnit options:
  muAutoNoWait = $0001;
  muAutoNext = $0002;
  muNoSuicideCheck = $0004;

  // ProcessRect options:
  prPaint = $0001;
  prAutoBounds = $0002;
  prInvalidate = $0004;

  // FocusOnLoc options:
  flRepaintPanel = $0001;
  flImmUpdate = $0002;

var
  Jump: array [0 .. nPl - 1] of Integer;
  pTurn: Integer;
  pLogo: Integer;
  UnStartLoc: Integer;
  ToldSlavery: Integer;
  SmallScreen: Boolean;
  GameOK: Boolean;
  MapValid: Boolean;
  Skipped: Boolean;
  Idle: Boolean;

  SaveOption: array of Integer;
  CurrentMoveInfo: TCurrentMoveInfo;

function CityEventName(I: Integer): string;
begin
  if I = 14 then // chAllImpsMade
    if not Phrases2FallenBackToEnglish then
      Result := Phrases2.Lookup('CITYEVENT_ALLIMPSMADE')
    else
      Result := Phrases.Lookup('CITYEVENTS', 1)
  else
    Result := Phrases.Lookup('CITYEVENTS', I);
end;

procedure InitSmallImp;
const
  Cut = 4;
  Sharpen = 80;
type
  TBuffer = array [0 .. 99999, 0 .. 2] of Integer;
var
  Sum, Cnt, dx, dy, nx, ny, ix, iy, ir, X, Y, C, ch: Integer;
  xdivider, ydivider: Integer;
  Resampled: ^TBuffer;
  PixelPtr: TPixelPointer;
begin
  nx := BigImp.Width div xSizeBig * xSizeSmall;
  ny := BigImp.Height div ySizeBig * ySizeSmall;

  // resample icons
  GetMem(Resampled, nx * ny * 12);
  FillChar(Resampled^, nx * ny * 12, 0);
  BigImp.BeginUpdate;
  for ix := 0 to BigImp.Width div xSizeBig - 1 do
    for iy := 0 to BigImp.Height div ySizeBig - 1 do begin
      PixelPtr := PixelPointer(BigImp, ScaleToNative(ix * xSizeBig),
        ScaleToNative(Cut + iy * ySizeBig));
      for Y := 0 to ScaleToNative(ySizeBig - 2 * Cut) - 1 do begin
        ydivider := (ScaleFromNative(Y) * ySizeSmall div (ySizeBig - 2 * Cut) + 1) *
          (ySizeBig - 2 * Cut) - ScaleFromNative(Y) * ySizeSmall;
        if ydivider > ySizeSmall then
          ydivider := ySizeSmall;
        for X := 0 to ScaleToNative(xSizeBig) - 1 do begin
          ir := ix * xSizeSmall + iy * nx * ySizeSmall + ScaleFromNative(X) *
            xSizeSmall div xSizeBig + ScaleFromNative(Y) *
            ySizeSmall div (ySizeBig - 2 * Cut) * nx;
          xdivider := (ScaleFromNative(X) * xSizeSmall div xSizeBig + 1) *
            xSizeBig - ScaleFromNative(X) * xSizeSmall;
          if xdivider > xSizeSmall then
            xdivider := xSizeSmall;
          for ch := 0 to 2 do begin
            C := PixelPtr.Pixel^.Planes[ch];
            Inc(Resampled[ir, ch], C * xdivider * ydivider);
            if xdivider < xSizeSmall then
              Inc(Resampled[ir + 1, ch], C * (xSizeSmall - xdivider) *
                ydivider);
            if ydivider < ySizeSmall then
              Inc(Resampled[ir + nx, ch],
                C * xdivider * (ySizeSmall - ydivider));
            if (xdivider < xSizeSmall) and (ydivider < ySizeSmall) then
              Inc(Resampled[ir + nx + 1, ch], C * (xSizeSmall - xdivider) *
                (ySizeSmall - ydivider));
          end;
          PixelPtr.NextPixel;
        end;
        PixelPtr.NextLine;
      end;
    end;
  BigImp.EndUpdate;

  // Sharpen Resampled icons
  SmallImp.SetSize(nx, ny);
  SmallImp.BeginUpdate;
  PixelPtr := PixelPointer(SmallImp);
  for Y := 0 to ScaleToNative(ny) - 1 do begin
    for X := 0 to ScaleToNative(nx) - 1 do begin
      for ch := 0 to 2 do begin
        Sum := 0;
        Cnt := 0;
        for dy := -1 to 1 do
          if ((dy >= 0) or (ScaleFromNative(Y) mod ySizeSmall > 0)) and
            ((dy <= 0) or (ScaleFromNative(Y) mod ySizeSmall < ySizeSmall - 1)) then
            for dx := -1 to 1 do
              if ((dx >= 0) or (ScaleFromNative(X) mod xSizeSmall > 0)) and
                ((dx <= 0) or (ScaleFromNative(X) mod xSizeSmall < xSizeSmall - 1)) then
              begin
                Inc(Sum, Resampled[ScaleFromNative(X) + dx + nx * (ScaleFromNative(Y) + dy), ch]);
                Inc(Cnt);
              end;
        Sum := ((Cnt * Sharpen + 800) * Resampled[ScaleFromNative(X) + nx * ScaleFromNative(Y), ch] - Sum *
          Sharpen) div (800 * xSizeBig * (ySizeBig - 2 * Cut));
        if Sum < 0 then Sum := 0;
        if Sum > 255 then Sum := 255;
        PixelPtr.Pixel^.Planes[ch] := Sum;
      end;
      PixelPtr.NextPixel;
    end;
    PixelPtr.NextLine;
  end;
  SmallImp.EndUpdate;
  FreeMem(Resampled);
end;

procedure ImpImage(ca: TCanvas; X, Y, iix: Integer; Government: Integer;
  IsControl: Boolean);
begin
  if Government < 0 then
    Government := MyRO.Government;
  if (iix = imPalace) and (Government <> gAnarchy) then
    iix := Government - 8;
  FrameImage(ca, BigImp, X, Y, xSizeBig, ySizeBig, (iix + SystemIconLines * 7)
    mod 7 * xSizeBig, (iix + SystemIconLines * 7) div 7 * ySizeBig, IsControl);
end;

procedure HelpOnTerrain(Loc: Integer; NewMode: TWindowMode);
begin
  if MyMap[Loc] and fDeadLands <> 0 then
    HelpDlg.ShowNewContent(NewMode, hkTer, 3 * 12)
  else if (MyMap[Loc] and fTerrain = fForest) and IsJungle(Loc div G.lx) then
    HelpDlg.ShowNewContent(NewMode, hkTer,
      fJungle + (MyMap[Loc] shr 5 and 3) * 12)
  else
    HelpDlg.ShowNewContent(NewMode, hkTer, MyMap[Loc] and fTerrain +
      (MyMap[Loc] shr 5 and 3) * 12);
end;

function AlignUp(Value, Alignment: Integer): Integer;
begin
  Result := Value or (Alignment - 1);
end;

{ *** tribe management procedures *** }

function RoughCredibility(Credibility: Integer): Integer;
begin
  case Credibility of
    0 .. 69:
      Result := 0;
    70 .. 89:
      Result := 1;
    90 .. 99:
      Result := 2;
    100:
      Result := 3;
  end;
end;

procedure ChooseModelPicture(P, mix, Code, Hash, Turn: Integer;
  ForceNew, final: Boolean);
var
  I: Integer;
  Picture: TModelPictureInfo;
  IsNew: Boolean;
begin
  Picture.trix := P;
  Picture.mix := mix;
  if Code = 74 then
  begin // use correct pictures for slaves
    if Tribe[P].mixSlaves < 0 then
      if not TribeOriginal[P] then
        Tribe[P].mixSlaves := mix
      else
      begin
        I := mix + P shl 16;
        Server(cSetSlaveIndex, 0, 0, I);
      end;
    if ToldSlavery = 1 then
      Picture.pix := pixSlaves
    else
      Picture.pix := pixNoSlaves;
    Picture.Hash := 0;
    Picture.GrName := 'StdUnits.png';
    IsNew := True;
  end
  else
  begin
    Picture.Hash := Hash;
    IsNew := Tribe[P].ChooseModelPicture(Picture, Code, Turn, ForceNew);
  end;
  if final then
    if not TribeOriginal[P] then
      Tribe[P].SetModelPicture(Picture, IsNew)
    else if IsNew then
      Server(CommandWithData(cSetNewModelPicture, Picture.GetCommandDataSize),
        0, 0, Picture)
    else
      Server(CommandWithData(cSetModelPicture, Picture.GetCommandDataSize),
        0, 0, Picture)
  else
    with Tribe[P].ModelPicture[mix] do
    begin
      HGr := LoadGraphicSet(Picture.GrName);
      pix := Picture.pix;
    end;
end;

function InitEnemyModel(emix: Integer): Boolean;
begin
  if GameMode = cMovie then
  begin
    Result := False;
    Exit;
  end;
  with MyRO.EnemyModel[emix] do
    ChooseModelPicture(Owner, mix, ModelCode(MyRO.EnemyModel[emix]),
      ModelHash(MyRO.EnemyModel[emix]), MyRO.Turn, False, True);
  Result := True;
end;

procedure InitAllEnemyModels;
var
  emix: Integer;
begin
  for emix := 0 to MyRO.nEnemyModel - 1 do
    with MyRO.EnemyModel[emix] do
      if not Assigned(Tribe[Owner].ModelPicture[mix].HGr) then
        InitEnemyModel(emix);
end;

procedure InitMyModel(mix: Integer; final: Boolean);
var
  mi: TModelInfo;
begin
  if (GameMode = cMovie) and (MyModel[mix].Kind < $08) then
    Exit;
  // don't exit for special units because cSetModelPicture comes after TellNewModels
  MakeModelInfo(Me, mix, MyModel[mix], mi);
  ChooseModelPicture(Me, mix, ModelCode(mi), ModelHash(mi), MyRO.Turn,
    False, final);
end;

function AttackSound(Code: Integer): string;
begin
  Result := 'ATTACK_' + char(48 + Code div 100 mod 10) +
    char(48 + Code div 10 mod 10) + char(48 + Code mod 10);
end;

procedure CheckToldNoReturn(uix: Integer);
// check whether aircraft survived low-fuel warning
begin
  Assert(not Supervising);
  with MyUn[uix] do
    if (Status and usToldNoReturn <> 0) and
      ((MyMap[Loc] and fCity <> 0) or (MyMap[Loc] and fTerImp = tiBase) or
      (Master >= 0)) then
      Status := Status and not usToldNoReturn;
end;

function CreateTribe(P: Integer; FileName: string; Original: Boolean): Boolean;
begin
  FileName := LocalizedFilePath('Tribes' + DirectorySeparator + FileName +
    CevoTribeExt);
  if not FileExists(FileName) then
  begin
    Result := False;
    Exit;
  end;

  TribeOriginal[P] := Original;
  Tribe[P] := TTribe.Create(FileName);
  with Tribe[P] do
  begin
    if (GameMode = cNewGame) or not Original then
    begin
      Term.ChooseModelPicture(P, 0, 010, 1, 0, True, True);
      Term.ChooseModelPicture(P, 1, 040, 1, 0, True, True);
      Term.ChooseModelPicture(P, 2, 041, 1, 0, True, True);
      Term.ChooseModelPicture(P, -1, 017, 1, 0, True, True);
    end;
    DipMem[P].pContact := -1;
  end;
  Result := True;
end;

procedure TellNewContacts;
var
  p1: Integer;
begin
  if not Supervising then
    for p1 := 0 to nPl - 1 do
      if (p1 <> Me) and (1 shl p1 and MyData.ToldContact = 0) and
        (1 shl p1 and MyRO.Alive <> 0) and (MyRO.Treaty[p1] > trNoContact) then
      begin
        TribeMessage(p1, Tribe[p1].TPhrase('FRNEWNATION'), '');
        MyData.ToldContact := MyData.ToldContact or (1 shl p1);
      end;
end;

procedure TellNewModels;
var
  mix: Integer;
  ModelNameInfo: TModelNameInfo;
begin
  if Supervising then
    Exit;
  with Tribe[Me] do
    while MyData.ToldModels < MyRO.nModel do
    begin { new Unit class available }
      if Assigned(ModelPicture[MyData.ToldModels].HGr) and
        (MyModel[MyData.ToldModels].Kind <> mkSelfDeveloped) then
      begin // save picture of DevModel
        ModelPicture[MyData.ToldModels + 1] := ModelPicture[MyData.ToldModels];
        ModelName[MyData.ToldModels + 1] := ModelName[MyData.ToldModels];
        ModelPicture[MyData.ToldModels].HGr := nil;
      end;
      if not Assigned(ModelPicture[MyData.ToldModels].HGr) then
        InitMyModel(MyData.ToldModels, True);
      { only run if no researched model }
      with MessgExDlg do
      begin
        { MakeModelInfo(me,MyData.ToldModels,MyModel[MyData.ToldModels],mi);
          if mi.Attack=0 then OpenSound:='MSG_DEFAULT'
          else OpenSound:=AttackSound(ModelCode(mi)); }
        if MyModel[MyData.ToldModels].Kind = mkSelfDeveloped then
          OpenSound := 'NEWMODEL_' + char(48 + Age);
        MessgText := Phrases.Lookup('MODELAVAILABLE');
        if GameMode = cMovie then
        begin
          Kind := mkOkHelp; // doesn't matter
          MessgText := MessgText + '\' + ModelName[MyData.ToldModels];
        end
        else
        begin
          Kind := mkModel;
          EInput.Text := ModelName[MyData.ToldModels];
        end;
        IconKind := mikModel;
        IconIndex := MyData.ToldModels;
        ShowModal;
        if (EInput.Text <> '') and (EInput.Text <> ModelName[MyData.ToldModels])
        then
        begin // user renamed model
          ModelNameInfo.mix := MyData.ToldModels;
          ModelNameInfo.NewName := EInput.Text;
          if ModelNameInfo.GetCommandDataSize > CommandDataMaxSize then
            Delete(ModelNameInfo.NewName, Length(ModelNameInfo.NewName) -
             (ModelNameInfo.GetCommandDataSize - 1 - CommandDataMaxSize), MaxInt);
          Server(CommandWithData(cSetModelName, ModelNameInfo.GetCommandDataSize),
            Me, 0, ModelNameInfo);
        end;
      end;
      if MyModel[MyData.ToldModels].Kind = mkSettler then
      begin // engineers make settlers obsolete
        for mix := 0 to MyData.ToldModels - 1 do
          if MyModel[mix].Kind = mkSettler then
            MyModel[mix].Status := MyModel[mix].Status or msObsolete;
      end;
      Inc(MyData.ToldModels);
    end;
end;

{ TTribeInfo }

function TTribeInfo.GetCommandDataSize: Byte;
begin
  Result := SizeOf(trix) + 1 + Length(FileName)
end;

{ TModelNameInfo }

function TModelNameInfo.GetCommandDataSize: Byte;
begin
  Result := SizeOf(mix) + 1 + Length(NewName);
end;

{ TCityNameInfo }

function TCityNameInfo.GetCommandDataSize: Byte;
begin
  Result := SizeOf(ID) + 1 + Length(NewName);
end;

procedure TMainScreen.PaintZoomedTile(dst: TBitmap; X, Y, Loc: Integer);

  procedure TSprite(xDst, yDst, xSrc, ySrc: Integer);
  begin
    with NoMapPanel do
      Sprite(dst, HGrTerrain, X + xDst, Y + yDst, xxt * 2, yyt * 3,
        1 + xSrc * (xxt * 2 + 1), 1 + ySrc * (yyt * 3 + 1));
  end;

  procedure TSprite4(xSrc, ySrc: Integer);
  begin
    with NoMapPanel do begin
      Sprite(dst, HGrTerrain, X + xxt, Y + yyt + 2, xxt * 2, yyt * 2 - 2,
        1 + xSrc * (xxt * 2 + 1), 3 + yyt + ySrc * (yyt * 3 + 1));
      Sprite(dst, HGrTerrain, X + 4, Y + 2 * yyt, xxt * 2 - 4, yyt * 2,
        5 + xSrc * (xxt * 2 + 1), 1 + yyt + ySrc * (yyt * 3 + 1));
      Sprite(dst, HGrTerrain, X + xxt * 2, Y + 2 * yyt, xxt * 2 - 4, yyt * 2,
        1 + xSrc * (xxt * 2 + 1), 1 + yyt + ySrc * (yyt * 3 + 1));
      Sprite(dst, HGrTerrain, X + xxt, Y + yyt * 3, xxt * 2, yyt * 2 - 2,
        1 + xSrc * (xxt * 2 + 1), 1 + yyt + ySrc * (yyt * 3 + 1));
    end;
  end;

var
  cix, ySrc, Tile: Integer;
begin
  with NoMapPanel do begin
    Tile := MyMap[Loc];
    if Tile and fCity <> 0 then
    begin
      if MyRO.Tech[adRailroad] >= tsApplicable then
        Tile := Tile or fRR
      else
        Tile := Tile or fRoad;
      if Tile and fOwned <> 0 then
      begin
        cix := MyRO.nCity - 1;
        while (cix >= 0) and (MyCity[cix].Loc <> Loc) do
          Dec(cix);
        Assert(cix >= 0);
        if MyCity[cix].Built[imSupermarket] > 0 then
          Tile := Tile or tiFarm
        else
          Tile := Tile or tiIrrigation;
      end
      else Tile := Tile or tiIrrigation;
    end;

    if Tile and fTerrain >= fForest then
      TSprite4(2, 2)
    else
      TSprite4(Tile and fTerrain, 0);
    if Tile and fTerrain >= fForest then
    begin
      if (Tile and fTerrain = fForest) and IsJungle(Loc div G.lx) then
        ySrc := 18
      else
        ySrc := 3 + 2 * (Tile and fTerrain - fForest);
      TSprite(xxt, 0, 6, ySrc);
      TSprite(0, yyt, 3, ySrc);
      TSprite((xxt * 2), yyt, 4, ySrc + 1);
      TSprite(xxt, (yyt * 2), 1, ySrc + 1);
    end;

    // irrigation
    case Tile and fTerImp of
      tiIrrigation: begin
        TSprite(xxt, 0, 0, 12);
        TSprite(xxt * 2, yyt, 0, 12);
      end;
      tiFarm: begin
        TSprite(xxt, 0, 1, 12);
        TSprite(xxt * 2, yyt, 1, 12);
      end;
    end;

    // river/canal/road/railroad
    if Tile and fRiver <> 0 then begin
      TSprite(0, yyt, 2, 14);
      TSprite(xxt, (yyt * 2), 2, 14);
    end;
    if Tile and fCanal <> 0 then begin
      TSprite(xxt, 0, 7, 11);
      TSprite(xxt, 0, 3, 11);
      TSprite(xxt * 2, yyt, 7, 11);
      TSprite(xxt * 2, yyt, 3, 11);
    end;
    if Tile and fRR <> 0 then begin
      TSprite((xxt * 2), yyt, 1, 10);
      TSprite((xxt * 2), yyt, 5, 10);
      TSprite(xxt, (yyt * 2), 1, 10);
      TSprite(xxt, (yyt * 2), 5, 10);
    end
    else if Tile and fRoad <> 0 then begin
      TSprite((xxt * 2), yyt, 8, 9);
      TSprite((xxt * 2), yyt, 5, 9);
      TSprite(xxt, (yyt * 2), 1, 9);
      TSprite(xxt, (yyt * 2), 5, 9);
    end;

    if Tile and fPoll <> 0 then
      TSprite(xxt, (yyt * 2), 6, 12);

    // special
    if Tile and (fTerrain or fSpecial) = fGrass or fSpecial1 then TSprite4(2, 1)
    else if Tile and fSpecial <> 0 then
      if Tile and fTerrain < fForest then
        TSprite(0, yyt, Tile and fTerrain, Tile and fSpecial shr 5)
      else if (Tile and fTerrain = fForest) and IsJungle(Loc div G.lx) then
        TSprite(0, yyt, 8, 17 + Tile and fSpecial shr 5)
      else
        TSprite(0, yyt, 8, 2 + (Tile and fTerrain - fForest) * 2 + Tile and
          fSpecial shr 5)
    else if Tile and fDeadLands <> 0 then begin
      TSprite4(6, 2);
      TSprite(xxt, yyt, 8, 12 + Tile shr 25 and 3);
    end;

    // other improvements
    case Tile and fTerImp of
      tiMine: TSprite(xxt, 0, 2, 12);
      tiFort: begin
        TSprite(xxt, 0, 7, 12);
        TSprite(xxt, 0, 3, 12);
      end;
     tiBase: TSprite(xxt, 0, 4, 12);
    end;
  end;
end;

function ChooseResearch: Boolean;
var
  ChosenResearch: Integer;
begin
  if (MyData.FarTech <> adNone) and (MyRO.Tech[MyData.FarTech] >= tsApplicable)
  then
    MyData.FarTech := adNone;
  repeat
    { research complete -- select new }
    repeat
      ModalSelectDlg.ShowNewContent(wmModal, kAdvance);
      if ModalSelectDlg.Result < 0 then
      begin
        Result := False;
        Exit;
      end;
      ChosenResearch := ModalSelectDlg.Result;
      if ChosenResearch = adMilitary then
      begin
        DraftDlg.ShowNewContent(wmModal);
        if DraftDlg.ModalResult <> mrOK then
          Tribe[Me].ModelPicture[MyRO.nModel].HGr := nil;
      end;
    until (ChosenResearch <> adMilitary) or (DraftDlg.ModalResult = mrOK);

    if ChosenResearch = adMilitary then
      InitMyModel(MyRO.nModel, True)
    else if ChosenResearch = adFar then
    begin
      ModalSelectDlg.ShowNewContent(wmModal, kFarAdvance);
      if ModalSelectDlg.Result >= 0 then
        if (ModalSelectDlg.Result = adNone) or
          (Server(sSetResearch - sExecute, Me, ModalSelectDlg.Result, nil^) <
          rExecuted) then
          MyData.FarTech := ModalSelectDlg.Result
        else
        begin
          ChosenResearch := ModalSelectDlg.Result;
          // can be researched immediately
          MyData.FarTech := adNone;
        end;
    end;
  until ChosenResearch <> adFar;
  if ChosenResearch = adNexus then
    MyData.FarTech := adNexus
  else
    Server(sSetResearch, Me, ChosenResearch, nil^);
  ListDlg.TechChange;
  Result := True;
end;

procedure ApplyToVisibleForms(FormAction: TFormAction);
var
  I: Integer;
  Form: TForm;
begin
  for I := 0 to Screen.FormCount - 1 do begin
    Form := Screen.Forms[I];
    if Form.Visible and (Form is TBufferedDrawDlg) then begin
      case FormAction of
        faClose: Form.Close;
        faEnable: Form.Enabled := True;
        faDisable: Form.Enabled := False;
        faUpdate: begin
          if @Form.OnShow <> nil then Form.OnShow(nil);
            Form.Invalidate;
            Form.Update;
        end;
        faSmartUpdateContent: TBufferedDrawDlg(Form).SmartUpdateContent(False);
      end;
    end;
  end;
end;

(* ** client function handling ** *)

function TMainScreen.DipCall(Command: Integer): Integer;
var
  I: Integer;
  IsTreatyDeal: Boolean;
begin
  Result := Server(Command, Me, 0, nil^);
  if Result >= rExecuted then
  begin
    if Command and $FF0F = scContact then
    begin
      DipMem[Me].pContact := Command shr 4 and $F;
      NegoDlg.Initiate;
      DipMem[Me].DeliveredPrices := [];
      DipMem[Me].ReceivedPrices := [];
    end;

    DipMem[Me].SentCommand := Command;
    DipMem[Me].FormerTreaty := MyRO.Treaty[DipMem[Me].pContact];
    if Command = scDipCancelTreaty then
      Play('CANCELTREATY')
    else if Command = scDipAccept then
    begin // remember delivered and received prices
      for I := 0 to ReceivedOffer.nDeliver - 1 do
        Include(DipMem[Me].ReceivedPrices, ReceivedOffer.Price[I] shr 24);
      for I := 0 to ReceivedOffer.nCost - 1 do
        Include(DipMem[Me].DeliveredPrices,
          ReceivedOffer.Price[ReceivedOffer.nDeliver + I] shr 24);
      IsTreatyDeal := False;
      for I := 0 to ReceivedOffer.nDeliver + ReceivedOffer.nCost - 1 do
        if ReceivedOffer.Price[I] and opMask = opTreaty then
          IsTreatyDeal := True;
      if IsTreatyDeal then
        Play('NEWTREATY')
      else
        Play('ACCEPTOFFER');
    end;
    CityDlg.CloseAction := None;
    if G.RO[DipMem[Me].pContact] <> nil then
    begin // close windows for next player
      ApplyToVisibleForms(faClose);
    end
    else
    begin
      if CityDlg.Visible then
        CityDlg.Close;
      if UnitStatDlg.Visible then
        UnitStatDlg.Close;
    end;
  end;
end;

function TMainScreen.OfferCall(var Offer: TOffer): Integer;
begin
  Result := Server(scDipOffer, Me, 0, Offer);
  if Result >= rExecuted then
  begin
    DipMem[Me].SentCommand := scDipOffer;
    DipMem[Me].FormerTreaty := MyRO.Treaty[DipMem[Me].pContact];
    DipMem[Me].SentOffer := Offer;
    CityDlg.CloseAction := None;
    if G.RO[DipMem[Me].pContact] <> nil then
    begin // close windows for next player
      ApplyToVisibleForms(faClose);
    end
    else
    begin
      if CityDlg.Visible then
        CityDlg.Close;
      if UnitStatDlg.Visible then
        UnitStatDlg.Close;
    end;
  end;
end;

procedure TMainScreen.SetUnFocus(uix: Integer);
var
  Loc0: Integer;
begin
  Assert(not((uix >= 0) and Supervising));
  if uix <> UnFocus then
  begin
    DestinationMarkON := False;
    PaintDestination;
    if uix >= 0 then
      UnStartLoc := MyUn[uix].Loc;
    BlinkON := False;
    BlinkTime := -1;
    if UnFocus >= 0 then
    begin
      Loc0 := MyUn[UnFocus].Loc;
      if (uix < 0) or (Loc0 <> MyUn[uix].Loc) then
      begin
        UnFocus := -1;
        PaintLoc(Loc0);
      end;
    end;
    UnFocus := uix;
  end;
  UnitInfoBtn.Visible := UnFocus >= 0;
  UnitBtn.Visible := UnFocus >= 0;
  CheckTerrainBtnVisible;
end;

procedure TMainScreen.CheckTerrainBtnVisible;
var
  Tile: Integer;
  mox: ^TModel;
begin
  if UnFocus >= 0 then
  begin
    mox := @MyModel[MyUn[UnFocus].mix];
    Tile := MyMap[MyUn[UnFocus].Loc];
    TerrainBtn.Visible := (Tile and fCity = 0) and (MyUn[UnFocus].Master < 0)
      and ((mox.Kind = mkSettler) or (mox.Kind = mkSlaves) and
      (MyRO.Wonder[woPyramids].EffectiveOwner >= 0));
  end
  else
    TerrainBtn.Visible := False;
end;

procedure TMainScreen.CheckMovieSpeedBtnState;
begin
  if GameMode = cMovie then
  begin
    MovieSpeed1Btn.Down := MovieSpeed = 1;
    MovieSpeed1Btn.Visible := True;
    MovieSpeed2Btn.Down := MovieSpeed = 2;
    MovieSpeed2Btn.Visible := True;
    MovieSpeed3Btn.Down := MovieSpeed = 3;
    MovieSpeed3Btn.Visible := True;
    MovieSpeed4Btn.Down := MovieSpeed = 4;
    MovieSpeed4Btn.Visible := True;
  end
  else
  begin
    MovieSpeed1Btn.Visible := False;
    MovieSpeed2Btn.Visible := False;
    MovieSpeed3Btn.Visible := False;
    MovieSpeed4Btn.Visible := False;
  end;
end;

procedure TMainScreen.SetMapOptions;
begin
  MiniMap.MapOptions := MapOptionChecked;
  MapOptions := MapOptionChecked;
  if ClientMode = cEditMap then
    MapOptions := MapOptions + [moEditMode];
  if mLocCodes.Checked then
    MapOptions := MapOptions + [moLocCodes];
end;

procedure TMainScreen.UpdateViews(UpdateCityScreen: Boolean);
begin
  SumCities(TaxSum, ScienceSum);
  PanelPaint; // TopBar was enough!!!
  ListDlg.EcoChange;
  NatStatDlg.EcoChange;
  if UpdateCityScreen then
    CityDlg.SmartUpdateContent;
end;

procedure TMainScreen.SetAIName(P: Integer; Name: string);
begin
  if Name = '' then
  begin
    if AILogo[P] <> nil then
    begin
      FreeAndNil(AILogo[P]);
    end;
  end
  else
  begin
    if AILogo[P] = nil then
      AILogo[P] := TBitmap.Create;
    if not LoadGraphicFile(AILogo[P], HomeDir + Name + '.png', [gfNoError]) then
    begin
      FreeAndNil(AILogo[P]);
    end;
  end;
end;

function TMainScreen.ContactRefused(P: Integer; Item: String): Boolean;
// return whether treaty was cancelled
var
  S: string;
begin
  Assert(MyRO.Treaty[P] >= trPeace);
  S := Tribe[P].TPhrase(Item);
  if MyRO.Turn < MyRO.LastCancelTreaty[P] + CancelTreatyTurns then
  begin
    SimpleMessage(S);
    Result := False;
  end
  else
  begin
    case MyRO.Treaty[P] of
      trPeace:
        S := S + ' ' + Phrases.Lookup('FRCANCELQUERY_PEACE');
      trFriendlyContact:
        S := S + ' ' + Phrases.Lookup('FRCANCELQUERY_FRIENDLY');
      trAlliance:
        S := S + ' ' + Phrases.Lookup('FRCANCELQUERY_ALLIANCE');
    end;
    Result := SimpleQuery(mkYesNo, S, 'NEGO_REJECTED') = mrOK;
    if Result then
    begin
      Play('CANCELTREATY');
      Server(sCancelTreaty, Me, 0, nil^);
      if MyRO.Treaty[P] = trNone then
        CityOptimizer_BeginOfTurn;
      // peace treaty was cancelled -- use formerly forbidden tiles
      MapValid := False;
      PaintAllMaps;
    end;
  end;
end;

procedure TMainScreen.RememberPeaceViolation;
var
  uix, p1: Integer;
begin
  MyData.PeaceEvaHappened := 0;
  for uix := 0 to MyRO.nUn - 1 do
    with MyUn[uix] do
      if Loc >= 0 then
      begin
        p1 := MyRO.Territory[Loc];
        if (p1 <> Me) and (p1 >= 0) and
          (MyRO.Turn = MyRO.EvaStart[p1] + (PeaceEvaTurns - 1)) then
          MyData.PeaceEvaHappened := MyData.PeaceEvaHappened or (1 shl p1);
      end;
end;

procedure TMainScreen.SoundPreload(Check: TSoundBlocks);
const
  nStartBlock = 27;
  StartBlock: array [0 .. nStartBlock - 1] of string = ('INVALID', 'TURNEND',
    'DISBAND', 'CHEAT', 'MSG_DEFAULT', 'WARNING_DISORDER', 'WARNING_FAMINE',
    'WARNING_LOWSUPPORT', 'WARNING_LOWFUNDS', 'MOVE_MOUNTAIN', 'MOVE_LOAD',
    'MOVE_UNLOAD', 'MOVE_DIE', 'NOMOVE_TIME', 'NOMOVE_DOMAIN',
    'NOMOVE_DEFAULT', 'CITY_SELLIMP', 'CITY_REBUILDIMP', 'CITY_BUYPROJECT',
    'CITY_UTILIZE', 'NEWMODEL_0', 'NEWADVANCE_0', 'AGE_0', 'REVOLUTION',
    'NEWGOV', 'CITY_INVALIDTYPE', 'MSG_GAMEOVER');

  nWonderBlock = 6;
  WonderBlock: array [0 .. nWonderBlock - 1] of string = ('WONDER_BUILT',
    'WONDER_CAPTURED', 'WONDER_EXPIRED', 'WONDER_DESTROYED', 'MSG_COLDWAR',
    'NEWADVANCE_GRLIB');

  nScienceBlock = 17;
  ScienceBlock: array [0 .. nScienceBlock - 1] of string = ('MOVE_PARACHUTE',
    'MOVE_PLANESTART', 'MOVE_PLANELANDING', 'MOVE_COVERT', 'NEWMODEL_1',
    'NEWMODEL_2', 'NEWMODEL_3', 'NEWADVANCE_1', 'NEWADVANCE_2',
    'NEWADVANCE_3', 'AGE_1', 'AGE_2', 'AGE_3', 'SHIP_BUILT', 'SHIP_TRADED',
    'SHIP_CAPTURED', 'SHIP_DESTROYED');

  nContactBlock = 20;
  ContactBlock: array [0 .. nContactBlock - 1] of string = ('NEWTREATY',
    'CANCELTREATY', 'ACCEPTOFFER', 'MSG_WITHDRAW', 'MSG_BANKRUPT',
    'CONTACT_0', 'CONTACT_1', 'CONTACT_2', 'CONTACT_3', 'CONTACT_4',
    'CONTACT_5', 'CONTACT_5', 'CONTACT_6', 'NEGO_REJECTED', 'MOVE_CAPTURE',
    'MOVE_EXPEL', 'NOMOVE_TREATY', 'NOMOVE_ZOC', 'NOMOVE_SUBMARINE',
    'NOMOVE_STEALTH');

var
  I, cix, mix: Integer;
  need: Boolean;
  mi: TModelInfo;
begin
  if (sbStart in Check) and not (sbStart in SoundPreloadDone) then begin
    for I := 0 to nStartBlock - 1 do
      PreparePlay(StartBlock[I]);
    SoundPreloadDone := SoundPreloadDone + [sbStart];
  end;
  if (sbWonder in Check) and not (sbWonder in SoundPreloadDone) then begin
    need := False;
    for I := 0 to nWonder - 1 do
      if MyRO.Wonder[I].CityID <> WonderNotBuiltYet then
        need := True;
    if need then begin
      for I := 0 to nWonderBlock - 1 do
        PreparePlay(WonderBlock[I]);
      SoundPreloadDone := SoundPreloadDone + [sbWonder];
    end;
  end;
  if ((sbScience in Check) and not (sbScience in SoundPreloadDone)) and
    (MyRO.Tech[adScience] >= tsApplicable) then begin
    for I := 0 to nScienceBlock - 1 do
      PreparePlay(ScienceBlock[I]);
    SoundPreloadDone := SoundPreloadDone + [sbScience];
  end;
  if ((sbContact in Check) and not (sbContact in SoundPreloadDone)) and
    (MyRO.nEnemyModel + MyRO.nEnemyCity > 0) then begin
    for I := 0 to nContactBlock - 1 do
      PreparePlay(ContactBlock[I]);
    SoundPreloadDone := SoundPreloadDone + [sbContact];
  end;
  if sbTurn in Check then begin
    if MyRO.Happened and phShipComplete <> 0 then
      PreparePlay('MSG_YOUWIN');
    if MyData.ToldAlive <> MyRO.Alive then
      PreparePlay('MSG_EXTINCT');
    for cix := 0 to MyRO.nCity - 1 do
      with MyCity[cix] do
        if (Loc >= 0) and (Flags and CityRepMask <> 0) then
          for I := 0 to 12 do
            if 1 shl I and Flags and CityRepMask <> 0 then
              PreparePlay(CityEventSoundItem[I]);
    for mix := 0 to MyRO.nModel - 1 do
      with MyModel[mix] do
        if Attack > 0 then
        begin
          MakeModelInfo(Me, mix, MyModel[mix], mi);
          PreparePlay(AttackSound(ModelCode(mi)));
        end;
  end;
end;

procedure TMainScreen.GetTribeList;
var
  SearchRec: TSearchRec;
  Color: TColor;
  Name: string;
  ok: Boolean;
begin
  UnusedTribeFiles.Clear;
  ok := FindFirst(LocalizedFilePath('Tribes') + DirectorySeparator + '*' + CevoTribeExt,
    faArchive + faReadOnly, SearchRec) = 0;
  if not ok then
  begin
    FindClose(SearchRec);
    ok := FindFirst(LocalizedFilePath('Tribes' + DirectorySeparator + '*' + CevoTribeExt),
      faArchive + faReadOnly, SearchRec) = 0;
  end;
  if ok then
    repeat
      SearchRec.Name := Copy(SearchRec.Name, 1, Length(SearchRec.Name) - Length(CevoTribeExt));
      if GetTribeInfo(SearchRec.Name, Name, Color) then
        UnusedTribeFiles.AddObject(SearchRec.Name, TObject(Color));
    until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

function TMainScreen.ChooseUnusedTribe: Integer;
var
  I: Integer;
  J: Integer;
  ColorDistance: Integer;
  BestColorDistance: Integer;
  TestColorDistance: Integer;
  CountBest: Integer;
begin
  Assert(UnusedTribeFiles.Count > 0);
  Result := -1;
  BestColorDistance := -1;
  for J := 0 to UnusedTribeFiles.Count - 1 do
  begin
    ColorDistance := 250; // consider differences more than this infinite
    for I := 0 to nPl - 1 do
      if Tribe[I] <> nil then
      begin
        TestColorDistance := abs(Integer(UnusedTribeFiles.Objects[J])
          shr 16 and $FF - Tribe[I].Color shr 16 and $FF) +
          abs(Integer(UnusedTribeFiles.Objects[J]) shr 8 and
          $FF - Tribe[I].Color shr 8 and $FF) * 3 +
          abs(Integer(UnusedTribeFiles.Objects[J]) and
          $FF - Tribe[I].Color and $FF) * 2;
        if TestColorDistance < ColorDistance then
          ColorDistance := TestColorDistance;
      end;
    if ColorDistance > BestColorDistance then
    begin
      CountBest := 0;
      BestColorDistance := ColorDistance;
    end;
    if ColorDistance = BestColorDistance then
    begin
      Inc(CountBest);
      if DelphiRandom(CountBest) = 0 then
        Result := J;
    end;
  end;
end;

procedure TMainScreen.ShowEnemyShipChange(ShowShipChange: TShowShipChange);
var
  I, TestCost, MostCost: Integer;
  Ship1Plus, Ship2Plus: Boolean;
begin
  with ShowShipChange, MessgExDlg do
  begin
    case Reason of
      scrProduction:
        begin
          OpenSound := 'SHIP_BUILT';
          MessgText := Tribe[Ship1Owner].TPhrase('SHIPBUILT');
          IconKind := mikShip;
          IconIndex := Ship1Owner;
        end;

      scrDestruction:
        begin
          OpenSound := 'SHIP_DESTROYED';
          MessgText := Tribe[Ship1Owner].TPhrase('SHIPDESTROYED');
          IconKind := mikImp;
        end;

      scrTrade:
        begin
          OpenSound := 'SHIP_TRADED';
          Ship1Plus := False;
          Ship2Plus := False;
          for I := 0 to nShipPart - 1 do
          begin
            if Ship1Change[I] > 0 then
              Ship1Plus := True;
            if Ship2Change[I] > 0 then
              Ship2Plus := True;
          end;
          if Ship1Plus and Ship2Plus then
            MessgText := Tribe[Ship1Owner].TPhrase('SHIPBITRADE1') + ' ' +
              Tribe[Ship2Owner].TPhrase('SHIPBITRADE2')
          else if Ship1Plus then
            MessgText := Tribe[Ship1Owner].TPhrase('SHIPUNITRADE1') + ' ' +
              Tribe[Ship2Owner].TPhrase('SHIPUNITRADE2')
          else // if Ship2Plus then
            MessgText := Tribe[Ship2Owner].TPhrase('SHIPUNITRADE1') + ' ' +
              Tribe[Ship1Owner].TPhrase('SHIPUNITRADE2');
          IconKind := mikImp;
        end;

      scrCapture:
        begin
          OpenSound := 'SHIP_CAPTURED';
          MessgText := Tribe[Ship2Owner].TPhrase('SHIPCAPTURE1') + ' ' +
            Tribe[Ship1Owner].TPhrase('SHIPCAPTURE2');
          IconKind := mikShip;
          IconIndex := Ship2Owner;
        end;
    end;

    if IconKind = mikImp then
    begin
      MostCost := 0;
      for I := 0 to nShipPart - 1 do
      begin
        TestCost := abs(Ship1Change[I]) * Imp[imShipComp + I].Cost;
        if TestCost > MostCost then
        begin
          MostCost := TestCost;
          IconIndex := imShipComp + I;
        end;
      end;
    end;

    Kind := mkOk;
    ShowModal;
  end;
end;

procedure TMainScreen.InitModule;
var
  I, J, Domain: Integer;
begin
  { search icons for advances: }
  for I := 0 to nAdv - 1 do
    if I in FutureTech then
      AdvIcon[I] := 96 + I - futResearchTechnology
    else
    begin
      AdvIcon[I] := -1;
      for Domain := 0 to nDomains - 1 do
        for J := 0 to nUpgrade - 1 do
          if upgrade[Domain, J].Preq = I then
            if AdvIcon[I] >= 0 then
              AdvIcon[I] := 85
            else
              AdvIcon[I] := 86 + Domain;
      for J := 0 to nFeature - 1 do
        if Feature[J].Preq = I then
          for Domain := 0 to nDomains - 1 do
            if 1 shl Domain and Feature[J].Domains <> 0 then
              if (AdvIcon[I] >= 0) and (AdvIcon[I] <> 86 + Domain) then
                AdvIcon[I] := 85
              else
                AdvIcon[I] := 86 + Domain;
      for J := nWonder to nImp - 1 do
        if Imp[J].Preq = I then
          AdvIcon[I] := J;
      for J := nWonder to nImp - 1 do
        if (Imp[J].Preq = I) and (Imp[J].Kind <> ikCommon) then
          AdvIcon[I] := J;
      for J := 0 to nJob - 1 do
        if I = JobPreq[J] then
          AdvIcon[I] := 84;
      for J := 0 to nWonder - 1 do
        if Imp[J].Preq = I then
          AdvIcon[I] := J;
      if AdvIcon[I] < 0 then
        if AdvValue[I] < 1000 then
          AdvIcon[I] := -7
        else
          AdvIcon[I] := 24 + AdvValue[I] div 1000;
      for J := 2 to nGov - 1 do
        if GovPreq[J] = I then
          AdvIcon[I] := J - 8;
    end;
  AdvIcon[adConscription] := 86 + dGround;

  UnusedTribeFiles := tstringlist.Create;
  UnusedTribeFiles.Sorted := True;
  TribeNames := tstringlist.Create;

  IsoEngine.Init(InitEnemyModel);
  // non-default tile size is missing a file, switch to default
  MainMap.SetOutput(Offscreen);

  HGrStdUnits := LoadGraphicSet('StdUnits.png');
  SmallImp := TBitmap.Create;
  SmallImp.PixelFormat := pf24bit;
  InitSmallImp;
  SoundPreloadDone := [];
  StartRunning := False;
  StayOnTop_Ensured := False;

  sb := TPVScrollbar.Create(Self);
  sb.OnUpdate := ScrollBarUpdate;
end;

procedure TMainScreen.DoneModule;
begin
  FreeAndNil(SmallImp);
  FreeAndNil(UnusedTribeFiles);
  FreeAndNil(TribeNames);
  // AdvisorDlg.DeInit;
end;

procedure TMainScreen.InitTurn(NewPlayer: Integer);
const
  nAdvBookIcon = 16;
  AdvBookIcon: array [0 .. nAdvBookIcon - 1] of record Adv,
    Icon: Integer end = ((Adv: adPolyTheism; Icon: woZeus),
    (Adv: adBronzeWorking; Icon: woColossus), (Adv: adMapMaking;
    Icon: woLighthouse), (Adv: adPoetry; Icon: imTheater), (Adv: adMonotheism;
    Icon: woMich), (Adv: adPhilosophy; Icon: woLeo), (Adv: adTheoryOfGravity;
    Icon: woNewton), (Adv: adSteel; Icon: woEiffel), (Adv: adDemocracy;
    Icon: woLiberty), (Adv: adAutomobile; Icon: imHighways),
    (Adv: adSanitation; Icon: imSewer), (Adv: adElectronics; Icon: woHoover),
    (Adv: adNuclearFission; Icon: woManhattan), (Adv: adRecycling;
    Icon: imRecycling), (Adv: adComputers; Icon: imResLab),
    (Adv: adSpaceFlight; Icon: woMIR));
  sbAll = [sbStart, sbWonder, sbScience, sbContact, sbTurn];
var
  p1, I, ad, uix, cix, MoveOptions, MoveResult, Loc1,
    NewAgeCenterTo, Winners, NewGovAvailable, dx,
    dy: Integer;
  MoveAdviceData: TMoveAdviceData;
  Picture: TModelPictureInfo;
  S, Item, Item2: string;
  UpdatePanel, OwnWonder, ok, Stop, ShowCityList, WondersOnly,
    AllowCityScreen: Boolean;
begin
  if IsMultiPlayerGame and (NewPlayer <> Me) then
  begin
    UnitInfoBtn.Visible := False;
    UnitBtn.Visible := False;
    TerrainBtn.Visible := False;
    EOT.Visible := False;
  end;
  if IsMultiPlayerGame and (NewPlayer <> Me) and
    (G.RO[0].Happened and phShipComplete = 0) then
  begin // inter player screen
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TButtonC then
        Controls[I].Visible := False;
    Me := -1;
    MainTexture.Age := -1;
    with Panel.Canvas do
    begin
      Brush.Color := $000000;
      FillRect(Rect(0, 0, Panel.width, Panel.height));
      Brush.Style := bsClear;
    end;
    with TopBar.Canvas do
    begin
      Brush.Color := $000000;
      FillRect(Rect(0, 0, TopBar.width, TopBar.height));
      Brush.Style := bsClear;
    end;
    Invalidate;

    S := TurnToString(G.RO[0].Turn);
    if Supervising then
      SimpleMessage(Format(Phrases.Lookup('SUPERTURN'), [S]))
    else
      SimpleMessage(Format(Tribe[NewPlayer].TPhrase('TURN'), [S]));
  end;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TButtonC then
      Controls[I].Visible := True;

  ItsMeAgain(NewPlayer);
  MyData := G.RO[NewPlayer].Data;
  if not Supervising then
    SoundPreload(sbAll);
  if (Me = 0) and ((MyRO.Turn = 0) or (ClientMode = cResume)) then
    Invalidate; // colorize empty space

  if not Supervising then
  begin

    { if MyRO.Happened and phGameEnd<>0 then
      begin
      Age := 3;
      MainTexture.Age := -1;
      end
      else }
    begin
      Age := GetAge(Me);
      if MainTexture.Age <> Age then begin
        MainTexture.Age := Age;
        EOT.Invalidate; // has visible background parts in its bounds
      end;
    end;
    // age:=MyRO.Turn mod 4; //!!!
    if ClientMode = cMovieTurn then
      EOT.ButtonIndex := eotCancel
    else if ClientMode < scContact then
      EOT.ButtonIndex := eotGray
    else
      EOT.ButtonIndex := eotBackToNego;
  end
  else
  begin
    Age := 0;
    MainTexture.Age := -1;
    if ClientMode = cMovieTurn then
      EOT.ButtonIndex := eotCancel
    else
      EOT.ButtonIndex := eotBlinkOn;
  end;
  InitCityMark(MainTexture);
  CityDlg.CheckAge;
  NatStatDlg.CheckAge;
  UnitStatDlg.CheckAge;
  HelpDlg.Difficulty := G.Difficulty[Me];

  UnFocus := -1;
  MarkCityLoc := -1;
  BlinkON := False;
  BlinkTime := -1;
  Tracking := False;
  TurnComplete := False;

  if (ToldSlavery < 0) or
    ((ToldSlavery = 1) <> (MyRO.Wonder[woPyramids].EffectiveOwner >= 0)) then
  begin
    if MyRO.Wonder[woPyramids].EffectiveOwner >= 0 then
      ToldSlavery := 1
    else
      ToldSlavery := 0;
    for p1 := 0 to nPl - 1 do
      if (Tribe[p1] <> nil) and (Tribe[p1].mixSlaves >= 0) then
        with Picture do
        begin // replace unit picture
          mix := Tribe[p1].mixSlaves;
          if ToldSlavery = 1 then
            pix := pixSlaves
          else
            pix := pixNoSlaves;
          Hash := 0;
          GrName := 'StdUnits.png';
          Tribe[p1].SetModelPicture(Picture, True);
        end;
  end;

  if not Supervising and (ClientMode = cTurn) then
  begin
    for cix := 0 to MyRO.nCity - 1 do
      if (MyCity[cix].Loc >= 0) and
        ((MyRO.Turn = 0) or (MyCity[cix].Flags and chFounded <> 0)) then
        MyCity[cix].Status := MyCity[cix].Status and
          not csResourceWeightsMask or (3 shl 4);
    // new city, set to maximum growth
  end;
  if (ClientMode = cTurn) or (ClientMode = cContinue) then
    CityOptimizer_BeginOfTurn; // maybe peace was made or has ended
  SumCities(TaxSum, ScienceSum);

  if ClientMode = cMovieTurn then
  begin
    UnitInfoBtn.Visible := False;
    UnitBtn.Visible := False;
    TerrainBtn.Visible := False;
    EOT.Hint := Phrases.Lookup('BTN_STOP');
    EOT.Visible := True;
  end
  else if ClientMode < scContact then
  begin
    UnitInfoBtn.Visible := UnFocus >= 0;
    UnitBtn.Visible := UnFocus >= 0;
    CheckTerrainBtnVisible;
    TurnComplete := Supervising;
    EOT.Hint := Phrases.Lookup('BTN_ENDTURN');
    EOT.Visible := Server(sTurn - sExecute, Me, 0, nil^) >= rExecuted;
  end
  else
  begin
    UnitInfoBtn.Visible := False;
    UnitBtn.Visible := False;
    TerrainBtn.Visible := False;
    EOT.Hint := Phrases.Lookup('BTN_NEGO');
    EOT.Visible := True;
  end;
  SetTroopLoc(-1);
  MapValid := False;
  NewAgeCenterTo := 0;
  if ((MyRO.Turn = 0) and not Supervising or IsMultiPlayerGame or
    (ClientMode = cResume)) and (MyRO.nCity > 0) then
  begin
    Loc1 := MyCity[0].Loc;
    if (ClientMode = cTurn) and (MyRO.Turn = 0) then
    with MainMap do begin // move city out of center to not be covered by welcome screen
      dx := MapWidth div (xxt * 5);
      if dx > 5 then
        dx := 5;
      dy := MapHeight div (yyt * 5);
      if dy > 5 then
        dy := 5;
      if Loc1 >= G.lx * G.ly div 2 then
      begin
        NewAgeCenterTo := -1;
        Loc1 := dLoc(Loc1, -dx, -dy);
      end
      else
      begin
        NewAgeCenterTo := 1;
        Loc1 := dLoc(Loc1, -dx, dy);
      end
    end;
    Centre(Loc1);
  end;

  ApplyToVisibleForms(faEnable);

  if ClientMode <> cResume then
  begin
    PaintAll;
    if (MyRO.Happened and phChangeGov <> 0) and (MyRO.NatBuilt[imPalace] > 0)
    then
      ImpImage(Panel.Canvas, ClientWidth - xPalace, yPalace, imPalace,
        gAnarchy { , GameMode<>cMovie } );
    // first turn after anarchy -- don't show despotism palace!
    Update;
    ApplyToVisibleForms(faUpdate);

    if MyRO.Happened and phGameEnd <> 0 then
      with MessgExDlg do
      begin // game ended
        if MyRO.Happened and phExtinct <> 0 then
        begin
          OpenSound := 'MSG_GAMEOVER';
          MessgText := Tribe[Me].TPhrase('GAMEOVER');
          IconKind := mikBigIcon;
          IconIndex := 8;
        end
        else if MyRO.Happened and phShipComplete <> 0 then
        begin
          Winners := 0;
          for p1 := 0 to nPl - 1 do
            if 1 shl p1 and MyRO.Alive <> 0 then
            begin
              Winners := Winners or 1 shl p1;
              for I := 0 to nShipPart - 1 do
                if MyRO.Ship[p1].Parts[I] < ShipNeed[I] then
                  Winners := Winners and not(1 shl p1);
            end;
          Assert(Winners <> 0);
          if Winners and (1 shl Me) <> 0 then
          begin
            S := '';
            for p1 := 0 to nPl - 1 do
              if (p1 <> Me) and (1 shl p1 and Winners <> 0) then
                if S = '' then
                  S := Tribe[p1].TPhrase('SHORTNAME')
                else
                  S := Format(Phrases.Lookup('SHAREDWIN_CONCAT'),
                    [S, Tribe[p1].TPhrase('SHORTNAME')]);

            OpenSound := 'MSG_YOUWIN';
            MessgText := Tribe[Me].TPhrase('MYSPACESHIP');
            if S <> '' then
              MessgText := MessgText + '\' +
                Format(Phrases.Lookup('SHAREDWIN'), [S]);
            IconKind := mikBigIcon;
            IconIndex := 9;
          end
          else
          begin
            Assert(Me = 0);
            OpenSound := 'MSG_GAMEOVER';
            MessgText := '';
            for p1 := 0 to nPl - 1 do
              if Winners and (1 shl p1) <> 0 then
                MessgText := MessgText + Tribe[p1].TPhrase('SPACESHIP1');
            MessgText := MessgText + '\' + Phrases.Lookup('SPACESHIP2');
            IconKind := mikEnemyShipComplete;
          end
        end
        else { if MyRO.Happened and fTimeUp<>0 then }
        begin
          Assert(Me = 0);
          OpenSound := 'MSG_GAMEOVER';
          if not Supervising then
            MessgText := Tribe[Me].TPhrase('TIMEUP')
          else
            MessgText := Phrases.Lookup('TIMEUPSUPER');
          IconKind := mikImp;
          IconIndex := 22;
        end;
        Kind := mkOk;
        ShowModal;
        if MyRO.Happened and phExtinct = 0 then
        begin
          p1 := 0;
          while (p1 < nPl - 1) and (Winners and (1 shl p1) = 0) do
            Inc(p1);
          if MyRO.Happened and phShipComplete = 0 then
            DiaDlg.ShowNewContent_Charts(wmModal);
        end;
        TurnComplete := True;
        Exit;
      end;
    if not Supervising and (1 shl Me and MyRO.Alive = 0) then
    begin
      TurnComplete := True;
      Exit;
    end;

    if (ClientMode = cContinue) and
      (DipMem[Me].SentCommand and $FF0F = scContact) then
      // contact was refused
      if MyRO.Treaty[DipMem[Me].pContact] >= trPeace then
        ContactRefused(DipMem[Me].pContact, 'FRREJECTED')
      else
        SoundMessage(Tribe[DipMem[Me].pContact].TPhrase('FRREJECTED'),
          'NEGO_REJECTED');

    if not Supervising and (Age > MyData.ToldAge) and
      ((Age > 0) or (ClientMode <> cMovieTurn)) then
      with MessgExDlg do
      begin
        if Age = 0 then
        begin
          if Phrases2FallenBackToEnglish then
          begin
            S := Tribe[Me].TPhrase('AGE0');
            MessgText :=
              Format(S, [TurnToString(MyRO.Turn), CityName(MyCity[0].ID)]);
          end
          else
          begin
            S := Tribe[Me].TString(Phrases2.Lookup('AGE0'));
            MessgText := Format(S, [TurnToString(MyRO.Turn)]);
          end;
        end
        else
        begin
          S := Tribe[Me].TPhrase('AGE' + char(48 + Age));
          MessgText := Format(S, [TurnToString(MyRO.Turn)]);
        end;
        IconKind := mikAge;
        IconIndex := Age;
        { if age=0 then } Kind := mkOk
        { else begin Kind:=mkOkHelp; HelpKind:=hkAdv; HelpNo:=AgePreq[age]; end };
        CenterTo := NewAgeCenterTo;
        OpenSound := 'AGE_' + char(48 + Age);
        Application.ProcessMessages;
        ShowModal;
        MyData.ToldAge := Age;
        if Age > 0 then
          MyData.ToldTech[AgePreq[Age]] := MyRO.Tech[AgePreq[Age]];
      end;

    if MyData.ToldAlive <> MyRO.Alive then
    begin
      for p1 := 0 to nPl - 1 do
        if (MyData.ToldAlive - MyRO.Alive) and (1 shl p1) <> 0 then
          with MessgExDlg do
          begin
            OpenSound := 'MSG_EXTINCT';
            S := Tribe[p1].TPhrase('EXTINCT');
            MessgText := Format(S, [TurnToString(MyRO.Turn)]);
            if MyRO.Alive = 1 shl Me then
              MessgText := MessgText + Phrases.Lookup('EXTINCTALL');
            Kind := mkOk;
            IconKind := mikImp;
            IconIndex := 21;
            ShowModal;
          end;
      if (ClientMode <> cMovieTurn) and not Supervising then
        DiaDlg.ShowNewContent_Charts(wmModal);
    end;

    // tell changes of own credibility
    if not Supervising then
    begin
      if RoughCredibility(MyRO.Credibility) <>
        RoughCredibility(MyData.ToldOwnCredibility) then
      begin
        if RoughCredibility(MyRO.Credibility) >
          RoughCredibility(MyData.ToldOwnCredibility) then
          S := Phrases.Lookup('CREDUP')
        else
          S := Phrases.Lookup('CREDDOWN');
        TribeMessage(Me, Format(S, [Phrases.Lookup('CREDIBILITY',
          RoughCredibility(MyRO.Credibility))]), '');
      end;
      MyData.ToldOwnCredibility := MyRO.Credibility;
    end;

    for I := 0 to nWonder - 1 do
    begin
      OwnWonder := False;
      for cix := 0 to MyRO.nCity - 1 do
        if (MyCity[cix].Loc >= 0) and (MyCity[cix].ID = MyRO.Wonder[I].CityID)
        then
          OwnWonder := True;
      if MyRO.Wonder[I].CityID <> MyData.ToldWonders[I].CityID then
      begin
        if MyRO.Wonder[I].CityID = WonderDestroyed then
          with MessgExDlg do
          begin { tell about destroyed wonders }
            OpenSound := 'WONDER_DESTROYED';
            MessgText := Format(Phrases.Lookup('WONDERDEST'),
              [Phrases.Lookup('IMPROVEMENTS', I)]);
            Kind := mkOkHelp;
            HelpKind := hkImp;
            HelpNo := I;
            IconKind := mikImp;
            IconIndex := I;
            ShowModal;
          end
        else
        begin
          if I = woManhattan then
            if MyRO.Wonder[I].EffectiveOwner > Me then
              MyData.ColdWarStart := MyRO.Turn - 1
            else
              MyData.ColdWarStart := MyRO.Turn;
          if not OwnWonder then
            with MessgExDlg do
            begin { tell about newly built wonders }
              if I = woManhattan then
              begin
                OpenSound := 'MSG_COLDWAR';
                S := Tribe[MyRO.Wonder[I].EffectiveOwner].TPhrase('COLDWAR');
              end
              else if MyRO.Wonder[I].EffectiveOwner >= 0 then
              begin
                OpenSound := 'WONDER_BUILT';
                S := Tribe[MyRO.Wonder[I].EffectiveOwner]
                  .TPhrase('WONDERBUILT');
              end
              else
              begin
                OpenSound := 'MSG_DEFAULT';
                S := Phrases.Lookup('WONDERBUILTEXP');
                // already expired when built
              end;
              MessgText := Format(S, [Phrases.Lookup('IMPROVEMENTS', I),
                CityName(MyRO.Wonder[I].CityID)]);
              Kind := mkOkHelp;
              HelpKind := hkImp;
              HelpNo := I;
              IconKind := mikImp;
              IconIndex := I;
              ShowModal;
            end;
        end;
      end
      else if (MyRO.Wonder[I].EffectiveOwner <> MyData.ToldWonders[I]
        .EffectiveOwner) and (MyRO.Wonder[I].CityID > WonderDestroyed) then
        if MyRO.Wonder[I].EffectiveOwner < 0 then
        begin
          if I <> woMIR then
            with MessgExDlg do
            begin { tell about expired wonders }
              OpenSound := 'WONDER_EXPIRED';
              MessgText := Format(Phrases.Lookup('WONDEREXP'),
                [Phrases.Lookup('IMPROVEMENTS', I),
                CityName(MyRO.Wonder[I].CityID)]);
              Kind := mkOkHelp;
              HelpKind := hkImp;
              HelpNo := I;
              IconKind := mikImp;
              IconIndex := I;
              ShowModal;
            end;
        end
        else if (MyData.ToldWonders[I].EffectiveOwner >= 0) and not OwnWonder
        then
          with MessgExDlg do
          begin { tell about capture of wonders }
            OpenSound := 'WONDER_CAPTURED';
            S := Tribe[MyRO.Wonder[I].EffectiveOwner].TPhrase('WONDERCAPT');
            MessgText := Format(S, [Phrases.Lookup('IMPROVEMENTS', I),
              CityName(MyRO.Wonder[I].CityID)]);
            Kind := mkOkHelp;
            HelpKind := hkImp;
            HelpNo := I;
            IconKind := mikImp;
            IconIndex := I;
            ShowModal;
          end;
    end;

    if MyRO.Turn = MyData.ColdWarStart + ColdWarTurns then
    begin
      SoundMessageEx(Phrases.Lookup('COLDWAREND'), 'MSG_DEFAULT');
      MyData.ColdWarStart := -ColdWarTurns - 1;
    end;

    TellNewModels;
  end; // ClientMode<>cResume
  MyData.ToldAlive := MyRO.Alive;
  Move(MyRO.Wonder, MyData.ToldWonders, SizeOf(MyData.ToldWonders));

  NewGovAvailable := -1;
  if ClientMode <> cResume then
  begin // tell about new techs
    for ad := 0 to nAdv - 1 do
      if (MyRO.TestFlags and tfAllTechs = 0) and
        ((MyRO.Tech[ad] >= tsApplicable) <> (MyData.ToldTech[ad] >=
        tsApplicable)) or (ad in FutureTech) and
        (MyRO.Tech[ad] <> MyData.ToldTech[ad]) then
        with MessgExDlg do
        begin
          Item := 'RESEARCH_GENERAL';
          if GameMode <> cMovie then
            OpenSound := 'NEWADVANCE_' + char(48 + Age);
          Item2 := Phrases.Lookup('ADVANCES', ad);
          if ad in FutureTech then
            Item2 := Item2 + ' ' + IntToStr(MyRO.Tech[ad]);
          MessgText := Format(Phrases.Lookup(Item), [Item2]);
          Kind := mkOkHelp;
          HelpKind := hkAdv;
          HelpNo := ad;
          IconKind := mikBook;
          IconIndex := -1;
          for I := 0 to nAdvBookIcon - 1 do
            if AdvBookIcon[I].Adv = ad then
              IconIndex := AdvBookIcon[I].Icon;
          ShowModal;
          MyData.ToldTech[ad] := MyRO.Tech[ad];
          for I := gMonarchy to nGov - 1 do
            if GovPreq[I] = ad then
              NewGovAvailable := I;
        end;
  end;

  ShowCityList := False;
  if ClientMode = cTurn then
  begin
    if (MyRO.Happened and phTech <> 0) and (MyData.FarTech <> adNexus) then
      ChooseResearch;

    UpdatePanel := False;
    if MyRO.Happened and phChangeGov <> 0 then
    begin
      ModalSelectDlg.ShowNewContent(wmModal, kGov);
      Play('NEWGOV');
      Server(sSetGovernment, Me, ModalSelectDlg.Result, nil^);
      CityOptimizer_BeginOfTurn;
      UpdatePanel := True;
    end;
  end; // ClientMode=cTurn

  if not Supervising and ((ClientMode = cTurn) or (ClientMode = cMovieTurn))
  then
    for cix := 0 to MyRO.nCity - 1 do
      with MyCity[cix] do
        Status := Status and not csToldBombard;

  if ((ClientMode = cTurn) or (ClientMode = cMovieTurn)) and
    (MyRO.Government <> gAnarchy) then
  begin
    // tell what happened in cities
    for WondersOnly := True downto False do
      for cix := 0 to MyRO.nCity - 1 do
        with MyCity[cix] do
          if (MyRO.Turn > 0) and (Loc >= 0) and (Flags and chCaptured = 0) and
            (WondersOnly = (Flags and chProduction <> 0) and
            (Project0 and cpImp <> 0) and (Project0 and cpIndex < nWonder)) then
          begin
            if WondersOnly then
              with MessgExDlg do
              begin { tell about newly built wonder }
                OpenSound := 'WONDER_BUILT';
                S := Tribe[Me].TPhrase('WONDERBUILTOWN');
                MessgText :=
                  Format(S, [Phrases.Lookup('IMPROVEMENTS',
                  Project0 and cpIndex), CityName(ID)]);
                Kind := mkOkHelp;
                HelpKind := hkImp;
                HelpNo := Project0 and cpIndex;
                IconKind := mikImp;
                IconIndex := Project0 and cpIndex;
                ShowModal;
              end;
            if not Supervising and (ClientMode = cTurn) then
            begin
              AllowCityScreen := True;
              if (Status and 7 <> 0) and
                (Project and (cpImp + cpIndex) = cpImp + imTrGoods) then
                if (MyData.ImpOrder[Status and 7 - 1, 0] >= 0) then
                begin
                  if AutoBuild(cix, MyData.ImpOrder[Status and 7 - 1]) then
                    AllowCityScreen := False
                  else if Flags and chProduction <> 0 then
                    Flags := (Flags and not chProduction) or chAllImpsMade
                end
                else
                  Flags := Flags or chTypeDel;
              if (Size >= NeedAqueductSize) and
                (MyRO.Tech[Imp[imAqueduct].Preq] < tsApplicable) or
                (Size >= NeedSewerSize) and
                (MyRO.Tech[Imp[imSewer].Preq] < tsApplicable) then
                Flags := Flags and not chNoGrowthWarning;
              // don't remind of unknown building
              if Flags and chNoSettlerProd = 0 then
                Status := Status and not csToldDelay
              else if Status and csToldDelay = 0 then
                Status := Status or csToldDelay
              else
                Flags := Flags and not chNoSettlerProd;
              if mRepScreens.Checked then
              begin
                if (Flags and CityRepMask <> 0) and AllowCityScreen then
                begin { show what happened in cities }
                  SetTroopLoc(MyCity[cix].Loc);
                  MarkCityLoc := MyCity[cix].Loc;
                  PanelPaint;
                  CityDlg.CloseAction := None;
                  CityDlg.ShowNewContent(wmModal, MyCity[cix].Loc,
                    Flags and CityRepMask);
                  UpdatePanel := True;
                end;
              end
              else { if mRepList.Checked then }
              begin
                if Flags and CityRepMask <> 0 then
                  ShowCityList := True;
              end;
            end;
          end; { city loop }
  end; // ClientMode=cTurn

  if ClientMode = cTurn then
  begin
    if NewGovAvailable >= 0 then
      with MessgExDlg do
      begin
        MessgText := Format(Phrases.Lookup('AUTOREVOLUTION'),
          [Phrases.Lookup('GOVERNMENT', NewGovAvailable)]);
        Kind := mkYesNo;
        IconKind := mikPureIcon;
        IconIndex := 6 + NewGovAvailable;
        ShowModal;
        if ModalResult = mrOK then
        begin
          Play('REVOLUTION');
          Server(sRevolution, Me, 0, nil^);
        end;
      end;
  end; // ClientMode=cTurn

  if (ClientMode = cTurn) or (ClientMode = cMovieTurn) then
  begin
    if MyRO.Happened and phGliderLost <> 0 then
      ContextMessage(Phrases.Lookup('GLIDERLOST'), 'MSG_DEFAULT',
        hkModel, 200);
    if MyRO.Happened and phPlaneLost <> 0 then
      ContextMessage(Phrases.Lookup('PLANELOST'), 'MSG_DEFAULT',
        hkFeature, mcFuel);
    if MyRO.Happened and phPeaceEvacuation <> 0 then
      for p1 := 0 to nPl - 1 do
        if 1 shl p1 and MyData.PeaceEvaHappened <> 0 then
          SoundMessageEx(Tribe[p1].TPhrase('WITHDRAW'), 'MSG_DEFAULT');
    if MyRO.Happened and phPeaceViolation <> 0 then
      for p1 := 0 to nPl - 1 do
        if (1 shl p1 and MyRO.Alive <> 0) and (MyRO.EvaStart[p1] = MyRO.Turn)
        then
          SoundMessageEx(Format(Tribe[p1].TPhrase('VIOLATION'),
            [TurnToString(MyRO.Turn + PeaceEvaTurns - 1)]), 'MSG_WITHDRAW');
    TellNewContacts;
  end;

  if ClientMode = cMovieTurn then
    Update
  else if ClientMode = cTurn then
  begin
    if UpdatePanel then
      UpdateViews;
    Application.ProcessMessages;

    if not Supervising then
      for uix := 0 to MyRO.nUn - 1 do
        with MyUn[uix] do
          if Loc >= 0 then
          begin
            if Flags and unWithdrawn <> 0 then
              Status := 0;
            if Health = 100 then
              Status := Status and not usRecover;
            if (Master >= 0) or UnitExhausted(uix) then
              Status := Status and not usWaiting
            else
              Status := Status or usWaiting;
            CheckToldNoReturn(uix);
            if Status and usGoto <> 0 then
            begin { continue multi-turn goto }
              SetUnFocus(uix);
              SetTroopLoc(Loc);
              FocusOnLoc(TroopLoc, flRepaintPanel or flImmUpdate);
              if Status shr 16 = $7FFF then
                MoveResult := GetMoveAdvice(UnFocus, maNextCity,
                  MoveAdviceData)
              else
                MoveResult := GetMoveAdvice(UnFocus, Status shr 16,
                  MoveAdviceData);
              if MoveResult >= rExecuted then
              begin // !!! Shinkansen
                MoveResult := eOK;
                ok := True;
                for I := 0 to MoveAdviceData.nStep - 1 do
                begin
                  Loc1 := dLoc(Loc, MoveAdviceData.dx[I],
                    MoveAdviceData.dy[I]);
                  if (MyMap[Loc1] and (fCity or fOwned) = fCity)
                  // don't capture cities during auto move
                    or (MyMap[Loc1] and (fUnit or fOwned) = fUnit) then
                  // don't attack during auto move
                  begin
                    ok := False;
                    Break
                  end
                  else
                  begin
                    if (Loc1 = MoveAdviceData.ToLoc) or
                      (MoveAdviceData.ToLoc = maNextCity) and
                      (MyMap[dLoc(Loc, MoveAdviceData.dx[I],
                      MoveAdviceData.dy[I])] and fCity <> 0) then
                      MoveOptions := muAutoNoWait
                    else
                      MoveOptions := 0;
                    MoveResult := MoveUnit(MoveAdviceData.dx[I],
                      MoveAdviceData.dy[I], MoveOptions);
                    if (MoveResult < rExecuted) or (MoveResult = eEnemySpotted)
                    then
                    begin
                      ok := False;
                      Break
                    end;
                  end
                end;
                Stop := not ok or (Loc = MoveAdviceData.ToLoc) or
                  (MoveAdviceData.ToLoc = maNextCity) and
                  (MyMap[Loc] and fCity <> 0)
              end
              else
              begin
                MoveResult := eOK;
                Stop := True;
              end;

              if MoveResult <> eDied then
                if Stop then
                  Status := Status and ($FFFF - usGoto)
                else
                  Status := Status and not usWaiting;
            end;

            if Status and (usEnhance or usGoto) = usEnhance then
            // continue terrain enhancement
            begin
              MoveResult := ProcessEnhancement(uix, MyData.EnhancementJobs);
              if MoveResult <> eDied then
                if MoveResult = eJobDone then
                  Status := Status and not usEnhance
                else
                  Status := Status and not usWaiting;
            end;
          end;
  end; // ClientMode=cTurn

  HaveStrategyAdvice := False;
  // (GameMode<>cMovie) and not supervising
  // and AdvisorDlg.HaveStrategyAdvice;
  GoOnPhase := True;
  if Supervising or (GameMode = cMovie) then
  begin
    SetTroopLoc(-1);
    PaintAll;
  end { supervisor }
  { else if (ClientMode=cTurn) and (MyRO.Turn=0) then
    begin
    SetUnFocus(0);
    ZoomToCity(MyCity[0].Loc)
    end }
  else
  begin
    if ClientMode >= scContact then
      SetUnFocus(-1)
    else
      NextUnit(-1, False);
    if UnFocus < 0 then
    begin
      UnStartLoc := -1;
      if IsMultiPlayerGame or (ClientMode = cResume) then
        if MyRO.nCity > 0 then
          FocusOnLoc(MyCity[0].Loc)
        else
          FocusOnLoc(G.lx * G.ly div 2);
      SetTroopLoc(-1);
      PanelPaint;
    end;
    if ShowCityList then
      ListDlg.ShowNewContent(wmPersistent, kCityEvents);
  end;
end;

procedure TMainScreen.Client(Command, NewPlayer: Integer; var Data);
var
  I, J, p1, mix, ToLoc, AnimationSpeed, ShowMoveDomain, cix, ecix: Integer;
  Color: TColor;
  Name, S: string;
  TribeInfo: TTribeInfo;
  mi: TModelInfo;
  SkipTurn, IsAlpine, IsTreatyDeal: Boolean;
begin
  case Command of
    cTurn, cResume, cContinue, cMovieTurn, scContact, scDipStart .. scDipBreak:
      begin
        Supervising := G.Difficulty[NewPlayer] = 0;
        ArrangeMidPanel;
      end
  end;
  case Command of
    cDebugMessage:
      LogDlg.Add(NewPlayer, G.RO[0].Turn, PChar(@Data));

    cShowNego:
      with TShowNegoData(Data) do
      begin
        S := Format('P%d to P%d: ', [pSender, pTarget]);
        if (Action = scDipOffer) and (Offer.nDeliver + Offer.nCost > 0) then
        begin
          S := S + 'Offer ';
          for I := 0 to Offer.nDeliver + Offer.nCost - 1 do
          begin
            if I = Offer.nDeliver then
              S := S + ' for '
            else if I > 0 then
              S := S + '+';
            case Offer.Price[I] and opMask of
              opChoose:
                S := S + 'Price of choice';
              opCivilReport:
                S := S + 'State report';
              opMilReport:
                S := S + 'Military report';
              opMap:
                S := S + 'Map';
              opTreaty:
                S := S + 'Treaty';
              opShipParts:
                S := S + 'Ship part';
              opMoney:
                S := S + IntToStr(Offer.Price[I] and $FFFFFF) + 'o';
              opTribute:
                S := S + IntToStr(Offer.Price[I] and $FFFFFF) + 'o tribute';
              opTech:
                S := S + Phrases.Lookup('ADVANCES', Offer.Price[I] and $FFFFFF);
              opAllTech:
                S := S + 'All advances';
              opModel:
                S := S + Tribe[pSender].ModelName[Offer.Price[I] and $FFFFFF];
              opAllModel:
                S := S + 'All models';
            end;
          end;
          LogDlg.Add(NewPlayer, G.RO[0].Turn, PChar(S));
        end
        else if Action = scDipAccept then
        begin
          S := S + '--- ACCEPTED! ---';
          LogDlg.Add(NewPlayer, G.RO[0].Turn, PChar(S));
        end;
      end;

    cInitModule:
      begin
        Server := TInitModuleData(Data).Server;
        // AdvisorDlg.Init;
        InitModule;
        TInitModuleData(Data).DataSize := SizeOf(TPersistentData);
        TInitModuleData(Data).Flags := aiThreaded;
      end;

    cReleaseModule: DoneModule;

    cHelpOnly, cStartHelp, cStartCredits:
      begin
        Age := 0;
        if Command = cHelpOnly then
          MainTexture.Age := -1;
        Tribes.Init;
        HelpDlg.UserLeft := (Screen.width - HelpDlg.width) div 2;
        HelpDlg.UserTop := (Screen.height - HelpDlg.height) div 2;
        HelpDlg.Difficulty := 0;
        if Command = cStartCredits then
          HelpDlg.ShowNewContent(wmModal, hkMisc, miscCredits)
        else
          HelpDlg.ShowNewContent(wmModal, hkMisc, miscMain);
        Tribes.Done;
      end;

    cNewGame, cLoadGame, cMovie, cNewMap:
      begin
        { if (Command=cNewGame) or (Command=cLoadGame) then
          AdvisorDlg.NewGame(Data); }
        GenerateNames := mNames.Checked;
        GameOK := True;
        G := TNewGameData(Data);
        Me := -1;
        pLogo := -1;
        ClientMode := -1;
        SetMapOptions;
        MainMap.pDebugMap := -1;
        Idle := False;
        FillChar(Jump, SizeOf(Jump), 0);
        if StartRunning then
          Jump[0] := 999999;
        GameMode := Command;
        GrExt.ResetPixUsed;
        MainMap.Reset;
        NoMap.Reset;
        Tribes.Init;
        GetTribeList;
        for p1 := 0 to nPl - 1 do
          if (G.RO[p1] <> nil) and (G.RO[p1].Data <> nil) then
            with TPersistentData(G.RO[p1].Data^) do
            begin
              FarTech := adNone;
              FillChar(EnhancementJobs, SizeOf(EnhancementJobs), jNone);
              FillChar(ImpOrder, SizeOf(ImpOrder), Byte(-1));
              ColdWarStart := -ColdWarTurns - 1;
              ToldAge := -1;
              ToldModels := 3;
              ToldAlive := 0;
              ToldContact := 0;
              ToldOwnCredibility := InitialCredibility;
              for I := 0 to nPl - 1 do
                if G.Difficulty[I] > 0 then
                  Inc(ToldAlive, 1 shl I);
              PeaceEvaHappened := 0;
              for I := 0 to nWonder - 1 do
                with ToldWonders[I] do
                begin
                  CityID := -1;
                  EffectiveOwner := -1
                end;
              FillChar(ToldTech, SizeOf(ToldTech), Byte(tsNA));
              if G.Difficulty[p1] > 0 then
                SoundPreload([sbStart]);
            end;

        // arrange dialogs
        ListDlg.UserLeft := 8;
        ListDlg.UserTop := TopBarHeight + 8;
        HelpDlg.UserLeft := Screen.width - HelpDlg.width - 8;
        HelpDlg.UserTop := TopBarHeight + 8;
        UnitStatDlg.UserLeft := 397;
        UnitStatDlg.UserTop := TopBarHeight + 64;
        DiaDlg.UserLeft := (Screen.width - DiaDlg.width) div 2;
        DiaDlg.UserTop := (Screen.height - DiaDlg.height) div 2;
        NatStatDlg.UserLeft := Screen.width - NatStatDlg.width - 8;
        NatStatDlg.UserTop := Screen.height - PanelHeight -
          NatStatDlg.height - 8;
        if NatStatDlg.UserTop < 8 then
          NatStatDlg.UserTop := 8;

        Age := 0;
        MovieSpeed := 1;
        LogDlg.mSlot.Visible := True;
        LogDlg.Host := self;
        HelpDlg.ClearHistory;
        CityDlg.Reset;

        MiniMap.Size := Point(G.lx, G.ly);
        for I := 0 to nPl - 1 do
        begin
          Tribe[I] := nil;
          TribeOriginal[I] := False;
        end;
        ToldSlavery := -1;
        RepaintOnResize := False;
        Closable := False;
        FirstMovieTurn := True;

        MenuArea.Visible := GameMode <> cMovie;
        TreasuryArea.Visible := GameMode < cMovie;
        ResearchArea.Visible := GameMode < cMovie;
        ManagementArea.Visible := GameMode < cMovie;
      end;

    cGetReady, cReplay:
      if NewPlayer = 0 then
      begin
        I := 0;
        for p1 := 0 to nPl - 1 do
          if (G.Difficulty[p1] > 0) and (Tribe[p1] = nil) then
            Inc(I);
        if I > UnusedTribeFiles.Count then
        begin
          GameOK := False;
          SimpleMessage(Phrases.Lookup('TOOFEWTRIBES'));
        end
        else
        begin
          for p1 := 0 to nPl - 1 do
            if (G.Difficulty[p1] > 0) and (Tribe[p1] = nil) and (G.RO[p1] <> nil)
            then
            begin // let player select own tribes
              TribeInfo.trix := p1;
              TribeNames.Clear;
              for J := 0 to UnusedTribeFiles.Count - 1 do
              begin
                GetTribeInfo(UnusedTribeFiles[J], Name, Color);
                TribeNames.AddObject(Name, TObject(Color));
              end;
              Assert(TribeNames.Count > 0);
              ModalSelectDlg.ShowNewContent(wmModal, kTribe);
              Application.ProcessMessages;
              TribeInfo.FileName := UnusedTribeFiles[ModalSelectDlg.Result];
              UnusedTribeFiles.Delete(ModalSelectDlg.Result);

              if GameMode = cLoadGame then
                CreateTribe(TribeInfo.trix, TribeInfo.FileName, False)
              else
                Server(CommandWithData(cSetTribe, TribeInfo.GetCommandDataSize),
                  0, 0, TribeInfo);
            end;

          for p1 := 0 to nPl - 1 do
            if (G.Difficulty[p1] > 0) and (Tribe[p1] = nil) and (G.RO[p1] = nil)
            then
            begin // autoselect enemy tribes
              J := ChooseUnusedTribe;
              TribeInfo.FileName := UnusedTribeFiles[J];
              UnusedTribeFiles.Delete(J);
              TribeInfo.trix := p1;
              if GameMode = cLoadGame then
                CreateTribe(TribeInfo.trix, TribeInfo.FileName, False)
              else
                Server(CommandWithData(cSetTribe, TribeInfo.GetCommandDataSize),
                  0, 0, TribeInfo);
            end;
        end;
        if not mNames.Checked then
          for p1 := 0 to nPl - 1 do
            if Tribe[p1] <> nil then
              Tribe[p1].NumberName := p1;
      end;

    cBreakGame:
      begin
        SaveSettings;
        CityDlg.CloseAction := None;
        ApplyToVisibleForms(faClose);
        if LogDlg.Visible then
          LogDlg.Close;
        LogDlg.List.Clear;
        StartRunning := not Idle and (Jump[0] > 0); // AI called Reload
        Me := -1;
        Idle := False;
        ClientMode := -1;
        UnitInfoBtn.Visible := False;
        UnitBtn.Visible := False;
        TerrainBtn.Visible := False;
        MovieSpeed1Btn.Visible := False;
        MovieSpeed2Btn.Visible := False;
        MovieSpeed3Btn.Visible := False;
        MovieSpeed4Btn.Visible := False;
        EOT.Visible := False;
        for I := 0 to ControlCount - 1 do
          if Controls[I] is TButtonC then
            Controls[I].Visible := False;
        sb.Init(0, 1);
        for p1 := 0 to nPl - 1 do
          if Tribe[p1] <> nil then
            FreeAndNil(Tribe[p1]);
        Tribes.Done;
        RepaintOnResize := False;
        Closable := True;
        Close;
        { if (GameMode=cNewGame) or (GameMode=cLoadGame) then
          AdvisorDlg.BreakGame; }
      end;

    cShowGame:
      begin
        with Panel.Canvas do
        begin
          Brush.Color := $000000;
          FillRect(Rect(0, 0, Panel.width, Panel.height));
          Brush.Style := bsClear;
        end;
        with TopBar.Canvas do
        begin
          Brush.Color := $000000;
          FillRect(Rect(0, 0, TopBar.width, TopBar.height));
          Brush.Style := bsClear;
        end;
        FormResize(nil); // place mini map correctly according to its size
        Show;
        Update;
        RepaintOnResize := True;
        xw := 0;
        yw := ywcenter;
        if not StayOnTop_Ensured then
        begin
          StayOnTop_Ensured := True;
          CityDlg.StayOnTop_Workaround;
          CityTypeDlg.StayOnTop_Workaround;
          DiaDlg.StayOnTop_Workaround;
          DraftDlg.StayOnTop_Workaround;
          EnhanceDlg.StayOnTop_Workaround;
          HelpDlg.StayOnTop_Workaround;
          NatStatDlg.StayOnTop_Workaround;
          NegoDlg.StayOnTop_Workaround;
          ModalSelectDlg.StayOnTop_Workaround;
          ListDlg.StayOnTop_Workaround;
          UnitStatDlg.StayOnTop_Workaround;
          WondersDlg.StayOnTop_Workaround;
          RatesDlg.StayOnTop_Workaround;
        end;
      end;

    cShowTurnChange:
      begin
        if Integer(Data) >= 0 then
        begin
          pLogo := Integer(Data);
          if G.RO[pLogo] = nil then
          begin
            if AILogo[pLogo] <> nil then
              BitBltCanvas(Canvas, (xRightPanel + 10) - (16 + 64),
                ClientHeight - PanelHeight, 64, 64, AILogo[pLogo].Canvas,
                0, 0);
          end;
        end;
      end;

    cTurn, cResume, cContinue:
      if not GameOK then
        Server(sResign, NewPlayer, 0, nil^)
      else
      begin
        ClientMode := Command;
        pTurn := NewPlayer;
        pLogo := NewPlayer;

        if Command = cResume then
        begin // init non-original model pictures (maybe tribes not found)
          for p1 := 0 to nPl - 1 do
            if G.RO[p1] <> nil then
            begin
              ItsMeAgain(p1);
              for mix := 0 to MyRO.nModel - 1 do
                if not Assigned(Tribe[Me].ModelPicture[mix].HGr) then
                  InitMyModel(mix, True);
            end;
          Me := -1;
        end;

        if Jump[pTurn] > 0 then
          Application.ProcessMessages;
        if Jump[pTurn] > 0 then
          if G.RO[NewPlayer].Happened and phGameEnd <> 0 then
            Jump[pTurn] := 0
          else
            Dec(Jump[pTurn]);
        SkipTurn := Jump[pTurn] > 0;
        if SkipTurn then
        begin
          ItsMeAgain(NewPlayer);
          MyData := G.RO[NewPlayer].Data;
          SetTroopLoc(-1);
          MiniMapPaint;
          InitAllEnemyModels; // necessary for correct replay
          if not EndTurn(True) then
            SkipTurn := False;
        end;
        if not SkipTurn then
        begin
          if ((ClientMode < scDipStart) or (ClientMode > scDipBreak)) and
            NegoDlg.Visible then
            NegoDlg.Close;
          skipped := False; // always show my moves during my turn
          Idle := True;
          InitTurn(NewPlayer);
          DipMem[Me].pContact := -1;
          (* if (Me=0) and (MyRO.Alive and (1 shl Me)=0)} then
            begin
            if SimpleQuery(Phrases.Lookup('RESIGN'))=mrIgnore then
            Server(sResign,Me,0,nil^)
            else Server(sBreak,Me,0,nil^)
            end
            else Play('TURNSTART'); *)
        end;
      end;

    cMovieTurn:
      begin
        ClientMode := Command;
        pTurn := NewPlayer;
        pLogo := -1;
        skipped := False; // always show my moves during my turn
        Idle := True;
        if FirstMovieTurn then
        begin
          CheckMovieSpeedBtnState;
          FirstMovieTurn := False;
        end;
        InitTurn(NewPlayer);
        Application.ProcessMessages;
        if MovieSpeed = 4 then
        begin
          Sleep(75);
          // this break will ensure speed of fast forward does not depend on cpu speed
          Application.ProcessMessages;
        end;
      end;

    cMovieEndTurn:
      begin
        RememberPeaceViolation;
        pTurn := -1;
        pLogo := -1;
        MapValid := False;
        ClientMode := -1;
        Idle := False;
        skipped := False;
      end;

    cEditMap:
      begin
        ClientMode := cEditMap;
        SetMapOptions;
        MainMap.pDebugMap := -1;
        ItsMeAgain(0);
        MyData := nil;
        UnitInfoBtn.Visible := False;
        UnitBtn.Visible := False;
        TerrainBtn.Visible := False;
        MovieSpeed1Btn.Visible := False;
        MovieSpeed2Btn.Visible := False;
        MovieSpeed3Btn.Visible := False;
        MovieSpeed4Btn.Visible := False;
        EOT.Visible := False;
        HelpDlg.Difficulty := 0;
        BrushType := fGrass;
        BrushLoc := -1;
        Edited := False;
        UnFocus := -1;
        MarkCityLoc := -1;
        Tracking := False;
        TurnComplete := False;
        MapValid := False;
        FormResize(nil); // calculate geometrics and paint all
        SetTroopLoc(-1);
        Idle := True;
      end;

    (* cNewContact:
      begin
      end;
    *)

    scContact:
      begin
        DipMem[NewPlayer].pContact := Integer(Data);
        if Jump[NewPlayer] > 0 then
          DipCall(scReject)
        else
        begin
          ClientMode := Command;
          InitTurn(NewPlayer);
          MyData.ToldContact := MyData.ToldContact or (1 shl Integer(Data));
          // don't tell about new nation when already contacted by them
          with MessgExDlg do
          begin
            OpenSound := 'CONTACT_' + char(48 + MyRO.EnemyReport[Integer(Data)
              ].Attitude);
            MessgText := Tribe[Integer(Data)].TPhrase('FRCONTACT');
            Kind := mkYesNo;
            IconKind := mikTribe;
            IconIndex := Integer(Data);
            ShowModal;
            if ModalResult = mrOK then
            begin
              NegoDlg.Respond;
              DipMem[Me].DeliveredPrices := [];
              DipMem[Me].ReceivedPrices := [];
              DipCall(scDipStart);
            end
            else
            begin
              DipCall(scReject);
              EndNego;
            end;
          end;
        end;
      end;

    scDipStart .. scDipBreak:
      begin
        ClientMode := Command;
        InitTurn(NewPlayer);
        if Command = scDipStart then
          Play('CONTACT_' + char(48 + MyRO.Attitude[DipMem[NewPlayer]
            .pContact]))
        else if Command = scDipCancelTreaty then
          Play('CANCELTREATY')
        else if Command = scDipOffer then
        begin
          ReceivedOffer := TOffer(Data);
          InitAllEnemyModels;
        end
        else if Command = scDipAccept then
        begin // remember delivered and received prices
          for I := 0 to DipMem[Me].SentOffer.nDeliver - 1 do
            Include(DipMem[Me].DeliveredPrices,
              DipMem[Me].SentOffer.Price[I] shr 24);
          for I := 0 to DipMem[Me].SentOffer.nCost - 1 do
            Include(DipMem[Me].ReceivedPrices,
              DipMem[Me].SentOffer.Price[DipMem[Me].SentOffer.nDeliver +
              I] shr 24);
          IsTreatyDeal := False;
          for I := 0 to ReceivedOffer.nDeliver + ReceivedOffer.nCost - 1 do
            if DipMem[Me].SentOffer.Price[I] and opMask = opTreaty then
              IsTreatyDeal := True;
          if IsTreatyDeal then
            Play('NEWTREATY')
          else
            Play('ACCEPTOFFER');
        end;
        NegoDlg.Start;
        Idle := True;
      end;

    cShowCancelTreaty:
      if not IsMultiPlayerGame then
      begin
        case G.RO[NewPlayer].Treaty[Integer(Data)] of
          trPeace:
            S := Tribe[Integer(Data)].TPhrase('FRCANCELBYREJECT_PEACE');
          trFriendlyContact:
            S := Tribe[Integer(Data)].TPhrase('FRCANCELBYREJECT_FRIENDLY');
          trAlliance:
            S := Tribe[Integer(Data)].TPhrase('FRCANCELBYREJECT_ALLIANCE');
        end;
        TribeMessage(Integer(Data), S, 'CANCELTREATY');
      end;

    cShowCancelTreatyByAlliance:
      if Idle and (NewPlayer = Me) then
        TribeMessage(Integer(Data), Tribe[Integer(Data)
          ].TPhrase('FRENEMYALLIANCE'), 'CANCELTREATY');

    cShowSupportAllianceAgainst:
      if not IsMultiPlayerGame and (Jump[0] = 0) then
        TribeMessage(Integer(Data) and $F, Tribe[Integer(Data) and $F]
          .TPhrase('FRMYALLIANCE1') + ' ' + Tribe[Integer(Data) shr 4]
          .TPhrase('FRMYALLIANCE2'), 'CANCELTREATY');

    cShowPeaceViolation:
      if not IsMultiPlayerGame and (Jump[0] = 0) then
        TribeMessage(Integer(Data),
          Format(Tribe[Integer(Data)].TPhrase('EVIOLATION'),
          [TurnToString(MyRO.Turn + PeaceEvaTurns - 1)]), 'MSG_WITHDRAW');

    cShowEndContact:
      EndNego;

    cShowUnitChanged, cShowCityChanged, cShowAfterMove, cShowAfterAttack:
      if (Idle and (NewPlayer = Me) or not Idle and not skipped) and
        not((GameMode = cMovie) and (MovieSpeed = 4)) then
      begin
        Assert(NewPlayer = Me);
        if not Idle or (GameMode = cMovie) then
          Application.ProcessMessages;
        if Command = cShowCityChanged then
        begin
          CurrentMoveInfo.DoShow := False;
          if Idle then
            CurrentMoveInfo.DoShow := True
          else if CurrentMoveInfo.IsAlly then
            CurrentMoveInfo.DoShow := not mAlNoMoves.Checked
          else
            CurrentMoveInfo.DoShow := not mEnNoMoves.Checked;
        end
        else if Command = cShowUnitChanged then
        begin
          CurrentMoveInfo.DoShow := False;
          if Idle then
            CurrentMoveInfo.DoShow := not mEffectiveMovesOnly.Checked
          else if CurrentMoveInfo.IsAlly then
            CurrentMoveInfo.DoShow :=
              not(mAlNoMoves.Checked or mAlEffectiveMovesOnly.Checked)
          else
            CurrentMoveInfo.DoShow :=
              not(mEnNoMoves.Checked or mEnAttacks.Checked);
        end;
        // else keep DoShow from cShowMove/cShowAttack

        if CurrentMoveInfo.DoShow then
        begin
          if Command = cShowCityChanged then
            MapValid := False;
          FocusOnLoc(Integer(Data), flImmUpdate);
          // OldUnFocus:=UnFocus;
          // UnFocus:=-1;
          if Command = cShowAfterMove then
            PaintLoc(Integer(Data), CurrentMoveInfo.AfterMovePaintRadius)
            // show discovered areas
          else
            PaintLoc(Integer(Data), 1);
          // UnFocus:=OldUnFocus;
          if (Command = cShowAfterAttack) and
            (CurrentMoveInfo.AfterAttackExpeller >= 0) then
          begin
            SoundMessageEx(Tribe[CurrentMoveInfo.AfterAttackExpeller]
              .TPhrase('EXPEL'), '');
            CurrentMoveInfo.AfterAttackExpeller := -1;
            Update; // remove message box from screen
          end
          else if not Idle then
            if Command = cShowCityChanged then
              Sleep(MoveTime * WaitAfterShowMove div 16)
            else if (Command = cShowUnitChanged) and
              (MyMap[Integer(Data)] and fUnit <> 0) then
              Sleep(MoveTime * WaitAfterShowMove div 32)
        end // if CurrentMoveInfo.DoShow
        else
          MapValid := False;
      end;

    cShowMoving, cShowCapturing:
      if (Idle and (NewPlayer = Me) or not Idle and not skipped and
        (TShowMove(Data).emix <> $FFFF)) and
        not((GameMode = cMovie) and (MovieSpeed = 4)) then
      begin
        Assert(NewPlayer = Me);
        if not Idle or (GameMode = cMovie) then
          Application.ProcessMessages;
        with TShowMove(Data) do
        begin
          CurrentMoveInfo.DoShow := False;
          if not Idle and (not Assigned(Tribe[Owner].ModelPicture[mix].HGr)) then
            InitEnemyModel(emix);

          ToLoc := dLoc(FromLoc, dx, dy);
          if Idle then
          begin // own unit -- make discovered land visible
            Assert(Owner = Me); // no foreign moves during my turn!
            CurrentMoveInfo.DoShow := not mEffectiveMovesOnly.Checked or
              (Command = cShowCapturing);
            if CurrentMoveInfo.DoShow then
            begin
              if GameMode = cMovie then
              begin
                if MovieSpeed = 3 then
                  AnimationSpeed := 4
                else if MovieSpeed = 2 then
                  AnimationSpeed := 8
                else
                  AnimationSpeed := 16;
              end
              else
              begin
                if mVeryFastMoves.Checked then
                  AnimationSpeed := 4
                else if mFastMoves.Checked then
                  AnimationSpeed := 8
                else
                  AnimationSpeed := 16;
              end;
              with MyModel[mix] do
              begin
                if (Kind = mkDiplomat) or (Domain = dAir) or
                  (Cap[mcRadar] + Cap[mcCarrier] + Cap[mcAcademy] > 0) or
                  (MyMap[ToLoc] and fTerrain = fMountains) or
                  (MyMap[ToLoc] and fTerImp = tiFort) or
                  (MyMap[ToLoc] and fTerImp = tiBase) then
                  CurrentMoveInfo.AfterMovePaintRadius := 2
                else
                  CurrentMoveInfo.AfterMovePaintRadius := 1;
                if (MyRO.Wonder[woShinkansen].EffectiveOwner = Me) and
                  (Domain = dGround) and
                  (MyMap[FromLoc] and (fRR or fCity) <> 0) and
                  (MyMap[ToLoc] and (fRR or fCity) <> 0) and
                  (Flags and umPlaneUnloading = 0) then
                  AnimationSpeed := 4;
                ShowMoveDomain := Domain;
                IsAlpine := Cap[mcAlpine] > 0;
              end;
            end;
          end
          else
          begin
            CurrentMoveInfo.IsAlly := MyRO.Treaty[Owner] = trAlliance;
            if GameMode = cMovie then
              CurrentMoveInfo.DoShow := True
            else if CurrentMoveInfo.IsAlly then
              CurrentMoveInfo.DoShow := not mAlNoMoves.Checked and
                not(mAlEffectiveMovesOnly.Checked and
                (Command <> cShowCapturing))
            else
              CurrentMoveInfo.DoShow := not mEnNoMoves.Checked and
                not(mEnAttacks.Checked and (Command <> cShowCapturing));
            if CurrentMoveInfo.DoShow then
            begin
              if Command = cShowCapturing then
              begin // show capture message
                if MyMap[ToLoc] and fOwned <> 0 then
                begin // own city, search
                  cix := MyRO.nCity - 1;
                  while (cix >= 0) and (MyCity[cix].Loc <> ToLoc) do
                    Dec(cix);
                  S := CityName(MyCity[cix].ID);
                end
                else
                begin // foreign city, search
                  ecix := MyRO.nEnemyCity - 1;
                  while (ecix >= 0) and (MyRO.EnemyCity[ecix].Loc <> ToLoc) do
                    Dec(ecix);
                  S := CityName(MyRO.EnemyCity[ecix].ID);
                end;
                TribeMessage(Owner, Format(Tribe[Owner].TPhrase('CAPTURE'),
                  [S]), '');
                Update; // remove message box from screen
              end;

              if CurrentMoveInfo.IsAlly then
              begin // allied unit -- make discovered land visible
                if mAlFastMoves.Checked then
                  AnimationSpeed := 8
                else
                  AnimationSpeed := 16;
                with MyRO.EnemyModel[emix] do
                  if (Kind = mkDiplomat) or (Domain = dAir) or (ATrans_Fuel > 0)
                    or (Cap and (1 shl (mcRadar - mcFirstNonCap) or
                    1 shl (mcAcademy - mcFirstNonCap)) <> 0) or
                    (MyMap[ToLoc] and fTerrain = fMountains) or
                    (MyMap[ToLoc] and fTerImp = tiFort) or
                    (MyMap[ToLoc] and fTerImp = tiBase) then
                    CurrentMoveInfo.AfterMovePaintRadius := 2
                  else
                    CurrentMoveInfo.AfterMovePaintRadius := 1;
              end
              else
              begin
                if mEnFastMoves.Checked then
                  AnimationSpeed := 8
                else
                  AnimationSpeed := 16;
                CurrentMoveInfo.AfterMovePaintRadius := 0;
                // enemy unit, nothing discovered
              end;
              if GameMode = cMovie then
              begin
                if MovieSpeed = 3 then
                  AnimationSpeed := 4
                else if MovieSpeed = 2 then
                  AnimationSpeed := 8
                else
                  AnimationSpeed := 16;
              end;
              ShowMoveDomain := MyRO.EnemyModel[emix].Domain;
              IsAlpine := MyRO.EnemyModel[emix].Cap and
                (1 shl (mcAlpine - mcFirstNonCap)) <> 0;
            end
          end;

          if CurrentMoveInfo.DoShow then
          begin
            if Command = cShowCapturing then
              Play('MOVE_CAPTURE')
            else if EndHealth <= 0 then
              Play('MOVE_DIE')
            else if Flags and umSpyMission <> 0 then
              Play('MOVE_COVERT')
            else if Flags and umShipLoading <> 0 then
              if ShowMoveDomain = dAir then
                Play('MOVE_PLANELANDING')
              else
                Play('MOVE_LOAD')
            else if Flags and umPlaneLoading <> 0 then
              Play('MOVE_LOAD')
            else if Flags and umShipUnloading <> 0 then
              if ShowMoveDomain = dAir then
                Play('MOVE_PLANESTART')
              else
                Play('MOVE_UNLOAD')
            else if Flags and umPlaneUnloading <> 0 then
              if (MyMap[FromLoc] and fCity = 0) and
                (MyMap[FromLoc] and fTerImp <> tiBase) then
                Play('MOVE_PARACHUTE')
              else
                Play('MOVE_UNLOAD')
            else if (ShowMoveDomain = dGround) and not IsAlpine and
              (MyMap[ToLoc] and fTerrain = fMountains) and
              ((MyMap[FromLoc] and (fRoad or fRR or fCity) = 0) or
              (MyMap[ToLoc] and (fRoad or fRR or fCity) = 0)) then
              Play('MOVE_MOUNTAIN');

            FocusOnLoc(FromLoc, flImmUpdate);
            PaintLoc_BeforeMove(FromLoc);
            if Command = cShowCapturing then
              MoveOnScreen(TShowMove(Data), 1, 32, 32)
            else
              MoveOnScreen(TShowMove(Data), 1, AnimationSpeed, AnimationSpeed)
          end // if CurrentMoveInfo.DoShow
          else
            MapValid := False;
        end;
      end;

    cShowAttacking:
      if (Idle and (NewPlayer = Me) or not Idle and not skipped and
        (TShowMove(Data).emix <> $FFFF)) and
        not((GameMode = cMovie) and (MovieSpeed = 4)) then
      begin
        Assert(NewPlayer = Me);
        if not Idle or (GameMode = cMovie) then
          Application.ProcessMessages;
        with TShowMove(Data) do
        begin
          CurrentMoveInfo.AfterAttackExpeller := -1;
          CurrentMoveInfo.DoShow := False;
          if Idle then
            CurrentMoveInfo.DoShow := True // own unit -- always show attacks
          else
          begin
            CurrentMoveInfo.IsAlly := MyRO.Treaty[Owner] = trAlliance;
            if CurrentMoveInfo.IsAlly then
              CurrentMoveInfo.DoShow := not mAlNoMoves.Checked
            else
              CurrentMoveInfo.DoShow := not mEnNoMoves.Checked;
          end;
          if CurrentMoveInfo.DoShow then
          begin
            ToLoc := dLoc(FromLoc, dx, dy);
            if not Assigned(Tribe[Owner].ModelPicture[mix].HGr) then
              InitEnemyModel(emix);

            if (MyMap[ToLoc] and (fCity or fUnit or fOwned) = fCity or fOwned)
            then
            begin // tell about bombardment
              cix := MyRO.nCity - 1;
              while (cix >= 0) and (MyCity[cix].Loc <> ToLoc) do
                Dec(cix);
              if MyCity[cix].Status and csToldBombard = 0 then
              begin
                if not Supervising then
                  MyCity[cix].Status := MyCity[cix].Status or csToldBombard;
                S := CityName(MyCity[cix].ID);
                SoundMessageEx(Format(Tribe[Owner].TPhrase('BOMBARD'),
                  [S]), '');
                Update; // remove message box from screen
              end;
            end
            else if Flags and umExpelling <> 0 then
              CurrentMoveInfo.AfterAttackExpeller := Owner;

            if Flags and umExpelling <> 0 then
              Play('MOVE_EXPEL')
            else if Owner = Me then
            begin
              MakeModelInfo(Me, mix, MyModel[mix], mi);
              Play(AttackSound(ModelCode(mi)));
            end
            else
              Play(AttackSound(ModelCode(MyRO.EnemyModel[emix])));

            FocusOnLoc(FromLoc, flImmUpdate);

            // before combat
            MainMap.AttackBegin(TShowMove(Data));
            if MyMap[ToLoc] and fCity <> 0 then
              PaintLoc(ToLoc);
            PaintLoc(FromLoc);
            MoveOnScreen(TShowMove(Data), 1, 9, 16);
            MoveOnScreen(TShowMove(Data), 17, 12, 32);
            MoveOnScreen(TShowMove(Data), 7, 11, 16);

            // after combat
            MainMap.AttackEffect(TShowMove(Data));
            PaintLoc(ToLoc);
            if EndHealth > 0 then
            begin
              Health := EndHealth;
              MoveOnScreen(TShowMove(Data), 10, 0, 16);
            end
            else if not Idle then
              Sleep(MoveTime div 2);
            MainMap.AttackEnd;
          end // if CurrentMoveInfo.DoShow
          else
            MapValid := False;
        end;
      end;

    cShowMissionResult:
      if Cardinal(Data) = 0 then
        SoundMessageEx(Phrases.Lookup('NOFOREIGNINFO'), '')
      else
      begin
        S := Phrases.Lookup('FOREIGNINFO');
        for p1 := 0 to nPl - 1 do
          if 3 shl (p1 * 2) and Cardinal(Data) <> 0 then
            S := S + '\' + Tribe[p1].TPhrase('SHORTNAME');
        SoundMessageEx(S, '');
      end;

    cShowShipChange:
      if not IsMultiPlayerGame and (Jump[0] = 0) then
        ShowEnemyShipChange(TShowShipChange(Data));

    cShowGreatLibTech:
      if not IsMultiPlayerGame and (Jump[0] = 0) then
        with MessgExDlg do
        begin
          MessgText := Format(Phrases.Lookup('GRLIB_GENERAL'),
            [Phrases.Lookup('ADVANCES', Integer(Data))]);
          OpenSound := 'NEWADVANCE_GRLIB';
          Kind := mkOk;
          IconKind := mikImp;
          IconIndex := woGrLibrary;
          ShowModal;
        end;

    cRefreshDebugMap:
      begin
        if Integer(Data) = MainMap.pDebugMap then
        begin
          MapValid := False;
          MainOffscreenPaint;
          Update;
        end;
      end;

  else
    if Command >= cClientEx then
      case Command and (not Integer(CommandDataElementCountMask)) of
        cSetTribe:
          with TTribeInfo(Data) do begin
            I := UnusedTribeFiles.Count - 1;
            while (I >= 0) and
              (AnsiCompareFileName(UnusedTribeFiles[I], FileName) <> 0) do
              Dec(I);
            if I >= 0 then
              UnusedTribeFiles.Delete(I);
            CreateTribe(trix, FileName, True);
          end;
        cSetNewModelPicture:
          if TribeOriginal[TModelPictureInfo(Data).trix] then
            Tribe[TModelPictureInfo(Data).trix].SetModelPicture
              (TModelPictureInfo(Data), True);
        cSetModelPicture:
          if TribeOriginal[TModelPictureInfo(Data).trix] then
            Tribe[TModelPictureInfo(Data).trix].SetModelPicture
              (TModelPictureInfo(Data), False);
        cSetSlaveIndex:
          Tribe[Integer(Data) shr 16].mixSlaves := Integer(Data) and $FFFF;
        cSetCityName:
          with TCityNameInfo(Data) do
            if TribeOriginal[ID shr 12] then
              Tribe[ID shr 12].SetCityName(ID and $FFF, NewName);
        cSetModelName:
          with TModelNameInfo(Data) do
            if TribeOriginal[NewPlayer] then
              Tribe[NewPlayer].ModelName[mix] := NewName;
      end;
  end;
end;

{ *** main part *** }

procedure TMainScreen.FormCreate(Sender: TObject);
var
  I, J: Integer;
begin
  NoMap := TIsoMap.Create;
  MainMap := TIsoMap.Create;
  NoMapPanel := TIsoMap.Create;

  UpdateKeyShortcuts;

  MainFormKeyDown := FormKeyDown;
  BaseWin.CreateOffscreen(Offscreen);

  // define which menu settings to save
  SetLength(SaveOption, 22);
  SaveOption[0] := mAlEffectiveMovesOnly.Tag;
  SaveOption[1] := mEnMoves.Tag;
  SaveOption[2] := mEnAttacks.Tag;
  SaveOption[3] := mEnNoMoves.Tag;
  SaveOption[4] := mWaitTurn.Tag;
  SaveOption[5] := mEffectiveMovesOnly.Tag;
  SaveOption[6] := mEnFastMoves.Tag;
  SaveOption[7] := mSlowMoves.Tag;
  SaveOption[8] := mFastMoves.Tag;
  SaveOption[9] := mVeryFastMoves.Tag;
  SaveOption[10] := mNames.Tag;
  SaveOption[11] := mRepList.Tag;
  SaveOption[12] := mRepScreens.Tag;
  SaveOption[13] := mSoundOff.Tag;
  SaveOption[14] := mSoundOn.Tag;
  SaveOption[15] := mSoundOnAlt.Tag;
  SaveOption[16] := mScrollSlow.Tag;
  SaveOption[17] := mScrollFast.Tag;
  SaveOption[18] := mScrollOff.Tag;
  SaveOption[19] := mAlSlowMoves.Tag;
  SaveOption[20] := mAlFastMoves.Tag;
  SaveOption[21] := mAlNoMoves.Tag;

  LoadSettings;

  Screen.Cursors[crImpDrag] := LoadCursor(HInstance, 'DRAG');
  Screen.Cursors[crFlatHand] := LoadCursor(HInstance, 'FLATHAND');

  // tag-controlled language
  for I := 0 to ComponentCount - 1 do
    if Components[I].Tag and $FF <> 0 then
      if Components[I] is TMenuItem then begin
        TMenuItem(Components[I]).Caption := Phrases.Lookup('CONTROLS',
          -1 + Components[I].Tag and $FF);
        for J := 0 to Length(SaveOption) - 1 do
          if Components[I].Tag and $FF = SaveOption[J] then
            TMenuItem(Components[I]).Checked := TSaveOption(J) in OptionChecked;
      end else
      if Components[I] is TButtonBase then begin
        TButtonBase(Components[I]).Hint := Phrases.Lookup('CONTROLS',
          -1 + Components[I].Tag and $FF);
        if (Components[I] is TButtonC) and
          (TButtonC(Components[I]).ButtonIndex <> 1) then
          TButtonC(Components[I]).ButtonIndex :=
            Integer(MapOptionChecked) shr (Components[I].Tag shr 8) and 1 + 2
      end;

  // non-tag-controlled language
  mTechTree.Caption := Phrases2.Lookup('MENU_ADVTREE');
  mViewpoint.Caption := Phrases2.Lookup('MENU_VIEWPOINT');
  if not Phrases2FallenBackToEnglish then
  begin
    MenuArea.Hint := Phrases2.Lookup('BTN_MENU');
    TreasuryArea.Hint := Phrases2.Lookup('TIP_TREASURY');
    ResearchArea.Hint := Phrases.Lookup('SCIENCE');
    ManagementArea.Hint := Phrases2.Lookup('BTN_MANAGE');
  end;
  for I := 0 to mRep.Count - 1 do
  begin
    J := mRep[I].Tag shr 8;
    mRep[I].Caption := CityEventName(J);
    mRep[I].Checked := CityRepMask and (1 shl J) <> 0;
  end;

  MiniMap := TMiniMap.Create;
  Panel := TBitmap.Create;
  Panel.PixelFormat := pf24bit;
  Panel.Canvas.Font.Assign(UniFont[ftSmall]);
  Panel.Canvas.Brush.Style := bsClear;
  TopBar := TBitmap.Create;
  TopBar.PixelFormat := pf24bit;
  TopBar.Canvas.Font.Assign(UniFont[ftNormal]);
  TopBar.Canvas.Brush.Style := bsClear;
  Buffer := TBitmap.Create;
  Buffer.PixelFormat := pf24bit;
  if 2 * lxmax > 3 * xSizeBig then Buffer.width := 2 * lxmax
    else Buffer.width := 3 * xSizeBig;
  if lymax > 3 * ySizeBig then Buffer.height := lymax
    else Buffer.height := 3 * ySizeBig;
  Buffer.Canvas.Font.Assign(UniFont[ftSmall]);
  for I := 0 to nPl - 1 do
    AILogo[I] := nil;
  Canvas.Font.Assign(UniFont[ftSmall]);
  InitButtons;
  EOT.Template := Templates.Data;
end;

procedure TMainScreen.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  MainFormKeyDown := nil;
  FreeAndNil(sb);
  FreeAndNil(TopBar);
  FreeAndNil(MiniMap);
  FreeAndNil(Buffer);
  FreeAndNil(Panel);
  for I := 0 to nPl - 1 do
    if AILogo[I] <> nil then
      FreeAndNil(AILogo[I]);
  FreeAndNil(Offscreen);
  FreeAndNil(MainMap);
  FreeAndNil(NoMap);
  FreeAndNil(NoMapPanel);
end;

procedure TMainScreen.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  MouseLoc: Integer;
begin
  if (MousePos.Y > ClientHeight - MidPanelHeight) and
    (MousePos.Y < ClientHeight) then begin
    if sb.ProcessMouseWheel(WheelDelta) then begin
      PanelPaint;
      Update;
    end;
  end else begin
    if (WheelDelta > 0) and (MainMap.TileSize < High(TTileSize)) then begin
      MouseLoc := LocationOfScreenPixel(MousePos.X, MousePos.Y);
      SetTileSize(Succ(MainMap.TileSize), MouseLoc, Point(MousePos.X, MousePos.Y));
    end
    else if (WheelDelta < 0) and (MainMap.TileSize > Low(TTileSize)) then begin
      MouseLoc := LocationOfScreenPixel(MousePos.X, MousePos.Y);
      SetTileSize(Pred(MainMap.TileSize), MouseLoc, Point(MousePos.X, MousePos.Y));
    end;
  end;
end;

procedure TMainScreen.mAfforestClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jAfforest);
end;

procedure TMainScreen.mAirBaseClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jBase);
end;

procedure TMainScreen.mCanalClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jCanal);
end;

procedure TMainScreen.mCancelClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    DestinationMarkON := False;
    PaintDestination;
    Status := Status and ($FFFF - usRecover - usGoto - usEnhance);
    if Job > jNone then
      Server(sStartJob + jNone shl 4, Me, UnFocus, nil^);
  end;
end;

procedure TMainScreen.mCentreClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with TUn(MyUn[UnFocus]) do begin
    Centre(Loc);
    PaintAllMaps;
  end;
end;

procedure TMainScreen.mcityClick(Sender: TObject);
var
  Loc0: Integer;
  cix: Integer;
  ServerResult: Integer;
begin
  if UnFocus >= 0 then
  with TUn(MyUn[UnFocus]) do begin
    Loc0 := Loc;
    if MyMap[Loc] and fCity = 0 then
    begin // build city
      if DoJob(jCity) = eCity then
      begin
        MapValid := False;
        PaintAll;
        ZoomToCity(Loc0, True, chFounded);
      end;
    end else begin
      CityOptimizer_BeforeRemoveUnit(UnFocus);
      ServerResult := Server(sAddToCity, Me, UnFocus, nil^);
      if ServerResult >= rExecuted then
      begin
        cix := MyRO.nCity - 1;
        while (cix >= 0) and (MyCity[cix].Loc <> Loc0) do
          Dec(cix);
        Assert(cix >= 0);
        CityOptimizer_CityChange(cix);
        CityOptimizer_AfterRemoveUnit; // does nothing here
        SetTroopLoc(Loc0);
        UpdateViews(True);
        DestinationMarkON := False;
        PaintDestination;
        UnFocus := -1;
        PaintLoc(Loc0);
        NextUnit(UnStartLoc, True);
      end
      else if ServerResult = eMaxSize then
        SimpleMessage(Phrases.Lookup('ADDTOMAXSIZE'));
    end;
  end;
end;

procedure TMainScreen.mCityStatClick(Sender: TObject);
begin
  ListDlg.ShowNewContent(wmPersistent, kCities);
end;

procedure TMainScreen.mCityTypesClick(Sender: TObject);
begin
  CityTypeDlg.ShowNewContent(wmModal);
  // must be modal because types are not saved before closing
end;

procedure TMainScreen.mClearClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jClear);
end;

procedure TMainScreen.mDiagramClick(Sender: TObject);
begin
  DiaDlg.ShowNewContent_Charts(wmPersistent);
end;

procedure TMainScreen.mEmpireClick(Sender: TObject);
begin
  RatesDlg.ShowNewContent(wmPersistent);
end;

procedure TMainScreen.mEnhanceClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(-1);
end;

procedure TMainScreen.mEnhanceDefClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    EnhanceDlg.ShowNewContent(wmPersistent,
      MyMap[MyUn[UnFocus].Loc] and fTerrain)
  else
    EnhanceDlg.ShowNewContent(wmPersistent);
end;

procedure TMainScreen.mEUnitStatClick(Sender: TObject);
begin
  if MyRO.nEnemyModel > 0 then
    ListDlg.ShowNewContent(wmPersistent, kAllEModels);
end;

procedure TMainScreen.mFarmClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jFarm);
end;

procedure TMainScreen.mfortClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jFort);
end;

procedure TMainScreen.mGoOnClick(Sender: TObject);
var
  Destination: Integer;
begin
  if UnFocus >= 0 then
  with TUn(MyUn[UnFocus]) do begin
    if Status shr 16 = $7FFF then
      Destination := maNextCity
    else
      Destination := Status shr 16;
    Status := Status and not(usStay or usRecover) or usWaiting;
    MoveToLoc(Destination, True);
  end;
end;

procedure TMainScreen.mHelpClick(Sender: TObject);
begin
  if ClientMode = cEditMap then
    HelpDlg.ShowNewContent(wmPersistent, hkText, HelpDlg.TextIndex('MAPEDIT'))
  else
    HelpDlg.ShowNewContent(wmPersistent, hkMisc, miscMain);
end;

procedure TMainScreen.mhomeClick(Sender: TObject);
var
  cixOldHome: Integer;
begin
  if UnFocus >= 0 then
  with TUn(MyUn[UnFocus]) do begin
    if MyMap[Loc] and fCity <> 0 then
    begin
      cixOldHome := Home;
      if Server(sSetUnitHome, Me, UnFocus, nil^) >= rExecuted then
      begin
        CityOptimizer_CityChange(cixOldHome);
        CityOptimizer_CityChange(Home);
        UpdateViews(True);
      end
      else
        Play('INVALID');
    end
    else
    begin
      Status := Status and not(usStay or usRecover or usEnhance);
      MoveToLoc(maNextCity, True);
    end;
  end;
end;

procedure TMainScreen.mirrigationClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
        DoJob(jIrr);
end;

procedure TMainScreen.mirrigationDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
begin

end;

procedure TMainScreen.mJumpClick(Sender: TObject);
begin
  if Supervising then
    Jump[0] := 20
  else
    Jump[Me] := 20;
  EndTurn(True);
end;

procedure TMainScreen.mLoadClick(Sender: TObject);
var
  I: Integer;
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    I := Server(sLoadUnit, Me, UnFocus, nil^);
    if I >= rExecuted then
    begin
      if MyModel[mix].Domain = dAir then
        Play('MOVE_PLANELANDING')
      else
        Play('MOVE_LOAD');
      DestinationMarkON := False;
      PaintDestination;
      Status := Status and ($FFFF - usWaiting - usStay - usRecover - usGoto - usEnhance);
      NextUnit(UnStartLoc, True);
    end
    else if I = eNoTime_Load then
      if MyModel[mix].Domain = dAir then
        SoundMessage(Phrases.Lookup('NOTIMELOADAIR'), 'NOMOVE_TIME')
      else
        SoundMessage(Format(Phrases.Lookup('NOTIMELOADGROUND'),
          [MovementToString(MyModel[mix].speed)]), 'NOMOVE_TIME');
  end;
end;

procedure TMainScreen.mmineClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jMine);
end;

procedure TMainScreen.mNationsClick(Sender: TObject);
begin
  NatStatDlg.ShowNewContent(wmPersistent);
end;

procedure TMainScreen.mNextUnitClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    Status := Status and not usWaiting;
    FocusNextUnit(1);
  end;
end;

procedure TMainScreen.mnoordersClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    Status := Status and not usWaiting;
    NextUnit(UnStartLoc, True);
  end;
end;

procedure TMainScreen.mPillageClick(Sender: TObject);
begin
  DoJob(jPillage);
end;

procedure TMainScreen.mpollutionClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jPoll);
end;

procedure TMainScreen.mPrevUnitClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    Status := Status and not usWaiting;
    FocusNextUnit(-1);
  end;
end;

procedure TMainScreen.mRandomMapClick(Sender: TObject);
begin
  if not Edited or (SimpleQuery(mkYesNo, Phrases.Lookup('MAP_RANDOM'), '')
      = mrOK) then begin
    Server(sRandomMap, Me, 0, nil^);
    Edited := True;
    MapValid := False;
    PaintAllMaps;
  end;
end;

procedure TMainScreen.mRecoverClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    DestinationMarkON := False;
    PaintDestination;
    Status := Status and ($FFFF - usStay - usGoto - usEnhance) or usRecover;
    if Job > jNone then
      Server(sStartJob + jNone shl 4, Me, UnFocus, nil^);
    NextUnit(UnStartLoc, True);
  end;
end;

procedure TMainScreen.mResignClick(Sender: TObject);
var
  QueryText: string;
begin
  if ClientMode = cEditMap then begin
    if Edited then begin
      QueryText := Phrases.Lookup('MAP_CLOSE');
      case SimpleQuery(mkYesNoCancel, QueryText, '') of
        mrIgnore: Server(sAbandonMap, Me, 0, nil^);
        mrOK: Server(sSaveMap, Me, 0, nil^);
      end;
    end else
      Server(sAbandonMap, Me, 0, nil^);
  end else begin
    if Server(sGetGameChanged, 0, 0, nil^) = eOK then begin
      QueryText := Phrases.Lookup('RESIGN');
      case SimpleQuery(mkYesNoCancel, QueryText, '') of
        mrIgnore: Server(sResign, 0, 0, nil^);
        mrOK: Server(sBreak, 0, 0, nil^);
      end;
    end else
      Server(sResign, 0, 0, nil^);
  end;
end;

procedure TMainScreen.mRevolutionClick(Sender: TObject);
var
  AltGovs: Boolean;
  RevolutionChanged: Boolean;
  I: Integer;
begin
  AltGovs := False;
    for I := 2 to nGov - 1 do
      if (GovPreq[I] <> preNA) and
        ((GovPreq[I] = preNone) or (MyRO.Tech[GovPreq[I]] >= tsApplicable)) then
        AltGovs := True;

    if not AltGovs then
      SoundMessage(Phrases.Lookup('NOALTGOVS'), 'MSG_DEFAULT')
    else
    begin
      RevolutionChanged := False;
      if MyRO.Happened and phChangeGov <> 0 then
      begin
        ModalSelectDlg.ShowNewContent(wmModal, kGov);
        if ModalSelectDlg.Result >= 0 then
        begin
          Play('NEWGOV');
          Server(sSetGovernment, Me, ModalSelectDlg.Result, nil^);
          CityOptimizer_BeginOfTurn;
          RevolutionChanged := True;
        end;
      end
      else
      with MessgExDlg do
      begin // revolution!
        MessgExDlg.MessgText := Tribe[Me].TPhrase('REVOLUTION');
        MessgExDlg.Kind := mkYesNo;
        MessgExDlg.IconKind := mikPureIcon;
        MessgExDlg.IconIndex := 72; // anarchy palace
        MessgExDlg.ShowModal;
        if ModalResult = mrOK then
        begin
          Play('REVOLUTION');
          Server(sRevolution, Me, 0, nil^);
          RevolutionChanged := True;
          if NatStatDlg.Visible then
            NatStatDlg.Close;
          if CityDlg.Visible then
            CityDlg.Close;
        end
      end;
      if RevolutionChanged then
        UpdateViews(True);
    end;
end;

procedure TMainScreen.mroadClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jRoad);
end;

procedure TMainScreen.mRailRoadClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jRR);
end;

procedure TMainScreen.mRunClick(Sender: TObject);
begin
  if Supervising then
    Jump[0] := 999999
  else
    Jump[Me] := 999999;
  EndTurn(True);
end;

procedure TMainScreen.mScienceStatClick(Sender: TObject);
begin
  ListDlg.ShowNewContent(wmPersistent, kScience);
end;

procedure TMainScreen.mSelectTransportClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      Server(sSelectTransport, Me, UnFocus, nil^);
end;

procedure TMainScreen.mShipsClick(Sender: TObject);
begin
  DiaDlg.ShowNewContent_Ship(wmPersistent);
end;

procedure TMainScreen.mstayClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with TUn(MyUn[UnFocus]) do begin
    DestinationMarkON := False;
    PaintDestination;
    Status := Status and ($FFFF - usRecover - usGoto - usEnhance) or usStay;
    if Job > jNone then
      Server(sStartJob + jNone shl 4, Me, UnFocus, nil^);
    NextUnit(UnStartLoc, True);
  end;
end;

procedure TMainScreen.mTechTreeClick(Sender: TObject);
begin
  TechTreeDlg.ShowModal;
end;

procedure TMainScreen.mtransClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      DoJob(jTrans);
end;

procedure TMainScreen.mUnitStatClick(Sender: TObject);
var
  I: Integer;
begin
  if G.Difficulty[Me] > 0 then
    ListDlg.ShowNewContent_MilReport(wmPersistent, Me)
  else
  begin
    I := 1;
    while (I < nPl) and (1 shl I and MyRO.Alive = 0) do
      Inc(I);
    if I < nPl then
      ListDlg.ShowNewContent_MilReport(wmPersistent, I);
  end;
end;

procedure TMainScreen.mUnloadClick(Sender: TObject);
var
  I: Integer;
  OldMaster: Integer;
  NewFocus: Integer;
  uix: Integer;
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    if Master >= 0 then begin
      OldMaster := Master;
      I := Server(sUnloadUnit, Me, UnFocus, nil^);
      if I >= rExecuted then
      begin
        if MyModel[mix].Domain = dAir then
          Play('MOVE_PLANESTART')
        else if (MyModel[MyUn[OldMaster].mix].Domain = dAir) and
          (MyMap[Loc] and fCity = 0) and (MyMap[Loc] and fTerImp <> tiBase)
        then
          Play('MOVE_PARACHUTE')
        else
          Play('MOVE_UNLOAD');
        Status := Status and not usWaiting;
        if MyModel[mix].Domain <> dAir then
          NextUnit(Loc, True)
        else
          PanelPaint;
      end
      else if I = eNoTime_Load then
        if MyModel[mix].Domain = dAir then
          SoundMessage(Phrases.Lookup('NOTIMELOADAIR'), 'NOMOVE_TIME')
        else
          SoundMessage(Format(Phrases.Lookup('NOTIMELOADGROUND'),
            [MovementToString(MyModel[mix].speed)]), 'NOMOVE_TIME');
    end else begin
      NewFocus := -1;
      uix := UnFocus;
      for I := 1 to MyRO.nUn - 1 do
      begin
        uix := (uix + MyRO.nUn - 1) mod MyRO.nUn;
        if (MyUn[uix].Master = UnFocus) and
          (MyUn[uix].Movement = Integer(MyModel[MyUn[uix].mix].speed)) then
        begin
          MyUn[uix].Status := MyUn[uix].Status or usWaiting;
          NewFocus := uix;
        end;
      end;
      if NewFocus >= 0 then
      begin
        SetUnFocus(NewFocus);
        SetTroopLoc(Loc);
        PanelPaint;
      end;
    end;
  end;
end;

procedure TMainScreen.mwaitClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    DestinationMarkON := False;
    PaintDestination;
    Status := Status and ($FFFF - usStay - usRecover - usGoto - usEnhance) or usWaiting;
  end;
  NextUnit(-1, False);
end;

procedure TMainScreen.mWebsiteClick(Sender: TObject);
begin
  OpenURL(CevoHomepage);
end;

procedure TMainScreen.mWondersClick(Sender: TObject);
begin
  WondersDlg.ShowNewContent(wmPersistent);
end;

procedure TMainScreen.FormResize(Sender: TObject);
var
  MiniFrame, MaxMapWidth: Integer;
begin
  SmallScreen := ClientWidth < 1024;
  with MainMap do begin
    MaxMapWidth := (G.lx * 2 - 3) * xxt;
    // avoide the same tile being visible left and right
    if ClientWidth <= MaxMapWidth then begin
      MapWidth := ClientWidth;
      MapOffset := 0;
    end else begin
      MapWidth := MaxMapWidth;
      MapOffset := (ClientWidth - MapWidth) div 2;
    end;
    MapHeight := ClientHeight - TopBarHeight - PanelHeight + overlap;
    Panel.SetSize(ClientWidth, PanelHeight);
    TopBar.SetSize(ClientWidth, TopBarHeight);
    MiniFrame := (lxmax_xxx - G.ly) div 2;
    xMidPanel := (G.lx + MiniFrame) * 2 + 1;
    xRightPanel := ClientWidth - LeftPanelWidth - 10;
    if ClientMode = cEditMap then
      TrPitch := 2 * xxt
    else
      TrPitch := 66;
    xMini := MiniFrame - 5;
    yMini := (PanelHeight - 26 - lxmax_xxx) div 2 + MiniFrame;
    ywmax := (G.ly - MapHeight div yyt + 1) and not 1;
    ywcenter := -((MapHeight - yyt * (G.ly - 1)) div (4 * yyt)) * 2;
  end;
  // only for ywmax<=0
  if ywmax <= 0 then
    yw := ywcenter
  else if yw < 0 then
    yw := 0
  else if yw > ywmax then
    yw := ywmax;
  UnitInfoBtn.Top := ClientHeight - 29;
  UnitInfoBtn.Left := xMidPanel + 7 + 99;
  UnitBtn.Top := ClientHeight - 29;
  UnitBtn.Left := xMidPanel + 7 + 99 + 31;
  TerrainBtn.Top := ClientHeight - 29;
  TerrainBtn.Left := xMidPanel + 7 + 99 + 62;
  MovieSpeed1Btn.Top := ClientHeight - 91;
  MovieSpeed1Btn.Left := ClientWidth div 2 - 62;
  MovieSpeed2Btn.Top := ClientHeight - 91;
  MovieSpeed2Btn.Left := ClientWidth div 2 - 62 + 29;
  MovieSpeed3Btn.Top := ClientHeight - 91;
  MovieSpeed3Btn.Left := ClientWidth div 2 - 62 + 2 * 29;
  MovieSpeed4Btn.Top := ClientHeight - 91;
  MovieSpeed4Btn.Left := ClientWidth div 2 - 62 + 3 * 29 + 12;
  EOT.Top := ClientHeight - 64;
  EOT.Left := ClientWidth - 62;
  sb.SetBorderSpacing(ClientHeight - yTroop - 24, ClientWidth - xRightPanel + 8, 8);
  {TODO:
  SetWindowPos(sb.ScrollBar.Handle, 0, xRightPanel + 10 - 14 - GetSystemMetrics(SM_CXVSCROLL),
    ClientHeight - MidPanelHeight + 8, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
    }
  MapBtn0.Left := xMini + G.lx - 44;
  MapBtn0.Top := ClientHeight - 15;
  MapBtn1.Left := xMini + G.lx - 28;
  MapBtn1.Top := ClientHeight - 15;
  { MapBtn2.Left:=xMini+G.lx-20;
    MapBtn2.Top:=ClientHeight-15;
    MapBtn3.Left:=xMini+G.lx-4;
    MapBtn3.Top:=ClientHeight-15; }
  MapBtn5.Left := xMini + G.lx - 12;
  MapBtn5.Top := ClientHeight - 15;
  MapBtn4.Left := xMini + G.lx + 20;
  MapBtn4.Top := ClientHeight - 15;
  MapBtn6.Left := xMini + G.lx + 36;
  MapBtn6.Top := ClientHeight - 15;
  TreasuryArea.Left := ClientWidth div 2 - 172;
  ResearchArea.Left := ClientWidth div 2;
  ManagementArea.Left := ClientWidth - xPalace;
  ManagementArea.Top := TopBarHeight + MapHeight - overlap + yPalace;
  ArrangeMidPanel;
  if RepaintOnResize then
  begin
    RectInvalidate(0, TopBarHeight, ClientWidth, TopBarHeight + MapHeight);
    MapValid := False;
    PaintAll;
  end;
end;

procedure TMainScreen.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Closable;
  if not Closable and Idle and (Me = 0) and (ClientMode < scContact) then
    mResign.Click;
end;

procedure TMainScreen.OnScroll(var Msg: TMessage);
begin
  if sb.Process(Msg) then begin
    PanelPaint;
    Update;
  end;
end;

procedure TMainScreen.OnEOT(var Msg: TMessage);
begin
  EndTurn;
end;

procedure TMainScreen.EOTClick(Sender: TObject);
begin
  if GameMode = cMovie then
  begin
    MessgExDlg.CancelMovie;
    Server(sBreak, Me, 0, nil^);
  end
  else if ClientMode < 0 then
    skipped := True
  else if ClientMode >= scContact then
    NegoDlg.ShowNewContent(wmPersistent)
  else if Jump[pTurn] > 0 then
  begin
    Jump[pTurn] := 0;
    StartRunning := False;
  end
  else
    EndTurn;
end;

// set xTerrain, xTroop, and TrRow
procedure TMainScreen.ArrangeMidPanel;
begin
  if ClientMode = cEditMap then
    xTroop := xMidPanel + 15
  else
  with MainMap do begin
    if Supervising then
      xTerrain := xMidPanel + 2 * xxt + 14
    else if ClientWidth < 1280 then
      xTerrain := ClientWidth div 2 + (1280 - ClientWidth) div 3
    else
      xTerrain := ClientWidth div 2;
    xTroop := xTerrain + 2 * xxt + 12;
    if SmallScreen and not Supervising then
      xTroop := xRightPanel + 10 - 3 * 66 -
        GetSystemMetrics(SM_CXVSCROLL) - 19 - 4;
    // not perfect but we assume almost no one is still playing on a 800x600 screen
  end;
  TrRow := (xRightPanel + 10 - xTroop - GetSystemMetrics(SM_CXVSCROLL) - 19)
    div TrPitch;
end;

function TMainScreen.EndTurn(WasSkipped: Boolean): Boolean;

  function IsResourceUnused(cix, NeedFood, NeedProd: Integer): Boolean;
  var
    dx, dy, fix: Integer;
    CityAreaInfo: TCityAreaInfo;
    TileInfo: TTileInfo;
  begin
    Server(sGetCityAreaInfo, Me, cix, CityAreaInfo);
    for dy := -3 to 3 do
      for dx := -3 to 3 do
        if ((dx + dy) and 1 = 0) and (dx * dx * dy * dy < 81) then
        begin
          fix := (dy + 3) shl 2 + (dx + 3) shr 1;
          if (MyCity[cix].Tiles and (1 shl fix) = 0) // not used yet
            and (CityAreaInfo.Available[fix] = faAvailable) then // usable
          begin
            TileInfo.ExplCity := cix;
            Server(sGetHypoCityTileInfo, Me, dLoc(MyCity[cix].Loc, dx, dy),
              TileInfo);
            if (TileInfo.Food >= NeedFood) and (TileInfo.Prod >= NeedProd) then
            begin
              Result := True;
              Exit
            end;
          end
        end;
    Result := False;
  end;

var
  p1, uix, cix, CenterLoc: Integer;
  MsgItem: string;
  CityReport: TCityReport;
  PlaneReturnData: TPlaneReturnData;
  Zoom: Boolean;
begin
  Result := False;
  if ClientMode >= scDipOffer then
    Exit;

  if Supervising and (Me <> 0) then begin
    ApplyToVisibleForms(faClose);
    ItsMeAgain(0);
  end;

  CityOptimizer_EndOfTurn;

  if not WasSkipped then // check warnings
  begin
    // need to move planes home?
    for uix := 0 to MyRO.nUn - 1 do
      with MyUn[uix] do
        if (Loc >= 0) and (MyModel[mix].Domain = dAir) and
          (Status and usToldNoReturn = 0) and (Master < 0) and
          (MyMap[Loc] and fCity = 0) and (MyMap[Loc] and fTerImp <> tiBase) then
        begin
          PlaneReturnData.Fuel := Fuel;
          PlaneReturnData.Loc := Loc;
          PlaneReturnData.Movement := 0; // end turn without further movement?
          if Server(sGetPlaneReturn, Me, uix, PlaneReturnData) = eNoWay then
          begin
            CenterLoc := Loc + G.lx * 6;
            // centering the unit itself would make it covered by the query dialog
            while CenterLoc >= G.lx * G.ly do
              Dec(CenterLoc, G.lx * 2);
            Centre(CenterLoc);
            SetTroopLoc(-1);
            PaintAll;

            if MyModel[mix].Kind = mkSpecial_Glider then
              MsgItem := 'LOWFUEL_GLIDER'
            else
              MsgItem := 'LOWFUEL';
            if SimpleQuery(mkYesNo, Phrases.Lookup(MsgItem),
              'WARNING_LOWSUPPORT') <> mrOK then
            begin
              SetUnFocus(uix);
              SetTroopLoc(Loc);
              PanelPaint;
              Exit;
            end;
            MyUn[uix].Status := MyUn[uix].Status or usToldNoReturn;
          end;
        end;

    if not Supervising and (MyRO.TestFlags and tfImmImprove = 0) and
      (MyRO.Government <> gAnarchy) and (MyRO.Money + TaxSum < 0) and
      (MyRO.TaxRate < 100) then // low funds!
      with MessgExDlg do
      begin
        OpenSound := 'WARNING_LOWFUNDS';
        MessgText := Phrases.Lookup('LOWFUNDS');
        Kind := mkYesNo;
        IconKind := mikImp;
        IconIndex := imTrGoods;
        ShowModal;
        if ModalResult <> mrOK then
          Exit;
      end;

    if MyRO.Government <> gAnarchy then
      for cix := 0 to MyRO.nCity - 1 do
        with MyCity[cix] do
          if (Loc >= 0) and (Flags and chCaptured = 0) then
          begin
            Zoom := False;
            CityReport.HypoTiles := -1;
            CityReport.HypoTax := -1;
            CityReport.HypoLux := -1;
            Server(sGetCityReport, Me, cix, CityReport);

            if (CityReport.Working - CityReport.Happy > Size shr 1) and
              (Flags and chCaptured <= $10000) then
              with MessgExDlg do
              begin
                OpenSound := 'WARNING_DISORDER';
                if Status and csResourceWeightsMask = 0 then
                  MsgItem := 'DISORDER'
                else
                  MsgItem := 'DISORDER_UNREST';
                MessgText := Format(Phrases.Lookup(MsgItem), [CityName(ID)]);
                Kind := mkYesNo;
                // BigIcon:=29;
                ShowModal;
                Zoom := ModalResult <> mrOK;
              end;
            if not Zoom and (Food + CityReport.FoodRep - CityReport.Eaten < 0)
            then
              with MessgExDlg do
              begin
                OpenSound := 'WARNING_FAMINE';
                if Status and csResourceWeightsMask = 0 then
                  MsgItem := 'FAMINE'
                else if (CityReport.Deployed <> 0) and
                  IsResourceUnused(cix, 1, 0) then
                  MsgItem := 'FAMINE_UNREST'
                else
                  MsgItem := 'FAMINE_TILES';
                MessgText := Format(Phrases.Lookup(MsgItem), [CityName(ID)]);
                Kind := mkYesNo;
                IconKind := mikImp;
                IconIndex := 22;
                ShowModal;
                Zoom := ModalResult <> mrOK;
              end;
            if not Zoom and (CityReport.ProdRep < CityReport.Support) then
              with MessgExDlg do
              begin
                OpenSound := 'WARNING_LOWSUPPORT';
                if Status and csResourceWeightsMask = 0 then
                  MsgItem := 'LOWSUPPORT'
                else if (CityReport.Deployed <> 0) and
                  IsResourceUnused(cix, 0, 1) then
                  MsgItem := 'LOWSUPPORT_UNREST'
                else
                  MsgItem := 'LOWSUPPORT_TILES';
                MessgText := Format(Phrases.Lookup(MsgItem), [CityName(ID)]);
                Kind := mkYesNo;
                IconKind := mikImp;
                IconIndex := 29;
                ShowModal;
                Zoom := ModalResult <> mrOK;
              end;
            if Zoom then
            begin // zoom to city
              ZoomToCity(Loc);
              Exit;
            end;
          end;

    if (MyRO.Happened and phTech <> 0) and (MyRO.ResearchTech < 0) and
      (MyData.FarTech <> adNexus) then
      if not ChooseResearch then
        Exit;
  end;

  RememberPeaceViolation;

  SetUnFocus(-1);
  for uix := 0 to MyRO.nUn - 1 do
    MyUn[uix].Status := MyUn[uix].Status and usPersistent;

  CityDlg.CloseAction := None;
  if IsMultiPlayerGame then begin
    // Close windows for next player
    ApplyToVisibleForms(faClose);
  end else begin
    if CityDlg.Visible then
      CityDlg.Close;
    if UnitStatDlg.Visible then
      UnitStatDlg.Close;
  end;
  ApplyToVisibleForms(faDisable);

  if Server(sTurn, pTurn, 0, nil^) >= rExecuted then
  begin
    if Jump[pTurn] > 0 then
      EOT.Hint := Phrases.Lookup('BTN_STOP')
    else
      EOT.Hint := Phrases.Lookup('BTN_SKIP');
    Result := True;
    SetTroopLoc(-1);
    pTurn := -1;
    pLogo := -1;
    UnitInfoBtn.Visible := False;
    UnitBtn.Visible := False;
    TerrainBtn.Visible := False;
    EOT.ButtonIndex := eotCancel;
    EOT.Visible := True;
    MapValid := False;
    PanelPaint;
    Update;
    ClientMode := -1;
    Idle := False;
    skipped := WasSkipped;
    for p1 := 1 to nPl - 1 do
      if G.RO[p1] <> nil then
        skipped := True; // don't show enemy moves in hotseat mode
  end
  else
    PanelPaint;
end;

procedure TMainScreen.EndNego;
begin
  if NegoDlg.Visible then
    NegoDlg.Close;
  HaveStrategyAdvice := False;
  // AdvisorDlg.HaveStrategyAdvice;
  // negotiation might have changed advices
  EOT.ButtonIndex := eotCancel;
  EOT.Visible := True;
  PanelPaint;
  Update;
  ClientMode := -1;
  Idle := False;
end;

procedure TMainScreen.ProcessRect(x0, y0, nx, ny, Options: Integer);
var
  xs, ys, xl, yl: Integer;
begin
  with MainMap do begin
    xl := nx * xxt + xxt;
    yl := ny * yyt + yyt * 2;
    xs := (x0 - xw) * (xxt * 2) + y0 and 1 * xxt - G.lx * (xxt * 2);
    // |xs+xl/2-MapWidth/2| -> min
    while abs(2 * (xs + G.lx * (xxt * 2)) + xl - MapWidth) <
      abs(2 * xs + xl - MapWidth) do
        Inc(xs, G.lx * (xxt * 2));
    ys := (y0 - yw) * yyt - yyt;
    if xs + xl > MapWidth then
      xl := MapWidth - xs;
    if ys + yl > MapHeight then
      yl := MapHeight - ys;
    if (xl <= 0) or (yl <= 0) then
      Exit;
    if Options and prPaint <> 0 then begin
      if Options and prAutoBounds <> 0 then
        MainMap.SetPaintBounds(xs, ys, xs + xl, ys + yl);
      MainMap.Paint(xs, ys, x0 + G.lx * y0, nx, ny, -1, -1);
    end;
    if Options and prInvalidate <> 0 then
      RectInvalidate(MapOffset + xs, TopBarHeight + ys, MapOffset + xs + xl,
        TopBarHeight + ys + yl)
  end;
end;

procedure TMainScreen.PaintLoc(Loc: Integer; Radius: Integer = 0);
var
  yLoc, x0: Integer;
begin
  if MapValid then begin
    yLoc := (Loc + G.lx * 1024) div G.lx - 1024;
    x0 := (Loc + (yLoc and 1 - 2 * Radius + G.lx * 1024) div 2) mod G.lx;
    Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
    ProcessRect(x0, yLoc - 2 * Radius, 4 * Radius + 1, 4 * Radius + 1,
      prPaint or prAutoBounds or prInvalidate);
    Update;
  end;
end;

procedure TMainScreen.PaintLocTemp(Loc: Integer; Style: TPaintLocTempStyle);
var
  y0, x0, xMap, yMap: Integer;
begin
  with NoMap do begin
    if not MapValid then
      Exit;
    Buffer.Canvas.Font.Assign(UniFont[ftSmall]);
    y0 := Loc div G.lx;
    x0 := Loc mod G.lx;
    xMap := (x0 - xw) * (xxt * 2) + y0 and 1 * xxt - G.lx * (xxt * 2);
    // |xMap+xxt-MapWidth/2| -> min
    while abs(2 * (xMap + G.lx * (xxt * 2)) + 2 * xxt - MapWidth) <
      abs(2 * xMap + 2 * xxt - MapWidth) do
      Inc(xMap, G.lx * (xxt * 2));
    yMap := (y0 - yw) * yyt - yyt;
    NoMap.SetOutput(Buffer);
    NoMap.SetPaintBounds(0, 0, 2 * xxt, 3 * yyt);
    NoMap.Paint(0, 0, Loc, 1, 1, -1, -1, Style = pltsBlink);
    PaintBufferToScreen(xMap, yMap, 2 * xxt, 3 * yyt);
  end;
end;

// paint content of buffer directly to screen instead of offscreen
// panel protusions are added
// NoMap must be set to buffer and bounds before
procedure TMainScreen.PaintBufferToScreen(xMap, yMap, width, height: Integer);
begin
  if xMap + width > MapWidth then
    width := MapWidth - xMap;
  if yMap + height > MapHeight then
    height := MapHeight - yMap;
  if (width <= 0) or (height <= 0) or (width + xMap <= 0) or (height + yMap <= 0)
  then
    Exit;

  NoMap.BitBltBitmap(Panel, -xMap - MapOffset, -yMap + MapHeight - overlap, xMidPanel,
    overlap, 0, 0, SRCCOPY);
  NoMap.BitBltBitmap(Panel, -xMap - MapOffset + xRightPanel,
    -yMap + MapHeight - overlap, Panel.width - xRightPanel, overlap,
    xRightPanel, 0, SRCCOPY);
  if yMap < 0 then
  begin
    if xMap < 0 then
      BitBltCanvas(Canvas, MapOffset, TopBarHeight, width + xMap,
        height + yMap, Buffer.Canvas, -xMap, -yMap)
    else
      BitBltCanvas(Canvas, xMap + MapOffset, TopBarHeight, width,
        height + yMap, Buffer.Canvas, 0, -yMap);
  end
  else
  begin
    if xMap < 0 then
      BitBltCanvas(Canvas, MapOffset, TopBarHeight + yMap, width + xMap,
        height, Buffer.Canvas, -xMap, 0)
    else
      BitBltCanvas(Canvas, xMap + MapOffset, TopBarHeight + yMap, width,
        height, Buffer.Canvas, 0, 0);
  end;
end;

procedure TMainScreen.PaintLoc_BeforeMove(FromLoc: Integer);
var
  yLoc, x0: Integer;
begin
  if MapValid then
  begin
    yLoc := (FromLoc + G.lx * 1024) div G.lx - 1024;
    x0 := (FromLoc + (yLoc and 1 + G.lx * 1024) div 2) mod G.lx;
    Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
    ProcessRect(x0, yLoc, 1, 1, prPaint or prAutoBounds);
  end
end;

procedure TMainScreen.PaintDestination;
var
  Destination: Integer;
begin
  if (UnFocus >= 0) and (MyUn[UnFocus].Status and usGoto <> 0) then
  begin
    Destination := MyUn[UnFocus].Status shr 16;
    if (Destination <> $7FFF) and (Destination <> MyUn[UnFocus].Loc) then
      PaintLocTemp(Destination, pltsBlink);
  end;
end;

{$IFDEF UNIX}
// Can't do scrolling of DC under Linux, then fallback into BitBlt.
function ScrollDC(Canvas: TCanvas; dx: longint; dy: longint; const lprcScroll:TRect; const lprcClip:TRect; hrgnUpdate:HRGN; lprcUpdate: PRect):Boolean;
begin
  Result := BitBltCanvas(Canvas, lprcScroll.Left + dx, lprcScroll.Top + dy, lprcScroll.Right - lprcScroll.Left, lprcScroll.Bottom - lprcScroll.Top,
    Canvas, lprcScroll.Left, lprcScroll.Top);
end;
{$ENDIF}

procedure TMainScreen.MainOffscreenPaint;
var
  ProcessOptions: Integer;
  rec: TRect;
  DoInvalidate: Boolean;
begin
  if Me < 0 then
    with Offscreen.Canvas do
    begin
      Brush.Color := $000000;
      FillRect(Rect(0, 0, MapWidth, MapHeight));
      Brush.Style := bsClear;
      OffscreenUser := self;
      Exit;
    end;

  MainMap.SetPaintBounds(0, 0, MapWidth, MapHeight);
  if OffscreenUser <> self then
  begin
    if OffscreenUser <> nil then
      OffscreenUser.Update;
    // complete working with old owner to prevent rebound
    if MapValid and (xwd = xw) and (ywd = yw) then
      MainMap.SetPaintBounds(0, 0, UsedOffscreenWidth, UsedOffscreenHeight);
    MapValid := False;
    OffscreenUser := self;
  end;

  with MainMap do begin
    if xw - xwd > G.lx div 2 then
      xwd := xwd + G.lx
    else if xwd - xw > G.lx div 2 then
      xwd := xwd - G.lx;
    if not MapValid or (xw - xwd > MapWidth div (xxt * 2)) or
      (xwd - xw > MapWidth div (xxt * 2)) or (yw - ywd > MapHeight div yyt) or
      (ywd - yw > MapHeight div yyt) then
    begin
      Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
      ProcessRect(xw, yw, MapWidth div xxt, MapHeight div yyt,
        prPaint or prInvalidate);
    end else begin
      if (xwd = xw) and (ywd = yw) then
        Exit; { map window not moved }
      Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
      rec := Rect(0, 0, MapWidth, MapHeight);
{$IFDEF WINDOWS}
      ScrollDC(Offscreen.Canvas.Handle, (xwd - xw) * (xxt * 2), (ywd - yw) * yyt,
        rec, rec, 0, nil);
{$ENDIF}
{$IFDEF UNIX}
      ScrollDC(Offscreen.Canvas, (xwd - xw) * (xxt * 2), (ywd - yw) * yyt,
        rec, rec, 0, nil);
{$ENDIF}
      for DoInvalidate := False to FastScrolling do begin
        if DoInvalidate then begin
          rec.Bottom := MapHeight - overlap;
{$IFDEF WINDOWS}
          ScrollDC(Canvas.Handle, (xwd - xw) * (xxt * 2), (ywd - yw) * yyt, rec,
            rec, 0, nil);
{$ENDIF}
{$IFDEF UNIX}
          ScrollDC(Canvas, (xwd - xw) * (xxt * 2), (ywd - yw) * yyt,
            rec, rec, 0, nil);
{$ENDIF}
          ProcessOptions := prInvalidate;
        end
        else ProcessOptions := prPaint or prAutoBounds;
        if yw < ywd then begin
          ProcessRect(xw, yw, MapWidth div xxt, ywd - yw - 1, ProcessOptions);
          if xw < xwd then
            ProcessRect(xw, ywd, (xwd - xw) * 2 - 1, MapHeight div yyt - ywd + yw,
              ProcessOptions)
          else if xw > xwd then
            ProcessRect((xwd + MapWidth div (xxt * 2)) mod G.lx, ywd,
              (xw - xwd) * 2 + 1, MapHeight div yyt - ywd + yw, ProcessOptions)
        end
        else if yw > ywd then begin
          if DoInvalidate then
            RectInvalidate(MapOffset, TopBarHeight + MapHeight - overlap -
              (yw - ywd) * yyt, MapOffset + MapWidth, TopBarHeight + MapHeight
              - overlap)
          else
            ProcessRect(xw, (ywd + MapHeight div (yyt * 2) * 2), MapWidth div xxt,
              yw - ywd + 1, ProcessOptions);
          if xw < xwd then
            ProcessRect(xw, yw, (xwd - xw) * 2 - 1, MapHeight div yyt - yw + ywd -
              2, ProcessOptions)
          else if xw > xwd then
            ProcessRect((xwd + MapWidth div (xxt * 2)) mod G.lx, yw,
              (xw - xwd) * 2 + 1, MapHeight div yyt - yw + ywd - 2,
              ProcessOptions);
        end
        else if xw < xwd then
          ProcessRect(xw, yw, (xwd - xw) * 2 - 1, MapHeight div yyt,
            ProcessOptions)
        else if xw > xwd then
          ProcessRect((xwd + MapWidth div (xxt * 2)) mod G.lx, yw,
            (xw - xwd) * 2 + 1, MapHeight div yyt, ProcessOptions);
      end;
      if not FastScrolling then
        RectInvalidate(MapOffset, TopBarHeight, MapOffset + MapWidth,
          TopBarHeight + MapHeight - overlap);
      RectInvalidate(xMidPanel, TopBarHeight + MapHeight - overlap, xRightPanel,
        TopBarHeight + MapHeight);
    end;
  end;
  // if (xwd<>xw) or (ywd<>yw) then
  // Server(sChangeSuperView,me,yw*G.lx+xw,nil^); // for synchronizing client side viewer, not used currently
  xwd := xw;
  ywd := yw;
  MapValid := True;
end;

procedure TMainScreen.MiniMapPaint;
begin
  with MainMap do
    MiniMap.Paint(MyMap, MapWidth, ClientMode, xxt, xwMini);
end;

procedure TMainScreen.PaintAll;
begin
  MainOffscreenPaint;
  xwMini := xw;
  ywMini := yw;
  MiniMapPaint;
  PanelPaint;
end;

procedure TMainScreen.PaintAllMaps;
begin
  MainOffscreenPaint;
  xwMini := xw;
  ywMini := yw;
  MiniMapPaint;
  CopyMiniToPanel;
  RectInvalidate(xMini + 2, TopBarHeight + MapHeight - overlap + yMini + 2,
    xMini + 2 + G.lx * 2, TopBarHeight + MapHeight - overlap + yMini +
    2 + G.ly);
end;

procedure TMainScreen.CopyMiniToPanel;
begin
  with MainMap do begin
    BitBltCanvas(Panel.Canvas, xMini + 2, yMini + 2, G.lx * 2, G.ly,
      MiniMap.Bitmap.Canvas, 0, 0);
    if MarkCityLoc >= 0 then
      Sprite(Panel, HGrSystem, xMini - 2 + (4 * G.lx + 2 * (MarkCityLoc mod G.lx)
        + (G.lx - MapWidth div (xxt * 2)) - 2 * xwd) mod (2 * G.lx) +
        MarkCityLoc div G.lx and 1, yMini - 3 + MarkCityLoc div G.lx, CityMark2.Width,
        CityMark2.Height, CityMark2.Left, CityMark2.Top)
    else if ywmax <= 0 then
      Frame(Panel.Canvas,
        xMini + 2 + G.lx - MapWidth div (xxt * 2), yMini + 2,
        xMini + 1 + G.lx + MapWidth div (xxt * 2), yMini + 2 + G.ly - 1,
        MainTexture.ColorMark, MainTexture.ColorMark)
    else
      Frame(Panel.Canvas,
        xMini + 2 + G.lx - MapWidth div (xxt * 2), yMini + 2 + yw,
        xMini + 1 + G.lx + MapWidth div (xxt * 2), yMini + yw + MapHeight div yyt,
        MainTexture.ColorMark, MainTexture.ColorMark);
  end;
end;

procedure TMainScreen.PanelPaint;

  function MovementToString(var Un: TUn): string;
  begin
    Result := ScreenTools.MovementToString(Un.Movement);
    if Un.Master >= 0 then
      Result := '(' + Result + ')'
    else if (MyModel[Un.mix].Domain = dAir) and
      (MyModel[Un.mix].Kind <> mkSpecial_Glider) then
      Result := Format('%s(%d)', [Result, Un.Fuel]);
  end;

var
  I, uix, uixDefender, X, xSrc, ySrc, xSrcBase, ySrcBase, CostFactor, Count,
    mixShow, xTreasurySection, xResearchSection, JobFocus, TrueMoney,
    TrueResearch: Integer;
  Tile: Cardinal;
  S: string;
  unx: TUn;
  UnitInfo: TUnitInfo;
  JobProgressData: TJobProgressData;
  Prio: Boolean;
begin
  if not Assigned(MyRO) then Exit;
  with Panel.Canvas do
  begin
    Fill(Panel.Canvas, 0, 3, xMidPanel + 7 - 10, PanelHeight - 3,
      MainTexture.Width - (xMidPanel + 7 - 10), MainTexture.Height - PanelHeight);
    Fill(Panel.Canvas, xRightPanel + 10 - 7, 3, Panel.width - xRightPanel - 10 +
      7, PanelHeight - 3, -(xRightPanel + 10 - 7), MainTexture.Height - PanelHeight);
    FillLarge(Panel.Canvas, xMidPanel - 2, PanelHeight - MidPanelHeight,
      xRightPanel + 2, PanelHeight, ClientWidth div 2);

    Brush.Style := bsClear;
    Pen.Color := $000000;
    MoveTo(0, 0);
    LineTo(xMidPanel + 7 - 8, 0);
    LineTo(xMidPanel + 7 - 8, PanelHeight - MidPanelHeight);
    LineTo(xRightPanel, PanelHeight - MidPanelHeight);
    LineTo(xRightPanel, 0);
    LineTo(ClientWidth, 0);
    Pen.Color := MainTexture.ColorBevelLight;
    MoveTo(xMidPanel + 7 - 9, PanelHeight - MidPanelHeight + 2);
    LineTo(xRightPanel + 10 - 8, PanelHeight - MidPanelHeight + 2);
    Pen.Color := MainTexture.ColorBevelLight;
    MoveTo(0, 1);
    LineTo(xMidPanel + 7 - 9, 1);
    Pen.Color := MainTexture.ColorBevelShade;
    LineTo(xMidPanel + 7 - 9, PanelHeight - MidPanelHeight + 1);
    Pen.Color := MainTexture.ColorBevelLight;
    LineTo(xRightPanel + 10 - 9, PanelHeight - MidPanelHeight + 1);
    Pen.Color := MainTexture.ColorBevelLight;
    LineTo(xRightPanel + 10 - 9, 1);
    LineTo(ClientWidth, 1);
    MoveTo(ClientWidth, 2);
    LineTo(xRightPanel + 10 - 8, 2);
    LineTo(xRightPanel + 10 - 8, PanelHeight);
    MoveTo(0, 2);
    LineTo(xMidPanel + 7 - 10, 2);
    Pen.Color := MainTexture.ColorBevelShade;
    LineTo(xMidPanel + 7 - 10, PanelHeight);
    Corner(Panel.Canvas, xMidPanel + 7 - 16, 1, 1, MainTexture);
    Corner(Panel.Canvas, xRightPanel + 10 - 9, 1, 0, MainTexture);
    if ClientMode <> cEditMap then
    begin
      if Supervising then
      begin
        ScreenTools.Frame(Panel.Canvas, ClientWidth - xPalace - 1, yPalace - 1,
          ClientWidth - xPalace + xSizeBig, yPalace + ySizeBig,
          $B0B0B0, $FFFFFF);
        RFrame(Panel.Canvas, ClientWidth - xPalace - 2, yPalace - 2,
          ClientWidth - xPalace + xSizeBig + 1, yPalace + ySizeBig + 1,
          $FFFFFF, $B0B0B0);
        BitBltCanvas(Panel.Canvas, ClientWidth - xPalace, yPalace, xSizeBig,
          ySizeBig, HGrSystem2.Data.Canvas, 70, 123);
      end
      else if MyRO.NatBuilt[imPalace] > 0 then
        ImpImage(Panel.Canvas, ClientWidth - xPalace, yPalace, imPalace, -1,
          GameMode <> cMovie
          { (GameMode<>cMovie) and (MyRO.Government<>gAnarchy) } )
      else
        ImpImage(Panel.Canvas, ClientWidth - xPalace, yPalace, 21, -1,
          GameMode <> cMovie
          { (GameMode<>cMovie) and (MyRO.Government<>gAnarchy) } );
    end;

    if GameMode = cMovie then
      ScreenTools.Frame(Panel.Canvas, xMini + 1, yMini + 1,
        xMini + 2 + G.lx * 2, yMini + 2 + G.ly, $000000, $000000)
    else
    begin
      ScreenTools.Frame(Panel.Canvas, xMini + 1, yMini + 1,
        xMini + 2 + G.lx * 2, yMini + 2 + G.ly, $B0B0B0, $FFFFFF);
      RFrame(Panel.Canvas, xMini, yMini, xMini + 3 + G.lx * 2, yMini + 3 + G.ly,
        $FFFFFF, $B0B0B0);
    end;
    CopyMiniToPanel;
    if ClientMode <> cEditMap then // MapBtn icons
      for I := 0 to 5 do
        if I <> 3 then
          Dump(Panel, HGrSystem, xMini + G.lx - 42 + 16 * I, PanelHeight - 26,
            8, 8, 121 + I * 9, 61);

    if ClientMode = cEditMap then
    begin
      for I := 0 to TrRow - 1 do
        trix[I] := -1;
      Count := 0;
      for I := 0 to nBrushTypes - 1 do
      begin // display terrain types
        if (Count >= TrRow * sb.Position) and (Count < TrRow * (sb.Position + 1))
        then
        begin
          trix[Count - TrRow * sb.Position] := BrushTypes[I];
          X := (Count - TrRow * sb.Position) * TrPitch;
          xSrcBase := -1;
          case BrushTypes[I] of
            0 .. 8:
              begin
                xSrc := BrushTypes[I];
                ySrc := 0
              end;
            9 .. 30:
              begin
                xSrcBase := 2;
                ySrcBase := 2;
                xSrc := 0;
                ySrc := 2 * Integer(BrushTypes[I]) - 15
              end;
            fRiver:
              begin
                xSrc := 7;
                ySrc := 14
              end;
            fRoad:
              begin
                xSrc := 0;
                ySrc := 9
              end;
            fRR:
              begin
                xSrc := 0;
                ySrc := 10
              end;
            fCanal:
              begin
                xSrc := 0;
                ySrc := 11
              end;
            fPoll:
              begin
                xSrc := 6;
                ySrc := 12
              end;
            fDeadLands, fDeadLands or fCobalt, fDeadLands or fUranium,
              fDeadLands or fMercury:
              begin
                xSrcBase := 6;
                ySrcBase := 2;
                xSrc := 8;
                ySrc := 12 + BrushTypes[I] shr 25;
              end;
            tiIrrigation, tiFarm, tiMine, tiBase:
              begin
                xSrc := BrushTypes[I] shr 12 - 1;
                ySrc := 12
              end;
            tiFort:
              begin
                xSrc := 3;
                ySrc := 12;
                xSrcBase := 7;
                ySrcBase := 12
              end;
            fPrefStartPos:
              begin
                xSrc := 0;
                ySrc := 1
              end;
            fStartPos:
              begin
                xSrc := 0;
                ySrc := 2
              end;
          end;
          with MainMap do begin
            if xSrcBase >= 0 then
              Sprite(Panel, HGrTerrain, xTroop + 2 + X, yTroop + 9 - yyt, xxt * 2,
                yyt * 3, 1 + xSrcBase * (xxt * 2 + 1),
                1 + ySrcBase * (yyt * 3 + 1));
            Sprite(Panel, HGrTerrain, xTroop + 2 + X, yTroop + 9 - yyt, xxt * 2,
              yyt * 3, 1 + xSrc * (xxt * 2 + 1), 1 + ySrc * (yyt * 3 + 1));
            if BrushTypes[I] = BrushType then begin
              ScreenTools.Frame(Panel.Canvas, xTroop + 2 + X,
                yTroop + 7 - yyt div 2, xTroop + 2 * xxt + X,
                yTroop + 2 * yyt + 11, $000000, $000000);
              ScreenTools.Frame(Panel.Canvas, xTroop + 1 + X,
                yTroop + 6 - yyt div 2, xTroop + 2 * xxt - 1 + X,
                yTroop + 2 * yyt + 10, MainTexture.ColorMark, MainTexture.ColorMark);
            end;
          end;
        end;
        Inc(Count)
      end;
      case BrushType of
        fDesert, fPrairie, fTundra, fArctic, fSwamp, fHills, fMountains:
          S := Phrases.Lookup('TERRAIN', BrushType);
        fShore:
          S := Format(Phrases.Lookup('TWOTERRAINS'),
            [Phrases.Lookup('TERRAIN', fOcean), Phrases.Lookup('TERRAIN',
            fShore)]);
        fGrass:
          S := Format(Phrases.Lookup('TWOTERRAINS'),
            [Phrases.Lookup('TERRAIN', fGrass), Phrases.Lookup('TERRAIN',
            fGrass + 12)]);
        fForest:
          S := Format(Phrases.Lookup('TWOTERRAINS'),
            [Phrases.Lookup('TERRAIN', fForest), Phrases.Lookup('TERRAIN',
            fJungle)]);
        fRiver:
          S := Phrases.Lookup('RIVER');
        fDeadLands, fDeadLands or fCobalt, fDeadLands or fUranium,
          fDeadLands or fMercury:
          S := Phrases.Lookup('TERRAIN', 3 * 12 + BrushType shr 25);
        fPrefStartPos:
          S := Phrases.Lookup('MAP_PREFSTART');
        fStartPos:
          S := Phrases.Lookup('MAP_START');
        fPoll:
          S := Phrases.Lookup('POLL');
      else // terrain improvements
        begin
          case BrushType of
            fRoad:
              I := 1;
            fRR:
              I := 2;
            tiIrrigation:
              I := 4;
            tiFarm:
              I := 5;
            tiMine:
              I := 7;
            fCanal:
              I := 8;
            tiFort:
              I := 10;
            tiBase:
              I := 12;
          end;
          S := Phrases.Lookup('JOBRESULT', I);
        end
      end;
      LoweredTextOut(Panel.Canvas, -1, MainTexture, xTroop + 1,
        PanelHeight - 19, S);
    end
    else if TroopLoc >= 0 then
    begin
      Brush.Style := bsClear;
      if UnFocus >= 0 then
        with MyUn^[UnFocus] do
        with MyModel^[mix] do
        begin { display info about selected unit }
          if Job = jCity then
            mixShow := -1 // building site
          else
            mixShow := mix;
          with Tribe[Me].ModelPicture[mixShow] do
          begin
            Sprite(Panel, HGr, xMidPanel + 7 + 12, yTroop + 1, 64, 48,
              pix mod 10 * 65 + 1, pix div 10 * 49 + 1);
            if MyUn[UnFocus].Flags and unFortified <> 0 then
              Sprite(Panel, HGrStdUnits, xMidPanel + 7 + 12, yTroop + 1,
                xxu * 2, yyu * 2, 1 + 6 * (xxu * 2 + 1), 1);
          end;

          MakeBlue(Panel, xMidPanel + 7 + 12 + 10, yTroop - 13, 44, 12);
          S := MovementToString(MyUn[UnFocus]);
          RisedTextOut(Panel.Canvas, xMidPanel + 7 + 12 + 32 -
            BiColorTextWidth(Panel.Canvas, S) div 2, yTroop - 16, S);

          S := IntToStr(Health) + '%';
          LightGradient(Panel.Canvas, xMidPanel + 7 + 12 + 7, PanelHeight - 22,
            (Health + 1) div 2, (ColorOfHealth(Health) and $FEFEFE shr 2) * 3);
          if Health < 100 then
            LightGradient(Panel.Canvas, xMidPanel + 7 + 12 + 7 + (Health + 1)
              div 2, PanelHeight - 22, 50 - (Health + 1) div 2, $000000);
          RisedTextOut(Panel.Canvas, xMidPanel + 7 + 12 + 32 -
            BiColorTextWidth(Panel.Canvas, S) div 2, PanelHeight - 23, S);

          FrameImage(Panel.Canvas, HGrSystem.Data,
            xMidPanel + 7 + xUnitText, yTroop + 15, 12, 14,
            121 + Exp div ExpCost * 13, 28);
          if Job = jCity then
            S := Tribe[Me].ModelName[-1]
          else
            S := Tribe[Me].ModelName[mix];
          if Home >= 0 then
          begin
            LoweredTextOut(Panel.Canvas, -1, MainTexture,
              xMidPanel + 7 + xUnitText + 18, yTroop + 5, S);
            LoweredTextOut(Panel.Canvas, -1, MainTexture,
              xMidPanel + 7 + xUnitText + 18, yTroop + 21,
              '(' + CityName(MyCity[Home].ID) + ')');
          end
          else
            LoweredTextOut(Panel.Canvas, -1, MainTexture,
              xMidPanel + 7 + xUnitText + 18, yTroop + 13, S);
        end;

      if (UnFocus >= 0) and (MyUn[UnFocus].Loc <> TroopLoc) then
      begin // divide panel
        if SmallScreen and not Supervising then
          X := xTroop - 8
        else
          X := xTroop - 152;
        Pen.Color := MainTexture.ColorBevelShade;
        MoveTo(X - 1, PanelHeight - MidPanelHeight + 2);
        LineTo(X - 1, PanelHeight);
        Pen.Color := MainTexture.ColorBevelLight;
        MoveTo(X, PanelHeight - MidPanelHeight + 2);
        LineTo(X, PanelHeight);
      end;

      for I := 0 to 23 do
        trix[I] := -1;
      if MyMap[TroopLoc] and fUnit <> 0 then
      begin
        if MyMap[TroopLoc] and fOwned <> 0 then
        begin
          if (TrCnt > 1) or (UnFocus < 0) or (MyUn[UnFocus].Loc <> TroopLoc)
          then
          begin
            LoweredTextOut(Panel.Canvas, -1, MainTexture, xTroop + 10,
              PanelHeight - 24, Phrases.Lookup('PRESENT'));
            Server(sGetDefender, Me, TroopLoc, uixDefender);
            Count := 0;
            for Prio := True downto False do
              for uix := 0 to MyRO.nUn - 1 do
                if (uix = uixDefender) = Prio then
                begin // display own units
                  unx := MyUn[uix];
                  if unx.Loc = TroopLoc then
                  begin
                    if (Count >= TrRow * sb.Position) and
                      (Count < TrRow * (sb.Position + 1)) then
                    begin
                      trix[Count - TrRow * sb.Position] := uix;
                      MakeUnitInfo(Me, unx, UnitInfo);
                      X := (Count - TrRow * sb.Position) * TrPitch;
                      if uix = UnFocus then
                      begin
                        ScreenTools.Frame(Panel.Canvas, xTroop + 4 + X,
                          yTroop + 3, xTroop + 64 + X, yTroop + 47,
                          $000000, $000000);
                        ScreenTools.Frame(Panel.Canvas, xTroop + 3 + X,
                          yTroop + 2, xTroop + 63 + X, yTroop + 46,
                          MainTexture.ColorMark, MainTexture.ColorMark);
                      end
                      else if (unx.Master >= 0) and (unx.Master = UnFocus) then
                      begin
                        CFrame(Panel.Canvas, xTroop + 4 + X, yTroop + 3,
                          xTroop + 64 + X, yTroop + 47, 8, $000000);
                        CFrame(Panel.Canvas, xTroop + 3 + X, yTroop + 2,
                          xTroop + 63 + X, yTroop + 46, 8, MainTexture.ColorMark);
                      end;
                      NoMapPanel.SetOutput(Panel);
                      NoMapPanel.PaintUnit(xTroop + 2 + X, yTroop + 1, UnitInfo,
                        unx.Status);
                      if (ClientMode < scContact) and
                        ((unx.Job > jNone) or
                        (unx.Status and (usStay or usRecover or usGoto) <> 0))
                      then
                        Sprite(Panel, HGrSystem, xTroop + 2 + 60 - 20 + X,
                          yTroop + 35, 20, 20, 81, 25);

                      if not Supervising then
                      begin
                        MakeBlue(Panel, xTroop + 2 + 10 + X,
                          yTroop - 13, 44, 12);
                        S := MovementToString(unx);
                        RisedTextOut(Panel.Canvas,
                          xTroop + X + 34 - BiColorTextWidth(Panel.Canvas, S)
                          div 2, yTroop - 16, S);
                      end;
                    end;
                    Inc(Count)
                  end;
                end; // for uix:=0 to MyRO.nUn-1
            Assert(Count = TrCnt);
          end;
        end
        else
        begin
          LoweredTextOut(Panel.Canvas, -1, MainTexture, xTroop + 8,
            PanelHeight - 24, Phrases.Lookup('PRESENT'));
          Server(sGetUnits, Me, TroopLoc, Count);
          for I := 0 to Count - 1 do
            if (I >= TrRow * sb.Position) and (I < TrRow * (sb.Position + 1)) then
            begin // display enemy units
              trix[I - TrRow * sb.Position] := I;
              X := (I - TrRow * sb.Position) * TrPitch;
              NoMapPanel.SetOutput(Panel);
              NoMapPanel.PaintUnit(xTroop + 2 + X, yTroop + 1,
                MyRO.EnemyUn[MyRO.nEnemyUn + I], 0);
            end;
        end;
      end;
      if not SmallScreen or Supervising then
      begin // show terrain and improvements
        with NoMapPanel do
          PaintZoomedTile(Panel, xTerrain - xxt * 2, 110 - yyt * 3, TroopLoc);
        if (UnFocus >= 0) and (MyUn[UnFocus].Job <> jNone) then begin
          JobFocus := MyUn[UnFocus].Job;
          Server(sGetJobProgress, Me, MyUn[UnFocus].Loc, JobProgressData);
          MakeBlue(Panel, xTerrain - 72, 148 - 17, 144, 31);
          PaintRelativeProgressBar(Panel.Canvas, 3, xTerrain - 68, 148 + 3, 63,
            JobProgressData[JobFocus].Done,
            JobProgressData[JobFocus].NextTurnPlus,
            JobProgressData[JobFocus].Required, True, MainTexture);
          S := Format('%s/%s',
            [ScreenTools.MovementToString(JobProgressData[JobFocus].Done),
            ScreenTools.MovementToString(JobProgressData[JobFocus].Required)]);
          RisedTextOut(Panel.Canvas, xTerrain + 6, 148 - 3, S);
          Tile := MyMap[MyUn[UnFocus].Loc];
          if (JobFocus = jRoad) and (Tile and fRiver <> 0) then
            JobFocus := nJob + 0
          else if (JobFocus = jRR) and (Tile and fRiver <> 0) then
            JobFocus := nJob + 1
          else if JobFocus = jClear then
          begin
            if Tile and fTerrain = fForest then
              JobFocus := nJob + 2
            else if Tile and fTerrain = fDesert then
              JobFocus := nJob + 3
            else
              JobFocus := nJob + 4
          end;
          S := Phrases.Lookup('JOBRESULT', JobFocus);
          RisedTextOut(Panel.Canvas, xTerrain - BiColorTextWidth(Panel.Canvas,
            S) div 2, 148 - 19, S);
        end;
        if MyMap[TroopLoc] and (fTerrain or fSpecial) = fGrass or fSpecial1 then
          S := Phrases.Lookup('TERRAIN', fGrass + 12)
        else if MyMap[TroopLoc] and fDeadLands <> 0 then
          S := Phrases.Lookup('TERRAIN', 3 * 12)
        else if (MyMap[TroopLoc] and fTerrain = fForest) and
          IsJungle(TroopLoc div G.lx) then
          S := Phrases.Lookup('TERRAIN', fJungle)
        else
          S := Phrases.Lookup('TERRAIN', MyMap[TroopLoc] and fTerrain);
        RisedTextOut(Panel.Canvas, xTerrain - BiColorTextWidth(Panel.Canvas, S)
          div 2, 99, S);
      end;

      if TerrainBtn.Visible then
        with TerrainBtn do
          RFrame(Panel.Canvas, Left - 1, Top - self.ClientHeight +
            (PanelHeight - 1), Left + width, Top + height - self.ClientHeight +
            PanelHeight, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight)
    end; { if TroopLoc>=0 }
  end;

  for I := 0 to ControlCount - 1 do
    if Controls[I] is TButtonB then
      with TButtonB(Controls[I]) do
      begin
        if Visible then
        begin
          Dump(Panel, HGrSystem, Left, Top - self.ClientHeight + PanelHeight,
            25, 25, 169, 243);
          Sprite(Panel, HGrSystem, Left, Top - self.ClientHeight + PanelHeight,
            25, 25, 1 + 26 * ButtonIndex, 337);
          RFrame(Panel.Canvas, Left - 1, Top - self.ClientHeight +
            (PanelHeight - 1), Left + width, Top + height - self.ClientHeight +
            PanelHeight, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
        end;
      end;

  if ClientMode <> cEditMap then
  begin
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TButtonC then
        with TButtonC(Controls[I]) do
        begin
          Dump(Panel, HGrSystem, Left, Top - self.ClientHeight + PanelHeight,
            12, 12, 169, 178 + 13 * ButtonIndex);
          RFrame(Panel.Canvas, Left - 1, Top - self.ClientHeight +
            (PanelHeight - 1), Left + width, Top + height - self.ClientHeight +
            PanelHeight, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
        end;
  end;
  EOT.SetBack(Panel.Canvas, EOT.Left, EOT.Top - (ClientHeight - PanelHeight));
  SmartRectInvalidate(0, ClientHeight - PanelHeight, ClientWidth, ClientHeight);

  // topbar
  xTreasurySection := ClientWidth div 2 - 172;
  xResearchSection := ClientWidth div 2;
  // ClientWidth div 2+68 = maximum to right
  FillLarge(TopBar.Canvas, 0, 0, ClientWidth, TopBarHeight - 3,
    ClientWidth div 2);
  with TopBar.Canvas do
  begin
    Pen.Color := $000000;
    MoveTo(0, TopBarHeight - 1);
    LineTo(ClientWidth, TopBarHeight - 1);
    Pen.Color := MainTexture.ColorBevelShade;
    MoveTo(0, TopBarHeight - 2);
    LineTo(ClientWidth, TopBarHeight - 2);
    MoveTo(0, TopBarHeight - 3);
    LineTo(ClientWidth, TopBarHeight - 3);
    Pen.Color := MainTexture.ColorBevelLight;
    ScreenTools.Frame(TopBar.Canvas, 40, -1, xTreasurySection - 1,
      TopBarHeight - 7, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
    ScreenTools.Frame(TopBar.Canvas, xResearchSection + 332, -1, ClientWidth,
      TopBarHeight - 7, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
  end;
  if GameMode <> cMovie then
    ImageOp_BCC(TopBar, Templates.Data, Point(2, 1), MenuLogo.BoundsRect, $BFBF20, $4040DF);
  if MyRO.nCity > 0 then
  begin
    TrueMoney := MyRO.Money;
    TrueResearch := MyRO.Research;
    if Supervising then
    begin // normalize values from after-turn state
      Dec(TrueMoney, TaxSum);
      if TrueMoney < 0 then
        TrueMoney := 0; // shouldn't happen
      Dec(TrueResearch, ScienceSum);
      if TrueResearch < 0 then
        TrueResearch := 0; // shouldn't happen
    end;

    // treasury section
    ImageOp_BCC(TopBar, Templates.Data, Point(xTreasurySection + 8, 1), TreasuryIcon.BoundsRect,
      $40A040, $4030C0);
    S := IntToStr(TrueMoney);
    LoweredTextOut(TopBar.Canvas, -1, MainTexture, xTreasurySection + 48, 0,
      S + '%c');
    if MyRO.Government <> gAnarchy then
    begin
      ImageOp_BCC(TopBar, Templates.Data, Point(xTreasurySection + 48, 22), ChangeIcon.BoundsRect,
        $0000C0, $0080C0);
      if TaxSum >= 0 then
        S := Format(Phrases.Lookup('MONEYGAINPOS'), [TaxSum])
      else
        S := Format(Phrases.Lookup('MONEYGAINNEG'), [TaxSum]);
      LoweredTextOut(TopBar.Canvas, -1, MainTexture, xTreasurySection + 48 +
        15, 18, S);
    end;

    // research section
    ImageOp_BCC(TopBar, Templates.Data, Point(xResearchSection + 8, 1), ResearchIcon.BoundsRect,
      $FF0000, $00FFE0);
    if MyData.FarTech <> adNexus then
    begin
      if MyRO.ResearchTech < 0 then
        CostFactor := 2
      else if (MyRO.ResearchTech = adMilitary) or
        (MyRO.Tech[MyRO.ResearchTech] = tsSeen) then
        CostFactor := 1
      else if MyRO.ResearchTech in FutureTech then
        if MyRO.Government = gFuture then
          CostFactor := 4
        else
          CostFactor := 8
      else
        CostFactor := 2;
      Server(sGetTechCost, Me, 0, I);
      CostFactor := CostFactor * 22; // length of progress bar
      PaintRelativeProgressBar(TopBar.Canvas, 2, xResearchSection + 48 + 1, 26,
        CostFactor, TrueResearch, ScienceSum, I, True, MainTexture);

      if MyRO.ResearchTech < 0 then
        S := Phrases.Lookup('SCIENCE')
      else if MyRO.ResearchTech = adMilitary then
        S := Phrases.Lookup('INITUNIT')
      else
      begin
        S := Phrases.Lookup('ADVANCES', MyRO.ResearchTech);
        if MyRO.ResearchTech in FutureTech then
          if MyRO.Tech[MyRO.ResearchTech] >= 1 then
            S := S + ' ' + IntToStr(MyRO.Tech[MyRO.ResearchTech] + 1)
          else
            S := S + ' 1';
      end;
      if ScienceSum > 0 then
      begin
        { j:=(i-MyRO.Research-1) div ScienceSum +1;
          if J<1 then J:=1;
          if J>1 then
          S:=Format(Phrases.Lookup('TECHWAIT'),[S,J]); }
        LoweredTextOut(TopBar.Canvas, -1, MainTexture,
          xResearchSection + 48, 0, S);
      end
      else
        LoweredTextOut(TopBar.Canvas, -1, MainTexture,
          xResearchSection + 48, 0, S);
    end
    else
      CostFactor := 0;
    if (MyData.FarTech <> adNexus) and (ScienceSum > 0) then
    begin
      ImageOp_BCC(TopBar, Templates.Data, Point(xResearchSection + 48 + CostFactor + 11,
        22), ChangeIcon.BoundsRect, $0000C0, $0080C0);
      S := Format(Phrases.Lookup('TECHGAIN'), [ScienceSum]);
      LoweredTextOut(TopBar.Canvas, -1, MainTexture, xResearchSection + 48 +
        CostFactor + 26, 18, S);
    end;
  end;
  if ClientMode <> cEditMap then
  begin
    TopBar.Canvas.Font.Assign(UniFont[ftCaption]);
    S := TurnToString(MyRO.Turn);
    RisedTextOut(TopBar.Canvas,
      40 + (xTreasurySection - 40 - BiColorTextWidth(TopBar.Canvas, S))
      div 2, 6, S);
    TopBar.Canvas.Font.Assign(UniFont[ftNormal]);
  end;
  RectInvalidate(0, 0, ClientWidth, TopBarHeight);
end;

procedure TMainScreen.FocusNextUnit(Dir: Integer);
var
  I, uix, NewFocus: Integer;
begin
  if ClientMode >= scContact then
    Exit;
  DestinationMarkON := False;
  PaintDestination;
  NewFocus := -1;
  for I := 1 to MyRO.nUn do begin
    uix := (UnFocus + I * Dir + MyRO.nUn) mod MyRO.nUn;
    if (MyUn[uix].Loc >= 0) and (MyUn[uix].Status and usStay = 0) then begin
      NewFocus := uix;
      Break;
    end;
  end;
  if NewFocus >= 0 then begin
    SetUnFocus(NewFocus);
    SetTroopLoc(MyUn[NewFocus].Loc);
    FocusOnLoc(TroopLoc, flRepaintPanel);
  end;
end;

procedure TMainScreen.FocusOnLoc(Loc: Integer; Options: Integer = 0);
var
  dx: Integer;
  Outside, Changed: Boolean;
begin
  with MainMap do begin
    dx := G.lx + 1 - (xw - Loc + G.lx * 1024 + 1) mod G.lx;
    Outside := (dx >= (MapWidth + 1) div (xxt * 2) - 2) or (ywmax > 0) and
      ((yw > 0) and (Loc div G.lx <= yw + 1) or (yw < ywmax) and
      (Loc div G.lx >= yw + (MapHeight - 1) div yyt - 2));
  end;
  Changed := True;
  if Outside then begin
    Centre(Loc);
    PaintAllMaps;
  end
  else if not MapValid then
    PaintAllMaps
  else
    Changed := False;
  if Options and flRepaintPanel <> 0 then
    PanelPaint;
  if Changed and (Options and flImmUpdate <> 0) then
    Update;
end;

procedure TMainScreen.NextUnit(NearLoc: Integer; AutoTurn: Boolean);
var
  Dist, TestDist: Single;
  I, uix, NewFocus: Integer;
  GotoOnly: Boolean;
begin
  Dist := 0;
  if ClientMode >= scContact then
    Exit;
  DestinationMarkON := False;
  PaintDestination;
  for GotoOnly := GoOnPhase downto False do begin
    NewFocus := -1;
    for I := 1 to MyRO.nUn do begin
      uix := (UnFocus + I) mod MyRO.nUn;
      if (MyUn[uix].Loc >= 0) and (MyUn[uix].Job = jNone) and
        (MyUn[uix].Status and (usStay or usRecover or usWaiting) = usWaiting)
        and (not GotoOnly or (MyUn[uix].Status and usGoto <> 0)) then
        if NearLoc < 0 then begin
          NewFocus := uix;
          Break;
        end else begin
          TestDist := Distance(NearLoc, MyUn[uix].Loc);
          if (NewFocus < 0) or (TestDist < Dist) then begin
            NewFocus := uix;
            Dist := TestDist;
          end;
        end;
    end;
    if GotoOnly then
      if NewFocus < 0 then GoOnPhase := False
        else Break;
  end;
  if NewFocus >= 0 then begin
    SetUnFocus(NewFocus);
    SetTroopLoc(MyUn[NewFocus].Loc);
    FocusOnLoc(TroopLoc, flRepaintPanel);
  end else
  if AutoTurn and not mWaitTurn.Checked then begin
    TurnComplete := True;
    SetUnFocus(-1);
    SetTroopLoc(-1);
    PostMessage(Handle, WM_EOT, 0, 0);
  end else begin
    if { (UnFocus>=0) and } not TurnComplete and EOT.Visible then
      Play('TURNEND');
    TurnComplete := True;
    SetUnFocus(-1);
    SetTroopLoc(-1);
    PanelPaint;
  end;
end;

procedure TMainScreen.Scroll(dx, dy: Integer);
begin
  xw := (xw + G.lx + dx) mod G.lx;
  if ywmax > 0 then
  begin
    yw := yw + 2 * dy;
    if yw < 0 then
      yw := 0
    else if yw > ywmax then
      yw := ywmax;
  end;
  MainOffscreenPaint;
  xwMini := xw;
  ywMini := yw;
  MiniMapPaint;
  CopyMiniToPanel;
  RectInvalidate(xMini + 2, TopBarHeight + MapHeight - overlap + yMini + 2,
    xMini + 2 + G.lx * 2, TopBarHeight + MapHeight - overlap + yMini +
    2 + G.ly);
  Update;
end;

procedure TMainScreen.Timer1Timer(Sender: TObject);
var
  dx, dy, ScrollSpeed: Integer;
begin
  if Idle and (Me >= 0) and (GameMode <> cMovie) then
    if (fsModal in Screen.ActiveForm.FormState) or
      (Screen.ActiveForm is TBufferedDrawDlg) and
      (TBufferedDrawDlg(Screen.ActiveForm).WindowMode <> wmPersistent) then
    begin
      BlinkTime := BlinkOnTime + BlinkOffTime - 1;
      if not BlinkON then
      begin
        BlinkON := True;
        if UnFocus >= 0 then
          PaintLocTemp(MyUn[UnFocus].Loc)
        else if TurnComplete and not Supervising then
          EOT.SetButtonIndexFast(eotBlinkOn);
      end;
    end
    else
    begin
      if Application.Active and not mScrollOff.Checked then
      begin
        if mScrollFast.Checked then ScrollSpeed := 2
          else ScrollSpeed := 1;
        dx := 0;
        dy := 0;
        if Mouse.CursorPos.Y < Screen.height - PanelHeight then
          if Mouse.CursorPos.X = 0 then
            dx := -ScrollSpeed // scroll left
          else if Mouse.CursorPos.X = Screen.width - 1 then
            dx := ScrollSpeed; // scroll right
        if Mouse.CursorPos.Y = 0 then
          dy := -ScrollSpeed // scroll up
        else if (Mouse.CursorPos.Y = Screen.height - 1) and
          (Mouse.CursorPos.X >= TerrainBtn.Left + TerrainBtn.width) and
          (Mouse.CursorPos.X < xRightPanel + 10 - 8) then
          dy := ScrollSpeed; // scroll down
        if (dx <> 0) or (dy <> 0) then
        begin
          if (Screen.ActiveForm <> MainScreen) and
            (@Screen.ActiveForm.OnDeactivate <> nil) then
            Screen.ActiveForm.OnDeactivate(nil);
          Scroll(dx, dy);
        end;
      end;

      BlinkTime := (BlinkTime + 1) mod (BlinkOnTime + BlinkOffTime);
      BlinkON := BlinkTime >= BlinkOffTime;
      DestinationMarkON := True;
      if UnFocus >= 0 then
      begin
        if (BlinkTime = 0) or (BlinkTime = BlinkOffTime) then
        begin
          PaintLocTemp(MyUn[UnFocus].Loc, pltsBlink);
          PaintDestination;
          // if MoveHintToLoc>=0 then
          // ShowMoveHint(MoveHintToLoc, true);
        end;
      end
      else if TurnComplete and not Supervising then
      begin
        if BlinkTime = 0 then
          EOT.SetButtonIndexFast(eotBlinkOff)
        else if BlinkTime = BlinkOffTime then
          EOT.SetButtonIndexFast(eotBlinkOn);
      end;
    end;
end;

procedure TMainScreen.SetMapPos(Loc: Integer; MapPos: TPoint);
begin
  with MainMap do begin
    if FastScrolling and MapValid then
      Update;
    // necessary because ScrollDC for form canvas is called after
    xw := (Loc mod G.lx) - (MapPos.X - ((xxt * 2) * ((Loc div G.lx) and 1)) div 2)
      div (xxt * 2);
    xw := (xw + G.lx) mod G.lx;
    if ywmax <= 0 then yw := ywcenter
    else begin
      yw := (Loc div G.lx - (MapPos.Y * 2) div (yyt * 2) + 1) and not 1;
      if yw < 0 then yw := 0
        else if yw > ywmax then yw := ywmax;
    end;
  end;
end;

procedure TMainScreen.Centre(Loc: Integer);
begin
  SetMapPos(Loc, Point(MapWidth div 2, MapHeight div 2));
end;

function TMainScreen.ZoomToCity(Loc: Integer; NextUnitOnClose: Boolean = False;
  ShowEvent: Integer = 0): Boolean;
begin
  Result := MyMap[Loc] and (fOwned or fSpiedOut) <> 0;
  if Result then
    with CityDlg do
    begin
      if ClientMode >= scContact then
      begin
        CloseAction := None;
        RestoreUnFocus := -1;
      end
      else if NextUnitOnClose then
      begin
        CloseAction := StepFocus;
        RestoreUnFocus := -1;
      end
      else if not Visible then
      begin
        CloseAction := RestoreFocus;
        RestoreUnFocus := UnFocus;
      end;
      SetUnFocus(-1);
      SetTroopLoc(Loc);
      MarkCityLoc := Loc;
      PanelPaint;
      ShowNewContent(wmPersistent, Loc, ShowEvent);
    end;
end;

function TMainScreen.LocationOfScreenPixel(X, Y: Integer): Integer;
var
  qx, qy: Integer;
begin
  with MainMap do begin
    qx := (X * (yyt * 2) + Y * (xxt * 2) + xxt * yyt * 2) div (xxt * yyt * 4) - 1;
    qy := (Y * (xxt * 2) - X * (yyt * 2) - xxt * yyt * 2 + 4000 * xxt * yyt)
      div (xxt * yyt * 4) - 999;
    Result := (xw + (qx - qy + 2048) div 2 - 1024 + G.lx) mod G.lx + G.lx *
      (yw + qx + qy);
  end;
end;

function TMainScreen.GetCenterLoc: Integer;
begin
  Result := (xw + MapWidth div (MainMap.xxt * 4)) mod G.lx +
    (yw + MapHeight div (MainMap.yyt * 2)) * G.lx;
end;

procedure TMainScreen.MapBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I, uix, emix, p1, dx, dy, MouseLoc: Integer;
  EditTileData: TEditTileData;
  M, m2: TMenuItem;
  MoveAdviceData: TMoveAdviceData;
  DoCenter: Boolean;
begin
  if GameMode = cMovie then
    Exit;

  if CityDlg.Visible then
    CityDlg.Close;
  if UnitStatDlg.Visible then
    UnitStatDlg.Close;
  MouseLoc := LocationOfScreenPixel(X, Y);
  if (MouseLoc < 0) or (MouseLoc >= G.lx * G.ly) then
    Exit;
  if (Button = mbLeft) and not(ssShift in Shift) then
  begin
    DoCenter := True;
    if ClientMode = cEditMap then
    begin
      DoCenter := False;
      EditTileData.Loc := MouseLoc;
      if ssCtrl in Shift then // toggle special resource
        case MyMap[MouseLoc] and fTerrain of
          fOcean:
            EditTileData.NewTile := MyMap[MouseLoc];
          fGrass, fArctic:
            EditTileData.NewTile := MyMap[MouseLoc] and not fSpecial or
              ((MyMap[MouseLoc] shr 5 and 3 + 1) mod 2 shl 5);
        else
          EditTileData.NewTile := MyMap[MouseLoc] and not fSpecial or
            ((MyMap[MouseLoc] shr 5 and 3 + 1) mod 3 shl 5)
        end
      else if BrushType <= fTerrain then
        EditTileData.NewTile := MyMap[MouseLoc] and not fTerrain or fSpecial or
          BrushType
      else if BrushType and fDeadLands <> 0 then
        if MyMap[MouseLoc] and (fDeadLands or fModern) = BrushType and
          (fDeadLands or fModern) then
          EditTileData.NewTile := MyMap[MouseLoc] and not(fDeadLands or fModern)
        else
          EditTileData.NewTile := MyMap[MouseLoc] and not(fDeadLands or fModern)
            or BrushType
      else if BrushType and fTerImp <> 0 then
        if MyMap[MouseLoc] and fTerImp = BrushType then
          EditTileData.NewTile := MyMap[MouseLoc] and not fTerImp
        else
          EditTileData.NewTile := MyMap[MouseLoc] and not fTerImp or BrushType
      else if BrushType and (fPrefStartPos or fStartPos) <> 0 then
        if MyMap[MouseLoc] and (fPrefStartPos or fStartPos) = BrushType and
          (fPrefStartPos or fStartPos) then
          EditTileData.NewTile := MyMap[MouseLoc] and
            not(fPrefStartPos or fStartPos)
        else
          EditTileData.NewTile := MyMap[MouseLoc] and
            not(fPrefStartPos or fStartPos) or BrushType
      else
        EditTileData.NewTile := MyMap[MouseLoc] xor BrushType;
      Server(sEditTile, Me, 0, EditTileData);
      Edited := True;
      BrushLoc := MouseLoc;
      PaintLoc(MouseLoc, 2);
      MiniMapPaint;
      BitBltCanvas(Panel.Canvas, xMini + 2, yMini + 2, G.lx * 2, G.ly,
        MiniMap.Bitmap.Canvas, 0, 0);
      with MainMap do begin
        if ywmax <= 0 then
          Frame(Panel.Canvas, xMini + 2 + G.lx - MapWidth div (2 * xxt),
            yMini + 2, xMini + 1 + G.lx + MapWidth div (2 * xxt),
            yMini + 2 + G.ly - 1, MainTexture.ColorMark, MainTexture.ColorMark)
        else
          Frame(Panel.Canvas, xMini + 2 + G.lx - MapWidth div (2 * xxt),
            yMini + 2 + yw, xMini + 2 + G.lx + MapWidth div (2 * xxt) - 1,
            yMini + 2 + yw + MapHeight div yyt - 2, MainTexture.ColorMark,
            MainTexture.ColorMark);
      end;
      RectInvalidate(xMini + 2, TopBarHeight + MapHeight - overlap + yMini + 2,
        xMini + 2 + G.lx * 2, TopBarHeight + MapHeight - overlap + yMini
        + 2 + G.ly);
    end
    else if MyMap[MouseLoc] and fCity <> 0 then { city clicked }
    begin
      if MyMap[MouseLoc] and (fOwned or fSpiedOut) <> 0 then
      begin
        ZoomToCity(MouseLoc);
        DoCenter := False;
      end
      else
      begin
        UnitStatDlg.ShowNewContent_EnemyCity(wmPersistent, MouseLoc);
        DoCenter := False;
      end;
    end
    else if MyMap[MouseLoc] and fUnit <> 0 then { unit clicked }
      if MyMap[MouseLoc] and fOwned <> 0 then
      begin
        DoCenter := False;
        if not Supervising and (ClientMode < scContact) then
        begin // not in negotiation mode
          if (UnFocus >= 0) and (MyUn[UnFocus].Loc = MouseLoc) then
          begin // rotate
            uix := (UnFocus + 1) mod MyRO.nUn;
            I := MyRO.nUn - 1;
            while I > 0 do
            begin
              if (MyUn[uix].Loc = MouseLoc) and (MyUn[uix].Job = jNone) and
                (MyUn[uix].Status and (usStay or usRecover or usEnhance or
                usWaiting) = usWaiting) then
                Break;
              Dec(I);
              uix := (uix + 1) mod MyRO.nUn;
            end;
            if I = 0 then
              uix := UnFocus;
          end
          else
            Server(sGetDefender, Me, MouseLoc, uix);
          if uix <> UnFocus then
            SetUnFocus(uix);
          TurnComplete := False;
          EOT.ButtonIndex := eotGray;
        end;
        SetTroopLoc(MouseLoc);
        PanelPaint;
      end // own unit
      else if (MyMap[MouseLoc] and fSpiedOut <> 0) and not(ssCtrl in Shift) then
      begin
        DoCenter := False;
        SetTroopLoc(MouseLoc);
        PanelPaint;
      end
      else
      begin
        DoCenter := False;
        UnitStatDlg.ShowNewContent_EnemyLoc(wmPersistent, MouseLoc);
      end;
    if DoCenter then
    begin
      Centre(MouseLoc);
      PaintAllMaps;
    end;
  end
  else if (ClientMode <> cEditMap) and (Button = mbRight) and
    not(ssShift in Shift) then
  begin
    if Supervising then
    begin
      EditLoc := MouseLoc;
      Server(sGetModels, Me, 0, nil^);
      EmptyMenu(mCreateUnit);
      for p1 := 0 to nPl - 1 do
        if 1 shl p1 and MyRO.Alive <> 0 then
        begin
          M := TMenuItem.Create(mCreateUnit);
          M.Caption := Tribe[p1].TPhrase('SHORTNAME');
          for emix := MyRO.nEnemyModel - 1 downto 0 do
            if (MyRO.EnemyModel[emix].Owner = p1) and
              (Server(sCreateUnit - sExecute + p1 shl 4, Me,
              MyRO.EnemyModel[emix].mix, MouseLoc) >= rExecuted) then
            begin
              if not Assigned(Tribe[p1].ModelPicture[MyRO.EnemyModel[emix].mix].HGr) then
                InitEnemyModel(emix);
              m2 := TMenuItem.Create(M);
              m2.Caption := Tribe[p1].ModelName[MyRO.EnemyModel[emix].mix];
              m2.Tag := p1 shl 16 + MyRO.EnemyModel[emix].mix;
              m2.OnClick := CreateUnitClick;
              M.Add(m2);
            end;
          M.Visible := M.Count > 0;
          mCreateUnit.Add(M);
        end;
      if FullScreen then
        EditPopup.Popup(Left + X, Top + Y)
      else
        EditPopup.Popup(Left + X + 4,
          Top + Y + GetSystemMetrics(SM_CYCAPTION) + 4);
    end
    else if (UnFocus >= 0) and (MyUn[UnFocus].Loc <> MouseLoc) then
      with MyUn[UnFocus] do
      begin
        dx := ((MouseLoc mod G.lx * 2 + MouseLoc div G.lx and 1) -
          (Loc mod G.lx * 2 + Loc div G.lx and 1) + 3 * G.lx)
          mod (2 * G.lx) - G.lx;
        dy := MouseLoc div G.lx - Loc div G.lx;
        if abs(dx) + abs(dy) < 3 then
        begin
          DestinationMarkON := False;
          PaintDestination;
          Status := Status and ($FFFF - usStay - usRecover - usGoto - usEnhance)
            or usWaiting;
          MoveUnit(dx, dy, muAutoNext); { simple move }
        end
        else if GetMoveAdvice(UnFocus, MouseLoc, MoveAdviceData) >= rExecuted
        then
        begin
          if MyMap[MouseLoc] and (fUnit or fOwned) = fUnit then
          begin // check for suicide mission before movement
            with MyUn[UnFocus], BattleDlg.Forecast do
            begin
              pAtt := Me;
              mixAtt := mix;
              HealthAtt := Health;
              ExpAtt := Exp;
              FlagsAtt := Flags;
            end;
            BattleDlg.Forecast.Movement := MyUn[UnFocus].Movement;
            if (Server(sGetBattleForecastEx, Me, MouseLoc, BattleDlg.Forecast)
              >= rExecuted) and (BattleDlg.Forecast.EndHealthAtt <= 0) then
            begin
              BattleDlg.uix := UnFocus;
              BattleDlg.ToLoc := MouseLoc;
              BattleDlg.IsSuicideQuery := True;
              BattleDlg.ShowModal;
              if BattleDlg.ModalResult <> mrOK then
                Exit;
            end;
          end;
          DestinationMarkON := False;
          PaintDestination;
          Status := Status and not(usStay or usRecover or usEnhance) or
            usWaiting;
          MoveToLoc(MouseLoc, False); { goto }
        end;
      end;
  end
  else if (Button = mbMiddle) and (UnFocus >= 0) and
    (MyModel[MyUn[UnFocus].mix].Kind in [mkSettler, mkSlaves]) then
  begin
    DestinationMarkON := False;
    PaintDestination;
    MyUn[UnFocus].Status := MyUn[UnFocus].Status and
      ($FFFF - usStay - usRecover - usGoto) or usEnhance;
    uix := UnFocus;
    if MouseLoc <> MyUn[uix].Loc then
      MoveToLoc(MouseLoc, True); { goto }
    if (UnFocus = uix) and (MyUn[uix].Loc = MouseLoc) then
      mEnhance.Click;
  end
  else if (Button = mbLeft) and (ssShift in Shift) and
    (MyMap[MouseLoc] and fTerrain <> fUNKNOWN) then
    HelpOnTerrain(MouseLoc, wmPersistent)
  else if (ClientMode <= cContinue) and (Button = mbRight) and
    (ssShift in Shift) and (UnFocus >= 0) and
    (MyMap[MouseLoc] and (fUnit or fOwned) = fUnit) then
  begin // battle forecast
    with MyUn[UnFocus], BattleDlg.Forecast do
    begin
      pAtt := Me;
      mixAtt := mix;
      HealthAtt := Health;
      ExpAtt := Exp;
      FlagsAtt := Flags;
    end;
    BattleDlg.Forecast.Movement := MyUn[UnFocus].Movement;
    if Server(sGetBattleForecastEx, Me, MouseLoc, BattleDlg.Forecast) >= rExecuted
    then
    begin
      BattleDlg.uix := UnFocus;
      BattleDlg.ToLoc := MouseLoc;
      BattleDlg.Left := X - BattleDlg.width div 2;
      if BattleDlg.Left < 0 then
        BattleDlg.Left := 0
      else if BattleDlg.Left + BattleDlg.width > Screen.width then
        BattleDlg.Left := Screen.width - BattleDlg.width;
      BattleDlg.Top := Y - BattleDlg.height div 2;
      if BattleDlg.Top < 0 then
        BattleDlg.Top := 0
      else if BattleDlg.Top + BattleDlg.height > Screen.height then
        BattleDlg.Top := Screen.height - BattleDlg.height;
      BattleDlg.IsSuicideQuery := False;
      BattleDlg.Show;
    end;
  end;
end;

function TMainScreen.MoveUnit(dx, dy: Integer; Options: Integer): Integer;
// move focused unit to adjacent tile
var
  I, cix, uix, euix, FromLoc, ToLoc, DirCode, UnFocus0, Defender, Mission, p1,
    NewTiles, cixChanged: Integer;
  OldToTile: Cardinal;
  CityCaptured, IsAttack, OldUnrest, NewUnrest, NeedEcoUpdate, NeedRepaintPanel,
    ToTransport, ToShip: Boolean;
  PlaneReturnData: TPlaneReturnData;
  QueryItem: string;
begin
  Result := eInvalid;
  UnFocus0 := UnFocus;
  FromLoc := MyUn[UnFocus].Loc;
  ToLoc := dLoc(FromLoc, dx, dy);
  if (ToLoc < 0) or (ToLoc >= G.lx * G.ly) then
  begin
    Result := eInvalid;
    Exit;
  end;
  if MyMap[ToLoc] and fStealthUnit <> 0 then
  begin
    SoundMessage(Phrases.Lookup('ATTACKSTEALTH'), '');
    Exit;
  end;
  if MyMap[ToLoc] and fHiddenUnit <> 0 then
  begin
    SoundMessage(Phrases.Lookup('ATTACKSUB'), '');
    Exit;
  end;

  if MyMap[ToLoc] and (fUnit or fOwned) = fUnit then
  begin // attack -- search enemy unit
    if (MyModel[MyUn[UnFocus].mix].Attack = 0) and
      not((MyModel[MyUn[UnFocus].mix].Cap[mcBombs] > 0) and
      (MyUn[UnFocus].Flags and unBombsLoaded <> 0)) then
    begin
      SoundMessage(Phrases.Lookup('NOATTACKER'), '');
      Exit;
    end;
    euix := MyRO.nEnemyUn - 1;
    while (euix >= 0) and (MyRO.EnemyUn[euix].Loc <> ToLoc) do
      Dec(euix);
  end;

  DirCode := dx and 7 shl 4 + dy and 7 shl 7;
  Result := Server(sMoveUnit - sExecute + DirCode, Me, UnFocus, nil^);
  if (Result < rExecuted) and (MyUn[UnFocus].Job > jNone) then
    Server(sStartJob + jNone shl 4, Me, UnFocus, nil^);
  if (Result < rExecuted) and (Result <> eNoTime_Move) then
  begin
    case Result of
      eNoTime_Load:
        if MyModel[MyUn[UnFocus].mix].Domain = dAir then
          SoundMessage(Phrases.Lookup('NOTIMELOADAIR'), 'NOMOVE_TIME')
        else
          SoundMessage(Format(Phrases.Lookup('NOTIMELOADGROUND'),
            [MovementToString(MyModel[MyUn[UnFocus].mix].speed)]),
            'NOMOVE_TIME');
      eNoTime_Bombard:
        SoundMessage(Phrases.Lookup('NOTIMEBOMBARD'), 'NOMOVE_TIME');
      eNoTime_Expel:
        SoundMessage(Phrases.Lookup('NOTIMEEXPEL'), 'NOMOVE_TIME');
      eNoRoad:
        SoundMessage(Phrases.Lookup('NOROAD'), 'NOMOVE_DEFAULT');
      eNoNav:
        SoundMessage(Phrases.Lookup('NONAV'), 'NOMOVE_DEFAULT');
      eNoCapturer:
        SoundMessage(Phrases.Lookup('NOCAPTURER'), 'NOMOVE_DEFAULT');
      eNoBombarder:
        SoundMessage(Phrases.Lookup('NOBOMBARDER'), 'NOMOVE_DEFAULT');
      eZOC:
        ContextMessage(Phrases.Lookup('ZOC'), 'NOMOVE_ZOC', hkText,
          HelpDlg.TextIndex('MOVEMENT'));
      eTreaty:
        if MyMap[ToLoc] and (fUnit or fOwned) <> fUnit
        then { no enemy unit -- move }
          SoundMessage(Tribe[MyRO.Territory[ToLoc]].TPhrase('PEACE_NOMOVE'),
            'NOMOVE_TREATY')
        else
          SoundMessage(Tribe[MyRO.EnemyUn[euix].Owner]
            .TPhrase('PEACE_NOATTACK'), 'NOMOVE_TREATY');
      eDomainMismatch:
        begin
          if (MyModel[MyUn[UnFocus].mix].Domain < dSea) and
            (MyMap[ToLoc] and (fUnit or fOwned) = fUnit or fOwned) then
          begin // false load attempt
            ToShip := False;
            ToTransport := False;
            for uix := 0 to MyRO.nUn - 1 do
              if (MyUn[uix].Loc = ToLoc) and
                (MyModel[MyUn[uix].mix].Domain = dSea) then
              begin
                ToShip := True;
                if MyModel[MyUn[uix].mix].Cap[mcSeaTrans] > 0 then
                  ToTransport := True;
              end;
            if ToTransport then
              SoundMessage(Phrases.Lookup('FULLTRANSPORT'), 'NOMOVE_DEFAULT')
            else if ToShip then
              SoundMessage(Phrases.Lookup('NOTRANSPORT'), 'NOMOVE_DEFAULT')
            else
              Play('NOMOVE_DOMAIN');
          end
          else
            Play('NOMOVE_DOMAIN');
        end
    else
      Play('NOMOVE_DEFAULT');
    end;
    Exit;
  end;

  if ((Result = eWon) or (Result = eLost) or (Result = eBloody)) and
    (MyUn[UnFocus].Movement < 100) and
    (MyModel[MyUn[UnFocus].mix].Cap[mcWill] = 0) then
  begin
    if SimpleQuery(mkYesNo, Format(Phrases.Lookup('FASTATTACK'),
      [MyUn[UnFocus].Movement]), 'NOMOVE_TIME') <> mrOK then
    begin
      Result := eInvalid;
      Exit;
    end;
    Update; // remove message box from screen
  end;

  OldUnrest := False;
  NewUnrest := False;
  if (Result >= rExecuted) and (Result and rUnitRemoved = 0) and
    (MyMap[ToLoc] and (fUnit or fOwned) <> fUnit) then
  begin
    OldUnrest := UnrestAtLoc(UnFocus, FromLoc);
    NewUnrest := UnrestAtLoc(UnFocus, ToLoc);
    if NewUnrest > OldUnrest then
    begin
      if MyRO.Government = gDemocracy then
      begin
        QueryItem := 'UNREST_NOTOWN';
        p1 := Me;
      end
      else
      begin
        QueryItem := 'UNREST_FOREIGN';
        p1 := MyRO.Territory[ToLoc];
      end;
      with MessgExDlg do
      begin
        MessgText := Format(Tribe[p1].TPhrase(QueryItem),
          [Phrases.Lookup('GOVERNMENT', MyRO.Government)]);
        Kind := mkYesNo;
        IconKind := mikImp;
        IconIndex := imPalace;
        ShowModal;
        if ModalResult <> mrOK then
        begin
          Result := eInvalid;
          Exit;
        end;
      end;
      Update; // remove message box from screen
    end;
  end;

  if (Result >= rExecuted) and (MyModel[MyUn[UnFocus].mix].Domain = dAir) and
    (MyUn[UnFocus].Status and usToldNoReturn = 0) then
  begin // can plane return?
    PlaneReturnData.Fuel := MyUn[UnFocus].Fuel;
    if (MyMap[ToLoc] and (fUnit or fOwned) = fUnit) or
      (MyMap[ToLoc] and (fCity or fOwned) = fCity) then
    begin // attack/expel/bombard -> 100MP
      PlaneReturnData.Loc := FromLoc;
      PlaneReturnData.Movement := MyUn[UnFocus].Movement - 100;
      if PlaneReturnData.Movement < 0 then
        PlaneReturnData.Movement := 0;
    end
    else // move
    begin
      PlaneReturnData.Loc := ToLoc;
      if dx and 1 <> 0 then
        PlaneReturnData.Movement := MyUn[UnFocus].Movement - 100
      else
        PlaneReturnData.Movement := MyUn[UnFocus].Movement - 150;
    end;
    if Server(sGetPlaneReturn, Me, UnFocus, PlaneReturnData) = eNoWay then
    begin
      if MyModel[MyUn[UnFocus].mix].Kind = mkSpecial_Glider then
        QueryItem := 'LOWFUEL_GLIDER'
      else
        QueryItem := 'LOWFUEL';
      if SimpleQuery(mkYesNo, Phrases.Lookup(QueryItem), 'WARNING_LOWSUPPORT')
        <> mrOK then
      begin
        Result := eInvalid;
        Exit;
      end;
      Update; // remove message box from screen
      MyUn[UnFocus].Status := MyUn[UnFocus].Status or usToldNoReturn;
    end;
  end;

  if Result = eMissionDone then
  begin
    ModalSelectDlg.ShowNewContent(wmModal, kMission);
    Update; // dialog still on screen
    Mission := ModalSelectDlg.Result;
    if Mission < 0 then
      Exit;
    Server(sSetSpyMission + Mission shl 4, Me, 0, nil^);
  end;

  CityCaptured := False;
  if Result = eNoTime_Move then
    Play('NOMOVE_TIME')
  else
  begin
    NeedEcoUpdate := False;
    DestinationMarkON := False;
    PaintDestination;
    if Result and rUnitRemoved <> 0 then
      CityOptimizer_BeforeRemoveUnit(UnFocus);
    IsAttack := (Result = eBombarded) or (Result <> eMissionDone) and
      (MyMap[ToLoc] and (fUnit or fOwned) = fUnit);
    if not IsAttack then
    begin // move
      cix := MyRO.nCity - 1; { look for own city at dest location }
      while (cix >= 0) and (MyCity[cix].Loc <> ToLoc) do
        Dec(cix);
      if (Result <> eMissionDone) and (MyMap[ToLoc] and fCity <> 0) and (cix < 0)
      then
        CityCaptured := True;
      Result := Server(sMoveUnit + DirCode, Me, UnFocus, nil^);
      case Result of
        eHiddenUnit:
          begin
            Play('NOMOVE_SUBMARINE');
            PaintLoc(ToLoc);
          end;
        eStealthUnit:
          begin
            Play('NOMOVE_STEALTH');
            PaintLoc(ToLoc);
          end;
        eZOC_EnemySpotted:
          begin
            Play('NOMOVE_ZOC');
            PaintLoc(ToLoc, 1);
          end;
        rExecuted .. maxint:
          begin
            if Result and rUnitRemoved <> 0 then
              UnFocus := -1 // unit died
            else
            begin
              Assert(UnFocus >= 0);
              MyUn[UnFocus].Status := MyUn[UnFocus].Status and
                not(usStay or usRecover);
              for uix := 0 to MyRO.nUn - 1 do
                if MyUn[uix].Master = UnFocus then
                  MyUn[uix].Status := MyUn[uix].Status and not usWaiting;
              if CityCaptured and
                (MyRO.Government in [gRepublic, gDemocracy, gFuture]) then
              begin // borders have moved, unrest might have changed in any city
                CityOptimizer_BeginOfTurn;
                NeedEcoUpdate := True;
              end
              else
              begin
                if OldUnrest <> NewUnrest then
                begin
                  CityOptimizer_CityChange(MyUn[UnFocus].Home);
                  for uix := 0 to MyRO.nUn - 1 do
                    if MyUn[uix].Master = UnFocus then
                      CityOptimizer_CityChange(MyUn[uix].Home);
                  NeedEcoUpdate := True;
                end;
                if (MyRO.Government = gDespotism) and
                  (MyModel[MyUn[UnFocus].mix].Kind = mkSpecial_TownGuard) then
                begin
                  if MyMap[FromLoc] and fCity <> 0 then
                  begin // town guard moved out of city in despotism -- reoptimize!
                    cixChanged := MyRO.nCity - 1;
                    while (cixChanged >= 0) and
                      (MyCity[cixChanged].Loc <> FromLoc) do
                      Dec(cixChanged);
                    Assert(cixChanged >= 0);
                    if cixChanged >= 0 then
                    begin
                      CityOptimizer_CityChange(cixChanged);
                      NeedEcoUpdate := True;
                    end;
                  end;
                  if (MyMap[ToLoc] and fCity <> 0) and not CityCaptured then
                  begin // town guard moved into city in despotism -- reoptimize!
                    cixChanged := MyRO.nCity - 1;
                    while (cixChanged >= 0) and
                      (MyCity[cixChanged].Loc <> ToLoc) do
                      Dec(cixChanged);
                    Assert(cixChanged >= 0);
                    if cixChanged >= 0 then
                    begin
                      CityOptimizer_CityChange(cixChanged);
                      NeedEcoUpdate := True;
                    end;
                  end;
                end;
              end;
            end;
          end;
      else
        Assert(False);
      end;
      SetTroopLoc(ToLoc);
    end
    else
    begin { enemy unit -- attack }
      if Result = eBombarded then
        Defender := MyRO.Territory[ToLoc]
      else
        Defender := MyRO.EnemyUn[euix].Owner;
      { if MyRO.Treaty[Defender]=trCeaseFire then
        if SimpleQuery(mkYesNo,Phrases.Lookup('FRCANCELQUERY_CEASEFIRE'),
        'MSG_DEFAULT')<>mrOK then
        Exit; }
      if (Options and muNoSuicideCheck = 0) and (Result and rUnitRemoved <> 0)
        and (Result <> eMissionDone) then
      begin // suicide query
        with MyUn[UnFocus], BattleDlg.Forecast do
        begin
          pAtt := Me;
          mixAtt := mix;
          HealthAtt := Health;
          ExpAtt := Exp;
          FlagsAtt := Flags;
        end;
        BattleDlg.Forecast.Movement := MyUn[UnFocus].Movement;
        Server(sGetBattleForecastEx, Me, ToLoc, BattleDlg.Forecast);
        BattleDlg.uix := UnFocus;
        BattleDlg.ToLoc := ToLoc;
        BattleDlg.IsSuicideQuery := True;
        BattleDlg.ShowModal;
        if BattleDlg.ModalResult <> mrOK then
          Exit;
      end;

      cixChanged := -1;
      if (Result and rUnitRemoved <> 0) and (MyRO.Government = gDespotism) and
        (MyModel[MyUn[UnFocus].mix].Kind = mkSpecial_TownGuard) and
        (MyMap[FromLoc] and fCity <> 0) then
      begin // town guard died in city in despotism -- reoptimize!
        cixChanged := MyRO.nCity - 1;
        while (cixChanged >= 0) and (MyCity[cixChanged].Loc <> FromLoc) do
          Dec(cixChanged);
        Assert(cixChanged >= 0);
      end;

      for I := 0 to MyRO.nEnemyModel - 1 do
        LostArmy[I] := MyRO.EnemyModel[I].Lost;
      OldToTile := MyMap[ToLoc];
      Result := Server(sMoveUnit + DirCode, Me, UnFocus, nil^);
      nLostArmy := 0;
      for I := 0 to MyRO.nEnemyModel - 1 do
      begin
        LostArmy[I] := MyRO.EnemyModel[I].Lost - LostArmy[I];
        Inc(nLostArmy, LostArmy[I]);
      end;
      if Result and rUnitRemoved <> 0 then
      begin
        UnFocus := -1;
        SetTroopLoc(FromLoc);
      end;
      if (OldToTile and not MyMap[ToLoc] and fCity <> 0) and
        (MyRO.Government in [gRepublic, gDemocracy, gFuture]) then
      begin // city was destroyed, borders have moved, unrest might have changed in any city
        CityOptimizer_BeginOfTurn;
        NeedEcoUpdate := True;
      end
      else
      begin
        if cixChanged >= 0 then
        begin
          CityOptimizer_CityChange(cixChanged);
          NeedEcoUpdate := True;
        end;
        if (Result = eWon) or (Result = eBloody) or (Result = eExpelled) then
        begin
          CityOptimizer_TileBecomesAvailable(ToLoc);
          NeedEcoUpdate := True;
        end;
      end;
      if nLostArmy > 1 then
      begin
        with MessgExDlg do
        begin
          Kind := mkOk;
          IconKind := mikEnemyArmy;
          MessgText := Tribe[Defender].TString(Phrases.Lookup('ARMYLOST',
            MyRO.EnemyModel[MyRO.EnemyUn[euix].emix].Domain));
          ShowModal;
        end;
      end;
    end;
    if Result and rUnitRemoved <> 0 then
    begin
      CityOptimizer_AfterRemoveUnit;
      ListDlg.RemoveUnit;
      NeedEcoUpdate := True;
    end;
    if NeedEcoUpdate then
    begin
      UpdateViews(True);
      Update;
    end;
  end;

  if Result = eMissionDone then
  begin
    p1 := MyRO.Territory[ToLoc];
    case Mission of
      smStealMap:
        begin
          MapValid := False;
          PaintAllMaps
        end;
      smStealCivilReport:
        TribeMessage(p1, Tribe[p1].TPhrase('DOSSIER_PREPARED'), '');
      smStealMilReport:
        ListDlg.ShowNewContent_MilReport(wmPersistent, p1);
    end;
  end;

  if UnFocus >= 0 then
    CheckToldNoReturn(UnFocus);

  NeedRepaintPanel := False;
  if Result >= rExecuted then
  begin
    if CityCaptured and (MyMap[ToLoc] and fCity = 0) then
    begin // city destroyed
      for I := 0 to nWonder - 1 do { tell about destroyed wonders }
        if (MyRO.Wonder[I].CityID = WonderDestroyed) and (MyData.ToldWonders[I].CityID <> WonderDestroyed)
        then
          with MessgExDlg do
          begin
            if WondersDlg.Visible then
              WondersDlg.SmartUpdateContent(False);
            OpenSound := 'WONDER_DESTROYED';
            MessgText := Format(Phrases.Lookup('WONDERDEST'),
              [Phrases.Lookup('IMPROVEMENTS', I)]);
            Kind := mkOkHelp;
            HelpKind := hkImp;
            HelpNo := I;
            IconKind := mikImp;
            IconIndex := I;
            ShowModal;
            MyData.ToldWonders[I] := MyRO.Wonder[I];
          end;
    end;
    if CityCaptured and (MyMap[ToLoc] and fCity <> 0) then
    begin // city captured
      ListDlg.AddCity;
      for I := 0 to nWonder - 1 do { tell about capture of wonders }
        if MyRO.City[MyRO.nCity - 1].Built[I] > 0 then
          with MessgExDlg do
          begin
            if WondersDlg.Visible then
              WondersDlg.SmartUpdateContent(False);
            OpenSound := 'WONDER_CAPTURED';
            MessgText := Format(Tribe[Me].TPhrase('WONDERCAPTOWN'),
              [Phrases.Lookup('IMPROVEMENTS', I)]);
            Kind := mkOkHelp;
            HelpKind := hkImp;
            HelpNo := I;
            IconKind := mikImp;
            IconIndex := I;
            ShowModal;
            MyData.ToldWonders[I] := MyRO.Wonder[I];
          end;

      if MyRO.Happened and phStealTech <> 0 then
      begin { Temple of Zeus -- choose advance to steal }
        ModalSelectDlg.ShowNewContent(wmModal, kStealTech);
        Server(sStealTech, Me, ModalSelectDlg.Result, nil^);
      end;
      TellNewModels;

      cix := MyRO.nCity - 1;
      while (cix >= 0) and (MyCity[cix].Loc <> ToLoc) do
        Dec(cix);
      Assert(cix >= 0);
      MyCity[cix].Status := MyCity[cix].Status and not csResourceWeightsMask or
        (3 shl 4);
      // captured city, set to maximum growth
      NewTiles := 1 shl 13; { exploit central tile only }
      Server(sSetCityTiles, Me, cix, NewTiles);
    end
    else
      NeedRepaintPanel := True;
  end;
  TellNewContacts;

  if (UnFocus >= 0) and (MyUn[UnFocus].Master >= 0) then
    with MyUn[MyUn[UnFocus].Master] do
      if Status and usStay <> 0 then
      begin
        Status := Status and not usStay;
        if (Movement >= 100) and (Status and (usRecover or usGoto) = 0) then
          Status := Status or usWaiting;
      end;
  if Options and (muAutoNoWait or muAutoNext) <> 0 then
  begin
    if (UnFocus >= 0) and ((Result = eNoTime_Move) or UnitExhausted(UnFocus) or
      (MyUn[UnFocus].Master >= 0) or (MyModel[MyUn[UnFocus].mix].Domain = dAir)
      and ((MyMap[MyUn[UnFocus].Loc] and fCity <> 0)
      { aircrafts stop in cities }
      or (MyMap[MyUn[UnFocus].Loc] and fTerImp = tiBase))) then
    begin
      MyUn[UnFocus].Status := MyUn[UnFocus].Status and not usWaiting;
      if Options and muAutoNext <> 0 then
        if CityCaptured and (MyMap[ToLoc] and fCity <> 0) then
        begin
          UnFocus := -1;
          PaintLoc(ToLoc); // don't show unit in city if not selected
        end
        else
          NextUnit(UnStartLoc, True);
    end
    else if (UnFocus < 0) and (Options and muAutoNext <> 0) then
      NextUnit(UnStartLoc, Result <> eMissionDone);
  end;

  if NeedRepaintPanel and (UnFocus = UnFocus0) then
    if IsAttack then
      PanelPaint
    else
    begin
      Assert(Result <> eMissionDone);
      CheckTerrainBtnVisible;
      FocusOnLoc(ToLoc, flRepaintPanel or flImmUpdate);
    end;

  if (Result >= rExecuted) and CityCaptured and (MyMap[ToLoc] and fCity <> 0)
  then
    ZoomToCity(ToLoc, UnFocus < 0, chCaptured); // show captured city
end;

procedure TMainScreen.MoveOnScreen(ShowMove: TShowMove;
  Step0, Step1, nStep: Integer; Restore: Boolean = True);
var
  ToLoc, xFromLoc, yFromLoc, xToLoc, yToLoc, xFrom, yFrom, xTo, yTo, xMin, yMin,
    xRange, yRange, xw1, Step, xMoving, yMoving, SliceCount: Integer;
  UnitInfo: TUnitInfo;
  Ticks0, Ticks: TDateTime;
begin
  Timer1.Enabled := False;
  Ticks0 := NowPrecise;
  with ShowMove do
  begin
    UnitInfo.Owner := Owner;
    UnitInfo.mix := mix;
    UnitInfo.Health := Health;
    UnitInfo.Job := jNone;
    UnitInfo.Flags := Flags;
    if Owner <> Me then
      UnitInfo.emix := emix;

    ToLoc := dLoc(FromLoc, dx, dy);
    xToLoc := ToLoc mod G.lx;
    yToLoc := ToLoc div G.lx;
    xFromLoc := FromLoc mod G.lx;
    yFromLoc := FromLoc div G.lx;
    if xToLoc > xFromLoc + 2 then
      xToLoc := xToLoc - G.lx
    else if xToLoc < xFromLoc - 2 then
      xToLoc := xToLoc + G.lx;

    xw1 := xw + G.lx;
    // ((xFromLoc-xw1)*2+yFromLoc and 1+1)*xxt+dx*xxt/2-MapWidth/2 -> min
    with MainMap do begin
      while abs(((xFromLoc - xw1 + G.lx) * 2 + yFromLoc and 1 + 1) * xxt * 2 + dx
        * xxt - MapWidth) < abs(((xFromLoc - xw1) * 2 + yFromLoc and 1 + 1) * xxt
        * 2 + dx * xxt - MapWidth) do
        Dec(xw1, G.lx);

      xTo := (xToLoc - xw1) * (xxt * 2) + yToLoc and 1 * xxt + (xxt - xxu);
      yTo := (yToLoc - yw) * yyt + (yyt - yyu_anchor);
      xFrom := (xFromLoc - xw1) * (xxt * 2) + yFromLoc and 1 * xxt + (xxt - xxu);
      yFrom := (yFromLoc - yw) * yyt + (yyt - yyu_anchor);
      if xFrom < xTo then begin
        xMin := xFrom;
        xRange := xTo - xFrom;
      end else begin
        xMin := xTo;
        xRange := xFrom - xTo;
      end;
      if yFrom < yTo then begin
        yMin := yFrom;
        yRange := yTo - yFrom;
      end else begin
        yMin := yTo;
        yRange := yFrom - yTo;
      end;
      Inc(xRange, xxt * 2);
      Inc(yRange, yyt * 3);
    end;

    MainOffscreenPaint;
    NoMap.SetOutput(Buffer);
    NoMap.SetPaintBounds(0, 0, xRange, yRange);
    for Step := 0 to abs(Step1 - Step0) do
    begin
      BitBltCanvas(Buffer.Canvas, 0, 0, xRange, yRange,
        Offscreen.Canvas, xMin, yMin);
      if Step1 <> Step0 then
      begin
        xMoving := xFrom +
          Round((Step0 + Step * (Step1 - Step0) div abs(Step1 - Step0)) *
          (xTo - xFrom) / nStep);
        yMoving := yFrom +
          Round((Step0 + Step * (Step1 - Step0) div abs(Step1 - Step0)) *
          (yTo - yFrom) / nStep);
      end
      else
      begin
        xMoving := xFrom;
        yMoving := yFrom;
      end;
      NoMap.PaintUnit(xMoving - xMin, yMoving - yMin, UnitInfo, 0);
      PaintBufferToScreen(xMin, yMin, xRange, yRange);
      {$IFDEF UNIX}
      // TODO: Force animation under UNIX
      Application.ProcessMessages;
      {$ENDIF}

      SliceCount := 0;
      Ticks := Ticks0;
      repeat
        if (SliceCount = 0) or
          (Round(((Ticks - Ticks0) * 12) / OneMillisecond) * (SliceCount + 1) div SliceCount
          < MoveTime) then
        begin
          if not Idle or (GameMode = cMovie) then
            Application.ProcessMessages;
          Sleep(1);
          Inc(SliceCount)
        end;
        Ticks := NowPrecise;
      until (((Ticks - Ticks0) * 12) / OneMillisecond) >= MoveTime;
      Ticks0 := Ticks;
    end;
  end;
  if Restore then
  begin
    BitBltCanvas(Buffer.Canvas, 0, 0, xRange, yRange, Offscreen.Canvas, xMin, yMin);
    PaintBufferToScreen(xMin, yMin, xRange, yRange);
  end;
  BlinkTime := -1;
  Timer1.Enabled := True;
end;

procedure TMainScreen.MoveToLoc(Loc: Integer; CheckSuicide: Boolean);
// path finder: move focused unit to loc, start multi-turn goto if too far
var
  uix, I, MoveOptions, NextLoc, MoveResult: Integer;
  MoveAdviceData: TMoveAdviceData;
  StopReason: (None, Arrived, Dead, NoTime, EnemySpotted, MoveError);
begin
  if MyUn[UnFocus].Job > jNone then
    Server(sStartJob + jNone shl 4, Me, UnFocus, nil^);
  if GetMoveAdvice(UnFocus, Loc, MoveAdviceData) >= rExecuted then
  begin
    uix := UnFocus;
    StopReason := None;
    repeat
      for I := 0 to MoveAdviceData.nStep - 1 do
      begin
        if I = MoveAdviceData.nStep - 1 then
          MoveOptions := muAutoNext
        else
          MoveOptions := 0;
        NextLoc := dLoc(MyUn[uix].Loc, MoveAdviceData.dx[I],
          MoveAdviceData.dy[I]);
        if (NextLoc = Loc) or (Loc = maNextCity) and
          (MyMap[NextLoc] and fCity <> 0) then
          StopReason := Arrived;
        if not CheckSuicide and (NextLoc = Loc) then
          MoveOptions := MoveOptions or muNoSuicideCheck;
        MoveResult := MoveUnit(MoveAdviceData.dx[I], MoveAdviceData.dy[I],
          MoveOptions);
        if MoveResult < rExecuted then
          StopReason := MoveError
        else if MoveResult and rUnitRemoved <> 0 then
          StopReason := Dead
        else if (StopReason = None) and (MoveResult and rEnemySpotted <> 0) then
          StopReason := EnemySpotted;
        if StopReason <> None then
          Break;
      end;
      if (StopReason = None) and ((MoveAdviceData.nStep < 25) or
        (MyRO.Wonder[woShinkansen].EffectiveOwner <> Me)) then
        StopReason := NoTime;
      if StopReason <> None then
        Break;
      if GetMoveAdvice(UnFocus, Loc, MoveAdviceData) < rExecuted then
      begin
        Assert(False);
        Break;
      end;
    until False;

    case StopReason of
      None:
        Assert(False);
      Arrived:
        MyUn[uix].Status := MyUn[uix].Status and ($FFFF - usGoto);
      Dead:
        if UnFocus < 0 then
          NextUnit(UnStartLoc, False);
    else
      begin // multi-turn goto
        if Loc = maNextCity then
          MyUn[uix].Status := MyUn[uix].Status and ($FFFF - usStay - usRecover)
            or usGoto + $7FFF shl 16
        else
          MyUn[uix].Status := MyUn[uix].Status and ($FFFF - usStay - usRecover)
            or usGoto + Loc shl 16;
        PaintLoc(MyUn[uix].Loc);
        if (StopReason = NoTime) and (UnFocus = uix) then
        begin
          MyUn[uix].Status := MyUn[uix].Status and not usWaiting;
          NextUnit(UnStartLoc, True);
        end;
      end;
    end;
  end;
end;

procedure TMainScreen.PanelBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I, xMouse, MouseLoc, p1: Integer;
begin
  if GameMode = cMovie then
    Exit;

  if Button = mbLeft then
  begin
    if (X >= xMini + 2) and (Y >= yMini + 2) and (X < xMini + 2 + 2 * G.lx) and
      (Y < yMini + 2 + G.ly) then
      if ssShift in Shift then
      begin
        with MainMap do
          xMouse := (xwMini + (X - (xMini + 2) + MapWidth div (xxt * 2) + G.lx)
            div 2) mod G.lx;
        MouseLoc := xMouse + G.lx * (Y - (yMini + 2));
        if MyMap[MouseLoc] and fTerrain <> fUNKNOWN then
        begin
          p1 := MyRO.Territory[MouseLoc];
          if (p1 = Me) or (p1 >= 0) and (MyRO.Treaty[p1] >= trNone) then
            NatStatDlg.ShowNewContent(wmPersistent, p1);
        end;
      end
      else
      begin
        if CityDlg.Visible then
          CityDlg.Close;
        if UnitStatDlg.Visible then
          UnitStatDlg.Close;
        Tracking := True;
        PanelBoxMouseMove(Sender, Shift + [ssLeft], X, Y);
      end
    else if (ClientMode <> cEditMap) and (X >= ClientWidth - xPalace) and
      (Y >= yPalace) and (X < ClientWidth - xPalace + xSizeBig) and
      (Y < yPalace + ySizeBig) then
    begin
      InitPopup(StatPopup);
      if FullScreen then
        StatPopup.Popup(Left + ClientWidth - xPalace + xSizeBig + 2,
          Top + ClientHeight - PanelHeight + yPalace - 1)
      else
        StatPopup.Popup(Left + ClientWidth - xPalace + 6,
          Top + ClientHeight - PanelHeight + yPalace + ySizeBig +
          GetSystemMetrics(SM_CYCAPTION) + 3)
    end
    (* else if (X>=xAdvisor-3) and (Y>=yAdvisor-3)
      and (X<xAdvisor+16+3) and (Y<yAdvisor+16+3) and HaveStrategyAdvice then
      AdviceBtnClick *)
    else if (X >= xTroop + 1) and (Y >= yTroop + 1) and
      (X < xTroop + TrRow * TrPitch) and (Y <= yTroop + 55) then
    begin
      I := (X - xTroop - 1) div TrPitch;
      if trix[I] >= 0 then
        if ClientMode = cEditMap then
        begin
          BrushType := trix[I];
          PanelPaint
        end
        else if (TroopLoc >= 0) then
          if MyMap[TroopLoc] and fOwned <> 0 then
          begin
            if ssShift in Shift then
              UnitStatDlg.ShowNewContent_OwnModel(wmPersistent,
                MyUn[trix[I]].mix)
            else if not Supervising and (ClientMode < scContact) and
              (X - xTroop - 1 - I * TrPitch >= 60 - 20) and (Y >= yTroop + 35)
              and ((MyUn[trix[I]].Job > jNone) or (MyUn[trix[I]].Status and
              (usStay or usRecover or usGoto) <> 0)) then
            begin // wake up
              MyUn[trix[I]].Status := MyUn[trix[I]].Status and
                ($FFFF - usStay - usRecover - usGoto - usEnhance) or usWaiting;
              if MyUn[trix[I]].Job > jNone then
                Server(sStartJob + jNone shl 4, Me, trix[I], nil^);
              if (UnFocus < 0) and not CityDlg.Visible then
              begin
                SetUnFocus(trix[I]);
                SetTroopLoc(MyUn[trix[I]].Loc);
                FocusOnLoc(TroopLoc, flRepaintPanel)
              end
              else
              begin
                if CityDlg.Visible and (CityDlg.RestoreUnFocus < 0) then
                  CityDlg.RestoreUnFocus := trix[I];
                PanelPaint;
              end
            end
            else if (ClientMode < scContact) then
            begin
              if Supervising then
                UnitStatDlg.ShowNewContent_OwnUnit(wmPersistent, trix[I])
              else if CityDlg.Visible then
              begin
                CityDlg.CloseAction := None;
                CityDlg.Close;
                SumCities(TaxSum, ScienceSum);
                SetUnFocus(trix[I]);
              end
              else
              begin
                DestinationMarkON := False;
                PaintDestination;
                UnFocus := trix[I];
                UnStartLoc := TroopLoc;
                BlinkTime := 0;
                BlinkON := False;
                PaintLoc(TroopLoc);
              end;
              if UnFocus >= 0 then
              begin
                UnitInfoBtn.Visible := True;
                UnitBtn.Visible := True;
                TurnComplete := False;
                EOT.ButtonIndex := eotGray;
              end;
              CheckTerrainBtnVisible;
              PanelPaint;
            end;
          end
          else if Server(sGetUnits, Me, TroopLoc, TrCnt) >= rExecuted then
            if ssShift in Shift then
              UnitStatDlg.ShowNewContent_EnemyModel(wmPersistent,
                MyRO.EnemyUn[MyRO.nEnemyUn + trix[I]].emix) // model info
            else
              UnitStatDlg.ShowNewContent_EnemyUnit(wmPersistent,
                MyRO.nEnemyUn + trix[I]); // unit info
    end;
  end;
end;

procedure TMainScreen.SetTroopLoc(Loc: Integer);
var
  trixFocus, uix, uixDefender: Integer;
  Prio: Boolean;
begin
  TroopLoc := Loc;
  TrRow := (xRightPanel + 10 - xTroop - GetSystemMetrics(SM_CXVSCROLL) - 19)
    div TrPitch;
  TrCnt := 0;
  trixFocus := -1;
  if ClientMode = cEditMap then
    TrCnt := nBrushTypes
  else if (Loc >= 0) and (MyMap[Loc] and fUnit <> 0) then
    if MyMap[Loc] and fOwned <> 0 then
    begin // count own units here
      Server(sGetDefender, Me, TroopLoc, uixDefender);
      for Prio := True downto False do
        for uix := 0 to MyRO.nUn - 1 do
          if ((uix = uixDefender) = Prio) and (MyUn[uix].Loc = Loc) then
          begin
            if uix = UnFocus then
              trixFocus := TrCnt;
            Inc(TrCnt);
          end;
    end
    else // count enemy units here
      Server(sGetUnits, Me, Loc, TrCnt);
  if TrCnt = 0 then
    sb.Init(0, 1)
  else
  begin
    sb.Init((TrCnt + TrRow - 1) div TrRow - 1, 1);
    if (sb.Max >= sb.PageSize) and (trixFocus >= 0) then
      sb.Position := trixFocus div TrRow;
  end;
end;

(* procedure TMainScreen.ShowMoveHint(ToLoc: Integer; Force: Boolean = False);
  var
  Step,Loc,x0,y0,xs,ys: Integer;
  Info: string;
  InfoSize: TSize;
  MoveAdvice: TMoveAdviceData;
  begin
  if (ToLoc<0) or (ToLoc>=G.lx*G.ly)
  or (UnFocus<0) or (MyUn[UnFocus].Loc=ToLoc) then
  ToLoc:=-1
  else
  begin
  MoveAdvice.ToLoc:=ToLoc;
  MoveAdvice.MoreTurns:=0;
  MoveAdvice.MaxHostile_MovementLeft:=MyUn[UnFocus].Health-50;
  if Server(sGetMoveAdvice,Me,UnFocus,MoveAdvice)<rExecuted then
  ToLoc:=-1
  end;
  if (ToLoc=MoveHintToLoc) and not Force then Exit;
  if (ToLoc<>MoveHintToLoc) and (MoveHintToLoc>=0) then
  begin invalidate; update end; // clear old hint from screen
  MoveHintToLoc:=ToLoc;
  if ToLoc<0 then Exit;

  with Canvas do
  begin
  Pen.Color:=$80C0FF;
  Pen.Width:=3;
  Loc:=MyUn[UnFocus].Loc;
  for Step:=0 to MoveAdvice.nStep do
  begin
  y0:=(Loc+G.lx*1024) div G.lx -1024;
  x0:=(Loc+(y0 and 1+G.lx*1024) div 2) mod G.lx;
  xs:=(x0-xw)*66+y0 and 1*33-G.lx*66;
  while abs(2*(xs+G.lx*66)-MapWidth)<abs(2*xs-MapWidth) do
  Inc(xs,G.lx*66);
  ys:=(y0-yw)*16;
  if Step=0 then moveto(xs+33,ys+16)
  else lineto(xs+33,ys+16);
  if Step<MoveAdvice.nStep then
  Loc:=dLoc(Loc,MoveAdvice.dx[Step],MoveAdvice.dy[Step]);
  end;
  Brush.Color:=$80C0FF;
  Info:=' '+IntToStr(88)+' ';
  InfoSize:=TextExtent(Info);
  TextOut(xs+33-InfoSize.cx div 2, ys+16-InfoSize.cy div 2, Info);
  Brush.Style:=bsClear;
  end
  end; *)

procedure TMainScreen.SetDebugMap(P: Integer);
begin
  MainMap.pDebugMap := P;
  MapOptions := MapOptions - [moLocCodes];
  mLocCodes.Checked := False;
  MapValid := False;
  MainOffscreenPaint;
end;

procedure TMainScreen.SetViewpoint(P: Integer);
var
  I: Integer;
begin
  if Supervising and (G.RO[0].Turn > 0) and
    ((P = 0) or (1 shl P and G.RO[0].Alive <> 0)) then
  begin
    ApplyToVisibleForms(faClose);
    ItsMeAgain(P);
    SumCities(TaxSum, ScienceSum);
    for I := 0 to MyRO.nModel - 1 do
      if not Assigned(Tribe[Me].ModelPicture[I].HGr) then
        InitMyModel(I, True);

    SetTroopLoc(-1);
    PanelPaint;
    MapValid := False;
    PaintAllMaps;
  end;
end;

procedure TMainScreen.UpdateKeyShortcuts;
begin
  mHelp.ShortCut := BHelp.ShortCut;
  mUnitStat.ShortCut := BUnitStat.ShortCut;
  mCityStat.ShortCut := BCityStat.ShortCut;
  mScienceStat.ShortCut := BScienceStat.ShortCut;
  mEUnitStat.ShortCut := BEUnitStat.ShortCut;;
  mDiagram.ShortCut := BDiagram.ShortCut;
  mWonders.ShortCut := BWonders.ShortCut;
  mShips.ShortCut := BShips.ShortCut;
  mNations.ShortCut := BNations.ShortCut;
  mEmpire.ShortCut := BEmpire.ShortCut;
  mResign.ShortCut := BResign.ShortCut;
  mRandomMap.ShortCut := BRandomMap.ShortCut;
  mDisband.ShortCut := BDisbandUnit.ShortCut;
  mFort.ShortCut := BFortify.ShortCut;
  mCentre.ShortCut := BCenterUnit.ShortCut;
  mStay.ShortCut := BStay.ShortCut;
  mNoOrders.ShortCut := BNoOrders.ShortCut;
  mPrevUnit.ShortCut := BPrevUnit.ShortCut;
  mNextUnit.ShortCut := BNextUnit.ShortCut;
  mCancel.ShortCut := BCancel.ShortCut;
  mPillage.ShortCut := BPillage.ShortCut;
  mTechTree.ShortCut := BTechTree.ShortCut;
  mWait.ShortCut := BWait.ShortCut;
  mJump.ShortCut := BJump.ShortCut;;
  mDebugMap.ShortCut := BDebugMap.ShortCut;
  mLocCodes.ShortCut := BLocCodes.ShortCut;
  mNames.ShortCut := BNames.ShortCut;
  mRun.ShortCut := BRun.ShortCut;
  mAirBase.ShortCut := BAirBase.ShortCut;
  mCity.ShortCut := BBuildCity.ShortCut;
  mEnhance.ShortCut := BEnhance.ShortCut;
  mGoOn.ShortCut := BGoOn.ShortCut;
  mHome.ShortCut := BHome.ShortCut;
  mFarm.ShortCut := BFarmClearIrrigation.ShortCut;
  mClear.ShortCut := BFarmClearIrrigation.ShortCut;
  mIrrigation.ShortCut := BFarmClearIrrigation.ShortCut;
  mLoad.ShortCut := BLoad.ShortCut;
  mAfforest.ShortCut := BAfforestMine.ShortCut;
  mMine.ShortCut := BAfforestMine.ShortCut;
  mCanal.ShortCut := BCanal.ShortCut;
  MTrans.ShortCut := BTrans.ShortCut;
  mPollution.ShortCut := BPollution.ShortCut;
  mRailRoad.ShortCut := BRailRoad.ShortCut;
  mRoad.ShortCut := BRailRoad.ShortCut;
  mUnload.ShortCut := BUnload.ShortCut;
  mRecover.ShortCut := BRecover.ShortCut;
  mUtilize.ShortCut := BUtilize.ShortCut;
end;

procedure TMainScreen.SetFullScreen(Active: Boolean);
begin
    if Active and (CurrentWindowState <> wsFullScreen) then begin
      PrevWindowState := WindowState;
      CurrentWindowState := wsFullScreen;
      WindowState := CurrentWindowState;
      {$IFDEF WINDOWS}
      BorderStyle := bsNone;
      {$ENDIF}
      BorderIcons := [];
    end else
    if not Active and (CurrentWindowState = wsFullScreen) then begin
      if PrevWindowState = wsMaximized then begin
        CurrentWindowState := wsMaximized;
        WindowState := CurrentWindowState;
      end else begin
        CurrentWindowState := wsNormal;
        WindowState := CurrentWindowState;
        WindowState := wsFullScreen;
        WindowState := CurrentWindowState;
      end;
      {$IFDEF WINDOWS}
      BorderStyle := bsSizeable;
      {$ENDIF}
      BorderIcons := [biSystemMenu, biMinimize, biMaximize];
    end;
end;

procedure TMainScreen.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  procedure MenuClick_Check(Popup: TPopupMenu; Item: TMenuItem);
  begin
    InitPopup(Popup);
    if Item.Visible and Item.Enabled then
      Item.Click;
  end;

  procedure SetViewpointMe(P: Integer);
  begin
    if P = Me then SetViewpoint(P)
      else SetViewpoint(P);
  end;

  procedure DoMoveUnit(X, Y: Integer);
  begin
    DestinationMarkON := False;
    PaintDestination;
    MyUn[UnFocus].Status := MyUn[UnFocus].Status and
      ($FFFF - usStay - usRecover - usGoto - usEnhance) or usWaiting;
    MoveUnit(X, Y, muAutoNext);
  end;

var
  Time0, Time1: TDateTime;
  ShortCut: TShortCut;
begin
  ShortCut := KeyToShortCut(Key, Shift);

  if GameMode = cMovie then begin
    if BScienceStat.Test(ShortCut) then MenuClick_Check(StatPopup, mScienceStat)
    else if BDiagram.Test(ShortCut) then MenuClick_Check(StatPopup, mDiagram)
    else if BWonders.Test(ShortCut) then MenuClick_Check(StatPopup, mWonders)
    else if BShips.Test(ShortCut) then MenuClick_Check(StatPopup, mShips);
    Exit;
  end;

  if not Idle then Exit;

  if ClientMode = cEditMap then begin
    if BResign.Test(ShortCut) then mResign.Click
    else if BRandomMap.Test(ShortCut) then mRandomMap.Click
    else if BHelp.Test(ShortCut) then mHelp.Click;
    (*if Shift = [ssCtrl] then
      case char(Key) of
         'A':
          begin // auto symmetry
          Server($7F0,Me,0,nil^);
          MapValid:=False;
          PaintAll;
          end;
          'B':
          begin // land mass
          dy:=0;
          for dx:=G.lx to G.lx*(G.ly-1)-1 do
          if MyMap[dx] and fTerrain>=fGrass then Inc(dy);
          dy:=dy
          end;
      end;
    *)
    Exit;
  end;

  if BEndTurn.Test(ShortCut) then EndTurn
  else if BFullScreen.Test(ShortCut) then begin
    FullScreen := not FullScreen;
    SetFullScreen(FullScreen);
  end
  else if BHelp.Test(ShortCut) then mHelp.Click
  else if BUnitStat.Test(ShortCut) then MenuClick_Check(StatPopup, mUnitStat)
  else if BCityStat.Test(ShortCut) then MenuClick_Check(StatPopup, mCityStat)
  else if BScienceStat.Test(ShortCut) then MenuClick_Check(StatPopup, mScienceStat)
  else if BEUnitStat.Test(ShortCut) then MenuClick_Check(StatPopup, mEUnitStat)
  else if BDiagram.Test(ShortCut) then MenuClick_Check(StatPopup, mDiagram)
  else if BWonders.Test(ShortCut) then MenuClick_Check(StatPopup, mWonders)
  else if BShips.Test(ShortCut) then MenuClick_Check(StatPopup, mShips)
  else if BNations.Test(ShortCut) then MenuClick_Check(StatPopup, mNations)
  else if BEmpire.Test(ShortCut) then MenuClick_Check(StatPopup, mEmpire)

  else if BSetDebugMap0.Test(ShortCut) then SetDebugMap(-1)
  else if BSetDebugMap1.Test(ShortCut) then SetDebugMap(1)
  else if BSetDebugMap2.Test(ShortCut) then SetDebugMap(2)
  else if BSetDebugMap3.Test(ShortCut) then SetDebugMap(3)
  else if BSetDebugMap4.Test(ShortCut) then SetDebugMap(4)
  else if BSetDebugMap5.Test(ShortCut) then SetDebugMap(5)
  else if BSetDebugMap6.Test(ShortCut) then SetDebugMap(6)
  else if BSetDebugMap7.Test(ShortCut) then SetDebugMap(7)
  else if BSetDebugMap8.Test(ShortCut) then SetDebugMap(8)
  else if BSetDebugMap9.Test(ShortCut) then SetDebugMap(9)

  else if BJump.Test(ShortCut) then mJump.Click
  else if BDebugMap.Test(ShortCut) then mShowClick(mDebugMap)
  else if BLocCodes.Test(ShortCut) then mShowClick(mLocCodes)
  else if BLogDlg.Test(ShortCut) then begin
    if LogDlg.Visible then LogDlg.Close
      else LogDlg.Show;
  end
  else if BNames.Test(ShortCut) then mNamesClick(mNames)
  else if BResign.Test(ShortCut) then MenuClick_Check(GamePopup, mResign)
  else if BRun.Test(ShortCut) then mRun.Click
  else if BTestMapRepaint.Test(ShortCut) then begin // test map repaint time
    Time0 := NowPrecise;
    MapValid := False;
    MainOffscreenPaint;
    Time1 := NowPrecise;
    SimpleMessage(Format('Map repaint time: %.3f ms',
      [(Time1 - Time0) / OneMillisecond]));
  end
  else if BSetViewpoint0.Test(ShortCut) then SetViewpointMe(0)
  else if BSetViewpoint1.Test(ShortCut) then SetViewpointMe(1)
  else if BSetViewpoint2.Test(ShortCut) then SetViewpointMe(2)
  else if BSetViewpoint3.Test(ShortCut) then SetViewpointMe(3)
  else if BSetViewpoint4.Test(ShortCut) then SetViewpointMe(4)
  else if BSetViewpoint5.Test(ShortCut) then SetViewpointMe(5)
  else if BSetViewpoint6.Test(ShortCut) then SetViewpointMe(6)
  else if BSetViewpoint7.Test(ShortCut) then SetViewpointMe(7)
  else if BSetViewpoint8.Test(ShortCut) then SetViewpointMe(8)
  else if BSetViewpoint9.Test(ShortCut) then SetViewpointMe(9)

  else if BMapBtn0.Test(ShortCut) then MapBtnClick(MapBtn0)
  else if BMapBtn1.Test(ShortCut) then MapBtnClick(MapBtn1)
  else if BMapBtn4.Test(ShortCut) then MapBtnClick(MapBtn4)
  else if BMapBtn5.Test(ShortCut) then MapBtnClick(MapBtn5)
  else if BMapBtn6.Test(ShortCut) then MapBtnClick(MapBtn6)
  else if BTechTree.Test(ShortCut) then mTechTree.Click
  else if BWait.Test(ShortCut) then mWait.Click;

  if UnFocus >= 0 then begin
    if BDisbandUnit.Test(ShortCut) then mDisband.Click
    else if BFortify.Test(ShortCut) then MenuClick_Check(TerrainPopup, mFort)
    else if BCenterUnit.Test(ShortCut) then mCentre.Click
    else if BStay.Test(ShortCut) then mStay.Click
    else if BNoOrders.Test(ShortCut) then mNoOrders.Click
    else if BPrevUnit.Test(ShortCut) then mPrevUnit.Click
    else if BNextUnit.Test(ShortCut) then mNextUnit.Click
    else if BCancel.Test(ShortCut) then MenuClick_Check(UnitPopup, mCancel)
    else if BPillage.Test(ShortCut) then MenuClick_Check(UnitPopup, mPillage)
    else if BSelectTransport.Test(ShortCut) then MenuClick_Check(UnitPopup, mSelectTransport)
    else if BAirBase.Test(ShortCut) then MenuClick_Check(TerrainPopup, mAirBase)
    else if BBuildCity.Test(ShortCut) then MenuClick_Check(UnitPopup, mCity)
    else if BEnhance.Test(ShortCut) then begin
      InitPopup(TerrainPopup);
      if mEnhance.Visible and mEnhance.Enabled then mEnhance.Click
        else mEnhanceDef.Click
    end
    else if BGoOn.Test(ShortCut) then MenuClick_Check(UnitPopup, mGoOn)
    else if BHome.Test(ShortCut) then MenuClick_Check(UnitPopup, mHome)
    else if BFarmClearIrrigation.Test(ShortCut) then begin
      if JobTest(UnFocus, jFarm, [eTreaty]) then
        mFarm.Click
      else if JobTest(UnFocus, jClear, [eTreaty]) then
        mClear.Click
      else MenuClick_Check(TerrainPopup, mIrrigation);
    end
    else if BLoad.Test(ShortCut) then MenuClick_Check(UnitPopup, mLoad)
    else if BAfforestMine.Test(ShortCut) then begin
      if JobTest(UnFocus, jAfforest, [eTreaty]) then mAfforest.Click
        else MenuClick_Check(TerrainPopup, mMine);
    end
    else if BCanal.Test(ShortCut) then MenuClick_Check(TerrainPopup, mCanal)
    else if BTrans.Test(ShortCut) then MenuClick_Check(TerrainPopup, MTrans)
    else if BPollution.Test(ShortCut) then MenuClick_Check(TerrainPopup, mPollution)
    else if BRailRoad.Test(ShortCut) then begin
      if JobTest(UnFocus, jRR, [eTreaty]) then mRailRoad.Click
        else MenuClick_Check(TerrainPopup, mRoad);
    end
    else if BUnload.Test(ShortCut) then MenuClick_Check(UnitPopup, mUnload)
    else if BRecover.Test(ShortCut) then MenuClick_Check(UnitPopup, mRecover)
    else if BUtilize.Test(ShortCut) then MenuClick_Check(UnitPopup, mUtilize)
    else if BMoveLeftDown.Test(ShortCut) then DoMoveUnit(-1, 1)
    else if BMoveDown.Test(ShortCut) then DoMoveUnit(0, 2)
    else if BMoveRightDown.Test(ShortCut) then DoMoveUnit(1, 1)
    else if BMoveLeft.Test(ShortCut) then DoMoveUnit(-2, 0)
    else if BMoveRight.Test(ShortCut) then DoMoveUnit(2, 0)
    else if BMoveLeftUp.Test(ShortCut) then DoMoveUnit(-1, -1)
    else if BMoveUp.Test(ShortCut) then  DoMoveUnit(0, -2)
    else if BMoveRightUp.Test(ShortCut) then DoMoveUnit(1, -1);
  end;
end;

function TMainScreen.DoJob(j0: Integer): Integer;
var
  Loc0, Movement0: Integer;
begin
  with MyUn[UnFocus] do
  begin
    DestinationMarkON := False;
    PaintDestination;
    Loc0 := Loc;
    Movement0 := Movement;
    if j0 < 0 then
      Result := ProcessEnhancement(UnFocus, MyData.EnhancementJobs)
      // terrain enhancement
    else
      Result := Server(sStartJob + j0 shl 4, Me, UnFocus, nil^);
    if Result >= rExecuted then
    begin
      if Result = eDied then
        UnFocus := -1;
      PaintLoc(Loc0);
      if UnFocus >= 0 then
      begin
        if (j0 < 0) and (Result <> eJobDone) then
          // multi-turn terrain enhancement
          Status := Status and ($FFFF - usStay - usRecover - usGoto) or
            usEnhance
        else
          Status := Status and
            ($FFFF - usStay - usRecover - usGoto - usEnhance);
        if (Job <> jNone) or (Movement0 < 100) then
        begin
          Status := Status and not usWaiting;
          NextUnit(UnStartLoc, True);
        end
        else
          PanelPaint;
      end
      else
        NextUnit(UnStartLoc, True);
    end;
  end;
  case Result of
    eNoBridgeBuilding:
      SoundMessage(Phrases.Lookup('NOBB'), 'INVALID');
    eNoCityTerrain:
      SoundMessage(Phrases.Lookup('NOCITY'), 'INVALID');
    eTreaty:
      SoundMessage(Tribe[MyRO.Territory[Loc0]].TPhrase('PEACE_NOWORK'),
        'NOMOVE_TREATY');
  else
    if Result < rExecuted then
      Play('INVALID');
  end;
end;

procedure TMainScreen.mDisbandOrUtilizeClick(Sender: TObject);
var
  Loc0: Integer;
begin
  if UnFocus >= 0 then
  with TUn(MyUn[UnFocus]) do begin
    if (Sender = mUtilize) and
      not(Server(sRemoveUnit - sExecute, Me, UnFocus, nil^) = eUtilized) then
    begin
      SimpleMessage(Phrases2.Lookup('SHIP_UTILIZE'));
      // freight for colony ship is the only case in which the command is
      // available to player though not valid
      Exit;
    end;
    if (Sender = mUtilize) and (Health < 100) then
      if SimpleQuery(mkYesNo, Phrases.Lookup('DAMAGED_UTILIZE'), '') <> mrOK
      then
        Exit;
    Loc0 := Loc;
    CityOptimizer_BeforeRemoveUnit(UnFocus);
    if Server(sRemoveUnit, Me, UnFocus, nil^) = eUtilized then
      Play('CITY_UTILIZE')
    else
      Play('DISBAND');
    CityOptimizer_AfterRemoveUnit;
    SetTroopLoc(Loc0);
    UpdateViews(True);
    DestinationMarkON := False;
    PaintDestination;
    UnFocus := -1;
    PaintLoc(Loc0);
    NextUnit(UnStartLoc, True);
  end;
end;

procedure TMainScreen.InitPopup(Popup: TPopupMenu);
var
  I, p1, Tile, Test: Integer;
  NoSuper, Extended, Multi, NeedSep, HaveCities: Boolean;
  LastSep, M: TMenuItem;
  mox: ^TModel;
begin
  NoSuper := not Supervising and (1 shl Me and MyRO.Alive <> 0);
  HaveCities := False;
  for I := 0 to MyRO.nCity - 1 do
    if MyCity[I].Loc >= 0 then
    begin
      HaveCities := True;
      Break;
    end;
  if Popup = GamePopup then
  begin
    mTechTree.Visible := ClientMode <> cEditMap;
    mResign.Enabled := Supervising or (Me = 0) and (ClientMode < scContact);
    mRandomMap.Visible := (ClientMode = cEditMap) and
      (Server(sMapGeneratorRequest, Me, 0, nil^) = eOK);
    mOptions.Visible := ClientMode <> cEditMap;
    mManip.Visible := ClientMode <> cEditMap;
    if ClientMode <> cEditMap then
    begin
      mWaitTurn.Visible := NoSuper;
      mRep.Visible := NoSuper;
      mRepList.Visible := NoSuper;
      mRepScreens.Visible := NoSuper;
      N10.Visible := NoSuper;
      mOwnMovement.Visible := NoSuper;
      mAllyMovement.Visible := NoSuper;
      case SoundMode of
        smOff:
          mSoundOff.Checked := True;
        smOn:
          mSoundOn.Checked := True;
        smOnAlt:
          mSoundOnAlt.Checked := True;
      end;

      for I := 0 to nTestFlags - 1 do
        mManip[I].Checked := MyRO.TestFlags and (1 shl I) <> 0;
      mManip.Enabled := Supervising or (Me = 0);

      Multi := False;
      for p1 := 1 to nPl - 1 do
        if G.RO[p1] <> nil then
          Multi := True;
      mEnemyMovement.Visible := not Multi;
    end;
    mMacro.Visible := NoSuper and (ClientMode < scContact);
    if NoSuper and (ClientMode < scContact) then
    begin
      mCityTypes.Enabled := False;
      // check if city types already usefull:
      if MyRO.nCity > 0 then
        for I := nWonder to nImp - 1 do
          if (I <> imTrGoods) and (Imp[I].Kind = ikCommon) and
            (Imp[I].Preq <> preNA) and
            ((Imp[I].Preq = preNone) or (MyRO.Tech[Imp[I].Preq] >= tsApplicable))
          then
          begin
            mCityTypes.Enabled := True;
            Break
          end;
    end;
    mViewpoint.Visible := (ClientMode <> cEditMap) and Supervising;
    mViewpoint.Enabled := G.RO[0].Turn > 0;
    if Supervising then
    begin
      EmptyMenu(mViewpoint);
      for p1 := 0 to nPl - 1 do
        if (p1 = 0) or (1 shl p1 and G.RO[0].Alive <> 0) then
        begin
          M := TMenuItem.Create(mViewpoint);
          if p1 = 0 then
            M.Caption := Phrases.Lookup('SUPER')
          else
            M.Caption := Tribe[p1].TString(Phrases2.Lookup('BELONG'));
          M.Tag := p1;
          M.OnClick := ViewpointClick;
          if p1 < 10 then
            M.ShortCut := ShortCut(48 + p1, [ssCtrl]);
          M.RadioItem := True;
          if p1 = Me then
            M.Checked := True;
          mViewpoint.Add(M);
        end
    end;
    mDebugMap.Visible := (ClientMode <> cEditMap) and Supervising;
    if Supervising then
    begin
      EmptyMenu(mDebugMap);
      for p1 := 0 to nPl - 1 do
        if (p1 = 0) or (1 shl p1 and G.RO[0].Alive <> 0) then
        begin
          M := TMenuItem.Create(mDebugMap);
          if p1 = 0 then
            M.Caption := Phrases2.Lookup('MENU_DEBUGMAPOFF')
          else
            M.Caption := Tribe[p1].TString(Phrases2.Lookup('BELONG'));
          if p1 = 0 then
            M.Tag := -1
          else
            M.Tag := p1;
          M.OnClick := DebugMapClick;
          if p1 < 10 then
            M.ShortCut := ShortCut(48 + p1, [ssAlt]);
          M.RadioItem := True;
          if M.Tag = MainMap.pDebugMap then
            M.Checked := True;
          mDebugMap.Add(M);
        end;
    end;
    mSmallTiles.Checked := MainMap.TileSize = tsSmall;
    mNormalTiles.Checked := MainMap.TileSize = tsMedium;
    mBigTiles.Checked := MainMap.TileSize = tsBig;
  end
  else if Popup = StatPopup then
  begin
    mEmpire.Visible := NoSuper;
    mEmpire.Enabled := MyRO.Government <> gAnarchy;
    mRevolution.Visible := NoSuper;
    mRevolution.Enabled := (MyRO.Government <> gAnarchy) and
      (ClientMode < scContact);
    mUnitStat.Enabled := NoSuper or (MyRO.Turn > 0);
    mCityStat.Visible := 1 shl Me and MyRO.Alive <> 0;
    mCityStat.Enabled := HaveCities;
    mScienceStat.Visible := True;
    mScienceStat.Enabled := not NoSuper or (MyRO.ResearchTech >= 0) or
      (MyRO.Happened and phTech <> 0) or (MyRO.Happened and phGameEnd <> 0)
    // no researchtech in case just completed
      or (MyRO.TestFlags and (tfAllTechs or tfUncover or tfAllContact) <> 0);
    mEUnitStat.Enabled := MyRO.nEnemyModel > 0;
    { mWonders.Enabled:= false;
      for I:=0 to nWonder - 1 do if MyRO.Wonder[I].CityID <> WonderNotBuiltYet then
      mWonders.Enabled:=True; }
    mDiagram.Enabled := MyRO.Turn >= 2;
    mShips.Enabled := False;
    for p1 := 0 to nPl - 1 do
      if MyRO.Ship[p1].Parts[spComp] + MyRO.Ship[p1].Parts[spPow] +
        MyRO.Ship[p1].Parts[spHab] > 0 then
        mShips.Enabled := True;
  end
  else if Popup = UnitPopup then
  begin
    mox := @MyModel[MyUn[UnFocus].mix];
    Tile := MyMap[MyUn[UnFocus].Loc];
    Extended := Tile and fCity = 0;
    if Extended then
    begin
      mCity.Caption := Phrases.Lookup('BTN_FOUND');
      mHome.Caption := Phrases.Lookup('BTN_MOVEHOME')
    end
    else
    begin
      mCity.Caption := Phrases.Lookup('BTN_ADD');
      mHome.Caption := Phrases.Lookup('BTN_SETHOME')
    end;

    Extended := Extended and ((mox.Kind = mkSettler) or (mox.Kind = mkSlaves)
      and (MyRO.Wonder[woPyramids].EffectiveOwner >= 0)) and
      (MyUn[UnFocus].Master < 0) and (Tile and fDeadLands = 0);
    if (mox.Kind = mkFreight) and (Tile and fCity <> 0) and
      not Phrases2FallenBackToEnglish or
      (Server(sRemoveUnit - sExecute, Me, UnFocus, nil^) = eUtilized) then
    begin
      mDisband.Visible := False;
      mUtilize.Visible := True;
      if mox.Kind = mkFreight then
        mUtilize.Caption := Phrases.Lookup('UTILIZE')
      else
        mUtilize.Caption := Phrases.Lookup('INTEGRATE')
    end
    else
    begin
      mDisband.Visible := True;
      mUtilize.Visible := False
    end;
    mGoOn.Visible := MyUn[UnFocus].Status and (usGoto or usWaiting) = usGoto or
      usWaiting;
    mHome.Visible := HaveCities;
    mRecover.Visible := (MyUn[UnFocus].Health < 100) and
      (Tile and fTerrain >= fGrass) and
      ((MyRO.Wonder[woGardens].EffectiveOwner = Me) or
      (Tile and fTerrain <> fArctic) and (Tile and fTerrain <> fDesert)) and
      not((mox.Domain = dAir) and (Tile and fCity = 0) and
      (Tile and fTerImp <> tiBase));
    mStay.Visible := not((mox.Domain = dAir) and (Tile and fCity = 0) and
      (Tile and fTerImp <> tiBase));
    mCity.Visible := Extended and (mox.Kind = mkSettler) or
      (Tile and fCity <> 0) and ((mox.Kind in [mkSettler, mkSlaves]) or
      (MyUn[UnFocus].Flags and unConscripts <> 0));
    mPillage.Visible := (Tile and (fRoad or fRR or fCanal or fTerImp) <> 0) and
      (MyUn[UnFocus].Master < 0) and (mox.Domain = dGround);
    mCancel.Visible := (MyUn[UnFocus].Job > jNone) or
      (MyUn[UnFocus].Status and (usRecover or usGoto) <> 0);

    Test := Server(sLoadUnit - sExecute, Me, UnFocus, nil^);
    mLoad.Visible := (Test >= rExecuted) or (Test = eNoTime_Load);
    mUnload.Visible := (MyUn[UnFocus].Master >= 0) or
      (MyUn[UnFocus].TroopLoad + MyUn[UnFocus].AirLoad > 0);
    mSelectTransport.Visible := Server(sSelectTransport - sExecute, Me, UnFocus,
      nil^) >= rExecuted;
  end
  else { if Popup=TerrainPopup then }
  begin
    mox := @MyModel[MyUn[UnFocus].mix];
    Tile := MyMap[MyUn[UnFocus].Loc];
    Extended := Tile and fCity = 0;

    if (Tile and fRiver <> 0) and (MyRO.Tech[adBridgeBuilding] >= tsApplicable)
    then
    begin
      mRoad.Caption := Phrases.Lookup('BTN_BUILDBRIDGE');
      mRailRoad.Caption := Phrases.Lookup('BTN_BUILDRRBRIDGE');
    end
    else
    begin
      mRoad.Caption := Phrases.Lookup('BTN_BUILDROAD');
      mRailRoad.Caption := Phrases.Lookup('BTN_BUILDRR');
    end;
    if Tile and fTerrain = fForest then
      mClear.Caption := Phrases.Lookup('BTN_CLEAR')
    else if Tile and fTerrain = fDesert then
      mClear.Caption := Phrases.Lookup('BTN_UNDESERT')
    else
      mClear.Caption := Phrases.Lookup('BTN_DRAIN');

    Extended := Extended and ((mox.Kind = mkSettler) or (mox.Kind = mkSlaves)
      and (MyRO.Wonder[woPyramids].EffectiveOwner >= 0)) and
      (MyUn[UnFocus].Master < 0);
    if Extended then
    begin
      mRoad.Visible := JobTest(UnFocus, jRoad, [eNoBridgeBuilding, eTreaty]);
      mRailRoad.Visible := JobTest(UnFocus, jRR, [eNoBridgeBuilding, eTreaty]);
      mClear.Visible := JobTest(UnFocus, jClear, [eTreaty]);
      mIrrigation.Visible := JobTest(UnFocus, jIrr, [eTreaty]);
      mFarm.Visible := JobTest(UnFocus, jFarm, [eTreaty]);
      mAfforest.Visible := JobTest(UnFocus, jAfforest, [eTreaty]);
      mMine.Visible := JobTest(UnFocus, jMine, [eTreaty]);
      MTrans.Visible := JobTest(UnFocus, jTrans, [eTreaty]);
      mCanal.Visible := JobTest(UnFocus, jCanal, [eTreaty]);
      mFort.Visible := JobTest(UnFocus, jFort, [eTreaty]);
      mAirBase.Visible := JobTest(UnFocus, jBase, [eTreaty]);
      mPollution.Visible := JobTest(UnFocus, jPoll, [eTreaty]);
      mEnhance.Visible := (Tile and fDeadLands = 0) and
        (MyData.EnhancementJobs[MyMap[MyUn[UnFocus].Loc] and fTerrain, 0]
        <> jNone);
    end
    else
    begin
      for I := 0 to Popup.Items.Count - 1 do
        Popup.Items[I].Visible := False;
    end;
  end;

  // set menu seperators
  LastSep := nil;
  NeedSep := False;
  for I := 0 to Popup.Items.Count - 1 do
    if Popup.Items[I].Caption = '-' then
    begin
      Popup.Items[I].Visible := NeedSep;
      if NeedSep then
        LastSep := Popup.Items[I];
      NeedSep := False
    end
    else if Popup.Items[I].Visible then
      NeedSep := True;
  if (LastSep <> nil) and not NeedSep then
    LastSep.Visible := False
end;

procedure TMainScreen.PanelBtnClick(Sender: TObject);
var
  Popup: TPopupMenu;
begin
  if Sender = UnitBtn then
    Popup := UnitPopup
  else { if Sender=TerrainBtn then }
    Popup := TerrainPopup;
  InitPopup(Popup);
  if FullScreen then
    Popup.Popup(Left + TControl(Sender).Left, Top + TControl(Sender).Top)
  else
    Popup.Popup(Left + TControl(Sender).Left + 4, Top + TControl(Sender).Top +
      GetSystemMetrics(SM_CYCAPTION) + 4);
end;

procedure TMainScreen.CityClosed(Activateuix: Integer; StepFocus: Boolean;
  SelectFocus: Boolean);
begin
  if Supervising then
  begin
    SetTroopLoc(-1);
    PanelPaint;
  end
  else
  begin
    if Activateuix >= 0 then
    begin
      SetUnFocus(Activateuix);
      SetTroopLoc(MyUn[Activateuix].Loc);
      if SelectFocus then
        FocusOnLoc(TroopLoc, flRepaintPanel)
      else
        PanelPaint;
    end
    else if StepFocus then
      NextUnit(TroopLoc, True)
    else
    begin
      SetTroopLoc(-1);
      PanelPaint;
    end;
  end;
end;

procedure TMainScreen.Toggle(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
end;

procedure TMainScreen.PanelBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  xCentre, yCentre: Integer;
begin
  if Tracking and (ssLeft in Shift) then
  with MainMap do begin
    if (X >= xMini + 2) and (Y >= yMini + 2) and (X < xMini + 2 + 2 * G.lx) and
      (Y < yMini + 2 + G.ly) then
    begin
      xCentre := (xwMini + (X - xMini - 2) div 2 + G.lx div 2 +
        MapWidth div (xxt * 4)) mod G.lx;
      yCentre := (Y - yMini - 2);
      xw := (xCentre - MapWidth div (xxt * 4) + G.lx) mod G.lx;
      if ywmax <= 0 then
        yw := ywcenter
      else
      begin
        yw := (yCentre - MapHeight div (yyt * 2) + 1) and not 1;
        if yw < 0 then
          yw := 0
        else if yw > ywmax then
          yw := ywmax;
      end;
      BitBltCanvas(Buffer.Canvas, 0, 0, G.lx * 2, G.ly, MiniMap.Bitmap.Canvas, 0, 0);
      if ywmax <= 0 then
        Frame(Buffer.Canvas, X - xMini - 2 - MapWidth div (xxt * 2), 0,
          X - xMini - 2 + MapWidth div (xxt * 2) - 1, G.ly - 1,
          MainTexture.ColorMark, MainTexture.ColorMark)
      else
        Frame(Buffer.Canvas, X - xMini - 2 - MapWidth div (xxt * 2), yw,
          X - xMini - 2 + MapWidth div (xxt * 2) - 1, yw + MapHeight div yyt -
          2, MainTexture.ColorMark, MainTexture.ColorMark);
      BitBltCanvas(Panel.Canvas, xMini + 2, yMini + 2, G.lx * 2, G.ly,
        Buffer.Canvas, 0, 0);
      MainOffscreenPaint;
      RectInvalidate(xMini + 2, TopBarHeight + MapHeight - overlap + yMini + 2,
        xMini + 2 + G.lx * 2, TopBarHeight + MapHeight - overlap + yMini +
        2 + G.ly);
      Update;
    end;
  end
  else
    Tracking := False;
end;

procedure TMainScreen.PanelBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Tracking then
  begin
    Tracking := False;
    xwMini := xw;
    ywMini := yw;
    MiniMapPaint;
    PanelPaint;
  end;
end;

procedure TMainScreen.MapBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  MouseLoc: Integer;
begin
  xMouse := X;
  yMouse := Y;
  if (ClientMode = cEditMap) and (ssLeft in Shift) and not Tracking then
  begin
    MouseLoc := LocationOfScreenPixel(X, Y);
    if MouseLoc <> BrushLoc then
      MapBoxMouseDown(nil, mbLeft, Shift, X, Y);
  end
  (* else if Idle and (UnFocus>=0) then
    begin
    qx:=(xMouse*32+yMouse*66+16*66) div(32*66)-1;
    qy:=(yMouse*66-xMouse*32-16*66+2000*33*32) div(32*66)-999;
    MouseLoc:=(xw+(qx-qy+2048) div 2-1024+G.lx) mod G.lx+G.lx*(yw+qx+qy);
    ShowMoveHint(MouseLoc);
    end *)
end;

procedure TMainScreen.mShowClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  SetMapOptions;
  MapValid := False;
  PaintAllMaps;
end;

procedure TMainScreen.mNamesClick(Sender: TObject);
var
  p1: Integer;
begin
  mNames.Checked := not mNames.Checked;
  GenerateNames := mNames.Checked;
  for p1 := 0 to nPl - 1 do
    if Tribe[p1] <> nil then
      if GenerateNames then
        Tribe[p1].NumberName := -1
      else
        Tribe[p1].NumberName := p1;
  MapValid := False;
  PaintAll;
end;

function TMainScreen.IsPanelPixel(X, Y: Integer): Boolean;
begin
  Result := (Y >= TopBarHeight + MapHeight) or (Y >= ClientHeight - PanelHeight)
    and ((X < xMidPanel) or (X >= xRightPanel));
end;

procedure TMainScreen.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Idle then
    if (X < 40) and (Y < 40) then
    begin
      if GameMode <> cMovie then
      begin
        InitPopup(GamePopup);
        if FullScreen then
          GamePopup.Popup(Left, Top + TopBarHeight - 1)
        else
          GamePopup.Popup(Left + 4, Top + GetSystemMetrics(SM_CYCAPTION) + 4 +
            TopBarHeight - 1);
      end;
    end
    else if IsPanelPixel(X, Y) then
      PanelBoxMouseDown(Sender, Button, Shift, X,
        Y - (ClientHeight - PanelHeight))
    else if (Y >= TopBarHeight) and (X >= MapOffset) and
      (X < MapOffset + MapWidth) then
      MapBoxMouseDown(Sender, Button, Shift, X - MapOffset, Y - TopBarHeight)
end;

procedure TMainScreen.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Idle then
    if IsPanelPixel(X, Y) then
      PanelBoxMouseMove(Sender, Shift, X, Y - (ClientHeight - PanelHeight))
    else if (Y >= TopBarHeight) and (X >= MapOffset) and
      (X < MapOffset + MapWidth) then
      MapBoxMouseMove(Sender, Shift, X - MapOffset, Y - TopBarHeight);
end;

procedure TMainScreen.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Idle then
    PanelBoxMouseUp(Sender, Button, Shift, X, Y - (ClientHeight - PanelHeight));
end;

procedure TMainScreen.FormPaint(Sender: TObject);
begin
  MainOffscreenPaint;
  if (MapOffset > 0) or (MapOffset + MapWidth < ClientWidth) then
    with Canvas do
    begin // pillarbox, make left and right border black
      if Me < 0 then
        Brush.Color := $000000
      else
        Brush.Color := EmptySpaceColor;
      if xMidPanel > MapOffset then
        FillRect(Rect(0, TopBarHeight, MapOffset, TopBarHeight + MapHeight
          - overlap))
      else
      begin
        FillRect(Rect(0, TopBarHeight, xMidPanel, TopBarHeight + MapHeight -
          overlap));
        FillRect(Rect(xMidPanel, TopBarHeight, MapOffset,
          TopBarHeight + MapHeight));
      end;
      if xRightPanel < MapOffset + MapWidth then
        FillRect(Rect(MapOffset + MapWidth, TopBarHeight, ClientWidth,
          TopBarHeight + MapHeight - overlap))
      else
      begin
        FillRect(Rect(MapOffset + MapWidth, TopBarHeight, xRightPanel,
          TopBarHeight + MapHeight));
        FillRect(Rect(xRightPanel, TopBarHeight, ClientWidth,
          TopBarHeight + MapHeight - overlap));
      end;
      Brush.Style := bsClear;
    end;
  BitBltCanvas(Canvas, MapOffset, TopBarHeight, MapWidth, MapHeight - overlap,
    Offscreen.Canvas, 0, 0);
  BitBltCanvas(Canvas, 0, 0, ClientWidth, TopBarHeight, TopBar.Canvas,
    0, 0);
  if xMidPanel > MapOffset then
    BitBltCanvas(Canvas, xMidPanel, TopBarHeight + MapHeight - overlap,
      ClientWidth div 2 - xMidPanel, overlap, Offscreen.Canvas,
      xMidPanel - MapOffset, MapHeight - overlap)
  else
    BitBltCanvas(Canvas, MapOffset, TopBarHeight + MapHeight - overlap,
      ClientWidth div 2 - MapOffset, overlap, Offscreen.Canvas, 0,
      MapHeight - overlap);
  if xRightPanel < MapOffset + MapWidth then
    BitBltCanvas(Canvas, ClientWidth div 2, TopBarHeight + MapHeight - overlap,
      xRightPanel - ClientWidth div 2, overlap, Offscreen.Canvas,
      ClientWidth div 2 - MapOffset, MapHeight - overlap)
  else
    BitBltCanvas(Canvas, ClientWidth div 2, TopBarHeight + MapHeight - overlap,
      MapOffset + MapWidth - ClientWidth div 2, overlap,
      Offscreen.Canvas, ClientWidth div 2 - MapOffset,
      MapHeight - overlap);
  BitBltCanvas(Canvas, 0, TopBarHeight + MapHeight - overlap, xMidPanel,
    overlap, Panel.Canvas, 0, 0);
  BitBltCanvas(Canvas, xRightPanel, TopBarHeight + MapHeight - overlap,
    Panel.width - xRightPanel, overlap, Panel.Canvas, xRightPanel, 0);
  BitBltCanvas(Canvas, 0, TopBarHeight + MapHeight, Panel.width,
    PanelHeight - overlap, Panel.Canvas, 0, overlap);
  if (pLogo >= 0) and (G.RO[pLogo] = nil) and (AILogo[pLogo] <> nil) then
    BitBltCanvas(Canvas, xRightPanel + 10 - (16 + 64),
      ClientHeight - PanelHeight, 64, 64, AILogo[pLogo].Canvas, 0, 0);
end;

procedure TMainScreen.RectInvalidate(Left, Top, Rigth, Bottom: Integer);
var
  r0: HRgn;
begin
  r0 := CreateRectRgn(Left, Top, Rigth, Bottom);
  InvalidateRgn(Handle, r0, False);
  DeleteObject(r0);
end;

procedure TMainScreen.SmartRectInvalidate(Left, Top, Rigth, Bottom: Integer);
var
  I: Integer;
  r0, r1: HRgn;
begin
  r0 := CreateRectRgn(Left, Top, Rigth, Bottom);
  for I := 0 to ControlCount - 1 do
    if not(Controls[I] is TArea) and Controls[I].Visible then
    begin
      with Controls[I].BoundsRect do
        r1 := CreateRectRgn(Left, Top, Right, Bottom);
      CombineRgn(r0, r0, r1, RGN_DIFF);
      DeleteObject(r1);
    end;
  InvalidateRgn(Handle, r0, False);
  DeleteObject(r0);
end;

procedure TMainScreen.LoadSettings;
var
  Reg: TRegistry;
  DefaultOptionChecked: TSaveOptions;
begin
  DefaultOptionChecked := [soEnMoves, soSlowMoves, soNames, soRepScreens,
    soSoundOn, soScrollOff, soAlSlowMoves];
  Reg := TRegistry.Create;
  with Reg do try
    OpenKey(AppRegistryKey, False);
    if ValueExists('TileSize') then MainMap.TileSize := TTileSize(ReadInteger('TileSize'))
      else MainMap.TileSize := tsMedium;
    NoMap.TileSize := MainMap.TileSize;
    if ValueExists('OptionChecked') then OptionChecked := TSaveOptions(ReadInteger('OptionChecked'))
      else OptionChecked := DefaultOptionChecked;
    if ValueExists('MapOptionChecked') then MapOptionChecked := TMapOptions(ReadInteger('MapOptionChecked'))
      else MapOptionChecked := [moCityNames];
    if ValueExists('CityReport') then CityRepMask := Cardinal(ReadInteger('CityReport'))
      else CityRepMask := Cardinal(not chPopIncrease and not chNoGrowthWarning and
          not chCaptured);
    if (not (soScrollFast in OptionChecked)) and (not (soScrollSlow in OptionChecked)) and
      (not (soScrollOff in OptionChecked)) then
      OptionChecked := OptionChecked + [soScrollSlow];
      // old regver with no scrolling
  finally
    Free;
  end;

  if soSoundOff in OptionChecked  then
    SoundMode := smOff
  else if soSoundOnAlt in OptionChecked then
    SoundMode := smOnAlt
  else
    SoundMode := smOn;
end;

procedure TMainScreen.mRepClicked(Sender: TObject);
begin
  with TMenuItem(Sender) do
  begin
    Checked := not Checked;
    if Checked then
      CityRepMask := CityRepMask or (1 shl (Tag shr 8))
    else
      CityRepMask := CityRepMask and not(1 shl (Tag shr 8));
  end;
end;

procedure TMainScreen.mLogClick(Sender: TObject);
begin
  LogDlg.Show;
end;

procedure TMainScreen.FormShow(Sender: TObject);
begin
  SetFullScreen(FullScreen);
  Timer1.Enabled := True;
end;

procedure TMainScreen.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := False;
end;

procedure TMainScreen.Radio(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
end;

procedure TMainScreen.mManipClick(Sender: TObject);
var
  Flag: Integer;
begin
  with TMenuItem(Sender) do
  begin
    Flag := 1 shl (Tag shr 8);
    if Checked then
      Server(sClearTestFlag, 0, Flag, nil^)
    else
    begin
      Server(sSetTestFlag, 0, Flag, nil^);
      Play('CHEAT');
    end;
    if not Supervising then
    begin
      if Flag = tfUncover then
      begin
        MapValid := False;
        PaintAllMaps;
      end
      else if Flag = tfAllTechs then
        TellNewModels;
    end;
  end;
end;

procedure TMainScreen.MapBtnClick(Sender: TObject);
begin
  with TButtonC(Sender) do
  begin
    MapOptionChecked := TMapOptions(Integer(MapOptionChecked) xor (1 shl (Tag shr 8)));
    SetMapOptions;
    ButtonIndex := Integer(MapOptionChecked) shr (Tag shr 8) and 1 + 2;
  end;
  if Sender = MapBtn0 then
  begin
    MiniMapPaint;
    PanelPaint;
  end // update mini map only
  else
  begin
    MapValid := False;
    PaintAllMaps;
  end; // update main map
end;

procedure TMainScreen.GrWallBtnDownChanged(Sender: TObject);
begin
  if TButtonBase(Sender).Down then
  begin
    MapOptionChecked := MapOptionChecked + [moGreatWall];
    TButtonBase(Sender).Hint := '';
  end
  else
  begin
    MapOptionChecked := MapOptionChecked - [moGreatWall];
    TButtonBase(Sender).Hint := Phrases.Lookup('CONTROLS',
      -1 + TButtonBase(Sender).Tag and $FF);
  end;
  SetMapOptions;
  MapValid := False;
  PaintAllMaps;
end;

procedure TMainScreen.BareBtnDownChanged(Sender: TObject);
begin
  if TButtonBase(Sender).Down then
  begin
    MapOptionChecked := MapOptionChecked + [moBareTerrain];
    TButtonBase(Sender).Hint := '';
  end
  else
  begin
    MapOptionChecked := MapOptionChecked - [moBareTerrain];
    TButtonBase(Sender).Hint := Phrases.Lookup('CONTROLS',
      -1 + TButtonBase(Sender).Tag and $FF);
  end;
  SetMapOptions;
  MapValid := False;
  PaintAllMaps;
end;

procedure TMainScreen.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Idle and (Key = VK_APPS) then
  begin
    InitPopup(GamePopup);
    if FullScreen then
      GamePopup.Popup(Left, Top + TopBarHeight - 1)
    else
      GamePopup.Popup(Left + 4, Top + GetSystemMetrics(SM_CYCAPTION) + 4 +
        TopBarHeight - 1);
    Exit;
  end; // windows menu button calls game menu
end;

procedure TMainScreen.CreateUnitClick(Sender: TObject);
var
  p1, mix: Integer;
begin
  p1 := TComponent(Sender).Tag shr 16;
  mix := TComponent(Sender).Tag and $FFFF;
  if Server(sCreateUnit + p1 shl 4, Me, mix, EditLoc) >= rExecuted then
    PaintLoc(EditLoc);
end;

procedure TMainScreen.mSoundOffClick(Sender: TObject);
begin
  SoundMode := smOff;
end;

procedure TMainScreen.mSoundOnClick(Sender: TObject);
begin
  SoundMode := smOn;
end;

procedure TMainScreen.mSoundOnAltClick(Sender: TObject);
begin
  SoundMode := smOnAlt;
end;

{ procedure TMainScreen.AdviceBtnClick;
  var
  OldAdviceLoc: Integer;
  begin
  DestinationMarkON:=False;
  PaintDestination;
  AdvisorDlg.GiveStrategyAdvice;
  OldAdviceLoc:=MainMap.AdviceLoc;
  MainMap.AdviceLoc:=-1;
  PaintLoc(OldAdviceLoc);
  end; }

{ procedure TMainScreen.SetAdviceLoc(Loc: integer; AvoidRect: TRect);
  var
  OldAdviceLoc,X,Y: Integer;
  begin
  if Loc<>MainMap.AdviceLoc then
  begin
  if Loc>=0 then
  begin // center
  Y:=Loc div G.lx;
  X:=(Loc+G.lx - AvoidRect.Right div (2*66)) mod G.lx;
  Centre(Y*G.lx+X);
  PaintAllMaps;
  end;
  OldAdviceLoc:=MainMap.AdviceLoc;
  MainMap.AdviceLoc:=Loc;
  PaintLoc(OldAdviceLoc);
  PaintLoc(MainMap.AdviceLoc);
  end;
  end; }

procedure TMainScreen.UnitInfoBtnClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    UnitStatDlg.ShowNewContent_OwnModel(wmPersistent, MyUn[UnFocus].mix)
end;

procedure TMainScreen.ViewpointClick(Sender: TObject);
begin
  SetViewpoint(TMenuItem(Sender).Tag);
end;

procedure TMainScreen.DebugMapClick(Sender: TObject);
begin
  SetDebugMap(TMenuItem(Sender).Tag);
end;

procedure TMainScreen.mSmallTilesClick(Sender: TObject);
begin
  SetTileSizeCenter(tsSmall);
end;

procedure TMainScreen.mNormalTilesClick(Sender: TObject);
begin
  SetTileSizeCenter(tsMedium);
end;

procedure TMainScreen.mBigTilesClick(Sender: TObject);
begin
  SetTileSizeCenter(tsBig);
end;

procedure TMainScreen.SetTileSizeCenter(TileSize: TTileSize);
begin
  SetTileSize(TileSize, GetCenterLoc, Point(MapWidth div 2, MapHeight div 2));
end;

procedure TMainScreen.SetTileSize(TileSize: TTileSize; Loc: Integer; MapPos: TPoint);
begin
  MainMap.TileSize := TileSize;
  NoMap.TileSize := TileSize;
  FormResize(nil);
  SetMapPos(Loc, MapPos);
  PaintAllMaps;
  ApplyToVisibleForms(faSmartUpdateContent);
end;

procedure TMainScreen.SaveMenuItemsState;
var
  I, J: Integer;
begin
  if soTellAI in OptionChecked then OptionChecked := [soTellAI]
    else OptionChecked := [];
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TMenuItem then
      for J := 0 to Length(SaveOption) - 1 do
        if TMenuItem(Components[I]).Checked and
          (TMenuItem(Components[I]).Tag = SaveOption[J]) then
          OptionChecked := OptionChecked + [TSaveOption(J)];
end;

procedure TMainScreen.SaveSettings;
var
  Reg: TRegistry;
begin
  SaveMenuItemsState;

  Reg := TRegistry.Create;
  with Reg do
  try
    OpenKey(AppRegistryKey, True);
    WriteInteger('TileSize', Integer(MainMap.TileSize));
    WriteInteger('OptionChecked', Integer(OptionChecked));
    WriteInteger('MapOptionChecked', Integer(MapOptionChecked));
    WriteInteger('CityReport', Integer(CityRepMask));
  finally
    Free;
  end;
end;

procedure TMainScreen.MovieSpeedBtnClick(Sender: TObject);
begin
  MovieSpeed := TButtonB(Sender).Tag shr 8;
  CheckMovieSpeedBtnState;
end;

procedure TMainScreen.ScrollBarUpdate(Sender: TObject);
begin
  PanelPaint;
  Update;
end;

end.

