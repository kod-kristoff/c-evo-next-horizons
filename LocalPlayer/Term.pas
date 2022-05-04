﻿{$INCLUDE Switches.inc}
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
      Shift: TShiftState; x, y: integer);
    procedure EOTClick(Sender: TObject);
    procedure PanelBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure mDisbandOrUtilizeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PanelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure Toggle(Sender: TObject);
    procedure PanelBoxMouseMove(Sender: TObject; Shift: TShiftState;
      x, y: integer);
    procedure PanelBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure MapBoxMouseMove(Sender: TObject; Shift: TShiftState;
      x, y: integer);
    procedure mShowClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; x, y: integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure FormPaint(Sender: TObject);
    procedure mRepClicked(Sender: TObject);
    procedure mLogClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Radio(Sender: TObject);
    procedure mManipClick(Sender: TObject);
    procedure mNamesClick(Sender: TObject);
    procedure MapBtnClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
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
    function ChooseUnusedTribe: integer;
    function DoJob(j0: Integer): Integer;
    procedure GetTribeList;
    procedure InitModule;
    procedure DoneModule;
    procedure InitTurn(NewPlayer: integer);
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
    procedure Scroll(dx, dy: integer);
    procedure SetMapPos(Loc: integer; MapPos: TPoint);
    procedure Centre(Loc: integer);
    procedure SetTroopLoc(Loc: integer);
    procedure ProcessRect(x0, y0, nx, ny, Options: integer);
    procedure PaintLoc(Loc: integer; Radius: integer = 0);
    procedure PaintLoc_BeforeMove(FromLoc: integer);
    procedure PaintLocTemp(Loc: integer; Style: TPaintLocTempStyle = pltsNormal);
    procedure PaintBufferToScreen(xMap, yMap, width, height: integer);
    procedure PaintDestination;
    procedure SetUnFocus(uix: integer);
    function MoveUnit(dx, dy: integer; Options: integer = 0): integer;
    procedure MoveToLoc(Loc: integer; CheckSuicide: boolean);
    procedure MoveOnScreen(ShowMove: TShowMove; Step0, Step1, nStep: integer;
      Restore: boolean = true);
    procedure FocusOnLoc(Loc: integer; Options: integer = 0);
    function EndTurn(WasSkipped: boolean = false): boolean;
    procedure EndNego;
    function IsPanelPixel(x, y: integer): boolean;
    procedure InitPopup(Popup: TPopupMenu);
    procedure SetMapOptions;
    procedure CheckMovieSpeedBtnState;
    procedure CheckTerrainBtnVisible;
    procedure RememberPeaceViolation;
    procedure SetDebugMap(p: integer);
    procedure SetViewpoint(p: integer);
    function LocationOfScreenPixel(x, y: integer): Integer;
    function GetCenterLoc: Integer;
    procedure SetTileSizeCenter(TileSize: TTileSize);
    procedure SetTileSize(TileSize: TTileSize; Loc: Integer; MapPos: TPoint);
    procedure RectInvalidate(Left, Top, Rigth, Bottom: integer);
    procedure ShowEnemyShipChange(ShowShipChange: TShowShipChange);
    procedure SmartRectInvalidate(Left, Top, Rigth, Bottom: integer);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure OnScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure OnEOT(var Msg: TMessage); message WM_EOT;
    procedure SoundPreload(Check: TSoundBlocks);
    procedure UpdateKeyShortcuts;
    procedure SetFullScreen(Active: Boolean);
    procedure PaintZoomedTile(dst: TBitmap; x, y, Loc: integer);
  public
    UsedOffscreenWidth: Integer;
    UsedOffscreenHeight: Integer;
    Offscreen: TBitmap;
    OffscreenUser: TForm;
    procedure Client(Command, NewPlayer: integer; var Data);
    procedure SetAIName(p: integer; Name: string);
    function ZoomToCity(Loc: integer; NextUnitOnClose: boolean = false;
      ShowEvent: integer = 0): boolean;
    procedure CityClosed(Activateuix: integer; StepFocus: boolean = false;
      SelectFocus: boolean = false);
    function DipCall(Command: integer): integer;
    function OfferCall(var Offer: TOffer): integer;
    procedure UpdateViews(UpdateCityScreen: boolean = false);
    function ContactRefused(p: integer; Item: String): boolean;
  end;

var
  MainScreen: TMainScreen;

type
  TTribeInfo = record
    trix: integer;
    FileName: ShortString;
  end;

  TCityNameInfo = record
    ID: integer;
    NewName: ShortString;
  end;

  TModelNameInfo = record
    mix: integer;
    NewName: ShortString;
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
    integer($FFFF0000);

  { model status flags }
  msObsolete = $1;
  msAllowConscripts = $2;

  { additional city happened flags }
  chTypeDel = $8000;
  chAllImpsMade = $4000;

  adNone = $801;
  adFar = $802;
  adNexus = $803;

  SpecialModelPictureCode: array [0 .. nSpecialModel - 1] of integer = (10,
    11, 40, 41, 21, 30, { 50,51, } 64, 74, { 71, } 73);

  pixSlaves = 0;
  pixNoSlaves = 1; // index of slaves in StdUnits

  // icons.bmp properties
  xSizeSmall = 36;
  ySizeSmall = 20;
  SystemIconLines = 2;
  // lines of system icons in icons.bmp before improvements

  nCityEventPriority = 16;
  CityEventPriority: array [0 .. nCityEventPriority - 1] of integer =
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

function CityEventName(i: integer): string;
function RoughCredibility(Credibility: integer): integer;

function InitEnemyModel(emix: integer): boolean;
procedure InitAllEnemyModels;
procedure InitMyModel(mix: integer; final: boolean);

procedure ImpImage(ca: TCanvas; x, y, iix: integer; Government: integer = -1;
  IsControl: boolean = false);
procedure HelpOnTerrain(Loc: Integer; NewMode: TWindowMode);


implementation

uses
  Directories, CityScreen, Draft, MessgEx, Select, CityType, Help,
  UnitStat, Log, Diagram, NatStat, Wonders, Enhance, Nego, UPixelPointer, Sound,
  Battle, Rates, TechTree, Registry, Global, UKeyBindings;

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
  FastScrolling = false; // causes problems with overlapping windows

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

function CityEventName(i: integer): string;
begin
  if i = 14 then // chAllImpsMade
    if not Phrases2FallenBackToEnglish then
      result := Phrases2.Lookup('CITYEVENT_ALLIMPSMADE')
    else
      result := Phrases.Lookup('CITYEVENTS', 1)
  else
    result := Phrases.Lookup('CITYEVENTS', i);
end;

procedure InitSmallImp;
const
  Cut = 4;
  Sharpen = 80;
type
  TBuffer = array [0 .. 99999, 0 .. 2] of Integer;
var
  Sum, Cnt, dx, dy, nx, ny, ix, iy, ir, x, y, c, ch: Integer;
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
      for y := 0 to ScaleToNative(ySizeBig - 2 * Cut) - 1 do begin
        ydivider := (ScaleFromNative(y) * ySizeSmall div (ySizeBig - 2 * Cut) + 1) *
          (ySizeBig - 2 * Cut) - ScaleFromNative(y) * ySizeSmall;
        if ydivider > ySizeSmall then
          ydivider := ySizeSmall;
        for x := 0 to ScaleToNative(xSizeBig) - 1 do begin
          ir := ix * xSizeSmall + iy * nx * ySizeSmall + ScaleFromNative(x) *
            xSizeSmall div xSizeBig + ScaleFromNative(y) *
            ySizeSmall div (ySizeBig - 2 * Cut) * nx;
          xdivider := (ScaleFromNative(x) * xSizeSmall div xSizeBig + 1) *
            xSizeBig - ScaleFromNative(x) * xSizeSmall;
          if xdivider > xSizeSmall then
            xdivider := xSizeSmall;
          for ch := 0 to 2 do begin
            c := PixelPtr.Pixel^.Planes[ch];
            Inc(Resampled[ir, ch], c * xdivider * ydivider);
            if xdivider < xSizeSmall then
              Inc(Resampled[ir + 1, ch], c * (xSizeSmall - xdivider) *
                ydivider);
            if ydivider < ySizeSmall then
              Inc(Resampled[ir + nx, ch],
                c * xdivider * (ySizeSmall - ydivider));
            if (xdivider < xSizeSmall) and (ydivider < ySizeSmall) then
              Inc(Resampled[ir + nx + 1, ch], c * (xSizeSmall - xdivider) *
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
  for y := 0 to ScaleToNative(ny) - 1 do begin
    for x := 0 to ScaleToNative(nx) - 1 do begin
      for ch := 0 to 2 do begin
        Sum := 0;
        Cnt := 0;
        for dy := -1 to 1 do
          if ((dy >= 0) or (ScaleFromNative(y) mod ySizeSmall > 0)) and
            ((dy <= 0) or (ScaleFromNative(y) mod ySizeSmall < ySizeSmall - 1)) then
            for dx := -1 to 1 do
              if ((dx >= 0) or (ScaleFromNative(x) mod xSizeSmall > 0)) and
                ((dx <= 0) or (ScaleFromNative(x) mod xSizeSmall < xSizeSmall - 1)) then
              begin
                Inc(Sum, Resampled[ScaleFromNative(x) + dx + nx * (ScaleFromNative(y) + dy), ch]);
                Inc(Cnt);
              end;
        Sum := ((Cnt * Sharpen + 800) * Resampled[ScaleFromNative(x) + nx * ScaleFromNative(y), ch] - Sum *
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

procedure ImpImage(ca: TCanvas; x, y, iix: integer; Government: integer;
  IsControl: boolean);
begin
  if Government < 0 then
    Government := MyRO.Government;
  if (iix = imPalace) and (Government <> gAnarchy) then
    iix := Government - 8;
  FrameImage(ca, BigImp, x, y, xSizeBig, ySizeBig, (iix + SystemIconLines * 7)
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

{ *** tribe management procedures *** }

function RoughCredibility(Credibility: integer): integer;
begin
  case Credibility of
    0 .. 69:
      result := 0;
    70 .. 89:
      result := 1;
    90 .. 99:
      result := 2;
    100:
      result := 3;
  end;
end;

procedure ChooseModelPicture(p, mix, code, Hash, Turn: integer;
  ForceNew, final: boolean);
var
  i: integer;
  Picture: TModelPictureInfo;
  IsNew: boolean;
begin
  Picture.trix := p;
  Picture.mix := mix;
  if code = 74 then
  begin // use correct pictures for slaves
    if Tribe[p].mixSlaves < 0 then
      if not TribeOriginal[p] then
        Tribe[p].mixSlaves := mix
      else
      begin
        i := mix + p shl 16;
        Server(cSetSlaveIndex, 0, 0, i);
      end;
    if ToldSlavery = 1 then
      Picture.pix := pixSlaves
    else
      Picture.pix := pixNoSlaves;
    Picture.Hash := 0;
    Picture.GrName := 'StdUnits.png';
    IsNew := true;
  end
  else
  begin
    Picture.Hash := Hash;
    IsNew := Tribe[p].ChooseModelPicture(Picture, code, Turn, ForceNew);
  end;
  if final then
    if not TribeOriginal[p] then
      Tribe[p].SetModelPicture(Picture, IsNew)
    else if IsNew then
      Server(cSetNewModelPicture, 0, 0, Picture)
    else
      Server(cSetModelPicture, 0, 0, Picture)
  else
    with Tribe[p].ModelPicture[mix] do
    begin
      HGr := LoadGraphicSet(Picture.GrName);
      pix := Picture.pix;
    end;
end;

function InitEnemyModel(emix: integer): boolean;
begin
  if GameMode = cMovie then
  begin
    result := false;
    exit;
  end;
  with MyRO.EnemyModel[emix] do
    ChooseModelPicture(Owner, mix, ModelCode(MyRO.EnemyModel[emix]),
      ModelHash(MyRO.EnemyModel[emix]), MyRO.Turn, false, true);
  result := true;
end;

procedure InitAllEnemyModels;
var
  emix: integer;
begin
  for emix := 0 to MyRO.nEnemyModel - 1 do
    with MyRO.EnemyModel[emix] do
      if not Assigned(Tribe[Owner].ModelPicture[mix].HGr) then
        InitEnemyModel(emix);
end;

procedure InitMyModel(mix: integer; final: boolean);
var
  mi: TModelInfo;
begin
  if (GameMode = cMovie) and (MyModel[mix].Kind < $08) then
    exit;
  // don't exit for special units because cSetModelPicture comes after TellNewModels
  MakeModelInfo(me, mix, MyModel[mix], mi);
  ChooseModelPicture(me, mix, ModelCode(mi), ModelHash(mi), MyRO.Turn,
    false, final);
end;

function AttackSound(code: integer): string;
begin
  result := 'ATTACK_' + char(48 + code div 100 mod 10) +
    char(48 + code div 10 mod 10) + char(48 + code mod 10);
end;

procedure CheckToldNoReturn(uix: integer);
// check whether aircraft survived low-fuel warning
begin
  assert(not supervising);
  with MyUn[uix] do
    if (Status and usToldNoReturn <> 0) and
      ((MyMap[Loc] and fCity <> 0) or (MyMap[Loc] and fTerImp = tiBase) or
      (Master >= 0)) then
      Status := Status and not usToldNoReturn;
end;

function CreateTribe(p: integer; FileName: string; Original: boolean): boolean;
begin
  FileName := LocalizedFilePath('Tribes' + DirectorySeparator + FileName +
    CevoTribeExt);
  if not FileExists(FileName) then
  begin
    Result := False;
    Exit;
  end;

  TribeOriginal[p] := Original;
  Tribe[p] := TTribe.Create(FileName);
  with Tribe[p] do
  begin
    if (GameMode = cNewGame) or not Original then
    begin
      Term.ChooseModelPicture(p, 0, 010, 1, 0, true, true);
      Term.ChooseModelPicture(p, 1, 040, 1, 0, true, true);
      Term.ChooseModelPicture(p, 2, 041, 1, 0, true, true);
      Term.ChooseModelPicture(p, -1, 017, 1, 0, true, true);
    end;
    DipMem[p].pContact := -1;
  end;
  result := true;
end;

procedure TellNewContacts;
var
  p1: integer;
begin
  if not supervising then
    for p1 := 0 to nPl - 1 do
      if (p1 <> me) and (1 shl p1 and MyData.ToldContact = 0) and
        (1 shl p1 and MyRO.Alive <> 0) and (MyRO.Treaty[p1] > trNoContact) then
      begin
        TribeMessage(p1, Tribe[p1].TPhrase('FRNEWNATION'), '');
        MyData.ToldContact := MyData.ToldContact or (1 shl p1);
      end;
end;

procedure TellNewModels;
var
  mix: integer;
  ModelNameInfo: TModelNameInfo;
begin
  if supervising then
    exit;
  with Tribe[me] do
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
        InitMyModel(MyData.ToldModels, true);
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
          Server(cSetModelName, me, 0, ModelNameInfo);
        end;
      end;
      if MyModel[MyData.ToldModels].Kind = mkSettler then
      begin // engineers make settlers obsolete
        for mix := 0 to MyData.ToldModels - 1 do
          if MyModel[mix].Kind = mkSettler then
            MyModel[mix].Status := MyModel[mix].Status or msObsolete;
      end;
      inc(MyData.ToldModels);
    end;
end;

procedure TMainScreen.PaintZoomedTile(dst: TBitmap; x, y, Loc: integer);

  procedure TSprite(xDst, yDst, xSrc, ySrc: integer);
  begin
    with NoMapPanel do
      Sprite(dst, HGrTerrain, x + xDst, y + yDst, xxt * 2, yyt * 3,
        1 + xSrc * (xxt * 2 + 1), 1 + ySrc * (yyt * 3 + 1));
  end;

  procedure TSprite4(xSrc, ySrc: integer);
  begin
    with NoMapPanel do begin
      Sprite(dst, HGrTerrain, x + xxt, y + yyt + 2, xxt * 2, yyt * 2 - 2,
        1 + xSrc * (xxt * 2 + 1), 3 + yyt + ySrc * (yyt * 3 + 1));
      Sprite(dst, HGrTerrain, x + 4, y + 2 * yyt, xxt * 2 - 4, yyt * 2,
        5 + xSrc * (xxt * 2 + 1), 1 + yyt + ySrc * (yyt * 3 + 1));
      Sprite(dst, HGrTerrain, x + xxt * 2, y + 2 * yyt, xxt * 2 - 4, yyt * 2,
        1 + xSrc * (xxt * 2 + 1), 1 + yyt + ySrc * (yyt * 3 + 1));
      Sprite(dst, HGrTerrain, x + xxt, y + yyt * 3, xxt * 2, yyt * 2 - 2,
        1 + xSrc * (xxt * 2 + 1), 1 + yyt + ySrc * (yyt * 3 + 1));
    end;
  end;

var
  cix, ySrc, Tile: integer;
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
          dec(cix);
        assert(cix >= 0);
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

function ChooseResearch: boolean;
var
  ChosenResearch: integer;
begin
  if (MyData.FarTech <> adNone) and (MyRO.Tech[MyData.FarTech] >= tsApplicable)
  then
    MyData.FarTech := adNone;
  repeat
    { research complete -- select new }
    repeat
      ModalSelectDlg.ShowNewContent(wmModal, kAdvance);
      if ModalSelectDlg.result < 0 then
      begin
        result := false;
        exit;
      end;
      ChosenResearch := ModalSelectDlg.result;
      if ChosenResearch = adMilitary then
      begin
        DraftDlg.ShowNewContent(wmModal);
        if DraftDlg.ModalResult <> mrOK then
          Tribe[me].ModelPicture[MyRO.nModel].HGr := nil
      end;
    until (ChosenResearch <> adMilitary) or (DraftDlg.ModalResult = mrOK);

    if ChosenResearch = adMilitary then
      InitMyModel(MyRO.nModel, true)
    else if ChosenResearch = adFar then
    begin
      ModalSelectDlg.ShowNewContent(wmModal, kFarAdvance);
      if ModalSelectDlg.result >= 0 then
        if (ModalSelectDlg.result = adNone) or
          (Server(sSetResearch - sExecute, me, ModalSelectDlg.result, nil^) <
          rExecuted) then
          MyData.FarTech := ModalSelectDlg.result
        else
        begin
          ChosenResearch := ModalSelectDlg.result;
          // can be researched immediately
          MyData.FarTech := adNone;
        end;
    end;
  until ChosenResearch <> adFar;
  if ChosenResearch = adNexus then
    MyData.FarTech := adNexus
  else
    Server(sSetResearch, me, ChosenResearch, nil^);
  ListDlg.TechChange;
  result := true;
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

function TMainScreen.DipCall(Command: integer): integer;
var
  i: integer;
  IsTreatyDeal: boolean;
begin
  result := Server(Command, me, 0, nil^);
  if result >= rExecuted then
  begin
    if Command and $FF0F = scContact then
    begin
      DipMem[me].pContact := Command shr 4 and $F;
      NegoDlg.Initiate;
      DipMem[me].DeliveredPrices := [];
      DipMem[me].ReceivedPrices := [];
    end;

    DipMem[me].SentCommand := Command;
    DipMem[me].FormerTreaty := MyRO.Treaty[DipMem[me].pContact];
    if Command = scDipCancelTreaty then
      Play('CANCELTREATY')
    else if Command = scDipAccept then
    begin // remember delivered and received prices
      for i := 0 to ReceivedOffer.nDeliver - 1 do
        include(DipMem[me].ReceivedPrices, ReceivedOffer.Price[i] shr 24);
      for i := 0 to ReceivedOffer.nCost - 1 do
        include(DipMem[me].DeliveredPrices,
          ReceivedOffer.Price[ReceivedOffer.nDeliver + i] shr 24);
      IsTreatyDeal := false;
      for i := 0 to ReceivedOffer.nDeliver + ReceivedOffer.nCost - 1 do
        if ReceivedOffer.Price[i] and opMask = opTreaty then
          IsTreatyDeal := true;
      if IsTreatyDeal then
        Play('NEWTREATY')
      else
        Play('ACCEPTOFFER');
    end;
    CityDlg.CloseAction := None;
    if G.RO[DipMem[me].pContact] <> nil then
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

function TMainScreen.OfferCall(var Offer: TOffer): integer;
begin
  result := Server(scDipOffer, me, 0, Offer);
  if result >= rExecuted then
  begin
    DipMem[me].SentCommand := scDipOffer;
    DipMem[me].FormerTreaty := MyRO.Treaty[DipMem[me].pContact];
    DipMem[me].SentOffer := Offer;
    CityDlg.CloseAction := None;
    if G.RO[DipMem[me].pContact] <> nil then
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

procedure TMainScreen.SetUnFocus(uix: integer);
var
  Loc0: integer;
begin
  assert(not((uix >= 0) and supervising));
  if uix <> UnFocus then
  begin
    DestinationMarkON := false;
    PaintDestination;
    if uix >= 0 then
      UnStartLoc := MyUn[uix].Loc;
    BlinkON := false;
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
  Tile: integer;
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
    TerrainBtn.Visible := false;
end;

procedure TMainScreen.CheckMovieSpeedBtnState;
begin
  if GameMode = cMovie then
  begin
    MovieSpeed1Btn.Down := MovieSpeed = 1;
    MovieSpeed1Btn.Visible := true;
    MovieSpeed2Btn.Down := MovieSpeed = 2;
    MovieSpeed2Btn.Visible := true;
    MovieSpeed3Btn.Down := MovieSpeed = 3;
    MovieSpeed3Btn.Visible := true;
    MovieSpeed4Btn.Down := MovieSpeed = 4;
    MovieSpeed4Btn.Visible := true;
  end
  else
  begin
    MovieSpeed1Btn.Visible := false;
    MovieSpeed2Btn.Visible := false;
    MovieSpeed3Btn.Visible := false;
    MovieSpeed4Btn.Visible := false;
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

procedure TMainScreen.UpdateViews(UpdateCityScreen: boolean);
begin
  SumCities(TaxSum, ScienceSum);
  PanelPaint; // TopBar was enough!!!
  ListDlg.EcoChange;
  NatStatDlg.EcoChange;
  if UpdateCityScreen then
    CityDlg.SmartUpdateContent;
end;

procedure TMainScreen.SetAIName(p: integer; Name: string);
begin
  if Name = '' then
  begin
    if AILogo[p] <> nil then
    begin
      FreeAndNil(AILogo[p]);
    end;
  end
  else
  begin
    if AILogo[p] = nil then
      AILogo[p] := TBitmap.Create;
    if not LoadGraphicFile(AILogo[p], HomeDir + Name + '.png', [gfNoError]) then
    begin
      FreeAndNil(AILogo[p]);
    end;
  end;
end;

function TMainScreen.ContactRefused(p: integer; Item: String): boolean;
// return whether treaty was cancelled
var
  s: string;
begin
  assert(MyRO.Treaty[p] >= trPeace);
  s := Tribe[p].TPhrase(Item);
  if MyRO.Turn < MyRO.LastCancelTreaty[p] + CancelTreatyTurns then
  begin
    SimpleMessage(s);
    result := false;
  end
  else
  begin
    case MyRO.Treaty[p] of
      trPeace:
        s := s + ' ' + Phrases.Lookup('FRCANCELQUERY_PEACE');
      trFriendlyContact:
        s := s + ' ' + Phrases.Lookup('FRCANCELQUERY_FRIENDLY');
      trAlliance:
        s := s + ' ' + Phrases.Lookup('FRCANCELQUERY_ALLIANCE');
    end;
    result := SimpleQuery(mkYesNo, s, 'NEGO_REJECTED') = mrOK;
    if result then
    begin
      Play('CANCELTREATY');
      Server(sCancelTreaty, me, 0, nil^);
      if MyRO.Treaty[p] = trNone then
        CityOptimizer_BeginOfTurn;
      // peace treaty was cancelled -- use formerly forbidden tiles
      MapValid := false;
      PaintAllMaps;
    end;
  end;
end;

procedure TMainScreen.RememberPeaceViolation;
var
  uix, p1: integer;
begin
  MyData.PeaceEvaHappened := 0;
  for uix := 0 to MyRO.nUn - 1 do
    with MyUn[uix] do
      if Loc >= 0 then
      begin
        p1 := MyRO.Territory[Loc];
        if (p1 <> me) and (p1 >= 0) and
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
  i, cix, mix: integer;
  need: boolean;
  mi: TModelInfo;
begin
  if (sbStart in Check) and not (sbStart in SoundPreloadDone) then begin
    for i := 0 to nStartBlock - 1 do
      PreparePlay(StartBlock[i]);
    SoundPreloadDone := SoundPreloadDone + [sbStart];
  end;
  if (sbWonder in Check) and not (sbWonder in SoundPreloadDone) then begin
    need := false;
    for i := 0 to nWonder - 1 do
      if MyRO.Wonder[i].CityID <> WonderNotBuiltYet then
        need := true;
    if need then begin
      for i := 0 to nWonderBlock - 1 do
        PreparePlay(WonderBlock[i]);
      SoundPreloadDone := SoundPreloadDone + [sbWonder];
    end;
  end;
  if ((sbScience in Check) and not (sbScience in SoundPreloadDone)) and
    (MyRO.Tech[adScience] >= tsApplicable) then begin
    for i := 0 to nScienceBlock - 1 do
      PreparePlay(ScienceBlock[i]);
    SoundPreloadDone := SoundPreloadDone + [sbScience];
  end;
  if ((sbContact in Check) and not (sbContact in SoundPreloadDone)) and
    (MyRO.nEnemyModel + MyRO.nEnemyCity > 0) then begin
    for i := 0 to nContactBlock - 1 do
      PreparePlay(ContactBlock[i]);
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
          for i := 0 to 12 do
            if 1 shl i and Flags and CityRepMask <> 0 then
              PreparePlay(CityEventSoundItem[i]);
    for mix := 0 to MyRO.nModel - 1 do
      with MyModel[mix] do
        if Attack > 0 then
        begin
          MakeModelInfo(me, mix, MyModel[mix], mi);
          PreparePlay(AttackSound(ModelCode(mi)));
        end;
  end;
end;

procedure TMainScreen.GetTribeList;
var
  SearchRec: TSearchRec;
  Color: TColor;
  Name: string;
  ok: boolean;
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

function TMainScreen.ChooseUnusedTribe: integer;
var
  i: Integer;
  j: Integer;
  ColorDistance: Integer;
  BestColorDistance: Integer;
  TestColorDistance: Integer;
  CountBest: Integer;
begin
  assert(UnusedTribeFiles.Count > 0);
  result := -1;
  BestColorDistance := -1;
  for j := 0 to UnusedTribeFiles.Count - 1 do
  begin
    ColorDistance := 250; // consider differences more than this infinite
    for i := 0 to nPl - 1 do
      if Tribe[i] <> nil then
      begin
        TestColorDistance := abs(integer(UnusedTribeFiles.Objects[j])
          shr 16 and $FF - Tribe[i].Color shr 16 and $FF) +
          abs(integer(UnusedTribeFiles.Objects[j]) shr 8 and
          $FF - Tribe[i].Color shr 8 and $FF) * 3 +
          abs(integer(UnusedTribeFiles.Objects[j]) and
          $FF - Tribe[i].Color and $FF) * 2;
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
      inc(CountBest);
      if DelphiRandom(CountBest) = 0 then
        result := j;
    end;
  end;
end;

procedure TMainScreen.ShowEnemyShipChange(ShowShipChange: TShowShipChange);
var
  i, TestCost, MostCost: integer;
  Ship1Plus, Ship2Plus: boolean;
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
          Ship1Plus := false;
          Ship2Plus := false;
          for i := 0 to nShipPart - 1 do
          begin
            if Ship1Change[i] > 0 then
              Ship1Plus := true;
            if Ship2Change[i] > 0 then
              Ship2Plus := true;
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
      for i := 0 to nShipPart - 1 do
      begin
        TestCost := abs(Ship1Change[i]) * Imp[imShipComp + i].Cost;
        if TestCost > MostCost then
        begin
          MostCost := TestCost;
          IconIndex := imShipComp + i;
        end;
      end;
    end;

    Kind := mkOk;
    ShowModal;
  end;
end;

procedure TMainScreen.InitModule;
var
  i, j, Domain: integer;
begin
  { search icons for advances: }
  for i := 0 to nAdv - 1 do
    if i in FutureTech then
      AdvIcon[i] := 96 + i - futResearchTechnology
    else
    begin
      AdvIcon[i] := -1;
      for Domain := 0 to nDomains - 1 do
        for j := 0 to nUpgrade - 1 do
          if upgrade[Domain, j].Preq = i then
            if AdvIcon[i] >= 0 then
              AdvIcon[i] := 85
            else
              AdvIcon[i] := 86 + Domain;
      for j := 0 to nFeature - 1 do
        if Feature[j].Preq = i then
          for Domain := 0 to nDomains - 1 do
            if 1 shl Domain and Feature[j].Domains <> 0 then
              if (AdvIcon[i] >= 0) and (AdvIcon[i] <> 86 + Domain) then
                AdvIcon[i] := 85
              else
                AdvIcon[i] := 86 + Domain;
      for j := nWonder to nImp - 1 do
        if Imp[j].Preq = i then
          AdvIcon[i] := j;
      for j := nWonder to nImp - 1 do
        if (Imp[j].Preq = i) and (Imp[j].Kind <> ikCommon) then
          AdvIcon[i] := j;
      for j := 0 to nJob - 1 do
        if i = JobPreq[j] then
          AdvIcon[i] := 84;
      for j := 0 to nWonder - 1 do
        if Imp[j].Preq = i then
          AdvIcon[i] := j;
      if AdvIcon[i] < 0 then
        if AdvValue[i] < 1000 then
          AdvIcon[i] := -7
        else
          AdvIcon[i] := 24 + AdvValue[i] div 1000;
      for j := 2 to nGov - 1 do
        if GovPreq[j] = i then
          AdvIcon[i] := j - 8;
    end;
  AdvIcon[adConscription] := 86 + dGround;

  UnusedTribeFiles := tstringlist.Create;
  UnusedTribeFiles.Sorted := true;
  TribeNames := tstringlist.Create;

  IsoEngine.Init(InitEnemyModel);
  // non-default tile size is missing a file, switch to default
  MainMap.SetOutput(offscreen);

  HGrStdUnits := LoadGraphicSet('StdUnits.png');
  SmallImp := TBitmap.Create;
  SmallImp.PixelFormat := pf24bit;
  InitSmallImp;
  SoundPreloadDone := [];
  StartRunning := false;
  StayOnTop_Ensured := false;

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

procedure TMainScreen.InitTurn(NewPlayer: integer);
const
  nAdvBookIcon = 16;
  AdvBookIcon: array [0 .. nAdvBookIcon - 1] of record Adv,
    Icon: integer end = ((Adv: adPolyTheism; Icon: woZeus),
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
  p1, i, ad, uix, cix, MoveOptions, MoveResult, Loc1,
    NewAgeCenterTo, Winners, NewGovAvailable, dx,
    dy: integer;
  MoveAdviceData: TMoveAdviceData;
  Picture: TModelPictureInfo;
  s, Item, Item2: string;
  UpdatePanel, OwnWonder, ok, Stop, ShowCityList, WondersOnly,
    AllowCityScreen: boolean;
begin
  if IsMultiPlayerGame and (NewPlayer <> me) then
  begin
    UnitInfoBtn.Visible := false;
    UnitBtn.Visible := false;
    TerrainBtn.Visible := false;
    EOT.Visible := false;
  end;
  if IsMultiPlayerGame and (NewPlayer <> me) and
    (G.RO[0].Happened and phShipComplete = 0) then
  begin // inter player screen
    for i := 0 to ControlCount - 1 do
      if Controls[i] is TButtonC then
        Controls[i].Visible := false;
    me := -1;
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

    s := TurnToString(G.RO[0].Turn);
    if supervising then
      SimpleMessage(Format(Phrases.Lookup('SUPERTURN'), [s]))
    else
      SimpleMessage(Format(Tribe[NewPlayer].TPhrase('TURN'), [s]));
  end;
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TButtonC then
      Controls[i].Visible := true;

  ItsMeAgain(NewPlayer);
  MyData := G.RO[NewPlayer].Data;
  if not supervising then
    SoundPreload(sbAll);
  if (me = 0) and ((MyRO.Turn = 0) or (ClientMode = cResume)) then
    Invalidate; // colorize empty space

  if not supervising then
  begin

    { if MyRO.Happened and phGameEnd<>0 then
      begin
      Age := 3;
      MainTexture.Age := -1;
      end
      else }
    begin
      Age := GetAge(me);
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
  HelpDlg.Difficulty := G.Difficulty[me];

  UnFocus := -1;
  MarkCityLoc := -1;
  BlinkON := false;
  BlinkTime := -1;
  Tracking := false;
  TurnComplete := false;

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
          Tribe[p1].SetModelPicture(Picture, true);
        end;
  end;

  if not supervising and (ClientMode = cTurn) then
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
    UnitInfoBtn.Visible := false;
    UnitBtn.Visible := false;
    TerrainBtn.Visible := false;
    EOT.Hint := Phrases.Lookup('BTN_STOP');
    EOT.Visible := true;
  end
  else if ClientMode < scContact then
  begin
    UnitInfoBtn.Visible := UnFocus >= 0;
    UnitBtn.Visible := UnFocus >= 0;
    CheckTerrainBtnVisible;
    TurnComplete := supervising;
    EOT.Hint := Phrases.Lookup('BTN_ENDTURN');
    EOT.Visible := Server(sTurn - sExecute, me, 0, nil^) >= rExecuted;
  end
  else
  begin
    UnitInfoBtn.Visible := false;
    UnitBtn.Visible := false;
    TerrainBtn.Visible := false;
    EOT.Hint := Phrases.Lookup('BTN_NEGO');
    EOT.Visible := true;
  end;
  SetTroopLoc(-1);
  MapValid := false;
  NewAgeCenterTo := 0;
  if ((MyRO.Turn = 0) and not supervising or IsMultiPlayerGame or
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
          MessgText := Tribe[me].TPhrase('GAMEOVER');
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
              for i := 0 to nShipPart - 1 do
                if MyRO.Ship[p1].Parts[i] < ShipNeed[i] then
                  Winners := Winners and not(1 shl p1);
            end;
          assert(Winners <> 0);
          if Winners and (1 shl me) <> 0 then
          begin
            s := '';
            for p1 := 0 to nPl - 1 do
              if (p1 <> me) and (1 shl p1 and Winners <> 0) then
                if s = '' then
                  s := Tribe[p1].TPhrase('SHORTNAME')
                else
                  s := Format(Phrases.Lookup('SHAREDWIN_CONCAT'),
                    [s, Tribe[p1].TPhrase('SHORTNAME')]);

            OpenSound := 'MSG_YOUWIN';
            MessgText := Tribe[me].TPhrase('MYSPACESHIP');
            if s <> '' then
              MessgText := MessgText + '\' +
                Format(Phrases.Lookup('SHAREDWIN'), [s]);
            IconKind := mikBigIcon;
            IconIndex := 9;
          end
          else
          begin
            assert(me = 0);
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
          assert(me = 0);
          OpenSound := 'MSG_GAMEOVER';
          if not supervising then
            MessgText := Tribe[me].TPhrase('TIMEUP')
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
            inc(p1);
          if MyRO.Happened and phShipComplete = 0 then
            DiaDlg.ShowNewContent_Charts(wmModal);
        end;
        TurnComplete := true;
        exit;
      end;
    if not supervising and (1 shl me and MyRO.Alive = 0) then
    begin
      TurnComplete := true;
      exit;
    end;

    if (ClientMode = cContinue) and
      (DipMem[me].SentCommand and $FF0F = scContact) then
      // contact was refused
      if MyRO.Treaty[DipMem[me].pContact] >= trPeace then
        ContactRefused(DipMem[me].pContact, 'FRREJECTED')
      else
        SoundMessage(Tribe[DipMem[me].pContact].TPhrase('FRREJECTED'),
          'NEGO_REJECTED');

    if not supervising and (Age > MyData.ToldAge) and
      ((Age > 0) or (ClientMode <> cMovieTurn)) then
      with MessgExDlg do
      begin
        if Age = 0 then
        begin
          if Phrases2FallenBackToEnglish then
          begin
            s := Tribe[me].TPhrase('AGE0');
            MessgText :=
              Format(s, [TurnToString(MyRO.Turn), CityName(MyCity[0].ID)])
          end
          else
          begin
            s := Tribe[me].TString(Phrases2.Lookup('AGE0'));
            MessgText := Format(s, [TurnToString(MyRO.Turn)]);
          end
        end
        else
        begin
          s := Tribe[me].TPhrase('AGE' + char(48 + Age));
          MessgText := Format(s, [TurnToString(MyRO.Turn)]);
        end;
        IconKind := mikAge;
        IconIndex := Age;
        { if age=0 then } Kind := mkOk
        { else begin Kind:=mkOkHelp; HelpKind:=hkAdv; HelpNo:=AgePreq[age]; end };
        CenterTo := NewAgeCenterTo;
        OpenSound := 'AGE_' + char(48 + Age);
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
            s := Tribe[p1].TPhrase('EXTINCT');
            MessgText := Format(s, [TurnToString(MyRO.Turn)]);
            if MyRO.Alive = 1 shl me then
              MessgText := MessgText + Phrases.Lookup('EXTINCTALL');
            Kind := mkOk;
            IconKind := mikImp;
            IconIndex := 21;
            ShowModal;
          end;
      if (ClientMode <> cMovieTurn) and not supervising then
        DiaDlg.ShowNewContent_Charts(wmModal);
    end;

    // tell changes of own credibility
    if not supervising then
    begin
      if RoughCredibility(MyRO.Credibility) <>
        RoughCredibility(MyData.ToldOwnCredibility) then
      begin
        if RoughCredibility(MyRO.Credibility) >
          RoughCredibility(MyData.ToldOwnCredibility) then
          s := Phrases.Lookup('CREDUP')
        else
          s := Phrases.Lookup('CREDDOWN');
        TribeMessage(me, Format(s, [Phrases.Lookup('CREDIBILITY',
          RoughCredibility(MyRO.Credibility))]), '');
      end;
      MyData.ToldOwnCredibility := MyRO.Credibility;
    end;

    for i := 0 to nWonder - 1 do
    begin
      OwnWonder := false;
      for cix := 0 to MyRO.nCity - 1 do
        if (MyCity[cix].Loc >= 0) and (MyCity[cix].ID = MyRO.Wonder[i].CityID)
        then
          OwnWonder := true;
      if MyRO.Wonder[i].CityID <> MyData.ToldWonders[i].CityID then
      begin
        if MyRO.Wonder[i].CityID = WonderDestroyed then
          with MessgExDlg do
          begin { tell about destroyed wonders }
            OpenSound := 'WONDER_DESTROYED';
            MessgText := Format(Phrases.Lookup('WONDERDEST'),
              [Phrases.Lookup('IMPROVEMENTS', i)]);
            Kind := mkOkHelp;
            HelpKind := hkImp;
            HelpNo := i;
            IconKind := mikImp;
            IconIndex := i;
            ShowModal;
          end
        else
        begin
          if i = woManhattan then
            if MyRO.Wonder[i].EffectiveOwner > me then
              MyData.ColdWarStart := MyRO.Turn - 1
            else
              MyData.ColdWarStart := MyRO.Turn;
          if not OwnWonder then
            with MessgExDlg do
            begin { tell about newly built wonders }
              if i = woManhattan then
              begin
                OpenSound := 'MSG_COLDWAR';
                s := Tribe[MyRO.Wonder[i].EffectiveOwner].TPhrase('COLDWAR');
              end
              else if MyRO.Wonder[i].EffectiveOwner >= 0 then
              begin
                OpenSound := 'WONDER_BUILT';
                s := Tribe[MyRO.Wonder[i].EffectiveOwner]
                  .TPhrase('WONDERBUILT');
              end
              else
              begin
                OpenSound := 'MSG_DEFAULT';
                s := Phrases.Lookup('WONDERBUILTEXP');
                // already expired when built
              end;
              MessgText := Format(s, [Phrases.Lookup('IMPROVEMENTS', i),
                CityName(MyRO.Wonder[i].CityID)]);
              Kind := mkOkHelp;
              HelpKind := hkImp;
              HelpNo := i;
              IconKind := mikImp;
              IconIndex := i;
              ShowModal;
            end;
        end;
      end
      else if (MyRO.Wonder[i].EffectiveOwner <> MyData.ToldWonders[i]
        .EffectiveOwner) and (MyRO.Wonder[i].CityID > WonderDestroyed) then
        if MyRO.Wonder[i].EffectiveOwner < 0 then
        begin
          if i <> woMIR then
            with MessgExDlg do
            begin { tell about expired wonders }
              OpenSound := 'WONDER_EXPIRED';
              MessgText := Format(Phrases.Lookup('WONDEREXP'),
                [Phrases.Lookup('IMPROVEMENTS', i),
                CityName(MyRO.Wonder[i].CityID)]);
              Kind := mkOkHelp;
              HelpKind := hkImp;
              HelpNo := i;
              IconKind := mikImp;
              IconIndex := i;
              ShowModal;
            end;
        end
        else if (MyData.ToldWonders[i].EffectiveOwner >= 0) and not OwnWonder
        then
          with MessgExDlg do
          begin { tell about capture of wonders }
            OpenSound := 'WONDER_CAPTURED';
            s := Tribe[MyRO.Wonder[i].EffectiveOwner].TPhrase('WONDERCAPT');
            MessgText := Format(s, [Phrases.Lookup('IMPROVEMENTS', i),
              CityName(MyRO.Wonder[i].CityID)]);
            Kind := mkOkHelp;
            HelpKind := hkImp;
            HelpNo := i;
            IconKind := mikImp;
            IconIndex := i;
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
  move(MyRO.Wonder, MyData.ToldWonders, SizeOf(MyData.ToldWonders));

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
          for i := 0 to nAdvBookIcon - 1 do
            if AdvBookIcon[i].Adv = ad then
              IconIndex := AdvBookIcon[i].Icon;
          ShowModal;
          MyData.ToldTech[ad] := MyRO.Tech[ad];
          for i := gMonarchy to nGov - 1 do
            if GovPreq[i] = ad then
              NewGovAvailable := i;
        end;
  end;

  ShowCityList := false;
  if ClientMode = cTurn then
  begin
    if (MyRO.Happened and phTech <> 0) and (MyData.FarTech <> adNexus) then
      ChooseResearch;

    UpdatePanel := false;
    if MyRO.Happened and phChangeGov <> 0 then
    begin
      ModalSelectDlg.ShowNewContent(wmModal, kGov);
      Play('NEWGOV');
      Server(sSetGovernment, me, ModalSelectDlg.result, nil^);
      CityOptimizer_BeginOfTurn;
      UpdatePanel := true;
    end;
  end; // ClientMode=cTurn

  if not supervising and ((ClientMode = cTurn) or (ClientMode = cMovieTurn))
  then
    for cix := 0 to MyRO.nCity - 1 do
      with MyCity[cix] do
        Status := Status and not csToldBombard;

  if ((ClientMode = cTurn) or (ClientMode = cMovieTurn)) and
    (MyRO.Government <> gAnarchy) then
  begin
    // tell what happened in cities
    for WondersOnly := true downto false do
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
                s := Tribe[me].TPhrase('WONDERBUILTOWN');
                MessgText :=
                  Format(s, [Phrases.Lookup('IMPROVEMENTS',
                  Project0 and cpIndex), CityName(ID)]);
                Kind := mkOkHelp;
                HelpKind := hkImp;
                HelpNo := Project0 and cpIndex;
                IconKind := mikImp;
                IconIndex := Project0 and cpIndex;
                ShowModal;
              end;
            if not supervising and (ClientMode = cTurn) then
            begin
              AllowCityScreen := true;
              if (Status and 7 <> 0) and
                (Project and (cpImp + cpIndex) = cpImp + imTrGoods) then
                if (MyData.ImpOrder[Status and 7 - 1, 0] >= 0) then
                begin
                  if AutoBuild(cix, MyData.ImpOrder[Status and 7 - 1]) then
                    AllowCityScreen := false
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
                  UpdatePanel := true;
                end;
              end
              else { if mRepList.Checked then }
              begin
                if Flags and CityRepMask <> 0 then
                  ShowCityList := true;
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
          Server(sRevolution, me, 0, nil^);
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

    if not supervising then
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
                ok := true;
                for i := 0 to MoveAdviceData.nStep - 1 do
                begin
                  Loc1 := dLoc(Loc, MoveAdviceData.dx[i],
                    MoveAdviceData.dy[i]);
                  if (MyMap[Loc1] and (fCity or fOwned) = fCity)
                  // don't capture cities during auto move
                    or (MyMap[Loc1] and (fUnit or fOwned) = fUnit) then
                  // don't attack during auto move
                  begin
                    ok := false;
                    Break
                  end
                  else
                  begin
                    if (Loc1 = MoveAdviceData.ToLoc) or
                      (MoveAdviceData.ToLoc = maNextCity) and
                      (MyMap[dLoc(Loc, MoveAdviceData.dx[i],
                      MoveAdviceData.dy[i])] and fCity <> 0) then
                      MoveOptions := muAutoNoWait
                    else
                      MoveOptions := 0;
                    MoveResult := MoveUnit(MoveAdviceData.dx[i],
                      MoveAdviceData.dy[i], MoveOptions);
                    if (MoveResult < rExecuted) or (MoveResult = eEnemySpotted)
                    then
                    begin
                      ok := false;
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
                Stop := true;
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

  HaveStrategyAdvice := false;
  // (GameMode<>cMovie) and not supervising
  // and AdvisorDlg.HaveStrategyAdvice;
  GoOnPhase := true;
  if supervising or (GameMode = cMovie) then
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
      NextUnit(-1, false);
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

procedure TMainScreen.Client(Command, NewPlayer: integer; var Data);
var
  i, j, p1, mix, ToLoc, AnimationSpeed, ShowMoveDomain, cix, ecix: integer;
  Color: TColor;
  Name, s: string;
  TribeInfo: TTribeInfo;
  mi: TModelInfo;
  SkipTurn, IsAlpine, IsTreatyDeal: boolean;
begin
  case Command of
    cTurn, cResume, cContinue, cMovieTurn, scContact, scDipStart .. scDipBreak:
      begin
        supervising := G.Difficulty[NewPlayer] = 0;
        ArrangeMidPanel;
      end
  end;
  case Command of
    cDebugMessage:
      LogDlg.Add(NewPlayer, G.RO[0].Turn, pchar(@Data));

    cShowNego:
      with TShowNegoData(Data) do
      begin
        s := Format('P%d to P%d: ', [pSender, pTarget]);
        if (Action = scDipOffer) and (Offer.nDeliver + Offer.nCost > 0) then
        begin
          s := s + 'Offer ';
          for i := 0 to Offer.nDeliver + Offer.nCost - 1 do
          begin
            if i = Offer.nDeliver then
              s := s + ' for '
            else if i > 0 then
              s := s + '+';
            case Offer.Price[i] and opMask of
              opChoose:
                s := s + 'Price of choice';
              opCivilReport:
                s := s + 'State report';
              opMilReport:
                s := s + 'Military report';
              opMap:
                s := s + 'Map';
              opTreaty:
                s := s + 'Treaty';
              opShipParts:
                s := s + 'Ship part';
              opMoney:
                s := s + IntToStr(Offer.Price[i] and $FFFFFF) + 'o';
              opTribute:
                s := s + IntToStr(Offer.Price[i] and $FFFFFF) + 'o tribute';
              opTech:
                s := s + Phrases.Lookup('ADVANCES', Offer.Price[i] and $FFFFFF);
              opAllTech:
                s := s + 'All advances';
              opModel:
                s := s + Tribe[pSender].ModelName[Offer.Price[i] and $FFFFFF];
              opAllModel:
                s := s + 'All models';
            end;
          end;
          LogDlg.Add(NewPlayer, G.RO[0].Turn, pchar(s));
        end
        else if Action = scDipAccept then
        begin
          s := s + '--- ACCEPTED! ---';
          LogDlg.Add(NewPlayer, G.RO[0].Turn, pchar(s));
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
        GameOK := true;
        G := TNewGameData(Data);
        me := -1;
        pLogo := -1;
        ClientMode := -1;
        SetMapOptions;
        MainMap.pDebugMap := -1;
        idle := false;
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
              for i := 0 to nPl - 1 do
                if G.Difficulty[i] > 0 then
                  inc(ToldAlive, 1 shl i);
              PeaceEvaHappened := 0;
              for i := 0 to nWonder - 1 do
                with ToldWonders[i] do
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
        LogDlg.mSlot.Visible := true;
        LogDlg.Host := self;
        HelpDlg.ClearHistory;
        CityDlg.Reset;

        MiniMap.Size := Point(G.lx, G.ly);
        for i := 0 to nPl - 1 do
        begin
          Tribe[i] := nil;
          TribeOriginal[i] := false;
        end;
        ToldSlavery := -1;
        RepaintOnResize := false;
        Closable := false;
        FirstMovieTurn := true;

        MenuArea.Visible := GameMode <> cMovie;
        TreasuryArea.Visible := GameMode < cMovie;
        ResearchArea.Visible := GameMode < cMovie;
        ManagementArea.Visible := GameMode < cMovie;
      end;

    cGetReady, cReplay:
      if NewPlayer = 0 then
      begin
        i := 0;
        for p1 := 0 to nPl - 1 do
          if (G.Difficulty[p1] > 0) and (Tribe[p1] = nil) then
            inc(i);
        if i > UnusedTribeFiles.Count then
        begin
          GameOK := false;
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
              for j := 0 to UnusedTribeFiles.Count - 1 do
              begin
                GetTribeInfo(UnusedTribeFiles[j], Name, Color);
                TribeNames.AddObject(Name, TObject(Color));
              end;
              assert(TribeNames.Count > 0);
              ModalSelectDlg.ShowNewContent(wmModal, kTribe);
              Application.ProcessMessages;
              TribeInfo.FileName := UnusedTribeFiles[ModalSelectDlg.result];
              UnusedTribeFiles.Delete(ModalSelectDlg.result);

              if GameMode = cLoadGame then
                CreateTribe(TribeInfo.trix, TribeInfo.FileName, false)
              else
                Server(cSetTribe, 0, 0, TribeInfo);
            end;

          for p1 := 0 to nPl - 1 do
            if (G.Difficulty[p1] > 0) and (Tribe[p1] = nil) and (G.RO[p1] = nil)
            then
            begin // autoselect enemy tribes
              j := ChooseUnusedTribe;
              TribeInfo.FileName := UnusedTribeFiles[j];
              UnusedTribeFiles.Delete(j);
              TribeInfo.trix := p1;
              if GameMode = cLoadGame then
                CreateTribe(TribeInfo.trix, TribeInfo.FileName, false)
              else
                Server(cSetTribe, 0, 0, TribeInfo);
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
        StartRunning := not idle and (Jump[0] > 0); // AI called Reload
        me := -1;
        idle := false;
        ClientMode := -1;
        UnitInfoBtn.Visible := false;
        UnitBtn.Visible := false;
        TerrainBtn.Visible := false;
        MovieSpeed1Btn.Visible := false;
        MovieSpeed2Btn.Visible := false;
        MovieSpeed3Btn.Visible := false;
        MovieSpeed4Btn.Visible := false;
        EOT.Visible := false;
        for i := 0 to ControlCount - 1 do
          if Controls[i] is TButtonC then
            Controls[i].Visible := false;
        sb.Init(0, 1);
        for p1 := 0 to nPl - 1 do
          if Tribe[p1] <> nil then
            FreeAndNil(Tribe[p1]);
        Tribes.Done;
        RepaintOnResize := false;
        Closable := true;
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
        RepaintOnResize := true;
        xw := 0;
        yw := ywcenter;
        if not StayOnTop_Ensured then
        begin
          StayOnTop_Ensured := true;
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
        if integer(Data) >= 0 then
        begin
          pLogo := integer(Data);
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
                if not Assigned(Tribe[me].ModelPicture[mix].HGr) then
                  InitMyModel(mix, true);
            end;
          me := -1;
        end;

        if Jump[pTurn] > 0 then
          Application.ProcessMessages;
        if Jump[pTurn] > 0 then
          if G.RO[NewPlayer].Happened and phGameEnd <> 0 then
            Jump[pTurn] := 0
          else
            dec(Jump[pTurn]);
        SkipTurn := Jump[pTurn] > 0;
        if SkipTurn then
        begin
          ItsMeAgain(NewPlayer);
          MyData := G.RO[NewPlayer].Data;
          SetTroopLoc(-1);
          MiniMapPaint;
          InitAllEnemyModels; // necessary for correct replay
          if not EndTurn(true) then
            SkipTurn := false;
        end;
        if not SkipTurn then
        begin
          if ((ClientMode < scDipStart) or (ClientMode > scDipBreak)) and
            NegoDlg.Visible then
            NegoDlg.Close;
          skipped := false; // always show my moves during my turn
          idle := true;
          InitTurn(NewPlayer);
          DipMem[me].pContact := -1;
          (* if (me=0) and (MyRO.Alive and (1 shl me)=0)} then
            begin
            if SimpleQuery(Phrases.Lookup('RESIGN'))=mrIgnore then
            Server(sResign,me,0,nil^)
            else Server(sBreak,me,0,nil^)
            end
            else Play('TURNSTART'); *)
        end;
      end;

    cMovieTurn:
      begin
        ClientMode := Command;
        pTurn := NewPlayer;
        pLogo := -1;
        skipped := false; // always show my moves during my turn
        idle := true;
        if FirstMovieTurn then
        begin
          CheckMovieSpeedBtnState;
          FirstMovieTurn := false;
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
        MapValid := false;
        ClientMode := -1;
        idle := false;
        skipped := false;
      end;

    cEditMap:
      begin
        ClientMode := cEditMap;
        SetMapOptions;
        MainMap.pDebugMap := -1;
        ItsMeAgain(0);
        MyData := nil;
        UnitInfoBtn.Visible := false;
        UnitBtn.Visible := false;
        TerrainBtn.Visible := false;
        MovieSpeed1Btn.Visible := false;
        MovieSpeed2Btn.Visible := false;
        MovieSpeed3Btn.Visible := false;
        MovieSpeed4Btn.Visible := false;
        EOT.Visible := false;
        HelpDlg.Difficulty := 0;
        BrushType := fGrass;
        BrushLoc := -1;
        Edited := false;
        UnFocus := -1;
        MarkCityLoc := -1;
        Tracking := false;
        TurnComplete := false;
        MapValid := false;
        FormResize(nil); // calculate geometrics and paint all
        SetTroopLoc(-1);
        idle := true;
      end;

    (* cNewContact:
      begin
      end;
    *)

    scContact:
      begin
        DipMem[NewPlayer].pContact := integer(Data);
        if Jump[NewPlayer] > 0 then
          DipCall(scReject)
        else
        begin
          ClientMode := Command;
          InitTurn(NewPlayer);
          MyData.ToldContact := MyData.ToldContact or (1 shl integer(Data));
          // don't tell about new nation when already contacted by them
          with MessgExDlg do
          begin
            OpenSound := 'CONTACT_' + char(48 + MyRO.EnemyReport[integer(Data)
              ].Attitude);
            MessgText := Tribe[integer(Data)].TPhrase('FRCONTACT');
            Kind := mkYesNo;
            IconKind := mikTribe;
            IconIndex := integer(Data);
            ShowModal;
            if ModalResult = mrOK then
            begin
              NegoDlg.Respond;
              DipMem[me].DeliveredPrices := [];
              DipMem[me].ReceivedPrices := [];
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
          for i := 0 to DipMem[me].SentOffer.nDeliver - 1 do
            include(DipMem[me].DeliveredPrices,
              DipMem[me].SentOffer.Price[i] shr 24);
          for i := 0 to DipMem[me].SentOffer.nCost - 1 do
            include(DipMem[me].ReceivedPrices,
              DipMem[me].SentOffer.Price[DipMem[me].SentOffer.nDeliver +
              i] shr 24);
          IsTreatyDeal := false;
          for i := 0 to ReceivedOffer.nDeliver + ReceivedOffer.nCost - 1 do
            if DipMem[me].SentOffer.Price[i] and opMask = opTreaty then
              IsTreatyDeal := true;
          if IsTreatyDeal then
            Play('NEWTREATY')
          else
            Play('ACCEPTOFFER');
        end;
        NegoDlg.Start;
        idle := true;
      end;

    cShowCancelTreaty:
      if not IsMultiPlayerGame then
      begin
        case G.RO[NewPlayer].Treaty[integer(Data)] of
          trPeace:
            s := Tribe[integer(Data)].TPhrase('FRCANCELBYREJECT_PEACE');
          trFriendlyContact:
            s := Tribe[integer(Data)].TPhrase('FRCANCELBYREJECT_FRIENDLY');
          trAlliance:
            s := Tribe[integer(Data)].TPhrase('FRCANCELBYREJECT_ALLIANCE');
        end;
        TribeMessage(integer(Data), s, 'CANCELTREATY');
      end;

    cShowCancelTreatyByAlliance:
      if idle and (NewPlayer = me) then
        TribeMessage(integer(Data), Tribe[integer(Data)
          ].TPhrase('FRENEMYALLIANCE'), 'CANCELTREATY');

    cShowSupportAllianceAgainst:
      if not IsMultiPlayerGame and (Jump[0] = 0) then
        TribeMessage(integer(Data) and $F, Tribe[integer(Data) and $F]
          .TPhrase('FRMYALLIANCE1') + ' ' + Tribe[integer(Data) shr 4]
          .TPhrase('FRMYALLIANCE2'), 'CANCELTREATY');

    cShowPeaceViolation:
      if not IsMultiPlayerGame and (Jump[0] = 0) then
        TribeMessage(integer(Data),
          Format(Tribe[integer(Data)].TPhrase('EVIOLATION'),
          [TurnToString(MyRO.Turn + PeaceEvaTurns - 1)]), 'MSG_WITHDRAW');

    cShowEndContact:
      EndNego;

    cShowUnitChanged, cShowCityChanged, cShowAfterMove, cShowAfterAttack:
      if (idle and (NewPlayer = me) or not idle and not skipped) and
        not((GameMode = cMovie) and (MovieSpeed = 4)) then
      begin
        assert(NewPlayer = me);
        if not idle or (GameMode = cMovie) then
          Application.ProcessMessages;
        if Command = cShowCityChanged then
        begin
          CurrentMoveInfo.DoShow := false;
          if idle then
            CurrentMoveInfo.DoShow := true
          else if CurrentMoveInfo.IsAlly then
            CurrentMoveInfo.DoShow := not mAlNoMoves.Checked
          else
            CurrentMoveInfo.DoShow := not mEnNoMoves.Checked;
        end
        else if Command = cShowUnitChanged then
        begin
          CurrentMoveInfo.DoShow := false;
          if idle then
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
            MapValid := false;
          FocusOnLoc(integer(Data), flImmUpdate);
          // OldUnFocus:=UnFocus;
          // UnFocus:=-1;
          if Command = cShowAfterMove then
            PaintLoc(integer(Data), CurrentMoveInfo.AfterMovePaintRadius)
            // show discovered areas
          else
            PaintLoc(integer(Data), 1);
          // UnFocus:=OldUnFocus;
          if (Command = cShowAfterAttack) and
            (CurrentMoveInfo.AfterAttackExpeller >= 0) then
          begin
            SoundMessageEx(Tribe[CurrentMoveInfo.AfterAttackExpeller]
              .TPhrase('EXPEL'), '');
            CurrentMoveInfo.AfterAttackExpeller := -1;
            Update; // remove message box from screen
          end
          else if not idle then
            if Command = cShowCityChanged then
              Sleep(MoveTime * WaitAfterShowMove div 16)
            else if (Command = cShowUnitChanged) and
              (MyMap[integer(Data)] and fUnit <> 0) then
              Sleep(MoveTime * WaitAfterShowMove div 32)
        end // if CurrentMoveInfo.DoShow
        else
          MapValid := false;
      end;

    cShowMoving, cShowCapturing:
      if (idle and (NewPlayer = me) or not idle and not skipped and
        (TShowMove(Data).emix <> $FFFF)) and
        not((GameMode = cMovie) and (MovieSpeed = 4)) then
      begin
        assert(NewPlayer = me);
        if not idle or (GameMode = cMovie) then
          Application.ProcessMessages;
        with TShowMove(Data) do
        begin
          CurrentMoveInfo.DoShow := false;
          if not idle and (not Assigned(Tribe[Owner].ModelPicture[mix].HGr)) then
            InitEnemyModel(emix);

          ToLoc := dLoc(FromLoc, dx, dy);
          if idle then
          begin // own unit -- make discovered land visible
            assert(Owner = me); // no foreign moves during my turn!
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
                if (MyRO.Wonder[woShinkansen].EffectiveOwner = me) and
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
              CurrentMoveInfo.DoShow := true
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
                    dec(cix);
                  s := CityName(MyCity[cix].ID);
                end
                else
                begin // foreign city, search
                  ecix := MyRO.nEnemyCity - 1;
                  while (ecix >= 0) and (MyRO.EnemyCity[ecix].Loc <> ToLoc) do
                    dec(ecix);
                  s := CityName(MyRO.EnemyCity[ecix].ID);
                end;
                TribeMessage(Owner, Format(Tribe[Owner].TPhrase('CAPTURE'),
                  [s]), '');
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
            MapValid := false;
        end;
      end;

    cShowAttacking:
      if (idle and (NewPlayer = me) or not idle and not skipped and
        (TShowMove(Data).emix <> $FFFF)) and
        not((GameMode = cMovie) and (MovieSpeed = 4)) then
      begin
        assert(NewPlayer = me);
        if not idle or (GameMode = cMovie) then
          Application.ProcessMessages;
        with TShowMove(Data) do
        begin
          CurrentMoveInfo.AfterAttackExpeller := -1;
          CurrentMoveInfo.DoShow := false;
          if idle then
            CurrentMoveInfo.DoShow := true // own unit -- always show attacks
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
                dec(cix);
              if MyCity[cix].Status and csToldBombard = 0 then
              begin
                if not supervising then
                  MyCity[cix].Status := MyCity[cix].Status or csToldBombard;
                s := CityName(MyCity[cix].ID);
                SoundMessageEx(Format(Tribe[Owner].TPhrase('BOMBARD'),
                  [s]), '');
                Update; // remove message box from screen
              end;
            end
            else if Flags and umExpelling <> 0 then
              CurrentMoveInfo.AfterAttackExpeller := Owner;

            if Flags and umExpelling <> 0 then
              Play('MOVE_EXPEL')
            else if Owner = me then
            begin
              MakeModelInfo(me, mix, MyModel[mix], mi);
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
            else if not idle then
              Sleep(MoveTime div 2);
            MainMap.AttackEnd;
          end // if CurrentMoveInfo.DoShow
          else
            MapValid := false;
        end;
      end;

    cShowMissionResult:
      if Cardinal(Data) = 0 then
        SoundMessageEx(Phrases.Lookup('NOFOREIGNINFO'), '')
      else
      begin
        s := Phrases.Lookup('FOREIGNINFO');
        for p1 := 0 to nPl - 1 do
          if 3 shl (p1 * 2) and Cardinal(Data) <> 0 then
            s := s + '\' + Tribe[p1].TPhrase('SHORTNAME');
        SoundMessageEx(s, '');
      end;

    cShowShipChange:
      if not IsMultiPlayerGame and (Jump[0] = 0) then
        ShowEnemyShipChange(TShowShipChange(Data));

    cShowGreatLibTech:
      if not IsMultiPlayerGame and (Jump[0] = 0) then
        with MessgExDlg do
        begin
          MessgText := Format(Phrases.Lookup('GRLIB_GENERAL'),
            [Phrases.Lookup('ADVANCES', integer(Data))]);
          OpenSound := 'NEWADVANCE_GRLIB';
          Kind := mkOk;
          IconKind := mikImp;
          IconIndex := woGrLibrary;
          ShowModal;
        end;

    cRefreshDebugMap:
      begin
        if integer(Data) = MainMap.pDebugMap then
        begin
          MapValid := false;
          MainOffscreenPaint;
          Update;
        end;
      end;

  else
    if Command >= cClientEx then
      case Command  of
        cSetTribe:
          with TTribeInfo(Data) do begin
            i := UnusedTribeFiles.Count - 1;
            while (i >= 0) and
              (AnsiCompareFileName(UnusedTribeFiles[i], FileName) <> 0) do
              dec(i);
            if i >= 0 then
              UnusedTribeFiles.Delete(i);
            CreateTribe(trix, FileName, true);
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
          Tribe[integer(Data) shr 16].mixSlaves := integer(Data) and $FFFF;
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
  i, j: integer;
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
  for i := 0 to ComponentCount - 1 do
    if Components[i].Tag and $FF <> 0 then
      if Components[i] is TMenuItem then begin
        TMenuItem(Components[i]).Caption := Phrases.Lookup('CONTROLS',
          -1 + Components[i].Tag and $FF);
        for j := 0 to Length(SaveOption) - 1 do
          if Components[i].Tag and $FF = SaveOption[j] then
            TMenuItem(Components[i]).Checked := TSaveOption(j) in OptionChecked;
      end else
      if Components[i] is TButtonBase then begin
        TButtonBase(Components[i]).Hint := Phrases.Lookup('CONTROLS',
          -1 + Components[i].Tag and $FF);
        if (Components[i] is TButtonC) and
          (TButtonC(Components[i]).ButtonIndex <> 1) then
          TButtonC(Components[i]).ButtonIndex :=
            Integer(MapOptionChecked) shr (Components[i].Tag shr 8) and 1 + 2
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
  for i := 0 to mRep.Count - 1 do
  begin
    j := mRep[i].Tag shr 8;
    mRep[i].Caption := CityEventName(j);
    mRep[i].Checked := CityRepMask and (1 shl j) <> 0;
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
  for i := 0 to nPl - 1 do
    AILogo[i] := nil;
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
    if AILogo[i] <> nil then
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
      Server(sStartJob + jNone shl 4, me, UnFocus, nil^);
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
        MapValid := false;
        PaintAll;
        ZoomToCity(Loc0, true, chFounded);
      end;
    end else begin
      CityOptimizer_BeforeRemoveUnit(UnFocus);
      ServerResult := Server(sAddToCity, me, UnFocus, nil^);
      if ServerResult >= rExecuted then
      begin
        cix := MyRO.nCity - 1;
        while (cix >= 0) and (MyCity[cix].Loc <> Loc0) do
          dec(cix);
        assert(cix >= 0);
        CityOptimizer_CityChange(cix);
        CityOptimizer_AfterRemoveUnit; // does nothing here
        SetTroopLoc(Loc0);
        UpdateViews(true);
        DestinationMarkON := false;
        PaintDestination;
        UnFocus := -1;
        PaintLoc(Loc0);
        NextUnit(UnStartLoc, true);
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
    MoveToLoc(Destination, true);
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
      if Server(sSetUnitHome, me, UnFocus, nil^) >= rExecuted then
      begin
        CityOptimizer_CityChange(cixOldHome);
        CityOptimizer_CityChange(Home);
        UpdateViews(true);
      end
      else
        Play('INVALID');
    end
    else
    begin
      Status := Status and not(usStay or usRecover or usEnhance);
      MoveToLoc(maNextCity, true);
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
  if supervising then
    Jump[0] := 20
  else
    Jump[me] := 20;
  EndTurn(true);
end;

procedure TMainScreen.mLoadClick(Sender: TObject);
var
  I: Integer;
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    i := Server(sLoadUnit, me, UnFocus, nil^);
    if i >= rExecuted then
    begin
      if MyModel[mix].Domain = dAir then
        Play('MOVE_PLANELANDING')
      else
        Play('MOVE_LOAD');
      DestinationMarkON := false;
      PaintDestination;
      Status := Status and ($FFFF - usWaiting - usStay - usRecover - usGoto - usEnhance);
      NextUnit(UnStartLoc, true);
    end
    else if i = eNoTime_Load then
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
    NextUnit(UnStartLoc, true);
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
    Server(sRandomMap, me, 0, nil^);
    Edited := true;
    MapValid := false;
    PaintAllMaps;
  end;
end;

procedure TMainScreen.mRecoverClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with MyUn[UnFocus] do begin
    DestinationMarkON := false;
    PaintDestination;
    Status := Status and ($FFFF - usStay - usGoto - usEnhance) or usRecover;
    if Job > jNone then
      Server(sStartJob + jNone shl 4, me, UnFocus, nil^);
    NextUnit(UnStartLoc, true);
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
        mrIgnore: Server(sAbandonMap, me, 0, nil^);
        mrOK: Server(sSaveMap, me, 0, nil^);
      end;
    end else
      Server(sAbandonMap, me, 0, nil^);
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
  AltGovs := false;
    for i := 2 to nGov - 1 do
      if (GovPreq[i] <> preNA) and
        ((GovPreq[i] = preNone) or (MyRO.Tech[GovPreq[i]] >= tsApplicable)) then
        AltGovs := true;

    if not AltGovs then
      SoundMessage(Phrases.Lookup('NOALTGOVS'), 'MSG_DEFAULT')
    else
    begin
      RevolutionChanged := false;
      if MyRO.Happened and phChangeGov <> 0 then
      begin
        ModalSelectDlg.ShowNewContent(wmModal, kGov);
        if ModalSelectDlg.result >= 0 then
        begin
          Play('NEWGOV');
          Server(sSetGovernment, me, ModalSelectDlg.result, nil^);
          CityOptimizer_BeginOfTurn;
          RevolutionChanged := true;
        end;
      end
      else
      with MessgExDlg do
      begin // revolution!
        MessgExDlg.MessgText := Tribe[me].TPhrase('REVOLUTION');
        MessgExDlg.Kind := mkYesNo;
        MessgExDlg.IconKind := mikPureIcon;
        MessgExDlg.IconIndex := 72; // anarchy palace
        MessgExDlg.ShowModal;
        if ModalResult = mrOK then
        begin
          Play('REVOLUTION');
          Server(sRevolution, me, 0, nil^);
          RevolutionChanged := true;
          if NatStatDlg.Visible then
            NatStatDlg.Close;
          if CityDlg.Visible then
            CityDlg.Close;
        end
      end;
      if RevolutionChanged then
        UpdateViews(true);
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
  if supervising then
    Jump[0] := 999999
  else
    Jump[me] := 999999;
  EndTurn(true);
end;

procedure TMainScreen.mScienceStatClick(Sender: TObject);
begin
  ListDlg.ShowNewContent(wmPersistent, kScience);
end;

procedure TMainScreen.mSelectTransportClick(Sender: TObject);
begin
  if UnFocus >= 0 then
    with TUn(MyUn[UnFocus]) do
      Server(sSelectTransport, me, UnFocus, nil^);
end;

procedure TMainScreen.mShipsClick(Sender: TObject);
begin
  DiaDlg.ShowNewContent_Ship(wmPersistent);
end;

procedure TMainScreen.mstayClick(Sender: TObject);
begin
  if UnFocus >= 0 then
  with TUn(MyUn[UnFocus]) do begin
    DestinationMarkON := false;
    PaintDestination;
    Status := Status and ($FFFF - usRecover - usGoto - usEnhance) or usStay;
    if Job > jNone then
      Server(sStartJob + jNone shl 4, me, UnFocus, nil^);
    NextUnit(UnStartLoc, true);
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
  if G.Difficulty[me] > 0 then
    ListDlg.ShowNewContent_MilReport(wmPersistent, me)
  else
  begin
    i := 1;
    while (i < nPl) and (1 shl i and MyRO.Alive = 0) do
      inc(i);
    if i < nPl then
      ListDlg.ShowNewContent_MilReport(wmPersistent, i);
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
      i := Server(sUnloadUnit, me, UnFocus, nil^);
      if i >= rExecuted then
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
          NextUnit(Loc, true)
        else
          PanelPaint;
      end
      else if i = eNoTime_Load then
        if MyModel[mix].Domain = dAir then
          SoundMessage(Phrases.Lookup('NOTIMELOADAIR'), 'NOMOVE_TIME')
        else
          SoundMessage(Format(Phrases.Lookup('NOTIMELOADGROUND'),
            [MovementToString(MyModel[mix].speed)]), 'NOMOVE_TIME');
    end else begin
      NewFocus := -1;
      uix := UnFocus;
      for i := 1 to MyRO.nUn - 1 do
      begin
        uix := (uix + MyRO.nUn - 1) mod MyRO.nUn;
        if (MyUn[uix].Master = UnFocus) and
          (MyUn[uix].Movement = integer(MyModel[MyUn[uix].mix].speed)) then
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
    DestinationMarkON := false;
    PaintDestination;
    Status := Status and ($FFFF - usStay - usRecover - usGoto - usEnhance) or usWaiting;
  end;
  NextUnit(-1, false);
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
  MiniFrame, MaxMapWidth: integer;
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
    MapValid := false;
    PaintAll;
  end;
end;

procedure TMainScreen.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := Closable;
  if not Closable and idle and (me = 0) and (ClientMode < scContact) then
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
    Server(sBreak, me, 0, nil^);
  end
  else if ClientMode < 0 then
    skipped := true
  else if ClientMode >= scContact then
    NegoDlg.ShowNewContent(wmPersistent)
  else if Jump[pTurn] > 0 then
  begin
    Jump[pTurn] := 0;
    StartRunning := false;
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
    if supervising then
      xTerrain := xMidPanel + 2 * xxt + 14
    else if ClientWidth < 1280 then
      xTerrain := ClientWidth div 2 + (1280 - ClientWidth) div 3
    else
      xTerrain := ClientWidth div 2;
    xTroop := xTerrain + 2 * xxt + 12;
    if SmallScreen and not supervising then
      xTroop := xRightPanel + 10 - 3 * 66 -
        GetSystemMetrics(SM_CXVSCROLL) - 19 - 4;
    // not perfect but we assume almost no one is still playing on a 800x600 screen
  end;
  TrRow := (xRightPanel + 10 - xTroop - GetSystemMetrics(SM_CXVSCROLL) - 19)
    div TrPitch;
end;

function TMainScreen.EndTurn(WasSkipped: boolean): boolean;

  function IsResourceUnused(cix, NeedFood, NeedProd: integer): boolean;
  var
    dx, dy, fix: integer;
    CityAreaInfo: TCityAreaInfo;
    TileInfo: TTileInfo;
  begin
    Server(sGetCityAreaInfo, me, cix, CityAreaInfo);
    for dy := -3 to 3 do
      for dx := -3 to 3 do
        if ((dx + dy) and 1 = 0) and (dx * dx * dy * dy < 81) then
        begin
          fix := (dy + 3) shl 2 + (dx + 3) shr 1;
          if (MyCity[cix].Tiles and (1 shl fix) = 0) // not used yet
            and (CityAreaInfo.Available[fix] = faAvailable) then // usable
          begin
            TileInfo.ExplCity := cix;
            Server(sGetHypoCityTileInfo, me, dLoc(MyCity[cix].Loc, dx, dy),
              TileInfo);
            if (TileInfo.Food >= NeedFood) and (TileInfo.Prod >= NeedProd) then
            begin
              result := true;
              exit
            end;
          end
        end;
    result := false;
  end;

var
  p1, uix, cix, CenterLoc: integer;
  MsgItem: string;
  CityReport: TCityReport;
  PlaneReturnData: TPlaneReturnData;
  Zoom: boolean;
begin
  result := false;
  if ClientMode >= scDipOffer then
    Exit;

  if supervising and (me <> 0) then begin
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
          if Server(sGetPlaneReturn, me, uix, PlaneReturnData) = eNoWay then
          begin
            CenterLoc := Loc + G.lx * 6;
            // centering the unit itself would make it covered by the query dialog
            while CenterLoc >= G.lx * G.ly do
              dec(CenterLoc, G.lx * 2);
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
              exit;
            end;
            MyUn[uix].Status := MyUn[uix].Status or usToldNoReturn;
          end;
        end;

    if not supervising and (MyRO.TestFlags and tfImmImprove = 0) and
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
          exit;
      end;

    if MyRO.Government <> gAnarchy then
      for cix := 0 to MyRO.nCity - 1 do
        with MyCity[cix] do
          if (Loc >= 0) and (Flags and chCaptured = 0) then
          begin
            Zoom := false;
            CityReport.HypoTiles := -1;
            CityReport.HypoTax := -1;
            CityReport.HypoLux := -1;
            Server(sGetCityReport, me, cix, CityReport);

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
    result := true;
    SetTroopLoc(-1);
    pTurn := -1;
    pLogo := -1;
    UnitInfoBtn.Visible := false;
    UnitBtn.Visible := false;
    TerrainBtn.Visible := false;
    EOT.ButtonIndex := eotCancel;
    EOT.Visible := true;
    MapValid := false;
    PanelPaint;
    Update;
    ClientMode := -1;
    idle := false;
    skipped := WasSkipped;
    for p1 := 1 to nPl - 1 do
      if G.RO[p1] <> nil then
        skipped := true; // don't show enemy moves in hotseat mode
  end
  else
    PanelPaint;
end;

procedure TMainScreen.EndNego;
begin
  if NegoDlg.Visible then
    NegoDlg.Close;
  HaveStrategyAdvice := false;
  // AdvisorDlg.HaveStrategyAdvice;
  // negotiation might have changed advices
  EOT.ButtonIndex := eotCancel;
  EOT.Visible := true;
  PanelPaint;
  Update;
  ClientMode := -1;
  idle := false;
end;

procedure TMainScreen.ProcessRect(x0, y0, nx, ny, Options: integer);
var
  xs, ys, xl, yl: integer;
begin
  with MainMap do begin
    xl := nx * xxt + xxt;
    yl := ny * yyt + yyt * 2;
    xs := (x0 - xw) * (xxt * 2) + y0 and 1 * xxt - G.lx * (xxt * 2);
    // |xs+xl/2-MapWidth/2| -> min
    while abs(2 * (xs + G.lx * (xxt * 2)) + xl - MapWidth) <
      abs(2 * xs + xl - MapWidth) do
        inc(xs, G.lx * (xxt * 2));
    ys := (y0 - yw) * yyt - yyt;
    if xs + xl > MapWidth then
      xl := MapWidth - xs;
    if ys + yl > MapHeight then
      yl := MapHeight - ys;
    if (xl <= 0) or (yl <= 0) then
      exit;
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

procedure TMainScreen.PaintLoc(Loc: integer; Radius: integer = 0);
var
  yLoc, x0: integer;
begin
  if MapValid then begin
    yLoc := (Loc + G.lx * 1024) div G.lx - 1024;
    x0 := (Loc + (yLoc and 1 - 2 * Radius + G.lx * 1024) div 2) mod G.lx;
    offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
    ProcessRect(x0, yLoc - 2 * Radius, 4 * Radius + 1, 4 * Radius + 1,
      prPaint or prAutoBounds or prInvalidate);
    Update;
  end;
end;

procedure TMainScreen.PaintLocTemp(Loc: integer; Style: TPaintLocTempStyle);
var
  y0, x0, xMap, yMap: integer;
begin
  with NoMap do begin
    if not MapValid then
      exit;
    Buffer.Canvas.Font.Assign(UniFont[ftSmall]);
    y0 := Loc div G.lx;
    x0 := Loc mod G.lx;
    xMap := (x0 - xw) * (xxt * 2) + y0 and 1 * xxt - G.lx * (xxt * 2);
    // |xMap+xxt-MapWidth/2| -> min
    while abs(2 * (xMap + G.lx * (xxt * 2)) + 2 * xxt - MapWidth) <
      abs(2 * xMap + 2 * xxt - MapWidth) do
      inc(xMap, G.lx * (xxt * 2));
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
procedure TMainScreen.PaintBufferToScreen(xMap, yMap, width, height: integer);
begin
  if xMap + width > MapWidth then
    width := MapWidth - xMap;
  if yMap + height > MapHeight then
    height := MapHeight - yMap;
  if (width <= 0) or (height <= 0) or (width + xMap <= 0) or (height + yMap <= 0)
  then
    exit;

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

procedure TMainScreen.PaintLoc_BeforeMove(FromLoc: integer);
var
  yLoc, x0: integer;
begin
  if MapValid then
  begin
    yLoc := (FromLoc + G.lx * 1024) div G.lx - 1024;
    x0 := (FromLoc + (yLoc and 1 + G.lx * 1024) div 2) mod G.lx;
    offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
    ProcessRect(x0, yLoc, 1, 1, prPaint or prAutoBounds);
  end
end;

procedure TMainScreen.PaintDestination;
var
  Destination: integer;
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
  ProcessOptions: integer;
  rec: TRect;
  DoInvalidate: boolean;
begin
  if me < 0 then
    with offscreen.Canvas do
    begin
      Brush.Color := $000000;
      FillRect(Rect(0, 0, MapWidth, MapHeight));
      Brush.Style := bsClear;
      OffscreenUser := self;
      exit;
    end;

  MainMap.SetPaintBounds(0, 0, MapWidth, MapHeight);
  if OffscreenUser <> self then
  begin
    if OffscreenUser <> nil then
      OffscreenUser.Update;
    // complete working with old owner to prevent rebound
    if MapValid and (xwd = xw) and (ywd = yw) then
      MainMap.SetPaintBounds(0, 0, UsedOffscreenWidth, UsedOffscreenHeight);
    MapValid := false;
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
      offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
      ProcessRect(xw, yw, MapWidth div xxt, MapHeight div yyt,
        prPaint or prInvalidate);
    end else begin
      if (xwd = xw) and (ywd = yw) then
        exit; { map window not moved }
      offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
      rec := Rect(0, 0, MapWidth, MapHeight);
{$IFDEF WINDOWS}
      ScrollDC(offscreen.Canvas.Handle, (xwd - xw) * (xxt * 2), (ywd - yw) * yyt,
        rec, rec, 0, nil);
{$ENDIF}
{$IFDEF UNIX}
      ScrollDC(offscreen.Canvas, (xwd - xw) * (xxt * 2), (ywd - yw) * yyt,
        rec, rec, 0, nil);
{$ENDIF}
      for DoInvalidate := false to FastScrolling do begin
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
  MapValid := true;
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
    result := ScreenTools.MovementToString(Un.Movement);
    if Un.Master >= 0 then
      result := '(' + result + ')'
    else if (MyModel[Un.mix].Domain = dAir) and
      (MyModel[Un.mix].Kind <> mkSpecial_Glider) then
      result := Format('%s(%d)', [result, Un.Fuel]);
  end;

var
  i, uix, uixDefender, x, xSrc, ySrc, xSrcBase, ySrcBase, CostFactor, Count,
    mixShow, xTreasurySection, xResearchSection, JobFocus, TrueMoney,
    TrueResearch: integer;
  Tile: Cardinal;
  s: string;
  unx: TUn;
  UnitInfo: TUnitInfo;
  JobProgressData: TJobProgressData;
  Prio: boolean;
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
      for i := 0 to 5 do
        if i <> 3 then
          Dump(Panel, HGrSystem, xMini + G.lx - 42 + 16 * i, PanelHeight - 26,
            8, 8, 121 + i * 9, 61);

    if ClientMode = cEditMap then
    begin
      for i := 0 to TrRow - 1 do
        trix[i] := -1;
      Count := 0;
      for i := 0 to nBrushTypes - 1 do
      begin // display terrain types
        if (Count >= TrRow * sb.Position) and (Count < TrRow * (sb.Position + 1))
        then
        begin
          trix[Count - TrRow * sb.Position] := BrushTypes[i];
          x := (Count - TrRow * sb.Position) * TrPitch;
          xSrcBase := -1;
          case BrushTypes[i] of
            0 .. 8:
              begin
                xSrc := BrushTypes[i];
                ySrc := 0
              end;
            9 .. 30:
              begin
                xSrcBase := 2;
                ySrcBase := 2;
                xSrc := 0;
                ySrc := 2 * integer(BrushTypes[i]) - 15
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
                ySrc := 12 + BrushTypes[i] shr 25;
              end;
            tiIrrigation, tiFarm, tiMine, tiBase:
              begin
                xSrc := BrushTypes[i] shr 12 - 1;
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
              Sprite(Panel, HGrTerrain, xTroop + 2 + x, yTroop + 9 - yyt, xxt * 2,
                yyt * 3, 1 + xSrcBase * (xxt * 2 + 1),
                1 + ySrcBase * (yyt * 3 + 1));
            Sprite(Panel, HGrTerrain, xTroop + 2 + x, yTroop + 9 - yyt, xxt * 2,
              yyt * 3, 1 + xSrc * (xxt * 2 + 1), 1 + ySrc * (yyt * 3 + 1));
            if BrushTypes[i] = BrushType then begin
              ScreenTools.Frame(Panel.Canvas, xTroop + 2 + x,
                yTroop + 7 - yyt div 2, xTroop + 2 * xxt + x,
                yTroop + 2 * yyt + 11, $000000, $000000);
              ScreenTools.Frame(Panel.Canvas, xTroop + 1 + x,
                yTroop + 6 - yyt div 2, xTroop + 2 * xxt - 1 + x,
                yTroop + 2 * yyt + 10, MainTexture.ColorMark, MainTexture.ColorMark);
            end;
          end;
        end;
        inc(Count)
      end;
      case BrushType of
        fDesert, fPrairie, fTundra, fArctic, fSwamp, fHills, fMountains:
          s := Phrases.Lookup('TERRAIN', BrushType);
        fShore:
          s := Format(Phrases.Lookup('TWOTERRAINS'),
            [Phrases.Lookup('TERRAIN', fOcean), Phrases.Lookup('TERRAIN',
            fShore)]);
        fGrass:
          s := Format(Phrases.Lookup('TWOTERRAINS'),
            [Phrases.Lookup('TERRAIN', fGrass), Phrases.Lookup('TERRAIN',
            fGrass + 12)]);
        fForest:
          s := Format(Phrases.Lookup('TWOTERRAINS'),
            [Phrases.Lookup('TERRAIN', fForest), Phrases.Lookup('TERRAIN',
            fJungle)]);
        fRiver:
          s := Phrases.Lookup('RIVER');
        fDeadLands, fDeadLands or fCobalt, fDeadLands or fUranium,
          fDeadLands or fMercury:
          s := Phrases.Lookup('TERRAIN', 3 * 12 + BrushType shr 25);
        fPrefStartPos:
          s := Phrases.Lookup('MAP_PREFSTART');
        fStartPos:
          s := Phrases.Lookup('MAP_START');
        fPoll:
          s := Phrases.Lookup('POLL');
      else // terrain improvements
        begin
          case BrushType of
            fRoad:
              i := 1;
            fRR:
              i := 2;
            tiIrrigation:
              i := 4;
            tiFarm:
              i := 5;
            tiMine:
              i := 7;
            fCanal:
              i := 8;
            tiFort:
              i := 10;
            tiBase:
              i := 12;
          end;
          s := Phrases.Lookup('JOBRESULT', i);
        end
      end;
      LoweredTextOut(Panel.Canvas, -1, MainTexture, xTroop + 1,
        PanelHeight - 19, s);
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
          with Tribe[me].ModelPicture[mixShow] do
          begin
            Sprite(Panel, HGr, xMidPanel + 7 + 12, yTroop + 1, 64, 48,
              pix mod 10 * 65 + 1, pix div 10 * 49 + 1);
            if MyUn[UnFocus].Flags and unFortified <> 0 then
              Sprite(Panel, HGrStdUnits, xMidPanel + 7 + 12, yTroop + 1,
                xxu * 2, yyu * 2, 1 + 6 * (xxu * 2 + 1), 1);
          end;

          MakeBlue(Panel, xMidPanel + 7 + 12 + 10, yTroop - 13, 44, 12);
          s := MovementToString(MyUn[UnFocus]);
          RisedTextOut(Panel.Canvas, xMidPanel + 7 + 12 + 32 -
            BiColorTextWidth(Panel.Canvas, s) div 2, yTroop - 16, s);

          s := IntToStr(Health) + '%';
          LightGradient(Panel.Canvas, xMidPanel + 7 + 12 + 7, PanelHeight - 22,
            (Health + 1) div 2, (ColorOfHealth(Health) and $FEFEFE shr 2) * 3);
          if Health < 100 then
            LightGradient(Panel.Canvas, xMidPanel + 7 + 12 + 7 + (Health + 1)
              div 2, PanelHeight - 22, 50 - (Health + 1) div 2, $000000);
          RisedTextOut(Panel.Canvas, xMidPanel + 7 + 12 + 32 -
            BiColorTextWidth(Panel.Canvas, s) div 2, PanelHeight - 23, s);

          FrameImage(Panel.Canvas, HGrSystem.Data,
            xMidPanel + 7 + xUnitText, yTroop + 15, 12, 14,
            121 + Exp div ExpCost * 13, 28);
          if Job = jCity then
            s := Tribe[me].ModelName[-1]
          else
            s := Tribe[me].ModelName[mix];
          if Home >= 0 then
          begin
            LoweredTextOut(Panel.Canvas, -1, MainTexture,
              xMidPanel + 7 + xUnitText + 18, yTroop + 5, s);
            LoweredTextOut(Panel.Canvas, -1, MainTexture,
              xMidPanel + 7 + xUnitText + 18, yTroop + 21,
              '(' + CityName(MyCity[Home].ID) + ')');
          end
          else
            LoweredTextOut(Panel.Canvas, -1, MainTexture,
              xMidPanel + 7 + xUnitText + 18, yTroop + 13, s);
        end;

      if (UnFocus >= 0) and (MyUn[UnFocus].Loc <> TroopLoc) then
      begin // divide panel
        if SmallScreen and not supervising then
          x := xTroop - 8
        else
          x := xTroop - 152;
        Pen.Color := MainTexture.ColorBevelShade;
        MoveTo(x - 1, PanelHeight - MidPanelHeight + 2);
        LineTo(x - 1, PanelHeight);
        Pen.Color := MainTexture.ColorBevelLight;
        MoveTo(x, PanelHeight - MidPanelHeight + 2);
        LineTo(x, PanelHeight);
      end;

      for i := 0 to 23 do
        trix[i] := -1;
      if MyMap[TroopLoc] and fUnit <> 0 then
      begin
        if MyMap[TroopLoc] and fOwned <> 0 then
        begin
          if (TrCnt > 1) or (UnFocus < 0) or (MyUn[UnFocus].Loc <> TroopLoc)
          then
          begin
            LoweredTextOut(Panel.Canvas, -1, MainTexture, xTroop + 10,
              PanelHeight - 24, Phrases.Lookup('PRESENT'));
            Server(sGetDefender, me, TroopLoc, uixDefender);
            Count := 0;
            for Prio := true downto false do
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
                      MakeUnitInfo(me, unx, UnitInfo);
                      x := (Count - TrRow * sb.Position) * TrPitch;
                      if uix = UnFocus then
                      begin
                        ScreenTools.Frame(Panel.Canvas, xTroop + 4 + x,
                          yTroop + 3, xTroop + 64 + x, yTroop + 47,
                          $000000, $000000);
                        ScreenTools.Frame(Panel.Canvas, xTroop + 3 + x,
                          yTroop + 2, xTroop + 63 + x, yTroop + 46,
                          MainTexture.ColorMark, MainTexture.ColorMark);
                      end
                      else if (unx.Master >= 0) and (unx.Master = UnFocus) then
                      begin
                        CFrame(Panel.Canvas, xTroop + 4 + x, yTroop + 3,
                          xTroop + 64 + x, yTroop + 47, 8, $000000);
                        CFrame(Panel.Canvas, xTroop + 3 + x, yTroop + 2,
                          xTroop + 63 + x, yTroop + 46, 8, MainTexture.ColorMark);
                      end;
                      NoMapPanel.SetOutput(Panel);
                      NoMapPanel.PaintUnit(xTroop + 2 + x, yTroop + 1, UnitInfo,
                        unx.Status);
                      if (ClientMode < scContact) and
                        ((unx.Job > jNone) or
                        (unx.Status and (usStay or usRecover or usGoto) <> 0))
                      then
                        Sprite(Panel, HGrSystem, xTroop + 2 + 60 - 20 + x,
                          yTroop + 35, 20, 20, 81, 25);

                      if not supervising then
                      begin
                        MakeBlue(Panel, xTroop + 2 + 10 + x,
                          yTroop - 13, 44, 12);
                        s := MovementToString(unx);
                        RisedTextOut(Panel.Canvas,
                          xTroop + x + 34 - BiColorTextWidth(Panel.Canvas, s)
                          div 2, yTroop - 16, s);
                      end;
                    end;
                    inc(Count)
                  end;
                end; // for uix:=0 to MyRO.nUn-1
            assert(Count = TrCnt);
          end;
        end
        else
        begin
          LoweredTextOut(Panel.Canvas, -1, MainTexture, xTroop + 8,
            PanelHeight - 24, Phrases.Lookup('PRESENT'));
          Server(sGetUnits, me, TroopLoc, Count);
          for i := 0 to Count - 1 do
            if (i >= TrRow * sb.Position) and (i < TrRow * (sb.Position + 1)) then
            begin // display enemy units
              trix[i - TrRow * sb.Position] := i;
              x := (i - TrRow * sb.Position) * TrPitch;
              NoMapPanel.SetOutput(Panel);
              NoMapPanel.PaintUnit(xTroop + 2 + x, yTroop + 1,
                MyRO.EnemyUn[MyRO.nEnemyUn + i], 0);
            end;
        end;
      end;
      if not SmallScreen or supervising then
      begin // show terrain and improvements
        with NoMapPanel do
          PaintZoomedTile(Panel, xTerrain - xxt * 2, 110 - yyt * 3, TroopLoc);
        if (UnFocus >= 0) and (MyUn[UnFocus].Job <> jNone) then begin
          JobFocus := MyUn[UnFocus].Job;
          Server(sGetJobProgress, me, MyUn[UnFocus].Loc, JobProgressData);
          MakeBlue(Panel, xTerrain - 72, 148 - 17, 144, 31);
          PaintRelativeProgressBar(Panel.Canvas, 3, xTerrain - 68, 148 + 3, 63,
            JobProgressData[JobFocus].Done,
            JobProgressData[JobFocus].NextTurnPlus,
            JobProgressData[JobFocus].Required, true, MainTexture);
          s := Format('%s/%s',
            [ScreenTools.MovementToString(JobProgressData[JobFocus].Done),
            ScreenTools.MovementToString(JobProgressData[JobFocus].Required)]);
          RisedTextOut(Panel.Canvas, xTerrain + 6, 148 - 3, s);
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
          s := Phrases.Lookup('JOBRESULT', JobFocus);
          RisedTextOut(Panel.Canvas, xTerrain - BiColorTextWidth(Panel.Canvas,
            s) div 2, 148 - 19, s);
        end;
        if MyMap[TroopLoc] and (fTerrain or fSpecial) = fGrass or fSpecial1 then
          s := Phrases.Lookup('TERRAIN', fGrass + 12)
        else if MyMap[TroopLoc] and fDeadLands <> 0 then
          s := Phrases.Lookup('TERRAIN', 3 * 12)
        else if (MyMap[TroopLoc] and fTerrain = fForest) and
          IsJungle(TroopLoc div G.lx) then
          s := Phrases.Lookup('TERRAIN', fJungle)
        else
          s := Phrases.Lookup('TERRAIN', MyMap[TroopLoc] and fTerrain);
        RisedTextOut(Panel.Canvas, xTerrain - BiColorTextWidth(Panel.Canvas, s)
          div 2, 99, s);
      end;

      if TerrainBtn.Visible then
        with TerrainBtn do
          RFrame(Panel.Canvas, Left - 1, Top - self.ClientHeight +
            (PanelHeight - 1), Left + width, Top + height - self.ClientHeight +
            PanelHeight, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight)
    end; { if TroopLoc>=0 }
  end;

  for i := 0 to ControlCount - 1 do
    if Controls[i] is TButtonB then
      with TButtonB(Controls[i]) do
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
    for i := 0 to ControlCount - 1 do
      if Controls[i] is TButtonC then
        with TButtonC(Controls[i]) do
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
    if supervising then
    begin // normalize values from after-turn state
      dec(TrueMoney, TaxSum);
      if TrueMoney < 0 then
        TrueMoney := 0; // shouldn't happen
      dec(TrueResearch, ScienceSum);
      if TrueResearch < 0 then
        TrueResearch := 0; // shouldn't happen
    end;

    // treasury section
    ImageOp_BCC(TopBar, Templates.Data, Point(xTreasurySection + 8, 1), TreasuryIcon.BoundsRect,
      $40A040, $4030C0);
    s := IntToStr(TrueMoney);
    LoweredTextOut(TopBar.Canvas, -1, MainTexture, xTreasurySection + 48, 0,
      s + '%c');
    if MyRO.Government <> gAnarchy then
    begin
      ImageOp_BCC(TopBar, Templates.Data, Point(xTreasurySection + 48, 22), ChangeIcon.BoundsRect,
        $0000C0, $0080C0);
      if TaxSum >= 0 then
        s := Format(Phrases.Lookup('MONEYGAINPOS'), [TaxSum])
      else
        s := Format(Phrases.Lookup('MONEYGAINNEG'), [TaxSum]);
      LoweredTextOut(TopBar.Canvas, -1, MainTexture, xTreasurySection + 48 +
        15, 18, s);
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
      Server(sGetTechCost, me, 0, i);
      CostFactor := CostFactor * 22; // length of progress bar
      PaintRelativeProgressBar(TopBar.Canvas, 2, xResearchSection + 48 + 1, 26,
        CostFactor, TrueResearch, ScienceSum, i, true, MainTexture);

      if MyRO.ResearchTech < 0 then
        s := Phrases.Lookup('SCIENCE')
      else if MyRO.ResearchTech = adMilitary then
        s := Phrases.Lookup('INITUNIT')
      else
      begin
        s := Phrases.Lookup('ADVANCES', MyRO.ResearchTech);
        if MyRO.ResearchTech in FutureTech then
          if MyRO.Tech[MyRO.ResearchTech] >= 1 then
            s := s + ' ' + IntToStr(MyRO.Tech[MyRO.ResearchTech] + 1)
          else
            s := s + ' 1';
      end;
      if ScienceSum > 0 then
      begin
        { j:=(i-MyRO.Research-1) div ScienceSum +1;
          if j<1 then j:=1;
          if j>1 then
          s:=Format(Phrases.Lookup('TECHWAIT'),[s,j]); }
        LoweredTextOut(TopBar.Canvas, -1, MainTexture,
          xResearchSection + 48, 0, s);
      end
      else
        LoweredTextOut(TopBar.Canvas, -1, MainTexture,
          xResearchSection + 48, 0, s);
    end
    else
      CostFactor := 0;
    if (MyData.FarTech <> adNexus) and (ScienceSum > 0) then
    begin
      ImageOp_BCC(TopBar, Templates.Data, Point(xResearchSection + 48 + CostFactor + 11,
        22), ChangeIcon.BoundsRect, $0000C0, $0080C0);
      s := Format(Phrases.Lookup('TECHGAIN'), [ScienceSum]);
      LoweredTextOut(TopBar.Canvas, -1, MainTexture, xResearchSection + 48 +
        CostFactor + 26, 18, s);
    end;
  end;
  if ClientMode <> cEditMap then
  begin
    TopBar.Canvas.Font.Assign(UniFont[ftCaption]);
    s := TurnToString(MyRO.Turn);
    RisedTextOut(TopBar.Canvas,
      40 + (xTreasurySection - 40 - BiColorTextWidth(TopBar.Canvas, s))
      div 2, 6, s);
    TopBar.Canvas.Font.Assign(UniFont[ftNormal]);
  end;
  RectInvalidate(0, 0, ClientWidth, TopBarHeight);
end;

procedure TMainScreen.FocusNextUnit(Dir: Integer);
var
  i, uix, NewFocus: Integer;
begin
  if ClientMode >= scContact then
    Exit;
  DestinationMarkON := False;
  PaintDestination;
  NewFocus := -1;
  for i := 1 to MyRO.nUn do begin
    uix := (UnFocus + i * Dir + MyRO.nUn) mod MyRO.nUn;
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

procedure TMainScreen.FocusOnLoc(Loc: integer; Options: integer = 0);
var
  dx: integer;
  Outside, Changed: boolean;
begin
  with MainMap do begin
    dx := G.lx + 1 - (xw - Loc + G.lx * 1024 + 1) mod G.lx;
    Outside := (dx >= (MapWidth + 1) div (xxt * 2) - 2) or (ywmax > 0) and
      ((yw > 0) and (Loc div G.lx <= yw + 1) or (yw < ywmax) and
      (Loc div G.lx >= yw + (MapHeight - 1) div yyt - 2));
  end;
  Changed := true;
  if Outside then begin
    Centre(Loc);
    PaintAllMaps;
  end
  else if not MapValid then
    PaintAllMaps
  else
    Changed := false;
  if Options and flRepaintPanel <> 0 then
    PanelPaint;
  if Changed and (Options and flImmUpdate <> 0) then
    Update;
end;

procedure TMainScreen.NextUnit(NearLoc: Integer; AutoTurn: Boolean);
var
  Dist, TestDist: Single;
  i, uix, NewFocus: Integer;
  GotoOnly: Boolean;
begin
  Dist := 0;
  if ClientMode >= scContact then
    Exit;
  DestinationMarkON := False;
  PaintDestination;
  for GotoOnly := GoOnPhase downto False do begin
    NewFocus := -1;
    for i := 1 to MyRO.nUn do begin
      uix := (UnFocus + i) mod MyRO.nUn;
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

procedure TMainScreen.Scroll(dx, dy: integer);
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
  dx, dy, ScrollSpeed: integer;
begin
  if idle and (me >= 0) and (GameMode <> cMovie) then
    if (fsModal in Screen.ActiveForm.FormState) or
      (Screen.ActiveForm is TBufferedDrawDlg) and
      (TBufferedDrawDlg(Screen.ActiveForm).WindowMode <> wmPersistent) then
    begin
      BlinkTime := BlinkOnTime + BlinkOffTime - 1;
      if not BlinkON then
      begin
        BlinkON := true;
        if UnFocus >= 0 then
          PaintLocTemp(MyUn[UnFocus].Loc)
        else if TurnComplete and not supervising then
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
        if Mouse.CursorPos.y < Screen.height - PanelHeight then
          if Mouse.CursorPos.x = 0 then
            dx := -ScrollSpeed // scroll left
          else if Mouse.CursorPos.x = Screen.width - 1 then
            dx := ScrollSpeed; // scroll right
        if Mouse.CursorPos.y = 0 then
          dy := -ScrollSpeed // scroll up
        else if (Mouse.CursorPos.y = Screen.height - 1) and
          (Mouse.CursorPos.x >= TerrainBtn.Left + TerrainBtn.width) and
          (Mouse.CursorPos.x < xRightPanel + 10 - 8) then
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
      DestinationMarkON := true;
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
      else if TurnComplete and not supervising then
      begin
        if BlinkTime = 0 then
          EOT.SetButtonIndexFast(eotBlinkOff)
        else if BlinkTime = BlinkOffTime then
          EOT.SetButtonIndexFast(eotBlinkOn);
      end;
    end;
end;

procedure TMainScreen.SetMapPos(Loc: integer; MapPos: TPoint);
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

procedure TMainScreen.Centre(Loc: integer);
begin
  SetMapPos(Loc, Point(MapWidth div 2, MapHeight div 2));
end;

function TMainScreen.ZoomToCity(Loc: integer; NextUnitOnClose: boolean = false;
  ShowEvent: integer = 0): boolean;
begin
  result := MyMap[Loc] and (fOwned or fSpiedOut) <> 0;
  if result then
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

function TMainScreen.LocationOfScreenPixel(x, y: integer): Integer;
var
  qx, qy: integer;
begin
  with MainMap do begin
    qx := (x * (yyt * 2) + y * (xxt * 2) + xxt * yyt * 2) div (xxt * yyt * 4) - 1;
    qy := (y * (xxt * 2) - x * (yyt * 2) - xxt * yyt * 2 + 4000 * xxt * yyt)
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
  Shift: TShiftState; x, y: integer);
var
  i, uix, emix, p1, dx, dy, MouseLoc: integer;
  EditTileData: TEditTileData;
  m, m2: TMenuItem;
  MoveAdviceData: TMoveAdviceData;
  DoCenter: boolean;
begin
  if GameMode = cMovie then
    exit;

  if CityDlg.Visible then
    CityDlg.Close;
  if UnitStatDlg.Visible then
    UnitStatDlg.Close;
  MouseLoc := LocationOfScreenPixel(x, y);
  if (MouseLoc < 0) or (MouseLoc >= G.lx * G.ly) then
    exit;
  if (Button = mbLeft) and not(ssShift in Shift) then
  begin
    DoCenter := true;
    if ClientMode = cEditMap then
    begin
      DoCenter := false;
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
      Server(sEditTile, me, 0, EditTileData);
      Edited := true;
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
        DoCenter := false;
      end
      else
      begin
        UnitStatDlg.ShowNewContent_EnemyCity(wmPersistent, MouseLoc);
        DoCenter := false;
      end;
    end
    else if MyMap[MouseLoc] and fUnit <> 0 then { unit clicked }
      if MyMap[MouseLoc] and fOwned <> 0 then
      begin
        DoCenter := false;
        if not supervising and (ClientMode < scContact) then
        begin // not in negotiation mode
          if (UnFocus >= 0) and (MyUn[UnFocus].Loc = MouseLoc) then
          begin // rotate
            uix := (UnFocus + 1) mod MyRO.nUn;
            i := MyRO.nUn - 1;
            while i > 0 do
            begin
              if (MyUn[uix].Loc = MouseLoc) and (MyUn[uix].Job = jNone) and
                (MyUn[uix].Status and (usStay or usRecover or usEnhance or
                usWaiting) = usWaiting) then
                Break;
              dec(i);
              uix := (uix + 1) mod MyRO.nUn;
            end;
            if i = 0 then
              uix := UnFocus;
          end
          else
            Server(sGetDefender, me, MouseLoc, uix);
          if uix <> UnFocus then
            SetUnFocus(uix);
          TurnComplete := false;
          EOT.ButtonIndex := eotGray;
        end;
        SetTroopLoc(MouseLoc);
        PanelPaint;
      end // own unit
      else if (MyMap[MouseLoc] and fSpiedOut <> 0) and not(ssCtrl in Shift) then
      begin
        DoCenter := false;
        SetTroopLoc(MouseLoc);
        PanelPaint;
      end
      else
      begin
        DoCenter := false;
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
    if supervising then
    begin
      EditLoc := MouseLoc;
      Server(sGetModels, me, 0, nil^);
      EmptyMenu(mCreateUnit);
      for p1 := 0 to nPl - 1 do
        if 1 shl p1 and MyRO.Alive <> 0 then
        begin
          m := TMenuItem.Create(mCreateUnit);
          m.Caption := Tribe[p1].TPhrase('SHORTNAME');
          for emix := MyRO.nEnemyModel - 1 downto 0 do
            if (MyRO.EnemyModel[emix].Owner = p1) and
              (Server(sCreateUnit - sExecute + p1 shl 4, me,
              MyRO.EnemyModel[emix].mix, MouseLoc) >= rExecuted) then
            begin
              if not Assigned(Tribe[p1].ModelPicture[MyRO.EnemyModel[emix].mix].HGr) then
                InitEnemyModel(emix);
              m2 := TMenuItem.Create(m);
              m2.Caption := Tribe[p1].ModelName[MyRO.EnemyModel[emix].mix];
              m2.Tag := p1 shl 16 + MyRO.EnemyModel[emix].mix;
              m2.OnClick := CreateUnitClick;
              m.Add(m2);
            end;
          m.Visible := m.Count > 0;
          mCreateUnit.Add(m);
        end;
      if FullScreen then
        EditPopup.Popup(Left + x, Top + y)
      else
        EditPopup.Popup(Left + x + 4,
          Top + y + GetSystemMetrics(SM_CYCAPTION) + 4);
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
          DestinationMarkON := false;
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
              pAtt := me;
              mixAtt := mix;
              HealthAtt := Health;
              ExpAtt := Exp;
              FlagsAtt := Flags;
            end;
            BattleDlg.Forecast.Movement := MyUn[UnFocus].Movement;
            if (Server(sGetBattleForecastEx, me, MouseLoc, BattleDlg.Forecast)
              >= rExecuted) and (BattleDlg.Forecast.EndHealthAtt <= 0) then
            begin
              BattleDlg.uix := UnFocus;
              BattleDlg.ToLoc := MouseLoc;
              BattleDlg.IsSuicideQuery := true;
              BattleDlg.ShowModal;
              if BattleDlg.ModalResult <> mrOK then
                exit;
            end;
          end;
          DestinationMarkON := false;
          PaintDestination;
          Status := Status and not(usStay or usRecover or usEnhance) or
            usWaiting;
          MoveToLoc(MouseLoc, false); { goto }
        end;
      end;
  end
  else if (Button = mbMiddle) and (UnFocus >= 0) and
    (MyModel[MyUn[UnFocus].mix].Kind in [mkSettler, mkSlaves]) then
  begin
    DestinationMarkON := false;
    PaintDestination;
    MyUn[UnFocus].Status := MyUn[UnFocus].Status and
      ($FFFF - usStay - usRecover - usGoto) or usEnhance;
    uix := UnFocus;
    if MouseLoc <> MyUn[uix].Loc then
      MoveToLoc(MouseLoc, true); { goto }
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
      pAtt := me;
      mixAtt := mix;
      HealthAtt := Health;
      ExpAtt := Exp;
      FlagsAtt := Flags;
    end;
    BattleDlg.Forecast.Movement := MyUn[UnFocus].Movement;
    if Server(sGetBattleForecastEx, me, MouseLoc, BattleDlg.Forecast) >= rExecuted
    then
    begin
      BattleDlg.uix := UnFocus;
      BattleDlg.ToLoc := MouseLoc;
      BattleDlg.Left := x - BattleDlg.width div 2;
      if BattleDlg.Left < 0 then
        BattleDlg.Left := 0
      else if BattleDlg.Left + BattleDlg.width > Screen.width then
        BattleDlg.Left := Screen.width - BattleDlg.width;
      BattleDlg.Top := y - BattleDlg.height div 2;
      if BattleDlg.Top < 0 then
        BattleDlg.Top := 0
      else if BattleDlg.Top + BattleDlg.height > Screen.height then
        BattleDlg.Top := Screen.height - BattleDlg.height;
      BattleDlg.IsSuicideQuery := false;
      BattleDlg.Show;
    end;
  end;
end;

function TMainScreen.MoveUnit(dx, dy: integer; Options: integer): integer;
// move focused unit to adjacent tile
var
  i, cix, uix, euix, FromLoc, ToLoc, DirCode, UnFocus0, Defender, Mission, p1,
    NewTiles, cixChanged: integer;
  OldToTile: Cardinal;
  CityCaptured, IsAttack, OldUnrest, NewUnrest, NeedEcoUpdate, NeedRepaintPanel,
    ToTransport, ToShip: boolean;
  PlaneReturnData: TPlaneReturnData;
  QueryItem: string;
begin
  result := eInvalid;
  UnFocus0 := UnFocus;
  FromLoc := MyUn[UnFocus].Loc;
  ToLoc := dLoc(FromLoc, dx, dy);
  if (ToLoc < 0) or (ToLoc >= G.lx * G.ly) then
  begin
    result := eInvalid;
    exit;
  end;
  if MyMap[ToLoc] and fStealthUnit <> 0 then
  begin
    SoundMessage(Phrases.Lookup('ATTACKSTEALTH'), '');
    exit;
  end;
  if MyMap[ToLoc] and fHiddenUnit <> 0 then
  begin
    SoundMessage(Phrases.Lookup('ATTACKSUB'), '');
    exit;
  end;

  if MyMap[ToLoc] and (fUnit or fOwned) = fUnit then
  begin // attack -- search enemy unit
    if (MyModel[MyUn[UnFocus].mix].Attack = 0) and
      not((MyModel[MyUn[UnFocus].mix].Cap[mcBombs] > 0) and
      (MyUn[UnFocus].Flags and unBombsLoaded <> 0)) then
    begin
      SoundMessage(Phrases.Lookup('NOATTACKER'), '');
      exit;
    end;
    euix := MyRO.nEnemyUn - 1;
    while (euix >= 0) and (MyRO.EnemyUn[euix].Loc <> ToLoc) do
      dec(euix);
  end;

  DirCode := dx and 7 shl 4 + dy and 7 shl 7;
  result := Server(sMoveUnit - sExecute + DirCode, me, UnFocus, nil^);
  if (result < rExecuted) and (MyUn[UnFocus].Job > jNone) then
    Server(sStartJob + jNone shl 4, me, UnFocus, nil^);
  if (result < rExecuted) and (result <> eNoTime_Move) then
  begin
    case result of
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
            ToShip := false;
            ToTransport := false;
            for uix := 0 to MyRO.nUn - 1 do
              if (MyUn[uix].Loc = ToLoc) and
                (MyModel[MyUn[uix].mix].Domain = dSea) then
              begin
                ToShip := true;
                if MyModel[MyUn[uix].mix].Cap[mcSeaTrans] > 0 then
                  ToTransport := true;
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
    exit;
  end;

  if ((result = eWon) or (result = eLost) or (result = eBloody)) and
    (MyUn[UnFocus].Movement < 100) and
    (MyModel[MyUn[UnFocus].mix].Cap[mcWill] = 0) then
  begin
    if SimpleQuery(mkYesNo, Format(Phrases.Lookup('FASTATTACK'),
      [MyUn[UnFocus].Movement]), 'NOMOVE_TIME') <> mrOK then
    begin
      result := eInvalid;
      exit;
    end;
    Update; // remove message box from screen
  end;

  OldUnrest := false;
  NewUnrest := false;
  if (result >= rExecuted) and (result and rUnitRemoved = 0) and
    (MyMap[ToLoc] and (fUnit or fOwned) <> fUnit) then
  begin
    OldUnrest := UnrestAtLoc(UnFocus, FromLoc);
    NewUnrest := UnrestAtLoc(UnFocus, ToLoc);
    if NewUnrest > OldUnrest then
    begin
      if MyRO.Government = gDemocracy then
      begin
        QueryItem := 'UNREST_NOTOWN';
        p1 := me;
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
          result := eInvalid;
          exit;
        end;
      end;
      Update; // remove message box from screen
    end;
  end;

  if (result >= rExecuted) and (MyModel[MyUn[UnFocus].mix].Domain = dAir) and
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
    if Server(sGetPlaneReturn, me, UnFocus, PlaneReturnData) = eNoWay then
    begin
      if MyModel[MyUn[UnFocus].mix].Kind = mkSpecial_Glider then
        QueryItem := 'LOWFUEL_GLIDER'
      else
        QueryItem := 'LOWFUEL';
      if SimpleQuery(mkYesNo, Phrases.Lookup(QueryItem), 'WARNING_LOWSUPPORT')
        <> mrOK then
      begin
        result := eInvalid;
        exit;
      end;
      Update; // remove message box from screen
      MyUn[UnFocus].Status := MyUn[UnFocus].Status or usToldNoReturn;
    end;
  end;

  if result = eMissionDone then
  begin
    ModalSelectDlg.ShowNewContent(wmModal, kMission);
    Update; // dialog still on screen
    Mission := ModalSelectDlg.result;
    if Mission < 0 then
      exit;
    Server(sSetSpyMission + Mission shl 4, me, 0, nil^);
  end;

  CityCaptured := false;
  if result = eNoTime_Move then
    Play('NOMOVE_TIME')
  else
  begin
    NeedEcoUpdate := false;
    DestinationMarkON := false;
    PaintDestination;
    if result and rUnitRemoved <> 0 then
      CityOptimizer_BeforeRemoveUnit(UnFocus);
    IsAttack := (result = eBombarded) or (result <> eMissionDone) and
      (MyMap[ToLoc] and (fUnit or fOwned) = fUnit);
    if not IsAttack then
    begin // move
      cix := MyRO.nCity - 1; { look for own city at dest location }
      while (cix >= 0) and (MyCity[cix].Loc <> ToLoc) do
        dec(cix);
      if (result <> eMissionDone) and (MyMap[ToLoc] and fCity <> 0) and (cix < 0)
      then
        CityCaptured := true;
      result := Server(sMoveUnit + DirCode, me, UnFocus, nil^);
      case result of
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
            if result and rUnitRemoved <> 0 then
              UnFocus := -1 // unit died
            else
            begin
              assert(UnFocus >= 0);
              MyUn[UnFocus].Status := MyUn[UnFocus].Status and
                not(usStay or usRecover);
              for uix := 0 to MyRO.nUn - 1 do
                if MyUn[uix].Master = UnFocus then
                  MyUn[uix].Status := MyUn[uix].Status and not usWaiting;
              if CityCaptured and
                (MyRO.Government in [gRepublic, gDemocracy, gFuture]) then
              begin // borders have moved, unrest might have changed in any city
                CityOptimizer_BeginOfTurn;
                NeedEcoUpdate := true;
              end
              else
              begin
                if OldUnrest <> NewUnrest then
                begin
                  CityOptimizer_CityChange(MyUn[UnFocus].Home);
                  for uix := 0 to MyRO.nUn - 1 do
                    if MyUn[uix].Master = UnFocus then
                      CityOptimizer_CityChange(MyUn[uix].Home);
                  NeedEcoUpdate := true;
                end;
                if (MyRO.Government = gDespotism) and
                  (MyModel[MyUn[UnFocus].mix].Kind = mkSpecial_TownGuard) then
                begin
                  if MyMap[FromLoc] and fCity <> 0 then
                  begin // town guard moved out of city in despotism -- reoptimize!
                    cixChanged := MyRO.nCity - 1;
                    while (cixChanged >= 0) and
                      (MyCity[cixChanged].Loc <> FromLoc) do
                      dec(cixChanged);
                    assert(cixChanged >= 0);
                    if cixChanged >= 0 then
                    begin
                      CityOptimizer_CityChange(cixChanged);
                      NeedEcoUpdate := true;
                    end;
                  end;
                  if (MyMap[ToLoc] and fCity <> 0) and not CityCaptured then
                  begin // town guard moved into city in despotism -- reoptimize!
                    cixChanged := MyRO.nCity - 1;
                    while (cixChanged >= 0) and
                      (MyCity[cixChanged].Loc <> ToLoc) do
                      dec(cixChanged);
                    assert(cixChanged >= 0);
                    if cixChanged >= 0 then
                    begin
                      CityOptimizer_CityChange(cixChanged);
                      NeedEcoUpdate := true;
                    end;
                  end;
                end;
              end;
            end;
          end;
      else
        assert(false);
      end;
      SetTroopLoc(ToLoc);
    end
    else
    begin { enemy unit -- attack }
      if result = eBombarded then
        Defender := MyRO.Territory[ToLoc]
      else
        Defender := MyRO.EnemyUn[euix].Owner;
      { if MyRO.Treaty[Defender]=trCeaseFire then
        if SimpleQuery(mkYesNo,Phrases.Lookup('FRCANCELQUERY_CEASEFIRE'),
        'MSG_DEFAULT')<>mrOK then
        exit; }
      if (Options and muNoSuicideCheck = 0) and (result and rUnitRemoved <> 0)
        and (result <> eMissionDone) then
      begin // suicide query
        with MyUn[UnFocus], BattleDlg.Forecast do
        begin
          pAtt := me;
          mixAtt := mix;
          HealthAtt := Health;
          ExpAtt := Exp;
          FlagsAtt := Flags;
        end;
        BattleDlg.Forecast.Movement := MyUn[UnFocus].Movement;
        Server(sGetBattleForecastEx, me, ToLoc, BattleDlg.Forecast);
        BattleDlg.uix := UnFocus;
        BattleDlg.ToLoc := ToLoc;
        BattleDlg.IsSuicideQuery := true;
        BattleDlg.ShowModal;
        if BattleDlg.ModalResult <> mrOK then
          exit;
      end;

      cixChanged := -1;
      if (result and rUnitRemoved <> 0) and (MyRO.Government = gDespotism) and
        (MyModel[MyUn[UnFocus].mix].Kind = mkSpecial_TownGuard) and
        (MyMap[FromLoc] and fCity <> 0) then
      begin // town guard died in city in despotism -- reoptimize!
        cixChanged := MyRO.nCity - 1;
        while (cixChanged >= 0) and (MyCity[cixChanged].Loc <> FromLoc) do
          dec(cixChanged);
        assert(cixChanged >= 0);
      end;

      for i := 0 to MyRO.nEnemyModel - 1 do
        LostArmy[i] := MyRO.EnemyModel[i].Lost;
      OldToTile := MyMap[ToLoc];
      result := Server(sMoveUnit + DirCode, me, UnFocus, nil^);
      nLostArmy := 0;
      for i := 0 to MyRO.nEnemyModel - 1 do
      begin
        LostArmy[i] := MyRO.EnemyModel[i].Lost - LostArmy[i];
        inc(nLostArmy, LostArmy[i]);
      end;
      if result and rUnitRemoved <> 0 then
      begin
        UnFocus := -1;
        SetTroopLoc(FromLoc);
      end;
      if (OldToTile and not MyMap[ToLoc] and fCity <> 0) and
        (MyRO.Government in [gRepublic, gDemocracy, gFuture]) then
      begin // city was destroyed, borders have moved, unrest might have changed in any city
        CityOptimizer_BeginOfTurn;
        NeedEcoUpdate := true;
      end
      else
      begin
        if cixChanged >= 0 then
        begin
          CityOptimizer_CityChange(cixChanged);
          NeedEcoUpdate := true;
        end;
        if (result = eWon) or (result = eBloody) or (result = eExpelled) then
        begin
          CityOptimizer_TileBecomesAvailable(ToLoc);
          NeedEcoUpdate := true;
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
    if result and rUnitRemoved <> 0 then
    begin
      CityOptimizer_AfterRemoveUnit;
      ListDlg.RemoveUnit;
      NeedEcoUpdate := true;
    end;
    if NeedEcoUpdate then
    begin
      UpdateViews(true);
      Update;
    end;
  end;

  if result = eMissionDone then
  begin
    p1 := MyRO.Territory[ToLoc];
    case Mission of
      smStealMap:
        begin
          MapValid := false;
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

  NeedRepaintPanel := false;
  if result >= rExecuted then
  begin
    if CityCaptured and (MyMap[ToLoc] and fCity = 0) then
    begin // city destroyed
      for i := 0 to nWonder - 1 do { tell about destroyed wonders }
        if (MyRO.Wonder[i].CityID = WonderDestroyed) and (MyData.ToldWonders[i].CityID <> WonderDestroyed)
        then
          with MessgExDlg do
          begin
            if WondersDlg.Visible then
              WondersDlg.SmartUpdateContent(false);
            OpenSound := 'WONDER_DESTROYED';
            MessgText := Format(Phrases.Lookup('WONDERDEST'),
              [Phrases.Lookup('IMPROVEMENTS', i)]);
            Kind := mkOkHelp;
            HelpKind := hkImp;
            HelpNo := i;
            IconKind := mikImp;
            IconIndex := i;
            ShowModal;
            MyData.ToldWonders[i] := MyRO.Wonder[i];
          end;
    end;
    if CityCaptured and (MyMap[ToLoc] and fCity <> 0) then
    begin // city captured
      ListDlg.AddCity;
      for i := 0 to nWonder - 1 do { tell about capture of wonders }
        if MyRO.City[MyRO.nCity - 1].Built[i] > 0 then
          with MessgExDlg do
          begin
            if WondersDlg.Visible then
              WondersDlg.SmartUpdateContent(false);
            OpenSound := 'WONDER_CAPTURED';
            MessgText := Format(Tribe[me].TPhrase('WONDERCAPTOWN'),
              [Phrases.Lookup('IMPROVEMENTS', i)]);
            Kind := mkOkHelp;
            HelpKind := hkImp;
            HelpNo := i;
            IconKind := mikImp;
            IconIndex := i;
            ShowModal;
            MyData.ToldWonders[i] := MyRO.Wonder[i];
          end;

      if MyRO.Happened and phStealTech <> 0 then
      begin { Temple of Zeus -- choose advance to steal }
        ModalSelectDlg.ShowNewContent(wmModal, kStealTech);
        Server(sStealTech, me, ModalSelectDlg.result, nil^);
      end;
      TellNewModels;

      cix := MyRO.nCity - 1;
      while (cix >= 0) and (MyCity[cix].Loc <> ToLoc) do
        dec(cix);
      assert(cix >= 0);
      MyCity[cix].Status := MyCity[cix].Status and not csResourceWeightsMask or
        (3 shl 4);
      // captured city, set to maximum growth
      NewTiles := 1 shl 13; { exploit central tile only }
      Server(sSetCityTiles, me, cix, NewTiles);
    end
    else
      NeedRepaintPanel := true;
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
    if (UnFocus >= 0) and ((result = eNoTime_Move) or UnitExhausted(UnFocus) or
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
          NextUnit(UnStartLoc, true);
    end
    else if (UnFocus < 0) and (Options and muAutoNext <> 0) then
      NextUnit(UnStartLoc, result <> eMissionDone);
  end;

  if NeedRepaintPanel and (UnFocus = UnFocus0) then
    if IsAttack then
      PanelPaint
    else
    begin
      assert(result <> eMissionDone);
      CheckTerrainBtnVisible;
      FocusOnLoc(ToLoc, flRepaintPanel or flImmUpdate);
    end;

  if (result >= rExecuted) and CityCaptured and (MyMap[ToLoc] and fCity <> 0)
  then
    ZoomToCity(ToLoc, UnFocus < 0, chCaptured); // show captured city
end;

procedure TMainScreen.MoveOnScreen(ShowMove: TShowMove;
  Step0, Step1, nStep: integer; Restore: boolean = true);
var
  ToLoc, xFromLoc, yFromLoc, xToLoc, yToLoc, xFrom, yFrom, xTo, yTo, xMin, yMin,
    xRange, yRange, xw1, Step, xMoving, yMoving, SliceCount: integer;
  UnitInfo: TUnitInfo;
  Ticks0, Ticks: TDateTime;
begin
  Timer1.Enabled := false;
  Ticks0 := NowPrecise;
  with ShowMove do
  begin
    UnitInfo.Owner := Owner;
    UnitInfo.mix := mix;
    UnitInfo.Health := Health;
    UnitInfo.Job := jNone;
    UnitInfo.Flags := Flags;
    if Owner <> me then
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
        dec(xw1, G.lx);

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
      inc(xRange, xxt * 2);
      inc(yRange, yyt * 3);
    end;

    MainOffscreenPaint;
    NoMap.SetOutput(Buffer);
    NoMap.SetPaintBounds(0, 0, xRange, yRange);
    for Step := 0 to abs(Step1 - Step0) do
    begin
      BitBltCanvas(Buffer.Canvas, 0, 0, xRange, yRange,
        offscreen.Canvas, xMin, yMin);
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
          if not idle or (GameMode = cMovie) then
            Application.ProcessMessages;
          Sleep(1);
          inc(SliceCount)
        end;
        Ticks := NowPrecise;
      until (((Ticks - Ticks0) * 12) / OneMillisecond) >= MoveTime;
      Ticks0 := Ticks;
    end;
  end;
  if Restore then
  begin
    BitBltCanvas(Buffer.Canvas, 0, 0, xRange, yRange, offscreen.Canvas, xMin, yMin);
    PaintBufferToScreen(xMin, yMin, xRange, yRange);
  end;
  BlinkTime := -1;
  Timer1.Enabled := true;
end;

procedure TMainScreen.MoveToLoc(Loc: integer; CheckSuicide: boolean);
// path finder: move focused unit to loc, start multi-turn goto if too far
var
  uix, i, MoveOptions, NextLoc, MoveResult: integer;
  MoveAdviceData: TMoveAdviceData;
  StopReason: (None, Arrived, Dead, NoTime, EnemySpotted, MoveError);
begin
  if MyUn[UnFocus].Job > jNone then
    Server(sStartJob + jNone shl 4, me, UnFocus, nil^);
  if GetMoveAdvice(UnFocus, Loc, MoveAdviceData) >= rExecuted then
  begin
    uix := UnFocus;
    StopReason := None;
    repeat
      for i := 0 to MoveAdviceData.nStep - 1 do
      begin
        if i = MoveAdviceData.nStep - 1 then
          MoveOptions := muAutoNext
        else
          MoveOptions := 0;
        NextLoc := dLoc(MyUn[uix].Loc, MoveAdviceData.dx[i],
          MoveAdviceData.dy[i]);
        if (NextLoc = Loc) or (Loc = maNextCity) and
          (MyMap[NextLoc] and fCity <> 0) then
          StopReason := Arrived;
        if not CheckSuicide and (NextLoc = Loc) then
          MoveOptions := MoveOptions or muNoSuicideCheck;
        MoveResult := MoveUnit(MoveAdviceData.dx[i], MoveAdviceData.dy[i],
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
        (MyRO.Wonder[woShinkansen].EffectiveOwner <> me)) then
        StopReason := NoTime;
      if StopReason <> None then
        Break;
      if GetMoveAdvice(UnFocus, Loc, MoveAdviceData) < rExecuted then
      begin
        assert(false);
        Break;
      end;
    until false;

    case StopReason of
      None:
        assert(false);
      Arrived:
        MyUn[uix].Status := MyUn[uix].Status and ($FFFF - usGoto);
      Dead:
        if UnFocus < 0 then
          NextUnit(UnStartLoc, false);
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
          NextUnit(UnStartLoc, true);
        end;
      end;
    end;
  end;
end;

procedure TMainScreen.PanelBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
var
  i, xMouse, MouseLoc, p1: integer;
begin
  if GameMode = cMovie then
    exit;

  if Button = mbLeft then
  begin
    if (x >= xMini + 2) and (y >= yMini + 2) and (x < xMini + 2 + 2 * G.lx) and
      (y < yMini + 2 + G.ly) then
      if ssShift in Shift then
      begin
        with MainMap do
          xMouse := (xwMini + (x - (xMini + 2) + MapWidth div (xxt * 2) + G.lx)
            div 2) mod G.lx;
        MouseLoc := xMouse + G.lx * (y - (yMini + 2));
        if MyMap[MouseLoc] and fTerrain <> fUNKNOWN then
        begin
          p1 := MyRO.Territory[MouseLoc];
          if (p1 = me) or (p1 >= 0) and (MyRO.Treaty[p1] >= trNone) then
            NatStatDlg.ShowNewContent(wmPersistent, p1);
        end;
      end
      else
      begin
        if CityDlg.Visible then
          CityDlg.Close;
        if UnitStatDlg.Visible then
          UnitStatDlg.Close;
        Tracking := true;
        PanelBoxMouseMove(Sender, Shift + [ssLeft], x, y);
      end
    else if (ClientMode <> cEditMap) and (x >= ClientWidth - xPalace) and
      (y >= yPalace) and (x < ClientWidth - xPalace + xSizeBig) and
      (y < yPalace + ySizeBig) then
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
    (* else if (x>=xAdvisor-3) and (y>=yAdvisor-3)
      and (x<xAdvisor+16+3) and (y<yAdvisor+16+3) and HaveStrategyAdvice then
      AdviceBtnClick *)
    else if (x >= xTroop + 1) and (y >= yTroop + 1) and
      (x < xTroop + TrRow * TrPitch) and (y <= yTroop + 55) then
    begin
      i := (x - xTroop - 1) div TrPitch;
      if trix[i] >= 0 then
        if ClientMode = cEditMap then
        begin
          BrushType := trix[i];
          PanelPaint
        end
        else if (TroopLoc >= 0) then
          if MyMap[TroopLoc] and fOwned <> 0 then
          begin
            if ssShift in Shift then
              UnitStatDlg.ShowNewContent_OwnModel(wmPersistent,
                MyUn[trix[i]].mix)
            else if not supervising and (ClientMode < scContact) and
              (x - xTroop - 1 - i * TrPitch >= 60 - 20) and (y >= yTroop + 35)
              and ((MyUn[trix[i]].Job > jNone) or (MyUn[trix[i]].Status and
              (usStay or usRecover or usGoto) <> 0)) then
            begin // wake up
              MyUn[trix[i]].Status := MyUn[trix[i]].Status and
                ($FFFF - usStay - usRecover - usGoto - usEnhance) or usWaiting;
              if MyUn[trix[i]].Job > jNone then
                Server(sStartJob + jNone shl 4, me, trix[i], nil^);
              if (UnFocus < 0) and not CityDlg.Visible then
              begin
                SetUnFocus(trix[i]);
                SetTroopLoc(MyUn[trix[i]].Loc);
                FocusOnLoc(TroopLoc, flRepaintPanel)
              end
              else
              begin
                if CityDlg.Visible and (CityDlg.RestoreUnFocus < 0) then
                  CityDlg.RestoreUnFocus := trix[i];
                PanelPaint;
              end
            end
            else if (ClientMode < scContact) then
            begin
              if supervising then
                UnitStatDlg.ShowNewContent_OwnUnit(wmPersistent, trix[i])
              else if CityDlg.Visible then
              begin
                CityDlg.CloseAction := None;
                CityDlg.Close;
                SumCities(TaxSum, ScienceSum);
                SetUnFocus(trix[i]);
              end
              else
              begin
                DestinationMarkON := false;
                PaintDestination;
                UnFocus := trix[i];
                UnStartLoc := TroopLoc;
                BlinkTime := 0;
                BlinkON := false;
                PaintLoc(TroopLoc);
              end;
              if UnFocus >= 0 then
              begin
                UnitInfoBtn.Visible := true;
                UnitBtn.Visible := true;
                TurnComplete := false;
                EOT.ButtonIndex := eotGray;
              end;
              CheckTerrainBtnVisible;
              PanelPaint;
            end;
          end
          else if Server(sGetUnits, me, TroopLoc, TrCnt) >= rExecuted then
            if ssShift in Shift then
              UnitStatDlg.ShowNewContent_EnemyModel(wmPersistent,
                MyRO.EnemyUn[MyRO.nEnemyUn + trix[i]].emix) // model info
            else
              UnitStatDlg.ShowNewContent_EnemyUnit(wmPersistent,
                MyRO.nEnemyUn + trix[i]); // unit info
    end;
  end;
end;

procedure TMainScreen.SetTroopLoc(Loc: integer);
var
  trixFocus, uix, uixDefender: integer;
  Prio: boolean;
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
      Server(sGetDefender, me, TroopLoc, uixDefender);
      for Prio := true downto false do
        for uix := 0 to MyRO.nUn - 1 do
          if ((uix = uixDefender) = Prio) and (MyUn[uix].Loc = Loc) then
          begin
            if uix = UnFocus then
              trixFocus := TrCnt;
            inc(TrCnt);
          end;
    end
    else // count enemy units here
      Server(sGetUnits, me, Loc, TrCnt);
  if TrCnt = 0 then
    sb.Init(0, 1)
  else
  begin
    sb.Init((TrCnt + TrRow - 1) div TrRow - 1, 1);
    if (sb.Max >= sb.PageSize) and (trixFocus >= 0) then
      sb.Position := trixFocus div TrRow;
  end;
end;

(* procedure TMainScreen.ShowMoveHint(ToLoc: integer; Force: boolean = false);
  var
  Step,Loc,x0,y0,xs,ys: integer;
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
  if Server(sGetMoveAdvice,me,UnFocus,MoveAdvice)<rExecuted then
  ToLoc:=-1
  end;
  if (ToLoc=MoveHintToLoc) and not Force then exit;
  if (ToLoc<>MoveHintToLoc) and (MoveHintToLoc>=0) then
  begin invalidate; update end; // clear old hint from screen
  MoveHintToLoc:=ToLoc;
  if ToLoc<0 then exit;

  with canvas do
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
  inc(xs,G.lx*66);
  ys:=(y0-yw)*16;
  if Step=0 then moveto(xs+33,ys+16)
  else lineto(xs+33,ys+16);
  if Step<MoveAdvice.nStep then
  Loc:=dLoc(Loc,MoveAdvice.dx[Step],MoveAdvice.dy[Step]);
  end;
  Brush.Color:=$80C0FF;
  Info:=' '+inttostr(88)+' ';
  InfoSize:=TextExtent(Info);
  TextOut(xs+33-InfoSize.cx div 2, ys+16-InfoSize.cy div 2, Info);
  Brush.Style:=bsClear;
  end
  end; *)

procedure TMainScreen.SetDebugMap(p: integer);
begin
  MainMap.pDebugMap := p;
  MapOptions := MapOptions - [moLocCodes];
  mLocCodes.Checked := false;
  MapValid := false;
  MainOffscreenPaint;
end;

procedure TMainScreen.SetViewpoint(p: integer);
var
  i: Integer;
begin
  if supervising and (G.RO[0].Turn > 0) and
    ((p = 0) or (1 shl p and G.RO[0].Alive <> 0)) then
  begin
    ApplyToVisibleForms(faClose);
    ItsMeAgain(p);
    SumCities(TaxSum, ScienceSum);
    for i := 0 to MyRO.nModel - 1 do
      if not Assigned(Tribe[me].ModelPicture[i].HGr) then
        InitMyModel(i, True);

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

procedure TMainScreen.FormKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);

  procedure MenuClick_Check(Popup: TPopupMenu; Item: TMenuItem);
  begin
    InitPopup(Popup);
    if Item.Visible and Item.Enabled then
      Item.Click;
  end;

  procedure SetViewpointMe(p: Integer);
  begin
    if p = me then SetViewpoint(p)
      else SetViewpoint(p);
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
          Server($7F0,me,0,nil^);
          MapValid:=false;
          PaintAll;
          end;
          'B':
          begin // land mass
          dy:=0;
          for dx:=G.lx to G.lx*(G.ly-1)-1 do
          if MyMap[dx] and fTerrain>=fGrass then inc(dy);
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
  Loc0, Movement0: integer;
begin
  with MyUn[UnFocus] do
  begin
    DestinationMarkON := false;
    PaintDestination;
    Loc0 := Loc;
    Movement0 := Movement;
    if j0 < 0 then
      result := ProcessEnhancement(UnFocus, MyData.EnhancementJobs)
      // terrain enhancement
    else
      result := Server(sStartJob + j0 shl 4, me, UnFocus, nil^);
    if result >= rExecuted then
    begin
      if result = eDied then
        UnFocus := -1;
      PaintLoc(Loc0);
      if UnFocus >= 0 then
      begin
        if (j0 < 0) and (result <> eJobDone) then
          // multi-turn terrain enhancement
          Status := Status and ($FFFF - usStay - usRecover - usGoto) or
            usEnhance
        else
          Status := Status and
            ($FFFF - usStay - usRecover - usGoto - usEnhance);
        if (Job <> jNone) or (Movement0 < 100) then
        begin
          Status := Status and not usWaiting;
          NextUnit(UnStartLoc, true);
        end
        else
          PanelPaint;
      end
      else
        NextUnit(UnStartLoc, true);
    end;
  end;
  case result of
    eNoBridgeBuilding:
      SoundMessage(Phrases.Lookup('NOBB'), 'INVALID');
    eNoCityTerrain:
      SoundMessage(Phrases.Lookup('NOCITY'), 'INVALID');
    eTreaty:
      SoundMessage(Tribe[MyRO.Territory[Loc0]].TPhrase('PEACE_NOWORK'),
        'NOMOVE_TREATY');
  else
    if result < rExecuted then
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
      not(Server(sRemoveUnit - sExecute, me, UnFocus, nil^) = eUtilized) then
    begin
      SimpleMessage(Phrases2.Lookup('SHIP_UTILIZE'));
      // freight for colony ship is the only case in which the command is
      // available to player though not valid
      exit;
    end;
    if (Sender = mUtilize) and (Health < 100) then
      if SimpleQuery(mkYesNo, Phrases.Lookup('DAMAGED_UTILIZE'), '') <> mrOK
      then
        exit;
    Loc0 := Loc;
    CityOptimizer_BeforeRemoveUnit(UnFocus);
    if Server(sRemoveUnit, me, UnFocus, nil^) = eUtilized then
      Play('CITY_UTILIZE')
    else
      Play('DISBAND');
    CityOptimizer_AfterRemoveUnit;
    SetTroopLoc(Loc0);
    UpdateViews(true);
    DestinationMarkON := false;
    PaintDestination;
    UnFocus := -1;
    PaintLoc(Loc0);
    NextUnit(UnStartLoc, true);
  end;
end;

procedure TMainScreen.InitPopup(Popup: TPopupMenu);
var
  i, p1, Tile, Test: integer;
  NoSuper, extended, Multi, NeedSep, HaveCities: boolean;
  LastSep, m: TMenuItem;
  mox: ^TModel;
begin
  NoSuper := not supervising and (1 shl me and MyRO.Alive <> 0);
  HaveCities := false;
  for i := 0 to MyRO.nCity - 1 do
    if MyCity[i].Loc >= 0 then
    begin
      HaveCities := true;
      Break;
    end;
  if Popup = GamePopup then
  begin
    mTechTree.Visible := ClientMode <> cEditMap;
    mResign.Enabled := supervising or (me = 0) and (ClientMode < scContact);
    mRandomMap.Visible := (ClientMode = cEditMap) and
      (Server(sMapGeneratorRequest, me, 0, nil^) = eOK);
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
          mSoundOff.Checked := true;
        smOn:
          mSoundOn.Checked := true;
        smOnAlt:
          mSoundOnAlt.Checked := true;
      end;

      for i := 0 to nTestFlags - 1 do
        mManip[i].Checked := MyRO.TestFlags and (1 shl i) <> 0;
      mManip.Enabled := supervising or (me = 0);

      Multi := false;
      for p1 := 1 to nPl - 1 do
        if G.RO[p1] <> nil then
          Multi := true;
      mEnemyMovement.Visible := not Multi;
    end;
    mMacro.Visible := NoSuper and (ClientMode < scContact);
    if NoSuper and (ClientMode < scContact) then
    begin
      mCityTypes.Enabled := false;
      // check if city types already usefull:
      if MyRO.nCity > 0 then
        for i := nWonder to nImp - 1 do
          if (i <> imTrGoods) and (Imp[i].Kind = ikCommon) and
            (Imp[i].Preq <> preNA) and
            ((Imp[i].Preq = preNone) or (MyRO.Tech[Imp[i].Preq] >= tsApplicable))
          then
          begin
            mCityTypes.Enabled := true;
            Break
          end;
    end;
    mViewpoint.Visible := (ClientMode <> cEditMap) and supervising;
    mViewpoint.Enabled := G.RO[0].Turn > 0;
    if supervising then
    begin
      EmptyMenu(mViewpoint);
      for p1 := 0 to nPl - 1 do
        if (p1 = 0) or (1 shl p1 and G.RO[0].Alive <> 0) then
        begin
          m := TMenuItem.Create(mViewpoint);
          if p1 = 0 then
            m.Caption := Phrases.Lookup('SUPER')
          else
            m.Caption := Tribe[p1].TString(Phrases2.Lookup('BELONG'));
          m.Tag := p1;
          m.OnClick := ViewpointClick;
          if p1 < 10 then
            m.ShortCut := ShortCut(48 + p1, [ssCtrl]);
          m.RadioItem := true;
          if p1 = me then
            m.Checked := true;
          mViewpoint.Add(m);
        end
    end;
    mDebugMap.Visible := (ClientMode <> cEditMap) and supervising;
    if supervising then
    begin
      EmptyMenu(mDebugMap);
      for p1 := 0 to nPl - 1 do
        if (p1 = 0) or (1 shl p1 and G.RO[0].Alive <> 0) then
        begin
          m := TMenuItem.Create(mDebugMap);
          if p1 = 0 then
            m.Caption := Phrases2.Lookup('MENU_DEBUGMAPOFF')
          else
            m.Caption := Tribe[p1].TString(Phrases2.Lookup('BELONG'));
          if p1 = 0 then
            m.Tag := -1
          else
            m.Tag := p1;
          m.OnClick := DebugMapClick;
          if p1 < 10 then
            m.ShortCut := ShortCut(48 + p1, [ssAlt]);
          m.RadioItem := true;
          if m.Tag = MainMap.pDebugMap then
            m.Checked := true;
          mDebugMap.Add(m);
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
    mCityStat.Visible := 1 shl me and MyRO.Alive <> 0;
    mCityStat.Enabled := HaveCities;
    mScienceStat.Visible := true;
    mScienceStat.Enabled := not NoSuper or (MyRO.ResearchTech >= 0) or
      (MyRO.Happened and phTech <> 0) or (MyRO.Happened and phGameEnd <> 0)
    // no researchtech in case just completed
      or (MyRO.TestFlags and (tfAllTechs or tfUncover or tfAllContact) <> 0);
    mEUnitStat.Enabled := MyRO.nEnemyModel > 0;
    { mWonders.Enabled:= false;
      for i:=0 to nWonder - 1 do if MyRO.Wonder[i].CityID <> WonderNotBuiltYet then
      mWonders.Enabled:=true; }
    mDiagram.Enabled := MyRO.Turn >= 2;
    mShips.Enabled := false;
    for p1 := 0 to nPl - 1 do
      if MyRO.Ship[p1].Parts[spComp] + MyRO.Ship[p1].Parts[spPow] +
        MyRO.Ship[p1].Parts[spHab] > 0 then
        mShips.Enabled := true;
  end
  else if Popup = UnitPopup then
  begin
    mox := @MyModel[MyUn[UnFocus].mix];
    Tile := MyMap[MyUn[UnFocus].Loc];
    extended := Tile and fCity = 0;
    if extended then
    begin
      mCity.Caption := Phrases.Lookup('BTN_FOUND');
      mHome.Caption := Phrases.Lookup('BTN_MOVEHOME')
    end
    else
    begin
      mCity.Caption := Phrases.Lookup('BTN_ADD');
      mHome.Caption := Phrases.Lookup('BTN_SETHOME')
    end;

    extended := extended and ((mox.Kind = mkSettler) or (mox.Kind = mkSlaves)
      and (MyRO.Wonder[woPyramids].EffectiveOwner >= 0)) and
      (MyUn[UnFocus].Master < 0) and (Tile and fDeadLands = 0);
    if (mox.Kind = mkFreight) and (Tile and fCity <> 0) and
      not Phrases2FallenBackToEnglish or
      (Server(sRemoveUnit - sExecute, me, UnFocus, nil^) = eUtilized) then
    begin
      mDisband.Visible := false;
      mUtilize.Visible := true;
      if mox.Kind = mkFreight then
        mUtilize.Caption := Phrases.Lookup('UTILIZE')
      else
        mUtilize.Caption := Phrases.Lookup('INTEGRATE')
    end
    else
    begin
      mDisband.Visible := true;
      mUtilize.Visible := false
    end;
    mGoOn.Visible := MyUn[UnFocus].Status and (usGoto or usWaiting) = usGoto or
      usWaiting;
    mHome.Visible := HaveCities;
    mRecover.Visible := (MyUn[UnFocus].Health < 100) and
      (Tile and fTerrain >= fGrass) and
      ((MyRO.Wonder[woGardens].EffectiveOwner = me) or
      (Tile and fTerrain <> fArctic) and (Tile and fTerrain <> fDesert)) and
      not((mox.Domain = dAir) and (Tile and fCity = 0) and
      (Tile and fTerImp <> tiBase));
    mStay.Visible := not((mox.Domain = dAir) and (Tile and fCity = 0) and
      (Tile and fTerImp <> tiBase));
    mCity.Visible := extended and (mox.Kind = mkSettler) or
      (Tile and fCity <> 0) and ((mox.Kind in [mkSettler, mkSlaves]) or
      (MyUn[UnFocus].Flags and unConscripts <> 0));
    mPillage.Visible := (Tile and (fRoad or fRR or fCanal or fTerImp) <> 0) and
      (MyUn[UnFocus].Master < 0) and (mox.Domain = dGround);
    mCancel.Visible := (MyUn[UnFocus].Job > jNone) or
      (MyUn[UnFocus].Status and (usRecover or usGoto) <> 0);

    Test := Server(sLoadUnit - sExecute, me, UnFocus, nil^);
    mLoad.Visible := (Test >= rExecuted) or (Test = eNoTime_Load);
    mUnload.Visible := (MyUn[UnFocus].Master >= 0) or
      (MyUn[UnFocus].TroopLoad + MyUn[UnFocus].AirLoad > 0);
    mSelectTransport.Visible := Server(sSelectTransport - sExecute, me, UnFocus,
      nil^) >= rExecuted;
  end
  else { if Popup=TerrainPopup then }
  begin
    mox := @MyModel[MyUn[UnFocus].mix];
    Tile := MyMap[MyUn[UnFocus].Loc];
    extended := Tile and fCity = 0;

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

    extended := extended and ((mox.Kind = mkSettler) or (mox.Kind = mkSlaves)
      and (MyRO.Wonder[woPyramids].EffectiveOwner >= 0)) and
      (MyUn[UnFocus].Master < 0);
    if extended then
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
      for i := 0 to Popup.Items.Count - 1 do
        Popup.Items[i].Visible := false;
    end;
  end;

  // set menu seperators
  LastSep := nil;
  NeedSep := false;
  for i := 0 to Popup.Items.Count - 1 do
    if Popup.Items[i].Caption = '-' then
    begin
      Popup.Items[i].Visible := NeedSep;
      if NeedSep then
        LastSep := Popup.Items[i];
      NeedSep := false
    end
    else if Popup.Items[i].Visible then
      NeedSep := true;
  if (LastSep <> nil) and not NeedSep then
    LastSep.Visible := false
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

procedure TMainScreen.CityClosed(Activateuix: integer; StepFocus: boolean;
  SelectFocus: boolean);
begin
  if supervising then
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
      NextUnit(TroopLoc, true)
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
  x, y: integer);
var
  xCentre, yCentre: integer;
begin
  if Tracking and (ssLeft in Shift) then
  with MainMap do begin
    if (x >= xMini + 2) and (y >= yMini + 2) and (x < xMini + 2 + 2 * G.lx) and
      (y < yMini + 2 + G.ly) then
    begin
      xCentre := (xwMini + (x - xMini - 2) div 2 + G.lx div 2 +
        MapWidth div (xxt * 4)) mod G.lx;
      yCentre := (y - yMini - 2);
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
        Frame(Buffer.Canvas, x - xMini - 2 - MapWidth div (xxt * 2), 0,
          x - xMini - 2 + MapWidth div (xxt * 2) - 1, G.ly - 1,
          MainTexture.ColorMark, MainTexture.ColorMark)
      else
        Frame(Buffer.Canvas, x - xMini - 2 - MapWidth div (xxt * 2), yw,
          x - xMini - 2 + MapWidth div (xxt * 2) - 1, yw + MapHeight div yyt -
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
    Tracking := false;
end;

procedure TMainScreen.PanelBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
begin
  if Tracking then
  begin
    Tracking := false;
    xwMini := xw;
    ywMini := yw;
    MiniMapPaint;
    PanelPaint;
  end;
end;

procedure TMainScreen.MapBoxMouseMove(Sender: TObject; Shift: TShiftState;
  x, y: integer);
var
  MouseLoc: integer;
begin
  xMouse := x;
  yMouse := y;
  if (ClientMode = cEditMap) and (ssLeft in Shift) and not Tracking then
  begin
    MouseLoc := LocationOfScreenPixel(x, y);
    if MouseLoc <> BrushLoc then
      MapBoxMouseDown(nil, mbLeft, Shift, x, y);
  end
  (* else if idle and (UnFocus>=0) then
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
  MapValid := false;
  PaintAllMaps;
end;

procedure TMainScreen.mNamesClick(Sender: TObject);
var
  p1: integer;
begin
  mNames.Checked := not mNames.Checked;
  GenerateNames := mNames.Checked;
  for p1 := 0 to nPl - 1 do
    if Tribe[p1] <> nil then
      if GenerateNames then
        Tribe[p1].NumberName := -1
      else
        Tribe[p1].NumberName := p1;
  MapValid := false;
  PaintAll;
end;

function TMainScreen.IsPanelPixel(x, y: integer): boolean;
begin
  result := (y >= TopBarHeight + MapHeight) or (y >= ClientHeight - PanelHeight)
    and ((x < xMidPanel) or (x >= xRightPanel));
end;

procedure TMainScreen.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
begin
  if idle then
    if (x < 40) and (y < 40) then
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
    else if IsPanelPixel(x, y) then
      PanelBoxMouseDown(Sender, Button, Shift, x,
        y - (ClientHeight - PanelHeight))
    else if (y >= TopBarHeight) and (x >= MapOffset) and
      (x < MapOffset + MapWidth) then
      MapBoxMouseDown(Sender, Button, Shift, x - MapOffset, y - TopBarHeight)
end;

procedure TMainScreen.FormMouseMove(Sender: TObject; Shift: TShiftState;
  x, y: integer);
begin
  if idle then
    if IsPanelPixel(x, y) then
      PanelBoxMouseMove(Sender, Shift, x, y - (ClientHeight - PanelHeight))
    else if (y >= TopBarHeight) and (x >= MapOffset) and
      (x < MapOffset + MapWidth) then
      MapBoxMouseMove(Sender, Shift, x - MapOffset, y - TopBarHeight);
end;

procedure TMainScreen.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
begin
  if idle then
    PanelBoxMouseUp(Sender, Button, Shift, x, y - (ClientHeight - PanelHeight));
end;

procedure TMainScreen.FormPaint(Sender: TObject);
begin
  MainOffscreenPaint;
  if (MapOffset > 0) or (MapOffset + MapWidth < ClientWidth) then
    with Canvas do
    begin // pillarbox, make left and right border black
      if me < 0 then
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
    offscreen.Canvas, 0, 0);
  BitBltCanvas(Canvas, 0, 0, ClientWidth, TopBarHeight, TopBar.Canvas,
    0, 0);
  if xMidPanel > MapOffset then
    BitBltCanvas(Canvas, xMidPanel, TopBarHeight + MapHeight - overlap,
      ClientWidth div 2 - xMidPanel, overlap, offscreen.Canvas,
      xMidPanel - MapOffset, MapHeight - overlap)
  else
    BitBltCanvas(Canvas, MapOffset, TopBarHeight + MapHeight - overlap,
      ClientWidth div 2 - MapOffset, overlap, offscreen.Canvas, 0,
      MapHeight - overlap);
  if xRightPanel < MapOffset + MapWidth then
    BitBltCanvas(Canvas, ClientWidth div 2, TopBarHeight + MapHeight - overlap,
      xRightPanel - ClientWidth div 2, overlap, offscreen.Canvas,
      ClientWidth div 2 - MapOffset, MapHeight - overlap)
  else
    BitBltCanvas(Canvas, ClientWidth div 2, TopBarHeight + MapHeight - overlap,
      MapOffset + MapWidth - ClientWidth div 2, overlap,
      offscreen.Canvas, ClientWidth div 2 - MapOffset,
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

procedure TMainScreen.RectInvalidate(Left, Top, Rigth, Bottom: integer);
var
  r0: HRgn;
begin
  r0 := CreateRectRgn(Left, Top, Rigth, Bottom);
  InvalidateRgn(Handle, r0, false);
  DeleteObject(r0);
end;

procedure TMainScreen.SmartRectInvalidate(Left, Top, Rigth, Bottom: integer);
var
  i: integer;
  r0, r1: HRgn;
begin
  r0 := CreateRectRgn(Left, Top, Rigth, Bottom);
  for i := 0 to ControlCount - 1 do
    if not(Controls[i] is TArea) and Controls[i].Visible then
    begin
      with Controls[i].BoundsRect do
        r1 := CreateRectRgn(Left, Top, Right, Bottom);
      CombineRgn(r0, r0, r1, RGN_DIFF);
      DeleteObject(r1);
    end;
  InvalidateRgn(Handle, r0, false);
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
  Timer1.Enabled := false;
end;

procedure TMainScreen.Radio(Sender: TObject);
begin
  TMenuItem(Sender).Checked := true;
end;

procedure TMainScreen.mManipClick(Sender: TObject);
var
  Flag: integer;
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
    if not supervising then
    begin
      if Flag = tfUncover then
      begin
        MapValid := false;
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
    MapValid := false;
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
  MapValid := false;
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
  MapValid := false;
  PaintAllMaps;
end;

procedure TMainScreen.FormKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if idle and (Key = VK_APPS) then
  begin
    InitPopup(GamePopup);
    if FullScreen then
      GamePopup.Popup(Left, Top + TopBarHeight - 1)
    else
      GamePopup.Popup(Left + 4, Top + GetSystemMetrics(SM_CYCAPTION) + 4 +
        TopBarHeight - 1);
    exit;
  end; // windows menu button calls game menu
end;

procedure TMainScreen.CreateUnitClick(Sender: TObject);
var
  p1, mix: integer;
begin
  p1 := TComponent(Sender).Tag shr 16;
  mix := TComponent(Sender).Tag and $FFFF;
  if Server(sCreateUnit + p1 shl 4, me, mix, EditLoc) >= rExecuted then
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
  OldAdviceLoc: integer;
  begin
  DestinationMarkON:=false;
  PaintDestination;
  AdvisorDlg.GiveStrategyAdvice;
  OldAdviceLoc:=MainMap.AdviceLoc;
  MainMap.AdviceLoc:=-1;
  PaintLoc(OldAdviceLoc);
  end; }

{ procedure TMainScreen.SetAdviceLoc(Loc: integer; AvoidRect: TRect);
  var
  OldAdviceLoc,x,y: integer;
  begin
  if Loc<>MainMap.AdviceLoc then
  begin
  if Loc>=0 then
  begin // center
  y:=Loc div G.lx;
  x:=(Loc+G.lx - AvoidRect.Right div (2*66)) mod G.lx;
  Centre(y*G.lx+x);
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
  i, j: integer;
begin
  if soTellAI in OptionChecked then OptionChecked := [soTellAI]
    else OptionChecked := [];
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TMenuItem then
      for j := 0 to Length(SaveOption) - 1 do
        if TMenuItem(Components[i]).Checked and
          (TMenuItem(Components[i]).Tag = SaveOption[j]) then
          OptionChecked := OptionChecked + [TSaveOption(j)];
end;

procedure TMainScreen.SaveSettings;
var
  Reg: TRegistry;
begin
  SaveMenuItemsState;

  Reg := TRegistry.Create;
  with Reg do
  try
    OpenKey(AppRegistryKey, true);
    WriteInteger('TileSize', Integer(MainMap.TileSize));
    WriteInteger('OptionChecked', Integer(OptionChecked));
    WriteInteger('MapOptionChecked', Integer(MapOptionChecked));
    WriteInteger('CityReport', integer(CityRepMask));
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

initialization

end.

