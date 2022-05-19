{$INCLUDE Switches.inc}
unit CityScreen;

interface

uses
  {$IFDEF UNIX}LMessages,{$ENDIF}
  Protocol, ClientTools, Term, ScreenTools, IsoEngine, BaseWin,
  LCLIntf, LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  ButtonA, ButtonC, Area, GraphType, UTexture;

const
  WM_PLAYSOUND = WM_USER;

type
  TCityCloseAction = (None, RestoreFocus, StepFocus);
  TSmallMapMode = (smSupportedUnits, smImprovements);

  TCityDlg = class(TBufferedDrawDlg)
    Timer1: TTimer;
    CloseBtn: TButtonA;
    PrevCityBtn: TButtonC;
    NextCityBtn: TButtonC;
    PageUpBtn: TButtonC;
    PageDownBtn: TButtonC;
    BuyBtn: TButtonC;
    ProjectArea: TArea;
    PrimacyArea: TArea;
    Imp2Area: TArea;
    Imp4Area: TArea;
    Imp0Area: TArea;
    Imp3Area: TArea;
    Imp5Area: TArea;
    Imp1Area: TArea;
    Pop0Area: TArea;
    Pop1Area: TArea;
    SupportArea: TArea;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BuyClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure NextCityBtnClick(Sender: TObject);
    procedure PrevCityBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    // procedure AdviceBtnClick(Sender: TObject);
    procedure PageUpBtnClick(Sender: TObject);
    procedure PageDownBtnClick(Sender: TObject);
  private
    C: TCity;
    Report: TCityReportNew;
    cOwner: Integer;
    cGov: Integer;
    emix: Integer; { enemy model index of produced unit }
    cix: Integer;
    cLoc: Integer;
    SmallMapMode: TSmallMapMode;
    ZoomArea: Integer;
    Page: Integer;
    PageCount: Integer;
    BlinkTime: Integer;
    OpenSoundEvent: Integer;
    SizeClass: Integer;
    AgePrepared: Integer;
    Optimize_cixTileChange: Integer;
    Optimize_TilesBeforeChange: Integer;
    Happened: Cardinal;
    imix: array [0 .. 15] of Integer;
    CityAreaInfo: TCityAreaInfo;
    AreaMap: TIsoMap;
    NoMap: TIsoMap;
    CityMapTemplate: TBitmap;
    SmallCityMapTemplate: TBitmap;
    Back: TBitmap;
    SmallCityMap: TBitmap;
    ZoomCityMap: TBitmap;
    Template: TBitmap;
    IsPort: Boolean;
    ProdHint: Boolean;
    AllowChange: Boolean;
    RedTex: TTexture;
    BarTex: TTexture;
    procedure InitSmallCityMap;
    procedure InitZoomCityMap;
    procedure ChooseProject;
    procedure ChangeCity(D: Integer);
    procedure ChangeResourceWeights(iResourceWeights: Integer);
    procedure OnPlaySound(var Msg: TMessage); message WM_PLAYSOUND;
  public
    RestoreUnFocus: Integer;
    CloseAction: TCityCloseAction;
    procedure OffscreenPaint; override;
    procedure ShowNewContent(NewMode: TWindowMode; Loc: Integer; ShowEvent: Cardinal);
    procedure Reset;
    procedure CheckAge;
  end;

var
  CityDlg: TCityDlg;


implementation

uses
  Select, Messg, MessgEx, Help, Tribes, Directories, Math, Sound;

{$R *.lfm}

const
  wBar = 106;
  xDiv = 400;
  xService = 296;
  xmArea = 197;
  ymArea = 170;
  xView = 326;
  yView = 275;
  dxBar = wBar + 12;
  dyBar = 39;
  xHapp = 404;
  yHapp = 9;
  xFood = 404;
  yFood = yHapp + 3 * dyBar + 6;
  xProd = 404;
  yProd = yFood + 3 * dyBar + 6;
  xTrade = 404;
  yTrade = yProd + 2 * dyBar + 22;
  xPoll = xmArea - 186;
  yPoll = ymArea + 64;
  xmOpt = 40;
  ymOpt = ymArea + 96 + 34;
  xSmallMap = 271;
  ySmallMap = 339;
  wSmallMap = 98;
  hSmallMap = 74;
  xSupport = xSmallMap;
  ySupport = ySmallMap + hSmallMap + 2;
  wSupport = 64;
  hSupport = 18;
  xZoomMap = 34;
  yZoomMap = 338;
  wZoomMap = 228;
  hZoomMap = 124;
  wZoomEnvironment = 68;

  ImpPosition: array [28 .. nImp - 1] of Integer = (
    -1, // imTrGoods
    21, // imBarracks
    6, // imGranary
    1, // imTemple
    7, // imMarket
    14, // imLibrary
    8, // imCourt
    18, // imWalls
    10, // imAqueduct
    11, // imBank
    5, // imCathedral
    13, // imUniversity
    29, // imHarbor
    2, // imTheater
    24, // imFactory
    25, // imMfgPlant
    28, // imRecycling
    27, // imPower
    27, // imHydro
    27, // imNuclear
    26, // imPlatform
    8, // imTownHall
    10, // imSewer
    3, // imSupermarket
    17, // imHighways
    15, // imResLab
    19, // imMissileBat
    23, // imCoastalFort
    22, // imAirport
    20, // imDockyard
    8, // imPalace
    -1, // imGrWall
    4, // imColosseum
    16, // imObservatory
    21, // imMilAcademy
    -1, // imBunker
    -1, // imAlgae
    9, // imStockEx
    -1, // imSpacePort
    -1, // imShipComp
    -1, // imShipPow
    -1); // imShipHab

var
  ImpSorted: array [0 .. nImp - 1] of Integer;

procedure TCityDlg.FormCreate(Sender: TObject);
begin
  inherited;
  RedTex := TTexture.Create;
  BarTex := TTexture.Create;
  NoMap := TIsoMap.Create;
  AreaMap := TIsoMap.Create;
  AreaMap.SetOutput(Offscreen);
  AreaMap.SetPaintBounds(xmArea - 192, ymArea - 96 - 32, xmArea + 192,
    ymArea + 96);
  SmallMapMode := smImprovements;
  ZoomArea := 1;
  ProdHint := False;
  RestoreUnFocus := -1;
  OpenSoundEvent := -1;
  AgePrepared := -2;
  Optimize_cixTileChange := -1;
  InitButtons;
  // InitWindowRegion;
  CloseBtn.Caption := Phrases.Lookup('BTN_OK');
  BuyBtn.Hint := Phrases.Lookup('BTN_BUY');
  if not Phrases2FallenBackToEnglish then
    SupportArea.Hint := Phrases2.Lookup('TIP_SUPUNITS')
  else
    SupportArea.Hint := Phrases.Lookup('SUPUNITS');
  if not Phrases2FallenBackToEnglish then
  begin
    Pop0Area.Hint := Phrases2.Lookup('TIP_WORKING');
    Pop1Area.Hint := Phrases2.Lookup('TIP_CIVIL');
    PrimacyArea.Hint := Phrases2.Lookup('TIP_PRIMACY');
    ProjectArea.Hint := Phrases2.Lookup('TIP_PROJECT');
  end;

  Back := TBitmap.Create;
  Back.PixelFormat := pf24bit;
  Back.SetSize(Width, Height);
  Back.Canvas.FillRect(0, 0, Back.Width, Back.Height);
  Template := TBitmap.Create;
  Template.PixelFormat := pf24bit;
  LoadGraphicFile(Template, GetGraphicsDir + DirectorySeparator + 'City.png',
    [gfNoGamma]);
  CityMapTemplate := TBitmap.Create;
  CityMapTemplate.PixelFormat := pf24bit;
  LoadGraphicFile(CityMapTemplate, GetGraphicsDir + DirectorySeparator + 'BigCityMap.png',
    [gfNoGamma]);
  SmallCityMapTemplate := TBitmap.Create;
  SmallCityMapTemplate.PixelFormat := pf24bit;
  LoadGraphicFile(SmallCityMapTemplate, GetGraphicsDir + DirectorySeparator + 'SmallCityMap.png',
    [gfNoGamma]);
  SmallCityMap := TBitmap.Create;
  SmallCityMap.PixelFormat := pf24bit;
  SmallCityMap.SetSize(98, 74);
  SmallCityMap.Canvas.FillRect(0, 0, SmallCityMap.Width, SmallCityMap.Height);
  ZoomCityMap := TBitmap.Create;
  ZoomCityMap.PixelFormat := pf24bit;
  ZoomCityMap.SetSize(228, 124);
  ZoomCityMap.Canvas.FillRect(0, 0, ZoomCityMap.Width, ZoomCityMap.Height);
end;

procedure TCityDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(NoMap);
  FreeAndNil(AreaMap);
  FreeAndNil(SmallCityMap);
  FreeAndNil(ZoomCityMap);
  FreeAndNil(SmallCityMapTemplate);
  FreeAndNil(CityMapTemplate);
  FreeAndNil(Template);
  FreeAndNil(Back);
  FreeAndNil(RedTex);
  FreeAndNil(BarTex);
end;

procedure TCityDlg.Reset;
begin
  SmallMapMode := smImprovements;
  ZoomArea := 1;
end;

procedure TCityDlg.CheckAge;
begin
  if MainTexture.Age <> AgePrepared then begin
    AgePrepared := MainTexture.Age;

    UnshareBitmap(Back);
    BitBltCanvas(Back.Canvas, 0, 0, ClientWidth, ClientHeight,
      MainTexture.Image.Canvas, 0, 0);
    ImageOp_B(Back, Template, 0, 0, 0, 0, ClientWidth, ClientHeight);
  end;
end;

procedure TCityDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TCityDlg.InitSmallCityMap;
var
  I, iix, cli1, Color0, Color1, Color2: Integer;
begin
  if cix >= 0 then
    C := MyCity[cix];
  case MyMap[cLoc] and fTerrain of
    fPrairie: cli1 := cliPrairie;
    fHills: cli1 := cliHills;
    fTundra: cli1 := cliTundra;
  else
    cli1 := cliPlains;
  end;
  Color0 := Colors.Canvas.Pixels[clkAge0 + Age, cliRoad];
  Color1 := Colors.Canvas.Pixels[clkCity, cli1];
  Color2 := Colors.Canvas.Pixels[clkAge0 + Age, cliHouse];
  SmallCityMap.Canvas.FillRect(0, 0, SmallCityMap.Width, SmallCityMap.Height);
  BitBltCanvas(SmallCityMap.Canvas, 0, 0, 83, hSmallMap,
    SmallCityMapTemplate.Canvas, 83 * SizeClass, 0);
  if IsPort then
  begin
    BitBltCanvas(SmallCityMap.Canvas, 83, 0, 15, hSmallMap,
      SmallCityMapTemplate.Canvas, 332 + 15, 0);
    ImageOp_CCC(SmallCityMap, 0, 0, 83, hSmallMap, Color0, Color1, Color2);
    Color2 := Colors.Canvas.Pixels[clkCity, cliWater];
    ImageOp_CCC(SmallCityMap, 83, 0, 15, hSmallMap, Color0, Color1, Color2);
  end
  else
  begin
    BitBltCanvas(SmallCityMap.Canvas, 83, 0, 15, hSmallMap,
      SmallCityMapTemplate.Canvas, 332, 0);
    ImageOp_CCC(SmallCityMap, 0, 0, wSmallMap, hSmallMap, Color0,
      Color1, Color2);
  end;

  with SmallCityMap.Canvas do
  begin
    Brush.Color := ScreenTools.Colors.Canvas.Pixels[clkAge0 + Age, cliImp];
    for I := 0 to 29 do
    begin
      for iix := nWonder to nImp - 1 do
        if (ImpPosition[iix] = I) and (C.Built[iix] > 0) then
        begin
          FillRect(Rect(5 + 16 * (I mod 3) + 48 * (I div 18),
            3 + 12 * (I mod 18 div 3), 13 + 16 * (I mod 3) + 48 * (I div 18),
            11 + 12 * (I mod 18 div 3)));
          Break;
        end;
    end;
    I := 30;
    for iix := 0 to nImp do
      if (C.Built[iix] > 0) and ((iix < nWonder) or (ImpPosition[iix] < 0)) then
      begin
        FillRect(Rect(5 + 16 * (I mod 3) + 48 * (I div 18),
          3 + 12 * (I mod 18 div 3), 13 + 16 * (I mod 3) + 48 * (I div 18),
          11 + 12 * (I mod 18 div 3)));
        Inc(I);
        if I = 36 then
          Break; // area is full
      end;
    if C.Project and cpImp <> 0 then
    begin
      iix := C.Project and cpIndex;
      if iix <> imTrGoods then
      begin
        if (iix >= nWonder) and (ImpPosition[iix] >= 0) then
          I := ImpPosition[iix];
        if I < 36 then
        begin
          Brush.Color := ScreenTools.Colors.Canvas.Pixels[clkAge0 + Age, cliImpProject];
          FillRect(Rect(5 + 16 * (I mod 3) + 48 * (I div 18),
            3 + 12 * (I mod 18 div 3), 13 + 16 * (I mod 3) + 48 * (I div 18),
            11 + 12 * (I mod 18 div 3)));
        end;
      end;
    end;
    Brush.style := bsClear;
  end;
end;

procedure TCityDlg.InitZoomCityMap;
begin
  UnshareBitmap(ZoomCityMap);
  BitBltCanvas(ZoomCityMap.Canvas, 0, 0, wZoomMap, hZoomMap,
    Back.Canvas, xZoomMap, yZoomMap);
  if SmallMapMode = smImprovements then begin
    if ZoomArea < 3 then begin
      ImageOp_B(ZoomCityMap, CityMapTemplate, 0, 0, 376 * SizeClass,
        112 * ZoomArea, wZoomMap, hZoomMap);
    end else begin
      ImageOp_B(ZoomCityMap, CityMapTemplate, 0, 0, 376 * SizeClass + 216,
        112 * (ZoomArea - 3), wZoomMap - wZoomEnvironment, hZoomMap);
      ImageOp_B(ZoomCityMap, CityMapTemplate, wZoomMap - wZoomEnvironment, 0,
        1504 + wZoomEnvironment * Byte(IsPort), 112 * (ZoomArea - 3),
        wZoomEnvironment, hZoomMap);
    end;
  end;
end;

procedure TCityDlg.OffscreenPaint;

  procedure FillBar(X, Y, Pos, Growth, Max, Kind: Integer;
    IndicateComplete: Boolean);
  begin
    BarTex.Assign(MainTexture);
    if Kind = 3 then begin
      BarTex.ColorBevelLight := HGrSystem.Data.Canvas.Pixels[104, 36];
      BarTex.ColorBevelShade := BarTex.ColorBevelLight;
    end;
    PaintRelativeProgressBar(Offscreen.Canvas, Kind, X - 3, Y, wBar - 4, Pos,
      Growth, Max, IndicateComplete, BarTex);
  end;

  procedure PaintResources(X, Y, Loc: Integer; Add4Happy: Boolean);
  var
    D, I, Total, xGr, yGr: Integer;
    TileInfo: TTileInfo;
    rare: Boolean;
  begin
    with AreaMap do begin
    if Server(sGetCityTileInfo, Me, Loc, TileInfo) <> eOk then
    begin
      Assert(cix < 0);
      Exit
    end;
    Total := TileInfo.Food + TileInfo.Prod + TileInfo.Trade;
    rare := MyMap[Loc] and $06000000 > 0;
    if rare then
      Inc(Total);
    if Add4Happy then
      Inc(Total, 4);
    if Total > 1 then
      D := (xxt - 11) div (Total - 1);
    if D < 1 then
      D := 1;
    if D > 4 then
      D := 4;
    for I := 0 to Total - 1 do
    begin
      yGr := 115;
      if Add4Happy and (I >= Total - 4) then
      begin
        xGr := 132;
        yGr := 126
      end
      else if rare and (I = Total - 1) then
        xGr := 66 + 110
      else if I >= TileInfo.Food + TileInfo.Prod then
        xGr := 66 + 44
      else if I >= TileInfo.Prod then
        xGr := 66
      else
        xGr := 66 + 22;
      Sprite(Offscreen, HGrSystem, X + xxt - 5 + D * (2 * I + 1 - Total),
        Y + yyt - 5, 10, 10, xGr, yGr);
    end;
    end;
  end;
var
  Line, MessageCount: Integer;

  procedure CheckMessage(Flag: Integer);
  var
    I, Test: Integer;
    S: string;
  begin
    if Happened and Flag <> 0 then
    begin
      I := 0;
      Test := 1;
      while Test < Flag do
      begin
        Inc(I);
        Inc(Test, Test);
      end;

      if AllowChange and (Sounds <> nil) and (OpenSoundEvent = -1) then
      begin
        S := CityEventSoundItem[I];
        if S <> '' then
          S := Sounds.Lookup(S);
        if (Flag = chProduction) or (S <> '') and (S[1] <> '*') and (S[1] <> '[')
        then
          OpenSoundEvent := I;
      end;

      S := CityEventName(I);
      { if Flag=chNoGrowthWarning then
        if C.Built[imAqueduct]=0 then
        S:=Format(S,[Phrases.Lookup('IMPROVEMENTS',imAqueduct)])
        else S:=Format(S,[Phrases.Lookup('IMPROVEMENTS',imSewer)]); }
      RisedTextOut(Offscreen.Canvas, xmOpt + 40, ymOpt - 1 - 8 * MessageCount +
        16 * Line, S);
      Inc(Line);
    end;
  end;

var
  X, Y, xGr, I, J, iix, D, dx, dy, PrCost, Cnt, Loc1, FreeSupp, Paintiix,
    HappyGain, OptiType, rx, ry, TrueFood, TrueProd, TruePoll: Integer;
  av: Integer;
  PrName, S: string;
  UnitInfo: TUnitInfo;
  UnitReport: TUnitReport;
  IsCityAlive, CanGrow: Boolean;
begin
  inherited;
  if cix >= 0 then
    C := MyCity[cix];
  Report.HypoTiles := -1;
  Report.HypoTaxRate := -1;
  Report.HypoLuxuryRate := -1;
  if cix >= 0 then
    Server(sGetCityReportNew, Me, cix, Report) // own city
  else
    Server(sGetEnemyCityReportNew, Me, cLoc, Report); // enemy city
  TrueFood := C.Food;
  TrueProd := C.Prod;
  TruePoll := C.Pollution;
  if Supervising or (cix < 0) then
  begin // normalize city from after-turn state
    Dec(TrueFood, Report.FoodSurplus);
    if TrueFood < 0 then
      TrueFood := 0; // shouldn't happen
    Dec(TrueProd, Report.Production);
    if TrueProd < 0 then
      TrueProd := 0; // shouldn't happen
    Dec(TruePoll, Report.AddPollution);
    if TruePoll < 0 then
      TruePoll := 0; // shouldn't happen
  end;
  IsCityAlive := (cGov <> gAnarchy) and (C.Flags and chCaptured = 0);
  if not IsCityAlive then
    Report.Working := C.Size;

  RedTex.Assign(MainTexture);
  RedTex.ColorBevelLight := $0000FF;
  RedTex.ColorBevelShade := $000000;
  RedTex.ColorTextLight := $000000;
  RedTex.ColorTextShade := $0000FF;

  BitBltCanvas(Offscreen.Canvas, 0, 0, 640, 480, Back.Canvas, 0, 0);

  Offscreen.Canvas.Font.Assign(UniFont[ftCaption]);
  RisedTextOut(Offscreen.Canvas, 42, 7, Caption);
  with Offscreen.Canvas do
  begin // city size
    Brush.Color := $000000;
    FillRect(Rect(8 + 1, 7 + 1, 36 + 1, 32 + 1));
    Brush.Color := $FFFFFF;
    FillRect(Rect(8, 7, 36, 32));
    Brush.style := bsClear;
    Font.Color := $000000;
    S := IntToStr(C.Size);
    TextOut(8 + 14 - TextWidth(S) div 2, 7, S);
  end;
  Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);

  if not IsCityAlive then
  begin
    MakeRed(Offscreen, 18, 280, 298, 40);
    if cGov = gAnarchy then
      S := Phrases.Lookup('GOVERNMENT', gAnarchy)
    else { if c.Flags and chCaptured<>0 then }
      S := Phrases.Lookup('CITYEVENTS', 14);
    RisedTextOut(Offscreen.Canvas, 167 - BiColorTextWidth(Offscreen.Canvas, S)
      div 2, ymOpt - 9, S);
  end
  else if AllowChange then
  begin
    OptiType := C.Status shr 4 and $0F;
    Sprite(Offscreen, HGrSystem2, xmOpt - 32, ymOpt - 32, 64, 64,
      1 + OptiType mod 3 * 64, 217 + OptiType div 3 * 64);

    { display messages now }
    MessageCount := 0;
    for I := 0 to 31 do
      if Happened and ($FFFFFFFF - chCaptured) and (1 shl I) <> 0 then
        Inc(MessageCount);
    if MessageCount > 3 then
      MessageCount := 3;
    if MessageCount > 0 then
    begin
      MakeBlue(Offscreen, 74, 280, 242, 40);
      Line := 0;
      for I := 0 to nCityEventPriority - 1 do
        if Line < MessageCount then
          CheckMessage(CityEventPriority[I]);
    end
    else
    begin
      S := Phrases.Lookup('CITYMANAGETYPE', OptiType);
      J := Pos('\', S);
      if J = 0 then
        LoweredTextout(Offscreen.Canvas, -1, MainTexture, xmOpt + 40,
          ymOpt - 9, S)
      else
      begin
        LoweredTextout(Offscreen.Canvas, -1, MainTexture, xmOpt + 40,
          ymOpt - 17, Copy(S, 1, J - 1));
        LoweredTextout(Offscreen.Canvas, -1, MainTexture, xmOpt + 40, ymOpt - 1,
          Copy(S, J + 1, 255));
      end;
    end;
  end;

  with AreaMap do begin
    rx := (192 + xxt * 2 - 1) div (xxt * 2);
    ry := (96 + yyt * 2 - 1) div (yyt * 2);
    AreaMap.Paint(xmArea - xxt * 2 * rx, ymArea - yyt * 2 * ry - 3 * yyt,
      dLoc(cLoc, -2 * rx + 1, -2 * ry - 1), 4 * rx - 1, 4 * ry + 1, cLoc, cOwner,
      False, AllowChange and IsCityAlive and
      (C.Status and csResourceWeightsMask = 0));
    BitBltCanvas(Offscreen.Canvas, xmArea + 102, 42, 90, 33, Back.Canvas,
      xmArea + 102, 42);

    if IsCityAlive then
      for dy := -3 to 3 do
        for dx := -3 to 3 do
          if ((dx + dy) and 1 = 0) and (dx * dx * dy * dy < 81) then begin
            Loc1 := dLoc(cLoc, dx, dy);
            av := CityAreaInfo.Available[(dy + 3) shl 2 + (dx + 3) shr 1];
            if ((av = faNotAvailable) or (av = faTreaty) or (av = faInvalid)) and
              ((Loc1 < 0) or (Loc1 >= G.lx * G.ly) or (MyMap[Loc1] and fCity = 0))
            then
              Sprite(Offscreen, HGrTerrain, xmArea - xxt + xxt * dx,
                ymArea - yyt + yyt * dy, xxt * 2, yyt * 2, 1 + 5 * (xxt * 2 + 1),
                1 + yyt + 15 * (yyt * 3 + 1));
            if (1 shl ((dy + 3) shl 2 + (dx + 3) shr 1) and C.Tiles <> 0) then
              PaintResources(xmArea - xxt + xxt * dx, ymArea - yyt + yyt * dy,
                Loc1, (dx = 0) and (dy = 0));
          end;
  end;

  if Report.Working > 1 then
    D := (xService - (xmArea - 192) - 8 - 32) div (Report.Working - 1);
  if D > 28 then
    D := 28;
  for I := Report.Working - 1 downto 0 do
  begin
    if IsCityAlive then
      xGr := 29
    else
      xGr := 141;
    BitBltCanvas(Offscreen.Canvas, xmArea - 192 + 5 + I * D, ymArea - 96 - 29,
      27, 30, HGrSystem.Mask.Canvas, xGr, 171, SRCAND); { shadow }
    Sprite(Offscreen, HGrSystem, xmArea - 192 + 4 + I * D, ymArea - 96 - 30, 27,
      30, xGr, 171);
  end;
  if C.Size - Report.Working > 1 then
    D := (xmArea + 192 - xService - 32) div (C.Size - Report.Working - 1);
  if D > 28 then
    D := 28;
  for I := 0 to C.Size - Report.Working - 1 do
  begin
    xGr := 1 + 112;
    BitBltCanvas(Offscreen.Canvas, xmArea + 192 - 27 + 1 - I * D, 29 + 1, 27,
      30, HGrSystem.Mask.Canvas, xGr, 171, SRCAND); { shadow }
    Sprite(Offscreen, HGrSystem, xmArea + 192 - 27 - I * D, 29, 27, 30,
      xGr, 171);
    Sprite(Offscreen, HGrSystem, xmArea + 192 - 27 + 4 - I * D, 29 + 32, 10,
      10, 121, 126);
    Sprite(Offscreen, HGrSystem, xmArea + 192 - 27 + 13 - I * D, 29 + 32, 10,
      10, 121, 126);
    // Sprite(offscreen,HGrSystem,xmArea+192-31+18-i*d,ymArea-96-80+32,10,10,88,115);
  end;

  if C.Project and cpImp = 0 then
    PrName := Tribe[cOwner].ModelName[C.Project and cpIndex]
  else
    PrName := Phrases.Lookup('IMPROVEMENTS', C.Project and cpIndex);
  PrCost := Report.ProjectCost;

  // happiness section
  if IsCityAlive then
  begin
    if cGov = gFundamentalism then
      CountBar(Offscreen, xHapp, yHapp + dyBar, wBar, 17,
        Phrases.Lookup('FAITH'), Report.CollectedControl, MainTexture)
    else
    begin
      CountBar(Offscreen, xHapp, yHapp + dyBar, wBar, 17,
        Phrases.Lookup('HAPPINESS'), Report.Morale, MainTexture);
      CountBar(Offscreen, xHapp, yHapp + 2 * dyBar, wBar, 16,
        Phrases.Lookup('CONTROL'), Report.CollectedControl, MainTexture);
    end;
    CountBar(Offscreen, xHapp, yHapp, wBar, 8, Phrases.Lookup('LUX'),
      Report.Luxury, MainTexture);
    CountBar(Offscreen, xHapp + dxBar, yHapp, wBar, 19,
      Phrases.Lookup('UNREST'), 2 * Report.Deployed, MainTexture);
    CountBar(Offscreen, xHapp + dxBar, yHapp + dyBar, wBar, 17,
      Phrases.Lookup('HAPPINESSDEMAND'), C.Size, MainTexture);
    if Report.HappinessBalance >= 0 then
      CountBar(Offscreen, xHapp + dxBar, yHapp + 2 * dyBar, wBar, 17,
        Phrases.Lookup('HAPPINESSPLUS'), Report.HappinessBalance, MainTexture)
    else
    begin
      MakeRed(Offscreen, xHapp + dxBar - 6, yHapp + 2 * dyBar, wBar + 10, 38);
      CountBar(Offscreen, xHapp + dxBar, yHapp + 2 * dyBar, wBar, 18,
        Phrases.Lookup('LACK'), -Report.HappinessBalance, RedTex);
    end;
  end;

  // food section
  if IsCityAlive then
  begin
    CountBar(Offscreen, xFood, yFood + dyBar div 2, wBar, 0,
      Phrases.Lookup('FOOD'), Report.CollectedFood, MainTexture);
    CountBar(Offscreen, xFood + dxBar, yFood + dyBar, wBar, 0,
      Phrases.Lookup('DEMAND'), 2 * C.Size, MainTexture);
    CountBar(Offscreen, xFood + dxBar, yFood, wBar, 0,
      Phrases.Lookup('SUPPORT'), Report.FoodSupport, MainTexture);
    if Report.FoodSurplus >= 0 then
      if (cGov = gFuture) or (C.Size >= NeedAqueductSize) and
        (Report.FoodSurplus < 2) then
        CountBar(Offscreen, xFood + dxBar, yFood + 2 * dyBar, wBar, 6,
          Phrases.Lookup('PROFIT'), Report.FoodSurplus, MainTexture)
      else
        CountBar(Offscreen, xFood + dxBar, yFood + 2 * dyBar, wBar, 0,
          Phrases.Lookup('SURPLUS'), Report.FoodSurplus, MainTexture)
    else
    begin
      MakeRed(Offscreen, xFood + dxBar - 6, yFood + 2 * dyBar, wBar + 10, 38);
      CountBar(Offscreen, xFood + dxBar, yFood + 2 * dyBar, wBar, 1,
        Phrases.Lookup('LACK'), -Report.FoodSurplus, RedTex);
    end;
  end;
  CanGrow := (C.Size < MaxCitySize) and (cGov <> gFuture) and
    (Report.FoodSurplus > 0) and ((C.Size < NeedAqueductSize) or
    (C.Built[imAqueduct] = 1) and (C.Size < NeedSewerSize) or
    (C.Built[imSewer] = 1));
  FillBar(xFood + 3, yFood + 102, TrueFood,
    CutCityFoodSurplus(Report.FoodSurplus, IsCityAlive, cGov, C.Size),
    Report.Storage, 1, CanGrow);
  LoweredTextout(Offscreen.Canvas, -1, MainTexture, xFood + 3 - 5,
    yFood + 102 - 20, Format('%d/%d', [TrueFood, Report.Storage]));
  LoweredTextout(Offscreen.Canvas, -1, MainTexture, xFood - 2, yFood + 66,
    Phrases.Lookup('STORAGE'));

  // production section
  if IsCityAlive then
  begin
    CountBar(Offscreen, xProd, yProd, wBar, 2, Phrases.Lookup('MATERIAL'),
      Report.CollectedMaterial, MainTexture);
    CountBar(Offscreen, xProd + dxBar, yProd, wBar, 2,
      Phrases.Lookup('SUPPORT'), Report.MaterialSupport, MainTexture);
    if Report.Production >= 0 then
      if C.Project and (cpImp + cpIndex) = cpImp + imTrGoods then
        CountBar(Offscreen, xProd + dxBar, yProd + dyBar + 16, wBar, 6,
          Phrases.Lookup('PROFIT'), Report.Production, MainTexture)
      else
        CountBar(Offscreen, xProd + dxBar, yProd + dyBar + 16, wBar, 2,
          Phrases.Lookup('PROD'), Report.Production, MainTexture)
    else
    begin
      MakeRed(Offscreen, xProd + dxBar - 6, yProd + dyBar + 17, wBar + 10, 38);
      CountBar(Offscreen, xProd + dxBar, yProd + dyBar + 16, wBar, 3,
        Phrases.Lookup('LACK'), -Report.Production, RedTex);
    end;
  end;
  if C.Project and (cpImp + cpIndex) <> cpImp + imTrGoods then
    with Offscreen.Canvas do
    begin
      I := Report.Production;
      if (I < 0) or not IsCityAlive then
        I := 0;
      FillBar(xProd + 3, yProd + 16 + 63, TrueProd, I, PrCost, 4, True);
      LoweredTextout(Offscreen.Canvas, -1, MainTexture, xProd + 3 - 5,
        yProd + 16 + 43, Format('%d/%d', [TrueProd, PrCost]));
      if BiColorTextWidth(Offscreen.Canvas, PrName) > wBar + dxBar then
      begin
        repeat
          Delete(PrName, Length(PrName), 1)
        until BiColorTextWidth(Offscreen.Canvas, PrName) <= wBar + dxBar;
        PrName := PrName + '.'
      end;
    end;
  RisedTextOut(Offscreen.Canvas, xProd - 2, yProd + 36, PrName);

  // pollution section
  if IsCityAlive and (Report.AddPollution > 0) then
  begin
    FillBar(xPoll + 3, yPoll + 20, TruePoll, Report.AddPollution,
      MaxPollution, 3, True);
    RisedTextOut(Offscreen.Canvas, xPoll + 3 - 5, yPoll + 20 - 20,
      Phrases.Lookup('POLL'));
  end;

  // trade section
  if IsCityAlive and (Report.CollectedTrade > 0) then
  begin
    CountBar(Offscreen, xTrade, yTrade + dyBar div 2, wBar, 4,
      Phrases.Lookup('TRADE'), Report.CollectedTrade, MainTexture);
    CountBar(Offscreen, xTrade + dxBar, yTrade + 2 * dyBar, wBar, 5,
      Phrases.Lookup('CORR'), Report.Corruption, MainTexture);
    CountBar(Offscreen, xTrade + dxBar, yTrade, wBar, 6, Phrases.Lookup('TAX'),
      Report.Tax, MainTexture);
    CountBar(Offscreen, xTrade + dxBar, yTrade + dyBar, wBar, 12,
      Phrases.Lookup('SCIENCE'), Report.Science, MainTexture);
  end;

  // small map
  BitBltCanvas(Offscreen.Canvas, xSmallMap, ySmallMap, wSmallMap, hSmallMap,
    SmallCityMap.Canvas, 0, 0);
  if SmallMapMode = smImprovements then
    Frame(Offscreen.Canvas, xSmallMap + 48 * (ZoomArea div 3),
      ySmallMap + 24 * (ZoomArea mod 3), xSmallMap + 48 * (ZoomArea div 3) + 49,
      ySmallMap + 24 * (ZoomArea mod 3) + 25, MainTexture.ColorMark,
      MainTexture.ColorMark);
  Frame(Offscreen.Canvas, xSmallMap - 1, ySmallMap - 1, xSmallMap + wSmallMap,
    ySmallMap + hSmallMap, $B0B0B0, $FFFFFF);
  RFrame(Offscreen.Canvas, xSmallMap - 2, ySmallMap - 2, xSmallMap + wSmallMap +
    1, ySmallMap + hSmallMap + 1, $FFFFFF, $B0B0B0);

  Frame(Offscreen.Canvas, xSupport - 1, ySupport - 1, xSupport + wSupport,
    ySupport + hSupport, $B0B0B0, $FFFFFF);
  RFrame(Offscreen.Canvas, xSupport - 2, ySupport - 2, xSupport + wSupport + 1,
    ySupport + hSupport + 1, $FFFFFF, $B0B0B0);
  X := xSupport + wSupport div 2;
  Y := ySupport + hSupport div 2;
  if SmallMapMode = smSupportedUnits then
  begin
    Offscreen.Canvas.Brush.Color := MainTexture.ColorMark;
    Offscreen.Canvas.FillRect(Rect(X - 27, Y - 6, X + 27, Y + 6));
    Offscreen.Canvas.Brush.style := bsClear;
  end;
  Sprite(Offscreen, HGrSystem, X - 16, Y - 5, 10, 10, 88, 115);
  Sprite(Offscreen, HGrSystem, X - 5, Y - 5, 10, 10, 66, 115);
  Sprite(Offscreen, HGrSystem, X + 6, Y - 5, 10, 10, 154, 126);

  BitBltCanvas(Offscreen.Canvas, xZoomMap, yZoomMap, wZoomMap, hZoomMap,
    ZoomCityMap.Canvas, 0, 0);

  for I := 0 to 5 do
    imix[I] := -1;
  if SmallMapMode = smImprovements then
  begin
    if ZoomArea = 5 then
    begin
      Cnt := 0;
      for iix := 0 to nImp - 1 do
        if ((iix < nWonder) or (ImpPosition[iix] < 0)) and (C.Built[iix] > 0) then
        begin
          I := Cnt - Page * 6;
          if (I >= 0) and (I < 6) then
            imix[I] := iix;
          Inc(Cnt);
        end;
      PageCount := (Cnt + 5) div 6;
    end
    else
    begin
      for iix := nWonder to nImp - 1 do
      begin
        I := ImpPosition[iix] - 6 * ZoomArea;
        if (I >= 0) and (I < 6) and (C.Built[iix] > 0) then
          imix[I] := iix;
      end;
      PageCount := 0;
    end;
    for I := 0 to 5 do
      if imix[I] >= 0 then
      begin
        iix := imix[I];
        X := xZoomMap + 14 + 72 * (I mod 3);
        Y := yZoomMap + 14 + 56 * (I div 3);
        ImpImage(Offscreen.Canvas, X, Y, iix, cGov, AllowChange and
          (ClientMode < scContact));
        if IsCityAlive then
        begin
          if iix = imColosseum then
          begin
            Sprite(Offscreen, HGrSystem, X + 46, Y, 14, 14, 82, 100);
          end
          else
          begin
            HappyGain := 0;
            case iix of
              0 .. 27, imTemple:
                HappyGain := 2;
              imTheater:
                HappyGain := 4;
              imCathedral:
                if MyRO.Wonder[woBach].EffectiveOwner = cOwner then
                  HappyGain := 6
                else
                  HappyGain := 4;
            end;
            if HappyGain > 1 then
            begin
              D := 30 div (HappyGain - 1);
              if D > 10 then
                D := 10
            end;
            for J := 0 to HappyGain - 1 do
              Sprite(Offscreen, HGrSystem, X + 50, Y + D * J, 10, 10, 132, 126);
          end;
          for J := 0 to Imp[iix].Maint - 1 do
            Sprite(Offscreen, HGrSystem, X - 4, Y + 29 - 3 * J, 10, 10,
              132, 115);
        end
      end;
    if imix[0] >= 0 then
      Imp0Area.Hint := Phrases.Lookup('IMPROVEMENTS', imix[0])
    else
      Imp0Area.Hint := '';
    if imix[1] >= 0 then
      Imp1Area.Hint := Phrases.Lookup('IMPROVEMENTS', imix[1])
    else
      Imp1Area.Hint := '';
    if imix[2] >= 0 then
      Imp2Area.Hint := Phrases.Lookup('IMPROVEMENTS', imix[2])
    else
      Imp2Area.Hint := '';
    if imix[3] >= 0 then
      Imp3Area.Hint := Phrases.Lookup('IMPROVEMENTS', imix[3])
    else
      Imp3Area.Hint := '';
    if imix[4] >= 0 then
      Imp4Area.Hint := Phrases.Lookup('IMPROVEMENTS', imix[4])
    else
      Imp4Area.Hint := '';
    if imix[5] >= 0 then
      Imp5Area.Hint := Phrases.Lookup('IMPROVEMENTS', imix[5])
    else
      Imp5Area.Hint := '';
  end
  else { if SmallMapMode = smSupportedUnits then }
  begin
    LoweredTextout(Offscreen.Canvas, -1, MainTexture, xZoomMap + 6,
      yZoomMap + 2, Phrases.Lookup('SUPUNITS'));
    FreeSupp := C.Size * SupportFree[cGov] shr 1;
    Cnt := 0;
    for I := 0 to MyRO.nUn - 1 do
      if (MyUn[I].Loc >= 0) and (MyUn[I].Home = cix) then
        with MyModel[MyUn[I].mix] do
        begin
          Server(sGetUnitReport, Me, I, UnitReport);
          if (Cnt >= 6 * Page) and (Cnt < 6 * (Page + 1)) then
          begin // unit visible in display
            imix[Cnt - 6 * Page] := I;
            X := ((Cnt - 6 * Page) mod 3) * 64 + xZoomMap;
            Y := ((Cnt - 6 * Page) div 3) * 52 + yZoomMap + 20;
            MakeUnitInfo(Me, MyUn[I], UnitInfo);
            NoMap.SetOutput(Offscreen);
            NoMap.PaintUnit(X, Y, UnitInfo, MyUn[I].Status);

            for J := 0 to UnitReport.FoodSupport - 1 do
              Sprite(Offscreen, HGrSystem, X + 38 + 11 * J, Y + 40, 10,
                10, 66, 115);
            for J := 0 to UnitReport.ProdSupport - 1 do
            begin
              if (FreeSupp > 0) and
                (UnitReport.ReportFlags and urfAlwaysSupport = 0) then
              begin
                Sprite(Offscreen, HGrSystem, X + 16 - 11 * J, Y + 40, 10,
                  10, 143, 115);
                Dec(FreeSupp);
              end
              else
                Sprite(Offscreen, HGrSystem, X + 16 - 11 * J, Y + 40, 10,
                  10, 88, 115);
            end;
            if UnitReport.ReportFlags and urfDeployed <> 0 then
              for J := 0 to 1 do
                Sprite(Offscreen, HGrSystem, X + 27 + 11 * J, Y + 40, 10,
                  10, 154, 126)
          end // unit visible in display
          else
            Dec(FreeSupp, UnitReport.ProdSupport);
          Inc(Cnt);
        end;
    PageCount := (Cnt + 5) div 6;
    Imp0Area.Hint := '';
    Imp1Area.Hint := '';
    Imp2Area.Hint := '';
    Imp3Area.Hint := '';
    Imp4Area.Hint := '';
    Imp5Area.Hint := '';
  end;
  PageUpBtn.Visible := PageCount > 1;
  PageDownBtn.Visible := PageCount > 1;

  with Offscreen.Canvas do
  begin
    { display project now }
    DLine(Offscreen.Canvas, xView + 9 + xSizeBig, xProd + 2 * wBar + 10,
      yProd + dyBar + 16, $FFFFFF, $B0B0B0);
    if ProdHint then
    begin
      ScreenTools.Frame(Offscreen.Canvas, xView + 9 - 1, yView + 5 - 1,
        xView + 9 + xSizeBig, yView + 5 + ySizeBig, $B0B0B0, $FFFFFF);
      RFrame(Offscreen.Canvas, xView + 9 - 2, yView + 5 - 2,
        xView + 9 + xSizeBig + 1, yView + 5 + ySizeBig + 1, $FFFFFF, $B0B0B0);
      with Offscreen.Canvas do
      begin
        Brush.Color := $000000;
        FillRect(Rect(xView + 9, yView + 5, xView + 1 + 72 - 8,
          yView + 5 + 40));
        Brush.style := bsClear;
      end;
    end
    else if AllowChange and (C.Status and 7 <> 0) then
    begin // city type autobuild
      FrameImage(Offscreen.Canvas, bigimp, xView + 9, yView + 5, xSizeBig,
        ySizeBig, (C.Status and 7 - 1 + 3) * xSizeBig, 0, (cix >= 0) and
        (ClientMode < scContact));
    end
    else if C.Project and cpImp = 0 then
    begin // project is unit
      FrameImage(Offscreen.Canvas, bigimp, xView + 9, yView + 5, xSizeBig,
        ySizeBig, 0, 0, AllowChange and (ClientMode < scContact));
      with Tribe[cOwner].ModelPicture[C.Project and cpIndex] do
        Sprite(Offscreen, HGr, xView + 5, yView + 1, 64, 44,
          pix mod 10 * 65 + 1, pix div 10 * 49 + 1);
    end
    else
    begin // project is building
      if ProdHint then
        Paintiix := C.Project0 and cpIndex
      else
        Paintiix := C.Project and cpIndex;
      ImpImage(Offscreen.Canvas, xView + 9, yView + 5, Paintiix, cGov,
        AllowChange and (ClientMode < scContact));
    end;
  end;

  if AllowChange and (ClientMode < scContact) then
  begin
    I := Server(sBuyCityProject - sExecute, Me, cix, nil^);
    BuyBtn.Visible := (I = eOk) or (I = eViolation);
  end
  else
    BuyBtn.Visible := False;

  MarkUsedOffscreen(ClientWidth, ClientHeight);
end;

procedure TCityDlg.FormShow(Sender: TObject);
var
  dx, dy, Loc1: Integer;
  GetCityData: TGetCityData;
begin
  BlinkTime := 5;
  if cix >= 0 then
  begin { own city }
    C := MyCity[cix];
    cOwner := Me;
    cGov := MyRO.Government;
    ProdHint := (cGov <> gAnarchy) and
      (Happened and (chProduction or chFounded or chCaptured or
      chAllImpsMade) <> 0);
    Server(sGetCityAreaInfo, Me, cix, CityAreaInfo);
    NextCityBtn.Visible := WindowMode = wmPersistent;
    PrevCityBtn.Visible := WindowMode = wmPersistent;
  end
  else { enemy city }
  begin
    SmallMapMode := smImprovements;
    Server(sGetCity, Me, cLoc, GetCityData);
    C := GetCityData.C;
    cOwner := GetCityData.Owner;
    cGov := MyRO.EnemyReport[cOwner].Government;
    Happened := C.Flags and $7FFFFFFF;
    ProdHint := False;
    Server(sGetEnemyCityAreaInfo, Me, cLoc, CityAreaInfo);

    if C.Project and cpImp = 0 then
    begin
      emix := MyRO.nEnemyModel - 1;
      while (emix > 0) and ((MyRO.EnemyModel[emix].Owner <> cOwner) or
        (Integer(MyRO.EnemyModel[emix].mix) <> C.Project and cpIndex)) do
        Dec(emix);
      if not Assigned(Tribe[cOwner].ModelPicture[C.Project and cpIndex].HGr) then
        InitEnemyModel(emix);
    end;

    NextCityBtn.Visible := False;
    PrevCityBtn.Visible := False;
  end;
  Page := 0;

  if C.Size < 5 then
    SizeClass := 0
  else if C.Size < 9 then
    SizeClass := 1
  else if C.Size < 13 then
    SizeClass := 2
  else
    SizeClass := 3;

  // check if port
  IsPort := False;
  for dx := -2 to 2 do
    for dy := -2 to 2 do
      if Abs(dx) + Abs(dy) = 2 then
      begin
        Loc1 := dLoc(cLoc, dx, dy);
        if (Loc1 >= 0) and (Loc1 < G.lx * G.ly) and
          (MyMap[Loc1] and fTerrain < fGrass) then
          IsPort := True;
      end;

  if WindowMode = wmModal then
  begin { center on screen }
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
  end;

  Caption := CityName(C.ID);

  InitSmallCityMap;
  InitZoomCityMap;
  OpenSoundEvent := -1;
  OffscreenPaint;
  Timer1.Enabled := True;
end;

procedure TCityDlg.ShowNewContent(NewMode: TWindowMode; Loc: Integer; ShowEvent: Cardinal);
begin
  if MyMap[Loc] and fOwned <> 0 then
  begin // own city
    cix := MyRO.nCity - 1;
    while (cix >= 0) and (MyCity[cix].Loc <> Loc) do
      Dec(cix);
    Assert(cix >= 0);
    if (Optimize_cixTileChange >= 0) and
      (Optimize_TilesBeforeChange and not MyCity[Optimize_cixTileChange].Tiles
      <> 0) then
    begin
      CityOptimizer_ReleaseCityTiles(Optimize_cixTileChange,
        Optimize_TilesBeforeChange and
        not MyCity[Optimize_cixTileChange].Tiles);
      if WindowMode <> wmModal then
        MainScreen.UpdateViews;
    end;
    Optimize_cixTileChange := cix;
    Optimize_TilesBeforeChange := MyCity[cix].Tiles;
  end
  else
    cix := -1;
  AllowChange := not Supervising and (cix >= 0);
  cLoc := Loc;
  Happened := ShowEvent;
  inherited ShowNewContent(NewMode);
end;

procedure TCityDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I, qx, qy, dx, dy, fix, NewTiles, Loc1, iix, SellResult: Integer;
  Rebuild: Boolean;
begin
  if (ssLeft in Shift) and (X >= xSmallMap) and (X < xSmallMap + wSmallMap) and
    (Y >= ySmallMap) and (Y < ySmallMap + hSmallMap) then
  begin
    SmallMapMode := smImprovements;
    ZoomArea := (Y - ySmallMap) * 3 div hSmallMap + 3 *
      ((X - xSmallMap) * 2 div wSmallMap);
    Page := 0;
    InitZoomCityMap;
    SmartUpdateContent;
    Exit;
  end;
  if (ssLeft in Shift) and (X >= xSupport) and (X < xSupport + wSupport) and
    (Y >= ySupport) and (Y < ySupport + hSupport) then
  begin
    SmallMapMode := smSupportedUnits;
    Page := 0;
    InitZoomCityMap;
    SmartUpdateContent;
    Exit;
  end;
  if not AllowChange then
    Exit; // Not an own city

  if (ssLeft in Shift) then
    if (ClientMode < scContact) and (X >= xView) and (Y >= yView) and
      (X < xView + 73) and (Y < yView + 50) then
      if cGov = gAnarchy then
        with MessgExDlg do
        begin
          { MessgText:=Phrases.Lookup('OUTOFCONTROL');
            if C.Project and cpImp=0 then
            MessgText:=Format(MessgText,[Tribe[cOwner].ModelName[C.Project and cpIndex]])
            else MessgText:=Format(MessgText,[Phrases.Lookup('IMPROVEMENTS',C.Project and cpIndex)]); }
          MessgText := Phrases.Lookup('NOCHANGEINANARCHY');
          Kind := mkOk;
          ShowModal;
        end
      else
      begin
        if ProdHint then
        begin
          ProdHint := False;
          SmartUpdateContent
        end;
        ChooseProject;
      end
    else if (SmallMapMode = smImprovements) and (X >= xZoomMap) and (X < xZoomMap + wZoomMap) and
      (Y >= yZoomMap) and (Y < yZoomMap + hZoomMap) then
    begin
      I := 5;
      while (I >= 0) and not((X >= xZoomMap + 14 + 72 * (I mod 3)) and
        (X < xZoomMap + 14 + 56 + 72 * (I mod 3)) and
        (Y >= yZoomMap + 14 + 56 * (I div 3)) and
        (Y < yZoomMap + 14 + 40 + 56 * (I div 3))) do
        Dec(I);
      if I >= 0 then
      begin
        iix := imix[I];
        if iix >= 0 then
          if ssShift in Shift then
            HelpDlg.ShowNewContent(WindowModeMakePersistent(FWindowMode), hkImp, iix)
          else if (ClientMode < scContact) then
            with MessgExDlg do
            begin
              IconKind := mikImp;
              IconIndex := iix;
              if (iix = imPalace) or (Imp[iix].Kind = ikWonder) then
              begin
                MessgText := Phrases.Lookup('IMPROVEMENTS', iix);
                if iix = woOracle then
                  MessgText := MessgText + '\' +
                    Format(Phrases.Lookup('ORACLEINCOME'), [MyRO.OracleIncome]);
                Kind := mkOk;
                ShowModal;
              end
              else
              begin
                SellResult := Server(sSellCityImprovement - sExecute, Me,
                  cix, iix);
                if SellResult < rExecuted then
                begin
                  if SellResult = eOnlyOnce then
                    MessgText := Phrases.Lookup('NOSELLAGAIN')
                  else
                    MessgText := Phrases.Lookup('OUTOFCONTROL');
                  MessgText := Format(MessgText,
                    [Phrases.Lookup('IMPROVEMENTS', iix)]);
                  Kind := mkOk;
                  ShowModal;
                end
                else
                begin
                  if Server(sRebuildCityImprovement - sExecute, Me, cix, iix) < rExecuted
                  then
                  begin // no rebuild possible, ask for sell only
                    Rebuild := False;
                    MessgText := Phrases.Lookup('IMPROVEMENTS', iix);
                    if not Phrases2FallenBackToEnglish then
                      MessgText := Format(Phrases2.Lookup('SELL2'),
                        [MessgText, Imp[iix].Cost * BuildCostMod
                        [G.Difficulty[Me]] div 12])
                    else
                      MessgText := Format(Phrases.Lookup('SELL'), [MessgText]);
                    if iix = imSpacePort then
                      with MyRO.Ship[Me] do
                        if Parts[0] + Parts[1] + Parts[2] > 0 then
                          MessgText := MessgText + ' ' +
                            Phrases.Lookup('SPDESTRUCTQUERY');
                    Kind := mkYesNo;
                    ShowModal;
                    if ModalResult <> mrOK then
                      iix := -1
                  end
                  else
                  begin
                    Rebuild := True;
                    MessgText := Phrases.Lookup('IMPROVEMENTS', iix);
                    if not Phrases2FallenBackToEnglish then
                      MessgText := Format(Phrases2.Lookup('DISPOSE2'),
                        [MessgText, Imp[iix].Cost * BuildCostMod
                        [G.Difficulty[Me]] div 12 * 2 div 3])
                    else
                      MessgText := Format(Phrases.Lookup('DISPOSE'),
                        [MessgText]);
                    if iix = imSpacePort then
                      with MyRO.Ship[Me] do
                        if Parts[0] + Parts[1] + Parts[2] > 0 then
                          MessgText := MessgText + ' ' +
                            Phrases.Lookup('SPDESTRUCTQUERY');
                    Kind := mkYesNo;
                    ShowModal;
                    if ModalResult <> mrOK then
                      iix := -1
                  end;
                  if iix >= 0 then
                  begin
                    if Rebuild then
                    begin
                      Play('CITY_REBUILDIMP');
                      Server(sRebuildCityImprovement, Me, cix, iix);
                    end
                    else
                    begin
                      Play('CITY_SELLIMP');
                      Server(sSellCityImprovement, Me, cix, iix);
                    end;
                    CityOptimizer_CityChange(cix);
                    InitSmallCityMap;
                    SmartUpdateContent;
                    if WindowMode <> wmModal then
                      MainScreen.UpdateViews;
                  end;
                end;
              end;
            end;
      end;
    end
    else if (SmallMapMode = smSupportedUnits) and (X >= xZoomMap) and (X < xZoomMap + wZoomMap) and
      (Y >= yZoomMap) and (Y < yZoomMap + hZoomMap) then
    begin
      I := 5;
      while (I >= 0) and not((X >= xZoomMap + 64 * (I mod 3)) and
        (X < xZoomMap + 64 + 64 * (I mod 3)) and
        (Y >= yZoomMap + 20 + 48 * (I div 3)) and
        (Y < yZoomMap + 20 + 52 + 48 * (I div 3))) do
        Dec(I);
      if (I >= 0) and (imix[I] >= 0) then
        if ssShift in Shift then
        else if (cix >= 0) and (ClientMode < scContact) and
          (WindowMode <> wmModal) then
        begin
          CloseAction := None;
          Close;
          MainScreen.CityClosed(imix[I], False, True);
        end;
    end
    else if (X >= xmArea - 192) and (X < xmArea + 192) and (Y >= ymArea - 96)
      and (Y < ymArea + 96) then
    with AreaMap do begin
      qx := ((4000 * xxt * yyt) + (X - xmArea) * (yyt * 2) + (Y - ymArea + yyt)
        * (xxt * 2)) div (xxt * yyt * 4) - 1000;
      qy := ((4000 * xxt * yyt) + (Y - ymArea + yyt) * (xxt * 2) - (X - xmArea)
        * (yyt * 2)) div (xxt * yyt * 4) - 1000;
      dx := qx - qy;
      dy := qx + qy;
      if (dx >= -3) and (dx <= 3) and (dy >= -3) and (dy <= 3) and
        (dx * dx * dy * dy < 81) and ((dx <> 0) or (dy <> 0)) then
        if ssShift in Shift then
        begin // terrain help
          Loc1 := dLoc(cLoc, dx, dy);
          if (Loc1 >= 0) and (Loc1 < G.lx * G.ly) then
            HelpOnTerrain(Loc1, WindowModeMakePersistent(FWindowMode))
        end
        else if (ClientMode < scContact) and (cGov <> gAnarchy) and
          (C.Flags and chCaptured = 0) then
        begin // toggle exploitation
          Assert(not Supervising);
          if C.Status and csResourceWeightsMask <> 0 then
          begin
            with MessgExDlg do
            begin
              MessgText := Phrases.Lookup('CITYMANAGEOFF');
              OpenSound := 'MSG_DEFAULT';
              Kind := mkOkCancel;
              IconKind := mikFullControl;
              ShowModal;
            end;
            if MessgExDlg.ModalResult = mrOK then
            begin
              MyCity[cix].Status := MyCity[cix].Status and
                not csResourceWeightsMask; // off
              C.Status := MyCity[cix].Status;
              SmartUpdateContent;
            end;
            Exit;
          end;
          fix := (dy + 3) shl 2 + (dx + 3) shr 1;
          NewTiles := MyCity[cix].Tiles xor (1 shl fix);
          if Server(sSetCityTiles, Me, cix, NewTiles) >= rExecuted then
          begin
            SmartUpdateContent;
            if WindowMode <> wmModal then
              MainScreen.UpdateViews;
          end;
        end;
    end
    else if (ClientMode < scContact) and (cGov <> gAnarchy) and
      (C.Flags and chCaptured = 0) and (X >= xmOpt - 32) and (X < xmOpt + 32)
      and (Y >= ymOpt - 32) and (Y < ymOpt + 32) then
    begin
      I := sqr(X - xmOpt) + sqr(Y - ymOpt); // click radius
      if I <= 32 * 32 then
      begin
        if I < 16 * 16 then // inner area clicked
          if C.Status and csResourceWeightsMask <> 0 then
            I := (C.Status shr 4 and $0F) mod 5 + 1 // rotate except off
          else
            I := 3 // rwGrowth
        else
          case trunc(arctan2(X - xmOpt, ymOpt - Y) * 180 / pi) of
            - 25 - 52 * 2 .. -26 - 52:
              I := 1;
            -25 - 52 .. -26:
              I := 2;
            -25 .. 25:
              I := 3;
            26 .. 25 + 52:
              I := 4;
            26 + 52 .. 25 + 52 * 2:
              I := 5;
            180 - 26 .. 180, -180 .. -180 + 26:
              I := 0;
          else
            I := -1;
          end;
        if I >= 0 then
        begin
          ChangeResourceWeights(I);
          SmartUpdateContent;
          if WindowMode <> wmModal then
            MainScreen.UpdateViews;
        end;
      end;
    end;
end;

procedure TCityDlg.ChooseProject;
type
  TProjectType = (
    ptSelect = 0,
    ptTrGoods = 1,
    ptUn = 2,
    ptCaravan = 3,
    ptImp = 4,
    ptWonder = 6,
    ptShip = 7,
    ptInvalid = 8
  );

  function ProjectType(Project: Integer): TProjectType;
  begin
    if Project and cpCompleted <> 0 then
      Result := ptSelect
    else if Project and (cpImp + cpIndex) = cpImp + imTrGoods then
      Result := ptTrGoods
    else if Project and cpImp = 0 then begin
      if MyModel[Project and cpIndex].Kind = mkCaravan then
        Result := ptCaravan
      else Result := ptUn;
    end
    else if Project and cpIndex >= nImp then
      Result := ptInvalid
    else if Imp[Project and cpIndex].Kind = ikWonder then
      Result := ptWonder
    else if Imp[Project and cpIndex].Kind = ikShipPart then
      Result := ptShip
    else
      Result := ptImp;
  end;

var
  NewProject, OldMoney, cix1: Integer;
  pt0, pt1: TProjectType;
  QueryOk: Boolean;
begin
  Assert(not Supervising);
  ModalSelectDlg.ShowNewContent_CityProject(wmModal, cix);
  if ModalSelectDlg.Result <> -1 then
  begin
    if ModalSelectDlg.Result and cpType <> 0 then
    begin
      MyCity[cix].Status := MyCity[cix].Status and not 7 or
        (1 + ModalSelectDlg.Result and cpIndex);
      AutoBuild(cix, MyData.ImpOrder[ModalSelectDlg.Result and cpIndex]);
    end
    else
    begin
      NewProject := ModalSelectDlg.Result;
      QueryOk := True;
      if (NewProject and cpImp <> 0) and (NewProject and cpIndex >= 28) and
        (MyRO.NatBuilt[NewProject and cpIndex] > 0) then
        with MessgExDlg do
        begin
          cix1 := MyRO.nCity - 1;
          while (cix1 >= 0) and
            (MyCity[cix1].Built[NewProject and cpIndex] = 0) do
            Dec(cix1);
          MessgText := Format(Phrases.Lookup('DOUBLESTATEIMP'),
            [Phrases.Lookup('IMPROVEMENTS', NewProject and cpIndex),
            CityName(MyCity[cix1].ID)]);
          OpenSound := 'MSG_DEFAULT';
          Kind := mkOkCancel;
          IconKind := mikImp;
          IconIndex := NewProject and cpIndex;
          ShowModal;
          QueryOk := ModalResult = mrOK;
        end;
      if not QueryOk then
        Exit;

      if (MyCity[cix].Prod > 0) then
      begin
        pt0 := ProjectType(MyCity[cix].Project0);
        pt1 := ProjectType(NewProject);
        if (pt0 <> ptSelect) and (pt1 <> ptTrGoods) then
        begin
          if NewProject and (cpImp or cpIndex) <> MyCity[cix].Project0 and
            (cpImp or cpIndex) then
          begin // loss of material -- do query
            Gtk2Fix;
            if (pt1 = ptTrGoods) or (pt1 = ptShip) or (pt1 <> pt0) and
              (pt0 <> ptCaravan) then begin
              QueryOk := SimpleQuery(mkOkCancel,
                Format(Phrases.Lookup('LOSEMAT'), [MyCity[cix].Prod0,
                MyCity[cix].Prod0]), 'MSG_DEFAULT') = mrOK
            end else
            if MyCity[cix].Project and (cpImp or cpIndex) = MyCity[cix]
              .Project0 and (cpImp or cpIndex) then begin
                QueryOk := SimpleQuery(mkOkCancel, Phrases.Lookup('LOSEMAT3'),
                  'MSG_DEFAULT') = mrOK;
            end;
          end;
        end;
      end;
      if not QueryOk then
        Exit;

      OldMoney := MyRO.Money;
      MyCity[cix].Status := MyCity[cix].Status and not 7;
      if (NewProject and cpImp = 0) and
        ((MyCity[cix].Size < 4) and
        (MyModel[NewProject and cpIndex].Kind = mkSettler) or
        (MyCity[cix].Size < 3) and
        ((MyModel[NewProject and cpIndex].Kind = mkSlaves) or
        (NewProject and cpConscripts <> 0))) then
        if SimpleQuery(mkYesNo, Phrases.Lookup('EMIGRATE'), 'MSG_DEFAULT') <> mrOK
        then
          NewProject := NewProject or cpDisbandCity;
      Server(sSetCityProject, Me, cix, NewProject);
      C.Project := MyCity[cix].Project;
      if MyRO.Money > OldMoney then
        Play('CITY_SELLIMP');
    end;
    CityOptimizer_CityChange(cix);

    if WindowMode <> wmModal then
      MainScreen.UpdateViews;
    InitSmallCityMap;
    SmartUpdateContent;
  end;
end;

procedure TCityDlg.BuyClick(Sender: TObject);
var
  NextProd, Cost: Integer;
begin
  if (cix < 0) or (ClientMode >= scContact) then
    Exit;
  with MyCity[cix], MessgExDlg do
  begin
    Cost := Report.ProjectCost;
    NextProd := Report.Production;
    if NextProd < 0 then
      NextProd := 0;
    Cost := Cost - Prod - NextProd;
    if (MyRO.Wonder[woMich].EffectiveOwner = Me) and (Project and cpImp <> 0)
    then
      Cost := Cost * 2
    else
      Cost := Cost * 4;
    if (Cost <= 0) and (Report.HappinessBalance >= 0) { no disorder } then
    begin
      MessgText := Phrases.Lookup('READY');
      Kind := mkOk;
    end
    else if Cost > MyRO.Money then
    begin
      OpenSound := 'MSG_DEFAULT';
      MessgText := Format(Phrases.Lookup('NOMONEY'), [Cost, MyRO.Money]);
      Kind := mkOk;
    end
    else
    begin
      MessgText := Format(Phrases.Lookup('BUY'), [Cost]);
      Kind := mkYesNo;
    end;
    ShowModal;
    if (Kind = mkYesNo) and (ModalResult = mrOK) then
    begin
      if Server(sBuyCityProject, Me, cix, nil^) >= rExecuted then
      begin
        Play('CITY_BUYPROJECT');
        SmartUpdateContent;
        if WindowMode <> wmModal then
          MainScreen.UpdateViews;
      end;
    end;
  end;
end;

procedure TCityDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := False;
  ProdHint := False;
  MarkCityLoc := -1;
  if Optimize_cixTileChange >= 0 then
  begin
    if Optimize_TilesBeforeChange and not MyCity[Optimize_cixTileChange]
      .Tiles <> 0 then
    begin
      CityOptimizer_ReleaseCityTiles(Optimize_cixTileChange,
        Optimize_TilesBeforeChange and
        not MyCity[Optimize_cixTileChange].Tiles);
      if WindowMode <> wmModal then
        MainScreen.UpdateViews;
    end;
    Optimize_cixTileChange := -1;
  end;
  if CloseAction > None then
    MainScreen.CityClosed(RestoreUnFocus, CloseAction = StepFocus);
  RestoreUnFocus := -1;
  inherited;
end;

procedure TCityDlg.Timer1Timer(Sender: TObject);
begin
  if ProdHint then
  begin
    BlinkTime := (BlinkTime + 1) mod 12;
    if BlinkTime = 0 then
      with Canvas do
      begin
        BitBltCanvas(Canvas, xView + 5, yView + 1, 64, 2, Back.Canvas,
          xView + 5, yView + 1);
        BitBltCanvas(Canvas, xView + 5, yView + 3, 2, 42, Back.Canvas,
          xView + 5, yView + 3);
        BitBltCanvas(Canvas, xView + 5 + 62, yView + 3, 2, 42,
          Back.Canvas, xView + 5 + 62, yView + 3);
        ScreenTools.Frame(Canvas, xView + 9 - 1, yView + 5 - 1, xView + 9 + xSizeBig,
          yView + 5 + ySizeBig, $B0B0B0, $FFFFFF);
        RFrame(Canvas, xView + 9 - 2, yView + 5 - 2, xView + 9 + xSizeBig + 1,
          yView + 5 + ySizeBig + 1, $FFFFFF, $B0B0B0);
        Brush.Color := $000000;
        FillRect(Rect(xView + 9, yView + 5, xView + 1 + 72 - 8,
          yView + 5 + 40));
        Brush.style := bsClear;
      end
    else if BlinkTime = 6 then
    begin
      if AllowChange and (C.Status and 7 <> 0) then
      begin // city type autobuild
        FrameImage(Canvas, bigimp, xView + 9, yView + 5, xSizeBig, ySizeBig,
          (C.Status and 7 - 1 + 3) * xSizeBig, 0, True);
      end
      else if C.Project and cpImp = 0 then
      begin // project is unit
        BitBltCanvas(Canvas, xView + 9, yView + 5, xSizeBig, ySizeBig,
          Bigimp.Canvas, 0, 0);
        with Tribe[cOwner].ModelPicture[C.Project and cpIndex] do
          Sprite(Canvas, HGr, xView + 5, yView + 1, 64, 44, pix mod 10 * 65 + 1,
            pix div 10 * 49 + 1);
      end
      else
        ImpImage(Canvas, xView + 9, yView + 5, C.Project0 and cpIndex,
          cGov, True);
    end;
  end;
end;

procedure TCityDlg.FormPaint(Sender: TObject);
begin
  inherited;
  if OpenSoundEvent >= 0 then
    PostMessage(Handle, WM_PLAYSOUND, 0, 0);
end;

procedure TCityDlg.OnPlaySound(var Msg: TMessage);
begin
  if 1 shl OpenSoundEvent = chProduction then
  begin
    if C.Project0 and cpImp <> 0 then
    begin
      if C.Project0 and cpIndex >= 28 then
      // wonders have already extra message with sound
        if Imp[C.Project0 and cpIndex].Kind = ikShipPart then
          Play('SHIP_BUILT')
        else
          Play('CITY_IMPCOMPLETE')
    end
    else
      Play('CITY_UNITCOMPLETE');
  end
  else
  if OpenSoundEvent >= 0 then
    Play(CityEventSoundItem[OpenSoundEvent]);
  OpenSoundEvent := -2;
end;

function Prio(iix: Integer): Integer;
begin
  case Imp[iix].Kind of
    ikWonder:
      Result := iix + 10000;
    ikNatLocal, ikNatGlobal:
      case iix of
        imPalace:
          Result := 0;
      else
        Result := iix + 20000;
      end;
  else
    case iix of
      imTownHall, imCourt:
        Result := iix + 30000;
      imAqueduct, imSewer:
        Result := iix + 40000;
      imTemple, imTheater, imCathedral:
        Result := iix + 50000;
    else
      Result := iix + 90000;
    end;
  end;
end;

procedure TCityDlg.NextCityBtnClick(Sender: TObject);
begin
  ChangeCity(+1);
end;

procedure TCityDlg.PrevCityBtnClick(Sender: TObject);
begin
  ChangeCity(-1);
end;

procedure TCityDlg.ChangeCity(D: Integer);
var
  cixNew: Integer;
begin
  cixNew := cix;
  repeat
    cixNew := (cixNew + MyRO.nCity + D) mod MyRO.nCity;
  until (MyCity[cixNew].Loc >= 0) or (cixNew = cix);
  if cixNew <> cix then
    MainScreen.ZoomToCity(MyCity[cixNew].Loc);
end;

procedure TCityDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_UP) or (Key = VK_NUMPAD8)) and (cix >= 0) and
    (WindowMode = wmPersistent) then
    ChangeCity(-1)
  else if ((Key = VK_DOWN) or (Key = VK_NUMPAD2)) and (cix >= 0) and
    (WindowMode = wmPersistent) then
    ChangeCity(+1)
  else
    inherited;
end;

{ procedure TCityDlg.AdviceBtnClick(Sender: TObject);
  begin
  AdvisorDlg.GiveCityAdvice(cix);
  end; }

procedure TCityDlg.PageUpBtnClick(Sender: TObject);
begin
  if Page > 0 then
  begin
    Dec(Page);
    SmartUpdateContent;
  end;
end;

procedure TCityDlg.PageDownBtnClick(Sender: TObject);
begin
  if Page < PageCount - 1 then
  begin
    Inc(Page);
    SmartUpdateContent;
  end;
end;

procedure TCityDlg.ChangeResourceWeights(iResourceWeights: Integer);
var
  Advice: TCityTileAdviceData;
begin
  Assert(not Supervising);
  Assert(cix >= 0);
  MyCity[cix].Status := MyCity[cix].Status and not csResourceWeightsMask or
    (iResourceWeights shl 4);
  C.Status := MyCity[cix].Status;
  if iResourceWeights > 0 then
  begin
    Advice.ResourceWeights := OfferedResourceWeights[iResourceWeights];
    Server(sGetCityTileAdvice, Me, cix, Advice);
    if Advice.Tiles <> MyCity[cix].Tiles then
      Server(sSetCityTiles, Me, cix, Advice.Tiles);
  end;
end;

procedure SortImprovements;
var
  I, J, K: Integer;
begin
  for I := 0 to nImp - 1 do
    ImpSorted[I] := I;
  for I := 0 to nImp - 2 do
    for J := I + 1 to nImp - 1 do
      if Prio(ImpSorted[I]) > Prio(ImpSorted[J]) then begin
        K := ImpSorted[I];
        ImpSorted[I] := ImpSorted[J];
        ImpSorted[J] := K;
      end;
end;

initialization

SortImprovements;

end.
