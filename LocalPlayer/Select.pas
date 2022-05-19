{$INCLUDE Switches.inc}
unit Select;

interface

uses
  Protocol, ClientTools, Term, ScreenTools, PVSB, BaseWin,
  LCLIntf, LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, ButtonB, ButtonBase, Menus, Types;

const
  MaxLayer = 3;

type
  TListKind = (kProject, kAdvance, kFarAdvance, kCities, kCityEvents, kModels,
    kEModels, kAllEModels, kTribe, kScience, kShipPart, kEShipPart, kChooseTech,
    kChooseETech, kChooseModel, kChooseEModel, kChooseCity, kChooseECity,
    kStealTech, kGov, kMission);

  { TListDlg }

  TListDlg = class(TFramedDlg)
    CloseBtn: TButtonB;
    Layer2Btn: TButtonB;
    Layer1Btn: TButtonB;
    Layer0Btn: TButtonB;
    ToggleBtn: TButtonB;
    Popup: TPopupMenu;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure ModeBtnClick(Sender: TObject);
    procedure ToggleBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PlayerClick(Sender: TObject);
  private
    Kind: TListKind;
    LineDistance: Integer;
    MaxLines: Integer;
    cixProject: Integer;
    pView: Integer;
    Selected: Integer;
    DispLines: Integer;
    Layer: Integer;
    nColumn: Integer;
    TechNameSpace: Integer;
    ScienceNation: Integer;
    ScrollBar: TPVScrollbar;
    Lines: array [0 .. MaxLayer - 1] of Integer;
    FirstShrinkedLine: array [0 .. MaxLayer - 1] of Integer;
    Code: array [0 .. MaxLayer - 1, 0 .. 4095] of Integer;
    Column: array [0 .. nPl - 1] of Integer;
    Closable: Boolean;
    MultiPage: Boolean;
    ScienceNationDotBuffer: TBitmap;
    procedure ScrollBarUpdate(Sender: TObject);
    procedure InitLines;
    procedure Line(ca: TCanvas; L: Integer; NonText, lit: Boolean);
    function RenameCity(cix: Integer): Boolean;
    function RenameModel(mix: Integer): Boolean;
    procedure OnScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure OnMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  public
    Result: Integer;
    function OnlyChoice(TestKind: TListKind): Integer;
    // -2=empty, -1=ambiguous, other=only choice
    procedure OffscreenPaint; override;
    procedure ShowNewContent(NewMode: TWindowMode; ListKind: TListKind);
    procedure ShowNewContent_CityProject(NewMode: TWindowMode; cix: Integer);
    procedure ShowNewContent_MilReport(NewMode: TWindowMode; P: Integer);
    procedure EcoChange;
    procedure TechChange;
    procedure AddCity;
    procedure RemoveUnit;
  end;

  TModalSelectDlg = TListDlg;

const
  cpType = $10000;
  mixAll = $10000;
  adAll = $10000;

var
  ListDlg: TListDlg;
  ModalSelectDlg: TModalSelectDlg;


implementation

uses
  CityScreen, Help, UnitStat, Tribes, Inp, CmdList;

{$R *.lfm}

const
  CityNameSpace = 127;

  MustChooseKind = [kTribe, kStealTech, kGov];

procedure TListDlg.FormCreate(Sender: TObject);
begin
  inherited;
  Canvas.Font.Assign(UniFont[ftNormal]);
  ScrollBar := TPVScrollbar.Create(Self);
  ScrollBar.SetBorderSpacing(36, 10, 36);
  ScrollBar.OnUpdate := ScrollBarUpdate;
  InitButtons;
  Kind := kMission;
  Layer0Btn.Hint := Phrases.Lookup('BTN_IMPRS');
  Layer1Btn.Hint := Phrases.Lookup('BTN_WONDERS');
  Layer2Btn.Hint := Phrases.Lookup('BTN_CLASSES');
  ScienceNationDotBuffer := TBitmap.Create;
  ScienceNationDotBuffer.PixelFormat := pf24bit;
  ScienceNationDotBuffer.SetSize(17, 17);
  ScienceNationDotBuffer.Canvas.FillRect(0, 0, ScienceNationDotBuffer.Width, ScienceNationDotBuffer.Height);
end;

procedure TListDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ScrollBar);
  FreeAndNil(ScienceNationDotBuffer);
end;

procedure TListDlg.CloseBtnClick(Sender: TObject);
begin
  Closable := True;
  Close;
end;

procedure TListDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Closable or not(Kind in MustChooseKind);
end;

procedure TListDlg.OnScroll(var Msg: TMessage);
begin
  { TODO: Handled by MouseWheel event
  if ScrollBar.Process(Msg) then  begin
    Selected := -2;
    SmartUpdateContent(True);
  end;
  }
end;

procedure TListDlg.OnMouseLeave(var Msg: TMessage);
begin
  if not Closable and (Selected <> -2) then
  begin
    Line(Canvas, Selected, False, False);
    Selected := -2;
  end;
end;

procedure TListDlg.FormPaint(Sender: TObject);
var
  S: string;
begin
  inherited;
  Canvas.Font.Assign(UniFont[ftNormal]);
  if Selected <> -2 then
    Line(Canvas, Selected, False, True);
  S := '';
  if (Kind = kAdvance) and (MyData.FarTech <> adNone) then
    S := Format(Phrases.Lookup('TECHFOCUS'),
      [Phrases.Lookup('ADVANCES', MyData.FarTech)])
  else if Kind = kModels then
    S := Tribe[Me].TPhrase('SHORTNAME')
  else if Kind = kEModels then
    S := Tribe[pView].TPhrase('SHORTNAME') + ' (' +
      TurnToString(MyRO.EnemyReport[pView].TurnOfMilReport) + ')';
  if S <> '' then
    LoweredTextOut(Canvas, -1, MainTexture,
      (ClientWidth - BiColorTextWidth(Canvas, S)) div 2, 31, S);
  if not MultiPage and (Kind in [kProject, kAdvance, kFarAdvance]) and not Phrases2FallenBackToEnglish
  then
  begin
    S := Phrases2.Lookup('SHIFTCLICK');
    LoweredTextOut(Canvas, -2, MainTexture,
      (ClientWidth - BiColorTextWidth(Canvas, S)) div 2, ClientHeight - 29, S);
  end;
end;

procedure TListDlg.Line(ca: TCanvas; L: Integer; NonText, lit: Boolean);
// paint a line

  procedure DisplayProject(X, Y, pix: Integer);
  begin
    if pix and (cpType or cpImp) = 0 then
      with Tribe[Me].ModelPicture[pix and cpIndex] do
        Sprite(Offscreen, HGr, X, Y, 64, 48, pix mod 10 * 65 + 1,
          pix div 10 * 49 + 1)
    else
    begin
      Frame(Offscreen.Canvas, X + (16 - 1), Y + (16 - 2), X + (16 + xSizeSmall),
        Y + (16 - 1 + ySizeSmall), MainTexture.ColorBevelLight,
        MainTexture.ColorBevelShade);
      if pix and cpType = 0 then
        if (pix and cpIndex = imPalace) and (MyRO.Government <> gAnarchy) then
          BitBltCanvas(Offscreen.Canvas, X + 16, Y + (16 - 1), xSizeSmall,
            ySizeSmall, SmallImp.Canvas, (MyRO.Government - 1) *
            xSizeSmall, ySizeSmall)
        else
          BitBltCanvas(Offscreen.Canvas, X + 16, Y + (16 - 1), xSizeSmall,
            ySizeSmall, SmallImp.Canvas, pix and cpIndex mod 7 *
            xSizeSmall, (pix and cpIndex + SystemIconLines * 7) div 7 *
            ySizeSmall)
      else
        BitBltCanvas(Offscreen.Canvas, X + 16, Y + (16 - 1), xSizeSmall,
          ySizeSmall, SmallImp.Canvas, (3 + pix and cpIndex) *
          xSizeSmall, 0);
    end;
  end;

  procedure ReplaceText(X, Y, Color: Integer; S: string);
  var
    TextSize: TSize;
  begin
    if ca = Canvas then
    begin
      TextSize.cx := BiColorTextWidth(ca, S);
      TextSize.cy := ca.TextHeight(S);
      if Y + TextSize.cy >= TitleHeight + InnerHeight then
        TextSize.cy := TitleHeight + InnerHeight - Y;
      Fill(ca, X, Y, TextSize.cx, TextSize.cy, (Maintexture.Width - ClientWidth)
        div 2, (Maintexture.Height - ClientHeight) div 2);
    end;
    LoweredTextOut(ca, Color, MainTexture, X, Y, S);
  end;

var
  icon, ofs, X, Y, y0, lix, I, J, TextColor, Available, first, Test,
    FutureCount, growth, TrueFood, TrueProd: Integer;
  CityReport: TCityReportNew;
  mox: ^TModelInfo;
  S, number: string;
  CanGrow: Boolean;
begin
  lix := Code[Layer, ScrollBar.Position + L];
  y0 := 2 + (L + 1) * LineDistance;
  if ScrollBar.Position + L >= FirstShrinkedLine[Layer] then
    ofs := (ScrollBar.Position + L - FirstShrinkedLine[Layer]) and 1 * 33
  else { if FirstShrinkedLine[Layer]<Lines[Layer] then }
    ofs := 33;

  if Kind in [kCities, kCityEvents] then
    with MyCity[lix] do
    begin
      X := 104 - 76;
      Y := y0;
      if ca = Canvas then
      begin
        X := X + SideFrame;
        Y := Y + TitleHeight;
      end;
      if lit then
        TextColor := MainTexture.ColorLitText
      else
        TextColor := -1;
      S := CityName(ID);
      while BiColorTextWidth(ca, S) > CityNameSpace do
        Delete(S, Length(S), 1);
      ReplaceText(X + 15, Y, TextColor, S);

      if NonText then
        with Offscreen.Canvas do
        begin // city size
          Brush.Color := $000000;
          FillRect(rect(X - 4 - 11, Y + 1, X - 4 + 13, Y + 21));
          Brush.Color := $FFFFFF;
          FillRect(rect(X - 4 - 12, Y, X - 4 + 12, Y + 20));
          Brush.style := bsClear;
          Font.Color := $000000;
          S := IntToStr(MyCity[lix].Size);
          TextOut(X - 4 - textwidth(S) div 2, Y, S);
        end;

      if Kind = kCityEvents then
      begin
        first := -1;
        for J := 0 to nCityEventPriority - 1 do
          if (Flags and CityRepMask and CityEventPriority[J] <> 0) then
          begin
            first := J;
            Break;
          end;
        if first >= 0 then
        begin
          I := 0;
          Test := 1;
          while Test < CityEventPriority[first] do
          begin
            Inc(I);
            Inc(Test, Test);
          end;
          S := CityEventName(I);
          { if CityEventPriority[first]=chNoGrowthWarning then
            if Built[imAqueduct]=0 then
            S:=Format(S,[Phrases.Lookup('IMPROVEMENTS',imAqueduct)])
            else begin S:=Format(S,[Phrases.Lookup('IMPROVEMENTS',imSewer)]); I:=17 end; }
          ReplaceText(X + (CityNameSpace + 4 + 40 + 18 + 8), Y, TextColor, S);
          if NonText then
          begin
            Sprite(Offscreen, HGrSystem, 105 - 76 + CityNameSpace + 4 + 40,
              y0 + 1, 18, 18, 1 + I mod 3 * 19, 1 + I div 3 * 19);
            X := InnerWidth - 26;
            for J := nCityEventPriority - 1 downto first + 1 do
              if (Flags and CityRepMask and CityEventPriority[J] <> 0) then
              begin
                I := 0;
                Test := 1;
                while Test < CityEventPriority[J] do
                begin
                  Inc(I);
                  Inc(Test, Test);
                end;
                if (CityEventPriority[J] = chNoGrowthWarning) and
                  (Built[imAqueduct] > 0) then
                  I := 17;
                Sprite(Offscreen, HGrSystem, X, y0 + 1, 18, 18,
                  1 + I mod 3 * 19, 1 + I div 3 * 19);
                Dec(X, 20);
              end;
          end;
        end;
      end
      else
      begin
        CityReport.HypoTiles := -1;
        CityReport.HypoTaxRate := -1;
        CityReport.HypoLuxuryRate := -1;
        Server(sGetCityReportNew, Me, lix, CityReport);
        TrueFood := Food;
        TrueProd := Prod;
        if Supervising then
        begin // normalize city from after-turn state
          Dec(TrueFood, CityReport.FoodSurplus);
          if TrueFood < 0 then
            TrueFood := 0; // shouldn't happen
          Dec(TrueProd, CityReport.Production);
          if TrueProd < 0 then
            TrueProd := 0; // shouldn't happen
        end;

        S := ''; // disorder info
        if Flags and chCaptured <> 0 then
          S := Phrases.Lookup('CITYEVENTS', 14)
        else if CityReport.HappinessBalance < 0 then
          S := Phrases.Lookup('CITYEVENTS', 0);
        if S <> '' then
        begin { disorder }
          if NonText then
          begin
            DarkGradient(Offscreen.Canvas, 99 + 31 + CityNameSpace + 4,
              y0 + 2, 131, 3);
            ca.Font.Assign(UniFont[ftSmall]);
            RisedTextout(Offscreen.Canvas, 103 + CityNameSpace + 4 + 31,
              y0 + 1, S);
            ca.Font.Assign(UniFont[ftNormal]);
          end;
        end
        else
        begin
          { s:=IntToStr(CityReport.FoodSurplus);
            ReplaceText(X+(CityNameSpace+4+48)-BiColorTextWidth(ca,S),Y,TextColor,S); }
          S := IntToStr(CityReport.Science);
          ReplaceText(X + CityNameSpace + 4 + 370 + 48 - BiColorTextWidth(ca,
            S), Y, TextColor, S);
          S := IntToStr(CityReport.Production);
          ReplaceText(X + CityNameSpace + 4 + 132 - BiColorTextWidth(ca, S), Y,
            TextColor, S);
          if NonText then
          begin
            // Sprite(offscreen,HGrSystem,x+CityNameSpace+4+333+1,y+6,10,10,66,115);
            Sprite(Offscreen, HGrSystem, X + CityNameSpace + 4 + 370 + 48 + 1,
              Y + 6, 10, 10, 77, 126);
            Sprite(Offscreen, HGrSystem, X + CityNameSpace + 4 + 132 + 1, Y + 6,
              10, 10, 88, 115);
          end;
        end;
        S := IntToStr(CityTaxBalance(lix, CityReport));
        ReplaceText(X + CityNameSpace + 4 + 370 - BiColorTextWidth(ca, S), Y,
          TextColor, S);
        // if Project and (cpImp+cpIndex)<>cpImp+imTrGoods then
        // ReplaceText(x+CityNameSpace+4+333+1,y,TextColor,Format('%d/%d',[TrueProd,CityReport.ProjectCost]));
        if NonText then
        begin
          Sprite(Offscreen, HGrSystem, X + CityNameSpace + 4 + 370 + 1, Y + 6,
            10, 10, 132, 115);

          // food progress
          CanGrow := (Size < MaxCitySize) and (MyRO.Government <> gFuture) and
            (CityReport.FoodSurplus > 0) and
            ((Size < NeedAqueductSize) or (Built[imAqueduct] = 1) and
            (Size < NeedSewerSize) or (Built[imSewer] = 1));
          PaintRelativeProgressBar(Offscreen.Canvas, 1, X + 15 + CityNameSpace +
            4, Y + 7, 68, TrueFood, CutCityFoodSurplus(CityReport.FoodSurplus,
            (MyRO.Government <> gAnarchy) and (Flags and chCaptured = 0),
            MyRO.Government, Size), CityReport.Storage, CanGrow, MainTexture);

          if Project <> cpImp + imTrGoods then
          begin
            DisplayProject(ofs + 104 - 76 + X - 28 + CityNameSpace + 4 + 206 -
              60, y0 - 15, Project);

            // production progress
            growth := CityReport.Production;
            if (growth < 0) or (MyRO.Government = gAnarchy) or
              (Flags and chCaptured <> 0) then
              growth := 0;
            PaintRelativeProgressBar(Offscreen.Canvas, 4,
              X + CityNameSpace + 4 + 304 - 60 + 9, Y + 7, 68, TrueProd, growth,
              CityReport.ProjectCost, True, MainTexture);
          end;
        end;
      end;
    end
  else if Kind in [kModels, kEModels] then
  begin
    X := 104;
    Y := y0;
    if ca = Canvas then
    begin
      X := X + SideFrame;
      Y := Y + TitleHeight;
    end;
    if lit then
      TextColor := MainTexture.ColorLitText
    else
      TextColor := -1;
    if Kind = kModels then
    begin
      Available := 0;
      for J := 0 to MyRO.nUn - 1 do
        if (MyUn[J].Loc >= 0) and (MyUn[J].mix = lix) then
          Inc(Available);
      if MainScreen.mNames.Checked then
        S := Tribe[Me].ModelName[lix]
      else
        S := Format(Tribe[Me].TPhrase('GENMODEL'), [lix]);
      if NonText then
        DisplayProject(8 + ofs, y0 - 15, lix);
    end
    else
    begin
      Available := MyRO.EnemyReport[pView].UnCount[lix];
      if MainScreen.mNames.Checked then
        S := Tribe[pView].ModelName[lix]
      else
        S := Format(Tribe[pView].TPhrase('GENMODEL'), [lix]);
      if NonText then
        with Tribe[pView].ModelPicture[lix] do
          Sprite(Offscreen, HGr, 8 + ofs, y0 - 15, 64, 48, pix mod 10 * 65 + 1,
            pix div 10 * 49 + 1);
    end;
    if Available > 0 then
      ReplaceText(X + 32 - BiColorTextWidth(ca, IntToStr(Available)), Y,
        TextColor, IntToStr(Available));
    ReplaceText(X + 40, Y, TextColor, S);
  end
  else
  begin
    case Kind of
      kAllEModels, kChooseEModel:
        if lix = mixAll then
          S := Phrases.Lookup('PRICECAT_ALLMODEL')
        else
        begin
          mox := @MyRO.EnemyModel[lix];
          if MainScreen.mNames.Checked then
          begin
            S := Tribe[mox.Owner].ModelName[mox.mix];
            if (Kind = kAllEModels) and (Code[1, ScrollBar.Position + L] = 0) then
              S := Format(Tribe[mox.Owner].TPhrase('OWNED'), [S]);
          end
          else
            S := Format(Tribe[mox.Owner].TPhrase('GENMODEL'), [mox.mix]);
          if NonText then
            with Tribe[mox.Owner].ModelPicture[mox.mix] do
              Sprite(Offscreen, HGr, 8 + ofs, y0 - 15, 64, 48,
                pix mod 10 * 65 + 1, pix div 10 * 49 + 1);
        end;
      kChooseModel:
        if lix = mixAll then
          S := Phrases.Lookup('PRICECAT_ALLMODEL')
        else
        begin
          S := Tribe[Me].ModelName[lix];
          if NonText then
            DisplayProject(8 + ofs, y0 - 15, lix);
        end;
      kProject:
        begin
          if lix and cpType <> 0 then
            S := Phrases.Lookup('CITYTYPE', lix and cpIndex)
          else if lix and cpImp = 0 then
            with MyModel[lix and cpIndex] do
            begin
              S := Tribe[Me].ModelName[lix and cpIndex];
              if lix and cpConscripts <> 0 then
                S := Format(Phrases.Lookup('CONSCRIPTS'), [S]);
            end
          else
          begin
            S := Phrases.Lookup('IMPROVEMENTS', lix and cpIndex);
            if (Imp[lix and cpIndex].Kind in [ikNatLocal, ikNatGlobal]) and
              (MyRO.NatBuilt[lix and cpIndex] > 0) or
              (lix and cpIndex in [imPower, imHydro, imNuclear]) and
              (MyCity[cixProject].Built[imPower] + MyCity[cixProject].Built
              [imHydro] + MyCity[cixProject].Built[imNuclear] > 0) then
              S := Format(Phrases.Lookup('NATEXISTS'), [S]);
          end;
          if NonText then
            DisplayProject(8 + ofs, y0 - 15, lix);
        end;
      kAdvance, kFarAdvance, kScience, kChooseTech, kChooseETech, kStealTech:
        begin
          if lix = adAll then
            S := Phrases.Lookup('PRICECAT_ALLTECH')
          else
          begin
            if lix = adNexus then
              S := Phrases.Lookup('NEXUS')
            else if lix = adNone then
              S := Phrases.Lookup('NOFARTECH')
            else if lix = adMilitary then
              S := Phrases.Lookup('INITUNIT')
            else
            begin
              S := Phrases.Lookup('ADVANCES', lix);
              if (Kind = kAdvance) and (lix in FutureTech) then
                if MyRO.Tech[lix] < tsApplicable then
                  S := S + ' 1'
                else
                  S := S + ' ' + IntToStr(MyRO.Tech[lix] + 1);
            end;
            if BiColorTextWidth(ca, S) > TechNameSpace + 8 then
            begin
              repeat
                Delete(S, Length(S), 1);
              until BiColorTextWidth(ca, S) <= TechNameSpace + 5;
              S := S + '.';
            end;

            if NonText then
            begin // show tech icon
              if lix = adNexus then
              begin
                Frame(Offscreen.Canvas, (8 + 16 - 1), y0 - 1, (8 + 16 + 36),
                  y0 + 20, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
                Dump(Offscreen, HGrSystem, (8 + 16), y0, 36, 20, 223, 295)
              end
              else if lix = adNone then
              begin
                Frame(Offscreen.Canvas, (8 + 16 - 1), y0 - 1, (8 + 16 + 36),
                  y0 + 20, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
                Dump(Offscreen, HGrSystem, (8 + 16), y0, 36, 20, 260, 295)
              end
              else if lix = adMilitary then
              begin
                Frame(Offscreen.Canvas, (8 + 16 - 1), y0 - 1, (8 + 16 + 36),
                  y0 + 20, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
                Dump(Offscreen, HGrSystem, (8 + 16), y0, 36, 20, 38, 295)
              end
              else
              begin
                Frame(Offscreen.Canvas, (8 + 16 - 1), y0 - 1,
                  (8 + 16 + xSizeSmall), y0 + ySizeSmall,
                  MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
                if AdvIcon[lix] < 84 then
                  BitBltCanvas(Offscreen.Canvas, (8 + 16), y0, xSizeSmall,
                    ySizeSmall, SmallImp.Canvas,
                    (AdvIcon[lix] + SystemIconLines * 7) mod 7 * xSizeSmall,
                    (AdvIcon[lix] + SystemIconLines * 7) div 7 *
                    ySizeSmall)
                else
                  Dump(Offscreen, HGrSystem, (8 + 16), y0, 36, 20,
                    1 + (AdvIcon[lix] - 84) mod 8 * 37,
                    295 + (AdvIcon[lix] - 84) div 8 * 21);
                J := AdvValue[lix] div 1000;
                BitBltCanvas(Offscreen.Canvas, (8 + 16 - 4), y0 + 2, 14, 14,
                  HGrSystem.Mask.Canvas, 127 + J * 15,
                  85, SRCAND);
                Sprite(Offscreen, HGrSystem, (8 + 16 - 5), y0 + 1, 14, 14,
                  127 + J * 15, 85);
              end;
            end;
          end;

          if NonText and (Kind in [kAdvance, kScience]) then
          begin // show research state
            for J := 0 to nColumn - 1 do
            begin
              FutureCount := 0;
              if J = 0 then // own science
                if lix = MyRO.ResearchTech then
                begin
                  Server(sGetTechCost, Me, 0, icon);
                  icon := 4 + MyRO.Research * 4 div icon;
                  if icon > 4 + 3 then
                    icon := 4 + 3
                end
                else if (lix >= adMilitary) then
                  icon := -1
                else if lix in FutureTech then
                begin
                  icon := -1;
                  FutureCount := MyRO.Tech[lix];
                end
                else if MyRO.Tech[lix] = tsSeen then
                  icon := 1
                else if MyRO.Tech[lix] >= tsApplicable then
                  icon := 2
                else
                  icon := -1
              else
                with MyRO.EnemyReport[Column[J]]^ do // enemy science
                  if (MyRO.Alive and (1 shl Column[J]) <> 0) and
                    (TurnOfCivilReport >= 0) and (lix = ResearchTech) and
                    ((lix = adMilitary) or (lix in FutureTech) or
                    (Tech[lix] < tsApplicable)) then
                  begin
                    icon := 4 + ResearchDone div 25;
                    if icon > 4 + 3 then
                      icon := 4 + 3;
                  end
                  else if lix = adMilitary then
                    icon := -1
                  else if lix in FutureTech then
                  begin
                    icon := -1;
                    FutureCount := Tech[lix]
                  end
                  else if Tech[lix] >= tsApplicable then
                    icon := 2
                  else if Tech[lix] = tsSeen then
                    icon := 1
                  else
                    icon := -1;
              if icon >= 0 then
                Sprite(Offscreen, HGrSystem, 104 - 33 + 15 + 3 + TechNameSpace +
                  24 * J, y0 + 3, 14, 14, 67 + icon * 15, 85)
              else if (Kind = kScience) and (FutureCount > 0) then
              begin
                number := IntToStr(FutureCount);
                RisedTextout(ca, 104 - 33 + 15 + 10 + TechNameSpace + 24 * J -
                  BiColorTextWidth(ca, number) div 2, y0, number);
              end;
            end;
          end;
        end; // kAdvance, kScience
      kTribe:
        S := TribeNames[lix];
      kShipPart:
        begin
          S := Phrases.Lookup('IMPROVEMENTS', imShipComp + lix) + ' (' +
            IntToStr(MyRO.Ship[Me].Parts[lix]) + ')';
          if NonText then
            DisplayProject(8 + ofs, y0 - 15, cpImp + imShipComp + lix);
        end;
      kEShipPart:
        begin
          S := Phrases.Lookup('IMPROVEMENTS', imShipComp + lix) + ' (' +
            IntToStr(MyRO.Ship[DipMem[Me].pContact].Parts[lix]) + ')';
          if NonText then
            DisplayProject(8 + ofs, y0 - 15, cpImp + imShipComp + lix);
        end;
      kGov:
        begin
          S := Phrases.Lookup('GOVERNMENT', lix);
          if NonText then
          begin
            Frame(Offscreen.Canvas, 8 + 16 - 1, y0 - 15 + (16 - 2),
              8 + 16 + xSizeSmall, y0 - 15 + (16 - 1 + ySizeSmall),
              MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
            BitBltCanvas(Offscreen.Canvas, 8 + 16, y0 - 15 + (16 - 1),
              xSizeSmall, ySizeSmall, SmallImp.Canvas,
              (lix - 1) * xSizeSmall, ySizeSmall);
          end;
        end;
      kMission:
        S := Phrases.Lookup('SPYMISSION', lix);
    end;
    case Kind of
      kTribe, kMission: // center text
        if Lines[0] > MaxLines then
          X := (InnerWidth - GetSystemMetrics(SM_CXVSCROLL)) div 2 -
            BiColorTextWidth(ca, S) div 2
        else
          X := InnerWidth div 2 - BiColorTextWidth(ca, S) div 2;
      kAdvance, kFarAdvance, kScience, kChooseTech, kChooseETech,
        kStealTech, kGov:
        X := 104 - 33;
      kAllEModels:
        X := 104;
    else
      X := 104 + 15;
    end;
    Y := y0;
    if ca = Canvas then
    begin
      X := X + SideFrame;
      Y := Y + TitleHeight;
    end;
    if lit then
      TextColor := MainTexture.ColorLitText
    else
      TextColor := -1;
    { if Kind=kTribe then ReplaceText_Tribe(x,y,TextColor,
      Integer(TribeNames.Objects[lix]),S)
      else } ReplaceText(X, Y, TextColor, S);
  end;
end;

procedure TListDlg.OffscreenPaint;
var
  I, J: Integer;
begin
  case Kind of
    kCities:
      Caption := Tribe[Me].TPhrase('TITLE_CITIES');
    kCityEvents:
      Caption := Format(Phrases.Lookup('TITLE_EVENTS'),
        [TurnToString(MyRO.Turn)]);
  end;

  inherited;
  Offscreen.Canvas.Font.Assign(UniFont[ftNormal]);
  FillOffscreen(0, 0, InnerWidth, InnerHeight);
  with Offscreen.Canvas do
  begin
    if Kind = kScience then
      for I := 1 to nColumn - 1 do
      begin
        Pen.Color := $000000;
        MoveTo(104 - 33 + 15 + TechNameSpace + 24 * I, 0);
        LineTo(104 - 33 + 15 + TechNameSpace + 24 * I, InnerHeight);
        MoveTo(104 - 33 + 15 + TechNameSpace + 9 * 2 + 24 * I, 0);
        LineTo(104 - 33 + 15 + TechNameSpace + 9 * 2 + 24 * I, InnerHeight);
        if MyRO.EnemyReport[Column[I]].TurnOfCivilReport >= MyRO.Turn - 1 then
        begin
          Brush.Color := Tribe[Column[I]].Color;
          FillRect(rect(104 - 33 + 14 + TechNameSpace + 24 * I + 1 * 2, 0,
            104 - 33 + 17 + TechNameSpace + 24 * I + 8 * 2, InnerHeight));
          Brush.style := bsClear;
        end
        else
        begin // colored player columns
          Pen.Color := Tribe[Column[I]].Color;
          for J := 1 to 8 do
          begin
            MoveTo(104 - 33 + 15 + TechNameSpace + 24 * I + J * 2, 0);
            LineTo(104 - 33 + 15 + TechNameSpace + 24 * I + J * 2, InnerHeight);
          end;
        end;
      end;

    for I := -1 to DispLines do
      if (I + ScrollBar.Position >= 0) and (I + ScrollBar.Position < Lines[Layer]) then
        Self.Line(Offscreen.Canvas, I, True, False);
  end;
  MarkUsedOffscreen(InnerWidth, 8 + 48 + DispLines * LineDistance);
end;

procedure TListDlg.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  i0, Sel0, iColumn, OldScienceNation, xScreen: Integer;
  S: string;
begin
  Y := Y - TitleHeight;
  i0 := ScrollBar.Position;
  Sel0 := Selected;
  if (X >= SideFrame) and (X < SideFrame + InnerWidth) and (Y >= 0) and
    (Y < InnerHeight) and (Y mod LineDistance >= 4) and (Y mod LineDistance < 20)
  then
    Selected := Y div LineDistance - 1
  else
    Selected := -2;
  if (Selected < -1) or (Selected > DispLines) or (Selected + i0 < 0) or
    (Selected + i0 >= Lines[Layer]) then
    Selected := -2;
  if Selected <> Sel0 then
  begin
    if Sel0 <> -2 then
      Line(Canvas, Sel0, False, False);
    if Selected <> -2 then
      Line(Canvas, Selected, False, True);
  end;

  if Kind = kScience then
  begin // show nation under cursor position
    OldScienceNation := ScienceNation;
    ScienceNation := -1;
    if (X >= SideFrame + (104 - 33 + 15 + TechNameSpace)) and
      ((X - SideFrame - (104 - 33 + 15 + TechNameSpace)) mod 24 <= 18) and
      (Y >= 0) and (Y < InnerHeight) then
    begin
      iColumn := (X - SideFrame - (104 - 33 + 15 + TechNameSpace)) div 24;
      if (iColumn >= 1) and (iColumn < nColumn) then
        ScienceNation := Column[iColumn];
    end;
    if ScienceNation <> OldScienceNation then
    begin
      Fill(Canvas, 9, ClientHeight - 29, ClientWidth - 18, 24,
        (Maintexture.Width - ClientWidth) div 2,
        (Maintexture.Height - ClientHeight) div 2);
      if ScienceNation >= 0 then
      begin
        S := Tribe[ScienceNation].TPhrase('SHORTNAME');
        if MyRO.Alive and (1 shl ScienceNation) = 0 then
          S := Format(Phrases.Lookup('SCIENCEREPORT_EXTINCT'), [S]) // extinct
        else if MyRO.EnemyReport[ScienceNation].TurnOfCivilReport < MyRO.Turn - 1
        then
          S := S + ' (' + TurnToString(MyRO.EnemyReport[ScienceNation]
            .TurnOfCivilReport) + ')'; // old report
        xScreen := (ClientWidth - BiColorTextWidth(Canvas, S)) div 2;
        LoweredTextOut(Canvas, -1, MainTexture, xScreen + 10,
          ClientHeight - 29, S);
        BitBltCanvas(ScienceNationDotBuffer.Canvas, 0, 0, ScienceNationDot.Width,
          ScienceNationDot.Height, Canvas, xScreen - 10, ClientHeight - 27);
        ImageOp_BCC(ScienceNationDotBuffer, Templates.Data, Point(0, 0),
          ScienceNationDot.BoundsRect, MainTexture.ColorBevelShade, Tribe[ScienceNation].Color);
        BitBltCanvas(Canvas, xScreen - 10, ClientHeight - 27, ScienceNationDot.Width,
          ScienceNationDot.Height, ScienceNationDotBuffer.Canvas, 0, 0);
      end;
    end;
  end;
end;

procedure TListDlg.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ScrollBar.ProcessMouseWheel(WheelDelta) then begin
    PaintBox1MouseMove(nil, [], MousePos.X - Left,
      MousePos.Y - Top);
  end;
end;

procedure TListDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Gtk2Fix;
end;

function TListDlg.RenameCity(cix: Integer): Boolean;
var
  CityNameInfo: TCityNameInfo;
begin
  InputDlg.Caption := Phrases.Lookup('TITLE_CITYNAME');
  InputDlg.EInput.Text := CityName(MyCity[cix].ID);
  InputDlg.CenterToRect(BoundsRect);
  InputDlg.ShowModal;
  if (InputDlg.ModalResult = mrOK) and (InputDlg.EInput.Text <> '') and
    (InputDlg.EInput.Text <> CityName(MyCity[cix].ID)) then
  begin
    CityNameInfo.ID := MyCity[cix].ID;
    CityNameInfo.NewName := InputDlg.EInput.Text;
    if CityNameInfo.GetCommandDataSize > CommandDataMaxSize then
      Delete(CityNameInfo.NewName, Length(CityNameInfo.NewName) -
        (CityNameInfo.GetCommandDataSize - 1 - CommandDataMaxSize), MaxInt);
    Server(CommandWithData(cSetCityName, CityNameInfo.GetCommandDataSize),
      Me, 0, CityNameInfo);
    if CityDlg.Visible then
    begin
      CityDlg.FormShow(nil);
      CityDlg.Invalidate;
    end;
    Result := True;
  end
  else
    Result := False;
end;

function TListDlg.RenameModel(mix: Integer): Boolean;
var
  ModelNameInfo: TModelNameInfo;
begin
  InputDlg.Caption := Phrases.Lookup('TITLE_MODELNAME');
  InputDlg.EInput.Text := Tribe[Me].ModelName[mix];
  InputDlg.CenterToRect(BoundsRect);
  InputDlg.ShowModal;
  if (InputDlg.ModalResult = mrOK) and (InputDlg.EInput.Text <> '') and
    (InputDlg.EInput.Text <> Tribe[Me].ModelName[mix]) then
  begin
    ModelNameInfo.mix := mix;
    ModelNameInfo.NewName := InputDlg.EInput.Text;
    if ModelNameInfo.GetCommandDataSize > CommandDataMaxSize then
      Delete(ModelNameInfo.NewName, Length(ModelNameInfo.NewName) -
        (ModelNameInfo.GetCommandDataSize - 1 - CommandDataMaxSize), MaxInt);
    Server(CommandWithData(cSetModelName, ModelNameInfo.GetCommandDataSize),
      Me, 0, ModelNameInfo);
    if UnitStatDlg.Visible then
    begin
      UnitStatDlg.FormShow(nil);
      UnitStatDlg.Invalidate;
    end;
    Result := True;
  end
  else
    Result := False;
end;

procedure TListDlg.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lix: Integer;
begin
  if ScrollBar.Position + Selected >= 0 then
    lix := Code[Layer, ScrollBar.Position + Selected];
  if Kind in [kScience, kCities, kCityEvents, kModels, kEModels, kAllEModels]
  then
    Include(Shift, ssShift); // don't close list window
  if (ssLeft in Shift) and not(ssShift in Shift) then
  begin
    if Selected <> -2 then
    begin
      Result := lix;
      Closable := True;
      Close;
    end;
  end
  else if (ssLeft in Shift) and (ssShift in Shift) then
  begin // show help/info popup
    if Selected <> -2 then
      case Kind of
        kCities:
          MainScreen.ZoomToCity(MyCity[lix].Loc);
        kCityEvents:
          MainScreen.ZoomToCity(MyCity[lix].Loc, False, MyCity[lix].Flags and
            CityRepMask);
        kModels, kChooseModel:
          if lix <> mixAll then
            UnitStatDlg.ShowNewContent_OwnModel(wmPersistent, lix);
        kEModels:
          UnitStatDlg.ShowNewContent_EnemyModel(wmPersistent,
            Code[1, ScrollBar.Position + Selected]);
        kAllEModels, kChooseEModel:
          if lix <> mixAll then
            UnitStatDlg.ShowNewContent_EnemyModel(wmPersistent, lix);
        kAdvance, kFarAdvance, kScience, kChooseTech, kChooseETech, kStealTech:
          if lix = adMilitary then
            HelpDlg.ShowNewContent(wmPersistent, hkText,
              HelpDlg.TextIndex('MILRES'))
          else if lix < adMilitary then
            HelpDlg.ShowNewContent(wmPersistent, hkAdv, lix);
        kProject:
          if lix = cpImp + imTrGoods then
            HelpDlg.ShowNewContent(wmPersistent, hkText,
              HelpDlg.TextIndex('TRADINGGOODS'))
          else if lix and (cpImp + cpType) = 0 then
            UnitStatDlg.ShowNewContent_OwnModel(wmPersistent,
              lix and cpIndex)
          else if (lix and cpType = 0) and (lix <> cpImp + imTrGoods) then
            HelpDlg.ShowNewContent(wmPersistent, hkImp,
              lix and cpIndex);
        kGov:
          HelpDlg.ShowNewContent(wmPersistent, hkMisc,
            miscGovList);
        kShipPart, kEShipPart:
          ;
      end;
  end
  else if ssRight in Shift then
  begin
    if Selected <> -2 then
      case Kind of
        kCities, kCityEvents:
          if RenameCity(lix) then
            SmartUpdateContent;
        kModels:
          if RenameModel(lix) then
            SmartUpdateContent;
      end;
  end;
end;

procedure TListDlg.InitLines;
var
  required: array [0 .. nAdv - 1] of Integer;

  procedure TryAddImpLine(Layer, Project: Integer);
  begin
    if Server(sSetCityProject - sExecute, Me, cixProject, Project) >= rExecuted
    then
    begin
      Code[Layer, Lines[Layer]] := Project;
      Inc(Lines[Layer]);
    end;
  end;

  procedure SortTechs;
  var
    I, J, swap: Integer;
  begin // sort by advancedness
    for I := 0 to Lines[0] - 2 do
      if Code[0, I] < adMilitary then
        for J := I + 1 to Lines[0] - 1 do
          if AdvValue[Code[0, I]] * nAdv + Code[0, I] < AdvValue[Code[0, J]] *
            nAdv + Code[0, J] then
          begin
            swap := Code[0, I];
            Code[0, I] := Code[0, J];
            Code[0, J] := swap;
          end;
  end;

  procedure SortCities;
  var
    I, J, swap: Integer;
  begin
    for I := 0 to Lines[0] - 2 do
      for J := I + 1 to Lines[0] - 1 do
        if CityName(MyCity[Code[0, I]].ID) > CityName(MyCity[Code[0, J]].ID)
        then
        begin
          swap := Code[0, I];
          Code[0, I] := Code[0, J];
          Code[0, J] := swap;
        end;
  end;

  function ModelSortValue(const mi: TModelInfo;
    MixPlayers: Boolean = False): Integer;
  begin
    Result := (mi.Domain + 1) shl 28 - mi.mix;
    if MixPlayers then
      Dec(Result, ModelCode(mi) shl 16);
  end;

  procedure SortModels;
  var
    I, J, swap: Integer;
  begin // sort by code[2]
    for I := 0 to Lines[0] - 2 do
      for J := I + 1 to Lines[0] - 1 do
        if Code[2, I] > Code[2, J] then
        begin
          swap := Code[0, I];
          Code[0, I] := Code[0, J];
          Code[0, J] := swap;
          swap := Code[1, I];
          Code[1, I] := Code[1, J];
          Code[1, J] := swap;
          swap := Code[2, I];
          Code[2, I] := Code[2, J];
          Code[2, J] := swap;
        end;
  end;

  procedure MarkPreqs(I: Integer);
  begin
    required[I] := 1;
    if MyRO.Tech[I] < tsSeen then
    begin
      if (AdvPreq[I, 0] >= 0) then
        MarkPreqs(AdvPreq[I, 0]);
      if (AdvPreq[I, 1] >= 0) then
        MarkPreqs(AdvPreq[I, 1]);
    end;
  end;

var
  Loc1, I, J, p1, dx, dy, mix, emix, EnemyType, TestEnemyType: Integer;
  mi: TModelInfo;
  PPicture, PTestPicture: ^TModelPicture;
  ModelOk: array [0 .. 4095] of Boolean;
  ok: Boolean;
begin
  for I := 0 to MaxLayer - 1 do
  begin
    Lines[I] := 0;
    FirstShrinkedLine[I] := MaxInt;
  end;
  case Kind of
    kProject:
      begin
        // improvements
        Code[0, 0] := cpImp + imTrGoods;
        Lines[0] := 1;
        for I := nWonder to nImp - 1 do
          if Imp[I].Kind = ikCommon then
            TryAddImpLine(0, I + cpImp);
        for I := nWonder to nImp - 1 do
          if not(Imp[I].Kind in [ikCommon, ikTrGoods]) and
            ((MyRO.NatBuilt[I] = 0) or (Imp[I].Kind = ikNatLocal)) then
            TryAddImpLine(0, I + cpImp);
        for I := 0 to nCityType - 1 do
          if MyData.ImpOrder[I, 0] >= 0 then
          begin
            Code[0, Lines[0]] := cpType + I;
            Inc(Lines[0]);
          end;

        // wonders
        for I := 0 to nWonder - 1 do
          TryAddImpLine(1, I + cpImp);

        // units
        for I := 0 to MyRO.nModel - 1 do
        begin
          { if MyModel[i].Kind=mkSlaves then
            ok:= MyRO.Wonder[woPyramids].EffectiveOwner=Me
            else } if MyModel[I].Domain = dSea then
          begin
            ok := False;
            for dx := -2 to 2 do
              for dy := -2 to 2 do
                if abs(dx) + abs(dy) = 2 then
                begin
                  Loc1 := dLoc(MyCity[cixProject].Loc, dx, dy);
                  if (Loc1 >= 0) and (Loc1 < G.lx * G.ly) and
                    ((MyMap[Loc1] and fTerrain = fShore) or
                    (MyMap[Loc1] and fCanal > 0)) then
                    ok := True;
                end;
          end
          else
            ok := True;
          if ok then
          begin
            if MyModel[I].Status and msObsolete = 0 then
            begin
              Code[2, Lines[2]] := I;
              Inc(Lines[2]);
            end;
            if MyModel[I].Status and msAllowConscripts <> 0 then
            begin
              Code[2, Lines[2]] := I + cpConscripts;
              Inc(Lines[2]);
            end;
          end;
        end;
        FirstShrinkedLine[2] := 0;
      end;
    kAdvance:
      begin
        nColumn := 1;
        if MyData.FarTech <> adNone then
        begin
          FillChar(required, SizeOf(required), 0);
          MarkPreqs(MyData.FarTech);
        end;
        for I := 0 to nAdv - 1 do
          if ((I in FutureTech) or (MyRO.Tech[I] < tsApplicable)) and
            (Server(sSetResearch - sExecute, Me, I, nil^) >= rExecuted) and
            ((MyData.FarTech = adNone) or (required[I] > 0)) then
          begin
            Code[0, Lines[0]] := I;
            Inc(Lines[0]);
          end;
        SortTechs;
        if Lines[0] = 0 then // no more techs -- offer nexus
        begin
          Code[0, Lines[0]] := adNexus;
          Inc(Lines[0]);
        end;
        ok := False;
        for I := 0 to nDomains - 1 do
          if (upgrade[I, 0].Preq = preNone) or
            (MyRO.Tech[upgrade[I, 0].Preq] >= tsApplicable) then
            ok := True;
        if ok then { new unit class }
        begin
          Code[0, Lines[0]] := adMilitary;
          Inc(Lines[0]);
        end;
      end;
    kFarAdvance:
      begin
        Code[0, Lines[0]] := adNone;
        Inc(Lines[0]);
        for I := 0 to nAdv - 1 do
          if not(I in FutureTech) and (MyRO.Tech[I] < tsApplicable) and
            ((AdvValue[I] < 2000) or (MyRO.Tech[adMassProduction] > tsNA)) and
            ((AdvValue[I] < 1000) or (MyRO.Tech[adScience] > tsNA)) then
          begin
            Code[0, Lines[0]] := I;
            Inc(Lines[0]);
          end;
        SortTechs;
      end;
    kChooseTech:
      begin
        for I := 0 to nAdv - 1 do
          if not(I in FutureTech) and (MyRO.Tech[I] >= tsApplicable) and
            (MyRO.EnemyReport[DipMem[Me].pContact].Tech[I] < tsSeen) then
          begin
            Code[0, Lines[0]] := I;
            Inc(Lines[0]);
          end;
        SortTechs;
        // if Lines[0]>1 then
        begin
          Code[0, Lines[0]] := adAll;
          Inc(Lines[0]);
        end;
      end;
    kChooseETech:
      begin
        for I := 0 to nAdv - 1 do
          if not(I in FutureTech) and (MyRO.Tech[I] < tsSeen) and
            (MyRO.EnemyReport[DipMem[Me].pContact].Tech[I] >= tsApplicable) then
          begin
            Code[0, Lines[0]] := I;
            Inc(Lines[0]);
          end;
        SortTechs;
        // if Lines[0]>1 then
        begin
          Code[0, Lines[0]] := adAll;
          Inc(Lines[0]);
        end;
      end;
    kStealTech:
      begin
        for I := 0 to nAdv - 1 do
          if Server(sStealTech - sExecute, Me, I, nil^) >= rExecuted then
          begin
            Code[0, Lines[0]] := I;
            Inc(Lines[0]);
          end;
        SortTechs;
      end;
    kScience:
      begin
        Column[0] := Me;
        nColumn := 1;
        for EnemyType := 0 to 2 do
          for p1 := 0 to nPl - 1 do
            if (MyRO.EnemyReport[p1] <> nil) and
              ((MyRO.EnemyReport[p1].TurnOfContact >= 0) or
              (MyRO.EnemyReport[p1].TurnOfCivilReport >= 0)) then
            begin
              if MyRO.Alive and (1 shl p1) = 0 then
                TestEnemyType := 2 // extinct enemy -- move to right end
              else if MyRO.EnemyReport[p1].TurnOfCivilReport >= MyRO.Turn - 1
              then
                TestEnemyType := 0 // current report -- move to left end
              else
                TestEnemyType := 1;
              if TestEnemyType = EnemyType then
              begin
                Column[nColumn] := p1;
                Inc(nColumn);
              end;
            end;
        for I := 0 to nAdv - 1 do
        begin
          ok := (MyRO.Tech[I] <> tsNA) or (MyRO.ResearchTech = I);
          for J := 1 to nColumn - 1 do
            with MyRO.EnemyReport[Column[J]]^ do
              if (Tech[I] <> tsNA) or (TurnOfCivilReport >= 0) and
                (ResearchTech = I) then
                ok := True;
          if ok then
          begin
            Code[0, Lines[0]] := I;
            Inc(Lines[0]);
          end;
        end;
        SortTechs;

        ok := MyRO.ResearchTech = adMilitary;
        for J := 1 to nColumn - 1 do
          with MyRO.EnemyReport[Column[J]]^ do
            if (MyRO.Alive and (1 shl Column[J]) <> 0) and
              (TurnOfCivilReport >= 0) and (ResearchTech = adMilitary) then
              ok := True;
        if ok then
        begin
          Code[0, Lines[0]] := adMilitary;
          Inc(Lines[0]);
        end
      end;
    kCities { , kChooseCity } :
      begin
        if ClientMode < scContact then
          for I := 0 to MyRO.nCity - 1 do
            if MyCity[I].Loc >= 0 then
            begin
              Code[0, Lines[0]] := I;
              Inc(Lines[0]);
            end;
        SortCities;
        FirstShrinkedLine[0] := 0
      end;
    kCityEvents:
      begin
        for I := 0 to MyRO.nCity - 1 do
          if (MyCity[I].Loc >= 0) and (MyCity[I].Flags and CityRepMask <> 0)
          then
          begin
            Code[0, Lines[0]] := I;
            Inc(Lines[0]);
          end;
        SortCities;
        FirstShrinkedLine[0] := 0;
      end;
    { kChooseECity:
      begin
      for I:=0 to MyRO.nEnemyCity-1 do
      if (MyRO.EnemyCity[I].Loc>=0)
      and (MyRO.EnemyCity[I].owner=DipMem[Me].pContact) then
      begin Code[0,Lines[0]]:=I; Inc(Lines[0]); end;
      FirstShrinkedLine:=0
      end; }
    kModels:
      begin
        for mix := 0 to MyRO.nModel - 1 do
        begin
          Code[0, mix] := mix;
          MakeModelInfo(Me, mix, MyModel[mix], mi);
          Code[2, mix] := ModelSortValue(mi);
        end;
        Lines[0] := MyRO.nModel;
        SortModels;
        FirstShrinkedLine[0] := 0;
      end;
    kChooseModel:
      begin
        for mix := 3 to MyRO.nModel - 1 do
        begin // check if opponent already has this model
          MakeModelInfo(Me, mix, MyModel[mix], mi);
          ok := True;
          for emix := 0 to MyRO.nEnemyModel - 1 do
            if (MyRO.EnemyModel[emix].Owner = DipMem[Me].pContact) and
              IsSameModel(MyRO.EnemyModel[emix], mi) then
              ok := False;
          if ok then
          begin
            Code[0, Lines[0]] := mix;
            MakeModelInfo(Me, mix, MyModel[mix], mi);
            Code[2, Lines[0]] := ModelSortValue(mi);
            Inc(Lines[0]);
          end;
        end;
        SortModels;
        // if Lines[0]>1 then
        begin
          Code[0, Lines[0]] := mixAll;
          Inc(Lines[0]);;
        end;
        FirstShrinkedLine[0] := 0;
      end;
    kChooseEModel:
      begin
        if MyRO.TestFlags and tfUncover <> 0 then
          Server(sGetModels, Me, 0, nil^);
        for emix := 0 to MyRO.nEnemyModel - 1 do
          ModelOk[emix] := MyRO.EnemyModel[emix].Owner = DipMem[Me].pContact;
        for mix := 0 to MyRO.nModel - 1 do
        begin // don't list models I already have
          MakeModelInfo(Me, mix, MyModel[mix], mi);
          for emix := 0 to MyRO.nEnemyModel - 1 do
            ModelOk[emix] := ModelOk[emix] and
              not IsSameModel(MyRO.EnemyModel[emix], mi);
        end;
        for emix := 0 to MyRO.nEnemyModel - 1 do
          if ModelOk[emix] then
          begin
            if not Assigned(Tribe[DipMem[Me].pContact].ModelPicture
              [MyRO.EnemyModel[emix].mix].HGr) then
              InitEnemyModel(emix);
            Code[0, Lines[0]] := emix;
            Code[2, Lines[0]] := ModelSortValue(MyRO.EnemyModel[emix]);
            Inc(Lines[0]);
          end;
        SortModels;
        // if not IsMilReportNew(DipMem[me].pContact) or (Lines[0]>1) then
        begin
          Code[0, Lines[0]] := mixAll;
          Inc(Lines[0]);
        end;
        FirstShrinkedLine[0] := 0;
      end;
    kEModels:
      begin
        for I := 0 to MyRO.EnemyReport[pView].nModelCounted - 1 do
        begin
          Code[1, Lines[0]] := MyRO.nEnemyModel - 1;
          while (Code[1, Lines[0]] >= 0) and
            not((MyRO.EnemyModel[Code[1, Lines[0]]].Owner = pView) and
            (MyRO.EnemyModel[Code[1, Lines[0]]].mix = I)) do
            Dec(Code[1, Lines[0]]);
          if not Assigned(Tribe[pView].ModelPicture[I].HGr) then
            InitEnemyModel(Code[1, Lines[0]]);
          Code[0, Lines[0]] := I;
          Code[2, Lines[0]] :=
            ModelSortValue(MyRO.EnemyModel[Code[1, Lines[0]]]);
          Inc(Lines[0]);
        end;
        SortModels;
        FirstShrinkedLine[0] := 0;
      end;
    kAllEModels:
      begin
        if (MyRO.TestFlags and tfUncover <> 0) or (G.Difficulty[Me] = 0) then
          Server(sGetModels, Me, 0, nil^);
        for emix := 0 to MyRO.nEnemyModel - 1 do
          if (MyRO.EnemyModel[emix].mix >= 3) and
            (MyRO.EnemyModel[emix].Kind in [mkSelfDeveloped, mkEnemyDeveloped])
          then
          begin
            PPicture := @Tribe[MyRO.EnemyModel[emix].Owner].ModelPicture
              [MyRO.EnemyModel[emix].mix];
            if not Assigned(PPicture.HGr) then
              InitEnemyModel(emix);
            ok := True;
            if MainScreen.mNames.Checked then
              for J := 0 to Lines[0] - 1 do
              begin
                PTestPicture := @Tribe[MyRO.EnemyModel[Code[0, J]].Owner]
                  .ModelPicture[MyRO.EnemyModel[Code[0, J]].mix];
                if (PPicture.HGr = PTestPicture.HGr) and
                  (PPicture.pix = PTestPicture.pix) and
                  (ModelHash(MyRO.EnemyModel[emix])
                  = ModelHash(MyRO.EnemyModel[Code[0, J]])) then
                begin
                  Code[1, J] := 1;
                  ok := False;
                  Break;
                end;
              end;
            if ok then
            begin
              Code[0, Lines[0]] := emix;
              Code[1, Lines[0]] := 0;
              Code[2, Lines[0]] := ModelSortValue(MyRO.EnemyModel[emix], True);
              Inc(Lines[0]);
            end;
          end;
        SortModels;
        FirstShrinkedLine[0] := 0
      end;
    kTribe:
      for I := 0 to TribeNames.Count - 1 do
      begin
        Code[0, Lines[0]] := I;
        Inc(Lines[0]);
      end;
    (* kDeliver:
      if MyRO.Treaty[DipMem[Me].pContact]<trAlliance then
      begin // suggest next treaty level
      Code[0,Lines[0]]:=opTreaty+MyRO.Treaty[DipMem[Me].pContact]+1;
      Inc(Lines[0]);
      end;
      if MyRO.Treaty[DipMem[Me].pContact]=trNone then
      begin // suggest peace
      Code[0,Lines[0]]:=opTreaty+trPeace;
      Inc(Lines[0]);
      end;
      if MyRO.Treaty[DipMem[Me].pContact]>trNone then
      begin // suggest next treaty level
      Code[0,Lines[0]]:=opTreaty+MyRO.Treaty[DipMem[Me].pContact]-1;
      Inc(Lines[0]);
      end; *)
    kShipPart:
      begin
        Lines[0] := 0;
        for I := 0 to nShipPart - 1 do
          if MyRO.Ship[Me].Parts[I] > 0 then
          begin
            Code[0, Lines[0]] := I;
            Inc(Lines[0]);
          end;
      end;
    kEShipPart:
      begin
        Lines[0] := 0;
        for I := 0 to nShipPart - 1 do
          if MyRO.Ship[DipMem[Me].pContact].Parts[I] > 0 then
          begin
            Code[0, Lines[0]] := I;
            Inc(Lines[0]);
          end;
      end;
    kGov:
      for I := 1 to nGov - 1 do
        if (GovPreq[I] <> preNA) and
          ((GovPreq[I] = preNone) or (MyRO.Tech[GovPreq[I]] >= tsApplicable))
        then
        begin
          Code[0, Lines[0]] := I;
          Inc(Lines[0]);
        end;
    kMission:
      for I := 0 to nSpyMission - 1 do
      begin
        Code[0, Lines[0]] := I;
        Inc(Lines[0]);
      end;
  end;

  if Kind = kProject then // test if choice fitting to one screen
    if Lines[0] + Lines[1] + Lines[2] <= MaxLines then
    begin
      for I := 0 to Lines[1] - 1 do // add wonders to first page
      begin
        Code[0, Lines[0]] := Code[1, I];
        Inc(Lines[0]);
      end;
      Lines[1] := 0;
      FirstShrinkedLine[0] := Lines[0];
      for I := 0 to Lines[2] - 1 do // add models to first page
      begin
        Code[0, Lines[0]] := Code[2, I];
        Inc(Lines[0]);
      end;
      Lines[2] := 0;
    end;
end;

function TListDlg.OnlyChoice(TestKind: TListKind): Integer;
begin
  Kind := TestKind;
  InitLines;
  if Lines[0] = 0 then
    Result := -2
  else if Lines[0] > 1 then
    Result := -1
  else
    Result := Code[0, 0];
end;

procedure TListDlg.FormShow(Sender: TObject);
var
  I: Integer;
begin
  Result := -1;
  Closable := False;

  if Kind = kTribe then
  begin
    LineDistance := 21; // looks ugly with scrollbar
    MaxLines := (Maintexture.Height - (24 + TitleHeight + NarrowFrame))
      div LineDistance - 1;
  end
  else
  begin
    LineDistance := 24;
    MaxLines := (Maintexture.Height - (24 + TitleHeight + WideFrame))
      div LineDistance - 1;
  end;
  InitLines;

  MultiPage := False;
  for I := 1 to MaxLayer - 1 do
    if Lines[I] > 0 then
      MultiPage := True;
  WideBottom := MultiPage or (Kind = kScience) or
    not Phrases2FallenBackToEnglish and
    (Kind in [kProject, kAdvance, kFarAdvance]);
  if (Kind = kAdvance) and (MyData.FarTech <> adNone) or (Kind = kModels) or
    (Kind = kEModels) then begin
    ScrollBar.SetBorderSpacing(56, 10, 10);
    TitleHeight := WideFrame + 20;
  end else begin
    ScrollBar.SetBorderSpacing(36, 10, 34);
    TitleHeight := WideFrame;
  end;

  DispLines := Lines[0];
  for I := 0 to MaxLayer - 1 do
    if Lines[I] > DispLines then
      DispLines := Lines[I];
  if WideBottom then
  begin
    if DispLines > MaxLines then
      DispLines := MaxLines;
    InnerHeight := LineDistance * (DispLines + 1) + 24;
    ClientHeight := InnerHeight + TitleHeight + WideFrame;
  end
  else
  begin
    if DispLines > MaxLines then
      DispLines := MaxLines;
    InnerHeight := LineDistance * (DispLines + 1) + 24;
    ClientHeight := InnerHeight + TitleHeight + NarrowFrame;
  end;
  Assert(ClientHeight <= Maintexture.Height);

  TechNameSpace := 224;
  case Kind of
    kGov:
      InnerWidth := 272;
    kCities, kCityEvents:
      InnerWidth := 640 - 18;
    kTribe:
      if Lines[0] > MaxLines then
        InnerWidth := 280 + GetSystemMetrics(SM_CXVSCROLL)
      else
        InnerWidth := 280;
    kScience:
      begin
        InnerWidth := 104 - 33 + 15 + 8 + TechNameSpace + 24 * nColumn +
          GetSystemMetrics(SM_CXVSCROLL);
        if InnerWidth + 2 * SideFrame > 640 then
        begin
          TechNameSpace := TechNameSpace + 640 - InnerWidth - 2 * SideFrame;
          InnerWidth := 640 - 2 * SideFrame
        end;
      end;
    kAdvance, kFarAdvance:
      InnerWidth := 104 - 33 + 15 + 8 + TechNameSpace + 24 +
        GetSystemMetrics(SM_CXVSCROLL);
    kChooseTech, kChooseETech, kStealTech:
      InnerWidth := 104 - 33 + 15 + 8 + TechNameSpace +
        GetSystemMetrics(SM_CXVSCROLL);
  else
    InnerWidth := 363;
  end;
  ClientWidth := InnerWidth + 2 * SideFrame;

  CloseBtn.Left := ClientWidth - 38;
  CaptionLeft := ToggleBtn.Left + ToggleBtn.Width;
  CaptionRight := CloseBtn.Left;
  { TODO:
  SetWindowPos(ScrollBar.ScrollBar.Handle, 0, SideFrame + InnerWidth - GetSystemMetrics(SM_CXVSCROLL),
    TitleHeight, GetSystemMetrics(SM_CXVSCROLL), LineDistance * DispLines + 48,
    SWP_NOZORDER or SWP_NOREDRAW);
  }

  if WindowMode = wmModal then
  begin { center on screen }
    if Kind = kTribe then
      Left := (Screen.Width - 800) * 3 div 8 + 130
    else
      Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
    if Kind = kProject then
      Top := Top + 48;
  end;

  Layer0Btn.Visible := MultiPage and (Lines[0] > 0);
  Layer1Btn.Visible := MultiPage and (Lines[1] > 0);
  Layer2Btn.Visible := MultiPage and (Lines[2] > 0);
  if Kind = kProject then
  begin
    Layer0Btn.Top := ClientHeight - 31;
    Layer0Btn.Left := ClientWidth div 2 - (12 + 29);
    Layer0Btn.Down := True;
    Layer1Btn.Top := ClientHeight - 31;
    Layer1Btn.Left := ClientWidth div 2 - (12 - 29);
    Layer1Btn.Down := False;
    Layer2Btn.Top := ClientHeight - 31;
    Layer2Btn.Left := ClientWidth div 2 - 12;
    Layer2Btn.Down := False;
  end;

  Layer := 0;
  Selected := -2;
  ScienceNation := -1;
  ScrollBar.Init(Lines[Layer] - 1, DispLines);

  OffscreenPaint;
end;

procedure TListDlg.ShowNewContent(NewMode: TWindowMode; ListKind: TListKind);
var
  I: Integer;
  ShowFocus, forceclose: Boolean;
begin
  forceclose := (ListKind <> Kind) and
    not((Kind = kCities) and (ListKind = kCityEvents)) and
    not((Kind = kCityEvents) and (ListKind = kCities)) and
    not((Kind = kModels) and (ListKind = kEModels)) and
    not((Kind = kEModels) and (ListKind = kModels));

  Kind := ListKind;
  ModalIndication := not(Kind in MustChooseKind);
  case Kind of
    kProject:
      Caption := Phrases.Lookup('TITLE_PROJECT');
    kAdvance:
      Caption := Phrases.Lookup('TITLE_TECHSELECT');
    kFarAdvance:
      Caption := Phrases.Lookup('TITLE_FARTECH');
    kModels, kEModels:
      Caption := Phrases.Lookup('FRMILREP');
    kAllEModels:
      Caption := Phrases.Lookup('TITLE_EMODELS');
    kTribe:
      Caption := Phrases.Lookup('TITLE_TRIBE');
    kScience:
      Caption := Phrases.Lookup('TITLE_SCIENCE');
    kShipPart, kEShipPart:
      Caption := Phrases.Lookup('TITLE_CHOOSESHIPPART');
    kChooseTech, kChooseETech:
      Caption := Phrases.Lookup('TITLE_CHOOSETECH');
    kChooseModel, kChooseEModel:
      Caption := Phrases.Lookup('TITLE_CHOOSEMODEL');
    kStealTech:
      Caption := Phrases.Lookup('TITLE_CHOOSETECH');
    kGov:
      Caption := Phrases.Lookup('TITLE_GOV');
    kMission:
      Caption := Phrases.Lookup('TITLE_SPYMISSION');
  end;

  case Kind of
    kMission:
      HelpContext := 'SPYMISSIONS';
  else
    HelpContext := 'CONCEPTS'
  end;

  if Kind = kAdvance then
  begin
    ToggleBtn.ButtonIndex := 13;
    ToggleBtn.Hint := Phrases.Lookup('FARTECH');
  end
  else if Kind = kCities then
  begin
    ToggleBtn.ButtonIndex := 15;
    ToggleBtn.Hint := Phrases.Lookup('BTN_PAGE');
  end
  else
  begin
    ToggleBtn.ButtonIndex := 28;
    ToggleBtn.Hint := Phrases.Lookup('BTN_SELECT');
  end;

  if Kind = kAdvance then // show focus button?
    if MyData.FarTech <> adNone then
      ShowFocus := True
    else
    begin
      ShowFocus := False;
      for I := 0 to nAdv - 1 do
        if not(I in FutureTech) and (MyRO.Tech[I] < tsApplicable) and
          ((AdvValue[I] < 2000) or (MyRO.Tech[adMassProduction] > tsNA)) and
          ((AdvValue[I] < 1000) or (MyRO.Tech[adScience] > tsNA)) and
          (Server(sSetResearch - sExecute, Me, I, nil^) < rExecuted) then
          ShowFocus := True;
    end;
  ToggleBtn.Visible := (Kind = kCities) and not Supervising or (Kind = kAdvance)
    and ShowFocus or (Kind = kModels) or (Kind = kEModels);
  CloseBtn.Visible := not(Kind in MustChooseKind);

  inherited ShowNewContent(NewMode, forceclose);
end;

procedure TListDlg.ShowNewContent_CityProject(NewMode: TWindowMode; cix: Integer);
begin
  cixProject := cix;
  ShowNewContent(NewMode, kProject);
end;

procedure TListDlg.ShowNewContent_MilReport(NewMode: TWindowMode; P: Integer);
begin
  pView := P;
  if P = Me then
    ShowNewContent(NewMode, kModels)
  else
    ShowNewContent(NewMode, kEModels);
end;

procedure TListDlg.PlayerClick(Sender: TObject);
begin
  if TComponent(Sender).Tag = Me then
    Kind := kModels
  else
  begin
    Kind := kEModels;
    pView := TComponent(Sender).Tag;
  end;
  InitLines;
  Selected := -2;
  ScrollBar.Init(Lines[Layer] - 1, DispLines);
  OffscreenPaint;
  Invalidate;
end;

procedure TListDlg.ModeBtnClick(Sender: TObject);
begin
  Layer0Btn.Down := Sender = Layer0Btn;
  Layer1Btn.Down := Sender = Layer1Btn;
  Layer2Btn.Down := Sender = Layer2Btn;
  Layer := TComponent(Sender).Tag;

  Selected := -2;
  ScrollBar.Init(Lines[Layer] - 1, DispLines);
  SmartUpdateContent;
end;

procedure TListDlg.ToggleBtnClick(Sender: TObject);
var
  p1: Integer;
  M: TMenuItem;
begin
  case Kind of
    kAdvance:
      begin
        Result := adFar;
        Closable := True;
        Close;
      end;
    kCities, kCityEvents:
      begin
        if Kind = kCities then
          Kind := kCityEvents
        else
          Kind := kCities;
        OffscreenPaint;
        Invalidate;
      end;
    kModels, kEModels:
      begin
        EmptyMenu(Popup.Items);
        if G.Difficulty[Me] > 0 then
        begin
          M := TMenuItem.Create(Popup);
          M.RadioItem := True;
          M.Caption := Tribe[Me].TPhrase('SHORTNAME');
          M.Tag := Me;
          M.OnClick := PlayerClick;
          if Kind = kModels then
            M.Checked := True;
          Popup.Items.Add(M);
        end;
        for p1 := 0 to nPl - 1 do
          if (p1 <> Me) and (MyRO.EnemyReport[p1] <> nil) and
            (MyRO.EnemyReport[p1].TurnOfMilReport >= 0) then
          begin
            M := TMenuItem.Create(Popup);
            M.RadioItem := True;
            M.Caption := Tribe[p1].TPhrase('SHORTNAME');
            M.Tag := p1;
            M.OnClick := PlayerClick;
            if (Kind = kEModels) and (p1 = pView) then
              M.Checked := True;
            Popup.Items.Add(M);
          end;
        Popup.Popup(Left + ToggleBtn.Left, Top + ToggleBtn.Top +
          ToggleBtn.Height);
      end;
  end;
end;

procedure TListDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F2) and (Kind in [kModels, kEModels]) then // my key
    // !!! toggle
  else if (Key = VK_F3) and (Kind in [kCities, kCityEvents]) then // my key
    ToggleBtnClick(nil)
  else if ((Key = VK_ESCAPE) or (Key = VK_RETURN)) and not CloseBtn.Visible then
  // prevent closing
  else
    inherited;
end;

procedure TListDlg.EcoChange;
begin
  if Visible and (Kind = kCities) then
    SmartUpdateContent;
end;

procedure TListDlg.TechChange;
begin
  if Visible and (Kind = kScience) then
  begin
    FormShow(nil);
    Invalidate;
  end;
end;

procedure TListDlg.AddCity;
begin
  if Visible and (Kind = kCities) then
  begin
    FormShow(nil);
    Invalidate;
  end;
end;

procedure TListDlg.RemoveUnit;
begin
  if ListDlg.Visible and (Kind = kModels) then
    SmartUpdateContent;
end;

procedure TListDlg.ScrollBarUpdate(Sender: TObject);
begin
  Selected := -2;
  SmartUpdateContent(True);
end;

end.
