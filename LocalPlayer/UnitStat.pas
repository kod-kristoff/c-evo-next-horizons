{$INCLUDE Switches.inc}
unit UnitStat;

interface

uses
  Protocol, ClientTools, Term, ScreenTools, BaseWin,
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  ButtonB, ButtonC, IsoEngine;

type
  TUnitStatDlg = class(TBufferedDrawDlg)
    SwitchBtn: TButtonB;
    CloseBtn: TButtonB;
    ConscriptsBtn: TButtonB;
    HelpBtn: TButtonC;
    procedure FormShow(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ModelBoxChange(Sender: TObject);
    procedure SwitchBtnClick(Sender: TObject);
    procedure ConscriptsBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
  private
    NoMap: TIsoMap;
  public
    procedure CheckAge;
    procedure ShowNewContent_OwnModel(NewMode: TWindowMode; mix: Integer);
    procedure ShowNewContent_OwnUnit(NewMode: TWindowMode; uix: Integer);
    procedure ShowNewContent_EnemyUnit(NewMode: TWindowMode; euix: Integer);
    procedure ShowNewContent_EnemyLoc(NewMode: TWindowMode; Loc: Integer);
    procedure ShowNewContent_EnemyModel(NewMode: TWindowMode; emix: Integer);
    procedure ShowNewContent_EnemyCity(NewMode: TWindowMode; Loc: Integer);

  protected
    mixShow, // for dkOwnModel
    uixShow, euixShow, ecixShow, UnitLoc, AgePrepared: Integer;
    // for dkEnemyUnit, euixShow=-1 ->
    mox: ^TModelInfo; // for dkEnemyModel
    Kind: (dkOwnModel, dkOwnUnit, dkEnemyModel, dkEnemyUnit, dkEnemyCityDefense,
      dkEnemyCity);
    Back, Template: TBitmap;
    procedure OffscreenPaint; override;
  end;

var
  UnitStatDlg: TUnitStatDlg;

implementation

uses
  Tribes, Help, Directories, UTexture;

{$R *.lfm}

const
  xView = 71;
  xTotal = 20;
  StatDown = 112;
  yImp = 133;

  // window size
  wCommon = 208;
  hOwnModel = 293;
  hEnemyModel = 236;
  hEnemyUnit = 212;
  hEnemyCityDefense = 320;
  hEnemyCity = 166;
  hMax = 320;

procedure TUnitStatDlg.FormCreate(Sender: TObject);
begin
  inherited;
  NoMap := TIsoMap.Create;
  AgePrepared := -2;
  TitleHeight := Screen.Height;
  InitButtons;

  Back := TBitmap.Create;
  Back.PixelFormat := pf24bit;
  Back.SetSize(5 * wCommon, hMax);
  Back.Canvas.FillRect(0, 0, Back.Width,Back.Height);
  Template := TBitmap.Create;
  Template.PixelFormat := pf24bit;
  LoadGraphicFile(Template, GetGraphicsDir + DirectorySeparator + 'Unit.png',
    [gfNoGamma]);
end;

procedure TUnitStatDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Template);
  FreeAndNil(Back);
  FreeAndNil(NoMap);
end;

procedure TUnitStatDlg.CheckAge;
begin
  if MainTexture.Age <> AgePrepared then begin
    AgePrepared := MainTexture.Age;
    BitBltCanvas(Back.Canvas, 0, 0, wCommon, hOwnModel,
      MainTexture.Image.Canvas, (MainTexture.Width - wCommon) div 2,
      (MainTexture.Height - hOwnModel) div 2);
    BitBltCanvas(Back.Canvas, wCommon, 0, wCommon, hEnemyModel,
      MainTexture.Image.Canvas, (MainTexture.Width - wCommon) div 2,
      (MainTexture.Height - hEnemyModel) div 2);
    BitBltCanvas(Back.Canvas, 2 * wCommon, 0, wCommon, hEnemyUnit,
      MainTexture.Image.Canvas, (MainTexture.Width - wCommon) div 2,
      (MainTexture.Height - hEnemyUnit) div 2);
    BitBltCanvas(Back.Canvas, 3 * wCommon, 0, wCommon, hEnemyCityDefense,
      MainTexture.Image.Canvas, (MainTexture.Width - wCommon) div 2,
      (MainTexture.Height - hEnemyCityDefense) div 2);
    BitBltCanvas(Back.Canvas, 4 * wCommon, 0, wCommon, hEnemyCity,
      MainTexture.Image.Canvas, (MainTexture.Width - wCommon) div 2,
      (MainTexture.Height - hEnemyCity) div 2);
    ImageOp_B(Back, Template, 0, 0, 0, 0, 5 * wCommon, hMax);
  end;
end;

procedure TUnitStatDlg.FormShow(Sender: TObject);
var
  owner, mix: Integer;
  IsSpecialUnit: Boolean;
begin
  if Kind in [dkEnemyUnit, dkEnemyCityDefense, dkEnemyCity] then
  begin
    if MyMap[UnitLoc] and fUnit <> 0 then
    begin // find model
      if euixShow < 0 then
      begin
        euixShow := MyRO.nEnemyUn - 1;
        while (euixShow >= 0) and (MyRO.EnemyUn[euixShow].Loc <> UnitLoc) do
          Dec(euixShow);
        Assert(euixShow >= 0);
      end;
      with MyRO.EnemyUn[euixShow] do
      begin
        mox := @MyRO.EnemyModel[emix];
        if not Assigned(Tribe[owner].ModelPicture[mix].HGr) then
          InitEnemyModel(emix);
      end
    end
    else
      mox := nil;
    if Kind in [dkEnemyCityDefense, dkEnemyCity] then
    begin
      ecixShow := MyRO.nEnemyCity - 1;
      while (ecixShow >= 0) and (MyRO.EnemyCity[ecixShow].Loc <> UnitLoc) do
        Dec(ecixShow);
      Assert(ecixShow >= 0);
    end;
  end;
  case Kind of
    dkOwnModel:
      ClientHeight := hOwnModel;
    dkOwnUnit:
      ClientHeight := hEnemyUnit;
    dkEnemyModel:
      ClientHeight := hEnemyModel;
    dkEnemyUnit:
      ClientHeight := hEnemyUnit;
    dkEnemyCityDefense:
      ClientHeight := hEnemyCityDefense;
    dkEnemyCity:
      ClientHeight := hEnemyCity;
  end;

  if Kind in [dkOwnModel, dkEnemyModel] then
  begin
    Left := UserLeft;
    Top := UserTop;
  end
  else
  begin
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
  end;

  SwitchBtn.Visible := not Supervising and (Kind = dkOwnModel);
  ConscriptsBtn.Visible := not Supervising and (Kind = dkOwnModel) and
    (MyRO.Tech[adConscription] >= tsApplicable) and
    (MyModel[mixShow].Domain = dGround) and (MyModel[mixShow].Kind < mkScout);
  IsSpecialUnit := False;
  if Kind in [dkEnemyCity, dkEnemyCityDefense] then
    Caption := CityName(MyRO.EnemyCity[ecixShow].ID)
  else
  begin
    case Kind of
      dkOwnModel:
        begin
          owner := Me;
          mix := mixShow;
          IsSpecialUnit := MyModel[mix].Kind >= $10;
        end;
      dkOwnUnit:
        begin
          owner := Me;
          mix := MyUn[uixShow].mix;
          IsSpecialUnit := MyModel[mix].Kind >= $10;
        end
    else
      begin
        owner := mox.owner;
        mix := mox.mix;
        IsSpecialUnit := mox.Kind >= $10;
      end;
    end;
    if MainScreen.mNames.Checked then
      Caption := Tribe[owner].ModelName[mix]
    else
      Caption := Format(Tribe[owner].TPhrase('GENMODEL'), [mix])
  end;
  if IsSpecialUnit then
    HelpBtn.Hint := Phrases.Lookup('CONTROLS', 6);
  HelpBtn.Visible := IsSpecialUnit;
  OffscreenPaint;
end;

procedure TUnitStatDlg.ShowNewContent_OwnModel(NewMode: TWindowMode; mix: Integer);
begin
  Kind := dkOwnModel;
  mixShow := mix;
  inherited ShowNewContent(NewMode);
end;

procedure TUnitStatDlg.ShowNewContent_OwnUnit(NewMode: TWindowMode; uix: Integer);
begin
  Kind := dkOwnUnit;
  uixShow := uix;
  inherited ShowNewContent(NewMode);
end;

procedure TUnitStatDlg.ShowNewContent_EnemyUnit(NewMode: TWindowMode; euix: Integer);
begin
  Kind := dkEnemyUnit;
  euixShow := euix;
  UnitLoc := MyRO.EnemyUn[euix].Loc;
  inherited ShowNewContent(NewMode);
end;

procedure TUnitStatDlg.ShowNewContent_EnemyLoc(NewMode: TWindowMode; Loc: Integer);
begin
  Kind := dkEnemyUnit;
  UnitLoc := Loc;
  euixShow := -1;
  inherited ShowNewContent(NewMode);
end;

procedure TUnitStatDlg.ShowNewContent_EnemyModel(NewMode: TWindowMode; emix: Integer);
begin
  Kind := dkEnemyModel;
  mox := @MyRO.EnemyModel[emix];
  inherited ShowNewContent(NewMode);
end;

procedure TUnitStatDlg.ShowNewContent_EnemyCity(NewMode: TWindowMode; Loc: Integer);
begin
  if MyMap[Loc] and fUnit <> 0 then
    Kind := dkEnemyCityDefense
  else
    Kind := dkEnemyCity;
  UnitLoc := Loc;
  euixShow := -1;
  inherited ShowNewContent(NewMode);
end;

procedure TUnitStatDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Kind in [dkOwnModel, dkEnemyModel] then
  begin
    UserLeft := Left;
    UserTop := Top
  end;
  if OffscreenUser = self then
    OffscreenUser := nil;
end;

procedure TUnitStatDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TUnitStatDlg.OffscreenPaint;
var
  PPicture: ^TModelPicture;

  function IsToCount(emix: Integer): Boolean;
  var
    PTestPicture: ^TModelPicture;
  begin
    if MainScreen.mNames.Checked then
    begin
      PTestPicture := @Tribe[MyRO.EnemyModel[emix].owner].ModelPicture
        [MyRO.EnemyModel[emix].mix];
      Result := (PPicture.HGr = PTestPicture.HGr) and
        (PPicture.pix = PTestPicture.pix) and
        (ModelHash(mox^) = ModelHash(MyRO.EnemyModel[emix]));
    end
    else
      Result := (MyRO.EnemyModel[emix].owner = mox.owner) and
        (MyRO.EnemyModel[emix].mix = mox.mix);
  end;

  procedure FeatureBar(dst: TBitmap; X, Y: Integer; const mi: TModelInfo;
    T: TTexture);
  var
    I, W, dx, num: Integer;
    S: string;
  begin
    DarkGradient(dst.Canvas, X - 6, Y + 1, 180, 1);
    with dst.Canvas do
      if mi.Kind >= $10 then
      begin
        S := Phrases.Lookup('UNITSPECIAL');
        Font.Color := $000000;
        Textout(X - 1, Y + 1, S);
        Font.Color := $B0B0B0;
        Textout(X - 2, Y, S);
      end
      else
      begin
        Font.Color := $000000;
        dx := 2;
        for I := 3 to nFeature - 1 do
        begin
          num := 0;
          case I of
            mcSeaTrans:
              if mi.Domain = dSea then
                num := mi.TTrans;
            mcCarrier:
              if mi.Domain = dSea then
                num := mi.ATrans_Fuel;
            mcBombs:
              num := mi.Bombs;
            mcFuel:
              if mi.Domain = dAir then
                num := mi.ATrans_Fuel;
            mcAirTrans:
              if mi.Domain = dAir then
                num := mi.TTrans;
            mcFirstNonCap .. nFeature - 1:
              if mi.Cap and (1 shl (I - mcFirstNonCap)) <> 0 then
                num := 1
          end;
          if (num > 0) and
            ((I <> mcSE) or (mi.Cap and (1 shl (mcNP - mcFirstNonCap)) = 0))
          then
          begin
            if num > 1 then
            begin
              S := IntToStr(num);
              W := TextWidth(S);
              Brush.Color := $FFFFFF;
              FillRect(Rect(X - 3 + dx, Y + 2, X + W - 1 + dx, Y + 16));
              Brush.Style := bsClear;
              Textout(X - 3 + dx + 1, Y, S);
              Inc(dx, W + 1)
            end;
            Brush.Color := $C0C0C0;
            FrameRect(Rect(X - 3 + dx, Y + 2, X + 11 + dx, Y + 16));
            Brush.Style := bsClear;
            Sprite(dst, HGrSystem, X - 1 + dx, Y + 4, 10, 10,
              66 + I mod 11 * 11, 137 + I div 11 * 11);
            Inc(dx, 15)
          end;
        end;
      end;
  end; { featurebar }

  procedure NumberBarS(dst: TBitmap; X, Y: Integer; Cap, S: string; T: TTexture);
  begin
    DLine(dst.Canvas, X - 2, X + 170, Y + 16, T.ColorBevelShade, T.ColorBevelLight);
    LoweredTextOut(dst.Canvas, -1, T, X - 2, Y, Cap);
    RisedTextout(dst.Canvas, X + 170 - BiColorTextWidth(dst.Canvas, S), Y, S);
  end;

var
  I, J, X, Y, cix, uix, emix, InProd, Available, Destroyed, Loc, Cnt, yView,
    yTotal, yCaption: Integer;
  S: string;
  ui: TUnitInfo;
  mi: TModelInfo;
begin
  inherited;

  case Kind of
    dkOwnModel:
      begin
        BitBltCanvas(Offscreen.Canvas, 0, 0, wCommon, hOwnModel,
          Back.Canvas, 0, 0);
        yView := 13;
        yTotal := 92;
      end;
    dkEnemyModel:
      begin
        BitBltCanvas(Offscreen.Canvas, 0, 0, wCommon, hEnemyModel,
          Back.Canvas, wCommon, 0);
        yView := 13;
        yTotal := 92;
      end;
    dkEnemyUnit, dkOwnUnit:
      begin
        BitBltCanvas(Offscreen.Canvas, 0, 0, wCommon, hEnemyUnit,
          Back.Canvas, 2 * wCommon, 0);
        yView := 13;
        yTotal := 123;
      end;
    dkEnemyCityDefense:
      begin
        BitBltCanvas(Offscreen.Canvas, 0, 0, wCommon, hEnemyCityDefense,
          Back.Canvas, 3 * wCommon, 0);
        yView := 171;
        yTotal := 231;
      end;
    dkEnemyCity:
      begin
        BitBltCanvas(Offscreen.Canvas, 0, 0, wCommon, hEnemyCity,
          Back.Canvas, 4 * wCommon, 0);
      end;
  end;
  MarkUsedOffscreen(ClientWidth, ClientHeight);
  HelpBtn.Top := yTotal + 22;

  if Kind in [dkEnemyCityDefense, dkEnemyCity] then
  begin // show city defense facilities
    Cnt := 0;
    for I := 0 to 3 do
      if MyRO.EnemyCity[ecixShow].Flags and (2 shl I) <> 0 then
        Inc(Cnt);
    X := (wCommon - Cnt * xSizeSmall) div 2 - (Cnt - 1) * 2;
    for I := 0 to 3 do
      if MyRO.EnemyCity[ecixShow].Flags and (2 shl I) <> 0 then
      begin
        case I of
          0: J := imWalls;
          1: J := imCoastalFort;
          2: J := imMissileBat;
          3: J := imBunker
        end;
        Frame(Offscreen.Canvas, X - 1, yImp - 1, X + xSizeSmall,
          yImp + ySizeSmall, MainTexture.ColorBevelLight,
          MainTexture.ColorBevelShade);
        BitBltCanvas(Offscreen.Canvas, X, yImp, xSizeSmall, ySizeSmall,
          SmallImp.Canvas, J mod 7 * xSizeSmall,
          (J + SystemIconLines * 7) div 7 * ySizeSmall);
        Inc(X, xSizeSmall + 4);
      end;
  end;

  if Kind = dkEnemyModel then
  begin
    PPicture := @Tribe[mox.owner].ModelPicture[mox.mix];
    Available := 0;
    if G.Difficulty[Me] = 0 then // supervisor -- count stacked units too
      for Loc := 0 to G.lx * G.ly - 1 do
      begin
        if MyMap[Loc] and fUnit <> 0 then
        begin
          Server(sGetUnits, Me, Loc, Cnt);
          for uix := 0 to Cnt - 1 do
            if IsToCount(MyRO.EnemyUn[MyRO.nEnemyUn + uix].emix) then
              Inc(Available);
        end;
      end
    else // no supervisor -- can only count stack top units
      for uix := 0 to MyRO.nEnemyUn - 1 do
        if (MyRO.EnemyUn[uix].Loc >= 0) and IsToCount(MyRO.EnemyUn[uix].emix)
        then
          Inc(Available);
    Destroyed := 0;
    for emix := 0 to MyRO.nEnemyModel - 1 do
      if IsToCount(emix) then
        Inc(Destroyed, MyRO.EnemyModel[emix].Lost);
  end
  else
  begin
    Available := 0;
    for uix := 0 to MyRO.nUn - 1 do
      if (MyUn[uix].Loc >= 0) and (MyUn[uix].mix = mixShow) then
        Inc(Available);
    InProd := 0;
    for cix := 0 to MyRO.nCity - 1 do
      if (MyCity[cix].Loc >= 0) and
        (MyCity[cix].Project and (cpImp + cpIndex) = mixShow) then
        Inc(InProd);
  end;

  Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
  if Kind in [dkEnemyCityDefense, dkEnemyCity] then
  begin
    NoMap.SetOutput(Offscreen);
    NoMap.PaintCity(ClientWidth div 2, 53, MyRO.EnemyCity[ecixShow], False);

    S := Tribe[MyRO.EnemyCity[ecixShow].owner].TPhrase('UNITOWNER');
    LoweredTextOut(Offscreen.Canvas, -1, MainTexture,
      (ClientWidth - BiColorTextWidth(Offscreen.Canvas, S)) div 2, 105, S);
  end;

  if Kind <> dkEnemyCity then
  begin // show unit stats
    if Kind = dkOwnModel then
      MakeModelInfo(Me, mixShow, MyModel[mixShow], mi)
    else if Kind = dkOwnUnit then
    begin
      MakeUnitInfo(Me, MyUn[uixShow], ui);
      MakeModelInfo(Me, MyUn[uixShow].mix, MyModel[MyUn[uixShow].mix], mi);
    end
    else
    begin
      mi := mox^;
      if Kind in [dkEnemyUnit, dkEnemyCityDefense] then
        ui := MyRO.EnemyUn[euixShow];
    end;

    with Tribe[mi.owner].ModelPicture[mi.mix] do
    begin
      if Kind in [dkOwnUnit, dkEnemyUnit, dkEnemyCityDefense] then
        with ui, NoMap do
        begin
          { Frame(offscreen.canvas,xView-1,yView-1,xView+64,yView+48,
            MainTexture.ColorBevelShade,MainTexture.ColorBevelLight);
            RFrame(Offscreen.Canvas,xView-2,yView-2,xView+65,yView+49,
            MainTexture.ColorBevelShade,MainTexture.ColorBevelLight); }
          with Offscreen.Canvas do
          begin
            Brush.Color := HGrSystem.Data.Canvas.Pixels[98, 67];
            Offscreen.Canvas.FillRect(Rect(xView, yView, xView + 64,
              yView + 16));
            Brush.Style := bsClear;
          end;

          if MyMap[Loc] and fTerrain >= fForest then
          begin
            X := 1 + 2 * (xxt * 2 + 1);
            Y := 1 + yyt + 2 * (yyt * 3 + 1);
          end
          else
          begin
            X := Integer(MyMap[Loc] and fTerrain) * (xxt * 2 + 1) + 1;
            Y := 1 + yyt;
          end;
          for J := -1 to 1 do
            for I := -1 to 1 do
              if (I + J) and 1 = 0 then
              begin
                Sprite(Buffer, HGrTerrain, I * xxt, J * yyt, xxt * 2,
                  yyt * 2, X, Y);
                if MyMap[Loc] and (fTerrain or fSpecial) = fGrass or fSpecial1
                then
                  Sprite(Buffer, HGrTerrain, I * xxt, J * yyt, xxt * 2, yyt * 2,
                    1 + 2 * (xxt * 2 + 1), 1 + yyt + 1 * (yyt * 3 + 1))
                else if (MyMap[Loc] and fTerrain = fForest) and
                  IsJungle(Loc div G.lx) then
                  Sprite(Buffer, HGrTerrain, I * xxt, J * yyt, xxt * 2, yyt * 2,
                    1 + 7 * (xxt * 2 + 1), 1 + yyt + 19 * (yyt * 3 + 1))
                else if MyMap[Loc] and fTerrain >= fForest then
                  Sprite(Buffer, HGrTerrain, I * xxt, J * yyt, xxt * 2, yyt * 2,
                    1 + 7 * (xxt * 2 + 1),
                    1 + yyt + 2 * Integer(2 + MyMap[Loc] and fTerrain - fForest)
                    * (yyt * 3 + 1));
              end;
          BitBltCanvas(Offscreen.Canvas, xView, yView + 16, 64, 32,
            Buffer.Canvas, 1, 0);

          // show unit, experience and health
          Sprite(Offscreen, HGr, xView, yView, 64, 48, pix mod 10 * 65 + 1,
            pix div 10 * 49 + 1);
          if Flags and unFortified <> 0 then
            Sprite(Offscreen, HGrStdUnits, xView, yView, xxu * 2, yyu * 2,
              1 + 6 * (xxu * 2 + 1), 1);
          FrameImage(Offscreen.Canvas, HGrSystem.Data, xView - 20,
            yView + 5, 12, 14, 121 + Exp div ExpCost * 13, 28);
          if Health < 100 then
          begin
            S := IntToStr(Health) + '%';
            LightGradient(Offscreen.Canvas, xView - 45, yView + 24, 38,
              (ColorOfHealth(Health) and $FEFEFE shr 2) * 3);
            RisedTextout(Offscreen.Canvas, xView - 45 + 20 -
              BiColorTextWidth(Offscreen.Canvas, S) div 2, yView + 23, S);
          end;

          if Kind = dkEnemyUnit then
          begin
            S := Tribe[mox.owner].TPhrase('UNITOWNER');
            LoweredTextOut(Offscreen.Canvas, -1, MainTexture,
              (ClientWidth - BiColorTextWidth(Offscreen.Canvas, S)) div 2,
              yView + 80, S);
          end;
        end
      else
      begin
        FrameImage(Offscreen.Canvas, BigImp, xView + 4, yView, 56, 40, 0, 0);
        Sprite(Offscreen, HGr, xView, yView - 4, 64, 44, pix mod 10 * 65 + 1,
          pix div 10 * 49 + 1);
      end;

      DarkGradient(Offscreen.Canvas, xTotal - 6, yTotal + 1, 180, 2);
      RisedTextout(Offscreen.Canvas, xTotal - 2, yTotal,
        Phrases.Lookup('UNITSTRENGTH'));
      S := IntToStr(mi.Attack) + '/' + IntToStr(mi.Defense);
      RisedTextout(Offscreen.Canvas,
        xTotal + 170 - BiColorTextWidth(Offscreen.Canvas, S), yTotal, S);
      FeatureBar(Offscreen, xTotal, yTotal + 19, mi, MainTexture);
      NumberBarS(Offscreen, xTotal, yTotal + 38, Phrases.Lookup('UNITSPEED'),
        MovementToString(mi.Speed), MainTexture);
      LoweredTextOut(Offscreen.Canvas, -1, MainTexture, xTotal - 2, yTotal + 57,
        Phrases.Lookup('UNITCOST'));
      DLine(Offscreen.Canvas, xTotal - 2, xTotal + 170, yTotal + 57 + 16,
        MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
      if G.Difficulty[Me] = 0 then
        S := IntToStr(mi.cost)
      else
        S := IntToStr(mi.cost * BuildCostMod[G.Difficulty[Me]] div 12);
      RisedTextout(Offscreen.Canvas,
        xTotal + 159 - BiColorTextWidth(Offscreen.Canvas, S), yTotal + 57, S);
      Sprite(Offscreen, HGrSystem, xTotal + 160, yTotal + 57 + 5, 10,
        10, 88, 115);

      if Kind = dkOwnModel then
      begin
        if MyModel[mixShow].IntroTurn > 0 then
        begin
          if MyModel[mixShow].Kind = mkEnemyDeveloped then
            LoweredTextOut(Offscreen.Canvas, -1, MainTexture, xTotal - 2,
              (yTotal + StatDown - 19), Phrases.Lookup('UNITADOPT'))
          else
            LoweredTextOut(Offscreen.Canvas, -1, MainTexture, xTotal - 2,
              (yTotal + StatDown - 19), Phrases.Lookup('UNITINTRO'));
          DLine(Offscreen.Canvas, xTotal - 2, xTotal + 170,
            (yTotal + StatDown - 19) + 16, MainTexture.ColorTextShade,
            MainTexture.ColorTextLight);
          S := TurnToString(MyModel[mixShow].IntroTurn);
          RisedTextout(Offscreen.Canvas,
            xTotal + 170 - BiColorTextWidth(Offscreen.Canvas, S),
            (yTotal + StatDown - 19), S);
        end;

        NumberBar(Offscreen, xTotal, yTotal + StatDown,
          Phrases.Lookup('UNITBUILT'), MyModel[mixShow].Built, MainTexture);
        if MyModel[mixShow].Lost > 0 then
          NumberBar(Offscreen, xTotal, yTotal + StatDown + 19,
            Phrases.Lookup('UNITLOST'), MyModel[mixShow].Lost, MainTexture);
        if InProd > 0 then
          NumberBar(Offscreen, xTotal, yTotal + StatDown + 57,
            Phrases.Lookup('UNITINPROD'), InProd, MainTexture);
        if Available > 0 then
          NumberBar(Offscreen, xTotal, yTotal + StatDown + 38,
            Phrases.Lookup('UNITAVAILABLE'), Available, MainTexture);

        if MyModel[mixShow].Status and msObsolete <> 0 then
        begin
          SwitchBtn.ButtonIndex := 12;
          SwitchBtn.Hint := Phrases.Lookup('BTN_OBSOLETE');
        end
        else
        begin
          SwitchBtn.ButtonIndex := 11;
          SwitchBtn.Hint := Phrases.Lookup('BTN_NONOBSOLETE');
        end;
        if MyModel[mixShow].Status and msAllowConscripts = 0 then
        begin
          ConscriptsBtn.ButtonIndex := 30;
          ConscriptsBtn.Hint := Phrases.Lookup('BTN_NOCONSCRIPTS');
        end
        else
        begin
          ConscriptsBtn.ButtonIndex := 29;
          ConscriptsBtn.Hint := Phrases.Lookup('BTN_ALLOWCONSCRIPTS');
        end;
      end
      else if Kind = dkEnemyModel then
      begin
        if Destroyed > 0 then
          NumberBar(Offscreen, xTotal, yTotal + StatDown - 19,
            Phrases.Lookup('UNITDESTROYED'), Destroyed, MainTexture);
        if Available > 0 then
          NumberBar(Offscreen, xTotal, yTotal + StatDown,
            Phrases.Lookup('UNITKNOWN'), Available, MainTexture);
      end;
    end;
  end;

  Offscreen.Canvas.Font.Assign(UniFont[ftNormal]);
  case Kind of
    dkOwnModel, dkEnemyModel:
      yCaption := yView + 46;
    dkEnemyUnit, dkOwnUnit:
      yCaption := yView + 54;
    dkEnemyCityDefense, dkEnemyCity:
      yCaption := 79;
  end;
  RisedTextout(Offscreen.Canvas,
    (ClientWidth - BiColorTextWidth(Offscreen.Canvas, Caption)) div 2,
    yCaption, Caption);
end;

procedure TUnitStatDlg.ModelBoxChange(Sender: TObject);
begin
  SmartUpdateContent;
end;

procedure TUnitStatDlg.SwitchBtnClick(Sender: TObject);
begin
  MyModel[mixShow].Status := MyModel[mixShow].Status xor msObsolete;
  if MyModel[mixShow].Status and msObsolete <> 0 then
  begin
    SwitchBtn.ButtonIndex := 12;
    SwitchBtn.Hint := Phrases.Lookup('BTN_OBSOLETE');
  end
  else
  begin
    SwitchBtn.ButtonIndex := 11;
    SwitchBtn.Hint := Phrases.Lookup('BTN_NONOBSOLETE');
  end;
end;

procedure TUnitStatDlg.ConscriptsBtnClick(Sender: TObject);
begin
  MyModel[mixShow].Status := MyModel[mixShow].Status xor msAllowConscripts;
  if MyModel[mixShow].Status and msAllowConscripts = 0 then
  begin
    ConscriptsBtn.ButtonIndex := 30;
    ConscriptsBtn.Hint := Phrases.Lookup('BTN_NOCONSCRIPTS');
  end
  else
  begin
    ConscriptsBtn.ButtonIndex := 29;
    ConscriptsBtn.Hint := Phrases.Lookup('BTN_ALLOWCONSCRIPTS');
  end;
end;

procedure TUnitStatDlg.HelpBtnClick(Sender: TObject);
begin
  HelpDlg.ShowNewContent(wmPersistent, hkModel, 0);
end;

end.
