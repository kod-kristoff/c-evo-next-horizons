{$INCLUDE Switches.inc}
unit Draft;

interface

uses
  Protocol, ClientTools, Term, ScreenTools, BaseWin, LCLIntf, LCLType, SysUtils,
  Classes, Graphics, Controls, Forms, ExtCtrls, ButtonA, ButtonB, Area;

type
  TDraftDlg = class(TBufferedDrawDlg)
    OKBtn: TButtonA;
    CloseBtn: TButtonB;
    GroundArea: TArea;
    SeaArea: TArea;
    AirArea: TArea;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OKBtnClick(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    procedure ShowNewContent(NewMode: TWindowMode);
  protected
    procedure OffscreenPaint; override;
  private
    Domain, MaxLines, Lines, Cut, yDomain, yFeature, yWeight, yTotal, yView,
      IncCap, DecCap: Integer;
    Code: array [0 .. nFeature - 1] of Integer;
    Template, Back: TBitmap;
    function IsFeatureInList(D, I: Integer): Boolean;
    procedure SetDomain(D: Integer);
  end;

var
  DraftDlg: TDraftDlg;


implementation

uses
  Help, Tribes, Directories;

{$R *.lfm}

const
  MaxLines0 = 11;
  LinePitch = 20;
  xDomain = 30;
  yDomain0 = 464;
  DomainPitch = 40;
  xFeature = 38;
  yFeature0 = 42;
  xWeight = 100;
  yWeight0 = 271;
  xTotal = 20;
  xTotal2 = 34;
  yTotal0 = 354;
  xView = 17;
  yView0 = 283;

procedure TDraftDlg.FormCreate(Sender: TObject);
begin
  inherited;
  InitButtons;
  HelpContext := 'CLASSES';
  Caption := Phrases.Lookup('TITLE_DRAFT');
  OKBtn.Caption := Phrases.Lookup('BTN_OK');

  if not Phrases2FallenBackToEnglish then
  begin
    GroundArea.Hint := Phrases2.Lookup('DRAFTDOMAIN', 0);
    SeaArea.Hint := Phrases2.Lookup('DRAFTDOMAIN', 1);
    AirArea.Hint := Phrases2.Lookup('DRAFTDOMAIN', 2);
  end
  else
  begin
    GroundArea.Hint := Phrases.Lookup('DOMAIN', 0);
    SeaArea.Hint := Phrases.Lookup('DOMAIN', 1);
    AirArea.Hint := Phrases.Lookup('DOMAIN', 2);
  end;

  Back := TBitmap.Create;
  Back.PixelFormat := pf24bit;
  Back.SetSize(Width, Height);
  Back.Canvas.FillRect(0, 0, Back.Width, Back.Height);
  Template := TBitmap.Create;
  Template.PixelFormat := pf24bit;
  LoadGraphicFile(Template, GetGraphicsDir + DirectorySeparator + 'MiliRes.png',
    [gfNoGamma]);
end;

procedure TDraftDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Template);
  FreeAndNil(Back);
end;

procedure TDraftDlg.CloseBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDraftDlg.OffscreenPaint;

  function DomainAvailable(D: Integer): Boolean;
  begin
    Result := (upgrade[D, 0].Preq = preNone) or
      (MyRO.Tech[upgrade[D, 0].Preq] >= tsApplicable);
  end;

  procedure PaintTotalBars;
  var
    I, Y, dx, num, W: Integer;
    S: string;
  begin
    with Offscreen.Canvas do
    begin
      // strength bar
      Y := yTotal;
      DarkGradient(Offscreen.Canvas, xTotal - 6, Y + 1, 184, 2);
      DarkGradient(Offscreen.Canvas, xTotal2 + 172, Y + 1, 95, 2);
      RisedTextOut(Offscreen.Canvas, xTotal - 2, Y,
        Phrases.Lookup('UNITSTRENGTH'));
      RisedTextOut(Offscreen.Canvas, xTotal + 112 + 30, Y,
        'x' + IntToStr(MyRO.DevModel.MStrength));
      RisedTextOut(Offscreen.Canvas, xTotal2 + 148 + 30, Y, '=');
      S := IntToStr(MyRO.DevModel.Attack) + '/' +
        IntToStr(MyRO.DevModel.Defense);
      RisedTextOut(Offscreen.Canvas, xTotal2 + 170 + 64 + 30 -
        BiColorTextWidth(Offscreen.Canvas, S), Y, S);

      // transport bar
      if MyRO.DevModel.MTrans > 0 then
      begin
        Y := yTotal + 19;
        DarkGradient(Offscreen.Canvas, xTotal - 6, Y + 1, 184, 1);
        DarkGradient(Offscreen.Canvas, xTotal2 + 172, Y + 1, 95, 1);
        RisedTextOut(Offscreen.Canvas, xTotal - 2, Y,
          Phrases.Lookup('UNITTRANSPORT'));
        RisedTextOut(Offscreen.Canvas, xTotal + 112 + 30, Y,
          'x' + IntToStr(MyRO.DevModel.MTrans));
        RisedTextOut(Offscreen.Canvas, xTotal2 + 148 + 30, Y, '=');

        Font.Color := $000000;
        dx := -237 - 30;
        for I := mcFirstNonCap - 1 downto 3 do
          if I in [mcSeaTrans, mcCarrier, mcAirTrans] then
          begin
            num := MyRO.DevModel.Cap[I] * MyRO.DevModel.MTrans;
            if num > 0 then
            begin
              Inc(dx, 15);
              Brush.Color := $C0C0C0;
              FrameRect(Rect(xTotal2 - 3 - dx, Y + 2,
                xTotal2 + 11 - dx, Y + 16));
              Brush.Style := bsClear;
              Sprite(Offscreen, HGrSystem, xTotal2 - 1 - dx, Y + 4, 10, 10,
                66 + I mod 11 * 11, 137 + I div 11 * 11);
              if num > 1 then
              begin
                S := IntToStr(num);
                W := TextWidth(S);
                Inc(dx, W + 1);
                Brush.Color := $FFFFFF;
                FillRect(Rect(xTotal2 - 3 - dx, Y + 2,
                  xTotal2 + W - 1 - dx, Y + 16));
                Brush.Style := bsClear;
                Textout(xTotal2 - 3 - dx + 1, Y, S);
              end;
            end;
          end;
      end;

      // speed bar
      Y := yTotal + 38;
      LoweredTextOut(Offscreen.Canvas, -1, MainTexture, xTotal - 2, Y,
        Phrases.Lookup('UNITSPEED'));
      DLine(Offscreen.Canvas, xTotal - 2, xTotal + 174, Y + 16,
        MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
      DLine(Offscreen.Canvas, xTotal2 + 176, xTotal2 + 263, Y + 16,
        MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
      S := MovementToString(MyRO.DevModel.Speed);
      RisedTextOut(Offscreen.Canvas, xTotal2 + 170 + 64 + 30 -
        TextWidth(S), Y, S);

      // cost bar
      Y := yTotal + 57;
      LoweredTextOut(Offscreen.Canvas, -1, MainTexture, xTotal - 2, Y,
        Phrases.Lookup('UNITCOST'));
      LoweredTextOut(Offscreen.Canvas, -1, MainTexture, xTotal + 112 + 30, Y,
        'x' + IntToStr(MyRO.DevModel.MCost));
      LoweredTextOut(Offscreen.Canvas, -1, MainTexture,
        xTotal2 + 148 + 30, Y, '=');
      DLine(Offscreen.Canvas, xTotal - 2, xTotal + 174, Y + 16,
        MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
      DLine(Offscreen.Canvas, xTotal2 + 176, xTotal2 + 263, Y + 16,
        MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
      S := IntToStr(MyRO.DevModel.Cost);
      RisedTextOut(Offscreen.Canvas, xTotal2 + 170 + 64 + 30 - 12 -
        TextWidth(S), Y, S);
      Sprite(Offscreen, HGrSystem, xTotal2 + 170 + 54 + 30, Y + 4, 10,
        10, 88, 115);

      if G.Difficulty[Me] <> 2 then
      begin // corrected cost bar
        Y := yTotal + 76;
        LoweredTextOut(Offscreen.Canvas, -1, MainTexture, xTotal - 2, Y,
          Phrases.Lookup('COSTDIFF' + char(48 + G.Difficulty[Me])));
        LoweredTextOut(Offscreen.Canvas, -1, MainTexture,
          xTotal2 + 148 + 30, Y, '=');
        DLine(Offscreen.Canvas, xTotal - 2, xTotal + 174, Y + 16,
          MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
        DLine(Offscreen.Canvas, xTotal2 + 176, xTotal2 + 263, Y + 16,
          MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
        S := IntToStr(MyRO.DevModel.Cost * BuildCostMod
          [G.Difficulty[Me]] div 12);
        RisedTextOut(Offscreen.Canvas, xTotal2 + 170 + 64 + 30 - 12 -
          TextWidth(S), Y, S);
        Sprite(Offscreen, HGrSystem, xTotal2 + 170 + 54 + 30, Y + 4, 10,
          10, 88, 115);
      end;
    end;
  end;

var
  I, J, X, D, N, TextColor, CapWeight, DomainCount: Integer;
begin
  inherited;
  UnshareBitmap(Back);

  ClientHeight := Template.Height - Cut;
  if ClientHeight > MainTexture.Height then
  // assemble background from 2 texture tiles
  begin
    BitBltCanvas(Back.Canvas, 0, 0, ClientWidth, 64,
      MainTexture.Image.Canvas, (MainTexture.Width - ClientWidth) div 2,
      MainTexture.Height - 64);
    BitBltCanvas(Back.Canvas, 0, 64, ClientWidth, ClientHeight - 64,
      MainTexture.Image.Canvas, (MainTexture.Width - ClientWidth) div 2,
      0);
  end
  else
    BitBltCanvas(Back.Canvas, 0, 0, ClientWidth, ClientHeight,
      MainTexture.Image.Canvas, (MainTexture.Width - ClientWidth) div 2,
      (MainTexture.Height - ClientHeight) div 2);
  ImageOp_B(Back, Template, 0, 0, 0, 0, Template.Width, 64);
  ImageOp_B(Back, Template, 0, 64, 0, 64 + Cut, Template.Width,
    Template.Height - 64 - Cut);

  BitBltCanvas(Offscreen.Canvas, 0, 0, ClientWidth, ClientHeight,
    Back.Canvas, 0, 0);

  Offscreen.Canvas.Font.Assign(UniFont[ftCaption]);
  RisedTextOut(Offscreen.Canvas, 10, 7, Caption);
  Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);

  with MyRO.DevModel do
  begin
    DomainCount := 0;
    for D := 0 to nDomains - 1 do
      if DomainAvailable(D) then
        Inc(DomainCount);
    if DomainCount > 1 then
    begin
      for D := 0 to nDomains - 1 do
        if DomainAvailable(D) then
        begin
          X := xDomain + D * DomainPitch;
          if D = Domain then
            ImageOp_BCC(Offscreen, Templates.Data, X, yDomain, 142, 246 + 37 * D, 36,
              36, 0, $00C0FF)
          else
            ImageOp_BCC(Offscreen, Templates.Data, X, yDomain, 142, 246 + 37 * D, 36,
              36, 0, $606060);
        end;
      Frame(Offscreen.Canvas, xDomain - 11, yDomain - 3,
        xDomain + 2 * DomainPitch + 46, yDomain + 38, $B0B0B0, $FFFFFF);
      RFrame(Offscreen.Canvas, xDomain - 12, yDomain - 4,
        xDomain + 2 * DomainPitch + 47, yDomain + 39, $FFFFFF, $B0B0B0);
    end;
    GroundArea.Top := yDomain;
    GroundArea.Visible := DomainAvailable(dGround);
    SeaArea.Top := yDomain;
    SeaArea.Visible := DomainAvailable(dSea);
    AirArea.Top := yDomain;
    AirArea.Visible := DomainAvailable(dAir);

    PaintTotalBars;

    // display weight
    with Offscreen.Canvas do
    begin
      for I := 0 to MaxWeight - 1 do
        if I < Weight then
          ImageOp_BCC(Offscreen, Templates.Data, Point(xWeight + 20 * I, yWeight),
            WeightOn.BoundsRect, 0, $949494)
        else
          ImageOp_BCC(Offscreen, Templates.Data, Point(xWeight + 20 * I, yWeight),
            WeightOff.BoundsRect, 0, $949494);
    end;

    with Offscreen.Canvas do
      for I := 0 to Lines - 1 do
      begin
        if not(Code[I] in AutoFeature) then
        begin
          // paint +/- butttons
          if Code[I] < mcFirstNonCap then
          begin
            Dump(Offscreen, HGrSystem, xFeature - 21, yFeature + 2 + LinePitch *
              I, 12, 12, 169, 172);
            Dump(Offscreen, HGrSystem, xFeature - 9, yFeature + 2 + LinePitch *
              I, 12, 12, 169, 159);
            RFrame(Offscreen.Canvas, xFeature - (21 + 1),
              yFeature + 2 + LinePitch * I - 1, xFeature - (21 - 24),
              yFeature + 2 + LinePitch * I + 12, MainTexture.ColorBevelShade,
              MainTexture.ColorBevelLight);
          end
          else
          begin
            Dump(Offscreen, HGrSystem, xFeature - 9, yFeature + 2 + LinePitch *
              I, 12, 12, 169, 185 + 13 * MyRO.DevModel.Cap[Code[I]]);
            RFrame(Offscreen.Canvas, xFeature - (9 + 1),
              yFeature + 2 + LinePitch * I - 1, xFeature - (21 - 24),
              yFeature + 2 + LinePitch * I + 12, MainTexture.ColorBevelShade,
              MainTexture.ColorBevelLight);
          end;

          // paint cost
          LightGradient(Offscreen.Canvas, xFeature + 34,
            yFeature + LinePitch * I, 50, HGrSystem.Data.Canvas.Pixels
            [187, 137]);
          if (Domain = dGround) and (Code[I] = mcDefense) then
            CapWeight := 2
          else
            CapWeight := Feature[Code[I]].Weight;
          N := CapWeight + Feature[Code[I]].Cost;
          D := 6;
          while (N - 1) * D * 2 > 48 - 10 do
            Dec(D);
          for J := 0 to N - 1 do
            if J < CapWeight then
              Sprite(Offscreen, HGrSystem, xFeature + 54 + (J * 2 + 1 - N) * D,
                yFeature + 2 + LinePitch * I + 1, 10, 10, 88, 126)
            else
              Sprite(Offscreen, HGrSystem, xFeature + 54 + (J * 2 + 1 - N) * D,
                yFeature + 2 + LinePitch * I + 1, 10, 10, 88, 115);
        end; // if not (code[i] in AutoFeature)
        DarkGradient(Offscreen.Canvas, xFeature + 17,
          yFeature + LinePitch * I, 16, 1);
        ScreenTools.Frame(Offscreen.Canvas, xFeature + 18, yFeature + 1 + LinePitch * I,
          xFeature + 20 - 2 + 13, yFeature + 2 + 1 - 2 + 13 + LinePitch * I,
          $C0C0C0, $C0C0C0);
        Sprite(Offscreen, HGrSystem, xFeature + 20, yFeature + 2 + 1 + LinePitch
          * I, 10, 10, 66 + Code[I] mod 11 * 11, 137 + Code[I] div 11 * 11);

        if MyRO.DevModel.Cap[Code[I]] > 0 then
          TextColor := MainTexture.ColorLitText
        else
          TextColor := -1;

        if Code[I] < mcFirstNonCap then
          LoweredTextOut(Offscreen.Canvas, TextColor, MainTexture, xFeature + 7,
            yFeature + LinePitch * I - 1, IntToStr(MyRO.DevModel.Cap[Code[I]]));
        LoweredTextOut(Offscreen.Canvas, TextColor, MainTexture, xFeature + 88,
          yFeature + LinePitch * I - 1, Phrases.Lookup('FEATURES', Code[I]));
      end;
  end;

  // free features
  J := 0;
  for I := 0 to nFeature - 1 do
    if (I in AutoFeature) and (1 shl Domain and Feature[I].Domains <> 0) and
      (Feature[I].Preq <> preNA) and
      ((Feature[I].Preq = preSun) and (MyRO.Wonder[woSun].EffectiveOwner = Me)
      or (Feature[I].Preq >= 0) and (MyRO.Tech[Feature[I].Preq] >= tsApplicable)
      ) and not((Feature[I].Preq = adSteamEngine) and
      (MyRO.Tech[adNuclearPower] >= tsApplicable)) then
    begin
      DarkGradient(Offscreen.Canvas, xWeight + 4, yWeight + 32 + LinePitch
        * J, 16, 1);
      Frame(Offscreen.Canvas, xWeight + 5, yWeight + 33 + LinePitch * J,
        xWeight + 18, yWeight + 47 + LinePitch * J, $C0C0C0, $C0C0C0);
      Sprite(Offscreen, HGrSystem, xWeight + 7, yWeight + 36 + LinePitch * J,
        10, 10, 66 + I mod 11 * 11, 137 + I div 11 * 11);
      LoweredTextOut(Offscreen.Canvas, -1, MainTexture, xWeight + 26,
        yWeight + 31 + LinePitch * J, Phrases.Lookup('FEATURES', I));
      Inc(J);
    end;

  with Tribe[Me].ModelPicture[MyRO.nModel] do
  begin
    FrameImage(Offscreen.Canvas, BigImp, xView + 4, yView + 4, xSizeBig,
      ySizeBig, 0, 0);
    Sprite(Offscreen, HGr, xView, yView, 64, 44, pix mod 10 * 65 + 1,
      pix div 10 * 49 + 1);
  end;
  MarkUsedOffscreen(ClientWidth, ClientHeight);
end;

procedure TDraftDlg.SetDomain(D: Integer);

  function Prio(fix: Integer): Integer;
  var
    FeaturePreq: Integer;
  begin
    FeaturePreq := Feature[fix].Preq;
    Assert(FeaturePreq <> preNA);
    if fix < mcFirstNonCap then
      Result := 10000 + fix
    else if FeaturePreq = preNone then
      Result := 20000
    else if FeaturePreq < 0 then
      Result := 40000
    else
      Result := 30000 + AdvValue[FeaturePreq];
    if not(fix in AutoFeature) then
      Inc(Result, 90000);
  end;

var
  I, J, X: Integer;
begin
  Domain := D;
  Lines := 0;
  for I := 0 to nFeature - 1 do
    if IsFeatureInList(Domain, I) then
    begin
      Code[Lines] := I;
      Inc(Lines);
    end;
  yFeature := yFeature0 + (MaxLines - Lines) * LinePitch div 2;

  // sort features
  for I := 0 to Lines - 2 do
    for J := I + 1 to Lines - 1 do
      if Prio(Code[I]) > Prio(Code[J]) then
      begin // exchange
        X := Code[I];
        Code[I] := Code[J];
        Code[J] := X;
      end;
end;

function TDraftDlg.IsFeatureInList(D, I: Integer): Boolean;
begin
  Result := not(I in AutoFeature) and (1 shl D and Feature[I].Domains <> 0) and
    (Feature[I].Preq <> preNA) and
    ((Feature[I].Preq = preNone) or (Feature[I].Preq = preSun) and
    (MyRO.Wonder[woSun].EffectiveOwner = Me) or (Feature[I].Preq >= 0) and
    (MyRO.Tech[Feature[I].Preq] >= tsApplicable));
end;

procedure TDraftDlg.FormShow(Sender: TObject);
var
  count, D, I: Integer;
begin
  Domain := dGround;
  while (Domain < dAir) and (upgrade[Domain, 0].Preq <> preNone) and
    (MyRO.Tech[upgrade[Domain, 0].Preq] < tsApplicable) do
    Inc(Domain);

  // count max number of features in any domain
  MaxLines := 0;
  for D := 0 to nDomains - 1 do
    if (upgrade[D, 0].Preq = preNone) or
      (MyRO.Tech[upgrade[D, 0].Preq] >= tsApplicable) then
    begin
      count := 0;
      for I := 0 to nFeature - 1 do
        if IsFeatureInList(D, I) then
          Inc(count);
      if count > MaxLines then
        MaxLines := count;
    end;
  Cut := (MaxLines0 - MaxLines) * LinePitch;
  OKBtn.Top := 477 - Cut;
  yDomain := yDomain0 - Cut;
  yWeight := yWeight0 - Cut;
  yTotal := yTotal0 - Cut;
  yView := yView0 - Cut;

  if WindowMode = wmModal then
  begin { center on screen }
    Left := (Screen.Width - Template.Width) div 2;
    Top := (Screen.Height - (Template.Height - Cut)) div 2;
  end;

  SetDomain(Domain);
  Server(sCreateDevModel, Me, Domain, nil^);
  MyModel[MyRO.nModel] := MyRO.DevModel;
  InitMyModel(MyRO.nModel, False);
  OffscreenPaint;
  IncCap := -1;
  DecCap := -1;
end;

procedure TDraftDlg.ShowNewContent(NewMode: TWindowMode);
begin
  inherited ShowNewContent(NewMode);
end;

procedure TDraftDlg.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I, D: Integer;
begin
  if Button = mbLeft then
  begin
    for D := 0 to nDomains - 1 do
      if (D <> Domain) and ((upgrade[D, 0].Preq = preNone) or
        (MyRO.Tech[upgrade[D, 0].Preq] >= tsApplicable)) and
        (X >= xDomain + D * DomainPitch) and
        (X < xDomain + D * DomainPitch + 36) and (Y >= yDomain) and
        (Y < yDomain + 36) then
      begin
        SetDomain(D);
        Server(sCreateDevModel, Me, Domain, nil^);
        MyModel[MyRO.nModel] := MyRO.DevModel;
        InitMyModel(MyRO.nModel, False);
        SmartUpdateContent;
      end;

    if (Y >= yFeature) and (Y < yFeature + LinePitch * Lines) then
    begin
      I := (Y - yFeature) div LinePitch;
      if (X >= xFeature - 21) and (X < ClientWidth) and (ssShift in Shift) then
        HelpDlg.ShowNewContent(WindowModeMakePersistent(FWindowMode), hkFeature, Code[I])
      else if not(Code[I] in AutoFeature) then
      begin
        if (Code[I] < mcFirstNonCap) and (X >= xFeature - 21) and
          (X < xFeature - 21 + 12) then
        begin
          IncCap := Code[I];
          Dump(Offscreen, HGrSystem, xFeature - 21, yFeature + 2 + LinePitch *
            I, 12, 12, 182, 172);
          SmartInvalidate;
        end
        else if (X >= xFeature - 9) and (X < xFeature - 9 + 12) then
        begin
          DecCap := Code[I];
          if Code[I] < mcFirstNonCap then
            Dump(Offscreen, HGrSystem, xFeature - 9, yFeature + 2 + LinePitch *
              I, 12, 12, 182, 159)
          else
            Dump(Offscreen, HGrSystem, xFeature - 9, yFeature + 2 + LinePitch *
              I, 12, 12, 182, 185 + 13 * MyRO.DevModel.Cap[Code[I]]);
          SmartInvalidate;
        end;
      end;
    end;
  end;
end;

procedure TDraftDlg.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewValue: Integer;
begin
  if IncCap >= 0 then
  begin
    NewValue := MyRO.DevModel.Cap[IncCap] + 1;
    Server(sSetDevModelCap + NewValue shl 4, Me, IncCap, nil^);
    MyModel[MyRO.nModel] := MyRO.DevModel;
    InitMyModel(MyRO.nModel, False);
    SmartUpdateContent;
    IncCap := -1;
  end
  else if DecCap >= 0 then
  begin
    if (DecCap >= mcFirstNonCap) or (MyRO.DevModel.Cap[DecCap] > 0) then
    begin
      NewValue := MyRO.DevModel.Cap[DecCap] - 1;
      if DecCap >= mcFirstNonCap then
        NewValue := -NewValue;
      Server(sSetDevModelCap + NewValue shl 4, Me, DecCap, nil^);
      MyModel[MyRO.nModel] := MyRO.DevModel;
      InitMyModel(MyRO.nModel, False);
    end;
    SmartUpdateContent;
    DecCap := -1;
  end;
end;

procedure TDraftDlg.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.
