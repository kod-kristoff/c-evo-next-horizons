{$INCLUDE Switches.inc}
unit Rates;

interface

uses
  Protocol, ScreenTools, BaseWin, LCLIntf, LCLType, SysUtils, Classes, Graphics,
  Controls, Forms, ButtonB, ButtonC;

type
  TRatesDlg = class(TBufferedDrawDlg)
    CloseBtn: TButtonB;
    LuxBtn: TButtonC;
    ScienceBtn: TButtonC;
    TaxUpBtn: TButtonC;
    TaxDownBtn: TButtonC;
    procedure FormShow(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TaxLuxBtnClick(Sender: TObject);
  public
    procedure OffscreenPaint; override;
    procedure ShowNewContent(NewMode: TWindowMode);
  end;

var
  RatesDlg: TRatesDlg;


implementation

uses
  ClientTools, Term, Tribes;

{$R *.lfm}

const
  MessageLineSpacing = 20;

procedure TRatesDlg.FormCreate(Sender: TObject);
begin
  TitleHeight := Screen.Height;
  InitButtons;
end;

procedure TRatesDlg.OffscreenPaint;
var
  P, X, Y, current, Max, I: Integer;
  S, s1: string;
begin
  if (OffscreenUser <> nil) and (OffscreenUser <> self) then
    OffscreenUser.Update;
  // complete working with old owner to prevent rebound
  OffscreenUser := self;

  Fill(Offscreen.Canvas, 0, 0, ClientWidth, ClientHeight,
    (Maintexture.Width - ClientWidth) div 2, (Maintexture.Height - ClientHeight) div 2);
  Frame(Offscreen.Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);
  Frame(Offscreen.Canvas, 1, 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Offscreen.Canvas, 2, 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);

  BtnFrame(Offscreen.Canvas, CloseBtn.BoundsRect, MainTexture);
  Offscreen.Canvas.Font.Assign(UniFont[ftCaption]);
  S := Phrases.Lookup('TITLE_RATES');
  RisedTextOut(Offscreen.Canvas,
    (ClientWidth - BiColorTextWidth(Offscreen.Canvas, S)) div 2 - 1, 7, S);

  if MyRO.Wonder[woLiberty].EffectiveOwner = Me then
    S := Phrases.Lookup('NORATES')
  else
    S := Phrases.Lookup('RATES');
  Offscreen.Canvas.Font.Assign(UniFont[ftNormal]);
  P := Pos('\', S);
  if P = 0 then
    RisedTextOut(Offscreen.Canvas, (ClientWidth - BiColorTextWidth(Canvas, S))
      div 2, 114, S)
  else
  begin
    s1 := Copy(S, 1, P - 1);
    RisedTextOut(Offscreen.Canvas,
      (ClientWidth - BiColorTextWidth(Offscreen.Canvas, s1)) div 2,
      114 - MessageLineSpacing div 2, s1);
    s1 := Copy(S, P + 1, 255);
    RisedTextOut(Offscreen.Canvas,
      (ClientWidth - BiColorTextWidth(Offscreen.Canvas, s1)) div 2,
      114 + (MessageLineSpacing - MessageLineSpacing div 2), s1);
  end;
  Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);

  if MyRO.Wonder[woLiberty].EffectiveOwner = Me then
  begin
    GlowFrame(Offscreen, ClientWidth div 2 - xSizeBig div 2, 52, xSizeBig,
      ySizeBig, Tribe[Me].Color);
    BitBltCanvas(Offscreen.Canvas, ClientWidth div 2 - xSizeBig div 2, 52,
      xSizeBig, ySizeBig, BigImp.Canvas, (woLiberty mod 7) * xSizeBig,
      (woLiberty div 7 + SystemIconLines) * ySizeBig);
  end
  else
  begin
    // ImageOp_CBC(Offscreen,Templates,260,40,145,112,36,36,$404000,$8B8BEB);

    S := Phrases.Lookup('SCIENCE');
    RisedTextOut(Offscreen.Canvas,
      16 + 120 - BiColorTextWidth(Offscreen.Canvas, S), 44, S);
    S := Format('%d%%', [100 - MyRO.TaxRate - MyRO.LuxRate]);
    RisedTextOut(Offscreen.Canvas,
      16 + 120 - BiColorTextWidth(Offscreen.Canvas, S), 60, S);
    // PaintProgressBar(Offscreen.Canvas,2,16,81,(100-MyRO.LuxRate-MyRO.TaxRate)*120 div 100,0,120,MainTexture);

    // reverse progress bar for science
    X := 16;
    Y := 81;
    current := (100 - MyRO.LuxRate - MyRO.TaxRate) * 120 div 100;
    Max := 120;
    Frame(Offscreen.Canvas, X - 1, Y - 1, X + Max, Y + 7, $000000, $000000);
    RFrame(Offscreen.Canvas, X - 2, Y - 2, X + Max + 1, Y + 8,
      MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
    with Offscreen.Canvas do
    begin
      for I := 0 to current div 8 - 1 do
        BitBltCanvas(Offscreen.Canvas, X + Max - 8 - I * 8, Y, 8, 7,
          HGrSystem.Data.Canvas, 104, 9 + 8 * 2);
      BitBltCanvas(Offscreen.Canvas, X + Max - current, Y, current - 8 * (current div 8), 7,
        HGrSystem.Data.Canvas, 104, 9 + 8 * 2);
      Brush.Color := $000000;
      FillRect(Rect(X, Y, X + Max - current, Y + 7));
      Brush.Style := bsClear;
    end;

    RisedTextOut(Offscreen.Canvas, 16 + 160, 44, Phrases.Lookup('LUX'));
    S := Format('%d%%', [MyRO.LuxRate]);
    RisedTextOut(Offscreen.Canvas,
      16 + 160 { +120-BiColorTextWidth(Offscreen.Canvas,s) } , 60, S);
    PaintProgressBar(Offscreen.Canvas, 5, 16 + 160, 81,
      MyRO.LuxRate * 120 div 100, 0, 120, MainTexture);
    RFrame(Offscreen.Canvas, ScienceBtn.Left - 1, LuxBtn.Top - 1,
      LuxBtn.Left + 12, LuxBtn.Top + 12, MainTexture.ColorBevelShade,
      MainTexture.ColorBevelLight);
  end;

  DLine(Offscreen.Canvas, 1, ClientWidth - 2, 154, MainTexture.ColorBevelShade,
    MainTexture.ColorBevelLight);
  RisedTextOut(Offscreen.Canvas, 16 + 80, 164, Phrases.Lookup('TAXRATE'));
  S := Format('%d%%', [MyRO.TaxRate]);
  RisedTextOut(Offscreen.Canvas,
    16 + 80 { +120-BiColorTextWidth(Offscreen.Canvas,s) } , 180, S);
  PaintProgressBar(Offscreen.Canvas, 0, 16 + 80, 201,
    MyRO.TaxRate * 120 div 100, 0, 120, MainTexture);
  RFrame(Offscreen.Canvas, TaxUpBtn.Left - 1, TaxUpBtn.Top - 1,
    TaxUpBtn.Left + 12, TaxDownBtn.Top + 12, MainTexture.ColorBevelShade,
    MainTexture.ColorBevelLight);

  MarkUsedOffscreen(ClientWidth, ClientHeight);
end;

procedure TRatesDlg.ShowNewContent(NewMode: TWindowMode);
begin
  inherited ShowNewContent(NewMode);
end;

procedure TRatesDlg.FormShow(Sender: TObject);
begin
  Caption := Phrases.Lookup('TITLE_RATES');
  if MyRO.Wonder[woLiberty].EffectiveOwner = Me then
  begin
    ScienceBtn.Visible := False;
    LuxBtn.Visible := False;
  end
  else
  begin
    ScienceBtn.Visible := True;
    LuxBtn.Visible := True;
  end;
  OffscreenPaint;
end;

procedure TRatesDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TRatesDlg.TaxLuxBtnClick(Sender: TObject);
var
  NewTax, NewLux: Integer;
begin
  NewTax := MyRO.TaxRate div 10;
  NewLux := MyRO.LuxRate div 10;
  if Sender = TaxUpBtn then
  begin
    if NewTax < 10 then
      Inc(NewTax);
    if NewTax + NewLux > 10 then
      Dec(NewLux);
  end
  else if (Sender = TaxDownBtn) and (NewTax > 0) then
    Dec(NewTax)
  else if (Sender = ScienceBtn) and (NewLux > 0) then
    Dec(NewLux)
  else if (Sender = LuxBtn) and (NewLux + NewTax < 100) then
    Inc(NewLux);
  if Server(sSetRates, Me, NewTax + NewLux shl 4, nil^) <> eNotChanged then
  begin
    CityOptimizer_BeginOfTurn;
    SmartUpdateContent;
    MainScreen.UpdateViews(True);
  end;
end;

end.
