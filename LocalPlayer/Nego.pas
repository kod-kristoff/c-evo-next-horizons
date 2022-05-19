{$INCLUDE Switches.inc}
unit Nego;

interface

uses
  ScreenTools, BaseWin, Protocol, Term, LCLType, SysUtils, Classes, Graphics,
  Controls, Forms, ButtonA, ButtonB, ButtonN;

const
  MaxHistory = 62;
  scDipNoticeStart = scDipNotice - scDipStart;
  scDipBreakStart = scDipBreak - scDipStart;

type
  THistory = record
    N: Integer;
    Text: array[0 .. MaxHistory - 1] of ansistring;
  end;

  TCommandAllowedEnum = scDipNoticeStart..scDipBreakStart;

  { TNegoDlg }

  TNegoDlg = class(TBufferedDrawDlg)
    OkBtn: TButtonA;
    BwdBtn: TButtonB;
    FwdBtn: TButtonB;
    CloseBtn: TButtonB;
    WantStateReportBtn: TButtonN;
    WantMilReportBtn: TButtonN;
    WantMapBtn: TButtonN;
    WantTech2Btn: TButtonN;
    WantTech1Btn: TButtonN;
    WantModelBtn: TButtonN;
    WantMoneyBtn: TButtonN;
    WantShipPart2Btn: TButtonN;
    WantHiTreatyBtn: TButtonN;
    WantLoTreatyBtn: TButtonN;
    WantShipPart1Btn: TButtonN;
    WantAnythingBtn: TButtonN;
    OfferStateReportBtn: TButtonN;
    OfferMilReportBtn: TButtonN;
    OfferMapBtn: TButtonN;
    OfferTech2Btn: TButtonN;
    OfferTech1Btn: TButtonN;
    OfferModelBtn: TButtonN;
    OfferMoneyBtn: TButtonN;
    OfferShipPart2Btn: TButtonN;
    OfferHiTreatyBtn: TButtonN;
    OfferLoTreatyBtn: TButtonN;
    OfferShipPart1Btn: TButtonN;
    OfferAnythingBtn: TButtonN;
    AcceptBtn: TButtonN;
    PassBtn: TButtonN;
    ExitBtn: TButtonN;
    CancelTreatyBtn: TButtonN;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OkBtnClick(Sender: TObject);
    procedure BwdBtnClick(Sender: TObject);
    procedure FwdBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure WantClick(Sender: TObject);
    procedure OfferClick(Sender: TObject);
    procedure FastBtnClick(Sender: TObject);

  public
    procedure Initiate; // first turn of negotiation, initiate
    procedure Respond; // first turn of negotiation, respond
    procedure Start; // next turn of negotiation
    procedure OffscreenPaint; override;
    procedure ShowNewContent(NewMode: TWindowMode);

  private
    Page, DipCommand: Integer;
    CurrentOffer: TOffer;
    MyAllowed, OppoAllowed: TPriceSet;
    CommandAllowed: set of TCommandAllowedEnum;
    History: array [0 .. nPl - 1] of THistory;
    RomanFont: TFont;
    Costs, Delivers: array [0 .. 11] of Cardinal;
    procedure ResetCurrentOffer;
    procedure BuildCurrentOffer;
    procedure FindAllowed;
    procedure SplitText(Text: string; Bounds: TRect);
    procedure PaintNationPicture(X, Y, P: Integer);
    procedure SetButtonStates;
  end;

var
  NegoDlg: TNegoDlg;


implementation

uses
  Messg, ClientTools, Diplomacy, Inp, Select, NatStat, Tribes, MessgEx;

{$R *.lfm}

const
  xPadC = 140;
  yPadC = 427;
  xPad0 = 140;
  yPad0 = 13;
  xPad1 = 334;
  yPad1 = 13;
  wIcon = 40;
  hIcon = 40;
  wText = 300;
  hText = 256;
  xText0 = 14;
  yText0 = 154;
  xText1 = 326;
  yText1 = 154;
  xNationPicture0 = 20;
  xNationPicture1 = 556;
  yNationPicture = 40;
  yAttitude = 148;
  xCred0 = 42;
  yCred0 = 92;
  xCred1 = 578;
  yCred1 = 92;
  PaperShade = 3;
  PaperBorder_Left = 12;
  PaperBorder_Right = 8;
  ListIndent = 24;

  opLowTreaty = $FE000000;

  RomanNo: array [0 .. 15] of string = ('I', 'II', 'III', 'IV', 'V', 'VI',
    'VII', 'VIII', 'IX', 'X', 'XI', 'XII', 'XIII', 'XIV', 'XV', 'XVI');

  ButtonPrice: array [0 .. 11] of Cardinal = (opChoose, opCivilReport,
    opMilReport, opMap, opAllTech, opAllTech, opAllModel, opMoney, opTreaty,
    opLowTreaty, opShipParts, opShipParts);

procedure TNegoDlg.FormCreate(Sender: TObject);
var
  cix: Integer;
begin
  InitButtons;
  for cix := 0 to ComponentCount - 1 do
    if Components[cix] is TButtonN then
      with TButtonN(Components[cix]) do
      begin
        Graphic := HGrSystem.Data;
        Mask := HGrSystem.Mask;
        BackGraphic := HGrSystem2.Data;
        case Tag shr 8 of
          1: SmartHint := Phrases.Lookup('WANT', ButtonIndex - 6);
          2: SmartHint := Phrases.Lookup('OFFER', ButtonIndex - 6);
        end;
      end;

  FillChar(History, SizeOf(History), 0);
  RomanFont := TFont.Create;
  RomanFont.Name := 'Times New Roman';
  RomanFont.Size := Round(144 * 72 / RomanFont.PixelsPerInch);
  RomanFont.Color := Colors.Canvas.Pixels[clkMisc, cliPaper];
  HelpContext := 'DIPLOMACY';
  OkBtn.Caption := Phrases.Lookup('BTN_OK');
  AcceptBtn.SmartHint := Phrases.Lookup('BTN_ACCEPT');
  ExitBtn.SmartHint := Phrases.Lookup('BTN_BREAK');
  CancelTreatyBtn.SmartHint := Phrases.Lookup('BTN_CNTREATY');
end;

procedure TNegoDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(RomanFont);
end;

procedure TNegoDlg.FormShow(Sender: TObject);
begin
  OffscreenPaint;
end;

procedure TNegoDlg.ResetCurrentOffer;
var
  I: Integer;
begin
  CurrentOffer.nDeliver := 0;
  CurrentOffer.nCost := 0;
  for I := 0 to 11 do
    Costs[I] := $FFFFFFFF;
  for I := 0 to 11 do
    Delivers[I] := $FFFFFFFF;
end;

procedure TNegoDlg.ShowNewContent(NewMode: TWindowMode);
begin
  inherited ShowNewContent(NewMode);
  SetButtonStates;
  if (ClientMode = scDipCancelTreaty) or (ClientMode = scDipBreak) then
    PassBtn.SmartHint := Phrases.Lookup('BTN_NOTICE')
  else
    PassBtn.SmartHint := Phrases.Lookup('BTN_PASS');
  case MyRO.Treaty[DipMem[Me].pContact] of
    trNone:
      begin
        WantHiTreatyBtn.SmartHint := Phrases.Lookup('BTN_WANTPEACE');
        OfferHiTreatyBtn.SmartHint := Phrases.Lookup('BTN_OFFERPEACE');
        // WantLoTreatyBtn.SmartHint:=Phrases.Lookup('BTN_WANTCEASEFIRE');
        // OfferLoTreatyBtn.SmartHint:=Phrases.Lookup('BTN_OFFERCEASEFIRE');
      end;
    { trCeasefire:
      begin
      WantHiTreatyBtn.SmartHint:=Phrases.Lookup('BTN_WANTPEACE');
      OfferHiTreatyBtn.SmartHint:=Phrases.Lookup('BTN_OFFERPEACE');
      end; }
    trPeace:
      begin
        WantHiTreatyBtn.SmartHint := Phrases.Lookup('BTN_WANTFRIENDLY');
        OfferHiTreatyBtn.SmartHint := Phrases.Lookup('BTN_OFFERFRIENDLY');
        // WantLoTreatyBtn.SmartHint:=Phrases.Lookup('BTN_WANTENDPEACE');
        // OfferLoTreatyBtn.SmartHint:=Phrases.Lookup('BTN_OFFERENDPEACE');
      end;
    trFriendlyContact:
      begin
        WantHiTreatyBtn.SmartHint := Phrases.Lookup('BTN_WANTALLIANCE');
        OfferHiTreatyBtn.SmartHint := Phrases.Lookup('BTN_OFFERALLIANCE');
      end;
    { trAlliance:
      begin
      WantLoTreatyBtn.SmartHint:=Phrases.Lookup('BTN_WANTENDALLIANCE');
      OfferLoTreatyBtn.SmartHint:=Phrases.Lookup('BTN_OFFERENDALLIANCE');
      end; }
  end;
end;

procedure TNegoDlg.Start;
begin
  if ClientMode <> scDipStart then
    with History[Me] do
    begin
      if N = MaxHistory then
      begin
        Move(Text[2], Text[0], (MaxHistory - 2) * SizeOf(Integer));
        Dec(N, 2);
      end;
      Text[N] := Copy(DipCommandToString(DipMem[Me].pContact, Me,
        DipMem[Me].FormerTreaty, DipMem[Me].SentCommand, ClientMode,
        DipMem[Me].SentOffer, ReceivedOffer), 1, 255);
      Inc(N);
    end;
  Assert(History[Me].N mod 2 = 1);

  Page := History[Me].N;
  FindAllowed;
  ResetCurrentOffer;

  (* if (ClientMode=scDipOffer) and (ReceivedOffer.nDeliver=1)
    and (ReceivedOffer.nCost=0) and (ReceivedOffer.Price[0] and opMask=opTreaty) then
    begin // prepare to demand price for treaty
    CurrentOffer.nDeliver:=1;
    CurrentOffer.Price[0]:=ReceivedOffer.Price[0];
    CurrentOffer.nCost:=0;
    end
    else
    begin
    if (ClientMode=scDipOffer) and (ReceivedOffer.nCost>0) then
    begin
    CurrentOffer.nDeliver:=1;
    CurrentOffer.Price[0]:=ReceivedOffer.Price[ReceivedOffer.nDeliver]
    end
    else CurrentOffer.nDeliver:=0;
    if (ClientMode=scDipOffer) and (ReceivedOffer.nDeliver>0) then
    begin
    CurrentOffer.nCost:=1;
    CurrentOffer.Price[CurrentOffer.nDeliver]:=ReceivedOffer.Price[0]
    end
    else CurrentOffer.nCost:=0
    end; *)
  DipCommand := -1;
  ShowNewContent(wmPersistent);
end;

procedure TNegoDlg.SplitText(Text: string; Bounds: TRect);
var
  nLines, Line, Start, Stop, OrdinaryStop, Indent, Y: Integer;
  S: string;
  preview, Dot: Boolean;
begin
  nLines := 0;
  for preview := True downto False do
  begin
    Start := 1;
    Line := 0;
    Indent := 0;
    while Start < Length(Text) do
    begin
      Dot := False;
      if (Start = 1) or (Text[Start - 1] = '\') then
        if Text[Start] = '-' then
        begin
          Indent := ListIndent;
          Inc(Start);
          if Start = Length(Text) then
            Break;
          Dot := True;
        end
        else
          Indent := 0;
      Stop := Start;
      while (Stop < Length(Text)) and (Text[Stop] <> '\') do
      begin
        Inc(Stop);
        if BiColorTextWidth(Offscreen.Canvas,
          Copy(Text, Start, Stop - Start + 1)) > Bounds.Right - Bounds.Left -
          PaperBorder_Left - PaperBorder_Right - Indent then
        begin
          Dec(Stop);
          Break
        end;
      end;
      if Stop <> Length(Text) then
      begin
        OrdinaryStop := Stop;
        while (Text[OrdinaryStop + 1] <> ' ') and
          (Text[OrdinaryStop + 1] <> '\') do
          Dec(OrdinaryStop);
        if (OrdinaryStop + 1 - Start) * 2 >= Stop - Start then
          Stop := OrdinaryStop
      end;
      if not preview then
      begin
        Y := (Bounds.Top + Bounds.Bottom) div 2 - 10 * nLines + 20 * Line - 1;
        if Dot then
          Sprite(Offscreen, HGrSystem, Bounds.Left + PaperBorder_Left +
            (ListIndent - 14), Y + 7, 8, 8, 90, 16);
        S := Copy(Text, Start, Stop - Start + 1);
        BiColorTextOut(Offscreen.Canvas, Colors.Canvas.Pixels[clkMisc,
          cliPaperText], $7F007F, Bounds.Left + PaperBorder_Left +
          Indent, Y, S);
      end;
      Inc(Line);
      Start := Stop + 2;
    end;
    nLines := Line;
  end
end;

procedure TNegoDlg.FindAllowed;
var
  I: Integer;
begin
  CommandAllowed := [scDipOffer - scDipStart];
  if ClientMode <> scDipBreak then
    Include(CommandAllowed, scDipBreak - scDipStart);
  if MyRO.Treaty[DipMem[Me].pContact] >= trPeace then
    Include(CommandAllowed, scDipCancelTreaty - scDipStart);
  if (ClientMode = scDipOffer) and (Server(scDipAccept - sExecute, Me, 0, nil^)
    >= rExecuted) then
    Include(CommandAllowed, scDipAccept - scDipStart);

  MyAllowed := [opChoose shr 24, opMoney shr 24];
  OppoAllowed := [opChoose shr 24, opMoney shr 24];
  if not IsCivilReportNew(DipMem[Me].pContact) then
  begin // no up-to-date civil report
    MyAllowed := MyAllowed + [opCivilReport shr 24];
    for I := 0 to nAdv - 1 do
      if MyRO.Tech[I] >= tsApplicable then
      begin
        MyAllowed := MyAllowed + [opAllTech shr 24];
        Break
      end;
    OppoAllowed := OppoAllowed + [opCivilReport shr 24, opAllTech shr 24];
  end
  else
  begin // check techs
    for I := 0 to nAdv - 1 do
      if not(I in FutureTech) then
        if (MyRO.Tech[I] < tsSeen) and
          (MyRO.EnemyReport[DipMem[Me].pContact].Tech[I] >= tsApplicable) then
          OppoAllowed := OppoAllowed + [opAllTech shr 24]
        else if (MyRO.EnemyReport[DipMem[Me].pContact].Tech[I] < tsSeen) and
          (MyRO.Tech[I] >= tsApplicable) then
          MyAllowed := MyAllowed + [opAllTech shr 24];
  end;
  if not IsMilReportNew(DipMem[Me].pContact) then
  begin // no up-to-date military report
    MyAllowed := MyAllowed + [opMilReport shr 24];
    if MyRO.nModel > 3 then
      MyAllowed := MyAllowed + [opAllModel shr 24];
    OppoAllowed := OppoAllowed + [opMilReport shr 24, opAllModel shr 24];
  end
  else
  begin
    if ModalSelectDlg.OnlyChoice(kChooseModel) <> mixAll then
      MyAllowed := MyAllowed + [opAllModel shr 24];
    if ModalSelectDlg.OnlyChoice(kChooseEModel) <> mixAll then
      OppoAllowed := OppoAllowed + [opAllModel shr 24];
  end;
  if MyRO.Treaty[DipMem[Me].pContact] < trAlliance then
  begin
    MyAllowed := MyAllowed + [opTreaty shr 24, opMap shr 24];
    OppoAllowed := OppoAllowed + [opTreaty shr 24, opMap shr 24];
  end;
  { if MyRO.Treaty[DipMem[me].pContact] in [trNone,trPeace,trAlliance] then
    begin
    MyAllowed:=MyAllowed+[opLowTreaty shr 24];
    OppoAllowed:=OppoAllowed+[opLowTreaty shr 24];
    end; }
  for I := 0 to nShipPart - 1 do
  begin
    if MyRO.Ship[Me].Parts[I] > 0 then
      Include(MyAllowed, opShipParts shr 24);
    if MyRO.Ship[DipMem[Me].pContact].Parts[I] > 0 then
      Include(OppoAllowed, opShipParts shr 24);
  end;
  MyAllowed := MyAllowed - DipMem[Me].DeliveredPrices *
    [opAllTech shr 24, opAllModel shr 24, opCivilReport shr 24,
    opMilReport shr 24, opMap shr 24];
  OppoAllowed := OppoAllowed - DipMem[Me].ReceivedPrices *
    [opAllTech shr 24, opAllModel shr 24, opCivilReport shr 24,
    opMilReport shr 24, opMap shr 24];
end;

procedure TNegoDlg.PaintNationPicture(X, Y, P: Integer);
begin
  with Offscreen.Canvas do
  begin
    Pen.Color := $000000;
    Brush.Color := Tribe[P].Color;
    Rectangle(X - 6, Y - 1, X + 70, Y + 49);
    Brush.Color := $000000;
    Tribe[P].InitAge(GetAge(P));
    if Assigned(Tribe[P].faceHGr) then
      Dump(Offscreen, Tribe[P].faceHGr, X, Y, 64, 48,
        1 + Tribe[P].facepix mod 10 * 65, 1 + Tribe[P].facepix div 10 * 49)
    else
      FillRect(Rect(X, Y, X + 64, Y + 48));
    Brush.Style := bsClear;
    ScreenTools.Frame(Offscreen.Canvas, X - 1, Y - 1, X + 64, Y + 48, $000000, $000000);
  end
end;

procedure TNegoDlg.SetButtonStates;
var
  cix: Integer;
  IsActionPage: Boolean;
begin
  IsActionPage := Page = History[Me].N;

  AcceptBtn.Possible := IsActionPage and
    (scDipAccept - scDipStart in CommandAllowed);
  AcceptBtn.Lit := DipCommand = scDipAccept;
  PassBtn.Possible := IsActionPage and
    (scDipOffer - scDipStart in CommandAllowed);
  PassBtn.Lit := (DipCommand = scDipNotice) or (DipCommand = scDipOffer) and
    (CurrentOffer.nDeliver = 0) and (CurrentOffer.nCost = 0);
  ExitBtn.Possible := IsActionPage and
    (scDipBreak - scDipStart in CommandAllowed);
  ExitBtn.Lit := DipCommand = scDipBreak;
  CancelTreatyBtn.Possible := IsActionPage and
    (scDipCancelTreaty - scDipStart in CommandAllowed);
  CancelTreatyBtn.Lit := DipCommand = scDipCancelTreaty;

  for cix := 0 to ComponentCount - 1 do
    if Components[cix] is TButtonN then
      with TButtonN(Components[cix]) do
        case Tag shr 8 of
          1: // Costs
            begin
              Possible := IsActionPage and
                (ButtonPrice[Tag and $FF] shr 24 in OppoAllowed);
              Lit := Costs[Tag and $FF] <> $FFFFFFFF;
            end;
          2: // Delivers
            begin
              Possible := IsActionPage and
                (ButtonPrice[Tag and $FF] shr 24 in MyAllowed);
              Lit := Delivers[Tag and $FF] <> $FFFFFFFF;
            end;
        end;
end;

procedure TNegoDlg.OffscreenPaint;
var
  I, cred: Integer;
  S: string;
  OkEnabled: Boolean;
begin
  if (OffscreenUser <> nil) and (OffscreenUser <> self) then
    OffscreenUser.Update;
  // complete working with old owner to prevent rebound
  OffscreenUser := self;

  if (DipCommand >= 0) and (Page = History[Me].N) then
    History[Me].Text[History[Me].N] :=
      Copy(DipCommandToString(Me, DipMem[Me].pContact,
      MyRO.Treaty[DipMem[Me].pContact], ClientMode, DipCommand, ReceivedOffer,
      CurrentOffer), 1, 255);

  FwdBtn.Visible := Page < History[Me].N;
  BwdBtn.Visible := Page >= 2;
  if Page < History[Me].N then
    OkEnabled := False
  else if DipCommand = scDipOffer then
    OkEnabled := Server(scDipOffer - sExecute, Me, 0, CurrentOffer) >= rExecuted
  else
    OkEnabled := DipCommand >= 0;
  OkBtn.Visible := OkEnabled;

  Fill(Offscreen.Canvas, 3, 3, ClientWidth - 6, ClientHeight - 6,
    (Maintexture.Width - ClientWidth) div 2, (Maintexture.Height - ClientHeight) div 2);
  Frame(Offscreen.Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);
  Frame(Offscreen.Canvas, 1, 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Offscreen.Canvas, 2, 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Corner(Offscreen.Canvas, 1, 1, 0, MainTexture);
  Corner(Offscreen.Canvas, ClientWidth - 9, 1, 1, MainTexture);
  Corner(Offscreen.Canvas, 1, ClientHeight - 9, 2, MainTexture);
  Corner(Offscreen.Canvas, ClientWidth - 9, ClientHeight - 9, 3, MainTexture);

  BtnFrame(Offscreen.Canvas, OkBtn.BoundsRect, MainTexture);
  BtnFrame(Offscreen.Canvas, BwdBtn.BoundsRect, MainTexture);
  BtnFrame(Offscreen.Canvas, FwdBtn.BoundsRect, MainTexture);
  BtnFrame(Offscreen.Canvas, CloseBtn.BoundsRect, MainTexture);

  RFrame(Offscreen.Canvas, xPadC - 2, yPadC - 2, xPadC + 41 + 42 * 3,
    yPadC + 41, $FFFFFF, $B0B0B0);
  RFrame(Offscreen.Canvas, xPad0 - 2, yPad0 - 2, xPad0 + 41 + 42 * 3,
    yPad0 + 41 + 42 * 2, $FFFFFF, $B0B0B0);
  RFrame(Offscreen.Canvas, xPad1 - 2, yPad1 - 2, xPad1 + 41 + 42 * 3,
    yPad1 + 41 + 42 * 2, $FFFFFF, $B0B0B0);

  PaintNationPicture(xNationPicture0, yNationPicture, DipMem[Me].pContact);
  PaintNationPicture(xNationPicture1, yNationPicture, Me);

  if History[Me].Text[Page - 1] <> '' then
  begin
    FillSeamless(Offscreen.Canvas, xText0, yText0, wText, hText, 0, 0, Paper);
    I := Page - 1;
    if History[Me].Text[0] = '' then
      Dec(I);
    if I < 16 then
    begin
      Offscreen.Canvas.Font.Assign(RomanFont);
      Offscreen.Canvas.TextOut
        (xText0 + (wText - Offscreen.Canvas.TextWidth(RomanNo[I])) div 2,
        yText0 + (hText - Offscreen.Canvas.TextHeight(RomanNo[I])) div 2,
        RomanNo[I]);
    end
  end;
  FillSeamless(Offscreen.Canvas, xText1, yText1, wText, hText, 0, 0, Paper);
  I := Page;
  if History[Me].Text[0] = '' then
    Dec(I);
  if I < 16 then
  begin
    Offscreen.Canvas.Font.Assign(RomanFont);
    Offscreen.Canvas.TextOut
      (xText1 + (wText - Offscreen.Canvas.TextWidth(RomanNo[I])) div 2,
      yText1 + (hText - Offscreen.Canvas.TextHeight(RomanNo[I])) div 2,
      RomanNo[I]);
  end;
  with Offscreen.Canvas do
  begin
    Brush.Color := MainTexture.ColorBevelShade;
    if History[Me].Text[Page - 1] <> '' then
    begin
      FillRect(Rect(xText0 + wText, yText0 + PaperShade,
        xText0 + wText + PaperShade, yText0 + hText + PaperShade));
      FillRect(Rect(xText0 + PaperShade, yText0 + hText,
        xText0 + wText + PaperShade, yText0 + hText + PaperShade));
    end;
    FillRect(Rect(xText1 + wText, yText1 + PaperShade,
      xText1 + wText + PaperShade, yText1 + hText + PaperShade));
    FillRect(Rect(xText1 + PaperShade, yText1 + hText,
      xText1 + wText + PaperShade, yText1 + hText + PaperShade));
    Brush.Style := bsClear;
  end;

  Offscreen.Canvas.Font.Assign(UniFont[ftNormal]);

  { if Page=History[me].n then
    begin // show attitude
    S:=Phrases.Lookup('ATTITUDE',MyRO.EnemyReport[DipMem[Me].pContact].Attitude);
    //LoweredTextOut(Offscreen.Canvas,-1,MainTexture,
    RisedTextOut(Offscreen.Canvas,xText0+wText div 2-
    BiColorTextWidth(Offscreen.Canvas,S) div 2,yAttitude,S);
    S:=Phrases.Lookup('ATTITUDE',MyRO.Attitude[DipMem[Me].pContact]);
    //LoweredTextOut(Offscreen.Canvas,-1,MainTexture,
    RisedTextOut(Offscreen.Canvas,xText1+wText div 2-
    BiColorTextWidth(Offscreen.Canvas,S) div 2,yAttitude,S);
    end; }

  if History[Me].Text[Page - 1] <> '' then
    SplitText(History[Me].Text[Page - 1], Rect(xText0, yText0, xText0 + wText,
      yText0 + hText));
  if (Page < History[Me].N) or OkEnabled then
    SplitText(History[Me].Text[Page], Rect(xText1, yText1, xText1 + wText,
      yText1 + hText));

  // show credibility
  Offscreen.Canvas.Font.Assign(UniFont[ftTiny]);
  cred := MyRO.EnemyReport[DipMem[Me].pContact].Credibility;
  case cred of
    0 .. 49:
      I := 3;
    50 .. 90:
      I := 0;
    91 .. 100:
      I := 1;
  end;
  PaintProgressBar(Offscreen.Canvas, I, xCred0, yCred0 + 17, (cred + 2) div 5,
    0, 20, MainTexture);
  S := IntToStr(cred);
  RisedTextOut(Offscreen.Canvas, xCred0 + 10 -
    (BiColorTextWidth(Offscreen.Canvas, S) + 1) div 2, yCred0, S);
  case MyRO.Credibility of
    0 .. 49:
      I := 3;
    50 .. 90:
      I := 0;
    91 .. 100:
      I := 1;
  end;
  PaintProgressBar(Offscreen.Canvas, I, xCred1, yCred1 + 17,
    (MyRO.Credibility + 2) div 5, 0, 20, MainTexture);
  S := IntToStr(MyRO.Credibility);
  RisedTextOut(Offscreen.Canvas, xCred1 + 10 -
    (BiColorTextWidth(Offscreen.Canvas, S) + 1) div 2, yCred1, S);

  MarkUsedOffscreen(ClientWidth, ClientHeight);
end;

procedure TNegoDlg.Initiate;
begin
  History[Me].N := 1;
  History[Me].Text[0] := '';
end;

procedure TNegoDlg.Respond;
begin
  History[Me].N := 0;
end;

procedure TNegoDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (X >= xNationPicture0) and (X < xNationPicture0 + 64) and
    (Y >= yNationPicture) and (Y < yNationPicture + 48) then
    NatStatDlg.ShowNewContent(WindowModeMakePersistent(FWindowMode), DipMem[Me].pContact)
  else if (X >= xNationPicture1) and (X < xNationPicture1 + 64) and
    (Y >= yNationPicture) and (Y < yNationPicture + 48) then
    NatStatDlg.ShowNewContent(WindowModeMakePersistent(FWindowMode), Me)
end;

procedure TNegoDlg.BwdBtnClick(Sender: TObject);
begin
  Dec(Page, 2);
  SetButtonStates;
  SmartUpdateContent;
end;

procedure TNegoDlg.FwdBtnClick(Sender: TObject);
begin
  Inc(Page, 2);
  SetButtonStates;
  SmartUpdateContent;
end;

procedure TNegoDlg.OkBtnClick(Sender: TObject);
begin
  Inc(History[Me].N);
  if DipCommand = scDipOffer then
    MainScreen.OfferCall(CurrentOffer)
  else
    MainScreen.DipCall(DipCommand);
end;

procedure TNegoDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TNegoDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if OkBtn.Visible then
      OkBtnClick(nil);
  end
  else
    inherited;
end;

procedure TNegoDlg.BuildCurrentOffer;
var
  I: Integer;
begin
  CurrentOffer.nDeliver := 0;
  CurrentOffer.nCost := 0;
  for I := 0 to 11 do
    if Delivers[I] <> $FFFFFFFF then
    begin
      CurrentOffer.Price[CurrentOffer.nDeliver] := Delivers[I];
      Inc(CurrentOffer.nDeliver);
    end;
  for I := 0 to 11 do
    if Costs[I] <> $FFFFFFFF then
    begin
      CurrentOffer.Price[CurrentOffer.nDeliver + CurrentOffer.nCost] :=
        Costs[I];
      Inc(CurrentOffer.nCost);
    end;
end;

procedure TNegoDlg.WantClick(Sender: TObject);
var
  A, I, Max: Integer;
  Price: Cardinal;
begin
  if (Page <> History[Me].N) or (ClientMode = scDipCancelTreaty) or
    (ClientMode = scDipBreak) then
    Exit;
  if Costs[TButtonN(Sender).Tag and $FF] <> $FFFFFFFF then
    Price := $FFFFFFFF // toggle off
  else
  begin
    if CurrentOffer.nCost >= 2 then
    begin
      SimpleMessage(Phrases.Lookup('MAX2WANTS'));
      Exit
    end;
    Price := ButtonPrice[TButtonN(Sender).Tag and $FF];
    if not(Price shr 24 in OppoAllowed) then
      Exit;
    case Price of
      opCivilReport, opMilReport:
        Inc(Price, DipMem[Me].pContact shl 16 + MyRO.Turn);
        // !!! choose player and year!
      opMoney:
        begin // choose amount
          InputDlg.Caption := Phrases.Lookup('TITLE_AMOUNT');
          InputDlg.EInput.Text := '';
          InputDlg.CenterToRect(BoundsRect);
          InputDlg.ShowModal;
          if InputDlg.ModalResult <> mrOK then
            Exit;
          val(InputDlg.EInput.Text, A, I);
          if (I <> 0) or (A <= 0) or (A >= MaxMoneyPrice) then
            Exit;
          Inc(Price, A);
        end;
      opShipParts:
        begin // choose type and number
          if MyRO.NatBuilt[imSpacePort] = 0 then
            with MessgExDlg do
            begin
              OpenSound := 'WARNING_LOWSUPPORT';
              MessgText := Phrases.Lookup('NOSPACEPORT');
              Kind := mkYesNo;
              IconKind := mikImp;
              IconIndex := imSpacePort;
              ShowModal;
              if ModalResult <> mrOK then
                Exit
            end;
          ModalSelectDlg.ShowNewContent(wmModal, kEShipPart);
          if ModalSelectDlg.Result < 0 then
            Exit;
          Inc(Price, ModalSelectDlg.Result shl 16);
          Max := MyRO.Ship[DipMem[Me].pContact].Parts[ModalSelectDlg.Result];
          InputDlg.Caption := Phrases.Lookup('TITLE_NUMBER');
          InputDlg.EInput.Text := '';
          InputDlg.CenterToRect(BoundsRect);
          InputDlg.ShowModal;
          if InputDlg.ModalResult <> mrOK then
            Exit;
          val(InputDlg.EInput.Text, A, I);
          if (I <> 0) or (A <= 0) then
            Exit;
          if A > Max then
            A := Max;
          if A > MaxShipPartPrice then
            A := MaxShipPartPrice;
          Inc(Price, A);
        end;
      opAllTech:
        begin // choose technology
          ModalSelectDlg.ShowNewContent(wmModal, kChooseETech);
          if ModalSelectDlg.Result < 0 then
            Exit;
          if ModalSelectDlg.Result = adAll then
            Price := opAllTech
          else
            Price := OpTech + ModalSelectDlg.Result;
        end;
      opAllModel:
        begin // choose model
          ModalSelectDlg.ShowNewContent(wmModal, kChooseEModel);
          if ModalSelectDlg.Result < 0 then
            Exit;
          if ModalSelectDlg.Result = mixAll then
            Price := opAllModel
          else
            Price := OpModel + MyRO.EnemyModel[ModalSelectDlg.Result].mix;
        end;
      opTreaty:
        begin
          if MyRO.Treaty[DipMem[Me].pContact] < trPeace then
            Price := opTreaty + trPeace
          else
            Price := opTreaty + MyRO.Treaty[DipMem[Me].pContact] + 1;
        end;
      { opLowTreaty:
        begin
        if MyRO.Treaty[DipMem[Me].pContact]=trNone then Price:=opTreaty+trCeaseFire
        else Price:=opTreaty+MyRO.Treaty[DipMem[Me].pContact]-1;
        end }
    end;
  end;

  Costs[TButtonN(Sender).Tag and $FF] := Price;
  BuildCurrentOffer;
  DipCommand := scDipOffer;
  SetButtonStates;
  SmartUpdateContent;
end;

procedure TNegoDlg.OfferClick(Sender: TObject);
var
  A, I, Max: Integer;
  Price: Cardinal;
begin
  if (Page <> History[Me].N) or (ClientMode = scDipCancelTreaty) or
    (ClientMode = scDipBreak) then
    Exit;
  if Delivers[TButtonN(Sender).Tag and $FF] <> $FFFFFFFF then
    Price := $FFFFFFFF // toggle off
  else
  begin
    if CurrentOffer.nDeliver >= 2 then
    begin
      SimpleMessage(Phrases.Lookup('MAX2OFFERS'));
      Exit;
    end;
    Price := ButtonPrice[TButtonN(Sender).Tag and $FF];
    if not(Price shr 24 in MyAllowed) then
      Exit;
    case Price of
      opCivilReport, opMilReport:
        Inc(Price, Me shl 16 + MyRO.Turn); // !!! choose player and year!
      opMoney:
        begin // choose amount
          InputDlg.Caption := Phrases.Lookup('TITLE_AMOUNT');
          InputDlg.EInput.Text := '';
          InputDlg.CenterToRect(BoundsRect);
          InputDlg.ShowModal;
          if InputDlg.ModalResult <> mrOK then
            Exit;
          val(InputDlg.EInput.Text, A, I);
          if (I <> 0) or (A <= 0) or (A >= MaxMoneyPrice) then
            Exit;
          if (Price = opMoney) and (A > MyRO.Money) then
            A := MyRO.Money;
          Inc(Price, A);
        end;
      opShipParts:
        begin // choose type and number
          ModalSelectDlg.ShowNewContent(wmModal, kShipPart);
          if ModalSelectDlg.Result < 0 then
            Exit;
          Inc(Price, ModalSelectDlg.Result shl 16);
          Max := MyRO.Ship[Me].Parts[ModalSelectDlg.Result];
          InputDlg.Caption := Phrases.Lookup('TITLE_NUMBER');
          InputDlg.EInput.Text := '';
          InputDlg.CenterToRect(BoundsRect);
          InputDlg.ShowModal;
          if InputDlg.ModalResult <> mrOK then
            Exit;
          val(InputDlg.EInput.Text, A, I);
          if (I <> 0) or (A <= 0) then
            Exit;
          if A > Max then
            A := Max;
          if A > MaxShipPartPrice then
            A := MaxShipPartPrice;
          Inc(Price, A);
        end;
      opAllTech:
        begin // choose technology
          ModalSelectDlg.ShowNewContent(wmModal, kChooseTech);
          if ModalSelectDlg.Result < 0 then
            Exit;
          if ModalSelectDlg.Result = adAll then
            Price := opAllTech
          else
            Price := OpTech + ModalSelectDlg.Result;
        end;
      opAllModel:
        begin // choose model
          ModalSelectDlg.ShowNewContent(wmModal, kChooseModel);
          if ModalSelectDlg.Result < 0 then
            Exit;
          if ModalSelectDlg.Result = mixAll then
            Price := opAllModel
          else
            Price := OpModel + ModalSelectDlg.Result;
        end;
      opTreaty:
        begin
          if MyRO.Treaty[DipMem[Me].pContact] < trPeace then
            Price := opTreaty + trPeace
          else
            Price := opTreaty + MyRO.Treaty[DipMem[Me].pContact] + 1;
        end;
      { opLowTreaty:
        begin
        if MyRO.Treaty[DipMem[Me].pContact]=trNone then Price:=opTreaty+trCeaseFire
        else Price:=opTreaty+MyRO.Treaty[DipMem[Me].pContact]-1;
        end }
    end;
  end;

  Delivers[TButtonN(Sender).Tag and $FF] := Price;
  BuildCurrentOffer;
  DipCommand := scDipOffer;
  SetButtonStates;
  SmartUpdateContent;
end;

procedure TNegoDlg.FastBtnClick(Sender: TObject);
var
  NewCommand: Cardinal;
begin
  if Page <> History[Me].N then
    Exit;
  NewCommand := TButtonN(Sender).Tag and $FF + scDipStart;
  if not(NewCommand - scDipStart in CommandAllowed) then
    Exit;
  if (NewCommand = scDipCancelTreaty) and
    (MyRO.Turn < MyRO.LastCancelTreaty[DipMem[Me].pContact] + CancelTreatyTurns)
  then
  begin
    SimpleMessage(Phrases.Lookup('CANCELTREATYRUSH'));
    Exit;
  end;
  if (NewCommand = scDipOffer) and ((ClientMode = scDipCancelTreaty) or
    (ClientMode = scDipBreak)) then
    DipCommand := scDipNotice
  else
    DipCommand := NewCommand;
  ResetCurrentOffer;
  SetButtonStates;
  SmartUpdateContent;
end;

end.
