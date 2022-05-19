{$INCLUDE Switches.inc}
unit NatStat;

interface

uses
  Protocol, ClientTools, Term, ScreenTools, BaseWin, LCLIntf, LCLType, SysUtils,
  Classes, Graphics, Controls, Forms, ButtonB, ButtonC, Menus, EOTButton;

type
  PEnemyReport = ^TEnemyReport;

  TNatStatDlg = class(TBufferedDrawDlg)
    ToggleBtn: TButtonB;
    CloseBtn: TButtonB;
    Popup: TPopupMenu;
    ScrollUpBtn: TButtonC;
    ScrollDownBtn: TButtonC;
    ContactBtn: TEOTButton;
    TellAIBtn: TButtonC;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure DialogBtnClick(Sender: TObject);
    procedure ToggleBtnClick(Sender: TObject);
    procedure PlayerClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollUpBtnClick(Sender: TObject);
    procedure ScrollDownBtnClick(Sender: TObject);
    procedure TellAIBtnClick(Sender: TObject);
  public
    procedure CheckAge;
    procedure ShowNewContent(NewMode: TWindowMode; P: Integer = -1);
    procedure EcoChange;
  protected
    procedure OffscreenPaint; override;
  private
    pView, AgePrepared, LinesDown: Integer;
    SelfReport, CurrentReport: PEnemyReport;
    ShowContact, ContactEnabled: Boolean;
    Back, Template: TBitmap;
    ReportText: TStringList;
    procedure GenerateReportText;
  end;

var
  NatStatDlg: TNatStatDlg;


implementation

{$R *.lfm}

uses
  Messg, Tribes, Directories;

const
  xIcon = 326;
  yIcon = 49;
  xAttrib = 96;
  yAttrib = 40;
  xRelation = 16;
  yRelation = 110;
  PaperShade = 3;
  ReportLines = 12;
  LineSpacing = 22;
  xReport = 24;
  yReport = 165;
  wReport = 352;
  hReport = ReportLines * LineSpacing;

procedure TNatStatDlg.FormCreate(Sender: TObject);
begin
  inherited;
  AgePrepared := -2;
  GetMem(SelfReport, SizeOf(TEnemyReport) - 2 * (INFIN + 1));
  ReportText := TStringList.Create;
  InitButtons;
  ContactBtn.Template := Templates.Data;
  HelpContext := 'DIPLOMACY';
  ToggleBtn.Hint := Phrases.Lookup('BTN_SELECT');
  ContactBtn.Hint := Phrases.Lookup('BTN_DIALOG');

  Back := TBitmap.Create;
  Back.PixelFormat := pf24bit;
  Back.SetSize(Width, Height);
  Back.Canvas.FillRect(0, 0, Back.Width, Back.Height);
  Template := TBitmap.Create;
  Template.PixelFormat := pf24bit;
  LoadGraphicFile(Template, GetGraphicsDir + DirectorySeparator + 'Nation.png',
    [gfNoGamma]);
end;

procedure TNatStatDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ReportText);
  FreeMem(SelfReport);
  FreeAndNil(Template);
  FreeAndNil(Back);
end;

procedure TNatStatDlg.CheckAge;
begin
  if MainTexture.Age <> AgePrepared then begin
    AgePrepared := MainTexture.Age;
    BitBltCanvas(Back.Canvas, 0, 0, ClientWidth, ClientHeight,
      MainTexture.Image.Canvas, (MainTexture.Width - ClientWidth) div 2,
      (MainTexture.Height - ClientHeight) div 2);
    ImageOp_B(Back, Template, 0, 0, 0, 0, ClientWidth, ClientHeight);
  end;
end;

procedure TNatStatDlg.FormShow(Sender: TObject);
begin
  if pView = Me then
  begin
    SelfReport.TurnOfCivilReport := MyRO.Turn;
    SelfReport.TurnOfMilReport := MyRO.Turn;
    Move(MyRO.Treaty, SelfReport.Treaty, SizeOf(SelfReport.Treaty));
    SelfReport.Government := MyRO.Government;
    SelfReport.Money := MyRO.Money;
    CurrentReport := Pointer(SelfReport);
  end
  else
    CurrentReport := Pointer(MyRO.EnemyReport[pView]);
  if CurrentReport.TurnOfCivilReport >= 0 then
    GenerateReportText;
  ShowContact := (pView <> Me) and (not Supervising or (Me <> 0));
  ContactEnabled := ShowContact and not Supervising and
    (1 shl pView and MyRO.Alive <> 0);
  ContactBtn.Visible := ContactEnabled and (MyRO.Happened and phGameEnd = 0) and
    (ClientMode < scContact);
  ScrollUpBtn.Visible := (CurrentReport.TurnOfCivilReport >= 0) and
    (ReportText.Count > ReportLines);
  ScrollDownBtn.Visible := (CurrentReport.TurnOfCivilReport >= 0) and
    (ReportText.Count > ReportLines);
  if soTellAI in OptionChecked then
    TellAIBtn.ButtonIndex := 3
  else
    TellAIBtn.ButtonIndex := 2;
  Caption := Tribe[pView].TPhrase('TITLE_NATION');
  LinesDown := 0;

  OffscreenPaint;
end;

procedure TNatStatDlg.ShowNewContent(NewMode: TWindowMode; P: Integer);
begin
  if P < 0 then
    if ClientMode >= scContact then
      pView := DipMem[Me].pContact
    else
    begin
      pView := 0;
      while (pView < nPl) and ((MyRO.Treaty[pView] < trNone) or
        (1 shl pView and MyRO.Alive = 0)) do
        Inc(pView);
      if pView >= nPl then
        pView := Me;
    end
  else
    pView := P;
  inherited ShowNewContent(NewMode);
end;

procedure TNatStatDlg.PlayerClick(Sender: TObject);
begin
  ShowNewContent(FWindowMode, TComponent(Sender).Tag);
end;

procedure TNatStatDlg.GenerateReportText;
var
  List: ^TChart;

  function StatText(no: Integer): string;
  var
    I: Integer;
  begin
    if (CurrentReport.TurnOfCivilReport >= 0) and
      (Server(sGetChart + no shl 4, Me, pView, List^) >= rExecuted) then
    begin
      I := List[CurrentReport.TurnOfCivilReport];
      case no of
        stPop:
          Result := Format(Phrases.Lookup('FRSTATPOP'), [I]);
        stTerritory:
          Result := Format(Phrases.Lookup('FRSTATTER'), [I]);
        stScience:
          Result := Format(Phrases.Lookup('FRSTATTECH'), [I div nAdv]);
        stExplore:
          Result := Format(Phrases.Lookup('FRSTATEXP'),
            [I * 100 div (G.lx * G.ly)]);
      end;
    end
  end;

var
  p1, Treaty: Integer;
  S: string;
  HasContact, ExtinctPart: Boolean;
begin
  GetMem(List, 4 * (MyRO.Turn + 2));

  ReportText.Clear;
  ReportText.Add('');
  if (MyRO.Turn - CurrentReport.TurnOfCivilReport > 1) and
    (1 shl pView and MyRO.Alive <> 0) then
  begin
    S := Format(Phrases.Lookup('FROLDCIVILREP'),
      [TurnToString(CurrentReport.TurnOfCivilReport)]);
    ReportText.Add('C' + S);
    ReportText.Add('');
  end;

  if (1 shl pView and MyRO.Alive <> 0) then
  begin
    ReportText.Add('M' + Format(Phrases.Lookup('FRTREASURY'),
      [CurrentReport.Money]));
    ReportText.Add('P' + StatText(stPop));
    ReportText.Add('T' + StatText(stTerritory));
  end;
  ReportText.Add('S' + StatText(stScience));
  ReportText.Add('E' + StatText(stExplore));
  HasContact := False;
  for p1 := 0 to nPl - 1 do
    if (p1 <> Me) and (CurrentReport.Treaty[p1] > trNoContact) then
      HasContact := True;
  if HasContact then
  begin
    ReportText.Add('');
    ReportText.Add(' ' + Phrases.Lookup('FRRELATIONS'));
    for ExtinctPart := False to True do
      for Treaty := trAlliance downto trNone do
        for p1 := 0 to nPl - 1 do
          if (p1 <> Me) and (CurrentReport.Treaty[p1] = Treaty) and
            ((1 shl p1 and MyRO.Alive = 0) = ExtinctPart) then
          begin
            S := Tribe[p1].TString(Phrases.Lookup('HAVETREATY', Treaty));
            if ExtinctPart then
              S := '(' + S + ')';
            ReportText.Add(char(48 + Treaty) + S);
          end;
  end;
  ReportText.Add('');

  FreeMem(List);
end;

procedure TNatStatDlg.OffscreenPaint;
var
  I, Y: Integer;
  S: string;
  ps: PChar;
  Extinct: Boolean;

begin
  inherited;

  Extinct := 1 shl pView and MyRO.Alive = 0;

  BitBltCanvas(Offscreen.Canvas, 0, 0, ClientWidth, ClientHeight,
    Back.Canvas, 0, 0);

  Offscreen.Canvas.Font.Assign(UniFont[ftCaption]);
  RisedTextout(Offscreen.Canvas,
    40 { (ClientWidth-BiColorTextWidth(offscreen.canvas,caption)) div 2 } ,
    7, Caption);

  Offscreen.Canvas.Font.Assign(UniFont[ftNormal]);

  with Offscreen do
  begin
    // show leader picture
    Tribe[pView].InitAge(GetAge(pView));
    if Assigned(Tribe[pView].faceHGr) then
    begin
      Dump(Offscreen, Tribe[pView].faceHGr, 18, yIcon - 4, 64, 48,
        1 + Tribe[pView].facepix mod 10 * 65,
        1 + Tribe[pView].facepix div 10 * 49);
      frame(Offscreen.Canvas, 18 - 1, yIcon - 4 - 1, 18 + 64, yIcon - 4 + 48,
        $000000, $000000);
    end;

    if (pView = Me) or not Extinct then
      LoweredTextOut(Canvas, -1, MainTexture, xAttrib, yAttrib,
        Phrases.Lookup('GOVERNMENT', CurrentReport.Government) +
        Phrases.Lookup('FRAND'));
    if pView = Me then
    begin
      LoweredTextOut(Canvas, -1, MainTexture, xAttrib, yAttrib + 19,
        Phrases.Lookup('CREDIBILITY', RoughCredibility(MyRO.Credibility)));
      LoweredTextOut(Canvas, -1, MainTexture, xAttrib, yAttrib + 38,
        Format(Phrases.Lookup('FRCREDIBILITY'), [MyRO.Credibility]));
    end
    else
    begin
      if Extinct then
      begin
        LoweredTextOut(Canvas, -1, MainTexture, xAttrib, yAttrib + 9,
          Phrases.Lookup('FREXTINCT'));
        LoweredTextOut(Canvas, -1, MainTexture, xAttrib, yAttrib + 28,
          TurnToString(CurrentReport.TurnOfCivilReport));
      end
      else
      begin
        LoweredTextOut(Canvas, -1, MainTexture, xAttrib, yAttrib + 19,
          Phrases.Lookup('CREDIBILITY',
          RoughCredibility(CurrentReport.Credibility)));
        LoweredTextOut(Canvas, -1, MainTexture, xAttrib, yAttrib + 38,
          Format(Phrases.Lookup('FRCREDIBILITY'), [CurrentReport.Credibility]));
      end;

      if MyRO.Treaty[pView] = trNoContact then
      begin
        S := Phrases.Lookup('FRNOCONTACT');
        LoweredTextOut(Canvas, -1, MainTexture,
          (ClientWidth - BiColorTextWidth(Canvas, S)) div 2, yRelation + 9, S);
      end
      else if ShowContact then
      begin
        LoweredTextOut(Canvas, -1, MainTexture, xRelation, yRelation,
          Phrases.Lookup('FRTREATY'));
        LoweredTextOut(Canvas, -1, MainTexture, ClientWidth div 2, yRelation,
          Phrases.Lookup('TREATY', MyRO.Treaty[pView]));
        if CurrentReport.TurnOfContact < 0 then
          LoweredTextOut(Canvas, -1, MainTexture, ClientWidth div 2,
            yRelation + 19, Phrases.Lookup('FRNOVISIT'))
        else
        begin
          LoweredTextOut(Canvas, -1, MainTexture, xRelation, yRelation + 19,
            Phrases.Lookup('FRLASTCONTACT'));
          if CurrentReport.TurnOfContact >= 0 then
            LoweredTextOut(Canvas, -1, MainTexture, ClientWidth div 2,
              yRelation + 19, TurnToString(CurrentReport.TurnOfContact));
        end;
      end;

      if Extinct then
        FrameImage(Canvas, BigImp, xIcon, yIcon, xSizeBig, ySizeBig, 0, 200)
        { else if CurrentReport.Government=gAnarchy then
          FrameImage(Canvas,BigImp,xIcon,yIcon,xSizeBig,ySizeBig,112,400,
          ContactEnabled and (MyRO.Happened and phGameEnd=0) and (ClientMode<scContact))
          else
          FrameImage(Canvas,BigImp,xIcon,yIcon,xSizeBig,ySizeBig,
          56*(CurrentReport.Government-1),40,
          ContactEnabled and (MyRO.Happened and phGameEnd=0) and (ClientMode<scContact)) };
    end;

    if CurrentReport.TurnOfCivilReport >= 0 then
    begin // print state report
      FillSeamless(Canvas, xReport, yReport, wReport, hReport, 0, 0, Paper);
      with Canvas do
      begin
        Brush.Color := MainTexture.ColorBevelShade;
        FillRect(Rect(xReport + wReport, yReport + PaperShade,
          xReport + wReport + PaperShade, yReport + hReport + PaperShade));
        FillRect(Rect(xReport + PaperShade, yReport + hReport,
          xReport + wReport + PaperShade, yReport + hReport + PaperShade));
        Brush.Style := bsClear;
      end;

      Y := 0;
      for I := 0 to ReportText.Count - 1 do
      begin
        if (I >= LinesDown) and (I < LinesDown + ReportLines) then
        begin
          S := ReportText[I];
          if S <> '' then
          begin
            // LineType:=s[1];
            Delete(S, 1, 1);
            BiColorTextOut(Canvas, Colors.Canvas.Pixels[clkMisc, cliPaperText],
              $7F007F, xReport + 8, yReport + LineSpacing * Y, S);
          end;
          Inc(Y);
        end;
      end;
    end
    else
    begin
      S := Phrases.Lookup('FRNOCIVILREP');
      RisedTextout(Canvas, (ClientWidth - BiColorTextWidth(Canvas, S)) div 2,
        yReport + hReport div 2 - 10, S);
    end;

    if soTellAI in OptionChecked then begin
      Server(sGetAIInfo, Me, pView, ps);
      LoweredTextOut(Canvas, -1, MainTexture, 42, 445, ps);
    end else
      LoweredTextOut(Canvas, -2, MainTexture, 42, 445,
        Phrases2.Lookup('MENU_TELLAI'));
  end;
  ContactBtn.SetBack(Offscreen.Canvas, ContactBtn.Left, ContactBtn.Top);

  MarkUsedOffscreen(ClientWidth, ClientHeight);
end;

procedure TNatStatDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TNatStatDlg.DialogBtnClick(Sender: TObject);
var
  ContactResult: Integer;
begin
  ContactResult := MainScreen.DipCall(scContact + pView shl 4);
  if ContactResult < rExecuted then
  begin
    if ContactResult = eColdWar then
      SoundMessage(Phrases.Lookup('FRCOLDWAR'), 'MSG_DEFAULT')
    else if MyRO.Government = gAnarchy then
      SoundMessage(Tribe[Me].TPhrase('FRMYANARCHY'), 'MSG_DEFAULT')
    else if ContactResult = eAnarchy then
      if MyRO.Treaty[pView] >= trPeace then
      begin
        if MainScreen.ContactRefused(pView, 'FRANARCHY') then
          SmartUpdateContent;
      end
      else
        SoundMessage(Tribe[pView].TPhrase('FRANARCHY'), 'MSG_DEFAULT');
  end
  else
    Close;
end;

procedure TNatStatDlg.ToggleBtnClick(Sender: TObject);
var
  p1, StartCount: Integer;
  M: TMenuItem;
  ExtinctPart: Boolean;
begin
  EmptyMenu(Popup.Items);

  // own nation
  if G.Difficulty[Me] <> 0 then
  begin
    M := TMenuItem.Create(Popup);
    M.RadioItem := True;
    M.Caption := Tribe[Me].TPhrase('TITLE_NATION');
    M.Tag := Me;
    M.OnClick := PlayerClick;
    if Me = pView then
      M.Checked := True;
    Popup.Items.Add(M);
  end;

  // foreign nations
  for ExtinctPart := False to True do
  begin
    StartCount := Popup.Items.Count;
    for p1 := 0 to nPl - 1 do
      if ExtinctPart and (G.Difficulty[p1] > 0) and
        (1 shl p1 and MyRO.Alive = 0) or not ExtinctPart and
        (1 shl p1 and MyRO.Alive <> 0) and (MyRO.Treaty[p1] >= trNone) then
      begin
        M := TMenuItem.Create(Popup);
        M.RadioItem := True;
        M.Caption := Tribe[p1].TPhrase('TITLE_NATION');
        if ExtinctPart then
          M.Caption := '(' + M.Caption + ')';
        M.Tag := p1;
        M.OnClick := PlayerClick;
        if p1 = pView then
          M.Checked := True;
        Popup.Items.Add(M);
      end;
    if (StartCount > 0) and (Popup.Items.Count > StartCount) then
    begin // seperator
      M := TMenuItem.Create(Popup);
      M.Caption := '-';
      Popup.Items.Insert(StartCount, M);
    end;
  end;

  Popup.Popup(Left + ToggleBtn.Left, Top + ToggleBtn.Top + ToggleBtn.Height);
end;

procedure TNatStatDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
begin
  if Key = VK_F9 then // my key
  begin // toggle nation
    I := 0;
    repeat
      pView := (pView + 1) mod nPl;
      Inc(I);
    until (I >= nPl) or (1 shl pView and MyRO.Alive <> 0) and
      (MyRO.Treaty[pView] >= trNone);
    if I >= nPl then
      pView := Me;
    Tag := pView;
    PlayerClick(self); // no, this is not nice
  end
  else
    inherited;
end;

procedure TNatStatDlg.EcoChange;
begin
  if Visible and (pView = Me) then
  begin
    SelfReport.Government := MyRO.Government;
    SelfReport.Money := MyRO.Money;
    SmartUpdateContent;
  end;
end;

procedure TNatStatDlg.ScrollUpBtnClick(Sender: TObject);
begin
  if LinesDown > 0 then
  begin
    Dec(LinesDown);
    SmartUpdateContent;
  end;
end;

procedure TNatStatDlg.ScrollDownBtnClick(Sender: TObject);
begin
  if LinesDown + ReportLines < ReportText.Count then
  begin
    Inc(LinesDown);
    SmartUpdateContent;
  end;
end;

procedure TNatStatDlg.TellAIBtnClick(Sender: TObject);
begin
  if soTellAI in OptionChecked then OptionChecked := OptionChecked - [soTellAI]
    else OptionChecked := OptionChecked + [soTellAI];
  if soTellAI in OptionChecked then
    TellAIBtn.ButtonIndex := 3
  else
    TellAIBtn.ButtonIndex := 2;
  SmartUpdateContent;
end;

end.
