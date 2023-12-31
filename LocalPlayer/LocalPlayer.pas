{$INCLUDE Switches.inc}
unit LocalPlayer;

interface

procedure Client(Command, Player: Integer; var Data); stdcall;
procedure SetAIName(P: Integer; Name: string);

implementation

uses
  Term, CityScreen, Draft, MessgEx, Select, CityType, Help, UnitStat, Diagram,
  NatStat, Wonders, Nego, Enhance, BaseWin, Battle, Rates, TechTree, Forms;

var
  FormsCreated: Boolean;

procedure Client(Command, Player: Integer; var Data);
begin
  if not FormsCreated then
  begin
    FormsCreated := True;
    // TODO: Changing application name in runtime will cause change of Linux XML registry file path
//    Application.MainForm := MainScreen;
    Application.CreateForm(TMainScreen, MainScreen);
    Application.CreateForm(TCityDlg, CityDlg);
    Application.CreateForm(TModalSelectDlg, ModalSelectDlg);
    Application.CreateForm(TListDlg, ListDlg);
    Application.CreateForm(TMessgExDlg, MessgExDlg);
    Application.CreateForm(TDraftDlg, DraftDlg);
    Application.CreateForm(TCityTypeDlg, CityTypeDlg);
    Application.CreateForm(THelpDlg, HelpDlg);
    Application.CreateForm(TUnitStatDlg, UnitStatDlg);
    Application.CreateForm(TDiaDlg, DiaDlg);
    Application.CreateForm(TNatStatDlg, NatStatDlg);
    Application.CreateForm(TWondersDlg, WondersDlg);
    Application.CreateForm(TNegoDlg, NegoDlg);
    Application.CreateForm(TEnhanceDlg, EnhanceDlg);
    Application.CreateForm(TBattleDlg, BattleDlg);
    // Application.CreateForm(TAdvisorDlg, AdvisorDlg);
    Application.CreateForm(TRatesDlg, RatesDlg);
    Application.CreateForm(TTechTreeDlg, TechTreeDlg);
  end;
  MainScreen.Client(Command, Player, Data);
end;

procedure SetAIName(P: Integer; Name: string);
begin
  MainScreen.SetAIName(P, Name);
end;

initialization

FormsCreated := False;

end.
