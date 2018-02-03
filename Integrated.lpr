{$INCLUDE Switches.inc}
program Integrated;

uses
  Forms, Interfaces, SysUtils,
  Protocol in 'Protocol.pas',
  CmdList in 'CmdList.pas',
  Database in 'Database.pas',
  GameServer in 'GameServer.pas',
  CityProcessing in 'CityProcessing.pas',
  UnitProcessing in 'UnitProcessing.pas',
  Direct in 'Direct.pas' {DirectDlg} ,
  Start in 'Start.pas' {StartDlg} ,
  Messg in 'Messg.pas' {MessgDlg} ,
  Inp in 'Inp.pas' {InputDlg} ,
  Back in 'Back.pas' {Background} ,
  Log in 'Log.pas' {LogDlg} ,
  PVSB in 'LocalPlayer\PVSB.pas',
  LocalPlayer in 'LocalPlayer\LocalPlayer.pas',
  ClientTools in 'LocalPlayer\ClientTools.pas',
  Diplomacy in 'LocalPlayer\Diplomacy.pas',
  Tribes in 'LocalPlayer\Tribes.pas',
  IsoEngine in 'LocalPlayer\IsoEngine.pas',
  Term in 'LocalPlayer\Term.pas' {MainScreen} ,
  MessgEx in 'LocalPlayer\MessgEx.pas' {MessgExDlg} ,
  BaseWin in 'LocalPlayer\BaseWin.pas',
  Help in 'LocalPlayer\Help.pas' {HelpDlg} ,
  Select in 'LocalPlayer\Select.pas' {ListDlg} ,
  CityScreen in 'LocalPlayer\CityScreen.pas' {CityDlg} ,
  UnitStat in 'LocalPlayer\UnitStat.pas' {UnitStatDlg} ,
  Draft in 'LocalPlayer\Draft.pas' {DraftDlg} ,
  NatStat in 'LocalPlayer\NatStat.pas' {NatStatDlg} ,
  Diagram in 'LocalPlayer\Diagram.pas' {DiaDlg} ,
  Wonders in 'LocalPlayer\Wonders.pas' {WonderDlg} ,
  Nego in 'LocalPlayer\Nego.pas' {NegoDlg} ,
  CityType in 'LocalPlayer\CityType.pas' {CityTypeDlg} ,
  Enhance in 'LocalPlayer\Enhance.pas' {EnhanceDlg} ,
  NoTerm in 'NoTerm.pas' {NoTermDlg} ,
  Battle in 'LocalPlayer\Battle.pas' {BattleDlg} ,
  Rates in 'LocalPlayer\Rates.pas' {RatesDlg} ,
  TechTree in 'LocalPlayer\TechTree.pas' {TechTreeDlg},
  ScreenTools, Directories;

{$R cevo.res}

{$if declared(UseHeapTrace)}
const
  HeapTraceLog = 'heaptrclog.trc';
{$ENDIF}

{$R *.res}

begin
  {$if declared(UseHeapTrace)}
  // Heap trace
  DeleteFile(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  {$ENDIF}

  DotNetClient := nil;
  Application.Initialize;
  Application.Title := 'c-evo';
  Directories.InitUnit;
  ScreenTools.UnitInit;
  Application.CreateForm(TDirectDlg, DirectDlg);
  Application.CreateForm(TStartDlg, StartDlg);
  Application.CreateForm(TMessgDlg, MessgDlg);
  Application.CreateForm(TInputDlg, InputDlg);
  Application.CreateForm(TBackground, Background);
  Application.CreateForm(TLogDlg, LogDlg);
  Application.Run;
  ScreenTools.UnitDone;
end.
