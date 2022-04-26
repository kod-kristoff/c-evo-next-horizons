{$INCLUDE Switches.inc}
program Integrated;

uses
  {$IFDEF UNIX}
  cthreads, clocale,
  {$ENDIF}
  Forms, Interfaces, SysUtils, Protocol, GameServer, Direct, Start, Messg, Inp,
  Back, Log, LocalPlayer, ClientTools, Tribes, IsoEngine, Term, CityScreen, Nego,
  NoTerm, ScreenTools, Directories;

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
  Application.Title := 'C-evo';
  Application.TaskBarBehavior := tbMultiButton;
  Directories.UnitInit;
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
