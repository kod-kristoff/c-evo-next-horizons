{$INCLUDE Switches.inc}
unit Direct;

interface

uses
  Messg,

  LCLIntf, LCLType, {$IFDEF Linux}LMessages, {$ENDIF}Messages, SysUtils, Classes,
  Graphics, Controls, Forms, DrawDlg, GameServer;

const
  WM_GO = WM_USER;
  WM_CHANGECLIENT = WM_USER + 1; // hand over control to other client
  WM_NEXTPLAYER = WM_USER + 2; // active player's turn ended, next player
  WM_AIEXCEPTION = WM_USER + 3;

type
  TDirectDlg = class(TDrawDlg)
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    procedure DlgNotify(ID: TNotify; Index: Integer = 0);
  private
    Info: string;
    State: Integer;
    Gone: Boolean;
    Quick: Boolean;
    procedure SetInfo(x: string);
    procedure SetState(x: integer);
    procedure OnGo(var Msg: TMessage); message WM_GO;
    procedure OnChangeClient(var Msg: TMessage); message WM_CHANGECLIENT;
    procedure OnNextPlayer(var Msg: TMessage); message WM_NEXTPLAYER;
    procedure OnAIException(var Msg: TMessage); message WM_AIEXCEPTION;
  end;

var
  DirectDlg: TDirectDlg;

implementation

uses
  ScreenTools, Protocol, Start, LocalPlayer, NoTerm, Back, Global;

{$R *.lfm}

procedure Notify(ID: TNotify; Index: Integer = 0);
begin
  DirectDlg.DlgNotify(ID, Index);
end;

procedure TDirectDlg.DlgNotify(ID: TNotify; Index: Integer = 0);
var
//  hMem: Cardinal;
//  p: pointer;
  s: string;
begin
  case ID of
    ntInitLocalHuman: begin
      MainTexture.Age := -1;
      State := -1;
      Info := Phrases.Lookup('BUSY_MODLH');
      Show;
      {$IFDEF LINUX}
      Application.ProcessMessages;
      {$ENDIF}
      Invalidate;
      Update;
    end;
    ntInitModule:
      if visible then
      begin
        s := Format(Phrases.Lookup('BUSY_MOD'), [Brains[Index].Name]);
        while BiColorTextWidth(Canvas, s) + 64 > ClientWidth do
          Delete(s, Length(s), 1);
        SetInfo(s);
      end;
    ntCreateWorld:
      if visible then
        SetInfo(Phrases.Lookup('BUSY_START'));
    ntInitPlayers:
      if visible then
        SetInfo(Phrases.Lookup('BUSY_INIT'));
    ntDeactivationMissing:
      SimpleMessage(Format(Phrases.Lookup('MISSDEACT'), [Index]));
    ntSetAIName:
      LocalPlayer.SetAIName(Index, NotifyMessage);
    ntException:
      PostMessage(Handle, WM_AIEXCEPTION, Index, 0);
    ntLoadBegin: begin
      Info := Phrases.Lookup('BUSY_LOAD');
      SetState(0);
    end;
    ntLoadState: SetState(Index);
    ntDLLError:
      SimpleMessage(Format(Phrases.Lookup('DLLERROR'), [Brains[Index].FileName]));
    ntAIError:
      SimpleMessage(Format(Phrases.Lookup('AIERROR'), [NotifyMessage]));
    ntClientError:
      SimpleMessage(Format(Phrases.Lookup('CLIENTERROR'),
        [Brains[Index].FileName]));
    ntEndInfo: begin
      Hide;
      Background.Update;
    end;
    ntLoadError: begin
(* TODO        if OpenClipboard(Handle) then
        begin // copy file path to clipboard
          NotifyMessage := NotifyMessage + #0;
          hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,
            Length(NotifyMessage));
          p := GlobalLock(hMem);
          if p <> nil then
            move(NotifyMessage[1], p^, Length(NotifyMessage));
          GlobalUnlock(hMem);
          SetClipboardData(CF_TEXT, hMem);
          CloseClipboard;
        end;
        with MessgDlg do
        begin
          MessgText := Phrases.Lookup('LOADERROR');
          Kind := mkYesNo;
          ShowModal;
          if ModalResult = mrOK then
            OpenURL(CevoContactBug);
        end
    *)
      end;
    ntStartDone:
      if not Quick then begin
        StartDlg.Hide;
        Background.Update;
      end;
    ntStartGo, ntStartGoRefresh, ntStartGoRefreshMaps:
      if Quick then Close
      else begin
        if ID = ntStartGoRefresh then
          StartDlg.UpdateFormerGames
        else if ID = ntStartGoRefreshMaps then
          StartDlg.UpdateMaps;
        StartDlg.Show;
      end;
    ntChangeClient: PostMessage(Handle, WM_CHANGECLIENT, 0, 0);
    ntNextPlayer: PostMessage(Handle, WM_NEXTPLAYER, 0, 0);
    ntDeinitModule:
      begin
        Info := Format(Phrases2.Lookup('BUSY_DEINIT'),
          [Brains[Index].Name]);
        while BiColorTextWidth(Canvas, Info) + 64 > ClientWidth do
          Delete(Info, Length(Info), 1);
        MainTexture.Age := -1;
        State := -1;
        Show;
        {$IFDEF LINUX}
        Application.ProcessMessages;
        {$ENDIF}
        Invalidate;
        Update;
      end;
    ntBackOn: begin
      Background.Show;
      Background.Update;
      Sleep(50); // prevent flickering
    end;
    ntBackOff: Background.Close;
  end;
end;

procedure TDirectDlg.FormCreate(Sender: TObject);
begin
  Gone := False;
  State := -1;
  Info := '';
  GameServer.Init(Notify);
  BrainNoTerm.Client := NoTerm.Client;
  BrainNoTerm.Name := Phrases.Lookup('AIT');
  BrainSuperVirtual.Client := nil;
  BrainSuperVirtual.Name := Phrases.Lookup('SUPER');
  BrainTerm.Client := LocalPlayer.Client;
  BrainTerm.Name := Phrases.Lookup('HUMAN');
  BrainRandom.Name := Phrases.Lookup('RANDOMAI');
  Canvas.Font.Assign(UniFont[ftNormal]);
  Canvas.Brush.Style := bsClear;
end;

procedure TDirectDlg.FormShow(Sender: TObject);
begin
  if not Gone then
  begin
    PostMessage(Handle, WM_GO, 0, 0);
    Gone := true;
  end;
end;

procedure TDirectDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GameServer.Done;
end;

procedure TDirectDlg.OnGo(var Msg: TMessage);
var
  i: integer;
  s: string;
  FileName: string;
begin
  Hide;
  if Brains.Count = 3 then
  begin
    Application.MessageBox(PChar(Phrases.Lookup('NOAI')), 'C-evo', 0);
    Close;
    Exit;
  end;
  Quick := false;
  if ParamCount > 0 then
  begin
    s := ParamStr(1);
    if (s[1] = '-') {$IFDEF WINDOWS}or (s[1] = '/'){$ENDIF} then
    begin // special mode
      Delete(s, 1, 1);
      for i := 1 to Length(s) do
        if s[i] in ['a' .. 'z'] then
          dec(s[i], 32);
      if s = 'MAN' then
      begin
        Quick := true;
        DirectHelp(cHelpOnly);
        Close;
      end;
    end
    else if (FileExists(ParamStr(1))) then begin
      FileName := ParamStr(1);
      if ExtractFileExt(FileName) = CevoExt then begin
        Quick := True;
        if not LoadGame(ExtractFilePath(ParamStr(1)), ExtractFileName(ParamStr(1)
        ), -1, false) then begin
          SimpleMessage(Phrases.Lookup('LOADERR'));
          Close;
        end;
      end else
      if ExtractFileExt(FileName) = CevoMapExt then begin
        Quick := True;
        EditMap(FileName, lxmax, lymax, 30);
      end else begin
        SimpleMessage(Phrases.Lookup('LOADERR'));
        Close;
      end;
    end;
  end;
  if not Quick then begin
    Background.Show;
    StartDlg.Show;
  end;
end;

procedure TDirectDlg.OnChangeClient(var Msg: TMessage);
begin
  ChangeClient;
end;

procedure TDirectDlg.OnNextPlayer(var Msg: TMessage);
begin
  NextPlayer;
end;

procedure TDirectDlg.OnAIException(var Msg: TMessage);
begin
  Application.MessageBox(PChar(Format(Phrases.Lookup('AIEXCEPTION'),
    [Brains[Msg.WParam].Name])), 'C-evo', 0);
end;

procedure TDirectDlg.FormPaint(Sender: TObject);
begin
  PaintBackground(self, 3, 3, ClientWidth - 6, ClientHeight - 6);
  Frame(Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);
  Frame(Canvas, 1, 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Canvas, 2, 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  if State >= 0 then
    RisedTextOut(Canvas, (ClientWidth - BiColorTextWidth(Canvas, Info))
      div 2, 16, Info)
  else
    RisedTextOut(Canvas, (ClientWidth - BiColorTextWidth(Canvas, Info)) div 2,
      (ClientHeight - Canvas.TextHeight(Info)) div 2, Info);
  if State >= 0 then
    PaintProgressBar(Canvas, 3, ClientWidth div 2 - 64, 40, State, 0, 128,
      MainTexture);
end;

procedure TDirectDlg.SetInfo(x: string);
begin
  Info := x;
  Invalidate;
  Update;
  {$IFDEF LINUX}
  Application.ProcessMessages;
  {$ENDIF}
end;

procedure TDirectDlg.SetState(x: integer);
begin
  if (x < 0) <> (State < 0) then begin
    State := x;
    Invalidate;
    Update;
  end
  else if x <> State then begin
    State := x;
    PaintProgressBar(Canvas, 6, ClientWidth div 2 - 64, 40, State, 128 - State,
      128, MainTexture);
  end;
end;

end.
