{$INCLUDE Switches.inc}
unit Messg;

interface

uses
  ScreenTools, LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, ButtonA, DrawDlg;

const
  WM_PLAYSOUND = WM_USER;

type
  TMessgDlg = class(TBaseMessgDlg)
    Button1: TButtonA;
    Button2: TButtonA;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  public
    Kind: integer;
    OpenSound: string;
  private
    procedure OnPlaySound(var Msg: TMessage); message WM_PLAYSOUND;
  end;

const
  // message kinds
  mkOK = 1;
  mkOKCancel = 2;
  mkYesNo = 3;

var
  MessgDlg: TMessgDlg;

procedure SimpleMessage(SimpleText: string);
procedure SoundMessage(SimpleText, SoundItem: string);


implementation

{$R *.lfm}

procedure TMessgDlg.FormCreate(Sender: TObject);
begin
  inherited;
  OpenSound := '';
end;

procedure TMessgDlg.FormShow(Sender: TObject);
begin
  Button1.Visible := true;
  Button2.Visible := not(Kind in [mkOK]);
  if Button2.Visible then
    Button1.Left := 101
  else
    Button1.Left := 159;
  if Kind = mkYesNo then
  begin
    Button1.Caption := Phrases.Lookup('BTN_YES');
    Button2.Caption := Phrases.Lookup('BTN_NO')
  end
  else
  begin
    Button1.Caption := Phrases.Lookup('BTN_OK');
    Button2.Caption := Phrases.Lookup('BTN_CANCEL');
  end;

  SplitText(true);
  CorrectHeight;
end;

procedure TMessgDlg.FormPaint(Sender: TObject);
begin
  inherited;
  if OpenSound <> '' then
    PostMessage(Handle, WM_PLAYSOUND, 0, 0);
end; { FormPaint }

procedure TMessgDlg.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TMessgDlg.Button2Click(Sender: TObject);
begin
  ModalResult := mrIgnore;
end;

procedure TMessgDlg.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ModalResult := mrOK
    // else if (Key=#27) and (Button2.Visible) then ModalResult:=mrCancel
end;

procedure SimpleMessage(SimpleText: string);
begin
  with MessgDlg do
  begin
    MessgText := SimpleText;
    Kind := mkOK;
    ShowModal;
  end
end;

procedure SoundMessage(SimpleText, SoundItem: string);
begin
  with MessgDlg do
  begin
    MessgText := SimpleText;
    OpenSound := SoundItem;
    Kind := mkOK;
    ShowModal;
  end
end;

procedure TMessgDlg.OnPlaySound(var Msg: TMessage);
begin
  Play(OpenSound);
  OpenSound := '';
end;

end.
