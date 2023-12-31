{$INCLUDE Switches.inc}
unit Inp;

interface

uses
  ScreenTools, LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  DrawDlg, ButtonA, StdCtrls;

type
  TInputDlg = class(TDrawDlg)
    OKBtn: TButtonA;
    EInput: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EInputKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    procedure CenterToRect(Rect: TRect);
  private
    Center: Boolean;
  end;

var
  InputDlg: TInputDlg;


implementation

{$R *.lfm}

procedure TInputDlg.FormCreate(Sender: TObject);
begin
  Canvas.Font.Assign(UniFont[ftNormal]);
  Canvas.Brush.Style := bsClear;
  TitleHeight := Height;
  InitButtons;
  Center := True;
end;

procedure TInputDlg.FormPaint(Sender: TObject);
begin
  PaintBackground(self, 3, 3, ClientWidth - 6, ClientHeight - 6);
  Frame(Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);
  Frame(Canvas, 1, 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Canvas, 2, 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  EditFrame(Canvas, EInput.BoundsRect, MainTexture);
  BtnFrame(Canvas, OKBtn.BoundsRect, MainTexture);
  RisedTextOut(Canvas, (ClientWidth - BiColorTextWidth(Canvas, Caption)) div 2,
    9, Caption);
  { Corner(canvas,1,1,0,MainTexture);
    Corner(Canvas,ClientWidth-9,1,1,MainTexture);
    Corner(Canvas,1,ClientHeight-9,2,MainTexture);
    Corner(Canvas,ClientWidth-9,ClientHeight-9,3,MainTexture); }
end;

procedure TInputDlg.OKBtnClick(Sender: TObject);
begin
  if EInput.Text = '' then
    ModalResult := mrCancel
  else
    ModalResult := mrOK;
end;

procedure TInputDlg.EInputKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and (EInput.Text <> '') then
  begin
    Key := #0;
    ModalResult := mrOK;
  end
  else if Key = #27 then
  begin
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

procedure TInputDlg.FormShow(Sender: TObject);
begin
  OKBtn.Caption := Phrases.Lookup('BTN_OK');
  EInput.Font.Color := MainTexture.ColorMark;
  EInput.SelStart := 0;
  EInput.SelLength := Length(EInput.Text);
  if Center then
    CenterToRect(Rect(0, 0, Screen.Width, Screen.Height));
end;

procedure TInputDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Center := True;
end;

procedure TInputDlg.CenterToRect(Rect: TRect);
begin
  Center := False;
  Left := Rect.Left + (Rect.Right - Rect.Left - Width) div 2;
  Top := Rect.Top + (Rect.Bottom - Rect.Top - Height) div 2;
end;

end.
