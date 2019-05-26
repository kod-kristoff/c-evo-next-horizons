unit DrawDlg;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, LCLIntf, LCLType, LMessages, Messages, Graphics,
  Controls, ButtonBase, ButtonA, ButtonB, Area, ScreenTools;

type
  { TDrawDlg }

  TDrawDlg = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
    procedure SmartInvalidate; virtual;
  protected
    TitleHeight: integer;
    // defines area to grip the window for moving (from top)
    procedure InitButtons;
    procedure OnEraseBkgnd(var m: TMessage); message WM_ERASEBKGND;
    procedure OnHitTest(var Msg: TMessage); message WM_NCHITTEST;
  end;

  { TBaseMessgDlg }

  TBaseMessgDlg = class(TDrawDlg)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  public
    MessgText: string;
  protected
    Lines, TopSpace: integer;
    procedure SplitText(preview: boolean);
    procedure CorrectHeight;
  end;

const
  Border = 3;
  MessageLineSpacing = 20;

procedure Register;


implementation

procedure Register;
begin
  RegisterNoIcon([TDrawDlg]);
  RegisterNoIcon([TBaseMessgDlg]);
end;

{ TDrawDlg }

constructor TDrawDlg.Create(AOwner: TComponent);
begin
  inherited;
  TitleHeight := 0;
end;

procedure TDrawDlg.OnEraseBkgnd(var m: TMessage);
begin
end;

procedure TDrawDlg.OnHitTest(var Msg: TMessage);
var
  I: integer;
  ControlBounds: TRect;
  Pos: TPoint;
begin
  if BorderStyle <> bsNone then
    inherited
  else
  begin
    Pos := Point(Integer(Msg.LParam and $ffff),
      Integer((Msg.LParam shr 16) and $ffff));
    if Pos.Y >= Top + TitleHeight then
      Msg.Result := HTCLIENT
    else
    begin
      for I := 0 to ControlCount - 1 do
        if Controls[I].Visible then
        begin
          ControlBounds := Controls[I].BoundsRect;
          if (Pos.X >= Left + ControlBounds.Left) and
            (Pos.X < Left + ControlBounds.Right) and
            (Pos.Y >= Top + ControlBounds.Top) and
            (Pos.Y < Top + ControlBounds.Bottom) then
          begin
            Msg.result := HTCLIENT;
            Exit;
          end;
        end;
      Msg.Result := HTCAPTION
    end;
  end;
end;

procedure TDrawDlg.InitButtons;
var
  cix: integer;
  // ButtonDownSound, ButtonUpSound: string;
begin
  // ButtonDownSound:=Sounds.Lookup('BUTTON_DOWN');
  // ButtonUpSound:=Sounds.Lookup('BUTTON_UP');
  for cix := 0 to ComponentCount - 1 do
    if Components[cix] is TButtonBase then
    begin
      TButtonBase(Components[cix]).Graphic := GrExt[HGrSystem].Data;
      // if ButtonDownSound<>'*' then
      // DownSound:=HomeDir+'Sounds' + DirectorySeparator + ButtonDownSound + '.wav';
      // if ButtonUpSound<>'*' then
      // UpSound:=HomeDir+'Sounds' + DirectorySeparator + ButtonUpSound + '.wav';
      if Components[cix] is TButtonA then
        TButtonA(Components[cix]).Font := UniFont[ftButton];
      if Components[cix] is TButtonB then
        TButtonB(Components[cix]).Mask := GrExt[HGrSystem].Mask;
    end;
end;

procedure TDrawDlg.SmartInvalidate;
var
  i: integer;
  r0, r1: HRgn;
begin
  r0 := CreateRectRgn(0, 0, ClientWidth, ClientHeight);
  for i := 0 to ControlCount - 1 do
    if not(Controls[i] is TArea) and Controls[i].Visible then
    begin
      with Controls[i].BoundsRect do
        r1 := CreateRectRgn(Left, Top, Right, Bottom);
      CombineRgn(r0, r0, r1, RGN_DIFF);
      DeleteObject(r1);
    end;
  InvalidateRgn(Handle, r0, false);
  DeleteObject(r0);
end;

{ TBaseMessgDlg }

procedure TBaseMessgDlg.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Canvas.Font.Assign(UniFont[ftNormal]);
  Canvas.Brush.Style := bsClear;
  MessgText := '';
  TopSpace := 0;
  TitleHeight := Screen.Height;
  if csDesigning in ComponentState then Exit;
  InitButtons;
end;

procedure TBaseMessgDlg.FormPaint(Sender: TObject);
var
  i, cix: integer;
begin
  if csDesigning in ComponentState then Exit;
  PaintBackground(self, 3 + Border, 3 + Border, ClientWidth - (6 + 2 * Border),
    ClientHeight - (6 + 2 * Border));
  for i := 0 to Border do
    Frame(Canvas, i, i, ClientWidth - 1 - i, ClientHeight - 1 - i,
      $000000, $000000);
  Frame(Canvas, Border + 1, Border + 1, ClientWidth - (2 + Border),
    ClientHeight - (2 + Border), MainTexture.clBevelLight,
    MainTexture.clBevelShade);
  Frame(Canvas, 2 + Border, 2 + Border, ClientWidth - (3 + Border),
    ClientHeight - (3 + Border), MainTexture.clBevelLight,
    MainTexture.clBevelShade);
  SplitText(false);

  for cix := 0 to ControlCount - 1 do
    if (Controls[cix].Visible) and (Controls[cix] is TButtonBase) then
      BtnFrame(Canvas, Controls[cix].BoundsRect, MainTexture);
end;

procedure TBaseMessgDlg.SplitText(preview: boolean);
var
  Start, Stop, OrdinaryStop, LinesCount: integer;
  s: string;
begin
  Start := 1;
  LinesCount := 0;
  while Start < Length(MessgText) do
  begin
    Stop := Start;
    while (Stop < Length(MessgText)) and (MessgText[Stop] <> '\') and
      (BiColorTextWidth(Canvas, Copy(MessgText, Start, Stop - Start + 1)) <
      ClientWidth - 56) do
      inc(Stop);
    if Stop <> Length(MessgText) then
    begin
      OrdinaryStop := Stop;
      repeat
        dec(OrdinaryStop)
      until (MessgText[OrdinaryStop + 1] = ' ') or
        (MessgText[OrdinaryStop + 1] = '\');
      if (OrdinaryStop + 1 - Start) * 2 >= Stop - Start then
        Stop := OrdinaryStop
    end;
    if not preview then
    begin
      s := Copy(MessgText, Start, Stop - Start + 1);
      LoweredTextOut(Canvas, -1, MainTexture,
        (ClientWidth - BiColorTextWidth(Canvas, s)) div 2,
        19 + Border + TopSpace + LinesCount * MessageLineSpacing, s);
    end;
    Start := Stop + 2;
    inc(LinesCount)
  end;
  if preview then
    Lines := LinesCount;
end;

procedure TBaseMessgDlg.CorrectHeight;
var
  i: integer;
begin
  ClientHeight := 72 + Border + TopSpace + Lines * MessageLineSpacing;
  Top := (Screen.Height - ClientHeight) div 2;
  for i := 0 to ControlCount - 1 do
    Controls[i].Top := ClientHeight - (34 + Border);
end;

end.

