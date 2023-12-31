unit DrawDlg;

interface

uses
  Classes, SysUtils, Forms, LCLIntf, LCLType, {$IFDEF UNIX}LMessages,{$ENDIF}
  Messages, Graphics, Controls, ButtonBase, ButtonA, ButtonB, Area, ScreenTools
  {$IFDEF LCLGTK2}, Gtk2Globals{$ENDIF};

type
  { TDrawDlg }

  TDrawDlg = class(TForm)
  private
    MoveFormPos: TPoint;
    MoveMousePos: TPoint;
    MoveActive: Boolean;
    procedure VisibleChangedHandler(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
  protected
    TitleHeight: Integer;
    // defines area to grip the window for moving (from top)
    procedure InitButtons;
    procedure OnEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure OnHitTest(var Msg: TMessage); message WM_NCHITTEST;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SmartInvalidate; virtual;
  end;

  { TBaseMessgDlg }

  TBaseMessgDlg = class(TDrawDlg)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  public
    MessgText: string;
  protected
    Lines: Integer;
    TopSpace: Integer;
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
  Color := clBlack;
  TitleHeight := 0;
  MoveActive := False;
  AddHandlerOnVisibleChanged(VisibleChangedHandler);
  {$IFDEF UNIX}
  OnDeactivate := DoDeactivate;
  {$ENDIF}
end;

destructor TDrawDlg.Destroy;
begin
  RemoveHandlerOnVisibleChanged(VisibleChangedHandler);
  inherited;
end;

procedure TDrawDlg.OnEraseBkgnd(var Msg: TMessage);
begin
  // Full area should be covered by Paint method
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
    Pos := Point(ScaleFromNative(Integer(Msg.LParam and $ffff)),
      ScaleFromNative(Integer((Msg.LParam shr 16) and $ffff)));
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
      Msg.Result := HTCAPTION;
    end;
  end;
end;

procedure TDrawDlg.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  MousePos1: TPoint;
  MousePos2: TPoint;
{$IFDEF UNIX}
  MousePosNew: TPoint;
  NewFormPos: TPoint;
{$ENDIF}
begin
  MousePos1 := Mouse.CursorPos;
  inherited;
  MousePos2 := Mouse.CursorPos;
  {$IFDEF UNIX}
  // Only if client is not doing own mouse move handling
  if not Assigned(OnMouseDown) or not Assigned(OnMouseMove) or not Assigned(OnMouseUp) then begin
    // HitTest is not supported under Linux GTK2 so use form inside move mechanizm
    NewFormPos := ScreenToClient(Mouse.CursorPos);
    if (NewFormPos.X >= 0) and (NewFormPos.X < Width) and
      (NewFormPos.Y >= 0) and (NewFormPos.Y < Height) and
      (NewFormPos.Y < TitleHeight) then begin
      MoveMousePos := ClientToScreen(Point(X, Y));
      MoveFormPos := Point(Left, Top);
      MousePosNew := Mouse.CursorPos;
      // Activate move only if mouse position was not changed during inherited call
      if (MousePos1.X = MousePos2.X) and (MousePos1.Y = MousePos2.Y) then begin
        MoveActive := True;
      end;
    end else MoveActive := False;
  end;
  {$ENDIF}
end;

procedure TDrawDlg.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
begin
  inherited;
  if MoveActive then begin
    MousePos := ClientToScreen(Point(X, Y));
    SetBounds(MoveFormPos.X + MousePos.X - MoveMousePos.X,
      MoveFormPos.Y + MousePos.Y - MoveMousePos.Y,
      Width, Height);
  end;
end;

procedure TDrawDlg.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  MoveActive := False;
  inherited;
end;

procedure TDrawDlg.MouseLeave;
begin
  MoveActive := False;
  inherited;
end;

procedure TDrawDlg.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
  inherited;
end;

procedure TDrawDlg.VisibleChangedHandler(Sender: TObject);
begin
  MoveActive := False;

  {$IFDEF LCLGTK2}
  // GTK2 bug workaround https://bugs.freepascal.org/view.php?id=35720
  if Visible then LastMouse.WinControl := Self;
  {$ENDIF}
end;

procedure TDrawDlg.DoDeactivate(Sender: TObject);
begin
  MoveActive := False;
end;

procedure TDrawDlg.InitButtons;
var
  cix: integer;
  // ButtonDownSound, ButtonUpSound: string;
begin
  // ButtonDownSound := Sounds.Lookup('BUTTON_DOWN');
  // ButtonUpSound := Sounds.Lookup('BUTTON_UP');
  for cix := 0 to ComponentCount - 1 do
    if Components[cix] is TButtonBase then
    begin
      TButtonBase(Components[cix]).Graphic := HGrSystem.Data;
      // if ButtonDownSound <> '*' then
      // DownSound := GetSoundsDir + DirectorySeparator + ButtonDownSound + '.wav';
      // if ButtonUpSound <> '*' then
      // UpSound := GetSoundsDir + DirectorySeparator + ButtonUpSound + '.wav';
      if Components[cix] is TButtonA then
        TButtonA(Components[cix]).Font := UniFont[ftButton];
      if Components[cix] is TButtonB then
        TButtonB(Components[cix]).Mask := HGrSystem.Mask;
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
    ClientHeight - (2 + Border), MainTexture.ColorBevelLight,
    MainTexture.ColorBevelShade);
  Frame(Canvas, 2 + Border, 2 + Border, ClientWidth - (3 + Border),
    ClientHeight - (3 + Border), MainTexture.ColorBevelLight,
    MainTexture.ColorBevelShade);
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


