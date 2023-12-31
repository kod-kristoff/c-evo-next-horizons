unit BaseWin;

interface

uses
  ScreenTools, LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  DrawDlg;

type
  TWindowMode = (wmNone, wmModal, wmPersistent, wmSubmodal);
  TShowNewContent = procedure (NewMode: TWindowMode; HelpContext: string) of object;

  { TBufferedDrawDlg }

  TBufferedDrawDlg = class(TDrawDlg)
  public
    UserLeft: Integer;
    UserTop: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure SmartUpdateContent(ImmUpdate: Boolean = False);
    procedure StayOnTop_Workaround;
  protected
    FWindowMode: TWindowMode;
    ModalFrameIndent: Integer;
    HelpContext: string;
    procedure ShowNewContent(NewMode: TWindowMode; ForceClose: Boolean = False);
    procedure MarkUsedOffscreen(xMax, yMax: Integer);
    procedure OffscreenPaint; virtual;
    procedure VPaint; virtual;
  public
    UsedOffscreenWidth: Integer;
    UsedOffscreenHeight: Integer;
    Offscreen: TBitmap;
    OffscreenUser: TForm;
    property WindowMode: TWindowMode read FWindowMode;
  end;

  TFramedDlg = class(TBufferedDrawDlg)
  public
    constructor Create(AOwner: TComponent); override;
    procedure FormCreate(Sender: TObject);
    procedure SmartInvalidate; override;
  protected
    CaptionLeft: Integer;
    CaptionRight: Integer;
    InnerWidth: Integer;
    InnerHeight: Integer;
    WideBottom: Boolean;
    FullCaption: Boolean;
    TexOverride: Boolean;
    ModalIndication: Boolean;
    procedure InitWindowRegion;
    procedure VPaint; override;
    procedure FillOffscreen(Left, Top, Width, Height: Integer);
  end;

var
  ShowNewContentProc: TShowNewContent;
  MainFormKeyDown: TKeyEvent;

const
  yUnused = 161;
  NarrowFrame = 11;
  WideFrame = 36;
  SideFrame = 9;

procedure CreateOffscreen(var Offscreen: TBitmap);
function WindowModeMakePersistent(Mode: TWindowMode): TWindowMode;
procedure Register;


implementation

uses
  ButtonBase, Area;

function WindowModeMakePersistent(Mode: TWindowMode): TWindowMode;
begin
  if Mode = wmModal then Result := wmSubmodal
    else Result := wmPersistent;
end;

procedure Register;
begin
  RegisterNoIcon([TBufferedDrawDlg]);
  RegisterNoIcon([TFramedDlg]);
end;

constructor TBufferedDrawDlg.Create(AOwner: TComponent);
begin
  BaseWin.CreateOffscreen(Offscreen);
  OnClose := FormClose;
  OnPaint := FormPaint;
  OnKeyDown := FormKeyDown;
  OnDeactivate := FormDeactivate;
  inherited;
  FWindowMode := wmNone;
  HelpContext := 'CONCEPTS';
  TitleHeight := WideFrame;
  ModalFrameIndent := 45;
  UserLeft := (Screen.Width - Width) div 2;
  UserTop := (Screen.Height - Height) div 2;
end;

destructor TBufferedDrawDlg.Destroy;
begin
  FreeAndNil(Offscreen);
  inherited;
end;

procedure TBufferedDrawDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FWindowMode = wmPersistent then begin
    UserLeft := Left;
    UserTop := Top;
  end;
  if OffscreenUser = self then
    OffscreenUser := nil;
end;

procedure TBufferedDrawDlg.FormPaint(Sender: TObject);
begin
  if OffscreenUser <> self then
    OffscreenPaint;
  VPaint;
end;

procedure TBufferedDrawDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    if fsModal in FormState then
      ModalResult := mrCancel;
  end else
  if Key = VK_RETURN then begin
    if fsModal in FormState then
      ModalResult := mrOK;
  end else
  if Key = VK_F1 then begin
    if Assigned(ShowNewContentProc) then
      ShowNewContentProc(WindowModeMakePersistent(FWindowMode), HelpContext);
  end else
  if FWindowMode = wmPersistent then begin
    if Assigned(MainFormKeyDown) then
      MainFormKeyDown(Sender, Key, Shift);
  end;
end;

procedure TBufferedDrawDlg.FormDeactivate(Sender: TObject);
begin
  if FWindowMode = wmSubmodal then
    Close;
end;

procedure TBufferedDrawDlg.OffscreenPaint;
begin
  if (OffscreenUser <> nil) and (OffscreenUser <> Self) then
    OffscreenUser.Update; // complete working with old owner to prevent rebound
  OffscreenUser := Self;
end;

procedure TBufferedDrawDlg.VPaint;
begin
  BitBltCanvas(Canvas, 0, 0, ClientWidth, ClientHeight, Offscreen.Canvas, 0, 0);
end;

procedure TBufferedDrawDlg.ShowNewContent(NewMode: TWindowMode;
  ForceClose: Boolean);
begin
  if Visible then begin
    Assert((NewMode = wmModal) or (FWindowMode <> wmModal));
    // don't make modal window non-modal
    if (NewMode = wmModal) and (forceclose or (FWindowMode <> wmModal)) then
    begin // make modal
      UserLeft := Left;
      UserTop := Top;
      Visible := False;
      FWindowMode := NewMode;
      ShowModal;
    end
    else if forceclose then
    begin // make modal
      Visible := False;
      FWindowMode := NewMode;
      Left := UserLeft;
      Top := UserTop;
      Show;
    end
    else
    begin
      FWindowMode := NewMode;
      if @OnShow <> nil then
        OnShow(nil);
      Invalidate;
      BringToFront;
    end;
  end
  else
  begin
    FWindowMode := NewMode;
    Left := UserLeft;
    Top := UserTop;
    if FWindowMode = wmModal then begin
      Gtk2Fix;
      ShowModal;
    end
    else
      Show;
  end;
end;

procedure TBufferedDrawDlg.SmartUpdateContent(ImmUpdate: Boolean);
begin
  if Visible then begin
    OffscreenPaint;
    SmartInvalidate;
    if ImmUpdate then
      Update;
  end;
end;

procedure TBufferedDrawDlg.MarkUsedOffscreen(xMax, yMax: Integer);
begin
  if xMax > UsedOffscreenWidth then
    UsedOffscreenWidth := xMax;
  if yMax > UsedOffscreenHeight then
    UsedOffscreenHeight := yMax;
end;

procedure TBufferedDrawDlg.StayOnTop_Workaround;
// stayontop doesn't work when window is shown for the first time
// after application lost focus, so show all stayontop-windows in first turn
var
  SaveOnShow, SaveOnPaint: TNotifyEvent;
begin
  Top := Screen.Height;
  SaveOnShow := OnShow;
  OnShow := nil;
  SaveOnPaint := OnPaint;
  OnPaint := nil;
  FWindowMode := wmNone;
  Show;
  Hide;
  OnShow := SaveOnShow;
  OnPaint := SaveOnPaint;
end;

constructor TFramedDlg.Create;
begin
  OnCreate := FormCreate;
  inherited;
end;

procedure TFramedDlg.FormCreate(Sender: TObject);
begin
  CaptionLeft := 0;
  CaptionRight := $FFFF;
  WideBottom := False;
  FullCaption := True;
  TexOverride := False;
  ModalIndication := True;
  Canvas.Brush.Style := bsClear;
  InnerWidth := Width - 2 * SideFrame;
  InnerHeight := Height - TitleHeight - NarrowFrame;
end;

procedure TFramedDlg.SmartInvalidate;
var
  I, BottomFrame: Integer;
  r0, r1: HRgn;
begin
  if WideBottom then
    BottomFrame := WideFrame
  else
    BottomFrame := NarrowFrame;
  r0 := CreateRectRgn(SideFrame, TitleHeight, ClientWidth - SideFrame,
    ClientHeight - BottomFrame);
  for I := 0 to ControlCount - 1 do
    if not(Controls[I] is TArea) and Controls[I].Visible then
    begin
      with Controls[I].BoundsRect do
        r1 := CreateRectRgn(Left, Top, Right, Bottom);
      CombineRgn(r0, r0, r1, RGN_DIFF);
      DeleteObject(r1);
    end;
  InvalidateRgn(Handle, r0, False);
  DeleteObject(r0);
end;

procedure TFramedDlg.VPaint;

  procedure CornerFrame(x0, y0, x1, y1: Integer);
  begin
    Frame(Canvas, x0 + 1, y0 + 1, x1 - 2, y1 - 2, MainTexture.ColorBevelLight,
      MainTexture.ColorBevelShade);
    Frame(Canvas, x0 + 2, y0 + 2, x1 - 3, y1 - 3, MainTexture.ColorBevelLight,
      MainTexture.ColorBevelShade);
    Corner(Canvas, x0 + 1, y0 + 1, 0, MainTexture);
    Corner(Canvas, x1 - 9, y0 + 1, 1, MainTexture);
    Corner(Canvas, x0 + 1, y1 - 9, 2, MainTexture);
    Corner(Canvas, x1 - 9, y1 - 9, 3, MainTexture);
  end;

var
  I, L, FrameTop, FrameBottom, InnerBottom, Cut, xTexOffset,
    yTexOffset: Integer;
  R: TRect;
begin
  if not TexOverride then
  begin
    if (FWindowMode = wmModal) and ModalIndication then
      MainTexture := MainTexture
    else
      MainTexture := MainTexture;
    MainTexture := MainTexture;
  end;
  Canvas.Font.Assign(UniFont[ftCaption]);
  L := BiColorTextWidth(Canvas, Caption);
  Cut := (ClientWidth - L) div 2;
  xTexOffset := (Maintexture.Width - ClientWidth) div 2;
  yTexOffset := (Maintexture.Height - ClientHeight) div 2;
  if WideBottom then
    InnerBottom := ClientHeight - WideFrame
  else
    InnerBottom := ClientHeight - NarrowFrame;
  if FullCaption then
  begin
    FrameTop := 0;
    FrameBottom := ClientHeight;
  end
  else
  begin
    FrameTop := TitleHeight - NarrowFrame;
    if WideBottom then
      FrameBottom := ClientHeight - (WideFrame - NarrowFrame)
    else
      FrameBottom := ClientHeight;
  end;
  Fill(Canvas, 3, InnerBottom + 1, ClientWidth - 6, ClientHeight - InnerBottom -
    4, xTexOffset, yTexOffset);
  Fill(Canvas, 3, TitleHeight - 2, SideFrame - 3, InnerBottom - TitleHeight + 4,
    xTexOffset, yTexOffset);
  Fill(Canvas, ClientWidth - SideFrame, TitleHeight - 2, SideFrame - 3,
    InnerBottom - TitleHeight + 4, xTexOffset, yTexOffset);
  Frame(Canvas, 0, FrameTop, ClientWidth - 1, FrameBottom - 1, 0, 0);
  Frame(Canvas, SideFrame - 1, TitleHeight - 1, ClientWidth - SideFrame,
    InnerBottom, MainTexture.ColorBevelShade, MainTexture.ColorBevelLight);
  // RFrame(Canvas,SideFrame-2,TitleHeight-2,ClientWidth-SideFrame+1,
  // InnerBottom+1,MainTexture.ColorBevelShade,MainTexture.ColorBevelLight);
  if FullCaption then begin
    if (FWindowMode <> wmModal) or not ModalIndication then
    begin
      Fill(Canvas, 3, 3 + FrameTop, ClientWidth - 6, TitleHeight - FrameTop - 4,
        xTexOffset, yTexOffset);
      CornerFrame(0, FrameTop, ClientWidth, FrameBottom);
    end
    else
      with Canvas do
      begin
        Fill(Canvas, 3 + ModalFrameIndent, 3 + FrameTop,
          ClientWidth - 6 - 2 * ModalFrameIndent, TitleHeight - FrameTop - 4,
          xTexOffset, yTexOffset);
        Fill(Canvas, ClientWidth - 3 - ModalFrameIndent, 3 + FrameTop,
          ModalFrameIndent, TitleHeight - FrameTop - 4, xTexOffset, yTexOffset);
        Fill(Canvas, 3, 3 + FrameTop, ModalFrameIndent, TitleHeight - FrameTop -
          4, xTexOffset, yTexOffset);
        CornerFrame(0, FrameTop, ClientWidth, FrameBottom);
        Pen.Color := MainTexture.ColorBevelShade;
        MoveTo(3 + ModalFrameIndent, 2);
        LineTo(3 + ModalFrameIndent, TitleHeight);
        Pen.Color := MainTexture.ColorBevelShade;
        MoveTo(4 + ModalFrameIndent, TitleHeight - 1);
        LineTo(ClientWidth - 4 - ModalFrameIndent, TitleHeight - 1);
        LineTo(ClientWidth - 4 - ModalFrameIndent, 1);
        Pen.Color := MainTexture.ColorBevelLight;
        MoveTo(ClientWidth - 5 - ModalFrameIndent, 2);
        LineTo(4 + ModalFrameIndent, 2);
        LineTo(4 + ModalFrameIndent, TitleHeight);
        MoveTo(ClientWidth - 4 - ModalFrameIndent, 1);
        LineTo(3 + ModalFrameIndent, 1);
        Pen.Color := MainTexture.ColorBevelLight;
        MoveTo(ClientWidth - 3 - ModalFrameIndent, 3);
        LineTo(ClientWidth - 3 - ModalFrameIndent, TitleHeight);
      end;
  end
  else
  begin
    Fill(Canvas, 3, 3 + FrameTop, ClientWidth - 6, TitleHeight - FrameTop - 4,
      xTexOffset, yTexOffset);
    CornerFrame(0, FrameTop, ClientWidth, FrameBottom);

    Frame(Canvas, CaptionLeft, 0, ClientWidth - CaptionLeft - 1,
      FrameTop, 0, 0);
    Fill(Canvas, CaptionLeft + 3, 3, ClientWidth - 2 * (CaptionLeft) - 6,
      TitleHeight - 4, xTexOffset, yTexOffset);

    Frame(Canvas, CaptionLeft + 1, 0 + 1, ClientWidth - CaptionLeft - 2,
      TitleHeight - 1, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
    Frame(Canvas, CaptionLeft + 2, 0 + 2, ClientWidth - CaptionLeft - 3,
      TitleHeight - 1, MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
    Corner(Canvas, CaptionLeft + 1, 0 + 1, 0, MainTexture);
    Corner(Canvas, ClientWidth - CaptionLeft - 9, 0 + 1, 1, MainTexture);

    with Canvas do
    begin
      Pen.Color := MainTexture.ColorBevelShade;
      MoveTo(CaptionLeft + 1, FrameTop + 2);
      LineTo(CaptionLeft + 1, TitleHeight);
      Pen.Color := MainTexture.ColorBevelLight;
      MoveTo(ClientWidth - CaptionLeft - 2, FrameTop + 2);
      LineTo(ClientWidth - CaptionLeft - 2, TitleHeight);
    end;
    if WideBottom then
    begin
      Frame(Canvas, CaptionLeft, FrameBottom, ClientWidth - CaptionLeft - 1,
        ClientHeight - 1, 0, 0);
      Fill(Canvas, CaptionLeft + 3, ClientHeight - 3 - (WideFrame - 5),
        ClientWidth - 2 * (CaptionLeft) - 6, WideFrame - 5, xTexOffset,
        yTexOffset);
      Frame(Canvas, CaptionLeft + 1, ClientHeight - WideFrame - 1 + 1,
        ClientWidth - CaptionLeft - 2, ClientHeight - 2,
        MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
      Frame(Canvas, CaptionLeft + 2, ClientHeight - WideFrame - 1 + 1,
        ClientWidth - CaptionLeft - 3, ClientHeight - 3,
        MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
      Corner(Canvas, CaptionLeft + 1, ClientHeight - 9, 2, MainTexture);
      Corner(Canvas, ClientWidth - CaptionLeft - 9, ClientHeight - 9, 3,
        MainTexture);

      with Canvas do
      begin
        Pen.Color := MainTexture.ColorBevelShade;
        MoveTo(CaptionLeft + 1, ClientHeight - WideFrame);
        LineTo(CaptionLeft + 1, FrameBottom - 2);
        Pen.Color := MainTexture.ColorBevelLight;
        MoveTo(ClientWidth - CaptionLeft - 2, ClientHeight - WideFrame);
        LineTo(ClientWidth - CaptionLeft - 2, FrameBottom - 2);
      end;
    end;
  end;
  RisedTextOut(Canvas, Cut - 1, 7, Caption);

  for I := 0 to ControlCount - 1 do
    if Controls[I].Visible and (Controls[I] is TButtonBase) then
    begin
      R := Controls[I].BoundsRect;
      if (R.Bottom <= TitleHeight) or (R.Top >= InnerBottom) then
        BtnFrame(Canvas, R, MainTexture);
    end;

  BitBltCanvas(Canvas, SideFrame, TitleHeight, ClientWidth - 2 * SideFrame,
    InnerBottom - TitleHeight, Offscreen.Canvas, 0, 0);
end;

procedure TFramedDlg.InitWindowRegion;
var
  r0, r1: HRgn;
begin
  if FullCaption then
    Exit;
  r0 := CreateRectRgn(0, 0, ClientWidth, ClientHeight);
  r1 := CreateRectRgn(0, 0, CaptionLeft, TitleHeight - NarrowFrame);
  CombineRgn(r0, r0, r1, RGN_DIFF);
  // DeleteObject(r1);
  r1 := CreateRectRgn(ClientWidth - CaptionLeft, 0, ClientWidth,
    TitleHeight - NarrowFrame);
  CombineRgn(r0, r0, r1, RGN_DIFF);
  // DeleteObject(r1);
  if WideBottom then
  begin
    r1 := CreateRectRgn(0, ClientHeight - (WideFrame - NarrowFrame),
      CaptionLeft, ClientHeight);
    CombineRgn(r0, r0, r1, RGN_DIFF);
    // DeleteObject(r1);
    r1 := CreateRectRgn(ClientWidth - CaptionLeft,
      ClientHeight - (WideFrame - NarrowFrame), ClientWidth, ClientHeight);
    CombineRgn(r0, r0, r1, RGN_DIFF);
    // DeleteObject(r1);
  end;
  SetWindowRgn(Handle, r0, False);
  // DeleteObject(r0); // causes crash with Windows 95
end;

procedure TFramedDlg.FillOffscreen(Left, Top, Width, Height: Integer);
begin
  Fill(Offscreen.Canvas, Left, Top, Width, Height,
    SideFrame + (Maintexture.Width - ClientWidth) div 2,
    TitleHeight + (Maintexture.Height - ClientHeight) div 2);
end;

procedure CreateOffscreen(var Offscreen: TBitmap);
begin
  if Offscreen <> nil then
    Exit;
  Offscreen := TBitmap.Create;
  Offscreen.PixelFormat := pf24bit;
  if Screen.Height - yUnused < 480 then
    Offscreen.SetSize(Screen.Width, 480)
  else
    Offscreen.SetSize(Screen.Width, Screen.Height - yUnused);
  Offscreen.Canvas.FillRect(0, 0, Offscreen.Width, OffScreen.Height);
  Offscreen.Canvas.Brush.Style := bsClear;
end;

initialization

ShowNewContentProc := nil;
MainFormKeyDown := nil;

end.
