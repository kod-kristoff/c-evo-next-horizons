{$INCLUDE Switches.inc}
unit TechTree;

interface

uses
  ScreenTools, LCLIntf, LCLType, SysUtils, Classes, Graphics,
  Controls, Forms, ButtonB, DrawDlg;

type

  { TTechTreeDlg }

  TTechTreeDlg = class(TDrawDlg)
    CloseBtn: TButtonB;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CloseBtnClick(Sender: TObject);
  private
    xOffset, yOffset, xDown, yDown: Integer;
    Image: TBitmap;
    Dragging: Boolean;
  end;

var
  TechTreeDlg: TTechTreeDlg;


implementation

uses
  Directories;

{$R *.lfm}

const
  BlackBorder = 4;
  LeftBorder = 72;
  RightBorder = 45;
  TopBorder = 16;
  BottomBorder = 48;
  xStart = 0;
  yStart = 40;
  xPitch = 160;
  yPitch = 90;
  xLegend = 44;
  yLegend = 79;
  yLegendPitch = 32;

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

procedure TTechTreeDlg.FormCreate(Sender: TObject);
begin
  InitButtons;
  Image := nil;
end;

procedure TTechTreeDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Image);
end;

procedure TTechTreeDlg.FormPaint(Sender: TObject);
var
  X, W: Integer;
begin
  with Canvas do begin
    // black border
    Brush.Color := $000000;
    FillRect(rect(0, 0, BlackBorder, ClientHeight));
    FillRect(rect(BlackBorder, 0, ClientWidth - BlackBorder, BlackBorder));
    FillRect(rect(ClientWidth - BlackBorder, 0, ClientWidth, ClientHeight));
    FillRect(rect(BlackBorder, ClientHeight - BlackBorder,
      ClientWidth - BlackBorder, ClientHeight));

    // texturize empty space
    Brush.Color := $FFFFFF;
    if xOffset > 0 then
      FillRectSeamless(Canvas, BlackBorder, BlackBorder, BlackBorder + xOffset,
        ClientHeight - BlackBorder, -BlackBorder - xOffset,
        -BlackBorder - yOffset, Paper);
    if xOffset + Image.width < ClientWidth - 2 * BlackBorder then
      FillRectSeamless(Canvas, BlackBorder + xOffset + Image.width, BlackBorder,
        ClientWidth - BlackBorder, ClientHeight - BlackBorder,
        -BlackBorder - xOffset, -BlackBorder - yOffset, Paper);
    X := Max(BlackBorder, BlackBorder + xOffset);
    W := Min(BlackBorder + xOffset + Image.width, ClientWidth - BlackBorder);
    if yOffset > 0 then
      FillRectSeamless(Canvas, X, BlackBorder, W, BlackBorder + yOffset,
        -BlackBorder - xOffset, -BlackBorder - yOffset, Paper);
    if yOffset + Image.height < ClientHeight - 2 * BlackBorder then
      FillRectSeamless(Canvas, X, BlackBorder + yOffset + Image.height, W,
        ClientHeight - BlackBorder, -BlackBorder - xOffset,
        -BlackBorder - yOffset, Paper);
  end;
  BitBltCanvas(Canvas, Max(BlackBorder, BlackBorder + xOffset),
    Max(BlackBorder, BlackBorder + yOffset),
    Min(Image.width, Min(Image.width + xOffset,
    Min(ClientWidth - 2 * BlackBorder, ClientWidth - 2 * BlackBorder - xOffset))
    ), Min(Image.height, Min(Image.height + yOffset,
    Min(ClientHeight - 2 * BlackBorder, ClientHeight - 2 * BlackBorder -
    yOffset))), Image.Canvas, Max(0, -xOffset),
    Max(0, -yOffset));
end;

procedure TTechTreeDlg.FormShow(Sender: TObject);
var
  X, Y, ad: Integer;
  S: string;
  NewWidth: Integer;
  NewHeight: Integer;
begin
  Caption := Phrases2.Lookup('MENU_ADVTREE');
  if Image = nil then begin
    Image := TBitmap.Create;
    Image.PixelFormat := pf24bit;
    LoadGraphicFile(Image, HomeDir + 'Help' + DirectorySeparator + 'AdvTree.png',
      [gfNoGamma]);

    with Image.Canvas do begin
      // write advance names
      Font.Assign(UniFont[ftSmall]);
      Font.Color := clBlack;
      Brush.Style := bsClear;
      for X := 0 to (Image.width - xStart) div xPitch do
        for Y := 0 to (Image.height - yStart) div yPitch do
        begin
          ad := Pixels[xStart + X * xPitch + 10, yStart + Y * yPitch - 1];
          if ad and $FFFF00 = 0 then
          begin
            S := Phrases.Lookup('ADVANCES', ad);
            while TextWidth(S) > 112 do
              Delete(S, Length(S), 1);
            TextOut(xStart + X * xPitch + 2, yStart + Y * yPitch, S);
            Pixels[xStart + X * xPitch + 10, yStart + Y * yPitch - 1]
              := TransparentColor2;
          end
        end;

      // write legend
      TextOut(xLegend, yLegend, Phrases2.Lookup('ADVTREE_UP0'));
      TextOut(xLegend, yLegend + yLegendPitch, Phrases2.Lookup('ADVTREE_UP1'));
      TextOut(xLegend, yLegend + 2 * yLegendPitch,
        Phrases2.Lookup('ADVTREE_UP2'));
      TextOut(xLegend, yLegend + 3 * yLegendPitch,
        Phrases2.Lookup('ADVTREE_GOV'));
      TextOut(xLegend, yLegend + 4 * yLegendPitch,
        Phrases2.Lookup('ADVTREE_OTHER'));
    end;

    Texturize(Image, Paper, TransparentColor2);
  end;

  // fit window to image, center image in window, center window to screen
  NewWidth := Min(Screen.Width - 40, Image.Width + LeftBorder + RightBorder + 2 * BlackBorder);
  NewHeight := Min(Screen.Height - 40, Image.Height + TopBorder + BottomBorder + 2 * BlackBorder);
  BoundsRect := Bounds((Screen.Width - NewWidth) div 2,
    (Screen.Height - NewHeight) div 2,
    NewWidth, NewHeight);
  CloseBtn.Left := width - CloseBtn.width - BlackBorder - 8;
  CloseBtn.Top := BlackBorder + 8;
  xOffset := (ClientWidth - Image.Width + LeftBorder - RightBorder) div 2 -
    BlackBorder;
  yOffset := ClientHeight - 2 * BlackBorder - Image.Height - BottomBorder;
end;

procedure TTechTreeDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    dragging := True;
    xDown := X;
    yDown := Y;
  end;
end;

procedure TTechTreeDlg.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  dragging := False;
end;

procedure TTechTreeDlg.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if dragging then
  begin
    xOffset := xOffset + X - xDown;
    yOffset := yOffset + Y - yDown;
    xDown := X;
    yDown := Y;

    if xOffset > LeftBorder then
      xOffset := LeftBorder;
    if xOffset < ClientWidth - 2 * BlackBorder - Image.width - RightBorder then
      xOffset := ClientWidth - 2 * BlackBorder - Image.width - RightBorder;
    if yOffset > TopBorder then
      yOffset := TopBorder;
    if yOffset < ClientHeight - 2 * BlackBorder - Image.height - BottomBorder
    then
      yOffset := ClientHeight - 2 * BlackBorder - Image.height - BottomBorder;

    SmartInvalidate;
  end;
end;

procedure TTechTreeDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

end.
