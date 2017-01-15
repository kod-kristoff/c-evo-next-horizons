{$INCLUDE Switches.inc}
unit TechTree;

interface

uses
  ScreenTools, Messg, LCLIntf, LCLType,

  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ButtonBase, ButtonB;

type
  TTechTreeDlg = class(TDrawDlg)
    CloseBtn: TButtonB;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CloseBtnClick(Sender: TObject);
  private
    xOffset, yOffset, xDown, yDown: Integer;
    Image: TBitmap;
    dragging: boolean;
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

function min(a, b: Integer): Integer;
begin
  if a < b then
    result := a
  else
    result := b;
end;

function max(a, b: Integer): Integer;
begin
  if a > b then
    result := a
  else
    result := b;
end;

procedure TTechTreeDlg.FormCreate(Sender: TObject);
begin
  InitButtons;
  Image := nil;
end;

procedure TTechTreeDlg.FormPaint(Sender: TObject);
var
  X, w: Integer;
begin
  with Canvas do
  begin
    // black border
    brush.color := $000000;
    fillrect(rect(0, 0, BlackBorder, ClientHeight));
    fillrect(rect(BlackBorder, 0, ClientWidth - BlackBorder, BlackBorder));
    fillrect(rect(ClientWidth - BlackBorder, 0, ClientWidth, ClientHeight));
    fillrect(rect(BlackBorder, ClientHeight - BlackBorder,
      ClientWidth - BlackBorder, ClientHeight));

    // texturize empty space
    brush.color := $FFFFFF;
    if xOffset > 0 then
      FillRectSeamless(Canvas, BlackBorder, BlackBorder, BlackBorder + xOffset,
        ClientHeight - BlackBorder, -BlackBorder - xOffset,
        -BlackBorder - yOffset, Paper);
    if xOffset + Image.width < ClientWidth - 2 * BlackBorder then
      FillRectSeamless(Canvas, BlackBorder + xOffset + Image.width, BlackBorder,
        ClientWidth - BlackBorder, ClientHeight - BlackBorder,
        -BlackBorder - xOffset, -BlackBorder - yOffset, Paper);
    X := max(BlackBorder, BlackBorder + xOffset);
    w := min(BlackBorder + xOffset + Image.width, ClientWidth - BlackBorder);
    if yOffset > 0 then
      FillRectSeamless(Canvas, X, BlackBorder, w, BlackBorder + yOffset,
        -BlackBorder - xOffset, -BlackBorder - yOffset, Paper);
    if yOffset + Image.height < ClientHeight - 2 * BlackBorder then
      FillRectSeamless(Canvas, X, BlackBorder + yOffset + Image.height, w,
        ClientHeight - BlackBorder, -BlackBorder - xOffset,
        -BlackBorder - yOffset, Paper);
  end;
  BitBlt(Canvas.Handle, max(BlackBorder, BlackBorder + xOffset),
    max(BlackBorder, BlackBorder + yOffset),
    min(Image.width, min(Image.width + xOffset,
    min(ClientWidth - 2 * BlackBorder, ClientWidth - 2 * BlackBorder - xOffset))
    ), min(Image.height, min(Image.height + yOffset,
    min(ClientHeight - 2 * BlackBorder, ClientHeight - 2 * BlackBorder -
    yOffset))), Image.Canvas.Handle, max(0, -xOffset),
    max(0, -yOffset), SRCCOPY);
end;

procedure TTechTreeDlg.FormShow(Sender: TObject);
var
  X, Y, ad, TexWidth, TexHeight: Integer;
  s: string;
  SrcPixel, DstPixel: TPixelPointer;
begin
  if Image = nil then
  begin
    Image := TBitmap.Create;
    Image.PixelFormat := pf24bit;
    LoadGraphicFile(Image, HomeDir + 'Help' + DirectorySeparator + 'AdvTree.png', gfNoGamma);

    with Image.Canvas do
    begin
      // write advance names
      Font.Assign(UniFont[ftSmall]);
      Font.color := clBlack;
      brush.Style := bsClear;
      for X := 0 to (Image.width - xStart) div xPitch do
        for Y := 0 to (Image.height - yStart) div yPitch do
        begin
          ad := Pixels[xStart + X * xPitch + 10, yStart + Y * yPitch - 1];
          if ad and $FFFF00 = 0 then
          begin
            s := Phrases.Lookup('ADVANCES', ad);
            while TextWidth(s) > 112 do
              Delete(s, Length(s), 1);
            TextOut(xStart + X * xPitch + 2, yStart + Y * yPitch, s);
            Pixels[xStart + X * xPitch + 10, yStart + Y * yPitch - 1]
              := $7F007F;
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

    // texturize background
    Image.BeginUpdate;
    TexWidth := Paper.Width;
    TexHeight := Paper.Height;
    DstPixel.Init(Image);
    SrcPixel.Init(Paper);
    for Y := 0 to Image.Height - 1 do begin
      for X := 0 to Image.Width - 1 do begin
        if (DstPixel.Pixel^.ARGB and $FFFFFF) = $7F007F then begin // transparent
          SrcPixel.SetXY(X mod TexWidth, Y mod TexHeight);
          DstPixel.Pixel^.B := SrcPixel.Pixel^.B;
          DstPixel.Pixel^.G := SrcPixel.Pixel^.G;
          DstPixel.Pixel^.R := SrcPixel.Pixel^.R;
        end;
        DstPixel.NextPixel;
      end;
      DstPixel.NextLine;
    end;
    Image.EndUpdate;
  end;

  // fit window to image, center image in window, center window to screen
  width := min(Screen.width - 40, Image.width + LeftBorder + RightBorder + 2 *
    BlackBorder);
  height := min(Screen.height - 40, Image.height + TopBorder + BottomBorder + 2
    * BlackBorder);
  Left := (Screen.width - width) div 2;
  Top := (Screen.height - height) div 2;
  CloseBtn.Left := width - CloseBtn.width - BlackBorder - 8;
  CloseBtn.Top := BlackBorder + 8;
  xOffset := (ClientWidth - Image.width + LeftBorder - RightBorder) div 2 -
    BlackBorder;
  yOffset := ClientHeight - 2 * BlackBorder - Image.height - BottomBorder;
end;

procedure TTechTreeDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    dragging := true;
    xDown := X;
    yDown := Y;
  end
end;

procedure TTechTreeDlg.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  dragging := false;
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
  end
end;

procedure TTechTreeDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TTechTreeDlg.CloseBtnClick(Sender: TObject);
begin
  Close();
end;

end.
