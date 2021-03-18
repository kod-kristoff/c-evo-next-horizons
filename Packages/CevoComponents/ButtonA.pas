unit ButtonA;

interface

uses
  ButtonBase, Classes, Graphics, LCLIntf, LCLType, ScreenTools, Types;

type
  TButtonA = class(TButtonBase)
    constructor Create(aOwner: TComponent); override;
  private
    FCaption: string;
    procedure SetCaption(Text: string);
    procedure SetFont(const Font: TFont);
  published
    property Visible;
    property Caption: string read FCaption write SetCaption;
    property OnClick;
  public
    property Font: TFont write SetFont;
  protected
    procedure Paint; override;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('C-evo', [TButtonA]);
end;

constructor TButtonA.Create(aOwner: TComponent);
begin
  inherited;
  FCaption := '';
  SetBounds(0, 0, 100, 25);
end;

procedure TButtonA.Paint;
var
  TextSize: TSize;
begin
  with Canvas do
    if FGraphic <> nil then begin
      BitBltCanvas(Canvas, 0, 0, 100, 25, Graphic.Canvas, 195,
        243 + 26 * Byte(Down));
      Canvas.Brush.Style := bsClear;
      TextSize := TextExtent(FCaption);
      TextOut(50 - (TextSize.Width + 1) div 2,
        12 - TextSize.Height div 2, FCaption);
    end else begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 100, 25))
    end;
end;

procedure TButtonA.SetCaption(Text: string);
begin
  if Text <> FCaption then begin
    FCaption := Text;
    Invalidate;
  end;
end;

procedure TButtonA.SetFont(const Font: TFont);
begin
  Canvas.Font.Assign(Font);
  Canvas.Font.Color := $000000;
end;

end.
