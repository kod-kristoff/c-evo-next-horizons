unit ButtonA;

interface

uses
  ButtonBase, Classes, Graphics, LCLIntf, LCLType;

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
  inherited Create(aOwner);
  FCaption := '';
  SetBounds(0, 0, 100, 25);
end;

procedure TButtonA.Paint;
begin
  with Canvas do
    if FGraphic <> nil then
    begin
      BitBlt(Canvas.Handle, 0, 0, 100, 25, Graphic.Canvas.Handle, 195,
        243 + 26 * Byte(Down), SRCCOPY);
      Canvas.Brush.Style := bsClear;
      Textout(50 - (TextWidth(FCaption) + 1) div 2, 12 - textheight(FCaption)
        div 2, FCaption);
    end
    else
    begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 100, 25))
    end
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
