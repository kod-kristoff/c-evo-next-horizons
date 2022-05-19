unit ButtonB;

interface

uses
  ButtonBase, Classes, Graphics, LCLIntf, LCLType;

type
  TButtonB = class(TButtonBase)
    constructor Create(aOwner: TComponent); override;
  private
    FMask: TBitmap;
    FIndex: Integer;
    procedure SetIndex(Text: Integer);
  public
    property Mask: TBitmap read FMask write FMask;
  published
    property Visible;
    property ButtonIndex: Integer read FIndex write SetIndex;
    property OnClick;
  protected
    procedure Paint; override;
  end;

procedure Register;


implementation

uses
  ScreenTools;

procedure Register;
begin
  RegisterComponents('C-evo', [TButtonB]);
end;

constructor TButtonB.Create(aOwner: TComponent);
begin
  inherited;
  ShowHint := True;
  SetBounds(0, 0, 25, 25);
end;

procedure TButtonB.Paint;
begin
  with Canvas do
    if FGraphic <> nil then begin
      BitBltCanvas(Canvas, 0, 0, 25, 25, FGraphic.Canvas, 169,
        243 + 26 * Byte(FDown));
      if FIndex >= 0 then begin
        BitBltCanvas(Canvas, 0, 0, 25, 25, FMask.Canvas,
          1 + FIndex mod 12 * 26, 337 + FIndex div 12 * 26, SRCAND);
        BitBltCanvas(Canvas, 0, 0, 25, 25, FGraphic.Canvas,
          1 + FIndex mod 12 * 26, 337 + FIndex div 12 * 26, SRCPAINT);
      end
    end else begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 25, 25));
    end;
end;

procedure TButtonB.SetIndex(Text: Integer);
begin
  if Text <> FIndex then begin
    FIndex := Text;
    Invalidate;
  end;
end;

end.
