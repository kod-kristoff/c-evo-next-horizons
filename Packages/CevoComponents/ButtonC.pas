unit ButtonC;

interface

uses
  ButtonBase,
  Classes, Graphics, LCLIntf, LCLType;

type
  TButtonC = class(TButtonBase)
    constructor Create(aOwner: TComponent); override;
  private
    FIndex: Integer;
    procedure SetIndex(x: Integer);
  published
    property Visible;
    property ButtonIndex: integer read FIndex write SetIndex;
    property OnClick;
  protected
    procedure Paint; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('C-evo', [TButtonC]);
end;

constructor TButtonC.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ShowHint := True;
  SetBounds(0, 0, 12, 12);
end;

procedure TButtonC.Paint;
begin
  with Canvas do
    if FGraphic <> nil then
      BitBlt(Canvas.Handle, 0, 0, 12, 12, FGraphic.Canvas.Handle,
        169 + 13 * Byte(FDown), 159 + 13 * FIndex, SRCCOPY)
    else
    begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 12, 12))
    end
end;

procedure TButtonC.SetIndex(x: integer);
begin
  if x <> FIndex then
  begin
    FIndex := x;
    Invalidate;
  end;
end;

end.
