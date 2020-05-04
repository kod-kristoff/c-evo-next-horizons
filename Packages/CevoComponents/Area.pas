unit Area;

interface

uses
  Classes, Graphics, Controls;

type
  TArea = class(TGraphicControl)
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Paint; override;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('C-evo', [TArea]);
end;

constructor TArea.Create(AOwner: TComponent);
begin
  inherited;
  Enabled := False;
  ShowHint := True;
end;

procedure TArea.Paint;
begin
  if csDesigning in ComponentState then
    with Canvas do
      begin
        Brush.Color := $FF0000;
        FrameRect(Rect(0, 0, Width, Height));
      end;
end;

end.

