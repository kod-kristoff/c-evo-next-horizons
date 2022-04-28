unit ButtonC;

interface

uses
  ButtonBase, Classes, Graphics, LCLIntf, LCLType, ScreenTools;

type
  TButtonC = class(TButtonBase)
    constructor Create(aOwner: TComponent); override;
  private
    FIndex: Integer;
    procedure SetIndex(Text: Integer);
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
  inherited;
  ShowHint := True;
  SetBounds(0, 0, 12, 12);
end;

procedure TButtonC.Paint;
begin
  with Canvas do
    if FGraphic <> nil then
      BitBltCanvas(Canvas, 0, 0, 12, 12, FGraphic.Canvas,
        169 + 13 * Byte(FDown), 159 + 13 * FIndex)
    else
    begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 12, 12));
    end;
end;

procedure TButtonC.SetIndex(Text: integer);
begin
  if Text <> FIndex then
  begin
    FIndex := Text;
    Invalidate;
  end;
end;

end.
