unit ButtonN;

interface

uses
  Classes, Graphics, Controls, LCLIntf, LCLType, ScreenTools;

type
  TButtonN = class(TGraphicControl)
    constructor Create(aOwner: TComponent); override;
  private
    FPossible, FLit: Boolean;
    FGraphic, FMask, FBackGraphic: TBitmap;
    FIndex, BackIndex: Integer;
    FSmartHint: string;
    ChangeProc: TNotifyEvent;
    procedure SetPossible(X: Boolean);
    procedure SetLit(X: Boolean);
    procedure SetIndex(X: Integer);
    procedure SetSmartHint(X: string);
  published
    property Possible: Boolean read FPossible write SetPossible;
    property Lit: Boolean read FLit write SetLit;
    property SmartHint: string read FSmartHint write SetSmartHint;
    property Graphic: TBitmap read FGraphic write FGraphic;
    property Mask: TBitmap read FMask write FMask;
    property BackGraphic: TBitmap read FBackGraphic write FBackGraphic;
    property ButtonIndex: Integer read FIndex write SetIndex;
    property OnClick: TNotifyEvent read ChangeProc write ChangeProc;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('C-evo', [TButtonN]);
end;

constructor TButtonN.Create(aOwner: TComponent);
begin
  inherited;
  ShowHint := True;
  FGraphic := nil;
  FBackGraphic := nil;
  FPossible := True;
  FLit := False;
  FIndex := -1;
  ChangeProc := nil;
  SetBounds(0, 0, 42, 42);
end;

procedure TButtonN.Paint;
begin
  with Canvas do
  begin
    if FGraphic <> nil then
    begin
      BitBltCanvas(Canvas, 1, 1, 40, 40, FBackGraphic.Canvas,
        1 + 80 * BackIndex + 40 * Byte(FPossible and FLit), 176);
      if FPossible then
      begin
        BitBltCanvas(Canvas, 3, 3, 36, 36, FMask.Canvas,
          195 + 37 * (FIndex mod 3), 21 + 37 * (FIndex div 3), SRCAND);
        BitBltCanvas(Canvas, 3, 3, 36, 36, FGraphic.Canvas,
          195 + 37 * (FIndex mod 3), 21 + 37 * (FIndex div 3), SRCPAINT);
      end;
    end;
    MoveTo(0, 41);
    Pen.Color := $B0B0B0;
    LineTo(0, 0);
    LineTo(41, 0);
    Pen.Color := $FFFFFF;
    LineTo(41, 41);
    LineTo(0, 41);
  end;
end;

procedure TButtonN.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FPossible and (Button = mbLeft) and (@ChangeProc <> nil) then
    ChangeProc(Self);
end;

procedure TButtonN.SetPossible(X: Boolean);
begin
  if X <> FPossible then
  begin
    FPossible := X;
    if X then
      Hint := FSmartHint
    else
      Hint := '';
    Invalidate;
  end;
end;

procedure TButtonN.SetLit(X: Boolean);
begin
  if X <> FLit then
  begin
    FLit := X;
    Invalidate;
  end;
end;

procedure TButtonN.SetIndex(X: Integer);
begin
  if X <> FIndex then
  begin
    FIndex := X;
    if X < 6 then
      BackIndex := 1
    else
      BackIndex := 0;
    Invalidate;
  end;
end;

procedure TButtonN.SetSmartHint(X: string);
begin
  if X <> FSmartHint then
  begin
    FSmartHint := X;
    if FPossible then
      Hint := X;
  end;
end;

end.
