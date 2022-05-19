unit EOTButton;

interface

uses
  ButtonBase, Classes, SysUtils, Graphics, LCLIntf, LCLType;

const
  eotBlinkOff = -1;
  eotCancel = 0;
  eotGray = 1;
  eotBlinkOn = 2;
  eotBackToNego = 3;

type
  // EndOfTurn button
  TEOTButton = class(TButtonBase)
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetButtonIndexFast(X: Integer);
    procedure SetBack(ca: TCanvas; X, Y: Integer);
  private
    FTemplate: TBitmap;
    FIndex: Integer;
    procedure SetIndex(X: Integer);
  public
    property Template: TBitmap read FTemplate write FTemplate;
  published
    property Visible;
    property ButtonIndex: Integer read FIndex write SetIndex;
    property OnClick;
  protected
    Buffer, Back: TBitmap;
    procedure Paint; override;
  end;

procedure Register;


implementation

uses
  ScreenTools;

procedure Register;
begin
  RegisterComponents('C-evo', [TEOTButton]);
end;

constructor TEOTButton.Create;
begin
  inherited;
  Buffer := TBitmap.Create;
  Buffer.PixelFormat := pf24bit;
  Buffer.SetSize(48, 48);
  Buffer.Canvas.FillRect(0, 0, Buffer.Width, Buffer.Height);
  Back := TBitmap.Create;
  Back.PixelFormat := pf24bit;
  Back.SetSize(48, 48);
  Back.Canvas.FillRect(0, 0, Back.Width, Back.Height);
  ShowHint := True;
  SetBounds(0, 0, 48, 48);
end;

destructor TEOTButton.Destroy;
begin
  FreeAndNil(Buffer);
  FreeAndNil(Back);
  inherited;
end;

procedure TEOTButton.Paint;
begin
  with Canvas do
    if FGraphic <> nil then begin
      UnshareBitmap(Buffer);
      BitBltCanvas(Buffer.Canvas, 0, 0, 48, 48, Back.Canvas, 0, 0);
      ImageOp_CBC(Buffer, Template, 0, 0, 133, 149 + 48 * Byte(FDown), 48, 48,
        $000000, $FFFFFF);
      if FIndex >= 0 then
        ImageOp_CBC(Buffer, Template, 8, 8, 1 + 32 * Byte(FIndex), 246, 32, 32,
          $000000, $FFFFFF);
      BitBltCanvas(Canvas, 0, 0, 48, 48, Buffer.Canvas, 0, 0);
    end else begin
      Brush.Color := $0000FF;
      FrameRect(Rect(0, 0, 48, 48))
    end;
end;

procedure TEOTButton.SetIndex(X: Integer);
begin
  if X <> FIndex then begin
    FIndex := X;
    Invalidate;
  end;
end;

procedure TEOTButton.SetButtonIndexFast(X: Integer);
begin
  if Visible and (X <> FIndex) then begin
    FIndex := X;
    try
      Paint;
    except
    end;
  end;
end;

procedure TEOTButton.SetBack(ca: TCanvas; X, Y: Integer);
begin
  BitBltCanvas(Back.Canvas, 0, 0, 48, 48, ca, X, Y);
end;

end.
