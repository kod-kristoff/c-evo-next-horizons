unit ButtonBase;

interface

uses
  Classes, Graphics, Controls;

type
  TButtonBase = class(TGraphicControl)
  protected
    FDown: Boolean;
    FPermanent: Boolean;
    FGraphic: TBitmap;
    // FDownSound, FUpSound: string;
    ClickProc: TNotifyEvent;
    DownChangedProc: TNotifyEvent;
    procedure SetDown(X: Boolean);
    // procedure PlayDownSound;
    // procedure PlayUpSound;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  private
    Active: Boolean;
  public
    constructor Create(aOwner: TComponent); override;
    property Graphic: TBitmap read FGraphic write FGraphic;
    // property DownSound: string read FDownSound write FDownSound;
    // property UpSound: string read FUpSound write FUpSound;
  published
    property Visible;
    property Down: Boolean read FDown write SetDown;
    property Permanent: Boolean read FPermanent write FPermanent;
    property OnClick: TNotifyEvent read ClickProc write ClickProc;
    property OnDownChanged: TNotifyEvent read DownChangedProc
      write DownChangedProc;
  end;

implementation

// uses
// MMSystem;

constructor TButtonBase.Create(aOwner: TComponent);
begin
  inherited;
  // FDownSound:='';
  // FUpSound:='';
  FGraphic := nil;
  Active := False;
  FDown := False;
  FPermanent := False;
  ClickProc := nil;
end;

procedure TButtonBase.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Active := True;
  MouseMove(Shift, X, Y);
end;

procedure TButtonBase.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    Exit;
  MouseMove(Shift, X, Y);
  if Active and FDown then
  begin
    // PlayUpSound;
    Active := False;
    if FDown <> FPermanent then
    begin
      FDown := FPermanent;
      Invalidate;
      if @DownChangedProc <> nil then
        DownChangedProc(self);
    end;
    if (Button = mbLeft) and (@ClickProc <> nil) then
      ClickProc(self);
  end
  else
  begin
    // if FDown then PlayUpSound;
    Active := False;
    if FDown then
    begin
      FDown := False;
      Invalidate;
      if @DownChangedProc <> nil then
        DownChangedProc(self);
    end;
  end;
end;

procedure TButtonBase.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Active then
    if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
      if (ssLeft in Shift) and not FDown then
      begin
        { PlayDownSound; }
        FDown := True;
        Paint;
        if @DownChangedProc <> nil then
          DownChangedProc(self);
      end
      else
    else if FDown and not FPermanent then
    begin
      { PlayUpSound; }
      FDown := False;
      Paint;
      if @DownChangedProc <> nil then
        DownChangedProc(self);
    end;
end;

procedure TButtonBase.SetDown(X: Boolean);
begin
  FDown := X;
  Invalidate;
end;

// procedure TButtonBase.PlayDownSound;
// begin
// if DownSound<>'' then SndPlaySound(pchar(DownSound),SND_ASYNC)
// end;

// procedure TButtonBase.PlayUpSound;
// begin
// if UpSound<>'' then SndPlaySound(pchar(UpSound),SND_ASYNC)
// end;

end.
