{$INCLUDE Switches.inc}
unit PVSB;

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, Controls, Forms, LCLIntf, LCLType, LMessages, Messages, SysUtils,
  StdCtrls, Math;

type

  { TPVScrollbar }

  TPVScrollBar = class
  private
    FOnUpdate: TNotifyEvent;
    ScrollBar: TScrollBar;
    FMax: Integer;
    function GetMax: Integer;
    function GetPageSize: Integer;
    function GetPosition: Integer;
    procedure ScrollBarChanged(Sender: TObject);
    procedure SetMax(AValue: Integer);
    procedure SetPageSize(AValue: Integer);
    procedure SetPosition(AValue: Integer);
  public
    constructor Create(Parent: TWinControl);
    destructor Destroy; override;
    procedure Init(Max, PageSize: Integer);
    procedure SetPos(Pos: Integer);
    function Process(const m: TMessage): boolean;
    function ProcessMouseWheel(Delta: Integer): Boolean;
    procedure Show(Visible: boolean);
    procedure EndSB;
    procedure SetBorderSpacing(Top, Right, Bottom: Integer);
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property Position: Integer read GetPosition write SetPosition;
    property Max: Integer read GetMax write SetMax;
    property PageSize: Integer read GetPageSize write SetPageSize;
  end;


implementation

const
  Count: Integer = 0;

procedure TPVScrollBar.Init(Max, PageSize: Integer);
begin
  ScrollBar.PageSize := PageSize;
  ScrollBar.Min := 0;
  Self.Max := Max;
  ScrollBar.Position := 0;
  ScrollBar.Visible := Max >= ScrollBar.PageSize;
end;

procedure TPVScrollBar.SetPos(Pos: Integer);
begin
  if Pos <> 0 then begin
    ScrollBar.Position := Pos;
  end;
end;

function TPVScrollBar.Process(const m: TMessage): boolean;
var
  NewPos: integer;
begin
    if Max < ScrollBar.PageSize then
      result := false
    else
    begin
      if (m.wParam and $ffff) in [SB_THUMBPOSITION, SB_THUMBTRACK] then
      begin
        result := ((m.wParam shr 16) and $ffff) <> ScrollBar.Position;
        ScrollBar.Position := (m.wParam shr 16) and $ffff;
      end else begin
        case (m.wParam and $ffff) of
          SB_LINEUP:
            NewPos := ScrollBar.Position - 1;
          SB_LINEDOWN:
            NewPos := ScrollBar.Position + 1;
          SB_PAGEUP:
            NewPos := ScrollBar.Position - ScrollBar.PageSize;
          SB_PAGEDOWN:
            NewPos := ScrollBar.Position + ScrollBar.PageSize;
        else
          NewPos := ScrollBar.Position
        end;
        if NewPos < 0 then
          NewPos := 0;
        if NewPos > Max - ScrollBar.PageSize + 1 then
          NewPos := Max - ScrollBar.PageSize + 1;
        result := NewPos <> ScrollBar.Position;
        if (NewPos <> ScrollBar.Position) or ((m.wParam and $ffff) = SB_ENDSCROLL) then
        begin
          ScrollBar.Position := NewPos;
        end;
      end;
    end;
end;

function TPVScrollBar.ProcessMouseWheel(Delta: Integer): Boolean;
var
  NewPos: integer;
begin
    if Max < ScrollBar.PageSize then Result := False
    else begin
      NewPos := ScrollBar.Position - Delta div 30;
      if NewPos < 0 then NewPos := 0;
      if NewPos > Max - ScrollBar.PageSize + 1 then
        NewPos := Max - ScrollBar.PageSize + 1;
      Result := NewPos <> ScrollBar.Position;
      if NewPos <> ScrollBar.Position then begin
        ScrollBar.Position := NewPos;
      end;
    end;
end;

procedure TPVScrollBar.Show(Visible: boolean);
begin
  if not Visible or (Max < ScrollBar.PageSize) then
    ScrollBar.Visible := False
    else ScrollBar.Visible := True;
end;

procedure TPVScrollBar.EndSB;
begin
  if Max < ScrollBar.PageSize then
    ScrollBar.Position := 0 // hidden
  else begin
    ScrollBar.Position := Max - ScrollBar.PageSize + 1;
  end;
end;

procedure TPVScrollBar.SetBorderSpacing(Top, Right, Bottom: Integer);
begin
  ScrollBar.BorderSpacing.Top := Top;
  ScrollBar.BorderSpacing.Right := Right;
  ScrollBar.BorderSpacing.Bottom := Bottom;
end;

{ TPVScrollbar }

procedure TPVScrollBar.ScrollBarChanged(Sender: TObject);
begin
  if Assigned(FOnUpdate) then FOnUpdate(Self);
end;

procedure TPVScrollBar.SetMax(AValue: Integer);
begin
  FMax := AValue;
  ScrollBar.Max := Math.Max(0, FMax);
end;

procedure TPVScrollBar.SetPageSize(AValue: Integer);
begin
  ScrollBar.PageSize := AValue;
end;

function TPVScrollBar.GetPosition: Integer;
begin
  Result := ScrollBar.Position;
end;

function TPVScrollBar.GetMax: Integer;
begin
  Result := FMax;
end;

function TPVScrollBar.GetPageSize: Integer;
begin
  Result := ScrollBar.PageSize;
end;

procedure TPVScrollBar.SetPosition(AValue: Integer);
begin
  ScrollBar.Position := AValue;
end;

constructor TPVScrollBar.Create(Parent: TWinControl);
begin
  Inc(Count);
  ScrollBar := TScrollBar.Create(Parent);
  ScrollBar.Kind := sbVertical;
  ScrollBar.Name := 'PVSB' + IntToStr(Count);
  ScrollBar.Align := alRight;
  ScrollBar.OnChange := ScrollBarChanged;
  ScrollBar.Parent := Parent;
end;

destructor TPVScrollBar.Destroy;
begin
  FreeAndNil(ScrollBar);
end;

end.
