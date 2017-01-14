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
    procedure ScrollBarChanged(Sender: TObject);
  public
    ScrollBar: TScrollBar;
    si: TScrollInfo;
    destructor Destroy; override;
    procedure Setup(TopSpacing, RightSpacing, BottomSpacing: integer; Parent: TWinControl);
    procedure Init(max, Page: integer);
    procedure SetPos(Pos: Integer);
    function Process(const m: TMessage): boolean;
    function ProcessMouseWheel(Delta: Integer) : boolean;
    procedure Show(Visible: boolean);
    procedure EndSB;
    procedure UpdateScrollBar;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;


implementation

const
  Count: integer = 0;

procedure TPVScrollBar.Setup(TopSpacing, RightSpacing, BottomSpacing: integer;
  Parent: TWinControl);
begin
  inc(Count);
  //{$IFDEF LINUX}
//  sb.Form := TForm.Create(nil);
//  sb.Form.SetBounds(x1 - 100, y0, 100, y1 - y0);
//  sb.Form.Name := 'PVSB' + IntToStr(Count);
  ScrollBar := TScrollBar.Create(Parent);
  ScrollBar.Kind := sbVertical;
  ScrollBar.Name := 'PVSB' + IntToStr(Count);
  ScrollBar.Parent := Parent;
  ScrollBar.BorderSpacing.Top := TopSpacing;
  ScrollBar.BorderSpacing.Right := RightSpacing;
  ScrollBar.BorderSpacing.Bottom := BottomSpacing;
  ScrollBar.Align := alRight;
  ScrollBar.OnChange := ScrollBarChanged;
  //sb.h := sb.ScrollBar.Handle;
  (*
  {$ENDIF}
  {$IFDEF WINDOWS}
  sb.h := CreateWindowEx(0, 'SCROLLBAR', pchar('PVSB' + IntToStr(Count)),
    SBS_VERT or WS_CHILD or SBS_RIGHTALIGN, x1 - 100, y0, 100, y1 - y0,
    Handle, 0, 0, nil);
  {$ENDIF}
  *)
  si.cbSize := 28;
end;

procedure TPVScrollBar.Init(max, Page: integer);
begin
  with si do begin
    nMin := 0;
    nMax := max;
    npos := 0;
    nPage := Page;
    FMask := SIF_PAGE or SIF_POS or SIF_RANGE;
  end;
  UpdateScrollBar;
  //SetScrollInfo(sb.ScrollBar.Handle, SB_CTL, sb.si, true);
  if max < Page then ScrollBar.Visible := False
    else ScrollBar.Visible := True;
end;

procedure TPVScrollBar.SetPos(Pos: Integer);
begin
  if Pos <> 0 then begin
    si.npos := Pos;
    si.FMask := SIF_POS;
    //SetScrollInfo(sb.ScrollBar.Handle, SB_CTL, sb.si, true);
    UpdateScrollBar;
  end;
end;

function TPVScrollBar.Process(const m: TMessage): boolean;
var
  NewPos: integer;
begin
  with si do
    if nMax < integer(nPage) then
      result := false
    else
    begin
      if (m.wParam and $ffff) in [SB_THUMBPOSITION, SB_THUMBTRACK] then
      begin
        result := ((m.wParam shr 16) and $ffff) <> npos;
        npos := (m.wParam shr 16) and $ffff;
      end
      else
      begin
        case (m.wParam and $ffff) of
          SB_LINEUP:
            NewPos := npos - 1;
          SB_LINEDOWN:
            NewPos := npos + 1;
          SB_PAGEUP:
            NewPos := npos - integer(nPage);
          SB_PAGEDOWN:
            NewPos := npos + integer(nPage);
        else
          NewPos := npos
        end;
        if NewPos < 0 then
          NewPos := 0;
        if NewPos > nMax - integer(nPage) + 1 then
          NewPos := nMax - integer(nPage) + 1;
        result := NewPos <> npos;
        if (NewPos <> npos) or ((m.wParam and $ffff) = SB_ENDSCROLL) then
        begin
          npos := NewPos;
          FMask := SIF_POS;
          UpdateScrollBar;
          //SetScrollInfo(sb.ScrollBar.Handle, SB_CTL, sb.si, true);
        end;
      end
    end
end;

function TPVScrollBar.ProcessMouseWheel(Delta: Integer
  ): boolean;
var
  NewPos: integer;
begin
  with si do
    if nMax < integer(nPage) then
      result := false
    else
    begin
      NewPos := npos - Delta div 300;
      if NewPos < 0 then
        NewPos := 0;
      if NewPos > nMax - integer(nPage) + 1 then
        NewPos := nMax - integer(nPage) + 1;
      result := NewPos <> npos;
      if NewPos <> npos then
      begin
        npos := NewPos;
        FMask := SIF_POS;
        UpdateScrollBar;
        //SetScrollInfo(sb.ScrollBar.Handle, SB_CTL, sb.si, true);
      end
    end
end;

procedure TPVScrollBar.Show(Visible: boolean);
begin
  if not Visible or (si.nMax < integer(si.nPage)) then
    ScrollBar.Visible := False
    else ScrollBar.Visible := True;
end;

procedure TPVScrollBar.EndSB;
begin
  with si do begin
    if nMax < integer(nPage) then
      npos := 0 // hidden
    else begin
      si.npos := nMax - integer(nPage) + 1;
      si.FMask := SIF_POS;
      UpdateScrollBar;
      //SetScrollInfo(sb.ScrollBar.Handle, SB_CTL, sb.si, true);
    end
  end
end;

procedure TPVScrollBar.UpdateScrollBar;
begin
  ScrollBar.Min := si.nMin;
  ScrollBar.Max := Max(si.nMax{$IFDEF LINUX} - si.nPage + 1{$ENDIF}, 0);
  ScrollBar.PageSize := si.nPage;
  ScrollBar.Position := si.nPos;
end;

{ TPVScrollbar }

procedure TPVScrollBar.ScrollBarChanged(Sender: TObject);
begin
  si.npos := ScrollBar.Position;
  if Assigned(FOnUpdate) then FOnUpdate(Self);
end;

destructor TPVScrollBar.Destroy;
begin
  //h := 0;
  si.cbSize := 0;
  FreeAndNil(ScrollBar);
end;

end.
