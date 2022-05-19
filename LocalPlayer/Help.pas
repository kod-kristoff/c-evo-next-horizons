{$INCLUDE Switches.inc}
unit Help;

interface

uses
  Protocol, ScreenTools, BaseWin, StringTables, Math, LCLIntf, LCLType,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  ButtonB, PVSB, Types, Generics.Collections, IsoEngine;

const
  MaxHist = 16;

  { link categories }
  hkNoLink = 0;
  hkAdv = 1;
  hkImp = 2;
  hkTer = 3;
  hkFeature = 4;
  hkInternet = 5;
  hkModel = 6;
  hkMisc = 7;
  hkCrossLink = $40;
  hkText = $80;

  liInvalid = $3FFF; // link index indicates invalid link

  { link indices for category hkMisc }
  miscMain = 0;
  miscCredits = 1;
  miscGovList = 2;
  miscJobList = 3;
  miscSearchResult = 7;

  fJungle = 8; // pseudo terrain

type

  { THyperText }

  THyperText = class(TStringList)
  public
    procedure AddLine(S: String = ''; Format: Integer = 0; Picpix: Integer = 0;
      LinkCategory: Integer = 0; LinkIndex: Integer = 0);
    procedure LineFeed;
    procedure AppendList(Source: THyperText);
    destructor Destroy; override;
  end;

  { THistItem }

  THistItem = class
    Kind: Integer;
    No: Integer;
    Pos: Integer;
    SearchContent: string;
    procedure Assign(Source: THistItem);
  end;

  { THistItems }

  THistItems = class(TObjectList<THistItem>)
    function AddNew(Kind, No, Pos: Integer; SearchContent: string): THistItem;
  end;

  { THelpDlg }

  THelpDlg = class(TFramedDlg)
    CloseBtn: TButtonB;
    BackBtn: TButtonB;
    TopBtn: TButtonB;
    SearchBtn: TButtonB;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BackBtnClick(Sender: TObject);
    procedure TopBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchBtnClick(Sender: TObject);
  protected
    procedure OffscreenPaint; override;
  private
    Kind: Integer;
    no: Integer;
    Sel: Integer;
    CaptionColor: Integer;
    hADVHELP, hIMPHELP, hFEATUREHELP, hGOVHELP, hSPECIALMODEL, hJOBHELP: Integer;
    SearchContent: string;
    NewSearchContent: string;
    CaptionFont: TFont;
    MainText: THyperText;
    SearchResult: THyperText;
    HelpText: TStringTable;
    ExtPic, TerrIcon: TBitmap;
    ScrollBar: TPVScrollbar;
    NoMap: TIsoMap;
    x0: array [-2..180] of Integer;
    procedure PaintTerrIcon(X, Y, xSrc, ySrc: Integer);
    procedure ScrollBarUpdate(Sender: TObject);
    procedure Line(ca: TCanvas; I: Integer; lit: Boolean);
    procedure Prepare(sbPos: Integer = 0);
    procedure ShowNewContentProcExecute(NewMode: TWindowMode; HelpContext: string);
    procedure WaterSign(x0, y0, iix: Integer);
    procedure Search(SearchString: string);
    procedure OnScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure OnMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  public
    HistItems: THistItems;
    Difficulty: Integer;
    procedure ClearHistory;
    procedure ShowNewContent(NewMode: TWindowMode; Category, Index: Integer);
    function TextIndex(Item: string): Integer;
  end;

var
  HelpDlg: THelpDlg;


implementation

uses
  Directories, ClientTools, Term, Tribes, Inp, Messg, UPixelPointer, Global,
  UKeyBindings;

{$R *.lfm}

type

  { THelpLineInfo }

  THelpLineInfo = class
    Format: Byte;
    Picpix: Byte;
    Link: Word;
    procedure Assign(Source: THelpLineInfo);
  end;

{ THelpLineInfo }

procedure THelpLineInfo.Assign(Source: THelpLineInfo);
begin
  Format := Source.Format;
  PicPix := Source.PicPix;
  Link := Source.Link;
end;

{ THistItem }

procedure THistItem.Assign(Source: THistItem);
begin
  Kind := Source.Kind;
  No := Source.No;
  Pos := Source.Pos;
  SearchContent := Source.SearchContent;
end;

{ THistItems }

function THistItems.AddNew(Kind, No, Pos: Integer; SearchContent: string
  ): THistItem;
begin
  Result := THistItem.Create;
  Result.Kind := Kind;
  Result.No := No;
  Result.Pos := Pos;
  Result.SearchContent := SearchContent;
  Add(Result);
end;

procedure THyperText.AddLine(S: String; Format: Integer; Picpix: Integer;
  LinkCategory: Integer; LinkIndex: Integer);
var
  HelpLineInfo: THelpLineInfo;
begin
  HelpLineInfo := THelpLineInfo.Create;
  if LinkIndex < 0 then
    LinkIndex := liInvalid;
  HelpLineInfo.Format := Format;
  HelpLineInfo.Picpix := Picpix;
  HelpLineInfo.Link := LinkCategory shl 8 + LinkIndex;
  AddObject(S, TObject(HelpLineInfo));
end;

procedure THyperText.LineFeed;
begin
  AddLine;
end;

procedure THyperText.AppendList(Source: THyperText);
var
  I: Integer;
  HelpLineInfo: THelpLineInfo;
begin
  for I := 0 to Source.Count - 1 do begin
    HelpLineInfo := THelpLineInfo.Create;
    HelpLineInfo.Assign(THelpLineInfo(Source.Objects[I]));
    AddObject(Source.Strings[I], HelpLineInfo);
  end;
end;

destructor THyperText.Destroy;
begin
  inherited;
end;

const
  { text formats }
  pkNormal = 0;
  pkCaption = 1;
  pkSmallIcon = 2;
  pkBigIcon = 3;
  pkAdvIcon = 4;
  pkTer = 5;
  pkBigTer = 6;
  pkFeature = 7;
  pkDot = 8;
  pkNormal_Dot = 9;
  pkDomain = 10;
  pkSection = 11;
  pkBigFeature = 12;
  pkExp = 13;
  pkAITStat = 14;
  pkExternal = 15;
  pkModel = 16;
  pkNormal_64 = 17;
  pkIllu = 18;
  pkLogo = 19;
  pkTerImp = 20;
  pkRightIcon = 21;
  pkAdvIcon_AsPreq = 22;
  pkSmallIcon_AsPreq = 23;
  pkSpecialIcon = 24;
  pkGov = 25;

  nSeeAlso = 14;
  SeeAlso: array [0 .. nSeeAlso - 1] of record
    Kind: Integer;
    no: Integer;
    SeeKind: Integer;
    SeeNo: Integer;
  end = ((Kind: hkImp; no: imWalls; SeeKind: hkFeature;
    SeeNo: mcArtillery), (Kind: hkImp; no: imHydro; SeeKind: hkImp;
    SeeNo: woHoover), (Kind: hkImp; no: imWalls; SeeKind: hkImp;
    SeeNo: imGrWall), (Kind: hkImp; no: imHighways; SeeKind: hkAdv;
    SeeNo: adWheel), (Kind: hkImp; no: imCathedral; SeeKind: hkImp;
    SeeNo: woBach), (Kind: hkImp; no: imBank; SeeKind: hkImp; SeeNo: imStockEx),
    (Kind: hkImp; no: imShipComp; SeeKind: hkImp; SeeNo: imSpacePort),
    (Kind: hkImp; no: imShipPow; SeeKind: hkImp; SeeNo: imSpacePort),
    (Kind: hkImp; no: imShipHab; SeeKind: hkImp; SeeNo: imSpacePort),
    (Kind: hkFeature; no: mcSub; SeeKind: hkFeature; SeeNo: mcRadar),
    (Kind: hkFeature; no: mcDefense; SeeKind: hkAdv; SeeNo: adSteel),
    (Kind: hkFeature; no: mcSE; SeeKind: hkFeature; SeeNo: mcNP), (Kind: hkAdv;
    no: adWheel; SeeKind: hkImp; SeeNo: imHighways), (Kind: hkAdv; no: adSteel;
    SeeKind: hkFeature; SeeNo: mcDefense));

  nTerrainHelp = 14;
  TerrainHelp: array [0 .. nTerrainHelp - 1] of Integer = (fGrass, fGrass + 12,
    fPrairie, fForest, fJungle, fHills, fMountains, fSwamp, fTundra, fArctic,
    fDesert, 3 * 12 { DeadLands } , fShore, fOcean);

  nJobHelp = 8;
  JobHelp: array [0 .. nJobHelp - 1] of Integer = (jRoad, jRR, jCanal, jIrr,
    jFarm, jMine, jFort, jBase);

procedure THelpDlg.FormCreate(Sender: TObject);
begin
  inherited;
  NoMap := TIsoMap.Create;

  HistItems := THistItems.Create;

  CaptionLeft := BackBtn.Left + BackBtn.Width;
  CaptionRight := SearchBtn.Left;
  Inc(ModalFrameIndent, 29);
  MainText := THyperText.Create;
  MainText.OwnsObjects := True;
  SearchResult := THyperText.Create;
  SearchResult.OwnsObjects := True;
  ScrollBar := TPVScrollbar.Create(Self);
  ScrollBar.SetBorderSpacing(36, 9, 11);
  ScrollBar.OnUpdate := ScrollBarUpdate;

  HelpText := TStringTable.Create;
  HelpText.LoadFromFile(LocalizedFilePath('Help' + DirectorySeparator + 'Help.txt'));
  hADVHELP := HelpText.Gethandle('ADVHELP');
  hIMPHELP := HelpText.Gethandle('IMPHELP');
  hFEATUREHELP := HelpText.Gethandle('FEATUREHELP');
  hGOVHELP := HelpText.Gethandle('GOVHELP');
  hSPECIALMODEL := HelpText.Gethandle('SPECIALMODEL');
  hJOBHELP := HelpText.Gethandle('JOBHELP');

  CaptionFont := Font.Create;
  CaptionFont.Assign(UniFont[ftNormal]);
  CaptionFont.Style := CaptionFont.Style + [fsItalic, fsBold];
  InitButtons;

  TopBtn.Hint := Phrases.Lookup('BTN_CONTENTS');
  BackBtn.Hint := Phrases.Lookup('BTN_BACK');
  SearchBtn.Hint := Phrases.Lookup('BTN_SEARCH');

  ExtPic := TBitmap.Create;
  TerrIcon := TBitmap.Create;
  TerrIcon.PixelFormat := pf24bit;
  TerrIcon.SetSize(xSizeBig, ySizeBig);
  TerrIcon.Canvas.FillRect(0, 0, TerrIcon.Width, TerrIcon.Height);
  SearchContent := '';
  ShowNewContentProc := ShowNewContentProcExecute;
end;

procedure THelpDlg.ShowNewContentProcExecute(NewMode: TWindowMode;
  HelpContext: string);
begin
  HelpDlg.ShowNewContent(NewMode, hkText,
    HelpDlg.TextIndex(HelpContext))
end;

procedure THelpDlg.FormDestroy(Sender: TObject);
begin
  ShowNewContentProc := nil;
  FreeAndNil(ScrollBar);
  FreeAndNil(MainText);
  FreeAndNil(SearchResult);
  FreeAndNil(ExtPic);
  FreeAndNil(TerrIcon);
  FreeAndNil(HelpText);
  // FreeAndNil(CaptionFont);
  FreeAndNil(HistItems);
  FreeAndNil(NoMap);
end;

procedure THelpDlg.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ScrollBar.ProcessMouseWheel(WheelDelta) then begin
    PaintBox1MouseMove(nil, [], MousePos.X - Left,
      MousePos.Y - Top);
  end;
end;

procedure THelpDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure THelpDlg.OnScroll(var Msg: TMessage);
begin
  { TODO: Handled by MouseWheel event
  if ScrollBar.Process(Msg) then begin
    Sel := -1;
    SmartUpdateContent(True)
  end;
  }
end;

procedure THelpDlg.OnMouseLeave(var Msg: TMessage);
begin
  if Sel <> -1 then begin
    Line(Canvas, Sel, False);
    Sel := -1
  end;
end;

procedure THelpDlg.ClearHistory;
begin
  HistItems.Clear;
end;

procedure THelpDlg.FormPaint(Sender: TObject);
begin
  inherited;
  Canvas.Font.Assign(UniFont[ftNormal]);
end;

procedure THelpDlg.Line(ca: TCanvas; I: Integer; lit: Boolean);
var
  TextColor, X, Y: Integer;
  TextSize: TSize;
  S: string;
begin
  S := MainText[ScrollBar.Position + I];
  if S = '' then
    Exit;
  X := x0[I];
  Y := 2 + I * 24;
  if ca = Canvas then
  begin
    X := X + SideFrame;
    Y := Y + WideFrame
  end;
  if THelpLineInfo(MainText.Objects[ScrollBar.Position + I]).Format
    in [pkCaption, pkBigTer, pkRightIcon, pkBigFeature] then
  begin
    ca.Font.Assign(CaptionFont);
    { ca.brush.color:=CaptionColor;
      ca.FillRect(rect(X,I*24,X+24,I*24+24));
      ca.Brush.Color:=$FFFFFF;
      ca.FrameRect(rect(X+1,I*24+1,X+24-1,I*24+24-1));
      ca.Brush.Style:=bsClear; }
    BitBltCanvas(ca, X, Y - 4, 24, 24, HGrSystem.Data.Canvas, 1,
      146);
    BiColorTextOut(ca, $FFFFFF, $7F007F, X + 10 - ca.Textwidth(S[1]) div 2,
      Y - 3, S[1]);
    BiColorTextOut(ca, CaptionColor, $7F007F, X + 24, Y - 3, Copy(S, 2, 255));
    ca.Font.Assign(UniFont[ftNormal]);
  end
  else if THelpLineInfo(MainText.Objects[ScrollBar.Position + I]).Format = pkSection
  then
  begin
    ca.Font.Assign(CaptionFont);
    BiColorTextOut(ca, CaptionColor, $7F007F, X, Y - 3, S);
    ca.Font.Assign(UniFont[ftNormal]);
  end
  else
  begin
    if (Kind = hkMisc) and (no = miscMain) then
      ca.Font.Assign(CaptionFont);
    TextColor := Colors.Canvas.Pixels[clkMisc, cliPaperText];
    if ca = Canvas then
    begin
      TextSize.cx := BiColorTextWidth(ca, S);
      TextSize.cy := ca.TextHeight(S);
      if Y + TextSize.cy >= WideFrame + InnerHeight then
        TextSize.cy := WideFrame + InnerHeight - Y;
      FillSeamless(ca, X, Y, TextSize.cx, TextSize.cy, -SideFrame,
        ScrollBar.Position * 24 - WideFrame, Paper);
    end;
    BiColorTextOut(ca, TextColor, $7F007F, X, Y, S);
    if lit then
      with ca do
      begin
        Assert(ca = Canvas);
        Pen.Color := TextColor;
        MoveTo(X + 1, Y + TextSize.cy - 2);
        LineTo(X + TextSize.cx, Y + TextSize.cy - 2);
      end;
    if (Kind = hkMisc) and (no = miscMain) then
      ca.Font.Assign(UniFont[ftNormal]);
  end;
end;

procedure THelpDlg.WaterSign(x0, y0, iix: Integer);
const
  nHeaven = 28;
  MaxSum = 9 * 9 * 255 * 75 div 100;
var
  X, Y, dx, dy, xSrc, ySrc, Sum, xx: Integer;
  Heaven: array [0..nHeaven] of Integer;
  PaintPtr: TPixelPointer;
  CoalPtr: TPixelPointer;
  ImpPtr: array [-1..1] of TPixelPointer;
begin
  // assume eiffel tower has free common heaven
  for dy := 0 to nHeaven - 1 do
    Heaven[dy] := BigImp.Canvas.Pixels[woEiffel mod 7 * xSizeBig,
      (SystemIconLines + woEiffel div 7) * ySizeBig + dy];

  BigImp.BeginUpdate;
  Offscreen.BeginUpdate;
  xSrc := iix mod 7 * xSizeBig;
  ySrc := (iix div 7 + 1) * ySizeBig;
  PaintPtr := PixelPointer(OffScreen, ScaleToNative(x0), ScaleToNative(y0));
  CoalPtr := PixelPointer(Templates.Data, ScaleToNative(xCoal), ScaleToNative(yCoal));
  for dy := -1 to 1 do
    ImpPtr[dy] := PixelPointer(BigImp, ScaleToNative(xSrc), ScaleToNative(ySrc));
  for Y := 0 to ScaleToNative(ySizeBig) * 2 - 1 do begin
    if ((ScaleToNative(y0) + Y) >= 0) and ((ScaleToNative(y0) + Y) < ScaleToNative(InnerHeight)) then begin
      for dy := -1 to 1 do
        if ((Max(Y + ScaleToNative(dy), 0) shr 1) >= 0) and ((Max(Y + ScaleToNative(dy), 0) shr 1) < ScaleToNative(ySizeBig)) then
          ImpPtr[dy].SetXY(0, Max(Y + ScaleToNative(dy), 0) shr 1);
      for X := 0 to ScaleToNative(xSizeBig * 2) - 1 do begin
        Sum := 0;
        for dx := -1 to 1 do begin
          xx := Max((X + ScaleToNative(dx)), 0) shr 1;
          for dy := -1 to 1 do begin
            ImpPtr[dy].SetX(xx);
            if ((Y + ScaleToNative(dy)) shr 1 < 0) or ((Y + ScaleToNative(dy)) shr 1 >= ScaleToNative(ySizeBig)) or
              ((X + ScaleToNative(dx)) shr 1 < 0) or ((X + ScaleToNative(dx)) shr 1 >= ScaleToNative(xSizeBig)) or
              ((Y + ScaleToNative(dy)) shr 1 < ScaleToNative(nHeaven)) and
              (ImpPtr[dy].Pixel^.B shl 16 + ImpPtr[dy].Pixel^.G shl 8 +
              ImpPtr[dy].Pixel^.R = Heaven[(ScaleFromNative(Y + ScaleToNative(dy))) shr 1]) then
              Sum := Sum + 9 * 255
            else
              Sum := Sum + ImpPtr[dy].Pixel^.B + 5 * ImpPtr[dy].Pixel^.G + 3 *
                ImpPtr[dy].Pixel^.R;
          end;
        end;
        if Sum < MaxSum then begin // no saturation
          Sum := 1 shl 22 - (MaxSum - Sum) * (256 - CoalPtr.Pixel^.B * 2);
          PaintPtr.Pixel^.B := Min(PaintPtr.Pixel^.B * Sum shr 22, 255);
          PaintPtr.Pixel^.G := Min(PaintPtr.Pixel^.G * Sum shr 22, 255);
          PaintPtr.Pixel^.R := Min(PaintPtr.Pixel^.R * Sum shr 22, 255);
        end;
        PaintPtr.NextPixel;
        CoalPtr.NextPixel;
      end;
    end;
    PaintPtr.NextLine;
    CoalPtr.NextLine;
  end;
  Offscreen.EndUpdate;
  BigImp.EndUpdate;
end;

procedure THelpDlg.PaintTerrIcon(X, Y, xSrc, ySrc: Integer);
begin
  with NoMap do begin
    Frame(OffScreen.Canvas, X - 1, Y - 1, X + xSizeBig, Y + ySizeBig,
      $000000, $000000);
    if 2 * yyt < 40 then begin
      Sprite(OffScreen, HGrTerrain, X, Y, 56, 2 * yyt, xSrc, ySrc);
      Sprite(OffScreen, HGrTerrain, X, Y + 2 * yyt, 56, 40 - 2 * yyt,
        xSrc, ySrc);
    end else
      Sprite(OffScreen, HGrTerrain, X, Y, 56, 40, xSrc, ySrc);
    Sprite(OffScreen, HGrTerrain, X, Y, xxt, yyt, xSrc + xxt, ySrc + yyt);
    Sprite(OffScreen, HGrTerrain, X, Y + yyt, xxt, 40 - yyt, xSrc + xxt, ySrc);
    Sprite(OffScreen, HGrTerrain, X + xxt, Y, 56 - xxt, yyt, xSrc, ySrc + yyt);
    Sprite(OffScreen, HGrTerrain, X + xxt, Y + yyt, 56 - xxt, 40 - yyt,
      xSrc, ySrc);
  end;
end;

procedure THelpDlg.OffscreenPaint;
var
  I, J, yl, srcno, ofs, cnt, Y: Integer;
  S: string;
  HelpLineInfo: THelpLineInfo;
begin
  inherited;
  CaptionColor := Colors.Canvas.Pixels[clkMisc, cliPaperCaption];
  FillSeamless(OffScreen.Canvas, 0, 0, InnerWidth, InnerHeight, 0,
    ScrollBar.Position * 24, Paper);
  with OffScreen.Canvas do
  begin
    Font.Assign(UniFont[ftNormal]);
    for I := -ScrollBar.Position to InnerHeight div 24 do
      if ScrollBar.Position + I < MainText.Count then
      begin
        HelpLineInfo := THelpLineInfo(MainText.Objects[ScrollBar.Position + I]);
        if HelpLineInfo.Format = pkExternal then
        begin
          yl := ExtPic.Height;
          if 4 + I * 24 + yl > InnerHeight then
            yl := InnerHeight - (4 + I * 24);
          BitBltCanvas(OffScreen.Canvas, 8, 4 + I * 24, ExtPic.Width, yl, ExtPic.Canvas,
            0, 0);
        end;
      end;
    for I := -2 to InnerHeight div 24 do
      if (ScrollBar.Position + I >= 0) and (ScrollBar.Position + I < MainText.Count) then
      begin
        HelpLineInfo := THelpLineInfo(MainText.Objects[ScrollBar.Position + I]);
        if HelpLineInfo.Link <> 0 then
        begin
          if (Kind = hkMisc) and (no = miscSearchResult) then
            Sprite(OffScreen, HGrSystem, 18, 9 + I * 24, 8, 8, 90, 16)
          else if HelpLineInfo.Format in [pkSmallIcon_AsPreq, pkAdvIcon_AsPreq]
          then
            Sprite(OffScreen, HGrSystem, 12, I * 24 + 5, 14, 14, 65, 20)
          else if HelpLineInfo.Link and (hkCrossLink shl 8) <> 0 then
            Sprite(OffScreen, HGrSystem, 12, I * 24 + 5, 14, 14, 80, 1)
          else if not((Kind = hkMisc) and (no = miscMain)) then
            Sprite(OffScreen, HGrSystem, 10, I * 24 + 6, 14, 14, 65, 1);
          x0[I] := 24;
        end
        else
          x0[I] := 0;
        case HelpLineInfo.Format of
          pkLogo:
            begin
              Server(sGetVersion, 0, 0, J);
              S := Format('%d.%d.%d', [J shr 16 and $FF, J shr 8 and $FF,
                J and $FF]);
              PaintLogo(OffScreen.Canvas, (InnerWidth - 122) div 2, I * 24 + 1,
                HGrSystem.Data.Canvas.Pixels[95, 1], $000000);
              Font.Assign(UniFont[ftSmall]);
              BiColorTextOut(OffScreen.Canvas, $000000, $7F007F,
                (InnerWidth - Textwidth(S)) div 2, I * 24 + 26, S);
              Font.Assign(UniFont[ftNormal]);
            end;
          pkSmallIcon, pkSmallIcon_AsPreq:
            begin
              ScreenTools.Frame(OffScreen.Canvas, 8 - 1 + x0[I], 2 - 1 + I * 24,
                8 + xSizeSmall + x0[I], 2 + 20 + I * 24, $000000, $000000);
              if HelpLineInfo.Picpix = imPalace then
                BitBltCanvas(OffScreen.Canvas, 8 + x0[I], 2 + I * 24,
                  xSizeSmall, ySizeSmall, SmallImp.Canvas,
                  0 * xSizeSmall, 1 * ySizeSmall)
              else
                BitBltCanvas(OffScreen.Canvas, 8 + x0[I], 2 + I * 24,
                  xSizeSmall, ySizeSmall, SmallImp.Canvas,
                  HelpLineInfo.Picpix mod 7 * xSizeSmall,
                  (HelpLineInfo.Picpix + SystemIconLines * 7) div 7 *
                  ySizeSmall);
              x0[I] := x0[I] + (8 + 8 + 36);
            end;
          pkBigIcon:
            begin
              FrameImage(OffScreen.Canvas, BigImp, x0[I] + 12, I * 24 - 7, 56,
                40, HelpLineInfo.Picpix mod 7 * xSizeBig,
                HelpLineInfo.Picpix div 7 * ySizeBig);
              x0[I] := 64 + 8 + 8 + x0[I];
            end;
          pkSpecialIcon:
            begin
              case HelpLineInfo.Picpix of
                0:
                  FrameImage(OffScreen.Canvas, HGrSystem2.Data,
                    12 + x0[I], -7 + I * 24, 56, 40, 137, 127);
                1:
                  with NoMap do begin
                    PaintTerrIcon(12 + x0[I], -7 + I * 24,
                      1 + 3 * (xxt * 2 + 1), 1 + yyt);
                    if 2 * yyt < 40 then
                      Sprite(OffScreen, HGrTerrain, 12 + x0[I], -7 + 4 + I * 24,
                        56, 2 * yyt, 1 + 3 * (xxt * 2 + 1) + xxt - 28,
                        1 + yyt + 1 * (yyt * 3 + 1))
                    else
                      Sprite(OffScreen, HGrTerrain, 12 + x0[I],
                        -7 + 4 + I * 24 - 4, 56, 40, 1 + 3 * (xxt * 2 + 1) + xxt
                        - 28, 1 + yyt + 1 * (yyt * 3 + 1) + yyt - 20);
                  end;
                2:
                  with NoMap do begin
                    PaintTerrIcon(12 + x0[I], -7 + I * 24,
                      1 + 7 * (xxt * 2 + 1), 1 + yyt + 4 * (yyt * 3 + 1));
                    if 2 * yyt < 40 then
                      Sprite(OffScreen, HGrTerrain, 12 + x0[I], -7 + 4 + I * 24,
                        56, 32, 1 + 4 * (xxt * 2 + 1) + xxt - 28,
                        1 + yyt + 12 * (yyt * 3 + 1) + yyt - 16)
                    else
                      Sprite(OffScreen, HGrTerrain, 12 + x0[I], -7 + 4 + I * 24,
                        56, 32, 1 + 4 * (xxt * 2 + 1) + xxt - 28,
                        1 + yyt + 12 * (yyt * 3 + 1) + yyt - 16)
                  end;
              end;
              x0[I] := 64 + 8 + 8 + x0[I];
            end;
          pkDomain:
            begin
              ScreenTools.Frame(OffScreen.Canvas, 8 - 1 + x0[I], 2 - 1 + I * 24,
                8 + 36 + x0[I], 2 + 20 + I * 24, $000000, $000000);
              Dump(OffScreen, HGrSystem, 8 + x0[I], 2 + I * 24, 36, 20,
                75 + HelpLineInfo.Picpix * 37, 295);
              x0[I] := x0[I] + (8 + 8 + 36);
            end;
          pkAdvIcon, pkAdvIcon_AsPreq:
            begin
              ScreenTools.Frame(OffScreen.Canvas, 8 - 1 + x0[I], 2 - 1 + I * 24,
                8 + xSizeSmall + x0[I], 2 + ySizeSmall + I * 24,
                $000000, $000000);
              if AdvIcon[HelpLineInfo.Picpix] < 84 then
                BitBltCanvas(OffScreen.Canvas, 8 + x0[I], 2 + I * 24,
                  xSizeSmall, ySizeSmall, SmallImp.Canvas,
                  (AdvIcon[HelpLineInfo.Picpix] + SystemIconLines * 7) mod 7 *
                  xSizeSmall, (AdvIcon[HelpLineInfo.Picpix] + SystemIconLines *
                  7) div 7 * ySizeSmall)
              else
                Dump(OffScreen, HGrSystem, 8 + x0[I], 2 + I * 24, 36, 20,
                  1 + (AdvIcon[HelpLineInfo.Picpix] - 84) mod 8 * 37,
                  295 + (AdvIcon[HelpLineInfo.Picpix] - 84) div 8 * 21);
              J := AdvValue[HelpLineInfo.Picpix] div 1000;
              BitBltCanvas(OffScreen.Canvas, x0[I] + 4, 4 + I * 24, 14, 14,
                HGrSystem.Mask.Canvas, 127 + J * 15, 85, SRCAND);
              Sprite(OffScreen, HGrSystem, x0[I] + 3, 3 + I * 24, 14, 14,
                127 + J * 15, 85);
              x0[I] := x0[I] + (8 + 8 + 36);
            end;
          pkRightIcon:
            begin
              if Imp[HelpLineInfo.Picpix].Kind <> ikWonder then
                ImpImage(OffScreen.Canvas, InnerWidth - (40 + xSizeBig), I * 24,
                  HelpLineInfo.Picpix, gDespotism)
              else
                WaterSign(InnerWidth - (40 + 2 * xSizeBig), I * 24 - 8,
                  HelpLineInfo.Picpix + 7);
              x0[I] := x0[I] + 8;
            end;
          pkIllu:
            WaterSign(8, I * 24 - 8, HelpLineInfo.Picpix);
          pkBigFeature:
            begin
              cnt := 0;
              for J := nDomains - 1 downto 0 do
                if 1 shl J and Feature[HelpLineInfo.Picpix].Domains <> 0 then
                begin
                  Inc(cnt);
                  Dump(OffScreen, HGrSystem, InnerWidth - 38 - 38 * cnt,
                    I * 24 + 1, 36, 20, 75 + J * 37, 295);
                  ScreenTools.Frame(OffScreen.Canvas, InnerWidth - 39 - 38 * cnt, I * 24,
                    InnerWidth - 2 - 38 * cnt, I * 24 + 21, $000000, $000000);
                end;
              DarkGradient(OffScreen.Canvas, InnerWidth - 38 - 38 * cnt,
                I * 24 + 23, cnt * 38 - 2, 1);
              ofs := InnerWidth - (39 + 7) - 19 * cnt;
              with OffScreen.Canvas do
              begin
                Brush.Color := $C0C0C0;
                FrameRect(Rect(ofs, 1 + 23 + I * 24, ofs + 14,
                  15 + 23 + I * 24));
                Brush.Style := bsClear;
                Sprite(OffScreen, HGrSystem, ofs + 2, 3 + 23 + I * 24, 10, 10,
                  66 + HelpLineInfo.Picpix mod 11 * 11,
                  137 + HelpLineInfo.Picpix div 11 * 11);
              end;
              x0[I] := x0[I] + 8;
            end;
          pkTer, pkBigTer:
            with NoMap do begin
              if HelpLineInfo.Format = pkBigTer then
                Y := I * 24 - 3 + yyt
              else
                Y := I * 24 + 13;
              if HelpLineInfo.Picpix >= 3 * 12 then
                srcno := 2 * 9 + 6
              else if HelpLineInfo.Picpix mod 12 = fJungle then
                srcno := 18 * 9
              else if HelpLineInfo.Picpix mod 12 < fJungle then
                srcno := HelpLineInfo.Picpix mod 12
              else
                srcno := 27 + (HelpLineInfo.Picpix mod 12 - 9) * 18;
              if HelpLineInfo.Format = pkTer then
              begin
                ofs := x0[I] + 8;
                x0[I] := 2 * xxt + 8 + ofs;
              end
              else
              begin
                ofs := InnerWidth - (2 * xxt + 38);
                x0[I] := x0[I] + 8;
              end;
              if srcno >= fJungle then
              begin
                Sprite(OffScreen, HGrTerrain, ofs + 4, Y - yyt + 2, xxt * 2 - 8,
                  yyt * 2 - 4, 5 + 2 * (xxt * 2 + 1),
                  3 + yyt + 2 * (yyt * 3 + 1));
                Sprite(OffScreen, HGrTerrain, ofs, Y - 2 * yyt, xxt * 2,
                  yyt * 3 - 2, 1 + srcno mod 9 * (xxt * 2 + 1),
                  1 + srcno div 9 * (yyt * 3 + 1));
              end
              else
                Sprite(OffScreen, HGrTerrain, ofs + 4, Y - yyt + 2, xxt * 2 - 8,
                  yyt * 2 - 4, 5 + srcno mod 9 * (xxt * 2 + 1),
                  3 + yyt + srcno div 9 * (yyt * 3 + 1));
              if HelpLineInfo.Picpix >= 3 * 12 then { rare resource }
                Sprite(OffScreen, HGrTerrain, ofs, Y - 2 * yyt, xxt * 2,
                  yyt * 3, 1 + 8 * (xxt * 2 + 1),
                  1 + (HelpLineInfo.Picpix - 2 * 12) * (yyt * 3 + 1))
              else if HelpLineInfo.Picpix >= 12 then { special tile }
              begin
                if HelpLineInfo.Picpix mod 12 = fJungle then
                  srcno := 17 * 9 + 8
                else if HelpLineInfo.Picpix mod 12 < fJungle then
                  srcno := HelpLineInfo.Picpix mod 12
                else
                  srcno := 18 + 8 + (HelpLineInfo.Picpix mod 12 - 9) * 18;
                srcno := srcno + HelpLineInfo.Picpix div 12 * 9;
                Sprite(OffScreen, HGrTerrain, ofs, Y - 2 * yyt, xxt * 2,
                  yyt * 3, 1 + srcno mod 9 * (xxt * 2 + 1),
                  1 + srcno div 9 * (yyt * 3 + 1));
              end;
            end;
          pkTerImp:
            with NoMap do begin
              ofs := 8;
              if HelpLineInfo.Picpix = 5 then
              begin // display mine on hills
                Sprite(OffScreen, HGrTerrain, ofs + 4, I * 24 + 13 - yyt,
                  xxt * 2 - 8, yyt * 2 - 4, 5 + 2 * (xxt * 2 + 1),
                  3 + yyt + 2 * (yyt * 3 + 1));
                srcno := 45;
              end
              else
                srcno := fPrairie; // display on prairie
              Sprite(OffScreen, HGrTerrain, ofs + 4, I * 24 + 13 - yyt,
                xxt * 2 - 8, yyt * 2 - 4, 5 + srcno mod 9 * (xxt * 2 + 1),
                3 + yyt + srcno div 9 * (yyt * 3 + 1));
              if HelpLineInfo.Picpix = 12 then { river }
                Sprite(OffScreen, HGrTerrain, ofs, I * 24 + 11 - yyt, xxt * 2,
                  yyt * 2, 1 + 5 * (xxt * 2 + 1), 1 + yyt + 13 * (yyt * 3 + 1))
              else if HelpLineInfo.Picpix >= 3 then { improvement 2 }
              begin
                if HelpLineInfo.Picpix = 6 then
                  Sprite(OffScreen, HGrTerrain, ofs, I * 24 + 11 - 2 * yyt,
                    xxt * 2, yyt * 3, 1 + 7 * (xxt * 2 + 1),
                    1 + 12 * (yyt * 3 + 1));
                Sprite(OffScreen, HGrTerrain, ofs, I * 24 + 11 - 2 * yyt,
                  xxt * 2, yyt * 3, 1 + (HelpLineInfo.Picpix - 3) *
                  (xxt * 2 + 1), 1 + 12 * (yyt * 3 + 1))
              end
              else { improvement 1 }
              begin
                Sprite(OffScreen, HGrTerrain, ofs, I * 24 + 11 - 2 * yyt,
                  xxt * 2, yyt * 3, 1 + 2 * (xxt * 2 + 1),
                  1 + (9 + HelpLineInfo.Picpix) * (yyt * 3 + 1));
                Sprite(OffScreen, HGrTerrain, ofs, I * 24 + 11 - 2 * yyt,
                  xxt * 2, yyt * 3, 1 + 5 * (xxt * 2 + 1),
                  1 + (9 + HelpLineInfo.Picpix) * (yyt * 3 + 1))
              end;
              x0[I] := x0[I] + 8;
            end;
          pkModel:
            begin
              FrameImage(OffScreen.Canvas, BigImp, x0[I] + 12, I * 24 - 7,
                56, 40, 0, 0);
              Sprite(OffScreen, HGrStdUnits, x0[I] + 8, I * 24 - 11, 64, 44,
                1 + HelpLineInfo.Picpix mod 10 * 65,
                1 + HelpLineInfo.Picpix div 10 * 49);
              x0[I] := 64 + 8 + 8 + x0[I];
            end;
          pkFeature:
            begin
              DarkGradient(OffScreen.Canvas, x0[I] + 8 - 1,
                7 + I * 24 - 3, 16, 1);
              ScreenTools.Frame(OffScreen.Canvas, x0[I] + 8, 7 + I * 24 - 2, x0[I] + 8 + 13,
                7 + I * 24 - 2 + 13, $C0C0C0, $C0C0C0);
              Sprite(OffScreen, HGrSystem, x0[I] + 8 + 2, 7 + I * 24, 10, 10,
                66 + HelpLineInfo.Picpix mod 11 * 11,
                137 + HelpLineInfo.Picpix div 11 * 11);
              x0[I] := x0[I] + 8 + 8 + 2 + 13;
            end;
          pkExp:
            begin
              ScreenTools.Frame(OffScreen.Canvas, 20 - 1, 8 - 4 + I * 24, 20 + 12,
                8 + 11 + I * 24, $000000, $000000);
              Dump(OffScreen, HGrSystem, 20, 8 - 3 + I * 24, 12, 14,
                121 + HelpLineInfo.Picpix * 13, 28);
              x0[I] := 20 + 8 + 11;
            end;
          pkAITStat:
            begin
              Sprite(OffScreen, HGrSystem, 20, 6 + I * 24, 14, 14,
                1 + HelpLineInfo.Picpix * 15, 316);
              x0[I] := 20 + 8 + 11;
            end;
          pkGov:
            begin
              ScreenTools.Frame(OffScreen.Canvas, 8 - 1 + x0[I], 2 - 1 + I * 24,
                8 + xSizeSmall + x0[I], 2 + 20 + I * 24, $000000, $000000);
              BitBltCanvas(OffScreen.Canvas, 8 + x0[I], 2 + I * 24, xSizeSmall,
                ySizeSmall, SmallImp.Canvas, (HelpLineInfo.Picpix - 1) *
                xSizeSmall, ySizeSmall);
              x0[I] := x0[I] + (8 + 8 + 36);
            end;
          pkDot:
            begin
              Sprite(OffScreen, HGrSystem, x0[I] + 18, 9 + I * 24, 8,
                8, 81, 16);
              x0[I] := 20 + 8 + 4;
            end;
          pkNormal_Dot:
            x0[I] := 20 + 8 + 4;
          pkNormal_64:
            x0[I] := 64 + 8 + 8;
        else
          x0[I] := x0[I] + 8;
        end;
        Self.Line(OffScreen.Canvas, I, False)
      end;
  end;
  MarkUsedOffscreen(InnerWidth, InnerHeight + 13 + 48);
end;

procedure THelpDlg.ScrollBarUpdate(Sender: TObject);
begin
  Sel := -1;
  SmartUpdateContent(True)
end;

procedure THelpDlg.Prepare(sbPos: Integer = 0);
var
  I, J, Special, Domain, Headline, TerrType, TerrSubType: Integer;
  S: string;
  ps: PChar;
  List: THyperText;
  CheckSeeAlso: Boolean;

  procedure AddAdvance(I: Integer);
  begin
    MainText.AddLine(Phrases.Lookup('ADVANCES', I), pkAdvIcon, I,
      hkAdv + hkCrossLink, I);
  end;

  procedure AddPreqAdv(I: Integer);
  begin
    MainText.AddLine(Phrases.Lookup('ADVANCES', I), pkAdvIcon_AsPreq, I,
      hkAdv + hkCrossLink, I);
  end;

  procedure AddImprovement(I: Integer);
  begin
    MainText.AddLine(Phrases.Lookup('IMPROVEMENTS', I), pkSmallIcon, I,
      hkImp + hkCrossLink, I);
  end;

  procedure AddPreqImp(I: Integer);
  begin
    MainText.AddLine(Phrases.Lookup('IMPROVEMENTS', I), pkSmallIcon_AsPreq, I,
      hkImp + hkCrossLink, I);
  end;

  procedure AddTerrain(I: Integer);
  begin
    if MainText.Count > 1 then
    begin
      MainText.LineFeed;
    end;
    MainText.AddLine(Phrases.Lookup('TERRAIN', I), pkTer, I, hkTer, I);
  end;

  procedure AddFeature(I: Integer);
  begin
    MainText.AddLine(Phrases.Lookup('FEATURES', I), pkFeature, I,
      hkFeature + hkCrossLink, I);
  end;

  procedure AddModel(I: Integer);
  var
    pix: Integer;
    Name: string;
  begin
    if MainText.Count > 1 then
      MainText.LineFeed;
    FindStdModelPicture(SpecialModelPictureCode[I], pix, Name);
    MainText.AddLine(Name, pkModel, pix, hkModel + hkCrossLink, I)
  end;

  procedure AddStandardBlock(Item: string);
  var
    I: Integer;
  begin
    with MainText do
    begin
      if Item = 'LOGO' then
      begin
        AddLine('', pkLogo);
        LineFeed;
      end
      else if Item = 'TECHFORMULA' then
      begin
        I := Difficulty;
        if I = 0 then
          I := 2;
        AddLine(Format(HelpText.Lookup('TECHFORMULA'), [TechFormula_M[I],
          TechFormula_D[I]]))
      end
      else if Item = 'EXPERIENCE' then
        for I := 0 to nExp - 1 do
          AddLine(Phrases.Lookup('EXPERIENCE', I), pkExp, I)
      else if Item = 'MODERN' then
        for I := 1 to 3 do
        begin
          LineFeed;
          AddLine(Phrases.Lookup('TERRAIN', 3 * 12 + I), pkTer, 3 * 12 + I);
        end
      else if Item = 'SAVED' then
        AddLine(DataDir + 'Saved', pkNormal)
      else if Item = 'AITSTAT' then
        for I := 0 to 3 do
          AddLine(Phrases2.Lookup('AITSTAT', I), pkAITStat, I)
    end
  end;

  procedure DecodeItem(S: string; var Category, Index: Integer);
  var
    I: Integer;
  begin
    if (Length(S) > 0) and (S[1] = ':') then begin
      Category := hkMisc;
      Index := 0;
      for I := 3 to Length(S) do
        Index := Index * 10 + Ord(S[I]) - 48;
      case S[2] of
        'A': Category := hkAdv;
        'B': Category := hkImp;
        'T': Category := hkTer;
        'F': Category := hkFeature;
        'E': Category := hkInternet;
        'S': Category := hkModel;
        'C': Index := miscCredits;
        'J': Index := miscJobList;
        'G': Index := miscGovList;
      end;
      if (Category <> hkMisc) and (Index = 0) then
        Index := 200;
    end else begin
      Category := hkText;
      Index := HelpText.Gethandle(Copy(S, 1, 255));
    end;
  end;

  procedure AddTextual(S: string);
  var
    I: Integer;
    P: Integer;
    L: Integer;
    ofs: Integer;
    CurrentFormat: Integer;
    FollowFormat: Integer;
    Picpix: Integer;
    LinkCategory: Integer;
    LinkIndex: Integer;
    RightMargin: Integer;
    Name: string;
    Text: string;
  begin
    RightMargin := InnerWidth - 16 - GetSystemMetrics(SM_CXVSCROLL);
    FollowFormat := pkNormal;
    while S <> '' do
    begin
      Picpix := 0;
      LinkCategory := 0;
      LinkIndex := 0;
      if S[1] = '$' then
      begin // window caption
        P := 1;
        repeat
          Inc(P);
        until (P > Length(S)) or (S[P] = '\');
        Caption := Copy(S, 2, P - 2);
        Delete(S, 1, P);
      end
      else if S[1] = '&' then
      begin // standard block
        P := 1;
        repeat
          Inc(P);
        until (P > Length(S)) or (S[P] = '\');
        AddStandardBlock(Copy(S, 2, P - 2));
        Delete(S, 1, P);
      end
      else if S[1] = '@' then
      begin // image
        if (Length(S) >= 2) and (S[2] = '@') then
        begin // generate from icon
          Picpix := 0;
          P := 3;
          while (P <= Length(S)) and (S[P] <> '\') do
          begin
            Picpix := Picpix * 10 + Ord(S[P]) - 48;
            Inc(P);
          end;
          if (Picpix < 0) or (Picpix >= nImp) then
            Picpix := 0;
          MainText.AddLine('', pkIllu, Picpix);
          MainText.LineFeed;
          MainText.LineFeed;
        end
        else
        begin // external image
          P := 1;
          repeat
            Inc(P);
          until (P > Length(S)) or (S[P] = '\');
          if LoadGraphicFile(ExtPic, LocalizedFilePath('Help' +
            DirectorySeparator + Copy(S, 2, P - 2)) + '.png') then
          begin
            MainText.AddLine('', pkExternal);
            for I := 0 to (ExtPic.Height - 12) div 24 do
              MainText.LineFeed;
          end;
        end;
        Delete(S, 1, P);
      end
      else
      begin
        case S[1] of
          ':', ';':
            begin // link
              P := 1;
              repeat
                Inc(P)
              until (P > Length(S)) or (S[P] = '\') or (S[P] = ' ');
              DecodeItem(Copy(S, 2, P - 2), LinkCategory, LinkIndex);
              CurrentFormat := 0;
              if (LinkCategory <> hkText) and (LinkIndex < 200) then
              // show icon
                case LinkCategory of
                  hkAdv:
                    begin
                      CurrentFormat := pkAdvIcon;
                      Picpix := LinkIndex
                    end;
                  hkImp:
                    begin
                      CurrentFormat := pkSmallIcon;
                      Picpix := LinkIndex
                    end;
                  hkTer:
                    begin
                      CurrentFormat := pkTer;
                      Picpix := LinkIndex;
                    end;
                  hkFeature:
                    begin
                      CurrentFormat := pkFeature;
                      Picpix := LinkIndex
                    end;
                  hkModel:
                    begin
                      CurrentFormat := pkModel;
                      FindStdModelPicture(SpecialModelPictureCode[LinkIndex],
                        Picpix, Name);
                    end;
                end;
              if S[1] = ':' then
                LinkCategory := LinkCategory + hkCrossLink;
              if (P > Length(S)) or (S[P] = ' ') then
                Delete(S, 1, P)
              else
                Delete(S, 1, P - 1)
            end;
          '!': // highlited
            if (Length(S) >= 2) and (S[2] = '!') then
            begin
              if MainText.Count > 1 then
                MainText.LineFeed;
              FollowFormat := pkCaption;
              CurrentFormat := pkCaption;
              Delete(S, 1, 2);
            end
            else
            begin
              FollowFormat := pkSection;
              CurrentFormat := pkSection;
              Delete(S, 1, 1);
            end;
          '-':
            begin // list
              FollowFormat := pkNormal_Dot;
              CurrentFormat := pkDot;
              Delete(S, 1, 1);
            end;
        else
          CurrentFormat := FollowFormat;
        end;
        if FollowFormat = pkNormal_Dot then
          ofs := 20 + 4 + 8
        else
          ofs := 8;
        P := 0;
        repeat
          repeat
            Inc(P)
          until (P > Length(S)) or (S[P] = ' ') or (S[P] = '\');
          if (BiColorTextWidth(OffScreen.Canvas, Copy(S, 1, P - 1)) <=
            RightMargin - ofs) then
            L := P - 1
          else
            Break;
        until (P >= Length(S)) or (S[L + 1] = '\');
        Text := Copy(S, 1, L);
        if LinkCategory and $3F = hkInternet then begin
          if LinkIndex = 1 then Text := AITemplateManual
          else if LinkIndex = 2 then Text := CevoHomepageShort
          else if LinkIndex = 3 then Text := CevoContactShort;
        end;
        MainText.AddLine(Text, CurrentFormat, Picpix, LinkCategory,
          LinkIndex);
        if (L < Length(S)) and (S[L + 1] = '\') then
          FollowFormat := pkNormal;
        Delete(S, 1, L + 1);
      end
    end
  end;

  procedure AddItem(Item: string);
  begin
    AddTextual(HelpText.Lookup(Item));
  end;

  procedure AddModelText(I: Integer);
  var
    pix: Integer;
    S: string;
  begin
    with MainText do begin
      if Count > 1 then begin
        LineFeed;
        LineFeed;
      end;
      FindStdModelPicture(SpecialModelPictureCode[I], pix, S);
      AddLine(S, pkSection);
      AddLine(Format(HelpText.Lookup('STRENGTH'), [SpecialModel[I].Attack,
        SpecialModel[I].Defense]), pkNormal_64);
      AddLine(Format(HelpText.Lookup('SPEED'),
        [MovementToString(SpecialModel[I].Speed)]), pkModel, pix);
      if Difficulty = 0 then
        AddLine(Format(HelpText.Lookup('BUILDCOST'), [SpecialModel[I].Cost]),
          pkNormal_64)
      else
        AddLine(Format(HelpText.Lookup('BUILDCOST'),
          [SpecialModel[I].Cost * BuildCostMod[Difficulty] div 12]),
          pkNormal_64);
      S := HelpText.LookupByHandle(hSPECIALMODEL, I);
      if (S <> '') and (S <> '*') then
        AddTextual(S);
      if SpecialModelPreq[I] >= 0 then
        AddPreqAdv(SpecialModelPreq[I])
      else if SpecialModelPreq[I] = preLighthouse then
        AddPreqImp(woLighthouse)
      else if SpecialModelPreq[I] = preBuilder then
        AddPreqImp(woPyramids)
      else if SpecialModelPreq[I] = preLeo then
        AddPreqImp(woLeo);
      if SpecialModelPreq[I] <> preNone then
        MainText[Count - 1] := Format(HelpText.Lookup('REQUIRED'),
          [MainText[Count - 1]]);
    end
  end;

  procedure AddJobList;
  var
    I, JobCost: Integer;
  begin
    with MainText do begin
      for I := 0 to nJobHelp - 1 do begin
        if I > 0 then begin
          LineFeed;
          LineFeed;
        end;
        AddLine(Phrases.Lookup('JOBRESULT', JobHelp[I]), pkSection);
        AddLine;
        AddLine('', pkTerImp, I);
        AddLine;
        AddTextual(HelpText.LookupByHandle(hJOBHELP, I));
        JobCost := -1;
        case JobHelp[I] of
          jCanal: JobCost := CanalWork;
          jFort: JobCost := FortWork;
          jBase: JobCost := BaseWork;
        end;
        if JobCost >= 0 then
          AddTextual(Format(HelpText.Lookup('JOBCOST'),
            [MovementToString(JobCost)]))
        else
          AddTextual(HelpText.Lookup('JOBCOSTVAR'));
        if JobPreq[JobHelp[I]] <> preNone then begin
          AddPreqAdv(JobPreq[JobHelp[I]]);
          MainText[Count - 1] := Format(HelpText.Lookup('REQUIRED'),
            [MainText[Count - 1]]);
        end;
      end;
    end;
  end;

  procedure AddGraphicCredits;
  var
    I: Integer;
    S: string;
    sr: TSearchRec;
    List, Plus: TStringList;
  begin
    List := TStringList.Create;
    Plus := TStringList.Create;
    if FindFirst(GetGraphicsDir + DirectorySeparator + '*.credits.txt', $27, sr) = 0 then
      repeat
        Plus.LoadFromFile(GetGraphicsDir + DirectorySeparator + sr.Name);
        List.AddStrings(Plus);
      until FindNext(sr) <> 0;
    FindClose(sr);
    FreeAndNil(Plus);

    List.Sort;
    I := 1;
    while I < List.Count do
      if List[I] = List[I - 1] then
        List.Delete(I)
      else
        Inc(I);

    for I := 0 to List.Count - 1 do begin
      S := List[I];
      while BiColorTextWidth(OffScreen.Canvas, S) > InnerWidth - 16 -
        GetSystemMetrics(SM_CXVSCROLL) do
        Delete(S, Length(S), 1);
      MainText.AddLine(S);
    end;
    FreeAndNil(List);
  end;

  procedure AddSoundCredits;
  var
    I: Integer;
    S: string;
    List: TStringList;
  begin
    List := TStringList.Create;
    List.LoadFromFile(GetSoundsDir + DirectorySeparator + 'sound.credits.txt');
    for I := 0 to List.Count - 1 do begin
      S := List[I];
      while BiColorTextWidth(OffScreen.Canvas, S) > InnerWidth - 16 -
        GetSystemMetrics(SM_CXVSCROLL) do
        Delete(S, Length(S), 1);
      MainText.AddLine(S);
    end;
    FreeAndNil(List);
  end;

  procedure NextSection(Item: string);
  begin
    if MainText.Count > 1 then
      if MainText.Count = Headline + 1 then
        MainText.Delete(Headline)
      else
        MainText.LineFeed;
    MainText.AddLine(HelpText.Lookup(Item), pkSection);
    Headline := MainText.Count - 1;
  end;

begin { Prepare }
  with MainText do begin
    OffScreen.Canvas.Font.Assign(UniFont[ftNormal]);
    CheckSeeAlso := False;
    Clear;
    Headline := -1;
    if (no >= 200) or not(Kind in [hkAdv, hkImp, hkTer, hkFeature]) then
      LineFeed;
    case Kind of
      hkText:
        AddTextual(HelpText.LookupByHandle(no));
      hkMisc:
        begin
          case no of
            miscMain:
              begin
                Caption := HelpText.Lookup('HELPTITLE_MAIN');
                AddLine(HelpText.Lookup('HELPTITLE_QUICKSTART'), pkSpecialIcon,
                  0, { pkBigIcon,22, } hkText, HelpText.Gethandle('QUICK'));
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_CONCEPTS'), pkBigIcon, 6,
                  hkText, HelpText.Gethandle('CONCEPTS'));
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_TERLIST'), pkSpecialIcon, 1,
                  hkTer, 200);
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_JOBLIST'), pkSpecialIcon, 2,
                  hkMisc, miscJobList);
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_TECHLIST'), pkBigIcon, 39,
                  hkAdv, 200);
                LineFeed;
                FindStdModelPicture(SpecialModelPictureCode[6], I, S);
                AddLine(HelpText.Lookup('HELPTITLE_MODELLIST'), pkModel, I,
                  hkModel, 0);
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_FEATURELIST'), pkBigIcon, 28,
                  hkFeature, 200);
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_IMPLIST'), pkBigIcon,
                  7 * SystemIconLines + imCourt, hkImp, 200);
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_UNIQUELIST'), pkBigIcon,
                  7 * SystemIconLines + imStockEx, hkImp, 201);
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_WONDERLIST'), pkBigIcon,
                  7 * SystemIconLines, hkImp, 202);
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_GOVLIST'), pkBigIcon,
                  gDemocracy + 6, hkMisc, miscGovList);
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_KEYS'), pkBigIcon, 2, hkText,
                  HelpText.Gethandle('HOTKEYS'));
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_ABOUT'), pkBigIcon, 1,
                  hkText, HelpText.Gethandle('ABOUT'));
                LineFeed;
                AddLine(HelpText.Lookup('HELPTITLE_CREDITS'), pkBigIcon, 22,
                  hkMisc, miscCredits);
              end;
            miscCredits:
              begin
                AddItem('CREDITS');
                LineFeed;
                AddGraphicCredits;
                NextSection('CRED_CAPSOUND');
                AddSoundCredits;
                NextSection('CRED_CAPAI');
                Server(sGetAICredits, 0, 0, ps);
                AddTextual(ps);
                NextSection('CRED_CAPLANG');
                AddItem('AUTHOR');
              end;
            miscJobList:
              begin
                Caption := HelpText.Lookup('HELPTITLE_JOBLIST');
                AddJobList;
                LineFeed;
                AddItem('TERIMPEXCLUDE');
                LineFeed;
                AddItem('TERIMPCITY');
              end;
            miscGovList:
              begin
                Caption := HelpText.Lookup('HELPTITLE_GOVLIST');
                for I := 1 to nGov do
                begin
                  AddLine(Phrases.Lookup('GOVERNMENT', I mod nGov), pkSection);
                  LineFeed;
                  if I = nGov then
                    AddLine('', pkBigIcon, 7 * SystemIconLines + imPalace)
                  else
                    AddLine('', pkBigIcon, I + 6);
                  LineFeed;
                  AddTextual(HelpText.LookupByHandle(hGOVHELP, I mod nGov));
                  if I mod nGov >= 2 then
                  begin
                    AddPreqAdv(GovPreq[I mod nGov]);
                    MainText[Count - 1] := Format(HelpText.Lookup('REQUIRED'),
                      [MainText[Count - 1]]);
                  end;
                  if I < nGov then
                  begin
                    LineFeed;
                    LineFeed;
                  end;
                end;
              end;
            miscSearchResult:
              begin
                Caption := HelpText.Lookup('HELPTITLE_SEARCHRESULTS');
                AddTextual(Format(HelpText.Lookup('MATCHES'), [SearchContent]));
                MainText.AppendList(SearchResult);
              end;
          end; // case no
        end;

      hkAdv:
        if no = 200 then
        begin // complete advance list
          Caption := HelpText.Lookup('HELPTITLE_TECHLIST');
          List := THyperText.Create;
          List.OwnsObjects := True;
          for J := 0 to 3 do
          begin
            if J > 0 then
            begin
              LineFeed;
              LineFeed;
            end;
            AddLine(HelpText.Lookup('TECHAGE', J), pkSection);
            if J = 1 then
              AddLine(Phrases.Lookup('ADVANCES', adScience) + ' ' +
                HelpText.Lookup('BASETECH'), pkAdvIcon, adScience, hkAdv,
                adScience);
            if J = 2 then
              AddLine(Phrases.Lookup('ADVANCES', adMassProduction) + ' ' +
                HelpText.Lookup('BASETECH'), pkAdvIcon, adMassProduction, hkAdv,
                adMassProduction);
            List.Clear;
            for I := 0 to nAdv - 1 do
              if (I <> adScience) and (I <> adMassProduction) and
                (AdvValue[I] div 1000 = J) then
                List.AddLine(Phrases.Lookup('ADVANCES', I), pkAdvIcon, I,
                  hkAdv, I);
            List.Sort;
            AppendList(List);
          end;
          FreeAndNil(List);
        end
        else // single advance
        begin
          Caption := Phrases.Lookup('ADVANCES', no);
          LineFeed;
          AddLine(Phrases.Lookup('ADVANCES', no), pkCaption);
          if no in FutureTech then
          begin
            AddLine(HelpText.Lookup('HELPSPEC_FUTURE'));
            LineFeed;
            if no = futResearchTechnology then
              AddItem('FUTURETECHHELP100')
            else
              AddItem('FUTURETECHHELP25');
          end
          else
            AddLine(HelpText.Lookup('HELPSPEC_ADV'));
          if AdvPreq[no, 2] <> preNone then
            NextSection('PREREQALT')
          else
            NextSection('PREREQ');
          for I := 0 to 2 do
            if AdvPreq[no, I] <> preNone then
              AddPreqAdv(AdvPreq[no, I]);
          NextSection('GOVALLOW');
          for I := 2 to nGov - 1 do
            if GovPreq[I] = no then
              AddLine(Phrases.Lookup('GOVERNMENT', I), pkGov, I,
                hkMisc + hkCrossLink, miscGovList);
          NextSection('BUILDALLOW');
          for I := 0 to nWonder - 1 do
            if Imp[I].Preq = no then
              AddImprovement(I);
          for I := nWonder to nImp - 1 do
            if (Imp[I].Preq = no) and (Imp[I].Kind <> ikCommon) then
              AddImprovement(I);
          for I := nWonder to nImp - 1 do
            if (Imp[I].Preq = no) and (Imp[I].Kind = ikCommon) then
              AddImprovement(I);
          NextSection('MODELALLOW');
          for I := 0 to nSpecialModel - 1 do
            if SpecialModelPreq[I] = no then
              AddModel(I);
          NextSection('FEATALLOW');
          for I := 0 to nFeature - 1 do
            if Feature[I].Preq = no then
              AddFeature(I);
          NextSection('FOLLOWADV');
          for I := 0 to nAdv - 1 do
            if (AdvPreq[I, 0] = no) or (AdvPreq[I, 1] = no) or
              (AdvPreq[I, 2] = no) then
              AddAdvance(I);
          NextSection('UPGRADEALLOW');
          for Domain := 0 to nDomains - 1 do
            for I := 1 to nUpgrade - 1 do
              if upgrade[Domain, I].Preq = no then
              begin
                if upgrade[Domain, I].Strength > 0 then
                  AddLine(Format(HelpText.Lookup('STRENGTHUP'),
                    [Phrases.Lookup('DOMAIN', Domain), upgrade[Domain,
                    I].Strength]), pkDomain, Domain);
                if upgrade[Domain, I].Trans > 0 then
                  AddLine(Format(HelpText.Lookup('TRANSUP'),
                    [Phrases.Lookup('DOMAIN', Domain), upgrade[Domain, I].Trans]
                    ), pkDomain, Domain);
                if no in FutureTech then
                  AddLine(Format(HelpText.Lookup('COSTUP'),
                    [upgrade[Domain, I].Cost]), pkNormal_Dot)
                else
                  AddLine(Format(HelpText.Lookup('COSTMIN'),
                    [upgrade[Domain, I].Cost]), pkNormal_Dot)
              end;
          NextSection('EXPIRATION');
          for I := 0 to nWonder - 1 do
            if (Imp[I].Preq <> preNA) and (Imp[I].Expiration = no) then
              AddImprovement(I);
          NextSection('ADVEFFECT');
          S := HelpText.LookupByHandle(hADVHELP, no);
          if S <> '*' then
            AddTextual(S);
          NextSection('SEEALSO');
          CheckSeeAlso := True
        end;

      hkImp:
        if no = 200 then
        begin // complete city improvement list
          Caption := HelpText.Lookup('HELPTITLE_IMPLIST');
          // AddLine(HelpText.Lookup('HELPTITLE_IMPLIST'),pkSection);
          List := THyperText.Create;
          List.OwnsObjects := True;
          for I := nWonder to nImp - 1 do
            if (I <> imTrGoods) and (Imp[I].Preq <> preNA) and
              (Imp[I].Kind = ikCommon) then
              List.AddLine(Phrases.Lookup('IMPROVEMENTS', I), pkSmallIcon,
                I, hkImp, I);
          List.Sort;
          AppendList(List);
          FreeAndNil(List);
        end
        else if no = 201 then
        begin // complete nat. project list
          Caption := HelpText.Lookup('HELPTITLE_UNIQUELIST');
          // AddLine(HelpText.Lookup('HELPTITLE_UNIQUELIST'),pkSection);
          for I := nWonder to nImp - 1 do
            if (Imp[I].Preq <> preNA) and
              ((Imp[I].Kind = ikNatLocal) or (Imp[I].Kind = ikNatGlobal)) then
              AddLine(Phrases.Lookup('IMPROVEMENTS', I), pkSmallIcon, I,
                hkImp, I);
          { LineFeed;
            LineFeed;
            AddLine(HelpText.Lookup('HELPTITLE_SHIPPARTLIST'),pkSection);
            for I:= nWonder to nImp-1 do
            if (Imp[I].Preq<>preNA) and (Imp[I].Kind=ikShipPart) then
            AddLine(Phrases.Lookup('IMPROVEMENTS',I),pkSmallIcon,I,hkImp,I); }
        end
        else if no = 202 then
        begin // complete wonder list
          Caption := HelpText.Lookup('HELPTITLE_WONDERLIST');
          // AddLine(HelpText.Lookup('HELPTITLE_WONDERLIST'),pkSection);
          for I := 0 to nWonder - 1 do
            if Imp[I].Preq <> preNA then
              AddLine(Phrases.Lookup('IMPROVEMENTS', I), pkSmallIcon, I,
                hkImp, I);
        end
        else
        begin // single building
          Caption := Phrases.Lookup('IMPROVEMENTS', no);
          LineFeed;
          AddLine(Phrases.Lookup('IMPROVEMENTS', no), pkRightIcon, no);
          case Imp[no].Kind of
            ikWonder: AddLine(HelpText.Lookup('HELPSPEC_WONDER'));
            ikCommon: AddLine(HelpText.Lookup('HELPSPEC_IMP'));
            ikShipPart: AddLine(HelpText.Lookup('HELPSPEC_SHIPPART'));
          else
            AddLine(HelpText.Lookup('HELPSPEC_NAT'))
          end;
          if Imp[no].Kind <> ikShipPart then begin
            NextSection('EFFECT');
            AddTextual(HelpText.LookupByHandle(hIMPHELP, no));
          end;
          if no = woSun then begin
            AddFeature(mcFirst);
            AddFeature(mcWill);
            AddFeature(mcAcademy);
          end;
          if (no < nWonder) and not Phrases2FallenBackToEnglish then
          begin
            LineFeed;
            if Imp[no].Expiration >= 0 then
              AddTextual(Phrases2.Lookup('HELP_WONDERMORALE1'))
            else
              AddTextual(Phrases2.Lookup('HELP_WONDERMORALE2'));
          end;
          if Imp[no].Preq <> preNone then
          begin
            NextSection('PREREQ');
            AddPreqAdv(Imp[no].Preq);
          end;
          NextSection('COSTS');
          if Difficulty = 0 then
            S := Format(HelpText.Lookup('BUILDCOST'), [Imp[no].Cost])
          else
            S := Format(HelpText.Lookup('BUILDCOST'),
              [Imp[no].Cost * BuildCostMod[Difficulty] div 12]);
          AddLine(S);
          if Imp[no].Maint > 0 then
            AddLine(Format(HelpText.Lookup('MAINTCOST'), [Imp[no].Maint]));
          J := 0;
          for I := 0 to nImpReplacement - 1 do
            if ImpReplacement[I].NewImp = no then
            begin
              if J = 0 then
              begin
                NextSection('REPLACE');
                AddItem('REPLACETEXT');
                J := 1;
              end;
              AddImprovement(ImpReplacement[I].OldImp);
            end;
          if Imp[no].Kind = ikShipPart then
          begin
            LineFeed;
            if no = imShipComp then
              I := 1
            else if no = imShipPow then
              I := 2
            else { if no=imShipHab then }
              I := 3;
            AddLine(Format(HelpText.Lookup('RAREREQUIRED'),
              [Phrases.Lookup('TERRAIN', 3 * 12 + I)]), pkTer, 3 * 12 + I);
          end;
          if (no < nWonder) and (Imp[no].Expiration >= 0) then
          begin
            NextSection('EXPIRATION');
            S := Format(HelpText.Lookup('EXPWITH'),
              [Phrases.Lookup('ADVANCES', Imp[no].Expiration)]);
            if no = woPyramids then
              S := S + ' ' + HelpText.Lookup('EXPSLAVE');
            AddTextual(S);
          end;
          NextSection('SEEALSO');
          if (no < nWonder) and (Imp[no].Expiration >= 0) then
            AddImprovement(woEiffel);
          for I := 0 to nImpReplacement - 1 do
            if ImpReplacement[I].OldImp = no then
              AddImprovement(ImpReplacement[I].NewImp);
          if no = imSupermarket then
            AddLine(HelpText.Lookup('HELPTITLE_JOBLIST'), pkNormal, 0,
              hkMisc + hkCrossLink, miscJobList);
          CheckSeeAlso := True;
        end;

      hkTer:
        if no = 200 then
        begin // complete terrain type list
          Caption := HelpText.Lookup('HELPTITLE_TERLIST');
          // AddLine(HelpText.Lookup('HELPTITLE_TERLIST'),pkSection);
          for I := 0 to nTerrainHelp - 1 do
            AddTerrain(TerrainHelp[I]);
        end
        else
        begin // sigle terrain type
          TerrType := no mod 12;
          if TerrType = fJungle then
            TerrType := fForest;
          TerrSubType := no div 12;
          if no = 3 * 12 then
          begin
            TerrType := fDesert;
            TerrSubType := 0;
          end;
          with Terrain[TerrType] do
          begin
            Caption := Phrases.Lookup('TERRAIN', no);
            LineFeed;
            AddLine(Phrases.Lookup('TERRAIN', no), pkBigTer, no);
            AddLine(HelpText.Lookup('HELPSPEC_TER'));
            LineFeed;
            if (ProdRes[TerrSubType] > 0) or (MineEff > 0) then
              AddLine(Format(HelpText.Lookup('RESPROD'),
                [ProdRes[TerrSubType]]));
            if (no < 3 * 12) and (MineEff > 0) then
              MainText[Count - 1] := MainText[Count - 1] + ' ' +
                Format(HelpText.Lookup('MOREMINE'), [MineEff]);
            if (FoodRes[TerrSubType] > 0) or (IrrEff > 0) then
              AddLine(Format(HelpText.Lookup('RESFOOD'),
                [FoodRes[TerrSubType]]));
            if (no < 3 * 12) and (IrrEff > 0) then
              MainText[Count - 1] := MainText[Count - 1] + ' ' +
                Format(HelpText.Lookup('MOREIRR'), [IrrEff]);
            if TradeRes[TerrSubType] > 0 then
              AddLine(Format(HelpText.Lookup('RESTRADE'),
                [TradeRes[TerrSubType]]));
            if Defense > 4 then
              AddLine(Format(HelpText.Lookup('DEFBONUS'),
                [(Defense - 4) * 25]));
            if (TerrType >= fGrass) and (TerrType <> fMountains) then
              if MoveCost = 2 then
                AddLine(HelpText.Lookup('MOVEHEAVY'))
              else
                AddLine(HelpText.Lookup('MOVEPLAIN'));
            if no = 3 * 12 then
            begin
              LineFeed;
              AddTextual(HelpText.Lookup('DEADLANDS'));
            end;
            if (TerrType = fDesert) and (no <> fDesert + 12) then
            begin
              LineFeed;
              AddTextual(Format(HelpText.Lookup('HOSTILE'), [DesertThurst]));
            end;
            if TerrType = fArctic then
            begin
              LineFeed;
              AddTextual(Format(HelpText.Lookup('HOSTILE'), [ArcticThurst]));
            end;
            if (no < 3 * 12) and (TransTerrain >= 0) then
            begin
              LineFeed;
              I := TransTerrain;
              if (TerrType <> fGrass) and (I <> fGrass) then
                I := I + TerrSubType * 12;
              // trafo to same Special resource group
              AddLine(Format(HelpText.Lookup('TRAFO'),
                [Phrases.Lookup('TERRAIN', I)]), pkTer, I,
                hkTer + hkCrossLink, I);
              if no = fSwamp + 12 then
              begin
                LineFeed;
                AddLine(Format(HelpText.Lookup('TRAFO'),
                  [Phrases.Lookup('TERRAIN', TransTerrain + 24)]), pkTer,
                  TransTerrain + 24, hkTer + hkCrossLink, TransTerrain + 24);
              end
              else if I = fGrass then
              begin
                LineFeed;
                AddLine(Format(HelpText.Lookup('TRAFO'),
                  [Phrases.Lookup('TERRAIN', fGrass + 12)]), pkTer, fGrass + 12,
                  hkTer + hkCrossLink, fGrass + 12);
              end;
            end;
            NextSection('SPECIAL');
            if no = 3 * 12 then
            begin
              LineFeed;
              for Special := 1 to 3 do
              begin
                if Special > 1 then
                  LineFeed;
                AddLine(Phrases.Lookup('TERRAIN', 3 * 12 + Special), pkTer,
                  3 * 12 + Special);
              end;
            end
            else if (no < 12) and (no <> fGrass) and (no <> fOcean) then
            begin
              LineFeed;
              for Special := 1 to 2 do
                if (no <> fArctic) and (no <> fSwamp) or (Special < 2) then
                begin
                  if Special > 1 then
                    LineFeed;
                  AddLine(Phrases.Lookup('TERRAIN', no + Special * 12), pkTer,
                    no + Special * 12);
                  I := FoodRes[Special] - FoodRes[0];
                  if I <> 0 then
                    MainText[Count - 1] := MainText[Count - 1] +
                      Format(HelpText.Lookup('SPECIALFOOD'), [I]);
                  I := ProdRes[Special] - ProdRes[0];
                  if I <> 0 then
                    MainText[Count - 1] := MainText[Count - 1] +
                      Format(HelpText.Lookup('SPECIALPROD'), [I]);
                  I := TradeRes[Special] - TradeRes[0];
                  if I <> 0 then
                    MainText[Count - 1] := MainText[Count - 1] +
                      Format(HelpText.Lookup('SPECIALTRADE'), [I]);
                end;
            end;
            if no = 3 * 12 then
            begin
              LineFeed;
              AddTextual(HelpText.Lookup('RARE'));
            end;
            if (no < 3 * 12) and (TerrType in [fDesert, fArctic]) then
            begin
              NextSection('SEEALSO');
              AddImprovement(woGardens);
              CheckSeeAlso := True
            end;
          end;
        end;

      hkFeature:
        if no = 200 then
        begin // complete feature list
          Caption := HelpText.Lookup('HELPTITLE_FEATURELIST');
          List := THyperText.Create;
          List.OwnsObjects := True;
          for Special := 0 to 2 do
          begin
            if Special > 0 then
            begin
              LineFeed;
              LineFeed;
            end;
            case Special of
              0: AddLine(HelpText.Lookup('HELPTITLE_FEATURE1LIST'), pkSection);
              1: AddLine(HelpText.Lookup('HELPTITLE_FEATURE2LIST'), pkSection);
              2: AddLine(HelpText.Lookup('HELPTITLE_FEATURE3LIST'), pkSection);
            end;
            List.Clear;
            for I := 0 to nFeature - 1 do
              if Feature[I].Preq <> preNA then
              begin
                if I < mcFirstNonCap then
                  J := 0
                else if I in AutoFeature then
                  J := 2
                else
                  J := 1;
                if J = Special then
                  List.AddLine(Phrases.Lookup('FEATURES', I), pkFeature, I,
                    hkFeature, I);
              end;
            List.Sort;
            AppendList(List);
          end;
          FreeAndNil(List);
        end
        else
        begin // single feature
          Caption := Phrases.Lookup('FEATURES', no);
          LineFeed;
          AddLine(Phrases.Lookup('FEATURES', no), pkBigFeature, no);
          if no < mcFirstNonCap then
            AddLine(HelpText.Lookup('HELPSPEC_CAP'))
          else if no in AutoFeature then
            AddLine(HelpText.Lookup('HELPSPEC_STANDARD'))
          else
            AddLine(HelpText.Lookup('HELPSPEC_FEATURE'));
          NextSection('EFFECT');
          AddTextual(HelpText.LookupByHandle(hFEATUREHELP, no));
          if (Feature[no].Weight <> 0) or (Feature[no].Cost <> 0) then
          begin
            NextSection('COSTS');
            S := IntToStr(Feature[no].Cost);
            if Feature[no].Cost >= 0 then
              S := '+' + S;
            AddLine(Format(HelpText.Lookup('COSTBASE'), [S]));
            if Feature[no].Weight > 0 then
            begin
              AddLine(Format(HelpText.Lookup('WEIGHT'),
                ['+' + IntToStr(Feature[no].Weight)]));
              if no = mcDefense then
                AddLine(Format(HelpText.Lookup('WEIGHT'), ['+2']),
                  pkDomain, dGround);
            end;
          end;
          if Feature[no].Preq <> preNone then
          begin
            LineFeed;
            if Feature[no].Preq = preSun then
              AddPreqImp(woSun) // sun tsu feature
            else
              AddPreqAdv(Feature[no].Preq);
            MainText[Count - 1] := Format(HelpText.Lookup('REQUIRED'),
              [MainText[Count - 1]]);
          end;
          NextSection('SEEALSO');
          CheckSeeAlso := True;
        end;

      hkModel:
        begin
          Caption := HelpText.Lookup('HELPTITLE_MODELLIST');
          for I := 0 to nSpecialModel - 1 do
            if I <> 2 then
              AddModelText(I);
          LineFeed;
          AddItem('MODELNOTE');
        end;

    end;
    if CheckSeeAlso then
      for I := 0 to nSeeAlso - 1 do
        if (SeeAlso[I].Kind = Kind) and (SeeAlso[I].no = no) then
          case SeeAlso[I].SeeKind of
            hkImp: AddImprovement(SeeAlso[I].SeeNo);
            hkAdv: AddAdvance(SeeAlso[I].SeeNo);
            hkFeature: AddFeature(SeeAlso[I].SeeNo);
          end;
    if (Headline >= 0) and (Count = Headline + 1) then
      Delete(Headline)
    else
      LineFeed;

    //Self.Show;
    ScrollBar.Init(Count - 1, InnerHeight div 24);
    ScrollBar.SetPos(sbPos);
    BackBtn.Visible := HistItems.Count > 1;
    TopBtn.Visible := (HistItems.Count > 1) or (Kind <> hkMisc) or (no <> miscMain);
    Sel := -1;
  end; // with MainText
end;

procedure THelpDlg.ShowNewContent(NewMode: TWindowMode; Category, Index: Integer);
begin
  if (Category <> Kind) or (Index <> no) or (Category = hkMisc) and
    (Index = miscSearchResult) then begin
    if HistItems.Count = MaxHist then HistItems.Delete(0);
    if HistItems.Count = 0 then
      HistItems.AddNew(Category, Index, ScrollBar.Position, NewSearchContent)
      else HistItems.AddNew(Kind, No, ScrollBar.Position, SearchContent);
  end;
  Kind := Category;
  no := Index;
  SearchContent := NewSearchContent;
  Prepare;
  OffscreenPaint;
  inherited ShowNewContent(NewMode);
end;

procedure THelpDlg.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  i0, Sel0: Integer;
begin
  Y := Y - WideFrame;
  i0 := ScrollBar.Position;
  Sel0 := Sel;
  if (X >= SideFrame) and (X < SideFrame + InnerWidth) and (Y >= 0) and
    (Y < InnerHeight) and (Y mod 24 >= 8) then
    Sel := Y div 24
  else
    Sel := -1;
  if (Sel + i0 >= MainText.Count) or (Sel >= 0) and
    (THelpLineInfo(MainText.Objects[Sel + i0]).Link = 0) then
    Sel := -1;
  if Sel <> Sel0 then
  begin
    if Sel0 <> -1 then
      Line(Canvas, Sel0, False);
    if Sel <> -1 then
      Line(Canvas, Sel, True);
  end;
end;

procedure THelpDlg.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sel >= 0 then
    with THelpLineInfo(MainText.Objects[Sel + ScrollBar.Position]) do
      if Link shr 8 and $3F = hkInternet then
        case Link and $FF of
          1: OpenDocument(HomeDir + AITemplateFileName);
          2: OpenURL(CevoHomepage);
          3: OpenURL(CevoContact);
        end
      else
      begin
        if (Link >= $8000) and (Link and $3FFF = liInvalid) then
          Exit; // invalid link;
        if Link >= $8000 then
          ShowNewContent(FWindowMode, hkText, Link and $3FFF)
        else
          ShowNewContent(FWindowMode, Link shr 8 and $3F, Link and $FF);
      end;
end;

procedure THelpDlg.BackBtnClick(Sender: TObject);
var
  HistItem: THistItem;
begin
  if HistItems.Count > 1 then begin
    HistItem := THistItem.Create;
    HistItem.Assign(HistItems.Last);
    HistItems.Delete(HistItems.Count - 1);
    if (HistItem.Kind = hkMisc) and (HistItem.No = miscSearchResult) and
      (HistItem.SearchContent <> SearchContent) then
    begin
      SearchContent := HistItem.SearchContent;
      Search(SearchContent);
    end;
    Kind := HistItem.Kind;
    no := HistItem.No;
    Prepare(HistItem.Pos);
    OffscreenPaint;
    Invalidate;
    FreeAndNil(HistItem);
  end;
end;

procedure THelpDlg.TopBtnClick(Sender: TObject);
begin
  while HistItems.Count > 1 do HistItems.Delete(HistItems.Count - 1);
  Kind := hkMisc;
  no := miscMain;
  Prepare;
  OffscreenPaint;
  Invalidate;
end;

procedure THelpDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ExtPic.Height := 0;
  inherited;
end;

function THelpDlg.TextIndex(Item: string): Integer;
begin
  Result := HelpText.Gethandle(Item);
end;

procedure THelpDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if KeyToShortCut(Key, Shift) = BHelp.ShortCut then // my key
  else
    inherited;
end;

procedure THelpDlg.SearchBtnClick(Sender: TObject);
begin
  InputDlg.Caption := Phrases.Lookup('SEARCH');
  InputDlg.EInput.Text := SearchContent;
  InputDlg.CenterToRect(BoundsRect);
  InputDlg.ShowModal;
  if (InputDlg.ModalResult = mrOK) and (Length(InputDlg.EInput.Text) >= 2) then
  begin
    Search(InputDlg.EInput.Text);
    case SearchResult.Count of
      0: begin
        Gtk2Fix;
        SimpleMessage(Format(HelpText.Lookup('NOMATCHES'),
          [InputDlg.EInput.Text]));
      end;
      1:
        with THelpLineInfo(SearchResult.Objects[0]) do
          if Link >= $8000 then
            ShowNewContent(FWindowMode, hkText, Link and $3FFF)
          else
            ShowNewContent(FWindowMode, Link shr 8 and $3F, Link and $FF);
    else
      begin
        NewSearchContent := InputDlg.EInput.Text;
        ShowNewContent(FWindowMode, hkMisc, miscSearchResult);
      end;
    end;
  end;
end;

procedure THelpDlg.Search(SearchString: string);
var
  H, I, PrevHandle, PrevIndex, P, RightMargin: Integer;
  S: string;
  mADVHELP, mIMPHELP, mFEATUREHELP: set of 0 .. 255;
  bGOVHELP, bSPECIALMODEL, bJOBHELP: Boolean;
begin
  SearchResult.Clear;
  mADVHELP := [];
  mIMPHELP := [];
  mFEATUREHELP := [];
  bGOVHELP := False;
  bSPECIALMODEL := False;
  bJOBHELP := False;

  // search in generic reference
  SearchString := UpperCase(SearchString);
  for I := 0 to 35 + 4 do begin
    S := Phrases.Lookup('TERRAIN', I);
    if Pos(SearchString, UpperCase(S)) > 0 then
      if I < 36 then
        SearchResult.AddLine(S + ' ' + HelpText.Lookup('HELPSPEC_TER'),
          pkNormal, 0, hkTer + hkCrossLink, I)
      else
      begin
        SearchResult.AddLine(Phrases.Lookup('TERRAIN', 36) + ' ' +
          HelpText.Lookup('HELPSPEC_TER'), pkNormal, 0,
          hkTer + hkCrossLink, 36);
        if I > 36 then
          SearchResult.AddLine(Phrases.Lookup('IMPROVEMENTS',
            imShipComp + I - 37) + ' ' + HelpText.Lookup('HELPSPEC_SHIPPART'),
            pkNormal, 0, hkImp + hkCrossLink, imShipComp + I - 37);
        Break;
      end;
  end;
  for I := 0 to nJobHelp - 1 do
    if Pos(SearchString, UpperCase(Phrases.Lookup('JOBRESULT', JobHelp[I]))) > 0
    then
    begin
      SearchResult.AddLine(HelpText.Lookup('HELPTITLE_JOBLIST'), pkNormal, 0,
        hkMisc + hkCrossLink, miscJobList);
      bJOBHELP := True;
      Break;
    end;
  for I := 0 to nAdv - 1 do
  begin
    S := Phrases.Lookup('ADVANCES', I);
    if Pos(SearchString, UpperCase(S)) > 0 then
    begin
      if I in FutureTech then
        S := S + ' ' + HelpText.Lookup('HELPSPEC_FUTURE')
      else
        S := S + ' ' + HelpText.Lookup('HELPSPEC_ADV');
      SearchResult.AddLine(S, pkNormal, 0, hkAdv + hkCrossLink, I);
      Include(mADVHELP, I);
    end;
  end;
  for I := 0 to nSpecialModel - 1 do
  begin
    FindStdModelPicture(SpecialModelPictureCode[I], H, S);
    if Pos(SearchString, UpperCase(S)) > 0 then
    begin
      SearchResult.AddLine(HelpText.Lookup('HELPTITLE_MODELLIST'), pkNormal, 0,
        hkModel + hkCrossLink, 0);
      bSPECIALMODEL := True;
      Break;
    end;
  end;
  for I := 0 to nFeature - 1 do
  begin
    S := Phrases.Lookup('FEATURES', I);
    if Pos(SearchString, UpperCase(S)) > 0 then
    begin
      if I < mcFirstNonCap then
        S := S + ' ' + HelpText.Lookup('HELPSPEC_CAP')
      else if I in AutoFeature then
        S := S + ' ' + HelpText.Lookup('HELPSPEC_STANDARD')
      else
        S := S + ' ' + HelpText.Lookup('HELPSPEC_FEATURE');
      SearchResult.AddLine(S, pkNormal, 0, hkFeature + hkCrossLink, I);
      Include(mFEATUREHELP, I);
    end;
  end;
  for I := 0 to nImp - 1 do
  begin
    S := Phrases.Lookup('IMPROVEMENTS', I);
    if Pos(SearchString, UpperCase(S)) > 0 then
    begin
      case Imp[I].Kind of
        ikWonder:
          S := S + ' ' + HelpText.Lookup('HELPSPEC_WONDER');
        ikCommon:
          S := S + ' ' + HelpText.Lookup('HELPSPEC_IMP');
        ikShipPart:
          S := S + ' ' + HelpText.Lookup('HELPSPEC_SHIPPART');
      else
        S := S + ' ' + HelpText.Lookup('HELPSPEC_NAT')
      end;
      SearchResult.AddLine(S, pkNormal, 0, hkImp + hkCrossLink, I);
      Include(mIMPHELP, I);
    end
  end;
  for I := 0 to nGov - 1 do
    if Pos(SearchString, UpperCase(Phrases.Lookup('GOVERNMENT', I))) > 0 then
    begin
      SearchResult.AddLine(HelpText.Lookup('HELPTITLE_GOVLIST'), pkNormal, 0,
        hkMisc + hkCrossLink, miscGovList);
      bGOVHELP := True;
      Break;
    end;

  // full text search
  H := -1;
  repeat
    PrevHandle := H;
    PrevIndex := I;
    if not HelpText.Search(SearchString, H, I) then
      Break;
    if H = hADVHELP then
    begin
      if (I >= 0) and ((I <> PrevIndex) or (H <> PrevHandle)) and
        not(I in mADVHELP) then
      begin
        S := Phrases.Lookup('ADVANCES', I);
        if I in FutureTech then
          S := S + ' ' + HelpText.Lookup('HELPSPEC_FUTURE')
        else
          S := S + ' ' + HelpText.Lookup('HELPSPEC_ADV');
        SearchResult.AddLine(S, pkNormal, 0, hkAdv + hkCrossLink, I)
      end;
    end
    else if H = hIMPHELP then
    begin
      if (I >= 0) and ((I <> PrevIndex) or (H <> PrevHandle)) and
        not(I in mIMPHELP) then
      begin
        S := Phrases.Lookup('IMPROVEMENTS', I);
        case Imp[I].Kind of
          ikWonder:
            S := S + ' ' + HelpText.Lookup('HELPSPEC_WONDER');
          ikCommon:
            S := S + ' ' + HelpText.Lookup('HELPSPEC_IMP');
          ikShipPart:
            S := S + ' ' + HelpText.Lookup('HELPSPEC_SHIPPART');
        else
          S := S + ' ' + HelpText.Lookup('HELPSPEC_NAT')
        end;
        SearchResult.AddLine(S, pkNormal, 0, hkImp + hkCrossLink, I)
      end;
    end
    else if H = hFEATUREHELP then
    begin
      if (I >= 0) and ((I <> PrevIndex) or (H <> PrevHandle)) and
        not(I in mFEATUREHELP) then
      begin
        S := Phrases.Lookup('FEATURES', I);
        if I < mcFirstNonCap then
          S := S + ' ' + HelpText.Lookup('HELPSPEC_CAP')
        else if I in AutoFeature then
          S := S + ' ' + HelpText.Lookup('HELPSPEC_STANDARD')
        else
          S := S + ' ' + HelpText.Lookup('HELPSPEC_FEATURE');
        SearchResult.AddLine(S, pkNormal, 0, hkFeature + hkCrossLink, I);
      end;
    end
    else if H = hGOVHELP then
    begin
      if (I >= 0) and (H <> PrevHandle) and not bGOVHELP then
        SearchResult.AddLine(HelpText.Lookup('HELPTITLE_GOVLIST'), pkNormal, 0,
          hkMisc + hkCrossLink, miscGovList)
    end
    else if H = hSPECIALMODEL then
    begin
      if (I >= 0) and (H <> PrevHandle) and not bSPECIALMODEL then
        SearchResult.AddLine(HelpText.Lookup('HELPTITLE_MODELLIST'), pkNormal,
          0, hkModel + hkCrossLink, 0)
    end
    else if H = hJOBHELP then
    begin
      if (I >= 0) and (H <> PrevHandle) and not bJOBHELP then
        SearchResult.AddLine(HelpText.Lookup('HELPTITLE_JOBLIST'), pkNormal, 0,
          hkMisc + hkCrossLink, miscJobList)
    end
    else if { (h<>hMAIN) and } (H <> PrevHandle) then
    begin
      S := HelpText.LookupByHandle(H);
      P := Pos('$', S);
      if P > 0 then
      begin
        S := Copy(S, P + 1, maxint);
        P := Pos('\', S);
        if P > 0 then
          S := Copy(S, 1, P - 1);
        SearchResult.AddLine(S, pkNormal, 0, hkText + hkCrossLink, H);
      end;
    end;
    until False;

    // cut lines to fit to window
    RightMargin := InnerWidth - 16 - GetSystemMetrics(SM_CXVSCROLL);
    OffScreen.Canvas.Font.Assign(UniFont[ftNormal]);
    for I := 0 to SearchResult.Count - 1 do
    begin
      while BiColorTextWidth(OffScreen.Canvas, SearchResult[I]) >
        RightMargin - 32 do
        SearchResult[I] := Copy(SearchResult[I], 1, Length(SearchResult[I]) - 1)
    end;
  end;

end.
