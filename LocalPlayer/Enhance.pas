{$INCLUDE Switches.inc}
unit Enhance;

interface

uses
  ScreenTools, BaseWin, Protocol, ClientTools, Term, LCLIntf, LCLType, SysUtils,
  Classes, Graphics, Controls, Forms, IsoEngine, ButtonB, ButtonC, Menus;

type

  { TEnhanceDlg }

  TEnhanceDlg = class(TFramedDlg)
    MenuItem1: TMenuItem;
    ToggleBtn: TButtonB;
    CloseBtn: TButtonB;
    job1: TButtonC;
    job2: TButtonC;
    job4: TButtonC;
    job5: TButtonC;
    job7: TButtonC;
    job3: TButtonC;
    job6: TButtonC;
    job9: TButtonC;
    Popup: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure ToggleBtnClick(Sender: TObject);
    procedure TerrClick(Sender: TObject);
    procedure JobClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    NoMap: TIsoMap;
  public
    procedure ShowNewContent(NewMode: TWindowMode; TerrType: Integer = -1);
  protected
    Page: Integer;
    procedure OffscreenPaint; override;
  end;

var
  EnhanceDlg: TEnhanceDlg;


implementation

uses
  Help, UKeyBindings;

{$R *.lfm}

procedure TEnhanceDlg.FormCreate(Sender: TObject);
var
  TerrType: Integer;
  M: TMenuItem;
begin
  inherited;
  NoMap := TIsoMap.Create;
  CaptionRight := CloseBtn.Left;
  CaptionLeft := ToggleBtn.Left + ToggleBtn.Width;
  InitButtons;
  HelpContext := 'MACRO';
  Caption := Phrases.Lookup('TITLE_ENHANCE');
  ToggleBtn.Hint := Phrases.Lookup('BTN_SELECT');

  for TerrType := fGrass to fMountains do
    if TerrType <> fJungle then
    begin
      M := TMenuItem.Create(Popup);
      M.RadioItem := True;
      if TerrType = fGrass then
        M.Caption := Format(Phrases.Lookup('TWOTERRAINS'),
          [Phrases.Lookup('TERRAIN', fGrass), Phrases.Lookup('TERRAIN',
          fGrass + 12)])
      else if TerrType = fForest then
        M.Caption := Format(Phrases.Lookup('TWOTERRAINS'),
          [Phrases.Lookup('TERRAIN', fForest), Phrases.Lookup('TERRAIN',
          fJungle)])
      else
        M.Caption := Phrases.Lookup('TERRAIN', TerrType);
      M.Tag := TerrType;
      M.OnClick := TerrClick;
      Popup.Items.Add(M);
    end;
end;

procedure TEnhanceDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(NoMap);
end;

procedure TEnhanceDlg.FormPaint(Sender: TObject);
var
  I: Integer;
begin
  inherited;
  BtnFrame(Canvas, Rect(job1.Left, job1.Top, job7.Left + job7.Width,
    job1.Top + job1.Height), MainTexture);
  BtnFrame(Canvas, Rect(job3.Left, job3.Top, job9.Left + job9.Width,
    job3.Top + job3.Height), MainTexture);
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TButtonC then
      BitBltCanvas(Canvas, Controls[I].Left + 2, Controls[I].Top - 11, 8, 8,
        HGrSystem.Data.Canvas, 121 + Controls[I].Tag mod 7 * 9,
        1 + Controls[I].Tag div 7 * 9);
end;

procedure TEnhanceDlg.FormShow(Sender: TObject);
begin
  OffscreenPaint;
end;

procedure TEnhanceDlg.ShowNewContent(NewMode: TWindowMode; TerrType: Integer);
begin
  if (TerrType < fGrass) or (TerrType > fMountains) then
    Page := fGrass
  else
    Page := TerrType;
  inherited ShowNewContent(NewMode);
end;

procedure TEnhanceDlg.OffscreenPaint;
var
  I, stage, TerrType, TileImp, X, EndStage, Cost, LastJob: Integer;
  S: string;
  Done: Set of jNone .. jTrans;
  TypeChanged: Boolean;
begin
  OffscreenUser := self;
  Offscreen.Canvas.Font.Assign(UniFont[ftSmall]);
  FillOffscreen(0, 0, InnerWidth, InnerHeight);

  EndStage := 0;
  while (EndStage < 5) and (MyData.EnhancementJobs[Page, EndStage] <> jNone) do
    Inc(EndStage);
  with NoMap do
    X := InnerWidth div 2 - xxt - (xxt + 3) * EndStage;

  TerrType := Page;
  TileImp := 0;
  Done := [];
  Cost := 0;
  for stage := 0 to EndStage do
  begin
    if stage > 0 then
    begin
      Sprite(Offscreen, HGrSystem, X - 10, 66, 14, 14, 80, 1);
      case MyData.EnhancementJobs[Page, stage - 1] of
        jRoad:
          begin
            Inc(Cost, Terrain[TerrType].MoveCost * RoadWork);
            TileImp := TileImp or fRoad;
          end;
        jRR:
          begin
            Inc(Cost, Terrain[TerrType].MoveCost * RRWork);
            TileImp := TileImp or fRR;
          end;
        jIrr:
          begin
            Inc(Cost, Terrain[TerrType].IrrClearWork);
            TileImp := TileImp and not fTerImp or tiIrrigation;
          end;
        jFarm:
          begin
            Inc(Cost, Terrain[TerrType].IrrClearWork * FarmWork);
            TileImp := TileImp and not fTerImp or tiFarm;
          end;
        jMine:
          begin
            Inc(Cost, Terrain[TerrType].MineAfforestWork);
            TileImp := TileImp and not fTerImp or tiMine;
          end;
        jClear:
          begin
            Inc(Cost, Terrain[TerrType].IrrClearWork);
            TerrType := Terrain[TerrType].ClearTerrain;
          end;
        jAfforest:
          begin
            Inc(Cost, Terrain[TerrType].MineAfforestWork);
            TerrType := Terrain[TerrType].AfforestTerrain;
          end;
        jTrans:
          begin
            Inc(Cost, Terrain[TerrType].TransWork);
            TerrType := Terrain[TerrType].TransTerrain;
          end;
      end;
      Include(Done, MyData.EnhancementJobs[Page, stage - 1]);
    end;

    with NoMap do begin
      if TerrType < fForest then
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2,
          1 + TerrType * (xxt * 2 + 1), 1 + yyt)
      else
      begin
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2,
          1 + 2 * (xxt * 2 + 1), 1 + yyt + 2 * (yyt * 3 + 1));
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2,
          1 + 7 * (xxt * 2 + 1), 1 + yyt + 2 * (2 + TerrType - fForest) *
          (yyt * 3 + 1));
      end;
      if TileImp and fTerImp = tiFarm then
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2,
          1 + (xxt * 2 + 1), 1 + yyt + 12 * (yyt * 3 + 1))
      else if TileImp and fTerImp = tiIrrigation then
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2, 1,
          1 + yyt + 12 * (yyt * 3 + 1));
      if TileImp and fRR <> 0 then
      begin
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2,
          1 + 6 * (xxt * 2 + 1), 1 + yyt + 10 * (yyt * 3 + 1));
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2,
          1 + 2 * (xxt * 2 + 1), 1 + yyt + 10 * (yyt * 3 + 1));
      end
      else if TileImp and fRoad <> 0 then
      begin
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2,
          1 + 6 * (xxt * 2 + 1), 1 + yyt + 9 * (yyt * 3 + 1));
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2,
          1 + 2 * (xxt * 2 + 1), 1 + yyt + 9 * (yyt * 3 + 1));
      end;
      if TileImp and fTerImp = tiMine then
        Sprite(Offscreen, HGrTerrain, X, 64 - yyt, xxt * 2, yyt * 2,
          1 + 2 * (xxt * 2 + 1), 1 + yyt + 12 * (yyt * 3 + 1));
      Inc(X, xxt * 2 + 6);
    end;
  end;

  for I := 0 to Popup.Items.Count - 1 do
    if Popup.Items[I].Tag = Page then
      S := Popup.Items[I].Caption;
  if Cost > 0 then
    S := Format(Phrases.Lookup('ENHANCE'), [S, MovementToString(Cost)]);
  LoweredTextOut(Offscreen.Canvas, -1, MainTexture,
    (InnerWidth - BiColorTextWidth(Offscreen.Canvas, S)) div 2, 12, S);

  if EndStage > 0 then
    LastJob := MyData.EnhancementJobs[Page, EndStage - 1]
  else
    LastJob := jNone;
  if jRoad in Done then
    job1.ButtonIndex := 3
  else
    job1.ButtonIndex := 2;
  if jRR in Done then
    job2.ButtonIndex := 3
  else
    job2.ButtonIndex := 2;
  if jIrr in Done then
    job4.ButtonIndex := 3
  else
    job4.ButtonIndex := 2;
  if jFarm in Done then
    job5.ButtonIndex := 3
  else
    job5.ButtonIndex := 2;
  if jMine in Done then
    job7.ButtonIndex := 3
  else
    job7.ButtonIndex := 2;
  if LastJob = jClear then
    job3.ButtonIndex := 3
  else
    job3.ButtonIndex := 2;
  if LastJob = jAfforest then
    job6.ButtonIndex := 3
  else
    job6.ButtonIndex := 2;
  if LastJob = jTrans then
    job9.ButtonIndex := 3
  else
    job9.ButtonIndex := 2;

  TypeChanged := LastJob in [jClear, jAfforest, jTrans];
  job1.Visible := (jRoad in Done) or not TypeChanged;
  job2.Visible := (jRR in Done) or not TypeChanged;
  job4.Visible := (jIrr in Done) or not TypeChanged and
    (Terrain[TerrType].IrrEff > 0);
  job5.Visible := (jFarm in Done) or not TypeChanged and
    (Terrain[TerrType].IrrEff > 0);
  job7.Visible := (jMine in Done) or not TypeChanged and
    (Terrain[TerrType].MineEff > 0);
  job3.Visible := not TypeChanged and (Terrain[TerrType].ClearTerrain >= 0) and
    ((TerrType <> fDesert) or (MyRO.Wonder[woGardens].EffectiveOwner = Me)) or
    (LastJob = jClear);
  job6.Visible := not TypeChanged and (Terrain[TerrType].AfforestTerrain >= 0)
    or (LastJob = jAfforest);
  job9.Visible := not TypeChanged and (Terrain[TerrType].TransTerrain >= 0) or
    (LastJob = jTrans);

  MarkUsedOffscreen(InnerWidth, InnerHeight);
end;

procedure TEnhanceDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TEnhanceDlg.ToggleBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Popup.Items.Count - 1 do
    Popup.Items[I].Checked := Popup.Items[I].Tag = Page;
  Popup.Popup(Left + ToggleBtn.Left, Top + ToggleBtn.Top + ToggleBtn.Height);
end;

procedure TEnhanceDlg.TerrClick(Sender: TObject);
begin
  Page := TComponent(Sender).Tag;
  SmartUpdateContent;
end;

procedure TEnhanceDlg.JobClick(Sender: TObject);
var
  stage, NewJob: Integer;
  Done: Set of jNone .. jTrans;

  procedure RemoveJob(J: Integer);
  begin // remove job
    stage := 0;
    while (stage < 5) and (MyData.EnhancementJobs[Page, stage] <> jNone) do
    begin
      if (MyData.EnhancementJobs[Page, stage] = J) or (J = jRoad) and
        (MyData.EnhancementJobs[Page, stage] = jRR) or (J = jIrr) and
        (MyData.EnhancementJobs[Page, stage] = jFarm) then
      begin
        if stage < 4 then
          Move(MyData.EnhancementJobs[Page, stage + 1],
            MyData.EnhancementJobs[Page, stage], 4 - stage);
        MyData.EnhancementJobs[Page, 4] := jNone;
      end
      else
        Inc(stage);
    end;
  end;

begin
  NewJob := TButtonC(Sender).Tag;
  Done := [];
  stage := 0;
  while (stage < 5) and (MyData.EnhancementJobs[Page, stage] <> jNone) do
  begin
    Include(Done, MyData.EnhancementJobs[Page, stage]);
    Inc(stage);
  end;
  if NewJob in Done then
    RemoveJob(NewJob)
  else
  begin // add job
    if NewJob in [jMine, jAfforest] then
      RemoveJob(jIrr);
    if NewJob in [jIrr, jFarm, jTrans] then
      RemoveJob(jMine);
    if (NewJob = jRR) and not(jRoad in Done) then
    begin
      MyData.EnhancementJobs[Page, stage] := jRoad;
      Inc(stage);
    end;
    if (NewJob = jFarm) and not(jIrr in Done) then
    begin
      MyData.EnhancementJobs[Page, stage] := jIrr;
      Inc(stage);
    end;
    MyData.EnhancementJobs[Page, stage] := NewJob;
  end;
  SmartUpdateContent;
end;

procedure TEnhanceDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ShortCut: TShortCut;
begin
  ShortCut := KeyToShortCut(Key, Shift);
  if BHelp.Test(ShortCut) then
    HelpDlg.ShowNewContent(WindowModeMakePersistent(FWindowMode), hkText,
      HelpDlg.TextIndex('MACRO'))
end;

end.
