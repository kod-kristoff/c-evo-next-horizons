unit Settings;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLProc, ScreenTools, Messg, ButtonA, Registry, fgl, Directories, DrawDlg,
  ButtonC, UKeyBindings, ULanguages;

type
  { TSettingsDlg }

  TSettingsDlg = class(TDrawDlg)
    ButtonFullscreen: TButtonC;
    Down2Btn: TButtonC;
    EditShortCutPrimary: TEdit;
    EditShortCutSecondary: TEdit;
    ListLanguages: TListBox;
    ListKeyBindings: TListBox;
    ButtonOk: TButtonA;
    ButtonCancel: TButtonA;
    ButtonReset: TButtonA;
    Up2Btn: TButtonC;
    procedure ButtonFullscreenClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure Down2BtnClick(Sender: TObject);
    procedure EditShortCutPrimaryKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditShortCutSecondaryKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListKeyBindingsSelectionChange(Sender: TObject; User: Boolean);
    procedure ButtonOkClick(Sender: TObject);
    procedure Up2BtnClick(Sender: TObject);
  private
    LocalGamma: Integer;
    LocalKeyBindings: TKeyBindings;
    CurrentKeyBinding: TKeyBinding;
    procedure UpdateShortCutItem;
  public
    procedure LoadData;
    procedure SaveData;
  end;

var
  SettingsDlg: TSettingsDlg;


implementation

{$R *.lfm}

uses
  Start;

var
  SFullScreen, SGamma, SRestartMsg, SShortCutPrimary, SShortCutSecondary,
  SLanguages, SKeyBindings: string;

procedure ReloadLanguages;
begin
  SFullScreen := Phrases.Lookup('SETTINGS', 0);
  SGamma := Phrases.Lookup('SETTINGS', 1);
  SRestartMsg := Phrases.Lookup('SETTINGS', 2);
  SShortCutPrimary := Phrases.Lookup('SETTINGS', 3);
  SShortCutSecondary := Phrases.Lookup('SETTINGS', 4);
  SLanguages := Phrases.Lookup('SETTINGS', 5);
  SKeyBindings := Phrases.Lookup('SETTINGS', 6);
end;

{ TSettingsDlg }

procedure TSettingsDlg.FormCreate(Sender: TObject);
begin
  LocalKeyBindings := TKeyBindings.Create;

  Canvas.Font.Assign(UniFont[ftNormal]);
  Canvas.Brush.Style := bsClear;

  ButtonOk.Caption := Phrases.Lookup('BTN_OK');
  ButtonCancel.Caption := Phrases.Lookup('BTN_CANCEL');
  ButtonReset.Caption := Phrases.Lookup('BTN_RESET');
  InitButtons;
end;

procedure TSettingsDlg.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSettingsDlg.ButtonResetClick(Sender: TObject);
begin
  ListLanguages.ItemIndex := 0;
  ButtonFullscreen.ButtonIndex := 3;
  LocalGamma := 100;
  ListKeyBindings.ItemIndex := -1;
  ListKeyBindingsSelectionChange(nil, False);
  LocalKeyBindings.ResetToDefault;
  LocalKeyBindings.LoadToStrings(ListKeyBindings.Items);
  Repaint;
end;

procedure TSettingsDlg.Down2BtnClick(Sender: TObject);
begin
  if LocalGamma > 50 then
  begin
    Dec(LocalGamma);
    Invalidate;
  end;
end;

procedure TSettingsDlg.EditShortCutPrimaryKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Sender is TEdit) and Assigned(CurrentKeyBinding) and not (Key in [16..18]) then begin
    CurrentKeyBinding.ShortCut := Key or
      (scShift * Integer(ssShift in Shift)) or
      (scCtrl * Integer(ssCtrl in Shift)) or
      (scAlt * Integer(ssAlt in Shift));
    EditShortCutPrimary.Text := ShortCutToText(CurrentKeyBinding.ShortCut);
    Key := 0;
    UpdateShortCutItem;
  end;
end;

procedure TSettingsDlg.EditShortCutSecondaryKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Sender is TEdit) and Assigned(CurrentKeyBinding) and not (Key in [16..18]) then begin
    CurrentKeyBinding.ShortCut2 := Key or
      (scShift * Integer(ssShift in Shift)) or
      (scCtrl * Integer(ssCtrl in Shift)) or
      (scAlt * Integer(ssAlt in Shift));
    EditShortCutSecondary.Text := ShortCutToText(CurrentKeyBinding.ShortCut2);
    Key := 0;
    UpdateShortCutItem;
  end;
end;

procedure TSettingsDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  ListKeyBindings.ItemIndex := -1;
end;

procedure TSettingsDlg.ButtonFullscreenClick(Sender: TObject);
begin
  ButtonFullscreen.ButtonIndex := ButtonFullscreen.ButtonIndex xor 1;
end;

procedure TSettingsDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(LocalKeyBindings);
end;

procedure TSettingsDlg.FormPaint(Sender: TObject);
begin
  PaintBackground(self, 3, 3, ClientWidth - 6, ClientHeight - 6);
  Frame(Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);
  Frame(Canvas, 1, 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  Frame(Canvas, 2, 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.ColorBevelLight, MainTexture.ColorBevelShade);
  EditFrame(Canvas, EditShortCutPrimary.BoundsRect, MainTexture);
  EditFrame(Canvas, EditShortCutSecondary.BoundsRect, MainTexture);
  EditFrame(Canvas, ListLanguages.BoundsRect, MainTexture);
  BtnFrame(Canvas, ButtonOk.BoundsRect, MainTexture);
  BtnFrame(Canvas, ButtonCancel.BoundsRect, MainTexture);

  RFrame(Canvas, ButtonFullscreen.Left - 1, ButtonFullscreen.Top - 1,
    ButtonFullscreen.Left + 12, ButtonFullscreen.Top + 12, MainTexture.ColorBevelShade,
    MainTexture.ColorBevelLight);

  LoweredTextOut(Canvas, -2, MainTexture, ListLanguages.Left,
    ListLanguages.Top - 26, SLanguages);
  LoweredTextOut(Canvas, -2, MainTexture, ListKeyBindings.Left,
    ListKeyBindings.Top - 26, SKeyBindings);
  LoweredTextOut(Canvas, -2, MainTexture, ButtonFullscreen.Left + 32,
    ButtonFullscreen.Top - 4, SFullScreen);
  UnderlinedTitleValue(Canvas, SGamma, IntToStr(LocalGamma) + '%',
    Up2Btn.Left - 150 - 4, Up2Btn.Top + 2, 150);
  LoweredTextOut(Canvas, -2, MainTexture, EditShortCutPrimary.Left,
    EditShortCutPrimary.Top - 26, SShortCutPrimary);
  LoweredTextOut(Canvas, -2, MainTexture, EditShortCutSecondary.Left,
    EditShortCutSecondary.Top - 26, SShortCutSecondary);
end;

procedure TSettingsDlg.FormShow(Sender: TObject);
begin
  ReloadLanguages;
  StartDlg.Translator.LanguageListToStrings(ListLanguages.Items);
  ListLanguages.Font.Color := MainTexture.ColorMark;
  ListKeyBindings.Font.Color := MainTexture.ColorMark;
  LoadData;
  LocalKeyBindings.LoadToStrings(ListKeyBindings.Items);
  EditShortCutPrimary.Font.Color := MainTexture.ColorMark;
  EditShortCutSecondary.Font.Color := MainTexture.ColorMark;
end;

procedure TSettingsDlg.ListKeyBindingsSelectionChange(Sender: TObject;
  User: Boolean);
begin
  if Assigned(CurrentKeyBinding) then begin
    CurrentKeyBinding.ShortCut := TextToShortCut(EditShortCutPrimary.Text);
    CurrentKeyBinding.ShortCut2 := TextToShortCut(EditShortCutSecondary.Text);
  end;

  if ListKeyBindings.ItemIndex >= 0 then
    CurrentKeyBinding := LocalKeyBindings[ListKeyBindings.ItemIndex]
    else CurrentKeyBinding := nil;

  if Assigned(CurrentKeyBinding) then begin
    if CurrentKeyBinding.ShortCut <> 0 then
      EditShortCutPrimary.Text := ShortCutToText(CurrentKeyBinding.ShortCut)
      else EditShortCutPrimary.Text := '';
    EditShortCutPrimary.Enabled := True;
    if CurrentKeyBinding.ShortCut2 <> 0 then
      EditShortCutSecondary.Text := ShortCutToText(CurrentKeyBinding.ShortCut2)
      else EditShortCutSecondary.Text := '';
    EditShortCutSecondary.Enabled := True;
  end else begin
    EditShortCutPrimary.Text := '';
    EditShortCutPrimary.Enabled := False;
    EditShortCutSecondary.Text := '';
    EditShortCutSecondary.Enabled := False;
  end;
end;

procedure TSettingsDlg.ButtonOkClick(Sender: TObject);
begin
  SaveData;
  ModalResult := mrOk;
end;

procedure TSettingsDlg.Up2BtnClick(Sender: TObject);
begin
  if LocalGamma < 150 then begin
    Inc(LocalGamma);
    Invalidate;
  end;
end;

procedure TSettingsDlg.UpdateShortCutItem;
begin
  if Assigned(CurrentKeyBinding) then begin
    if CurrentKeyBinding.ShortCut > 0 then
      LocalKeyBindings.RemoveShortCut(CurrentKeyBinding.ShortCut);
    if CurrentKeyBinding.ShortCut2 > 0 then
      LocalKeyBindings.RemoveShortCut(CurrentKeyBinding.ShortCut2);
    CurrentKeyBinding.ShortCut := TextToShortCut(EditShortCutPrimary.Text);
    CurrentKeyBinding.ShortCut2 := TextToShortCut(EditShortCutSecondary.Text);
    LocalKeyBindings.LoadToStrings(ListKeyBindings.Items);
  end;
end;

procedure TSettingsDlg.LoadData;
begin
  StartDlg.Translator.Language := StartDlg.Translator.Languages.SearchByCode(LocaleCode);
  StartDlg.Translator.LanguageListToStrings(ListLanguages.Items, False);
  ListLanguages.ItemIndex := ListLanguages.Items.IndexOfObject(StartDlg.Translator.Language);
  if ListLanguages.ItemIndex = -1 then ListLanguages.ItemIndex := 0;
  if FullScreen then ButtonFullscreen.ButtonIndex := 3
    else ButtonFullscreen.ButtonIndex := 2;
  LocalGamma := Gamma;
  LocalKeyBindings.Assign(KeyBindings);
end;

procedure TSettingsDlg.SaveData;
var
  NeedRestart: Boolean;
begin
  NeedRestart := Gamma <> LocalGamma;
  if ListLanguages.ItemIndex <> -1 then begin
    StartDlg.Translator.Language := TLanguage(ListLanguages.Items.Objects[ListLanguages.ItemIndex]);
    LocaleCode := StartDlg.Translator.Language.Code;
  end else begin
    StartDlg.Translator.Language := nil;
    LocaleCode := '';
  end;
  FullScreen := (ButtonFullscreen.ButtonIndex and 1) = 1;
  Gamma := LocalGamma;
  if NeedRestart then SimpleMessage(SRestartMsg);
  KeyBindings.Assign(LocalKeyBindings);
end;

end.

