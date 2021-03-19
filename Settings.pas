unit Settings;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ScreenTools, Messg, ButtonA, Registry, fgl, Directories, DrawDlg, ButtonC;

type
  TLanguage = class
    ShortName: string;
    FullName: string;
    Author: string;
  end;

  { TLanguages }

  TLanguages = class(TFPGObjectList<TLanguage>)
    procedure AddItem(const ShortName, FullName: string);
    procedure LoadToStrings(Strings: TStrings);
    function Search(ShortName: string): Integer;
  end;

  { TSettingsDlg }

  TSettingsDlg = class(TDrawDlg)
    ButtonFullscreen: TButtonC;
    Down2Btn: TButtonC;
    List: TListBox;
    OKBtn: TButtonA;
    CancelBtn: TButtonA;
    Up2Btn: TButtonC;
    procedure ButtonFullscreenClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure Down2BtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure Up2BtnClick(Sender: TObject);
  private
    LocalGamma: Integer;
  public
    Languages: TLanguages;
    procedure LoadData;
    procedure SaveData;
  end;

var
  SettingsDlg: TSettingsDlg;

implementation

{$R *.lfm}

{ TLanguages }

procedure TLanguages.AddItem(const ShortName, FullName: string);
var
  Language: TLanguage;
begin
  Language := TLanguage.Create;
  Language.ShortName := ShortName;
  Language.FullName := FullName;
  Add(Language);
end;

procedure TLanguages.LoadToStrings(Strings: TStrings);
var
  I: Integer;
begin
  Strings.Clear;
  for I := 0 to Count - 1 do
    Strings.Add(Items[I].FullName);
end;

function TLanguages.Search(ShortName: string): Integer;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].ShortName <> ShortName) do Inc(I);
  if I < Count then Result := I
    else Result := -1;
end;

{ TSettingsDlg }

procedure TSettingsDlg.FormCreate(Sender: TObject);
begin
  Canvas.Font.Assign(UniFont[ftNormal]);
  Canvas.Brush.Style := bsClear;

  Languages := TLanguages.Create;
  Languages.AddItem('', 'System');
  Languages.AddItem('cs', 'Czech');
  Languages.AddItem('de', 'German');
  Languages.AddItem('en', 'English');
  Languages.AddItem('it', 'Italian');
  Languages.AddItem('ru', 'Russian');
  Languages.AddItem('zh-Hant', 'Traditional Chinese');
  Languages.AddItem('zh-Hans', 'Simplified Chinese');

  OKBtn.Caption := Phrases.Lookup('BTN_OK');
  CancelBtn.Caption := Phrases.Lookup('BTN_CANCEL');
  InitButtons;
end;

procedure TSettingsDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSettingsDlg.Down2BtnClick(Sender: TObject);
begin
  if LocalGamma > 50 then
  begin
    Dec(LocalGamma);
    Invalidate;
  end;
end;

procedure TSettingsDlg.ButtonFullscreenClick(Sender: TObject);
begin
  ButtonFullscreen.ButtonIndex := ButtonFullscreen.ButtonIndex xor 1;
end;

procedure TSettingsDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Languages);
end;

procedure TSettingsDlg.FormPaint(Sender: TObject);
var
  S: string;
begin
  PaintBackground(self, 3, 3, ClientWidth - 6, ClientHeight - 6);
  Frame(Canvas, 0, 0, ClientWidth - 1, ClientHeight - 1, 0, 0);
  Frame(Canvas, 1, 1, ClientWidth - 2, ClientHeight - 2,
    MainTexture.clBevelLight, MainTexture.clBevelShade);
  Frame(Canvas, 2, 2, ClientWidth - 3, ClientHeight - 3,
    MainTexture.clBevelLight, MainTexture.clBevelShade);
  EditFrame(Canvas, List.BoundsRect, MainTexture);
  BtnFrame(Canvas, OKBtn.BoundsRect, MainTexture);
  BtnFrame(Canvas, CancelBtn.BoundsRect, MainTexture);

  RFrame(Canvas, ButtonFullscreen.Left - 1, ButtonFullscreen.Top - 1,
    ButtonFullscreen.Left + 12, ButtonFullscreen.Top + 12, MainTexture.clBevelShade,
    MainTexture.clBevelLight);

  S := Phrases.Lookup('SETTINGS', 0);
  LoweredTextOut(Canvas, -2, MainTexture, ButtonFullscreen.Left + 32,
    ButtonFullscreen.Top - 4, S);

  // Gamma
  UnderlinedTitleValue(Canvas, Phrases.Lookup('SETTINGS', 1), IntToStr(LocalGamma) + '%',
    Up2Btn.Left - 150 - 4, Up2Btn.Top + 2, 150);
end;

procedure TSettingsDlg.FormShow(Sender: TObject);
begin
  Languages.LoadToStrings(List.Items);
  List.Font.Color := MainTexture.clMark;
  LoadData;
end;

procedure TSettingsDlg.OKBtnClick(Sender: TObject);
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

procedure TSettingsDlg.LoadData;
begin
  List.ItemIndex := Languages.Search(LocaleCode);
  if (List.ItemIndex = -1) and (Languages.Count > 0) then
    List.ItemIndex := 0;
  if FullScreen then ButtonFullscreen.ButtonIndex := 3
    else ButtonFullscreen.ButtonIndex := 2;
  LocalGamma := Gamma;
end;

procedure TSettingsDlg.SaveData;
var
  NeedRestart: Boolean;
begin
  NeedRestart := Gamma <> LocalGamma;
  LocaleCode := Languages[List.ItemIndex].ShortName;
  FullScreen := (ButtonFullscreen.ButtonIndex and 1) = 1;
  Gamma := LocalGamma;
  if NeedRestart then SimpleMessage(Phrases.Lookup('SETTINGS', 2));
end;

end.

