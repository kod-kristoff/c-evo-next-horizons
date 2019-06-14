unit Locale;

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

  { TLocaleDlg }

  TLocaleDlg = class(TDrawDlg)
    ButtonFullscreen: TButtonC;
    List: TListBox;
    OKBtn: TButtonA;
    CancelBtn: TButtonA;
    procedure ButtonFullscreenClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    Languages: TLanguages;
  end;

var
  LocaleDlg: TLocaleDlg;

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

{ TLocaleDlg }

procedure TLocaleDlg.FormCreate(Sender: TObject);
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
  OkBtn.Graphic := GrExt[HGrSystem].Data;
  CancelBtn.Graphic := GrExt[HGrSystem].Data;

  ButtonFullscreen.Graphic := GrExt[HGrSystem].Data;
  if FullScreen then ButtonFullscreen.ButtonIndex := 3
    else ButtonFullscreen.ButtonIndex := 2;
end;

procedure TLocaleDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TLocaleDlg.ButtonFullscreenClick(Sender: TObject);
begin
  FullScreen := not FullScreen;
  ButtonFullscreen.ButtonIndex := ButtonFullscreen.ButtonIndex xor 1;
end;

procedure TLocaleDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Languages);
end;

procedure TLocaleDlg.FormPaint(Sender: TObject);
var
  S: string;
  W: Integer;
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

  s := Phrases.Lookup('SETTINGS', 0);
  LoweredTextOut(Canvas, -2, MainTexture, ButtonFullscreen.Left + 32,
    ButtonFullscreen.Top - 4, s);
end;

procedure TLocaleDlg.FormShow(Sender: TObject);
begin
  Languages.LoadToStrings(List.Items);
  List.ItemIndex := Languages.Search(LocaleCode);
  if (List.ItemIndex = -1) and (Languages.Count > 0) then
    List.ItemIndex := 0;
  List.Font.Color := MainTexture.clMark;
end;

procedure TLocaleDlg.OKBtnClick(Sender: TObject);
begin
  LocaleCode := Languages[List.ItemIndex].ShortName;
  ModalResult := mrCancel;
end;

end.

