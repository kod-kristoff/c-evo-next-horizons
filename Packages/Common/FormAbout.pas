unit FormAbout;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, ApplicationInfo, Common, Translator, Theme;

type
  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonClose: TButton;
    ButtonHomePage: TButton;
    ImageLogo: TImage;
    LabelAppName: TLabel;
    LabelDescription: TLabel;
    LabelContent: TLabel;
    PanelTop: TPanel;
    PanelButtons: TPanel;
    procedure ButtonHomePageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    AboutDialog: TObject; //TAboutDialog
    procedure UpdateInterface;
  end;


implementation

{$R *.lfm}

uses
  AboutDialog;

resourcestring
  SVersion = 'Version';
  SReleaseDate = 'Release date';
  SLicense = 'License';

{ TFormAbout }

procedure TFormAbout.FormShow(Sender: TObject);
begin
  if Assigned(AboutDialog) then
  with TAboutDialog(AboutDialog) do begin
    if Assigned(Translator) then
      Translator.TranslateComponentRecursive(Self);
    if Assigned(ThemeManager) then
      ThemeManager.UseTheme(Self);

    if Assigned(ApplicationInfo) then
    with ApplicationInfo do begin
      LabelAppName.Caption := AppName;
      LabelContent.Caption := SVersion + ': ' + Version + LineEnding +
        SReleaseDate + ': ' + DateToStr(ReleaseDate) + LineEnding +
        SLicense + ': ' + License;
      LabelDescription.Caption := Description;
      ImageLogo.Picture.Bitmap.Assign(Icon);
    end;
  end;
  UpdateInterface;
end;

procedure TFormAbout.UpdateInterface;
begin
  ButtonHomePage.Enabled := Assigned(AboutDialog) and
    Assigned(TAboutDialog(AboutDialog).ApplicationInfo);
end;

procedure TFormAbout.ButtonHomePageClick(Sender: TObject);
begin
  OpenWebPage(TAboutDialog(AboutDialog).ApplicationInfo.HomePage);
end;

end.
