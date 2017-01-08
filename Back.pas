{$INCLUDE Switches.pas}
unit Back;

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Forms;

type

  { TBackground }

  TBackground = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    img: TBitmap;
  end;

var
  Background: TBackground;

implementation

uses
  Directories, ScreenTools, Start;

{$R *.lfm}

procedure TBackground.FormCreate(Sender: TObject);
begin
  img := nil;
end;

procedure TBackground.FormShow(Sender: TObject);
begin
  img := nil;
  if FullScreen then
  begin
    if FileExists(HomeDir + 'Graphics' + DirectorySeparator + 'Background.bmp') or
      FileExists(HomeDir + 'Graphics' + DirectorySeparator + 'Background.png') then
    begin
      img := TBitmap.Create;
      LoadGraphicFile(img, HomeDir + 'Graphics' + DirectorySeparator + 'Background');
    end
  end
  else
  begin
    WindowState := wsNormal;
    Width := StartDlg.Width + 16;
    Height := StartDlg.Height + 16;
    Left := StartDlg.Left - 8;
    Top := StartDlg.Top - 8;
  end
end;

procedure TBackground.FormDestroy(Sender: TObject);
begin
  // TODO Why FormClose is not executed?
  if img <> nil then
    FreeAndNil(img);
end;

procedure TBackground.FormPaint(Sender: TObject);
begin
  if img <> nil then
    BitBlt(Canvas.Handle, Screen.Width - img.Width - (Screen.Width - 800) *
      3 div 8, (Screen.Height - 600) div 3, img.Width, img.Height,
      img.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TBackground.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if img <> nil then
  begin
    img.Free;
    img := nil
  end;
end;

end.
