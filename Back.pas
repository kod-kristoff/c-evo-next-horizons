{$INCLUDE Switches.inc}
unit Back;

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Forms, Controls;

type

  { TBackground }

  TBackground = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Img: TBitmap;
  end;

var
  Background: TBackground;

implementation

uses
  Directories, ScreenTools, Start;

{$R *.lfm}

procedure TBackground.FormCreate(Sender: TObject);
begin
  Img := nil;
end;

procedure TBackground.FormShow(Sender: TObject);
var
  FileName: string;
begin
  if FullScreen then begin
    if not Assigned(Img) then begin
      FileName := GetGraphicsDir + DirectorySeparator + 'Background.png';
      if FileExists(FileName) then begin
        Img := TBitmap.Create;
        LoadGraphicFile(img, FileName);
      end;
    end;
  end else begin
    WindowState := wsNormal;
    Width := StartDlg.Width + 16;
    Height := StartDlg.Height + 16;
    Left := StartDlg.Left - 8;
    Top := StartDlg.Top - 8;
  end;
end;

procedure TBackground.FormDestroy(Sender: TObject);
begin
  if Assigned(Img) then FreeAndNil(Img);
end;

procedure TBackground.FormPaint(Sender: TObject);
begin
  if Assigned(Img) then
    BitBltCanvas(Canvas, Screen.Width - Img.Width - (Screen.Width - 800) *
      3 div 8, (Screen.Height - 600) div 3, Img.Width, Img.Height,
      Img.Canvas, 0, 0);
end;

procedure TBackground.FormClose(Sender: TObject; var Action: TCloseAction);
begin
end;

end.
