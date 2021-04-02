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
  public
    procedure UpdateInterface;
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
begin
  UpdateInterface;
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

procedure TBackground.UpdateInterface;
var
  FileName: string;
begin
  if FullScreen then begin
    WindowState := wsFullScreen;
    if not Assigned(Img) then begin
      FileName := GetGraphicsDir + DirectorySeparator + 'Background.png';
      if FileExists(FileName) then begin
        Img := TBitmap.Create;
        LoadGraphicFile(img, FileName);
      end;
    end;
  end else begin
    WindowState := wsNormal;
    WindowState := wsFullScreen;
    WindowState := wsNormal;
    BoundsRect := Bounds(StartDlg.Left - 8, StartDlg.Top - 8,
      StartDlg.Width + 16, StartDlg.Height + 16);
  end;
end;

end.
