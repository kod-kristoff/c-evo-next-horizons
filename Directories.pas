{$INCLUDE Switches.pas}
unit Directories;

interface

var
  HomeDir, DataDir: string;

function LocalizedFilePath(path: string): string;


implementation

uses
  LCLIntf, LCLType, SysUtils, FileUtil;

var
  AppDataDir: string;
  src, dst: TSearchRec;

function DirectoryExists(path: string): boolean;
var
  f: TSearchRec;
begin
  result := FindFirst(path, faDirectory, f) = 0;
end;

function LocalizedFilePath(path: string): string;
begin
  result := DataDir + 'Localization\' + path;
  if not FileExists(result) then
    result := HomeDir + path
end;

procedure InitUnit;
begin
  HomeDir := ExtractFilePath(ParamStr(0));

  AppDataDir := GetAppConfigDir(False);
  if AppDataDir = '' then
    DataDir := HomeDir
  else
  begin
    if not DirectoryExists(AppDataDir) then
      CreateDir(AppDataDir);
    DataDir := AppDataDir;
  end;
  if not DirectoryExists(DataDir + 'Saved') then
    CreateDir(DataDir + 'Saved');
  if not DirectoryExists(DataDir + 'Maps') then
    CreateDir(DataDir + 'Maps');

  // Copy appdata if not done yet
  if FindFirst(HomeDir + 'AppData\Saved\*.cevo', $21, src) = 0 then
    repeat
      if (FindFirst(DataDir + 'Saved\' + src.Name, $21, dst) <> 0) or
        (dst.Time < src.Time) then
        CopyFile(PChar(HomeDir + 'AppData\Saved\' + src.Name),
          PChar(DataDir + 'Saved\' + src.Name), false);
    until FindNext(src) <> 0;
end;

initialization

InitUnit;

end.
