{$INCLUDE Switches.inc}
unit Directories;

interface

var
  HomeDir, DataDir: string;
  LocaleCode: string;

function LocalizedFilePath(const Path: string): string;


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

function LocalizedFilePath(const Path: string): string;
begin
  if LocaleCode <> '' then begin
    Result := HomeDir + 'Localization' + DirectorySeparator + LocaleCode + DirectorySeparator + Path;
    if not FileExists(Result) then
      Result := HomeDir + Path;
  end else Result := HomeDir + Path;
end;

procedure InitUnit;
begin
  LocaleCode := '';
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
  if FindFirst(HomeDir + 'AppData' + DirectorySeparator + 'Saved' + DirectorySeparator + '*.cevo', $21, src) = 0 then
    repeat
      if (FindFirst(DataDir + 'Saved' + DirectorySeparator + src.Name, $21, dst) <> 0) or
        (dst.Time < src.Time) then
        CopyFile(PChar(HomeDir + 'AppData' + DirectorySeparator + 'Saved' + DirectorySeparator + src.Name),
          PChar(DataDir + 'Saved' + DirectorySeparator + src.Name), false);
    until FindNext(src) <> 0;
end;

initialization

InitUnit;

end.
