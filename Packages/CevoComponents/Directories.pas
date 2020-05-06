unit Directories;

interface

var
  HomeDir: string;
  DataDir: string;
  LocaleCode: string = '';
  LocaleCodeAuto: string = '';

function LocalizedFilePath(const Path: string): string;
procedure UnitInit;
function GetSavedDir(Home: Boolean = False): string;
function GetMapsDir(Home: Boolean = False): string;
function GetGraphicsDir: string;
function GetSoundsDir: string;


implementation

uses
  FileUtil, LCLIntf, LCLType, LCLProc, LazUTF8, SysUtils;

function GetLocale: string;
var
  Lang: string;
  I: Integer;
  T: string;
begin
  // Win32 user may decide to override locale with LANG variable.
  Lang := Copy(GetEnvironmentVariableUTF8('LANG'), 1, 2);

  if Lang = '' then begin
    for i := 1 to Paramcount - 1 do
      if (ParamStrUTF8(i) = '--LANG') or (ParamStrUTF8(i) = '-l') or
        (ParamStrUTF8(i) = '--lang') then
          Lang := ParamStrUTF8(i + 1);
  end;
  if Lang = '' then begin
    LazGetLanguageIDs(Lang, T);
    Lang := Copy(Lang, 1, 2);
  end;

  Result := Lang;
end;

function LocalizedFilePath(const Path: string): string;
var
  LocaleCodeDir: string;
begin
  if LocaleCode = '' then begin
    if LocaleCodeAuto = '' then LocaleCodeAuto := GetLocale;
    LocaleCodeDir := LocaleCodeAuto;
  end else LocaleCodeDir := LocaleCode;

  if LocaleCode <> 'en' then begin
    Result := HomeDir + 'Localization' + DirectorySeparator + LocaleCodeDir + DirectorySeparator + Path;
    if not FileExists(Result) then
      Result := HomeDir + Path;
  end else Result := HomeDir + Path;
end;

procedure UnitInit;
var
  AppDataDir: string;
  src, dst: TSearchRec;
begin
  LocaleCode := '';
  HomeDir := ExtractFilePath(ParamStr(0));

  AppDataDir := GetAppConfigDir(False);
  if AppDataDir = '' then
    DataDir := HomeDir
  else
  begin
    if not DirectoryExists(AppDataDir) then CreateDir(AppDataDir);
    DataDir := AppDataDir;
  end;
  if not DirectoryExists(GetSavedDir) then CreateDir(GetSavedDir);
  if not DirectoryExists(GetMapsDir) then CreateDir(GetMapsDir);

  // Copy appdata if not done yet
  if FindFirst(GetSavedDir(True) + DirectorySeparator + '*.cevo', $21, src) = 0 then
    repeat
      if (FindFirst(GetSavedDir(True) + DirectorySeparator + src.Name, $21, dst) <> 0) or
        (dst.Time < src.Time) then
        CopyFile(PChar(GetSavedDir(True) + DirectorySeparator + src.Name),
          PChar(GetSavedDir(True) + DirectorySeparator + src.Name), false);
      FindClose(dst);
    until FindNext(src) <> 0;
  FindClose(src);

  // Copy appdata if not done yet
  if FindFirst(GetMapsDir(True) + DirectorySeparator + '*.*', $21, src) = 0 then
    repeat
      if (FindFirst(GetMapsDir(True) + DirectorySeparator + src.Name, $21, dst) <> 0) or
        (dst.Time < src.Time) then
        CopyFile(PChar(GetMapsDir(True) + DirectorySeparator + src.Name),
          PChar(GetMapsDir(True) + DirectorySeparator + src.Name), false);
      FindClose(dst);
    until FindNext(src) <> 0;
  FindClose(src);
end;

function GetSavedDir(Home: Boolean = False): string;
begin
  if Home then Result := HomeDir + 'Saved'
    else Result := DataDir + 'Saved';
end;

function GetMapsDir(Home: Boolean = False): string;
begin
  if Home then Result := HomeDir + 'Maps'
    else Result := DataDir + 'Maps';
end;

function GetGraphicsDir: string;
begin
  Result := HomeDir + 'Graphics';
end;

function GetSoundsDir: string;
begin
  Result := HomeDir + 'Sounds';
end;

end.
