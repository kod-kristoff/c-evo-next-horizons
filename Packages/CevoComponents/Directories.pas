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
function GetAiDir: string;


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
    T := '';
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
    if not DirectoryExists(Result) and not FileExists(Result) then
      Result := HomeDir + Path;
  end else Result := HomeDir + Path;
end;

procedure CopyDir(SourceDir, DestinationDir, Filter: string);
var
  Src, Dst: TSearchRec;
begin
  if not DirectoryExists(DestinationDir) then ForceDirectories(DestinationDir);
  if FindFirst(SourceDir + DirectorySeparator + Filter, $21, Src) = 0 then
    repeat
      if (FindFirst(DestinationDir + DirectorySeparator + Src.Name, $21, Dst) <> 0) or
        (Dst.Time < Src.Time) then
        CopyFile(SourceDir + DirectorySeparator + Src.Name,
          DestinationDir + DirectorySeparator + Src.Name, false);
      FindClose(Dst);
    until FindNext(Src) <> 0;
  FindClose(Src);
end;

procedure CopyFiles;
begin
  if DirectoryExists(GetSavedDir(True)) and not DirectoryExists(GetSavedDir(False)) then
    CopyDir(GetSavedDir(True), GetSavedDir(False), '*.*');
  if DirectoryExists(GetMapsDir(True)) and not DirectoryExists(GetMapsDir(False)) then
    CopyDir(GetMapsDir(True), GetMapsDir(False), '*.*');
end;

procedure UnitInit;
var
  AppDataDir: string;
begin
  LocaleCode := '';
  HomeDir := ExtractFilePath(ParamStr(0));

  AppDataDir := GetAppConfigDir(False);
  if AppDataDir = '' then DataDir := HomeDir
  else begin
    if not DirectoryExists(AppDataDir) then ForceDirectories(AppDataDir);
    DataDir := AppDataDir;
  end;
  CopyFiles;
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

function GetAiDir: string;
begin
  Result := HomeDir + 'AI';
end;

end.
