unit Directories;

interface

var
  HomeDir, DataDir: string;
  LocaleCode: string = '';
  LocaleCodeAuto: string = '';

function LocalizedFilePath(const Path: string): string;
procedure InitUnit;


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

procedure InitUnit;
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
    if not DirectoryExists(AppDataDir) then
      CreateDir(AppDataDir);
    DataDir := AppDataDir;
  end;
  if not DirectoryExists(DataDir + 'Saved') then
    CreateDir(DataDir + 'Saved');
  if not DirectoryExists(DataDir + 'Maps') then
    CreateDir(DataDir + 'Maps');

  // Copy appdata if not done yet
  if FindFirst(HomeDir + 'Saved' + DirectorySeparator + '*.cevo', $21, src) = 0 then
    repeat
      if (FindFirst(DataDir + 'Saved' + DirectorySeparator + src.Name, $21, dst) <> 0) or
        (dst.Time < src.Time) then
        CopyFile(PChar(HomeDir + 'Saved' + DirectorySeparator + src.Name),
          PChar(DataDir + 'Saved' + DirectorySeparator + src.Name), false);
      FindClose(dst);
    until FindNext(src) <> 0;
  FindClose(src);

  // Copy appdata if not done yet
  if FindFirst(HomeDir + 'Maps' + DirectorySeparator + '*.*', $21, src) = 0 then
    repeat
      if (FindFirst(DataDir + 'Maps' + DirectorySeparator + src.Name, $21, dst) <> 0) or
        (dst.Time < src.Time) then
        CopyFile(PChar(HomeDir + 'Maps' + DirectorySeparator + src.Name),
          PChar(DataDir + 'Maps' + DirectorySeparator + src.Name), false);
      FindClose(dst);
    until FindNext(src) <> 0;
  FindClose(src);
end;

end.