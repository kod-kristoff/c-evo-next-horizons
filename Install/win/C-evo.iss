; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "C-evo"
#define MyAppVersion "1.2.0"
#define MyAppPublisher "Chronosoft"
#define MyAppPublisherShort "Chronosoft"
#define MyAppURL "http://svn.zdechov.net/trac/c-evo"
#define MyAppExeName "C-evo.exe"
#define MyAppDebugName "C-evo.dbg"
#define MyAppSubDir "../.."

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{6B40AF4D-C38C-4474-9614-8F0C4376C1CF}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=.
OutputBaseFilename=Install-{#MyAppName}-{#MyAppVersion}
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes
; "ArchitecturesInstallIn64BitMode=x64" requests that the install be
; done in "64-bit mode" on x64, meaning it should use the native
; 64-bit Program Files directory and the 64-bit view of the registry.
; On all other architectures it will install in "32-bit mode".
ArchitecturesInstallIn64BitMode=x64
; Note: We don't set ProcessorsAllowed because we want this
; installation to run on all architectures (including Itanium,
; since it's capable of running 32-bit code too).

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "cs"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
Name: "it"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags:

[Registry]
Root: HKCU; Subkey: "Software\C-evo"; Flags: uninsdeletekey

#define FileTypeName "C-evo book"
Root: HKCR; Subkey: ".cevo"; ValueType: string; ValueName: ""; ValueData: "{#FileTypeName}"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "{#FileTypeName}"; ValueType: string; ValueName: ""; ValueData: "{#FileTypeName}"; Flags: uninsdeletekey
Root: HKCR; Subkey: "{#FileTypeName}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKCR; Subkey: "{#FileTypeName}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""

[Files]
Source: "{#MyAppSubDir}\lib\x86_64-win64-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Check: Is64BitInstallMode
Source: "{#MyAppSubDir}\lib\i386-win32-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\Graphics\*"; DestDir: "{app}\Graphics"; Flags: ignoreversion
Source: "{#MyAppSubDir}\Sounds\*.*"; DestDir: "{app}\Sounds"; Flags: ignoreversion
Source: "{#MyAppSubDir}\Tribes\*.*"; DestDir: "{app}\Tribes"; Flags: ignoreversion
Source: "{#MyAppSubDir}\Localization\*.*"; DestDir: "{app}\Localization"; Flags: ignoreversion recursesubdirs
Source: "{#MyAppSubDir}\Help\*.*"; DestDir: "{app}\Help"; Flags: ignoreversion
Source: "{#MyAppSubDir}\Fonts.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#MyAppSubDir}\Language.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#MyAppSubDir}\Language2.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#MyAppSubDir}\AI\lib\x86_64-win64\StdAI.dll"; DestDir: "{app}\AI"; DestName: "StdAI-win64.dll"; Flags: ignoreversion; Check: Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\lib\i386-win32\StdAI.dll"; DestDir: "{app}\AI"; DestName: "StdAI-win32.dll"; Flags: ignoreversion; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\StdAI.png"; DestDir: "{app}\AI"; Flags: ignoreversion; 
Source: "{#MyAppSubDir}\AI\StdAI.ai.txt"; DestDir: "{app}\AI"; Flags: ignoreversion;
Source: "{#MyAppSubDir}\Maps\*.*"; DestDir: "{localappdata}\c-evo\Maps"; Flags: ignoreversion createallsubdirs recursesubdirs comparetimestamp
Source: "{#MyAppSubDir}\Saved\*.*"; DestDir: "{localappdata}\c-evo\Saved"; Flags: ignoreversion createallsubdirs recursesubdirs comparetimestamp
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}}"; Flags: nowait postinstall skipifsilent

