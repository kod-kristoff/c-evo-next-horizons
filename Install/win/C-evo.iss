; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "C-evo"
#define MyAppVersion "1.2.0"
#define MyAppPublisher "Chronosoft"
#define MyAppPublisherShort "Chronosoft"
#define MyAppURL "https://app.zdechov.net/c-evo"
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
UninstallDisplayName={#MyAppName}
UninstallDisplayIcon="{app}\{#MyAppExeName}"
VersionInfoVersion={#MyAppVersion}
VersionInfoCompany={#MyAppPublisher}  
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

[Components]
Name: "main"; Description: "Main Files"; Types: full compact custom; Flags: fixed
Name: "maps"; Description: "Maps"; Types: full
Name: "lang"; Description: "Language files"; Types: full
Name: "lang\cs"; Description: "Czech"; Types: full
Name: "lang\de"; Description: "German"; Types: full
Name: "lang\it"; Description: "Italian"; Types: full
Name: "lang\ru"; Description: "Russian"; Types: full
Name: "lang\zhHans"; Description: "Chinese Simplified"; Types: full
Name: "lang\zhHant"; Description: "Chinese Traditional"; Types: full
Name: "ai"; Description: "AI files"; Types: full
Name: "ai\stdai"; Description: "StdAI"; Types: full
Name: "ai\ai_uo"; Description: "AI_UO"; Types: full; Check: not Is64BitInstallMode; 
Name: "ai\capital_ai"; Description: "Capital AI"; Types: full; Check: not Is64BitInstallMode; 
Name: "ai\aias"; Description: "AIAS"; Types: full; Check: not Is64BitInstallMode; 
Name: "ai\civseed"; Description: "Civilisation Seed AI"; Types: full; Check: not Is64BitInstallMode; 
Name: "ai\crystal"; Description: "Crystal"; Types: full; Check: not Is64BitInstallMode; 
Name: "ai\kiai"; Description: "KIAI"; Types: full; Check: not Is64BitInstallMode; 
Name: "ai\liberator"; Description: "Liberator"; Types: full; Check: not Is64BitInstallMode; 
Name: "ai\seti"; Description: "SETI"; Types: full; Check: not Is64BitInstallMode; 
Name: "ai\shah"; Description: "Shah"; Types: full; Check: not Is64BitInstallMode; 

[Files]
Source: "{#MyAppSubDir}\lib\x86_64-win64-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Check: Is64BitInstallMode; Components: main
Source: "{#MyAppSubDir}\lib\i386-win32-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Check: not Is64BitInstallMode; Components: main
Source: "{#MyAppSubDir}\Graphics\*"; DestDir: "{app}\Graphics"; Flags: ignoreversion; Components: main
Source: "{#MyAppSubDir}\Sounds\*.*"; DestDir: "{app}\Sounds"; Flags: ignoreversion; Components: main
Source: "{#MyAppSubDir}\Tribes\*.*"; DestDir: "{app}\Tribes"; Flags: ignoreversion; Components: main
Source: "{#MyAppSubDir}\Localization\cs\*.*"; DestDir: "{app}\Localization\cs"; Flags: ignoreversion recursesubdirs; Components: lang\cs
Source: "{#MyAppSubDir}\Localization\de\*.*"; DestDir: "{app}\Localization\de"; Flags: ignoreversion recursesubdirs; Components: lang\de
Source: "{#MyAppSubDir}\Localization\it\*.*"; DestDir: "{app}\Localization\it"; Flags: ignoreversion recursesubdirs; Components: lang\it
Source: "{#MyAppSubDir}\Localization\ru\*.*"; DestDir: "{app}\Localization\ru"; Flags: ignoreversion recursesubdirs; Components: lang\ru
Source: "{#MyAppSubDir}\Localization\zh-Hans\*.*"; DestDir: "{app}\Localization\zh-Hans"; Flags: ignoreversion recursesubdirs; Components: lang\zhHans
Source: "{#MyAppSubDir}\Localization\zh-Hant\*.*"; DestDir: "{app}\Localization\zh-Hant"; Flags: ignoreversion recursesubdirs; Components: lang\zhHant
Source: "{#MyAppSubDir}\Help\*.*"; DestDir: "{app}\Help"; Flags: ignoreversion; Components: main
Source: "{#MyAppSubDir}\Fonts.txt"; DestDir: "{app}"; Flags: ignoreversion; Components: main
Source: "{#MyAppSubDir}\Language.txt"; DestDir: "{app}"; Flags: ignoreversion; Components: main
Source: "{#MyAppSubDir}\Language2.txt"; DestDir: "{app}"; Flags: ignoreversion; Components: main
Source: "{#MyAppSubDir}\AI\StdAI\lib\x86_64-win64-Release\StdAI.dll"; DestDir: "{app}\AI\StdAI"; DestName: "StdAI-win64.dll"; Flags: ignoreversion; Components: ai\stdai
Source: "{#MyAppSubDir}\AI\StdAI\lib\i386-win32-Release\StdAI.dll"; DestDir: "{app}\AI\StdAI"; DestName: "StdAI-win32.dll"; Flags: ignoreversion; Components: ai\stdai
Source: "{#MyAppSubDir}\AI\StdAI\StdAI.png"; DestDir: "{app}\AI\StdAI"; Flags: ignoreversion; Components: ai\stdai
Source: "{#MyAppSubDir}\AI\StdAI\StdAI.ai.txt"; DestDir: "{app}\AI\StdAI"; Flags: ignoreversion; Components: ai\stdai
Source: "{#MyAppSubDir}\AI\AI_UO\*.*"; DestDir: "{app}\AI\AI_UO"; Flags: ignoreversion; Components: ai\ai_uo
Source: "{#MyAppSubDir}\AI\AIAS\*.*"; DestDir: "{app}\AI\AIAS"; Flags: ignoreversion; Components: ai\aias
Source: "{#MyAppSubDir}\AI\Capital AI\*.*"; DestDir: "{app}\AI\Capital AI"; Flags: ignoreversion; Components: ai\capital_ai
Source: "{#MyAppSubDir}\AI\Civilisation Seed AI\*.*"; DestDir: "{app}\AI\Civilisation Seed AI"; Flags: ignoreversion; Components: ai\civseed
Source: "{#MyAppSubDir}\AI\Crystal\*.*"; DestDir: "{app}\AI\Crystal"; Flags: ignoreversion; Components: ai\crystal
Source: "{#MyAppSubDir}\AI\KIAI\*.*"; DestDir: "{app}\AI\KIAI"; Flags: ignoreversion; Components: ai\kiai
Source: "{#MyAppSubDir}\AI\Liberator\*.*"; DestDir: "{app}\AI\Liberator"; Flags: ignoreversion; Components: ai\liberator
Source: "{#MyAppSubDir}\AI\SETI\*.*"; DestDir: "{app}\AI\SETI"; Flags: ignoreversion; Components: ai\seti
Source: "{#MyAppSubDir}\AI\Shah\*.*"; DestDir: "{app}\AI\Shah"; Flags: ignoreversion; Components: ai\shah
Source: "{#MyAppSubDir}\Maps\*.*"; DestDir: "{localappdata}\c-evo\Maps"; Flags: ignoreversion; Components: maps
Source: "{#MyAppSubDir}\Saved\*.*"; DestDir: "{localappdata}\c-evo\Saved"; Flags: ignoreversion; Components: main
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}}"; Flags: nowait postinstall skipifsilent

