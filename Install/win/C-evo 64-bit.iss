#include "Common.iss"

[Setup]
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
OutputBaseFilename=Install-{#MyAppNameShort}-{#MyAppVersion}{#MyAppVersionSuffix}-win64

[Files]
Source: "{#MyAppSubDir}\lib\x86_64-win64-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Components: main
Source: "{#MyAppSubDir}\AI\StdAI\lib\x86_64-win64-Release\StdAI.dll"; DestDir: "{app}\AI\StdAI"; DestName: "StdAI-win64.dll"; Flags: ignoreversion; Components: ai\stdai
