#include "Common.iss"

[Setup]
OutputBaseFilename=Install-{#MyAppNameShort}-{#MyAppVersion}{#MyAppVersionSuffix}-win32

[Components]
Name: "ai\ai_uo"; Description: "AI_UO"; Types: full
Name: "ai\capital_ai"; Description: "Capital AI"; Types: full
Name: "ai\aias"; Description: "AIAS"; Types: full
Name: "ai\civseed"; Description: "Civilisation Seed AI"; Types: full
Name: "ai\crystal"; Description: "Crystal"; Types: full
Name: "ai\kiai"; Description: "KIAI"; Types: full
Name: "ai\liberator"; Description: "Liberator"; Types: full
Name: "ai\seti"; Description: "SETI"; Types: full
Name: "ai\shah"; Description: "Shah"; Types: full

[Files]
Source: "{#MyAppSubDir}\lib\i386-win32-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Components: main
Source: "{#MyAppSubDir}\AI\StdAI\lib\i386-win32-Release\StdAI.dll"; DestDir: "{app}\AI\StdAI"; DestName: "StdAI-win32.dll"; Flags: ignoreversion; Components: ai\stdai
Source: "{#MyAppSubDir}\AI\AI_UO\*.*"; DestDir: "{app}\AI\AI_UO"; Flags: ignoreversion; Components: ai\ai_uo
Source: "{#MyAppSubDir}\AI\AIAS\*.*"; DestDir: "{app}\AI\AIAS"; Flags: ignoreversion; Components: ai\aias
Source: "{#MyAppSubDir}\AI\Capital AI\*.*"; DestDir: "{app}\AI\Capital AI"; Flags: ignoreversion; Components: ai\capital_ai
Source: "{#MyAppSubDir}\AI\Civilisation Seed AI\*.*"; DestDir: "{app}\AI\Civilisation Seed AI"; Flags: ignoreversion; Components: ai\civseed
Source: "{#MyAppSubDir}\AI\Crystal\*.*"; DestDir: "{app}\AI\Crystal"; Flags: ignoreversion; Components: ai\crystal
Source: "{#MyAppSubDir}\AI\KIAI\*.*"; DestDir: "{app}\AI\KIAI"; Flags: ignoreversion; Components: ai\kiai
Source: "{#MyAppSubDir}\AI\Liberator\*.*"; DestDir: "{app}\AI\Liberator"; Flags: ignoreversion; Components: ai\liberator
Source: "{#MyAppSubDir}\AI\SETI\*.*"; DestDir: "{app}\AI\SETI"; Flags: ignoreversion; Components: ai\seti
Source: "{#MyAppSubDir}\AI\Shah\*.*"; DestDir: "{app}\AI\Shah"; Flags: ignoreversion; Components: ai\shah
