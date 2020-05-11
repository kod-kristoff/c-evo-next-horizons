#include "Common.iss"

[Setup]
; "ArchitecturesInstallIn64BitMode=x64" requests that the install be
; done in "64-bit mode" on x64, meaning it should use the native
; 64-bit Program Files directory and the 64-bit view of the registry.
; On all other architectures it will install in "32-bit mode".
ArchitecturesInstallIn64BitMode=x64
; Note: We don't set ProcessorsAllowed because we want this
; installation to run on all architectures (including Itanium,
; since it's capable of running 32-bit code too).
OutputBaseFilename=Install-{#MyAppName}-{#MyAppVersion}

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
Source: "{#MyAppSubDir}\lib\x86_64-win64-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Components: main; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\lib\i386-win32-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Components: main; Check: Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\StdAI\lib\i386-win32-Release\StdAI.dll"; DestDir: "{app}\AI\StdAI"; DestName: "StdAI-win32.dll"; Flags: ignoreversion; Components: ai\stdai; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\StdAI\lib\x86_64-win64-Release\StdAI.dll"; DestDir: "{app}\AI\StdAI"; DestName: "StdAI-win64.dll"; Flags: ignoreversion; Components: ai\stdai; Check: Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\AI_UO\*.*"; DestDir: "{app}\AI\AI_UO"; Flags: ignoreversion; Components: ai\ai_uo; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\AIAS\*.*"; DestDir: "{app}\AI\AIAS"; Flags: ignoreversion; Components: ai\aias; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\Capital AI\*.*"; DestDir: "{app}\AI\Capital AI"; Flags: ignoreversion; Components: ai\capital_ai; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\Civilisation Seed AI\*.*"; DestDir: "{app}\AI\Civilisation Seed AI"; Flags: ignoreversion; Components: ai\civseed; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\Crystal\*.*"; DestDir: "{app}\AI\Crystal"; Flags: ignoreversion; Components: ai\crystal; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\KIAI\*.*"; DestDir: "{app}\AI\KIAI"; Flags: ignoreversion; Components: ai\kiai; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\Liberator\*.*"; DestDir: "{app}\AI\Liberator"; Flags: ignoreversion; Components: ai\liberator; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\SETI\*.*"; DestDir: "{app}\AI\SETI"; Flags: ignoreversion; Components: ai\seti; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\AI\Shah\*.*"; DestDir: "{app}\AI\Shah"; Flags: ignoreversion; Components: ai\shah; Check: not Is64BitInstallMode
