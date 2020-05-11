@echo off

if not defined LAZDIR (
  set LAZDIR=C:\lazarus
)

set INNO_SETUP="c:\Program Files (x86)\Inno Setup 6\ISCC.exe"

rem Build AI
SET PROJECTNAME=StdAI
SET MAIN_DLL=..\..\AI\StdAI\StdAI.dll
SET WIN32_DLL=..\..\AI\StdAI\lib\i386-win32-Release\StdAI.dll
SET WIN64_DLL=..\..\AI\StdAI\lib\x86_64-win64-Release\StdAI.dll
IF EXIST %MAIN_DLL% del %MAIN_DLL%
IF EXIST %WIN32_DLL% del %WIN32_DLL%
IF EXIST %WIN64_DLL% del %WIN64_DLL%

%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=i386 --operating-system=Win32 ..\..\AI\StdAI\%PROJECTNAME%.lpi
copy %MAIN_DLL% %WIN32_DLL%
%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=x86_64 --operating-system=Win64 ..\..\AI\StdAI\%PROJECTNAME%.lpi
copy %MAIN_DLL% %WIN64_DLL%

rem Build game
SET PROJECTNAME=Integrated
SET MAIN_EXE=..\..\c-evo.exe
SET WIN32_EXE=..\..\lib\i386-win32-Release\c-evo.exe
SET WIN64_EXE=..\..\lib\x86_64-win64-Release\c-evo.exe
IF EXIST %MAIN_EXE% del %MAIN_EXE%
IF EXIST %WIN32_EXE% del %WIN32_EXE%
IF EXIST %WIN64_EXE% del %WIN64_EXE%

%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=i386 --operating-system=Win32 ..\..\%PROJECTNAME%.lpi
copy %MAIN_EXE% %WIN32_EXE%
%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=x86_64 --operating-system=Win64 ..\..\%PROJECTNAME%.lpi
copy %MAIN_EXE% %WIN64_EXE%

%INNO_SETUP% "C-evo.iss"
%INNO_SETUP% "C-evo 32-bit.iss"
%INNO_SETUP% "C-evo 64-bit.iss"
