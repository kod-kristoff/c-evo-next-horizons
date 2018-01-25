@echo off

if not defined LAZDIR (
  set LAZDIR=C:\lazarus
)
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

"c:\Program Files (x86)\Inno Setup 5\ISCC.exe" "C-evo.iss"
