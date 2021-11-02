C-evo: New Horizons
===================

* Developed with: Lazarus 2.0.12 (https://www.lazarus-ide.org/)
* Supported platforms: Windows and Linux
* Supported architectures: 32-bit and 64-bit x86

= Main code changes to original source =

* Converted from Delphi to Lazarus
* Merged source code and binaries from installed game
* Graphics files converted from BMP to PNG
* Game text files .txt converted to UTF-8
* Binary .dfm files converted to text .lfm
* Removed external Configurator application written in C#. Use in-game config interface.
* Available localizations included in the installed game
* Used latest Delphi StdAI. Newer is implemented in C#.
* Added installer scripts for Windows and Ubuntu/Debian Linux.
* Design time components converted to Lazarus package (cevocomponenets.lpk)
* User configurable key bindings

= Development =

* Home page: https://app.zdechov.net/c-evo/
* Source code: https://svn.zdechov.net/c-evo/
* Developed in [http://www.lazarus-ide.org/ Lazarus/FPC] 2.0.12
* To build new Windows installer run Install/build.bat. InnoSetup (http://www.jrsoftware.org/isdl.php) needs to be installed).

== Release new version ==

* Update version in Global.pas CevoVersion constants.
* Update version in Install\win\Common.iss MyAppVersion define.
* Update version in Install\rpm\c-evo.spec Version field.
* Update version in Install\deb\control Standards-Version field.
* Build all binary installer packages and put them into bin directory.

= Original readme content =

The C-evo sources

- Please read the "Module Concept" section of the document 
  http://c-evo.org/aidev.html
  to understand the client/server architecture of the game before you 
  try to understand the rest.

- The package includes components named TButtonA, TButtonB, TButtonC,
  TButtonN and TEOTButton. You must install these before you open the 
  cevo project.

- If you're using Delphi 3, ignore the missing properties.

- The code is in the Public Domain.
