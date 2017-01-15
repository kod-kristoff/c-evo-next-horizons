C-evo 1.2.0 sources ported to Lazarus/FPC

* Used development environment: Lazarus 1.6.2
* Supported platforms: Windows and Linux
* Supported architectures: 32-bit and 64-bit x86

=Code changes to original source=

* Converted from Delphi to Lazarus
* Merged source code and binaries from installed game
* Graphics files converted from BMP to PNG
* Game text files .txt converted to UTF-8
* Binary .dfm files converted to text .lfm
* Removed external Configurator application written in C#. Use ingame config interface.
* Available localizations included in installed game
* Used latest Delphi StdAI. Newer is implemented in C#.
* Added installer scripts for Windows and Ubuntu/Debian Linux.
* Design time components converted to Lazarus package (cevocomponenets.lpk)

=Original readme content=

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
