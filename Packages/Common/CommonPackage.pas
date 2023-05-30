{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CommonPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  StopWatch, Common, DebugLog, Common.Delay, PrefixMultiplier, URI, Threading,
  Memory, ResetableThread, Pool, LastOpenedList, RegistryEx, JobProgressView, 
  XML, ApplicationInfo, SyncCounter, ListViewSort, PersistentForm, FindFile, 
  ScaleDPI, Theme, StringTable, MetaCanvas, Geometric, Translator, Languages, 
  FormAbout, AboutDialog, PixelPointer, DataFile, TestCase, Generics, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DebugLog', @DebugLog.Register);
  RegisterUnit('PrefixMultiplier', @PrefixMultiplier.Register);
  RegisterUnit('LastOpenedList', @LastOpenedList.Register);
  RegisterUnit('JobProgressView', @JobProgressView.Register);
  RegisterUnit('ApplicationInfo', @ApplicationInfo.Register);
  RegisterUnit('ListViewSort', @ListViewSort.Register);
  RegisterUnit('PersistentForm', @PersistentForm.Register);
  RegisterUnit('FindFile', @FindFile.Register);
  RegisterUnit('ScaleDPI', @ScaleDPI.Register);
  RegisterUnit('Theme', @Theme.Register);
  RegisterUnit('Translator', @Translator.Register);
  RegisterUnit('AboutDialog', @AboutDialog.Register);
end;

initialization
  RegisterPackage('Common', @Register);
end.
