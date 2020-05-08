{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CevoComponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  Area, ButtonA, ButtonB, ButtonC, ButtonN, EOTButton, ButtonBase, DrawDlg, 
  Sound, BaseWin, UPixelPointer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Area', @Area.Register);
  RegisterUnit('ButtonA', @ButtonA.Register);
  RegisterUnit('ButtonB', @ButtonB.Register);
  RegisterUnit('ButtonC', @ButtonC.Register);
  RegisterUnit('ButtonN', @ButtonN.Register);
  RegisterUnit('EOTButton', @EOTButton.Register);
  RegisterUnit('DrawDlg', @DrawDlg.Register);
  RegisterUnit('BaseWin', @BaseWin.Register);
end;

initialization
  RegisterPackage('CevoComponents', @Register);
end.
