{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CevoComponents;

interface

uses
  Area, ButtonA, ButtonB, ButtonC, ButtonN, EOTButton, ButtonBase, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Area', @Area.Register);
  RegisterUnit('ButtonA', @ButtonA.Register);
  RegisterUnit('ButtonB', @ButtonB.Register);
  RegisterUnit('ButtonC', @ButtonC.Register);
  RegisterUnit('ButtonN', @ButtonN.Register);
  RegisterUnit('EOTButton', @EOTButton.Register);
end;

initialization
  RegisterPackage('CevoComponents', @Register);
end.
