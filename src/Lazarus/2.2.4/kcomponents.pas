{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit kComponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  kEditField, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('kEditField', @kEditField.Register);
end;

initialization
  RegisterPackage('kComponents', @Register);
end.
