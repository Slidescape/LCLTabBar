{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LCLTabBar;

{$warn 5023 off : no warning about unused units}
interface

uses
  TabBar, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TabBar', @TabBar.Register);
end;

initialization
  RegisterPackage('LCLTabBar', @Register);
end.
