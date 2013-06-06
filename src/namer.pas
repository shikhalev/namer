{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Namer;

interface

uses
  NameEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit( 'NameEdit', @NameEdit.Register);
end;

initialization
  RegisterPackage( 'Namer', @Register);
end.
