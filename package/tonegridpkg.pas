{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit tonegridpkg; 

interface

uses
  GraphicGrid, Notes, PasMidi, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GraphicGrid', @GraphicGrid.Register); 
end; 

initialization
  RegisterPackage('tonegridpkg', @Register); 
end.
