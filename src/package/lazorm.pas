{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazORM;

interface

uses
  loGraphPanel, loDrawGrid, loDataModelPanel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('loGraphPanel', @loGraphPanel.Register);
  RegisterUnit('loDrawGrid', @loDrawGrid.Register);
  RegisterUnit('loDataModelPanel', @loDataModelPanel.Register);
end;

initialization
  RegisterPackage('LazORM', @Register);
end.
