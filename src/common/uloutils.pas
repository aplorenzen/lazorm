unit uloUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,

  uloConstants;

function FilenameIsLazORMUnit(const AFileName: String): Boolean;

implementation

function FilenameIsLazORMUnit(const AFileName: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := Low(LazORMFileExt) to High(LazORMFileExt) do
    if CompareFileExt(AFilename, LazORMFileExt[i], False) = 0 then
      Exit(True);
end;

end.

