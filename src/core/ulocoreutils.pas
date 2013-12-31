unit ulocoreutils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  FileUtil,

  uloCoreConstants;

{ TODO -oAPL 2 Change ToSQL to something that can work with all database implementations }
function ToSQL(aString: String): String;
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

function ToSQL(aString: String): String;
begin
  Result := '''' + StringReplace(aString, '''', '''''', [rfReplaceAll, rfIgnoreCase]) + '''';

end;

end.

