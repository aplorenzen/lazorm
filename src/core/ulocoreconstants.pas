unit ulocoreconstants;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TloTaskStatus = (
    loTaskReady,
    loTaskRunning,
    loTaskCompleted,
    loTaskPaused,
    loTaskTerminated,
    loTaskFailed);

const
  LazORMFileExt: array[1..1] of string = ('.lazorm');

implementation

end.

