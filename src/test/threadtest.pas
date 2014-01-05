unit threadtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  SyncObjs,
  XMLConf,

  uloCoreInterfaces,
  uloCoreTypes;

type

  { TloTestThread }

  TloTestThread = class(TloTaskThread)
  public
    constructor Create(
      aOwner: IloObject;
      aOnTaskStatusChangeProcedure: TThreadMethod;
      aCreateSuspended: Boolean = True;
      aFreeOnTerminate: Boolean = False;
      const aStackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;

    procedure Execute; override;
  end;


  { TForm2 }

  TForm2 = class(TForm, IloObject)
    StartButton: TButton;
    TerminateButton: TButton;
    PauseButton: TButton;
    TaskInfoLabel: TLabel;
    TaskProgressBar: TProgressBar;
    procedure FormDestroy(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
  private
    fThread: TloTestThread;
    procedure UpdateTaskStatus;
  public
    { public declarations }
    function GetOwner: IloObject;
    function GetMutex: TCriticalSection;
    function GetLog: IloLogger;
    function GetConfig: TXMLConfig;
    procedure SetOwner(aOwner: IloObject);
    procedure SetMutex(aMutex: TCriticalSection);
    procedure SetLog(aLog: IloLogger);
    procedure SetConfig(aConfig: TXMLConfig);
  end;

var
  Form2: TForm2;

implementation

{ TForm2 }

procedure TForm2.StartButtonClick(Sender: TObject);
begin
  if not Assigned(fThread) or (fThread = nil) then
  begin
    fThread := TloTestThread.Create(
      Self,
      @UpdateTaskStatus,
      True,
      True);

    fThread.Start;
  end
  else
  begin
    if Assigned(fThread) then
      fThread.Free;

    fThread := TloTestThread.Create(
      Self,
      @UpdateTaskStatus,
      True,
      True);

    fThread.Start;
  end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  if Assigned(fThread) then
    fThread.Free;
end;

procedure TForm2.UpdateTaskStatus;
begin
  //if Assigned(fThread) then
  //begin
  //  TaskInfoLabel.Caption := fThread.TaskTitle;
  //  TaskProgressBar.Position := fThread.TaskProgressPercentage;
  //  TaskProgressBar.Caption:=fThread.TaskTitle;
  //
  //  if fThread.TaskStatus = loThreadCompleted then
  //    fThread := nil;
  //end;
end;

function TForm2.GetOwner: IloObject;
begin
  Result := nil;
end;

function TForm2.GetMutex: TCriticalSection;
begin
  Result := nil;
end;

function TForm2.GetLog: IloLogger;
begin
  Result := nil;
end;

function TForm2.GetConfig: TXMLConfig;
begin
  Result := nil;
end;

procedure TForm2.SetOwner(aOwner: IloObject);
begin

end;

procedure TForm2.SetMutex(aMutex: TCriticalSection);
begin

end;

procedure TForm2.SetLog(aLog: IloLogger);
begin

end;

procedure TForm2.SetConfig(aConfig: TXMLConfig);
begin

end;

{$R *.lfm}

{ TloTestThread }

constructor TloTestThread.Create(aOwner: IloObject; aOnTaskStatusChangeProcedure: TThreadMethod; aCreateSuspended: Boolean; aFreeOnTerminate: Boolean;
  const aStackSize: SizeUInt);
begin
  inherited Create(
    aOwner,
    aOnTaskStatusChangeProcedure,
    aCreateSuspended,
    aFreeOnTerminate,
    aStackSize);
end;

destructor TloTestThread.Destroy;
begin
  inherited Destroy;
end;

procedure TloTestThread.Execute;
const
  total = 10000;
var
  i: Integer;
begin
  TaskStatus := loThreadRunning;

  for i := 0 to total do
  begin
    TaskProgressPercentage := Round(i / (total / 100));
    TaskTitle := Format('Processing %d of %d...', [i, total]);
  end;

  TaskTitle := Format('Done processing %d items!', [total]);
  TaskStatus := loThreadCompleted;
end;

end.

