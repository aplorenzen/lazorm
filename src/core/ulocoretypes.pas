{******************************************************************************}
{                                                                              }
{  LazORM Project                                                              }
{                                                                              }
{  uloCoreTypes                                                                }
{                                                                              }
{  Description:                                                                }
{                                                                              }
{  Missing                                                                     }
{    -                                                                         }
{                                                                              }
{  Copyright (c) 2013 Andreas Lorenzen                                         }
{                                                                              }
{******************************************************************************}

unit uloCoreTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FGL,
  SyncObjs,
  XMLConf,
  SQLDB,

  uloCoreConstants,
  uloCoreUtils,
  uloCoreInterfaces;

type
  TloException = class(Exception);

  TloObject = class;
  TloTaskThread = class;
  TloThreadedTask = class;
  TloTask = class;
  TloDatabaseTask = class;
  TloThreadedDatabaseTask = class;
  TloDatabaseObject = class;

  { TloObject }

  TloObject = class(TInterfacedPersistent, IloObject)
  private
    fOwner: IloObject;
    fMutex: TCriticalSection;
    fLog: IloLogger;
    fConfig: TXMLConfig;
    function GetOwner: IloObject;
    function GetMutex: TCriticalSection;
    function GetLog: IloLogger;
    function GetConfig: TXMLConfig;
    procedure SetOwner(aOwner: IloObject);
    procedure SetMutex(aMutex: TCriticalSection);
    procedure SetLog(aLog: IloLogger);
    procedure SetConfig(aConfig: TXMLConfig);
  protected
    procedure LogFatal(const aMsg: String);
    procedure LogError(const aMsg: String);
    procedure LogWarning(const aMsg: String);
    procedure LogInfo(const aMsg: String);
    procedure LogDebug(const aMsg: String);
    procedure LogTrace(const aMsg: String);
    procedure MutexEnter;
    procedure MutexExit;
  public
    constructor Create(
      aOwner: IloObject;
      aLog: IloLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;

    property Owner: IloObject read GetOwner write SetOwner;
    property Mutex: TCriticalSection read GetMutex write SetMutex;
    property Log: IloLogger read GetLog write SetLog;
    property Config: TXMLConfig read GetConfig write SetConfig;
  end;

  { TloTaskThread }

  TloTaskThread = class(TThread)
  private
    fOwner: TloThreadedTask;
    fMutex: TCriticalSection;
    fLog: IloLogger;
    fConfig: TXMLConfig;

    fTaskProgressPercentage: Integer;
    fTaskStatus: TloTaskStatus;
    fTaskWorkDescription: String;
    fTaskName: String;

    fOnTaskProgressPercentageChange: TThreadMethod;
    fOnTaskStatusChange: TThreadMethod;
    fOnTaskWorkDescriptionChange: TThreadMethod;
    fOnTaskNameChange: TThreadMethod;

    fLogMsg: String;

    procedure SetTaskProgressPercentage(aTaskProgressPercentage: Integer);
    function GetTaskProgressPercentage: Integer;
    procedure SetTaskStatus(aTaskStatus: TloTaskStatus);
    function GetTaskStatus: TloTaskStatus;
    procedure SetTaskWorkDescription(aTaskWorkDescription: String);
    function GetTaskWorkDescription: String;
    procedure SetTaskName(aTaskName: String);
    function GetTaskName: String;

    { DONE -oAPL -cTloThread 4: Need to implement logging functions, and sync the calls to them (use local var to hold log message from parameterized log function }
    procedure LogFatal();
    procedure LogError();
    procedure LogWarning();
    procedure LogInfo();
    procedure LogDebug();
    procedure LogTrace();
  protected
    procedure LogFatal(const aMsg: String);
    procedure LogError(const aMsg: String);
    procedure LogWarning(const aMsg: String);
    procedure LogInfo(const aMsg: String);
    procedure LogDebug(const aMsg: String);
    procedure LogTrace(const aMsg: String);
  public
    constructor Create(
      aOwner: TloThreadedTask;
      aCreateSuspended: Boolean = True;
      aFreeOnTerminate: Boolean = False;
      const aStackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;

    procedure DoTerminate; override;
    procedure Execute; override; abstract;

    property TaskProgressPercentage: Integer read GetTaskProgressPercentage write SetTaskProgressPercentage;
    property TaskStatus: TloTaskStatus read GetTaskStatus write SetTaskStatus;
    property TaskWorkDescription: String read GetTaskWorkDescription write SetTaskWorkDescription;
    property TaskName: String read GetTaskName write SetTaskName;

    property OnTaskProgressPercentageChange: TThreadMethod read fOnTaskProgressPercentageChange write fOnTaskProgressPercentageChange;
    property OnTaskStatusChange: TThreadMethod read fOnTaskStatusChange write fOnTaskStatusChange;
    property OnTaskWorkDescriptionChange: TThreadMethod read fOnTaskWorkDescriptionChange write fOnTaskWorkDescriptionChange;
    property OnTaskNameChange: TThreadMethod read fOnTaskNameChange write fOnTaskNameChange;

    property Mutex: TCriticalSection read fMutex;
    property Log: IloLogger read fLog;
    property Config: TXMLConfig read fConfig;
  end;

  { TloTask }

  TloTask = class(TloObject, IloTask)
  private
    fOnTaskProgressPercentageChange: TThreadMethod;
    fOnTaskStatusChange: TThreadMethod;
    fOnTaskWorkDescriptionChange: TThreadMethod;
    fOnTaskNameChange: TThreadMethod;
    fOnTaskStatusUpdate: TThreadMethod;

    fTaskProgressPercentage: Integer;
    fTaskStatus: TloTaskStatus;
    fTaskWorkDescription: String;
    fTaskName: String;

    function GetOnTaskNameChange: TThreadMethod;
    function GetOnTaskProgressPercentageChange: TThreadMethod;
    function GetOnTaskStatusChange: TThreadMethod;
    function GetOnTaskStatusUpdate: TThreadMethod;
    function GetOnTaskWorkDescriptionChange: TThreadMethod;
    procedure SetOnTaskNameChange(aOnTaskNameChange: TThreadMethod);
    procedure SetOnTaskProgressPercentageChange(aOnTaskProgressPercentageChange: TThreadMethod);
    procedure SetOnTaskStatusChange(aOnTaskStatusChange: TThreadMethod);
    procedure SetOnTaskStatusUpdate(aOnTaskStatusUpdate: TThreadMethod);
    procedure SetOnTaskWorkDescriptionChange(aOnTaskWorkDescriptionChange: TThreadMethod);

    function GetTaskProgressPercentage: Integer;
    procedure SetTaskProgressPercentage(aTaskProgressPercentage: Integer);
    function GetTaskStatus: TloTaskStatus;
    procedure SetTaskStatus(aTaskStatus: TloTaskStatus);
    function GetTaskWorkDescription: String;
    procedure SetTaskWorkDescription(aTaskWorkDescription: String);
    function GetTaskName: String;
    procedure SetTaskName(aTaskName: String);
  public
    constructor Create(
      aOwner: IloObject;
      aTaskName: String = 'Unnamed task';
      aLog: IloLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;

    function Start: Boolean; virtual; abstract;
    function Pause: Boolean; virtual; abstract;
    function Abort: Boolean; virtual; abstract;

    property OnTaskProgressPercentageChange: TThreadMethod read GetOnTaskProgressPercentageChange write SetOnTaskProgressPercentageChange;
    property OnTaskStatusChange: TThreadMethod read GetOnTaskStatusChange write SetOnTaskStatusChange;
    property OnTaskWorkDescriptionChange: TThreadMethod read GetOnTaskWorkDescriptionChange write SetOnTaskWorkDescriptionChange;
    property OnTaskNameChange: TThreadMethod read GetOnTaskNameChange write SetOnTaskNameChange;
    property OnTaskStatusUpdate: TThreadMethod read GetOnTaskStatusUpdate write SetOnTaskStatusUpdate;
  end;

  { TloDatabaseTask }

  TloDatabaseTask = class(TloTask, IloDatabaseObject)
  private
    fConnection: TSQLConnector;

    function GetConnection: TSQLConnector;
    procedure SetConnection(aConnection: TSQLConnector);
  public
    constructor Create(
      aOwner: IloObject;
      aTaskName: String = 'Unnamed database task';
      aConnection: TSQLConnector = nil;
      aLog: IloLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;

    function Start: Boolean; override; abstract;
    function Pause: Boolean; override; abstract;
    function Abort: Boolean; override; abstract;

    property Connection: TSQLConnector read GetConnection write SetConnection;
  end;

  { TloThreadedTask }

  TloThreadedTask = class(TloTask)
  private
    fTaskThread: TloTaskThread;

    function GetTaskThread: TloTaskThread;
    procedure SetTaskThread(aTaskThread: TloTaskThread);
  public
    procedure OnTaskStatusChanged;
    procedure OnTaskWorkDescriptionChanged;
    procedure OnTaskNameChanged;
    procedure OnTaskProgressPercentageChanged;

    function Start: Boolean; override; abstract;
    function Pause: Boolean; override; abstract;
    function Abort: Boolean; override; abstract;

    property TaskThread: TloTaskThread read GetTaskThread write SetTaskThread;
  end;

  { TloThreadedDatabaseTask }

  TloThreadedDatabaseTask = class(TloThreadedTask, IloDatabaseObject)
  private
    fConnection: TSQLConnector;

    function GetConnection: TSQLConnector;
    procedure SetConnection(aConnection: TSQLConnector);
  public
    constructor Create(
      aOwner: IloObject;
      aTaskName: String = 'Unnamed database task';
      aConnection: TSQLConnector = nil;
      aLog: IloLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;

    function Start: Boolean; override; abstract;
    function Pause: Boolean; override; abstract;
    function Abort: Boolean; override; abstract;

    property Connection: TSQLConnector read GetConnection write SetConnection;
  end;


  { TloDatabaseObject }

  TloDatabaseObject = class(TloObject, IloDatabaseObject)
  private
    fConnection: TSQLConnector;
    function GetConnection: TSQLConnector;
    procedure SetConnection(aConnection: TSQLConnector);
  public
    constructor Create(
      aOwner: IloObject;
      aConnection: TSQLConnector = nil;
      aLog: IloLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;

    property Connection: TSQLConnector read GetConnection write SetConnection;
  end;

implementation

{ TloThreadedDatabaseTask }

function TloThreadedDatabaseTask.GetConnection: TSQLConnector;
begin
  Result := fConnection;
end;

procedure TloThreadedDatabaseTask.SetConnection(aConnection: TSQLConnector);
begin
  fConnection := aConnection;
end;

constructor TloThreadedDatabaseTask.Create(aOwner: IloObject; aTaskName: String; aConnection: TSQLConnector; aLog: IloLogger; aConfig: TXMLConfig;
  aMutex: TCriticalSection);
var
  lDatabaseObject: IloDatabaseObject;
begin
  inherited Create(
    aOwner,
    aTaskName,
    aLog,
    aConfig,
    aMutex);

  if Assigned(aOwner) then
  begin
    // aOwner and aConnection parameters are assigned, take the aConnection parameter for the local objects fConnection
    if Assigned(aConnection) then
      fConnection := aConnection
    else
      // aConnection not assigned, but aOwner is, try to take Connection reference from aOwner
      if Supports(aOwner, IloDatabaseObject, lDatabaseObject) then
        fConnection := lDatabaseObject.GetConnection;
  end
  else
    // aOwner not assigned, unconditionally take the aConnection parameter for the local objects fConnection
    fConnection := aConnection
end;

destructor TloThreadedDatabaseTask.Destroy;
begin
  inherited Destroy;
end;

{ TloDatabaseTask }

function TloDatabaseTask.GetConnection: TSQLConnector;
begin
  Result := fConnection;
end;

procedure TloDatabaseTask.SetConnection(aConnection: TSQLConnector);
begin
  fConnection := aConnection;
end;

constructor TloDatabaseTask.Create(aOwner: IloObject; aTaskName: String; aConnection: TSQLConnector; aLog: IloLogger; aConfig: TXMLConfig;
  aMutex: TCriticalSection);
var
  lDatabaseObject: IloDatabaseObject;
begin
  inherited Create(
    aOwner,
    aTaskName,
    aLog,
    aConfig,
    aMutex);

  if Assigned(aOwner) then
  begin
    // aOwner and aConnection parameters are assigned, take the aConnection parameter for the local objects fConnection
    if Assigned(aConnection) then
      fConnection := aConnection
    else
      // aConnection not assigned, but aOwner is, try to take Connection reference from aOwner
      if Supports(aOwner, IloDatabaseObject, lDatabaseObject) then
        fConnection := lDatabaseObject.GetConnection;
  end
  else
    // aOwner not assigned, unconditionally take the aConnection parameter for the local objects fConnection
    fConnection := aConnection
end;

destructor TloDatabaseTask.Destroy;
begin
  inherited Destroy;
end;

{ TloTask }

function TloTask.GetOnTaskNameChange: TThreadMethod;
begin
  Result := fOnTaskNameChange;
end;

function TloTask.GetOnTaskProgressPercentageChange: TThreadMethod;
begin
  Result := fOnTaskProgressPercentageChange;
end;

function TloTask.GetOnTaskStatusChange: TThreadMethod;
begin
  Result := fOnTaskStatusChange;
end;

function TloTask.GetOnTaskStatusUpdate: TThreadMethod;
begin
  Result := fOnTaskStatusUpdate;
end;

function TloTask.GetOnTaskWorkDescriptionChange: TThreadMethod;
begin
  Result := fOnTaskWorkDescriptionChange;
end;

procedure TloTask.SetOnTaskNameChange(aOnTaskNameChange: TThreadMethod);
begin
  fOnTaskNameChange := aOnTaskNameChange;
end;

procedure TloTask.SetOnTaskProgressPercentageChange(aOnTaskProgressPercentageChange: TThreadMethod);
begin
  fOnTaskProgressPercentageChange := aOnTaskProgressPercentageChange;
end;

procedure TloTask.SetOnTaskStatusChange(aOnTaskStatusChange: TThreadMethod);
begin
  fOnTaskStatusChange := aOnTaskStatusChange;
end;

procedure TloTask.SetOnTaskStatusUpdate(aOnTaskStatusUpdate: TThreadMethod);
begin
  fOnTaskStatusUpdate := aOnTaskStatusUpdate;
end;

procedure TloTask.SetOnTaskWorkDescriptionChange(aOnTaskWorkDescriptionChange: TThreadMethod);
begin
  fOnTaskWorkDescriptionChange := aOnTaskWorkDescriptionChange;
end;

function TloTask.GetTaskProgressPercentage: Integer;
begin
  Result := fTaskProgressPercentage;
end;

procedure TloTask.SetTaskProgressPercentage(aTaskProgressPercentage: Integer);
begin
  fTaskProgressPercentage := aTaskProgressPercentage;

  if Assigned(fOnTaskProgressPercentageChange) then
    fOnTaskProgressPercentageChange();
  if Assigned(fOnTaskStatusUpdate) then
    fOnTaskStatusUpdate();
end;

function TloTask.GetTaskStatus: TloTaskStatus;
begin
  Result := fTaskStatus;
end;

procedure TloTask.SetTaskStatus(aTaskStatus: TloTaskStatus);
begin
  fTaskStatus := aTaskStatus;

  if Assigned(fOnTaskStatusChange) then
    fOnTaskStatusChange();
  if Assigned(fOnTaskStatusUpdate) then
    fOnTaskStatusUpdate();
end;

function TloTask.GetTaskWorkDescription: String;
begin
  Result := fTaskWorkDescription;
end;

procedure TloTask.SetTaskWorkDescription(aTaskWorkDescription: String);
begin
  fTaskWorkDescription := aTaskWorkDescription;

  if Assigned(fOnTaskWorkDescriptionChange) then
    fOnTaskWorkDescriptionChange();
  if Assigned(fOnTaskStatusUpdate) then
    fOnTaskStatusUpdate();
end;

function TloTask.GetTaskName: String;
begin
  Result := fTaskName;
end;

procedure TloTask.SetTaskName(aTaskName: String);
begin
  fTaskName := aTaskName;

  if Assigned(fOnTaskNameChange) then
    fOnTaskNameChange();
  if Assigned(fOnTaskStatusUpdate) then
    fOnTaskStatusUpdate();
end;

constructor TloTask.Create(aOwner: IloObject; aTaskName: String; aLog: IloLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
begin
  inherited Create(
    aOwner,
    aLog,
    aConfig,
    aMutex);

  fTaskName := aTaskName;
  fTaskStatus := loTaskReady;
end;

destructor TloTask.Destroy;
begin
  inherited Destroy;
end;

{ TloThreadedTask }

function TloThreadedTask.GetTaskThread: TloTaskThread;
begin
  Result := fTaskThread;
end;

procedure TloThreadedTask.SetTaskThread(aTaskThread: TloTaskThread);
begin
  fTaskThread := aTaskThread;

  if Assigned(fTaskThread) then
  begin
    fTaskThread.OnTaskNameChange := @OnTaskNameChanged;
    fTaskThread.OnTaskStatusChange := @OnTaskStatusChanged;
    fTaskThread.OnTaskProgressPercentageChange := @OnTaskProgressPercentageChanged;
    fTaskThread.OnTaskWorkDescriptionChange := @OnTaskWorkDescriptionChanged;
  end;
end;

procedure TloThreadedTask.OnTaskStatusChanged;
begin
  if Assigned(fTaskThread) then
    SetTaskStatus(fTaskThread.TaskStatus);
end;

procedure TloThreadedTask.OnTaskWorkDescriptionChanged;
begin
  if Assigned(fTaskThread) then
    SetTaskWorkDescription(fTaskThread.TaskWorkDescription);
end;

procedure TloThreadedTask.OnTaskNameChanged;
begin
  if Assigned(fTaskThread) then
    SetTaskName(fTaskThread.TaskName);
end;

procedure TloThreadedTask.OnTaskProgressPercentageChanged;
begin
  if Assigned(fTaskThread) then
    SetTaskProgressPercentage(fTaskThread.TaskProgressPercentage);
end;

{ TloTaskThread }

procedure TloTaskThread.SetTaskProgressPercentage(aTaskProgressPercentage: Integer);
begin
  fTaskProgressPercentage := aTaskProgressPercentage;

  if Assigned(fOnTaskProgressPercentageChange) then
    Synchronize(fOnTaskProgressPercentageChange);
end;

function TloTaskThread.GetTaskName: String;
begin
  Result := fTaskName;
end;

function TloTaskThread.GetTaskWorkDescription: String;
begin
  Result := fTaskWorkDescription;
end;

procedure TloTaskThread.SetTaskName(aTaskName: String);
begin
  fTaskName := aTaskName;

  if Assigned(fOnTaskNameChange) then
    Synchronize(fOnTaskNameChange);
end;

function TloTaskThread.GetTaskProgressPercentage: Integer;
begin
  Result := fTaskProgressPercentage;
end;

procedure TloTaskThread.SetTaskStatus(aTaskStatus: TloTaskStatus);
begin
  fTaskStatus := aTaskStatus;

  if Assigned(fOnTaskStatusChange) then
    Synchronize(fOnTaskStatusChange);
end;

function TloTaskThread.GetTaskStatus: TloTaskStatus;
begin
  Result := fTaskStatus;
end;

procedure TloTaskThread.LogFatal;
begin
  if Assigned(fLog) then
    fLog.Fatal(fLogMsg);
end;

procedure TloTaskThread.LogError;
begin
  if Assigned(fLog) then
    fLog.Error(fLogMsg);
end;

procedure TloTaskThread.LogWarning;
begin
  if Assigned(fLog) then
    fLog.Warning(fLogMsg);
end;

procedure TloTaskThread.LogInfo;
begin
  if Assigned(fLog) then
    fLog.Info(fLogMsg);
end;

procedure TloTaskThread.LogDebug;
begin
  if Assigned(fLog) then
    fLog.Debug(fLogMsg);
end;

procedure TloTaskThread.LogTrace;
begin
  if Assigned(fLog) then
    fLog.Trace(fLogMsg);
end;

procedure TloTaskThread.SetTaskWorkDescription(aTaskWorkDescription: String);
begin
  fTaskWorkDescription := aTaskWorkDescription;

  if Assigned(fOnTaskWorkDescriptionChange) then
    Synchronize(fOnTaskWorkDescriptionChange);
end;

procedure TloTaskThread.LogFatal(const aMsg: String);
begin
  fLogMsg := aMsg;
  if Assigned(fLog) then
    Synchronize(@LogFatal);
end;

procedure TloTaskThread.LogError(const aMsg: String);
begin
  fLogMsg := aMsg;
  if Assigned(fLog) then
    Synchronize(@LogError);
end;

procedure TloTaskThread.LogWarning(const aMsg: String);
begin
  fLogMsg := aMsg;
  if Assigned(fLog) then
    Synchronize(@LogWarning);
end;

procedure TloTaskThread.LogInfo(const aMsg: String);
begin
  fLogMsg := aMsg;
  if Assigned(fLog) then
    Synchronize(@LogInfo);
end;

procedure TloTaskThread.LogDebug(const aMsg: String);
begin
  fLogMsg := aMsg;
  if Assigned(fLog) then
    Synchronize(@LogDebug);
end;

procedure TloTaskThread.LogTrace(const aMsg: String);
begin
  fLogMsg := aMsg;
  if Assigned(fLog) then
    Synchronize(@LogTrace);
end;

constructor TloTaskThread.Create(aOwner: TloThreadedTask; aCreateSuspended: Boolean; aFreeOnTerminate: Boolean; const aStackSize: SizeUInt);
begin
  inherited Create(
    aCreateSuspended,
    aStackSize);

  fOwner := aOwner;
  FreeOnTerminate := aFreeOnTerminate;

  if Assigned(aOwner) then
  begin
    fLog := aOwner.GetLog;
    fMutex := aOwner.GetMutex;
    fConfig := aOwner.GetConfig;
    fOwner.TaskThread := Self;
  end;
end;

destructor TloTaskThread.Destroy;
begin
  inherited Destroy;
end;

procedure TloTaskThread.DoTerminate;
begin
  inherited DoTerminate;
end;

{ TloThreadObject }

//function TloThreadObject.GetTaskProgressPercentage: SmallInt;
//begin
//  Result := fThread.GetTaskProgressPercentage;
//end;
//
//function TloThreadObject.GetTaskTitle: String;
//begin
//  Result := fThread.GetTaskTitle;
//end;
//
//procedure TloThreadObject.SetOnTaskStatusChangeProcedure(aOnTaskStatusChangeProcedure: TThreadMethod);
//begin
//  fOnTaskStatusChangeProcedure := aOnTaskStatusChangeProcedure;
//end;
//
//function TloThreadObject.GetOnTaskStatusChangeProcedure: TThreadMethod;
//begin
//  Result := fOnTaskStatusChangeProcedure;
//end;
//
//constructor TloThreadObject.Create(aOwner: IloObject; aOnTaskStatusChangeProcedure: TThreadMethod; aLog: IloLogger; aConfig: TXMLConfig;
//  aMutex: TCriticalSection);
//begin
//  inherited Create(
//    aOwner,
//    aLog,
//    aConfig,
//    aMutex);
//
//  fThreadStatus := lotsCreated;
//  fOnTaskStatusChangeProcedure := aOnTaskStatusChangeProcedure;
//  fThread := TloTaskThread.Create(Self);
//end;
//
//destructor TloThreadObject.Destroy;
//begin
//  if Assigned(fThread) then
//    fThread.Free;
//
//  inherited Destroy;
//end;
//
//procedure TloThreadObject.Start;
//begin
//
//end;
//
//procedure TloThreadObject.Abort;
//begin
//
//end;
//
//procedure TloThreadObject.Pause;
//begin
//
//end;

{ TloDatabaseObject }

function TloDatabaseObject.GetConnection: TSQLConnector;
begin
  Result := fConnection;
end;

procedure TloDatabaseObject.SetConnection(aConnection: TSQLConnector);
begin
  fConnection := aConnection;
end;

constructor TloDatabaseObject.Create(aOwner: IloObject; aConnection: TSQLConnector; aLog: IloLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
var
  lDatabaseObject: IloDatabaseObject;
begin
  // call TloObject constructor with parameters from this constructor
  inherited Create(
    aOwner,
    aLog,
    aConfig,
    aMutex);

  if Assigned(aOwner) then
  begin
    // aOwner and aConnection parameters are assigned, take the aConnection parameter for the local objects fConnection
    if Assigned(aConnection) then
      fConnection := aConnection
    else
      // aConnection not assigned, but aOwner is, try to take Connection reference from aOwner
      if Supports(aOwner, IloDatabaseObject, lDatabaseObject) then
        fConnection := lDatabaseObject.GetConnection;
  end
  else
    // aOwner not assigned, unconditionally take the aConnection parameter for the local objects fConnection
    fConnection := aConnection
end;

destructor TloDatabaseObject.Destroy;
begin
  inherited Destroy;
end;

{ TloObject }

function TloObject.GetOwner: IloObject;
begin
  Result := fOwner;
end;

function TloObject.GetMutex: TCriticalSection;
begin
  Result := fMutex;
end;

function TloObject.GetLog: IloLogger;
begin
  Result := fLog;
end;

function TloObject.GetConfig: TXMLConfig;
begin
  Result := fConfig;
end;

procedure TloObject.SetOwner(aOwner: IloObject);
begin
  fOwner := aOwner;
end;

procedure TloObject.SetMutex(aMutex: TCriticalSection);
begin
  fMutex := aMutex;
end;

procedure TloObject.SetLog(aLog: IloLogger);
begin
  fLog := aLog;
end;

procedure TloObject.SetConfig(aConfig: TXMLConfig);
begin
  fConfig := aConfig;
end;

procedure TloObject.LogFatal(const AMsg: String);
begin
  if Assigned(fLog) then
    fLog.Fatal(AMsg);
end;

procedure TloObject.LogError(const AMsg: String);
begin
  if Assigned(fLog) then
    fLog.Error(AMsg);
end;

procedure TloObject.LogWarning(const aMsg: String);
begin
  if Assigned(fLog) then
    fLog.Warning(AMsg);
end;

procedure TloObject.LogInfo(const AMsg: String);
begin
  if Assigned(fLog) then
    fLog.Info(AMsg);
end;

procedure TloObject.LogDebug(const AMsg: String);
begin
  if Assigned(fLog) then
    fLog.Debug(AMsg);
end;

procedure TloObject.LogTrace(const AMsg: String);
begin
  if Assigned(fLog) then
    fLog.Trace(AMsg);
end;

procedure TloObject.MutexEnter;
begin
  if Assigned(fMutex) then
    fMutex.Enter;
end;

procedure TloObject.MutexExit;
begin
  if Assigned(fMutex) then
    fMutex.Leave;
end;

constructor TloObject.Create(aOwner: IloObject; aLog: IloLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
begin
  inherited Create();

  fOwner := aOwner;

  if Assigned(aOwner) then
  begin
    if Assigned(aOwner.Log) then
      fLog := aOwner.Log
    else
      fLog := aLog;

    if Assigned(aOwner.Mutex) then
      fMutex := aOwner.Mutex
    else
      fMutex := aMutex;

    if Assigned(aOwner.Config) then
      fConfig := aOwner.Config
    else
      fConfig := aConfig;
  end
  else
  begin
    fLog := aLog;
    fMutex := aMutex;
    fConfig := aConfig;
  end;
end;

destructor TloObject.Destroy;
begin
  inherited Destroy;
end;

end.

