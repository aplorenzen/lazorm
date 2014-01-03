{******************************************************************************}
{                                                                              }
{  LazORM Project                                                              }
{                                                                              }
{  uloCoreTypes                                                                }
{                                                                              }
{  Description:                                                                }
{    Core types of the project. The base types TloObject and TloObjectList are }
{    the foundation of all other classes in the project. This allows logging,  }
{    configuration and threadsafety to be implemented at a lower level         }
{                                                                              }
{  Missing                                                                     }
{    -                                                                         }
{                                                                              }
{  Copyright (c) 2013 Andreas Lorenzen                                         }
{                                                                              }
{******************************************************************************}

unit ulocoretypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FGL,
  SyncObjs,
  XMLConf,
  SQLDB,

  uloCoreUtils,
  ulocoreinterfaces;

  //LCLIntf,
  //lcl,
  //LMessages;

type
  TloThreadStatus = (
    lotsReady,
    lotsRunning,
    lotsCompleted,
    lotsPaused,
    lotsTerminated);

  TloException = class(Exception);

  TloObject = class;
  TloThread = class;

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
    procedure LogWarn(const aMsg: String);
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

    {property Owner: IloObject read GetOwner write SetOwner;
    property Mutex: TCriticalSection read GetMutex write SetMutex;
    property Log: IloLogger read GetLog write SetLog;
    property Config: TXMLConfig read GetConfig write SetConfig;}
  end;

  //IloThreadObject = interface(IInterface)
  //  { DONE -oAPL -cTloThread 5: Do we need a ref to the inside thread in this interface at all? Remove if possible! }
  //  // procedure SetThread(aThread: TloThread);
  //  // function GetThread: TloThread;
  //  procedure Start;
  //  procedure Abort;
  //  procedure Pause;
  //
  //  function GetTaskProgressPercentage: SmallInt;
  //  function GetTaskTitle: String;
  //  procedure SetOnTaskStatusChangeProcedure(aThreadMethod: TThreadMethod);
  //  function GetOnTaskStatusChangeProcedure: TThreadMethod;
  //
  //  // property Thread: TloThread read GetThread write SetThread;
  //  property TaskTitle: String read GetTaskTitle;
  //  property TaskProgressPercentage: SmallInt read GetTaskProgressPercentage;
  //  property OnTaskStatusChange: TThreadMethod read GetOnTaskStatusChangeProcedure write SetOnTaskStatusChangeProcedure;
  //end;

  { TloThread }

  TloThread = class(TThread)
  private
    fOwner: IloObject;
    fOnTaskStatusChangeProcedure: TThreadMethod;
    fTaskProgressPercentage: SmallInt;
    fTaskTitle: String;
    fTaskStatus: TloThreadStatus;

    fMutex: TCriticalSection;
    fLog: IloLogger;
    fConfig: TXMLConfig;

    procedure SetTaskProgressPercentage(aPercentage: SmallInt);
    function GetTaskProgressPercentage: SmallInt;
    procedure SetTaskTitle(aTaskTitle: String);
    function GetTaskTitle: String;
    procedure SetTaskStatus(aTaskStatus: TloThreadStatus);
    function GetTaskStatus: TloThreadStatus;

    { TODO -oAPL -cTloThread 4: Need to implement logging functions, and sync the calls to them (use local var to hold log message from parameterized log function }
  public
    constructor Create(
      aOwner: IloObject;
      aOnTaskStatusChangeProcedure: TThreadMethod;
      aCreateSuspended: Boolean = True;
      aFreeOnTerminate: Boolean = False;
      const aStackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;

    procedure DoTerminate; override;
    procedure Execute; override; abstract;

    property TaskProgressPercentage: SmallInt read GetTaskProgressPercentage write SetTaskProgressPercentage;
    property TaskStatus: TloThreadStatus read GetTaskStatus write SetTaskStatus;
    property TaskTitle: String read GetTaskTitle write SetTaskTitle;

    property Mutex: TCriticalSection read fMutex;
    property Log: IloLogger read fLog;
    property Config: TXMLConfig read fConfig;
  end;

  { TloThreadObject }

  //TloThreadObject = class(TloObject, IloThreadObject)
  //private
  //  fThread: TloThread;
  //  fOnTaskStatusChangeProcedure: TThreadMethod;
  //  fThreadStatus: TloThreadStatus;
  //
  //  // procedure SetThread(aThread: TloThread);
  //  // function GetThread: TloThread;
  //  function GetTaskProgressPercentage: SmallInt;
  //  function GetTaskTitle: String;
  //  procedure SetOnTaskStatusChangeProcedure(aOnTaskStatusChangeProcedure: TThreadMethod);
  //  function GetOnTaskStatusChangeProcedure: TThreadMethod;
  //public
  //  constructor Create(
  //    aOwner: IloObject;
  //    aOnTaskStatusChangeProcedure: TThreadMethod;
  //    aLog: IloLogger = nil;
  //    aConfig: TXMLConfig = nil;
  //    aMutex: TCriticalSection = nil);
  //  destructor Destroy; override;
  //
  //  procedure Start;
  //  procedure Abort;
  //  procedure Pause;
  //end;

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

procedure TloThread.SetTaskProgressPercentage(aPercentage: SmallInt);
begin
  fTaskProgressPercentage := aPercentage;

  if Assigned(fOnTaskStatusChangeProcedure) then
    Synchronize(fOnTaskStatusChangeProcedure);
end;

function TloThread.GetTaskProgressPercentage: SmallInt;
begin
  Result := fTaskProgressPercentage;
end;

procedure TloThread.SetTaskTitle(aTaskTitle: String);
begin
  fTaskTitle := aTaskTitle;

  if Assigned(fOnTaskStatusChangeProcedure) then
    Synchronize(fOnTaskStatusChangeProcedure);
end;

function TloThread.GetTaskTitle: String;
begin
  Result := fTaskTitle;
end;

procedure TloThread.SetTaskStatus(aTaskStatus: TloThreadStatus);
begin
  fTaskStatus := aTaskStatus;

  if Assigned(fOnTaskStatusChangeProcedure) then
    Synchronize(fOnTaskStatusChangeProcedure);
end;

function TloThread.GetTaskStatus: TloThreadStatus;
begin
  Result := fTaskStatus;
end;

constructor TloThread.Create(aOwner: IloObject; aOnTaskStatusChangeProcedure: TThreadMethod; aCreateSuspended: Boolean; aFreeOnTerminate: Boolean;
  const aStackSize: SizeUInt);
begin
  inherited Create(
    aCreateSuspended,
    aStackSize);

  fOwner := aOwner;

  if Assigned(aOwner) then
  begin
    fLog := aOwner.GetLog;
    fMutex := aOwner.GetMutex;
    fConfig := aOwner.GetConfig;
  end;

  FreeOnTerminate := aFreeOnTerminate;
  fOnTaskStatusChangeProcedure := aOnTaskStatusChangeProcedure;
end;

destructor TloThread.Destroy;
begin
  inherited Destroy;
end;

procedure TloThread.DoTerminate;
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
//  fThread := TloThread.Create(Self);
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

procedure TloObject.LogWarn(const AMsg: String);
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

