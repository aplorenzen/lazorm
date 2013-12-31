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

  ulocoreinterfaces;

  //LCLIntf,
  //lcl,
  //LMessages;

type
  TloException = class(Exception);

  TloObject = class;

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

    property Owner: IloObject read GetOwner write SetOwner;
    property Mutex: TCriticalSection read GetMutex write SetMutex;
    property Log: IloLogger read GetLog write SetLog;
    property Config: TXMLConfig read GetConfig write SetConfig;
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

  { TloDatabaseWorkerObject }

  TloDatabaseWorkerObject = class(TloDatabaseObject, IloWorker)
  private
    fThread: TThread;
    fSyncMutex: TCriticalSection;
    fWorkTitle: String;
    fWorkProgressPercentage: Integer;
    fWorkDescription: String;
    fWorkMonitor: IloWorkMonitor;
  public
    constructor Create(
      aOwner: IloObject;
      aWorkMonitor: IloWorkMonitor;
      aConnection: TSQLConnector = nil;
      aLog: IloLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;

    function GetWorkTitle: String;
    procedure SetWorkTitle(aWorkTitle: String);
    function GetWorkProgressPercentage: Integer;
    procedure SetWorkProgressPercentage(aWorkProgressPercentage: Integer);
    function GetWorkDescription: String;
    procedure SetWorkDescription(aWorkDescription: String);
    function GetWorkThread: TThread;
    procedure SetWorkThread(aThread: TThread);

    procedure AbortWork; virtual; abstract;
    function GetWorkMethod: TThreadMethod; virtual; abstract;

    property WorkMonitor: IloWorkMonitor read fWorkMonitor write fWorkMonitor;
    property WorkTitle: String read GetWorkTitle write SetWorkTitle;
    property WorkProgressPercentage: Integer read GetWorkProgressPercentage write SetWorkProgressPercentage;
    property WorkDescription: String read GetWorkDescription write SetWorkDescription;
  end;

  { TloWorkerThread }

  TloWorkerThread = class(TThread)
  private
    fWorker: IloWorker;
  public
    constructor Create(
      aWorker: IloWorker;
      const aCreateSuspended: Boolean = False;
      const aFreeOnTerminate: Boolean = True;
      const aStackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure DoTerminate; override;
    procedure Execute; override;

    property Worker: IloWorker read fWorker write fWorker;
  end;

implementation

{ TloDatabaseWorkerObject }

constructor TloDatabaseWorkerObject.Create(aOwner: IloObject; aWorkMonitor: IloWorkMonitor; aConnection: TSQLConnector; aLog: IloLogger; aConfig: TXMLConfig;
  aMutex: TCriticalSection);
begin
  inherited Create(
    aOwner,
    aConnection,
    aLog,
    aConfig,
    aMutex);

  fWorkMonitor := aWorkMonitor;
  fSyncMutex := TCriticalSection.Create;
end;

destructor TloDatabaseWorkerObject.Destroy;
begin
  if Assigned(fSyncMutex) then
    fSyncMutex.Free;

  inherited Destroy;
end;

function TloDatabaseWorkerObject.GetWorkTitle: String;
begin
  fSyncMutex.Enter;

  try
    Result := fWorkTitle;

    finally
      fSyncMutex.Leave;
  end;
end;

procedure TloDatabaseWorkerObject.SetWorkTitle(aWorkTitle: String);
begin
  fSyncMutex.Enter;

  try
    fWorkTitle := aWorkTitle;
    WorkThread.Synchronize(WorkMonitor.StatusUpdateMethod();

    finally
      fSyncMutex.Leave;
  end;
end;

function TloDatabaseWorkerObject.GetWorkProgressPercentage: Integer;
begin
  fSyncMutex.Enter;

  try
    Result := fWorkProgressPercentage;

    finally
      fSyncMutex.Leave;
  end;
end;

procedure TloDatabaseWorkerObject.SetWorkProgressPercentage(aWorkProgressPercentage: Integer);
begin

end;

function TloDatabaseWorkerObject.GetWorkDescription: String;
begin
  fSyncMutex.Enter;

  try
    Result := fWorkDescription;

    finally
      fSyncMutex.Leave;
  end;
end;

procedure TloDatabaseWorkerObject.SetWorkDescription(aWorkDescription: String);
begin

end;

function TloDatabaseWorkerObject.GetWorkThread: TThread;
begin
  Result := fThread;
end;

procedure TloDatabaseWorkerObject.SetWorkThread(aThread: TThread);
begin
  fThread := aThread;
end;

{ TloWorkerThread }

constructor TloWorkerThread.Create(aWorker: IloWorker; const aCreateSuspended: Boolean; const aFreeOnTerminate: Boolean; const aStackSize: SizeUInt);
begin
  inherited Create(
    aCreateSuspended,
    aStackSize);

  FreeOnTerminate := aFreeOnTerminate;
  fWorker := aWorker;

  if Assigned(aWorker) then
    aWorker.SetWorkThread(Self);
end;

destructor TloWorkerThread.Destroy;
begin
  inherited Destroy;
end;

procedure TloWorkerThread.DoTerminate;
begin
  inherited DoTerminate;
  Terminated := True;
  if Assigned(fWorker) then
    fWorker.AbortWork;
end;

procedure TloWorkerThread.Execute;
begin
  if Assigned(fWorker) then
    fWorker.GetWorkMethod();
end;

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
  if Assigned(Log) then
    Log.Fatal(AMsg);
end;

procedure TloObject.LogError(const AMsg: String);
begin
  if Assigned(Log) then
    Log.Error(AMsg);
end;

procedure TloObject.LogWarn(const AMsg: String);
begin
  if Assigned(Log) then
    Log.Warning(AMsg);
end;

procedure TloObject.LogInfo(const AMsg: String);
begin
  if Assigned(Log) then
    Log.Info(AMsg);
end;

procedure TloObject.LogDebug(const AMsg: String);
begin
  if Assigned(Log) then
    Log.Debug(AMsg);
end;

procedure TloObject.LogTrace(const AMsg: String);
begin
  if Assigned(Log) then
    Log.Trace(AMsg);
end;

procedure TloObject.MutexEnter;
begin
  if Assigned(Mutex) then
    Mutex.Enter;
end;

procedure TloObject.MutexExit;
begin
  if Assigned(Mutex) then
    Mutex.Leave;
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

