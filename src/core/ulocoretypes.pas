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
{  TODO:                                                                       }
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

implementation

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

