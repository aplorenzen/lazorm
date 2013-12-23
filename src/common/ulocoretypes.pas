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
  sysutils,
  fgl,
  syncobjs,
  TLoggerUnit,
  XMLConf,
  sqldb;


type
  TloException = class(Exception);

  TloObject = class;

  { IloObject }

  IloObject = interface(IInterface)
    ['{CC0C9225-C547-4450-9B7C-4777A0C19DE1}']
    function GetOwner: TloObject;
    function GetMutex: TCriticalSection;
    function GetLog: TLogger;
    function GetConfig: TXMLConfig;
    procedure SetMutex(aMutex: TCriticalSection);
    procedure SetLog(aLog: TLogger);
    procedure SetConfig(aConfig: TXMLConfig);

    property Owner: TloObject read GetOwner;
    property Mutex: TCriticalSection read GetMutex write SetMutex;
    property Log: TLogger read GetLog write SetLog;
    property Config: TXMLConfig read GetConfig write SetConfig;
  end;

  { IloDatabaseObject }

  IloDatabaseObject = interface(IInterface)
    ['{51145759-F1DE-42C9-9308-EEE0344DE147}']
    function GetConnection: TSQLConnector;
    procedure SetConnection(aConnection: TSQLConnector);

    property Connection: TSQLConnector read GetConnection write SetConnection;
  end;

  { TloObject }

  TloObject = class(TComponent, IloObject)
  private
    fOwner: TloObject;
    fMutex: TCriticalSection;
    fLog: TLogger;
    fConfig: TXMLConfig;

    function GetOwner: TloObject;
    function GetMutex: TCriticalSection;
    function GetLog: TLogger;
    function GetConfig: TXMLConfig;
    procedure SetMutex(aMutex: TCriticalSection);
    procedure SetLog(aLog: TLogger);
    procedure SetConfig(aConfig: TXMLConfig);
  public
    constructor Create(
      aOwner: TloObject;
      aLog: TLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;

    property Owner: TloObject read GetOwner;
    property Mutex: TCriticalSection read GetMutex write SetMutex;
    property Log: TLogger read GetLog write SetLog;
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
      aOwner: TloObject;
      aConnection: TSQLConnector = nil;
      aLog: TLogger = nil;
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

constructor TloDatabaseObject.Create(aOwner: TloObject; aConnection: TSQLConnector; aLog: TLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
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

function TloObject.GetOwner: TloObject;
begin
  Result := fOwner;
end;

function TloObject.GetMutex: TCriticalSection;
begin
  Result := fMutex;
end;

function TloObject.GetLog: TLogger;
begin
  Result := fLog;
end;

function TloObject.GetConfig: TXMLConfig;
begin
  Result := fConfig;
end;

procedure TloObject.SetMutex(aMutex: TCriticalSection);
begin
  fMutex := aMutex;
end;

procedure TloObject.SetLog(aLog: TLogger);
begin
  fLog := aLog;
end;

procedure TloObject.SetConfig(aConfig: TXMLConfig);
begin
  fConfig := aConfig;
end;

constructor TloObject.Create(aOwner: TloObject; aLog: TLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
begin
  inherited Create(aOwner);

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

