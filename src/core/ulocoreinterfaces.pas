unit ulocoreinterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  XMLConf,
  SQLDB;

type

  { IloLogger }

  IloLogger = interface(IInterface)
    ['{354AB046-EA70-448C-9582-18F7C2E462E7}']
    procedure Fatal(aMsg: String);
    procedure Error(aMsg: String);
    procedure Warning(aMsg: String);
    procedure Info(aMsg: String);
    procedure Debug(aMsg: String);
    procedure Trace(aMsg: String);
  end;

  { IloObject }

  IloObject = interface(IInterface)
    ['{CC0C9225-C547-4450-9B7C-4777A0C19DE1}']
    function GetOwner: IloObject;
    function GetMutex: TCriticalSection;
    function GetLog: IloLogger;
    function GetConfig: TXMLConfig;
    procedure SetOwner(aOwner: IloObject);
    procedure SetMutex(aMutex: TCriticalSection);
    procedure SetLog(aLog: IloLogger);
    procedure SetConfig(aConfig: TXMLConfig);

    property Owner: IloObject read GetOwner write SetOwner;
    property Mutex: TCriticalSection read GetMutex write SetMutex;
    property Log: IloLogger read GetLog write SetLog;
    property Config: TXMLConfig read GetConfig write SetConfig;
  end;

  { IloDatabaseObject }

  IloDatabaseObject = interface(IInterface)
    ['{51145759-F1DE-42C9-9308-EEE0344DE147}']
    function GetConnection: TSQLConnector;
    procedure SetConnection(aConnection: TSQLConnector);

    property Connection: TSQLConnector read GetConnection write SetConnection;
  end;

  IloWorker = interface(IInterface)
    {WIP}
    ['{58FD8735-A22A-42C5-B107-1B9D1534E7A2}']
    function GetWorkThread: TThread;
    procedure SetWorkThread(aThread: TThread);

    function GetWorkTitle: String;
    function GetWorkProgressPercentage: Integer;
    function GetWorkDescription: String;
    procedure AbortWork;

    function GetWorkMethod: TThreadMethod;

    property WorkMethod: TThreadMethod read GetWorkMethod;
    property WorkThread: TThread read GetWorkThread write SetWorkThread;
  end;

  IloWorkMonitor = interface(IInterface)
    ['{64B8DA98-C3F8-434D-84A5-A517690AFE0D}']
    function GetStatusUpdateMethod: TThreadMethod;

    property StatusUpdateMethod: TThreadMethod read GetStatusUpdateMethod;
  end;

implementation

end.

