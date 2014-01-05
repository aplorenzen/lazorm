unit ulocoreinterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  XMLConf,
  SQLDB,

  uloCoreConstants;

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

  { IloTask }

  IloTask = interface(IInterface)
    ['{96F49351-F3E5-4097-AC14-0B241A70CA28}']
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

    function Start: Boolean;
    function Pause: Boolean;
    function Abort: Boolean;

    property OnTaskProgressPercentageChange: TThreadMethod read GetOnTaskProgressPercentageChange write SetOnTaskProgressPercentageChange;
    property OnTaskStatusChange: TThreadMethod read GetOnTaskStatusChange write SetOnTaskStatusChange;
    property OnTaskWorkDescriptionChange: TThreadMethod read GetOnTaskWorkDescriptionChange write SetOnTaskWorkDescriptionChange;
    property OnTaskNameChange: TThreadMethod read GetOnTaskNameChange write SetOnTaskNameChange;
    property OnTaskStatusUpdate: TThreadMethod read GetOnTaskStatusUpdate write SetOnTaskStatusUpdate;

    property TaskProgressPercentage: Integer read GetTaskProgressPercentage write SetTaskProgressPercentage;
    property TaskStatus: TloTaskStatus read GetTaskStatus write SetTaskStatus;
    property TaskWorkDescription: String read GetTaskWorkDescription write SetTaskWorkDescription;
    property TaskName: String read GetTaskName write SetTaskName;
  end;


implementation

end.

