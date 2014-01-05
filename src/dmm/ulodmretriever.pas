unit uloDMRetriever;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SQLDB,
  XMLConf,
  SyncObjs,
  FGL,

  uloCoreConstants,
  uloCoreInterfaces,
  uloCoreTypes,
  uloDMInterfaces,
  uloDMTypes;

type
   { TloDMRetriever }

   TloDMRetriever = class(TloThreadedDatabaseTask, IloDMRetriever)
   private
     fModel: TloDMModel;
     fSelection: IloDMSelection;
     fOnRetrieveCompleteProcedure: TThreadMethod;
   public
     constructor Create(
       aOwner: IloObject;
       // aSelection: IloDMSelection;
       // aOnRetrieveCompleteProcedure: TThreadMethod;
       aTaskName: String = 'Unnamed database metadata model retriever task';
       aConnection: TSQLConnector = nil;
       aLog: IloLogger = nil;
       aConfig: TXMLConfig = nil;
       aMutex: TCriticalSection = nil);

     destructor Destroy; override;

     function GetModel: TloDMModel;
     function GetSelection: IloDMSelection;
     procedure SetSelection(aSelection: IloDMSelection);
     function GetOnRetrieveCompleteProcedure: TThreadMethod;
     procedure SetOnRetrieveCompleteProcedure(aOnRetrieveCompleteProcedure: TThreadMethod);

     procedure StartRetrieve;

     function Start: Boolean; override;
     function Pause: Boolean; override;
     function Abort: Boolean; override;

     property Selection: IloDMSelection read GetSelection;
     property Model: TloDMModel read GetModel;
     property OnRetrieveCompleteProcedure: TThreadMethod read GetOnRetrieveCompleteProcedure write SetOnRetrieveCompleteProcedure;

     { TODO -oAPL -cCoreTypes 3: Move 'ConnectionTest' up to TloDatabaseObject or IloDatabaseObject? }
     // function ConnectionTest: Boolean;
     // function RetrieveDatabaseMetadataModel(aSelection: IloDMSelection): TloDMModel; virtual; abstract;
   end;

implementation

{ TloDMRetriever }

//function TloDMRetriever.ConnectionTest: Boolean;
//const
//  lProcedureName = 'ConnectionTest';
//var
//  lExceptionMessage: String;
//begin
//  MutexEnter;
//
//  LogDebug('Testing connection');
//
//  try
//    Result := False;
//
//    if not Assigned(Connection) then
//      raise TloDMException.Create(Format(
//        '[%s.%s]: Unable to connect, Connection not assigned.',
//        [ClassName, lProcedureName]));
//
//    // Test connection
//    try
//      // Attempt to open the connection
//      Connection.Open;
//      Result := Connection.Connected;
//      Connection.Close;
//
//      LogDebug('Connection ok');
//
//      except on e:Exception do
//      begin
//        // Some exception occurred when opening the connection
//        { TODO -oAPL 3 -cDMM: Log as a warning? }
//        // raise e;
//        LogWarning(Format('Exception when testing connection: %s', [e.Message]));
//
//        raise TloDMException.Create(Format(
//          '[%s.%s]: Unable to connect, exception when connecting: ' + e.Message,
//          [ClassName, lProcedureName]));
//      end;
//    end;
//
//    finally
//      MutexExit;
//  end;
//end;

function TloDMRetriever.GetModel: TloDMModel;
begin
  Result := fModel;
end;

function TloDMRetriever.GetSelection: IloDMSelection;
begin
  Result := fSelection;
end;

procedure TloDMRetriever.SetSelection(aSelection: IloDMSelection);
begin
  case GetTaskStatus of
    loTaskReady,
    loTaskCompleted,
    loTaskTerminated,
    loTaskFailed:
      fSelection := aSelection;
    else
      raise TloDMException.CreateFmt('[%s]: Cannot set selection, task already in progress.', [Self.ClassName]);
  end;
end;

function TloDMRetriever.GetOnRetrieveCompleteProcedure: TThreadMethod;
begin
  Result := fOnRetrieveCompleteProcedure;
end;

procedure TloDMRetriever.SetOnRetrieveCompleteProcedure(aOnRetrieveCompleteProcedure: TThreadMethod);
begin
  fOnRetrieveCompleteProcedure := aOnRetrieveCompleteProcedure;
end;

procedure TloDMRetriever.StartRetrieve;
begin
  case GetTaskStatus of
    loTaskReady,
    loTaskCompleted,
    loTaskTerminated,
    loTaskFailed:
      Start;
    else
      raise TloDMException.CreateFmt('[%s]: Cannot start database metadata retrieve task, task already in progress.', [Self.ClassName]);
  end;
end;

function TloDMRetriever.Start: Boolean;
begin
  Result:=inherited Start;
end;

function TloDMRetriever.Pause: Boolean;
begin
  Result:=inherited Pause;
end;

function TloDMRetriever.Abort: Boolean;
begin
  Result:=inherited Abort;
end;

constructor TloDMRetriever.Create(aOwner: IloObject; aTaskName: String; aConnection: TSQLConnector; aLog: IloLogger; aConfig: TXMLConfig;
  aMutex: TCriticalSection);
const
  lProcedureName = 'Create';
begin
  inherited Create(
    aOwner,
    aTaskName,
    aConnection,
    aLog,
    aConfig,
    aMutex);

  // fSelection := aSelection;
  // fOnRetrieveCompleteProcedure := aOnRetrieveCompleteProcedure;
end;

destructor TloDMRetriever.Destroy;
const
  lProcedureName = 'Destroy';
begin
  inherited Destroy;
end;

end.

