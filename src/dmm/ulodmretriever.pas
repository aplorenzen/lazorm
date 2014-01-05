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

  uloCoreInterfaces,
  uloCoreTypes,
  uloDMInterfaces,
  uloDMTypes;

type
   { TloDMRetriever }

   TloDMRetriever = class(TloThreadedDatabaseTask, IloDMRetriever)
   private
     fModel: TloDMModel;
     fSelection: TloDMSelection;
   public
     constructor Create(
       aOwner: IloObject;
       aTaskName: String = 'Unnamed database metadata model retriever task';
       aConnection: TSQLConnector = nil;
       aLog: IloLogger = nil;
       aConfig: TXMLConfig = nil;
       aMutex: TCriticalSection = nil);

     destructor Destroy; override;

     function ConnectionTest: Boolean;

     function RetrieveDatabaseMetadataModel(aSelection: IloDMSelection): TloDMModel; virtual; abstract;

     function Start: Boolean; override; abstract;
     function Pause: Boolean; override; abstract;
     function Abort: Boolean; override; abstract;
   end;

implementation

{ TloDMRetriever }

function TloDMRetriever.ConnectionTest: Boolean;
const
  lProcedureName = 'ConnectionTest';
var
  lExceptionMessage: String;
begin
  MutexEnter;

  LogDebug('Testing connection');

  try
    Result := False;

    if not Assigned(Connection) then
      raise TloDMException.Create(Format(
        '[%s.%s]: Unable to connect, Connection not assigned.',
        [ClassName, lProcedureName]));

    // Test connection
    try
      // Attempt to open the connection
      Connection.Open;
      Result := Connection.Connected;
      Connection.Close;

      LogDebug('Connection ok');

      except on e:Exception do
      begin
        // Some exception occurred when opening the connection
        { TODO -oAPL 3 -cDMM: Log as a warning? }
        // raise e;
        LogWarning(Format('Exception when testing connection: %s', [e.Message]));

        raise TloDMException.Create(Format(
          '[%s.%s]: Unable to connect, exception when connecting: ' + e.Message,
          [ClassName, lProcedureName]));
      end;
    end;

    finally
      MutexExit;
  end;
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
end;

destructor TloDMRetriever.Destroy;
const
  lProcedureName = 'Destroy';
begin
  inherited Destroy;
end;

end.

