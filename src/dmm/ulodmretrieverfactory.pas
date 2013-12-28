unit ulodmretrieverfactory;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  XMLConf,
  SyncObjs,
  FGL,
  SQLDB,

  uloCoreInterfaces,
  uloCoreTypes,
  uloDMTypes,
  uloDMRetriever,
  uloDMRetriever_MSSQLServer;

type
   { TloDMRetrieverFactoryFunction }

   TloDMRetrieverFactoryFunction = function (aConnectorType: String): TloDMRetriever of object;

   { TloDMRetrieverFactoryFunctionNode }

   TloDMRetrieverFactoryFunctionNode = class(TloObject)
   private
     fFactoryFunction: TloDMRetrieverFactoryFunction;
     fConnecterType: String;
   public
     constructor Create(
       aOwner: IloObject;
       aFactoryFunction: TloDMRetrieverFactoryFunction;
       aConnecterType: String;
       aLog: IloLogger = nil;
       aConfig: TXMLConfig = nil;
       aMutex: TCriticalSection = nil);
     destructor Destroy; override;

     property FactoryFunction: TloDMRetrieverFactoryFunction read fFactoryFunction write fFactoryFunction;
     property ConnecterType: String read fConnecterType write fConnecterType;
   end;

   { TloDMRetrieverFactoryFunctionNodeList }

   TloDMRetrieverFactoryFunctionNodeListBase = specialize TFPGList<TloDMRetrieverFactoryFunctionNode>;

   TloDMRetrieverFactoryFunctionNodeList = class(TloDMRetrieverFactoryFunctionNodeListBase)
   public
     function FindFactoryFunctionByConnectorType(aConnecterType: String): TloDMRetrieverFactoryFunctionNode;
   end;

   { TloDMRetrieverFactory }

   TloDMRetrieverFactory = class(TloDatabaseObject)
   private
     fFactoryFunctionNodeList: TloDMRetrieverFactoryFunctionNodeList;

     function GetDMRetriever(aConnectorType: String): TloDMRetriever; overload;

     // Anonymous methods not available in FPC, so named functions here for creating
     // the DMRetrievers
     function CreateDMRetriever_MSSQLServer(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_Sybase(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_Firebird(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_SQLite3(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_MySQL_4_0(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_MySQL_4_1(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_MySQL_5_0(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_MySQL_5_1(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_MySQL_5_5(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_ODBC(aConnectorType: String): TloDMRetriever;
     function CreateDMRetriever_PostGreSQL(aConnectorType: String): TloDMRetriever;
   public
     constructor Create(
       aOwner: IloObject;
       aConnection: TSQLConnector = nil;
       aLog: IloLogger = nil;
       aConfig: TXMLConfig = nil;
       aMutex: TCriticalSection = nil);
     destructor Destroy; override;

     function GetDMRetriever(): TloDMRetriever; overload;
     function GetDMRetriever(aConnectionDef: TConnectionDef): TloDMRetriever; overload;
     function GetDMRetriever(aConnection: TSQLConnector): TloDMRetriever; overload;
   end;

implementation

{ TloDMRetrieverFactoryFunctionNodeList }

function TloDMRetrieverFactoryFunctionNodeList.FindFactoryFunctionByConnectorType(aConnecterType: String): TloDMRetrieverFactoryFunctionNode;
const
  lProcedureName = 'FindFactoryFunctionByConnectorType';
var
  lFactoryFunctionNode: TloDMRetrieverFactoryFunctionNode;
begin
  Result := nil;

  for lFactoryFunctionNode in Self do
    if lFactoryFunctionNode.ConnecterType = aConnecterType then
    begin
      Result := lFactoryFunctionNode;
      Exit;
    end;
end;

{ TloDMRetrieverFactoryFunctionNode }

constructor TloDMRetrieverFactoryFunctionNode.Create(aOwner: IloObject; aFactoryFunction: TloDMRetrieverFactoryFunction; aConnecterType: String;
  aLog: IloLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
const
  lProcedureName = 'Create';
begin
  inherited Create(
    aOwner,
    aLog,
    aConfig,
    aMutex);

  fFactoryFunction := aFactoryFunction;
  fConnecterType := aConnecterType;
end;

destructor TloDMRetrieverFactoryFunctionNode.Destroy;
const
  lProcedureName = 'Destroy';
begin
  inherited Destroy;
end;

{ TloDMRetrieverFactory }

function TloDMRetrieverFactory.GetDMRetriever(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'GetDMRetriever';
var
  lFactoryFunctionNode: TloDMRetrieverFactoryFunctionNode;
begin
  Result := nil;

  lFactoryFunctionNode := fFactoryFunctionNodeList.FindFactoryFunctionByConnectorType(aConnectorType);

  if Assigned(lFactoryFunctionNode) then
    Result := lFactoryFunctionNode.FactoryFunction(aConnectorType);

  if Result = nil then
    raise TloDMException.Create(Format(
      '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, unknown ConnectorType.',
      [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_MSSQLServer(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_MSSQLServer';
begin
  Result := TloDMRetriever_MSSQL.Create(Self);
end;

function TloDMRetrieverFactory.CreateDMRetriever_Sybase(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_Sybase';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_Firebird(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_Firebird';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_SQLite3(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_SQLite3';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_MySQL_4_0(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_MySQL_4_0';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_MySQL_4_1(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_MySQL_4_1';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_MySQL_5_0(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_MySQL_5_0';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_MySQL_5_1(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_MySQL_5_1';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_MySQL_5_5(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_MySQL_5_5';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_ODBC(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_ODBC';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

function TloDMRetrieverFactory.CreateDMRetriever_PostGreSQL(aConnectorType: String): TloDMRetriever;
const
  lProcedureName = 'CreateDMRetriever_PostGreSQL';
begin
  Result := nil;

  raise TloDMException.Create(Format(
    '[%s.%s]: Unable to construct TloDMRetriever, retriever for ConnectorType: %s, not yet implemented.',
    [ClassName, lProcedureName, aConnectorType]));
end;

constructor TloDMRetrieverFactory.Create(aOwner: IloObject; aConnection: TSQLConnector; aLog: IloLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
const
  lProcedureName = 'Create';
begin
  inherited Create(
    aOwner,
    aConnection,
    aLog,
    aConfig,
    aMutex);

  fFactoryFunctionNodeList := TloDMRetrieverFactoryFunctionNodeList.Create;

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_MSSQLServer,
      cConnectorType_MSSQLServer));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_Sybase,
      cConnectorType_Sybase));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_Firebird,
      cConnectorType_Firebird));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_SQLite3,
      cConnectorType_SQLite3));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_MySQL_4_0,
      cConnectorType_MySQL_4_0));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_MySQL_4_1,
      cConnectorType_MySQL_4_1));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_MySQL_5_0,
      cConnectorType_MySQL_5_0));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_MySQL_5_1,
      cConnectorType_MySQL_5_1));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_MySQL_5_5,
      cConnectorType_MySQL_5_5));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_ODBC,
      cConnectorType_ODBC));

  fFactoryFunctionNodeList.Add(
    TloDMRetrieverFactoryFunctionNode.Create(
      Self,
      @CreateDMRetriever_PostGreSQL,
      cConnectorType_PostGreSQL));

end;

destructor TloDMRetrieverFactory.Destroy;
const
  lProcedureName = 'Destroy';
var
  lFactoryFunctionNode: TloDMRetrieverFactoryFunctionNode;
begin
  if Assigned(fFactoryFunctionNodeList) then
  begin
    for lFactoryFunctionNode in fFactoryFunctionNodeList do
      lFactoryFunctionNode.Free;

    fFactoryFunctionNodeList.Free;
  end;

  inherited Destroy;
end;

function TloDMRetrieverFactory.GetDMRetriever: TloDMRetriever;
const
  lProcedureName = 'GetDMRetriever';
begin
  Result := nil;

  if not Assigned(Connection) then
    raise TloDMException.Create(Format(
      '[%s.%s]: Unable to construct DMRetriever, TloDMRetrieverFactory.Connection not assigned.',
      [ClassName, lProcedureName]))
  else
    Result := GetDMRetriever(Connection.ConnectorType);
end;

function TloDMRetrieverFactory.GetDMRetriever(aConnectionDef: TConnectionDef): TloDMRetriever;
const
  lProcedureName = 'GetDMRetriever';
begin
  Result := nil;

  if not Assigned(Connection) then
    raise TloDMException.Create(Format(
      '[%s.%s]: Unable to construct DMRetriever, TConnectionDef not assigned.',
      [ClassName, lProcedureName]))
  else
    Result := GetDMRetriever(aConnectionDef.TypeName);
end;

function TloDMRetrieverFactory.GetDMRetriever(aConnection: TSQLConnector): TloDMRetriever;
const
  lProcedureName = 'GetDMRetriever';
begin
  Result := nil;

  if not Assigned(aConnection) then
    raise TloDMException.Create(Format(
      '[%s.%s]: Unable to construct DMRetriever, TSQLConnector not assigned.',
      [ClassName, lProcedureName]))
  else
  begin
    Result := GetDMRetriever(aConnection.ConnectorType);
    Result.Connection := aConnection;
  end;
end;

end.

