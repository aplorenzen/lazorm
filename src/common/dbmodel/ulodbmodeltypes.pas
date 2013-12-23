{******************************************************************************}
{                                                                              }
{  LazORM Project                                                              }
{                                                                              }
{  uloDatabaseMapTypes                                                         }
{                                                                              }
{  Description:                                                                }
{    This unit prvovides all the classes needed to translate the table         }
{    definitions of a database into an objectmap. The objects will describe    }
{    the tables, and can in turn be used for generating code to access the     }
{    entities fast and native.                                                 }
{                                                                              }
{  TODO:                                                                       }
{    -                                                                         }
{                                                                              }
{  Copyright (c) 2013 Andreas Lorenzen                                         }
{                                                                              }
{******************************************************************************}

unit ulodbmodeltypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  db,
  sqldb,
  fgl,
  TLoggerUnit,
  XMLConf,
  uloDatabaseTypes,
  uloCoreTypes,
  syncobjs;

type
  TloDMException = class(TloException);

  TloDMField = class;
  TloDMFieldList = class;
  TloDMTable = class;
  TloDMTableList = class;
  TloDMModel = class;

  { TloDMField }

  TloDMField = class(TloObject)
  private
    fFieldName: String;
    fFieldNo: LongInt;
    fFieldKind: TFieldKind;
    fDataType: TFieldType;
    fDataSize: String;
    fHasDefault: Boolean;
    fDefaultValue: String;
    fAttributeSet: String;
    fProviderFlags: TProviderFlags;
    fIsIndexed: Boolean;
    fIsPrimaryKey: Boolean;
    fDescription: String;
    fForeignKeyReferences: TloDMFieldList;
    fReferencingForeignKeys: TloDMFieldList;
  public
    constructor Create(
      aOwner: TloObject;
      aLog: TLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;
  published
    property FieldName: String read fFieldName write fFieldName;
    property FieldNo: LongInt read fFieldNo write fFieldNo;
    property FieldKind: TFieldKind read fFieldKind write fFieldKind;
    property DataType: TFieldType read fDataType write fDataType;
    property DataSize: String read fDataSize write fDataSize;
    property HasDefault: Boolean read fHasDefault write fHasDefault;
    property DefaultValue: String read fDefaultValue write fDefaultValue;
    property AttributeSet: String read fAttributeSet write fAttributeSet;
    property ProviderFlags: TProviderFlags read fProviderFlags write fProviderFlags;
    property IsIndexed: Boolean read fIsIndexed write fIsIndexed;
    property IsPrimaryKey: Boolean read fIsPrimaryKey write fIsPrimaryKey;
    property Description: String read fDescription write fDescription;
    property ForeignKeyReferences: TloDMFieldList read fForeignKeyReferences write fForeignKeyReferences;
    property ReferencingForeignKeys: TloDMFieldList read fReferencingForeignKeys write fReferencingForeignKeys;
  end;

  { DONE -oAPL -cDatabaseTypes 1: Complete the TloFieldList type }
  TloDMFieldList = class(specialize TFPGList<TloDMField>);

  { TloDMTable }

  TloDMTable = class(TloObject)
  private
    fTableName: String;
    fSchemeName: String;
    fCatalogName: String;
    fFieldList: TloDMFieldList;
  public
    constructor Create(
      aOwner: TloObject;
      aLog: TLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;
  published
    property TableName: String read fTableName write fTableName;
    property SchemeName: String read fSchemeName write fSchemeName;
    property CatalogName: String read fCatalogName write fCatalogName;
    property FieldList: TloDMFieldList read fFieldList write fFieldList;
  end;

  TloDMTableList = class(specialize TFPGList<TloDMTable>);

  { TloDMModel }

  TloDMModel = class(TloObject)
  private
    fTableList: TloDMTableList;
  public
    constructor Create(
      aOwner: TloObject;
      aLog: TLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;
  published
    property TableList: TloDMTableList read fTableList write fTableList;
  end;

  { IloDMSelection }

  IloDMSelection = interface(IInterface)
    ['{458128B5-697F-4645-93C6-F947343B2FC3}']

  end;

  { IloDMRetriever }

  IloDMRetriever = interface(IInterface)
    ['{E4B25D15-11BE-4D94-A0A8-721B8821B0B8}']
    function RetrieveDatabaseMetadataModel(aSelection: IloDMSelection): TloDMModel;
  end;

  { TloDMRetriever }

  TloDMRetriever = class(TloDatabaseObject, IloDMRetriever)
  public
    constructor Create(
      aOwner: TloObject;
      aConnection: TSQLConnector = nil;
      aLog: TLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;
    function RetrieveDatabaseMetadataModel(aSelection: IloDMSelection): TloDMModel; virtual; abstract;
  end;

  { TloDMRetrieverFactory }

  TloDMRetrieverFactory = class(TloDatabaseObject)
  private
    function GetDMRetriever(aConnectorType: String): TloDMRetriever; overload;
  public
    function GetDMRetriever(): TloDMRetriever; overload;
    function GetDMRetriever(aConnectionDef: TConnectionDef): TloDMRetriever; overload;
    function GetDMRetriever(aConnection: TSQLConnector): TloDMRetriever; overload;
  end;


  { TloDM_MSSQL_Retriever }

  TloDM_MSSQL_Retriever = class(TloDMRetriever)
  public
    constructor Create(
      aOwner: TloObject;
      aConnection: TSQLConnector = nil;
      aLog: TLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;

    function RetrieveDatabaseMetadataModel(aSelection: IloDMSelection): TloDMModel;
  end;


implementation

{ TloDMRetrieverFactory }

function TloDMRetrieverFactory.GetDMRetriever(aConnectorType: String): TloDMRetriever;
begin
  if aConnectorType = 'Sybase' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else if aConnectorType = 'MSSQLServer' then
    Result := TloDM_MSSQL_Retriever.Create(Owner)

  else if aConnectorType = 'Firebird' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else if aConnectorType = 'SQLite3' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else if aConnectorType = 'MySQL 4.0' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else if aConnectorType = 'MySQL 4.1' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else if aConnectorType = 'MySQL 5.0' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else if aConnectorType = 'MySQL 5.1' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else if aConnectorType = 'MySQL 5.5' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else if aConnectorType = 'ODBC' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else if aConnectorType = 'PostGreSQL' then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Not yet implemented metadata retriever for ConnectorType: ''' + aConnectorType + '''')

  else
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Unable to construct DMRetriever, unknown ConnectorType: ''' + aConnectorType + '''');
end;

function TloDMRetrieverFactory.GetDMRetriever: TloDMRetriever;
begin
  Result := nil;

  if not Assigned(Connection) then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Unable to construct DMRetriever, TloDMRetrieverFactory.Connection not assigned.')
  else
    Result := GetDMRetriever(Connection.ConnectorType);
end;

function TloDMRetrieverFactory.GetDMRetriever(aConnectionDef: TConnectionDef): TloDMRetriever;
begin
  Result := nil;

  if not Assigned(Connection) then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Unable to construct DMRetriever, TConnectionDef not assigned.')
  else
    Result := GetDMRetriever(aConnectionDef.TypeName);
end;

function TloDMRetrieverFactory.GetDMRetriever(aConnection: TSQLConnector): TloDMRetriever;
begin
  Result := nil;

  if not Assigned(aConnection) then
    raise TloDMException.Create('TloDMRetrieverFactory.GetDMRetriever:' +
      'Unable to construct DMRetriever, TSQLConnector not assigned.')
  else
    Result := GetDMRetriever(aConnection.ConnectorType);
end;

{ TloDM_MSSQL_Retriever }

constructor TloDM_MSSQL_Retriever.Create(aOwner: TloObject; aConnection: TSQLConnector; aLog: TLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
begin

end;

destructor TloDM_MSSQL_Retriever.Destroy;
begin
  inherited Destroy;
end;

function TloDM_MSSQL_Retriever.RetrieveDatabaseMetadataModel(aSelection: IloDMSelection): TloDMModel;
begin
  HERE WE ARE
  // check that connection can be made
  // check that the database exists
  // check that items in the selection exists
  //
  // 1. Create model to return
  // 2.

end;

{ TloDMRetriever }

constructor TloDMRetriever.Create(aOwner: TloObject; aConnection: TSQLConnector; aLog: TLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
begin
  inherited Create(
    aOwner,
    aConnection,
    aLog,
    aConfig,
    aMutex);
end;

destructor TloDMRetriever.Destroy;
begin
  inherited Destroy;
end;

{ TloDMModel }

constructor TloDMModel.Create(aOwner: TloObject; aLog: TLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
begin
  inherited Create(
    aOwner,
    aLog,
    aConfig,
    aMutex);

  fTableList := TloDMTableList.Create;
end;

destructor TloDMModel.Destroy;
begin
  if Assigned(fTableList) then
    fTableList.Free;

  inherited Destroy;
end;

{ TloDMMetaDataRetriever }

{procedure TloDMMetaDataRetriever.DoLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
begin
  { TODO -oAPL -cMetaDataRetriever 2: Implement this logging function }
end;}

//constructor TloDMMetaDataRetriever.Create(aSQLConnector: TSQLConnector);
//begin
//  inherited Create;
//  fSQLConnector := aSQLConnector;
//end;
//
//destructor TloDMMetaDataRetriever.Destroy;
//begin
//  inherited Destroy;
//end;
//
//function TloDMMetaDataRetriever.RetrieveTableMetaData: TloDMTableList;
//var
//  lSQLQuery: TSQLQuery;
//  lTableIndex: Integer;
//  lTable: TloDMTable;
//  lTransaction: TSQLTransaction;
//begin
//  Result := TloDMTableList.Create;
//
//  lTransaction := TSQLTransaction.Create(nil);
//  lTransaction.DataBase := fSQLConnector;
//  fSQLConnector.Transaction := lTransaction;
//
//  lSQLQuery := TSQLQuery.Create(nil);
//  lSQLQuery.Transaction := lTransaction;
//
//  try
//    // fSQLConnector.StartTransaction;
//    lSQLQuery.DataBase := fSQLConnector;
//
//    {TDBEventType = (detCustom, detPrepare, detExecute, detFetch, detCommit,detRollBack);
//    TDBEventTypes = set of TDBEventType;
//    TDBLogNotifyEvent = Procedure (Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String) of object;}
//    fSQLConnector.ExecuteDirect('USE ' + fSQLConnector.DatabaseName + ';');
//
//    fSQLConnector.LogEvents := [detCustom, detPrepare, detExecute, detFetch, detCommit, detRollBack];
//    // fSQLConnector.OnLog := DoLog;
//
//    // lSQLQuery.SQL.Add('USE ' + fSQLConnector.DatabaseName + ';');
//    lSQLQuery.SQL.Add('SELECT * FROM information_schema.tables;');
//
//    lSQLQuery.Open;
//
//    lTableIndex := 0;
//
//    if not lSQLQuery.IsEmpty then
//    begin
//      while not lSQLQuery.EOF do
//      begin
//        // ProcessEvents;
//        Inc(lTableIndex);
//        //lTable := TloDMTable.Create;
//        //
//        //with lTable do
//        //begin
//        //  fCatalogName := lSQLQuery.FieldByName('TABLE_CATALOG').AsString;
//        //  fSchemeName := lSQLQuery.FieldByName('TABLE_SCHEMA').AsString;
//        //  fTableName := lSQLQuery.FieldByName('TABLE_NAME').AsString;
//        //  // fTableType := lSQLQuery.FieldByName('TABLE_TYPE').AsString;
//        //end;
//        //
//        ////ReportProgress(1 +  + Round((7 * (lTableIndex / lTableCount))), Format('Adding table: ''%s''...', ['[' + lTableItem.TABLE_CATALOG + '].[' + lTableItem.TABLE_SCHEMA + '].[' + lTableItem.TABLE_NAME +']']));
//        //
//        //Result.Add(lTable);
//
//        lSQLQuery.Next;
//      end;
//    end;
//
//  finally
//    lSQLQuery.Free;
//  end;
//end;

{ TloDMTable }

constructor TloDMTable.Create(aOwner: TloObject; aLog: TLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
begin
  inherited Create(
    aOwner,
    aLog,
    aConfig,
    aMutex);

  fFieldList := TloDMFieldList.Create;
end;

destructor TloDMTable.Destroy;
begin
  if Assigned(fFieldList) then
    fFieldList.Free;

  inherited Destroy;
end;

{ TloDMField }

constructor TloDMField.Create(aOwner: TloObject; aLog: TLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
begin
  inherited Create(
    aOwner,
    aLog,
    aConfig,
    aMutex);

  fForeignKeyReferences := TloDMFieldList.Create;
  fReferencingForeignKeys := TloDMFieldList.Create;
end;

destructor TloDMField.Destroy;
begin
  if Assigned(fForeignKeyReferences) then
    fForeignKeyReferences.Free;
  if Assigned(fReferencingForeignKeys) then
    fReferencingForeignKeys.Free;

  inherited Destroy;
end;

end.

