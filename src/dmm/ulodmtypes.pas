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
{  Missing:                                                                    }
{    -                                                                         }
{                                                                              }
{  Copyright (c) 2013 Andreas Lorenzen                                         }
{                                                                              }
{******************************************************************************}

unit ulodmtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DB,
  SQLDB,
  FGL,
  XMLConf,
  SyncObjs,

  uloCoreInterfaces,
  uloCoreTypes,
  uloDatabaseTypes;

const
  cConnectorType_Sybase = 'Sybase';
  cConnectorType_Firebird = 'Firebird';
  cConnectorType_SQLite3 = 'SQLite3';
  cConnectorType_MySQL_4_0 = 'MySQL 4.0';
  cConnectorType_MySQL_4_1 = 'MySQL 4.1';
  cConnectorType_MySQL_5_0 = 'MySQL 5.0';
  cConnectorType_MySQL_5_1 = 'MySQL 5.1';
  cConnectorType_MySQL_5_5 = 'MySQL 5.5';
  cConnectorType_ODBC = 'ODBC';
  cConnectorType_PostGreSQL = 'PostGreSQL';
  cConnectorType_MSSQLServer = 'MSSQLServer';

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
    fDataSize: Integer;
    fHasDefault: Boolean;
    fDefaultValue: String;
    fAttributeSet: String;
    fProviderFlags: TProviderFlags;
    fIsIndexed: Boolean;
    fIsPrimaryKey: Boolean;
    fDescription: String;
    fIsRequired: Boolean;
    fForeignKeyReference: TloDMField;
    fReferencingForeignKeys: TloDMFieldList;
  public
    constructor Create(
      aOwner: IloObject;
      aLog: IloLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;
  published
    property FieldName: String read fFieldName write fFieldName;
    property FieldNo: LongInt read fFieldNo write fFieldNo;
    property FieldKind: TFieldKind read fFieldKind write fFieldKind;
    property DataType: TFieldType read fDataType write fDataType;
    property DataSize: Integer read fDataSize write fDataSize;
    property HasDefault: Boolean read fHasDefault write fHasDefault;
    property DefaultValue: String read fDefaultValue write fDefaultValue;
    property AttributeSet: String read fAttributeSet write fAttributeSet;
    property ProviderFlags: TProviderFlags read fProviderFlags write fProviderFlags;
    property IsIndexed: Boolean read fIsIndexed write fIsIndexed;
    property IsPrimaryKey: Boolean read fIsPrimaryKey write fIsPrimaryKey;
    property Description: String read fDescription write fDescription;
    property IsRequired: Boolean read fIsRequired write fIsRequired;
    property ForeignKeyReference: TloDMField read fForeignKeyReference write fForeignKeyReference;
    property ReferencingForeignKeys: TloDMFieldList read fReferencingForeignKeys write fReferencingForeignKeys;
  end;

  { DONE -oAPL -cDatabaseTypes 1: Complete the TloFieldList type }

  { TloDMFieldList }

  TloDMFieldList = class(specialize TFPGList<TloDMField>)
  public
    function FindFieldByName(aFieldName: String): TloDMField;
  end;

  { TloDMTable }

  TloDMTable = class(TloObject)
  private
    fTableName: String;
    fSchemeName: String;
    fCatalogName: String;
    fDescription: String;
    fFieldList: TloDMFieldList;
  public
    constructor Create(
      aOwner: IloObject;
      aLog: IloLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;
  published
    property TableName: String read fTableName write fTableName;
    property SchemeName: String read fSchemeName write fSchemeName;
    property CatalogName: String read fCatalogName write fCatalogName;
    property Description: String read fDescription write fDescription;
    property FieldList: TloDMFieldList read fFieldList write fFieldList;
  end;

  { TloDMTableList }

  TloDMTableList = class(specialize TFPGList<TloDMTable>)
  public
    function FindTable(aDatabaseName: String; aSchemaName: String; aTableName: String): TloDMTable;
  end;

  { TloDMModel }

  TloDMModel = class(TloDatabaseObject)
  private
    fTableList: TloDMTableList;
  public
    constructor Create(
      aOwner: IloObject;
      aConnection: TSQLConnector = nil;
      aLog: IloLogger = nil;
      aConfig: TXMLConfig = nil;
      aMutex: TCriticalSection = nil);
    destructor Destroy; override;
  published
    property TableList: TloDMTableList read fTableList write fTableList;
  end;

implementation

{ TloDMTableList }

function TloDMTableList.FindTable(aDatabaseName: String; aSchemaName: String; aTableName: String): TloDMTable;
var
  lTable: TloDMTable;
begin
  Result := nil;

  for lTable in Self do
    if Assigned(lTable) then
      if (lTable.CatalogName = aDatabaseName)
        and (lTable.SchemeName = aSchemaName)
        and (lTable.TableName = aTableName) then
      begin
        Result := lTable;
        Exit;
      end;
end;

{ TloDMFieldList }

function TloDMFieldList.FindFieldByName(aFieldName: String): TloDMField;
const
  lProcedureName = 'FindFieldByName';
var
  lField: TloDMField;
begin
  Result := nil;

  for lField in Self do
    if Assigned(lField) then
      if lField.FieldName = aFieldName then
      begin
        Result := lField;
        Exit;
      end;
end;

{ TloDMModel }

constructor TloDMModel.Create(aOwner: IloObject; aConnection: TSQLConnector; aLog: IloLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
const
  lProcedureName = 'Create';
begin
  inherited Create(
    aOwner,
    aConnection,
    aLog,
    aConfig,
    aMutex);

  fTableList := TloDMTableList.Create;
end;

destructor TloDMModel.Destroy;
const
  lProcedureName = 'Destroy';
var
  lTable: TloDMTable;
begin

  if Assigned(fTableList) then
  begin
    for lTable in fTableList do
      if Assigned(lTable) then
        lTable.Free;

    fTableList.Free;
  end;

  inherited Destroy;
end;

{ TloDMTable }

constructor TloDMTable.Create(aOwner: IloObject; aLog: IloLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
const
  lProcedureName = 'Create';
begin
  inherited Create(
    aOwner,
    aLog,
    aConfig,
    aMutex);

  fFieldList := TloDMFieldList.Create;
end;

destructor TloDMTable.Destroy;
const
  lProcedureName = 'Destroy';
begin
  { TODO -oAPL 2 Free objects in list }

  if Assigned(fFieldList) then
    fFieldList.Free;

  inherited Destroy;
end;

{ TloDMField }

constructor TloDMField.Create(aOwner: IloObject; aLog: IloLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
const
  lProcedureName = 'Create';
begin
  inherited Create(
    aOwner,
    aLog,
    aConfig,
    aMutex);

  fForeignKeyReference := nil;
  fReferencingForeignKeys := TloDMFieldList.Create;
end;

destructor TloDMField.Destroy;
const
  lProcedureName = 'Destroy';
begin
  if Assigned(fReferencingForeignKeys) then
    fReferencingForeignKeys.Free;

  inherited Destroy;
end;

end.

