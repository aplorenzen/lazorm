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

unit uloDatabaseMapTypes;

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
  uloDatabaseTypes;

type
  TloDMTable = class;
  TloDMFieldList = class;

  { TloDMField }

  TloDMField = class(TInterfacedPersistent)
  private
    fOwnerTable: TloDMTable;
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
    constructor Create(aOwnerTable: TloDMTable);
    destructor Destroy; override;
  published
    property OwnerTable: TloDMTable read fOwnerTable write fOwnerTable;
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

  TloDMTable = class(TInterfacedPersistent)
  private
    fTableName: String;
    fSchemeName: String;
    fCatalogName: String;
    fFieldList: TloDMFieldList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property TableName: String read fTableName write fTableName;
    property SchemeName: String read fSchemeName write fSchemeName;
    property CatalogName: String read fCatalogName write fCatalogName;
    property FieldList: TloDMFieldList read fFieldList write fFieldList;
  end;

  TloDMTableList = class(specialize TFPGList<TloDMTable>);

  { TloDMMetaDataRetriever }

  TloDMMetaDataRetriever = class(TInterfacedPersistent)
  private
    fSQLConnector: TSQLConnector;
    // procedure DoLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
  public
    constructor Create(aSQLConnector: TSQLConnector);
    destructor Destroy; override;

    function RetrieveTableMetaData: TloDMTableList;
  end;


implementation

{ TloDMMetaDataRetriever }

{procedure TloDMMetaDataRetriever.DoLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
begin
  { TODO -oAPL -cMetaDataRetriever 2: Implement this logging function }
end;}

constructor TloDMMetaDataRetriever.Create(aSQLConnector: TSQLConnector);
begin
  inherited Create; -
  fSQLConnector := aSQLConnector;
end;

destructor TloDMMetaDataRetriever.Destroy;
begin
  inherited Destroy;
end;

function TloDMMetaDataRetriever.RetrieveTableMetaData: TloDMTableList;
var
  lSQLQuery: TSQLQuery;
  lTableIndex: Integer;
  lTable: TloDMTable;
begin
  Result := TloDMTableList.Create;

  lSQLQuery := TSQLQuery.Create(nil);

  try
    lSQLQuery.DataBase := fSQLConnector;

    {TDBEventType = (detCustom, detPrepare, detExecute, detFetch, detCommit,detRollBack);
    TDBEventTypes = set of TDBEventType;
    TDBLogNotifyEvent = Procedure (Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String) of object;}

    fSQLConnector.LogEvents := [detCustom, detPrepare, detExecute, detFetch, detCommit, detRollBack];
    // fSQLConnector.OnLog := DoLog;

    lSQLQuery.SQL.Add('USE ' + fSQLConnector.DatabaseName + ';');
    lSQLQuery.SQL.Add('SELECT * FROM information_schema.tables;');

    lSQLQuery.Open;

    lTableIndex := 0;

    if not lSQLQuery.IsEmpty then
    begin
      while not lSQLQuery.EOF do
      begin
        // ProcessEvents;
        Inc(lTableIndex);
        lTable := TloDMTable.Create;

        with lTable do
        begin
          fCatalogName := lSQLQuery.FieldByName('TABLE_CATALOG').AsString;
          fSchemeName := lSQLQuery.FieldByName('TABLE_SCHEMA').AsString;
          fTableName := lSQLQuery.FieldByName('TABLE_NAME').AsString;
          // fTableType := lSQLQuery.FieldByName('TABLE_TYPE').AsString;
        end;

        //ReportProgress(1 +  + Round((7 * (lTableIndex / lTableCount))), Format('Adding table: ''%s''...', ['[' + lTableItem.TABLE_CATALOG + '].[' + lTableItem.TABLE_SCHEMA + '].[' + lTableItem.TABLE_NAME +']']));

        Result.Add(lTable);

        lSQLQuery.Next;
      end;
    end;

  finally
    lSQLQuery.Free;
  end;
end;

{ TloDMTable }

constructor TloDMTable.Create;
begin
  inherited Create;

  fFieldList := TloDMFieldList.Create;
end;

destructor TloDMTable.Destroy;
begin
  fFieldList.Free;
  inherited Destroy;
end;

{ TloDMField }

constructor TloDMField.Create(aOwnerTable: TloDMTable);
begin
  inherited Create;
  fOwnerTable := aOwnerTable;
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

