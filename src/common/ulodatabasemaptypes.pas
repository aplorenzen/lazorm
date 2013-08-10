unit uloDatabaseMapTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  db,
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




implementation

{ TloDMTable }

constructor TloDMTable.Create;
begin
  inherited Create;
end;

destructor TloDMTable.Destroy;
begin
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

