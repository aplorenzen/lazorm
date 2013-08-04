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
  TloTable = class;
  TloFieldList = class;

  { TloField }

  TloField = class(TInterfacedPersistent)
  private
    fOwnerTable: TloTable;
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
    fForeignKeyReferences: TloFieldList;
    fReferencingForeignKeys: TloFieldList;
  public
    constructor Create(aOwnerTable: TloTable);
    destructor Destroy; override;
  published
    property OwnerTable: TloTable read fOwnerTable write fOwnerTable;
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
    property ForeignKeyReferences: TloFieldList read fForeignKeyReferences write fForeignKeyReferences;
    property ReferencingForeignKeys: TloFieldList read fReferencingForeignKeys write fReferencingForeignKeys;
  end;

  { DONE -oAPL -cDatabaseTypes 1: Complete the TloFieldList type }
  TloFieldList = class(specialize TFPGList<TloField>);

  { TloTable }

  TloTable = class(TInterfacedPersistent)
  private
    fTableName: String;
    fSchemeName: String;
    fCatalogName: String;
    fFieldList: TloFieldList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property TableName: String read fTableName write fTableName;
    property SchemeName: String read fSchemeName write fSchemeName;
    property CatalogName: String read fCatalogName write fCatalogName;
    property FieldList: TloFieldList read fFieldList write fFieldList;
  end;

  TloTableList = class(specialize TFPGList<TloTable>);




implementation

{ TloTable }

constructor TloTable.Create;
begin
  inherited Create;
end;

destructor TloTable.Destroy;
begin
  inherited Destroy;
end;

{ TloField }

constructor TloField.Create(aOwnerTable: TloTable);
begin
  inherited Create;
  fOwnerTable := aOwnerTable;
  fForeignKeyReferences := TloFieldList.Create;
  fReferencingForeignKeys := TloFieldList.Create;
end;

destructor TloField.Destroy;
begin
  if Assigned(fForeignKeyReferences) then
    fForeignKeyReferences.Free;
  if Assigned(fReferencingForeignKeys) then
    fReferencingForeignKeys.Free;

  inherited Destroy;
end;

end.

