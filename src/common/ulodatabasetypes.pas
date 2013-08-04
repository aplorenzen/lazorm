unit uloDatabaseTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  sqldb,
  fgl,
  syncobjs,
  contnrs,
  TLoggerUnit,
  XMLConf,

  uloDatabaseConstants;

type
  TloELComponent = class;
  TloELComponentList = class;
  TloELContainer = class;
  TloELTable = class;
  TloELAbstractField = class;

  { TODO -oAPL -cDatabaseTypes 2: The basic loEL component must be one that does not have a child list, but only has the
    logging, mutex and config things going on. So that the class can be used for list types, and so that we do not risk
    that the basic component that has a list of all sub components - will end in some recursive create statement.
    Anyways, the list objects must not have the RegisterChild feature of the constructor that is currently the
    TloELComponent constructor. What to do? }
  { TODO -oAPL -cDatabaseTypes 4: Make an event system in the super classes, something that can be connected to in an
     eventual UI, or service app - like: OnUpdate, OnInsert, OnDelete, OnChange and that sort of stuff. }

  { TloELComponent }

  TloELComponent = class(TInterfacedPersistent)
  private
    fOwnerComponent: TloELComponent;
    fConnection: TSQLConnector;
    fMutex: TCriticalSection;
    fLogger: TLogger;
    fConfig: TXMLConfig;
  public
    constructor Create(
        aOwner: TloELComponent;
        aConnection: TSQLConnector = nil;
        aMutex: TCriticalSection = nil;
        aLogger: TLogger = nil;
        aConfig: TXMLConfig = nil);
    destructor Destroy; override;
  published
    property Owner: TloELComponent read fOwnerComponent;
    property Connection: TSQLConnector read fConnection write fConnection;
    property Mutex: TCriticalSection read fMutex write fMutex;
    property Log: TLogger read fLogger write fLogger;
    property Config: TXMLConfig read fConfig write fConfig;
  end;

  { TloELComponentList }

  TloELComponentList = class(specialize TFPGList<TloELComponent>);

  { TloELContainer }

  TloELContainer = class(TloELComponent)
  private
    fChildComponents: TloELComponentList;
  public
    constructor Create(
      aOwner: TloELComponent;
      aConnection: TSQLConnector = nil;
      aMutex: TCriticalSection = nil;
      aLogger: TLogger = nil;
      aConfig: TXMLConfig = nil);
    destructor Destroy; override;
    procedure RegisterChild(aComponent: TloELComponent);
  published
    property ChildComponents: TloELComponentList read fChildComponents write fChildComponents;
  end;

  { TloELDataFormatter }

  TloELDataFormatter = class(TloELComponent)
  private
    constructor Create(
      aOwner: TloELComponent);
    destructor Destroy; override;

  end;

  { TloELSQLDataFormatter }

  TloELSQLDataFormatter = class(TloELDataFormatter)
  private
    constructor Create(
      aOwner: TloELComponent);
    destructor Destroy; override;
  public
    TBytes
    String
    Extended
    Currency
    Double
    Single
    Boolean
    Int64
    ShortInt
    LongWord
    Integer
    SmallInt
    Byte
    TDateTime
  end;

  { TloELSQLDataFormatter }

  TloELXMLDataFormatter = class(TloELDataFormatter)
  private
    constructor Create(
      aOwner: TloELComponent);
    destructor Destroy; override;
  end;

  { TloELSQLDataFormatter }

  TloELStringDataFormatter = class(TloELDataFormatter)
  private
    constructor Create(
      aOwner: TloELComponent);
    destructor Destroy; override;
  end;

  { TloELAbstractField }

  TloELAbstractField = class abstract(TloELComponent)
  private
    fOwnerContainer: TloELContainer;
    fFieldName: String;
    fIsNull: Boolean;
    fIsAssigned: Boolean;
    fHasChanged: Boolean;
    fIsIndexed: Boolean;
    fIsPrimaryKey: Boolean;
    fIsForeignKey: Boolean;
    fHasDefault: Boolean;

    procedure SetIsNull(aIsNull: Boolean);
    procedure SetIsAssigned(aIsAssigned: Boolean);
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String); reintroduce;
    destructor Destroy; override;
    function ToSql(): String; virtual; abstract;
    function ToXML(): String; virtual; abstract;
    function AsString(): String; virtual; abstract;
  published
    property Owner: TloELContainer read fOwnerContainer;
    property IsNull: Boolean read fIsNull write SetIsNull;
    property IsAssigned: Boolean read fIsAssigned write SetIsAssigned;
    property HasChanged: Boolean read fHasChanged write fHasChanged;
    property IsIndexed: Boolean read fIsIndexed write fIsIndexed;
    property IsPrimaryKey: Boolean read fIsPrimaryKey write fIsPrimaryKey;
    property IsForeignKey: Boolean read fIsForeignKey write fIsForeignKey;
    property HasDefault: Boolean read fHasDefault write fHasDefault;
  end;

  { TloELAbstractFieldList }

  TloELAbstractFieldList = class(TloELComponent)
  private
    fOwnerContainer: TloELContainer;
    fFieldList: TFPObjectList;
    function GetCount: Integer;
  public
    type

    { TloELAbstractFieldListEnumerator }

      TloELAbstractFieldListEnumerator = class(TInterfacedObject)
      private
        fFieldList: TloELAbstractFieldList;
        fCurrenIndex: Integer;
      public
        constructor Create(aFieldList: TloELAbstractFieldList);
        destructor Destroy; override;
        function GetCurrent: TloELAbstractField;
        function MoveNext: Boolean;
        property Current: TloELAbstractField read GetCurrent;
      end;

    constructor Create(aOwner: TloELContainer; aFreeObjects: Boolean = False);
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(aField: TloELAbstractField): Integer; virtual;
    function IsEmpty: Boolean;
    procedure Add(aField: TloELAbstractField); virtual;
    function Get(aIndex: Integer): TloELAbstractField; virtual;
    procedure Insert(aIndex: Integer; aField: TloELAbstractField); virtual;
    function Remove(aField: TloELAbstractField): Boolean; virtual;
    procedure Pack;
    function First: TloELAbstractField; virtual;
    function Last: TloELAbstractField; virtual;
    property Count: Integer read GetCount;
    function GetEnumerator: TloELAbstractFieldListEnumerator;
  published
    { TODO -oAPL -cDatabaseTypes 1: Must change this to some sort of "named database object type" -
      because we need to be able also to use these classes for f.ex. views, and other things may come along }
    property Owner: TloELContainer read fOwnerContainer;
  end;

  { TloELGenericField }

  generic TloELGenericField<T> = class abstract(TloELAbstractField)
  private
    fValue: T;
    procedure SetValue(aValue: T);
    function GetValue: T;
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; virtual;
    function ValueEquals(aValue: T): Boolean; virtual;
    property Value: T read GetValue write SetValue;
  end;

  { TloELTable }

  TloELTable = class(TloELContainer)
  private
    fTableName: String;
    fSchemeName: String;
    fCatalogName: String;
    fFieldList: TloELAbstractFieldList;
    procedure AddField(aField: TloELAbstractField);
  public
    constructor Create(
      aOwner: TloELComponent;
      aConnection: TSQLConnector = nil;
      aMutex: TCriticalSection = nil;
      aLogger: TLogger = nil;
      aConfig: TXMLConfig = nil);
    destructor Destroy; override;
  published
    property TableName: String read fTableName;
    property SchemeName: String read fSchemeName;
    property CatalogName: String read fCatalogName;
    property Fields: TloELAbstractFieldList read fFieldList;
  end;

  { TloELView }

  TloELView = class(TloELContainer)
  private
    fViewName: String;
    fSchemeName: String;
    fCatalogName: String;
    fFieldList: TloELAbstractFieldList;
    procedure AddField(aField: TloELAbstractField);
  public
    constructor Create(
      aOwner: TloELComponent;
      aConnection: TSQLConnector = nil;
      aMutex: TCriticalSection = nil;
      aLogger: TLogger = nil;
      aConfig: TXMLConfig = nil);
    destructor Destroy; override;
  published
    property ViewName: String read fViewName;
    property SchemeName: String read fSchemeName;
    property CatalogName: String read fCatalogName;
    property Fields: TloELAbstractFieldList read fFieldList;
  end;

  { TODO -oAPL -cDatabaseTypes 3: The following is declarations for all the types that the system maps to,
    from various database types. It is the intention that these types either have knowledge of how to
    format themselves to the various database implementations, or that they utilize some exterior object
    that does. Is there one such object already made? Look around for it! }

  { TloELFieldBytes }

  TloELFieldBytes = class(specialize TloELGenericField<TBytes>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldBytes): Boolean; overload;
  end;

  { TloELFieldDateTime }

  TloELFieldDateTime = class(specialize TloELGenericField<TDateTime>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldDateTime): Boolean; overload;
  end;

  { TloELFieldString }

  TloELFieldString = class(specialize TloELGenericField<String>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldString): Boolean; overload;
  end;

  { TloELFieldExtended }

  TloELFieldExtended = class(specialize TloELGenericField<Extended>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldExtended): Boolean; overload;
  end;

  { TloELFieldCurrency }

  TloELFieldCurrency = class(specialize TloELGenericField<Currency>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldCurrency): Boolean; overload;
  end;

  { TloELFieldDouble }

  TloELFieldDouble = class(specialize TloELGenericField<Double>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldDouble): Boolean; overload;
  end;

  { TloELFieldSingle }

  TloELFieldSingle = class(specialize TloELGenericField<Single>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldSingle): Boolean; overload;
  end;

  { TloELFieldBoolean }

  TloELFieldBoolean = class(specialize TloELGenericField<Boolean>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldBoolean): Boolean; overload;
  end;

  { TloELFieldInt64 }

  TloELFieldInt64 = class(specialize TloELGenericField<Int64>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldInt64): Boolean; overload;
  end;

  { TloELFieldShortInt }

  TloELFieldShortInt = class(specialize TloELGenericField<ShortInt>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldShortInt): Boolean; overload;
  end;

  { TloELFieldLongWord }

  TloELFieldLongWord = class(specialize TloELGenericField<LongWord>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldLongWord): Boolean; overload;
  end;

  { TloELFieldInteger }

  TloELFieldInteger = class(specialize TloELGenericField<Integer>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldInteger): Boolean; overload;
  end;

  { TloELFieldSmallInt }

  TloELFieldSmallInt = class(specialize TloELGenericField<SmallInt>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldSmallInt): Boolean; overload;
  end;

  { TloELFieldByte }

  TloELFieldByte = class(specialize TloELGenericField<Byte>)
  public
    constructor Create(
      aOwner: TloELContainer;
      aFieldName: String);
    destructor Destroy; override;
    procedure ClearValue; override;
    function ToSql(): String; override;
    function ToXML(): String; override;
    function AsString(): String; override;
    function Equals(aValue: TloELFieldByte): Boolean; overload;
  end;


    {
    TInterfacedPersisten  TloDatabaseTableObject = class(TInterfacedPersistent)
  private
    fConnection: TSQLConnector;
    fTableInfo: TTableItem;
    fDBMutex: TCriticalSection;
    fObj_IsNew: Boolean;
    fObj_IsLoaded: Boolean;
    fObj_IsChanged: Boolean;
    fObj_IsDeleted: Boolean;
    fObj_ChangedTime: TDateTime;
    fObj_SavedToDBTime: TDateTime;
    fObj_CreatedTime: TDateTime;
    fObj_LoadedFromDBTime: TDateTime;
    fObj_DeletedTime: TDateTime;
    fDBFieldList: TDBFieldBaseList;
    fIsDestroying: Boolean;
  public
    constructor Create(AConnection: TADOConnection; ADBMutex: TCriticalSection);
    destructor Destroy; override;
    function GetConnection(): TADOConnection; override;
    procedure SetConnection(AConnection: TADOConnection); override;
    function GetTableInfo(): TTableItem; override;
    procedure SetTableInfo(ATableInfo: TTableItem); override;
    function GetDBMutex(): TCriticalSection;
    procedure SetDBMutex(ADBMutex: TCriticalSection);
    function GetObj_IsNew(): Boolean;
    procedure SetObj_IsNew(AObj_IsNew: Boolean);
    function GetObj_IsLoaded(): Boolean;
    procedure SetObj_IsLoaded(AObj_IsLoaded: Boolean);
    function GetObj_IsChanged(): Boolean;
    procedure SetObj_IsChanged(AObj_IsChanged: Boolean);
    function GetObj_IsDeleted(): Boolean;
    procedure SetObj_IsDeleted(AObj_IsDeleted: Boolean);
    function GetObj_ChangedTime(): TDateTime;
    procedure SetObj_ChangedTime(AObj_ChangedTime: TDateTime);
    function GetObj_SavedToDBTime(): TDateTime;
    procedure SetObj_SavedToDBTime(AObj_SavedToDBTime: TDateTime);
    function GetObj_CreatedTime(): TDateTime;
    procedure SetObj_CreatedTime(AObj_CreatedTime: TDateTime);
    function GetObj_LoadedFromDBTime(): TDateTime;
    procedure SetObj_LoadedFromDBTime(AObj_LoadedFromDBTime: TDateTime);
    function GetObj_DeletedTime(): TDateTime;
    procedure SetObj_DeletedTime(AObj_DeletedTime: TDateTime);
    function GetDBFieldList(): TDBFieldBaseList;
    procedure SetDBFieldList(ADBFieldList: TDBFieldBaseList);
    function GetIsDestroying(): Boolean;
    procedure SetIsDestroying(AIsDestroying: Boolean);
    function LoadedOrNotNew(AFreeIfNot: Boolean = True): Boolean;
    property Connection: TADOConnection read GetConnection write SetConnection;
    property TableInfo: TTableItem read GetTableInfo write SetTableInfo;
    property DBMutex: TCriticalSection read GetDBMutex write SetDBMutex;
    property Obj_IsNew: Boolean read GetObj_IsNew write SetObj_IsNew;
    property Obj_IsLoaded: Boolean read GetObj_IsLoaded write SetObj_IsLoaded;
    property Obj_IsChanged: Boolean read GetObj_IsChanged write SetObj_IsChanged;
    property Obj_IsDeleted: Boolean read GetObj_IsDeleted write SetObj_IsDeleted;
    property Obj_ChangedTime: TDateTime read GetObj_ChangedTime write SetObj_ChangedTime;
    property Obj_SavedToDBTime: TDateTime read GetObj_SavedToDBTime write SetObj_SavedToDBTime;
    property Obj_CreatedTime: TDateTime read GetObj_CreatedTime write SetObj_CreatedTime;
    property Obj_LoadedFromDBTime: TDateTime read GetObj_LoadedFromDBTime write SetObj_LoadedFromDBTime;
    property Obj_DeletedTime: TDateTime read GetObj_DeletedTime write SetObj_DeletedTime;
    property DBFieldList: TDBFieldBaseList read GetDBFieldList write SetDBFieldList;
    property IsDestroying: Boolean read GetIsDestroying write SetIsDestroying;
  end;     }

implementation

{ TloELView }

procedure TloELView.AddField(aField: TloELAbstractField);
begin
  fFieldList.Add(aField);
end;

constructor TloELView.Create(aOwner: TloELComponent; aConnection: TSQLConnector; aMutex: TCriticalSection; aLogger: TLogger; aConfig: TXMLConfig);
begin
  inherited Create(
    aOwner,
    aConnection,
    aMutex,
    aLogger,
    aConfig);

  fFieldList := TloELAbstractFieldList.Create(Self);
end;

destructor TloELView.Destroy;
var
  lField: TloELAbstractField;
begin
  if Assigned(fFieldList) then
    for lField in fFieldList do
      lField.Free;

  fFieldList.Free;

  inherited Destroy;
end;

{ TloELTable }

procedure TloELTable.AddField(aField: TloELAbstractField);
begin
  fFieldList.Add(aField);
end;

constructor TloELTable.Create(aOwner: TloELComponent; aConnection: TSQLConnector; aMutex: TCriticalSection; aLogger: TLogger; aConfig: TXMLConfig);
begin
  inherited Create(
    aOwner,
    aConnection,
    aMutex,
    aLogger,
    aConfig);

  fFieldList := TloELAbstractFieldList.Create(Self);
end;

destructor TloELTable.Destroy;
var
  lField: TloELAbstractField;
begin
  if Assigned(fFieldList) then
    for lField in fFieldList do
      lField.Free;

  fFieldList.Free;

  inherited Destroy;
end;

{ TloELFieldSmallInt }

constructor TloELFieldSmallInt.Create(aOwner: TloELContainer; aFieldName: String);
begin
  inherited Create(
    aOwner,
    aFieldName);
end;

destructor TloELFieldSmallInt.Destroy;
begin
  inherited Destroy;
end;

procedure TloELFieldSmallInt.ClearValue;
begin
  fValue := 0;
  inherited ClearValue;
end;

function TloELFieldSmallInt.ToSql: String;
begin
  if fIsNull then
    Result := lo_SQL_NULL
  else
    Result := IntToStr(fValue);
end;

function TloELFieldSmallInt.ToXML: String;
begin
  if fIsNull then
    Result := lo_XML_NULL
  else
    Result := IntToStr(fValue);
end;

function TloELFieldSmallInt.AsString: String;
begin
  if fIsNull then
    Result := lo_Str_NULL
  else
    Result := IntToStr(fValue);
end;

function TloELFieldSmallInt.Equals(aValue: TloELFieldSmallInt): Boolean;
begin
  if fIsNull then
    Result := aValue.IsNull
  else
    if aValue.IsNull then
      Result := False
    else
      Result := inherited ValueEquals(aValue.Value);
end;

{ TloELContainer }

constructor TloELContainer.Create(aOwner: TloELComponent; aConnection: TSQLConnector; aMutex: TCriticalSection; aLogger: TLogger; aConfig: TXMLConfig);
begin
  inherited Create(
    aOwner,
    aConnection,
    aMutex,
    aLogger,
    aConfig);
end;

destructor TloELContainer.Destroy;
begin
  if Assigned(fChildComponents) then
    fChildComponents.Free;
  inherited Destroy;
end;

procedure TloELContainer.RegisterChild(aComponent: TloELComponent);
begin
  { TODO -oAPL -cDatabaseTypes 2: Make this list type a non generic decendant, perhaps a wrapped type with some helper methods and with it's own enumerator }
  if not Assigned(fChildComponents) then
    fChildComponents := TloELComponentList.Create;
  fChildComponents.Add(aComponent);
end;

function TloELAbstractFieldList.GetCount: Integer;
begin
  Result := fFieldList.Count;
end;

constructor TloELAbstractFieldList.Create(aOwner: TloELContainer; aFreeObjects: Boolean);
begin
  inherited Create(aOwner);
  fOwnerContainer := aOwner;
  fFieldList := TFPObjectList.Create(aFreeObjects);
end;

destructor TloELAbstractFieldList.Destroy;
begin
  fFieldList.Clear;
  fFieldList.Free;
  inherited Destroy;
end;

procedure TloELAbstractFieldList.Clear;
begin
  fFieldList.Clear;
end;

function TloELAbstractFieldList.IndexOf(aField: TloELAbstractField): Integer;
begin
  Result := fFieldList.IndexOf(aField);
end;

function TloELAbstractFieldList.IsEmpty: Boolean;
begin
  Result := fFieldList.Count = 0;
end;

procedure TloELAbstractFieldList.Add(aField: TloELAbstractField);
begin
  fFieldList.Add(aField);
end;

function TloELAbstractFieldList.Get(aIndex: Integer): TloELAbstractField;
begin
  if ((aIndex > fFieldList.Count) or (aIndex < 0)) then
    Result := nil
  else
    Result := TloELAbstractField(fFieldList.Items[aIndex]);
end;

procedure TloELAbstractFieldList.Insert(aIndex: Integer; aField: TloELAbstractField);
begin
  fFieldList.Insert(aIndex, aField);
end;

function TloELAbstractFieldList.Remove(aField: TloELAbstractField): Boolean;
begin
  Result := (fFieldList.Remove(aField) <> -1);
end;

procedure TloELAbstractFieldList.Pack;
begin
  fFieldList.Pack;
end;

function TloELAbstractFieldList.First: TloELAbstractField;
begin
  if fFieldList.Count > 0 then
      Result := TloELAbstractField(fFieldList.First)
    else
      Result := nil;
end;

function TloELAbstractFieldList.Last: TloELAbstractField;
begin
  if fFieldList.Count > 0 then
    Result := TloELAbstractField(fFieldList.Last)
  else
    Result := nil;
end;

function TloELAbstractFieldList.GetEnumerator: TloELAbstractFieldListEnumerator;
begin
  Result := TloELAbstractFieldList.TloELAbstractFieldListEnumerator.Create(Self);
end;

{ TloELAbstractFieldList.TloELAbstractFieldListEnumerator }

constructor TloELAbstractFieldList.TloELAbstractFieldListEnumerator.Create(aFieldList: TloELAbstractFieldList);
begin
  inherited Create;
  fFieldList := aFieldList;
  fCurrenIndex := -1;
end;

destructor TloELAbstractFieldList.TloELAbstractFieldListEnumerator.Destroy;
begin
  inherited Destroy;
end;

function TloELAbstractFieldList.TloELAbstractFieldListEnumerator.GetCurrent: TloELAbstractField;
begin
  Result := fFieldList.Get(fCurrenIndex);
end;

function TloELAbstractFieldList.TloELAbstractFieldListEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fCurrenIndex = -1 then
    fCurrenIndex := fFieldList.IndexOf(fFieldList.First)
  else
    Inc(fCurrenIndex);

  Result := fCurrenIndex < fFieldList.Count;
end;

{ TloELFieldInteger }

constructor TloELFieldInteger.Create(aOwner: TloELContainer; aFieldName: String);
begin
  inherited Create(
    aOwner,
    aFieldName);
end;

destructor TloELFieldInteger.Destroy;
begin
  inherited Destroy;
end;

procedure TloELFieldInteger.ClearValue;
begin
  fValue := 0;
  inherited ClearValue;
end;

function TloELFieldInteger.ToSql: String;
begin
  if fIsNull then
    Result := lo_SQL_NULL
  else
    Result := IntToStr(fValue);
end;

function TloELFieldInteger.ToXML: String;
begin
  if fIsNull then
    Result := lo_XML_NULL
  else
    Result := IntToStr(fValue);
end;

function TloELFieldInteger.AsString: String;
begin
  if fIsNull then
    Result := lo_Str_NULL
  else
    Result := IntToStr(fValue);
end;

function TloELFieldInteger.Equals(aValue: TloELFieldInteger): Boolean;
begin
  if fIsNull then
    Result := aValue.IsNull
  else
    if aValue.IsNull then
      Result := False
    else
      Result := inherited ValueEquals(aValue.Value);
end;

{ TloELGenericField }

procedure TloELGenericField.SetValue(aValue: T);
var
  lChanged: Boolean;
begin
  if not ValueEquals(aValue) then
  begin
    fHasChanged := True;
    { TODO -oAPL -cDatabaseTypes 3: Implement the TloTable type so that these variables can be set here }
    // OwnerTable.Obj_IsChanged := True;
    // OwnerTable.Obj_ChangedTime := Now;
  end;

  fIsNull := False;
  fIsAssigned := True;
  fValue := aValue;
end;

function TloELGenericField.GetValue: T;
begin
  Result := fValue;
end;

constructor TloELGenericField.Create(aOwner: TloELContainer; aFieldName: String);
begin
  inherited Create(
    aOwner,
    aFieldName);
end;

destructor TloELGenericField.Destroy;
begin
  inherited Destroy;
end;

procedure TloELGenericField.ClearValue;
begin
  fHasChanged := not fIsNull;
  fIsNull := True;
  fIsAssigned := False;
end;

function TloELGenericField.ValueEquals(aValue: T): Boolean;
begin
  Result := aValue = Self.Value;
end;

{ TloELAbstractField }

procedure TloELAbstractField.SetIsNull(aIsNull: Boolean);
begin
  if fIsNull <> aIsNull then
    fHasChanged := True;
  if not aIsNull then
    fIsAssigned := True;
  fIsNull := aIsNull;
end;

procedure TloELAbstractField.SetIsAssigned(aIsAssigned: Boolean);
begin
  if fIsAssigned <> aIsAssigned then
    fHasChanged := True;
  if not aIsAssigned then
    fIsNull := True;
  fIsAssigned := aIsAssigned;
end;

constructor TloELAbstractField.Create(aOwner: TloELContainer; aFieldName: String);
begin
  inherited Create(aOwner);
  fOwnerContainer := aOwner;
  fFieldName := aFieldName;
  fIsNull := True;
  fIsAssigned := False;
  fHasChanged := False;
  fIsIndexed := False;
  fIsPrimaryKey := False;
  fIsForeignKey := False;
  fHasDefault := False;
end;

destructor TloELAbstractField.Destroy;
begin
  inherited Destroy;
end;

{ TloELComponent }

constructor TloELComponent.Create(
  aOwner: TloELComponent;
  aConnection: TSQLConnector;
  aMutex: TCriticalSection;
  aLogger: TLogger;
  aConfig: TXMLConfig);
begin
  inherited Create;

  fOwnerComponent := aOwner;

  if Assigned(aOwner) then
  begin
    fConnection := aOwner.Connection;
    fMutex := aOwner.Mutex;
    fLogger := aOwner.Log;
    fConfig := aOwner.Config;

    if aOwner is TloELContainer then
      TloELContainer(aOwner).RegisterChild(Self);
  end;

  if Assigned(aConnection) then
    fConnection := aConnection;
  if Assigned(aMutex) then
    fMutex := aMutex;
  if Assigned(aLogger) then
    fLogger := aLogger;
  if Assigned(aConfig) then
    fConfig := aConfig;
end;

destructor TloELComponent.Destroy;
begin
  inherited Destroy;
end;

end.

