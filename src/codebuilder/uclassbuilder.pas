// NOTE: Rename objects: TlocbXXXX for Lazarus ORM Code Builder

unit uclassbuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  TLoggerUnit,
  XMLConf;

type
  TcbMethodParameterModifier = (
    mpmNone,
    mpmOut,
    mpmConst,
    mpmVar);

  TcbMethodDirectiveTypes = (
    mdtOverride,
    mdtOverload,
    mdtAbstract,
    mdtVirtual,
    mdtDynamic);

  TcbMethodDirectives = set of TcbMethodDirectiveTypes;

  TcbMethodType = (
    mtProcedure,
    mtFunction,
    mtConstructor,
    mtDestructor);

  TcbClassSection = (
    csPackage,
    csPrivate,
    csPublic,
    csProtected,
    csPublished);

  TcbComponent = class;
  TcbClass = class;
  TcbClassList = class;
  TcbMethodParameter = class;
  TcbMethodParameterList = class;
  TcbMethod = class;
  TcbMethodList = class;
  TcbClassMethod = class;
  TcbClassMethodList = class;
  TcbVariable = class;
  TcbVariableList = class;
  TcbClassVariable = class;
  TcbClassVariableList = class;
  TcbClassProperty = class;
  TcbClassPropertyList = class;
  TcbUnit = class;
  TcbUnitList = class;

  // TcbCodeBuilderConfiguration

  IcbClassElement = interface(IInterface)
    ['{161FD953-6C66-472A-873B-C4167CE45A60}']
    function GetOwnerClass: TcbClass;
    procedure SetOwnerClass(AOwnerClass: TcbClass);
    property OwnerClass: TcbClass read GetOwnerClass write SetOwnerClass;
  end;

  IcbNamedElement = interface(IInterface)
    ['{035C5796-ABB1-4523-A20A-4A7AC382CF47}']
    function GetElementName: String;
    procedure SetElementName(AName: String);
    property ElementName: String read GetElementName write SetElementName;
  end;

  TcbNamedElementList = class(specialize TFPGList<IcbNamedElement>);

  IcbNamedClassElement = interface(IcbNamedElement)
    ['{294F48AA-5C20-4B01-97A9-C7D7CEB633B2}']
    function GetClassDeclaration: String;
    property ClassDeclaration: String read GetClassDeclaration;
  end;

  TcbNamedClassElementList = class(specialize TFPGList<IcbNamedClassElement>);

  IcbLoggerConsumer = interface(IInterface)
    ['{C6C1AA83-B523-468A-AE02-1206F49A468E}']
    procedure SetLogger(ALogger: TLogger);
    function GetLogger: TLogger;
    property Log: TLogger read GetLogger write SetLogger;
  end;

  IcbXMLConfigConsumer = interface(IInterface)
    ['{950F7159-1660-42EF-A741-4AEF689D920D}']
    procedure SetConfig(AConfig: TXMLConfig);
    function GetConfig: TXMLConfig;
    property Config: TXMLConfig read GetConfig write SetConfig;
  end;

  IcbComponent = interface(IInterface)
    ['{1C9D78EE-A564-4EBF-88BF-841D4EBFB389}']
    procedure SetOwnerComponent(AOwnerComponent: TcbComponent);
    function GetOwnerComponent: TcbComponent;
    property OwnerComponent: TcbComponent read GetOwnerComponent write SetOwnerComponent;
  end;

  { TcbComponent }

  TcbComponent = class(TInterfacedPersistent, IcbComponent, IcbXMLConfigConsumer, IcbLoggerConsumer)
  private
    fOwnerComponent: TcbComponent;
    fLogger: TLogger;
    fConfig: TXMLConfig;
  public
    constructor Create(AOwner: TcbComponent; AConfig: TXMLConfig = nil; ALogger: TLogger = nil); overload;
    destructor Destroy; override;

    procedure SetLogger(ALogger: TLogger);
    function GetLogger: TLogger;
    procedure SetConfig(AConfig: TXMLConfig);
    function GetConfig: TXMLConfig;
    procedure SetOwnerComponent(AOwnerComponent: TcbComponent); virtual;
    function GetOwnerComponent: TcbComponent; virtual;
  end;

  { TcbComponentList }

  TcbComponentList = class(specialize TFPGList<TcbComponent>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TcbMethodParameter }

  TcbMethodParameter = class(TcbComponent, IcbNamedElement)
  private
    fParameterName: String;
    fParameterType: String;
    fParameterDefault: String;
    fParameterModifier: TcbMethodParameterModifier;
  public
    constructor Create(
      AOwner: TcbComponent;
      AParameterName: String;
      AParameterType: String;
      AParameterModifier: TcbMethodParameterModifier = mpmNone;
      AParameterDefault: String = '');
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
  published
    property ParameterName: String read fParameterName write fParameterName;
    property ParameterType: String read fParameterType write fParameterType;
    property ParameterDefault: String read fParameterDefault write fParameterDefault;
    property ParameterModifier: TcbMethodParameterModifier read fParameterModifier write fParameterModifier;
  end;

  { TcbMethodParameterList }

  TcbMethodParameterList = class(specialize TFPGList<TcbMethodParameter>)
  public
    constructor Create;
    destructor Destroy; override;
    function WriteMethodParameters: String;
  end;

  TcbMethod = class(TcbComponent, IcbNamedElement)
  private
    fMethodType: TcbMethodType;
    fMethodName: String;
    fReturnType: String;
    fMethodDirectives: TcbMethodDirectives;
    fParameterList: TcbMethodParameterList;
    fImplementationVars: TcbVariableList;
    fMethodImplementation: TStringList;
  public
    constructor Create(
      AOwner: TcbComponent;
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: String = '');
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function WriteMethodPrototype: String; virtual;
  published
    property MethodType: TcbMethodType read fMethodType write fMethodType;
    property MethodName: String read fMethodName write fMethodName;
    property ReturnType: String read fReturnType write fReturnType;
    property MethodDirectives: TcbMethodDirectives read fMethodDirectives write fMethodDirectives;
    property ParameterList: TcbMethodParameterList read fParameterList write fParameterList;
    property ImplementationVars: TcbVariableList read fImplementationVars write fImplementationVars;
    property MethodImplementation: TStringList read fMethodImplementation write fMethodImplementation;
  end;

  { TcbMethodList }

  TcbMethodList = class(specialize TFPGList<TcbMethod>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TcbClassMethod }

  TcbClassMethod = class(TcbMethod, IcbClassElement, IcbNamedClassElement)
  private
    fOwnerClass: TcbClass;
    fClassSection : TcbClassSection;
  public
    constructor Create(AOwner: TcbClass);
    destructor Destroy; override;
    function GetClassDeclaration: String;
    function GetOwnerClass: TcbClass;
    procedure SetOwnerClass(AOwnerClass: TcbClass);
  published
    property ClassSection: TcbClassSection read fClassSection  write fClassSection ;
  end;

  { TcbClassMethodList }

  TcbClassMethodList = class(specialize TFPGList<TcbClassMethod>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TcbVariable }

  TcbVariable = class(TcbComponent, IcbNamedElement)
  private
    fVariableName: String;
    fVariableType: String;
  public
    constructor Create(
      AOwner: TcbComponent;
      AVariableName: String;
      AVariableType: String);
      destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
  published
    property VariableName: String read fVariableName write fVariableName;
    property VariableType: String read fVariableType write fVariableType;
  end;

  { TcbVariableList }

  TcbVariableList = class(specialize TFPGList<TcbVariable>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TcbClassVariable }

  TcbClassVariable = class(TcbVariable, IcbNamedClassElement)
  private
    fOwnerClass: TcbClass;
    fClassSection : TcbClassSection;
  public
    constructor Create(AOwner: TcbClass);
    destructor Destroy; override;
    function GetClassDeclaration: String;
    function GetOwnerClass: TcbClass;
    procedure SetOwnerClass(AOwnerClass: TcbClass);
  published
    property OwnerClass: TcbClass read fOwnerClass write fOwnerClass;
    property ClassSection : TcbClassSection read fClassSection  write fClassSection ;
  end;

  { TcbClassVariableList }

  TcbClassVariableList = class(specialize TFPGList<TcbClassVariable>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TcbClassProperty }

  TcbClassProperty = class(TcbComponent, IcbNamedClassElement)
  private
    fOwnerClass: TcbClass;
    fClassSection : TcbClassSection;
    fPropertyName: String;
    fPropertyType: String;
    fPropertyReadElement: IcbNamedClassElement;
    fPropertyWriteElement: IcbNamedClassElement;
  public
    constructor Create(
      AOwner: TcbClass;
      APropertyName: String;
      APropertyType: String;
      AClassSection : TcbClassSection = csPackage;
      APropertyReadElement: IcbNamedClassElement = nil;
      APropertyWriteElement: IcbNamedClassElement = nil);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function GetClassDeclaration: String;
    function GetOwnerClass: TcbClass;
    procedure SetOwnerClass(AOwnerClass: TcbClass);
  published
    property ClassSection : TcbClassSection read fClassSection  write fClassSection ;
    property PropertyName: String read fPropertyName write fPropertyName;
    property PropertyType: String read fPropertyType write fPropertyType;
    property PropertyReadElement: IcbNamedClassElement read fPropertyReadElement write fPropertyReadElement;
    property PropertyWriteElement: IcbNamedClassElement read fPropertyWriteElement write fPropertyWriteElement;
  end;

  { TcbClassPropertyList }

  TcbClassPropertyList = class(specialize TFPGList<TcbClassProperty>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TcbClass = class(TcbComponent, IcbNamedElement)
  private
    fOwnerUnit: TcbUnit;
    fClassName: String;
    fHasClassForwardDeclaration: Boolean;
    fExtendingClass: String;
    fImplementingInterfaces: TStringList;
    fClassVariableList: TcbClassVariableList;
    fClassMethodList: TcbClassMethodList;
    fClassPropertyList: TcbClassPropertyList;
    fClassElementList: TcbNamedClassElementList;
  public
    constructor Create(AOwner: TcbUnit);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
  published
    property ClassName: String read fClassName write fClassName;
    property ExtendingClass: String read fExtendingClass write fExtendingClass;
    property ImplementingInterfaces: TStringList read fImplementingInterfaces write fImplementingInterfaces;
    property ClassVariableList: TcbClassVariableList read fClassVariableList write fClassVariableList;
    property ClassMethodList: TcbClassMethodList read fClassMethodList write fClassMethodList;
    property ClassPropertyList: TcbClassPropertyList read fClassPropertyList write fClassPropertyList;
    property ClassElementList: TcbNamedClassElementList read fClassElementList write fClassElementList;
    property HasClassForwardDeclaration: Boolean read fHasClassForwardDeclaration write fHasClassForwardDeclaration;
  end;

  { TcbClassList }

  TcbClassList = class(specialize TFPGList<TcbClass>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TcbUnit }

  TcbUnit = class(TcbComponent, IcbNamedElement)
  private
    fUnitName: String;
    fUnitFileName: String;
    fUnitTopCommentBlock: TStringList;
    fUnitTopCompilerDirectives: TStringList;
    fUnitInterfaceUsesList: TStringList;
    fUnitImplementationUsesList: TStringList;
    fUnitMethods: TcbMethodList;
    fUnitVariables: TcbVariableList;
    fUnitInitialization: TStringList;
    fUnitFinalization: TStringList;
    fUnitClassList: TcbClassList;
    fUnitConstants: TStringList;
    // TODO -oAPL -cClassBuilder 5: Intefaces, records, constants, sets... and more?
  public
    constructor Create(AOwner: TcbComponent);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
  published
    property UnitName: String read fUnitName write fUnitName;
    property UnitFileName: String read fUnitFileName write fUnitFileName;
    property UnitTopCommentBlock: TStringList read fUnitTopCommentBlock write fUnitTopCommentBlock;
    property UnitTopCompilerDirectives: TStringList read fUnitTopCompilerDirectives write fUnitTopCompilerDirectives;
    property UnitInterfaceUsesList: TStringList read fUnitInterfaceUsesList write fUnitInterfaceUsesList;
    property UnitImplementationUsesList: TStringList read fUnitImplementationUsesList write fUnitImplementationUsesList;
    property UnitMethods: TcbMethodList read fUnitMethods write fUnitMethods;
    property UnitVariables: TcbVariableList read fUnitVariables write fUnitVariables;
    property UnitInitialization: TStringList read fUnitInitialization write fUnitInitialization;
    property UnitFinalization: TStringList read fUnitFinalization write fUnitFinalization;
    property UnitClassList: TcbClassList read fUnitClassList write fUnitClassList;
    property UnitConstants: TStringList read fUnitConstants write fUnitConstants;
  end;

  { TcbUnitList }

  TcbUnitList = class(specialize TFPGList<TcbUnit>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  IcbUnitWriter = interface(IInterface)
    ['{595599B1-6832-4C71-9FC0-56BB768760DC}']
    function WriteUnit(AUnit: TcbUnit): TStrings;
  end;

  { TcbUnitWriter }

  TcbUnitWriter = class(TcbComponent, IcbUnitWriter)
  private
  public
    constructor Create(AOwner: TcbComponent);
    destructor Destroy; override;
    function WriteUnit(AUnit: TcbUnit): TStrings;
  end;


  { TcbCodeWriter }

  TcbCodeWriter = class(TcbComponent)
  private
    fUnitList: TcbUnitList;
  public
    constructor Create(AOwner: TcbComponent);
    destructor Destroy; override;

    property UnitList: TcbUnitList read fUnitList write fUnitList;
  end;

function MethodTypeToString(AMethodType: TcbMethodType): String;
function MethodParameterModifierToString(AParameterModifier: TcbMethodParameterModifier): String;

implementation

function MethodTypeToString(AMethodType: TcbMethodType): String;
begin
  case AMethodType of
    mtProcedure: Result := 'procedure';
    mtFunction: Result := 'function';
    mtConstructor: Result := 'constructor';
    mtDestructor: Result := 'destructor';
  end;
end;

{ TcbUnitList }

constructor TcbUnitList.Create;
begin
  inherited Create;
end;

destructor TcbUnitList.Destroy;
var
  lUnit: TcbUnit;
begin
  for lUnit in Self do
    if Assigned(lUnit) then
      lUnit.Free;

  Self.Clear;

  inherited Destroy;
end;

{ TcbClassList }

constructor TcbClassList.Create;
begin
  inherited Create;
end;

destructor TcbClassList.Destroy;
var
  lClass: TcbClass;
begin
  for lClass in Self do
    if Assigned(lClass) then
      lClass.Free;

  Self.Clear;

  inherited Destroy;
end;

{ TcbClassPropertyList }

constructor TcbClassPropertyList.Create;
begin
  inherited Create;
end;

destructor TcbClassPropertyList.Destroy;
var
  lClassProperty: TcbClassProperty;
begin
  for lClassProperty in Self do
    if Assigned(lClassProperty) then
      lClassProperty.Free;

  Self.Clear;

  inherited Destroy;
end;

{ TcbClassVariableList }

constructor TcbClassVariableList.Create;
begin
  inherited Create;
end;

destructor TcbClassVariableList.Destroy;
var
  lClassVariable: TcbClassVariable;
begin
  for lClassVariable in Self do
    if Assigned(lClassVariable) then
      lClassVariable.Free;

  Self.Clear;

  inherited Destroy;
end;

{ TcbVariableList }

constructor TcbVariableList.Create;
begin
  inherited Create;
end;

destructor TcbVariableList.Destroy;
var
  lVariable: TcbVariable;
begin
  for lVariable in Self do
    if Assigned(lVariable) then
      lVariable.Free;

  Self.Clear;

  inherited Destroy;
end;

{ TcbClassMethodList }

constructor TcbClassMethodList.Create;
begin
  inherited Create;
end;

destructor TcbClassMethodList.Destroy;
var
  lClassMethod: TcbClassMethod;
begin
  for lClassMethod in Self do
    if Assigned(lClassMethod) then
      lClassMethod.Free;

  Self.Clear;

  inherited Destroy;
end;

{ TcbMethodList }

constructor TcbMethodList.Create;
begin
  inherited Create;
end;

destructor TcbMethodList.Destroy;
var
  lMethod: TcbMethod;
begin
  for lMethod in Self do
    if Assigned(lMethod) then
      lMethod.Free;

  Self.Clear;

  inherited Destroy;
end;

{ TcbComponentList }

constructor TcbComponentList.Create;
begin
  inherited Create;
end;

destructor TcbComponentList.Destroy;
var
  lComponent: TcbComponent;
begin
  for lComponent in Self do
    if Assigned(lComponent) then
      lComponent.Free;

  Self.Clear;

  inherited Destroy;
end;

{ TcbMethodParameterList }

constructor TcbMethodParameterList.Create;
begin
  inherited Create;
end;

destructor TcbMethodParameterList.Destroy;
var
  lMethodParameter: TcbMethodParameter;
begin
  for lMethodParameter in Self do
    if Assigned(lMethodParameter) then
      lMethodParameter.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbMethodParameterList.WriteMethodParameters: String;
var
  lMethodParameter: TcbMethodParameter;
begin
  Result := '';

  if Count > 0 then
    Result := '(';

  for lMethodParameter in Self do
    with lMethodParameter do
    begin
      // property ParameterModifier: TcbMethodParameterModifier read fParameterModifier write fParameterModifier;
      Result := Result + MethodParameterModifierToString(ParameterModifier);
      // property ParameterName: String read fParameterName write fParameterName;
      Result := Result + ParameterName;
      // property ParameterType: String read fParameterType write fParameterType;
      Result := Result + ': ' + ParameterType;
      // property ParameterDefault: String read fParameterDefault write fParameterDefault;
      if ParameterDefault <> '' then
        Result := Result + ' = ' + ParameterDefault;
    end;

  if Count > 0 then
    Result := ')';
end;

{ TcbUnitWriter }

constructor TcbUnitWriter.Create(AOwner: TcbComponent);
begin
  inherited Create(AOwner);
end;

destructor TcbUnitWriter.Destroy;
begin

end;

function TcbUnitWriter.WriteUnit(AUnit: TcbUnit): TStrings;
var
  lIndex: Integer;
  lUnitMethod: TcbMethod;
  lClass: TcbClass;
  lVariable: TcbVariable;
begin
  Result := TStringList.Create;

  // property UnitFileName: String read fUnitFileName write fUnitFileName;

  with Result, AUnit do
  begin
    // Write unit top comment
    Add('(*');
    AddStrings(UnitTopCommentBlock);
    Add('*)');

    // Write the unit name
    Add('unit ' + UnitName);

    // Write any unit top compiler directives
    AddStrings(UnitTopCompilerDirectives);

    // Write interface section
    Add('interface');

    // Write unit interface uses clause
    if UnitInterfaceUsesList.Count > 0 then
    begin
      Add('uses');

      for lIndex := 0 to UnitInterfaceUsesList.Count - 1 do
        if lIndex < (UnitInterfaceUsesList.Count - 1) then
          Add(UnitInterfaceUsesList.Strings[lIndex] + ',')
        else
          Add(UnitInterfaceUsesList.Strings[lIndex] + ';');
    end;

    // Write unit constants
    if UnitConstants.Count > 0 then
    begin
      Add('const');
      AddStrings(UnitConstants);
    end;

    // Write the type unit section
    Add('type');

    // Write forward class declarations
    for lClass in UnitClassList do
      if lClass.HasClassForwardDeclaration then
        Add(lClass.ClassName + ' = class;');

    // Write interfaces
    // TODO -oAPL -cCodeBuilder 2: Write interfaces

    // Write types
    // TODO-oAPL -cCodeBuilder 1: Write types

    // Write classless methods in the unit
    if UnitMethods.Count > 0 then
      for lUnitMethod in UnitMethods do
        Add(lUnitMethod.WriteMethodPrototype);

    // Write unit variables
    if UnitVariables.Count > 0 then
    begin
      Add('var');

      for lVariable in UnitVariables do
        Add(lVariable.VariableName + ': ' + lVariable.VariableType);
    end;

    Add('implementation');

    // Write unit interface uses clause
    if UnitImplementationUsesList.Count > 0 then
    begin
      Add('uses');

      for lIndex := 0 to UnitImplementationUsesList.Count - 1 do
        if lIndex < (UnitImplementationUsesList.Count - 1) then
          Add(UnitImplementationUsesList.Strings[lIndex] + ',')
        else
          Add(UnitImplementationUsesList.Strings[lIndex] + ';');
    end;

    // Write unit functions
    if UnitMethods.Count > 0 then
      for lUnitMethod in UnitMethods do
      begin
        Add(lUnitMethod.WriteMethodPrototype);

        if lUnitMethod.ImplementationVars.Count > 0 then
        begin
          Add('var');

          for lVariable in lUnitMethod.ImplementationVars do
            Add(lVariable.fVariableName + ': ' + lVariable.VariableType + ';';
        end;

        Add('begin');
        AddStrings(lUnitMethod.MethodImplementation);
        Add('end;');
      end;

    // Write class implementations
    // TODO-oAPL -cCodeBuilder 1: Write type implementations

    // Write unit initialization section
    if UnitInitialization.Count > 0 then
    begin
      Add('initialization');

      for lIndex := 0 to UnitInitialization.Count - 1 do
        Add(UnitImplementationUsesList.Strings[lIndex]);
    end;

    // Write unit finalization section
    if UnitFinalization.Count > 0 then
    begin
      Add('finalization');

      for lIndex := 0 to UnitFinalization.Count - 1 do
        Add(UnitFinalization.Strings[lIndex]);
    end;
  end;
end;

{ TcbCodeWriter }

constructor TcbCodeWriter.Create(AOwner: TcbComponent; AConfig: TXMLConfig;
  ALogger: TLogger);
begin
  inherited;

  fUnitList := TcbUnitList.Create;
end;

destructor TcbCodeWriter.Destroy;
begin
  if Assigned(fUnitList) then
    fUnitList.Free;

  inherited Destroy;
end;

{ TcbComponent }

constructor TcbComponent.Create(AOwner: TcbComponent; AConfig: TXMLConfig; ALogger: TLogger);
var
  lConfigConsumer: IcbXMLConfigConsumer;
  lLoggerConsumer: IcbLoggerConsumer;
begin
  inherited Create;

  fOwnerComponent := AOwner;

  // Assign the specified config locally, or take from AOwner component if
  // possible
   if Assigned(AConfig) then
    fConfig := AConfig
   else if Assigned(AOwner) then
    if Supports(AOwner, IcbXMLConfigConsumer, lConfigConsumer) then
      fConfig := lConfigConsumer.Config;

  // Assign the specified logger locally, or take from AOwner component if
  // possible
  if Assigned(ALogger) then
    fLogger := ALogger
  else if Assigned(AOwner) then
    if Supports(AOwner, IcbLoggerConsumer, lLoggerConsumer) then
      fLogger := lLoggerConsumer.Log;
end;

destructor TcbComponent.Destroy;
begin
  inherited;
end;

procedure TcbComponent.SetLogger(ALogger: TLogger);
begin
  fLogger := ALogger;
end;

function TcbComponent.GetLogger: TLogger;
begin
  if not Assigned(fLogger) then
    fLogger := TLogger.GetInstance();
  Result := fLogger;
end;

procedure TcbComponent.SetConfig(AConfig: TXMLConfig);
begin
  fConfig := AConfig;
end;

function TcbComponent.GetConfig: TXMLConfig;
begin
  Result := fConfig;
end;

procedure TcbComponent.SetOwnerComponent(AOwnerComponent: TcbComponent);
begin
  fOwnerComponent := AOwnerComponent;
end;

function TcbComponent.GetOwnerComponent: TcbComponent;
begin
  Result := fOwnerComponent;
end;

{ TcbUnit }

constructor TcbUnit.Create(AOwner: TcbComponent; AConfig: TXMLConfig; ALogger: TLogger);
begin
  inherited;

  fUnitTopCommentBlock := TStringList.Create;
  fUnitTopCompilerDirectives := TStringList.Create;
  fUnitInterfaceUsesList := TStringList.Create;
  fUnitImplementationUsesList := TStringList.Create;
  fUnitMethods := TcbMethodList.Create;
  fUnitVariabled := TcbVariableList.Create;
  fUnitInitialization := TStringList.Create;
  fUnitFinalization := TStringList.Create;
  fUnitClassList := TcbClassList.Create;
  fUnitConstants := TStringList.Create;
end;

destructor TcbUnit.Destroy;
begin
  if Assigned(fUnitTopCommentBlock) then
    fUnitTopCommentBlock.Free;
  if Assigned(fUnitTopCompilerDirectives) then
    fUnitTopCompilerDirectives.Free;
  if Assigned(fUnitInterfaceUsesList) then
    fUnitInterfaceUsesList.Free;
  if Assigned(fUnitImplementationUsesList) then
    fUnitImplementationUsesList.Free;
  if Assigned(fUnitMethods) then
    fUnitMethods.Free;
  if Assigned(fUnitVariabled) then
    fUnitVariabled.Free;
  if Assigned(fUnitInitialization) then
    fUnitInitialization.Free;
  if Assigned(fUnitFinalization) then
    fUnitFinalization.Free;
  if Assigned(fUnitClassList) then
    fUnitClassList.Free;
  if Assigned(fUnitConstants) then
    fUnitConstants.Free;

  inherited;
end;

function TcbUnit.GetElementName: String;
begin
  Result := fUnitName;
end;

procedure TcbUnit.SetElementName(AName: String);
begin
  fUnitName := AName;
end;

{ TcbVariable }

constructor TcbVariable.Create(AOwner: TcbComponent; AVariableName: String; AVariableType: String);
begin
  inherited Create(AOwner);

  fVariableName := AVariableName;
  fVariableType := AVariableType;
end;

destructor TcbVariable.Destroy;
begin
  inherited;
end;

function TcbVariable.GetElementName: String;
begin
  Result := fVariableName;
end;

procedure TcbVariable.SetElementName(AName: String);
begin
  fVariableName := AName;
end;

constructor TcbMethod.Create(AOwner: TcbComponent; AMethodName: String; AMethodType: TcbMethodType; AMethodDirectives: TcbMethodDirectives; AReturnType: String
  );
begin
  inherited Create(AOwner);

  fMethodType := AMethodType;
  fMethodName := AMethodName;
  fReturnType := AReturnType;
  fMethodDirectives := AMethodDirectives;
  fParameterList := TcbMethodParameterList.Create;
  fImplementationVars := TcbVariableList.Create;
  fMethodImplementation := TStringList.Create;
end;

destructor TcbMethod.Destroy;
begin
  if Assigned(fParameterList) then
    fParameterList.Free;
  if Assigned(fImplementationVars) then
    fImplementationVars.Free;
  if Assigned(fMethodImplementation) then
    fMethodImplementation.Free;

  inherited Destroy;
end;

function TcbMethod.GetElementName: String;
begin
  Result := fMethodName;
end;

procedure TcbMethod.SetElementName(AName: String);
begin
  fMethodName := AName;
end;

function TcbMethod.WriteMethodPrototype: String;
begin
  // fMethodType: TcbMethodType;
  Result := MethodTypeToString(fMethodType) + ' ';
  // fMethodName: String;
  Result := Result + fMethodName;
  // fParameterList: TcbMethodParameterList;
  Result := Result + fParameterList.WriteMethodParameters;

  if fMethodType = mtFunction then
    Result := Result + ': ' + fReturnType;

  Result := Result + ';';

  if mdtOverride in fMethodDirectives then
    Result := Result + ' override;';
  if mdtOverload in fMethodDirectives then
    Result := Result + ' overload;';
  if mdtAbstract in fMethodDirectives then
    Result := Result + ' abstract;';
  if mdtVirtual in fMethodDirectives then
    Result := Result + ' virtual;'
  else if mdtDynamic in fMethodDirectives then
    Result := Result + ' dynamic;';

end;

{ TcbMethodParameter }

constructor TcbMethodParameter.Create(AOwner: TcbComponent; AParameterName: String; AParameterType: String; AParameterModifier: TcbMethodParameterModifier;
  AParameterDefault: String);
begin
  inherited Create(AOwner);

  fParameterName := AParameterName;
  fParameterType := AParameterType;
  fParameterDefault := AParameterDefault;
  fParameterModifier := AParameterModifier;
end;

destructor TcbMethodParameter.Destroy;
begin
  inherited;
end;

function TcbMethodParameter.GetElementName: String;
begin
  Result := fParameterName;
end;

procedure TcbMethodParameter.SetElementName(AName: String);
begin
  fParameterName := AName;
end;

constructor TcbClass.Create(AOwner: TcbUnit; AConfig: TXMLConfig; ALogger: TLogger);
begin
  inherited Create(AOwner, AConfig, ALogger);

  fOwnerUnit := AOwner;

  fImplementingInterfaces := TStringList.Create;
  fClassVariableList := TcbClassVariableList.Create;
  fClassMethodList := TcbClassMethodList.Create;
  fClassPropertyList := TcbClassPropertyList.Create;
  fClassElementList := TcbNamedClassElementList.Create;
end;

destructor TcbClass.Destroy;
begin
  if Assigned(fImplementingInterfaces) then
    fImplementingInterfaces.Free;
  if Assigned(fClassVariableList) then
    fClassVariableList.Free;
  if Assigned(fClassMethodList) then
    fClassMethodList.Free;
  if Assigned(fClassPropertyList) then
    fClassPropertyList.Free;
  if Assigned(fClassElementList) then
    fClassElementList.Free;

  inherited;
end;

function TcbClass.GetElementName: String;
begin
  Result := fClassName;
end;

procedure TcbClass.SetElementName(AName: String);
begin
  fClassName := AName;
end;

{ TcbClassProperty }

function TcbClassProperty.GetElementName: String;
begin
  Result := fPropertyName;
end;

procedure TcbClassProperty.SetElementName(AName: String);
begin
  fPropertyName := AName;
end;

function TcbClassProperty.GetClassDeclaration: String;
begin
  // TODO: Make code taht generates the property class code
end;

function TcbClassProperty.GetOwnerClass: TcbClass;
begin
  Result := fOwnerClass;
end;

procedure TcbClassProperty.SetOwnerClass(AOwnerClass: TcbClass);
begin
  fOwnerClass := AOwnerClass;
end;

constructor TcbClassProperty.Create(AOwner: TcbClass; APropertyName: String; APropertyType: String; AClassSection: TcbClassSection;
  APropertyReadElement: IcbNamedClassElement; APropertyWriteElement: IcbNamedClassElement);
begin
  inherited Create(AOwner);

  fOwnerClass := AOwner;
  fPropertyName := APropertyName;
  fPropertyType := APropertyType;
  fClassSection := AClassSection;
  fPropertyReadElement := APropertyReadElement;
  fPropertyWriteElement := APropertyWriteElement;
end;

destructor TcbClassProperty.Destroy;
begin
  inherited;
end;

{ TcbClassVariable }

function TcbClassVariable.GetClassDeclaration: String;
begin
  // TODO: Make code that generates the declaration
end;

function TcbClassVariable.GetOwnerClass: TcbClass;
begin
  Result := fOwnerClass;
end;

procedure TcbClassVariable.SetOwnerClass(AOwnerClass: TcbClass);
begin
  fOwnerClass := AOwnerClass;
end;

constructor TcbClassVariable.Create(AOwner: TcbClass);
begin
  inherited Create(AOwner);

  fOwnerClass := AOwner;
end;

destructor TcbClassVariable.Destroy;
begin
  inherited;
end;

{ TcbClassMethod }

function TcbClassMethod.GetClassDeclaration: String;
begin
  // TODO: Write code that generates the declaration, the prototype
end;

function TcbClassMethod.GetOwnerClass: TcbClass;
begin
  Result := fOwnerClass;
end;

procedure TcbClassMethod.SetOwnerClass(AOwnerClass: TcbClass);
begin
  fOwnerClass := AOwnerClass;
end;

constructor TcbClassMethod.Create(AOwner: TcbClass);
begin
  inherited Create(AOwner);

  fOwnerClass := AOwner;
end;

destructor TcbClassMethod.Destroy;
begin
  inherited;
end;

end.

