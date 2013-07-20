unit uclassbuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl;

type
  TfpMethodParameterModifier = (
    mpmNone,
    mpmOut,
    mpmConst,
    mpmVar);

  TfpMethodDirectiveTypes = (
    mdtOverride,
    mdtOverload,
    mdtAbstract,
    mdtVirtual,
    mdtDynamic);

  TfpMethodDirectives = set of TfpMethodDirectiveTypes;

  TfpMethodType = (
    mtProcedure,
    mtFunction,
    mtConstructor,
    mtDestructor);

  TfpClassSection = (
    cPackage,
    csPrivate,
    csPublic,
    csProtected,
    csPublished);

  TfpClass = class;
  TfpClassList = class;
  TfpMethodParameter = class;
  TfpMethodParameterList = class;
  TfpMethod = class;
  TfpMethodList = class;
  TfpClassMethod = class;
  TfpClassMethodList = class;
  TfpVariable = class;
  TfpVariableList = class;
  TfpClassVariable = class;
  TfpClassVariableList = class;
  TfpClassProperty = class;
  TfpClassPropertyList = class;
  TfpUnit = class;
  TpfUnitList = class;

  TfpCodeBuilderConfiguration

  IfpNamedElement = interface(IInterface)
    ['{035C5796-ABB1-4523-A20A-4A7AC382CF47}']
    function GetName: String;
    procedure SetName(AName: String);
    property Name: String read GetName write SetName;
  end;

  TfpNamedElementList = class(specialize TFPGList<IfpNamedElement>);

  IfpNamedClassElement = interface(IfpNamedElement)
    ['{294F48AA-5C20-4B01-97A9-C7D7CEB633B2}']
    function GetClassDeclaration: String;
    property ClassDeclaration: String read GetClassDeclaration;
  end;

  TfpNamedClassElementList = class(specialize TFPGList<IfpNamedClassElement>);

  { TfpMethodParameter }

  TfpMethodParameter = class(TInterfacedPersistent, IfpNamedElement)
  private
    fParameterName: String;
    fParameterType: String;
    fParameterDefault: String;
    fParameterModifier: TfpMethodParameterModifier;
  public
    constructor Create;
    destructor Destroy; override;

    function GetName: String;
    procedure SetName(AName: String);

    property ParameterName: String read fParameterName write fParameterName;
    property ParameterType: String read fParameterType write fParameterType;
    property ParameterDefault: String read fParameterDefault write fParameterDefault;
    property ParameterModifier: TfpMethodParameterModifier read fParameterModifier write fParameterModifier;
  end;

  TfpMethodParameterList = class(specialize TFPGList<TfpMethodParameter>);

  TfpMethod = class(TInterfacedPersistent, IfpNamedElement)
  private
    fMethodType: TfpMethodType;
    fMethodName: String;
    fReturnType: String;
    fParameterList: TfpMethodParameterList;
    fImplementationVars: TfpVariableList;
    fMethodImplementation: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetName: String;
    procedure SetName(AName: String);

    property MethodType: TfpMethodType read fMethodType write fMethodType;
    property MethodName: String read fMethodName write fMethodName;
    property ReturnType: String read fReturnType write fReturnType;
    property ParameterList: TfpMethodParameterList read fParameterList write fParameterList;
    property ImplementationVars: TfpVariableList read fImplementationVars write fImplementationVars;
    property MethodImplementation: TStringList read fMethodImplementation write fMethodImplementation;
  end;

  TfpMethodList = class(specialize TFPGList<TfpMethod>);

  { TfpClassMethod }

  TfpClassMethod = class(TfpMethod, IfpNamedClassElement)
  private
    fOwnerClass: TfpClass;
    fClassSection : TfpClassSection;
  public
    constructor Create;
    destructor Destroy; override;

    function GetClassDeclaration: String;

    property OwnerClass: TfpClass read fOwnerClass write fOwnerClass;
    property ClassSection: TfpClassSection read fClassSection  write fClassSection ;
  end;

  TfpClassMethodList = class(specialize TFPGList<TfpClassMethod>);

  { TfpVariable }

  TfpVariable = class(TInterfacedObject, IfpNamedElement)
  private
    fVariableName: String;
    fVariableType: String;
  public
    constructor Create;
    destructor Destroy; override;

    function GetName: String;
    procedure SetName(AName: String);

    property VariableName: String read fVariableName write fVariableName;
    property VariableType: String read fVariableType write fVariableType;
  end;

  TfpVariableList = class(specialize TFPGList<TfpVariable>);

  { TfpClassVariable }

  TfpClassVariable = class(TfpVariable, IfpNamedClassElement)
  private
    fOwnerClass: TfpClass;
    fClassSection : TfpClassSection;

  public
    constructor Create;
    destructor Destroy; override;

    function GetClassDeclaration: String;

    property OwnerClass: TfpClass read fOwnerClass write fOwnerClass;
    property ClassSection : TfpClassSection read fClassSection  write fClassSection ;
  end;

  TfpClassVariableList = class(specialize TFPGList<TfpClassVariable>);

  { TfpClassProperty }

  TfpClassProperty = class(TInterfacedObject, IfpNamedClassElement)
  private
    fOwnerClass: TfpClass;
    fClassSection : TfpClassSection;
    fPropertyName: String;
    fPropertyType: String;
    fPropertyReadElement: IfpNamedClassElement;
    fPropertyWriteElement: IfpNamedClassElement;

  public
    constructor Create;
    destructor Destroy; override;

    function GetName: String;
    procedure SetName(AName: String);
    function GetClassDeclaration: String;

    property OwnerClass: TfpClass read fOwnerClass write fOwnerClass;
    property ClassSection : TfpClassSection read fClassSection  write fClassSection ;
    property PropertyName: String read fPropertyName write fPropertyName;
    property PropertyType: String read fPropertyType write fPropertyType;
    property PropertyReadElement: IfpNamedClassElement read fPropertyReadElement write fPropertyReadElement;
    property PropertyWriteElement: IfpNamedClassElement read fPropertyWriteElement write fPropertyWriteElement;
  end;

  TfpClassPropertyList = class(specialize TFPGList<TfpClassProperty>);

  TfpClass = class(TObject)
  private
    fClassName: String;
    fExtendingClass: String;
    fImplementingInterfaces: TStringList;
    fClassVariableList: TfpClassVariableList;
    fClassMethodList: TfpClassMethodList;
    fClassPropertyList: TfpClassPropertyList;
    fClassElementList: TfpNamedClassElementList;
  public
    constructor Create;
    destructor Destroy; override;

    property ClassName: String read fClassName write fClassName;
    property ExtendingClass: String read fExtendingClass write fExtendingClass;
    property ImplementingInterfaces: TStringList read fImplementingInterfaces write fImplementingInterfaces;
    property ClassVariableList: TfpClassVariableList read fClassVariableList write fClassVariableList;
    property ClassMethodList: TfpClassMethodList read fClassMethodList write fClassMethodList;
    property ClassPropertyList: TfpClassPropertyList read fClassPropertyList write fClassPropertyList;
    property ClassElementList: TfpNamedClassElementList read fClassElementList write fClassElementList;
  end;

  TfpClassList = class(specialize TFPGList<TfpClass>);

  { TfpUnit }

  TfpUnit = class(TInterfacedPersistent, IfpNamedElement)
  private
    fUnitName: String;
    fUnitTopCommentBlock: TStringList;
    fUnitTopCompilerDirectives: TStringList;
    fUnitInterfaceUsesList: TStringList;
    fUnitImplementationUsesList: TStringList;
    fUnitMethods: TfpMethodList;
    fUnitVariabled: TfpVariableList;
    fUnitInitialization: TStringList;
    fUnitFinalization: TStringList;
    fUnitClassList TfpClassList;
    fUnitConstants: TStringList;
    // TODO -oAPL -cClassBuilders 5: Intefaces, records, constants, sets... and more?
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TfpUnit }

constructor TfpUnit.Create;
begin
  inherited;
end;

destructor TfpUnit.Destroy;
begin
  inherited Destroy;
end;

{ TfpVariable }

constructor TfpVariable.Create;
begin
  inherited;
end;

destructor TfpVariable.Destroy;
begin
  inherited Destroy;
end;

function TfpVariable.GetName: String;
begin
  Result := fVariableName;
end;

procedure TfpVariable.SetName(AName: String);
begin
  fVariableName := AName;
end;

constructor TfpMethod.Create;
begin
  inherited;
end;

destructor TfpMethod.Destroy;
begin
  inherited Destroy;
end;

function TfpMethod.GetName: String;
begin
  Result := fMethodName;
end;

procedure TfpMethod.SetName(AName: String);
begin
  fMethodName := AName;
end;

{ TfpMethodParameter }

constructor TfpMethodParameter.Create;
begin
  inherited;
end;

destructor TfpMethodParameter.Destroy;
begin
  inherited Destroy;
end;

function TfpMethodParameter.GetName: String;
begin
  Result := fParameterName;
end;

procedure TfpMethodParameter.SetName(AName: String);
begin
  fParameterName := AName;
end;

constructor TfpClass.Create;
begin
  inherited;
end;

destructor TfpClass.Destroy;
begin
  inherited Destroy;
end;

{ TfpClassProperty }

function TfpClassProperty.GetName: String;
begin
  Result := fPropertyName;
end;

procedure TfpClassProperty.SetName(AName: String);
begin
  fPropertyName := AName;
end;

function TfpClassProperty.GetClassDeclaration: String;
begin
  // TODO: Make code taht generates the property class code
end;

constructor TfpClassProperty.Create;
begin
  inherited Create;
end;

destructor TfpClassProperty.Destroy;
begin
  inherited Destroy;
end;

{ TfpClassVariable }

function TfpClassVariable.GetClassDeclaration: String;
begin
  // TODO: Make code that generates the declaration
end;

constructor TfpClassVariable.Create;
begin
  inherited Create;
end;

destructor TfpClassVariable.Destroy;
begin
  inherited Destroy;
end;

{ TfpClassMethod }

function TfpClassMethod.GetClassDeclaration: String;
begin
  // TODO: Write code that generates the declaration, the prototype
end;

constructor TfpClassMethod.Create;
begin
  inherited Create;

end;

destructor TfpClassMethod.Destroy;
begin

  inherited Destroy;
end;

end.

