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

  IfpClassElement = interface(IInterface)
    ['{294F48AA-5C20-4B01-97A9-C7D7CEB633B2}']
    function GetName: String;
    procedure SetName(AName: String);
    function GetClassDeclaration: String;
    property Name: String read GetName write SetName;
    property ClassDeclaration: String read GetClassDeclaration;
  end;

  TfpClassElementList = class(specialize TFPGList<IfpClassElement>);

  { TfpMethodParameter }

  TfpMethodParameter = class(TObject)
  private
    fParameterName: String;
    fParameterType: String;
    fParameterDefault: String;
    fParameterModifier: TfpMethodParameterModifier;
  public
    constructor Create;
    destructor Destroy; override;

    property ParameterName: String read fParameterName write fParameterName;
    property ParameterType: String read fParameterType write fParameterType;
    property ParameterDefault: String read fParameterDefault write fParameterDefault;
    property ParameterModifier: TfpMethodParameterModifier read fParameterModifier write fParameterModifier;
  end;

  TfpMethodParameterList = class(specialize TFPGList<TfpMethodParameter>);

  TfpMethod = class(TInterfacedObject, IInterface)
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

    property MethodType: TfpMethodType read fMethodType write fMethodType;
    property MethodName: String read fMethodName write fMethodName;
    property ReturnType: String read fReturnType write fReturnType;
    property ParameterList: TfpMethodParameterList read fParameterList write fParameterList;
    property ImplementationVars: TfpVariableList read fImplementationVars write fImplementationVars;
    property MethodImplementation: TStringList read fMethodImplementation write fMethodImplementation;
  end;

  TfpMethodList = class(specialize TFPGList<TfpMethod>);

  { TfpClassMethod }

  TfpClassMethod = class(TfpMethod, IfpClassElement)
  private
    fOwnerClass: TfpClass;
    fClassSection : TfpClassSection;

    function GetName: String;
    procedure SetName(AName: String);
    function GetClassDeclaration: String;
  public
    constructor Create;
    destructor Destroy; override;

    property OwnerClass: TfpClass read fOwnerClass write fOwnerClass;
    property ClassSection: TfpClassSection read fClassSection  write fClassSection ;
  end;

  TfpClassMethodList = class(specialize TFPGList<TfpClassMethod>);

  { TfpVariable }

  TfpVariable = class(TInterfacedObject, IInterface)
  private
    fVariableName: String;
    fVariableType: String;
  public
    constructor Create;
    destructor Destroy; override;

    property VariableName: String read fVariableName write fVariableName;
    property VariableType: String read fVariableType write fVariableType;
  end;

  TfpVariableList = class(specialize TFPGList<TfpVariable>);

  { TfpClassVariable }

  TfpClassVariable = class(TfpVariable, IfpClassElement)
  private
    fOwnerClass: TfpClass;
    fClassSection : TfpClassSection;

    function GetName: String;
    procedure SetName(AName: String);
    function GetClassDeclaration: String;
  public
    constructor Create;
    destructor Destroy; override;

    property OwnerClass: TfpClass read fOwnerClass write fOwnerClass;
    property ClassSection : TfpClassSection read fClassSection  write fClassSection ;
  end;

  TfpClassVariableList = class(specialize TFPGList<TfpClassVariable>);

  { TfpClassProperty }

  TfpClassProperty = class(TInterfacedObject, IfpClassElement)
  private
    fOwnerClass: TfpClass;
    fClassSection : TfpClassSection;
    fPropertyName: String;
    fPropertyType: String;
    fPropertyReadElement: IfpClassElement;
    fPropertyWriteElement: IfpClassElement;

    function GetName: String;
    procedure SetName(AName: String);
    function GetClassDeclaration: String;
  public
    constructor Create;
    destructor Destroy; override;

    property OwnerClass: TfpClass read fOwnerClass write fOwnerClass;
    property ClassSection : TfpClassSection read fClassSection  write fClassSection ;
    property PropertyName: String read fPropertyName write fPropertyName;
    property PropertyType: String read fPropertyType write fPropertyType;
    property PropertyReadElement: IfpClassElement read fPropertyReadElement write fPropertyReadElement;
    property PropertyWriteElement: IfpClassElement read fPropertyWriteElement write fPropertyWriteElement;
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
    fClassElementList: TfpClassElementList;
  public
    constructor Create;
    destructor Destroy; override;

    property ClassName: String read fClassName write fClassName;
    property ExtendingClass: String read fExtendingClass write fExtendingClass;
    property ImplementingInterfaces: TStringList read fImplementingInterfaces write fImplementingInterfaces;
    property ClassVariableList: TfpClassVariableList read fClassVariableList write fClassVariableList;
    property ClassMethodList: TfpClassMethodList read fClassMethodList write fClassMethodList;
    property ClassPropertyList: TfpClassPropertyList read fClassPropertyList write fClassPropertyList;
    property ClassElementList: TfpClassElementList read fClassElementList write fClassElementList;
  end;

  TfpClassList = class(specialize TFPGList<TfpClass>);

implementation

{ TfpVariable }

constructor TfpVariable.Create;
begin
  inherited;
end;

destructor TfpVariable.Destroy;
begin
  inherited Destroy;
end;

constructor TfpMethod.Create;
begin
  inherited;
end;

destructor TfpMethod.Destroy;
begin
  inherited Destroy;
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

function TfpClassVariable.GetName: String;
begin
  Result := fVariableName;
end;

procedure TfpClassVariable.SetName(AName: String);
begin
  fVariableName := AName;
end;

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

function TfpClassMethod.GetName: String;
begin
  Result := fMethodName;
end;

procedure TfpClassMethod.SetName(AName: String);
begin
  fMethodName := AName;
end;

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

