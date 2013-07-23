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

var
  EMPTY_GUID: TGUID;

type
  TcbCodeCommentStyle = (
    ccsSlashes,
    ccsBrackets,
    ccsParenthesis);

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
  TcbSourceComment = class;
  TcbSourceCommentBlock = class;
  TcbCodeLine = class;
  TcbCodeBlock = class;
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
  TcbInterfaceProperty = class;
  TcbInterfacePropertyList = class;
  TcbInterface = class;
  TcbInterfaceList = class;

  IcbNamedElement = interface(IInterface)
    ['{035C5796-ABB1-4523-A20A-4A7AC382CF47}']
    function GetElementName: String;
    procedure SetElementName(AName: String);
    property ElementName: String read GetElementName write SetElementName;
  end;

  TcbNamedElementList = class(specialize TFPGList<IcbNamedElement>);

  IcbClassElement = interface(IcbNamedElement)
    ['{161FD953-6C66-472A-873B-C4167CE45A60}']
    function GetOwnerClass: TcbClass;
    procedure SetOwnerClass(AOwnerClass: TcbClass);
    property OwnerClass: TcbClass read GetOwnerClass write SetOwnerClass;
  end;

  TcbClassElementList = class(specialize TFPGList<IcbClassElement>);

  IcbInterfaceElement = interface(IcbNamedElement)
    ['{206DAD07-1AC9-4DF1-8A4C-C39B9ACE9A99}']
    function GetOwnerInterface: TcbInterface;
    procedure SetOwnerInterface(AOwnerInterface: TcbInterface);
    property OwnerInterface: TcbInterface read GetOwnerInterface write SetOwnerInterface;
  end;

  TcbInterfaceElementList = class(specialize TFPGList<IcbInterfaceElement>);

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

  { TcbSourceCodeComponent }

  TcbSourceCodeComponent = class(TcbComponent)
  private
    fSourceCodeStrings: TStringList;
  public
    constructor Create(AOwner: TcbComponent);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; virtual;
  end;

  { TcbSourceCodeLines }

  TcbSourceCodeLines = class(specialize TFPGList<TcbSourceCodeComponent>)
   private
     fOwner: TcbComponent;
     fSourceCodeLines: TStringList;
   public
     constructor Create(AOwner: TcbComponent);
     destructor Destroy; override;
     function WriteSourceCode: TStringList; virtual;
   end;

  { TcbSourceComment }

  TcbSourceComment = class(TcbSourceCodeComponent)
  private
    fSourceComment: String;
    fCommentStyle: TcbCodeCommentStyle;
  public
    constructor Create(AOwner: TcbComponent; ASourceComment: String; ACommentStyle: TcbCodeCommentStyle = ccsSlashes);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
    property SourceComment: String read fSourceComment write fSourceComment;
    property CommentStyle: TcbCodeCommentStyle read fCommentStyle write fCommentStyle;
  end;

  { TcbSourceCommentBlock }

  TcbSourceCommentBlock = class(TcbSourceCodeLines)
  public
    constructor Create(AOwner: TcbComponent);
    destructor Destroy; override;
    procedure Add(ASourceComment: String; ACommentStyle: TcbCodeCommentStyle = ccsSlashes);
  end;

  { TcbCodeLine }

  TcbCodeLine = class(TcbSourceCodeComponent)
  private
    fOwnerMethod: TcbMethod;
    fCodeLine: String;
    fComment: TcbSourceComment;
  public
    constructor Create(AOwnerMethod: TcbMethod; ACodeLine: String; AComment: TcbSourceComment = nil);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
    property CodeLine: String read fCodeLine write fCodeLine;
    property Comment: TcbSourceComment read fComment write fComment;
    property OwnerMethod: TcbMethod read fOwnerMethod;
  end;

  { TcbCodeBlock }

  TcbCodeBlock = class(TcbSourceCodeLines)
  private
    fOwnerMethod: TcbMethod;
  public
    constructor Create(AOwnerMethod: TcbMethod);
    destructor Destroy; override;
    procedure Add(ACodeString: String; AComment: TcbSourceComment = nil); overload;
  end;

  { TcbMethodParameter }

  TcbMethodParameter = class(TcbComponent, IcbNamedElement)
  private
    fOwnerMethod: TcbMethod;
    fParameterName: String;
    fParameterType: String;
    fParameterDefault: String;
    fParameterModifier: TcbMethodParameterModifier;
  public
    constructor Create(
      AOwnerMethod: TcbMethod;
      AParameterName: String;
      AParameterType: String;
      AParameterModifier: TcbMethodParameterModifier = mpmNone;
      AParameterDefault: String = '');
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function WriteParameter: String;
  published
    property OwnerMethod: TcbMethod read fOwnerMethod write fOwnerMethod;
    property ParameterName: String read fParameterName write fParameterName;
    property ParameterType: String read fParameterType write fParameterType;
    property ParameterDefault: String read fParameterDefault write fParameterDefault;
    property ParameterModifier: TcbMethodParameterModifier read fParameterModifier write fParameterModifier;
  end;

  { TcbMethodParameterList }

  TcbMethodParameterList = class(specialize TFPGList<TcbMethodParameter>)
  private
    fOwnerMethod: TcbMethod;
  public
    constructor Create(AOwnerMethod: TcbMethod);
    destructor Destroy; override;
    function WriteParameters: String;
    procedure Add(
      AParameterName: String;
      AParameterType: String;
      AParameterModifier: TcbMethodParameterModifier = mpmNone;
      AParameterDefault: String = ''); overload;
  published
    property OwnerMethod: TcbMethod read fOwnerMethod write fOwnerMethod;
  end;

  { TcbMethod }

  TcbMethod = class(TcbSourceCodeComponent, IcbNamedElement)
  private
    fMethodType: TcbMethodType;
    fMethodName: String;
    fReturnType: String;
    fLeadingCommentBlock: TcbSourceCommentBlock;
    fMethodDirectives: TcbMethodDirectives;
    fParameterList: TcbMethodParameterList;
    fImplementationVars: TcbVariableList;
    fMethodImplementation: TcbCodeBlock;

    function WriteImplementationPrototype: String;
  public
    constructor Create(
      AOwner: TcbComponent;
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      ALeadingCommentBlock: TcbSourceCommentBlock = nil;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: String = '');
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function WritePrototype: String; virtual;
    function WriteImplementation: TStringList; virtual;
    procedure AddParameter(AParameter: TcbMethodParameter); overload;
    procedure AddParameter(
      AParameterName: String;
      AParameterType: String;
      AParameterModifier: TcbMethodParameterModifier = mpmNone;
      AParameterDefault: String = ''); overload;
    procedure AddVariable(AVariable: TcbVariable); overload;
    procedure AddVariable(
      AVariableName: String;
      AVariableType: String); overload;
  published
    property MethodType: TcbMethodType read fMethodType write fMethodType;
    property MethodName: String read fMethodName write fMethodName;
    property ReturnType: String read fReturnType write fReturnType;
    property MethodDirectives: TcbMethodDirectives read fMethodDirectives write fMethodDirectives;
    property ParameterList: TcbMethodParameterList read fParameterList write fParameterList;
    property ImplementationVars: TcbVariableList read fImplementationVars write fImplementationVars;
    property MethodImplementation: TcbCodeBlock read fMethodImplementation write fMethodImplementation;
    property LeadingCommentBlock: TcbSourceCommentBlock read fLeadingCommentBlock write fLeadingCommentBlock;
  end;

  { TcbMethodList }

  TcbMethodList = class(specialize TFPGList<TcbMethod>)
  private
    fOwner: TcbComponent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      ALeadingCommentBlock: TcbSourceCommentBlock = nil;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: String = '');

  end;

  { TcbClassMethod }

  TcbClassMethod = class(TcbMethod, IcbClassElement)
  private
    fOwnerClass: TcbClass;
    fClassSection : TcbClassSection;
  public
    constructor Create(
      AOwnerClass: TcbClass;
      AMethodName: String;
      AClassSection : TcbClassSection;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: String = '');
    destructor Destroy; override;
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
    function WriteVariable: String;
  published
    property VariableName: String read fVariableName write fVariableName;
    property VariableType: String read fVariableType write fVariableType;
  end;

  { TcbVariableList }

  TcbVariableList = class(specialize TFPGList<TcbVariable>)
  private
    fOwner: TcbComponent;
    fVariableStrings: TStringList;
  public
    constructor Create(AOwner: TcbComponent);
    destructor Destroy; override;
    procedure Add(
      AVariableName: String;
      AVariableType: String); overload;
    function WriteVariables: TStringList; virtual;
  end;

  { TcbClassVariable }

  TcbClassVariable = class(TcbVariable, IcbClassElement)
  private
    fOwnerClass: TcbClass;
    fClassSection : TcbClassSection;
  public
    constructor Create(
      AOwnerClass: TcbClass;
      AVariableName: String;
      AVariableType: String;
      AClassSection : TcbClassSection = csPackage);
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

  { TcbInterfaceProperty }

  TcbInterfaceProperty = class(TcbComponent, IcbInterfaceElement)
  private
    fOwnerInterface: TcbInterface;
    fClassSection : TcbClassSection;
    fPropertyName: String;
    fPropertyType: String;
    fPropertyReadElement: IcbInterfaceElement;
    fPropertyWriteElement: IcbInterfaceElement;
    fInterfaceDeclaration: TStringList;
  public
    constructor Create(
      AOwner: TcbInterface;
      APropertyName: String;
      APropertyType: String;
      APropertyReadElement: IcbInterfaceElement = nil;
      APropertyWriteElement: IcbInterfaceElement = nil);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function GetOwnerInterface: TcbInterface;
    procedure SetOwnerInterface(AOwnerInterface: TcbInterface);
    function WritePropertyDeclaration: String;
  published
    property ClassSection : TcbClassSection read fClassSection  write fClassSection ;
    property PropertyName: String read fPropertyName write fPropertyName;
    property PropertyType: String read fPropertyType write fPropertyType;
    property PropertyReadElement: IcbInterfaceElement read fPropertyReadElement write fPropertyReadElement;
    property PropertyWriteElement: IcbInterfaceElement read fPropertyWriteElement write fPropertyWriteElement;
  end;

  { TcbInterfacePropertyList }

  TcbInterfacePropertyList = class(specialize TFPGList<TcbInterfaceProperty>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TcbClassProperty }

  TcbClassProperty = class(TcbComponent, IcbClassElement)
  private
    fOwnerClass: TcbClass;
    fClassSection : TcbClassSection;
    fPropertyName: String;
    fPropertyType: String;
    fPropertyReadElement: IcbClassElement;
    fPropertyWriteElement: IcbClassElement;
  public
    constructor Create(
      AOwner: TcbClass;
      APropertyName: String;
      APropertyType: String;
      AClassSection : TcbClassSection = csPackage;
      APropertyReadElement: IcbClassElement = nil;
      APropertyWriteElement: IcbClassElement = nil);
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
    property PropertyReadElement: IcbClassElement read fPropertyReadElement write fPropertyReadElement;
    property PropertyWriteElement: IcbClassElement read fPropertyWriteElement write fPropertyWriteElement;
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
    fClassElementList: TcbClassElementList;
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
    property ClassElementList: TcbClassElementList read fClassElementList write fClassElementList;
    property HasClassForwardDeclaration: Boolean read fHasClassForwardDeclaration write fHasClassForwardDeclaration;
  end;

  { TcbClassList }

  TcbClassList = class(specialize TFPGList<TcbClass>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TcbInterface = class(TcbComponent, IcbNamedElement)
  private
    fOwnerUnit: TcbUnit;
    fInterfaceName: String;
    fInterfaceExtends: String;
    fInterfaceGUID: TGUID;
    fInterfaceMethodList: TcbMethodList;
    fInterfacePropertyList: TcbInterfacePropertyList;
    fTypeDeclaration: TStringList;
  public
    constructor Create(
      AOwner: TcbUnit;
      AInterfaceName: String;
      AInterfaceGUID: TGUID;
      AInterfaceExtends: String = '');
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function WriteTypeDeclaration: TStringList;
    property OwnerUnit: TcbUnit read fOwnerUnit write fOwnerUnit;
    property InterfaceName: String read fInterfaceName write fInterfaceName;
    property InterfaceExtends: String read fInterfaceExtends write fInterfaceExtends;
    property InterfaceGUID: TGUID read fInterfaceGUID write fInterfaceGUID;
    property InterfaceMethodList: TcbMethodList read fInterfaceMethodList write fInterfaceMethodList;
    property InterfacePropertyList: TcbInterfacePropertyList read fInterfacePropertyList write fInterfacePropertyList;
  end;

  { TcbInterfaceList }

  TcbInterfaceList = class(specialize TFPGList<TcbInterface>)
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
    fUnitInterfaces: TcbInterfaceList;

    // TODO -oAPL -cClassBuilder 5: Records, sets... and more?
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
    property UnitInterfaces: TcbInterfaceList read fUnitInterfaces write fUnitInterfaces;
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

function MethodParameterModifierToString(AParameterModifier: TcbMethodParameterModifier): String;
begin
  case AParameterModifier of
    mpmNone: Result := '';
    mpmOut: Result := 'out';
    mpmConst: Result := 'const';
    mpmVar: Result := 'var';
  end;
end;

{ TcbCodeBlock }

constructor TcbCodeBlock.Create(AOwnerMethod: TcbMethod);
begin
  inherited Create(AOwnerMethod);

  fOwnerMethod := AOwnerMethod;
end;

destructor TcbCodeBlock.Destroy;
begin
  inherited Destroy;
end;

procedure TcbCodeBlock.Add(ACodeString: String; AComment: TcbSourceComment);
var
  lCodeLine: TcbCodeLine;
begin
  lCodeLine := TcbCodeLine.Create(fOwnerMethod, ACodeString, AComment);
  Add(lCodeLine);
end;

{ TcbSourceCommentBlock }

constructor TcbSourceCommentBlock.Create(AOwner: TcbComponent);
begin
  inherited Create(AOwner);
end;

destructor TcbSourceCommentBlock.Destroy;
begin
  inherited Destroy;
end;

procedure TcbSourceCommentBlock.Add(ASourceComment: String; ACommentStyle: TcbCodeCommentStyle);
var
  lComment: TcbSourceComment;
begin
  lComment := TcbSourceComment.Create(
    fOwner,
    ASourceComment,
    ACommentStyle);

  inherited Add(lComment);
end;

{ TcbSourceCodeLines }

constructor TcbSourceCodeLines.Create(AOwner: TcbComponent);
begin
  inherited Create;

  fOwner := AOwner;
  fSourceCodeLines := TStringList.Create;
end;

destructor TcbSourceCodeLines.Destroy;
begin
  if Assigned(fSourceCodeLines) then
    fSourceCodeLines.Free;

  inherited Destroy;
end;

function TcbSourceCodeLines.WriteSourceCode: TStringList;
var
  lSourceCodeComponent: TcbSourceCodeComponent;
begin
  fSourceCodeLines.Clear;

  fSourceCodeLines.Add('begin');

  for lSourceCodeComponent in Self do
    fSourceCodeLines.AddStrings(lSourceCodeComponent.WriteSourceCode);

  fSourceCodeLines.Add('end');

  Result := fSourceCodeLines;
end;

{ TcbSourceCodeComponent }

constructor TcbSourceCodeComponent.Create(AOwner: TcbComponent);
begin
  inherited Create(AOwner);

  fSourceCodeStrings := TStringList.Create;
end;

destructor TcbSourceCodeComponent.Destroy;
begin
  if Assigned(fSourceCodeStrings) then
    fSourceCodeStrings.Free;

  inherited Destroy;
end;

function TcbSourceCodeComponent.WriteSourceCode: TStringList;
begin
  fSourceCodeStrings.Clear;
  // NOTE: Not sure if this is possible
  Result := fSourceCodeStrings;
end;

{ TcbSourceComment }

constructor TcbSourceComment.Create(AOwner: TcbComponent; ASourceComment: String; ACommentStyle: TcbCodeCommentStyle);
begin
  inherited Create(AOwner);

  fSourceComment := ASourceComment;
  fCommentStyle := ACommentStyle;
end;

destructor TcbSourceComment.Destroy;
begin
  inherited Destroy;
end;

function TcbSourceComment.WriteSourceCode: TStringList;
var
  lCommentStrings: TStringList;
  lStringEnum: TStringsEnumerator;
begin
  Result := inherited WriteSourceCode;

  with Result do
  begin
    case fCommentStyle of
      ccsSlashes:
      begin
        // TODO -oAPL -cClassBuilder 3: The comment string can contain linefeeds, so either clean the string, og parse it into a stringlist and preceed each line with the slashes, if that is chosen    end

        lCommentStrings := TStringList.Create;

        try
          lCommentStrings.Text := fSourceComment;
          lStringEnum := lCommentStrings.GetEnumerator;

          try
            while lStringEnum.MoveNext do
              Add('// ' + lStringEnum.Current);

          finally
            lStringEnum.Free;
          end;
        finally
          lCommentStrings.Free;;
        end;
      end;
      ccsBrackets: Add('{ ' + fSourceComment + ' }');
      ccsParenthesis: Add('(* ' + fSourceComment + ' *)');
    end;
  end;

  // NOTE: Depending on if the inherited assignment of Result worked, this is not needed
  // Result := fCommentLines;
end;

{ TcbCodeLine }

constructor TcbCodeLine.Create(AOwnerMethod: TcbMethod; ACodeLine: String; AComment: TcbSourceComment);
begin
  inherited Create(AOwnerMethod);

  fOwnerMethod := AOwnerMethod;
  fCodeLine := ACodeLine;
  fComment := AComment;
end;

destructor TcbCodeLine.Destroy;
begin
  inherited Destroy;
end;

function TcbCodeLine.WriteSourceCode: TStringList;
begin
  Result := inherited WriteSourceCode;
  // TODO -oAPL -cClassBuilder 5: Introduce a variable to control if the comment should be writted before or after the codeline

  with Result do
  begin
    // Write the comment, if any
    if Assigned(fComment) then
      AddStrings(fComment.WriteSourceCode);

    // Write the codeline
    Add(fCodeLine);
  end;

  // NOTE: Depending on if the inherited assignment of Result worked, this is not needed
  // Result := fCodeLines;
end;

{ TcbInterfaceList }

constructor TcbInterfaceList.Create;
begin
  inherited;
end;

destructor TcbInterfaceList.Destroy;
var
  lInterface: TcbInterface;
begin
  for lInterface in Self do
    if Assigned(lInterface) then
      lInterface.Free;

  Self.Clear;

  inherited Destroy;
end;

constructor TcbInterface.Create(AOwner: TcbUnit; AInterfaceName: String; AInterfaceGUID: TGUID; AInterfaceExtends: String);
begin
  inherited Create(AOwner);

  fInterfaceName := AInterfaceName;
  fInterfaceExtends := AInterfaceExtends;
  if GUIDToString(AInterfaceGUID) = GUIDToString(EMPTY_GUID) then
    CreateGUID(fInterfaceGUID);
  fInterfaceMethodList := TcbMethodList.Create;
  fInterfacePropertyList := TcbInterfacePropertyList.Create;
  fTypeDeclaration := TStringList.Create;
end;

destructor TcbInterface.Destroy;
begin
  if Assigned(fInterfaceMethodList) then
    fInterfaceMethodList.Free;
  if Assigned(fInterfacePropertyList) then
    fInterfacePropertyList.Free;
  if Assigned(fTypeDeclaration) then
    fTypeDeclaration.Free;

  inherited Destroy;
end;

function TcbInterface.GetElementName: String;
begin
  Result := fInterfaceName;
end;

procedure TcbInterface.SetElementName(AName: String);
begin
  fInterfaceName := AName;
end;

function TcbInterface.WriteTypeDeclaration: TStringList;
var
  lMethod: TcbMethod;
  lProperty: TcbInterfaceProperty;
begin
  fTypeDeclaration.Clear;


  with fTypeDeclaration do
  begin
    // Write interface typename, with the optional inheritence
    if fInterfaceExtends <> '' then
      Add(fInterfaceName + ' = interface(' + fInterfaceExtends + ')')
    else
      Add(fInterfaceName + ' = interface');

    // Write interface GUID
    Add('[' + GUIDToString(fInterfaceGUID) + ']');

    // Write interface method declarations
    for lMethod in fInterfaceMethodList do
      Add(lMethod.WritePrototype);

    // Write interface properties
    for lProperty in fInterfacePropertyList do
    begin
      // TODO -oAPL -cCodeBuilder 2: Finish this
      // lProperty.wri

    end;
  end;

  Result := fTypeDeclaration;
end;

{ TcbInterfacePropertyList }

constructor TcbInterfacePropertyList.Create;
begin
  inherited;
end;

destructor TcbInterfacePropertyList.Destroy;
var
  lInterfaceProperty: TcbInterfaceProperty;
begin
  for lInterfaceProperty in Self do
    if Assigned(lInterfaceProperty) then
      lInterfaceProperty.Free;

  Self.Clear;

  inherited Destroy;
end;

{ TcbInterfaceProperty }

constructor TcbInterfaceProperty.Create(AOwner: TcbInterface; APropertyName: String; APropertyType: String; APropertyReadElement: IcbInterfaceElement;
  APropertyWriteElement: IcbInterfaceElement);
begin
  inherited Create(AOwner);

  fOwnerInterface := AOwner;
  fPropertyName := APropertyName;
  fPropertyType := APropertyType;
  fPropertyReadElement := APropertyReadElement;
  fPropertyWriteElement := APropertyWriteElement;
  fClassSection := csPublic;
end;

destructor TcbInterfaceProperty.Destroy;
begin
  inherited Destroy;
end;

function TcbInterfaceProperty.GetElementName: String;
begin
  Result := fPropertyName;
end;

procedure TcbInterfaceProperty.SetElementName(AName: String);
begin
  fPropertyName := AName;
end;

function TcbInterfaceProperty.GetOwnerInterface: TcbInterface;
begin
  Result := fOwnerInterface;
end;

procedure TcbInterfaceProperty.SetOwnerInterface(AOwnerInterface: TcbInterface);
begin
  SetOwnerComponent(AOwnerInterface);
  fOwnerInterface := AOwnerInterface;
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

constructor TcbVariableList.Create(AOwner: TcbComponent);
begin
  inherited Create;

  fOwner := AOwner;
  fVariableStrings := TStringList.Create;
end;

destructor TcbVariableList.Destroy;
var
  lVariable: TcbVariable;
begin
  if Assigned(fVariableStrings) then
    fVariableStrings.Free;

  for lVariable in Self do
    if Assigned(lVariable) then
      lVariable.Free;

  Self.Clear;

  inherited Destroy;
end;

procedure TcbVariableList.Add(AVariableName: String; AVariableType: String);
var
  lVariable: TcbVariable;
begin
  lVariable := TcbVariable.Create(
    fOwner,
    AVariableName,
    AVariableType);

  inherited Add(lVariable);
end;

function TcbVariableList.WriteVariables: TStringList;
var
  lVariable: TcbVariable;
begin
  fVariableStrings.Clear;

  with fVariableStrings do
  begin
    Add('var');

    for lVariable in Self do
      Add(lVariable.WriteVariable);
  end;

  Result := fVariableStrings;
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

constructor TcbMethodParameterList.Create(AOwnerMethod: TcbMethod);
begin
  inherited Create;

  fOwnerMethod := AOwnerMethod;
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

function TcbMethodParameterList.WriteParameters: String;
var
  lMethodParameter: TcbMethodParameter;
begin
  // DONE -oAPL -cClassBuilder 1: Write the parameter strings
  // TODO -oAPL -cClassBuilder 2: Make sure that items with defaults are in the end of the written string

  Result := '';

  if Count > 0 then
    Result := '(';

  for lMethodParameter in Self do
    Result := Result + lMethodParameter.WriteParameter;

  if Count > 0 then
    Result := ')';
end;

procedure TcbMethodParameterList.Add(AParameterName: String; AParameterType: String; AParameterModifier: TcbMethodParameterModifier;
  AParameterDefault: String);
var
  lParameter: TcbMethodParameter;
begin
  lParameter := TcbMethodParameter.Create(
    fOwnerMethod,
    AParameterName,
    AParameterType,
    AParameterModifier,
    AParameterDefault);

  inherited Add(lParameter);
end;

{ TcbUnitWriter }

constructor TcbUnitWriter.Create(AOwner: TcbComponent);
begin
  inherited Create(AOwner);
end;

destructor TcbUnitWriter.Destroy;
begin
  inherited;
end;

function TcbUnitWriter.WriteUnit(AUnit: TcbUnit): TStrings;
var
  lIndex: Integer;
  lUnitMethod: TcbMethod;
  lClass: TcbClass;
  lVariable: TcbVariable;
  lInterface: TcbInterface;
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
    // TODO -oAPL -cCodeBuilder 1: Write interfaces
    for lInterface in UnitInterfaces do
      AddStrings(lInterface.WriteTypeDeclaration);

    // Write types
    // TODO-oAPL -cCodeBuilder 1: Write class types

    // Write classless methods in the unit
    if UnitMethods.Count > 0 then
      for lUnitMethod in UnitMethods do
        Add(lUnitMethod.WritePrototype);

    // Write unit variables
    if UnitVariables.Count > 0 then
    begin
      Add('var');

      for lVariable in UnitVariables do
        Add(lVariable.VariableName + ': ' + lVariable.VariableType);
    end;

    Add('implementation');

    // Write unit interface uses clause
    // TODO -oAPL -cClassBuilder 2: Write class for uses lists
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
        Add(lUnitMethod.WritePrototype);

        if lUnitMethod.ImplementationVars.Count > 0 then
        begin
          Add('var');

          for lVariable in lUnitMethod.ImplementationVars do
            Add(lVariable.fVariableName + ': ' + lVariable.VariableType + ';');
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

constructor TcbCodeWriter.Create(AOwner: TcbComponent);
begin
  inherited Create(AOwner);

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

constructor TcbUnit.Create(AOwner: TcbComponent);
begin
  inherited Create(AOwner);

  fUnitTopCommentBlock := TStringList.Create;
  fUnitTopCompilerDirectives := TStringList.Create;
  fUnitInterfaceUsesList := TStringList.Create;
  fUnitImplementationUsesList := TStringList.Create;
  fUnitMethods := TcbMethodList.Create;
  fUnitVariables := TcbVariableList.Create;
  fUnitInitialization := TStringList.Create;
  fUnitFinalization := TStringList.Create;
  fUnitClassList := TcbClassList.Create;
  fUnitConstants := TStringList.Create;
  fUnitInterfaces := TcbInterfaceList.Create;
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
  if Assigned(fUnitVariables) then
    fUnitVariables.Free;
  if Assigned(fUnitInitialization) then
    fUnitInitialization.Free;
  if Assigned(fUnitFinalization) then
    fUnitFinalization.Free;
  if Assigned(fUnitClassList) then
    fUnitClassList.Free;
  if Assigned(fUnitConstants) then
    fUnitConstants.Free;
  if Assigned(fUnitInterfaces) then
    fUnitInterfaces.Free;

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

function TcbVariable.WriteVariable: String;
begin
  Result := '';

  Result := fVariableName + ': ' + fVariableType + ';';
end;

function TcbMethod.WriteImplementationPrototype: String;
begin
  Result := '';

  // Write method type
  Result := MethodTypeToString(fMethodType) + ' ';
  // Write methodname
  Result := Result + fMethodName;
  // Write parameters
  Result := Result + fParameterList.WriteParameters;
  // Write returntype, if this is a function
  if fMethodType = mtFunction then
    Result := Result + ': ' + fReturnType;

  // End the header
  Result := Result + ';';

  // Write method directives
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

constructor TcbMethod.Create(AOwner: TcbComponent; AMethodName: String; AMethodType: TcbMethodType; ALeadingCommentBlock: TcbSourceCommentBlock;
  AMethodDirectives: TcbMethodDirectives; AReturnType: String);
begin
  inherited Create(AOwner);

  fMethodType := AMethodType;
  fMethodName := AMethodName;
  fReturnType := AReturnType;
  fMethodDirectives := AMethodDirectives;
  fParameterList := TcbMethodParameterList.Create(Self);
  fImplementationVars := TcbVariableList.Create(Self);
  fMethodImplementation := TStringList.Create;
  fLeadingCommentBlock := TcbSourceCommentBlock.Create(Self);
end;

destructor TcbMethod.Destroy;
begin
  if Assigned(fParameterList) then
    fParameterList.Free;
  if Assigned(fImplementationVars) then
    fImplementationVars.Free;
  if Assigned(fMethodImplementation) then
    fMethodImplementation.Free;
  if Assigned(fLeadingCommentBlock) then
    fLeadingCommentBlock.Free;

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

function TcbMethod.WriteImplementation: TStringList;
var
  lPrototype: String;
begin
  Result := inherited WriteSourceCode;

  with Result do
  begin
    // Write the leading comment block
    AddStrings(fLeadingCommentBlock.WriteSourceCode);
    // Write the implementation prototype
    Add(WriteImplementationPrototype);
    // Write the local vars
    Add(ImplementationVars.WriteVariables);
    // Write the implementation
    Add(MethodImplementation.WriteSourceCode);
  end;
end;

procedure TcbMethod.AddParameter(AParameter: TcbMethodParameter);
begin
  fParameterList.Add(AParameter);
end;

procedure TcbMethod.AddParameter(AParameterName: String; AParameterType: String; AParameterModifier: TcbMethodParameterModifier; AParameterDefault: String);
begin
  fParameterList.Add(
    AParameterName,
    AParameterType,
    AParameterModifier,
    AParameterDefault);
end;

procedure TcbMethod.AddVariable(AVariable: TcbVariable);
begin
  fImplementationVars.Add(AVariable);
end;

procedure TcbMethod.AddVariable(AVariableName: String; AVariableType: String);
begin
  fImplementationVars.Add(
    AVariableName,
    AVariableType);
end;

function TcbMethod.WritePrototype: String;
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

constructor TcbMethodParameter.Create(AOwnerMethod: TcbMethod; AParameterName: String; AParameterType: String; AParameterModifier: TcbMethodParameterModifier;
  AParameterDefault: String);
begin
  inherited Create(AOwnerMethod);

  fOwnerMethod := AOwnerMethod;
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

function TcbMethodParameter.WriteParameter: String;
begin
  Result := '';

  // Wtite parameter modifier
  Result := Result + MethodParameterModifierToString(ParameterModifier);
  // Write parameter name
  Result := Result + ParameterName;
  // Write parameter type
  Result := Result + ': ' + ParameterType;
  // Write parameter default
  if ParameterDefault <> '' then
    Result := Result + ' = ' + ParameterDefault;
end;

constructor TcbClass.Create(AOwner: TcbUnit);
begin
  inherited Create(AOwner);

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
  // TODO -oAPL -cCodeBuilder 2: Make code that generates the property class code
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
  // TODO -oAPL -cCodeBuilder 2: Make code that generates the declaration
end;

function TcbClassVariable.GetOwnerClass: TcbClass;
begin
  Result := fOwnerClass;
end;

procedure TcbClassVariable.SetOwnerClass(AOwnerClass: TcbClass);
begin
  fOwnerClass := AOwnerClass;
end;

constructor TcbClassVariable.Create(AOwnerClass: TcbClass; AVariableName: String; AVariableType: String; AClassSection: TcbClassSection);
begin
  inherited Create(AOwnerClass, AVariableName, AVariableType);

  fOwnerClass := AOwnerClass;
  fClassSection := AClassSection;
end;

destructor TcbClassVariable.Destroy;
begin
  inherited;
end;

{ TcbClassMethod }

function TcbClassMethod.GetClassDeclaration: String;
begin
  // TODO -oAPL -cCodeBuilder 2: Write code that generates the declaration, the prototype
end;

function TcbClassMethod.GetOwnerClass: TcbClass;
begin
  Result := fOwnerClass;
end;

procedure TcbClassMethod.SetOwnerClass(AOwnerClass: TcbClass);
begin
  fOwnerClass := AOwnerClass;
end;

constructor TcbClassMethod.Create(AOwnerClass: TcbClass; AMethodName: String; AClassSection: TcbClassSection; AMethodType: TcbMethodType;
  AMethodDirectives: TcbMethodDirectives; AReturnType: String);
begin
  inherited Create(AOwnerClass, AMethodName, AMethodType, AMethodDirectives, AReturnType);

  fOwnerClass := AOwnerClass;
  fClassSection := AClassSection;
end;

destructor TcbClassMethod.Destroy;
begin
  inherited;
end;

end.

