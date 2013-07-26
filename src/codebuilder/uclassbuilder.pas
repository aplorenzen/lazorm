// TODO -oAPL -cClassBuilder 2: Find some way to implement generics
// TODO -oAPL -cClassBuilder 1: Clean up the interface named element on properties
// TODO -oAPL -cClassBuilder 3: Handle non prototyped functions
// TODO -oAPL -cClassBuilder 3: Handle method level constants
// TODO -oAPL -cClassBuilder 2: Make logic that makes sure that things are written in the right order, forward declarations solve some things, but constants and variables are not handled
// TODO -oAPL -cClassBuilder 5: Perhaps clean up the unused list types
// TODO -oAPL -cClassBuilder 3: Make logic that inspects all types used in a unit, to compose the uses clause for the implementation and the interface sections. Need to associate all types with an origin unit in this case

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
  LO_EMPTY_GUID: TGUID;

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

  // TODO -oAPL -cClassBuilder 4: Find out id there are more of these class modifiers, find them all and implement them
  TcbClassModifierTypes = (
    cmAbstract,
    cmSealed);

  // TODO -oAPL -cClassBuilder 5: Assert that all classes are forward delared... not very important, perhaps not even a good idea
  TcbComponent = class;
  TcbSourceComment = class;
  TcbSourceCommentBlock = class;
  TcbCodeLine = class;
  TcbSourceCodeMethodBody = class;
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

// TODO -oAPL -cClassBuilder 5: Forward declare interfaces?

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
    function GetClassSection: TcbClassSection;
    property OwnerClass: TcbClass read GetOwnerClass;
  end;

  { TcbClassElementList }

  TcbClassElementList = class(specialize TFPGList<IcbClassElement>)
  private
    fOwnerClass: TcbClass;
  public
    constructor Create(AOwnerClass: TcbClass);
    destructor Destroy; override;
  published
    property OwnerClass: TcbClass read fOwnerClass;
  end;

  IcbInterfaceElement = interface(IcbNamedElement)
    ['{206DAD07-1AC9-4DF1-8A4C-C39B9ACE9A99}']
    function GetOwnerInterface: TcbInterface;
    property OwnerInterface: TcbInterface read GetOwnerInterface;
  end;

  { TcbInterfaceElementList }

  TcbInterfaceElementList = class(specialize TFPGList<IcbInterfaceElement>)
  private
    fOwnerInterface: TcbInterface;
  public
    constructor Create(AOwnerInterface: TcbInterface);
    destructor Destroy; override;
  published
    property OwnerInterface: TcbInterface read fOwnerInterface;
  end;

  {IcbNamedElementContainer = interface(IInterface)
    ['{19AEBB68-2358-45F6-AD30-5DDC4ECCD176}']
    procedure AddNamedElement(AElement: IcbNamedElement);
    function GetNamedElements: TcbNamedElementList;
  end;}

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
    function GetOwnerComponent: TcbComponent;
    property OwnerComponent: TcbComponent read GetOwnerComponent;
  end;

  { TcbType }

  IcbType = interface(IInterface)
    ['{040ADA55-CD7A-4BF0-AC60-05C032CEBF8C}']
    function GetTypeName: String;
    procedure SetTypeName(ATypeName: String);
    property TypeName: String read GetTypeName write SetTypeName;
  end;

  { TcbTypeList }

  TcbTypeList = class(specialize TFPGList<IcbType>)
  public
    constructor Create();
    destructor Destroy; override;
  end;

  { TcbComponent }

  TcbComponent = class(TInterfacedPersistent, IcbComponent, IcbXMLConfigConsumer, IcbLoggerConsumer)
  private
    fOwnerComponent: TcbComponent;
    fLogger: TLogger;
    fConfig: TXMLConfig;
  public
    constructor Create(AOwnerComponent: TcbComponent; AConfig: TXMLConfig = nil; ALogger: TLogger = nil); overload;
    destructor Destroy; override;

    procedure SetLogger(ALogger: TLogger);
    function GetLogger: TLogger;
    procedure SetConfig(AConfig: TXMLConfig);
    function GetConfig: TXMLConfig;
    function GetOwnerComponent: TcbComponent; virtual;
  published
    property OwnerComponent: TcbComponent read fOwnerComponent;
  end;

  { TcbComponentList }

  TcbComponentList = class(specialize TFPGList<TcbComponent>)
  private
    fOwnerComponent: TcbComponent;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
  published
    property OwnerComponent: TcbComponent read fOwnerComponent;
  end;

  { TcbExternalType }

  TcbExternalType = class(TcbComponent, IcbType)
  private
    fTypeName: String;
  public
    constructor Create(AOwnerComponent: TcbComponent; ATypeName: String);
    destructor Destroy; override;
    function GetTypeName: String;
    procedure SetTypeName(ATypeName: String);
  end;

  { TcbPrototypedCodeComponent }

  TcbPrototypedCodeComponent = class(TcbComponent)
  private
    fPrototypeCode: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WritePrototype: TStringList; virtual;
  end;

  { TcbSourceCodeComponent }

  TcbSourceCodeComponent = class(TcbComponent)
  private
    fSourceCode: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; virtual;
  end;

  { TcbPrototypedSourceCodeComponent }

  TcbPrototypedSourceCodeComponent = class(TcbPrototypedCodeComponent)
  private
    fSourceCode: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; virtual;
  end;

  { TcbSourceCodeBlock }

  TcbSourceCodeBlock = class(specialize TFPGList<TcbSourceCodeComponent>)
  private
    fOwnerComponent: TcbComponent;
    fSourceCodeLines: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; virtual;
  published
     property OwnerComponent: TcbComponent read fOwnerComponent;
  end;

  { TcbSourceCodeBeginEndBlock }

  TcbSourceCodeBeginEndBlock = class(TcbSourceCodeBlock)
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
  end;

  { TcbSourceComment }

  TcbSourceComment = class(TcbSourceCodeComponent)
  private
    fSourceComment: String;
    fCommentStyle: TcbCodeCommentStyle;
  public
    constructor Create(AOwnerComponent: TcbComponent; ASourceComment: String; ACommentStyle: TcbCodeCommentStyle = ccsSlashes);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
  published
    property SourceComment: String read fSourceComment write fSourceComment;
    property CommentStyle: TcbCodeCommentStyle read fCommentStyle write fCommentStyle;
  end;

  { TcbSourceCommentBlock }

  TcbSourceCommentBlock = class(TcbSourceCodeBeginEndBlock)
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    procedure Add(ASourceComment: String; ACommentStyle: TcbCodeCommentStyle = ccsSlashes);
  end;

  { TcbCodeLine }

  TcbCodeLine = class(TcbSourceCodeComponent)
  private
    fCodeLine: String;
    fComment: TcbSourceComment;
  public
    constructor Create(AOwnerComponent: TcbComponent; ACodeLine: String; AComment: TcbSourceComment);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
  published
    property CodeLine: String read fCodeLine write fCodeLine;
    property Comment: TcbSourceComment read fComment write fComment;
  end;

  { TcbSourceCodeMethodBody }

  TcbSourceCodeMethodBody = class(TcbSourceCodeBeginEndBlock)
  private
    fOwnerMethod: TcbMethod;
  public
    constructor Create(AOwnerMethod: TcbMethod);
    destructor Destroy; override;
    procedure Add(ACodeString: String; AComment: TcbSourceComment = nil); overload;
    procedure Add(ACodeString: String; ASourceComment: String = ''; ACommentStyle: TcbCodeCommentStyle = ccsSlashes); overload;
  published
    property OwnerMethod: TcbMethod read fOwnerMethod;
  end;

  { TcbMethodParameter }

  TcbMethodParameter = class(TcbComponent, IcbNamedElement)
  private
    fOwnerMethod: TcbMethod;
    fParameterName: String;
    fParameterType: IcbType;
    fParameterDefault: String;
    fParameterModifier: TcbMethodParameterModifier;
  public
    constructor Create(
      AOwnerMethod: TcbMethod;
      AParameterName: String;
      AParameterType: IcbType;
      AParameterModifier: TcbMethodParameterModifier = mpmNone;
      AParameterDefault: String = '');
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function WriteParameter: String;
  published
    property OwnerMethod: TcbMethod read fOwnerMethod;
    property ParameterName: String read fParameterName write fParameterName;
    property ParameterType: IcbType read fParameterType write fParameterType;
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
      AParameterType: IcbType;
      AParameterModifier: TcbMethodParameterModifier = mpmNone;
      AParameterDefault: String = ''); overload;
    procedure Add(AParameter: TcbMethodParameter); overload;
  published
    property OwnerMethod: TcbMethod read fOwnerMethod;
  end;

  { TcbMethod }

  TcbMethod = class(TcbPrototypedSourceCodeComponent, IcbNamedElement)
  private
    fMethodType: TcbMethodType;
    fMethodName: String;
    fReturnType: IcbType;
    fLeadingCommentBlock: TcbSourceCommentBlock;
    fMethodDirectives: TcbMethodDirectives;
    fParameterList: TcbMethodParameterList;
    fImplementationVars: TcbVariableList;
    fMethodImplementation: TcbSourceCodeMethodBody;
  protected
    fImplementationPrototypeCode: TStringList;
    function WriteImplementationPrototype: TStringList; virtual;
  public
    constructor Create(
      AOwnerComponent: TcbComponent;
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodImplementation: TcbSourceCodeMethodBody = nil;
      ALeadingCommentBlock: TcbSourceCommentBlock = nil;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function WritePrototype: TStringList; override;
    function WriteSourceCode: TStringList; override;
    procedure AddParameter(AParameter: TcbMethodParameter); overload;
    procedure AddParameter(
      AParameterName: String;
      AParameterType: IcbType;
      AParameterModifier: TcbMethodParameterModifier = mpmNone;
      AParameterDefault: String = ''); overload;
    procedure AddVariable(AVariable: TcbVariable); overload;
    procedure AddVariable(
      AVariableName: String;
      AVariableType: IcbType); overload;
  published
    property MethodType: TcbMethodType read fMethodType write fMethodType;
    property MethodName: String read fMethodName write fMethodName;
    property ReturnType: IcbType read fReturnType write fReturnType;
    property MethodDirectives: TcbMethodDirectives read fMethodDirectives write fMethodDirectives;
    property ParameterList: TcbMethodParameterList read fParameterList write fParameterList;
    property ImplementationVars: TcbVariableList read fImplementationVars write fImplementationVars;
    property MethodImplementation: TcbSourceCodeMethodBody read fMethodImplementation write fMethodImplementation;
    property LeadingCommentBlock: TcbSourceCommentBlock read fLeadingCommentBlock write fLeadingCommentBlock;
  end;

  { TcbMethodList }

  TcbMethodList = class(specialize TFPGList<TcbMethod>)
  private
    fOwnerComponent: TcbComponent;
    fPrototypeCodeStrings: TStringList;
    fImplementationCodeStrings: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    procedure Add(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodImplementation: TcbSourceCodeMethodBody = nil;
      ALeadingCommentBlock: TcbSourceCommentBlock = nil;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil); overload;
    procedure Add(AMethod: TcbMethod); overload;
    function WritePrototypes: TStringList;
    function WriteImplementations: TStringList;
  published
    property OwnerComponent: TcbComponent read fOwnerComponent;
  end;

  { TcbClassMethod }

  TcbClassMethod = class(TcbMethod, IcbClassElement)
  private
    fOwnerClass: TcbClass;
    fClassSection : TcbClassSection;
    fPrototypeCodeStrings: TStringList;
    fImplementationCodeStrings: TStringList;
  protected
    function WriteImplementationPrototype: TStringList; override;
  public
    constructor Create(
      AOwnerClass: TcbClass;
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodImplementation: TcbSourceCodeMethodBody = nil;
      ALeadingCommentBlock: TcbSourceCommentBlock = nil;
      AClassSection : TcbClassSection = csPackage;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil);
    destructor Destroy; override;
    function GetOwnerClass: TcbClass;
    function GetClassSection: TcbClassSection;
  published
    property OwnerClass: TcbClass read fOwnerClass;
    property ClassSection: TcbClassSection read fClassSection write fClassSection;
  end;

  { TcbClassMethodList }

  TcbClassMethodList = class(specialize TFPGList<TcbClassMethod>)
  private
    fOwnerClass: TcbClass;
    fPrototypeCodeStrings: array [TcbClassSection] of TStringList;
    fImplementationCodeStrings: TStringList;
  public
    constructor Create(AOwnerClass: TcbClass);
    destructor Destroy; override;
    procedure Add(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodImplementation: TcbSourceCodeMethodBody = nil;
      ALeadingCommentBlock: TcbSourceCommentBlock = nil;
      AClassSection : TcbClassSection = csPackage;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil); overload;
    procedure Add(AClassMethod: TcbClassMethod); overload;
    function WritePrototypes(ASection: TcbClassSection): TStringList;
    function WriteImplementations: TStringList;
    function HasElementsInSection(ASection: TcbClassSection): Boolean;
  published
    property OwnerClass: TcbClass read fOwnerClass;
  end;

  { TcbVariable }

  TcbVariable = class(TcbCodeLine, IcbNamedElement)
  private
    fVariableName: String;
    fVariableType: IcbType;
  public
    constructor Create(
      AOwnerComponent: TcbComponent;
      AVariableName: String;
      AVariableType: IcbType;
      AComment: TcbSourceComment = nil);
      destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function WriteSourceCode: TStringList; override;
  published
    property VariableName: String read fVariableName write fVariableName;
    property VariableType: IcbType read fVariableType write fVariableType;
  end;

  { TcbVariableList }

  TcbVariableList = class(specialize TFPGList<TcbVariable>)
  private
    fOwnerComponent: TcbComponent;
    fVariableStrings: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    procedure Add(
      AVariableName: String;
      AVariableType: IcbType); overload;
    procedure Add(AVariable: TcbVariable); overload;
    function WriteVariables: TStringList; virtual;
  published
    property OwnerComponent: TcbComponent read fOwnerComponent;
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
      AVariableType: IcbType;
      AClassSection : TcbClassSection = csPackage);
    destructor Destroy; override;
    function GetOwnerClass: TcbClass;
    function GetClassSection: TcbClassSection;
  published
    property OwnerClass: TcbClass read fOwnerClass write fOwnerClass;
    property ClassSection: TcbClassSection read fClassSection  write fClassSection ;
  end;

  { TcbClassVariableList }

  TcbClassVariableList = class(specialize TFPGList<TcbClassVariable>)
  private
    fOwnerClass: TcbClass;
    fVariableStrings: array [TcbClassSection] of TStringList;
  public
    constructor Create(AOwnerClass: TcbClass);
    destructor Destroy; override;
    function WriteVariables(ASection: TcbClassSection): TStringList;
    procedure Add(
      AVariableName: String;
      AVariableType: IcbType;
      AClassSection : TcbClassSection = csPackage); overload;
    procedure Add(AClassVariable: TcbClassVariable); overload;
    function HasElementsInSection(ASection: TcbClassSection): Boolean;
  published
    property OwnerClass: TcbClass read fOwnerClass;
  end;

  { TcbInterfaceProperty }

  TcbInterfaceProperty = class(TcbCodeLine, IcbInterfaceElement)
  private
    fOwnerInterface: TcbInterface;
    fPropertyName: String;
    fPropertyType: IcbType;
    fPropertyReadElement: IcbInterfaceElement;
    fPropertyWriteElement: IcbInterfaceElement;
  public
    constructor Create(
      AOwner: TcbInterface;
      APropertyName: String;
      APropertyType: IcbType;
      APropertyReadElement: IcbInterfaceElement = nil;
      APropertyWriteElement: IcbInterfaceElement = nil;
      AComment: TcbSourceComment = nil);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function GetOwnerInterface: TcbInterface;
    function WriteSourceCode: TStringList; override;
  published
    property OwnerInterface: TcbInterface read fOwnerInterface;
    property PropertyName: String read fPropertyName write fPropertyName;
    property PropertyType: IcbType read fPropertyType write fPropertyType;
    property PropertyReadElement: IcbInterfaceElement read fPropertyReadElement write fPropertyReadElement;
    property PropertyWriteElement: IcbInterfaceElement read fPropertyWriteElement write fPropertyWriteElement;
  end;

  { TcbInterfacePropertyList }

  TcbInterfacePropertyList = class(specialize TFPGList<TcbInterfaceProperty>)
  private
    fOwnerInterface: TcbInterface;
    fPropertyStrings: TStringList;
  public
    constructor Create(AOwnerInterface: TcbInterface);
    destructor Destroy; override;
    function WriteProperties: TStringList;
    procedure Add(
      APropertyName: String;
      APropertyType: IcbType;
      APropertyReadElement: IcbInterfaceElement = nil;
      APropertyWriteElement: IcbInterfaceElement = nil;
      AComment: TcbSourceComment = nil); overload;
    procedure Add(AInterfaceProperty: TcbInterfaceProperty); overload;
  published
      property OwnerInterface: TcbInterface read fOwnerInterface;
  end;

  { TcbClassProperty }

  TcbClassProperty = class(TcbCodeLine, IcbClassElement)
  private
    fOwnerClass: TcbClass;
    fClassSection : TcbClassSection;
    fPropertyName: String;
    fPropertyType: IcbType;
    fPropertyReadElement: IcbClassElement;
    fPropertyWriteElement: IcbClassElement;
  public
    constructor Create(
      AOwnerClass: TcbClass;
      APropertyName: String;
      APropertyType: IcbType;
      AClassSection: TcbClassSection = csPackage;
      APropertyReadElement: IcbClassElement = nil;
      APropertyWriteElement: IcbClassElement = nil;
      AComment: TcbSourceComment = nil);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function GetOwnerClass: TcbClass;
    function GetClassSection: TcbClassSection;
    function WriteSourceCode: TStringList; override;
  published
    property OwnerClass: TcbClass read fOwnerClass;
    property ClassSection : TcbClassSection read fClassSection  write fClassSection ;
    property PropertyName: String read fPropertyName write fPropertyName;
    property PropertyType: IcbType read fPropertyType write fPropertyType;
    property PropertyReadElement: IcbClassElement read fPropertyReadElement write fPropertyReadElement;
    property PropertyWriteElement: IcbClassElement read fPropertyWriteElement write fPropertyWriteElement;
  end;

  { TcbClassPropertyList }

  TcbClassPropertyList = class(specialize TFPGList<TcbClassProperty>)
  private
    fOwnerClass: TcbClass;
    fPropertyStrings: array [TcbClassSection] of TStringList;
  public
    constructor Create(AOwnerClass: TcbClass);
    destructor Destroy; override;
    function WriteProperties(ASection: TcbClassSection): TStringList;
    procedure Add(
      APropertyName: String;
      APropertyType: IcbType;
      AClassSection: TcbClassSection = csPackage;
      APropertyReadElement: IcbClassElement = nil;
      APropertyWriteElement: IcbClassElement = nil;
      AComment: TcbSourceComment = nil); overload;
    procedure Add(AClassProperty : TcbClassProperty); overload;
    function HasElementsInSection(ASection: TcbClassSection): Boolean;
  published
    property OwnerClass: TcbClass read fOwnerClass;
  end;

  TcbClass = class(TcbPrototypedSourceCodeComponent, IcbNamedElement, IcbType)
  private
    fOwnerUnit: TcbUnit;
    fClassTypeName: String;
    fHasClassForwardDeclaration: Boolean;
    fExtendingClass: IcbType;
    fImplementingInterfaces: TcbTypeList;
    fClassVariableList: TcbClassVariableList;
    fClassMethodList: TcbClassMethodList;
    fClassPropertyList: TcbClassPropertyList;
    fClassElementList: TcbClassElementList;
    fPrototypeClassSections: array [TcbClassSection] of TStringList;
    function HasElementsInSection(ASection: TcbClassSection): Boolean;
    function WritePrototypeSection(ASection: TcbClassSection): TStringList;
  public
    constructor Create(
      AOwnerUnit: TcbUnit;
      AClassTypeName: String;
      AHasClassForwardDeclaration: Boolean;
      AExtendingClass: IcbType = nil);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function GetTypeName: String;
    procedure SetTypeName(ATypeName: String);
    function WritePrototype: TStringList; override;
    function WriteSourceCode: TStringList; override;
    function WriteForwardDeclaration: String;
    procedure AddVariable(AVariable: TcbClassVariable); overload;
    procedure AddVariable(
      AVariableName: String;
      AVariableType: IcbType;
      AClassSection : TcbClassSection = csPackage); overload;
    procedure AddMethod(AMethod: TcbClassMethod); overload;
    procedure AddMethod(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodImplementation: TcbSourceCodeMethodBody = nil;
      ALeadingCommentBlock: TcbSourceCommentBlock = nil;
      AClassSection: TcbClassSection = csPackage;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil); overload;
    procedure AddProperty(AProperty: TcbClassProperty); overload;
    procedure AddProperty(
      APropertyName: String;
      APropertyType: IcbType;
      AClassSection: TcbClassSection = csPackage;
      APropertyReadElement: IcbClassElement = nil;
      APropertyWriteElement: IcbClassElement = nil;
      AComment: TcbSourceComment = nil); overload;
  published
    property ClassTypeName: String read fClassTypeName write fClassTypeName;
    property ExtendingClass: IcbType read fExtendingClass write fExtendingClass;
    property ImplementingInterfaces: TcbTypeList read fImplementingInterfaces write fImplementingInterfaces;
    property ClassVariableList: TcbClassVariableList read fClassVariableList write fClassVariableList;
    property ClassMethodList: TcbClassMethodList read fClassMethodList write fClassMethodList;
    property ClassPropertyList: TcbClassPropertyList read fClassPropertyList write fClassPropertyList;
    property ClassElementList: TcbClassElementList read fClassElementList write fClassElementList;
    property HasClassForwardDeclaration: Boolean read fHasClassForwardDeclaration write fHasClassForwardDeclaration;
  end;

  { TcbClassList }

  TcbClassList = class(specialize TFPGList<TcbClass>)
  private
    fOwnerUnit: TcbUnit;
    fPrototypeCode: TStringList;
    fImplementationCode: TStringList;
    fForwardDeclarations: TStringList;
  public
    constructor Create(AOwnerUnit: TcbUnit);
    destructor Destroy; override;
    function WriteForwardDeclarations: TStringList;
    function WritePrototypes: TStringList;
    function WriteImplementations: TStringList;
  published
    property OwnerUnit: TcbUnit read fOwnerUnit;
  end;

  TcbInterface = class(TcbPrototypedCodeComponent, IcbNamedElement, IcbType)
  private
    fOwnerUnit: TcbUnit;
    fInterfaceName: String;
    fInterfaceExtends: IcbType;
    fInterfaceGUID: TGUID;
    fInterfaceMethodList: TcbMethodList;
    fInterfacePropertyList: TcbInterfacePropertyList;
  public
    constructor Create(
      AOwnerUnit: TcbUnit;
      AInterfaceName: String;
      AInterfaceGUID: TGUID;
      AInterfaceExtends: IcbType);
    destructor Destroy; override;

    function GetElementName: String;
    procedure SetElementName(AName: String);
    function GetTypeName: String;
    procedure SetTypeName(ATypeName: String);
    function WritePrototype: TStringList; override;

    property InterfaceGUID: TGUID read fInterfaceGUID write fInterfaceGUID;
  published
    property OwnerUnit: TcbUnit read fOwnerUnit write fOwnerUnit;
    property InterfaceName: String read fInterfaceName write fInterfaceName;
    property InterfaceExtends: IcbType read fInterfaceExtends write fInterfaceExtends;
    property InterfaceMethodList: TcbMethodList read fInterfaceMethodList write fInterfaceMethodList;
    property InterfacePropertyList: TcbInterfacePropertyList read fInterfacePropertyList write fInterfacePropertyList;
  end;

  { TcbInterfaceList }

  TcbInterfaceList = class(specialize TFPGList<TcbInterface>)
  private
    fOwnerUnit: TcbUnit;
    fPrototypeCode: TStringList;
    fForwardDeclarations: TStringList;
  public
    constructor Create(AOwnerUnit: TcbUnit);
    destructor Destroy; override;
    function WriteForwardDeclarations: TStringList;
    function WritePrototypes: TStringList;
  published
    property OwnerUnit: TcbUnit read fOwnerUnit;
  end;

  { TcbUnit }

  TcbUnit = class(TcbComponent, IcbNamedElement)
  private
    fUnitName: String;
    fUnitFileName: String;
    fUnitTopCommentBlock: TcbSourceCommentBlock;
    fUnitTopCompilerDirectives: TStringList;
    fUnitInterfaceUsesList: TStringList;
    fUnitImplementationUsesList: TStringList;
    fUnitMethods: TcbMethodList;
    fUnitVariables: TcbVariableList;
    fUnitInitialization: TcbSourceCodeBlock;
    fUnitFinalization: TcbSourceCodeBlock;
    fUnitClassList: TcbClassList;
    fUnitConstants: TStringList;
    fUnitInterfaces: TcbInterfaceList;

    // TODO -oAPL -cClassBuilder 3: Records, sets... and more?
  public
    constructor Create(AOwner: TcbComponent);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
  published
    property UnitName: String read fUnitName write fUnitName;
    property UnitFileName: String read fUnitFileName write fUnitFileName;
    property UnitTopCommentBlock: TcbSourceCommentBlock read fUnitTopCommentBlock write fUnitTopCommentBlock;
    property UnitTopCompilerDirectives: TStringList read fUnitTopCompilerDirectives write fUnitTopCompilerDirectives;
    property UnitInterfaceUsesList: TStringList read fUnitInterfaceUsesList write fUnitInterfaceUsesList;
    property UnitImplementationUsesList: TStringList read fUnitImplementationUsesList write fUnitImplementationUsesList;
    property UnitMethods: TcbMethodList read fUnitMethods write fUnitMethods;
    property UnitVariables: TcbVariableList read fUnitVariables write fUnitVariables;
    property UnitInitialization: TcbSourceCodeBlock read fUnitInitialization write fUnitInitialization;
    property UnitFinalization: TcbSourceCodeBlock read fUnitFinalization write fUnitFinalization;
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

  TcbNilReferenceException = class(Exception);

function MethodTypeToString(AMethodType: TcbMethodType): String;
function MethodParameterModifierToString(AParameterModifier: TcbMethodParameterModifier): String;
function ClassSectionToString(AClassSection: TcbClassSection): String;

implementation

function ClassSectionToString(AClassSection: TcbClassSection): String;
begin
  case AClassSection of
    csPackage: Result := '';
    csPrivate: Result := 'private';
    csPublic: Result := 'public';
    csProtected: Result := 'protected';
    csPublished: Result := 'published';
  end;
end;

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

{ TcbSourceCodeBlock }

constructor TcbSourceCodeBlock.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create;
  fSourceCodeLines := TStringList.Create;
end;

destructor TcbSourceCodeBlock.Destroy;
begin
  if Assigned(fSourceCodeLines) then
    fSourceCodeLines.Free;

  inherited Destroy;
end;

function TcbSourceCodeBlock.WriteSourceCode: TStringList;
var
  lSourceCodeComponent: TcbSourceCodeComponent;
begin
  fSourceCodeLines.Clear;

  for lSourceCodeComponent in Self do
    fSourceCodeLines.AddStrings(lSourceCodeComponent.WriteSourceCode);

  Result := fSourceCodeLines;
end;

{ TcbPrototypedSourceCodeComponent }

constructor TcbPrototypedSourceCodeComponent.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);
end;

destructor TcbPrototypedSourceCodeComponent.Destroy;
begin
  inherited Destroy;
end;

function TcbPrototypedSourceCodeComponent.WriteSourceCode: TStringList;
begin
  fSourceCode.Clear;
  Result := fSourceCode;
end;

{ TcbInterfaceElementList }

constructor TcbInterfaceElementList.Create(AOwnerInterface: TcbInterface);
begin
  inherited Create;
  fOwnerInterface := AOwnerInterface;
end;

destructor TcbInterfaceElementList.Destroy;
begin
  inherited Destroy;
end;

{ TcbClassElementList }

constructor TcbClassElementList.Create(AOwnerClass: TcbClass);
begin
  inherited Create;

  fOwnerClass := AOwnerClass;
end;

destructor TcbClassElementList.Destroy;
begin
  inherited Destroy;
end;

{ TcbPrototypedCodeComponent }

constructor TcbPrototypedCodeComponent.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);
  fPrototypeCode := TStringList.Create;
end;

destructor TcbPrototypedCodeComponent.Destroy;
begin
  if Assigned(fPrototypeCode) then
    fPrototypeCode.Free;
  inherited Destroy;
end;

function TcbPrototypedCodeComponent.WritePrototype: TStringList;
begin
  fPrototypeCode.Clear;
  Result := fPrototypeCode;
end;

{ TcbTypeList }

constructor TcbTypeList.Create;
begin
  inherited;
end;

destructor TcbTypeList.Destroy;
begin
  inherited Destroy;
end;

{ TcbExternalType }

constructor TcbExternalType.Create(AOwnerComponent: TcbComponent; ATypeName: String);
begin
  inherited Create(AOwnerComponent);

  fTypeName := ATypeName;
end;

destructor TcbExternalType.Destroy;
begin
  inherited Destroy;
end;

function TcbExternalType.GetTypeName: String;
begin
  Result := fTypeName;
end;

procedure TcbExternalType.SetTypeName(ATypeName: String);
begin
  fTypeName := ATypeName;
end;

{ TcbSourceCodeMethodBody }

constructor TcbSourceCodeMethodBody.Create(AOwnerMethod: TcbMethod);
begin
  inherited Create(AOwnerMethod);

  fOwnerMethod := AOwnerMethod;
end;

destructor TcbSourceCodeMethodBody.Destroy;
begin
  inherited Destroy;
end;

procedure TcbSourceCodeMethodBody.Add(ACodeString: String; AComment: TcbSourceComment);
var
  lCodeLine: TcbCodeLine;
begin
  lCodeLine := TcbCodeLine.Create(fOwnerMethod, ACodeString, AComment);
  inherited Add(lCodeLine);
end;

procedure TcbSourceCodeMethodBody.Add(ACodeString: String; ASourceComment: String; ACommentStyle: TcbCodeCommentStyle);
var
  lComment: TcbSourceComment;
begin
  lComment := TcbSourceComment.Create(
    fOwnerMethod,
    ASourceComment,
    ACommentStyle);

  Add(ACodeString, lComment);
end;

{ TcbSourceCommentBlock }

constructor TcbSourceCommentBlock.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);
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
    fOwnerComponent,
    ASourceComment,
    ACommentStyle);

  inherited Add(lComment);
end;

{ TcbSourceCodeBeginEndBlock }

constructor TcbSourceCodeBeginEndBlock.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);

  fOwnerComponent := AOwnerComponent;
  fSourceCodeLines := TStringList.Create;
end;

destructor TcbSourceCodeBeginEndBlock.Destroy;
begin
  if Assigned(fSourceCodeLines) then
    fSourceCodeLines.Free;

  inherited Destroy;
end;

function TcbSourceCodeBeginEndBlock.WriteSourceCode: TStringList;
begin
  Result := inherited WriteSourceCode;
  Result.Insert(0, 'begin');
  Result.Add('end');
end;

{ TcbSourceCodeComponent }

constructor TcbSourceCodeComponent.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);

  fSourceCode := TStringList.Create;
end;

destructor TcbSourceCodeComponent.Destroy;
begin
  if Assigned(fSourceCode) then
    fSourceCode.Free;
  inherited Destroy;
end;

function TcbSourceCodeComponent.WriteSourceCode: TStringList;
begin
  fSourceCode.Clear;
  Result := fSourceCode;
end;

{ TcbSourceComment }

constructor TcbSourceComment.Create(AOwnerComponent: TcbComponent; ASourceComment: String; ACommentStyle: TcbCodeCommentStyle);
begin
  inherited Create(AOwnerComponent);

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

constructor TcbCodeLine.Create(AOwnerComponent: TcbComponent; ACodeLine: String; AComment: TcbSourceComment);
begin
  inherited Create(AOwnerComponent);

  fOwnerComponent := AOwnerComponent;
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

constructor TcbInterfaceList.Create(AOwnerUnit: TcbUnit);
begin
  inherited Create;
  fOwnerUnit := AOwnerUnit;
  fPrototypeCode := TStringList.Create;
  fForwardDeclarations := TStringList.Create;
end;

destructor TcbInterfaceList.Destroy;
var
  lInterface: TcbInterface;
begin
  if Assigned(fPrototypeCode) then
    fPrototypeCode.Free;
  if Assigned(fForwardDeclarations) then
    fForwardDeclarations.Free;

  for lInterface in Self do
    if Assigned(lInterface) then
      lInterface.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbInterfaceList.WriteForwardDeclarations: TStringList;
begin

end;

function TcbInterfaceList.WritePrototypes: TStringList;
var
  lInterface: TcbInterface;
begin
  fPrototypeCode.Clear;

  for lInterface in Self do
    fPrototypeCode.AddStrings(lInterface.WritePrototype);

  Result := fPrototypeCode;
end;

constructor TcbInterface.Create(AOwnerUnit: TcbUnit; AInterfaceName: String; AInterfaceGUID: TGUID; AInterfaceExtends: IcbType);
begin
  inherited Create(AOwnerUnit);

  fOwnerUnit := AOwnerUnit;
  fInterfaceName := AInterfaceName;
  fInterfaceExtends := AInterfaceExtends;

  if GUIDToString(AInterfaceGUID) = GUIDToString(LO_EMPTY_GUID) then
    CreateGUID(fInterfaceGUID);

  fInterfaceMethodList := TcbMethodList.Create(Self);
  fInterfacePropertyList := TcbInterfacePropertyList.Create(Self);
end;

destructor TcbInterface.Destroy;
begin
  if Assigned(fInterfaceMethodList) then
    fInterfaceMethodList.Free;
  if Assigned(fInterfacePropertyList) then
    fInterfacePropertyList.Free;

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

function TcbInterface.GetTypeName: String;
begin
  Result := fInterfaceName;
end;

procedure TcbInterface.SetTypeName(ATypeName: String);
begin
  fInterfaceName := ATypeName;
end;

function TcbInterface.WritePrototype: TStringList;
var
  lInterfaceDeclaration: String;
  lHasElements: Boolean;
begin
  Result := inherited WritePrototype;

  lHasElements :=
    (fInterfaceMethodList.Count > 0)
    or (fInterfacePropertyList.Count > 0);

  with Result do
  begin
    lInterfaceDeclaration := fInterfaceName + ' = interface';

    if Assigned(fInterfaceExtends) then
      lInterfaceDeclaration := lInterfaceDeclaration + '(' + fInterfaceExtends.GetTypeName + ')';

    if not lHasElements then
      lInterfaceDeclaration := lInterfaceDeclaration + ';';

    Add(lInterfaceDeclaration);

    AddStrings(fInterfaceMethodList.WritePrototypes);
    AddStrings(fInterfacePropertyList.WriteProperties);

    if lHasElements then
      Add('end;');
  end;
end;

{ TcbInterfacePropertyList }

constructor TcbInterfacePropertyList.Create(AOwnerInterface: TcbInterface);
begin
  inherited Create;

  fOwnerInterface := AOwnerInterface;
  fPropertyStrings := TStringList.Create;
end;

destructor TcbInterfacePropertyList.Destroy;
var
  lInterfaceProperty: TcbInterfaceProperty;
begin
  if Assigned(fPropertyStrings) then
    fPropertyStrings.Free;

  for lInterfaceProperty in Self do
    if Assigned(lInterfaceProperty) then
      lInterfaceProperty.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbInterfacePropertyList.WriteProperties: TStringList;
var
  lInterfaceProperty: TcbInterfaceProperty;
begin
  fPropertyStrings.Clear;

  for lInterfaceProperty in Self do
      fPropertyStrings.AddStrings(lInterfaceProperty.WriteSourceCode);

  Result := fPropertyStrings;
end;

procedure TcbInterfacePropertyList.Add(APropertyName: String; APropertyType: IcbType; APropertyReadElement: IcbInterfaceElement;
  APropertyWriteElement: IcbInterfaceElement; AComment: TcbSourceComment);
var
  lInterfaceProperty: TcbInterfaceProperty;
begin
  lInterfaceProperty := TcbInterfaceProperty.Create(
    fOwnerInterface,
    APropertyName,
    APropertyType,
    APropertyReadElement,
    APropertyWriteElement,
    AComment);

  Add(lInterfaceProperty);
end;

procedure TcbInterfacePropertyList.Add(AInterfaceProperty: TcbInterfaceProperty);
begin
  inherited Add(AInterfaceProperty);
end;

{ TcbInterfaceProperty }

constructor TcbInterfaceProperty.Create(AOwner: TcbInterface; APropertyName: String; APropertyType: IcbType; APropertyReadElement: IcbInterfaceElement;
  APropertyWriteElement: IcbInterfaceElement; AComment: TcbSourceComment);
begin
  inherited Create(AOwner, '', AComment);

  fOwnerInterface := AOwner;
  fPropertyName := APropertyName;
  fPropertyType := APropertyType;
  fPropertyReadElement := APropertyReadElement;
  fPropertyWriteElement := APropertyWriteElement;
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


function TcbInterfaceProperty.WriteSourceCode: TStringList;
begin
  if not Assigned(fPropertyType) then
    raise TcbNilReferenceException.CreateFmt('Cannot write interface property: %s. PropertyType is not assigned.', [fPropertyName]);

  CodeLine := fPropertyName + ': ' + fPropertyType.GetTypeName;

  if Assigned(fPropertyReadElement) then
    CodeLine := CodeLine + ' read ' + fPropertyReadElement.GetElementName;

  if Assigned(fPropertyWriteElement) then
    CodeLine := CodeLine + ' write ' + fPropertyWriteElement.GetElementName;

  CodeLine := CodeLine + ';';

  Result := inherited WriteSourceCode;
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

constructor TcbClassList.Create(AOwnerUnit: TcbUnit);
begin
  inherited Create;
  fOwnerUnit := AOwnerUnit;

  fPrototypeCode := TStringList.Create;
  fImplementationCode := TStringList.Create;
  fForwardDeclarations := TStringList.Create;
end;

destructor TcbClassList.Destroy;
var
  lClass: TcbClass;
begin
  if Assigned(fPrototypeCode) then
    fPrototypeCode.Free;
  if Assigned(fImplementationCode) then
    fImplementationCode.Free;
  if Assigned(fForwardDeclarations) then
    fForwardDeclarations.Free;

  for lClass in Self do
    if Assigned(lClass) then
      lClass.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbClassList.WriteForwardDeclarations: TStringList;
var
  lClass: TcbClass;
begin
  fForwardDeclarations.Clear;

  for lClass in Self do
    if lClass.HasClassForwardDeclaration then
      fForwardDeclarations.Add(lClass.WriteForwardDeclaration);

  Result := fForwardDeclarations;
end;

function TcbClassList.WritePrototypes: TStringList;
var
  lClass: TcbClass;
begin
  fPrototypeCode.Clear;

  for lClass in Self do
    fPrototypeCode.AddStrings(lClass.WritePrototype);

  Result := fPrototypeCode;
end;

function TcbClassList.WriteImplementations: TStringList;
var
  lClass: TcbClass;
begin
  fImplementationCode.Clear;

  for lClass in Self do
    fImplementationCode.AddStrings(lClass.WriteSourceCode);

  Result := fImplementationCode;
end;

{ TcbClassPropertyList }

constructor TcbClassPropertyList.Create(AOwnerClass: TcbClass);
begin
  inherited Create;

  fOwnerClass := AOwnerClass;

  fPropertyStrings[csPackage] := TStringList.Create;
  fPropertyStrings[csPrivate] := TStringList.Create;
  fPropertyStrings[csProtected] := TStringList.Create;
  fPropertyStrings[csPublic] := TStringList.Create;
  fPropertyStrings[csPublished] := TStringList.Create;
end;

destructor TcbClassPropertyList.Destroy;
var
  lClassProperty: TcbClassProperty;
begin
  if Assigned(fPropertyStrings[csPackage]) then
    fPropertyStrings[csPackage].Free;
  if Assigned(fPropertyStrings[csPrivate]) then
    fPropertyStrings[csPrivate].Free;
  if Assigned(fPropertyStrings[csProtected]) then
    fPropertyStrings[csProtected].Free;
  if Assigned(fPropertyStrings[csPublic]) then
    fPropertyStrings[csPublic].Free;
  if Assigned(fPropertyStrings[csPublished]) then
    fPropertyStrings[csPublished].Free;

  for lClassProperty in Self do
    if Assigned(lClassProperty) then
      lClassProperty.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbClassPropertyList.WriteProperties(ASection: TcbClassSection): TStringList;
var
  lClassProperty: TcbClassProperty;
begin
  fPropertyStrings[ASection].Clear;

  for lClassProperty in Self do
    if lClassProperty.ClassSection = ASection then
      fPropertyStrings[ASection].AddStrings(lClassProperty.WriteSourceCode);

  Result := fPropertyStrings[ASection];
end;

procedure TcbClassPropertyList.Add(APropertyName: String; APropertyType: IcbType; AClassSection: TcbClassSection; APropertyReadElement: IcbClassElement;
  APropertyWriteElement: IcbClassElement; AComment: TcbSourceComment);
var
  lClassProperty: TcbClassProperty;
begin
  lClassProperty := TcbClassProperty.Create(
    fOwnerClass,
    APropertyName,
    APropertyType,
    AClassSection,
    APropertyReadElement,
    APropertyWriteElement,
    AComment);

  Add(lClassProperty);
end;

procedure TcbClassPropertyList.Add(AClassProperty: TcbClassProperty);
begin
  inherited Add(AClassProperty);
end;

function TcbClassPropertyList.HasElementsInSection(ASection: TcbClassSection): Boolean;
var
  lClassProperty: TcbClassProperty;
begin
  Result := False;

  for lClassProperty in Self do
    if lClassProperty.ClassSection = ASection then
    begin
      Result := True;
      Break;
    end;
end;

{ TcbClassVariableList }

constructor TcbClassVariableList.Create(AOwnerClass: TcbClass);
begin
  inherited Create;

  fOwnerClass := AOwnerClass;
  fVariableStrings[csPackage] := TStringList.Create;
  fVariableStrings[csPrivate] := TStringList.Create;
  fVariableStrings[csProtected] := TStringList.Create;
  fVariableStrings[csPublic] := TStringList.Create;
  fVariableStrings[csPublished] := TStringList.Create;
end;

destructor TcbClassVariableList.Destroy;
var
  lClassVariable: TcbClassVariable;
begin
  if Assigned(fVariableStrings[csPackage]) then
    fVariableStrings[csPackage].Free;
  if Assigned(fVariableStrings[csPrivate]) then
    fVariableStrings[csPrivate].Free;
  if Assigned(fVariableStrings[csProtected]) then
    fVariableStrings[csProtected].Free;
  if Assigned(fVariableStrings[csPublic]) then
    fVariableStrings[csPublic].Free;
  if Assigned(fVariableStrings[csPublished]) then
    fVariableStrings[csPublished].Free;


  for lClassVariable in Self do
    if Assigned(lClassVariable) then
      lClassVariable.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbClassVariableList.WriteVariables(ASection: TcbClassSection): TStringList;
var
  lClassVariable: TcbClassVariable;
begin
  fVariableStrings[ASection].Clear;

  for lClassVariable in Self do
    if lClassVariable.ClassSection = ASection then
      fVariableStrings[ASection].AddStrings(lClassVariable.WriteSourceCode);

  Result := fVariableStrings[ASection];
end;

procedure TcbClassVariableList.Add(AVariableName: String; AVariableType: IcbType; AClassSection: TcbClassSection);
var
  lClassVariable: TcbClassVariable;
begin
  lClassVariable := TcbClassVariable.Create(
    fOwnerClass,
    AVariableName,
    AVariableType,
    AClassSection);

  Add(lClassVariable);
end;

procedure TcbClassVariableList.Add(AClassVariable: TcbClassVariable);
begin
  inherited Add(AClassVariable);
end;

function TcbClassVariableList.HasElementsInSection(ASection: TcbClassSection): Boolean;
var
  lClassVariable: TcbClassVariable;
begin
  Result := False;

  for lClassVariable in Self do
    if lClassVariable.ClassSection = ASection then
    begin
      Result := True;
      Break;
    end;
end;

{ TcbVariableList }

constructor TcbVariableList.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create;

  fOwnerComponent := AOwnerComponent;
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

procedure TcbVariableList.Add(AVariableName: String; AVariableType: IcbType);
var
  lVariable: TcbVariable;
begin
  lVariable := TcbVariable.Create(
    fOwnerComponent,
    AVariableName,
    AVariableType);

  Add(lVariable);
end;

procedure TcbVariableList.Add(AVariable: TcbVariable);
begin
  inherited Add(AVariable);
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
      AddStrings(lVariable.WriteSourceCode);
  end;

  Result := fVariableStrings;
end;

{ TcbClassMethodList }

constructor TcbClassMethodList.Create(AOwnerClass: TcbClass);
begin
  inherited Create;

  fOwnerClass := AOwnerClass;

  fPrototypeCodeStrings[csPackage] := TStringList.Create;
  fPrototypeCodeStrings[csPrivate] := TStringList.Create;
  fPrototypeCodeStrings[csPublic] := TStringList.Create;
  fPrototypeCodeStrings[csProtected] := TStringList.Create;
  fPrototypeCodeStrings[csPublished] := TStringList.Create;
  fImplementationCodeStrings := TStringList.Create;
end;

destructor TcbClassMethodList.Destroy;
var
  lClassMethod: TcbClassMethod;
begin
  if Assigned(fPrototypeCodeStrings[csPackage]) then
    fPrototypeCodeStrings[csPackage].Free;
  if Assigned(fPrototypeCodeStrings[csPrivate]) then
    fPrototypeCodeStrings[csPrivate].Free;
  if Assigned(fPrototypeCodeStrings[csPublic]) then
    fPrototypeCodeStrings[csPublic].Free;
  if Assigned(fPrototypeCodeStrings[csProtected]) then
    fPrototypeCodeStrings[csProtected].Free;
  if Assigned(fPrototypeCodeStrings[csPublished]) then
    fPrototypeCodeStrings[csPublished].Free;

  if Assigned(fImplementationCodeStrings) then
    fImplementationCodeStrings.Free;

  for lClassMethod in Self do
    if Assigned(lClassMethod) then
      lClassMethod.Free;

  Self.Clear;

  inherited Destroy;
end;

procedure TcbClassMethodList.Add(AMethodName: String; AMethodType: TcbMethodType; AMethodImplementation: TcbSourceCodeMethodBody;
  ALeadingCommentBlock: TcbSourceCommentBlock; AClassSection: TcbClassSection; AMethodDirectives: TcbMethodDirectives; AReturnType: IcbType);
var
  lClassMethod: TcbClassMethod;
begin
  lClassMethod := TcbClassMethod.Create(
    fOwnerClass,
    AMethodName,
    AMethodType,
    AMethodImplementation,
    ALeadingCommentBlock,
    AClassSection,
    AMethodDirectives,
    AReturnType);

  Add(lClassMethod);
end;

procedure TcbClassMethodList.Add(AClassMethod: TcbClassMethod);
begin
  fOwnerClass.ClassElementList.Add(AClassMethod);
  inherited Add(AClassMethod);
end;

function TcbClassMethodList.WritePrototypes(ASection: TcbClassSection): TStringList;
var
  lMethod: TcbClassMethod;
begin
  fPrototypeCodeStrings[ASection].Clear;

  for lMethod in Self do
    if lMethod.ClassSection = ASection then
      fPrototypeCodeStrings[ASection].AddStrings(lMethod.WritePrototype);

  Result := fPrototypeCodeStrings[ASection];
end;

function TcbClassMethodList.WriteImplementations: TStringList;
var
  lMethod: TcbClassMethod;
begin
  fImplementationCodeStrings.Clear;

  for lMethod in Self do
    fImplementationCodeStrings.AddStrings(lMethod.WriteSourceCode);

  Result := fImplementationCodeStrings;
end;

function TcbClassMethodList.HasElementsInSection(ASection: TcbClassSection): Boolean;
var
  lClassMethod: TcbClassMethod;
begin
  Result := False;

  for lClassMethod in Self do
    if lClassMethod.ClassSection = ASection then
    begin
      Result := True;
      Break;
    end;
end;

{ TcbMethodList }

constructor TcbMethodList.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create;

  fOwnerComponent := AOwnerComponent;
  fPrototypeCodeStrings := TStringList.Create;
  fImplementationCodeStrings := TStringList.Create;
end;

destructor TcbMethodList.Destroy;
var
  lMethod: TcbMethod;
begin
  if Assigned(fPrototypeCodeStrings) then
    fPrototypeCodeStrings.Free;

  if Assigned(fImplementationCodeStrings) then
    fImplementationCodeStrings.Free;

  for lMethod in Self do
    if Assigned(lMethod) then
      lMethod.Free;

  Self.Clear;

  inherited Destroy;
end;

procedure TcbMethodList.Add(AMethodName: String; AMethodType: TcbMethodType; AMethodImplementation: TcbSourceCodeMethodBody;
  ALeadingCommentBlock: TcbSourceCommentBlock; AMethodDirectives: TcbMethodDirectives; AReturnType: IcbType);
var
  lMethod: TcbMethod;
begin
  lMethod := TcbMethod.Create(
    fOwnerComponent,
    AMethodName,
    AMethodType,
    AMethodImplementation,
    ALeadingCommentBlock,
    AMethodDirectives,
    AReturnType);

  Add(lMethod);
end;

procedure TcbMethodList.Add(AMethod: TcbMethod);
begin
  inherited Add(AMethod);
end;

function TcbMethodList.WritePrototypes: TStringList;
var
  lMethod: TcbMethod;
begin
  fPrototypeCodeStrings.Clear;

  for lMethod in Self do
    fPrototypeCodeStrings.AddStrings(lMethod.WritePrototype);

  Result := fPrototypeCodeStrings;
end;

function TcbMethodList.WriteImplementations: TStringList;
var
  lMethod: TcbMethod;
begin
  fImplementationCodeStrings.Clear;

  for lMethod in Self do
    fImplementationCodeStrings.AddStrings(lMethod.WriteSourceCode);

  Result := fImplementationCodeStrings;
end;

{ TcbComponentList }

constructor TcbComponentList.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create;

  fOwnerComponent := AOwnerComponent;
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
  // DONE -oAPL -cClassBuilder 2: Make sure that items with defaults are in the end of the written string

  Result := '';

  if Count > 0 then
    Result := '(';

  for lMethodParameter in Self do
    if lMethodParameter.ParameterDefault = '' then
      Result := Result + lMethodParameter.WriteParameter;

  for lMethodParameter in Self do
    if lMethodParameter.ParameterDefault <> '' then
      Result := Result + lMethodParameter.WriteParameter;

  if Count > 0 then
    Result := ')';
end;

procedure TcbMethodParameterList.Add(AParameterName: String; AParameterType: IcbType; AParameterModifier: TcbMethodParameterModifier; AParameterDefault: String
  );
var
  lParameter: TcbMethodParameter;
begin
  lParameter := TcbMethodParameter.Create(
    fOwnerMethod,
    AParameterName,
    AParameterType,
    AParameterModifier,
    AParameterDefault);

  Add(lParameter);
end;

procedure TcbMethodParameterList.Add(AParameter: TcbMethodParameter);
begin
  inherited Add(AParameter);
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
  (*Result := TStringList.Create;

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
    for lInterface in UnitInterfaces do
      AddStrings(lInterface.WriteTypeDeclaration);

    // Write types

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
        AddStrings(lUnitMethod.WriteImplementation);
        Add('end;');
      end;

    // Write class implementations

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
  end;*)
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

constructor TcbComponent.Create(AOwnerComponent: TcbComponent; AConfig: TXMLConfig; ALogger: TLogger);
var
  lConfigConsumer: IcbXMLConfigConsumer;
  lLoggerConsumer: IcbLoggerConsumer;
begin
  inherited Create;

  fOwnerComponent := AOwnerComponent;

  // fNamedItemList := TcbNamedElementList.Create;

  // Assign the specified config locally, or take from AOwner component if
  // possible
   if Assigned(AConfig) then
    fConfig := AConfig
   else if Assigned(fOwnerComponent) then
    if Supports(fOwnerComponent, IcbXMLConfigConsumer, lConfigConsumer) then
      fConfig := lConfigConsumer.Config;

  // Assign the specified logger locally, or take from AOwner component if
  // possible
  if Assigned(ALogger) then
    fLogger := ALogger
  else if Assigned(fOwnerComponent) then
    if Supports(fOwnerComponent, IcbLoggerConsumer, lLoggerConsumer) then
      fLogger := lLoggerConsumer.Log;
end;

destructor TcbComponent.Destroy;
begin
  // if Assigned(fNamedItemList) then
    // fNamedItemList.Free;

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

function TcbComponent.GetOwnerComponent: TcbComponent;
begin
  Result := fOwnerComponent;
end;

{ TcbUnit }

constructor TcbUnit.Create(AOwner: TcbComponent);
begin
  inherited Create(AOwner);

  fUnitTopCommentBlock := TcbSourceCommentBlock.Create(Self);
  fUnitTopCompilerDirectives := TStringList.Create;
  fUnitInterfaceUsesList := TStringList.Create;
  fUnitImplementationUsesList := TStringList.Create;
  fUnitMethods := TcbMethodList.Create(Self);
  fUnitVariables := TcbVariableList.Create(Self);
  fUnitInitialization := TcbSourceCodeBlock.Create(Self);
  fUnitFinalization := TcbSourceCodeBlock.Create(Self);
  fUnitClassList := TcbClassList.Create(Self);
  fUnitConstants := TStringList.Create;
  fUnitInterfaces := TcbInterfaceList.Create(Self);
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

constructor TcbVariable.Create(AOwnerComponent: TcbComponent; AVariableName: String; AVariableType: IcbType; AComment: TcbSourceComment);
begin
  inherited Create(AOwnerComponent, '', AComment);

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

function TcbVariable.WriteSourceCode: TStringList;
begin
  if not Assigned(fVariableType) then
    raise TcbNilReferenceException.CreateFmt('Cannot write variable: %s. VariableType is not assigned.', [fVariableName]);

  CodeLine := fVariableName + ': ' + fVariableType.GetTypeName + ';';

  Result := inherited WriteSourceCode;
end;

function TcbMethod.WriteImplementationPrototype: TStringList;
var
  lString: String;
begin
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write method implementation prototype for %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);

  fImplementationPrototypeCode.Clear;

  lString := '';

  // Write method type
  lString := MethodTypeToString(fMethodType) + ' ';
  // Write methodname
  lString := lString + fMethodName;
  // Write parameters
  lString := lString + fParameterList.WriteParameters;
  // Write returntype, if this is a function
  if fMethodType = mtFunction then
    lString := lString + ': ' + fReturnType.GetTypeName;

  // End the header
  lString := lString + ';';

  // Write method directives
  if mdtOverride in fMethodDirectives then
    lString := lString + ' override;';
  if mdtOverload in fMethodDirectives then
    lString := lString + ' overload;';
  if mdtAbstract in fMethodDirectives then
    lString := lString + ' abstract;';
  if mdtVirtual in fMethodDirectives then
    lString := lString + ' virtual;'
  else if mdtDynamic in fMethodDirectives then
    lString := lString + ' dynamic;';

  fImplementationPrototypeCode.Add(lString);
  Result := fImplementationPrototypeCode;
end;

constructor TcbMethod.Create(AOwnerComponent: TcbComponent; AMethodName: String; AMethodType: TcbMethodType; AMethodImplementation: TcbSourceCodeMethodBody;
  ALeadingCommentBlock: TcbSourceCommentBlock; AMethodDirectives: TcbMethodDirectives; AReturnType: IcbType);
begin
  inherited Create(AOwnerComponent);

  fMethodType := AMethodType;
  fMethodName := AMethodName;
  fReturnType := AReturnType;
  fLeadingCommentBlock := ALeadingCommentBlock;
  fMethodImplementation := AMethodImplementation;
  fMethodDirectives := AMethodDirectives;
  fParameterList := TcbMethodParameterList.Create(Self);
  fImplementationVars := TcbVariableList.Create(Self);
  fImplementationPrototypeCode := TStringList.Create;

  if not Assigned(fMethodImplementation) then
    fMethodImplementation := TcbSourceCodeMethodBody.Create(Self);

  if not Assigned(fLeadingCommentBlock) then
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
  if Assigned(fImplementationPrototypeCode) then
    fImplementationPrototypeCode.Free;

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

function TcbMethod.WriteSourceCode: TStringList;
begin
  if not Assigned(fMethodImplementation) then
    raise TcbNilReferenceException.CreateFmt('Cannot write method implementation for %s %s. MethodImplementation is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);

  Result := inherited WriteSourceCode;

  with Result do
  begin
    // Write the leading comment block
    AddStrings(fLeadingCommentBlock.WriteSourceCode);
    // Write the implementation prototype
    AddStrings(WriteImplementationPrototype);
    // Write the local vars
    AddStrings(ImplementationVars.WriteVariables);
    // Write the implementation
    AddStrings(MethodImplementation.WriteSourceCode);
  end;
end;

procedure TcbMethod.AddParameter(AParameter: TcbMethodParameter);
begin
  fParameterList.Add(AParameter);
end;

procedure TcbMethod.AddParameter(AParameterName: String; AParameterType: IcbType; AParameterModifier: TcbMethodParameterModifier; AParameterDefault: String);
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

procedure TcbMethod.AddVariable(AVariableName: String; AVariableType: IcbType);
begin
  fImplementationVars.Add(
    AVariableName,
    AVariableType);
end;

function TcbMethod.WritePrototype: TStringList;
var
  lString: String;
begin
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write method prototype for %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);

  Result := inherited WritePrototype;

  // fMethodType: TcbMethodType;
  lString := MethodTypeToString(fMethodType) + ' ';
  // fMethodName: String;
  lString := lString + fMethodName;
  // fParameterList: TcbMethodParameterList;
  lString := lString + fParameterList.WriteParameters;

  if fMethodType = mtFunction then
    lString := lString + ': ' + fReturnType.GetTypeName;

  lString := lString + ';';

  if mdtOverride in fMethodDirectives then
    lString := lString + ' override;';
  if mdtOverload in fMethodDirectives then
    lString := lString + ' overload;';
  if mdtAbstract in fMethodDirectives then
    lString := lString + ' abstract;';
  if mdtVirtual in fMethodDirectives then
    lString := lString + ' virtual;'
  else if mdtDynamic in fMethodDirectives then
    lString := lString + ' dynamic;';

  Result.Add(lString);
end;

{ TcbMethodParameter }

constructor TcbMethodParameter.Create(AOwnerMethod: TcbMethod; AParameterName: String; AParameterType: IcbType; AParameterModifier: TcbMethodParameterModifier;
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
  if not Assigned(fParameterType) then
    raise TcbNilReferenceException.CreateFmt('Cannot write method parameter: %s. ParameterType is not assigned.', [fParameterName]);

  Result := '';

  // Wtite parameter modifier
  Result := Result + MethodParameterModifierToString(fParameterModifier);
  // Write parameter name
  Result := Result + fParameterName;
  // Write parameter type
  Result := Result + ': ' + fParameterType.GetTypeName;
  // Write parameter default
  if fParameterDefault <> '' then
    Result := Result + ' = ' + fParameterDefault;
end;

function TcbClass.HasElementsInSection(ASection: TcbClassSection): Boolean;
begin
  Result :=
    fClassVariableList.HasElementsInSection(ASection)
    or fClassPropertyList.HasElementsInSection(ASection)
    or fClassMethodList.HasElementsInSection(ASection);
end;

function TcbClass.WritePrototypeSection(ASection: TcbClassSection): TStringList;
begin
  fPrototypeClassSections[ASection].Clear;

  with fPrototypeClassSections[ASection] do
  begin
    AddStrings(fClassVariableList.WriteVariables(ASection));
    AddStrings(fClassMethodList.WritePrototypes(ASection));
    AddStrings(fClassPropertyList.WriteProperties(ASection));
  end;

  Result := fPrototypeClassSections[ASection];
end;

constructor TcbClass.Create(AOwnerUnit: TcbUnit; AClassTypeName: String; AHasClassForwardDeclaration: Boolean; AExtendingClass: IcbType);
begin
  inherited Create(AOwnerUnit);

  fOwnerUnit := AOwnerUnit;
  fClassTypeName := AClassTypeName;
  fHasClassForwardDeclaration := AHasClassForwardDeclaration;
  fExtendingClass := AExtendingClass;

  fImplementingInterfaces := TcbTypeList.Create;
  fClassVariableList := TcbClassVariableList.Create(Self);
  fClassMethodList := TcbClassMethodList.Create(Self);
  fClassPropertyList := TcbClassPropertyList.Create(Self);
  fClassElementList := TcbClassElementList.Create(Self);

  fPrototypeClassSections[csPrivate] := TStringList.Create;
  fPrototypeClassSections[csPackage] := TStringList.Create;
  fPrototypeClassSections[csProtected] := TStringList.Create;
  fPrototypeClassSections[csPublic] := TStringList.Create;
  fPrototypeClassSections[csPublished] := TStringList.Create;
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

  if Assigned(fPrototypeClassSections[csPrivate]) then
    fPrototypeClassSections[csPrivate].Free;
  if Assigned(fPrototypeClassSections[csPackage]) then
    fPrototypeClassSections[csPackage].Free;
  if Assigned(fPrototypeClassSections[csProtected]) then
    fPrototypeClassSections[csProtected].Free;
  if Assigned(fPrototypeClassSections[csPublic]) then
    fPrototypeClassSections[csPublic].Free;
  if Assigned(fPrototypeClassSections[csPublished]) then
    fPrototypeClassSections[csPublished].Free;

  inherited;
end;

function TcbClass.GetElementName: String;
begin
  Result := fClassTypeName;
end;

procedure TcbClass.SetElementName(AName: String);
begin
  fClassTypeName := AName;
end;

function TcbClass.GetTypeName: String;
begin
  Result := fClassTypeName;
end;

procedure TcbClass.SetTypeName(ATypeName: String);
begin
  fClassTypeName := ATypeName;
end;

function TcbClass.WritePrototype: TStringList;
var
  lClassDeclaration: String;
  lHasElements: Boolean;
  lInterfaces: IcbType;
begin
  Result := inherited WritePrototype;

  lHasElements :=
    (fClassVariableList.Count > 0)
    or (fClassMethodList.Count > 0)
    or (fClassPropertyList.Count > 0)
    or (fClassElementList.Count > 0);

  with Result do
  begin
    lClassDeclaration := fClassTypeName + ' = class';

    if Assigned(fExtendingClass) then
    begin
      lClassDeclaration := lClassDeclaration + '(' + fExtendingClass.GetTypeName;

      for lInterfaces in fImplementingInterfaces do
        lClassDeclaration := lClassDeclaration + ', ' + lInterfaces.GetTypeName;

      lClassDeclaration := lClassDeclaration + ')';
    end;

    if not lHasElements then
      lClassDeclaration := lClassDeclaration + ';';

    Add(lClassDeclaration);

    // Write package section
    AddStrings(WritePrototypeSection(csPackage));

    // Write private section
    if HasElementsInSection(csPrivate) then
      Add(ClassSectionToString(csPrivate));
    AddStrings(WritePrototypeSection(csPrivate));

    // Write protected section
    if HasElementsInSection(csProtected) then
      Add(ClassSectionToString(csProtected));
    AddStrings(WritePrototypeSection(csProtected));

    // Write public section
    if HasElementsInSection(csPublic) then
      Add(ClassSectionToString(csPublic));
    AddStrings(WritePrototypeSection(csPublic));

    // Write published section
    if HasElementsInSection(csPublished) then
      Add(ClassSectionToString(csPublished));
    AddStrings(WritePrototypeSection(csPublished));

    if lHasElements then
      Add('end;');
  end;
end;

function TcbClass.WriteSourceCode: TStringList;
begin
  Result := inherited WriteSourceCode;
  Result.AddStrings(fClassMethodList.WriteImplementations);
end;

function TcbClass.WriteForwardDeclaration: String;
begin
  Result := fClassTypeName + ' = class;';
end;

procedure TcbClass.AddVariable(AVariable: TcbClassVariable);
begin
  fClassVariableList.Add(AVariable);
end;

procedure TcbClass.AddVariable(AVariableName: String; AVariableType: IcbType; AClassSection: TcbClassSection);
begin
  fClassVariableList.Add(
    AVariableName,
    AVariableType,
    AClassSection);
end;

procedure TcbClass.AddMethod(AMethod: TcbClassMethod);
begin
  fClassMethodList.Add(AMethod);
end;

procedure TcbClass.AddMethod(AMethodName: String; AMethodType: TcbMethodType; AMethodImplementation: TcbSourceCodeMethodBody;
  ALeadingCommentBlock: TcbSourceCommentBlock; AClassSection: TcbClassSection; AMethodDirectives: TcbMethodDirectives; AReturnType: IcbType);
begin
  fClassMethodList.Add(
    AMethodName,
    AMethodType,
    AMethodImplementation,
    ALeadingCommentBlock,
    AClassSection,
    AMethodDirectives,
    AReturnType);
end;

procedure TcbClass.AddProperty(AProperty: TcbClassProperty);
begin
  fClassPropertyList.Add(AProperty);
end;

procedure TcbClass.AddProperty(APropertyName: String; APropertyType: IcbType; AClassSection: TcbClassSection; APropertyReadElement: IcbClassElement;
  APropertyWriteElement: IcbClassElement; AComment: TcbSourceComment);
begin
  fClassPropertyList.Add(
    APropertyName,
    APropertyType,
    AClassSection,
    APropertyReadElement,
    APropertyWriteElement,
    AComment);
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

function TcbClassProperty.GetOwnerClass: TcbClass;
begin
  Result := fOwnerClass;
end;

function TcbClassProperty.GetClassSection: TcbClassSection;
begin
  Result := fClassSection;
end;

function TcbClassProperty.WriteSourceCode: TStringList;
begin
  if not Assigned(fPropertyType) then
    raise TcbNilReferenceException.CreateFmt('Cannot write class property: %s. PropertyType is not assigned.', [fPropertyName]);

  CodeLine := fPropertyName + ': ' + fPropertyType.GetTypeName;

  if Assigned(fPropertyReadElement) then
    CodeLine := CodeLine + ' read ' + fPropertyReadElement.GetElementName;

  if Assigned(fPropertyWriteElement) then
    CodeLine := CodeLine + ' write ' + fPropertyWriteElement.GetElementName;

  CodeLine := CodeLine + ';';

  Result := inherited WriteSourceCode;
end;

constructor TcbClassProperty.Create(AOwnerClass: TcbClass; APropertyName: String; APropertyType: IcbType; AClassSection: TcbClassSection;
  APropertyReadElement: IcbClassElement; APropertyWriteElement: IcbClassElement; AComment: TcbSourceComment);
begin
  inherited Create(AOwnerClass, '', AComment);

  fOwnerClass := AOwnerClass;
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

function TcbClassVariable.GetOwnerClass: TcbClass;
begin
  Result := fOwnerClass;
end;

function TcbClassVariable.GetClassSection: TcbClassSection;
begin
  Result := fClassSection;
end;

constructor TcbClassVariable.Create(AOwnerClass: TcbClass; AVariableName: String; AVariableType: IcbType; AClassSection: TcbClassSection);
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

function TcbClassMethod.WriteImplementationPrototype: TStringList;
var
  lString: String;
begin
  if not Assigned(fOwnerClass) then
    raise TcbNilReferenceException.CreateFmt('Cannot write class method implementation prototype for %s %s. OwnerClass is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);

  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write class method implementation prototype for %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);

  // DONE -oAPL -cCodeBuilder 2: Write code that generates the declaration, the prototype

  lString := '';

  // Write method type
  lString := MethodTypeToString(fMethodType) + ' ';
  // Write the classname
  lString := lString + fOwnerClass.ClassTypeName + '.';
  // Write methodname
  lString := lString + fMethodName;
  // Write parameters
  lString := lString + fParameterList.WriteParameters;
  // Write returntype, if this is a function
  if fMethodType = mtFunction then
    lString := lString + ': ' + fReturnType.GetTypeName;

  // End the header
  lString := lString + ';';

  // Write method directives
  if mdtOverride in fMethodDirectives then
    lString := lString + ' override;';
  if mdtOverload in fMethodDirectives then
    lString := lString + ' overload;';
  if mdtAbstract in fMethodDirectives then
    lString := lString + ' abstract;';
  if mdtVirtual in fMethodDirectives then
    lString := lString + ' virtual;'
  else if mdtDynamic in fMethodDirectives then
    lString := lString + ' dynamic;';

  fImplementationPrototypeCode.Add(lString);
  Result := fImplementationPrototypeCode;
end;

function TcbClassMethod.GetOwnerClass: TcbClass;
begin
  Result := fOwnerClass;
end;

function TcbClassMethod.GetClassSection: TcbClassSection;
begin
  Result := fClassSection;
end;

constructor TcbClassMethod.Create(AOwnerClass: TcbClass; AMethodName: String; AMethodType: TcbMethodType; AMethodImplementation: TcbSourceCodeMethodBody;
  ALeadingCommentBlock: TcbSourceCommentBlock; AClassSection: TcbClassSection; AMethodDirectives: TcbMethodDirectives; AReturnType: IcbType);
begin
  inherited Create(
    AOwnerClass,
    AMethodName,
    AMethodType,
    AMethodImplementation,
    ALeadingCommentBlock,
    AMethodDirectives,
    AReturnType);

  fOwnerClass := AOwnerClass;
  fClassSection := AClassSection;
  fPrototypeCodeStrings := TStringList.Create;
  fImplementationCodeStrings := TStringList.Create;
end;

destructor TcbClassMethod.Destroy;
begin
  if Assigned(fPrototypeCodeStrings) then
    fPrototypeCodeStrings.Free;

  if Assigned(fImplementationCodeStrings) then
    fImplementationCodeStrings.Free;

  inherited;
end;

end.

