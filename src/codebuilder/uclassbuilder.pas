// TODO -oAPL -cClassBuilder 2: Find some way to implement generics
// TODO -oAPL -cClassBuilder 3: Clean up the interface named element on properties
// TODO -oAPL -cClassBuilder 3: Handle non prototyped functions
// TODO -oAPL -cClassBuilder 3: Handle method level constants
// TODO -oAPL -cClassBuilder 2: Make logic that makes sure that things are written in the right order, forward declarations solve some things, but constants and variables are not handled
// TODO -oAPL -cClassBuilder 5: Perhaps clean up the unused list types
// TODO -oAPL -cClassBuilder 3: Make logic that inspects all types used in a unit, to compose the uses clause for the implementation and the interface sections. Need to associate all types with an origin unit in this case
// TODO -oAPL -cClassBuilder 1: Change the use of TMethod to the other Method types.
// TODO -oAPL -cClassBuidler 1: Change it so that the sourcecode block used in the method bodies can contain comments

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
  TcbCommentStyle = (
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
    mdtDynamic,
    mdtReintroduce);

  TcbMethodDirectives = set of TcbMethodDirectiveTypes;

  TcbMethodType = (
    mtProcedure,
    mtFunction,
    mtConstructor,
    mtDestructor);

  TcbClassSection = (
    csDefault,
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
  TcbComponentList = class;
  TcbCommentLine = class;
  TcbCommentBlock = class;
  TcbCodeLine = class;
  TcbCodeMethodBody = class;
  TcbClass = class;
  TcbClassList = class;
  TcbMethodParameter = class;
  TcbMethodParameterList = class;
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
    property ClassSection: TcbClassSection read GetClassSection;
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
    fChildComponents: TcbComponentList;
  public
    constructor Create(AOwnerComponent: TcbComponent; AConfig: TXMLConfig = nil; ALogger: TLogger = nil); overload;
    destructor Destroy; override;

    procedure SetLogger(ALogger: TLogger);
    function GetLogger: TLogger;
    procedure SetConfig(AConfig: TXMLConfig);
    function GetConfig: TXMLConfig;
    function GetOwnerComponent: TcbComponent; virtual;
    procedure RegisterChild(AComponent: TcbComponent); virtual;
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
  published
    property TypeName: String read GetTypeName write SetTypeName;
  end;

  { TcbPrototypedCodeComponent }

  TcbPrototypedCodeComponent = class(TcbComponent)
  private
    fPrototypeStrings: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WritePrototype: TStringList; virtual;
  end;

  { TcbSourceCodeComponent }

  TcbSourceCodeComponent = class(TcbComponent)
  private
    fCodeStrings: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; virtual;
  end;

  { TcbPrototypedSourceCodeComponent }

  TcbPrototypedSourceCodeComponent = class(TcbPrototypedCodeComponent)
  private
    fCodeStrings: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; virtual;
  end;

  { TcbCommentLine }

  TcbCommentLine = class(TcbSourceCodeComponent)
  private
    fComment: String;
    fCommentStyle: TcbCommentStyle;
  public
    constructor Create(AOwnerComponent: TcbComponent; AComment: String; ACommentStyle: TcbCommentStyle = ccsSlashes);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
  published
    property Comment: String read fComment write fComment;
    property CommentStyle: TcbCommentStyle read fCommentStyle write fCommentStyle;
  end;

  { TcbCommentBlock }

  TcbCommentBlock = class(specialize TFPGList<TcbCommentLine>)
  private
    fOwnerComponent: TcbComponent;
    fCodeStrings: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent; AComment: String = ''; ACommentStyle: TcbCommentStyle = ccsSlashes);
    destructor Destroy; override;
    function Add(AComment: String; ACommentStyle: TcbCommentStyle = ccsSlashes): TcbCommentLine; overload;
    function WriteSourceCode: TStringList; virtual;
  published
    property OwnerComponent: TcbComponent read fOwnerComponent;
  end;

  { TcbCodeLine }

  TcbCodeLine = class(TcbSourceCodeComponent)
  private
    fCodeLine: String;
    fComments: TcbCommentBlock;
  public
    constructor Create(AOwnerComponent: TcbComponent); overload;
    constructor Create(AOwnerComponent: TcbComponent; ACodeLine: String; ACommentBlock: TcbCommentBlock = nil); overload;
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
  published
    property CodeLine: String read fCodeLine write fCodeLine;
    property Comments: TcbCommentBlock read fComments write fComments;
  end;

  { TcbCodeBlock }

  TcbCodeBlock = class(specialize TFPGList<TcbCodeLine>)
  private
    fOwnerComponent: TcbComponent;
    fCodeStrings: TStringList;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; virtual;
    function Add(ACodeLine: String; AComment: TcbCommentLine = nil): TcbCodeLine; overload;
    function Add(ACodeLine: String; AComment: String; ACommentStyle: TcbCommentStyle = ccsSlashes): TcbCodeLine; overload;
  published
     property OwnerComponent: TcbComponent read fOwnerComponent;
  end;

  { TcbCodeBeginEndBlock }

  TcbCodeBeginEndBlock = class(TcbCodeBlock)
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
  end;

  { TcbCodeMethodBody }

  TcbCodeMethodBody = class(TcbCodeBeginEndBlock)
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
  end;

  { TcbMethodParameter }

  TcbMethodParameter = class(TcbComponent, IcbNamedElement)
  private
    fParameterName: String;
    fParameterType: IcbType;
    fParameterDefault: String;
    fParameterModifier: TcbMethodParameterModifier;
  public
    constructor Create(
      AOwnerComponent: TcbComponent;
      AParameterName: String;
      AParameterType: IcbType;
      AParameterModifier: TcbMethodParameterModifier = mpmNone;
      AParameterDefault: String = '');
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function WriteParameter: String;
    function WriteImlementationParameter: String;
  published
    property ParameterName: String read fParameterName write fParameterName;
    property ParameterType: IcbType read fParameterType write fParameterType;
    property ParameterDefault: String read fParameterDefault write fParameterDefault;
    property ParameterModifier: TcbMethodParameterModifier read fParameterModifier write fParameterModifier;
    property ElementName: String read GetElementName write SetElementName;
  end;

  { TcbMethodParameterList }

  TcbMethodParameterList = class(specialize TFPGList<TcbMethodParameter>)
  private
    fOwnerComponent: TcbComponent;
  public
    constructor Create(AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteParameters: String; virtual;
    function WriteImplementationParameters: String; virtual;
    function Add(
      AParameterName: String;
      AParameterType: IcbType;
      AParameterModifier: TcbMethodParameterModifier = mpmNone;
      AParameterDefault: String = ''): TcbMethodParameter; overload;
  published
     property OwnerComponent: TcbComponent read fOwnerComponent;
  end;

  { TcbConstant }

  TcbConstant = class(TcbSourceCodeComponent, IcbNamedElement)
  private
    fConstantName: String;
    fConstantType: IcbType;
    fConstantValue: String;
  public
    constructor Create(
      AOwnerComponent: TcbComponent;
      AConstantName: String;
      AConstantValue: String;
      AConstantType: IcbType = nil);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
  published
    property ConstantName: String read fConstantName write fConstantName;
    property ConstantType: IcbType read fConstantType write fConstantType;
    property ConstantValue: String read fConstantValue write fConstantValue;
    property ElementName: String read GetElementName write SetElementName;
  end;

  { TcbConstantBlock }

  TcbConstantBlock = class(specialize TFPGList<TcbConstant>)
  private
    fOwnerComponent: TcbComponent;
    fCodeStrings: TStringList;
  public
    constructor Create(
      AOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function WriteConstants: TStringList;
    function Add(
      AConstantName: String;
      AConstantValue: String;
      AConstantType: IcbType = nil): TcbConstant; overload;
  published
    property OwnerComponent: TcbComponent read fOwnerComponent;
  end;

  { TcbInterfaceMethod }

  TcbInterfaceMethod = class(TcbPrototypedCodeComponent, IcbInterfaceElement)
  private
    fOwnerInterface: TcbInterface;
    fMethodType: TcbMethodType;
    fMethodName: String;
    fReturnType: IcbType;
    fMethodDirectives: TcbMethodDirectives;
    fParameters: TcbMethodParameterList;
  public
    constructor Create(
      AOwnerInterface: TcbInterface;
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameters: TcbMethodParameterList = nil);
    destructor Destroy; override;
    function WritePrototype: TStringList; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function GetOwnerInterface: TcbInterface;
  published
    property OwnerInterface: TcbInterface read GetOwnerInterface;
    property MethodType: TcbMethodType read fMethodType write fMethodType;
    property MethodName: String read fMethodName write fMethodName;
    property ReturnType: IcbType read fReturnType write fReturnType;
    property MethodDirectives: TcbMethodDirectives read fMethodDirectives write fMethodDirectives;
    property Parameters: TcbMethodParameterList read fParameters write fParameters;
    property ElementName: String read GetElementName write SetElementName;
  end;

  { TcbInterfaceMethodList }

  TcbInterfaceMethodList = class(specialize TFPGList<TcbInterfaceMethod>)
  private
    fOwnerInterface: TcbInterface;
    fPrototypeCodeStrings: TStringList;
  public
    constructor Create(AOwnerInterface: TcbInterface);
    destructor Destroy; override;
    function Add(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameterList: TcbMethodParameterList = nil): TcbInterfaceMethod;
    function WritePrototypes: TStringList;
  published
    property OwnerInterface: TcbInterface read fOwnerInterface;
  end;

  { TcbPrototypedUnitMethod }

  TcbPrototypedUnitMethod = class(TcbPrototypedSourceCodeComponent, IcbNamedElement)
  private
    fOwnerUnit: TcbUnit;
    fMethodType: TcbMethodType;
    fMethodName: String;
    fReturnType: IcbType;
    fMethodDirectives: TcbMethodDirectives;
    fParameters: TcbMethodParameterList;
    fMethodCode: TcbCodeBeginEndBlock;
    fConstants: TcbConstantBlock;
    fVariables: TcbVariableList;
    fLeadingCommentBlock: TcbCommentBlock;
    fImplementationProtoTypeStrings: TStringList;
  protected
    function WriteImplementationPrototype: TStringList;
  public
    constructor Create(
      AOwnerUnit: TcbUnit;
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameters: TcbMethodParameterList = nil);
    destructor Destroy; override;
    function WritePrototype: TStringList; override;
    function WriteSourceCode: TStringList; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
  published
    property OwnerUnit: TcbUnit read fOwnerUnit;
    property MethodType: TcbMethodType read fMethodType write fMethodType;
    property MethodName: String read fMethodName write fMethodName;
    property ReturnType: IcbType read fReturnType write fReturnType;
    property MethodDirectives: TcbMethodDirectives read fMethodDirectives write fMethodDirectives;
    property MethodCode: TcbCodeBeginEndBlock read fMethodCode write fMethodCode;
    property Constants: TcbConstantBlock read fConstants write fConstants;
    property Variables: TcbVariableList read fVariables write fVariables;
    property LeadingCommentBlock: TcbCommentBlock read fLeadingCommentBlock write fLeadingCommentBlock;
    property ElementName: String read GetElementName write SetElementName;
  end;

  { TcbPrototypedUnitMethodList }

  TcbPrototypedUnitMethodList = class(specialize TFPGList<TcbPrototypedUnitMethod>)
  private
    fOwnerUnit: TcbUnit;
    fPrototypeStrings: TStringList;
    fCodeStrings: TStringList;
  public
    constructor Create(AOwnerUnit: TcbUnit);
    destructor Destroy; override;
    function Add(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameters: TcbMethodParameterList = nil): TcbPrototypedUnitMethod;
    function WritePrototypes: TStringList;
    function WriteImplementations: TStringList;
  published
    property OwnerUnit: TcbUnit read fOwnerUnit;
  end;

  { TcbUnprototypedUnitMethod }

  TcbUnprototypedUnitMethod = class(TcbSourceCodeComponent, IcbNamedElement)
  private
    fOwnerUnit: TcbUnit;
    fMethodType: TcbMethodType;
    fMethodName: String;
    fReturnType: IcbType;
    fMethodDirectives: TcbMethodDirectives;
    fParameters: TcbMethodParameterList;
    fMethodCode: TcbCodeBeginEndBlock;
    fConstants: TcbConstantBlock;
    fVariables: TcbVariableList;
    fLeadingCommentBlock: TcbCommentBlock;
    fProtoTypeStrings: TStringList;
  protected
    function WriteImplementationPrototype: TStringList;
  public
    constructor Create(
      AOwnerUnit: TcbUnit;
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameterList: TcbMethodParameterList = nil);
    destructor Destroy; override;
    function WriteSourceCode: TStringList; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
  published
    property OwnerUnit: TcbUnit read fOwnerUnit;
    property MethodType: TcbMethodType read fMethodType write fMethodType;
    property MethodName: String read fMethodName write fMethodName;
    property ReturnType: IcbType read fReturnType write fReturnType;
    property MethodDirectives: TcbMethodDirectives read fMethodDirectives write fMethodDirectives;
    property MethodCode: TcbCodeBeginEndBlock read fMethodCode write fMethodCode;
    property Constants: TcbConstantBlock read fConstants write fConstants;
    property Variables: TcbVariableList read fVariables write fVariables;
    property LeadingCommentBlock: TcbCommentBlock read fLeadingCommentBlock write fLeadingCommentBlock;
    property ElementName: String read GetElementName write SetElementName;
  end;

  { TcbUnprototypedUnitMethodList }

  TcbUnprototypedUnitMethodList = class(specialize TFPGList<TcbUnprototypedUnitMethod>)
  private
    fOwnerUnit: TcbUnit;
    fCodeStrings: TStringList;
  public
    constructor Create(AOwnerUnit: TcbUnit);
    destructor Destroy; override;
    function Add(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameters: TcbMethodParameterList = nil): TcbUnprototypedUnitMethod;
    function WriteImplementations: TStringList;
  published
    property OwnerUnit: TcbUnit read fOwnerUnit;
  end;

  { TcbClassMethod }

  TcbClassMethod = class(TcbPrototypedSourceCodeComponent, IcbClassElement)
  private
    fOwnerClass: TcbClass;
    fMethodType: TcbMethodType;
    fMethodName: String;
    fReturnType: IcbType;
    fClassSection: TcbClassSection;
    fMethodDirectives: TcbMethodDirectives;
    fParameters: TcbMethodParameterList;
    fMethodCode: TcbCodeBeginEndBlock;
    fConstants: TcbConstantBlock;
    fVariables: TcbVariableList;
    fLeadingCommentBlock: TcbCommentBlock;
    fImplementationPrototypeCode: TStringList;
  protected
    function WriteImplementationPrototype: TStringList; virtual;
  public
    constructor Create(
      AOwnerClass: TcbClass;
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AClassSection: TcbClassSection = csDefault;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameters: TcbMethodParameterList = nil);
    destructor Destroy; override;
    function WritePrototype: TStringList; override;
    function WriteSourceCode: TStringList; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function GetOwnerClass: TcbClass;
    function GetClassSection: TcbClassSection;
  published
    property MethodType: TcbMethodType read fMethodType write fMethodType;
    property MethodName: String read fMethodName write fMethodName;
    property ReturnType: IcbType read fReturnType write fReturnType;
    property MethodDirectives: TcbMethodDirectives read fMethodDirectives write fMethodDirectives;
    property Parameters: TcbMethodParameterList read fParameters write fParameters;
    property ElementName: String read GetElementName write SetElementName;
    property OwnerClass: TcbClass read GetOwnerClass;
    property ClassSection: TcbClassSection read GetClassSection;
    property MethodCode: TcbCodeBeginEndBlock read fMethodCode write fMethodCode;
    property Constants: TcbConstantBlock read fConstants write fConstants;
    property Variables: TcbVariableList read fVariables write fVariables;
    property LeadingCommentBlock: TcbCommentBlock read fLeadingCommentBlock write fLeadingCommentBlock;
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
    function Add(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AClassSection: TcbClassSection = csDefault;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameterList: TcbMethodParameterList = nil): TcbClassMethod; overload;
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
      AVariableType: IcbType); overload;
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
    function Add(
      AVariableName: String;
      AVariableType: IcbType): TcbVariable; overload;
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
      AClassSection : TcbClassSection = csDefault);
    destructor Destroy; override;
    function GetOwnerClass: TcbClass;
    function GetClassSection: TcbClassSection;
  published
    property OwnerClass: TcbClass read GetOwnerClass;
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
    function Add(
      AVariableName: String;
      AVariableType: IcbType;
      AClassSection : TcbClassSection = csDefault): TcbClassVariable; overload;
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
      APropertyWriteElement: IcbInterfaceElement = nil);
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
    function Add(
      APropertyName: String;
      APropertyType: IcbType;
      APropertyReadElement: IcbInterfaceElement = nil;
      APropertyWriteElement: IcbInterfaceElement = nil): TcbInterfaceProperty; overload;
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
      AClassSection: TcbClassSection = csDefault;
      APropertyReadElement: IcbClassElement = nil;
      APropertyWriteElement: IcbClassElement = nil);
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
    function Add(
      APropertyName: String;
      APropertyType: IcbType;
      AClassSection: TcbClassSection = csDefault;
      APropertyReadElement: IcbClassElement = nil;
      APropertyWriteElement: IcbClassElement = nil): TcbClassProperty; overload;
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
    fVariables: TcbClassVariableList;
    fMethods: TcbClassMethodList;
    fProperties: TcbClassPropertyList;
    fClassElements: TcbClassElementList;
    fPrototypeClassSections: array [TcbClassSection] of TStringList;
    function HasElementsInSection(ASection: TcbClassSection): Boolean;
    function WritePrototypeSection(ASection: TcbClassSection): TStringList;
  public
    constructor Create(
      AOwnerUnit: TcbUnit;
      AClassTypeName: String;
      AHasClassForwardDeclaration: Boolean = False;
      AExtendingClass: IcbType = nil);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function GetTypeName: String;
    procedure SetTypeName(ATypeName: String);
    function WritePrototype: TStringList; override;
    function WriteSourceCode: TStringList; override;
    function WriteForwardDeclaration: String;
    function AddVariable(
      AVariableName: String;
      AVariableType: IcbType;
      AClassSection : TcbClassSection = csDefault): TcbClassVariable;
    function AddMethod(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AClassSection: TcbClassSection = csDefault;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameters: TcbMethodParameterList = nil): TcbClassMethod;
    function AddProperty(
      APropertyName: String;
      APropertyType: IcbType;
      AClassSection: TcbClassSection = csDefault;
      APropertyReadElement: IcbClassElement = nil;
      APropertyWriteElement: IcbClassElement = nil): TcbClassProperty;
  published
    property ClassTypeName: String read fClassTypeName write fClassTypeName;
    property ExtendingClass: IcbType read fExtendingClass write fExtendingClass;
    property ImplementingInterfaces: TcbTypeList read fImplementingInterfaces write fImplementingInterfaces;
    property Variables: TcbClassVariableList read fVariables write fVariables;
    property Methods: TcbClassMethodList read fMethods write fMethods;
    property Properties: TcbClassPropertyList read fProperties write fProperties;
    property ClassElements: TcbClassElementList read fClassElements write fClassElements;
    property HasClassForwardDeclaration: Boolean read fHasClassForwardDeclaration write fHasClassForwardDeclaration;
    property TypeName: String read GetTypeName write SetTypeName;
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
    function Add(
      AClassTypeName: String;
      AHasClassForwardDeclaration: Boolean = False;
      AExtendingClass: IcbType = nil): TcbClass;
  published
    property OwnerUnit: TcbUnit read fOwnerUnit;
  end;

  TcbInterface = class(TcbPrototypedCodeComponent, IcbNamedElement, IcbType)
  private
    fOwnerUnit: TcbUnit;
    fInterfaceName: String;
    fInterfaceExtends: IcbType;
    fHasForwardDeclaration: Boolean;
    fInterfaceGUID: TGUID;
    fMethods: TcbInterfaceMethodList;
    fProperties: TcbInterfacePropertyList;
  public
    constructor Create(
      AOwnerUnit: TcbUnit;
      AInterfaceName: String;
      AInterfaceGUID: TGUID;
      AHasForwardDeclaration: Boolean = False;
      AInterfaceExtends: IcbType = nil);
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
    property Methods: TcbInterfaceMethodList read fMethods write fMethods;
    property Properties: TcbInterfacePropertyList read fProperties write fProperties;
    property HasForwardDeclaration: Boolean read fHasForwardDeclaration write fHasForwardDeclaration;
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
    function Add(
      AInterfaceName: String;
      AInterfaceGUID: TGUID;
      AHasForwardDeclaration: Boolean = False;
      AInterfaceExtends: IcbType = nil): TcbInterface;
  published
    property OwnerUnit: TcbUnit read fOwnerUnit;
  end;

  { TcbUnitInterfaceSection }

  TcbUnitInterfaceSection = class(TcbPrototypedSourceCodeComponent)
  private
    fOwnerUnit: TcbUnit;
    fConstants: TcbConstantBlock;
    fVariables: TcbVariableList;
    fClasses: TcbClassList;
    fInterfaces: TcbInterfaceList;
    fPrototypedUnitMethods: TcbPrototypedUnitMethodList;
    fUnprototypedUnitMethods: TcbUnprototypedUnitMethodList;
    // TODO -oAPL -cClassBuilder 2: Make records
  public
    constructor Create(AOwnerUnit: TcbUnit);
    destructor Destroy; override;
    function WritePrototype: TStringList; override;
    function WriteSourceCode: TStringList; override;

    function AddConstant(
      AConstantName: String;
      AConstantValue: String;
      AConstantType: IcbType = nil): TcbConstant;

    function AddVariable(
      AVariableName: String;
      AVariableType: IcbType): TcbVariable;

    function AddClass(
      AClassTypeName: String;
      AHasClassForwardDeclaration: Boolean = False;
      AExtendingClass: IcbType = nil): TcbClass;

    function AddInterface(
      AInterfaceName: String;
      AInterfaceGUID: TGUID;
      AHasClassForwardDeclaration: Boolean = False;
      AInterfaceExtends: IcbType = nil): TcbInterface;

    function AddPrototypedUnitMethod(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameters: TcbMethodParameterList = nil): TcbPrototypedUnitMethod;

    function AddUnprototypedUnitMethod(
      AMethodName: String;
      AMethodType: TcbMethodType = mtProcedure;
      AMethodDirectives: TcbMethodDirectives = [];
      AReturnType: IcbType = nil;
      AParameterList: TcbMethodParameterList = nil): TcbUnprototypedUnitMethod;
  published
    property OwnerUnit: TcbUnit read fOwnerUnit write fOwnerUnit;
    property Constants: TcbConstantBlock read fConstants write fConstants;
    property Variables: TcbVariableList read fVariables write fVariables;
    property Classes: TcbClassList read fClasses write fClasses;
    property Interfaces: TcbInterfaceList read fInterfaces write fInterfaces;
    property PrototypedUnitMethods: TcbPrototypedUnitMethodList read fPrototypedUnitMethods write fPrototypedUnitMethods;
    property UnprototypedUnitMethods: TcbUnprototypedUnitMethodList read fUnprototypedUnitMethods write fUnprototypedUnitMethods;
  end;

  { TcbUnitInterfaceSectionList }

  TcbUnitInterfaceSectionList = class(specialize TFPGList<TcbUnitInterfaceSection>)
  private
    fOwnerUnit: TcbUnit;
    fPrototypeCode: TStringList;
    fImplementationCode: TStringList;
  public
    constructor Create(AOwnerUnit: TcbUnit);
    destructor Destroy; override;
    function AddSection: TcbUnitInterfaceSection;
    function WritePrototypeCode: TStringList;
    function WriteImplementationCode: TStringList;
  published
    property OwnerUnit: TcbUnit read fOwnerUnit write fOwnerUnit;
  end;

  { TcbUnit }

  TcbUnit = class(TcbSourceCodeComponent, IcbNamedElement)
  private
    fUnitName: String;
    fUnitTopCommentBlock: TcbCommentBlock;
    fUnitTopCompilerDirectives: TStringList;
    fUnitInterfaceUsesList: TStringList;
    fUnitImplementationUsesList: TStringList;
    fUnitInitialization: TcbCodeBlock;
    fUnitFinalization: TcbCodeBlock;
    fUnitSections: TcbUnitInterfaceSectionList;

    // TODO -oAPL -cClassBuilder 3: Records, sets... and more?
  public
    constructor Create(AOwner: TcbComponent; AUnitName: String);
    destructor Destroy; override;
    function GetElementName: String;
    procedure SetElementName(AName: String);
    function WriteSourceCode: TStringList; override;
  published
    property UnitName: String read fUnitName write fUnitName;
    property UnitTopCommentBlock: TcbCommentBlock read fUnitTopCommentBlock write fUnitTopCommentBlock;
    property UnitTopCompilerDirectives: TStringList read fUnitTopCompilerDirectives write fUnitTopCompilerDirectives;
    property UnitInterfaceUsesList: TStringList read fUnitInterfaceUsesList write fUnitInterfaceUsesList;
    property UnitImplementationUsesList: TStringList read fUnitImplementationUsesList write fUnitImplementationUsesList;
    property UnitInitialization: TcbCodeBlock read fUnitInitialization write fUnitInitialization;
    property UnitFinalization: TcbCodeBlock read fUnitFinalization write fUnitFinalization;
    property UnitSections: TcbUnitInterfaceSectionList read fUnitSections write fUnitSections;
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
function MethodDirectivesToString(AMethodDirectives: TcbMethodDirectives): String;

implementation

function ClassSectionToString(AClassSection: TcbClassSection): String;
begin
  case AClassSection of
    csDefault: Result := '';
    csPrivate: Result := 'private';
    csPublic: Result := 'public';
    csProtected: Result := 'protected';
    csPublished: Result := 'published';
  end;
end;

function MethodDirectivesToString(AMethodDirectives: TcbMethodDirectives): String;
begin
  Result := '';

  if mdtOverride in AMethodDirectives then
    Result := Result + ' override;';
  if mdtOverload in AMethodDirectives then
    Result := Result + ' overload;';
  if mdtAbstract in AMethodDirectives then
    Result := Result + ' abstract;';
  if mdtVirtual in AMethodDirectives then
    Result := Result + ' virtual;'
  else if mdtDynamic in AMethodDirectives then
    Result := Result + ' dynamic;';
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

{ TcbUnitInterfaceSectionList }

constructor TcbUnitInterfaceSectionList.Create(AOwnerUnit: TcbUnit);
begin
  inherited Create;

  fOwnerUnit := AOwnerUnit;
  fPrototypeCode := TStringList.Create;
  fImplementationCode := TStringList.Create;
end;

destructor TcbUnitInterfaceSectionList.Destroy;
var
  lUnitSection: TcbUnitInterfaceSection;
begin
  if Assigned(fPrototypeCode) then
    fPrototypeCode.Free;
  if Assigned(fImplementationCode) then
    fImplementationCode.Free;

  for lUnitSection in Self do
    if Assigned(lUnitSection) then
      lUnitSection.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbUnitInterfaceSectionList.AddSection: TcbUnitInterfaceSection;
var
  lUnitSection: TcbUnitInterfaceSection;
begin
  lUnitSection := TcbUnitInterfaceSection.Create(fOwnerUnit);
  inherited Add(lUnitSection);
  Result := lUnitSection;
end;

function TcbUnitInterfaceSectionList.WritePrototypeCode: TStringList;
var
  lUnitSection: TcbUnitInterfaceSection;
begin
  fPrototypeCode.Clear;

  for lUnitSection in Self do
    if Assigned(lUnitSection) then
      fPrototypeCode.AddStrings(lUnitSection.WritePrototype);

  Result := fPrototypeCode;
end;

function TcbUnitInterfaceSectionList.WriteImplementationCode: TStringList;
var
  lUnitSection: TcbUnitInterfaceSection;
begin
  fImplementationCode.Clear;

  for lUnitSection in Self do
    if Assigned(lUnitSection) then
      fImplementationCode.AddStrings(lUnitSection.WriteSourceCode);

  Result := fImplementationCode;
end;

{ TcbUnitInterfaceSection }

constructor TcbUnitInterfaceSection.Create(AOwnerUnit: TcbUnit);
begin
  inherited Create(AOwnerUnit);

  fOwnerUnit := AOwnerUnit;

  fConstants :=  TcbConstantBlock.Create(fOwnerUnit);
  fVariables := TcbVariableList.Create(fOwnerUnit);
  fClasses := TcbClassList.Create(fOwnerUnit);
  fInterfaces := TcbInterfaceList.Create(fOwnerUnit);
  fPrototypedUnitMethods := TcbPrototypedUnitMethodList.Create(fOwnerUnit);
  fUnprototypedUnitMethods := TcbUnprototypedUnitMethodList.Create(fOwnerUnit);
end;

destructor TcbUnitInterfaceSection.Destroy;
begin
  if Assigned(fConstants) then
    fConstants.Free;
  if Assigned(fVariables) then
    fVariables.Free;
  if Assigned(fClasses) then
    fClasses.Free;
  if Assigned(fInterfaces) then
    fInterfaces.Free;
  if Assigned(fPrototypedUnitMethods) then
    fPrototypedUnitMethods.Free;
  if Assigned(fUnprototypedUnitMethods) then
    fUnprototypedUnitMethods.Free;
  inherited Destroy;
end;

function TcbUnitInterfaceSection.WritePrototype: TStringList;
begin
  Result := inherited WritePrototype;

  with Result do
  begin
    AddStrings(fClasses.WriteForwardDeclarations);
    AddStrings(fInterfaces.WriteForwardDeclarations);
    AddStrings(fConstants.WriteConstants);
    AddStrings(fClasses.WritePrototypes);
    AddStrings(fVariables.WriteVariables);
    AddStrings(fPrototypedUnitMethods.WritePrototypes);
    AddStrings(fInterfaces.WritePrototypes);
  end;
end;

function TcbUnitInterfaceSection.WriteSourceCode: TStringList;
begin
  Result := inherited WriteSourceCode;

  Result := inherited WritePrototype;

  with Result do
  begin
    AddStrings(fClasses.WriteImplementations);
    AddStrings(fPrototypedUnitMethods.WriteImplementations);
    AddStrings(fUnprototypedUnitMethods.WriteImplementations);
  end;
end;

function TcbUnitInterfaceSection.AddConstant(AConstantName: String; AConstantValue: String; AConstantType: IcbType): TcbConstant;
begin
  Result := fConstants.Add(
    AConstantName,
    AConstantValue);
end;

function TcbUnitInterfaceSection.AddVariable(AVariableName: String; AVariableType: IcbType): TcbVariable;
begin
  Result := fVariables.Add(
    AVariableName,
    AVariableType);
end;

function TcbUnitInterfaceSection.AddClass(AClassTypeName: String; AHasClassForwardDeclaration: Boolean; AExtendingClass: IcbType): TcbClass;
begin
  Result := fClasses.Add(
    AClassTypeName,
    AHasClassForwardDeclaration,
    AExtendingClass);
end;

function TcbUnitInterfaceSection.AddInterface(AInterfaceName: String; AInterfaceGUID: TGUID; AHasClassForwardDeclaration: Boolean; AInterfaceExtends: IcbType
  ): TcbInterface;
begin
  Result := fInterfaces.Add(
    AInterfaceName,
    AInterfaceGUID,
    AHasClassForwardDeclaration,
    AInterfaceExtends);
end;

function TcbUnitInterfaceSection.AddPrototypedUnitMethod(AMethodName: String; AMethodType: TcbMethodType; AMethodDirectives: TcbMethodDirectives;
  AReturnType: IcbType; AParameters: TcbMethodParameterList): TcbPrototypedUnitMethod;
begin
  Result := fPrototypedUnitMethods.Add(
    AMethodName,
    AMethodType,
    AMethodDirectives,
    AReturnType,
    AParameters);
end;

function TcbUnitInterfaceSection.AddUnprototypedUnitMethod(AMethodName: String; AMethodType: TcbMethodType; AMethodDirectives: TcbMethodDirectives;
  AReturnType: IcbType; AParameterList: TcbMethodParameterList): TcbUnprototypedUnitMethod;
begin
  Result := fUnprototypedUnitMethods.Add(
    AMethodName,
    AMethodType,
    AMethodDirectives,
    AReturnType,
    AParameterList);
end;

{ TcbUnprototypedUnitMethodList }

constructor TcbUnprototypedUnitMethodList.Create(AOwnerUnit: TcbUnit);
begin
  inherited Create;

  fOwnerUnit := AOwnerUnit;
  fCodeStrings := TStringList.Create;
end;

destructor TcbUnprototypedUnitMethodList.Destroy;
var
  lMethod: TcbUnprototypedUnitMethod;
begin
  if Assigned(fCodeStrings) then
    fCodeStrings.Free;

  for lMethod in Self do
    if Assigned(lMethod) then
      lMethod.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbUnprototypedUnitMethodList.Add(AMethodName: String; AMethodType: TcbMethodType; AMethodDirectives: TcbMethodDirectives; AReturnType: IcbType;
  AParameters: TcbMethodParameterList): TcbUnprototypedUnitMethod;
var
  lMethod: TcbUnprototypedUnitMethod;
begin
  lMethod := TcbUnprototypedUnitMethod.Create(
    fOwnerUnit,
    AMethodName,
    AMethodType,
    AMethodDirectives,
    AReturnType,
    AParameters);

  inherited Add(lMethod);

  Result := lMethod;
end;

function TcbUnprototypedUnitMethodList.WriteImplementations: TStringList;
var
  lMethod: TcbUnprototypedUnitMethod;
begin
  fCodeStrings.Clear;

  for lMethod in Self do
    if Assigned(lMethod) then
      fCodeStrings.AddStrings(lMethod.WriteSourceCode);

  Result := fCodeStrings;
end;

{ TcbPrototypedUnitMethodList }

constructor TcbPrototypedUnitMethodList.Create(AOwnerUnit: TcbUnit);
begin
  inherited Create;

  fOwnerUnit := AOwnerUnit;
  fPrototypeStrings := TStringList.Create;
  fCodeStrings := TStringList.Create;
end;

destructor TcbPrototypedUnitMethodList.Destroy;
var
  lMethod: TcbPrototypedUnitMethod;
begin
  if Assigned(fPrototypeStrings) then
    fPrototypeStrings.Free;
  if Assigned(fCodeStrings) then
    fCodeStrings.Free;

  for lMethod in Self do
    if Assigned(lMethod) then
      lMethod.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbPrototypedUnitMethodList.Add(AMethodName: String; AMethodType: TcbMethodType; AMethodDirectives: TcbMethodDirectives; AReturnType: IcbType;
  AParameters: TcbMethodParameterList): TcbPrototypedUnitMethod;
var
  lMethod: TcbPrototypedUnitMethod;
begin
  lMethod := TcbPrototypedUnitMethod.Create(
    fOwnerUnit,
    AMethodName,
    AMethodType,
    AMethodDirectives,
    AReturnType,
    AParameters);

  inherited Add(lMethod);

  Result := lMethod;
end;

function TcbPrototypedUnitMethodList.WritePrototypes: TStringList;
var
  lMethod: TcbPrototypedUnitMethod;
begin
  fPrototypeStrings.Clear;

  for lMethod in Self do
    if Assigned(lMethod) then
      fPrototypeStrings.AddStrings(lMethod.WritePrototype);

  Result := fProtoTypeStrings;
end;

function TcbPrototypedUnitMethodList.WriteImplementations: TStringList;
var
  lMethod: TcbPrototypedUnitMethod;
begin
  fCodeStrings.Clear;

  for lMethod in Self do
    if Assigned(lMethod) then
      fCodeStrings.AddStrings(lMethod.WriteSourceCode);

  Result := fCodeStrings;
end;

{ TcbInterfaceMethodList }

constructor TcbInterfaceMethodList.Create(AOwnerInterface: TcbInterface);
begin
  inherited Create;

  fOwnerInterface := AOwnerInterface;
  fPrototypeCodeStrings := TStringList.Create;
end;

destructor TcbInterfaceMethodList.Destroy;
var
  lInterfaceMethod: TcbInterfaceMethod;
begin
  if Assigned(fPrototypeCodeStrings) then
    fPrototypeCodeStrings.Free;

  for lInterfaceMethod in Self do
    if Assigned(lInterfaceMethod) then
      lInterfaceMethod.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbInterfaceMethodList.Add(AMethodName: String; AMethodType: TcbMethodType; AMethodDirectives: TcbMethodDirectives; AReturnType: IcbType;
  AParameterList: TcbMethodParameterList): TcbInterfaceMethod;
var
  lInterfaceMethod: TcbinterfaceMethod;
begin
  lInterfaceMethod := TcbinterfaceMethod.Create(
    fOwnerInterface,
    AMethodName,
    AMethodType,
    AMethodDirectives,
    AReturnType,
    AParameterList);

  inherited Add(lInterfaceMethod);

  Result := lInterfaceMethod;
end;

function TcbInterfaceMethodList.WritePrototypes: TStringList;
var
  lInterfaceMethod: TcbInterfaceMethod;
begin
  fPrototypeCodeStrings.Clear;

  for lInterfaceMethod in Self do
    if Assigned(lInterfaceMethod) then
      fPrototypeCodeStrings.AddStrings(lInterfaceMethod.WritePrototype);

  Result := fPrototypeCodeStrings;
end;

{ TcbUnprototypedUnitMethod }

function TcbUnprototypedUnitMethod.WriteImplementationPrototype: TStringList;
var
  lString: String;
begin
  fProtoTypeStrings.Clear;
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write imlpementaion prototype for unit %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);
  lString := MethodTypeToString(fMethodType) + ' ';
  lString := lString + fMethodName;
  lString := lString + fParameters.WriteImplementationParameters;
  if fMethodType = mtFunction then
    lString := lString + ': ' + fReturnType.GetTypeName;
  lString := lString + ';';
  lString := lString + MethodDirectivesToString(fMethodDirectives);
  fProtoTypeStrings.Add(lString);
  Result := fProtoTypeStrings;
end;

constructor TcbUnprototypedUnitMethod.Create(AOwnerUnit: TcbUnit; AMethodName: String; AMethodType: TcbMethodType; AMethodDirectives: TcbMethodDirectives;
  AReturnType: IcbType; AParameterList: TcbMethodParameterList);
begin
  inherited Create(AOwnerUnit);

  fOwnerUnit := AOwnerUnit;
  fMethodType := AMethodType;
  fMethodName := AMethodName;
  fReturnType := AReturnType;
  fMethodDirectives := AMethodDirectives;
  fParameters := AParameterList;

  if not Assigned(fParameters) then
    fParameters := TcbMethodParameterList.Create(Self);

  fMethodCode := TcbCodeBeginEndBlock.Create(Self);
  fConstants := TcbConstantBlock.Create(Self);
  fVariables := TcbVariableList.Create(Self);
  fLeadingCommentBlock := TcbCommentBlock.Create(Self);
  fProtoTypeStrings := TStringList.Create;
end;

destructor TcbUnprototypedUnitMethod.Destroy;
begin
  if Assigned(fParameters) then
    fParameters.Free;
  if Assigned(fMethodCode) then
    fMethodCode.Free;
  if Assigned(fConstants) then
    fConstants.Free;
  if Assigned(fLeadingCommentBlock) then
    fLeadingCommentBlock.Free;
  if Assigned(fVariables) then
    fVariables.Free;
  if Assigned(fProtoTypeStrings) then
    fProtoTypeStrings.Free;
  inherited Destroy;
end;

function TcbUnprototypedUnitMethod.WriteSourceCode: TStringList;
begin
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write unprototyped unit method implementation for %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);
  Result := inherited WriteSourceCode;
  with Result do
  begin
    AddStrings(fLeadingCommentBlock.WriteSourceCode);
    AddStrings(WriteImplementationPrototype);
    AddStrings(fConstants.WriteConstants);
    AddStrings(fVariables.WriteVariables);
    AddStrings(fMethodCode.WriteSourceCode);
  end;
end;

function TcbUnprototypedUnitMethod.GetElementName: String;
begin
  Result := fMethodName;
end;

procedure TcbUnprototypedUnitMethod.SetElementName(AName: String);
begin
  fMethodName := AName;
end;

{ TcbCommentLine }

constructor TcbCommentLine.Create(AOwnerComponent: TcbComponent; AComment: String; ACommentStyle: TcbCommentStyle);
begin
  inherited Create(AOwnerComponent);

  fComment := AComment;
  fCommentStyle := ACommentStyle;
end;

destructor TcbCommentLine.Destroy;
begin
  inherited Destroy;
end;

function TcbCommentLine.WriteSourceCode: TStringList;
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
        // DONE oAPL -cClassBuilder 3: The comment string can contain linefeeds, so either clean the string, og parse it into a stringlist and preceed each line with the slashes, if that is chosen

        lCommentStrings := TStringList.Create;

        try
          lCommentStrings.Text := fComment;
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
      ccsBrackets: Add('{ ' + fComment + ' }');
      ccsParenthesis: Add('(* ' + fComment + ' *)');
    end;
  end;

  // NOTE: Depending on if the inherited assignment of Result worked, this is not needed
  // Result := fCommentLines;
end;

{ TcbConstantBlock }

constructor TcbConstantBlock.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create;
  fCodeStrings := TStringList.Create;
end;

destructor TcbConstantBlock.Destroy;
var
  lConstant: TcbConstant;
begin
  if Assigned(fCodeStrings) then
    fCodeStrings.Free;
  for lConstant in Self do
    if Assigned(lConstant) then
      lConstant.Free;
  Self.Clear;
  inherited Destroy;
end;

function TcbConstantBlock.WriteConstants: TStringList;
var
  lConstant: TcbConstant;
begin
  fCodeStrings.Clear;

  if Self.Count > 0 then
  begin
    fCodeStrings.Add('const');

    for lConstant in Self do
      if Assigned(lConstant) then
        fCodeStrings.AddStrings(lConstant.WriteSourceCode);
  end;
  Result := fCodeStrings;
end;

function TcbConstantBlock.Add(AConstantName: String; AConstantValue: String; AConstantType: IcbType): TcbConstant;
var
  lConstant: TcbConstant;
begin
  lConstant := TcbConstant.Create(
    fOwnerComponent,
    AConstantName,
    AConstantValue,
    AConstantType);
  inherited Add(lConstant);
  Result := lConstant;
end;

{ TcbConstant }

constructor TcbConstant.Create(AOwnerComponent: TcbComponent; AConstantName: String; AConstantValue: String; AConstantType: IcbType);
begin
  inherited Create(AOwnerComponent);

  fConstantName := AConstantName;
  fConstantValue := AConstantValue;
  fConstantType := AConstantType;
end;

destructor TcbConstant.Destroy;
begin
  inherited Destroy;
end;

function TcbConstant.WriteSourceCode: TStringList;
var
  lString: String;
begin
  Result := inherited WriteSourceCode;
  lString := fConstantName;
  if Assigned(fConstantType) then
    lString := lString + ': ' + fConstantType.GetTypeName;
  lString := lString + ' = ' + fConstantValue;
  Result.Add(lString);
end;

function TcbConstant.GetElementName: String;
begin
  Result := fConstantName;
end;

procedure TcbConstant.SetElementName(AName: String);
begin
  fConstantName := AName;
end;

{ TcbPrototypedUnitMethod }

function TcbPrototypedUnitMethod.WriteImplementationPrototype: TStringList;
var
  lString: String;
begin
  fImplementationProtoTypeStrings.Clear;
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write imlpementaion prototype for unit %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);
  lString := MethodTypeToString(fMethodType) + ' ';
  lString := lString + fMethodName;
  lString := lString + fParameters.WriteImplementationParameters;
  if fMethodType = mtFunction then
    lString := lString + ': ' + fReturnType.GetTypeName;
  lString := lString + ';';
  lString := lString + MethodDirectivesToString(fMethodDirectives);
  fImplementationProtoTypeStrings.Add(lString);
  Result := fImplementationProtoTypeStrings;
end;

constructor TcbPrototypedUnitMethod.Create(AOwnerUnit: TcbUnit; AMethodName: String; AMethodType: TcbMethodType; AMethodDirectives: TcbMethodDirectives;
  AReturnType: IcbType; AParameters: TcbMethodParameterList);
begin
  inherited Create(AOwnerUnit);

  fOwnerUnit := AOwnerUnit;
  fMethodType := AMethodType;
  fMethodName := AMethodName;
  fReturnType := AReturnType;
  fMethodDirectives := AMethodDirectives;
  fParameters := AParameters;

  if not Assigned(fParameters) then
    fParameters := TcbMethodParameterList.Create(Self);

  fMethodCode := TcbCodeBeginEndBlock.Create(Self);
  fConstants := TcbConstantBlock.Create(Self);
  fVariables := TcbVariableList.Create(Self);
  fLeadingCommentBlock := TcbCommentBlock.Create(Self);
  fImplementationProtoTypeStrings := TStringList.Create;
end;

destructor TcbPrototypedUnitMethod.Destroy;
begin
  if Assigned(fImplementationProtoTypeStrings) then
    fImplementationProtoTypeStrings.Free;
  if Assigned(fParameters) then
    fParameters.Free;
  if Assigned(fMethodCode) then
    fMethodCode.Free;
  if Assigned(fConstants) then
    fConstants.Free;
  if Assigned(fLeadingCommentBlock) then
    fLeadingCommentBlock.Free;
  if Assigned(fVariables) then
    fVariables.Free;
  inherited Destroy;
end;

function TcbPrototypedUnitMethod.WritePrototype: TStringList;
var
  lString: String;
begin
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write prototyped unit method prototype for %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);
  Result := inherited WritePrototype;
  lString := MethodTypeToString(fMethodType) + ' ';
  lString := lString + fMethodName;
  lString := lString + fParameters.WriteParameters;
  if fMethodType = mtFunction then
    lString := lString + ': ' + fReturnType.GetTypeName;
  lString := lString + ';';
  lString := lString + MethodDirectivesToString(fMethodDirectives);
  Result.Add(lString);
end;

function TcbPrototypedUnitMethod.WriteSourceCode: TStringList;
begin
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write prototyped unit method implementation for %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);
  Result := inherited WriteSourceCode;
  with Result do
  begin
    AddStrings(fLeadingCommentBlock.WriteSourceCode);
    AddStrings(WriteImplementationPrototype);
    AddStrings(fConstants.WriteConstants);
    AddStrings(fVariables.WriteVariables);
    AddStrings(fMethodCode.WriteSourceCode);
  end;
end;

function TcbPrototypedUnitMethod.GetElementName: String;
begin
  Result := fMethodName;
end;

procedure TcbPrototypedUnitMethod.SetElementName(AName: String);
begin
  fMethodName := AName;
end;

{ TcbInterfaceMethod }

constructor TcbInterfaceMethod.Create(AOwnerInterface: TcbInterface; AMethodName: String; AMethodType: TcbMethodType; AMethodDirectives: TcbMethodDirectives;
  AReturnType: IcbType; AParameters: TcbMethodParameterList);
begin
  inherited Create(AOwnerInterface);
  fOwnerInterface := AOwnerInterface;
  fMethodType := AMethodType;
  fMethodName := AMethodName;
  fReturnType := AReturnType;
  fMethodDirectives := AMethodDirectives;
  fParameters := AParameters;
  if not Assigned(fParameters) then
    fParameters := TcbMethodParameterList.Create(Self);
end;

destructor TcbInterfaceMethod.Destroy;
begin
  if Assigned(fParameters) then
    fParameters.Free;
  inherited Destroy;
end;

function TcbInterfaceMethod.WritePrototype: TStringList;
var
  lString: String;
begin
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write interface %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);

  // Get the inherited StringList
  Result := inherited WritePrototype;

  // fMethodType: TcbMethodType;
  lString := MethodTypeToString(fMethodType) + ' ';
  // fMethodName: String;
  lString := lString + fMethodName;
  // fParameterList: TcbMethodParameterList;
  lString := lString + fParameters.WriteParameters;

  if fMethodType = mtFunction then
    lString := lString + ': ' + fReturnType.GetTypeName;

  lString := lString + ';';

  lString := lString + MethodDirectivesToString(fMethodDirectives);

  Result.Add(lString);
end;

function TcbInterfaceMethod.GetElementName: String;
begin
  Result := fMethodName;
end;

procedure TcbInterfaceMethod.SetElementName(AName: String);
begin
  fMethodName := AName;
end;

function TcbInterfaceMethod.GetOwnerInterface: TcbInterface;
begin
  Result := fOwnerInterface;
end;

{ TcbCodeBlock }

constructor TcbCodeBlock.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create;
  fCodeStrings := TStringList.Create;
end;

destructor TcbCodeBlock.Destroy;
var
  lCodeComponent: TcbSourceCodeComponent;
begin
  if Assigned(fCodeStrings) then
    fCodeStrings.Free;

  for lCodeComponent in Self do
    if Assigned(lCodeComponent) then
      lCodeComponent.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbCodeBlock.WriteSourceCode: TStringList;
var
  lCodeLine: TcbCodeLine;
begin
  fCodeStrings.Clear;

  for lCodeLine in Self do
    fCodeStrings.AddStrings(lCodeLine.WriteSourceCode);

  Result := fCodeStrings;
end;

function TcbCodeBlock.Add(ACodeLine: String; AComment: TcbCommentLine): TcbCodeLine;
var
  lCodeLine: TcbCodeLine;
begin
  lCodeLine := TcbCodeLine.Create(
    fOwnerComponent,
    ACodeLine);
  if Assigned(AComment) then
    lCodeLine.Comments.Add(AComment);
  inherited Add(lCodeLine);
  Result := lCodeLine;
end;

function TcbCodeBlock.Add(ACodeLine: String; AComment: String; ACommentStyle: TcbCommentStyle): TcbCodeLine;
var
  lCodeLine: TcbCodeLine;
begin
  lCodeLine := TcbCodeLine.Create(
    fOwnerComponent,
    ACodeLine);
  if AComment <> '' then
    lCodeLine.Comments.Add(AComment, ACommentStyle);
  inherited Add(lCodeLine);
  Result := lCodeLine;
end;

{ TcbPrototypedSourceCodeComponent }

constructor TcbPrototypedSourceCodeComponent.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);
  fCodeStrings := TStringList.Create;
end;

destructor TcbPrototypedSourceCodeComponent.Destroy;
begin
  if Assigned(fCodeStrings) then
    fCodeStrings.Free;

  inherited Destroy;
end;

function TcbPrototypedSourceCodeComponent.WriteSourceCode: TStringList;
begin
  fCodeStrings.Clear;
  Result := fCodeStrings;
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
  fPrototypeStrings := TStringList.Create;
end;

destructor TcbPrototypedCodeComponent.Destroy;
begin
  if Assigned(fPrototypeStrings) then
    fPrototypeStrings.Free;
  inherited Destroy;
end;

function TcbPrototypedCodeComponent.WritePrototype: TStringList;
begin
  fPrototypeStrings.Clear;
  Result := fPrototypeStrings;
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

{ TcbCodeMethodBody }

constructor TcbCodeMethodBody.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);
end;

destructor TcbCodeMethodBody.Destroy;
begin
  inherited Destroy;
end;

{ TcbCommentBlock }

constructor TcbCommentBlock.Create(AOwnerComponent: TcbComponent; AComment: String; ACommentStyle: TcbCommentStyle);
begin
  inherited Create();

  fOwnerComponent := AOwnerComponent;

  fCodeStrings := TStringList.Create;

  if AComment <> '' then
    inherited Add(
      TcbCommentLine.Create(
        fOwnerComponent,
        AComment,
        ACommentStyle));
end;

destructor TcbCommentBlock.Destroy;
var
  lSourceComment: TcbCommentLine;
begin
  if Assigned(fCodeStrings) then
    fCodeStrings.Free;

  for lSourceComment in Self do
    if Assigned(lSourceComment) then
      lSourceComment.Free;

  Self.Clear;

  inherited Destroy;
end;

function TcbCommentBlock.Add(AComment: String; ACommentStyle: TcbCommentStyle): TcbCommentLine;
var
  lComment: TcbCommentLine;
begin
  lComment := TcbCommentLine.Create(
    fOwnerComponent,
    AComment,
    ACommentStyle);

  inherited Add(lComment);

  Result := lComment;
end;

function TcbCommentBlock.WriteSourceCode: TStringList;
var
  lSourceComment: TcbCommentLine;
begin
  fCodeStrings.Clear;
  for lSourceComment in Self do
    if Assigned(lSourceComment) then
      fCodeStrings.AddStrings(lSourceComment.WriteSourceCode);
  Result := fCodeStrings;
end;

{ TcbCodeBeginEndBlock }

constructor TcbCodeBeginEndBlock.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);
end;

destructor TcbCodeBeginEndBlock.Destroy;
begin
  inherited Destroy;
end;

function TcbCodeBeginEndBlock.WriteSourceCode: TStringList;
begin
  Result := inherited WriteSourceCode;
  Result.Insert(0, 'begin');
  Result.Add('end;');
end;

{ TcbSourceCodeComponent }

constructor TcbSourceCodeComponent.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);

  fCodeStrings := TStringList.Create;
end;

destructor TcbSourceCodeComponent.Destroy;
begin
  if Assigned(fCodeStrings) then
    fCodeStrings.Free;

  inherited Destroy;
end;

function TcbSourceCodeComponent.WriteSourceCode: TStringList;
begin
  fCodeStrings.Clear;
  Result := fCodeStrings;
end;

{ TcbCodeLine }

constructor TcbCodeLine.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create(AOwnerComponent);
  fCodeLine := '';
  fComments := TcbCommentBlock.Create(Self);
end;

constructor TcbCodeLine.Create(AOwnerComponent: TcbComponent; ACodeLine: String; ACommentBlock: TcbCommentBlock);
begin
  inherited Create(AOwnerComponent);
  fCodeLine := ACodeLine;
  fComments := ACommentBlock;
  if not Assigned(fComments) then
    fComments := TcbCommentBlock.Create(Self);
end;

destructor TcbCodeLine.Destroy;
begin
  if Assigned(fComments) then
    fComments.Free;
  inherited Destroy;
end;

function TcbCodeLine.WriteSourceCode: TStringList;
var
  lComment: TcbCommentLine;
begin
  Result := inherited WriteSourceCode;
  // TODO -oAPL -cClassBuilder 5: Introduce a variable to control if the comment should be writted before or after the codeline

  with Result do
  begin
    // Write the comment, if any
    if Assigned(fComments) then
      for lComment in fComments do
        if Assigned(lComment) then
          AddStrings(lComment.WriteSourceCode);

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
    if lInterface.HasForwardDeclaration then
      fPrototypeCode.AddStrings(lInterface.WritePrototype);

  Result := fPrototypeCode;
end;

function TcbInterfaceList.Add(AInterfaceName: String; AInterfaceGUID: TGUID; AHasForwardDeclaration: Boolean; AInterfaceExtends: IcbType): TcbInterface;
var
  lInterface: TcbInterface;
begin
  lInterface := TcbInterface.Create(
    fOwnerUnit,
    AInterfaceName,
    AInterfaceGUID,
    AHasForwardDeclaration,
    AInterfaceExtends);

  inherited Add(lInterface);

  Result := lInterface;
end;

constructor TcbInterface.Create(AOwnerUnit: TcbUnit; AInterfaceName: String; AInterfaceGUID: TGUID; AHasForwardDeclaration: Boolean; AInterfaceExtends: IcbType
  );
begin
  inherited Create(AOwnerUnit);

  fOwnerUnit := AOwnerUnit;
  fInterfaceName := AInterfaceName;
  fInterfaceExtends := AInterfaceExtends;
  fHasForwardDeclaration := AHasForwardDeclaration;

  if GUIDToString(AInterfaceGUID) = GUIDToString(LO_EMPTY_GUID) then
    CreateGUID(fInterfaceGUID);

  fMethods := TcbInterfaceMethodList.Create(Self);
  fProperties := TcbInterfacePropertyList.Create(Self);
end;

destructor TcbInterface.Destroy;
begin
  if Assigned(fMethods) then
    fMethods.Free;
  if Assigned(fProperties) then
    fProperties.Free;

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
    (fMethods.Count > 0)
    or (fProperties.Count > 0);

  with Result do
  begin
    lInterfaceDeclaration := fInterfaceName + ' = interface';

    if Assigned(fInterfaceExtends) then
      lInterfaceDeclaration := lInterfaceDeclaration + '(' + fInterfaceExtends.GetTypeName + ')';

    if not lHasElements then
      lInterfaceDeclaration := lInterfaceDeclaration + ';';

    Add(lInterfaceDeclaration);

    AddStrings(fMethods.WritePrototypes);
    AddStrings(fProperties.WriteProperties);

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

function TcbInterfacePropertyList.Add(APropertyName: String; APropertyType: IcbType; APropertyReadElement: IcbInterfaceElement;
  APropertyWriteElement: IcbInterfaceElement): TcbInterfaceProperty;
var
  lInterfaceProperty: TcbInterfaceProperty;
begin
  lInterfaceProperty := TcbInterfaceProperty.Create(
    fOwnerInterface,
    APropertyName,
    APropertyType,
    APropertyReadElement,
    APropertyWriteElement);

  inherited Add(lInterfaceProperty);

  Result := lInterfaceProperty;
end;

{ TcbInterfaceProperty }

constructor TcbInterfaceProperty.Create(AOwner: TcbInterface; APropertyName: String; APropertyType: IcbType; APropertyReadElement: IcbInterfaceElement;
  APropertyWriteElement: IcbInterfaceElement);
begin
  inherited Create(AOwner);

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

function TcbClassList.Add(AClassTypeName: String; AHasClassForwardDeclaration: Boolean; AExtendingClass: IcbType): TcbClass;
var
  lClass: TcbClass;
begin
  lClass := TcbClass.Create(
    fOwnerUnit,
    AClassTypeName,
    AHasClassForwardDeclaration,
    AExtendingClass);

  inherited Add(lClass);

  Result := lClass;
end;

{ TcbClassPropertyList }

constructor TcbClassPropertyList.Create(AOwnerClass: TcbClass);
begin
  inherited Create;

  fOwnerClass := AOwnerClass;

  fPropertyStrings[csDefault] := TStringList.Create;
  fPropertyStrings[csPrivate] := TStringList.Create;
  fPropertyStrings[csProtected] := TStringList.Create;
  fPropertyStrings[csPublic] := TStringList.Create;
  fPropertyStrings[csPublished] := TStringList.Create;
end;

destructor TcbClassPropertyList.Destroy;
var
  lClassProperty: TcbClassProperty;
begin
  if Assigned(fPropertyStrings[csDefault]) then
    fPropertyStrings[csDefault].Free;
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

function TcbClassPropertyList.Add(APropertyName: String; APropertyType: IcbType; AClassSection: TcbClassSection; APropertyReadElement: IcbClassElement;
  APropertyWriteElement: IcbClassElement): TcbClassProperty;
var
  lClassProperty: TcbClassProperty;
begin
  lClassProperty := TcbClassProperty.Create(
    fOwnerClass,
    APropertyName,
    APropertyType,
    AClassSection,
    APropertyReadElement,
    APropertyWriteElement);

  inherited Add(lClassProperty);

  Result := lClassProperty;
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
  fVariableStrings[csDefault] := TStringList.Create;
  fVariableStrings[csPrivate] := TStringList.Create;
  fVariableStrings[csProtected] := TStringList.Create;
  fVariableStrings[csPublic] := TStringList.Create;
  fVariableStrings[csPublished] := TStringList.Create;
end;

destructor TcbClassVariableList.Destroy;
var
  lClassVariable: TcbClassVariable;
begin
  if Assigned(fVariableStrings[csDefault]) then
    fVariableStrings[csDefault].Free;
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

function TcbClassVariableList.Add(AVariableName: String; AVariableType: IcbType; AClassSection: TcbClassSection): TcbClassVariable;
var
  lClassVariable: TcbClassVariable;
begin
  lClassVariable := TcbClassVariable.Create(
    fOwnerClass,
    AVariableName,
    AVariableType,
    AClassSection);

  inherited Add(lClassVariable);

  Result := lClassVariable;
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

function TcbVariableList.Add(AVariableName: String; AVariableType: IcbType): TcbVariable;
var
  lVariable: TcbVariable;
begin
  lVariable := TcbVariable.Create(
    fOwnerComponent,
    AVariableName,
    AVariableType);

  inherited Add(lVariable);

  Result := lVariable;
end;

function TcbVariableList.WriteVariables: TStringList;
var
  lVariable: TcbVariable;
begin
  fVariableStrings.Clear;

  if Self.Count > 0 then
  begin
    fVariableStrings.Add('var');

    for lVariable in Self do
      if Assigned(lVariable) then
        fVariableStrings.AddStrings(lVariable.WriteSourceCode);
  end;

  Result := fVariableStrings;
end;

{ TcbClassMethodList }

constructor TcbClassMethodList.Create(AOwnerClass: TcbClass);
begin
  inherited Create;

  fOwnerClass := AOwnerClass;

  fPrototypeCodeStrings[csDefault] := TStringList.Create;
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
  if Assigned(fPrototypeCodeStrings[csDefault]) then
    fPrototypeCodeStrings[csDefault].Free;
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

function TcbClassMethodList.Add(AMethodName: String; AMethodType: TcbMethodType; AClassSection: TcbClassSection; AMethodDirectives: TcbMethodDirectives;
  AReturnType: IcbType; AParameterList: TcbMethodParameterList): TcbClassMethod;
var
  lClassMethod: TcbClassMethod;
begin
  lClassMethod := TcbClassMethod.Create(
    fOwnerClass,
    AMethodName,
    AMethodType,
    AClassSection,
    AMethodDirectives,
    AReturnType,
    AParameterList);

  inherited Add(lClassMethod);

  Result := lClassMethod;
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

constructor TcbMethodParameterList.Create(AOwnerComponent: TcbComponent);
begin
  inherited Create;

  fOwnerComponent := AOwnerComponent;
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
    begin
      Result := Result + lMethodParameter.WriteParameter;
      if lMethodParameter <> Self.Last then
        Result := Result + ';';
    end;

  for lMethodParameter in Self do
    if lMethodParameter.ParameterDefault <> '' then
    begin
      Result := Result + lMethodParameter.WriteParameter;
      if lMethodParameter <> Self.Last then
        Result := Result + ';';
    end;

  if Count > 0 then
    Result := ')';
end;

function TcbMethodParameterList.WriteImplementationParameters: String;
var
  lMethodParameter: TcbMethodParameter;
begin
  Result := '';

  if Count > 0 then
    Result := '(';

  for lMethodParameter in Self do
    if lMethodParameter.ParameterDefault = '' then
    begin
      Result := Result + lMethodParameter.WriteImlementationParameter;
      if lMethodParameter <> Self.Last then
        Result := Result + ';';
    end;

  for lMethodParameter in Self do
    if lMethodParameter.ParameterDefault <> '' then
    begin
      Result := Result + lMethodParameter.WriteImlementationParameter;
      if lMethodParameter <> Self.Last then
        Result := Result + ';';
    end;

  if Count > 0 then
    Result := ')';
end;

function TcbMethodParameterList.Add(AParameterName: String; AParameterType: IcbType; AParameterModifier: TcbMethodParameterModifier; AParameterDefault: String
  ): TcbMethodParameter;
var
  lParameter: TcbMethodParameter;
begin
  lParameter := TcbMethodParameter.Create(
    fOwnerComponent,
    AParameterName,
    AParameterType,
    AParameterModifier,
    AParameterDefault);

  inherited Add(lParameter);

  Result := lParameter;
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
(*var
  lIndex: Integer;
  lUnitMethod: TcbMethod;
  lClass: TcbClass;
  lVariable: TcbVariable;
  lInterface: TcbInterface;*)
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

  fChildComponents := TcbComponentList.Create(Self);

  if Assigned(fOwnerComponent) then
    fOwnerComponent.RegisterChild(Self);

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
  if Assigned(fChildComponents) then
  begin
    fChildComponents.Clear;
    fChildComponents.Free;
  end;

  inherited;
end;

procedure TcbComponent.SetLogger(ALogger: TLogger);
begin
  fLogger := ALogger;
end;

function TcbComponent.GetLogger: TLogger;
begin
  {if not Assigned(fLogger) then
  begin
    // TODO -oAPL -cClassBuilder 4: Hmm, perhaps it's not a good idea to create a logger here. The unit user should retain control of what is created
    TLoggerUnit.initialize();
    fLogger := TLogger.GetInstance();
  end;}
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

procedure TcbComponent.RegisterChild(AComponent: TcbComponent);
begin
  fChildComponents.Add(AComponent);
end;

{ TcbUnit }

constructor TcbUnit.Create(AOwner: TcbComponent; AUnitName: String);
begin
  inherited Create(AOwner);

  fUnitName := AUnitName;
  fUnitTopCommentBlock := TcbCommentBlock.Create(Self);
  fUnitTopCompilerDirectives := TStringList.Create;
  fUnitInterfaceUsesList := TStringList.Create;
  fUnitImplementationUsesList := TStringList.Create;
  fUnitInitialization := TcbCodeBlock.Create(Self);
  fUnitFinalization := TcbCodeBlock.Create(Self);
  fUnitSections := TcbUnitInterfaceSectionList.Create(Self);
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
  if Assigned(fUnitInitialization) then
    fUnitInitialization.Free;
  if Assigned(fUnitFinalization) then
    fUnitFinalization.Free;
  if Assigned(fUnitSections) then
    fUnitSections.Free;

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

function TcbUnit.WriteSourceCode: TStringList;
begin
  Result := inherited WriteSourceCode;

  with Result do
  begin
    AddStrings(fUnitTopCommentBlock.WriteSourceCode);
    Add('unit ' + fUnitName + ';');
    AddStrings(fUnitTopCompilerDirectives);
    Add('interface');
    if fUnitInterfaceUsesList.Count > 0 then
    begin
      Add('uses');
      AddStrings(fUnitInterfaceUsesList);
    end;

    AddStrings(fUnitSections.WritePrototypeCode);

    Add('implementation');

    if fUnitImplementationUsesList.Count > 0 then
    begin
      Add('uses');
      AddStrings(fUnitImplementationUsesList);
    end;

    AddStrings(fUnitSections.WriteImplementationCode);

    // fUnitInitialization: TcbCodeBlock;
    // fUnitFinalization: TcbCodeBlock;

    Add('end.');
  end;
end;

{ TcbVariable }

constructor TcbVariable.Create(AOwnerComponent: TcbComponent; AVariableName: String; AVariableType: IcbType);
begin
  inherited Create(AOwnerComponent);

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

{ TcbMethodParameter }

constructor TcbMethodParameter.Create(AOwnerComponent: TcbComponent; AParameterName: String; AParameterType: IcbType;
  AParameterModifier: TcbMethodParameterModifier; AParameterDefault: String);
begin
  inherited Create(AOwnerComponent);

  fParameterName := AParameterName;
  fParameterType := AParameterType;
  fParameterDefault := AParameterDefault;
  fParameterModifier := AParameterModifier;
end;

destructor TcbMethodParameter.Destroy;
begin
  inherited Destroy;
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

function TcbMethodParameter.WriteImlementationParameter: String;
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
end;

function TcbClass.HasElementsInSection(ASection: TcbClassSection): Boolean;
begin
  Result :=
    fVariables.HasElementsInSection(ASection)
    or fProperties.HasElementsInSection(ASection)
    or fMethods.HasElementsInSection(ASection);
end;

function TcbClass.WritePrototypeSection(ASection: TcbClassSection): TStringList;
begin
  fPrototypeClassSections[ASection].Clear;

  with fPrototypeClassSections[ASection] do
  begin
    AddStrings(fVariables.WriteVariables(ASection));
    AddStrings(fMethods.WritePrototypes(ASection));
    AddStrings(fProperties.WriteProperties(ASection));
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
  fVariables := TcbClassVariableList.Create(Self);
  fMethods := TcbClassMethodList.Create(Self);
  fProperties := TcbClassPropertyList.Create(Self);
  fClassElements := TcbClassElementList.Create(Self);

  fPrototypeClassSections[csPrivate] := TStringList.Create;
  fPrototypeClassSections[csDefault] := TStringList.Create;
  fPrototypeClassSections[csProtected] := TStringList.Create;
  fPrototypeClassSections[csPublic] := TStringList.Create;
  fPrototypeClassSections[csPublished] := TStringList.Create;
end;

destructor TcbClass.Destroy;
begin
  if Assigned(fImplementingInterfaces) then
    fImplementingInterfaces.Free;
  if Assigned(fVariables) then
    fVariables.Free;
  if Assigned(fMethods) then
    fMethods.Free;
  if Assigned(fProperties) then
    fProperties.Free;
  if Assigned(fClassElements) then
    fClassElements.Free;

  if Assigned(fPrototypeClassSections[csPrivate]) then
    fPrototypeClassSections[csPrivate].Free;
  if Assigned(fPrototypeClassSections[csDefault]) then
    fPrototypeClassSections[csDefault].Free;
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
    (fVariables.Count > 0)
    or (fMethods.Count > 0)
    or (fProperties.Count > 0)
    or (fClassElements.Count > 0);

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
    AddStrings(WritePrototypeSection(csDefault));

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
  Result.AddStrings(fMethods.WriteImplementations);
end;

function TcbClass.WriteForwardDeclaration: String;
begin
  Result := fClassTypeName + ' = class;';
end;

function TcbClass.AddVariable(AVariableName: String; AVariableType: IcbType; AClassSection: TcbClassSection): TcbClassVariable;
begin
  Result := fVariables.Add(
    AVariableName,
    AVariableType,
    AClassSection);
end;

function TcbClass.AddMethod(AMethodName: String; AMethodType: TcbMethodType; AClassSection: TcbClassSection; AMethodDirectives: TcbMethodDirectives;
  AReturnType: IcbType; AParameters: TcbMethodParameterList): TcbClassMethod;
begin
  Result := fMethods.Add(
    AMethodName,
    AMethodType,
    AClassSection,
    AMethodDirectives,
    AReturnType,
    AParameters);
end;

function TcbClass.AddProperty(APropertyName: String; APropertyType: IcbType; AClassSection: TcbClassSection; APropertyReadElement: IcbClassElement;
  APropertyWriteElement: IcbClassElement): TcbClassProperty;
begin
  Result := fProperties.Add(
    APropertyName,
    APropertyType,
    AClassSection,
    APropertyReadElement,
    APropertyWriteElement);
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
  APropertyReadElement: IcbClassElement; APropertyWriteElement: IcbClassElement);
begin
  inherited Create(AOwnerClass);

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
  lString := lString + fParameters.WriteImplementationParameters;
  // Write returntype, if this is a function
  if fMethodType = mtFunction then
    lString := lString + ': ' + fReturnType.GetTypeName;

  // End the header
  lString := lString + ';';

  // Write method directives
  lString := lString + MethodDirectivesToString(fMethodDirectives);

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

constructor TcbClassMethod.Create(AOwnerClass: TcbClass; AMethodName: String; AMethodType: TcbMethodType; AClassSection: TcbClassSection;
  AMethodDirectives: TcbMethodDirectives; AReturnType: IcbType; AParameters: TcbMethodParameterList);
begin
  inherited Create(AOwnerClass);

  fOwnerClass := AOwnerClass;
  fMethodName := AMethodName;
  fMethodType := AMethodType;
  fMethodDirectives := AMethodDirectives;
  fReturnType := AReturnType;
  fClassSection := AClassSection;
  fParameters := AParameters;

  if not Assigned(fParameters) then
    fParameters := TcbMethodParameterList.Create(Self);

  fImplementationPrototypeCode := TStringList.Create;
  fMethodCode := TcbCodeBeginEndBlock.Create(Self);
  fConstants := TcbConstantBlock.Create(Self);
  fVariables := TcbVariableList.Create(Self);
  fLeadingCommentBlock := TcbCommentBlock.Create(Self);
end;

destructor TcbClassMethod.Destroy;
begin
  if Assigned(fImplementationPrototypeCode) then
    fImplementationPrototypeCode.Free;
  if Assigned(fParameters) then
    fParameters.Free;
  if Assigned(fMethodCode) then
    fMethodCode.Free;
  if Assigned(fConstants) then
    fConstants.Free;
  if Assigned(fVariables) then
    fVariables.Free;
  if Assigned(fLeadingCommentBlock) then
    fLeadingCommentBlock.Free;

  inherited Destroy;
end;

function TcbClassMethod.WritePrototype: TStringList;
var
  lString: String;
begin
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write class method prototype for %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);

  Result := inherited WritePrototype;

  lString := '';

  // Write method type
  lString := MethodTypeToString(fMethodType) + ' ';
  // Write methodname
  lString := lString + fMethodName;
  // Write parameters
  lString := lString + fParameters.WriteParameters;
  // Write returntype, if this is a function
  if fMethodType = mtFunction then
    lString := lString + ': ' + fReturnType.GetTypeName;

  // End the header
  lString := lString + ';';

  // Write method directives
  lString := lString + MethodDirectivesToString(fMethodDirectives);

  Result.Add(lString);
end;

function TcbClassMethod.WriteSourceCode: TStringList;
begin
  if (fMethodType = mtFunction) and (not Assigned(fReturnType)) then
    raise TcbNilReferenceException.CreateFmt('Cannot write class method implementation for %s %s. ReturnType is not assigned.', [MethodTypeToString(fMethodType), fMethodName]);

  Result := inherited WriteSourceCode;

  with Result do
  begin
    AddStrings(fLeadingCommentBlock.WriteSourceCode);
    AddStrings(WriteImplementationPrototype);
    AddStrings(fConstants.WriteConstants);
    AddStrings(fVariables.WriteVariables);
    AddStrings(fMethodCode.WriteSourceCode);
  end;
end;

function TcbClassMethod.GetElementName: String;
begin
  Result := fMethodName;
end;

procedure TcbClassMethod.SetElementName(AName: String);
begin
  fMethodName := AName;
end;

end.

initialization

finalization
