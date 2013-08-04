(*
  2013-08-03: APL: This unit is an attempt to remake the classbuilder code, in a more discrete manner, with more
                   usage of inheritence. Mainly to enable comments on any code element. It does not seem to be
                   entirely possible, when also wanting to keep a high level of object-irisation of the elements
                   in the code.
*)

unit ucbCodeBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs;

type
  TcbCommentStyle = (
    cb_ccsSlashes,
    cb_ccsBrackets,
    cb_ccsParenthesis);

const
  cb_ccsDefault = cb_ccsSlashes;

type
  TcbComponent = class;
  TcbComponentList = class;

  { TcbComponent }

  TcbComponent = class(TInterfacedPersistent)
  private
    fOwnerComponent: TcbComponent;
    fChildComponents: TcbComponentList;
  public
    constructor Create(aOwnerComponent: TcbComponent);
    destructor Destroy; override;
    procedure RegisterChild(aComponent: TcbComponent); virtual;
  published
    property OwnerComponent: TcbComponent read fOwnerComponent;
    property ChildComponents: TcbComponentList read fChildComponents;
  end;

  { TcbComponentList }

  TcbComponentList = class(TInterfacedPersistent)
  private
    fOwnerComponent: TcbComponent;
    fComponentList: TFPObjectList;
    function GetCount: Integer;
  public
    type

      { TcbComponentListEnumerator }

      TcbComponentListEnumerator = class(TInterfacedObject)
      private
        fComponentList: TcbComponentList;
        fCurrenIndex: Integer;
      public
        constructor Create(aComponentList: TcbComponentList);
        destructor Destroy; override;
        function GetCurrent: TcbComponent;
        function MoveNext: Boolean;
        property Current: TcbComponent read GetCurrent;
      end;

    constructor Create(aOwnerComponent: TcbComponent);
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(aComponent: TcbComponent): Integer; virtual;
    function IsEmpty: Boolean;
    procedure Add(aComponent: TcbComponent); virtual;
    function Get(aIndex: Integer): TcbComponent; virtual;
    procedure Insert(aIndex: Integer; aComponent: TcbComponent); virtual;
    function Remove(aComponent: TcbComponent): Boolean; virtual;
    procedure Pack;
    function First: TcbComponent; virtual;
    function Last: TcbComponent; virtual;
    property Count: Integer read GetCount;
    function GetEnumerator: TcbComponentListEnumerator;
  published
    property OwnerComponent: TcbComponent read fOwnerComponent write fOwnerComponent;
  end;

  { TcbPrintable }

  TcbPrintable = class abstract(TcbComponent)
  public
    constructor Create(aOwnerComponent: TcbComponent);
    destructor Destroy; override;
    function Print: String; virtual; abstract;
  end;

  { TcbTextElement }

  TcbTextElement = class(TcbPrintable)
  private
    fText: String;
    function GetText: String;
    procedure SetText(aText: String);
  public
    constructor Create(aOwnerComponent: TcbComponent; aText: String);
    destructor Destroy; override;
    function Print: String; override;
  published
    property Text: String read GetText write SetText;
  end;

  { TcbComment }

  TcbComment = class(TcbTextElement)
  private
    fCommentStyle: TcbCommentStyle;
  public
    constructor Create(aOwnerComponent: TcbComponent; aComment: String; aCommentStyle: TcbCommentStyle = cb_ccsDefault);
    destructor Destroy; override;
    function Print: String; override;
    procedure Add(aComment: String);
  end;

  { TcbCodeElement }

  TcbCodeElement = class(TcbTextElement)
  private
    fComment: TcbComment;
  public
    constructor Create(aOwnerComponent: TcbComponent; aCodeElement: String; aComment: String = ''; aCommentStyle: TcbCommentStyle = cb_ccsDefault);
    destructor Destroy; override;
    function Print: String; override;
  published
    property Comment: TcbComment read fComment write fComment;
  end;

implementation

{ TcbCodeElement }

constructor TcbCodeElement.Create(aOwnerComponent: TcbComponent; aCodeElement: String; aComment: String; aCommentStyle: TcbCommentStyle);
begin
  inherited Create(aOwnerComponent, aCodeElement);

  if aComment <> '' then
    fComment := TcbComment.Create(Self, aComment, aCommentStyle);
end;

destructor TcbCodeElement.Destroy;
begin
  // if Assigned(fComment) then
    // fComment.Free;

  inherited Destroy;
end;

function TcbCodeElement.Print: String;
begin
  if Assigned(fComment) then
    Result := fComment.Print + fText
  else
    Result := fText;
end;

{ TcbComponentList.TcbComponentListEnumerator }

constructor TcbComponentList.TcbComponentListEnumerator.Create(aComponentList: TcbComponentList);
begin
  fComponentList := aComponentList;
  fCurrenIndex := -1;
end;

destructor TcbComponentList.TcbComponentListEnumerator.Destroy;
begin
  inherited Destroy;
end;

function TcbComponentList.TcbComponentListEnumerator.GetCurrent: TcbComponent;
begin
  Result := fComponentList.Get(fCurrenIndex);
end;

function TcbComponentList.TcbComponentListEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fCurrenIndex = -1 then
    fCurrenIndex := fComponentList.IndexOf(fComponentList.First)
  else
    Inc(fCurrenIndex);

  Result := fCurrenIndex < fComponentList.Count;
end;

{ TcbComponentList }

function TcbComponentList.GetCount: Integer;
begin
  Result := fComponentList.Count;
end;

constructor TcbComponentList.Create(aOwnerComponent: TcbComponent);
begin
  inherited Create;
  fOwnerComponent := aOwnerComponent;
  fComponentList := TFPObjectList.Create(False);
end;

destructor TcbComponentList.Destroy;
var
  lComponent: TcbComponent;
begin
  for lComponent in Self do
    if Assigned(lComponent) then
      lComponent.Free;

  Self.Clear;

  if Assigned(fComponentList) then
    fComponentList.Free;

  inherited Destroy;
end;

procedure TcbComponentList.Clear;
begin
  fComponentList.Clear;
end;

function TcbComponentList.IndexOf(aComponent: TcbComponent): Integer;
begin
  Result := fComponentList.IndexOf(aComponent);
end;

function TcbComponentList.IsEmpty: Boolean;
begin
  Result := fComponentList.Count = 0;
end;

procedure TcbComponentList.Add(aComponent: TcbComponent);
begin
  fComponentList.Add(aComponent);
end;

function TcbComponentList.Get(aIndex: Integer): TcbComponent;
begin
  if ((aIndex > fComponentList.Count) or (aIndex < 0)) then
    Result := nil
  else
    Result := TcbComponent(fComponentList.Items[aIndex]);
end;

procedure TcbComponentList.Insert(aIndex: Integer; aComponent: TcbComponent);
begin
  fComponentList.Insert(aIndex, aComponent);
end;

function TcbComponentList.Remove(aComponent: TcbComponent): Boolean;
begin
  Result := (fComponentList.Remove(aComponent) <> -1);
end;

procedure TcbComponentList.Pack;
begin
  fComponentList.Pack;
end;

function TcbComponentList.First: TcbComponent;
begin
  if fComponentList.Count > 0 then
    Result := TcbComponent(fComponentList.First)
  else
    Result := nil;
end;

function TcbComponentList.Last: TcbComponent;
begin
  if fComponentList.Count > 0 then
    Result := TcbComponent(fComponentList.Last)
  else
    Result := nil;
end;

function TcbComponentList.GetEnumerator: TcbComponentListEnumerator;
begin
  Result := TcbComponentList.TcbComponentListEnumerator.Create(Self);
end;

{ TcbTextElement }

function TcbTextElement.GetText: String;
begin
  Result := fText;
end;

procedure TcbTextElement.SetText(aText: String);
begin
  fText := aText;
end;

constructor TcbTextElement.Create(aOwnerComponent: TcbComponent; aText: String);
begin
  inherited Create(aOwnerComponent);
  fText := aText;
end;

destructor TcbTextElement.Destroy;
begin
  inherited Destroy;
end;

function TcbTextElement.Print: String;
begin
  Result := fText;
end;

{ TcbPrintable }

constructor TcbPrintable.Create(aOwnerComponent: TcbComponent);
begin
  inherited Create(aOwnerComponent);
end;

destructor TcbPrintable.Destroy;
begin
  inherited Destroy;
end;

{ TcbComponent }

constructor TcbComponent.Create(aOwnerComponent: TcbComponent);
begin
  fOwnerComponent := aOwnerComponent;

  if Assigned(fOwnerComponent) then
    fOwnerComponent.RegisterChild(Self);
end;

destructor TcbComponent.Destroy;
begin
  if Assigned(fChildComponents) then
    fChildComponents.Free;

  inherited Destroy;
end;

procedure TcbComponent.RegisterChild(aComponent: TcbComponent);
begin
  if not Assigned(fChildComponents) then
    fChildComponents := TcbComponentList.Create(Self);
  fChildComponents.Add(aComponent);
end;

{ TcbComment }

constructor TcbComment.Create(aOwnerComponent: TcbComponent; aComment: String; aCommentStyle: TcbCommentStyle);
begin
  inherited Create(aOwnerComponent, aComment);

  fCommentStyle := aCommentStyle;
end;

destructor TcbComment.Destroy;
begin
  inherited Destroy;
end;

function TcbComment.Print: String;
var
  lInList: TStringList;
  lOutList: TStringList;
  lString: String;
begin
  case fCommentStyle of
    cb_ccsSlashes:
    begin
      lInList := TStringList.Create;
      lOutList := TStringList.Create;

      try
        lInList.Text := fText;

        for lString in lInList do
          lOutList.Add('// ' + lString);

        Result := lOutList.Text;
      finally
        lInList.Free;
        lOutList.Free;
      end;
    end;
    cb_ccsBrackets:
    begin
      lOutList := TStringList.Create;

      try
        lOutList.Text := '{ ' + fText + ' }';
        Result := lOutList.Text;
      finally
        lOutList.Free;
      end;
    end;
    cb_ccsParenthesis:
    begin
      lOutList := TStringList.Create;

      try
        lOutList.Text := '(* ' + fText + ' *)';
        Result := lOutList.Text;
      finally
        lOutList.Free;
      end;
    end;
  end;
end;

procedure TcbComment.Add(aComment: String);
begin
  // TODO: Do this
end;

end.

