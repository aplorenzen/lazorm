unit persistencetest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources;

type

  { TMyObject }

  TMyObject = class(TObject)
  private
    fSomeInteger: Integer;
    fSomeString: String;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SomeInteger: Integer read fSomeInteger write fSomeInteger;
    property SomeString: String read fSomeString write fSomeString;
  end;

  { TMyComponent }

  TMyComponent = class(TComponent)
  private
    fSomeInteger: Integer;
    fSomeString: String;
    fSomeObject: TObject;
    fSomeComponent: TMyComponent;
  public
    constructor Create(aOwner: TComponent);
    destructor Destroy; override;
  published
    property SomeInteger: Integer read fSomeInteger write fSomeInteger;
    property SomeString: String read fSomeString write fSomeString;
    property SomeObject: TObject read fSomeObject write fSomeObject;
    property SomeComponent: TMyComponent read fSomeComponent write fSomeComponent;
  end;

  { TTester }

  TTester = class(TComponent)
  public
    constructor Create(aOwner: TComponent);
    destructor Destroy; override;

    procedure Test;

  end;

implementation

{ TMyObject }

constructor TMyObject.Create;
begin
  inherited Create;
end;

destructor TMyObject.Destroy;
begin
  inherited Destroy;
end;

{ TTester }

constructor TTester.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor TTester.Destroy;
begin
  inherited Destroy;
end;

procedure TTester.Test;
var
  testComp1: TMyComponent;
  testComp2: TMyComponent;
  testComp3: TMyComponent;
  testComp4: TMyComponent;
  testObj1: TMyObject;
  testObj2: TMyObject;
  testObj3: TMyObject;
  testObj4: TMyObject;
  memStream: TMemoryStream;

begin
  testObj1 := TMyObject.Create;
  with testObj1 do
  begin
    SomeString:='test obj1 string';
    SomeInteger:=1;
  end;
  testObj2 := TMyObject.Create;
  with testObj2 do
  begin
    SomeString:='test obj2 string';
    SomeInteger:=1;
  end;
  testObj3 := TMyObject.Create;
  with testObj3 do
  begin
    SomeString:='test obj3 string';
    SomeInteger:=1;
  end;
  testObj4 := TMyObject.Create;
  with testObj4 do
  begin
    SomeString:='test obj4 string';
    SomeInteger:=4;
  end;


  testComp1 := TMyComponent.Create(self);
  with testComp1 do
  begin
    SomeComponent := nil;
    SomeInteger := 1;
    SomeObject := testObj1;
    SomeString := 'test component 1';
  end;

  testComp2 := TMyComponent.Create(self);
  with testComp2 do
  begin
    SomeComponent := testComp1;
    SomeInteger := 2;
    SomeObject := testObj2;
    SomeString := 'test component 2';
  end;
  testComp3 := TMyComponent.Create(self);
  with testComp3 do
  begin
    SomeComponent := testComp2;
    SomeInteger := 3;
    SomeObject := testObj3;
    SomeString := 'test component 3';
  end;
  testComp4 := TMyComponent.Create(self);
  with testComp4 do
  begin
    SomeComponent := testComp3;
    SomeInteger := 4;
    SomeObject := testObj4;
    SomeString := 'test component 4';
  end;

  memStream := TMemoryStream.Create;
  try
    //WriteComponentAsBinaryToStream(memStream, testComp4);

    memStream.WriteComponent(testComp4);
    memStream.SaveToFile('c:\testcompfile.obj');

  finally
    memStream.Free;
  end;

  testComp4.Free;


end;



{ TMyComponent }

constructor TMyComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor TMyComponent.Destroy;
begin
  inherited Destroy;
end;

end.

