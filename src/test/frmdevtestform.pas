unit frmDevTestForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,

  ucbClassBuilder;
  //uCodeBuilder;

type

  { TClassBuilderTestForm }

  TClassBuilderTestForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CenterPanel: TPanel;
    SimpleTestButton: TButton;
    OutputMemo: TMemo;
    TopPanel: TPanel;
    // procedure Button1Click(Sender: TObject);
    // procedure Button2Click(Sender: TObject);
    procedure SimpleTestButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ClassBuilderTestForm: TClassBuilderTestForm;

implementation

{$R *.lfm}

{ TClassBuilderTestForm }

procedure TClassBuilderTestForm.SimpleTestButtonClick(Sender: TObject);
var
  lUnit: TcbUnit;
  lSection1: TcbUnitInterfaceSection;
  lClass: TcbClass;
  cbTObject: TcbExternalType;
  cbInteger: TcbExternalType;
  lClassMethod: TcbClassMethod;
  lTempInt: TcbVariable;
  lIntParam: TcbMethodParameter;
begin
  lUnit := TcbUnit.Create(nil, 'TestUnit');
  lUnit.UnitTopCompilerDirectives.Add('{$mode objfpc}{$H+}');
  cbTObject := TcbExternalType.Create(nil, 'TObject');
  cbInteger := TcbExternalType.Create(nil, 'Integer');

  try
    lSection1 := lUnit.UnitSections.AddSection;

    with lSection1 do
    begin
      lClass := AddClass(
        'SomeClass',
        False,
        cbTObject);

      lClassMethod := lClass.AddMethod(
        'Test',
        mtFunction,
        csPublic,
        [mdtDynamic],
        cbInteger);

      lIntParam := lClassMethod.Parameters.Add(
        'TestParam',
        cbInteger,
        mpmConst,
        '14');

      lTempInt := lClassMethod.Variables.Add(
        'TempInt',
        cbInteger);

      with lClassMethod.MethodCode do
      begin
        Add(lTempInt.VariableName + ' := ' + lIntParam.ParameterName + ';');
      end;

    end;

    OutputMemo.Lines.AddStrings(lUnit.WriteSourceCode);

  finally
    lUnit.Free;
    cbTObject.Free;
    cbInteger.Free;
  end;
end;

(*procedure TClassBuilderTestForm.Button1Click(Sender: TObject);
var
  aList: TcbComponentList;
  aCom1: TcbComponent;
  aCom2: TcbComponent;
  aCom3: TcbComponent;
  aCom: TcbComponent;
begin
  aList := TcbComponentList.Create(nil);
  aCom1 := TcbComponent.Create(nil);
  aCom2 := TcbComponent.Create(nil);
  aCom3 := TcbComponent.Create(nil);

  aList.Add(aCom1);
  aList.Add(aCom2);

  aList.Add(aCom3);

  // aCom2.Free;
  for aCom in aList do
  begin
    if aCom = aCom1 then
      OutputMemo.lines.add('com1');
    if aCom = aCom2 then
      OutputMemo.lines.add('com2');
    if aCom = aCom3 then
      OutputMemo.lines.add('com3');
  end;

  // aCom1.Free;
  // aCom2.Free;
  // aCom3.Free;
  aList.Free;




end;*)
  (*
procedure TClassBuilderTestForm.Button2Click(Sender: TObject);
var
  lCode: TcbCodeElement;
begin
  lCode := TcbCodeElement.Create(nil, 'some code here!', 'and a comment over here!', cb_ccsBrackets);

  try
    OutputMemo.Lines.Add(lCode.Print);



  finally
    lCode.Free;
  end;

end;*)

end.

