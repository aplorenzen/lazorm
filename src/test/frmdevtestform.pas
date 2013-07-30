unit frmDevTestForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,

  uClassBuilder;

type

  { TClassBuilderTestForm }

  TClassBuilderTestForm = class(TForm)
    CenterPanel: TPanel;
    SimpleTestButton: TButton;
    OutputMemo: TMemo;
    TopPanel: TPanel;
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

end.

