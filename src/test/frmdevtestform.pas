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
begin
  lUnit := TcbUnit.Create(nil, 'TestUnit');
  lUnit.UnitTopCompilerDirectives.Add('{$mode objfpc}{$H+}');
  cbTObject := TcbExternalType.Create(nil, 'TObject');

  lSection1 := lUnit.UnitSections.AddSection;

  with lSection1 do
  begin
    lClass := AddClass(
      'SomeClass',
      False,
      cbTObject);

    lClass.AddMethod(
      'Test');
  end;

  OutputMemo.Lines.AddStrings(lUnit.WriteSourceCode);

  lUnit.Free;
  cbTObject.Free;
end;

end.

