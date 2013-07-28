unit frmDevTestForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,

  uClassBuilder;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  lUnit: TcbUnit;
  lSection1: TcbUnitInterfaceSection;
  lClass: TcbClass;
  cbTObject: TcbExternalType;
begin
  lUnit := TcbUnit.Create(nil, 'TestUnit');
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

  Memo1.Lines.AddStrings(lUnit.WriteSourceCode);

  lUnit.Free;
  cbTObject.Free;
end;

end.

