program lazorm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, log4fpc,

  flazormnewmodelwizard,
  uloTypes,
  uloUtils,
  uloConstants, ucbClassBuilder, frmDevTestForm, ucbCodeBuilder, uloDatabaseMapTypes, uloDatabaseTypes, uloDatabaseConstants, frmGraphTesting;

{$R *.res}

begin
  Application.Title:='LazORM';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TClassBuilderTestForm, ClassBuilderTestForm);
  Application.Run;
end.

