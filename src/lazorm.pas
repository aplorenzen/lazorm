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
  uloConstants, ucbClassBuilder, frmDevTestForm, ulodbmodeltypes, uloDatabaseTypes, uloDatabaseConstants, frmGraphTesting, ulocoretypes, persistencetest;

{$R *.res}

begin
  Application.Title := 'LazORM';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TloNewModelForm, loNewModelForm);
  Application.Run;
end.

