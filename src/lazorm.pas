program lazorm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  flazormnewmodelwizard,
  uloTypes,
  ulocoreconstants, ucbClassBuilder, frmDevTestForm, ulodmtypes, uloDatabaseTypes, uloDatabaseConstants, frmGraphTesting, ulocoretypes, persistencetest,
uloDMRetriever, ulodmretriever_mssqlserver, ulodmretrieverfactory, ulocoreinterfaces, ulodminterfaces, ulocoreutils, threadtest;

{$R *.res}

begin
  Application.Title := 'LazORM';
  RequireDerivedFormResource := True;
  Application.Initialize;
  // Application.CreateForm(TloNewModelForm, loNewModelForm);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

