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
  uloUtils,
  uloConstants, ucbClassBuilder, frmDevTestForm, ulodmtypes, uloDatabaseTypes, uloDatabaseConstants, frmGraphTesting, ulocoretypes, persistencetest,
uloDMRetriever, ulodmretriever_mssqlserver, ulodmretrieverfactory, ulocoreinterfaces, ulodminterfaces;

{$R *.res}

begin
  Application.Title := 'LazORM';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TloNewModelForm, loNewModelForm);
  Application.Run;
end.

