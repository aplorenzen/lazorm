program lazorm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, log4fpc,

  flazormnewmodelwizard,
  ulazormtypes,
  ulazormutils,
  ulazormconstants, uclassbuilder, frmDevTestForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TClassBuilderTestForm, ClassBuilderTestForm);
  Application.Run;
end.

