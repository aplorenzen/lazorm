program lazorm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  flazormnewmodelwizard,
  ulazormtypes,
  ulazormutils,
  ulazormconstants, XMLSerializer, uclassbuilder;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TloNewModelForm, loNewModelForm);
  Application.Run;
end.

