unit ulodminterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  uloDMTypes;

type
  { IloDMSelection }

  IloDMSelection = interface(IInterface)
    ['{458128B5-697F-4645-93C6-F947343B2FC3}']

  end;

  { IloDMRetriever }

  IloDMRetriever = interface(IInterface)
    ['{E4B25D15-11BE-4D94-A0A8-721B8821B0B8}']
    function GetModel: TloDMModel;

    function GetSelection: IloDMSelection;
    procedure SetSelection(Selection: IloDMSelection);

    function GetOnRetrieveCompleteProcedure: TThreadMethod;
    procedure SetOnRetrieveCompleteProcedure(aOnRetrieveCompleteProcedure: TThreadMethod);

    procedure StartRetrieve;

    property Selection: IloDMSelection read GetSelection write SetSelection;
    property Model: TloDMModel read GetModel;
    property OnRetrieveCompleteProcedure: TThreadMethod read GetOnRetrieveCompleteProcedure write SetOnRetrieveCompleteProcedure;
  end;

implementation

end.

