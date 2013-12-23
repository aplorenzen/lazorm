unit uloTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  sqldb,
  fgl,
  ProjectIntf;

type

  { TloDBConfig }

  TloDBConfig = class(TInterfacedPersistent)
  private
    fHostName: String;
    fPort: Integer;
    fDBType: TConnectionDef;
    fLogin: String;
    fPassword: String;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property HostName: String read fHostName write fHostName;
    property Port: Integer read fPort write fPort;
    property DBType: TConnectionDef read fDBType write fDBType;
    property Login: String read fLogin write fLogin;
    property Password: String read fPassword write fPassword;
  end;

  { TConnectionDefList }

  TConnectionDefList = class(specialize TFPGList<TConnectionDef>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLazProjectFileList = class(specialize TFPGList<TLazProjectFile>)
  end;

implementation

{ TConnectionDefList }

constructor TConnectionDefList.Create;
begin
  inherited;
end;

destructor TConnectionDefList.Destroy;
var
  lConnectionDef: TConnectionDef;
begin
  for lConnectionDef in Self do
    if Assigned(lConnectionDef) then
      lConnectionDef.Free;

  inherited Destroy;
end;

{ TloDBConfig }

constructor TloDBConfig.Create;
begin
  inherited;
end;

destructor TloDBConfig.Destroy;
begin
  inherited;
end;

end.

