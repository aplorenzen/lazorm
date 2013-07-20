unit ulazormtypes;

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

  TloDBConfig = class(TObject)
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

  TConnectionDefList = class(specialize TFPGList<TConnectionDef>)
  end;

  TLazProjectFileList = class(specialize TFPGList<TLazProjectFile>)
  end;

implementation

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

