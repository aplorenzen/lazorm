unit flazormnewmodelwizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, sqlite3conn, mysql40conn, mysql41conn,
  mysql50conn, mysql51conn, mysql55conn, odbcconn,
  // NOTE: Currently no Oracle Win64 support
  {$IFNDEF WIN64}
  oracleconnection,
  {$ENDIF}
  pqconnection, mssqlconn, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, LCLProc, LazIDEIntf, ProjectIntf,

  uloTypes,
  uloUtils,
  uloDMTypes,
  uloDMRetriever,
  uloDMRetrieverFactory;

type

  TloWizardStep = (lsStep1, lsStep2, lsStep3);

  { TloNewModelForm }

  TloNewModelForm = class(TForm)
    BackButton: TButton;
    Button1: TButton;
    Button2: TButton;
    CancelButton: TButton;
    CenterPanel: TPanel;
    DatabaseTypeComboBox: TComboBox;
    DescriptionLabel: TLabel;
    HostnameEdit: TEdit;
    HostportEdit: TEdit;
    LoginEdit: TEdit;
    Memo1: TMemo;
    PasswordEdit: TEdit;
    Image1: TImage;
    DatabaseTypeLabel: TLabel;
    HostnameLabel: TLabel;
    HostportLabel: TLabel;
    LoginLabel: TLabel;
    CatalogEdit: TEdit;
    PasswordLabel: TLabel;
    NextButton: TButton;
    PageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    CatalogLabel: TLabel;
    SQLConnector1: TSQLConnector;
    SQLQuery1: TSQLQuery;
    Step1TabSheet: TTabSheet;
    Step2TabSheet: TTabSheet;
    Step3TabSheet: TTabSheet;
    TitleLabel: TLabel;
    TopLeftPanel: TPanel;
    TopRightPanel: TPanel;
    TitlePanel: TPanel;
    BottomPanel: TPanel;
    procedure BackButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
  private
    fStage: TloWizardStep;
    lConnectionDefList: TConnectionDefList;

    procedure SetStage(AStage: TloWizardStep);
    procedure PrepareStage(AStage: TloWizardStep);
    procedure SetCurrentPage(ATabSheet: TTabSheet);
    procedure DisableBackButton();
    procedure EnableBackButton();
    procedure DisableNextButton();
    procedure EnableNextButton();
    function ValidateStage(): Boolean;
    function GetProjectLazORMFiles: TLazProjectFileList;
  public
    property Stage: TloWizardStep read fStage write SetStage;
  end;

var
  loNewModelForm: TloNewModelForm;

resourcestring
  rsLO_Wizard_Step1_Title = '1. Connect to database';
  rsLO_Wizard_Step1_Description = 'In this step the we will attempt a '
    +'connection to the target database';
  rsLO_Wizard_Step2_Title = '2. Configure code generation';
  rsLO_Wizard_Step2_Description = 'Configure the parameters of the code '
    +'generation';
  rsLO_Wizard_Step3_Title = '3. Select objects';
  rsLO_Wizard_Step3_Description = 'Select the objects desired for object '
    +'mapping';

implementation

{$R *.lfm}

{ TloNewModelForm }

procedure TloNewModelForm.FormCreate(Sender: TObject);
var
  lConnectionDef: TConnectionDef;
begin

  // initialize all connection types
  lConnectionDefList := TConnectionDefList.Create;

  with lConnectionDefList do
  begin
    Add(TSybaseConnectionDef.Create());
    Add(TMSSQLConnectionDef.Create());
    Add(TIBConnectionDef.Create());
    Add(TSQLite3ConnectionDef.Create());
    Add(mysql40conn.TMySQLConnectionDef.Create());
    Add(mysql41conn.TMySQLConnectionDef.Create());
    Add(mysql50conn.TMySQLConnectionDef.Create());
    Add(mysql51conn.TMySQLConnectionDef.Create());
    Add(mysql55conn.TMySQLConnectionDef.Create());
    Add(TODBCConnectionDef.Create());
    // NOTE: Currently no Oracle Win64 support
    {$IFNDEF WIN64}
    Add(TOracleConnectionDef.Create());
    {$ENDIF}
    Add(TPQConnectionDef.Create());
  end;

  for lConnectionDef in lConnectionDefList do
    DatabaseTypeComboBox.AddItem(lConnectionDef.TypeName, lConnectionDef);

  for lConnectionDef in lConnectionDefList do
    Memo1.Lines.add(lConnectionDef.TypeName);

  PageControl.ShowTabs := False;

  Stage := lsStep1;
end;

procedure TloNewModelForm.FormDestroy(Sender: TObject);
begin
  lConnectionDefList.Free;
end;

procedure TloNewModelForm.BackButtonClick(Sender: TObject);
begin
  // -oAPL -cWizard 3: Progress to next state
  case Stage of
    lsStep2: Stage := lsStep1;
    lsStep3: Stage := lsStep2;
  end;
end;

procedure TloNewModelForm.Button1Click(Sender: TObject);
var
  lConnector: TSQLConnector;
  lConnectionDef: TConnectionDef;
  // lMetaDataRetreiver: TloDMMetaDataRetriever;
  // lTableList: TloDMTableList;
  lDMReceiverFactory: TloDMRetrieverFactory;
  lDMRetriever: TloDMRetriever;
  lModel: TloDMModel;
begin
  lConnector := TSQLConnector.Create(nil);



  // lMetaDataRetreiver := TloDMMetaDataRetriever.Create(lConnector);

  try
    try
      // Tested with MSSQL Server, works - FreeTDS and unixODBC
      lConnector.HostName := Format('%s:%d', [Trim(HostnameEdit.Text), StrToInt(Trim(HostportEdit.Text))]);
      lConnector.UserName := Trim(LoginEdit.Text);
      lConnector.Password := Trim(PasswordEdit.Text);
      lConnector.DatabaseName := Trim(CatalogEdit.Text);
      lConnector.ConnectorType := TConnectionDef(DatabaseTypeComboBox.Items.Objects[DatabaseTypeComboBox.ItemIndex]).TypeName;

      lDMReceiverFactory := TloDMRetrieverFactory.Create(nil, lConnector, nil, nil);

      try
        lDMRetriever := lDMReceiverFactory.GetDMRetriever();
        try
          if lDMRetriever.ConnectionTest then
            ShowMessage('Connection ok')
          else
            ShowMessage('Connection NOT ok');

          lModel := lDMRetriever.RetrieveDatabaseMetadataModel(nil);

        finally
          lDMRetriever.Free;
        end;

      finally
        lDMReceiverFactory.Free;
      end;



      except on e:Exception do
      begin
        // TODO -oAPL -cWizard 2: How to handle exceptions durring the wizard?
        ShowMessage(e.Message);
      end;
    end;

    finally
      lConnector.Free;
      // lMetaDataRetreiver.Free;
  end;

end;

procedure TloNewModelForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TloNewModelForm.NextButtonClick(Sender: TObject);
begin
  // DONE -oAPL -cWizard 4: Progress to next state
  case Stage of
    lsStep1: Stage := lsStep2;
    lsStep2: Stage := lsStep3;
    lsStep3: Stage := lsStep1;
  end;
end;

procedure TloNewModelForm.SetStage(AStage: TloWizardStep);
begin
  PrepareStage(AStage);

  case AStage of
    lsStep1:
    begin
      fStage := lsStep1;
      SetCurrentPage(Step1TabSheet);
      DisableBackButton();
    end;
    lsStep2:
    begin
      fStage := lsStep2;
      SetCurrentPage(Step2TabSheet);
      EnableBackButton();
    end;
    lsStep3:
    begin
      fStage := lsStep3;
      SetCurrentPage(Step3TabSheet);
      EnableBackButton();
    end;
  end;

  ValidateStage;
end;

procedure TloNewModelForm.PrepareStage(AStage: TloWizardStep);
begin
  case AStage of
    lsStep1:
    begin
      TitleLabel.Caption := rsLO_Wizard_Step1_Title;
      DescriptionLabel.Caption := rsLO_Wizard_Step1_Description;
    end;
    lsStep2:
    begin
      TitleLabel.Caption := rsLO_Wizard_Step2_Title;
      DescriptionLabel.Caption := rsLO_Wizard_Step2_Description;
    end;
    lsStep3:
    begin
      TitleLabel.Caption := rsLO_Wizard_Step3_Title;
      DescriptionLabel.Caption := rsLO_Wizard_Step3_Description;
    end;
  end;
end;

procedure TloNewModelForm.SetCurrentPage(ATabSheet: TTabSheet);
begin
  PageControl.ActivePage := ATabSheet;
end;

procedure TloNewModelForm.DisableBackButton;
begin
  BackButton.Enabled := False;
  BackButton.Visible := False;
end;

procedure TloNewModelForm.EnableBackButton;
begin
  BackButton.Enabled := True;
  BackButton.Visible := True;
end;

procedure TloNewModelForm.DisableNextButton;
begin
  NextButton.Enabled := False;
end;

procedure TloNewModelForm.EnableNextButton;
begin
  NextButton.Enabled := True;
end;

function TloNewModelForm.ValidateStage: Boolean;
begin
  Result := False;
  DisableNextButton();

  case Stage of
    lsStep1:
    begin
      // TODO -oAPL -cWizard 4: check if connection was established
      // TODO -oAPL -cWizard 4: Enable the next button
      Result := True;
      EnableNextButton();
    end;
    lsStep2:
    begin
      Result := True;
      EnableNextButton();
    end;
    lsStep3:
    begin
      Result := False;
    end;
  end;
end;

function TloNewModelForm.GetProjectLazORMFiles: TLazProjectFileList;
var
  lLazProject: TLazProject;
  i: Integer;
  lLazFile: TLazProjectFile;
begin
  Result := TLazProjectFileList.Create;

  lLazProject := LazarusIDE.ActiveProject;

  if lLazProject <> nil then
    for i := 0 to lLazProject.FileCount - 1 do
    begin
      lLazFile := lLazProject.Files[i];

      if lLazFile.IsPartOfProject
      and FilenameIsLazORMUnit(lLazFile.Filename)
      then
        Result.Add(lLazProject.Files[i]);
    end;
end;

end.


