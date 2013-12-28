unit ulodmretriever_mssqlserver;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  FGL,
  XMLConf,
  SQLDB,

  uloCoreInterfaces,
  uloCoreTypes,
  uloDMInterfaces,
  uloDMTypes,
  uloDMRetriever;

const
  cSQL_SwitchDatabase =
    'USE %s;';

  cSQL_GetAllTables =
    'SELECT * FROM information_schema.tables;';

  cSQL_GetTableDescription =
    'select' + LineEnding +
	    '[sysu].name as [SchemeName]' + LineEnding +
	    ',[syso].name as [TableName]' + LineEnding +
	    ',cast([exprop].value as nvarchar(max)) as [Description]' + LineEnding +
    'from sysobjects as [syso]' + LineEnding +
    'join sysusers as [sysu]' + LineEnding +
	    'on [sysu].uid = [syso].uid' + LineEnding +
    'join sys.extended_properties as [exprop]' + LineEnding +
	    'on [exprop].major_id = [syso].id' + LineEnding +
      'and [exprop].minor_id = 0' + LineEnding +
      'and [exprop].name = ''MS_Description''' + LineEnding +
    'where' + LineEnding +
	    '[syso].[type] = ''u'';';{ + LineEnding +
	    'and [sysu].name = ''%s''' + LineEnding +
	    'and [syso].name = ''%s'';';}

  	{select
	    [sysu].name as [SchemeName]
	    ,[syso].name as [TableName]
	    ,cast([exprop].value as nvarchar(max)) as [Description]
    from sysobjects as [syso]
    join sysusers as [sysu]
	    on [sysu].uid = [syso].uid
    join sys.extended_properties as [exprop]
	    on [exprop].major_id = [syso].id
      and [exprop].minor_id = 0
      and [exprop].name = 'MS_Description'
    where
	    [syso].[type] = 'u'
	    and [sysu].name = '%s'
	    'and [syso].name = '%s'}

  cSQL_GetFields =
    'SELECT TOP 0 * FROM [%s].[%s].[%s];';

  cSQL_GetFieldDescriptions =
    'select' + LineEnding +
	    '[sysu].name as [scheme]' + LineEnding +
	    ',[syso].name as [table]' + LineEnding +
	    ',[sysc].name as [FieldName]' + LineEnding +
      ',cast([exprop].value as nvarchar(max)) as [Description]' + LineEnding +
    'from sysobjects as [syso]' + LineEnding +
    'join sysusers as [sysu]' + LineEnding +
	    'on [sysu].uid = [syso].uid' + LineEnding +
    'join syscolumns as [sysc]' + LineEnding +
	    'on [sysc].id = [syso].id' + LineEnding +
    'join sys.extended_properties as [exprop]' + LineEnding +
	    'on [exprop].major_id = [sysc].id' + LineEnding +
      'and [exprop].minor_id = [sysc].colid' + LineEnding +
      'and [exprop].name = ''MS_Description''' + LineEnding +
    'where' + LineEnding +
	    '[syso].[type] = ''u''' + LineEnding +
	    'and [sysu].name = ''%s''' + LineEnding +
	    'and [syso].name = ''%s'';';

  {select
  	[sysu].name as [scheme]
  	,[syso].name as [table]
  	,[sysc].name as [FieldName]
    ,cast([exprop].value as nvarchar(max)) as [Description]
  from sysobjects as [syso]
  join sysusers as [sysu]
  	on [sysu].uid = [syso].uid
  join syscolumns as [sysc]
  	on [sysc].id = [syso].id
  join sys.extended_properties as [exprop]
  	on [exprop].major_id = [sysc].id
      and [exprop].minor_id = [sysc].colid
      and [exprop].name = 'MS_Description'
  where
  	[syso].[type] = 'u'
  	and [sysu].name = '%s'
  	and [syso].name = '%s'}

  cSQL_GetPrimaryKeys =
    'select' + LineEnding +
	    '[schemas].name as [SchemaName]' + LineEnding +
	    ',[tables].name as [TableName]' + LineEnding +
	    ',[columns].name as [KeyColumnName]' + LineEnding +
	    ',[columns].is_identity as [KeyColumnIsIdentity]' + LineEnding +
    'from sys.key_constraints as [keyconstraints]' + LineEnding +
    'join sys.index_columns as [indexcolumns]' + LineEnding +
	    'on [indexcolumns].object_id = [keyconstraints].parent_object_id' + LineEnding +
    'join sys.columns as [columns]' + LineEnding +
	    'on [columns].object_id  = [indexcolumns].object_id' + LineEnding +
	    'and [columns].column_id = [indexcolumns].column_id' + LineEnding +
    'join sys.tables as [tables]' + LineEnding +
	    'on [tables].object_id = [keyconstraints].parent_object_id' + LineEnding +
    'join sys.schemas as [schemas]' + LineEnding +
	    'on [schemas].schema_id = [keyconstraints].schema_id' + LineEnding +
    'where' + LineEnding +
	    '[keyconstraints].[type] = ''PK'';';

  {select
  	[schemas].name as [SchemaName]
  	,[tables].name as [TableName]
  	,[columns].name as [KeyColumnName]
  	,[columns].is_identity as [KeyColumnIsIdentity]
  from sys.key_constraints as [keyconstraints]
  join sys.index_columns as [indexcolumns]
  	on [indexcolumns].object_id = [keyconstraints].parent_object_id
  join sys.columns as [columns]
  	on [columns].object_id  = [indexcolumns].object_id
  	and [columns].column_id = [indexcolumns].column_id
  join sys.tables as [tables]
  	on [tables].object_id = [keyconstraints].parent_object_id
  join sys.schemas as [schemas]
  	on [schemas].schema_id = [keyconstraints].schema_id
  where
  	[keyconstraints].[type] = 'PK'}

  cSQL_GetForeignKeys =
    'SELECT' + LineEnding +
    	'[parenttableschema].name as [ParentTableSchemaName]' + LineEnding +
    	',[parenttable].name as [ParentTableName]' + LineEnding +
    	',[columns].name as [ParentColumnName]' + LineEnding +
    	',[referencedtableschema].name as [ReferencedTableSchemaName]' + LineEnding +
    	',[referencedtable].name as [ReferencedTableName]' + LineEnding +
    	',cref.name as [ReferencedColumnName]' + LineEnding +
    'FROM sys.foreign_key_columns as fkc' + LineEnding +
    'JOIN sys.columns as [columns]' + LineEnding +
    	'ON fkc.parent_column_id = [columns].column_id' + LineEnding +
    	'AND fkc.parent_object_id = [columns].object_id' + LineEnding +
    'JOIN sys.columns as cref' + LineEnding +
    	'ON fkc.referenced_column_id = cref.column_id' + LineEnding +
    	'AND fkc.referenced_object_id = cref.object_id' + LineEnding +
    'join sys.tables as [parenttable]' + LineEnding +
    	'on [parenttable].object_id = fkc.parent_object_id' + LineEnding +
    'join sys.tables as [referencedtable]' + LineEnding +
    	'on [referencedtable].object_id = fkc.referenced_object_id' + LineEnding +
    'join sys.schemas as [parenttableschema]' + LineEnding +
    	'on [parenttableschema].schema_id = [parenttable].schema_id' + LineEnding +
    'join sys.schemas as [referencedtableschema]' + LineEnding +
    	'on [referencedtableschema].schema_id = [referencedtable].schema_id';

  {SELECT
	  [parenttableschema].name as [ParentTableSchemaName]
	  ,[parenttable].name as [ParentTableName]
	  ,[columns].name as [ParentColumnName]
	  ,[referencedtableschema].name as [ReferencedTableSchemaName]
	  ,[referencedtable].name as [ReferencedTableName]
	  ,cref.name as [ReferencedColumnName]
  FROM sys.foreign_key_columns as fkc
  JOIN sys.columns as [columns]
	  ON fkc.parent_column_id = [columns].column_id
	  AND fkc.parent_object_id = [columns].object_id
  JOIN sys.columns as cref
	  ON fkc.referenced_column_id = cref.column_id
	  AND fkc.referenced_object_id = cref.object_id
  join sys.tables as [parenttable]
	  on [parenttable].object_id = fkc.parent_object_id
  join sys.tables as [referencedtable]
	  on [referencedtable].object_id = fkc.referenced_object_id
  join sys.schemas as [parenttableschema]
	  on [parenttableschema].schema_id = [parenttable].schema_id
  join sys.schemas as [referencedtableschema]
	  on [referencedtableschema].schema_id = [referencedtable].schema_id}


type
  { TloDMRetriever_MSSQL }

   TloDMRetriever_MSSQL = class(TloDMRetriever)
   private
     procedure FetchTableDescriptions(var aModel: TloDMModel; var aQuery: TSQLQuery);
     procedure FetchTablePrimaryKeys(var aModel: TloDMModel; var aQuery: TSQLQuery);
     procedure FetchTableForeignKeys(var aModel: TloDMModel; var aQuery: TSQLQuery);
     procedure FetchTableFieldDescriptions(var aModel: TloDMModel; var aQuery: TSQLQuery);
     procedure FetchTableFields(var aModel: TloDMModel; var aQuery: TSQLQuery);
     procedure FetchTableList(const aSelection: IloDMSelection; var aModel: TloDMModel; var aQuery: TSQLQuery);
     procedure SwitchDatabase(aDatabaseName: String);
   public
     constructor Create(
       aOwner: IloObject;
       aConnection: TSQLConnector = nil;
       aLog: IloLogger = nil;
       aConfig: TXMLConfig = nil;
       aMutex: TCriticalSection = nil);
     destructor Destroy; override;

     function RetrieveDatabaseMetadataModel(aSelection: IloDMSelection): TloDMModel; override;
   end;

implementation

procedure TloDMRetriever_MSSQL.FetchTableDescriptions(var aModel: TloDMModel; var aQuery: TSQLQuery);
var
  lTable: TloDMTable;
begin
  // Get table descriptions for the tables in the list

  // Close and clear query
  aQuery.Close;
  aQuery.SQL.Clear;

  // aQuery.SQL.Add(Format(cSQL_GetTableDescription, [lTable.SchemeName, lTable.TableName]));
  aQuery.SQL.Add(cSQL_GetTableDescription);
  aQuery.Open;

  if not aQuery.IsEmpty then
  begin
    while aQuery.EOF do
    begin
      lTable := aModel.TableList.FindTable(
        aModel.Connection.DatabaseName,
        aQuery.FieldByName('SchemeName').AsString,
        aQuery.FieldByName('TableName').AsString);

      if Assigned(lTable) then
        lTable.Description := aQuery.FieldByName('Description').AsString;

      aQuery.Next;
    end;
  end;
end;

procedure TloDMRetriever_MSSQL.FetchTablePrimaryKeys(var aModel: TloDMModel; var aQuery: TSQLQuery);
type
  TloDMPrimaryKeyField = record
    SchemaName: String;
    TableName: String;
    KeyColumnName: String;
    KeyColumnIsIdentity: Boolean;
  end;

  TloDMPrimaryKeyFieldList = array of TloDMPrimaryKeyField;
var
  lPrimaryKeyFieldList: TloDMPrimaryKeyFieldList;
  lListIndex: Integer;
  lTable: TloDMTable;
  lField: TloDMField;
begin
  // Close and clear query
  aQuery.Close;
  aQuery.SQL.Clear;

  aQuery.SQL.Add(cSQL_GetPrimaryKeys);
  aQuery.Open;

  if not aQuery.IsEmpty then
  begin
    SetLength(lPrimaryKeyFieldList, 0);

    while not aQuery.EOF do
    begin
      SetLength(lPrimaryKeyFieldList, Length(lPrimaryKeyFieldList) + 1);

      with lPrimaryKeyFieldList[Length(lPrimaryKeyFieldList) - 1] do
      begin
        SchemaName := aQuery.FieldByName('SchemaName').AsString;
        TableName := aQuery.FieldByName('TableName').AsString;
        KeyColumnName := aQuery.FieldByName('KeyColumnName').AsString;
        KeyColumnIsIdentity := aQuery.FieldByName('KeyColumnIsIdentity').AsBoolean;
      end;

      aQuery.Next;
    end;

    aQuery.Close;

    for lListIndex := 0 to Length(lPrimaryKeyFieldList) - 1 do
    begin
      lTable := aModel.TableList.FindTable(
        aModel.Connection.DatabaseName,
        lPrimaryKeyFieldList[lListIndex].SchemaName,
        lPrimaryKeyFieldList[lListIndex].TableName);

      if Assigned(lTable) then
      begin
        lField := lTable.FieldList.FindFieldByName(lPrimaryKeyFieldList[lListIndex].KeyColumnName);

        if Assigned(lField) then
        begin
          lField.IsIndexed := True;

          if lPrimaryKeyFieldList[lListIndex].KeyColumnIsIdentity then
            lField.IsPrimaryKey := True;
        end;
      end;
    end;
  end;
end;

procedure TloDMRetriever_MSSQL.FetchTableForeignKeys(var aModel: TloDMModel; var aQuery: TSQLQuery);
type
  TloDMForiegnKey = record
    ParentTableSchemaName: String;
    ParentTableName: String;
    ParentColumnName: String;
    ReferencedTableSchemaName: String;
    ReferencedTableName: String;
    ReferencedColumnName: String;
  end;

  TloDMForiegnKeyList = array of TloDMForiegnKey;
var
  lForiegnKeyList: TloDMForiegnKeyList;
  lListIndex: Integer;
  lParentTable: TloDMTable;
  lReferencedTable: TloDMTable;
  lParentField: TloDMField;
  lReferencedField: TloDMField;
begin
  // Close and clear query
  aQuery.Close;
  aQuery.SQL.Clear;

  aQuery.SQL.Add(cSQL_GetForeignKeys);
  aQuery.Open;

  if not aQuery.IsEmpty then
  begin
    SetLength(lForiegnKeyList, 0);

    while not aQuery.EOF do
    begin
      SetLength(lForiegnKeyList, Length(lForiegnKeyList) + 1);

      with lForiegnKeyList[Length(lForiegnKeyList) - 1] do
      begin
        ParentTableSchemaName := aQuery.FieldByName('ParentTableSchemaName').AsString;
        ParentTableName := aQuery.FieldByName('ParentTableName').AsString;
        ParentColumnName := aQuery.FieldByName('ParentColumnName').AsString;
        ReferencedTableSchemaName := aQuery.FieldByName('ReferencedTableSchemaName').AsString;
        ReferencedTableName := aQuery.FieldByName('ReferencedTableName').AsString;
        ReferencedColumnName := aQuery.FieldByName('ReferencedColumnName').AsString;
      end;

      aQuery.Next;
    end;

    aQuery.Close;

    for lListIndex := 0 to Length(lForiegnKeyList) - 1 do
    begin
      lParentTable := aModel.TableList.FindTable(
        aModel.Connection.DatabaseName,
        lForiegnKeyList[lListIndex].ParentTableSchemaName,
        lForiegnKeyList[lListIndex].ParentTableName);

      if Assigned(lParentTable) then
      begin
        lParentField := lParentTable.FieldList.FindFieldByName(
          lForiegnKeyList[lListIndex].ParentColumnName);

        if Assigned(lParentField) then
        begin
          lReferencedTable := aModel.TableList.FindTable(
            aModel.Connection.DatabaseName,
            lForiegnKeyList[lListIndex].ReferencedTableSchemaName,
            lForiegnKeyList[lListIndex].ReferencedTableName);

          if Assigned(lReferencedTable) then
          begin
            lReferencedField := lParentTable.FieldList.FindFieldByName(
              lForiegnKeyList[lListIndex].ReferencedColumnName);

            if Assigned(lReferencedField) then
            begin
              // Found both tables and both fields in each table in the data in the model
              lParentField.ForeignKeyReference := lReferencedField;
              lReferencedField.ReferencingForeignKeys.Add(lParentField);
            end;
          end;
        end;
      end;
    end;






  end;




end;

procedure TloDMRetriever_MSSQL.FetchTableFieldDescriptions(var aModel: TloDMModel; var aQuery: TSQLQuery);
var
  lField: TloDMField;
  lTable: TloDMTable;
  lTableIndex: Integer;
begin
  lTableIndex := 0;

  // Get field descriptions for all fields in all tables
  for lTable in aModel.TableList do
  begin
    Inc(lTableIndex);

    // Close and clear query
    aQuery.Close;
    aQuery.SQL.Clear;

    aQuery.SQL.Add(Format(cSQL_GetFieldDescriptions, [lTable.SchemeName, lTable.TableName]));
    aQuery.Open;

    if not aQuery.IsEmpty then
    begin
      while not aQuery.EOF do
      begin
        lField := lTable.FieldList.FindFieldByName(
          aQuery.FieldByName('FieldName').AsString);

        if Assigned(lField) then
          lField.Description := aQuery.FieldByName('Description').AsString;

        aQuery.Next;
      end;
    end;
  end;
end;

procedure TloDMRetriever_MSSQL.FetchTableFields(var aModel: TloDMModel; var aQuery: TSQLQuery);
var
  lFieldCount: LongInt;
  lFieldIndex: LongInt;
  lTable: TloDMTable;
  lTableIndex: Integer;
  lField: TloDMField;
begin
  lTableIndex := 0;

  // Get basic field information for alle fields in the tables that have been put into the model
  for lTable in aModel.TableList do
  begin
    Inc(lTableIndex);

    // Close and clear query
    aQuery.Close;
    aQuery.SQL.Clear;

    aQuery.SQL.Add(Format(cSQL_GetFields, [lTable.CatalogName, lTable.SchemeName, lTable.TableName]));
    aQuery.Open;

    lFieldCount := aQuery.Fields.Count;

    for lFieldIndex := 0 to lFieldCount - 1 do
    begin
      lField := TloDMField.Create(lTable);

      with lField do
      begin
        FieldName := aQuery.Fields[lFieldIndex].FieldName;
        FieldNo := aQuery.Fields[lFieldIndex].FieldNo;
        FieldKind := aQuery.Fields[lFieldIndex].FieldKind;
        DataType := aQuery.Fields[lFieldIndex].DataType;
        DataSize := aQuery.Fields[lFieldIndex].DataSize;
        // Not sure if the default information is passed when selecting, investigate
        HasDefault := (Length(aQuery.Fields[lFieldIndex].DefaultExpression) > 0);
        DefaultValue := aQuery.Fields[lFieldIndex].DefaultExpression;
        AttributeSet := aQuery.Fields[lFieldIndex].AttributeSet;
        ProviderFlags := aQuery.Fields[lFieldIndex].ProviderFlags;
        IsIndexed := aQuery.Fields[lFieldIndex].IsIndexField;
        IsRequired := aQuery.Fields[lFieldIndex].Required;
        // IsPrimaryKey := <not known yet>
        // Description := <not known yet>
      end;

      lTable.FieldList.Add(lField);
    end;
  end;
end;

procedure TloDMRetriever_MSSQL.FetchTableList(const aSelection: IloDMSelection; var aModel: TloDMModel; var aQuery: TSQLQuery);
var
  lTableIndex: LongInt;
  lTableCount: LongInt;
  lTable: TloDMTable;
begin
  // Prepare to query all the tables in the selected database
  aQuery.SQL.Clear;

  { TODO -oAPL 2 Filter the selected tables here}
  aQuery.SQL.Add(cSQL_GetAllTables);

  // Open the query
  aQuery.Open;

  // If the query result is not empty, grab each resulting row in turn and stuff into
  // a list of TloDMTable objects
  if not aQuery.IsEmpty then
  begin
    lTableCount := aQuery.RecordCount;
    lTableIndex := 0;

    while not aQuery.EOF do
    begin
      Inc(lTableIndex);
      lTable := TloDMTable.Create(aModel);

      with lTable do
      begin
        TableName := aQuery.FieldByName('TABLE_NAME').AsString;
        SchemeName := aQuery.FieldByName('TABLE_SCHEMA').AsString;
        CatalogName := aQuery.FieldByName('TABLE_CATALOG').AsString;
      end;

      aModel.TableList.Add(lTable);

      aQuery.Next;
    end;
  end;
end;

procedure TloDMRetriever_MSSQL.SwitchDatabase(aDatabaseName: String);
const
  lProcedureName = 'SwitchDatabase';
begin
  // Switch to the specified database
  Connection.ExecuteDirect(Format(cSQL_SwitchDatabase, [aDatabaseName]));
end;

constructor TloDMRetriever_MSSQL.Create(aOwner: IloObject; aConnection: TSQLConnector; aLog: IloLogger; aConfig: TXMLConfig; aMutex: TCriticalSection);
const
  lProcedureName = 'Create';
begin
  inherited Create(
    aOwner,
    aConnection,
    aLog,
    aConfig,
    aMutex);
end;

destructor TloDMRetriever_MSSQL.Destroy;
const
  lProcedureName = 'Destroy';
begin
  inherited Destroy;
end;

function TloDMRetriever_MSSQL.RetrieveDatabaseMetadataModel(aSelection: IloDMSelection): TloDMModel;
const
  lProcedureName = 'RetrieveDatabaseMetadataModel';
var
  lQuery: TSQLQuery;
  lTransaction: TSQLTransaction;
  lModel: TloDMModel;

  lStartTime: TDateTime;
  lEndTime: TDateTime;
begin
  // Enter the objects assigned (if any) mutex as the first thing, the intention of this
  // is to assure that no other thread is using this connection while we are querying
  // the database in this function
  MutexEnter;

  lStartTime := Now;

  try
    try
      Result := nil;

      // Test the connection that exists in this object
      if not ConnectionTest then
      begin
        LogError('Unable to retreive database metadata model, could not open connection.');
        Exit;
      end;

      lModel := TloDMModel.Create(Self, Connection);

      try
        try
          // Create transaction and query objects for activities, not owned by any component, manage memory in local code
          lTransaction := TSQLTransaction.Create(nil);
          lQuery := TSQLQuery.Create(nil);

          try
            // Associate the transaction with the connection
            lTransaction.DataBase := Connection;
            // Associate the query with the transaction
            lQuery.Transaction := lTransaction;


            // Change the current connections active database to the selected
            SwitchDatabase(Connection.DatabaseName);

            // Get the list of all the selected tables
            FetchTableList(aSelection, lModel, lQuery);

            // Get descriptions for all the tables
            FetchTableDescriptions(lModel, lQuery);

            // Get all fields for all tables
            FetchTableFields(lModel, lQuery);

            // Get all descriptions for all fields in the tables
            FetchTableFieldDescriptions(lModel, lQuery);

            // Get single column index information and primary key data
            FetchTablePrimaryKeys(lModel, lQuery);


            FetchTableForeignKeys(lModel, lQuery);




            finally
            begin
              lTransaction.Free;
              lQuery.Free;
            end;
          end;

          except on e:Exception do
          begin
            LogError(e.Message);
          end;
        end;

        finally
        begin
          Connection.Close;
        end;
      end;

      // Finally, assign the generated model to the result
      Result := lModel;

      except on e:Exception do
      begin
        LogError(Format('[%s.%s]: An exception occurred when retrieving database metadata for the database. Exception message: %s',
        [Self.ClassName, lProcedureName, e.Message]));
      end;
    end;

    finally
    begin
      MutexExit;
    end;
  end;
end;

end.

