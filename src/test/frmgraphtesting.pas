unit frmGraphTesting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, loGraphPanel, loDataModelPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    loDataModelPanel1: TloDataModelPanel;
    Panel1: TPanel;
    ScrollBox2: TScrollBox;
    procedure Button1Click(Sender: TObject);
    procedure ScrollBox2Resize(Sender: TObject);
  private
    { private declarations }
  public

    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ScrollBox2Resize(Sender: TObject);
begin
  if ScrollBox2.ClientWidth > loDataModelPanel1.Width then
    loDataModelPanel1.Width := ScrollBox2.ClientWidth;
  if ScrollBox2.ClientHeight > loDataModelPanel1.Height then
    loDataModelPanel1.Height := ScrollBox2.ClientHeight;
end;

procedure TForm1.Button1Click(Sender: TObject);
const
  lRandMax: LongInt = 250;
var
  lSimpleBox: TloSimpleBox;
begin
  lSimpleBox := TloSimpleBox.Create(
    loDataModelPanel1,
    'Test',
    0,
    0,
    100,
    100);
  loDataModelPanel1.SimpleBoxList.Add(lSimpleBox);
  loDataModelPanel1.Refresh;

end;

end.

