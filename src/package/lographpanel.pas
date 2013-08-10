unit loGraphPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FPCanvas;

const
  lo_GraphPanel_Default_Width = 100;
  lo_GraphPanel_Default_Height = 100;

type




  { TloGraphPanel }

  TloGraphPanel = class(TCustomControl)
  private
    fShowGrid: Boolean;
    fGridHeight: Integer;
    fGridWidth: Integer;
    fGridColor: TColor;
    fGridStyle: TFPPenStyle;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property ShowGrid: Boolean read fShowGrid write fShowGrid default True;
    property GridHeight: Integer read fGridHeight write fGridHeight default 24;
    property GridWidth: Integer read fGridWidth write fGridWidth default 24;
    property GridColor: TColor read fGridColor write fGridColor default clBlack;
    property GridStyle: TFPPenStyle read fGridStyle write fGridStyle default psDot;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LazORM',[TloGraphPanel]);
end;

{ TloGraphPanel }

constructor TloGraphPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Width := lo_GraphPanel_Default_Width;
  Self.Height := lo_GraphPanel_Default_Height;

  Self.Color := $F5EFCF;

  fShowGrid := True;
  fGridHeight := 24;
  fGridWidth := 24;
  fGridColor := clBlack;
  fGridStyle := psDot;
end;

procedure TloGraphPanel.Paint;
var
  lGridX: Integer;
  lGridY: Integer;
begin
  inherited Paint;

  if Self.ShowGrid then
  begin
    { Paint the grid background}

    Self.Canvas.Pen.Color := Self.GridColor;
  {
  psSolid = FPCanvas.psSolid;
  psDash = FPCanvas.psDash;
  psDot = FPCanvas.psDot;
  psDashDot = FPCanvas.psDashDot;
  psDashDotDot = FPCanvas.psDashDotDot;
  psClear = FPCanvas.psClear;
  psInsideframe = FPCanvas.psInsideframe;
  psPattern = FPCanvas.psPattern;
  }
    Self.Canvas.Pen.Style := Self.GridStyle;

    lGridX := Self.GridWidth;

    while lGridX < Self.Width do
    begin
      Self.Canvas.Line(
        lGridX, 0,
        lGridX, Self.Height);
      lGridX := lGridX + Self.GridWidth;
    end;

    lGridY := Self.GridHeight;

    while lGridY < Self.Height do
    begin
      Self.Canvas.Line(
        0, lGridY,
        Self.Width, lGridY);
      lGridY := lGridY + Self.GridHeight;
    end;

  end;
end;

end.
