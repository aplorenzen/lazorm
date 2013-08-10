unit loDataModelPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, FPCanvas, fgl;

type
  TloDataModelPanel = class;

  IloSelectableModelElement = interface(IInterface)
    ['{2FC51848-40D2-4B76-8A34-FDDD2430C360}']
    procedure SelectElement;
  end;

  { TloGridOptions }

  TloGridOptions = class(TPersistent)
  private
    fGridWidth: Word;
    fGridHeight: Word;
    fGridColor: TColor;
    fGridStyle: TFPPenStyle;
    fShowGrid: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ShowGrid: Boolean read fShowGrid write fShowGrid default True;
    property GridHeight: Word read fGridHeight write fGridHeight default 24;
    property GridWidth: Word read fGridWidth write fGridWidth default 24;
    property GridColor: TColor read fGridColor write fGridColor default clBlack;
    property GridStyle: TFPPenStyle read fGridStyle write fGridStyle default psDot;
  end;

  { TloModelElement }

  TloModelElement = class abstract(TInterfacedPersistent)
  private
    fOwner: TloDataModelPanel;
    fElementBounds: TRect;
    fSelected: Boolean;
  public
    constructor Create(aOwner: TloDataModelPanel);
    destructor Destroy; override;
    procedure Paint; virtual; abstract;

    property Owner: TloDataModelPanel read fOwner;
    property ElementBounds: TRect read fElementBounds write fElementBounds;
    property Selected: Boolean read fSelected write fSelected;
  end;

  { TloSimpleBox }

  TloSimpleBox = class(TInterfacedPersistent)
  private
    fOwner: TloDataModelPanel;
    fCaption: String;
    fX: Integer;
    fY: Integer;
    fWidth: LongInt;
    fHeight: LongInt;
    fSelected: Boolean;
    fDragPoint: TPoint;
  public
    constructor Create(aOwner: TloDataModelPanel; aCaption: String = ''; aX: Integer = 0; aY: Integer = 0; aWidth: LongInt = 10; aHeight: LongInt = 10);
    destructor Destroy; override;
    procedure Paint(aCanvas: TCanvas);
    procedure Select(aPoint: TPoint);
    procedure UnSelect(aPoint: TPoint);
    procedure Drag(aPoint: TPoint);
    function HitTest(aPoint: TPoint): Boolean;

    property Caption: String read fCaption write fCaption;
    property Selected: Boolean read fSelected write fSelected;
    property X: Integer read fX write fX;
    property Y: Integer read fY write fY;
    property Width: LongInt read fWidth write fWidth;
    property Height: LongInt read fHeight write fHeight;
  end;

  TloSimpleBoxList = class(specialize TFPGList<TloSimpleBox>);

  { TloDataModelPanel }

  TloDataModelPanel = class(TPanel)
  private
    fDragging: Boolean;

    fGridOptions: TloGridOptions;
    fSimpleBoxList: TloSimpleBoxList;
    fSelectionList: TloSimpleBoxList;
    function GetBoxAt(X, Y: Integer): TloSimpleBox;

  protected
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);  override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter;  override;
    procedure MouseLeave;  override;
  published
    property GridOptions: TloGridOptions read fGridOptions write fGridOptions;
    property SimpleBoxList: TloSimpleBoxList read fSimpleBoxList write fSimpleBoxList;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LazORM',[TloDataModelPanel]);
end;

{ TloModelElement }

constructor TloModelElement.Create(aOwner: TloDataModelPanel);
begin

end;

destructor TloModelElement.Destroy;
begin
  inherited Destroy;
end;

{ TloSimpleBox }

constructor TloSimpleBox.Create(aOwner: TloDataModelPanel; aCaption: String; aX: Integer; aY: Integer; aWidth: LongInt; aHeight: LongInt);
begin
  fOwner := aOwner;
  inherited Create;
  fCaption := aCaption;
  fX := aX;
  fY := aY;
  fWidth := aWidth;
  fHeight := aHeight;
end;

destructor TloSimpleBox.Destroy;
begin
  inherited Destroy;
end;

procedure TloSimpleBox.Paint(aCanvas: TCanvas);
begin
  aCanvas.Rectangle(fX, fY, fX + fWidth, fY + fHeight);
end;

procedure TloSimpleBox.Select(aPoint: TPoint);
begin
  fSelected := True;
  fDragPoint := aPoint;
end;

procedure TloSimpleBox.UnSelect(aPoint: TPoint);
var
  lWidth: Integer;
  lHeight: Integer;
begin
  fSelected := False;
  // fY := fY + aPoint.y - fDragPoint.y;
  // fX := fX + aPoint.x - fDragPoint.x;
end;

procedure TloSimpleBox.Drag(aPoint: TPoint);
begin
  fY := fY + aPoint.y - fDragPoint.y;
  fX := fX + aPoint.x - fDragPoint.x;
end;

function TloSimpleBox.HitTest(aPoint: TPoint): Boolean;
begin
  //function Point(AX, AY: Integer): TPoint;
  //function SmallPoint(AX, AY: SmallInt): TSmallPoint;
  //function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
  //function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;

    if (aPoint.x >= fX) and (aPoint.x < fX + fWidth) and
       (aPoint.y >= fY) and (aPoint.y < fY + fHeight) then
      Result := True
    else
      Result := False;
end;

{ TloDataModelPanel }

function TloDataModelPanel.GetBoxAt(X, Y: Integer): TloSimpleBox;
var
  lSimpleBox: TloSimpleBox;
  lOwner: TScrollingWinControl;
  lScreenPoint: TPoint;
  lModelPoint: TPoint;
begin
  Result := nil;

  for lSimpleBox in SimpleBoxList do
  begin
    if lSimpleBox.HitTest(Point(X,Y)) then
    begin
      Result := lSimpleBox;
      Exit;
    end;
  end;

end;

constructor TloDataModelPanel.Create(TheOwner: TComponent);
//var
//  lOwner: TScrollingWinControl;
begin
  inherited Create(TheOwner);

  fGridOptions := TloGridOptions.Create;
  fSimpleBoxList := TloSimpleBoxList.Create;
  fSelectionList := TloSimpleBoxList.Create;

  //if Owner is TScrollingWinControl then
  //begin
  //  lOwner := TScrollingWinControl(Owner);
  //
  //  if Self.Width < lOwner.ClientWidth then
  //    Self.Width := lOwner.ClientWidth;
  //  if Self.Height < lOwner.ClientHeight then
  //    Self.Width := lOwner.ClientHeight;
  //end;
end;

destructor TloDataModelPanel.Destroy;
var
  lSimpleBox: TloSimpleBox;
begin
  fGridOptions.Free;
  fSelectionList.Free;

  for lSimpleBox in fSimpleBoxList do
    lSimpleBox.Free;

  fSimpleBoxList.Free;

  inherited Destroy;
end;

procedure TloDataModelPanel.Paint;
var
  lGridX: Integer;
  lGridY: Integer;
  lSimpleBox: TloSimpleBox;
begin
  inherited Paint;

    if Self.GridOptions.ShowGrid then
    begin
      { Paint the grid background}

      Self.Canvas.Pen.Color := Self.GridOptions.GridColor;

      {psSolid = FPCanvas.psSolid;
      psDash = FPCanvas.psDash;
      psDot = FPCanvas.psDot;
      psDashDot = FPCanvas.psDashDot;
      psDashDotDot = FPCanvas.psDashDotDot;
      psClear = FPCanvas.psClear;
      psInsideframe = FPCanvas.psInsideframe;
      psPattern = FPCanvas.psPattern;}

      Self.Canvas.Pen.Style := Self.GridOptions.GridStyle;

      lGridX := Self.GridOptions.GridWidth;

      while lGridX < Self.Width do
      begin
        Self.Canvas.Line(
          lGridX, 0,
          lGridX, Self.Height);
        lGridX := lGridX + Self.GridOptions.GridWidth;
      end;

      lGridY := Self.GridOptions.GridHeight;

      while lGridY < Self.Height do
      begin
        Self.Canvas.Line(
          0, lGridY,
          Self.Width, lGridY);
        lGridY := lGridY + Self.GridOptions.GridHeight;
      end;
    end;

  for lSimpleBox in SimpleBoxList do
    lSimpleBox.Paint(Self.Canvas);
end;

procedure TloDataModelPanel.Resize;
//var
//  lOwner: TScrollingWinControl;
begin
  inherited Resize;
//
//  if Owner is TScrollingWinControl then
//  begin
//    lOwner := TScrollingWinControl(Owner);
//
//    if Self.Width < lOwner.ClientWidth then
//      Self.Width := lOwner.ClientWidth;
//    if Self.Height < lOwner.ClientHeight then
//      Self.Width := lOwner.ClientHeight;
//  end;
end;

procedure TloDataModelPanel.Click;
begin
  inherited Click;
end;

procedure TloDataModelPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lSimpleBox: TloSimpleBox;
  lDragPoint,
  lScreenPoint,
  lModelPoint: TPoint;
  lOwner: TScrollingWinControl;
begin
  inherited;

  // find the target that was clicked!
  if Button = mbLeft then
  begin
    lSimpleBox := GetBoxAt(X, Y);

    if Assigned(lSimpleBox) then
    begin
      lDragPoint := Point(
        x - lSimpleBox.X,
        y - lSimpleBox.Y);

      lSimpleBox.Select(lDragPoint);
      fSelectionList.Add(lSimpleBox);
      fDragging := True;
    end;
  end;
end;

procedure TloDataModelPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lSimpleBox: TloSimpleBox;
  lOwner: TScrollingWinControl;
  lScreenPoint,
  lModelPoint,
  lDragPoint: TPoint;
begin
  inherited;

  if fDragging and (fSelectionList.Count > 0) then
  begin
    for lSimpleBox in fSelectionList do
    begin
      lDragPoint := Point(
        x - lSimpleBox.X,
        y - lSimpleBox.Y);

      lSimpleBox.Drag(lDragPoint);
      Refresh;
    end;
  end;
end;

procedure TloDataModelPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lSimpleBox: TloSimpleBox;
  lOwner: TScrollingWinControl;
  lScreenPoint,
  lModelPoint,
  lDragPoint: TPoint;
begin
  inherited;

  // find the target that was clicked!
  if Button = mbLeft then
  begin
    if fDragging and (fSelectionList.Count > 0) then
    begin
      lDragPoint := Point(
        x - lSimpleBox.X,
        y - lSimpleBox.Y);

      for lSimpleBox in fSelectionList do
      begin
        lSimpleBox.UnSelect(lDragPoint);
        fSelectionList.Remove(lSimpleBox);
      end;
      fDragging := False;
    end;
  end;
end;

procedure TloDataModelPanel.MouseEnter;
begin
  inherited;
  fDragging := False;
end;

procedure TloDataModelPanel.MouseLeave;
begin
  inherited;
  if fDragging then
    fDragging := False;

  // do the drop routine
end;

{ TloGridOptions }

constructor TloGridOptions.Create;
begin
  inherited Create;

  fGridWidth := 100;
  fGridHeight := 100;
  fGridColor := clBlue;
  fGridStyle := psDot;
  fShowGrid := True;
end;

destructor TloGridOptions.Destroy;
begin
  inherited Destroy;
end;

end.
