{
*************************************************************************
    Copyright (C) 2010  Piotr Karasinski

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    email: peter.karasinski@gmail.com
************************************************************************

  Unit defines a visual component TGraphicGrid used in ToneGrid.
}
unit GraphicGrid;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Math;

const
  DefaultSize = 3;
  DefaultPenWidth = 3;
  DefaultColorBackground = clWhite;
  DefaultColorGrid = clBlack;
  DefaultColorCellSet = clRed;
  DefaultAddFromTop = true;

type

  { Custom mouse event types - they provide the same kind of information
    as regular mouse event types, extended by index of a cell in the grid }
  TGridMouseEvent = procedure(Sender : TObject;
                              Button : TMouseButton;
                              Shift : TShiftState;
                              X, Y, IndexX, IndexY : Integer) of object;
  TGridMouseMoveEvent = procedure(Sender : TObject;
                                  Shift : TShiftState;
                                  X, Y, IndexX, IndexY : Integer) of object;

  TBooleanMatrix = array of array of Boolean;

  { TGraphicGrid
    Main graphic component used in ToneGrid app. User can set/switch-on
    fields (called "cells" later on) with left mouse button, and
    unset/clear/switch-off with right mouse button }

  TGraphicGrid = class(TGraphicControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FSizeX, FSizeY : Integer; // number of cells in each dimension
    { Two dimensional array to hold set/unset cell values }
    FCells : TBooleanMatrix;
    FSizeCellX, FSizeCellY : Double; // sizes of cells
    { If set to true, cells are added from the top to the FCells array,
      preserving configuration on the bottom }
    FAddCellsFromTop : Boolean;
    FColorBackground, FColorGrid, FColorCellSet : TColor; // colors
    FPenWidth : Integer;
    { When cell is set, the inner rectangle will be smaller than the cell by
      this much... }
    FBorderCell : Integer;

    { Custom event procedure pointers }
    FOnMouseDown : TGridMouseEvent;
    FOnMouseUp : TGridMouseEvent;
    FOnMouseMove : TGridMouseMoveEvent;
    { sets number of cells in X dimension }
    procedure SetSizeX(NewSize : Integer); virtual;
    { ...same for Y dimension }
    procedure SetSizeY(NewSize : Integer); virtual;
    procedure SetColorBackground(NewColor : TColor); virtual;
    procedure SetColorGrid(NewColor : TColor); virtual;
    procedure SetColorCellSet(NewColor : TColor); virtual;
    { sets thickness of grid }
    procedure SetPenWidth(NewWidth : Integer); virtual;
    { Sets thickness of spacing between grid and a set cell }
    procedure SetBorderCell(NewSize : Integer); virtual;

    { Repainting procedure, here we draw the component }
    procedure Paint; override;
    procedure Resize; override;
    { Custom mouse event handling procedrues }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                        Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
                      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    { Public declarations }

    { We override only the constructor. It's not necessary to override destructor
      since we don't allocate memory dynamically in this component (except for
      FCells dynamic array, which is reference counted and therefore freed
      automatically). }
    constructor Create(AOwner: TComponent); override;

    { Is cell set or not }
    function IsSet(IndexX, IndexY : Integer) : Boolean; virtual;
    { Recomputes mouse X/Y position to index in FCells }
    function XToIndex(X : Integer) : Integer; virtual;
    function YToIndex(Y : Integer) : Integer; virtual;
    { Unsets all the cells }
    procedure Clear; virtual;

    property CellWidth : Double
      read FSizeCellX;
    property CellHeight : Double
      read FSizeCellY;

  published
    { Published declarations }
    property SizeX : Integer
      read FSizeX write SetSizeX default DefaultSize;
    property SizeY : Integer
      read FSizeY write SetSizeY default DefaultSize;
    property AddCellsFromTop : Boolean
      read FAddCellsFromTop write FAddCellsFromTop default DefaultAddFromTop;
    property ColorBackground : TColor
      read FColorBackground write SetColorBackground default DefaultColorBackground;
    property ColorGrid : TColor
      read FColorGrid write SetColorGrid default DefaultColorGrid;
    property ColorCellSet : TColor
      read FColorCellSet write SetColorCellSet default DefaultColorCellSet;
    property PenWidth : Integer
      read FPenWidth write SetPenWidth default DefaultPenWidth;
    property BorderCell : Integer
      read FBorderCell write SetBorderCell default DefaultPenWidth;

    { Publish some of the inherited protected properties of TGraphicControl }
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Constraints;

    { event handlers }
    property OnMouseDown : TGridMouseEvent
      read FOnMouseDown write FOnMouseDown;
    property OnMouseUp : TGridMouseEvent
      read FOnMouseUp write FOnMouseUp;
    property OnMouseMove : TGridMouseMoveEvent
      read FOnMouseMove write FOnMouseMove;
    property OnClick;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaint;
    property OnResize;
  end;

procedure Register;

implementation

{ Registers TGraphicGrid in Custom tab in IDE }
procedure Register;
begin
  RegisterComponents('Custom',[TGraphicGrid]);
end;

{ TGraphicGrid }
{$I graphicgrid.inc}

initialization
{$I tonegridpkg.lrs} // sets component's icon

end.
