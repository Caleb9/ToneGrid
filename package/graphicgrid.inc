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
}

{ TGraphicGrid }

procedure TGraphicGrid.SetSizeX(NewSize: Integer);
begin
  if NewSize <> SizeX then
  begin
    FSizeX := NewSize;
    FSizeCellX := Width / SizeX;

    { Reset length of the FCells array to new size }
    SetLength(FCells, SizeX, SizeY);

    Invalidate; // redraw component
  end;
end;

procedure TGraphicGrid.SetSizeY(NewSize: Integer);
var
  TempCells : TBooleanMatrix;
  X, Y, TempY : Integer;
begin
  if NewSize <> SizeY then
  begin
    if AddCellsFromTop then
      TempCells := Copy(FCells); // copy the current FCells

    FSizeY := NewSize;
    FSizeCellY := Height / SizeY;

    SetLength(FCells, SizeX, SizeY);

    if AddCellsFromTop then
    begin
      { Copy contents of TempCells into new FCells - this way we extend/shrink
        the array "from the top", preserving cells configuration on the bottom
        of the grid. This is useful in ToneGrid application where we set the
        bottom note, and manipulate the Y size adding/removing rows from the top.}
      for X := 0 to SizeX-1 do
      begin
        TempY := High(TempCells[X]);
        for Y := SizeY-1 downto 0 do
        begin
          if TempY >= 0 then
          begin
            FCells[X,Y] := TempCells[X,TempY];
            Dec(TempY);
          end
          else
            { Set new rows to be empty }
            FCells[X,Y] := false;
        end;
      end;
    end;

    Invalidate;
  end;
end;

procedure TGraphicGrid.SetColorBackground(NewColor: TColor);
begin
  if NewColor <> ColorBackground then
  begin
    FColorBackground := NewColor;
    Invalidate;
  end;
end;

procedure TGraphicGrid.SetColorGrid(NewColor: TColor);
begin
  if NewColor <> ColorGrid then
  begin
    FColorGrid := NewColor;
    Invalidate;
  end;
end;

procedure TGraphicGrid.SetColorCellSet(NewColor: TColor);
begin
  if NewColor <> ColorCellSet then
  begin
    FColorCellSet := NewColor;
    Invalidate;
  end;
end;

procedure TGraphicGrid.SetPenWidth(NewWidth: Integer);
begin
  if NewWidth <> PenWidth then
  begin
    FPenWidth := NewWidth;
    Invalidate;
  end;
end;

procedure TGraphicGrid.SetBorderCell(NewSize: Integer);
begin
  if NewSize <> BorderCell then
  begin
    FBorderCell := NewSize;
    Invalidate;
  end;
end;

procedure TGraphicGrid.Paint;
var
  X, Y : Integer;
  Border : Integer;
begin
  { draw grid and set cells }
  Canvas.Pen.Width := PenWidth;
  Canvas.Pen.Color := ColorGrid;
  Border := Ceil((PenWidth / 2) + BorderCell);
  for X := 0 to SizeX-1 do
    for Y := 0 to SizeY-1 do
    begin
      { draw grid }
      Canvas.Brush.Color := ColorBackground;
      Canvas.Rectangle(Round(X * FSizeCellX),
                       Round(Y * FSizeCellY),
                       Round(X * FSizeCellX + FSizeCellX),
                       Round(Y * FSizeCellY + FSizeCellY));
      { fill cell }
      if FCells[X, Y] then
      begin
        Canvas.Brush.Color := ColorCellSet;
        Canvas.FillRect(Ceil(X * FSizeCellX + Border),
                        Ceil(Y * FSizeCellY + Border),
                        Floor(X * FSizeCellX - Border + FSizeCellX),
                        Floor(Y * FSizeCellY - Border + FSizeCellY));
      end;
    end;

  inherited Paint; // dispatches OnPaint event
end;

procedure TGraphicGrid.Resize;
begin
  FSizeCellX := Width / SizeX;
  FSizeCellY := Height / SizeY;

  inherited Resize; // dispatches OnResize event
end;

procedure TGraphicGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  IndexX, IndexY : Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  IndexX := XToIndex(X);
  IndexY := YToIndex(Y);

  { fill/clear the cell }
  case Button of
    mbLeft :
      FCells[IndexX, IndexY] := true;
    mbRight :
      FCells[IndexX, IndexY] := false;
  end;

  { dispatch TGridMouseEvent custom event }
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y, IndexX, IndexY);

  Invalidate;
end;

procedure TGraphicGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  { dispatch event }
  if Assigned(OnMouseUp) then
    OnMouseUp(Self, Button, Shift, X, Y, XToIndex(X), YToIndex(Y));
end;

procedure TGraphicGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, X, Y, XToIndex(X), YToIndex(Y));
end;

constructor TGraphicGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { initialize default field values }
  FSizeX := DefaultSize;
  FSizeY := DefaultSize;
  FAddCellsFromTop := DefaultAddFromTop;
  FColorBackground := DefaultColorBackground;
  FColorGrid := DefaultColorGrid;
  FColorCellSet := DefaultColorCellSet;
  FPenWidth := DefaultPenWidth;
  FBorderCell := DefaultPenWidth;
  Width := 50;
  Height := 50;

  { Initialize FCells array, by default the values are set to 'false' }
  SetLength(FCells, FSizeX, FSizeY);
end;

function TGraphicGrid.IsSet(IndexX, IndexY: Integer): Boolean;
begin
  Result := FCells[IndexX, IndexY];
end;

function TGraphicGrid.XToIndex(X: Integer): Integer;
var
  Index : Integer;
begin
  Index := Round((X - (X mod Round(FSizeCellX))) / FSizeCellX);
  if (0 <= Index) and (Index < SizeX) then
    Result := Index
  else
    Result := -1;
end;

function TGraphicGrid.YToIndex(Y: Integer): Integer;
var
  Index : Integer;
begin
  Index := Round((Y - (Y mod Round(FSizeCellY))) / FSizeCellY);
  if (0 <= Index) and (Index < SizeY) then
    Result := Index
  else
    Result := -1;
end;

procedure TGraphicGrid.Clear;
var
  X, Y : Integer;
begin
  for X := 0 to SizeX-1 do
    for Y := 0 to SizeY-1 do
      FCells[X, Y] := false;
  Invalidate;
end;

