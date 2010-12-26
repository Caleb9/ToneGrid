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

    email: caleb9@users.sourceforge.net
************************************************************************
}
program tonegridproject;

{$mode objfpc}{$H+}

uses
  // CMem, { Custom added for sharing memory with C library }
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tonegridpkg, LResources, PreferencesUnit, tonegridunit;

{$IFDEF WINDOWS}{$R tonegridproject.rc}{$ENDIF}

begin
  Application.Title:='Tone Grid';
  {$I tonegridproject.lrs}
  Application.Initialize;
  Application.CreateForm(TToneGridMainForm, ToneGridMainForm);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.Run;
end.

