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
*************************************************************************
}

unit PreferencesUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ActnList, ExtCtrls, StdCtrls,
  { custom added }
  PasMidi, DynLibs
{$IFNDEF DARWIN}
  , OpenAl
{$ENDIF}
  ;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    ExitBtn: TBitBtn;
    ImageList1: TImageList;
    Label1: TLabel;
    DeviceListBox: TListBox;
    procedure DeviceListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FDevice : Integer;
    FOpenAlDetected : Boolean;
  public
    function OpenAlSelected : boolean; virtual;

    { public declarations }
    property Device : Integer
      read FDevice;
  end; 

var
  PreferencesForm: TPreferencesForm;

const
  {$ifdef Win32}
    LibOpenAlName = 'openal32.';
  {$else}
    LibOpenAlName = 'libopenal.';
  {$endif}

implementation

{ TPreferencesForm }

procedure TPreferencesForm.FormCreate(Sender: TObject);
var
  LibOpenAlHandle : TLibHandle;
  MidiCount : Byte;
  I : Integer;
  { Unfortunately for now this method has to use compilation conditions - in
    Linux, the MIDI calls are done through C bindings, which are incompatible
    with Windows - that includes treating PChar and string variables. }
  {$ifdef Linux}
    MidiName : PChar = nil;
  {$else}
    MidiName : string;
  {$endif}
begin
  { Set icon for exit button }
  ImageList1.GetBitmap(0, ExitBtn.Glyph);

  { Populate list of devices }
  DeviceListBox.Style := lbOwnerDrawFixed;
  DeviceListBox.ItemHeight := 20;

  {$ifdef Linux}
    { On Linux we need to open the "/dev/sequencer" before we can query for
      devices }
    cMidiInit(0);
    GetMem(MidiName, cMidiNameLength);
  {$endif}
  cMidiMidiCount(@MidiCount);
  for I := 0 to MidiCount-1 do
  begin
    cMidiMidiName(I, MidiName);
    DeviceListBox.Items.Add(MidiName);
  end;
  {$ifdef Linux}
    FreeMem(MidiName);
    cMidiExit;
  {$endif}

  { Detect if OpenAl shared/dynamic library exists }

  FOpenAlDetected := false;
  LibOpenAlHandle := LoadLibrary(LibOpenAlName + SharedSuffix);
  if LibOpenAlHandle <> NilHandle then
  begin
    UnloadLibrary(LibOpenAlHandle);
    FOpenAlDetected := true;
{$IFNDEF DARWIN}
    InitOpenAL();
{$ENDIF}
    DeviceListBox.Items.Add('Internal synthesizer (OpenAl)');
  end;

  { By default select the first available device }
  if DeviceListBox.Items.Count > 0 then
    DeviceListBox.ItemIndex := 0;
  FDevice := -1;
  self.DeviceListBoxSelectionChange(Sender, false);
end;

procedure TPreferencesForm.FormDestroy(Sender: TObject);
begin
  { Close MIDI }
  cMidiExit;
  { Close OpenAL }
{$IFNDEF DARWIN}
  if FOpenAlDetected then
    AlUtExit;
{$ENDIF}
end;

function TPreferencesForm.OpenAlSelected: boolean;
begin
  if FOpenAlDetected and
     (FDevice = (DeviceListBox.Items.Count - 1)) then
    Result := true
  else
    Result := false;
end;

procedure TPreferencesForm.ExitBtnClick(Sender: TObject);
begin
  self.Hide;
end;

procedure TPreferencesForm.DeviceListBoxSelectionChange(Sender: TObject;
  User: boolean);
{$IFNDEF DARWIN}
var
  ArgV : array of PALbyte = nil;
{$ENDIF}
begin
  if FDevice <> DeviceListBox.ItemIndex then
  begin
    FDevice := DeviceListBox.ItemIndex;
    if self.OpenAlSelected then
    begin
      { Close possibly opened MIDI device }
      cMidiExit;
      { initialize OpenAL }
{$IFNDEF DARWIN}
      AlUtInit(nil, ArgV);
{$ENDIF}
    end
    else
    begin
      { Close OpenAL }
{$IFNDEF DARWIN}
      if FOpenAlDetected then
        AlUtExit;
{$ENDIF}
      { Close previously selected MIDI device }
      cMidiExit;
      { Open new MIDI device }
      cMidiInit(FDevice);
    end;
  end;
end;

initialization
  {$I preferencesunit.lrs}

end.
