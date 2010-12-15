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
*************************************************************************
}

unit PreferencesUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ActnList, ExtCtrls, StdCtrls,
  { custom added }
  PasMidi,
  OpenAl;

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
  public
    { public declarations }
    property Device : Integer
      read FDevice;
  end; 

var
  PreferencesForm: TPreferencesForm;

implementation

{ TPreferencesForm }

procedure TPreferencesForm.FormCreate(Sender: TObject);
var
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
  ArgV : array of PALbyte = nil;
begin
  { Set icon for exit button }
  ImageList1.GetBitmap(0, ExitBtn.Glyph);

  { Populate list of devices }
  DeviceListBox.Style := lbOwnerDrawFixed;
  DeviceListBox.ItemHeight := 20;
  DeviceListBox.Items.Add('Internal synthesizer (OpenAl)');
  { On Linux we need to open the "/dev/sequencer" before we can query for
    devices }

  {$ifdef Linux}
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
  DeviceListBox.ItemIndex := 0;

  { By default select the OpenAl synthesizer }
  FDevice := -1;
  { initialize OpenAL }
  InitOpenAL();
  AlUtInit(nil, ArgV);
end;

procedure TPreferencesForm.FormDestroy(Sender: TObject);
begin
  { Close MIDI }
  cMidiExit;
  { Close OpenAL }
  AlUtExit;
end;

procedure TPreferencesForm.ExitBtnClick(Sender: TObject);
begin
  self.Hide;
end;

procedure TPreferencesForm.DeviceListBoxSelectionChange(Sender: TObject;
  User: boolean);
var
  ArgV : array of PALbyte = nil;
begin
  if FDevice <> (DeviceListBox.ItemIndex - 1) then
  begin
    FDevice := (DeviceListBox.ItemIndex - 1);
    if FDevice = -1 then
    begin
      { Close possibly opened MIDI device }
      cMidiExit;
      { initialize OpenAL }
      InitOpenAL();
      AlUtInit(nil, ArgV);
    end
    else
    begin
      { Close OpenAL }
      AlUtExit;
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

