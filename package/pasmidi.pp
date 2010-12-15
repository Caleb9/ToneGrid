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

PasMidi unit declares platform dependend MIDI calls.
}

unit PasMidi;

{$mode delphi}

interface

{$ifdef linux}

{$link cmidi.o}
{$linklib c}

uses CTypes;

function cMidiInit(Ignored : CTypes.CUChar = 0) : CTypes.CInt32; cdecl; external;

function cMidiExit : CTypes.CInt32; cdecl; external;

function cMidiMidiCount(MidiCount : CTypes.PCUChar) : CTypes.CInt32; cdecl;
         external;

function cMidiSynthCount(SynthCount : CTypes.PCUChar) : CTypes.CInt32; cdecl;
         external;

function cMidiMidiName(Device : CTypes.CUChar; Name : PChar) : CTypes.CInt32;
         cdecl; external;

function cMidiNoteOn(KeyNumber : CTypes.CUChar; Velocity : CTypes.CUChar;
                     Device : CTypes.CUChar) : CTypes.CInt32; cdecl; external;

function cMidiNoteOff(KeyNumber : CTypes.CUChar;(* Velocity : CTypes.CUChar; *)
                      Device : CTypes.CUChar) : CTypes.CInt32; cdecl; external;

function cMidiAllSoundsOff(Device : CTypes.CUChar) : CTypes.CInt32; cdecl;
         external;

function cMidiAllNotesOff(Device : CTypes.CUChar) : CTypes.CInt32; cdecl;
         external;

const
   cMidiNameLength =  30;

implementation

{$else} // {$ifndef Linux}
{$ifdef Win32}

uses Windows, MMSystem;

function cMidiInit(Device : Byte) : Integer;
function cMidiExit : Integer;
function cMidiMidiCount(MidiCount : PByte) : Integer;
function cMidiSynthCount(SynthCount : PByte) : Integer;
function cMidiMidiName(const Ignored : Byte; out Name : string) : Integer;
function cMidiNoteOn(const KeyNumber : Byte; const Velocity : Byte;
                      const Ignored : Byte = 0) : Integer;
function cMidiNoteOff(const KeyNumber : Byte;(* const Velocity : Byte; *)
                       const Ignored : Byte = 0) : Integer;
function cMidiAllSoundsOff(const Ignored : Byte) : Integer;
function cMidiAllNotesOff(const Ignored : Byte = 0) : Integer;
const
   cMidiNameLength =  30;

var
  OutHandle : HMidiOut;

implementation

function cMidiInit(Device : Byte) : Integer;
begin
  Result := midiOutOpen(@OutHandle, Device, 0, 0, CALLBACK_NULL);
end;

function cMidiExit : Integer;
begin
  Result := midiOutClose(OutHandle);
end;

function cMidiMidiCount(MidiCount : PByte) : Integer;
begin
  MidiCount^ := midiOutGetNumDevs();
  Result := 0;
end;

{ ??? TODO }
function cMidiSynthCount(SynthCount : PByte) : Integer;
begin
  SynthCount^ := 0;
  Result := 0;
end;

function cMidiMidiName(const Ignored : Byte; out Name : string) : Integer;
var
  MidiInfo : MidiOutCaps;
begin
  Result := midiOutGetDevCaps(Ignored, @MidiInfo, sizeof(MIDIOUTCAPS));
  Name := MidiInfo.szPname;
end;

function cMidiNoteOn(const KeyNumber : Byte; const Velocity : Byte;
                     const Ignored : Byte) : Integer;
var
  Msg : DWord;
begin
  Msg := (Velocity << 16) or (KeyNumber << 8) or $90;
  Result := midiOutShortMsg(OutHandle, Msg);
end;

function cMidiNoteOff(const KeyNumber : Byte;(* const Velocity : Byte; *)
                      const Ignored : Byte) : Integer;
var
  Msg : DWord;
begin
  //Msg := (Velocity << 16) or (KeyNumber << 8) or $80;
  Msg := (KeyNumber << 8) or $90;
  Result := midiOutShortMsg(OutHandle, Msg);
end;

function cMidiAllSoundsOff(const Ignored : Byte) : Integer;
var
  Msg : DWord;
begin
  Msg :=$78B0;
  Result := midiOutShortMsg(OutHandle, Msg);
end;

function cMidiAllNotesOff(const Ignored : Byte) : Integer;
var
  Msg : DWord;
begin
  Msg := $7BB0;
  Result := midiOutShortMsg(OutHandle, Msg);
end;

{$else} //{$ifndef Win32} and {$ifndefLinux} - unknown OS, empty functions

function cMidiInit(Ignored : Byte = 0) : Integer;
function cMidiExit : Integer;
function cMidiMidiCount(MidiCount : PByte) : Integer;
function cMidiSynthCount(SynthCount : PByte) : Integer;
function cMidiMidiName(const Device : Byte; Name : PChar) : Integer;
function cMidiNoteOn(const KeyNumber : Byte; const Velocity : Byte;
                     const Ignored : Byte) : Integer;
function cMidiNoteOff(const KeyNumber : Byte; const Velocity : Byte;
                      const Ignored : Byte) : Integer;
function cMidiAllSoundsOff(const Ignored : Byte) : Integer;
function cMidiAllNotesOff(const Ignored : Byte) : Integer;
const
   cMidiNameLength =  30;

implementation

function cMidiInit(Ignored : Byte) : Integer;
begin
  Result := 0;
end;

function cMidiExit : Integer;
begin
  Result := 0;
end;

function cMidiMidiCount(MidiCount : PByte) : Integer;
begin
  MidiCount^ := 0;
  Result := 0;
end;

function cMidiSynthCount(SynthCount : PByte) : Integer;
begin
  SynthCount^ := 0;
  Result := 0;
end;

function cMidiMidiName(const Device : Byte; Name : PChar) : Integer;
begin
  Name := nil;
  Result := 0;
end;

function cMidiNoteOn(const KeyNumber : Byte; const Velocity : Byte;
                     const Ignored : Byte) : Integer;
begin
  Result := 0;
end;

function cMidiNoteOff(const KeyNumber : Byte; const Velocity : Byte;
                      const Ignored : Byte) : Integer;
begin
  Result := 0;
end;

function cMidiAllSoundsOff(const Ignored : Byte) : Integer;
begin
  Result := 0;
end;

function cMidiAllNotesOff(const Ignored : Byte) : Integer;
begin
  Result := 0;
end;

{$endif} // {$ifdef Win32}
{$endif} // {$ifdef Linux}

end.
