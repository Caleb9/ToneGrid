/************************************************************************
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
************************************************************************/

#ifndef CMIDI_H
#define CMIDI_H

#include <inttypes.h>
#include <unistd.h>

/*
 * Opens /dev/sequencer in writing mode. This has to be called before
 * any of the other functions can be used. ignored parameter is there
 * just to keep compatible with Windows version of the function.
 */
int32_t cMidiInit(unsigned char ignored);

/*
 * Closes /dev/sequencer
 */
int32_t cMidiExit(void);

/*
 * Sets midiCount to the number of available EXTERNAL midi devices.
 */
int32_t cMidiMidiCount(unsigned char* midiCount);

/*
 * Sets synthCount to the number of available INTERNAL synthesisers.
 */
int32_t cMidiSynthCount(unsigned char* synthCount);

/*
 * Sets name to the name of specific device
 */
int32_t cMidiMidiName(unsigned char device, char* name);

/*
 * Writes the note-on message to *external* MIDI output 
 */
ssize_t cMidiNoteOn(unsigned char keyNumber, unsigned char velocity, unsigned char device);

/* 
 * Writes the note-off message
 */
ssize_t cMidiNoteOff(unsigned char keyNumber, /* unsigned char velocity,*/ unsigned char device);


ssize_t cMidiAllNotesOff(unsigned char device);

#endif
