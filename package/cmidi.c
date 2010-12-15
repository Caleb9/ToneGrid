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

    email: peter.karasinski@gmail.com
************************************************************************/

#include "cmidi.h"

#ifdef linux

#include <linux/soundcard.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

int32_t seqfd;

int32_t cMidiInit(unsigned char ignored) {
  char* device = "/dev/sequencer";
  /* open the OSS device for writing */
  return (seqfd = open(device, O_WRONLY));
}

int32_t cMidiExit(void) {
  return close(seqfd);
}

int32_t cMidiMidiCount(unsigned char* midiCount) {
  return ioctl(seqfd, SNDCTL_SEQ_NRMIDIS, midiCount);
}

int32_t cMidiSynthCount(unsigned char* synthCount) {
  return ioctl(seqfd, SNDCTL_SEQ_NRSYNTHS, synthCount);
}

int32_t cMidiMidiName(unsigned char device, char* name) {
  struct midi_info midiInfo;
  midiInfo.device = device;
  int32_t retval = ioctl(seqfd, SNDCTL_MIDI_INFO, &midiInfo);
  strcpy(name, midiInfo.name);
  return retval;
}

/*
 * Functions wrapping specific midi messages. See
 * http://www.midi.org/techspecs/midimessages.php for description of
 * the messages.
 */
ssize_t cMidiNoteOn(unsigned char keyNumber, unsigned char velocity, unsigned char device) {
  unsigned char packet[12] = {SEQ_MIDIPUTC, 0x90, device, 0,      /* note-on MIDI command */
			      SEQ_MIDIPUTC, keyNumber, device, 0, /* kenumber (e.g. 60 = middle c) */
			      SEQ_MIDIPUTC, velocity, device, 0}; /* attack velocity (127 = loud) */
  return write(seqfd, packet, sizeof(packet));
}

/*
 * In this function we could use 0x80 note-off midi message, but then
 * we need to know velocity of the note. Putting velocity down to 0
 * like we do here, allows us to dynamically adjust velocities of
 * notes.
 */
ssize_t cMidiNoteOff(unsigned char keyNumber, /*unsigned char velocity,*/ unsigned char device) {
  unsigned char packet[12] = {SEQ_MIDIPUTC, 0x90, device, 0,      /* note-on MIDI command */
			      SEQ_MIDIPUTC, keyNumber, device, 0, /* kenumber (e.g. 60 = middle c) */
			      SEQ_MIDIPUTC, 0x00, device, 0};     /* puts velocity down to 0 */
  return write(seqfd, packet, sizeof(packet));
}

ssize_t cMidiAllSoundsOff(unsigned char device) {
  unsigned char packet[12] = {SEQ_MIDIPUTC, 0xB0, device, 0,      /* channel mode message */
			      SEQ_MIDIPUTC, 0x78, device, 0,      /* All sounds off */
			      SEQ_MIDIPUTC, 0, device, 0};
  return write(seqfd, packet, sizeof(packet));
}

ssize_t cMidiAllNotesOff(unsigned char device) {
  unsigned char packet[12] = {SEQ_MIDIPUTC, 0xB0, device, 0,      /* channel mode message */
			      SEQ_MIDIPUTC, 0x7B, device, 0,      /* All notes off */
			      SEQ_MIDIPUTC, 0, device, 0};
  return write(seqfd, packet, sizeof(packet));
}

#elif defined _WIN32

// TODO

#endif
