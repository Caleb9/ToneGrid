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

ToneGrid

peter.karasinski@gmail.com
https://sourceforge.net/projects/tonegrid/


What is this?
=============
Program is inspired by Tone Matrix as can be seen for example here
http://www.youtube.com/watch?v=OZFMpWKTAjY . On the Y axis we have
tones of a musical scale, on the X axis we have a timeline. In other
words it is a simplistic version of an audio sequencer. In my
implementation, the sound is synthesized as a single sine wave of
given frequency (fundamental frequency of a tone), therefore it sounds
very minimalistic. On the other hand it extends the basic idea by
several features, like control over scale mode, size, beats-per-minute
etc.

The program has been written as an exercise in Free Pascal / Lazarus
and should be regarded as a pre-alpha release. Right now user needs to
build the project in Lazarus IDE himself. An installer will possibly
be added in the future.


Supported platforms:
====================
Program was developed entirely, and works in Linux environment. It has
been successfully compiled for 32bit Windows as well. It should be
possible to compile it also for Mac OS X (without MIDI support), as
Lazarus and OpenAL (see below) are both available for this platform,
and the code is completely cross-platform, however I did not have
access to Mac to test it.

How to compile?
===============
Program is written and designed in Lazarus IDE (a cross-platform, open
source emulator of Delphi). Lazarus can be obtained from
http://www.lazarus.freepascal.org.  It should be fairly easy to
convert the project to Delphi, as Lazarus is almost completely
compatible with it. In current implementation of ToneGrid, there is
only one data structure specific for Lazarus/Free Pascal
(TFPObjectHashTable, which in Delphi could be replaced with
TDictionary).

The application is comprised of two parts:
1) 'package' which contains Notes unit (all the musical theory
   part is implemented there), GraphicGrid, which is a custom
   graphical component used to draw the tone grid, and PasMidi which
   is the unit responsible for MIDI support and working with Notes.
2) 'project' which contains the application's main form and
   project files.

To compile 'project', the user needs to install the 'package' in
Lazarus IDE first. The process is analogous to Delphi's, and this
short tutorial might be helpful
http://wiki.lazarus.freepascal.org/Install_Packages

Next the project-file (tonegridproject.lpi) has to be opened and built
in Lazarus.


How to run?
===========
To synthesize sound, program makes use of OpenAL free software
cross-platform audio API. OpenAL for different operating systems can
be obtained from
http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx
and needs to be present on the client's computer. In a more
professional scenario, I would prepare an installation package which
would install OpenAL silently on Windows, or require a dependency
package on Linux, however it would be an overkill in current case.
On Linux and Windows, the MIDI support has been added, for external
and software synthesizers (currently, the sound card's internal
synthesizers are not supported). 


Troubleshooting:
================
In case of audible glitches or other problems, please first of all try
to install OpenAL from the installer downloaded from
http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx
and, if in Windows, remove the OpenAL32.dll file from the ToneMatrix
project directory. In Linux the installation of OpenAL is dependent on
the distro. Do not hesitate to email me for support.


List of files:
==============
ToneGrid/
|-- package			(Package containing units and components used in the program)
|   |-- cmidi.c
|   |-- cmidi.h			(C low-level MIDI functions for Linux)
|   |-- graphicgrid.inc
|   |-- graphicgrid.pas		(GraphicGrid custom component)
|   |-- notes.inc
|   |-- notes.pas
|   |-- openal.pas		(OpenAL Delphi bindings from http://www.noeska.com/doal/)
|   |-- pasmidi.pp
|   |-- TGraphicGrid.png
|   |-- tonegridpkg.lpk
|   |-- tonegridpkg.lrs
|   `-- tonegridpkg.pas
|-- project			(Main application project)
|   |-- preferencesunit.lfm
|   |-- preferencesunit.lrs
|   |-- preferencesunit.pas
|   |-- preferencesunit.ppu
|   |-- tonegridproject.ico
|   |-- tonegridproject.lpi
|   |-- tonegridproject.lpr
|   |-- tonegridproject.lrs
|   |-- tonegridunit.lfm
|   |-- tonegridunit.lrs
|   |-- tonegridunit.pas
`-- README.txt			(this file)

Enjoy! ;)
