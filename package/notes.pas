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

  Unit defines music notation elements and actual sound handling in ToneGrid.
  This unit uses OpenAL audio API to communicate with sound card.
}
unit Notes;

{$mode delphi}


interface

uses
  Classes, SysUtils, OpenAl, Math, Contnrs,
  { custom added }
  PasMidi;

const
  { For simplicity we'll use a fixed sampling rate }
  DefaultSampleRate = 44100;

  { Limitations for scales octave designation - B9's frequency is 15804.16,
    therefore B10 (B9 * 2) would be beyond Nyquist frequency (and beyond human
    ear capabilities). For simplicity, we limit upper octave designation to 9. }
  MinOctave = 1;
  DefaultOctave = 4;
  MaxOctave = 9;

type

IAbstractSource = interface['{F4199078-93CC-4944-AB00-0C6C74C67C44}']
    procedure Play;
    procedure Stop;
    procedure SetGain(const NewGain : Double);
    property Gain : Double
      //read GetGain
      write SetGain;
end;

{ TEmptySource
  Used if no MIDI device nor OpenAl shared library was detected}

TEmptySource = class(TInterfacedObject, IAbstractSource)
  protected
    procedure SetGain(const NewGain : Double); virtual;
  public
    procedure Play; virtual;
    procedure Stop; virtual;
    procedure Pause; virtual;
end;


{ TOpenAlSource
  Tones of specific frequency and duration.
  Tones are generated with sine waves, and played with OpenAL audio API. To use
  this class, OpenAL needs to be initialized already. }

TOpenAlSource = class(TInterfacedObject, IAbstractSource)
  protected
    FFreq : TALfloat;             // frequency of the tone (in Hz)
    FSampleRate : TALSizeI;       // sampling rate
    FSize : TALSizeI;             // length of the signal (in samples)
    FBuffer, FSource : TALuint;   // OpenAL's buffer and source handlers
    FGain : TALFloat;             // amplification of the signal (0.0 .. 1.0)
    FFormat : TALenum;            // OpenAL's sound data format

    { method generates a sine wave of length 'FSize' samples in the given array }
    procedure GenerateSine(out Sine : array of SmallInt); virtual;

    { set attack time (in samples) }
    procedure SetAttack(var Sine : array of SmallInt; Len : TALSizeI); virtual;

    { set release time (in samples) }
    procedure SetRelease(var Sine : array of SmallInt; Len : TALSizeI); virtual;

    procedure SetGain(const NewGain : Double); virtual;

  public
    constructor Create(Freq : TALFloat;
                       Size : TALSizeI; // desired signal length in samples
                       SampleRate : TALSizeI = DefaultSampleRate;
                       Attack : TALSizeI = 0;  // in samples
                       Release : TALSizeI = 0); virtual; // in samples
    destructor Destroy; override;

    procedure Play; virtual;
    procedure Stop; virtual;
    procedure Pause; virtual;

    //property Gain : TALFloat
    //  //read FGain
    //  write SetGain;
end;

{ Names for natural pitch classes (C, D, E, F, G, A, B) }
TPitchClass = (pC, pD, pE, pF, pG, pA, pB);

{ Accidentals / chromatic alterations }
TAccidental = (acDoubleFlat, acFlat, acNatural, acSharp, acDoubleSharp);

{ TNote
  Class representing a pitch, deriving from TPersistent to provide the Assign
  copy method. }

TNote = class(TPersistent)
  protected
    FPitchClass : TPitchClass;
    FAccidental : TAccidental;
    FOctave : Byte; // Octave designation

    { Calculates frequency of this pitch in Hz }
    function GetFrequency : Single; virtual;

  public
    { Constructor is overloaded so we can still call for ancestor's (TObject's
      in this case) paremeterless constructor (to populate array of notes in
      TScale's constructor with "default" notes). }
    constructor Create(const Name : TPitchClass;
                       const Accidental : TAccidental = acNatural;
                       const Octave : Byte = DefaultOctave); virtual; overload;
    { Copying constructor }
    constructor Create(Source : TNote); virtual; overload;
    { Constructor creating TNote object out of string representation }
    constructor Create(const PitchStr : string); virtual; overload;

    { copies Source properties to 'self' }
    procedure Assign(Source: TPersistent); override;

    function ToString : string; virtual;

    property PitchClass : TPitchClass
      read FPitchClass;
    property Accidental : TAccidental
      read FAccidental write FAccidental;
    property Octave : Byte
      read FOctave write FOctave;
    property Frequency : Single
      read GetFrequency;
end;

{ TNoteOpenAlSource
  This class is a specification of TOpenAlSource for musical notes.
  Instead of frequencies, we use notion of notes as TNote
  objects. Lengths are expressed in milliseconds (instead of samples as in the
  ancestor class). }

TNoteOpenAlSource = class(TOpenAlSource)
  public
    { We're overloading the constructor so that TOpenAlSource.Create is also
      available for TNoteOpenAlSource instances (if someone wanted to explicitly
      use frequency and sample durations }
    constructor Create(Note : TNote;
                       const Duration : Word = 1000; // In milliseconds
                       const Attack : Word = 0;      // In milliseconds
                       const Release : Word = 0);    // In milliseconds
                       virtual; overload;

    { Recalculates duration in milliseconds to amount of samples. }
    class function MillisecondsToSamples(
          const Duration : Word; // In milliseconds
          const SampleRate : TALSizeI = DefaultSampleRate) : TALSizeI; virtual;

end;


{ TMidiSource
  Emits midi messages instead of synthesizing sound. Works in Linux (through
  C bindings) and Windows (through WinAPI). Supports external and software MIDI
  synthesizers. Currently it simply sends note-on/off messages. }

TMidiSource = class(TInterfacedObject, IAbstractSource)
  protected
    { Note midi code }
    FKey : Byte;
    { Midi velocity: 0 - 127 }
    FVelocity : Byte;
    { Device number (MIDI port) }
    FDevice : Byte;
    FChannel : 0..15;

    procedure SetGain(const NewGain : Double); virtual;
  public
    constructor Create(Note : TNote; const Device : Byte); virtual;
    procedure Play; virtual;
    procedure Stop; virtual;
end;


{ Possible modes of a scale
  ??? TODO: add harmonic minor mode }
TScaleMode = (smMajor, smMinor);

{ Used for finding pitch class half- or wholestep above another one.
  ??? TODO: add 3 halfsteps above for harmonic minor mode }
THalfSteps = 1..2;

{ For scale's tonic we allow octaves only within MinOctave .. MaxOctave }
TOctave = MinOctave .. MaxOctave;

{ TScale
  TScale holds list of TNote objects from specific scale, in all possible octaves.
  We'll refer to these notes in the circle of fifths implementation later on }

TScale = class
  protected
    FMode : TScaleMode;
    FPitches : TObjectList;
    { Amount of pitch classes within a scale. 7 for major and minor-natural modes,
      but can be different for other possible modes. }
    FDegrees : Byte;

    { Generates note "HalfSteps"-above the last element on the FPitches list,
      with next TPitchClass name (in other words it takes in consideration
      context of the current scale) }
    function NextHalfSteps(HalfSteps : THalfSteps = 1) : TNote;
             overload; virtual;

    function GetCount : Integer; virtual;
  public
    constructor Create(const PitchClass : TPitchClass;
                       const Accidental : TAccidental;
                       const NewMode : TScaleMode); virtual;
    destructor Destroy; override;
    { Returns scale's tonic note from specific octave }
    function GetTonic(Octave : TOctave) : TNote; virtual;
    { Returns note from the list, indexed from 1 to "Count" }
    function At(Index : Integer) : TNote; virtual;
    { Return index of tonic in specific octave }
    function GetOctaveIndex(Octave : TOctave) : Integer; virtual;
    { Return string representation of amount of accidentals }
    function GetAccidentals : string; virtual;

    property Mode : TScaleMode
      read FMode;
    property Count : Integer
      read GetCount;
    property ScaleDegrees : Byte
      read FDegrees;
end;

{ TCircleOfFifths
  This class generates and holds all the scales (TScale objects) on circle of
  fifths (minor and major) for different key signatures. Scales are indexed by
  mode and tonic pitch (as string, without octave designation). We use one name
  for every key signature, going 6 keys up from C major / A minor(clockwise) on
  circle of fifths, and 6 keys down (counter-clockwise). Therefore some
  key signatures containing the same, enharmonically equivalent pitches as other
  keys are not present (for example there's only B, but no Cb in major keys, as
  they contain the same pitches, just named differently). }

TCircleOfFifths = class
  protected
    { We have an array (indexed by scale modes) of maps (in this case hash tables)
      of scales. Maps will use string representation of scale's tonics as a
      hash-table keys, and TScale objects as values. We could use generics here,
      like TFPGMap<TPitch,TScale> from FGL unit (or TDictionary in Delphi),
      but since they are so far implemented only as a prototype in FPC 2.4.0,
      we'll stick to the old way.}
    FScales : array[TScaleMode] of TFPObjectHashTable;
    { The FKeys array provides list of all possible keys. }
    FKeys : array[TScaleMode] of TStringList;

    { Returns a string representation of TNote, without octave designation. Such
      strings are used as keys in the FScales' TFPObjectHahsTable }
    function GetKey(Pitch : TNote) : string; virtual;
  public
    { Constructor is reintroduced to prevent calling the default TObject's
      constructor. }
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    { Functions retrieving reference to specific scale on the circle of fifths }
    function Scale(Tonic : TNote;
                   const Mode : TScaleMode) : TScale; virtual; overload;
    function Scale(const Key : string;
                   const Mode : TScaleMode) : TScale; virtual; overload;
    function Scale(const TonicPitchClass : TPitchClass;
                   const Accidental : TAccidental;
                   const Octave : TOctave;
                   const Mode : TScaleMode) : TScale; virtual; overload;
    { Returns reference to list of all available hash-table keys }
    function Keys(const Mode : TScaleMode) : TStringList; virtual;
end;

{ Global functions and constants }

{ Returns successor of TPitchClass parameter, wrapping around B note. Used in
  TScale.NextHalfSteps method. }
function NextPitchClass(const PitchClass : TPitchClass) : TPitchClass;

{ Used to sort FKeys string list in TCircleOfFifths.Create according to pitch
  frequency. }
function ComparePitches(List : TStringList; Index1, Index2 : Integer) :  Integer; overload;

const
  { Frequencies of different notes. We use 4th octave's notes as reference.
    Used in TNoteGenerator's constructor. }
  ToneFrequencies : array[TPitchClass] of Single = (261.63, // C4
                                                    293.66, // D4
                                                    329.63, // E4
                                                    349.23, // F4
                                                    392.00, // G4
                                                    440.00, // A4
                                                    493.88);// B4

  MidiNoteNumbers : array[TPitchClass] of Byte = ( 0, // C4
                                                   2, // D4
                                                   4, // E4
                                                   5, // F4
                                                   7, // G4
                                                   9, // A4
                                                  11);// B4
implementation

{$I notes.inc}

end.

