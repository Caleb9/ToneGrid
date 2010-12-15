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

{ TOpenAlSource }

procedure TOpenAlSource.SetGain(const NewGain: Double);
begin
  AlSourceF(FSource, AL_GAIN, NewGain); // OpenAL routine
end;

procedure TOpenAlSource.GenerateSine(out Sine: array of SmallInt);
const
  { Used to rescale sample values to required format }
  Amplitude = High(TALshort);
var
  Phase, PhaseInc : Double;
  { We could make the sound more interesting by adding an overtone, but for
    now we'll leave it commented out }
  (* Phase2, PhaseInc2 : Double; *)
  I : LongWord;
begin
  { Generate sine wave }
  Phase := 0;
  PhaseInc := 2 * FFreq / FSampleRate;
  (*
  Phase2 := 0;
  PhaseInc2 := 4 * (FFreq-2) / FSampleRate;
  *)

  I := Low(Sine);
  while (I < FSize) and (I <= High(Sine)) do
  begin
    Sine[I] := Round(0.8 * Amplitude * Sin(Pi * Phase));
            (* + Round(0.2 * Amplitude * Sin(Pi * Phase2)); *)
    Phase := Phase + PhaseInc;
    if Phase > 1.0 then
      Phase := Phase - 2.0;
    (*
    Phase2 := Phase2 + PhaseInc2;
    if Phase2 > 1.0 then
      Phase2 := Phase2 - 2.0;
    *)
    Inc(I);
  end;

  { Fill up the rest of the array with silence }
  for I := FSize to High(Sine) do
    Sine[I] := 0;
end;

procedure TOpenAlSource.SetAttack(var Sine: array of SmallInt; Len : TALSizeI);
var
  EnvVal, EnvValInc : Double;
  I : LongWord;
begin
  { Scale signal up from 0 to 1 in initial samples }
  EnvVal := 0;
  EnvValInc := 1 / Len; // Incrementation step
  I := Low(Sine);
  while (I < Len) and (I <= High(Sine)) do
  begin
    Sine[I] := Round(Sine[I] * EnvVal);
    EnvVal := EnvVal + EnvValInc;
    Inc(I);
  end;
end;

procedure TOpenAlSource.SetRelease(var Sine: array of SmallInt; Len: TALSizeI);
var
  EnvVal, EnvValInc : Double;
  I : LongInt;
begin
  { Scale signal down to 0 in signal's ending samples }
  EnvVal := 1;
  EnvValInc := 1 / Len;
  I := FSize - Len;
  while (I < FSize) and (I <= High(Sine)) do
  begin
    if I >= Low(Sine) then
      Sine[I] := Round(Sine[I] * EnvVal);
    EnvVal := EnvVal - EnvValInc;
    Inc(I);
  end;
end;

constructor TOpenAlSource.Create(Freq: TALFloat; Size: TALSizeI;
  SampleRate: TALSizeI; Attack: TALSizeI; Release: TALSizeI);
var
  { We will generate sine wave signal in this array }
  Sine : array of SmallInt;

  { OpenAL's performs best if the buffer size is a power of 2. We assume the
    smallest buffer required will be 512 samples. }
  BufLen : TALsizei = 512;
begin
  inherited Create;

  FFreq := Freq;
  FSize := Size;
  FSampleRate := SampleRate;
  { For simplicity we'll always use 16 bit signed integer values for samples.
    Tones will be single channel. }
  FFormat := AL_FORMAT_MONO16;

  { Adjust buffer length to be a power of 2 >= to the desired length of sine
    wave. }
  while BufLen < FSize do
    BufLen := BufLen * 2;

  { Generate sine wave signal. Note that size of the 'Sine' array might be
    bigger than the actual signal's desired length (we want it to be
    a power of 2). }
  SetLength(Sine, BufLen);
  GenerateSine(Sine);
  SetAttack(Sine, Attack);
  SetRelease(Sine, Release);

  AlGenBuffers(1, @FBuffer); // Generate OpenAL buffers

  { Load SineWave to OpenAL buffer. In Linux, somehow, we have to
    allocate longer buffer than the actual sound array - otherwise playback
    ends prematurely giving an unplesant 'click' sound(therefore the
    multiplication by 2). }
  AlBufferData(FBuffer, FFormat, Sine, Length(Sine) * 2, FSampleRate);

  AlGenSources(1, @FSource); // Generate source
  AlSourceI(FSource, AL_BUFFER, FBuffer); // Assign buffer to source
  (* AlSourceF(source, AL_PITCH, 1.0); *)
  AlSourceF(FSource, AL_GAIN, 1.0);
  AlSourceI(FSource, AL_LOOPING, AL_FALSE);
end;

destructor TOpenAlSource.Destroy;
begin
  { OpenAL clean up }
  AlDeleteBuffers(1, @FBuffer);
  AlDeleteSources(1, @FSource);

  inherited Destroy;
end;

procedure TOpenAlSource.Play;
begin
  AlSourceRewind(FSource);
  AlSourcePlay(FSource); // OpenAL routine
end;

procedure TOpenAlSource.Stop;
begin
  AlSourceStop(FSource); // OpenAL routine
end;

procedure TOpenAlSource.Pause;
begin
  AlSourcePause(FSource); // OpenAL routine
end;

{ TNote }

constructor TNote.Create(const Name: TPitchClass; const Accidental: TAccidental;
  const Octave: Byte);
begin
  inherited Create;

  FPitchClass := Name;
  FAccidental := Accidental;
  FOctave := Octave;
end;

constructor TNote.Create(Source: TNote);
begin
  inherited Create;

  Assign(Source);
end;

constructor TNote.Create(const PitchStr: string);
var
  Pitch : TPitchClass;
  Acc : TAccidental;
  Oct : Byte;
  Chr : Char;
  I : Integer;
begin
  inherited Create;

  I := 1;
  { Pure pitch class name }
  Chr := UpCase(PitchStr[I]);
  case Chr of
    'C' : Pitch := pC;
    'D' : Pitch := pD;
    'E' : Pitch := pE;
    'F' : Pitch := pF;
    'G' : Pitch := pG;
    'A' : Pitch := pA;
    'B' : Pitch := pB;
  else
    raise Exception.Create('TPitch.Create(string): invalid argument');
  end;

  { Accidental }
  if Length(PitchStr) > I then
  begin
    Inc(I);
    Chr := LowerCase(PitchStr[I]);
    case Chr of
      'b' :
      begin
        { Check if it's a double flat }
        if (Length(PitchStr) > I) and (LowerCase(PitchStr[I+1]) = 'b') then
        begin
          Acc := acDoubleFlat;
          Inc(I);
        end
        else
          Acc := acFlat;
      end;
      '#' : Acc := acSharp;
      'x' : Acc := acDoubleSharp;
    else
      { No accidental - rest of the string should be the octave designation. }
      Oct := StrToInt(Copy(PitchStr, I, Length(PitchStr)-I+1));
      I := Length(PitchStr) // Prevents detecting octave in next if block.
    end;
  end
  else // string of length 1 - just pitch class name without accidental
    Acc := acNatural;

  { Octave designation. }
  if Length(PitchStr) > I then
  begin
    Inc(I);
    Oct := StrToInt(Copy(PitchStr, I, 2));
  end
  else
    Oct := 4;

  FPitchClass := Pitch;
  FAccidental := Acc;
  FOctave := Oct;
end;

procedure TNote.Assign(Source: TPersistent);
var
  SrcPtr : TNote;
begin
  if Source is TNote then
  begin
    SrcPtr := Source as TNote;
    FPitchClass := SrcPtr.PitchClass;
    FAccidental := SrcPtr.Accidental;
    FOctave := SrcPtr.Octave;
  end
  else
    inherited Assign(Source);
end;

function TNote.ToString: string;
var
  Str : string;
begin
  case PitchClass of
    pC : Str := 'C';
    pD : Str := 'D';
    pE : Str := 'E';
    pF : Str := 'F';
    pG : Str := 'G';
    pA : Str := 'A';
    pB : Str := 'B';
  end;
  case Accidental of
    acDoubleFlat : Str := Str + 'bb';
    acFlat : Str := Str + 'b';
    acSharp : Str := Str + '#';
    acDoubleSharp : Str := Str + 'x';
  end;
  Result := Str + IntToStr(Octave);
end;

function TNote.GetFrequency: Single;
var
  Freq : Single; // Note's frequency
  ValA : Single;
  HalfSteps : Integer = 0;
begin
  ValA := power(2, (1/12.0));

  case self.Accidental of
    acDoubleFlat : HalfSteps := -2;
    acFlat : HalfSteps := -1;
    acSharp : HalfSteps := 1;
    acDoubleSharp : HalfSteps := 2;
  end;

  Freq := ToneFrequencies[self.PitchClass] * power(ValA, HalfSteps);
  { Rescale depending on the octave (since we use octave 4 as reference for
    note frequencies, we need to reduce the exponentiation by 4. }
  Freq := Freq * power(2, self.Octave - 4);

  Result := Freq;
end;


{ TNoteOpenAlSource }

constructor TNoteOpenAlSource.Create(Note: TNote; const Duration: Word;
                                  const Attack: Word;
                                  const Release: Word);
var
  DurationInSamples, AttackInSamples, ReleaseInSamples : TAlSizeI;
begin
  DurationInSamples := MillisecondsToSamples(Duration, DefaultSampleRate);
  AttackInSamples := MillisecondsToSamples(Attack, DefaultSampleRate);
  ReleaseInSamples := MillisecondsToSamples(Release, DefaultSampleRate);

  inherited Create(Note.Frequency, DurationInSamples, DefaultSampleRate, AttackInSamples, ReleaseInSamples);
end;

class function TNoteOpenAlSource.MillisecondsToSamples(const Duration: Word;
  const SampleRate: TALSizeI): TALSizeI;
begin
  Result := Round(Duration / 1000.0 * SampleRate);
end;

{ TMidiSource }

procedure TMidiSource.SetGain(const NewGain: Double);
begin
  { Rescale 0.0 - 1.0 to 0 - 127 }
  FVelocity := Round(NewGain * 127);
end;

constructor TMidiSource.Create(Note: TNote; const Device: Byte);
var
  I : Integer;
begin
  FKey := MidiNoteNumbers[Note.PitchClass];
  for I := 1 to Note.Octave do
    FKey := FKey + 12;

  case Note.Accidental of
    acDoubleFlat : FKey := FKey - 2;
    acFlat : FKey := FKey - 1;
    acSharp : FKey := FKey + 1;
    acDoubleSharp : FKey := FKey + 2;
  end;

  FVelocity := 127;
  FDevice := Device;
end;

procedure TMidiSource.Play;
begin
  cMidiNoteOn(FKey, FVelocity, FDevice);
end;

procedure TMidiSource.Stop;
begin
  cMidiNoteOff(FKey,(* FVelocity, *) FDevice);
end;

{ TScale }

constructor TScale.Create(const PitchClass : TPitchClass;
                          const Accidental : TAccidental;
                          const NewMode : TScaleMode);
var
  Octave : TOctave;
begin
  FMode := NewMode;
  { True argument in TObjectList's constructor indicates, that the list is an
    owner of stored objects - therefore they are freed along with the list. }
  FPitches := TObjectList.Create(true);

  { Create tonic at lowest octave }
  FPitches.Add(TNote.Create(PitchClass, Accidental, MinOctave));

  case Mode of
    smMajor : // Major mode: w-w-h-w-w-w-h (whole/half-step pattern)
    begin
      FDegrees := 7;
      for Octave := Low(TOctave) to High(TOctave) do
      begin
        FPitches.Add(NextHalfSteps(2));
        FPitches.Add(NextHalfSteps(2));
        FPitches.Add(NextHalfSteps(1));
        FPitches.Add(NextHalfSteps(2));
        FPitches.Add(NextHalfSteps(2));
        FPitches.Add(NextHalfSteps(2));
        FPitches.Add(NextHalfSteps(1));
      end;
    end;
    smMinor : // Minor mode w-h-w-w-h-w-w
    begin
      FDegrees := 7;
      for Octave := Low(TOctave) to High(TOctave) do
      begin
        FPitches.Add(NextHalfSteps(2));
        FPitches.Add(NextHalfSteps(1));
        FPitches.Add(NextHalfSteps(2));
        FPitches.Add(NextHalfSteps(2));
        FPitches.Add(NextHalfSteps(1));
        FPitches.Add(NextHalfSteps(2));
        FPitches.Add(NextHalfSteps(2));
      end;
    end;
  else
    raise Exception.Create('TScale.Create: TScaleMode range error.');
  end;
end;

destructor TScale.Destroy;
begin
  { Freeing FPitches will free all contained objects as well because FPitches
    is the owner (true argument in FPitches.Create) }
  FreeAndNil(FPitches);
  inherited Destroy;
end;

function TScale.GetTonic(Octave : TOctave) : TNote;
begin
  Result := self.At(GetOctaveIndex(Octave));
end;

function TScale.At(Index: Integer): TNote;
begin
  Result := FPitches.Items[Index-1] as TNote;
end;

function TScale.GetOctaveIndex(Octave: TOctave): Integer;
var
  I : Integer;
begin
  I := 0;
  while I < FPitches.Count do
  begin
    if TNote(FPitches[I]).Octave = Octave then
    begin
      Result := I + 1;
      I := FPitches.Count; // exit loop
    end;
    Inc(I, FDegrees); // skip octave
  end;
end;

function TScale.GetAccidentals: string;
var
  I : Integer;
  Res : string = '';
  Note : TNote;
begin
  for I := 1 to FDegrees do
  begin
    Note := At(I);
    case Note.Accidental of
      acNatural : ;
      acFlat : Res := Res + 'b';
      acSharp : Res := Res + '#';
      acDoubleFlat : Res := Res + '(bb)';
      acDoubleSharp : Res := Res + 'x';
    end;
  end;
  Result := Res;
end;

function TScale.GetCount: Integer;
begin
  Result := FPitches.Count;
end;

function TScale.NextHalfSteps(HalfSteps: THalfSteps): TNote;
var
  Src, Dst : TNote;
begin
  Src := FPitches.Last as TNote;
  Dst := TNote.Create(NextPitchClass(Src.PitchClass));

  case HalfSteps of
    1 :
      begin
        { accidental }
        if Src.PitchClass in [pC, pD, pF, pG, pA] then
        begin
          case Src.Accidental of
            acFlat..acDoubleSharp : Dst.FAccidental := Pred(Src.Accidental);
          else
            { Double flatted C, D, F, G or A. We don't have designation for
              pitch that is halfstep above such. }
            raise Exception.Create('Impossible to generate a pitch halfstep above '
                                               + Src.ToString);
          end;
        end
        else // pE, pB
          Dst.FAccidental := Src.Accidental;
      end;
    2 :
      begin
        { accidental }
        if Src.PitchClass in [pE, pB] then
        begin
          case Src.Accidental of
            acDoubleFlat..acSharp : Dst.FAccidental := Succ(Src.Accidental);
          else
            { Double-sharped E or B ... }
            raise Exception.Create('Impossible to generate a pitch wholestep above '
                                               + Src.ToString);
          end;
        end
        else // pC, pD, pF, pG, pA
          Dst.FAccidental := Src.Accidental;
      end;
    else
      raise Exception.Create('TScale.NextHalfStep: out of THalfSteps range.');
  end;

  { octave }
  Dst.FOctave := Src.Octave;
  if Src.PitchClass = pB then
     Inc(Dst.FOctave);

  Result := Dst;
end;

{ TCircleOfFifths }

function TCircleOfFifths.GetKey(Pitch: TNote): string;
var
  Key : string;
begin
  Key := Pitch.ToString;
  { Remove octave designation }
  Result := Copy(Key, 1, Length(Key)-1);
end;

constructor TCircleOfFifths.Create;
var
  Mode : TScaleMode;
  Tonic, TonicPtr : TNote;
  Scale : TScale;
  I : Integer;
  Key : string;
begin
  for Mode := Low(TScaleMode) to High(TScaleMode) do
  begin
    FScales[Mode] := TFPObjectHashTable.Create(true);
    FKeys[Mode] := TStringList.Create;

    case Mode of
      { Start with key signature without accidentals }
      smMajor : Tonic := TNote.Create(pC, acNatural);
      smMinor : Tonic := TNote.Create(pA, acNatural);
    else
      raise Exception.Create('TCircleOfFifths.Create: TScaleMode range error');
    end;

      { Generate 6 scales clockwise on circle of fifths }
      TonicPtr := Tonic;
      for I := 1 to 6 do
      begin
        { Create scale, it will be owned by FScales so we'll free it up
          when freeing FScales. TonicPtr is copied. }
        Scale := TScale.Create(TonicPtr.PitchClass, TonicPtr.Accidental, Mode);

        { Add scale to FScales table }
        Key := GetKey(TonicPtr);
        FScales[Mode].Add(Key, Scale);
        FKeys[Mode].Add(Key);

        { Move tonic pointer to fifth degree of the scale (perfect 5th above the
          tonic) to generate next one on circle of fifths. }
        TonicPtr := Scale.At(5);
      end;

      { Generate 6 scales counter-clockwise on circle of fifths. }
      TonicPtr := TScale(FScales[Mode].Items[GetKey(Tonic)]).At(4);
      for I := 1 to 6 do
      begin
        Scale := TScale.Create(TonicPtr.PitchClass, TonicPtr.Accidental, Mode);
        Key := GetKey(TonicPtr);
        FScales[Mode].Add(Key, Scale);
        FKeys[Mode].Add(Key);
        { Move tonic pointer to fourth degree of the scale (perfect 5th below
          the tonic), to generate previous one on circle of fifths. }
        TonicPtr := Scale.At(4);
      end;

    FreeAndNil(Tonic);

    { Sort keys by pitch order }
    FKeys[Mode].CustomSort(ComparePitches);
  end;
end;

destructor TCircleOfFifths.Destroy;
var
  Mode : TScaleMode;
begin
  for Mode := Low(TScaleMode) to High(TScaleMode) do
  begin
    FreeAndNil(FKeys[Mode]);
    FreeAndNil(FScales[Mode]);
  end;
  inherited Destroy;
end;

function TCircleOfFifths.Scale(Tonic: TNote; const Mode: TScaleMode): TScale;
begin
  Result := Scale(GetKey(Tonic), Mode);
end;

function TCircleOfFifths.Scale(const Key: string; const Mode: TScaleMode
  ): TScale;
begin
  Result := FScales[Mode].Items[Key] as TScale;
end;

function TCircleOfFifths.Scale(const TonicPitchClass: TPitchClass;
  const Accidental: TAccidental; const Octave: TOctave; const Mode: TScaleMode
  ): TScale;
var
  Pitch : TNote;
begin
  Pitch := TNote.Create(TonicPitchClass, Accidental, Octave);
  Result := Scale(GetKey(Pitch), Mode);
  FreeAndNil(Pitch);
end;

function TCircleOfFifths.Keys(const Mode: TScaleMode): TStringList;
begin
  Result := FKeys[Mode];
end;

function NextPitchClass(const PitchClass : TPitchClass) : TPitchClass;
begin
  if PitchClass <> pB then
    Result := Succ(PitchClass)
  else
    Result := pC;
end;

{ We compare pitches by their frequency }
function ComparePitches(List: TStringList; Index1, Index2: Integer): Integer;
var
  Pitch1, Pitch2 : TNote;
  P1Freq, P2Freq : Integer;
begin
  Pitch1 := TNote.Create(List[Index1]);
  Pitch2 := TNote.Create(List[Index2]);
  { Within considered octave range, distinctive pitches (enharmonically inequivalent)
    always differ by more than one Hz in frequency. Therefore it is safe to
    compare them by frequencies rounded down - this way we avoid yelding inequality
    if they differ on e.g. fifth decimal position. }
  P1Freq := Floor(Pitch1.Frequency);
  P2Freq := Floor(Pitch2.Frequency);
  FreeAndNil(Pitch2);
  FreeAndNil(Pitch1);
  if P1Freq < P2Freq then
    Result := -1
  else if P1Freq > P2Freq then
    Result := 1
  else
    Result := 0;
end;

