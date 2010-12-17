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

  Main ToneGrid application unit.
}
unit ToneGridUnit;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, ComCtrls, ActnList, StdActns,
  { Added manually }
  GraphicGrid, Notes, // from ToneGridPkg
  LCLType,            // contains key codes
  PasMidi,            // support for midi
  PreferencesUnit;    // contains Preferences form

type
  TUpdateEvent = procedure(Sender : TObject) of object;

  { TToneGridMainForm }

  TToneGridMainForm = class(TForm)
    ClearGrid: TAction;
    ActionList: TActionList;
    FileExit1: TFileExit;
    HelpAbout: THelpAction;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PreferencesMenuItem: TMenuItem;
    PlayStopButton: TButton;
    ModeBox: TComboBox;
    BpmTimer: TTimer;
    TonicBox: TComboBox;
    OctaveEdit: TEdit;
    SizeXEdit: TEdit;
    SizeYEdit: TEdit;
    BpmEdit: TEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ToneGrid: TGraphicGrid;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel2: TPanel;
    PitchBox: TGroupBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    GainSlider: TScrollBar;
    OctaveUpDown: TUpDown;
    SizeXUpDown: TUpDown;
    SizeYUpDown: TUpDown;
    BpmUpDown: TUpDown;
    procedure BpmEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure BpmTimerStartTimer(Sender: TObject);
    procedure BpmTimerTimer(Sender: TObject);
    procedure BpmUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure ClearGridExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GainSliderChange(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure ModeBoxChange(Sender: TObject);
    procedure OctaveEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OctaveUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure PlayStopButtonClick(Sender: TObject);
    procedure PreferencesMenuItemClick(Sender: TObject);
    procedure SizeXEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SizeXUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure SizeYEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SizeYUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure ToneGridPaint(Sender: TObject);
    procedure ToneGridResize(Sender: TObject);
    procedure TonicBoxChange(Sender: TObject);
  private
    { private declarations }

    { Circle of fifths will contain all the scales and notes used within the
      program}
    FCircleOfFifths : TCircleOfFifths;
    { only natural minor or major for now }
    FMode : TScaleMode;
    { currently set scale }
    FScale : TScale;
    FBottomOct : TOctave;
    { array of note names shown to the left of the ToneGrid }
    FPitchLabels : array of TLabel;
    { FNotes will store TNote objects according to current settings }
    FNotes : array of TNote;
    { When user presses Play, this array will be populated }
    FSounds : array of IAbstractSource;
    FGain : Double;
    FSizeX, FSizeY, FBpm : Integer;
    { Cursor indicates position in time while playing (X dimension).
      FCursorIndex indicates which column of the ToneGrid is to be played,
      FCursorPos is just to draw the cursor in relevant position. }
    FCursorPos, FCursorIndex : Integer;

    { This procedure populates TonicBox with possible tonics of scales,
      depending on scale mode. }
    procedure PopulateTonicBox;

    { Enables/disables group of controls when pressing PlayStopButton }
    procedure SwitchControls;

    { (Re)populates FNotes array. FNotes will contain an excerpt from the FScale,
      of length FSizeY, and starting from FBottomOct octave. }
    procedure PopulateNotes;
  public
    { public declarations }
  end;

var
  ToneGridMainForm: TToneGridMainForm;

const
  { min, max and default values for fields }
  MinSizeX = 1;
  DefaultSizeX = 16;
  MaxSizeX = 32;

  MinSizeY = 1;
  DefaultSizeY = 8;
  MaxSizeY = 22; // 21 tones (3 octaves) + 1

  MinBpm = 20;
  DefaultBpm = 220;
  MaxBpm = 999;

  MinGain = 0;
  DefaultGain = 50;
  MaxGain = 100;

  MinBottomOct = 2;
  DefaultBottomOct = 4;
  MaxBottomOct = 5;

implementation

{ ToneGridMainForm }

procedure TToneGridMainForm.FormCreate(Sender: TObject);
begin
  FCircleOfFifths := TCircleOfFifths.Create;

  { Mode }
  FMode := smMajor;
  ModeBox.AddItem('Major', nil);
  ModeBox.AddItem('Minor', nil);
  ModeBox.Text := ModeBox.Items[0];

  { Bottom octave }
  FBottomOct := DefaultBottomOct;
  OctaveUpDown.Min := MinBottomOct;
  OctaveUpDown.Max := MaxBottomOct;
  OctaveUpDown.Position := FBottomOct;
  OctaveEdit.Text := IntToStr(FBottomOct);

  { Tonic }
  PopulateTonicBox;
  FScale := TonicBox.Items.Objects[0] as TScale;

  { Size Y }
  FSizeY := DefaultSizeY;
  ToneGrid.SizeY := FSizeY;
  SizeYUpDown.Min := MinSizeY;
  SizeYUpDown.Max := MaxSizeY;
  SizeYUpDown.Position := FSizeY;
  SizeYEdit.Text := IntToStr(FSizeY);

  { At this point we have all necessary data to generate FNotes array }
  PopulateNotes;

  { Size X }
  FSizeX := DefaultSizeX;
  ToneGrid.SizeX := FSizeX;
  SizeXUpDown.Min := MinSizeX;
  SizeXUpDown.Max := MaxSizeX;
  SizeXUpDown.Position := FSizeX;
  SizeXEdit.Text := IntToStr(FSizeX);

  { Bpm }
  FBpm := DefaultBpm;
  BpmUpDown.Min := MinBpm;
  BpmUpDown.Max := MaxBpm;
  BpmUpDown.Position := FBpm;
  BpmEdit.Text := IntToStr(FBpm);

  { Gain }
  FGain := DefaultGain / MaxGain;
  GainSlider.Min := MinGain;
  GainSlider.Max := MaxGain;
  GainSlider.Position := DefaultGain;

  { Cursor }
  FCursorPos := 0;
  FCursorIndex := 0;
end;

procedure TToneGridMainForm.BpmUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  FBpm := BpmUpDown.Position;
  BpmEdit.Text := IntToStr(FBpm);
end;

procedure TToneGridMainForm.ClearGridExecute(Sender: TObject);
begin
  ToneGrid.Clear;
end;

procedure TToneGridMainForm.BpmEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  NewBpm : Integer;
begin
  { Return key confirms the change in edit box }
  if Key = VK_RETURN then
  begin
    try
      NewBpm := StrToInt(BpmEdit.Text);
      if (NewBpm <> FBpm) and (MinBpm <= NewBpm) and (NewBpm <= MaxBpm) then
      begin
         FBpm := NewBpm;
         BpmUpDown.Position := FBpm;
      end
      else
        { If new value is invalid, show that the change has not been made }
        BpmEdit.Text := IntToStr(FBpm);
    except
      BpmEdit.Text := IntToStr(FBpm);
    end;
  end;
end;

procedure TToneGridMainForm.BpmTimerStartTimer(Sender: TObject);
begin
  { We want the tones to start playing immediately after pressing the 'Play'
    button }
  BpmTimerTimer(Sender);
end;

procedure TToneGridMainForm.BpmTimerTimer(Sender: TObject);
var
  I : Integer;
begin
  { Play the sounds from FSounds }
  for I := Low(FSounds) to High(FSounds) do
  begin
    { Stop previous note - is only relevant when using MIDI }
    FSounds[High(FSounds)-I].Stop;
    if ToneGrid.IsSet(FCursorIndex, I) then
    { We want the lower tones to appear lower on the ToneGrid - therefore we
      have to reverse the indexing. }
      FSounds[High(FSounds)-I].Play;
  end;

  { Advance cursor }
  FCursorIndex := (FCursorIndex + 1) mod ToneGrid.SizeX;
  FCursorPos := Round(FCursorIndex * ToneGrid.CellWidth) mod ToneGrid.Width;
  if FCursorPos = 0 then
    { we want the cursor to go up to the rightmost end of the grid }
    FCursorPos := ToneGrid.Width;

  { Repaint ToneGrid with cursor }
  ToneGrid.Invalidate;
end;

procedure TToneGridMainForm.FormDestroy(Sender: TObject);
var
  I : Integer;
begin
  { Free Labels created at runtime }
  for I := Low(FPitchLabels) to High(FPitchLabels) do
    FreeAndNil(FPitchLabels[I]);

  { Free the circle of fifths }
  FreeAndNil(FCircleOfFifths);
end;

procedure TToneGridMainForm.GainSliderChange(Sender: TObject);
var
  I : Integer;
begin
  { Rescale slider value to 0.0 - 1.0 }
  FGain := (GainSlider.Max - GainSlider.Position) / GainSlider.Max;
  { Set new gain value to all the notes }
  for I := Low(FSounds) to High(FSounds) do
    if Assigned(FSounds[I]) then
      FSounds[I].Gain := FGain;
end;

procedure TToneGridMainForm.HelpAboutExecute(Sender: TObject);
begin
  ShowMessage('Copyright © 2010 by Piotr Karasinski' + sLineBreak +
              'Use left mouse button to set a note to be played.' + sLineBreak +
              'Use right mouse button to clear a note.');
end;

procedure TToneGridMainForm.ModeBoxChange(Sender: TObject);
var
  Index : Integer;
begin
  Index := ModeBox.ItemIndex;
  case Index of
    0 : FMode := smMajor;
    1 : Fmode := smMinor;
  else
    raise Exception.Create('ModeBoxChange: TScaleMode range error.');
  end;

  { Depending on the mode, different key signatures can be used }
  PopulateTonicBox;
  FScale := TonicBox.Items.Objects[0] as TScale;
  { Repopulate FNotes }
  PopulateNotes;
end;

procedure TToneGridMainForm.OctaveEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  NewBottomOct : TOctave;
begin
  if Key = VK_RETURN then
  begin
    try
      NewBottomOct := StrToInt(OctaveEdit.Text);
      if (NewBottomOct <> FBottomOct) and
         (NewBottomOct in [MinBottomOct .. MaxBottomOct]) then
      begin
        FBottomOct := NewBottomOct;
        OctaveUpDown.Position := FBottomOct;
        PopulateNotes;
      end
      else
        OctaveEdit.Text := IntToStr(FBottomOct);
    except
      OctaveEdit.Text := IntToStr(FBottomOct);
    end;
  end;
end;

procedure TToneGridMainForm.OctaveUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  FBottomOct := OctaveUpDown.Position;
  OctaveEdit.Text := IntToStr(FBottomOct);

  PopulateNotes;
end;

procedure TToneGridMainForm.PlayStopButtonClick(Sender: TObject);
var
  Length : Integer; // sound length in milliseconds
  I : Integer;
begin
  SwitchControls;
  if not BpmTimer.Enabled then
  { user pressed 'Play' }
  begin
    PlayStopButton.Caption := 'STO&P';
    { 1000 milliseconds * 60 = one second}
    BpmTimer.Interval := Round(1000 * 60 / FBpm);

    { We shorten the Length by 1/8 to avoid glitches }
    Length := BpmTimer.Interval - (BpmTimer.Interval div 8);
    { Populate FSounds array - since elements of this array implement
      IAbstractSource interface, they are reference counted - we don't need
      to free the memory for them. }
    SetLength(FSounds, (High(FNotes)+1));
    for I := Low(FSounds) to High(FSounds) do
    begin
      if PreferencesForm.OpenAlSelected then
        FSounds[I] := TNoteOpenAlSource.Create(FNotes[I], Length, 30, 60)
      else if PreferencesForm.Device <> -1 then
        FSounds[I] := TMidiSource.Create(FNotes[I], PreferencesForm.Device)
      else
        FSounds[I] := TEmptySource.Create;


      FSounds[I].Gain := FGain;
    end;
    { Start playback }
    BpmTimer.Enabled := true;
  end
  else
  { user pressed 'Stop' }
  begin
    BpmTimer.Enabled := false;
    cMidiAllSoundsOff(PreferencesForm.Device);
    SetLength(FSounds, 0);
    FCursorIndex := 0;
    FCursorPos := 0;
    ToneGrid.Invalidate;
    PlayStopButton.Caption := '&PLAY';
  end;
end;

procedure TToneGridMainForm.PreferencesMenuItemClick(Sender: TObject);
begin
  PreferencesForm.ShowModal;
end;

procedure TToneGridMainForm.SizeXEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  NewSizeX : Integer;
begin
  if Key = VK_RETURN then
  begin
    try
      NewSizeX := StrToInt(SizeXEdit.Text);
      if (NewSizeX <> FSizeX) and (NewSizeX in [MinSizeX .. MaxSizeX]) then
      begin
        FSizeX := NewSizeX;
        SizeXUpDown.Position := FSizeX;
        ToneGrid.SizeX := FSizeX;
      end
      else
        SizeXEdit.Text := IntToStr(FSizeX);
    except
      SizeXEdit.Text := IntToStr(FSizeX);
    end;
  end;
end;

procedure TToneGridMainForm.SizeXUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  FSizeX := SizeXUpDown.Position;
  SizeXEdit.Text := IntToStr(FSizeX);
  ToneGrid.SizeX := FSizeX;
end;

procedure TToneGridMainForm.SizeYEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  NewSizeY : Integer;
begin
  if Key = VK_RETURN then
  begin
    try
      NewSizeY := StrToInt(SizeYEdit.Text);
      if (NewSizeY <> FSizeY) and (NewSizeY in [MinSizeY .. MaxSizeY]) then
      begin
        FSizeY := NewSizeY;
        SizeYUpDown.Position := FSizeY;
        ToneGrid.SizeY := FSizeY;
        PopulateNotes;
      end
      else
        SizeYEdit.Text := IntToStr(FSizeY);
    except
      SizeYEdit.Text := IntToStr(FSizeY);
    end;
  end;
end;

procedure TToneGridMainForm.SizeYUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  FSizeY := SizeYUpDown.Position;
  SizeYEdit.Text := IntToStr(FSizeY);
  ToneGrid.SizeY := FSizeY;
  PopulateNotes;
end;

{ Repaint ToneGrid with playback cursor }
procedure TToneGridMainForm.ToneGridPaint(Sender: TObject);
begin
  with ToneGrid.Canvas do
  begin
    Pen.Width := 10;
    Pen.Color := clNavy;
    Line(FCursorPos, 0, FCursorPos, Height);
  end;
end;

procedure TToneGridMainForm.ToneGridResize(Sender: TObject);
var
  I : Integer;
begin
  for I := Low(FPitchLabels) to High(FPitchLabels) do
  begin
    { Reposition pitch labels }
    FPitchLabels[I].Top := (High(FPitchLabels) - I) * Round(ToneGrid.CellHeight);
  end;
  { Adjust cursor position }
  FCursorPos := Round(FCursorIndex * ToneGrid.CellWidth) mod ToneGrid.Width;
end;

procedure TToneGridMainForm.TonicBoxChange(Sender: TObject);
var
  Index : Integer;
begin
  Index := TonicBox.ItemIndex;
  FScale := TonicBox.Items.Objects[Index] as TScale;
  PopulateNotes;
end;

{ PopulateTonicBox uses TNote references from FCircleOfFifths field, which
  is freed in the form's destructor. }
procedure TToneGridMainForm.PopulateTonicBox;
var
  I : Integer;
  PitchStr, Accidentals : string;
begin
  TonicBox.Clear; // Clear TonicBox

  { Populate TonicBox }
  for I := 0 to (FCircleOfFifths.Keys(FMode).Count-1) do
  begin
    PitchStr := FCircleOfFifths.Keys(FMode)[I];
    { Indicate amount of accidentals in given key signature }
    Accidentals := FCircleOfFifths.Scale(PitchStr, FMode).GetAccidentals;
    if Accidentals <> '' then
      TonicBox.AddItem(Format('%-2s %s',[PitchStr, '[' + Accidentals + ']']),
                       FCircleOfFifths.Scale(PitchStr, FMode))
    else
      { Add name and scale, which will be used when changing tonic box }
      TonicBox.AddItem(PitchStr, FCircleOfFifths.Scale(PitchStr, FMode));
  end;
  TonicBox.Text := TonicBox.Items[0];
end;

procedure TToneGridMainForm.SwitchControls;
begin
  ModeBox.Enabled := not ModeBox.Enabled;
  TonicBox.Enabled := not TonicBox.Enabled;
  OctaveEdit.Text := IntToStr(FBottomOct);
  OctaveEdit.Enabled := not OctaveEdit.Enabled;
  OctaveUpDown.Enabled := not OctaveUpDown.Enabled;
  SizeXEdit.Text := IntToStr(FSizeX);
  SizeXEdit.Enabled := not SizeXEdit.Enabled;
  SizeXUpDown.Enabled := not SizeXUpDown.Enabled;
  SizeYEdit.Text := IntToStr(FSizeY);
  SizeYEdit.Enabled := not SizeYEdit.Enabled;
  SizeYUpDown.Enabled := not SizeYUpDown.Enabled;
  BpmEdit.Text := IntToStr(FBpm);
  BpmEdit.Enabled := not BpmEdit.Enabled;
  BpmUpDown.Enabled := not BpmUpDown.Enabled;
  PreferencesMenuItem.Enabled := not PreferencesMenuItem.Enabled;
end;

procedure TToneGridMainForm.PopulateNotes;
var
  I, ScaleIndex : Integer;
begin
  { Adjust FNotes length }
  SetLength(FNotes, FSizeY);

  { Populate }
  ScaleIndex := FScale.GetOctaveIndex(FBottomOct);
  for I := Low(FNotes) to High(FNotes) do
  begin
    FNotes[I] := FScale.At(ScaleIndex);
    Inc(ScaleIndex);
  end;

  { Changing tones affects pitch labels as well.
    Clear FPitchLabels and adjust length }
  for I := Low(FPitchLabels) to High(FPitchLabels) do
    FreeAndNil(FPitchLabels[I]);
  SetLength(FPitchLabels, FSizeY);

  for I := Low(FPitchLabels) to High(FPitchLabels) do
  begin
    FPitchLabels[I] := TLabel.Create(self);
    FPitchLabels[I].Parent := PitchBox;
    { Reverse indexing as we have lower notes in the bottom of the grid }
    FPitchLabels[I].Top := (High(FPitchLabels) - I) * Round(ToneGrid.CellHeight);
    FPitchLabels[I].AnchorHorizontalCenterTo(FPitchLabels[I].Parent);
    FPitchLabels[I].Caption := FNotes[I].ToString;
  end;
end;

initialization
  {$I tonegridunit.lrs}

end.

