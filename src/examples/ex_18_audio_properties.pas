program ex_18_audio_properties;
(*
 *    Demonstrate the audio addons.
 *)
(*
  Copyright (c) 2024 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

{$IFDEF FPC}
{ Needed to support classes. }
  {$IFNDEF FPC_DELPHI}
    {$MODE DELPHI}
  {$ENDIF}
{ Windows manifest. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  uses
    algui, Common,
    allegro5, al5acodec, al5audio, al5strings,
    Classes, sysutils;

  const
    SliderRange = 100;

  type
  (* Extends dialog to build the example. *)
    TAudioPropertiesExample = class (TDialog)
    private
      fSample: ALLEGRO_SAMPLEptr;
      fSampleInstance: ALLEGRO_SAMPLE_INSTANCEptr;
      fPanCheckbox: TCheckBox;
      fPanSlider: TSlider;

      procedure OnLengthChange (aSender: TObject);
      procedure OnPanUpdate (aSender: TObject);
      procedure OnBidirPushed (aSender: TObject);
      procedure OnPlayPushed (aSender: TObject);
      procedure OnSpeedChange (aSender: TObject);

      procedure OnGainChange (aSender: TObject);
      procedure OnMixerGainChange (aSender: TObject);
    public
    (* Destructor. *)
      destructor Destroy; override;
    (* Initialize the example. *)
      procedure Initialize; override;
    end;



(*
 * TAudioPropertiesExample
 *************************************************************************)

  procedure TAudioPropertiesExample.OnLengthChange (aSender: TObject);
  var
    lSlider: TSlider absolute aSender;
  begin
  { This doesn't work very well in my Linux, I don't know why.

    It works only sometimes.
  }
    al_set_sample_instance_length (fSampleInstance, lSlider.Value)
  end;



  procedure TAudioPropertiesExample.OnPanUpdate (aSender: TObject);
  var
    lPan: Real;
  begin
    if fPanCheckbox.Checked then
      lPan := fPanSlider.Value / fPanSlider.Max
    else
      lPan := ALLEGRO_AUDIO_PAN_NONE;
    al_set_sample_instance_pan (fSampleInstance, lPan)
  end;



  procedure TAudioPropertiesExample.OnBidirPushed (aSender: TObject);
  var
    lCheckBox: TCheckBox absolute aSender;
  begin
    if lCheckBox.Checked then
      al_set_sample_instance_playmode (fSampleInstance, ALLEGRO_PLAYMODE_BIDIR)
    else
      al_set_sample_instance_playmode (fSampleInstance, ALLEGRO_PLAYMODE_LOOP)
  end;



  procedure TAudioPropertiesExample.OnPlayPushed (aSender: TObject);
  var
    lCheckBox: TCheckBox absolute aSender;
  begin
    al_set_sample_instance_playing (fSampleInstance, lCheckBox.Checked)
  end;



  procedure TAudioPropertiesExample.OnSpeedChange (aSender: TObject);
  var
    lSlider: TSlider absolute aSender;
  begin
    al_set_sample_instance_speed (fSampleInstance, lSlider.Value / SliderRange)
  end;



  procedure TAudioPropertiesExample.OnGainChange (aSender: TObject);
  var
    lSlider: TSlider absolute aSender;
  begin
    al_set_sample_instance_gain (fSampleInstance, lSlider.Value / SliderRange)
  end;



  procedure TAudioPropertiesExample.OnMixerGainChange (aSender: TObject);
  var
    lSlider: TSlider absolute aSender;
  begin
    al_set_mixer_gain (al_get_default_mixer, lSlider.Value / SliderRange)
  end;



  destructor TAudioPropertiesExample.Destroy;
  begin
    if Assigned (fSampleInstance) then
      al_destroy_sample_instance (fSampleInstance);
    if Assigned (fSample) then
      al_destroy_sample (fSample);
    al_uninstall_audio;
    inherited Destroy
  end;



  procedure TAudioPropertiesExample.Initialize;

    procedure InitDialog;
    var
      lFilename: String;
    begin
      if Self.Terminated then Exit;
      Self.Title := 'Allegro''s audio properties example';
    { Check command line options. }
      if ParamCount >= 1 then
        lFilename := ParamStr (1)
      else
        lFilename := 'data/welcome.wav';
    { Init Allegro and add-ons. }
      if not al_install_audio or not al_init_acodec_addon then
      begin
        Self.ShowErrorMessage ('Could not initialize audio.');
        Self.Terminate;
        Exit
      end;
    { Set up audio. }
      if not al_reserve_samples (1) then
      begin
        Self.ShowErrorMessage ('Could not set up voice and mixer.');
        Self.Terminate;
        Exit
      end;
      fSample := al_load_sample (lFilename);
      if not Assigned (fSample) then
      begin
        Self.ShowErrorMessage (al_str_format (
          'Could not load sample from "%s"!',
          [lFilename]
        ));
        Self.Terminate;
        Exit
      end;
    { Loop the sample. }
      fSampleInstance := al_create_sample_instance (fSample);
      al_set_sample_instance_playmode (fSampleInstance, ALLEGRO_PLAYMODE_LOOP);
      al_attach_sample_instance_to_mixer (
        fSampleInstance,
        al_get_default_mixer
      );
      al_play_sample_instance (fSampleInstance)
    end;

    procedure CreateWidgets;

    (* Calculate Y coordinate. *)
      function GetY (const aLine: Integer): Integer; inline;
      const
        Separation = 60;
        Top = 180;
      begin
        Result := (aLine * Separation) + Top
      end;

    (* Helper to create labels. *)
      function CreateLabel (const aLine: Integer; const aCaption: String)
        : TLabel;
      begin
        Result := TLabel.CreateLabel (40, GetY (aLine) + 15, aCaption)
      end;

      function CreateLabelColumn (aColumn: Integer; const aCaption: String)
        : TLabel;
      begin
        if aColumn = 1 then aColumn := 580 else aColumn := 660;
        Result := TLabel.CreateLabel (aColumn, 30, aCaption)
      end;

    (* Helper to create a sliders. *)
      function CreateHorizontalSlider (
        const aLine, aMin, aMax: Integer;
        aHandler: TNotifyEvent
      ): TSlider;
      begin
        Result := TSlider.CreateSlider (
          oHorizontal,
          128, GetY (aLine), 440, 30,
          aMin, aMax, (aMax - aMin) div 20
        );
        Result.OnChange := aHandler
      end;

      function CreateveVerticalSlider (
        const aColumn, aMin, aMax: Integer;
        aHandler: TNotifyEvent
      ): TSlider;
      var
        lX: Integer;
      begin
        if aColumn = 1 then lX := 580 else lX := 700;
        Result := TSlider.CreateSlider (
          oVertical,
          lX, 60, 40, 510,
          aMin, aMax, (aMax - aMin) div 20
        );
        Result.OnChange := aHandler
      end;

    (* Helper to create checkboxes. *)
      function CreateCheckbox (
        const aLine: Integer;
        const aCaption: String;
        aHandler: TNotifyEvent
      ): TCheckBox;
      begin
        Result := TCheckBox.CreateCheckbox (40, GetY (aLine), 80, 30, aCaption);
        Result.OnClick := aHandler
      end;

    var
      lCheckBox: TCheckBox;
      lSlider: TSlider;
    begin
      if Self.Terminated then Exit;
    { Length slider. }
      Self.AppendWidget (CreateLabel (1, 'Length'));
      lSlider := CreateHorizontalSlider (
        1,
        0, al_get_sample_instance_length (fSampleInstance),
        Self.OnLengthChange
      );
      lSlider.Value := SliderRange;
      Self.AppendWidget (lSlider);
    { Pan button and slider. }
      fPanCheckbox := CreateCheckbox (2, 'Pan', Self.OnPanUpdate);
      fPanCheckbox.Checked := True;
      Self.AppendWidget (fPanCheckbox);
      fPanSlider := CreateHorizontalSlider (
        2,
        -SliderRange, SliderRange,
        Self.OnPanUpdate
      );
      fPanSlider.Value := 0;
      Self.AppendWidget (fPanSlider);
    { Speed slider. }
      Self.AppendWidget (CreateLabel (3, 'Speed'));
      lSlider := CreateHorizontalSlider (
        3,
        1, SliderRange * 5,
        Self.OnSpeedChange
      );
      lSlider.Value := SliderRange;
      Self.AppendWidget (lSlider);
    { Play buttons.}
      Self.AppendWidget (CreateCheckbox (4, 'Bidir', Self.OnBidirPushed));
      lCheckBox := CreateCheckbox (4, 'Play', Self.OnPlayPushed);
      lCheckBox.x := 128;
      lCheckBox.checked := True;
      Self.AppendWidget (lCheckBox);
    { Gain slider. }
      Self.AppendWidget (CreateLabelColumn (1, 'Gain'));
      lSlider := CreateveVerticalSlider (
        1,
        0, SliderRange,
        Self.OnGainChange
      );
      lSlider.Inverted := True;
      lSlider.Value := SliderRange div 2;
      Self.AppendWidget (lSlider);
    { Mixer slider. }
      Self.AppendWidget (CreateLabelColumn (2, 'Mixer gain'));
      lSlider := CreateveVerticalSlider (
        2,
        0, SliderRange,
        Self.OnMixerGainChange
      );
      lSlider.Inverted := True;
      lSlider.Value := SliderRange div 2;
      Self.AppendWidget (lSlider);
    { Other labels. }
      Self.AppendWidget (TLabel.CreateLabel (640,  60, '2.0'));
      Self.AppendWidget (TLabel.CreateLabel (640, 300, '1.0'));
      Self.AppendWidget (TLabel.CreateLabel (640, 540, '0.0'));
    end;

  begin
    inherited Initialize;
    InitDialog;
    CreateWidgets
  end;



var
  AudioPropertiesExample: TAudioPropertiesExample;
begin
  AudioPropertiesExample := TAudioPropertiesExample.Create;
  try
    AudioPropertiesExample.Initialize;
    AudioPropertiesExample.Run
  finally
    AudioPropertiesExample.Free
  end
end.
