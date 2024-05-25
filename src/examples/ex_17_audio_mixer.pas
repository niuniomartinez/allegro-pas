program ex_17_audio_chain;
(*
 *    Example program for the Allegro library, by Peter Wang,
 *    Ported to Object Pascal by Guillermo Martínez J.
 *
 *    Demonstrate the audio addons.
 *)
(*
  Copyright (c) 2022-2024 Guillermo Martínez J.

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
    allegro5, al5acodec, al5audio, al5font, al5primitives, al5strings, al5ttf,
    Classes, Contnrs, Math, sysutils;

  const
  (* Some values. *)
    SampleRate = 44100;
    VoiceAudioDepth = ALLEGRO_AUDIO_DEPTH_INT16;
    MixerAudioDepth = ALLEGRO_AUDIO_DEPTH_FLOAT32;
    ChannelConfig = ALLEGRO_CHANNEL_CONF_2;
    AudioSampleSize = 2048;
    AudioBufferCount = 4;

    VolumeChangeRate = 0.1;

  type
  (* Forward declarations. *)
    TVoice          = class;
    TMixer          = class;
    TSampleInstance = class;
    TSample         = class;
    TAudioStream    = class;



    TElement = class (TWidget)
    private
      fAttachedTo: TElement;

      fDragButton, fConnectX, fConnectY: Integer;
    protected
    (* Helper to set error messages. *)
      procedure SetErrorMessage (const aMessage: String); inline;
      procedure ClearErrorMessage; inline;
    (* Element position. *)
      procedure SetX (const aValue: Integer); override;
      procedure SetY (const aValue: Integer); override;
    (* Element size. *)
      procedure SetWidth (const aValue: Integer); override;
      procedure SetHeight (const aValue: Integer); override;
    (* Gain. *)
      function GetGain: Real; virtual; abstract;
      procedure SetGain (const aValue: Real); virtual; abstract;
    (* Attachment. *)
      function DoAttach (aMixer: TMixer): Boolean; overload; virtual;
      function DoAttach (aSample: TSampleInstance): Boolean; overload; virtual;
      function DoAttach (aAudioStream: TAudioStream): Boolean;
        overload; virtual;
      function DoDetach: Boolean; virtual; abstract;
    (* Events. *)
      procedure onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT); override;
      procedure onMouseDown (aMouse: ALLEGRO_MOUSE_EVENT); override;
      procedure onMouseUp (aMouse: ALLEGRO_MOUSE_EVENT); override;
      procedure onMouseMove (aMouse: ALLEGRO_MOUSE_EVENT); override;
    public
    (* Constructor. *)
      constructor Create; override;
    (* Want focus? *)
      function WantFocus: Boolean; override;
    (* Draw an arrow from the element output point to the given point. *)
      procedure DrawArrow (const aX, aY: Integer);
    (* Render element. *)
      procedure Draw; override;
    (* Get element input point. *)
      procedure GetInputPoint (out aX, aY: Integer);
    (* Attach given element. *)
      procedure Attach (aElement: TElement);
    (* Remove attachment. *)
      procedure Detach;
    (* Check if is attached to given element. *)
      function IsAttachedTo (const aElement: TElement): Boolean;
    (* Tell if element is playing. *)
      function IsPlaying: Boolean; virtual; abstract;
    (* Change playing state. *)
      procedure TogglePlaying; virtual; abstract;

    (* Gain. *)
      property Gain: Real read GetGain write SetGain;
    end;



    TVoice = class (TElement)
    private
      fVoice: ALLEGRO_VOICEptr;

      function GetValid: Boolean;
    protected
    (* Gain. *)
      function GetGain: Real; override;
      procedure SetGain (const aValue: Real); override;
    (* Attachment. *)
      function DoAttach (aMixer: TMixer): Boolean; override;
      function DoAttach (aSample: TSampleInstance): Boolean; override;
      function DoAttach (aAudioStream: TAudioStream): Boolean;
        override;
      function DoDetach: Boolean; override;
    public
    (* Constructor. *)
      constructor Create; override;
    (* Destructor. *)
      destructor Destroy; override;
    (* Tell if element is playing. *)
      function IsPlaying: Boolean; override;
    (* Change playing state. *)
      procedure TogglePlaying; override;

    (* Tell if voice is valid. *)
      property Valid: Boolean read GetValid;
    end;



    TMixer = class (TElement)
    private
      fMixer: ALLEGRO_MIXERptr;
    protected
    (* Gain. *)
      function GetGain: Real; override;
      procedure SetGain (const aValue: Real); override;
    (* Attachment. *)
      function DoAttach (aMixer: TMixer): Boolean; override;
      function DoAttach (aSample: TSampleInstance): Boolean; override;
      function DoAttach (aAudioStream: TAudioStream): Boolean;
        override;
      function DoDetach: Boolean; override;
    public
    (* Constructor. *)
      constructor Create; override;
    (* Destructor. *)
      destructor Destroy; override;
    (* Tell if mixer is playing. *)
      function IsPlaying: Boolean; override;
    (* Change playing state. *)
      procedure TogglePlaying; override;

    (* Instance to the mixer. *)
      property Instance: ALLEGRO_MIXERptr read fMixer;
    end;



    TSampleInstance = class (TElement)
    private
      fSampleInstance: ALLEGRO_SAMPLE_INSTANCEptr;
      fSample: TSample;
      fPos: LongWord;

      procedure SetSample (aSample: TSample);
    protected
    (* Gain. *)
      function GetGain: Real; override;
      procedure SetGain (const aValue: Real); override;
    (* Attachment. *)
      function DoDetach: Boolean; override;
    (* Keyboard events (to allow sample change). *)
      procedure onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT); override;
    public
    (* Constructor. *)
      constructor Create; override;
    (* Destructor. *)
      destructor Destroy; override;
    (* Tell if sample is playing. *)
      function IsPlaying: Boolean; override;
    (* Change playing state. *)
      procedure TogglePlaying; override;

    (* Pointer to the instance. *)
      property Instance: ALLEGRO_SAMPLE_INSTANCEptr read fSampleInstance;
    (* Actual sample. *)
      property Sample: TSample read fSample write SetSample;
    end;



    TSample = class (TObject)
    private
      fSample: ALLEGRO_SAMPLEptr;
      fFileName: String;
    public
    (* Constructor. *)
      constructor Create (const aFileName: String);
    (* Destructor. *)
      destructor Destroy; override;
    (* Check if sample is valid. *)
      function IsValid: Boolean;

    (* Pointer to the sample. *)
      property Instance: ALLEGRO_SAMPLEptr read fSample;
    (* Sample file name. *)
      property Filename: String read fFileName;
    end;



    TAudioStream = class (TElement)
    private
      fAudioStream: ALLEGRO_AUDIO_STREAMptr;

      function GetValid: Boolean;
    protected
    (* Gain. *)
      function GetGain: Real; override;
      procedure SetGain (const aValue: Real); override;
    (* Attachment. *)
      function DoDetach: Boolean; override;
    public
    (* Constructor. *)
      constructor Create (const aFileName: String); overload;
    (* Destructor. *)
      destructor Destroy; override;
    (* Check if audio stream is playing. *)
      function IsPlaying: Boolean; override;
    (* Change playing state. *)
      procedure TogglePlaying; override;

    (* Pointer to the stream. *)
      property Instance: ALLEGRO_AUDIO_STREAMptr read fAudioStream;
    (* Tell if stream is valid. *)
      property Valid: Boolean read GetValid;
    end;



  (* Extends dialog to build the example. *)
    TAudioMixerExample = class (TDialog)
    private
    { TFPObjectList would be a bit faster but I'm trying to be Delphi7
      compatible. }
      fSamples: TObjectList;
      fStreamPaths: TStrings;
      fNextStreamPath, fNextSample: Integer;
      fErrorMessage: TLabel;

      fFirstElementIndex: Integer;

      function GetSample (const aNdx: Integer): TSample;
    (* Create elements. *)
      function CreateVoice: TVoice;
      function CreateMixer: TMixer;
      function CreateSampleInstance: TSampleInstance;
      function CreateAudioStream: TAudioStream;
    public
    (* Constructor. *)
      constructor Create; override;
    (* Destructor. *)
      destructor Destroy; override;
    (* Set (or remove) an error message. *)
      procedure SetErrorMessage (const aMessage: String);
    (* Initialize the example. *)
      procedure Initialize; override;
    (* Destroy an element. *)
      procedure DeleteElement (aElement: TElement);
    (* Process commands. *)
      procedure ProcessCommand (aCommand: AnsiChar);

    (* Indexed access to samples. *)
      property Sample[const aNdx: Integer]: TSample read GetSample;
    end;



(*
 * TElement
 *************************************************************************)

  procedure TElement.SetErrorMessage (const aMessage: String);
  begin
    TAudioMixerExample (Self.Dialog).SetErrorMessage (aMessage)
  end;



  procedure TElement.ClearErrorMessage;
  begin
    TAudioMixerExample (Self.Dialog).SetErrorMessage ('')
  end;



  procedure TElement.SetX (const aValue: Integer);
  begin
    if aValue < 0 then
      inherited SetX (0)
    else if aValue + Self.Width > wWidth then
      inherited SetX (wWidth - Self.Width)
    else
      inherited SetX (aValue)
  end;



  procedure TElement.SetY (const aValue: Integer);
  begin
    if aValue < 0 then
      inherited SetY (0)
    else if aValue + Self.Height > wHeight then
      inherited SetY (wHeight - Self.Height)
    else
      inherited SetY (aValue)
  end;



  procedure TElement.SetWidth (const aValue: Integer);
  begin
    inherited SetWidth (aValue);
    Self.SetX (Self.x) { To keep it inside the window. }
  end;



  procedure TElement.SetHeight (const aValue: Integer);
  begin
    inherited SetHeight (aValue);
    Self.SetY (Self.y) { To keep it inside the window. }
  end;



  function TElement.DoAttach (aMixer: TMixer): Boolean;
  begin
    Result := False
  end;



  function TElement.DoAttach (aSample: TSampleInstance): Boolean;
  begin
    Result := False
  end;



  function TElement.DoAttach (aAudioStream: TAudioStream): Boolean;
  begin
    Result := False
  end;



  procedure TElement.onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT);
  begin
    case aEvent.unichar of
    Ord (' '):
      Self.TogglePlaying;
    Ord ('+'):
      Self.SetGain (Self.GetGain + VolumeChangeRate);
    Ord ('-'):
      Self.SetGain (Self.GetGain - VolumeChangeRate);
    Ord ('x'), Ord ('X'):
      TAudioMixerExample (Self.Dialog).DeleteElement (Self);
    else { otherwise }
      if aEvent.unichar < 128 then
      { Delegate. }
        TAudioMixerExample (Self.Dialog).ProcessCommand (AnsiChar (aEvent.unichar));
    end
  end;



  procedure TElement.onMouseDown (aMouse: ALLEGRO_MOUSE_EVENT);
  begin
  { Set up drag information. }
    if fDragButton = 0 then fDragButton := aMouse.button;
    if fDragButton = 2 then
    begin
      fConnectX := aMouse.x;
      fConnectY := aMouse.y
    end
  end;



  procedure TElement.onMouseUp (aMouse: ALLEGRO_MOUSE_EVENT);
  var
    lNdx: Integer;
  begin
  { Only if it's the same button. }
    if aMouse.button <> fDragButton then Exit;
    fDragButton := 0;
  { Change attachment. }
    if aMouse.button = 2 then
    begin
      Self.Detach;
      lNdx := Self.Dialog.SearchWidgetIn (aMouse.x, aMouse.y);
      if lNdx > 0 then TElement (Self.Dialog.Widget[lNdx]).Attach (Self)
    { If not element is found, just detach. }
    end
  end;



  procedure TElement.onMouseMove (aMouse: ALLEGRO_MOUSE_EVENT);
  begin
  { If it is the wheel... }
    if aMouse.dz <> 0 then
      Self.SetGain (Self.GetGain + (aMouse.dz * VolumeChangeRate))
    else case fDragButton of
    1: { Drag the element. }
      Self.MoveBy (aMouse.dx, aMouse.dy);
    2: { Change attachment arrow point position. }
      begin
        fConnectX := aMouse.x;
        fConnectY := aMouse.Y
      end;
    end
  end;



  constructor TElement.Create;
  begin
    inherited Create (Random (wWidth), Random (wHeight), 40, 40);
    fAttachedTo := Nil
  end;



  function TElement.WantFocus: Boolean;
  begin
    Result := True
  end;



  procedure TElement.DrawArrow (const aX, aY: Integer);
  const
    LengthPointer = 7;
  var
    lx, ly: Integer;
    lAtan, la1, la2: Real;
    lColor: ALLEGRO_COLOR;
  begin
  { Get output point. }
    lx := Self.x + Self.Width div 2;
    ly := Self.y;
  { Draw arrow vector. }
    if Self.Focus then
      lColor := Self.dialog.clrSelectedText
    else
      lColor := Self.dialog.clrForeground;
    al_draw_line (lx, ly, ax, ay, lColor, 1);
  { Calculate point angles. }
    lAtan := ArcTan2 (ly - ay, lx - ax);
    la1 := lAtan + 0.5;
    la2 := lAtan - 0.5;
  { Draw point. }
    al_draw_line (
      ax, ay,
      ax + LengthPointer * Cos (la1), ay + LengthPointer * Sin (la1),
      lColor, 1
    );
    al_draw_line (
      ax, ay,
      ax + LengthPointer * Cos (la2), ay + LengthPointer * Sin (la2),
      lColor, 1
    )
  end;



  procedure TElement.Draw;
  const
    Radius = 7;
  var
    lTextCol, lBoxCol: ALLEGRO_COLOR;
    lGain: Real;
    lx, ly: Integer;
  begin
  { Mouse button 2 is used to select attachments, so if pressed draw an arrow to
    show the operation.
  }
    if fDragButton = 2 then
      Self.DrawArrow (fConnectX, fConnectY);
  { Draw arrow connecting attached elements. }
    if Assigned (fAttachedTo) then
    begin
      fAttachedTo.GetInputPoint (lx, ly);
      Self.DrawArrow (lx, ly)
    end;
  { Get colors. }
    if Self.Focus then
      lBoxCol := Self.Dialog.clrSelectedText
    else
      lBoxCol := Self.Dialog.clrForeground;
    if Self.IsPlaying then
      lTextCol := Self.Dialog.clrForeground
    else
      lTextCol := Self.Dialog.clrDisabled;
  { Draw control. }
    al_draw_filled_rounded_rectangle (
      Self.x + 0.5, Self.y + 0.5,
      Self.x + Self.Width - 0.5, Self.y + Self.Height -0.5,
      Radius, Radius,
      Self.Dialog.clrBackground
    );
    al_draw_rounded_rectangle (
      Self.x + 0.5, Self.y + 0.5,
      Self.x + Self.Width - 0.5, Self.y + Self.Height -0.5,
      Radius, Radius,
      lBoxCol,
      1
    );
    al_draw_text (
      Self.Dialog.TextFont, lTextCol,
      Self.x + Self.Width / 2,
      Self.y + Self.Height / 2 - Self.Dialog.LineHeight / 2,
      ALLEGRO_ALIGN_CENTRE,
      Self.CaptionStr
    );
    lGain := Self.Gain;
    if lGain >= 0 then
    begin
      lGain := 1 - Clamp (0, lGain, 2) / 2;
      al_draw_rectangle (
        Self.x + Self.Width + 1.5, Self.y + Self.Height * lGain,
        Self.x + Self.Width + 3.5, Self.y + Self.Height,
        lBoxCol, 1
      )
    end
  end;



  procedure TElement.GetInputPoint (out ax, ay: Integer);
  begin
    ax := Self.x + Self.Width div 2;
    ay := Self.y + Self.Height
  end;



  procedure TElement.Attach (aElement: TElement);
  var
    lAttachResult: Boolean;
  begin
    lAttachResult := False;
    if aElement is TMixer then
      lAttachResult := Self.DoAttach (TMixer (aElement))
    else if aElement is TSampleInstance then
      lAttachResult := Self.DoAttach (TSampleInstance (aElement))
    else if aElement is TAudioStream then
      lAttachResult := Self.DoAttach (TAudioStream (aElement))
    else
      Self.SetErrorMessage ('Trying to attach a non attachable element.');
    if lAttachResult then
    begin
      Self.ClearErrorMessage;
      aElement.fAttachedTo := Self
    end
    else
      Self.SetErrorMessage ('Can''t attach to that element.');
  end;



  procedure TElement.Detach;
  begin
    if Assigned (fAttachedTo) and Self.DoDetach then fAttachedTo := Nil
  end;



  function TElement.IsAttachedTo (const aElement: TElement): Boolean;
  begin
    Result := Assigned (aElement) and (aElement = fAttachedTo)
  end;



(*
 * TVoice
 *************************************************************************)

  function TVoice.GetValid: Boolean;
  begin
    Result := Assigned (fVoice)
  end;



  function TVoice.GetGain: Real;
  begin
    Result := -1
  end;



  procedure TVoice.SetGain (const aValue: Real);
  begin end;



  function TVoice.DoAttach (aMixer: TMixer): Boolean;
  begin
    Result := al_attach_mixer_to_voice (aMixer.Instance, fVoice)
  end;



  function TVoice.DoAttach (aSample: TSampleInstance): Boolean;
  begin
    Result := al_attach_sample_instance_to_voice (aSample.Instance, fVoice)
  end;



  function TVoice.DoAttach (aAudioStream: TAudioStream): Boolean;
  begin
    Result := al_attach_audio_stream_to_voice (aAudioStream.Instance, fVoice)
  end;



  function TVoice.DoDetach: Boolean;
  begin
    Result := False
  end;



  constructor TVoice.Create;
  begin
    inherited Create;
    Self.Caption := 'Voice';
    fVoice := al_create_voice (SampleRate, VoiceAudioDepth, ChannelConfig)
  end;



  destructor TVoice.Destroy;
  begin
    if Assigned (fVoice) then al_destroy_voice (fVoice);
    inherited Destroy
  end;



  function TVoice.IsPlaying: Boolean;
  begin
    Result := al_get_voice_playing (fVoice)
  end;



  procedure TVoice.TogglePlaying;
  begin
    if al_set_voice_playing (fVoice, not Self.IsPlaying) then
      Self.ClearErrorMessage
    else
      Self.SetErrorMessage ('Cannot toggle voice playing!')
  end;



(*
 * TMixer
 *************************************************************************)

  function TMixer.GetGain: Real;
  begin
    Result := al_get_mixer_gain (fMixer)
  end;



  procedure TMixer.SetGain (const aValue: Real);
  begin
    if al_set_mixer_gain (fMixer, Clamp (0, aValue, 2)) then
      Self.ClearErrorMessage
    else
      Self.SetErrorMessage ('Cannot set mixer gain!')
  end;



  function TMixer.DoAttach (aMixer: TMixer): Boolean;
  begin
    Result := al_attach_mixer_to_mixer (aMixer.Instance, fMixer)
  end;



  function TMixer.DoAttach (aSample: TSampleInstance): Boolean;
  begin
    Result := al_attach_sample_instance_to_mixer (aSample.Instance, fMixer)
  end;



  function TMixer.DoAttach (aAudioStream: TAudioStream): Boolean;
  begin
    Result := al_attach_audio_stream_to_mixer (aAudioStream.Instance, fMixer)
  end;



  function TMixer.DoDetach: Boolean;
  begin
    Result := al_detach_mixer (fMixer)
  end;



  constructor TMixer.Create;
  begin
    inherited Create;
    Self.Caption := 'Mixer';
    fMixer := al_create_mixer (SampleRate, MixerAudioDepth, ChannelConfig)
  end;



  destructor TMixer.Destroy;
  begin
    if Assigned (fMixer) then al_destroy_mixer (fMixer);
    inherited Destroy
  end;



  function TMixer.IsPlaying: Boolean;
  begin
    Result := al_get_mixer_playing (fMixer)
  end;



  procedure TMixer.TogglePlaying;
  begin
    if al_set_mixer_playing (fMixer, not Self.IsPlaying) then
      Self.ClearErrorMessage
    else
      Self.SetErrorMessage ('Cannot toggle mixer playing!')
  end;



(*
 * TSampleInstance
 *************************************************************************)

  procedure TSampleInstance.SetSample (aSample: TSample);
  var
    lPlaying: Boolean;
  begin
    lPlaying := Self.IsPlaying;
    if Assigned (aSample) and al_set_sample (fSampleInstance, aSample.Instance)
    then
    begin
      fSample := aSample;
      al_set_sample_instance_playmode (fSampleInstance, ALLEGRO_PLAYMODE_LOOP);
      al_set_sample_instance_playing (fSampleInstance, lPlaying);
      Self.Caption := aSample.Filename
    end
    else
      Self.Caption := 'No sample'
  end;



  function TSampleInstance.GetGain: Real;
  begin
    Result := al_get_sample_instance_gain (fSampleInstance)
  end;



  procedure TSampleInstance.SetGain (const aValue: Real);
  begin
    if al_set_sample_instance_gain (fSampleInstance, Clamp(0,aValue,2)) then
      Self.ClearErrorMessage
    else
      Self.SetErrorMessage ('Cannot set sample gain!')
  end;



  function TSampleInstance.DoDetach: Boolean;
  begin
    Result := al_detach_sample_instance (fSampleInstance)
  end;



  procedure TSampleInstance.onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT);
  begin
    case aEvent.unichar of
    Ord ('1')..Ord ('9'):
      Self.SetSample (
        TAudioMixerExample (Self.Dialog).Sample[aEvent.unichar - Ord ('1')]
      );
    else { otherwise }
      inherited onKeyChar (aEvent)
    end
  end;



  constructor TSampleInstance.Create;
  begin
    inherited Create;
    fSample := Nil;
    Self.Width := 150;
    fSampleInstance := al_create_sample_instance (Nil)
  end;



  destructor TSampleInstance.Destroy;
  begin
    al_destroy_Sample_Instance (fSampleInstance);
    inherited Destroy
  end;



  function TSampleInstance.IsPlaying: Boolean;
   begin
    Result := al_get_sample_instance_playing (fSampleInstance)
   end;



  procedure TSampleInstance.TogglePlaying;
  var
    lIsPlaying: Boolean;
  begin
    lIsPlaying := Self.IsPlaying;
    if lIsPlaying then
      fPos := al_get_sample_instance_position (fSampleInstance)
    else
      al_set_sample_instance_position (fSampleInstance, fPos);
    if al_set_sample_instance_playing (fSampleInstance, not lIsPlaying) then
      Self.ClearErrorMessage
    else
      Self.SetErrorMessage ('Cannot toggle sample playing!')
  end;



(*
 * TSample
 *************************************************************************)

  constructor TSample.Create (const aFileName: String);
  begin
    inherited Create;
    fSample := al_load_sample (al_string_to_str (aFileName));
    fFileName := ExtractFileName (aFileName)
  end;



  destructor TSample.Destroy;
  begin
    if Assigned (fSample) then al_destroy_sample (fSample);
    inherited Destroy
  end;



  function TSample.IsValid: Boolean;
  begin
    result := Assigned (fSample)
  end;



(*
 * TAudioStream
 *************************************************************************)

  function TAudioStream.GetValid: Boolean;
  begin
    Result := Assigned (fAudioStream)
  end;



  function TAudioStream.GetGain: Real;
  begin
    Result := al_get_audio_stream_gain (fAudioStream)
  end;



  procedure TAudioStream.SetGain (const aValue: Real);
  begin
    if al_set_audio_stream_gain (fAudioStream, Clamp (0, aValue, 2)) then
      Self.ClearErrorMessage
    else
      Self.SetErrorMessage ('Cannot set audio stream gain!')
  end;



  function TAudioStream.DoDetach: Boolean;
  begin
    Result := al_detach_audio_stream (fAudioStream)
  end;



  constructor TAudioStream.Create (const aFileName: String);
  begin
    inherited Create;
    Self.Width := 150;
    fAudioStream := al_load_audio_stream (
      al_string_to_str (aFileName),
      AudioBufferCount, AudioSampleSize
    );
    if Assigned (fAudioStream) then
    begin
      al_set_audio_stream_playmode (fAudioStream, ALLEGRO_PLAYMODE_LOOP);
      Self.Caption := ExtractFileName (aFileName)
    end
    else
      Self.Caption := 'No stream'
  end;



  destructor TAudioStream.Destroy;
  begin
    if Assigned (fAudioStream) then al_destroy_audio_stream (fAudioStream);
    inherited Destroy
  end;



  function TAudioStream.IsPlaying: Boolean;
  begin
    Result := al_get_audio_stream_playing (fAudioStream)
  end;



  procedure TAudioStream.TogglePlaying;
  begin
    if al_set_audio_stream_playing (fAudioStream, not Self.IsPlaying) then
      Self.ClearErrorMessage
    else
      Self.SetErrorMessage ('Cannot toggle audio stream playing!')
  end;



(*
 * TAudioMixerExample
 *************************************************************************)

  function TAudioMixerExample.GetSample (const aNdx: Integer): TSample;
  begin
    Result := TSample (fSamples[aNdx])
  end;



  function TAudioMixerExample.CreateVoice: TVoice;
  begin
    Result := TVoice.Create;
    if Assigned (Result) then
      Self.AppendWidget (Result)
    else
      FreeAndNil (Result)
  end;



  function TAudioMixerExample.CreateMixer: TMixer;
  begin
    Result := TMixer.Create;
    if Assigned (Result) then Self.AppendWidget (Result)
  end;



  function TAudioMixerExample.CreateSampleInstance: TSampleInstance;
  var
    lNdx: Integer;
  begin
    Result := TSampleInstance.Create;
    if Assigned (Result) then
    begin
      if fSamples.Count > 0 then
      begin
        lNdx := fNextSample mod fSamples.Count;
        Inc (fNextSample, 2);
        Result.Sample := TSample (fSamples[lNdx])
      end;
      Self.AppendWidget (Result)
    end
  end;



  function TAudioMixerExample.CreateAudioStream: TAudioStream;
  var
    lNdx: Integer;
  begin
    if fStreamPaths.Count > 0 then
    begin
      lNdx := fNextStreamPath mod fStreamPaths.Count;
      Inc (fNextStreamPath);
      Result := TAudioStream.Create (fStreamPaths[lNdx]);
      if not Assigned (Result) or not Result.Valid then
      begin
        Result.Free;
        Exit (Nil)
      end;
      Self.AppendWidget (Result);
    end
    else
      Result := Nil
  end;



  constructor TAudioMixerExample.Create;
  begin
    inherited Create;
    fSamples := TObjectList.Create (True);
    fStreamPaths := TStringList.Create;
    fNextStreamPath := 0; fNextSample := 0
  end;



  destructor TAudioMixerExample.Destroy;
  begin
    fSamples.Free;
    fStreamPaths.Free;
    inherited Destroy
  end;



  procedure TAudioMixerExample.SetErrorMessage (const aMessage: String);
  begin
    fErrorMessage.Caption := aMessage
  end;



  procedure TAudioMixerExample.Initialize;

    procedure InitDialog;
    begin
      if Self.Terminated then Exit;
      Self.Title := 'Allegro''s audio mixer example';
      if not al_install_audio or not al_init_acodec_addon then
      begin
        Self.ShowErrorMessage ('Could not initialize audio.');
        Self.Terminate;
        Exit
      end;
      if not al_init_ttf_addon then
      begin
        Self.ShowErrorMessage ('Could not initialize TTF fonts.');
        Self.Terminate;
        Exit
      end;
      Self.TextFont := al_load_ttf_font ('data/DejaVuSans.ttf', 10, 0);
      if not Assigned (Self.TextFont) then
      begin
        Self.ShowErrorMessage ('Could not load DejaVuSans.ttf.');
        Self.Terminate;
        Exit
      end;
      Self.clrSelectedText := al_map_rgb_f (1, 0.1, 0.1)
    end;

    procedure AddSample (aFileName: String);
    var
      lSample :TSample;
    begin
      DoDirSeparators (aFileName);
      lSample := TSample.Create (aFileName);
      if Assigned (lSample) and lSample.IsValid then
        fSamples.Add (lSample)
      else
        lSample.Free
    end;

    procedure AddStreamPath (aFileName: String);
    begin
      DoDirSeparators (aFileName);
      fStreamPaths.Add (aFileName)
    end;

    procedure CreateWidgets;
    var
      lVoice: TVoice;
      lMixer, lMixer2: TMixer;
      lSample: TSampleInstance;
      lStram: TAudioStream;
    begin
      if Self.Terminated then Exit;
    { Labels. }
      fErrorMessage := TColoredLabel.CreateColoredLabel (
        0, 0, '',
        al_map_rgb (204, 0, 0), al_map_rgb (255, 255, 255)
      );
      Self.AppendWidget (fErrorMessage);
      Self.AppendWidget (TLabel.CreateLabel (
        0, wHeight - Self.LineHeight * 2,
        Concat (
          'Create [v]oices, [m]ixers, [s]ample instances, [a]udiostreams.   ',
          '[SPACE] pause playback.     ',
          '[1]-[9] set sample.    ',
          '[x] delete.'
        )
      ));
      Self.AppendWidget (TLabel.CreateLabel (
        0, wHeight - Self.LineHeight * 1,
        Concat (
          'Mouse: [LBM] select element.   ',
          '[RBM] attach sources to sinks ',
          '(sample->mixer, mixer->mixer, mixer->voice, sample->voice)'
        )
      ));
    { Elements. }
      fFirstElementIndex := Self.WidgetCount;
      lVoice := Self.CreateVoice;
      if not Assigned (lVoice) then
      begin
        Self.ShowErrorMessage ('Could not create initial voice.');
        Self.Terminate;
        Exit
      end;
      lVoice.x := 300; lVoice.y := 50;

      lMixer := Self.CreateMixer;
      lMixer.x := 300; lMixer.y := 150;
      lVoice.Attach (lMixer);

      lMixer2 := Self.CreateMixer;
      lMixer2.x := 500; lMixer2.y := 250;
      lMixer.Attach (lMixer2);

      lSample := Self.CreateSampleInstance;
      lSample.x := 220; lSample.y := 300;
      lMixer.Attach (lSample);
      lSample.TogglePlaying;

      lSample := Self.CreateSampleInstance;
      lSample.x := 120; lSample.y := 240;
      lMixer.Attach (lSample);
      lSample.TogglePlaying;

      lStram := Self.CreateAudioStream;
      if Assigned (lStram) then
      begin
        lStram.x := 450; lStram.y := 350;
        lMixer2.Attach (lStram)
      end
      else
        Self.SetErrorMessage ('Could not init audio stream');
      Self.Reset
    end;

  const
    Samples: array [1..9] of String = (
      'data/haiku/air_0.ogg',
      'data/haiku/air_1.ogg',
      'data/haiku/earth_0.ogg',
      'data/haiku/earth_1.ogg',
      'data/haiku/earth_2.ogg',
      'data/haiku/fire_0.ogg',
      'data/haiku/fire_1.ogg',
      'data/haiku/water_0.ogg',
      'data/haiku/water_1.ogg'
    );
    StreamPaths: array [1..1] of String = (
    { TODO:
      '../demos/cosmic_protector/data/sfx/game_music.ogg',
      '../demos/cosmic_protector/data/sfx/title_music.ogg'
    }
      '../demos/furiouspaladin/data/auduntitledremix.ogg'
    );
  var
    lNdx: Integer;
  begin
    inherited Initialize;
    InitDialog;
    for lNdx := Low (StreamPaths) to High (StreamPaths) do
      AddStreamPath (StreamPaths[lNdx]);
    for lNdx := Low (Samples) to High (Samples) do
      AddSample (Samples[lNdx]);
    CreateWidgets
  end;



  procedure TAudioMixerExample.DeleteElement (aElement: TElement);
  var
    lNdx: Integer;
    lElement: TElement;
  begin
  { I would use a for..in loop here, but I'm trying to be Delphi7 compatible. }
  { Check where the element is attached to. }
    for lNdx := fFirstElementIndex to Self.WidgetCount - 1 do
    begin
      lElement := TElement (Self.Widget[lNdx]);
      if lElement.IsAttachedTo (aElement) then
        lElement.Detach
    end;
  { Detach element. }
    aElement.Detach;
  { Remove from dialog. }
    Self.RemoveWidget (aElement)
  end;



  procedure TAudioMixerExample.ProcessCommand (aCommand: AnsiChar);
  begin
    Self.SetErrorMessage ('');
    case aCommand of
    'A', 'a':
      if Self.CreateAudioStream = Nil then
        Self.SetErrorMessage ('Cannot create a new audio stream.');
    'M', 'm':
      if Self.CreateMixer = Nil then
        Self.SetErrorMessage ('Cannot create a new mixer.');
    'S', 's':
      if Self.CreateSampleInstance = Nil then
        Self.SetErrorMessage ('Cannot create a new sample instance.');
    'V', 'v':
      if Self.CreateVoice = Nil then
        Self.SetErrorMessage ('Cannot create a new voice.');
    end
  end;

var
  AudioMixerExample: TAudioMixerExample;
begin
  AudioMixerExample := TAudioMixerExample.Create;
  try
    AudioMixerExample.Initialize;
    AudioMixerExample.Run
  finally
    AudioMixerExample.Free
  end
end.
