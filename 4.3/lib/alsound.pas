UNIT alsound;
(*<Sound initialization and configuration.

   Allegro allows you to use the sound hardware in two ways:  automatic, or
   manual.  Usually you should try the automatic version first.  This means
   calling @link(al_install_sound) with the autodetection parameters and using
   the rest of the sound functions to play samples or music.  In this
   situation, Allegro will handle the sound devices and mix the samples and/or
   music the best way it can.

   However, sound hardware has a limitation on the number of samples it may
   play all at the same time (from now on, called hardware voices).  When you
   exceed this limit, Allegro will cut off one of the samples being played and
   reproduce the new one.  Depending on the type of sounds you are playing, how
   many of them you need at the same time and their nature (e.g: vital audio
   feedback to the user or useless "ping" when some shrapnel hits a rock in the
   scenery) you will want to specify more carefully how hardware voices are
   reserved and which samples have priority over others.

   The hardware voice reservation phase has to be done before the call to
   @link(al_install_sound), since it directly affects how Allegro talks to the
   sound drivers.  *)

INTERFACE

USES
  albase;



CONST
(* Identifier to pass to @link(al_install_sound). *)
  AL_DIGI_AUTODETECT	= -1;
(* Identifier to pass to @link(al_install_sound). *)
  AL_DIGI_NONE		= 0;
(* Identifier to pass to @link(al_install_sound). *)
  AL_MIDI_AUTODETECT	= -1;
(* Identifier to pass to @link(al_install_sound). *)
  AL_MIDI_NONE		=  0;
(* Identifier to pass to @link(al_install_sound). *)
  AL_MIDI_DIGMID	= $44494749; { AL_ID ('DIGI'); }



(* Call this function to specify the number of voices that are to be used by
   the digital and MIDI sound drivers respectively.  This must be done
   @bold(before) calling @link(al_install_sound).  If you reserve too many
   voices, subsequent calls to @code(al_install_sound) will fail.  How many
   voices are available depends on the driver, and in some cases you will
   actually get more than you reserve (eg. the FM synth drivers will always
   provide 9 voices on an OPL2 and 18 on an OPL3, and the SB digital driver
   will round the number of voices up to the nearest power of two).  Pass
   negative values to restore the default settings.  You should be aware that
   the sound quality is usually inversely related to how many voices you use,
   so don't reserve any more than you really need. *)
  PROCEDURE al_reserve_voices (digi_voices, midi_voidces: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'reserve_voices';

(* By default, Allegro will play a centered sample at half volume on both the
   left and right channel.  A sample panned to the far right or left will be
   played at maximum volume on that channel only.  This is done so you can play
   a single panned sample without distortion.  If you play multiple samples at
   full volume, the mixing process can result in clipping, a noticeable form of
   distortion.  The more samples, the more likely clipping is to occur, and the
   more clipping, the worse the output will sound.

   If clipping is a problem - or if the output is too quiet - this function can
   be used to adjust the volume of each voice.  You should first check that
   your speakers are at a reasonable volume, Allegro's global volume is at
   maximum (see @link(al_set_volume)), and any other mixers such as the Volume
   Control are set reasonably.  Once you are sure that Allegro's output level
   is unsuitable for your application, use this function to adjust it.

   Each time you increase the parameter by one, the volume of each voice will
   halve.  For example, if you pass 4, you can play up to 16 centred samples at
   maximum volume without distortion.

   If you pass 0 to this function, each centred sample will play at the maximum
   volume possible without distortion, as will all samples played through a
   mono driver.  Samples at the extreme left and right will distort if played
   at full volume.  If you wish to play panned samples at full volume without
   distortion, you should pass 1 to this function.

   Of course this function does not override the volume you specify with
   @link(al_play_sample) or @link(al_set_volume).  It simply alters the
   overall output of the program.  If you play samples at lower volumes, or if
   they are not normalised, then you can play more of them without distortion.

   It is recommended that you hard-code the parameter into your program, rather
   than offering it to the user.  The user can alter the volume with the
   configuration file instead, or you can provide for this with
   @link(al_set_volume). *)
  PROCEDURE al_set_volume_per_voice (scale: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_volume_per_voice';

(* Initialises the sound module.  You should normally pass
   @code(AL_DIGI_AUTODETECT) and @code(AL_MIDI_AUTODETECT) as the driver
   parameters to this function, in which case Allegro will read hardware
   settings from the current configuration file.  This allows the user to
   select different values with the setup utility:  see the @link(alconfig
   config unit) for details.

   @returns (@true if the sound is successfully installed, and @false on
     failure.  If it fails it will store a description of the problem in
     @link(al_error).) *)
  FUNCTION al_install_sound (digi, midi: LONGINT): BOOLEAN;

(* Cleans up after you are finished with the sound routines.  You don't
   normally need to call this, because @link(al_exit) will do it for you. *)
  PROCEDURE al_remove_sound; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_sound';



(* Alters the global sound output volume.  Specify volumes for both digital
   samples and MIDI playback, as integers from 0 to 255, or pass a negative
   value to leave one of the settings unchanged.  Values bigger than 255 will
   be reduced to 255.  This routine will not alter the volume of the hardware
   mixer if it exists (i.e. only your application will be affected). *)
  PROCEDURE al_set_volume (digi, midi: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_volume';

(* Alters the hardware sound output volume.  Specify volumes for both digital
   samples and MIDI playback, as integers from 0 to 255, or pass a negative
   value to leave one of the settings unchanged.  Values bigger than 255 will
   be reduced to 255.  This routine will use the hardware mixer to control the
   volume if it exists (i.e. the volume of all the applications on your machine
   will be affected), otherwise do nothing. *)
  PROCEDURE al_set_hardware_volume (digi, midi: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_hardware_volume';

(* Retrieves the global sound output volume, both for digital samples and MIDI
   playback, as integers from 0 to 255. *)
  PROCEDURE al_get_volume (VAR digi, midi: LONGINT);

(* Retrieves the hardware sound output volume, both for digital samples and
   MIDI playback, as integers from 0 to 255, or -1 if the information is not
   available. *)
  PROCEDURE al_get_hardware_volume (VAR digi, midi: LONGINT);



(* Sets the resampling quality of the mixer.  Valid values are the same as the
  @code(quality) config variable.  Please read chapter "Standard config
  variables" for details.  You can call this function at any point in your
  program, even before @link(al_init). *)
  PROCEDURE al_set_mixer_quality (quality: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mixer_quality';

(* Returns the current mixing quality, as specified by the @code(quality)
   config variable, or a previous call to @link(al_set_mixer_quality). *)
  FUNCTION al_get_mixer_quality: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_quality';

(* Returns the mixer frequency, in Hz. *)
  FUNCTION al_get_mixer_frequency: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_frequency';

(* Returns the mixer bit depth (8 or 16). *)
  FUNCTION al_get_mixer_bits: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_bits';

(* Returns the number of output channels. 2 for stereo, 1 for mono, 0 if the
   mixer isn't active. *)
  FUNCTION al_get_mixer_channels: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_channels';

(* Returns the number of voices allocated to the mixer. *)
  FUNCTION al_get_mixer_voices: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_voices';

(* Returns the number of samples per channel in the mixer buffer. *)
  FUNCTION al_get_mixer_buffer_length: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_buffer_length';



IMPLEMENTATION



  FUNCTION install_sound (digi, midi: LONGINT; c: POINTER): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_sound (digi, midi: LONGINT): BOOLEAN;
  BEGIN
    al_install_sound := install_sound (digi, midi, NIL) = 0;
  END;



  PROCEDURE get_volume (digi, midi: PLONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_get_volume (VAR digi, midi: LONGINT);
  BEGIN
    get_volume (@digi, @midi);
  END;



  PROCEDURE get_hardware_volume (digi, midi: PLONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_get_hardware_volume (VAR digi, midi: LONGINT);
  BEGIN
    get_hardware_volume (@digi, @midi);
  END;

END.
