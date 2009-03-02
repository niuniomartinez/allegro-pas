UNIT alsound;
(*
  ______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\ 
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	Sound support routines.
 *	by Ñuño Martínez <>
 *
 *	See readme.txt for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$LONGSTRINGS ON}
{$ELSE}
{ Assumes Borland Delphi/Turbo. }
 {$A-}
 {$H+}
{$ENDIF}



INTERFACE

USES
  albase;



CONST
(* Digital sound for passing to al_install_sound() *)
  AL_DIGI_AUTODETECT	= -1;
  AL_DIGI_NONE		= 0;     
(* MIDI sound for passing to al_install_sound() *)
  AL_MIDI_AUTODETECT	= -1;
  AL_MIDI_NONE		=  0;
  AL_MIDI_DIGMID	= $44494749; { AL_ID ('D','I','G','I'); }



  PROCEDURE al_reserve_voices (digi_voices, midi_voidces: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'reserve_voices';
  PROCEDURE al_set_volume_per_voice (scale: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_volume_per_voice';

  FUNCTION al_install_sound (digi, midi: AL_INT; c: AL_PTR): AL_INT;
  PROCEDURE al_remove_sound; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_sound';

  FUNCTION al_install_sound_input (digi, midi: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'install_sound_input';
  PROCEDURE al_remove_sound_input; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_sound_input';

  PROCEDURE al_set_volume (digi, midi: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_volume';
  PROCEDURE al_set_hardware_volume (digi, midi: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_hardware_volume';

  PROCEDURE al_get_volume (digi, midi: AL_INTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_volume';
  PROCEDURE al_get_hardware_volume (digi, midi: AL_INTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_hardware_volume';

  PROCEDURE al_set_mixer_quality (quality: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mixer_quality';
  FUNCTION al_get_mixer_quality: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_quality';
  FUNCTION al_get_mixer_frequency: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_frequency';
  FUNCTION al_get_mixer_bits: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_bits';
  FUNCTION al_get_mixer_channels: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_channels';
  FUNCTION al_get_mixer_voices: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_voices';
  FUNCTION al_get_mixer_buffer_length: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_buffer_length';



IMPLEMENTATION

USES
  almidi;



(* Delphi can't access to the public variables from Allegro, so we need some
 * magic to access them. *)
  FUNCTION install_sound (digi, midi: AL_INT; c: AL_PTR): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION _get_midi_pos_: AL_LONGptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_midi_time_: AL_LONGptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;

FUNCTION al_install_sound (digi, midi: AL_INT; c: AL_PTR): AL_INT;
BEGIN
  al_install_sound := install_sound (digi, midi, c);
  al_midi_pos := _get_midi_pos_;
  al_midi_time := _get_midi_time_;
END;



END.

