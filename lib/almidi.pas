UNIT almidi;
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
 *	MIDI support routines.
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
  AL_MIDI_VOICES = 64;
  AL_MIDI_TRACKS = 32;



TYPE
(* a midi file *)
  AL_MIDIptr = ^AL_MIDI;
  AL_MIDI = RECORD
    divisions : AL_INT;		{ number of ticks per quarter note  }
    track : ARRAY[0..(AL_MIDI_TRACKS)-1] OF RECORD
      data : AL_UCHARptr;	{ MIDI message stream  }
      len : AL_INT;		{ length of the track data  }
    END;
  END;



VAR
  al_midi_pos, al_midi_time: AL_LONGptr;

  FUNCTION al_load_midi (filename: AL_STRING): AL_MIDIptr;
  PROCEDURE al_destroy_midi (midi: AL_MIDIptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_midi';
  FUNCTION al_play_midi (midi: AL_MIDIptr; loop: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'play_midi';
  FUNCTION al_play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'play_looped_midi';
  PROCEDURE al_stop_midi; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stop_midi';
  PROCEDURE al_midi_pause; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_pause';
  PROCEDURE al_midi_resume; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_resume';
  FUNCTION al_midi_seek(target: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_seek';
  FUNCTION al_load_midi_patches: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_midi_patches';



IMPLEMENTATION

  FUNCTION load_midi (filename: PCHAR): AL_MIDIptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_midi';

  FUNCTION al_load_midi (filename: AL_STRING): AL_MIDIptr;
  BEGIN
    al_load_midi := load_midi (PCHAR (filename));
  END;

END.

