UNIT aldigi;
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
  albase; { Needs some basic definitions. }



CONST
  AL_DIGI_VOICES	= 64;	{ theorical maximun }

TYPE
(* a sample *)
  AL_SAMPLEptr = ^AL_SAMPLE;
  AL_SAMPLE = RECORD
    bits : AL_INT;	   { 8 or 16  }
    stereo : AL_INT;	   { sample type flag  }
    freq : AL_INT;	   { sample frequency  }
    priority : AL_INT;	   { 0-255  }
    len : AL_ULONG;	   { length (in samples)  }
    loop_start : AL_ULONG; { loop start position  }
    loop_end : AL_ULONG;   { loop finish position  }
    param : AL_ULONG;	   { for internal use by the driver  }
    data : AL_PTR;	   { sample data  }
  END;

(* Register sample types. *)
  AL_SAMPLE_LOAD_FUNC = FUNCTION (filename: PCHAR): AL_SAMPLEptr; CDECL;
  AL_SAMPLE_SAVE_FUNC = FUNCTION (filename: PCHAR; spl: AL_SAMPLEptr): AL_INT; CDECL;



  FUNCTION al_load_sample (filename: AL_STRING): AL_SAMPLEptr;
  FUNCTION al_load_wav (filename: AL_STRING): AL_SAMPLEptr;
  FUNCTION al_load_voc (filename: AL_STRING): AL_SAMPLEptr;
  FUNCTION al_create_sample (bits, stereo, freq, len: AL_INT): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sample';
  FUNCTION al_save_sample (filename: AL_STRING; spl: AL_SAMPLEptr): AL_INT;
  PROCEDURE al_destroy_sample (spl: AL_SAMPLEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_sample';

  FUNCTION al_play_sample (spl: AL_SAMPLEptr; vol, pan, freq, loop: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'play_sample';
  PROCEDURE al_stop_sample (spl: AL_SAMPLEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stop_sample';

  PROCEDURE al_register_sample_file_type (ext: AL_STRING; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC);


IMPLEMENTATION

  FUNCTION load_sample (filename: PCHAR): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_sample';

  FUNCTION al_load_sample (filename: AL_STRING): AL_SAMPLEptr;
  BEGIN
    al_load_sample := load_sample (PCHAR (filename));
  END;



  FUNCTION load_wav (filename: PCHAR): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_wav';

  FUNCTION al_load_wav (filename: AL_STRING): AL_SAMPLEptr;
  BEGIN
    al_load_wav := load_wav (PCHAR (filename));
  END;



  FUNCTION load_voc (filename: PCHAR): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_voc';

  FUNCTION al_load_voc (filename: AL_STRING): AL_SAMPLEptr;
  BEGIN
    al_load_voc := load_voc (PCHAR (filename));
  END;



  FUNCTION save_sample (filename: PCHAR; spl: AL_SAMPLEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_sample';

  FUNCTION al_save_sample (filename: AL_STRING; spl: AL_SAMPLEptr): AL_INT;
  BEGIN
    al_save_sample := save_sample (PCHAR (filename), spl);
  END;



  PROCEDURE register_sample_file_type (ext: PCHAR; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'register_sample_file_type';

  PROCEDURE al_register_sample_file_type (ext: AL_STRING; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC);
  BEGIN
    register_sample_file_type (PCHAR (ext), load, save); 
  END;
  
END.
