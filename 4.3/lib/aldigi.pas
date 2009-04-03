UNIT aldigi;
(*< Digital sample routines. *)

{$H+}
{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}





INTERFACE

USES
  albase; { Needs some basic definitions. }



TYPE
(* Pointer to @link(AL_SAMPLE). *)
  AL_SAMPLEptr = ^AL_SAMPLE;

(* A sample structure, which holds sound data, used by the digital sample
   routines.  You can consider all of these fields as read only except
   @code(priority), @code(loop_start) and @code(loop_end), which you can
   change them for example after loading a sample from disk.

   The variables @code(loop_start) and @code(loop_end) specify the loop
   position in sample units, and are set by default to the start and end of the
   sample.

   If you are creating your own samples on the fly, you might also want to
   modify the raw data of the sample pointed by the @code(data) field.  The
   sample data are always in unsigned format.  This means that if you are
   loading a PCM encoded sound file with signed 16-bit samples, you would have
   to XOR every two bytes (i.e. every sample value) with 0x8000 to change the
   signedness. *)
  AL_SAMPLE = RECORD
    bits : LONGINT;	 {< 8 or 16  }
    stereo : LONGINT;	 {< sample type flag  }
    freq : LONGINT;	 {< sample frequency  }
  (* It is a value from 0 to 255 (by default set to 128) and controls how
     hardware voices on the sound card are allocated if you attempt to play
     more than the driver can handle.  This may be used to ensure that the less
     important sounds are cut off while the important ones are preserved. *)
    priority : LONGINT;
    len : DWORD;	 {< length (in samples)  }
    loop_start : DWORD;	 {< loop start position  }
    loop_end : DWORD;	 {< loop finish position  }
    param : DWORD;	 {< @exclude  }
    data : POINTER;	 {< sample data  }
  END;

(* Used by @link(al_register_sample_file_type). *)
  AL_SAMPLE_LOAD_FUNC = FUNCTION (filename: PCHAR): AL_SAMPLEptr; CDECL;
  AL_SAMPLE_SAVE_FUNC = FUNCTION (filename: PCHAR; spl: AL_SAMPLEptr): LONGINT; CDECL;



(* Loads a sample from a file, supporting both mono and stereo WAV and mono VOC
   files, in 8 or 16-bit formats, as well as formats handled by functions
   registered using @link(al_register_sample_file_type).

   Remember to free this sample later to avoid memory leaks.

   @returns(a pointer to the @link(AL_SAMPLE) or @nil on error.) *)
  FUNCTION al_load_sample (filename: STRING): AL_SAMPLEptr;

(* Loads a sample from a RIFF WAV file.

   Remember to free this sample later to avoid memory leaks.

   @returns(a pointer to the @link(AL_SAMPLE) or @nil on error.) *)
  FUNCTION al_load_wav (filename: STRING): AL_SAMPLEptr;

(* Loads a sample from a Creative Labs VOC file.

  Remember to free this sample later to avoid memory leaks.

   @returns(a pointer to the @link(AL_SAMPLE) or @nil on error.) *)
  FUNCTION al_load_voc (filename: STRING): AL_SAMPLEptr;

(* Constructs a new sample structure of the specified type.

   Remember to free this sample later to avoid memory leaks. 
   @param(bits can be 8 or 16.)
   @param(stereo can be zero for mono samples and non-zero for stereo samples)
   @param(freq is the frequency in hertz)
   @param(len is the number of samples you want to allocate for the full sound
     buffer.)
   @returns(a pointer to the created sample, or @nil if the sample could not
     be created.) *)
  FUNCTION al_create_sample (bits, stereo, freq, len: LONGINT): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sample';

(* Writes a sample into a file.  The output format is determined from the
   @code(filename) extension.  At present Allegro does not natively support
   the writing of any sample formats, so you must register a custom saver
   routine with @link(al_register_sample_file_type).

   @returns(@true on success, @false otherwise.) *)
  FUNCTION al_save_sample (filename: STRING; spl: AL_SAMPLEptr): BOOLEAN;

(* Destroys a sample structure when you are done with it.  It is safe to call
   this even when the sample might be playing, because it checks and will kill
   it off if it is active.  Use this to avoid memory leaks in your program. *)
  PROCEDURE al_destroy_sample (spl: AL_SAMPLEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_sample';



(* Triggers a sample at the specified volume, pan position, and frequency.  The
   parameters @code(vol) and @code(pan) range from 0 (min/left) to 255
   (max/right).  Frequency is relative rather than absolute:  1000 represents
   the frequency that the sample was recorded at, 2000 is twice this, etc.  If
   @code(loop) is not zero, the sample will repeat until you call
   @link(al_stop_sample), and can be manipulated while it is playing by calling
   @link(al_adjust_sample).

   @returns(the voice number that was allocated for the sample or negative if
     no voices were available.) *)
  FUNCTION al_play_sample (spl: AL_SAMPLEptr; vol, pan, freq, loop: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'play_sample';

(* Alters the parameters of a sample while it is playing (useful for
   manipulating looped sounds).  You can alter the volume, pan, and frequency,
   and can also clear the loop flag, which will stop the sample when it next
   reaches the end of its loop.  The values of the parameters are just like
   those of @link(al_play_sample).  If there are several copies of the same
   sample playing, this will adjust the first one it comes across.  If the
   sample is not playing it has no effect. *)
  PROCEDURE al_adjust_sample (spl: AL_SAMPLEptr; vol, pan, freq, loop: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'adjust_sample';

(* Stop a sample from playing, which is required if you have set a sample going
   in looped mode.  If there are several copies of the sample playing, it will
   stop them all. You must still destroy the sample using
   @link(al_destroy_sample). *)
  PROCEDURE al_stop_sample (spl: AL_SAMPLEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stop_sample';



(* Informs the @link(al_load_sample) and the @link(al_save_sample) functions
   of a new sample file type, providing routines to read and write samples in
   this format (either function may be @nil).  Example:
   @longcode(#
   FUNCTION LoadMP3 (filename: PCHAR): AL_SAMPLEptr; CDECL;
   BEGIN
     ...
   END;

   ...

   al_register_sample_file_type ('mp3', LoadMPT, NIL);
   #) *)
  PROCEDURE al_register_sample_file_type (ext: STRING; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC);



IMPLEMENTATION

  FUNCTION load_sample (filename: PCHAR): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_sample';

  FUNCTION al_load_sample (filename: STRING): AL_SAMPLEptr;
  BEGIN
    al_load_sample := load_sample (PCHAR (filename));
  END;



  FUNCTION load_wav (filename: PCHAR): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_wav';

  FUNCTION al_load_wav (filename: STRING): AL_SAMPLEptr;
  BEGIN
    al_load_wav := load_wav (PCHAR (filename));
  END;



  FUNCTION load_voc (filename: PCHAR): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_voc';

  FUNCTION al_load_voc (filename: STRING): AL_SAMPLEptr;
  BEGIN
    al_load_voc := load_voc (PCHAR (filename));
  END;



  FUNCTION save_sample (filename: PCHAR; spl: AL_SAMPLEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_sample';

  FUNCTION al_save_sample (filename: STRING; spl: AL_SAMPLEptr): BOOLEAN;
  BEGIN
    al_save_sample := (save_sample (PCHAR (filename), spl) = 0);
  END;



  PROCEDURE register_sample_file_type (ext: PCHAR; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'register_sample_file_type';

  PROCEDURE al_register_sample_file_type (ext: STRING; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC);
  BEGIN
    register_sample_file_type (PCHAR (ext), load, save);
  END;

END.
