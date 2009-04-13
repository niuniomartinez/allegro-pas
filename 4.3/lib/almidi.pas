UNIT almidi;
(*<MIDI music routines.

  Allegro allows you to play MIDI files.  MIDI files basically contain notes
  and the type of instrument that is meant to play them, so they are usually
  very small in size.  However, it's up to the sound card of the end user to
  play the notes, and sound cards have been historically known to have poor
  MIDI performance (at least those oriented to the consumer market).  Few
  consumer cards feature decent MIDI playback.  Still, as a game creator you
  can never be sure if the music of your game will be played as you meant it,
  because it totally depends on the hardware of the user.

  For this reason Allegro also provides a @link(AL_MIDI_DIGMID) driver.  This
  is a software implementation of the so called Wavetable synthesis.  Sound
  cards featuring this store digital samples of real instruments at different
  pitches, interpolating those that are not recorded, thus achieving a high
  sound quality.  Implementing this in software makes you sure that the quality
  you hear on your computer is that which will be heard by end users using the
  same driver.

  The disadvantage of the @code(AL_MIDI_DIGMID) driver is that it uses more CPU
  than simple MIDI playback, and it steals some hardware voices from the sound
  card, which might be more critical for the end user experience than the
  background music . At the Allegro homepage (http://alleg.sourceforge.net/)
  you can find more information about @code(AL_MIDI_DIGMID) and where to
  download digital samples for your MIDI files.  *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$MODE FPC}
 {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

USES
  albase;



CONST
(* Max number of MIDI voices. *)
  AL_MIDI_VOICES = 64;
(* Max number of MIDI tracks. *)
  AL_MIDI_TRACKS = 32;



TYPE
(* Pointer to @link(AL_MIDI). *)
  AL_MIDIptr = ^AL_MIDI;
(* A structure holding MIDI data.
   @seealso(al_load_midi) @seealso(al_play_midi) @seealso(al_destroy_midi) *)
  AL_MIDI = RECORD
    divisions : LONGINT;		{< number of ticks per quarter note  }
    track : ARRAY[0..(AL_MIDI_TRACKS)-1] OF RECORD
      data : PBYTE;	{< MIDI message stream  }
      len : LONGINT;	{< length of the track data  }
    END;
  END;



VAR
(* Stores the current position (beat number) in the MIDI file, or contains a
   negative number if no music is currently playing.  Useful for synchronising
   animations with the music, and for checking whether a MIDI file has finished
   playing. *)
  al_midi_pos: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_pos';
(* Contains the position in seconds in the currently playing midi.  This is
   useful if you want to display the current song position in seconds, not as
   beat number. *)
  al_midi_time: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_time';
(* The loop start and end points, set by the @link(al_play_looped_midi)
   function.  These may safely be altered while the music is playing, but you
   should be sure they are always set to sensible values (start < end).  If you
   are changing them both at the same time, make sure to alter them in the
   right order in case a MIDI interrupt happens to occur in between your two
   writes!  Setting these values to -1 represents the start and end of the file
   respectively. *)
  al_midi_loop_start: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_loop_start';
  al_midi_loop_end  : LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_loop_end';



(* Loads a MIDI file (handles both format 0 and format 1).
   @returns(a pointer to a @link(AL_MIDI) structure, or @nil on error.
     Remember to free this MIDI file later to avoid memory leaks.) *)
  FUNCTION al_load_midi (filename: STRING): AL_MIDIptr;

(* Destroys a @link(AL_MIDI) structure when you are done with it.  It is safe
   to call this even when the MIDI file might be playing, because it checks and
   will kill it off if it is active.  Use this to avoid memory leaks in your
   program. *)
  PROCEDURE al_destroy_midi (midi: AL_MIDIptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_midi';

(* Starts playing the specified @link(AL_MIDI) file, first stopping whatever
   music was previously playing.  If the @code(loop) flag is set to @true,
   the data will be repeated until replaced with something else, otherwise it
   will stop at the end of the file.  Passing a @nil pointer will stop whatever
   music is currently playing.

   @returns(@false if an error occurs @(this may happen if a patch-caching
     wavetable driver is unable to load the required samples, or at least it
     might in the future when somebody writes some patch-caching wavetable
     drivers :-@)) *)
  FUNCTION al_play_midi (midi: AL_MIDIptr; loop: BOOLEAN): BOOLEAN;

(* Starts playing a MIDI file with a user-defined loop position.  When the
   player reaches the @code(loop_end) position or the end of the file
   (@code(loop_end) may be -1 to only loop at EOF), it will wind back to the
   @code(loop_start) point.  Both positions are specified in the same beat
   number format as the @link(al_midi_pos) variable.

   @returns(@false if an error occurs, @true otherwise.) *)
  FUNCTION al_play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: LONGINT): BOOLEAN;

(* Stops whatever music is currently playing. This is the same thing as calling
   @code(al_play_midi @(@nil, @false@)).
   @seealso(al_play_midi)*)
  PROCEDURE al_stop_midi; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stop_midi';

(* Pauses the MIDI player. @seealso(al_midi_resume) @seealso(al_play_midi) *)
  PROCEDURE al_midi_pause; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_pause';

(* Resumes playback of a paused MIDI file. @seealso(al_midi_pause) *)
  PROCEDURE al_midi_resume; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_resume';

(* Seeks to the given @link(al_midi_pos) in the current MIDI file.  If the
   target is earlier in the file than the current @code(al_midi_pos) it seeks
   from the beginning; otherwise it seeks from the current position.

   @returns(zero if it could successfully seek to the requested position.
     Otherwise, a return value of 1 means it stopped playing, and
     @code(al_midi_pos) is set to the negative length of the MIDI file @(so you
     can use this function to determine the length of a MIDI file@).  A return
   value of 2 means the MIDI file looped back to the start.) *)
  FUNCTION al_midi_seek(target: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_seek';

(* Streams a block of MIDI commands into the player in real-time, allowing you
   to trigger notes, jingles, etc, over the top of whatever MIDI file is
   currently playing. *)
  PROCEDURE al_midi_out (data: POINTER; length: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_out';

(* Forces the MIDI driver to load the entire set of patches ready for use.  You
   will not normally need to call this, because Allegro automatically loads
   whatever data is required for the current MIDI file, but you must call it
   before sending any program change messages via the @link(al_midi_out)
   command.

   @returns(@false if an error occurred.) *)
  FUNCTION al_load_midi_patches: BOOLEAN;



IMPLEMENTATION

  FUNCTION load_midi (filename: PCHAR): AL_MIDIptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_midi';

  FUNCTION al_load_midi (filename: STRING): AL_MIDIptr;
  BEGIN
    al_load_midi := load_midi (PCHAR (filename));
  END;



  FUNCTION play_midi (midi: AL_MIDIptr; loop: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_play_midi (midi: AL_MIDIptr; loop: BOOLEAN): BOOLEAN;
  BEGIN
    IF loop THEN
      al_play_midi := play_midi (midi, -1) = 0
    ELSE
      al_play_midi := play_midi (midi, 0) = 0;
  END;



  FUNCTION play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: LONGINT): BOOLEAN;
  BEGIN
    al_play_looped_midi := play_looped_midi (midi, loop_start, loop_end) = 0
  END;



  FUNCTION load_midi_patches: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_midi_patches: BOOLEAN;
  BEGIN
    al_load_midi_patches := load_midi_patches = 0;
  END;

END.
