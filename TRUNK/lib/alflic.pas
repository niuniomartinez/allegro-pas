UNIT alflic;
(*<FLIC movie routines.

  There are two high level functions for playing FLI/FLC animations:
  @link(al_play_fli), which reads the data directly from disk, and
  @link(al_play_memory_fli), which uses data that has already been loaded into
  RAM.  Apart from the different sources of the data, these two functions
  behave identically.  They draw the animation onto the specified bitmap, which
  should normally be the screen.  Frames will be aligned with the top left
  corner of the bitmap:  if you want to position them somewhere else you will
  need to create a sub-bitmap for the FLI player to draw onto.

  If the callback function is not @nil it will be called once for each frame,
  allowing you to perform background tasks of your own.  This callback should
  normally return zero:  if it returns non-zero the player will terminate (this
  is the only way to stop an animation that is playing in looped mode).

  The FLI player returns @code(AL_FLI_OK) if it reached the end of the file,
  @code(AL_FLI_ERROR) if something went wrong, and the value returned by the
  callback function if that was what stopped it.  If you need to distinguish
  between different return values, your callback should return positive
  integers, since @code(AL_FLI_OK) is zero and @code(AL_FLI_ERROR) is negative.

  Note that the FLI player will only work when the timer module is installed,
  and that it will alter the palette according to whatever palette data is
  present in the animation file.

  Occasionally you may need more detailed control over how an FLI is played,
  for example if you want to superimpose a text scroller on top of the
  animation, or to play it back at a different speed.  You could do both of
  these with the lower level functions described below. *)

{$INCLUDE allegro.cfg }

INTERFACE

USES
  albase, allegro; { Needs some basic definitions. }



CONST
(* @exclude Values returnded by FLIC player functions. *)
  AL_FLI_OK	  =  0;
  AL_FLI_EOF	  = -1;
  AL_FLI_ERROR	  = -2;
  AL_FLI_NOT_OPEN = -3;



(* Plays an Autodesk Animator FLI or FLC animation file on the specified
   @link(AL_BITMAP), reading the data from disk as it is required.  If
   @code(loop) is @true, the player will cycle when it reaches the end of the
   file, otherwise it will play through the animation once and then return.
   Read the introduction for @link(alflic) unit for a description of the
   @code(callback) parameter.
   @returns(@code(AL_FLI_OK) if it reached the end of the file,
    @code(AL_FLI_ERROR) if something went wrong, or the value returned by the
    @code(callback) function if that was what stopped it.)
   @seealso(al_play_memory_fli) @link(al_install_timer) @link(al_fli_frame) *)
  FUNCTION al_play_fli (CONST filename: STRING; bmp: AL_BITMAPptr; loop: BOOLEAN; callback: AL_SIMPLE_FUNC): AL_INT;
    INLINE;

(* Plays an Autodesk Animator FLI or FLC animation on the specified
   @link(AL_BITMAP), reading the data from a copy of the file which is held in
   memory.  You can obtain the @code(fli_data) pointer by allocating a block
   of memory and reading an FLI file into it, or by importing an FLI into a
   grabber datafile.  If @code(loop) is @true, the player will cycle when it
   reaches the end of the file, otherwise it will play through the animation
   once and then return.  Read the introduction for @link(alflic) unit for a
   description of the @code(callback) parameter.

   Playing animations from memory is obviously faster than cuing them directly
   from disk, and is particularly useful with short, looped FLI's.  Animations
   can easily get very large, though, so in most cases you will probably be
   better just using @link(al_play_fli).  You can think of this function as a
   wrapper on top of @link(al_open_memory_fli), @link(al_next_fli_frame) and
   @link(al_close_fli).
   @returns(@code(AL_FLI_OK) if it reached the end of the file,
    @code(AL_FLI_ERROR) if something went wrong, or the value returned by the
    @code(callback) function if that was what stopped it.)
   @seealso(al_install_timer) @seealso(al_fli_frame) *)
  FUNCTION al_play_memory_fli (fli_data: POINTER; bmp: AL_BITMAPptr; loop: BOOLEAN; callback: AL_SIMPLE_FUNC): LONGINT;
    INLINE;



(* Open FLI files ready for playing, reading the data from disk.  Information
   about the current FLI is held in the global variables @link(al_fli_bitmap)
   and @link(al_fli_palette), which you can use if this function succeeds.
   However, you can only have one animation open at a time.
   @returns(@code(AL_FLI_OK) on success, @code(AL_FLI_ERROR) if something went
     wrong, like trying to open another FLI file without closing the previous
     one.)
   @seealso(al_close_fli) @seealso(al_next_fli_frame) @link(al_open_memory_fli)
  *)
  FUNCTION al_open_fli (CONST filename: STRING): AL_INT;
    INLINE;

(* Open FLI files ready for playing, reading the data from memory.  Information
   about the current FLI is held in the global variables @link(al_fli_bitmap)
   and @link(al_fli_palette), which you can use if this function succeeds.
   However, you can only have one animation open at a time.
   @returns(@code(AL_FLI_OK) on success, @code(AL_FLI_ERROR) if something
     went wrong, like trying to open another FLI file without closing the
     previous one.)
   @seealso(al_close_fli) @seealso(al_next_fli_frame) @link(al_open_fli) *)
  FUNCTION al_open_memory_fli (fli_data: AL_VOIDptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'open_memory_fli';

(* Closes an FLI file when you have finished reading from it.  Remember to do
   this to avoid having memory leaks in your program. @seealso(al_open_fli) *)
  PROCEDURE al_close_fli;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'close_fli';

(* Reads the next frame of the current animation file.  If @code(loop) is
   @true, the player will cycle when it reaches the end of the file, otherwise
   it will return @code(AL_FLI_EOF).  The frame is read into the global
   variables @link(al_fli_bitmap) and @link(al_fli_palette).
   @returns(@code(AL_FLI_OK) on success, @code(AL_FLI_ERROR) or
    @code(AL_FLI_NOT_OPEN) on error, and @code(AL_FLI_EOF) on reaching the end
    of the file.)
   @seealso(al_open_fli) @seealso(al_fli_timer) @seealso(al_fli_frame) *)
  FUNCTION al_next_fli_frame (loop: BOOLEAN): AL_INT;
    INLINE;

(* Once you have done whatever you are going to do with the
   @link(al_fli_bitmap) and @link(al_fli_palette), call this function to reset
   the @code (al_fli_bmp_dirty_* )and @code(al_fli_pal_dirty_* )variables.
   @seealso(al_fli_bmp_dirty_from) @seealso(al_fli_pal_dirty_from) *)
  PROCEDURE al_reset_fli_variables;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'reset_fli_variables';



  (* Advanced FLIC player *)
  VAR
  (* Contains the current frame of the FLI/FLC animation.  If there is no open
     animation, its value will be @nil. @seealso(al_next_fli_frame)
     @seealso(al_fli_bmp_dirty_from) @seealso(al_fli_pal_dirty_from) *)
    al_fli_bitmap: AL_BITMAPptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fli_bitmap';
  (* Contains the current FLI palette. @seealso(al_next_fli_frame)
     @seealso(al_fli_pal_dirty_from) @seealso(al_fli_bitmap) *)
    al_fli_palette: AL_PALETTE;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fli_palette';

  (* These variables are set by @code(al_next_fli_frame) to indicate which part
     of the @link(al_fli_bitmap) has changed since the last call to
     @link(al_reset_fli_variables).  If @link(fli_bmp_dirty_from) is greater than
     @link(al_fli_bmp_dirty_to), the bitmap has not changed, otherwise lines
     @code(al_fli_bmp_dirty_from) to @code(al_fli_bmp_dirty_to) (inclusive)
     have altered.  You can use these when copying the @code(al_fli_bitmap) onto
     the screen, to avoid moving data unnecessarily. *)
    al_fli_bmp_dirty_from: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fli_bmp_dirty_from';
  (* @seealso(al_fli_bmp_dirty_from) *)
    al_fli_bmp_dirty_to: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fli_bmp_dirty_to';
  (* These variables are set by @link(al_next_fli_frame) to indicate which part
     of the @link(al_fli_palette) has changed since the last call to
     @link(al_reset_fli_variables).  If @link(al_fli_pal_dirty_from) is greater
     than @link(al_fli_pal_dirty_to), the palette has not changed, otherwise
     colors @code(al_fli_pal_dirty_from) to @code(al_fli_pal_dirty_to)
     (inclusive) have altered.  You can use these when updating the hardware
     palette, to avoid unnecessary calls to @link(al_set_palette). *)
    al_fli_pal_dirty_from: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fli_pal_dirty_from';
  (* @seealso(al_fli_pal_dirty_from) *)
    al_fli_pal_dirty_to: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fli_pal_dirty_to';

  (* Global variable containing the current frame number in the FLI file.  This
     is useful for synchronising other events with the animation, for instance
     you could check it in a @link(al_play_fli) callback function and use it to
     trigger a sample at a particular point.
     @seealso(al_play_memory_fli) @seealso(al_next_fli_frame) *)
    al_fli_frame: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fli_frame';

  (* Global variable for timing FLI playback.  When you open an FLI file, a timer
     interrupt is installed which increments this variable every time a new frame
     should be displayed.  Calling @link(al_next_fli_frame) decrements it, so
     you can test it and know that it is time to display a new frame if it is
     greater than zero. @seealso(al_install_timer) *)
    al_fli_timer: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fli_timer';

IMPLEMENTATION

  FUNCTION play_fli (CONST filename: AL_STRptr; bmp: AL_BITMAPptr; loop: AL_INT; callback: AL_SIMPLE_FUNC): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_play_fli (CONST filename: STRING; bmp: AL_BITMAPptr; loop: BOOLEAN; callback: AL_SIMPLE_FUNC): AL_INT;
  VAR
    DoLoop: AL_INT;
  BEGIN
    IF loop THEN DoLoop := -1 ELSE DoLoop := 0;
    al_play_fli := play_fli (AL_STRptr (filename), bmp, DoLoop, callback);
  END;



  FUNCTION play_memory_fli (fli_data: POINTER; bmp: AL_BITMAPptr; loop: LONGINT; callback: AL_SIMPLE_FUNC): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_play_memory_fli (fli_data: POINTER; bmp: AL_BITMAPptr; loop: BOOLEAN; callback: AL_SIMPLE_FUNC): LONGINT;
  VAR
    DoLoop: LONGINT;
  BEGIN
    IF loop THEN DoLoop := -1 ELSE DoLoop := 0;
    al_play_memory_fli := play_memory_fli (fli_data, bmp, DoLoop, callback);
  END;




  FUNCTION open_fli (CONST filename: AL_STRptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_open_fli (CONST filename: STRING): AL_INT;
  BEGIN
    al_open_fli := open_fli (AL_STRptr (filename));
  END;



  FUNCTION next_fli_frame (loop: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_next_fli_frame (loop: BOOLEAN): AL_INT;
  VAR
    DoLoop: AL_INT;
  BEGIN
    IF loop THEN DoLoop := -1 ELSE DoLoop := 0;
    al_next_fli_frame := next_fli_frame (DoLoop);
  END;

END.
