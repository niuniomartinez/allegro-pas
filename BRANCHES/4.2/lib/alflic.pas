UNIT alflic;
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
 *	FLIC routines.
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
  albase, albitmap, alpalete; { Needs some basic definitions. }



CONST
(* FLIC player return values *)
  AL_FLI_OK	  =  0;
  AL_FLI_EOF	  = -1;
  AL_FLI_ERROR	  = -2;
  AL_FLI_NOT_OPEN = -3;



(* FLIC player *)
  FUNCTION al_play_fli (filename: AL_STRING; bmp: AL_BITMAPptr; loop: AL_INT; callback: AL_SIMPLE_FUNC): AL_INT;
  FUNCTION al_play_memory_fli (fli_data: AL_PTR; bmp: AL_BITMAPptr; loop: AL_INT; callback: AL_SIMPLE_FUNC): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'play_memory_fli';



(* Advanced FLIC player *)
VAR
  al_fli_bitmap: AL_BITMAPptr;
  al_fli_palette: AL_PALETTEptr;
  al_fli_bmp_dirty_from, al_fli_bmp_dirty_to: AL_INTptr;
  al_fli_pal_dirty_from, al_fli_pal_dirty_to: AL_INTptr;
  al_fli_timer, al_fli_frame: AL_INTptr;

  FUNCTION al_open_fli (filename: AL_STRING): AL_INT;
  FUNCTION al_open_memory_fli (fli_data: AL_PTR): AL_INT;
  PROCEDURE al_close_fli; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'close_fli';
  FUNCTION al_next_fli_frame (loop: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'next_fli_frame';
  PROCEDURE al_reset_fli_variables; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'reset_fli_variables';



IMPLEMENTATION

  FUNCTION play_fli (filename: PCHAR; bmp: AL_BITMAPptr; loop: AL_INT; callback: AL_SIMPLE_FUNC): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'play_fli';

  FUNCTION al_play_fli (filename: AL_STRING; bmp: AL_BITMAPptr; loop: AL_INT; callback: AL_SIMPLE_FUNC): AL_INT;
  BEGIN
    al_play_fli := play_fli (PCHAR (filename), bmp, loop, callback);
  END;



(* Delphi can't access to the public variables from Allegro, so we need some
 * magic to access them. *)
  FUNCTION open_fli (filename: PCHAR): AL_INT; CDECL;  
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION open_memory_fli (filename: PCHAR): AL_INT; CDECL;  
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION _get_fli_bitmap_: AL_BITMAPptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_fli_palette_: AL_PALETTEptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_fli_bmp_dirty_from_: AL_INTptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_fli_bmp_dirty_to_: AL_INTptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_fli_pal_dirty_from_: AL_INTptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_fli_pal_dirty_to_: AL_INTptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_fli_timer_: AL_INTptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_fli_frame_: AL_INTptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;



FUNCTION al_open_fli (filename: AL_STRING): AL_INT;
BEGIN
  al_open_fli := open_fli (PCHAR (filename));
  al_fli_bitmap := _get_fli_bitmap_;
  al_fli_palette := _get_fli_palette_;
  al_fli_bmp_dirty_from := _get_fli_bmp_dirty_from_;
  al_fli_bmp_dirty_to   := _get_fli_bmp_dirty_to_;
  al_fli_pal_dirty_from := _get_fli_pal_dirty_from_;
  al_fli_pal_dirty_to   := _get_fli_pal_dirty_to_;
  al_fli_timer		:= _get_fli_timer_;
  al_fli_frame		:= _get_fli_frame_;
END;



FUNCTION al_open_memory_fli (fli_data: AL_PTR): AL_INT;
BEGIN
  al_open_memory_fli := open_memory_fli (fli_data);
  al_fli_bitmap := _get_fli_bitmap_;
  al_fli_palette := _get_fli_palette_;
  al_fli_bmp_dirty_from := _get_fli_bmp_dirty_from_;
  al_fli_bmp_dirty_to   := _get_fli_bmp_dirty_to_;
  al_fli_pal_dirty_from := _get_fli_pal_dirty_from_;
  al_fli_pal_dirty_to   := _get_fli_pal_dirty_to_;
  al_fli_timer		:= _get_fli_timer_;
END;



END.

