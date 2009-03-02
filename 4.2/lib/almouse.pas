UNIT almouse;
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
 *	Mouse routines.
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
  albase, albitmap;



CONST
(* Mouse cursors *)
  AL_MOUSE_CURSOR_NONE		= 0;
  AL_MOUSE_CURSOR_ALLEGRO	= 1;
  AL_MOUSE_CURSOR_ARROW		= 2;
  AL_MOUSE_CURSOR_BUSY		= 3;
  AL_MOUSE_CURSOR_QUESTION	= 4;
  AL_MOUSE_CURSOR_EDIT		= 5;
  AL_NUM_MOUSE_CURSORS		= 6;



VAR
  al_mouse_x, al_mouse_y, al_mouse_z, al_mouse_b, al_mouse_pos: AL_INTptr;
  al_mouse_sprite: AL_BITMAPptr;
  al_mouse_x_focus, al_mouse_y_focus: AL_INTptr;



  FUNCTION al_install_mouse: AL_INT;
  PROCEDURE al_remove_mouse; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_mouse';

  FUNCTION al_poll_mouse: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_mouse';
  FUNCTION al_mouse_needs_poll: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_needs_poll';

  PROCEDURE al_show_mouse (bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'show_mouse';
  PROCEDURE al_set_mouse_sprite (sprite: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_sprite';
  PROCEDURE al_set_mouse_sprite_focus (x, y: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_sprite_focus';

  PROCEDURE al_position_mouse (x, y: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse';
  PROCEDURE al_position_mouse_z (z: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse_z';

  PROCEDURE al_get_mouse_mickeys (mickeyx, mickeyy: AL_INTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mouse_mickeys';



IMPLEMENTATION

(* Delphi can't access to the public variables from Allegro, so we need some
 * magic to access them. *)
  FUNCTION install_mouse: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION _get_mouse_x_: AL_INTptr; CDECL
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_mouse_y_: AL_INTptr; CDECL
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_mouse_z_: AL_INTptr; CDECL
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_mouse_b_: AL_INTptr; CDECL
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_mouse_pos_: AL_INTptr; CDECL
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_mouse_sprite_: AL_BITMAPptr; CDECL
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_mouse_x_focus_: AL_INTptr; CDECL
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_mouse_y_focus_: AL_INTptr; CDECL
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;

FUNCTION al_install_mouse: AL_INT;
VAR
  R: AL_INT;
BEGIN
  R := install_mouse;
  IF R > 0 THEN
  BEGIN
    al_mouse_x      := _get_mouse_x_;
    al_mouse_y      := _get_mouse_y_;
    al_mouse_z      := _get_mouse_z_;
    al_mouse_b      := _get_mouse_b_;
    al_mouse_pos    := _get_mouse_pos_;
    al_mouse_sprite := _get_mouse_sprite_;
    al_mouse_x_focus := _get_mouse_x_focus_;
    al_mouse_y_focus := _get_mouse_y_focus_;
  END;
  al_install_mouse := R;
END;



END.

