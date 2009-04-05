UNIT almouse;
(*<Mouse routines.

   Allegro provides functions for reading the mouse state and displaying a
   mouse cursor on-screen.  You can read the absolute position of the mouse and
   the state of the mouse buttons from global variables.  Additionally, you can
   read the mouse position difference as mouse mickeys, which is the number of
   pixels the cursor moved since the last time this information was read.

   Allegro offers three ways to display the mouse cursor:
   @definitionList(
     @itemLabel(Standard Allegro cursor)@item(Allegro is responsible for drawing
       the mouse cursor from a timer.  Use @link(al_set_mouse_sprite) and
       @link(al_show_mouse) to define your own cursor and display it on the
       screen.  You need to call @link(al_scare_mouse)/@link(al_unscare_mouse)
       to hide the mouse cursor whenever you draw to the screen.)
     @itemLabel(Custom operating system cursor @(hardware cursor@))
       @item(Allegro will let the operating system draw the mouse cursor.  Use
       @code(al_set_mouse_sprite) and @code(al_show_mouse) @(or
       @link(al_show_os_cursor)@) to define your own cursor and display it on
       the screen.  Not all graphics drivers are capable of this and some may
       only be able to display cursors up to a certain size.  Allegro will fall
       back on its own cursor drawing if it cannot let the OS handle this.  On
       some platforms, the hardware cursor is incompatible with
       @link(al_get_mouse_mickeys) and it is therefor disabled by default.  In
       such cases you need to call @link(al_enable_hardware_cursor) to enable
       it explicitly.)
     @itemLabel(Default operating system cursor)@item(Allegro will not draw its
       own cursor, but use the operating system default cursor.  You can use
       the @link(al_select_mouse_cursor) function to select the cursor shape to
       display.  As with custom operating system cursors, you need to call
       @link(al_enable_hardware_cursor) before you can use this.  Or you can
       use the low level @link(al_show_os_cursor) function.)
   )
   Not all drivers will support all functionality. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$MODE FPC}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}
{$H+}



INTERFACE

USES
  albase, albitmap;



CONST
(* Indicates that the mouse cursor is the default system cursor, not Allegro's
   custom cursor. @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_NONE		= 0;
(* Selects the custom Allegro cursor, i.e. the one that you set with
   @link(al_set_mouse_sprite). @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_ALLEGRO	= 1;
(* The operating system default arrow cursor.
 @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_ARROW		= 2;
(* The operating system default `busy' cursor (hourglass).
   @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_BUSY		= 3;
(* The operating system default `question' cursor (arrow with question mark).
   @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_QUESTION	= 4;
(* The operating system default `edit' cursor (vertical bar).
   @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_EDIT		= 5;
(* @exclude *)
  AL_NUM_MOUSE_CURSORS		= 6;



VAR
(* Global variable containing the current mouse horizontal position.  Wherever
   possible these values will be updated asynchronously, but if
   @link(al_mouse_needs_poll) returns @true, you must manually call
   @link(al_poll_mouse) to update them with the current input state.

   The position is integer ranging from zero to the right side of the
   screen. *)
  al_mouse_x: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_x';
(* Global variable containing the current mouse vertical position.  Wherever
   possible these values will be updated asynchronously, but if
   @link(al_mouse_needs_poll) returns @true, you must manually call
   @link(al_poll_mouse) to update them with the current input state.

   The position is integer ranging from zero to the bottom side of the
   screen. *)
  al_mouse_y: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_y';
(* Global variable containing the current mouse position.  Wherever possible
   these values will be updated asynchronously, but if
   @link(al_mouse_needs_poll) returns @true, you must manually call
   @link(al_poll_mouse) to update them with the current input state.

   It holds the current vertical wheel position, when using an input driver
   that supports wheel mice. *)
  al_mouse_z: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_z';
(* Global variable containing the current mouse position.  Wherever possible
   these values will be updated asynchronously, but if
   @link(al_mouse_needs_poll) returns @true, you must manually call
   @link(al_poll_mouse) to update them with the current input state.

   It holds the current horizontal wheel position, when using an input driver
   that supports wheel mice. *)
  al_mouse_w: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_x';
(* Global variable containing the current mouse button state.  Wherever
   possible these values will be updated asynchronously, but if
   @link(al_mouse_needs_poll) returns @true, you must manually call
   @link(al_poll_mouse) to update them with the current input state.

   It is a bitfield indicating the state of each button:  bit 0 is the left
   button, bit 1 the right, and bit 2 the middle button.  Additional non
   standard mouse buttons might be available as higher bits in this variable.
   Usage example:
   @longcode(
IF (al_mouse_b AND 1) <> 0 THEN
   WriteLn ('Left button is pressed');
IF (al_mouse_b AND 2) = 0 THEN
   WriteLn ('Right button is not pressed');
   #)
*)
  al_mouse_b: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_b';
(* Global variable containing the current mouse position.  Wherever possible
   these values will be updated asynchronously, but if
   @link(al_mouse_needs_poll) returns @true, you must manually call
   @link(al_poll_mouse) to update them with the current input state.

   It has the current X coordinate in the upper 16 bits and the Y in the lower
   16 bits.  This may be useful in tight polling loops where a mouse interrupt
   could occur between your reading of the two separate variables, since you
   can copy this value into a local variable with a single instruction and then
   split it up at your leisure. Example:
   @longcode(#
VAR
  mpos, mx, my: LONGINT;
  ...
  mpos := al_mouse_pos;
  mx := mpos RSH 16;
  my := mpos AND $0000ffff;
  #)*)
  al_mouse_pos: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_pos';



(* Global variable containing the current mouse sprite. This is read-only, and
   only to be modified using the @link(al_set_mouse_sprite) procedure. *)
  al_mouse_sprite: AL_BITMAPptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_sprite';
(* Global variable containing the current mouse focus point. This is read-only,
   and only to be modified using the @link(al_set_mouse_sprite_focus)
   procedure. *)
  al_mouse_x_focus: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_x_focus';
(* Global variable containing the current mouse focus point. This is read-only,
   and only to be modified using the @link(al_set_mouse_sprite_focus)
   procedure. *)
  al_mouse_y_focus: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_y_focus';



(* Installs the Allegro mouse handler.  You must do this before using any other
   mouse functions.

   @returns(-1 on failure, zero if the mouse handler is already installed @(in
     which case this function does nothing@) or the number of buttons on the
     mouse if the mouse handler has successfully been installed @(ie. this is
     the first time a handler is installed or you have removed the previous
     one@).

     Note that the number of mouse buttons returned by this function is more an
     indication than a physical reality.  With most devices there is no way of
     telling how many buttons there are, and any user can override the number
     of mouse buttons returned by this function with a custom configuration
     file and the variable @code(num_buttons).  Even if this value is
     overridden by the user, the global mouse variables will still report
     whatever the hardware is sending.) *)
  FUNCTION al_install_mouse: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'install_mouse';

(* Removes the mouse handler. You don't normally need to bother calling this,
   because @link(al_exit) will do it for you. *)
  PROCEDURE al_remove_mouse; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_mouse';



(* Wherever possible, Allegro will read the mouse input asynchronously (ie.
   from inside an interrupt handler), but on some platforms that may not be
   possible, in which case you must call this routine at regular intervals to
   update the mouse state variables.  To help you test your mouse polling code
   even if you are programming on a platform that doesn't require it, after the
   first time that you call this function Allegro will switch into polling
   mode, so from that point onwards you will have to call this routine in order
   to get any mouse input at all, regardless of whether the current driver
   actually needs to be polled or not.
   @returns(zero on success, or a negative number on failure @(ie. no mouse
     driver installed@).) *)
  FUNCTION al_poll_mouse: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_mouse';

(* Returns @true if the current mouse driver is operating in polling mode. *)
  FUNCTION al_mouse_needs_poll: BOOLEAN;



(* After calling this function, Allegro will let the operating system draw the
   mouse cursor instead of doing it itself.  This is not possible with all
   graphics drivers though:  you'll need to check the
   @link(al_gfx_capabilities) flags after calling @link(al_show_mouse) to see
   if this works.  On some platforms, enabling the hardware cursor causes
   @link(al_get_mouse_mickeys) to return only a limited range of values, so you
   should not call this function if you need mouse mickeys. *)
  PROCEDURE al_enable_hardware_cursor; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'enable_hardware_cursor';

(* After calling this function, Allegro will be responsible for drawing the
   mouse cursor rather than the operating system.  On some platforms calling
   @link(al_enable_hardware_cursor) makes the return values of
   @link(al_get_mouse_mickeys) unreliable.  After calling this function,
   @code(al_get_mouse_mickeys) returns reliable results again. *)
  PROCEDURE al_disable_hardware_cursor; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'disable_hardware_cursor';

(* This function allows you to use the operating system's native mouse cursors
   rather than some custom cursor.  You will need to enable this functionality
   by calling @link(al_enable_hardware_cursor) beforehand.  If the operating
   system does not support this functionality, or if it has not been enabled,
   then Allegro will substitute its own cursor images.  You can change these
   substitute images using @link(al_set_mouse_cursor_bitmap).

   Note that the effects of this function are not apparent until
   @link(al_show_mouse) is called.

   To know whether the operating system's native cursor is being used, or if
   Allegro has made a substitution, you can check the
   @LINK(AL_GFX_SYSTEM_CURSOR) flag in @link(al_gfx_capabilities) after calling
   @link(al_show_mouse).

   The cursor argument selects the type of cursor to be displayed:
   @link(AL_MOUSE_CURSOR_NONE), @link(AL_MOUSE_CURSOR_ALLEGRO),
   @link(AL_MOUSE_CURSOR_ARROW), @link(AL_MOUSE_CURSOR_BUSY),
   @link(AL_MOUSE_CURSOR_QUESTION), @link(AL_MOUSE_CURSOR_EDIT) *)
  PROCEDURE al_select_mouse_cursor (cursor: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'select_mouse_cursor';

(* This function changes the cursor image Allegro uses if
   @link(al_select_mouse_cursor) is called but no native operating system
   cursor can be used, e.g. because you did not call
   @link(al_enable_hardware_cursor).

   The effect of this function will not be apparent until @link(al_show_mouse)
   is called.

   @param(cursor one of: @link(AL_MOUSE_CURSOR_ALLEGRO),
     @link(AL_MOUSE_CURSOR_ARROW), @link(AL_MOUSE_CURSOR_BUSY),
     @link(AL_MOUSE_CURSOR_QUESTION), @link(AL_MOUSE_CURSOR_EDIT) but not
     @link(AL_MOUSE_CURSOR_NONE).)
   @param(bmp can either point to a valid bitmap or it can be @nil.  Passing a
     bitmap makes Allegro use that image in place of its own default
     substitution @(should the operating system's native cursor be
     unavailable@).  The bitmap must remain available for the duration in which
     it could be used.  Passing @nil lets Allegro revert to its default
     substitutions.) *)
  PROCEDURE al_set_mouse_cursor_bitmap (cursor: LONGINT; bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_cursor_bitmap';



(* Tells Allegro to display a mouse pointer on the screen.  This will only work
   if the timer module has been installed.  The mouse pointer will be drawn
   onto the specified bitmap, which should normally be @link(al_screen) (see
   later for information about bitmaps).  To hide the mouse pointer, call
   @code(al_show_mouse @(@nil@)).

   @bold(Warning:) if you draw anything onto the screen while the pointer is
   visible, a mouse movement interrupt could occur in the middle of your
   drawing operation.  If this happens the mouse buffering and graphics drawing
   code will get confused and will leave @italic('mouse droppings') all over
   the screen.  To prevent this, you must make sure you turn off the mouse
   pointer whenever you draw onto the screen.  This is not needed if you are
   using a hardware cursor.

   Note: you must not be showing a mouse pointer on a bitmap at the time that
   the bitmap is destroyed with @link(al_destroy_bitmap), e.g. call
   @code(al_show_mouse @(@nil@);) before destroying the bitmap.  This does not
   apply to @code(al_screen) since you never destroy @code(al_screen) with
   @code(al_destroy_bitmap). *)
  PROCEDURE al_show_mouse (bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'show_mouse';

(* You don't like Allegro's mouse pointer?  No problem.  Use this function to
   supply an alternative of your own.  If you change the pointer and then want
   to get Allegro's lovely arrow back again, call @code(al_set_mouse_sprite
   @(@nil@)).

   As a bonus, @code(al_set_mouse_sprite @(@nil@)) uses the current palette in
   choosing colors for the arrow.  So if your arrow mouse sprite looks ugly
   after changing the palette, call @code(al_set_mouse_sprite @(@nil@)). *)
  PROCEDURE al_set_mouse_sprite (sprite: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_sprite';

(* The mouse focus is the bit of the pointer that represents the actual mouse
   position, ie. the (@link(al_mouse_x), @link(al_mouse_y)) position.  By
   default this is the top left corner of the arrow, but if you are using a
   different mouse pointer you might need to alter it. *)
  PROCEDURE al_set_mouse_sprite_focus (x, y: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_sprite_focus';



(* Moves the mouse to the specified screen position.  It is safe to call even
   when a mouse pointer is being displayed. *)
  PROCEDURE al_position_mouse (x, y: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse';

(* Sets the mouse wheel position variable to the specified value. *)
  PROCEDURE al_position_mouse_z (z: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse_z';

(* Sets the horizontal mouse wheel position variable to the specified value. *)
  PROCEDURE al_position_mouse_w (w: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse_w';

(* Measures how far the mouse has moved since the last call to this function.
   The values of @code(mickeyx) and @code(mickeyy) will become negative if the
   mouse is moved left or up, respectively.  The mouse will continue to
   generate movement mickeys even when it reaches the edge of the screen, so
   this form of input can be useful for games that require an infinite range of
   mouse movement.

   Note that the infinite movement may not work in windowed mode, since under
   some platforms the mouse would leave the window, and may not work at all if
   the hardware cursor is in use. *)
  PROCEDURE al_get_mouse_mickeys (VAR mickeyx, mickeyy: LONGINT);



(* Helper for hiding the mouse pointer prior to a drawing operation.  This will
   temporarily get rid of the pointer, but only if that is really required (ie.
   the mouse is visible, and is displayed on the physical screen rather than
   some other memory surface, and it is not a hardware or OS cursor).  The
   previous mouse state is stored for subsequent calls to
   @link(al_unscare_mouse). *)
  PROCEDURE al_scare_mouse; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scare_mouse';

(* Like @link(al_scare_mouse), but will only hide the cursor if it is inside
   the specified rectangle.  Otherwise the cursor will simply be frozen
   in place until you call @link(al_unscare_mouse), so it cannot interfere with
   your drawing. *)
  PROCEDURE al_scare_mouse_area (x, y, w, h: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scare_mouse_area';

(* Undoes the effect of a previous call to @link(al_scare_mouse) or
   @link(al_scare_mouse_area), restoring the original pointer state. *)
  PROCEDURE al_unscare_mouse; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unscare_mouse';


(* In case you do not need Allegro's mouse cursor API, which automatically
   emulates a cursor in software if no other cursor is available, you can use
   this low level function to try to display or hide the system cursor
   directly.  The cursor parameter takes the same values as
   @link(al_select_mouse_cursor).  This function is very similar to calling
   @link(al_enable_hardware_cursor), @link(al_select_mouse_cursor) and
   @link(al_show_mouse), but will not try to do anything if no system cursor
   is available.

   The most common use for this function is to just call it once at the
   beginning of the program to tell it to display the system cursor inside the
   Allegro window.  The return value can be used to see if this succeeded or
   not.  On some systems (e.g. DirectX fullscreen) this is not supported and
   the function will always fail, and in other cases only some of the cursors
   will work, or in the case of @link(AL_MOUSE_CURSOR_ALLEGRO), only certain
   bitmap sizes may be supported.

   You never should use @code(al_show_os_cursor) together with the function
   @code(al_show_mouse) and other functions affecting it
   (@code(al_select_mouse_cursor), @code(al_enable_hardware_cursor),
   @code(al_disable_hardware_cursor), @code(al_scare_mouse),
   @code(al_unscare_mouse)).  They implement the standard high level mouse API,
   and don't work together with this low level function.

   @returns(@true if a system cursor is being displayed after the function
     returns, or @false otherwise.) *)
  FUNCTION al_show_os_cursor (cursor: LONGINT): BOOLEAN;



IMPLEMENTATION

  FUNCTION mouse_needs_poll: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_mouse_needs_poll: BOOLEAN;
  BEGIN
    al_mouse_needs_poll := mouse_needs_poll <> 0;
  END;



  FUNCTION show_os_cursor (cursor: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_show_os_cursor (cursor: LONGINT): BOOLEAN;
  BEGIn
    al_show_os_cursor := show_os_cursor (cursor) = 0;
  END;



  PROCEDURE get_mouse_mickeys (mickeyx, mickeyy: PLONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_get_mouse_mickeys (VAR mickeyx, mickeyy: LONGINT);
  BEGIN
    get_mouse_mickeys (@mickeyx, @mickeyy);
  END;

END.
