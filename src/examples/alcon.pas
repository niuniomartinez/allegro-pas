unit alcon;
(* Defines a console-like output system to be used by some examples.  It aims to
 * be as simple as possible.
 *
 * The console has size of 80x60 characters.  It uses current display as output.
 * Must call DrawConsole in each frame to make it visible.
 *)
(*
  Copyright (c) 2024 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

interface

(* Initialize the console.

   It uses the current active display.
 *)
  function Initialize: Boolean;
(* Clear console. *)
  procedure ClrScr;
(* Change text cursor in console. *)
  procedure GotoXY (const aX, aY: Integer);
(* Write text in fake console. *)
  procedure PrintLn (const aText: String); overload;
  procedure PrintLn (const aFmt: String; aParms: array of const); overload;
(* Show console output on current display. *)
  procedure DrawConsole;
(* Release resources used by console. *)
  procedure Finalize;

implementation

  uses
    Common,
    allegro5, al5font, al5strings;

  const
    ConsoleWidth = 80;
    ConsoleHeight = 60;
    BmpWidth = ConsoleWidth * 8;
    BmpHeight = ConsoleHeight * 8;
  var
  (* Console information. *)
    Display: ALLEGRO_DISPLAYptr;
    DisplayWidth, DisplayHeight: Integer;
    TextFont: ALLEGRO_FONTptr;
    Bitmap, ScrollBitmap: ALLEGRO_BITMAPptr;
    bgColor, fgColor: ALLEGRO_COLOR;
    cX, cY: Integer;
  (* Additional information. *)
    OldBitmap: ALLEGRO_BITMAPptr;
    Initialized: Boolean = False;

(* Save state. *)
  procedure SaveCurrentState;
  begin
    OldBitmap := al_get_target_bitmap
  end;



(* Restore state. *)
  procedure RestoreState;
  begin
    al_set_target_bitmap (OldBitmap)
  end;



  function Initialize: Boolean;
  begin
  { Install base stuff. }
    al_install_keyboard;
    if not al_is_font_addon_initialized then
      if not al_init_font_addon then
        Exit (False);
  { Get current display. }
    Display := al_get_current_display;
    if not Assigned (Display) then Exit (False); { Fatal Error. Halt? Except? }
    DisplayWidth := al_get_display_width (Display);
    DisplayHeight := al_get_display_height (Display);
  { Console output bitmaps. }
    al_set_new_bitmap_flags (ALLEGRO_MAG_LINEAR); { Better looking scaling. }
    Bitmap := al_create_bitmap (BmpWidth, BmpHeight);
    al_set_new_bitmap_flags (0); { No scaled, so no filter. }
    ScrollBitmap := al_create_bitmap (BmpWidth, BmpHeight - 8);
    if not (Assigned (Bitmap) and Assigned (ScrollBitmap)) then
    begin
      ErrorMessage ('Can''t create console simulation.');
      Exit (False)
    end;
  { Font and colors. }
    TextFont := al_create_builtin_font;
    bgColor := al_map_rgb ($FF, $FF, $CC);
    fgColor := al_map_rgb (  0,   0,   0);;
  { Last initialization. }
    ClrScr;
    Initialized := True;
    Result := True
  end;



  procedure ClrScr;
  begin
    SaveCurrentState;
      al_set_target_bitmap (Bitmap);
      al_clear_to_color (bgColor);
    RestoreState;
    GotoXY (1, 1)
  end;



  procedure GotoXY (const aX, aY: Integer);
  begin
    cX := Clamp (1, aX, ConsoleWidth);
    cY := Clamp (1, aY, ConsoleHeight)
  end;



  procedure PrintLn (const aText: String);

    procedure DoScroll;
    begin
      al_set_target_bitmap (ScrollBitmap);
      al_draw_bitmap_region (
        Bitmap, 0, 8, BmpWidth, BmpHeight,
        0, 0, 0
      );
      al_set_target_bitmap (Bitmap);
      al_clear_to_color (bgColor);
      al_draw_bitmap (ScrollBitmap, 0, 0, 0);

      Dec (cY)
    end;

  begin
    SaveCurrentState;
  { Do scroll only if it's at the end of the console. }
    if cY > ConsoleHeight then DoScroll;
  { Draw text line. }
    al_set_target_bitmap (Bitmap);
    al_draw_text (
      TextFont, fgColor,
      cX * 8 - 8, cY * 8 - 8, 0,
      al_string_to_str (aText)
    );
  { Update cursor coordinates. }
    cX := 1; Inc (cY);

    RestoreState
  end;



  procedure PrintLn (const aFmt: String; aParms: array of const);
  begin
    PrintLn (al_str_to_string (al_str_format (aFmt, aParms)))
  end;



  procedure DrawConsole;
  var
    lOldBitmap: ALLEGRO_BITMAPptr;
  begin
    lOldBitmap := al_get_target_bitmap;
    al_set_target_backbuffer (Display);
    al_draw_scaled_bitmap (
      Bitmap,
      0, 0, BmpWidth, BmpHeight,
      0, 0, DisplayWidth, DisplayHeight,
      0
    );
    al_set_target_bitmap (lOldBitmap)
  end;



  procedure Finalize;
  begin
    if Initialized then
    begin
      if Assigned (TextFont) then
        al_destroy_font (TextFont);
      if Assigned (ScrollBitmap) then
        al_destroy_bitmap (ScrollBitmap);
      if Assigned (Bitmap) then
        al_destroy_bitmap (Bitmap);
      Initialized := False
    end
  end;

initialization
  ;
finalization
  Finalize
end.
