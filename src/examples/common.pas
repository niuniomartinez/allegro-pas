unit Common;
(* Defines common stuff used by the examples.

   Only the more advanced examples use this unit.
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

(* Helper that waits until user press key 'C'. *)
  procedure WaitCPress;
(* Print an error message in current display and wait for a key press. *)
  procedure ErrorMessage (const aText: String);

(* Returns aMid if aLo <= aMid <= aHi.
   Else returns aLo if aMid < aLo or aHi if aMid > aHi.
 *)
  function Clamp (aLo, aMid, aHi: Integer): Integer; overload;
  function Clamp (aLo, aMid, aHi: Real): Real; overload;
{$IFDEF DCC}
(* Delphi doesn't have DoDirSeparators function. *)
  procedure DoDirSeparators (var aPath: String);
{$ENDIF}

implementation

  uses
{$IFDEF DCC}
    SysUtils,
{$ENDIF}
    allegro5, al5font, al5primitives, al5strings;

  procedure WaitCPress;
  var
    lKeyboardState: ALLEGRO_KEYBOARD_STATE;
  begin
    repeat
      al_rest (0.1);
      al_get_keyboard_state (lKeyboardState)
    until al_key_down (lKeyboardState, ALLEGRO_KEY_C);
  end;



  procedure ErrorMessage (const aText: String);
  const
    TextFontSize = 8;
    Margin = TextFontSize;
  var
    lTextFont: ALLEGRO_FONTptr;
    lColor: ALLEGRO_COLOR;
  begin
  { Be sure base Allegro is installed. }
    al_install_keyboard;
    if not al_is_font_addon_initialized then
      if not al_init_font_addon then
        Exit;
    if not al_is_primitives_addon_initialized then
      if not al_init_primitives_addon then
        Exit;
  { Prepare to draw. }
    al_set_target_backbuffer (al_get_current_display);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA);
    lTextFont := al_create_builtin_font;
    lColor := al_map_rgb (255, 255, 255);
  { Draw message. }
    al_draw_filled_rectangle (
      0, 0,
      al_get_display_width (al_get_current_display), TextFontSize * 4,
      al_map_rgb (255, 51, 51)
    );
    al_draw_rectangle (
      1.5, 1.5,
      al_get_display_width (al_get_current_display) - 1.5, TextFontSize * 4 - 1.5,
      lColor, 1
    );
    al_draw_text (lTextFont, lColor, Margin, Margin, 0, al_string_to_str (aText));
    al_draw_text (lTextFont, lColor, Margin, Margin * 2, 0, 'Press [C] to close');
    al_flip_display;
  { No needed anymore. }
    al_destroy_font (lTextFont);
  { Wait keypress. }
    WaitCPress
  end;



  function Clamp (aLo, aMid, aHi: Integer): Integer;
  begin
    if aMid < aLo then
      Result := aLo
    else if aMid > aHi then
      Result := aHi
    else
      Result := aMid
  end;



  function Clamp (aLo, aMid, aHi: Real): Real;
  begin
    if aMid < aLo then
      Result := aLo
    else if aMid > aHi then
      Result := aHi
    else
      Result := aMid
  end;



{$IFDEF DCC}
{ Implementation is similar than FPC's DoDirSeparators one. }
  procedure DoDirSeparators (var aPath: String);
  const
    DirSeparators: array [0..1] of Char = ('/', '\');
  var
    lCnt : longint;
  begin
    for lCnt := 1 to Length (aPath) do
      if (aPath[lCnt] = DirSeparators[0])
      or (aPath[lCnt] = DirSeparators[1])
      then
        aPath[lCnt] := PathDelim
  end;
{$ENDIF}

end.
