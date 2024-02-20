program ex_01_helloworld;
(* Show basic initialization of Allegro.

   Just opens a window and waits for user to press a key and closes.
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

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  uses
    allegro5 in '../lib/allegro5.pas',
    al5font  in '../lib/al5font.pas';

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;
  var
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* To store a text font. *)
    TextFont: ALLEGRO_FONTptr;
  (* To store keyboard state. *)
    KeyboardState: ALLEGRO_KEYBOARD_STATE;
  (* To know when to stop. *)
    Terminated: Boolean;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Initialize keyboard. }
    if not al_install_keyboard then
    begin
      WriteLn ('Can''t initialize keyboard input!');
      Exit (False)
    end;
  { Initialize add-ons. }
    if not al_init_font_addon then
    begin
      WriteLn ('Can''t use text fonts!');
      Exit (False)
    end;
  { Create window. }
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
  { Create text font. }
    TextFont := al_create_builtin_font;
    if not Assigned (TextFont) then
    begin
    { Close window so it doesn't obscure the console. }
      al_destroy_display (Window); Window := Nil;
      WriteLn ('Error creating text font.');
      Exit (False)
    end;
    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
  { Allegro will destroy most objects at exit but it is a good idea to get used
    to destroy all created objects.
  }
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;

begin
{ Program initialization. }
  if not Initialize then Exit;
{ Draw text on screen. }
  al_draw_text (
    TextFont, al_map_rgb (255, 255, 255),
    wWidth div 2, wHeight div 2, ALLEGRO_ALIGN_CENTRE OR ALLEGRO_ALIGN_INTEGER,
    'Hello, World!'
  );
{ Make text visible. }
  al_flip_display;
{ "Game loop". }
  Terminated := False;
  repeat
  { Get key state. }
    al_get_keyboard_state (KeyboardState);
  { Check keyboard. }
    if al_key_down (KeyboardState, ALLEGRO_KEY_ESCAPE) then
      Terminated := True
  until Terminated;
{ Program finalization. }
  Finalize
end.
