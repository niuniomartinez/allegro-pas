PROGRAM ex_lines;
(*
 * This example exercises line drawing, and single buffer mode.
 *)
(*
  Copyright (c) 2012-2018 Guillermo Martínez J.

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
  common,
  Allegro5,
  al5primitives;

(* XXX the software line drawer currently doesn't perform clipping properly *)

  const
     W = 640;
     H = 480;

   var
      Display: ALLEGRO_DISPLAYptr;
      Queue: ALLEGRO_EVENT_QUEUEptr;
      Black, White, Background: ALLEGRO_COLOR;
      dBuf: ALLEGRO_BITMAPptr;

      LastX: Integer = -1;
      LastY: Integer = -1;

   procedure Fade;
   begin
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA);
      al_draw_filled_rectangle (0, 0, W, H, al_map_rgba_f (0.5, 0.5, 0.6, 0.2));
   end;



   procedure RedDot (x, y: Integer); inline;
   begin
      al_draw_filled_rectangle (x - 2, y - 2, x + 2, y + 2, al_map_rgb_f (1, 0, 0));
   end;



   procedure DrawClipRect; inline;
   begin
      al_draw_rectangle (100.5, 100.5, W - 100.5, H - 100.5, Black, 0);
   end;



   procedure SetClipRect; inline;
   begin
      al_set_clipping_rectangle (100, 100, W - 200, H - 200);
   end;



   procedure ResetClipRect; inline;
   begin
      al_set_clipping_rectangle (0, 0, W, H);
   end;



   procedure Flip;
   begin
      al_set_target_backbuffer (Display);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      al_draw_bitmap (dBuf, 0.0, 0.0, 0);
      al_flip_display;
   end;


   procedure Plonk (const x, y: Integer; Blend: Boolean);
   begin
      al_set_target_bitmap (dBuf);

      Fade;
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_ZERO);
      DrawClipRect;
      RedDot (x, y);

      if (LastX = -1) and (LastY = -1) then
      begin
         LastX := x;
         LastY := y;
      end
      else begin
         SetClipRect;
         if Blend then
            al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA);
         al_draw_line (LastX, LastY, x, y, white, 0);
         LastX := -1;
         LastY := -1;
         ResetClipRect;
      end;
      Flip;
   end;



   procedure Splat (const x, y: Integer; Blend: Boolean);
   var
      Theta: SINGLE;
   begin
      al_set_target_bitmap (dBuf);

      Fade;
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_ZERO);
      DrawClipRect;
      RedDot (x, y);

      SetClipRect;
      if Blend then
         al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA);
      Theta := 0;
      repeat
         al_draw_line (x, y, x + 40.0 * cos (Theta), y + 40.0 * sin (Theta), White, 0);
         Theta := Theta + ALLEGRO_PI / 16;
      until Theta >= 2 * ALLEGRO_PI;
      ResetClipRect;

      Flip;
   end;



var
   Event: ALLEGRO_EVENT;
   KeyboardState: ALLEGRO_KEYBOARD_STATE;
   Blend, EndLoop: Boolean;
begin
   if not al_init then
      AbortExample ('Could not init Allegro');

   al_init_primitives_addon;
   al_install_keyboard;
   al_install_mouse;

   Display := al_create_display (W, H);
   if Display = Nil then
      AbortExample ('Error creating display');

   Black := al_map_rgb_f (0.0, 0.0, 0.0);
   White := al_map_rgb_f (1.0, 1.0, 1.0);
   Background := al_map_rgb_f (0.5, 0.5, 0.6);

   if (Paramcount > 0) and (Paramstr (1) = '--memory-bitmap') then
      al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
   dBuf := al_create_bitmap (W, H);
   if dBuf = Nil then
      AbortExample ('Error creating double buffer');

   al_set_target_bitmap (dBuf);
   al_clear_to_color (Background);
   DrawClipRect;
   Flip;

   Queue := al_create_event_queue;
   al_register_event_source (Queue, al_get_keyboard_event_source);
   al_register_event_source (Queue, al_get_mouse_event_source);

   EndLoop := False;
   repeat
      al_wait_for_event (Queue, @Event);
      if Event.ftype = ALLEGRO_EVENT_MOUSE_BUTTON_DOWN then
      begin
         al_get_keyboard_state (KeyboardState);
         Blend := al_key_down (KeyboardState, ALLEGRO_KEY_LSHIFT)
               or al_key_down (KeyboardState, ALLEGRO_KEY_RSHIFT);
         if Event.mouse.button = 1 then
            Plonk (Event.mouse.x, Event.mouse.y, Blend)
         else
            Splat (Event.mouse.x, Event.mouse.y, Blend);
      end
      else if Event.ftype = ALLEGRO_EVENT_DISPLAY_SWITCH_OUT then
      begin
         LastX := -1;
         LastY := -1;
      end
      else if (Event.ftype = ALLEGRO_EVENT_KEY_DOWN)
      and (Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE) then
         EndLoop := True;
   until EndLoop;

   al_destroy_event_queue (Queue);
   al_destroy_bitmap (dBuf);
end.
