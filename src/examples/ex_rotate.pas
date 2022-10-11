program ex_rotate;
(*
 *    Example program for the Allegro library, by Peter Wang.
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
   al5image;

  const
     DisplayW = 640;
     DisplayH = 480;

  var
     Display: ALLEGRO_DISPLAYptr;
     Buf, Bmp, MemBmp, SrcBmp: ALLEGRO_BITMAPptr;
     Queue: ALLEGRO_EVENT_QUEUEptr;
     Event: ALLEGRO_EVENT;
     Theta, K: Single;
     Mode, Flags: Integer;
     WideMode, MemSrcMode, TransMode, ClipMode: Boolean;
     Trans: ALLEGRO_COLOR;
     EndLoop: Boolean;

begin
   Theta := 0;
   K := 1;
   Mode := 0;
   WideMode := False;
   MemSrcMode := False;
   TransMode := False;
   Flags := 0;
   ClipMode := False;

   if not al_init then
      AbortExample ('Could not init Allegro');

   al_install_keyboard;
   al_init_image_addon;

   OpenLog;
   LogWriteLn ('Press ''w'' to toggle wide mode.');
   LogWriteLn ('Press ''s'' to toggle memory source bitmap.');
   LogWriteLn ('Press space to toggle drawing to backbuffer or off-screen bitmap.');
   LogWriteLn ('Press ''t'' to toggle translucency.');
   LogWriteLn ('Press ''h'' to toggle horizontal flipping.');
   LogWriteLn ('Press ''v'' to toggle vertical flipping.');
   LogWriteLn ('Press ''c'' to toggle clipping.');
   LogWriteLn ('');

   Display := al_create_display (DisplayW, DisplayH);
   if Display = Nil then
      AbortExample ('Error creating display');

   Buf := al_create_bitmap (DisplayW, DisplayH);
   if Buf = Nil then
      AbortExample ('Unable to create buffer');

   Bmp := al_load_bitmap ('data/mysha.pcx');
   if Bmp = Nil then
      AbortExample ('Unable to load image');

   al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
   MemBmp := al_load_bitmap ('data/mysha.pcx');
   if MemBmp = Nil then
      AbortExample ('Unable to load image');

   Queue := al_create_event_queue;
   al_register_event_source (Queue, al_get_keyboard_event_source);

   EndLoop := False;
   repeat
      if al_get_next_event (Queue, Event) then
      begin
         if Event.ftype = ALLEGRO_EVENT_KEY_CHAR then
         begin
            if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
               EndLoop := True;
            if Event.keyboard.unichar = Ord (' ') then
            begin
               mode := 1 - mode;
               if Mode = 0 then
                  LogWriteLn ('Drawing to off-screen buffer')
               else
                  LogWriteLn ('Drawing to display backbuffer');
            end;
            if Event.keyboard.unichar = Ord ('w') then
               WideMode := not WideMode;
            if Event.keyboard.unichar = Ord ('s') then
            begin
               MemSrcMode := not MemSrcMode;
               if MemSrcMode then
                  LogWriteLn ('Source is memory bitmap')
               else
                  LogWriteLn ('Source is display bitmap');
            end;
            if Event.keyboard.unichar = Ord ('t') then
               TransMode := not transmode;
            if Event.keyboard.unichar = Ord ('h') then
               Flags := Flags XOR ALLEGRO_FLIP_HORIZONTAL;
            if Event.keyboard.unichar = Ord ('v') then
               Flags := Flags XOR ALLEGRO_FLIP_VERTICAL;
            if Event.keyboard.unichar = Ord ('c') then
               ClipMode := not ClipMode;
         end;
      end;
      (*
       * mode 0 = draw scaled to off-screen buffer before
       *          blitting to display backbuffer
       * mode 1 = draw scaled to display backbuffer
       *)

      if Mode = 0 then
         al_set_target_bitmap (Buf)
      else
         al_set_target_backbuffer (Display);

      if MemSrcMode then
         SrcBmp := MemBmp
      else
         SrcBmp := Bmp;
      if WideMode then
         K := 2
      else
         K := 1;

      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      Trans := al_map_rgba_f (1, 1, 1, 1);
      if Mode = 0 then
         al_clear_to_color (al_map_rgba_f (1, 0, 0, 1))
      else
         al_clear_to_color (al_map_rgba_f (0, 0, 1, 1));

      if TransMode then
      begin
         al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA);
         Trans := al_map_rgba_f (1, 1, 1, 0.5);
      end;

      if ClipMode then
         al_set_clipping_rectangle (50, 50, DisplayW - 100, DisplayH - 100)
      else
         al_set_clipping_rectangle (0, 0, DisplayW, DisplayH);

      al_draw_tinted_scaled_rotated_bitmap (SrcBmp,
         Trans,
         50, 50, DisplayW div 2, DisplayH div 2,
         K, K, Theta,
         Flags);

      if Mode = 0 then
      begin
         al_set_target_backbuffer (Display);
         al_set_clipping_rectangle (0, 0, DisplayW, DisplayH);
         al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
         al_draw_bitmap (Buf, 0, 0, 0);
      end;

      al_flip_display;
      al_rest (0.01);
      Theta := Theta - 0.01;
   until EndLoop;
   al_destroy_bitmap (Bmp);
   al_destroy_bitmap (MemBmp);
   al_destroy_bitmap (Buf);

   CloseLog (False);
end.
