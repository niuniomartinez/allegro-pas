PROGRAM ex_drawpixels;
(*
  Copyright (c) 2012-2020 Guillermo Martínez J.

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
    Allegro5, Common;

  const
    WIDTH = 640;
    HEIGHT = 480;
    NUM_STARS = 300;
    TARGET_FPS = 9999;

  type
    TPoint = record
      X, Y: SINGLE;
    end;

  var
    Display: ALLEGRO_DISPLAYptr;
    KeyState: ALLEGRO_KEYBOARD_STATE;
    Stars: array [1..3] of array [1..(NUM_STARS div 3)] of TPoint;
    Speeds: array [1..3] of SINGLE = (0.0001, 0.05, 0.15);
    Colors: array [1..3] of ALLEGRO_COLOR;
    Start, Now, Elapsed, FrameCount: LongInt;
    TotalFrames: Integer;
    ProgramStart, Length: Double;
    Layer, Star, X, Y: Integer;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

  OpenLog;

  al_install_keyboard;

  Display := al_create_display (WIDTH, HEIGHT);
  if Display = Nil then AbortExample ('Could not create display.');

  Colors[1] := al_map_rgba (255, 100, 255, 128);
  Colors[2] := al_map_rgba (255, 100, 100, 255);
  Colors[3] := al_map_rgba (100, 100, 255, 255);

  for Layer := Low (Stars) to High (Stars) do
  begin
    for Star := Low (Stars[Layer]) to High (Stars[Layer]) do
    begin
      Stars[Layer][Star].X := Random (WIDTH);
      Stars[Layer][Star].Y := Random (HEIGHT)
    end
  end;


  Start := Trunc (al_get_time * 1000);
  Elapsed := 0;
  FrameCount := 0;
  ProgramStart := al_get_time;

  TotalFrames := 0;
  repeat
    if FrameCount < (1000 / TARGET_FPS) then
      FrameCount := FrameCount + Elapsed
    else begin
      Dec (FrameCount, Trunc (1000 / TARGET_FPS));
      al_clear_to_color (al_map_rgb (0, 0, 0));
      for Star := Low (Stars[1]) to High (Stars[1]) do
        al_draw_pixel (Stars[1][Star].X, Stars[1][Star].Y, Colors[1]);
      al_lock_bitmap (
        al_get_backbuffer (Display),
        ALLEGRO_PIXEL_FORMAT_ANY, ALLEGRO_LOCK_readwrite
      );

      for Layer := 2 to High (Stars) do
      begin
        for Star := Low (Stars[Layer]) to High (Stars[Layer]) do
          al_draw_pixel (
            Stars[Layer][Star].X, Stars[Layer][Star].Y, Colors[Layer]
          )
      end;

    { Check that dots appear at the window extremes. }
      X := WIDTH - 1;
      Y := HEIGHT - 1;
      al_put_pixel (0, 0, al_map_rgb_f (1, 1, 1));
      al_put_pixel (X, 0, al_map_rgb_f (1, 1, 1));
      al_put_pixel (0, Y, al_map_rgb_f (1, 1, 1));
      al_put_pixel (X, Y, al_map_rgb_f (1, 1, 1));

      al_unlock_bitmap (al_get_backbuffer (Display));
      al_flip_display;
      Inc (TotalFrames)
    end;

    Now := Trunc (al_get_time * 1000);
    Elapsed := Now - Start;
    Start := Now;

    for Layer := Low (Stars) to High (Stars) do
    begin
      for Star := Low (Stars[Layer]) to High (Stars[Layer]) do
      begin
        Stars[Layer][Star].Y := Stars[Layer][Star].Y - Speeds[Layer] * Elapsed;
        if Stars[Layer][Star].Y < 0 then
        begin
          Stars[Layer][Star].X := Random (WIDTH);
          Stars[Layer][Star].Y := HEIGHT
        end
      end
    end;

    al_rest (0.001);

    al_get_keyboard_state (KeyState);
  until al_key_down (KeyState, ALLEGRO_KEY_ESCAPE);

  Length := al_get_time - ProgramStart;

  if Length <> 0 then
    LogPrintLn ('%d FPS', [Trunc (TotalFrames / Length)]);

  al_destroy_display (Display);

  CloseLog (True)
end.
