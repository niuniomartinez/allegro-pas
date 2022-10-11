program ex_depth_mask;
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
    Common,
    Allegro5, al5font, al5image, al5ttf,
    sysutils;

  const
    FPS = 60;
    COUNT = 80;

  var
    Display: ALLEGRO_DISPLAYptr;
    W, H: Integer;
    Timer: ALLEGRO_TIMERptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Mysha, Obp: ALLEGRO_BITMAPptr;
    Font, Font2: ALLEGRO_FONTptr;
    DirectSpeedMeasure: Single;
    Sprites: array [1..COUNT] of record
      X, Y, Angle: Double;
    end;

  procedure Redraw;
  var
    T: ALLEGRO_TRANSFORM;
    i, x, y: Integer;
  begin
  { We first draw the Obp background and clear the depth buffer to 1. }
    al_set_render_state (ALLEGRO_ALPHA_TEST, 1);
    al_set_render_state (ALLEGRO_ALPHA_function, ALLEGRO_RENDER_GREATER);
    al_set_render_state (ALLEGRO_ALPHA_TEST_VALUE, 0);

    al_set_render_state (ALLEGRO_DEPTH_TEST, 0);

    al_set_render_state (ALLEGRO_WRITE_MASK, ALLEGRO_MASK_DEPTH or ALLEGRO_MASK_RGBA);

    al_clear_depth_buffer (1);
    al_clear_to_color (al_map_rgb_f (0, 0, 0));

    al_draw_scaled_bitmap (Obp, 0, 0, 532, 416, 0, 0, W, 416 * W / 532, 0);

  { Next we draw all sprites but only to the depth buffer (with a depth value
    of 0). }
    al_set_render_state (ALLEGRO_DEPTH_TEST, 1);
    al_set_render_state (ALLEGRO_DEPTH_FUNCTION, ALLEGRO_RENDER_ALWAYS);
    al_set_render_state (ALLEGRO_WRITE_MASK, ALLEGRO_MASK_DEPTH);

    for i := Low (Sprites) to High (Sprites) do
    begin
      al_hold_bitmap_drawing (True);
      Y := -H;
      repeat
        x := -W;
        repeat
          al_identity_transform (t);
          al_rotate_transform (t, Sprites[i].Angle);
          al_translate_transform (t, Sprites[i].x + x, Sprites[i].y + y);
          al_use_transform (t);
          al_draw_text (
            Font, al_map_rgb (0, 0, 0),
            0, 0, ALLEGRO_ALIGN_CENTER,
            'Allegro 5'
          );
          Inc (x, W)
        until x > 0;
        Inc (y, H)
      until y > 0;
      al_hold_bitmap_drawing (False);
    end;
    al_identity_transform (t);
    al_use_transform (t);

  { Finally we draw Mysha, with depth testing so she only appears where
    sprites have been drawn before. }
    al_set_render_state (ALLEGRO_DEPTH_FUNCTION, ALLEGRO_RENDER_EQUAL);
    al_set_render_state (ALLEGRO_WRITE_MASK, ALLEGRO_MASK_RGBA);
    al_draw_scaled_bitmap (Mysha, 0, 0, 320, 200, 0, 0, 320 * H / 200, H, 0);

  { Finally we draw an FPS counter. }
    al_set_render_state (ALLEGRO_DEPTH_TEST, 0);

    al_draw_textf (
      Font2, al_map_rgb_f (1, 1, 1), W, 0, ALLEGRO_ALIGN_RIGHT,
      '%.1f FPS', [1.0 / DirectSpeedMeasure]
    )
  end;



  procedure Update;
  var
    i: Integer;
  begin
    for i := Low (Sprites) to High (Sprites) do
    begin
      Sprites[i].x := Sprites[i].x - 4;
      if Sprites[i].x < 80 then
	Sprites[i].x := Sprites[i].x + W;
      Sprites[i].angle := Sprites[i].angle + i * ALLEGRO_PI / 180 / COUNT
    end
  end;



  procedure Initialize;
  var
    i: Integer;
  begin
    if not al_init then AbortExample ('Could not init Allegro.');
    if not al_init_image_addon then AbortExample ('Failed to init IIO addon.');
    al_init_font_addon;
    if not al_init_ttf_addon then AbortExample ('Failed to init TTF addon.');
    InitPlatformSpecific;

    al_get_num_video_adapters;

{ TODO: #ifdef ALLEGRO_IPHONE
   al_set_new_display_flags(ALLEGRO_FULLSCREEN_WINDOW);
   #ENDIF
}
    al_set_new_display_option (
      ALLEGRO_SUPPORTED_ORIENTATIONS, ALLEGRO_DISPLAY_ORIENTATION_ALL,
      ALLEGRO_SUGGEST);

    al_set_new_display_option (ALLEGRO_DEPTH_SIZE, 8,
      ALLEGRO_SUGGEST);

    al_set_new_bitmap_flags (ALLEGRO_MIN_LINEAR or ALLEGRO_MAG_LINEAR);

    W := 640; H := 480;
    Display := al_create_display (W, H);
    if Display = Nil then AbortExample ('Error creating display.');

    if not al_install_keyboard then AbortExample ('Error installing keyboard.');

    Font := al_load_font ('data/DejaVuSans.ttf', 40, 0);
    if Font = Nil then AbortExample ('Error loading data/DejaVuSans.ttf');
    Font2 := al_load_font ('data/DejaVuSans.ttf', 12, 0);
    if Font2 = Nil then AbortExample ('Error loading data/DejaVuSans.ttf');

    Mysha := al_load_bitmap ('data/mysha.pcx');
    if Mysha = Nil then AbortExample ('Error loading data/mysha.pcx');

    Obp := al_load_bitmap ('data/obp.jpg');
    if Obp = Nil then AbortExample ('Error loading data/obp.jpg');

    for i := Low (Sprites) to High (Sprites) do
    begin
      Sprites[i].x := (i mod 4) * 160;
      Sprites[i].y := (i / 4) * 24
    end;
    Timer := al_create_timer (1.0 / FPS);

    Queue := al_create_event_queue;
    al_register_event_source (Queue, al_get_keyboard_event_source);
    al_register_event_source (Queue, al_get_timer_event_source (Timer));
    al_register_event_source (Queue, al_get_display_event_source (Display));

    DirectSpeedMeasure := al_get_time
  end;



  procedure Run;
  var
    Done, NeedRedraw, Background: Boolean;
    Event: ALLEGRO_EVENT;
    t: Double;
  begin
    Done := False;
    NeedRedraw := True;
    Background := False;
    al_start_timer (Timer);

    while not Done do
    begin
      if not Background and NeedRedraw
      and al_is_event_queue_empty (Queue) then
      begin
	t := -al_get_time;

	Redraw;

	t := t + al_get_time;
	DirectSpeedMeasure := t;
	al_flip_display;
	NeedRedraw := False;
      end;

      al_wait_for_event (Queue, @Event);
      case Event.ftype of
      ALLEGRO_EVENT_KEY_CHAR:
	if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Done := True;
      ALLEGRO_EVENT_DISPLAY_CLOSE:
	Done := True;
      ALLEGRO_EVENT_DISPLAY_HALT_DRAWING:
	begin
	  Background := True;
	  al_acknowledge_drawing_halt (Event.display.source)
	end;
      ALLEGRO_EVENT_DISPLAY_RESUME_DRAWING:
	Background := False;
      ALLEGRO_EVENT_DISPLAY_RESIZE:
	al_acknowledge_resize (Event.display.source);
      ALLEGRO_EVENT_TIMER:
	begin
	  Update;
	  NeedRedraw := True;
	end;
      end
    end
  end;

begin
  Initialize;
  Run
end.
