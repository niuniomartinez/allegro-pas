program ex_shader_multitex;
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
    Allegro5, al5image;

  function LoadBitmap (const FileName: String): ALLEGRO_BITMAPptr;
  var
    lBitmap: ALLEGRO_BITMAPptr;
  begin
    lBitmap := al_load_bitmap (FileName);
    if lBitmap = Nil then
      AbortExample (FileName + ' not found or failed to load');
    Exit (lBitmap)
  end;


  var
    Display: ALLEGRO_DISPLAYptr = Nil;
    Bitmap: array [0..1] of ALLEGRO_BITMAPptr = (Nil, Nil);
    Timer: ALLEGRO_TIMERptr = Nil;
    Queue: ALLEGRO_EVENT_QUEUEptr = Nil;
    Event: ALLEGRO_EVENT;
    Redraw: Boolean = True;
    Shader: ALLEGRO_SHADERptr = Nil;
    T, dw, dh: Integer;
    PixelFile: String;
    Scale, Angle, x, y: Double;
begin
  T := 0;
  PixelFile := '';

  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_mouse;
  al_install_keyboard;
  al_init_image_addon;
  InitPlatformSpecific;

  al_set_new_bitmap_flags (ALLEGRO_MIN_LINEAR or ALLEGRO_MAG_LINEAR or ALLEGRO_MIPMAP);
  al_set_new_display_option (ALLEGRO_SAMPLE_BUFFERS, 1, ALLEGRO_SUGGEST);
  al_set_new_display_option (ALLEGRO_SAMPLES, 4, ALLEGRO_SUGGEST);
  al_set_new_display_flags (ALLEGRO_PROGRAMMABLE_PIPELINE);
  Display := al_create_display (640, 480);
  if not Assigned (Display) then AbortExample ('Error creating display.');

  Bitmap[0] := LoadBitmap ('data/mysha.pcx');
  Bitmap[1] := LoadBitmap ('data/obp.jpg');

  Shader := al_create_shader (ALLEGRO_SHADER_AUTO);
  if not Assigned (Shader) then AbortExample ('Error creating shader.');

  if al_get_shader_platform (Shader) = ALLEGRO_SHADER_GLSL then
  { TODO: #ifdef ALLEGRO_CFG_SHADER_GLSL }
    PixelFile := 'data/ex_shader_multitex_pixel.glsl'
  else
  { TODO: #ifdef ALLEGRO_CFG_SHADER_HLSL }
    PixelFile := 'data/ex_shader_multitex_pixel.hlsl';

  if PixelFile = '' then AbortExample ('No shader source');
  if not al_attach_shader_source (
    Shader, ALLEGRO_VERTEX_SHADER,
    al_get_default_shader_source (ALLEGRO_SHADER_AUTO, ALLEGRO_VERTEX_SHADER))
  then
    AbortExample (
      'al_attach_shader_source for vertex shader failed: ' +
      al_get_shader_log (Shader)
    );
  if not al_attach_shader_source_file (Shader, ALLEGRO_PIXEL_SHADER, PixelFile)
  then
    AbortExample (
      'al_attach_shader_source_file for pixel shader failed: ' +
      al_get_shader_log (Shader)
    );
  if not al_build_shader (Shader) then
    AbortExample ('al_build_shader failed: ' + al_get_shader_log (Shader));

  al_use_shader (Shader);

  Timer := al_create_timer (1 / 60);
  Queue := al_create_event_queue;
  al_register_event_source(Queue, al_get_keyboard_event_source);
  al_register_event_source(Queue, al_get_display_event_source (Display));
  al_register_event_source(Queue, al_get_timer_event_source (Timer));
  al_start_timer (Timer);

  repeat
    al_wait_for_event (Queue, @Event);
    if Event.ftype = ALLEGRO_EVENT_DISPLAY_CLOSE then
       Break;
    if Event.ftype = ALLEGRO_EVENT_KEY_CHAR then
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Break;
    if Event.ftype = ALLEGRO_EVENT_TIMER then
    begin
      Redraw := True;
      Inc (T)
    end;

    if Redraw and al_is_event_queue_empty (Queue) then
    begin
      Scale := 1 + 100 * (1 + sin (t * ALLEGRO_PI * 2 / 60 / 10));
      Angle := ALLEGRO_PI * 2 * t / 60 / 15;
      x := 120 - 20 * cos (ALLEGRO_PI * 2 * t / 60 / 25);
      y := 120 - 20 * sin (ALLEGRO_PI * 2 * t / 60 / 25);

      dw := al_get_display_width (Display);
      dh := al_get_display_height (Display);

      Redraw := False;
      al_clear_to_color (al_map_rgb_f (0, 0, 0));

    { We set a second bitmap for texture unit 1. Unit 0 will have
      the normal texture which al_draw_*_bitmap will set up for us.
      We then draw the bitmap like normal, except it will use the
      custom shader.
    }
      al_set_shader_sampler ('tex2', Bitmap[1], 1);
      al_draw_scaled_rotated_bitmap (
        Bitmap[0], x, y, dw / 2, dh / 2,
        Scale, Scale, Angle, 0
      );
      al_flip_display
    end
  until False;

  al_use_shader (Nil);

  al_destroy_bitmap (Bitmap[0]);
  al_destroy_bitmap (Bitmap[1]);
  al_destroy_shader (Shader)
end.
