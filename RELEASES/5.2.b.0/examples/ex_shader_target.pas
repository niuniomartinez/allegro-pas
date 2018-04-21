PROGRAM ex_shader_target;
(*
 *    Example program for the Allegro library.
 *
 *    Test that shaders are applied per target bitmap.
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
  USES
    Common,
    Allegro5, al5base, al5image,
    sysutils;

   VAR
     Tints: ARRAY [0..11] OF AL_FLOAT = (
       4.0, 0.0, 1.0,
       0.0, 4.0, 1.0,
       1.0, 0.0, 4.0,
       4.0, 4.0, 1.0
     );



  PROCEDURE ChooseShaderSource
    (Platform: ALLEGRO_SHADER_PLATFORM; VAR vSource, pSource: STRING);
  BEGIN
    IF Platform = ALLEGRO_SHADER_HLSL THEN
    BEGIN
      vSource := 'data/ex_shader_vertex.hlsl';
      pSource := 'data/ex_shader_pixel.hlsl'
    END
    ELSE IF Platform = ALLEGRO_SHADER_GLSL THEN
    BEGIN
      vSource := 'data/ex_shader_vertex.glsl';
      pSource := 'data/ex_shader_pixel.glsl'
    END
    ELSE BEGIN
    { Shouldn't happen. }
      vSource := '';
      pSource := ''
    END
  END;



  FUNCTION MakeRegion
    (parent: ALLEGRO_BITMAPptr; x, y, w, h: INTEGER; Shader: ALLEGRO_SHADERptr)
  : ALLEGRO_BITMAPptr;
  VAR
    MR: ALLEGRO_BITMAPptr;
  BEGIN
    MR := al_create_sub_bitmap (Parent, x, y, w, h);
    IF MR <> NIL THEN
    BEGIN
      al_set_target_bitmap (MR);
      al_use_shader (Shader)
    { Not bothering to restore old target bitmap. }
    END;
    EXIT (MR)
  END;



  VAR
    Display: ALLEGRO_DISPLAYptr;
    Image: ALLEGRO_BITMAPptr;
    BackBuffer: ALLEGRO_BITMAPptr;
    Region: ARRAY [0..3] OF ALLEGRO_BITMAPptr;
    Shader: ALLEGRO_SHADERptr;
    vSource, pSource: STRING;
    T: ALLEGRO_TRANSFORM;
    i: INTEGER;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
  { TODO:
    S: ALLEGRO_KEYBOARD_STATE;
  }
BEGIN
  vSource := ''; pSource := '';
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  al_install_keyboard;
  al_init_image_addon;
  InitPlatformSpecific;

  al_set_new_display_flags (ALLEGRO_PROGRAMMABLE_PIPELINE);
  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Could not create display.');

  Queue := al_create_event_queue;
  al_register_event_source(Queue, al_get_keyboard_event_source);
  al_register_event_source(Queue, al_get_display_event_source (Display));

  Image := al_load_bitmap ('data/mysha.pcx');
  IF Image = NIL THEN AbortExample ('Could not load image.');

{ Create the shader. }
  Shader := al_create_shader (ALLEGRO_SHADER_AUTO);
  IF Shader = NIL THEN AbortExample ('Could not create shader.');
  ChooseShaderSource (al_get_shader_platform (Shader), vSource, pSource);
  IF (vSource = '') OR (pSource ='') THEN
    AbortExample ('Could not load source files.');
  IF NOT al_attach_shader_source_file (Shader, ALLEGRO_VERTEX_SHADER, vSource)
  THEN
    AbortExample (Format (
      'al_attach_shader_source_file failed: %s',
      [al_get_shader_log (Shader)]
    ));
  IF NOT al_attach_shader_source_file (Shader, ALLEGRO_PIXEL_SHADER, pSource)
  THEN
    AbortExample (Format (
      'al_attach_shader_source_file failed: %s',
      [al_get_shader_log (Shader)]
    ));
  IF NOT al_build_shader (Shader) THEN
    AbortExample (Format (
      'al_build_shader failed: %s', [al_get_shader_log (Shader)]
    ));

{ Create four sub-bitmaps of the backbuffer sharing a shader. }
  BackBuffer := al_get_backbuffer (Display);
  Region[0] := MakeRegion (BackBuffer, 0, 0, 320, 200, Shader);
  Region[1] := MakeRegion (BackBuffer, 320, 0, 320, 200, Shader);
  Region[2] := MakeRegion (BackBuffer, 0, 240, 320, 200, Shader);
  Region[3] := MakeRegion (BackBuffer, 320, 240, 320, 200, Shader);
  IF (Region[0]=NIL) OR (region[1]=NIL) OR (region[2]=NIL) OR (region[3]=NIL)
  THEN
    AbortExample ('make_region failed');

{ Apply a transformation to the last region (the current target). }
  al_identity_transform (t);
  al_scale_transform (t, 2.0, 2.0);
  al_translate_transform (t, -160, -100);
  al_use_transform (t);

  REPEAT
    IF al_get_next_event (Queue, Event) THEN
    BEGIN
      IF Event._type = ALLEGRO_EVENT_DISPLAY_CLOSE THEN
        BREAK;
      IF Event._type = ALLEGRO_EVENT_KEY_CHAR THEN
        IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
          BREAK
    END;

    FOR I := LOW (Region) TO HIGH (Region) DO
    BEGIN
    { When we change the target bitmap, the shader that was last used on
      that bitmap is automatically in effect.  All of our region
      sub-bitmaps use the same shader so we need to set the tint variable
      each time, as it was clobbered when drawing to the previous region. }
      al_set_target_bitmap (Region[i]);
      al_set_shader_float_vector ('tint', 3, @Tints[i * 3], 1);
      al_draw_bitmap (Image, 0, 0, 0);
    END;

    al_set_target_backbuffer (Display);
    al_draw_tinted_bitmap (
      Image, al_map_rgba_f (0.5, 0.5, 0.5, 0.5),
      320 DIV 2, 240 DIV 2, 0
    );

    al_flip_display
  UNTIL FALSE;

  al_set_target_backbuffer (Display);
  al_use_shader (NIL);
  al_destroy_shader (Shader)
END.
