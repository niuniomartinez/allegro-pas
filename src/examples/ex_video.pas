PROGRAM ex_video;
(*    Example program for the Allegro library.
 *
 *    Demonstrate how to use the al5video add-on.
 *)
(*
  Copyright (c) 2019-2020 Guillermo Martínez J.

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
   allegro5, al5base, al5audio, al5font, al5primitives, al5video, al5strings,
   sysutils;

VAR
  Screen: ALLEGRO_DISPLAYptr;
  Font: ALLEGRO_FONTptr;
  FileName: AL_STR;
  Zoom: REAL;

  PROCEDURE VideoDisplay (Video: ALLEGRO_VIDEOptr);
  VAR
    ScaledW, ScaledH, Position: REAL;
    Frame: ALLEGRO_BITMAPptr;
    w, h, x, y: INTEGER;
    tc, bc: ALLEGRO_COLOR;
  BEGIN
  { Videos often do not use square pixels - these return the scaled dimensions
    of the video frame. }
    ScaledW := al_get_video_scaled_width (Video);
    ScaledH := al_get_video_scaled_height (Video);
  { Get the currently visible frame of the video, based on clock time. }
    frame := al_get_video_frame (Video);
    tc := al_map_rgba_f (0, 0, 0, 0.5);
    bc := al_map_rgba_f (0.5, 0.5, 0.5, 0.5);

    IF Frame = NIL THEN EXIT;

    IF Zoom = 0 THEN
    BEGIN
    { Always make the video fit into the window. }
      h := al_get_display_height (Screen);
      w := TRUNC (h * ScaledW / ScaledH);
      IF w > al_get_display_width (Screen) THEN
      BEGIN
        w := al_get_display_width (Screen);
        h := TRUNC (w * ScaledH / ScaledW)
      END
    END
    ELSE BEGIN
      w := TRUNC (ScaledW);
      h := TRUNC (ScaledH)
    END;
    x := (al_get_display_width (Screen) - w) DIV 2;
    y := (al_get_display_height (Screen) - h) DIV 2;

  { Display the frame. }
    al_draw_scaled_bitmap (
      Frame,
      0, 0, al_get_bitmap_width (frame), al_get_bitmap_height (frame),
      x, y, w, h,
      0
    );

  { Show some video information. }
    al_draw_filled_rounded_rectangle (
      4, 4,
      al_get_display_width (Screen) - 4, 4 + 14 * 4,
      8, 8, bc
    );
    Position := al_get_video_position (Video, ALLEGRO_VIDEO_POSITION_ACTUAL);
    al_draw_text (Font, tc, 8, 8 , 0, FileName);
    al_draw_textf (
      Font, tc, 8, 8 + 13, 0,
      '%3d:%02d (V: %+5.2f A: %+5.2f)',
      [
        TRUNC (Position / 60),
        TRUNC (Position) MOD 60,
        al_get_video_position (Video, ALLEGRO_VIDEO_POSITION_VIDEO_DECODE) - Position,
        al_get_video_position (Video, ALLEGRO_VIDEO_POSITION_AUDIO_DECODE) - Position
      ]
    );
    al_draw_textf (
      font, tc, 8, 8 + 13 * 2, 0,
      'video rate %.02f (%dx%d, aspect %.1f) audio rate %.0f',
      [
        al_get_video_fps (Video),
        al_get_bitmap_width (Frame),
        al_get_bitmap_height (Frame),
        ScaledW / ScaledH,
        al_get_video_audio_rate (Video)
      ]
    );
    IF al_is_video_playing (Video) THEN
      al_draw_text (Font, tc, 8, 8 + 13 * 3, 0, 'playing: true')
    ELSE
      al_draw_text (Font, tc, 8, 8 + 13 * 3, 0, 'playing: false');
    al_flip_display;
    al_clear_to_color (al_map_rgb (0, 0, 0))
  END;



  PROCEDURE Done;
  BEGIN
    al_destroy_display (Screen);
    CloseLog (TRUE)
  END;

VAR
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Timer: ALLEGRO_TIMERptr;
  Video: ALLEGRO_VIDEOptr;
  FullScreen, Redraw, UseFrameEvents: BOOLEAN;
  FilenameArgIdx: INTEGER;

BEGIN
  FullScreen := FALSE;
  Redraw := TRUE;
  UseFrameEvents := FALSE;
  FilenameArgIdx := 1;

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLog;

  IF Paramcount < 2 THEN
  BEGIN
    LogWriteLn ('This example needs to be run from the command line.');
    LogPrintLn ('Usage: %s [--use-frame-events] <file>', [ExtractFileName (ParamStr (0))]);
    Done;
    Halt (1)
  END;

{ If UseFrameEvents is FALSE, we use a fixed FPS timer. If the video is
  displayed in a game this probably makes most sense. In a dedicated video
  player you probably want to listen to ALLEGRO_EVENT_VIDEO_FRAME_SHOW events
  and only redraw whenever one arrives - to reduce possible jitter and save CPU.
}
  IF (Paramcount = 3) AND (ParamStr (1) = '--use-frame-events') THEN
  BEGIN
    UseFrameEvents := TRUE;
    INC (FilenameArgIdx)
  END;

  IF NOT al_init_video_addon THEN
    AbortExample ('Could not initialize the video addon.');
  al_init_font_addon;
  al_install_keyboard;

  al_install_audio;
  al_reserve_samples (1);
  al_init_primitives_addon;

  Timer := al_create_timer(1.0 / 60);

  al_set_new_display_flags (ALLEGRO_RESIZABLE);
  al_set_new_display_option (ALLEGRO_VSYNC, 1, ALLEGRO_SUGGEST);
  Screen := al_create_display (640, 480);
  IF Screen = NIL THEN
    AbortExample ('Could not set video mode - exiting');

  Font := al_create_builtin_font;
  IF Font = NIL THEN AbortExample ('No font.');

  al_set_new_bitmap_flags (ALLEGRO_MIN_LINEAR OR ALLEGRO_MAG_LINEAR);

  FileName := al_string_to_str (ParamStr (FilenameArgIdx));
  Video := al_open_video (FileName);
  IF Video = NIL THEN
    AbortExample (al_str_format ('Cannot read %s.', [FileName]));
  LogPrintLn ('video FPS: %f', [al_get_video_fps (Video)]);
  LogPrintLn ('video audio rate: %f', [al_get_video_audio_rate (Video)]);
  LogWriteLn ('keys:');
  LogWriteLn ('Space: Play/Pause');
  LogWriteLn ('cursor right/left: seek 10 seconds');
  LogWriteLn ('cursor up/down: seek one minute');
  LogWriteLn ('F: toggle fullscreen');
  LogWriteLn ('1: disable scaling');
  LogWriteLn ('S: scale to window');

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_video_event_source (Video));
  al_register_event_source (Queue, al_get_display_event_source (Screen));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_register_event_source (Queue, al_get_keyboard_event_source);

  al_start_video (Video, al_get_default_mixer);
  al_start_timer (Timer);
  WHILE TRUE DO
  BEGIN
    IF Redraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      VideoDisplay (Video);
      Redraw := FALSE
    END;

    al_wait_for_event (Queue, @Event);
    CASE Event.ftype OF
    ALLEGRO_EVENT_KEY_DOWN:
      CASE event.keyboard.keycode OF
      ALLEGRO_KEY_SPACE:
          al_set_video_playing (Video, NOT al_is_video_playing (Video));
      ALLEGRO_KEY_ESCAPE:
        BEGIN
          al_close_video (Video);
          Done
        END;
      ALLEGRO_KEY_LEFT:
        al_seek_video (Video, al_get_video_position (Video, ALLEGRO_VIDEO_POSITION_ACTUAL) - 10);
      ALLEGRO_KEY_RIGHT:
        al_seek_video (Video, al_get_video_position (Video, ALLEGRO_VIDEO_POSITION_ACTUAL) + 10);
      ALLEGRO_KEY_UP:
        al_seek_video (Video, al_get_video_position (Video, ALLEGRO_VIDEO_POSITION_ACTUAL) + 60);
      ALLEGRO_KEY_DOWN:
        al_seek_video (Video, al_get_video_position (Video, ALLEGRO_VIDEO_POSITION_ACTUAL) - 60);
      ALLEGRO_KEY_F:
        BEGIN
          FullScreen := NOT FullScreen;
          al_set_display_flag (Screen, ALLEGRO_FULLSCREEN_WINDOW, FullScreen);
        END;
      ALLEGRO_KEY_1:
        Zoom := 1;
      ALLEGRO_KEY_S:
        Zoom := 0;
      END;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      BEGIN
        al_acknowledge_resize (Screen);
        al_clear_to_color (al_map_rgb (0, 0, 0));
      END;
    ALLEGRO_EVENT_TIMER:
       {
            display_time += 1.0 / 60;
            if (display_time >= video_time)
               video_time = display_time + video_refresh_timer(is);
       }
      IF NOT UseFrameEvents THEN Redraw := TRUE;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      BEGIN
        al_close_video (Video);
        Done
      END;
    ALLEGRO_EVENT_VIDEO_FRAME_SHOW:
      IF UseFrameEvents THEN Redraw := TRUE;
    ALLEGRO_EVENT_VIDEO_FINISHED:
      LogWriteLn ('video finished.');
    END
  END;
{ Done }
  al_destroy_display (Screen);
  CloseLog (TRUE)
END.
