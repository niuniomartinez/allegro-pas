PROGRAM ex_keyboard_events;
(*
 *    Example program for the Allegro library, by Peter Wang.
 *    Updated by Ryan Dickie.
 *    Pascal version by Guillermo "Ñuño" Martínez.
 *
 *    This program tests keyboard events.
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
     Allegro5,
     sysutils;

  CONST
    WIDTH    = 640;
    HEIGHT   = 480;

  VAR
(* globals *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Display: ALLEGRO_DISPLAYptr;



  PROCEDURE LogKey (CONST How: STRING; KeyCode, UniChar, Modifiers: INTEGER);
  VAR
  { TODO:
    Multibyte: ANSISTRING = '     ';
  }
    KeyName: STRING;
  BEGIN
    IF UniChar <= 32 THEN UniChar := 32;
  { TODO:  al_utf8_encode (Multibyte, UniChar); }
    KeyName := al_keycode_to_name (KeyCode);
    LogWriteLn (Format (
  { TODO :
      '%-8s  code=%03d, char=''%s'' (%4d), modifiers=%08x, [%s]',
      [How, KeyCode, Multibyte, UniChar, Modifiers, KeyName]
   }
      '%-8s  code=%03d, char=''%s'' (%4d), modifiers=%08x, [%s]',
      [How, KeyCode, '<na>', UniChar, Modifiers, KeyName]
    ))
  END;



(* main_loop:
 *  The main loop of the program.  Here we wait for events to come in from
 *  any one of the event sources and react to each one accordingly.  While
 *  there are no events to react to the program sleeps and consumes very
 *  little CPU time.  See main() to see how the event sources and event queue
 *  are set up.
 *)
  PROCEDURE MainLoop;
  VAR
    Event: ALLEGRO_EVENT;
    aLabel: STRING;
  BEGIN
    LogWriteLn ('Focus on the main window (black) and press keys to see events.');
    LogWriteLn ('Escape quits.');
    LogWriteLn ('');

    WHILE TRUE DO
    BEGIN
    { Take the next event out of the event queue, and store it in `event'. }
      al_wait_for_event (EventQueue, Event);

    {* Check what type of event we got and act accordingly.  ALLEGRO_EVENT
     * is a union type and interpretation of its contents is dependent on
     * the event type, which is given by the 'type' field.
     *
     * Each event also comes from an event source and has a timestamp.
     * These are accessible through the 'any.source' and 'any.timestamp'
     * fields respectively, e.g. 'event.any.timestamp'
     *}
       CASE Event._type OF
       ALLEGRO_EVENT_KEY_DOWN:
       { ALLEGRO_EVENT_KEY_DOWN - a keyboard key was pressed. }
         BEGIN
           IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
             EXIT;
           LogKey ('KEY_DOWN', Event.keyboard.keycode, 0, 0);
         END;
       ALLEGRO_EVENT_KEY_UP:
       { ALLEGRO_EVENT_KEY_UP - a keyboard key was released. }
         LogKey ('KEY_UP', Event.keyboard.keycode, 0, 0);
       ALLEGRO_EVENT_KEY_CHAR:
       { ALLEGRO_EVENT_KEY_CHAR - a character was typed or repeated. }
         BEGIN
           IF Event.keyboard._repeat THEN
             aLabel := 'repeat'
           ELSE
             aLabel := 'KEY_CHAR';
           LogKey (aLabel,
             Event.keyboard.keycode,
             Event.keyboard.unichar,
             Event.keyboard.modifiers
           )
         END;
       ALLEGRO_EVENT_DISPLAY_CLOSE:
       { ALLEGRO_EVENT_DISPLAY_CLOSE - the window close button was pressed. }
         EXIT;
       ELSE
       { We received an event of some type we don't know about.
         Just ignore it. }
         ;
      END;
    END
  END;



BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLogMonospace;

  Display := al_create_display(WIDTH, HEIGHT);
  IF Display = NIL THEN AbortExample ('al_create_display failed');
  al_clear_to_color (al_map_rgb_f (0, 0, 0));
  al_flip_display;

  IF NOT al_install_keyboard THEN AbortExample ('al_install_keyboard failed');

  EventQueue := al_create_event_queue;
  IF EventQueue = NIL THEN AbortExample ('al_create_event_queue failed');

  al_register_event_source (EventQueue, al_get_keyboard_event_source());
  al_register_event_source (EventQueue, al_get_display_event_source (Display));

  MainLoop;

  CloseLog (FALSE);
END.
