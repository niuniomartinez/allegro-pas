UNIT GameManager;
(*< Defines the game manager. *)
(*
  Copyright (c) 2019 Guillermo Martínez J.

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

INTERFACE

  USES
    Asteroids, Effects, Input,
    Allegro5;

  CONST
  (* Desired frame rate. *)
    FPS = 14;
  (* #pts per new extra ship. *)
    EXTRA_SHIP = 10000;

  TYPE
  (* The game manager. *)
    TGameManager = CLASS (TObject)
    PRIVATE
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;
      fDisplay: ALLEGRO_DISPLAYptr;
      fTimer: ALLEGRO_TIMERptr;

      fUserInput: TUserInput;

      fAsteroidManager: TAsteroidManager;
      fEffectManager: TEffectManager;

      PROCEDURE CreateDisplay;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes the game. *)
      PROCEDURE Initialize;
    (* The game loop. *)
      PROCEDURE Run;

    (* Access to user input. *)
      PROPERTY Input: TUserInput READ fUserInput;
    (* Access to asteroid manager. *)
      PROPERTY Asteroids: TAsteroidManager READ fAsteroidManager;
    (* Access to effects manager. *)
      PROPERTY Effects: TEffectManager READ fEffectManager;
    END;

  VAR
  (* Global reference to the game manager. *)
    Game: TGameManager;

IMPLEMENTATION

  USES
    Player,
    Engine, Graphics,
    al5font,
    sysutils;

(*
 * TGameManager
 ***************************************************************************)

(* Creates the display.

   Right now it just creates a window. *)
  PROCEDURE TGameManager.CreateDisplay;
  BEGIN
    IF fDisplay <> NIL THEN al_destroy_display (fDisplay);
    fDisplay := al_create_display (MAX_WIDTH, MAX_HEIGHT);
    IF fDisplay = NIL THEN
      RAISE Exception.Create ('Failed to create display.');
    al_set_window_title (fDisplay, 'Pascaleroids - Allegro game demo');
    al_register_event_source (fEventQueue, al_get_display_event_source (fDisplay))
  END;



(* Constructor. *)
  CONSTRUCTOR TGameManager.Create;
  BEGIN
    IF Game <> NIL THEN
      RAISE Exception.Create ('Only one game manager per game!');
    INHERITED Create;
    fUserInput := TKeyboardInput.Create;
    fAsteroidManager := TAsteroidManager.Create;
    fEffectManager := TEffectManager.Create;
    Game := SELF
  END;



(* Destructor. *)
  DESTRUCTOR TGameManager.Destroy;
  BEGIN
    fEffectManager.Free;
    fAsteroidManager.Free;
    fUserInput.Free;
  { Actually, most of these destructions aren't necessary as Allegro cleans them
    when terminating the application, but it is good to be polite. }
    IF fDisplay <> NIL THEN al_destroy_display (fDisplay);

    IF fEventQueue <> NIL THEN al_destroy_event_queue (fEventQueue);
    al_uninstall_system;
    INHERITED Destroy
  END;



(* Initializes the game. *)
  PROCEDURE TGameManager.Initialize;
  BEGIN
  { Initialize Allegro. }
    IF NOT al_init THEN
      RAISE Exception.Create ('Can''t initialize Allegro.');
    fEventQueue := al_create_event_queue;
    Graphics.Initialize;
    SELF.CreateDisplay;
    al_install_keyboard;
    al_register_event_source (fEventQueue, al_get_keyboard_event_source);
    fTimer := al_create_timer (ALLEGRO_BPS_TO_SECS (FPS));
    al_register_event_source (fEventQueue, al_get_timer_event_source (fTimer));
    Randomize;
  END;



(* Execution. *)
  PROCEDURE TGameManager.Run;
  VAR
    Event: ALLEGRO_EVENT;
    lPlayerShip: TShip;
    lFont: ALLEGRO_FONTptr;
  BEGIN
    lPlayerShip := TShip.Create;
TRY
    lFont := al_create_builtin_font;
    fAsteroidManager.Initialize;
    fEffectManager.Initialize;
    fUserInput.Initialize;
    lPlayerShip.Initialize;
    fAsteroidManager.NewGame;
    fEffectManager.NewGame;
    al_start_timer (fTimer);
    REPEAT
      al_wait_for_event (fEventQueue, Event);
      IF NOT fUserInput.ProcessEvent (Event) THEN
      CASE Event.ftype OF
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        EXIT;
      ALLEGRO_EVENT_KEY_CHAR:
        CASE Event.keyboard.keycode OF
        ALLEGRO_KEY_ESCAPE:
          EXIT;
        ALLEGRO_KEY_SPACE:
          BEGIN
            fAsteroidManager.NewBoard (Random (NUM_LARGE));
            fEffectManager.AddAsteroidExplosion (MAX_WIDTH DIV 2, MAX_HEIGHT DIV 2)
          END;
        END;
      ALLEGRO_EVENT_TIMER:
        BEGIN
          IF (Input.Direction AND DIR_LEFT) <> 0 THEN
            lPlayerShip.RotateLeft;
          IF (Input.Direction AND DIR_RIGHT) <> 0 THEN
            lPlayerShip.RotateRight;
          IF (Input.Direction AND DIR_UP) <> 0 THEN
            lPlayerShip.Thrust;
          fAsteroidManager.Update;
          fEffectManager.Update;
          lPlayerShip.Update;
        END;
      END;
    { Draw and wait. }
      IF al_is_event_queue_empty (fEventQueue) THEN
      BEGIN
        al_clear_to_color (Graphics.Black);
        al_use_transform (Graphics.IdentityMatrix);
        al_draw_text (lFont, Graphics.White, 1, 1, 0, Format (
          'dir: %d - btn: %d',
          [Input.Direction, Input.Button]
        ));
        fAsteroidManager.Paint;
        fEffectManager.Paint;
        lPlayerShip.Draw;
        al_flip_display
      END
    UNTIL FALSE
FINALLY
  lPlayerShip.Free;
END
  END;

END.
