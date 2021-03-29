unit GameManager;
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

interface

  uses
    Asteroids, Effects, Input,
    Allegro5;

  const
  (* Desired frame rate. *)
    FPS = 14;
  (* #pts per new extra ship. *)
    EXTRA_SHIP = 10000;

  type
  (* The game manager. *)
    TGameManager = class (TObject)
    private
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;
      fDisplay: ALLEGRO_DISPLAYptr;
      fTimer: ALLEGRO_TIMERptr;

      fUserInput: TUserInput;

      fAsteroidManager: TAsteroidManager;
      fEffectManager: TEffectManager;

      procedure CreateDisplay;
    public
    (* Constructor. *)
      constructor Create;
    (* Destructor. *)
      destructor Destroy; override;
    (* Initializes the game. *)
      procedure Initialize;
    (* The game loop. *)
      procedure Run;

    (* Access to user input. *)
      property Input: TUserInput read fUserInput;
    (* Access to asteroid manager. *)
      property Asteroids: TAsteroidManager read fAsteroidManager;
    (* Access to effects manager. *)
      property Effects: TEffectManager read fEffectManager;
    end;

  var
  (* Global reference to the game manager. *)
    Game: TGameManager;

implementation

  uses
    Player,
    Engine, Graphics,
    al5font,
    sysutils;

(*
 * TGameManager
 ***************************************************************************)

(* Creates the display.

   Right now it just creates a window. *)
  procedure TGameManager.CreateDisplay;
  begin
    if fDisplay <> Nil then al_destroy_display (fDisplay);
    fDisplay := al_create_display (MAX_WIDTH, MAX_HEIGHT);
    if fDisplay = Nil then
      RAISE Exception.Create ('Failed to create display.');
    al_set_window_title (fDisplay, 'Pascaleroids - Allegro game demo');
    al_register_event_source (fEventQueue, al_get_display_event_source (fDisplay))
  end;



(* Constructor. *)
  constructor TGameManager.Create;
  begin
    if Game <> Nil then
      RAISE Exception.Create ('Only one game manager per game!');
    inherited Create;
    fUserInput := TKeyboardInput.Create;
    fAsteroidManager := TAsteroidManager.Create;
    fEffectManager := TEffectManager.Create;
    Game := Self
  end;



(* Destructor. *)
  destructor TGameManager.Destroy;
  begin
    fEffectManager.Free;
    fAsteroidManager.Free;
    fUserInput.Free;
  { Actually, most of these destructions aren't necessary as Allegro cleans them
    when terminating the application, but it is good to be polite. }
    if fDisplay <> Nil then al_destroy_display (fDisplay);

    if fEventQueue <> Nil then al_destroy_event_queue (fEventQueue);
    al_uninstall_system;
    inherited Destroy
  end;



(* Initializes the game. *)
  procedure TGameManager.Initialize;
  begin
  { Initialize Allegro. }
    if not al_init then
      RAISE Exception.Create ('Can''t initialize Allegro.');
    fEventQueue := al_create_event_queue;
    Graphics.Initialize;
    Self.CreateDisplay;
    al_install_keyboard;
    al_register_event_source (fEventQueue, al_get_keyboard_event_source);
    fTimer := al_create_timer (ALLEGRO_BPS_TO_SECS (FPS));
    al_register_event_source (fEventQueue, al_get_timer_event_source (fTimer));
    Randomize;
  end;



(* Execution. *)
  procedure TGameManager.Run;
  var
    Event: ALLEGRO_EVENT;
    lPlayerShip: TShip;
    lFont: ALLEGRO_FONTptr;
  begin
    lPlayerShip := TShip.Create;
try
    lFont := al_create_builtin_font;
    fAsteroidManager.Initialize;
    fEffectManager.Initialize;
    fUserInput.Initialize;
    lPlayerShip.Initialize;
    fAsteroidManager.NewGame;
    fEffectManager.NewGame;
    al_start_timer (fTimer);
    repeat
      al_wait_for_event (fEventQueue, Event);
      if not fUserInput.ProcessEvent (Event) then
      case Event.ftype of
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        Exit;
      ALLEGRO_EVENT_KEY_CHAR:
        case Event.keyboard.keycode of
        ALLEGRO_KEY_ESCAPE:
          Exit;
        ALLEGRO_KEY_SPACE:
          begin
            fAsteroidManager.NewBoard (Random (NUM_LARGE));
            fEffectManager.AddAsteroidExplosion (MAX_WIDTH div 2, MAX_HEIGHT div 2)
          end;
        end;
      ALLEGRO_EVENT_TIMER:
        begin
          if (Input.Direction and DIR_LEFT) <> 0 then
            lPlayerShip.RotateLeft;
          if (Input.Direction and DIR_RIGHT) <> 0 then
            lPlayerShip.RotateRight;
          if (Input.Direction and DIR_UP) <> 0 then
            lPlayerShip.Thrust;
          fAsteroidManager.Update;
          fEffectManager.Update;
          lPlayerShip.Update;
        end;
      end;
    { Draw and wait. }
      if al_is_event_queue_empty (fEventQueue) then
      begin
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
      end
    until False
finally
  lPlayerShip.Free;
end
  end;

end.
