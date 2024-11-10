unit Engine;
(* Implements a basic game engine.

   It doesn't include sprites:  they're in their own unit.
   But includes a few util functions.
 *)
(*
  Copyright (c) 2024 Guillermo Martínez J.

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
    Allegro5, al5font;

  const
  (* Display size. *)
    DisplayWidth = 800; DisplayHeight = 600;
  (* Game speed in frames-per-second. *)
    FPS = 50;

  type
    TGame = class; { Forwarded. }

  (* Graphics context. *)
    TDisplay = class (TObject)
    private
      fDisplay: ALLEGRO_DISPLAYptr;
      fTitle: AnsiString;
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;
      fCloseButtonClicked: Boolean;

    (* Change window title. *)
      procedure SetTitle (const aTitle: AnsiString); inline;
    (* Process event queue. *)
      procedure ProcessEvents;
    public
    (* Constructor. *)
      constructor Create;
    (* Destructor. *)
      destructor Destroy; override;
    (* Initialize the graphics context. *)
      function Initialize: Boolean;
    (* Copy or update the front and back buffers so that what has been drawn
       previously on the currently selected display becomes visible on screen.
     *)
      procedure FlipDisplay;
    (* Close the graphics context. *)
      procedure Close;
    (* Return True if user clicked in the close button, False otherwise. *)
      function UserClickedClose: Boolean;

    (* Window title. *)
      property Title: AnsiString read fTitle write SetTitle;
    end;



  (* Base class for player input. *)
    TPlayerInput = class (TObject)
    public
    (* Constructor.  It calls Clear. *)
      constructor Create; virtual;
    (* Get input state.

      - aX The X axis state (<0 left, >0 right).
      - aY The Y axis state (<0 down, >0 up).
      - aB The Button state (<>0 triggered). *)
      procedure GetState (out aX, aY, aB: Integer); virtual abstract;
    (* Clear input state. *)
      procedure Clear; virtual abstract;
    end;



  (* Player input using keyboard. *)
    TKeyboardInput = class (TPlayerInput)
    private
      fOldButton: Integer;
    public
    (* Get input state.

      - aX The X axis state (<0 left, >0 right).
      - aY The Y axis state (<0 down, >0 up).
      - aB The Button state (<>0 triggered). *)
      procedure GetState (out aX, aY, aB: Integer); override;
    (* Clear input state. *)
      procedure Clear; override;
    end;



  (* Base class for game scenes (screens, rooms...) *)
    TGameScene = class (TObject)
    private
      fOwner: TGame;
    public
    (* Initialize the scene.

       Called by @link(TGame) when game chages to this scene. *)
      procedure Enter; virtual;
    (* Update the scene.

       @link(TGame) will call this @link(FPS) times per second. *)
      procedure Update; virtual; abstract;
    (* Draw the scene.  Called by @link(TGame) when needed. *)
      procedure Draw; virtual; abstract;
    (* Close the scene.

       Called by @link(TGame) when game changes to another scene. *)
      procedure Leave; virtual;

    (* Reference to the game. *)
      property Game: TGame read fOwner;
    end;



  (* Base class for the game object. *)
    TGame = class (TObject)
    private
      fDisplay: TDisplay;
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;
      fTimer: ALLEGRO_TIMERptr;
      fCurrentScene, fNextScene: TGameScene;
      fTerminated: Boolean;
      fTextFont: ALLEGRO_FONTptr;
    public
    (* Constructor. *)
      constructor Create; virtual;
    (* Destructor. *)
      destructor Destroy; override;
    (* Initialize the engine.

       Returns True on success or False on failure. *)
      function Initialize: Boolean; virtual;
    (* Run the game.  Note that Scene should be assigned. *)
      procedure Run;
    (* Tell the game to terminate. *)
      procedure Terminate;

    (* Access to the display. *)
      property Display: TDisplay read fDisplay;
    (* Game scene that is running. *)
      property Scene: TGameScene read fCurrentScene write fNextScene;
    (* Tells if should terminate the game. *)
      property Terminated: Boolean read fTerminated;
    (* Default text font. *)
      property TextFont: ALLEGRO_FONTptr read fTextFont;
    end;

(* Some random generators. *)
  function RandomBetween (aMin, aMax: Integer): Integer; overload;
  function RandomBetween (aMin, aMax: Double): Double; overload;

  var
  (* Global reference to the game object. *)
    GameObject: TGame;

implementation

  uses
    al5strings,
    sysutils;

  function RandomBetween (aMin, aMax: Integer): Integer; inline;
  begin
    Result := Random (aMax - aMin) + aMin
  end;



  function RandomBetween (aMin, aMax: Double): Double;
  begin
    Result := RandomBetween (Trunc (aMin * 100), Trunc (aMax * 100)) / 100
  end;



(*
 * TDisplay
 ************************************************************************)

  procedure TDisplay.SetTitle (const aTitle: AnsiString);
  begin
    fTitle := aTitle;
  { Set title only if display exists. }
    if Assigned (fDisplay) then
      al_set_window_title (fDisplay, al_string_to_str (fTitle))
  end;



  procedure TDisplay.ProcessEvents;
  var
    lEvent: ALLEGRO_EVENT;
  begin
    while al_get_next_event (fEventQueue, lEvent) do
      if lEvent.ftype = ALLEGRO_EVENT_DISPLAY_CLOSE then
        fCloseButtonClicked := True
  end;



  constructor TDisplay.Create;
  begin
    inherited Create;
    fDisplay := Nil;
    fTitle := ''
  end;



  destructor TDisplay.Destroy;
  begin
    Self.Close;
    inherited Destroy
  end;



  function TDisplay.Initialize: Boolean;
  begin
  { Be sure there's no other context opened. }
    Self.Close;
  { At the moment, windowed mode only. }
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    if fTitle <> '' then al_set_new_window_title (fTitle);
  { Create the display. }
    fDisplay := al_create_display (DisplayWidth, DisplayHeight);
    if Assigned (fDisplay) then
    begin
      fCloseButtonClicked := False;
      fEventQueue := al_create_event_queue;
      if Assigned (fEventQueue) then
      begin
      { Register the display in to the queue. }
        al_register_event_source (
          fEventQueue,
          al_get_display_event_source (fDisplay)
        );
        Exit (True)
      end
    end;
  { Something failed. }
    Result := False
  end;



  procedure TDisplay.FlipDisplay;
  begin
    al_flip_display
  end;



  procedure TDisplay.Close;
  begin
    if Assigned (fDisplay) then
    begin
      al_destroy_display (fDisplay);
      fDisplay := Nil
    end;
    if Assigned (fEventQueue) then
    begin
      al_destroy_event_queue (fEventQueue);
      fEventQueue := Nil
    end
  end;



  function TDisplay.UserClickedClose: Boolean;
  begin
    Self.ProcessEvents;
    Result := fCloseButtonClicked
  end;



(*
 * TPlayerInput
 ************************************************************************)

  constructor TPlayerInput.Create;
  begin
    inherited Create;
    Self.Clear
  end;



(*
 * TKeyboardInput
 ************************************************************************)

  procedure TKeyboardInput.GetState (out aX, aY, aB: Integer);
  var
    lKeyStatus: ALLEGRO_KEYBOARD_STATE;
    lButton: Integer;
  begin
  { Note this would be not the most efficent way. }
    al_get_keyboard_state (lKeyStatus);
    if al_key_down (lKeyStatus, ALLEGRO_KEY_LEFT) then aX := -1 else aX := 0;
    if al_key_down (lKeyStatus, ALLEGRO_KEY_RIGHT) then Inc (ax);
    if al_key_down (lKeyStatus, ALLEGRO_KEY_DOWN) then aY := -1 else aY := 0;
    if al_key_down (lKeyStatus, ALLEGRO_KEY_UP) then Inc (aY);
  { Return "1" only and only if it was pressed in this frame. }
    if al_key_down (lKeyStatus, ALLEGRO_KEY_SPACE) then
      lButton := 1
    else
      lButton := 0;
    if lButton <> fOldButton then aB := lButton else aB := 0;
    fOldButton := lButton
  end;



  procedure TKeyboardInput.Clear;
  begin
    fOldButton := 0
  end;



(*
 * TGameScene
 ************************************************************************)

  procedure TGameScene.Enter; begin end;



  procedure TGameScene.Leave; begin end;



(*
 * TGame
 ************************************************************************)

  constructor TGame.Create;
  begin
    inherited Create;
    fDisplay := TDisplay.Create;
    GameObject := Self
  end;



  destructor TGame.Destroy;
  begin
    if Assigned (fTextFont) then al_destroy_font (fTextFont);
    al_destroy_event_queue (fEventQueue);
    al_destroy_timer (fTimer);
    fDisplay.Free;
    GameObject := Nil;
    inherited Destroy
  end;



  function TGame.Initialize: Boolean;

    procedure RegisterEvents;
    begin
      al_register_event_source (fEventQueue, al_get_keyboard_event_source);
      al_register_event_source (fEventQueue, al_get_timer_event_source (fTimer))
    end;

    function InitializeAllegroSubsystems: Boolean;
    begin
      if al_init then
      begin
        fEventQueue := al_create_event_queue;
        if Assigned (fEventQueue) then
        begin
          al_install_keyboard;
          al_init_font_addon;
          fTimer := al_create_timer (ALLEGRO_BPS_TO_SECS (FPS));
          if Assigned (fTimer) then
          begin
            RegisterEvents;
            Exit (True)
          end
        end
      end;
      Result := False
    end;

  begin
    if InitializeAllegroSubsystems then
      if fDisplay.Initialize then
      begin
        fTextFont := al_create_builtin_font;
        fTerminated := False;
        Exit (True)
      end;
    Result := False
  end;



  procedure TGame.Run;
  var
    lEvent: ALLEGRO_EVENT;

    procedure ShowError (aError: Exception);

      procedure CoolPrint (
        aColor: ALLEGRO_COLOR;
        aX, aY: Integer;
        aText: String
      );
      begin
        al_draw_text (
          fTextFont, al_map_rgb (0, 0, 0),
          aX + 1, aY + 1, 0,
          al_string_to_str (aText)
        );
        al_draw_text (fTextFont, aColor, aX, aY, 0, al_string_to_str (aText))
      end;

    var
      lExit: Boolean;
      lMatrix: ALLEGRO_TRANSFORM;
    begin
      al_identity_transform (lMatrix);
      al_use_transform (lMatrix);
      CoolPrint (al_map_rgb (255, 255, 255), 0, 0, 'An error occurred.');
      CoolPrint (al_map_rgb (255, 0, 0), 0, 8, aError.Message);
      CoolPrint (al_map_rgb (255, 255, 255), 0, 16, 'Press [Esc] to terminate');
      fDisplay.FlipDisplay;
      lExit := False;
      repeat
        if al_get_next_event (fEventQueue, lEvent) then
          if (lEvent.ftype = ALLEGRO_EVENT_KEY_DOWN)
          and (lEvent.keyboard.keycode = ALLEGRO_KEY_ESCAPE)
          then
            lExit := True
      until lExit or fDisplay.UserClickedClose
    end;

  begin
    if not Assigned (fCurrentScene) and not Assigned (fNextScene) then
      raise Exception.Create ('No scene assigned to Game!');
    try try
    { Initialize the game loop. }
      fTerminated := False;
      while al_drop_next_event (fEventQueue) do { Empty event queue. } ;
      al_start_timer (fTimer);
      repeat
      { Should change scene? }
        if Assigned (fNextScene) then
        begin
          if Assigned (fCurrentScene) then fCurrentScene.Leave;
          fCurrentScene := fNextScene;
          fCurrentScene.fOwner := Self;
          fCurrentScene.Enter;
          fNextScene := Nil
        end;
      { Update. }
        repeat
          al_wait_for_event (fEventQueue, @lEvent);
          if lEvent.ftype = ALLEGRO_EVENT_TIMER then fCurrentScene.Update
        until al_is_event_queue_empty (fEventQueue);
      { Render. }
        fCurrentScene.Draw;
        fDisplay.FlipDisplay
      until fTerminated;
    finally
      al_stop_timer (fTimer);
      if Assigned (fCurrentScene) then fCurrentScene.Leave
    end
    except
      on Error: Exception do ShowError (Error);
    end
  end;



  procedure TGame.Terminate;
  begin
    fTerminated := True
  end;

end.
