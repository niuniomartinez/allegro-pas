unit Engine;
(* Implements a basic game engine.

   It doesn't include sprites:  they're in their own unit. *)
(*
  Copyright (c) 2022 Guillermo Martínez J.

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
    Allegro5;

  const
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

    (* Changes window title. *)
      procedure SetTitle (const aTitle: AnsiString); inline;
    (* Process event queue. *)
      procedure ProcessEvents;
    public
    (* Constructor. *)
      constructor Create;
    (* Destructor. *)
      destructor Destroy; override;
    (* Initializes the graphics context. *)
      function Initialize: Boolean;
    (* Copies or updates the front and back buffers so that what has been drawn
       previously on the currently selected display becomes visible on screen.
     *)
      procedure FlipDisplay;
    (* Closes the graphics context. *)
      procedure Close;
    (* Returns True if user clicked in the close button, False otherwise. *)
      function UserClickedClose: Boolean;

    (* Window title. *)
      property Title: AnsiString read fTitle write SetTitle;
    end;



  (* Base class for game scenes (screens, rooms...) *)
    TGameScene = class (TObject)
    private
      fOwner: TGame;
    public
    (* Initializes the scene. *)
      procedure Initialize; virtual;
    (* Updates the scene.

       @link(TGame) will call this @link(FPS) times per second. *)
      procedure Update; virtual; abstract;
    (* Draws the scene. *)
      procedure Draw; virtual; abstract;
    (* Closes the scene. *)
      procedure Finalize; virtual;

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
    public
    (* Constructor. *)
      constructor Create; virtual;
    (* Destructor. *)
      destructor Destroy; override;
    (* Initializes the engine.

       Returns True on success or False on failure. *)
      function Initialize: Boolean; virtual;
    (* Runs the game.  Note that Scene should be assigned. *)
      procedure Run;
    (* Tells the game to terminate. *)
      procedure Terminate;

    (* Access to the display. *)
      property Display: TDisplay read fDisplay;
    (* Game scene that is running. *)
      property Scene: TGameScene read fCurrentScene write fNextScene;
    (* Tells if should terminate the game. *)
      property Terminated: Boolean read fTerminated;
    end;

implementation

  uses
    al5strings,
    sysutils;

(*
 * TDisplay
 ************************************************************************)

(* Changes window title. *)
  procedure TDisplay.SetTitle (const aTitle: AnsiString);
  begin
    fTitle := aTitle;
  { Set title only if display exists. }
    if Assigned (fDisplay) then
      al_set_window_title (fDisplay, al_string_to_str (fTitle))
  end;



(* Process event queue. *)
  procedure TDisplay.ProcessEvents;
  var
    lEvent: ALLEGRO_EVENT;
  begin
    while al_get_next_event (fEventQueue, lEvent) do
      if lEvent.ftype = ALLEGRO_EVENT_DISPLAY_CLOSE then
        fCloseButtonClicked := True
  end;



(* Constructor. *)
  constructor TDisplay.Create;
  begin
    inherited Create;
    fDisplay := Nil;
    fTitle := ''
  end;



(* Destructor. *)
  destructor TDisplay.Destroy;
  begin
    Self.Close;
    inherited Destroy
  end;



(* Initializes the graphics context. *)
  function TDisplay.Initialize: Boolean;
  const
  { Window size. }
    WindowWidth = 800; WindowHeight = 600;
  begin
  { Be sure there's no other context opened. }
    Self.Close;
  { At the moment, windowed mode only. }
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    if fTitle <> '' then al_set_new_window_title (fTitle);
  { Create the display. }
    fDisplay := al_create_display (WindowWidth, WindowHeight);
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
  { Something's wrong. }
    Result := False
  end;



(* Flips display pages. *)
  procedure TDisplay.FlipDisplay;
  begin
    al_flip_display
  end;



(* Closes the graphics context. *)
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



(* Checks if user clicked in the close button. *)
  function TDisplay.UserClickedClose: Boolean;
  begin
    Self.ProcessEvents;
    Result := fCloseButtonClicked
  end;



(*
 * TGameScene
 ************************************************************************)

(* Initialization. *)
  procedure TGameScene.Initialize; begin end;



(* Finalization. *)
  procedure TGameScene.Finalize; begin end;



(*
 * TGame
 ************************************************************************)

(* Constructor. *)
  constructor TGame.Create;
  begin
    inherited Create;
    fDisplay := TDisplay.Create
  end;



(* Destructor. *)
  destructor TGame.Destroy;
  begin
    al_destroy_event_queue (fEventQueue);
    al_destroy_timer (fTimer);
    fDisplay.Free;
    inherited Destroy
  end;



(* Initializes. *)
  function TGame.Initialize: Boolean;

    procedure RegisterEvents;
    begin
      al_register_event_source (fEventQueue, al_get_timer_event_source (fTimer))
    end;

    function InitializeAllegroSubsystems: Boolean;
    begin
      if al_init then
      begin
        fEventQueue := al_create_event_queue;
        if Assigned (fEventQueue) then
        begin
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
        fTerminated := False;
        Exit (True)
      end;
    Result := False
  end;



(* Runs the game. *)
  procedure TGame.Run;
  var
    lEvent: ALLEGRO_EVENT;
  begin
    if not Assigned (fCurrentScene) and not Assigned (fNextScene) then
      raise Exception.Create ('No scene assigned to Game!');
    try
    { Initialize the game loop. }
      fTerminated := False;
      while al_drop_next_event (fEventQueue) do { Empty event queue. } ;
      al_start_timer (fTimer);
      repeat
      { Should change scene? }
        if Assigned (fNextScene) then
        begin
          if Assigned (fCurrentScene) then fCurrentScene.Finalize;
          fCurrentScene := fNextScene;
          fCurrentScene.fOwner := Self;
          fCurrentScene.Initialize
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
      if Assigned (fCurrentScene) then fCurrentScene.Finalize
    end
  end;



(* Terminates the game. *)
  procedure TGame.Terminate;
  begin
    fTerminated := True
  end;

end.
