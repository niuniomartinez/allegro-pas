program ex_threads;
(*    Example program for the Allegro library, by Peter Wang.
 *    Translated to Object Pascal by Guillermo Martínez.
 *
 *    In this example, each thread handles its own window and event queue.
 *
 * Implementation note:
 *    This example should use the Allegro's thread API.  Unfortunatelly there's
 *    something that doesn't work as expected (I suspect it is the MUTEX
 *    subsystem) so I've rewritten the example using RTL TThread class and
 *    critical section API.
 *)
(*
  Copyright (c) 2023-2024 Guillermo Martínez J.

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
{ Needed to support classes. }
  {$IFNDEF FPC_DELPHI}
    {$MODE DELPHI}
  {$ENDIF}
{ Windows manifest. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  uses
{$ifdef unix}cthreads,{$endif}
    Common,
    Allegro5, al5primitives, al5strings,
    Classes, sysutils;

  const
    MaxThreads     = 100;
    MaxBackgrounds = 10;
    MaxSquares     = 25;

  type
  (* Define background color range. *)
    TBackground = record rMax, gMax, bMax: Single end;
  (* Define a square. *)
    TSquare = record
      cx, cy,
      dx, dy,
      Size, dSize,
      Rot, dRot,
      Life, dLife: Single
    end;



  (* Thread.

     Each thread manages a different window.
   *)
    TExampleThread = class (TThread)
    private
      fId: Integer;
      fDisplay: ALLEGRO_DISPLAYptr;
      fQueue: ALLEGRO_EVENT_QUEUEptr;
      fTimer: ALLEGRO_TIMERptr;
      fSquares: array [1..MaxSquares] of TSquare;
      fBackground: TBackground;
      fTheta: Single;
    (* Used by Random. *)
      fRndSeed: Integer;

    (* Thread-safe pseudo-random number generator. *)
      function Random: Integer;
      function Rand01: Single;
      function Rand11: Single;
    (* Squares. *)
      function GenSquare (aw, ah: Integer): TSquare;
      procedure AnimateSquare (var aSquare: TSquare);
      procedure DrawSquare (const aSquare: TSquare);
    (* Example. *)
      procedure Render;
    protected
    (* Execution method. *)
      procedure Execute; override;
    public
    (* Constructor. *)
      constructor Create (const aId: Integer; const aBackground: TBackground);
    end;



(*
 * TExampleThread
 *************************************************************************)

  function TExampleThread.Random: Integer;
  const
    LocalRandMax = $FFFF;
  begin
    fRndSeed := Integer ((fRndSeed + 1) * 1103515245 + 12345);
    Result := (fRndSeed shr 16) and LocalRandMax
  end;



  function TExampleThread.Rand01: Single;
  begin
    Result := (Self.Random mod 10000) / 10000
  end;


  function TExampleThread.Rand11: Single;
  begin
    Result := (-10000 + (Self.Random mod 20000)) / 20000
  end;



  function TExampleThread.GenSquare (aw, ah: Integer): TSquare;
  begin
    Result.cx := Self.Random mod aw;
    Result.cy := Self.Random mod ah;
    Result.dx := 3 * Self.Rand11;
    Result.dy := 3 * Self.Rand11;
    Result.Size := 10 + (Self.Random mod 10);
    Result.dSize := Self.Rand11;
    Result.Rot := ALLEGRO_TAU * Self.Rand01 / 2;
    Result.dRot := Self.Rand11 / 3;
    Result.Life := 0;
    Result.dLife := (ALLEGRO_TAU / 200) + (ALLEGRO_TAU / 60) * Self.Rand01
  end;



  procedure TExampleThread.AnimateSquare (var aSquare: TSquare);
  var
    lBmp: ALLEGRO_BITMAPptr;
  begin
    aSquare.cx := aSquare.cx + aSquare.dx;
    aSquare.cy := aSquare.cy + aSquare.dy;
    aSquare.Size := aSquare.Size + aSquare.dSize;
    aSquare.Rot := aSquare.Rot + aSquare.dRot;
    aSquare.Life := aSquare.Life + aSquare.dLife;

    if (aSquare.Size < 1) or (aSquare.Life > ALLEGRO_TAU / 2) then
    begin
      lBmp := al_get_target_bitmap;
      aSquare := Self.GenSquare
                 (
                   al_get_bitmap_width (lBmp),
                   al_get_bitmap_height (lBmp)
                 )
    end
  end;



  procedure TExampleThread.DrawSquare (const aSquare: TSquare);
  var
    lTransform: ALLEGRO_TRANSFORM;
    lAlpha, lSize: Single;
    lTint: ALLEGRO_COLOR;
  begin
    al_build_transform (lTransform, aSquare.cx, aSquare.cy, 1, 1, aSquare.Rot);
    al_use_transform (lTransform);

    lAlpha := Sin (aSquare.Life);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_ONE);
    lTint := al_map_rgba_f (0.5, 0.3, 0, lAlpha);

    al_draw_filled_rounded_rectangle
    (
      -aSquare.Size, -aSquare.Size, aSquare.Size, aSquare.Size,
      3, 3,
      lTint
    );
    lSize := aSquare.Size * 1.1;
    al_draw_rounded_rectangle (-lSize, -lSize, lSize, lSize, 3, 3, lTint, 2)
  end;



  procedure TExampleThread.Render;
  var
    r: Double;
    lState: ALLEGRO_STATE;
    lSquare: TSquare;
  begin
    r := 0.7 + 0.3 * (Sin (fTheta) + 1) / 2;
    al_clear_to_color
    (
      al_map_rgb_f
      (
        fBackground.rMax * r,
        fBackground.gMax * r,
        fBackground.bMax * r
      )
    );
    al_store_state (lState, ALLEGRO_STATE_BLENDER or ALLEGRO_STATE_TRANSFORM);
    try
      for lSquare in fSquares do Self.DrawSquare (lSquare)
    finally
      al_restore_state (lState)
    end;
    al_flip_display
  end;



  procedure TExampleThread.Execute;

    function Init: Boolean;
    const
      InitialWidth = 300; InitialHeight = 300;
    var
      Ndx: Integer;
    begin
      al_set_new_display_flags (ALLEGRO_RESIZABLE);
      fDisplay := al_create_display (InitialWidth, InitialHeight);
      if not Assigned (fDisplay) then Exit (False);
      al_set_window_title (fDisplay, al_str_format ('Thread %d', [fId]));

      fQueue := al_create_event_queue;
      if not Assigned (fQueue) then Exit (False);

      fTimer := al_create_timer (0.1);
      if not Assigned (fTimer) then Exit (False);

      al_register_event_source (fQueue, al_get_display_event_source (fDisplay));
      al_register_event_source (fQueue, al_get_keyboard_event_source);
      al_register_event_source (fQueue, al_get_timer_event_source (fTimer));

      for Ndx := Low (fSquares) to High (fSquares) do
        fSquares[Ndx] := Self.GenSquare (InitialWidth, InitialHeight);
      Result := True
    end;

    procedure Finish;
    begin
      if Assigned (fQueue) then
      begin
      { Sometimes keyboard is 'glued' to the thread even after it is dead and
        main thread doesn't manage it properly.  So release keyboard to be sure
        it is not 'sticky'.
      }
        al_unregister_event_source (fQueue, al_get_keyboard_event_source);
        al_destroy_event_queue (fQueue)
      end;
      if Assigned (fTimer) then al_destroy_timer (fTimer);
      if Assigned (fDisplay) then al_destroy_display (fDisplay)
    end;

  var
    lRedraw: Boolean;
    lEvent: ALLEGRO_EVENT;
    lNdx: Integer;
  begin
    try
      if Init then
      begin
        lRedraw := True;
        al_start_timer (fTimer);
        repeat
          if al_is_event_queue_empty (fQueue) and lRedraw then
          begin
            Self.Render;
            lRedraw := False
          end;
          al_wait_for_event (fQueue, @lEvent);
          case lEvent.ftype of
          ALLEGRO_EVENT_DISPLAY_CLOSE:
            Self.Terminate;
          ALLEGRO_EVENT_DISPLAY_RESIZE:
            al_acknowledge_resize (lEvent.display.source);
          ALLEGRO_EVENT_KEY_DOWN:
            if lEvent.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
              Self.Terminate;
          ALLEGRO_EVENT_TIMER:
            begin
              for lNdx := Low (fSquares) to High (fSquares) do
                Self.AnimateSquare (fSquares[lNdx]);
              fTheta := fTheta + 0.1;
              lRedraw := True
            end;
          end;
        until Self.Terminated
      end
    finally
      Finish
    end
  end;



  constructor TExampleThread.Create (
    const aId: Integer;
    const aBackground: TBackground
  );
  begin
    inherited Create (True);
    fId := aId;
    fRndSeed := fId;
    fBackground := aBackground
  end;



(*
 * Main thread.
 *************************************************************************)

const
  Background: array [1..MaxBackgrounds] of TBackground =
  (
    ( rMax: 1.0; gMax: 0.5; bMax: 0.5),
    ( rMax: 0.5; gMax: 1.0; bMax: 0.5),
    ( rMax: 0.5; gMax: 0.5; bMax: 1.0),
    ( rMax: 1.0; gMax: 1.0; bMax: 0.5),
    ( rMax: 0.5; gMax: 1.0; bMax: 1.0),
    ( rMax: 1.0; gMax: 0.7; bMax: 0.5),
    ( rMax: 0.5; gMax: 1.0; bMax: 0.7),
    ( rMax: 0.7; gMax: 0.5; bMax: 1.0),
    ( rMax: 1.0; gMax: 0.7; bMax: 0.5),
    ( rMax: 0.5; gMax: 0.7; bMax: 1.0)
  );
var
  Threads: array [0..MaxThreads - 1] of TExampleThread;
  NumThreads, Ndx: Integer;



(* Initialize the program. *)
  function InitializeExample: Boolean;
  begin
    WriteLn ('Initializing...');
  { Check command line options. }
    if ParamCount > 0 then
      NumThreads := Clamp (1, StrToInt (ParamStr (1)), MaxThreads)
    else
      NumThreads := 3;
  { Initialize allegro. }
    if not al_init or not al_install_keyboard or not al_install_mouse
    or not al_init_primitives_addon
    then
    begin
      WriteLn ('Could not init Allegro.');
      Exit (False)
    end;
    WriteLn ('Allegro is up and running.');
  { Create threads. }
    WriteLn ('Creating threads...');
    for Ndx := 1 to NumThreads do
      Threads[Ndx] :=  TExampleThread.Create
                       (
                         Ndx,
                         Background[(Ndx mod MaxBackgrounds) + 1]
                       );
    Result := True
  end;



(* Release all used resources. *)
  procedure FinalizeExample;
  var
    lThread: TExampleThread;
  begin
  { Terminate threads. }
    WriteLn ('Closing threads...');
    for lThread in Threads do lThread.Free;
    WriteLn ('Done.')
  end;



(* Run example. *)
  procedure RunExample;
  var
    lThread: TExampleThread;
  begin
    WriteLn ('Starting threads...');
    for lThread in Threads do if Assigned (lThread) then lThread.Start;
  { Wait for threads to end. }
    WriteLn ('Waiting for threads ending...');
    for lThread in Threads do
    { Only for running threads. }
      if Assigned (lThread) and not lThread.Suspended then
        lThread.WaitFor
  end;

begin
  if InitializeExample then
    RunExample;
  FinalizeExample
end.
