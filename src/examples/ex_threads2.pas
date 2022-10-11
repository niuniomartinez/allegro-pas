program ex_threads;
(*    Example program for the Allegro library.
 *
 *    In this example, threads render to their own memory buffers, while the
 *    main thread handles events and drawing (copying from the memory buffers
 *    to the display).
 *
 * Implementation note:
 *    This example should use the Allegro's thread API.  Unfortunatelly there's
 *    something that doesn't work as expected (I suspect it is the MUTEX
 *    subsystem) so I've rewritten the example using RTL TThread class and
 *    critical section API.
 *
 *    It works (even better than the original C version!) but with window sizes
 *    bigger than current, there's a delay in the main thread.  The longer it
 *    runs, the bigger the delay.
 *)
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

{$MODE DELPHI} { To be sure classes and TThread are available. }
  uses
{$ifdef unix}cthreads,{$endif}
    Common,
    Allegro5, al5nativedlg,
    Classes, sysutils;

  const
    FPS = 20;
    MaxThreads = 9;
    ThreadsPerRow = 3;
    MaxIterations = 255;
  { This size generates a noticeable delay in the main thread.
    MandelSize = 200. }
    MandelSize = 150;
    DisplayWidth = MandelSize * ThreadsPerRow;
    DisplayHeight = MandelSize * (MaxThreads div ThreadsPerRow);

  type
  (* Imaginary number representation. *)
    Imaginary = record r, i: Double end;



  (* Thread that renders a Mandelbrot section in a bitmap. *)
    TMandelbrotRenderer = class (TThread)
    private
    (* Initial zoom. *)
      const InitialZoom = 1;
    (* Describes the colors used to render data. *)
      type TPalette = array [0..MaxIterations] of ALLEGRO_COLOR;
    var
      fIdentifier: Integer;
    (* For critical sections. *)
      fBmpCriticalSection: TRTLCriticalSection;
      fBmpLock: ALLEGRO_LOCKED_REGIONptr;
    (* Used by Random. *)
      fRndSeed: Integer;
    (* Bitmap where the Mandelbrot portion will be rendered. *)
      fBmp: ALLEGRO_BITMAPptr;
    (* Palette used to draw the Mandelbrot fractal. *)
      fColorPalette: TPalette;
    (* Central point of the portion to render. *)
      fCenter: Imaginary;
    (* Zoom.  That is, radius from the center to the limits of the Mandelbrot
       portion to be rendered. *)
      fZoom: Double;

    (* Thread-safe pseudo-random number generator. *)
      function Random: Integer;
    (* Generates a monochromatic palette from a random color. *)
      procedure GeneratePalette;

    (* Initializes the data needed by the thread. *)
      procedure Initialize;
    (* Releases data needed by the thread. *)
      procedure Finalize;

    (* Calculates Mandelbrot orbit for given point. *)
      function CalculateMandelPoint (const aPoint: Imaginary): Integer;
    (* Renders a Mandelbrot portion. *)
      procedure RenderMandelbrot (const lMin, lMax: Imaginary);
    protected
    (* Execute method. *)
      procedure Execute; override;
    public
    (* Constructor. *)
      constructor Create (const aId: Integer);
    (* Destructor. *)
      destructor Destroy; override;
    (* Tells to the thread that will start manipulating the otuput bitmap
       (reading or writing). *)
      procedure BeginDrawing; inline;
    (* Tells to the thread that output bitmap manipulation ended. *)
      procedure EndDrawing; inline;

    (* Changes center point. *)
      procedure SetCenter (aX, aY: Double);

    (* Access to the output bitmap. *)
      property Bmp: ALLEGRO_BITMAPptr read fBmp;
    end;



(*
 * TMandelbrotRenderer
 *************************************************************************)

(* Thread-safe pseudo-random number generator. *)
  function TMandelbrotRenderer.Random: Integer;
  const
    LocalRandMax = $FFFF;
  begin
    fRndSeed := Integer ((fRndSeed + 1) * 1103515245 + 12345);
    Result := (fRndSeed shr 16) and LocalRandMax
  end;



(* Generates a monochromatic palette from a random color. *)
  procedure TMandelbrotRenderer.GeneratePalette;

    function RandomValue: Byte; inline;
    begin
      Result := 128 + Self.Random mod 128
    end;

  var
    rMax, gMax, bMax: Byte;
    Ndx: Integer;
  begin
  { Get color. }
    rMax := RandomValue;
    gMax := RandomValue;
    bMax := RandomValue;
  { Create palette. }
    for Ndx := Low (fColorPalette) to High (fColorPalette) do
      fColorPalette[Ndx] := al_map_rgb (
        rMax * Ndx div 256,
        gMax * Ndx div 256,
        bMax * Ndx div 256
      )
  end;



(* Sets up the thread information.  It doesn't executes the thread. *)
  procedure TMandelbrotRenderer.Initialize;
  begin
    fRndSeed := fIdentifier;
  { Output bitmap. }
    al_set_new_bitmap_flags (
      ALLEGRO_MEMORY_BITMAP or ALLEGRO_NO_PRESERVE_TEXTURE
    );
    fBmp := al_create_bitmap (MandelSize, MandelSize);
    if not Assigned (fBmp) then
      LogPrintLn ('Can''t create bitmap for thread %d.', [fIdentifier]);
  { What to render. }
    fCenter.r := 0; fCenter.i := 0;
    fZoom := InitialZoom
  end;



(* Releases resources. *)
  procedure TMandelbrotRenderer.Finalize;
  begin
    if Assigned (fBmp) then
    begin
      al_destroy_bitmap (fBmp);
      fBmp := Nil
    end
  end;



(* Calculates Mandelbrot orbit for given point. *)
  function TMandelbrotRenderer.CalculateMandelPoint (const aPoint: Imaginary)
    : Integer;
  const
    Z_MAX2 = 4.0;
  var
    z, z1: Imaginary;
    lIterations: Integer;
  begin
    z := aPoint;
    for lIterations := 1 to MaxIterations do
    begin
      z1.r := (z.r * z.r - z.i * z.i) + aPoint.r;
      z1.i := (2 * z.r * z.i) + aPoint.i;
      if (z1.r * z1.r + z1.i * z1.i) > Z_MAX2 then
        Exit (lIterations);  { outside set }
      z := z1
    end;
    Result := 0 { inside set }
  end;



(* Renders a Mandelbrot portion in the given bitmap. *)
  procedure TMandelbrotRenderer.RenderMandelbrot (const lMin, lMax: Imaginary);
  type
    TLine = array of Integer;
  var
  (* Distance between points. *)
    lDistance: Double;
  (* Current point to be rendered. *)
    lX, lY: Integer;
    lCurrentPoint: Imaginary;
    lW, lH: Integer;
    lLine: TLine;
  begin
    if Assigned (fBmp) then
    begin
      Self.BeginDrawing;
      try
        lW := al_get_bitmap_width (fBmp);
        lH := al_get_bitmap_height (fBmp)
      finally
        Self.EndDrawing
      end;
    { Reserve space for a line. }
      lLine := Default (TLine); SetLength (lLine, lW);
    { Calculate distance between points. }
      lDistance := (lMax.r - lMin.r) / lW;
    { First point. }
      lCurrentPoint := lMin;
    { Line. }
      lY := 0;
      repeat
        lX := 0;
        lCurrentPoint.r := lMin.r;
        repeat
          lLine[lX] := CalculateMandelPoint (lCurrentPoint);
          lCurrentPoint.r := lCurrentPoint.r + lDistance;
          Inc (lX)
        until lX >= lW;
      { Render line. }
        Self.BeginDrawing;
        try
          for lX := lW - 1 downto 0 do
            al_put_pixel (lX, lY, fColorPalette[lLine[lX]])
        finally
          Self.EndDrawing;
          al_rest (0)
        end;
      { Next line. }
        lCurrentPoint.i := lCurrentPoint.i + lDistance;
        Inc (lY)
      { Until the last line or termination was requested. }
      until Self.Terminated or (lY >= lH)
    end
  end;



(* Execute method. *)
  procedure TMandelbrotRenderer.Execute;
  var
  (* Limits of the portion to be rendered. *)
    lMin, lMax: Imaginary;
  begin
    try
      al_set_target_bitmap (fBmp);
      repeat
        { if not fPaused then }
        begin
        { Calculate limits of the portion. }
          lMin.r := fCenter.r - fZoom;
          lMin.i := fCenter.i - fZoom;
          lMax.r := fCenter.r + fZoom;
          lMax.i := fCenter.i + fZoom;
        { Generate a new color palette. }
          GeneratePalette;
        { Do the render. }
          RenderMandelbrot (lMin, lMax);
        { Next frame. }
          fZoom := fZoom * 0.99
        end;
        al_rest (0)
      until Self.Terminated
    finally
      Self.Finalize
    end
  end;



(* Constructor. *)
  constructor TMandelbrotRenderer.Create (const aId: Integer);
  begin
    inherited Create (True);
    Self.FreeOnterminate := False;
    fIdentifier := aId;
    InitCriticalSection (fBmpCriticalSection);
    fBmpLock := Nil
  end;



(* Destructor. *)
  destructor TMandelbrotRenderer.Destroy;
  begin
    DoneCriticalSection (fBmpCriticalSection);
    inherited Destroy
  end;



(* Manipulating bitmap. *)
  procedure TMandelbrotRenderer.BeginDrawing;
  begin
    EnterCriticalSection (fBmpCriticalSection);
    fBmpLock := al_lock_bitmap (
      fBmp,
      al_get_bitmap_format (fBmp),
      ALLEGRO_LOCK_READWRITE
    )
  end;

  procedure TMandelbrotRenderer.EndDrawing;
  begin
    al_unlock_bitmap (fBmp);
    fBmpLock := Nil;
    LeaveCriticalSection (fBmpCriticalSection)
  end;



(* Changes center. *)
  procedure TMandelbrotRenderer.SetCenter (aX, aY: Double);
  begin
    fCenter.r := aX; fCenter.i := aY
  end;




(*
 * Main thread.
 *************************************************************************)

  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Display: ALLEGRO_DISPLAYptr;
    Timer: ALLEGRO_TIMERptr;
    Threads: array [0..MaxThreads - 1] of TMandelbrotRenderer;

(* Initializes the program. *)
  procedure InitializeExample;

    function CreateDisplay: Boolean;
    begin
      al_set_new_display_flags (ALLEGRO_WINDOWED { or ALLEGRO_GENERATE_EXPOSE_EVENTS });
      Display := al_create_display (DisplayWidth, DisplayHeight);
      Result := Assigned (Display);
      if Result then
        al_register_event_source (EventQueue, al_get_display_event_source (Display))
    end;

  var
    Cnt: Integer;
  begin
    LogWriteLn ('Initializing...');
    if not al_init then AbortExample ('Could not init Allegro.');
  { Initializes and displays a log window for debugging purposes. }
    OpenLog;
  { Configures Allegro. }
    EventQueue := al_create_event_queue;
    al_install_keyboard;
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    if not CreateDisplay then AbortExample ('Could not create display.');
  { Helper functions from common.pas. }
    InitPlatformSpecific;
  { Timer. }
    Timer := al_create_timer (ALLEGRO_BPS_TO_SECS (FPS));
    al_register_event_source (EventQueue, al_get_timer_event_source (Timer));

    LogWriteLn ('Allegro is up and running.');
  { Create threads. }
    LogWriteLn ('Creating threads...');
    for Cnt := Low (Threads) to High (Threads) do
    begin
      Threads[Cnt] := TMandelbrotRenderer.Create (Cnt);
      Threads[Cnt].Initialize
    end;
  { Change this if you change MaxThreads. }
    Threads[0].SetCenter (-0.56062033041600878303, -0.56064322926933807256);
    Threads[1].SetCenter (-0.57798076669230014080, -0.63449861991138123418);
    Threads[2].SetCenter ( 0.36676836392830602929, -0.59081385302214906030);
    Threads[3].SetCenter (-1.48319283039401317303, -0.00000000200514696273);
    Threads[4].SetCenter (-0.74052910500707636032,  0.18340899525730713915);
    Threads[5].SetCenter ( 0.25437906525768350097, -0.00046678223345789554);
    Threads[6].SetCenter (-0.56062033041600878303,  0.56064322926933807256);
    Threads[7].SetCenter (-0.57798076669230014080,  0.63449861991138123418);
    Threads[8].SetCenter ( 0.36676836392830602929,  0.59081385302214906030)
  end;



(* Releases all used resources. *)
  procedure FinalizeExample;
  var
    lThread: TMandelbrotRenderer;
  begin
  { Terminate threads. }
    LogWriteLn ('Closing threads...');
    for lThread in Threads do
    begin
    { Terminate only if threads are running. }
      if not lThread.Suspended then
      begin
	lThread.Terminate;
	lThread.WaitFor
      end;
      lThread.Free
    end;
  { Releasing other resources. }
    LogWriteLn ('Closing example...');
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Display) then al_destroy_display (Display);
    LogWriteLn ('Done.');
    CloseLog (True)
  end;



(* Executes the example. *)
  procedure RunExample;
  var
    EndProgram, UpdateDisplay: Boolean;
    Event: ALLEGRO_EVENT;

    lx, ly: Integer;
    lThread: TMandelbrotRenderer;

  (* Copies thread output to display. *)
    procedure GetOutput;
    begin
      al_set_target_backbuffer (Display);
      al_clear_to_color (al_map_rgba (0, 0, 0, 1));
      lx := 0; ly := 0;
      for lThread in Threads do
      begin
        lThread.BeginDrawing;
        try
          al_draw_bitmap (lThread.Bmp, lx, ly, 0)
        finally
          lThread.EndDrawing;
          al_rest (0)
        end;
        Inc (lx, MandelSize);
        if lx >= DisplayWidth then
        begin
          lx := 0;
          Inc (ly, MandelSize)
        end
      end;
    { Updated. }
      al_flip_display;
      UpdateDisplay := False
    end;

  begin
    EndProgram := False;
    UpdateDisplay := False;
  { Start example. }
    LogWriteLn ('Running...');
    al_start_timer (Timer);
    try
    { Start threads. }
      for lThread in Threads do lThread.Start;
    { Main loop. }
      repeat
        if UpdateDisplay then GetOutput;
      { Events. }
        repeat
          al_wait_for_event (EventQueue, @Event);
          case Event.ftype of
          ALLEGRO_EVENT_DISPLAY_CLOSE:
            EndProgram := True;
          ALLEGRO_EVENT_KEY_DOWN:
            if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
              EndProgram := True;
          ALLEGRO_EVENT_TIMER:
            UpdateDisplay := True;
          end
        until al_is_event_queue_empty (EventQueue)
      until EndProgram;
    except
      on Error: Exception do LogPrintLn ('[Error] %s', [Error.Message]);
    end
  end;

begin
  if al_show_native_message_box (
    Display,
    'ex_threads',
    'Warning',
    'This example will generate fast blinking colors.  That would be harming for some people.' +
    #10#10'Do you want to continue?',
    '',
    ALLEGRO_MESSAGEBOX_WARN or ALLEGRO_MESSAGEBOX_YES_NO
  ) = 1 then
  begin
    InitializeExample;
    RunExample;
    FinalizeExample
  end
end.

