UNIT Application;
(*<Implements a generic "Application" class.

  This is much like Delphi's and Lazarus' TApplication class but adapted to
  Allegro and the game engine. *)
(*
  Copyright (c) 2010-2015 Guillermo Martínez J.

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
    Allegro,
    Classes, sysutils;

  TYPE
  (* The prototype for the exception handling events in
    @link(TCustomGameApplication).
    @seealso(TCustomGameApplication.HandleException)
    @seealso(TCustomGameApplication.onException) *)
    TmngExceptionEvent = PROCEDURE (Sender: TObject; E: Exception) OF OBJECT;

  (* The base class for game application objects. It introduces several
    application-wide functionalities, including exception handling,
    initialization and command-line parsing.

    Descendent classes need to override the @code(DoRun) method to implement the
    functionality of the program.
    @seealso(TCustomGameApplication.HandleException)
    @seealso(TCustomGameApplication.ShowException)
    @seealso(TCustomGameApplication.onException)
    @seealso(TCustomGameApplication.StopOnException)
    @seealso(TCustomGameApplication.GetOptionValue)
    @seealso(TCustomGameApplication.HasOption)
    @seealso(TCustomGameApplication.Run)
    @seealso(TCustomGameApplication.DoRun) *)
    TCustomGameApplication = CLASS (TObject)
    PRIVATE
      fWindowTitle, fDataPath: STRING;
      fStopOnException, fTerminated: BOOLEAN;

      fHasSound, fHasMIDI: BOOLEAN;
      fTicksPerSecond, fTick: INTEGER;
      fBackBuffer: AL_BITMAPptr;
      fUseVsync: BOOLEAN;

      fOnException: TmngExceptionEvent;
      fOnCloseButtonPressed: TNotifyEvent;

      FUNCTION IndexCommandOption (Opt: STRING; CONST Short: BOOLEAN): INTEGER;
      PROCEDURE SetTitle (CONST aTitle: STRING);
      PROCEDURE SetDataPath (CONST aPath: STRING);
      PROCEDURE SetTicksPerSecond (CONST TPS: INTEGER);
    PROTECTED
    (* Defines the default exception handler used by @link(Run).
      By default it calls @code(ShowException).
      @seealso(ShowException) @seealso(onException) *)
      PROCEDURE HandleException (aException: Exception); VIRTUAL;
    (* Does actually execute the program.  It's called by method @code(Run).
      @seealso(TCustomGameApplication.Run) *)
      PROCEDURE DoRun; VIRTUAL; ABSTRACT;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes the application.

       @code(Initialize) can be overridden by descendent classes to perform
       aditional initialization after the class was created.  End-user code
       should call @code(Initialize) prior to calling @code(Run).

       In @code(TmngCustomApplication), @code(Initialize) installs Allegro,
       inits keyboard, timer, sound and input and sets @code(Terminated) to
       @false.
       @seealso(Run) @seealso(Terminated) *)
      PROCEDURE Initialize; VIRTUAL;
    (* Initializes graphics mode.

      This method closes the previous graphics mode, if any.
      @param(aBpp Bits per pixel.)
      @param(aW Window width in pixels.) @param(aH Window height in pixels.)
      @param(aFS If @true it creates a full-screen display, if @false it
	creates a desktop window.)
      @seealso(WindowTitle) @seealso(CloseGraphicMode) *)
      PROCEDURE SetGraphicMode (CONST aBpp, aW, aH: INTEGER; CONST aFS: BOOLEAN);
    (* Creates the backbuffer used by the double-buffer system.  This must be
      called before you can use the @code(ActivePage).

      Size of backbuffer doesn't need to be the same than the output window.
      If it isn't, @code(SwapPages) will scale it to fit the output window.
      Note that scaling and color conversion are slower than use a backbuffer
      that is same size and colour depth than output window.
      @param(aBpp Bits per pixel.)
      @param(aW Page width in pixels.) @param(aH Page height in pixels.)
      @seealso(TSetGraphicMode) @seealso(SwapPages)
      @seealso(ActivePage) *)
      PROCEDURE CreateBackbuffer (CONST aBpp, aW, aH: INTEGER);
    (* Swaps pages, showing the active page.  It also decreases @link(Ticks).
      @seealso(CreateBackbuffer) @seealso(ActivePage) *)
      PROCEDURE SwapPages;
    (* Closes current graphics mode if any. @seealso(SetGraphicMode) *)
      PROCEDURE CloseGraphicMode;
    (* It is the start of the user code:  when called, it starts a loop and
      repeatedly calls @code(DoRun) until @code(Terminated) is set to @true.
      If an exception is raised during the execution of @code(DoRun), it is
      caught and handled to @code(HandleException).  If @code(StopOnException)
      is set to @true (which is @bold(not) the default), @code(Run) will exit,
      and the application will then terminate.  The default is to call
      @code(DoRun) again.
       @seealso(DoRun) @seealso(HandleException) @seealso(StopOnException)
       @seealso(Initialize) @seealso(Terminated) *)
      PROCEDURE Run;
    (* Shows an exception to the user.

       The default behaviour is to write the exception message in the
       @code(stderr) output stream.
       @param(E Exception object to show) *)
      PROCEDURE ShowException (E: Exception); VIRTUAL;
    (* Sets the @code(Terminated) property to @true.  By itself, this does not
       terminate the application.  Instead, the @code(Run) method checks the
       value of the @code(Terminated) property and properly shut down the game
       if it is set to @true. @seealso(Terminated) @seealso(Run) *)
      PROCEDURE Terminate; VIRTUAL;

    (* Returns @true if the specified option was given on the command line.
      Either the @code(Short) option character or the @code(Long) option may be
      used.  Note that both options (requiring a value) and switches can be
      specified.  You may assign @code(#0) to @code(Short) or @code('') to
      @code(Long) if you're not looking for such option.

      Options are identified as command-line parameters which start with the
      dash ('@code(-)') character.
      @param(Short Short version of the option.  Should be one character only.)
      @param(Long Long version of the option.) @seealso(GetOptionValue) *)
      FUNCTION HasOption (CONST Short: CHAR; CONST Long: STRING ): BOOLEAN;
	INLINE;
    (* Returns the value of an option.

      Values are specified in the usual GNU option format, either of
      @longcode(#--longopt=Value#)
      or
      @longcode(#-c Value#)
      is supported.
      @return(The specified value, or the empty string if none was specified.)
      @param(Short Short version of the option.  Should be one character only.)
      @param(Long Long version of the option.) @seealso(HasOption) *)
      FUNCTION GetOptionValue (CONST Short: CHAR; CONST Long: STRING): STRING;

    (* Text used as window title in windowed modes. *)
      PROPERTY WindowTitle: STRING READ fWindowTitle WRITE SetTitle;
    (* Controls the behaviour of the @code(Run) method in case of an unhandled
      exception in the code.  If it is @true then @link(Terminate) will be
      called after the exception was handled.

      By default it is @true. *)
      PROPERTY StopOnException: BOOLEAN
        READ fStopOnException WRITE fStopOnException;
    (* Was Terminate called or not. @seealso(Terminate) *)
      PROPERTY Terminated: BOOLEAN READ fTerminated;
    (* Returns data directory path.  By default it returns the operating system
      default (application directory on Windows, '/usr/share/games/<appname>'
      on Linux/UNIX), but it may be assigned wich helps to implement
      stuff as modding, extensions, etc.

      Assing an empty string to restore to the default path. *)
      PROPERTY DataPath: STRING READ fDataPath WRITE SetDataPath;
    (* System has support for digital sound. *)
      PROPERTY HasSound: BOOLEAN READ fHasSound;
    (* System has support for MIDI music. *)
      PROPERTY HasMIDI: BOOLEAN READ fHasMIDI;
    (* Sets/gets the game speed in ticks-per-second. *)
      PROPERTY Speed: INTEGER READ fTicksPerSecond WRITE SetTicksPerSecond;
    (* How many ticks since last update. @seealso(Speed) *)
      PROPERTY Ticks: INTEGER READ fTick WRITE fTick;
    (* Active page.  @seealso(CreateBackbuffer) @seealso(SwapPages) *)
      PROPERTY ActivePage: AL_BITMAPptr READ fBackBuffer;
    (* If set to @true, @link(SwapPages) will try to use Vsync method.
      Default is @false. *)
      PROPERTY UseVsync: BOOLEAN READ fUseVsync WRITE fUseVsync;

    (* Exception handling event.

      @code(onException) can be set to provide custom handling of events,
      instead of the default action, which is simply to show the event using
      @code(ShowException).

      If set, @code(onException) is called by the @code(HandleException)
      routine.  Do not use the @code(onException) event directly, instead call
      @code(HandleException). @seealso(HandleException) *)
      PROPERTY onException: TmngExceptionEvent
	READ fOnException WRITE fOnException;
    (* Close button handling event.

       @code(OnCloseButtonPressed) can be set to provide custom handling of the
       window close button, instead of the default action, which is simply to
       call the @code(Terminate) method.  @seealso(Terminate) *)
       PROPERTY OnCloseButtonPressed: TNotifyEvent
         READ fOnCloseButtonPressed WRITE fOnCloseButtonPressed;
    END;

  CONST
  (* Default game speed. *)
    FPS = 50;

  VAR
  (* Public access to the game object.  It's assigned by
    @link(TCustomGameApplication.Create) and destroyed at
    @code(FINALIZATION). *)
    GameApplication: TCustomGameApplication;

IMPLEMENTATION

(*
 * TCustomGameApplication
 ****************************************************************************)

(* Timer interruption. *)
  PROCEDURE TickInt; CDECL;
  BEGIN
    IF GameApplication <> NIL THEN INC (GameApplication.fTick)
  END;



(* Allegro's handler for "close window button". *)
  PROCEDURE HandleCloseWindowButton; CDECL;
  BEGIN
    IF Assigned (GameApplication.fOnCloseButtonPressed) THEN
      GameApplication.fOnCloseButtonPressed (GameApplication)
    ELSE
      GameApplication.Terminate
  END;



  FUNCTION TCustomGameApplication.IndexCommandOption
    (Opt: STRING; CONST Short: BOOLEAN): INTEGER;
  VAR
    Comand: STRING;
    Cnt, P: INTEGER;
  BEGIN
    IF ParamCount > 0 THEN
    BEGIN
      Opt := Uppercase (Trim (Opt));
      IF Short THEN Opt := '-' + Opt ELSE Opt := '--' + Opt;
      FOR Cnt := 1 TO ParamCount DO
      BEGIN
	Comand := Uppercase (ParamStr (Cnt));
	IF Comand [1] = '-' THEN
	BEGIN
	  P := Pos ('=', Comand);
	  IF (P <> 0) THEN Comand := Copy (Comand, 1, P - 1);
	  IF Comand = Opt THEN EXIT (Cnt)
	END
      END
    END;
    RESULT := -1
  END;



  PROCEDURE TCustomGameApplication.SetTitle (CONST aTitle: STRING);
  BEGIN
    fWindowTitle := aTitle;
    IF al_screen <> NIL THEN al_set_window_title (fWindowTitle)
  END;



  PROCEDURE TCustomGameApplication.SetDataPath (CONST aPath: STRING);
  BEGIN
    IF Trim (aPath) = '' THEN
      fDataPath := IncludeTrailingPathDelimiter (
{$ifdef win32}
      { On windows systems, data are in the same directory than executable. }
	ExtractFilePath (ParamStr (0))
{$else}
      { We will asume POSIX-like. }
	'/usr/share/games/'+ChangeFileExt (
	  ExtractFileName (ParamStr (0)),
	  ''
	)
{$endif}
      )
    ELSE
      fDataPath := IncludeTrailingPathDelimiter (Trim (aPath))
  END;



  PROCEDURE TCustomGameApplication.SetTicksPerSecond (CONST TPS: INTEGER);
  BEGIN
    IF TPS <> fTicksPerSecond THEN
    BEGIN
      IF TPS > 0 THEN
      BEGIN
	IF NOT al_install_int_ex (@TickInt, AL_BPS_TO_TIMER (TPS)) THEN
	  RAISE Exception.Create ('Can''t install timer!!!')
      END
      ELSE
	al_remove_int (@TickInt);
      fTicksPerSecond := TPS;
      fTick := 0
    END
  END;



(* Defines the default exception handler used by Run. *)
  PROCEDURE TCustomGameApplication.HandleException (aException: Exception);
  BEGIN
    IF Assigned (fOnException) THEN
      fOnException (SELF, aException)
    ELSE BEGIN
      SELF.ShowException (aException);
      IF StopOnException THEN SELF.Terminate
    END
  END;



(* Constructor. *)
  CONSTRUCTOR TCustomGameApplication.Create;
  BEGIN
    INHERITED Create;
    GameApplication := SELF;
  { Set up defaults. }
    fWindowTitle := ApplicationName;
    SetDataPath ('');
    fStopOnException := TRUE;
    fHasSound := FALSE; fHasMIDI := FALSE;
    fUseVsync := TRUE
  END;



(* Destructor. *)
  DESTRUCTOR TCustomGameApplication.Destroy;
  BEGIN
    IF fBackBuffer <> NIL THEN al_destroy_bitmap (fBackBuffer);
    al_exit;
    INHERITED Destroy;
    GameApplication := NIL
  END;



(* Initializes the application. *)
  PROCEDURE TCustomGameApplication.Initialize;
  BEGIN
    fTerminated := FALSE;
    IF al_init THEN
    BEGIN
    { Init systems. }
      al_install_timer;
      al_install_keyboard;
      fHasMIDI := TRUE; fHasSound := TRUE;
      IF NOT al_install_sound (AL_DIGI_AUTODETECT, AL_MIDI_AUTODETECT) THEN
      BEGIN
	fHasMIDI := FALSE;
	IF NOT al_install_sound (AL_DIGI_AUTODETECT, AL_MIDI_NONE) THEN
	  fHasSound := FALSE
      END;
    { More timer. }
      SELF.SetTicksPerSecond (FPS)
    END
    ELSE
      RAISE Exception.Create ('Can''t init Allegro.  Did you install it?')
  END;



(* Initializes graphics mode. *)
  PROCEDURE TCustomGameApplication.SetGraphicMode
    (CONST aBpp, aW, aH: INTEGER; CONST aFS: BOOLEAN);
  VAR
    Driver: INTEGER;
  BEGIN
    IF aFS THEN
      Driver := AL_GFX_AUTODETECT_FULLSCREEN
    ELSE
      Driver := AL_GFX_AUTODETECT_WINDOWED;
    al_set_color_depth (aBpp);
    IF NOT al_set_gfx_mode (Driver, aW, aH, 0, 0) THEN
      RAISE Exception.Create ('NO GFX MODE.');
    al_set_window_title (fWindowTitle);
    al_set_close_button_callback (@HandleCloseWindowButton);
  END;



(* Creates the backbuffer used by the double-buffer system. *)
  PROCEDURE TCustomGameApplication.CreateBackbuffer
    (CONST aBpp, aW, aH: INTEGER);
  BEGIN
    IF fBackBuffer <> NIL THEN al_destroy_bitmap (fBackBuffer);
    fBackBuffer := al_create_bitmap_ex (aBpp, aW, aH);
    IF fBackBuffer = NIL THEN
      RAISE Exception.Create ('Can''t initializa double buffer system.')
  END;



(* Swaps pages, showing the active page. *)
  PROCEDURE TCustomGameApplication.SwapPages;
  BEGIN
    IF fUseVsync THEN al_vsync;
    IF (AL_SCREEN_W <> fBackBuffer^.w) OR (AL_SCREEN_H <> fBackBuffer^.h) THEN
      al_stretch_blit (
	fBackBuffer, AL_SCREEN,
	0, 0, fBackBuffer^.w, fBackBuffer^.h,
	0, 0, AL_SCREEN_W, AL_SCREEN_H
      )
    ELSE
      al_blit (fBackBuffer, AL_SCREEN, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
  { Next frame. }
    DEC (SELF.fTick)
  END;



(* Closes current graphics mode. *)
  PROCEDURE TCustomGameApplication.CloseGraphicMode;
  BEGIN
    al_set_close_button_callback (NIL);
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0)
  END;



(* Executes the application. *)
  PROCEDURE TCustomGameApplication.Run;
  BEGIN
    REPEAT
      TRY
	DoRun
      EXCEPT
	ON E: Exception DO HandleException (E)
      END
    UNTIL fTerminated
  END;



(* Shows an exception to the user. *)
  PROCEDURE TCustomGameApplication.ShowException (E: Exception);
  BEGIN
  { TODO: Check if we are on graphics mode, and if Allegro is up and running. }
    sysutils.ShowException (E, ExceptAddr)
  END;



(* Terminates the game. *)
  PROCEDURE TCustomGameApplication.Terminate;
  BEGIN
    fTerminated := TRUE
  END;



(* Returns TRUE if the specified option was given on the command line. *)
  FUNCTION TCustomGameApplication.HasOption
    (CONST Short: CHAR; CONST Long: STRING ): BOOLEAN;
  BEGIN
    RESULT := (SELF.IndexCommandOption (Short, TRUE) > 0)
           OR (SELF.IndexCommandOption (Long, FALSE) > 0)
  END;



(* Returns the value of an option. *)
  FUNCTION TCustomGameApplication.GetOptionValue
    (CONST Short: CHAR; CONST Long: STRING): STRING;
  VAR
    Ndx, P: INTEGER;
  BEGIN
    Ndx := SELF.IndexCommandOption (Short, TRUE);
    IF Ndx > 0 THEN EXIT (Trim (ParamStr (Ndx + 1)));
    Ndx := SELF.IndexCommandOption (Long, FALSE);
    IF Ndx > 0 THEN
    BEGIN
      P := Pos ('=', ParamStr (Ndx));
      EXIT (Trim (Copy (ParamStr (Ndx), P + 1, Length (ParamStr (Ndx)))));
    END;
    RESULT := ''
  END;

INITIALIZATION
  GameApplication := NIL
FINALIZATION
  FreeAndNil (GameApplication)
END.
