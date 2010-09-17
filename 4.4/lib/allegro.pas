(*Allegro core.

  This is the main module of the Allegro library.  There are very different
  stuff on this unit, but procedures, functions, types, variables and constants
  are grouped to make it easer to find them.  Read the @bold(Introduction)
  section for a brief description of this unit. *)
UNIT allegro;

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$MODE FPC}
 {$LONGSTRINGS ON}
 {$SMARTLINK ON}
{$ENDIF}

INTERFACE

USES
  albase, alfixed, alvtable;



(***************
 * Core system *
 ***************
 * Defines a collection of procedures, identificators and variables that allows
 * to comunicate with the core system.  That is, error handling, initialization
 * and configuration of the base system. *)

CONST
(* Tells to @code(al_install) that must autodetect the system driver. *)
  AL_SYSTEM_AUTODETECT	= 0;
(* Tells to @code(al_install) that must install a stripped down version of
   Allegro that won't even try to touch the hardware or do anything platform
   specific:  this can be useful for situations where you only want to
   manipulate memory bitmaps, such as the text mode datafile tools or the
   Windows GDI interfacing functions.

   Is equivalent to @code(AL_ID @('NONE'@);). *)
  AL_SYSTEM_NONE	= $4E4F4E45;
(* Defined to the major version of Allegro.  From a version number like 4.1.16,
   this would be defined to the integer 4. *)
  AL_VERSION = 4;
(* Defined to the middle version of Allegro.  From a version number like
   4.1.16, this would be defined to the integer 1. *)
  AL_SUB_VERSION = 4;
(* Defined to the minor version of Allegro.pas.  From a version number like
   4.1.16, this would be defined to the integer 16. *)
  AL_PAS_VERSION = 2;
(* Defined to TRUE if current version is a BETA version.  A BETA version is a
   test version and may be uncomplete or untested. *)
  AL_PAS_IS_BETA = FALSE;
(* Defined to a text string containing all version numbers and maybe some
   additional text. *)
  AL_PAS_VERSION_STR = '4.4.2';



VAR
(* Stores the last error number. *)
  al_errno: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_errno';
(* Text string used by @code(al_set_gfx_mode), @code(al_install_sound) and
   other functions to report error messages.  If they fail and you want to tell
   the user why, this is the place to look for a description of the problem. *)
  al_error: STRING; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_error';
(* Text string containing a date and version number for the library, in case
   you want to display these somewhere. *)
  al_id_string: STRING; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_id';



(* This function can be used to create a packed 32 bit integer from 8 bit
   characters, on both 32 and 64 bit machines.  These can be used for various
   things, like custom datafile objects or system IDs. Example:

@longcode(#
VAR
  OSTYPE_LINUX: LONGINT;
BEGIN
  OSTYPE_LINUX := AL_ID('TUX ');
END;
  #) *)
  FUNCTION AL_ID (str: SHORTSTRING): LONGINT;



(* Initialises the Allegro library.  You must call either this or al_init
   before doing anything other.  The functions that can be called before this
   one will be marked explicitly in the documentation, like
   @code(al_set_config_file).

   The available system ID codes will vary from one platform to another, but
   you will almost always want to pass @code(AL_SYSTEM_AUTODETECT).
   Alternatively, @code(AL_SYSTEM_NONE) installs a stripped down version of
   Allegro that won't even try to touch your hardware or do anything platform
   specific:  this can be useful for situations where you only want to
   manipulate memory bitmaps, such as the text mode datafile tools or the
   Windows GDI interfacing functions.
   @param(system_id Identification of the system.)
   @returns(@true on success or @false on failure @(e.g. no system driver
     could be used@).) *)
  FUNCTION al_install (system_id: LONGINT): BOOLEAN;

(* Function which initialises the Allegro library.  This is the same thing as
   calling @code(al_install @(AL_SYSTEM_AUTODETECT@)).

   @returns(@true on success or @false on failure @(e.g. no system driver
     could be used@).) *)
  FUNCTION al_init: BOOLEAN;

(* Closes down the Allegro system.  This includes returning the system to text
   mode and removing whatever mouse, keyboard, and timer routines have been
   installed.  This procedure must be called before exit the program.

   Note that after you call this function, other functions like
   al_destroy_bitmap will most likely crash. *)
  PROCEDURE al_exit; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_exit';



(* Outputs a message.  Usually you want to use this to report messages to the
   user in an OS independant way when some Allegro subsystem cannot be
   initialised.  But you must not use this function if you are in a graphic
   mode, only before calling @code(al_set_gfx_mode), or after a
   @code(al_set_gfx_mode @(AL_GFX_TEXT@)).  Also, this function depends on a
   system driver being installed, which means that it won't display the message
   at all on some platforms if Allegro has not been initialised correctly.

   On platforms featuring a windowing system, it will bring up a blocking GUI
   message box.  If there is no windowing system, it will try to print the
   string to a text console, attempting to work around codepage differences by
   reducing any accented characters to 7-bit ASCII approximations. Example:

@longcode(#
  IF NOT al_init THEN
    EXIT (1);
  IF NOT init_my_data THEN
  BEGIN
    al_message ('Sorry, missing game data!\n');
    EXIT (2);
  END;
  #) *)
  PROCEDURE al_message (CONST msg: STRING);



(* On platforms that have a close button, this routine installs a callback
   function to handle the close event.  In other words, when the user clicks
   the close button on your program's window or any equivalent device, the
   function you specify here will be called.

   This function should not generally attempt to exit the program or save any
   data itself.  The function could be called at any time, and there is usually
   a risk of conflict with the main thread of the program.  Instead, you should
   set a flag during this function, and test it on a regular basis in the main
   loop of the program.

   Pass @nil as the `proc' argument to this function to disable the close
   button functionality, which is the default state.

   Example:

@longcode(#
VAR
  CloseButtonPressed: BOOLEAN = FALSE;

PROCEDURE CloseButtonHandler; CDECL;
BEGIN
  ClosePuttonPressed := TRUE;
END;

    ...
  al_init;
  al_set_close_button_callback (@CloseButtonHandler);
    ...
  WHILE NOT ClosePuttonPressed DO
    DoStuff;
  #)
   @returns(@true on success or @false on failure @(e.g. the feature is not
     supported by the platform@).) *)
  FUNCTION al_set_close_button_callback (proc: AL_SIMPLE_PROC): BOOLEAN;
    INLINE;



(* Finds out the currently selected desktop color depth.  You can use this
   information to make your program use the same color depth as the desktop,
   which will likely make it run faster because the graphic driver won't be
   doing unnecessary color conversions behind your back.

   Under some OSes, switching to a full screen graphics mode may automatically
   change the desktop color depth.  You have, therefore, to call this function
   before setting any graphics mode in order to retrieve the real desktop color
   depth.
   @returns(the color depth or zero on platforms where this information is not
   available or does not apply.) *)
  FUNCTION al_desktop_color_depth: LONGINT;
    INLINE;



(* Finds out the currently selected desktop resolution.  You can use this
   information to avoid creating windows bigger than the current resolution.
   This is especially important for some windowed drivers which are unable to
   create windows bigger than the desktop.  Each parameter is a pointer to an
   integer where one dimension of the screen will be stored.

   Under some OSes, switching to a full screen graphics mode may automatically
   change the desktop resolution.  You have, therefore, to call this function
   before setting any graphics mode in order to retrieve the real desktop
   resolution.
   @param(w Width desktop resolution.) @param(h Height desktop resolution.)
   @returns(@true on success, or @false if this information is not available or
   does not apply, in which case the values stored in the variables you
   provided for `width' and `height' are undefined.) *)
  FUNCTION al_get_desktop_resolution (VAR w, h: LONGINT): BOOLEAN;
    INLINE;


(* On platforms that are capable of it, this routine alters the window title
   for your Allegro program.
   @param(title Title string.) *)
  PROCEDURE al_set_window_title (CONST title: STRING);
    INLINE;




(*******************
 * UNICODE support *
 *******************)

VAR
(* Fixed size, 8-bit ASCII characters. @seealso(al_set_uformat). *)
  AL_U_ASCII: LONGINT;
(* Alternative 8-bit codepage. @seealso(al_set_ucodepage)
   @seealso(al_set_uformat). *)
  AL_U_ASCII_CP: LONGINT;
(* Fixed size, 16-bit Unicode characters. @seealso(al_set_uformat). *)
  AL_U_UNICODE: LONGINT;
(* Variable size, UTF-8 format Unicode characters. @seealso(al_set_uformat). *)
  AL_U_UTF8: LONGINT;
(* Current encoding. *)
  AL_U_CURRENT: LONGINT;



(* Sets the current text encoding format.  This will affect all parts of
   Allegro, wherever you see a function that returns a string, or takes a
   string as a parameter.

   Although you can change the text format on the fly, this is not a good
   idea.  Many strings, for example the names of your hardware drivers and any
   language translations, are loaded when you call @code(al_init), so if you
   change the encoding format after this, they will be in the wrong format, and
   things will not work properly.  Generally you should only call
   @code(al_set_uformat) once, before @code(al_init), and then leave it on the
   same setting for the duration of your program.

   @param(type Should be one of these values: @code(AL_U_ASCII),
     @code(AL_U_ASCII_CP), @code(AL_U_UNICODE), @code(AL_U_UTF8).) *)
  PROCEDURE al_set_uformat (aType: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_uformat';



(* Finds out what text encoding format is currently selected.  This function is
   probably useful only if you are writing an Allegro addon dealing with text
   strings and you use a different codepath for each possible format.

   @returns(The currently selected text encoding format.)
   @seealso(al_set_uformat) *)
  FUNCTION al_get_uformat: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_uformat';



(* When you select the @code(AL_U_ASCII_CP) encoding mode, a set of tables are
   used to convert between 8-bit characters and their Unicode equivalents.  You
   can use this function to specify a custom set of mapping tables, which
   allows you to support different 8-bit codepages.

   Allegro will use the @code(table) parameter when it needs to convert an
   ASCII string to an Unicode string.  But when Allegro converts an Unicode
   string to ASCII, it will use both parameters.  First, it will loop through
   the @code(table) parameter looking for an index position pointing at the
   Unicode value it is trying to convert (ie. the @code(table) parameter is
   also used for reverse matching).  If that fails, the @code(extras) list is
   used.  If that fails too, Allegro will put the character @code(`^'), giving
   up the conversion.

   Note that Allegro comes with a default parameters set internally.  The
   default @code(table) will convert 8-bit characters to @code(`^').  The
   default @code(extras) list reduces Latin-1 and Extended-A characters to 7
   bits in a sensible way (eg. an accented vowel will be reduced to the same
   vowel without the accent).
   @param(table Points to an array of 256 short integers, which contain the
     Unicode value for each character in your codepage.)
   @param(extras If not @nil, points to a list of mapping pairs, which will be
     used when reducing Unicode data to your codepage.  Each pair consists of a
     Unicode value, followed by the way it should be represented in your
     codepage.  The list is terminated by a zero Unicode value.  This allows
     you to create a many->one mapping, where many different Unicode characters
     can be represented by a single codepage value @(eg. for reducing accented
     vowels to 7-bit ASCII@).) *)
  PROCEDURE al_set_ucodepage (CONST table, extras: PWORD); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_ucodepage';



(* Returns the number of characters in the string.  Note that this doesn't have
   to equal the string's size in bytes. *)
  FUNCTION al_ustrlen (s: STRING): LONGINT;
    INLINE;



(**********************
 * Configuration file *
 **********************)

(* Sets the configuration file to be used by all subsequent config functions.
   (Allegro will not search for this file in other locations as it does with
   allegro.cfg at the time of initialization.)

   All pointers returned by previous calls to @code(al_get_config_string) and
   other related functions are invalidated when you call this function!  You
   can call this function before @code(al_install) to change the configuration
   file, but after @code(al_set_uformat) if you want to use a text encoding
   format other than the default. *)
  PROCEDURE al_set_config_file (filename: STRING);
    INLINE;

(* Specifies a block of data to be used by all subsequent config functions,
   which you have already loaded from disk (eg. as part of some more
   complicated format of your own, or in a grabber datafile).  This routine
   makes a copy of the information, so you can safely free the data after
   calling it. *)
  PROCEDURE al_set_config_data (data: POINTER; lng: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_data';

(* Specifies a file containing config overrides.  These settings will be used
   in addition to the parameters in the main config file, and where a variable
   is present in both files this version will take priority.  This can be used
   by application programmers to override some of the config settings from
   their code, while still leaving the main config file free for the end user
   to customise.  For example, you could specify a particular sample frequency
   and IBK instrument file, but the user could still use an @code(allegro.cfg)
   file to specify the port settings and irq numbers.

   The override config file will not only take precedence when reading, but
   will also be used for storing values.  When you are done with using the
   override config file, you can call @code(al_override_config_file) with a
   @nil parameter, so config data will be directly read from the current config
   file again.

   @bold(Note:) The override file is completely independent from the current
   configuration.  You can e.g. call @code(al_set_config_file), and the
   override file will still be active.  Also the @code(al_flush_config_file)
   function will only affect the current config file (which can be changed with
   @code(al_set_config_file)), never the overriding one specified with this
   function.  The modified override config is written back to disk whenever you
   call @code(al_override_config_file).

    Note that this function and @code(al_override_config_data) are mutually
    exclusive, i.e. calling one will cancel the effects of the other. *)
  PROCEDURE al_override_config_file (filename: STRING);
    INLINE;

(* Version of @code(al_override_config_file) which uses a block of data that
   has already been read into memory.  The length of the block has to be
   specified in bytes.

   Note that this function and @code(al_override_config_file) are mutually
   exclusive, i.e. calling one will cancel the effects of the other. *)
  PROCEDURE al_override_config_data (data: POINTER; lng: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'override_config_data';

(* Writes the current config file to disk if the contents have changed since
   it was loaded or since the latest call to the function. *)
  PROCEDURE al_flush_config_file; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'flush_config_file';

(* Pushes the current configuration state (filename, variable values, etc).
   onto an internal stack, allowing you to select some other config source and
   later restore the current settings by calling @code(al_pop_config_state).
   This function is mostly intended for internal use by other library
   functions, for example when you specify a config filename to the
   @code(al_save_joystick_data) function, it pushes the config state before
   switching to the file you specified. *)
  PROCEDURE al_push_config_state; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'push_config_state';

(* Pops a configuration state previously stored by @code(al_push_config_state),
   replacing the current config source with it. *)
  PROCEDURE al_pop_config_state; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pop_config_state';

(* Retrieves a string variable from the current config file.  The section name
   may be set to an empty string to read variables from the root of the file,
   or used to control which set of parameters (eg. sound or joystick) you are
   interested in reading. Example:
@longcode(#
VAR
  Lang: STRING;
  ...
  Lang := al_get_config_string ('system', 'language', 'EN');
  #)

   @returns(the string to the constant string found in the configuration file.
     If the named variable cannot be found, or its entry in the config file is
     empty, the value of @code(def) is returned.) *)
  FUNCTION al_get_config_string (section, name, def: STRING): STRING;
    INLINE;

(* Reads an integer variable from the current config file.  See the comments
   about @code(al_get_config_string). *)
  FUNCTION al_get_config_int (section, name: STRING; def: LONGINT): LONGINT;
    INLINE;

(* Reads an integer variable from the current config file, in hexadecimal.
   See the comments about @code(al_get_config_string). *)
  FUNCTION al_get_config_hex (section, name: STRING; def: LONGINT): LONGINT;
    INLINE;

(* Reads a floating point variable from the current config file.  See the
   comments about @code(al_get_config_string). *)
  FUNCTION al_get_config_float (section, name: STRING; def: SINGLE): SINGLE;
    INLINE;

(* Reads a 4-letter driver ID variable from the current config file.  See the
   comments about @code(al_get_config_string). *)
  FUNCTION al_get_config_id (section, name: STRING; def: LONGINT): LONGINT;
    INLINE;

(* AFAIK this doesn't work.
  FUNCTION al_get_config_argv (section, name: STRING; argc: PLONGINT): PSTRING; *)

(* Writes a string variable to the current config file, replacing any existing
   value it may have, or removes the variable if @code(val) is empty.  The
   section name may be set to a empty string to write the variable to the root
   of the file, or used to control which section the variable is inserted into.
   The altered file will be cached in memory, and not actually written to disk
   until you call @code(al_exit).  Note that you can only write to files in
   this way, so the function will have no effect if the current config source
   was specified with @code(al_set_config_data) rather than
   @code(al_set_config_file).

   As a special case, variable or section names that begin with a '#' character
   are treated specially and will not be read from or written to the disk.
   Addon packages can use this to store version info or other status
   information into the config module, from where it can be read with the
   @code(al_get_config_string) function. *)
  PROCEDURE al_set_config_string (section, name, val: STRING);
    INLINE;

(* Writes an integer variable to the current config file.  See the comments
   about @code(al_set_config_string). *)
  PROCEDURE al_set_config_int (section, name: STRING; val: LONGINT);
    INLINE;

(* Writes an integer variable to the current config file, in hexadecimal
   format.  See the comments about @code(al_set_config_string). *)
  PROCEDURE al_set_config_hex (section, name: STRING; val: LONGINT);
    INLINE;

(* Writes a floating point variable to the current config file.  See the
   comments about @code(al_set_config_string). *)
  PROCEDURE al_set_config_float (section, name:STRING; val: SINGLE);
    INLINE;

(* Writes a 4-letter driver ID variable to the current config file.  See the
   comments about @code(al_set_config_string). *)
  PROCEDURE al_set_config_id (section, name: STRING; val: LONGINT);
    INLINE;



(******************
 * Timer routines *
 ******************)

(* Give the number of seconds between each tick to @code(al_install_int_ex). *)
  FUNCTION AL_SECS_TO_TIMER (x: LONGINT): LONGINT; INLINE;
(* Give the number of milliseconds between each tick to
   @code(al_install_int_ex). *)
  FUNCTION AL_MSEC_TO_TIMER (x: LONGINT): LONGINT; INLINE;
(* Give the number of ticks each second to @code(al_install_int_ex). *)
  FUNCTION AL_BPS_TO_TIMER  (x: LONGINT): LONGINT; INLINE;
(* Give the number of ticks each minute to @code(al_install_int_ex). *)
  FUNCTION AL_BPM_TO_TIMER  (x: LONGINT): LONGINT; INLINE;



(* Installs the Allegro timer interrupt handler.  You must do this before
   installing any user timer routines, and also before displaying a mouse
   pointer and playing FLI animations or MIDI music.

   @returns(@true on success, or @false on failure @(but you may decide not to
     check the return value as this function is very unlikely to fail@).)  *)
  FUNCTION al_install_timer: BOOLEAN;

(* Removes the Allegro timer handler.  You don't normally need to bother
   calling this, because @code(al_exit) will do it for you. *)
  PROCEDURE al_remove_timer; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_timer';

(* Adds a function to the list of user timer handlers or, if it is already
   installed, retroactively adjusts its speed (i.e makes as though the speed
   change occurred precisely at the last tick).  The speed is given in hardware
   clock ticks, of which there are 1193181 a second. You can convert from other
   time formats to hardware clock ticks with the functions
   @code(AL_SECS_TO_TIMER), @code(AL_MSEC_TO_TIMER), @code(AL_BPS_TO_TIMER) and
   @code(AL_BPM_TO_TIMER).

   There can only be sixteen timers in use at a time, and some other parts of
   Allegro (the mouse pointer display routines, @code(al_rest), the FLI
   player, and the MIDI player) need to install handlers of their own, so you
   should avoid using too many at the same time.  If you call this routine
   without having first installed the timer module, @code(al_install_timer)
   will be called automatically.

   Your function will be called by the Allegro interrupt handler and not
   directly by the processor, so it can be a normal @code(CDECL) function.
   You should be aware, however, that it will be called in an interrupt
   context, which imposes a lot of restrictions on what you can do in it.  It
   should not use large amounts of stack, it must not make any calls to the
   operating system, use the run-time library functions, or contain any
   floating point code, and it must execute very quickly.  Don't try to do lots
   of complicated code in a timer handler:  as a general rule you should just
   set some flags and respond to these later in your main control loop.

   @returns(@true on success, or @false if there is no room to add a new user
     timer.) *)
  FUNCTION al_install_int_ex (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;
    INLINE;

(* Installs a user timer handler, with the speed given as the number of
   milliseconds between ticks.  This is the same thing as
   @code(al_install_int_ex @(@@proc, AL_MSEC_TO_TIMER @(speed@)@)).  If you call
   this routine without having first installed the timer module,
   @code(al_install_timer) will be called automatically.  Calling again this
   routine with the same timer handler as parameter allows you to adjust its speed.

   @returns(@true on success, or @false if there is no room to add a new user
     timer.) *)
  FUNCTION al_install_int (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;
    INLINE;

(* Removes a function from the list of user interrupt routines.
   @code(al_exit) does this automatically. *)
  PROCEDURE al_remove_int (proc: AL_SIMPLE_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_int';

(* Like @code(al_install_int_ex), but the callback routine will be passed a
   copy of the specified void pointer parameter.  To disable the handler, use
   @code(al_remove_param_int) instead of @code(al_remove_int). *)
  FUNCTION al_install_param_int_ex (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;
    INLINE;

(* Like @code(al_install_int), but the callback routine will be passed a copy
   of the specified void pointer parameter.  To disable the handler, use
   @code(al_remove_param_int) instead of @code(al_remove_int). *)
  FUNCTION al_install_param_int (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;
    INLINE;

(* Like @code(al_remove_int), but for use with timer callbacks that have
   parameter values.  If there is more than one copy of the same callback
   active at a time, it identifies which one to remove by checking the
   parameter value (so you can't have more than one copy of a handler using an
   identical parameter).  *)
  PROCEDURE al_remove_param_int (proc: AL_PARAM_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_param_int';

(* This function waits for the specified number of milliseconds.

    Passing 0 as parameter will not wait, but just yield.  This can be useful
    in order to @italic("play nice") with other processes.  Other values will
    cause CPU time to be dropped on most platforms.  This will look better to
    users, and also does things like saving battery power and making fans less
    noisy.

    Note that calling this inside your active game loop is a bad idea, as you
    never know when the OS will give you the CPU back, so you could end up
    missing the vertical retrace and skipping frames.  On the other hand, on
    multitasking operating systems it is good form to give up the CPU for a
    while if you will not be using it. *)
  PROCEDURE al_rest (time: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest';

(* Like @code(al_rest), but for non-zero values continually calls the
   specified function while it is waiting for the required time to elapse.  If
   the provided `callback' parameter is @nil, this function does exactly the
   same thing as calling @code(al_rest). *)
  PROCEDURE al_rest_callback (time: LONGINT; callback: AL_PARAM_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest_callback';



VAR
(* If the retrace simulator is installed, this count is incremented on each
   vertical retrace; otherwise, if the refresh rate is known, the count is
   incremented at the same rate (ignoring retraces); otherwise, it is
   incremented 70 times a second.  This provides a way of controlling the speed
   of your program without installing user timer functions. *)
  al_retrace_count: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'retrace_count';



(******************
 * Keyboard input *
 ******************)

(* Installs the Allegro keyboard interrupt handler.  You must call this before
   using any of the keyboard input routines.  Once you have set up the Allegro
   handler, you can no longer use operating system calls or the run-time
   library functions to access the keyboard.

   Note that on some platforms the keyboard won't work unless you have set a
   graphics mode, even if this function returns a success value before calling
   @code(al_set_gfx_mode).  This can happen in environments with graphic
   windowed modes, since Allegro usually reads the keyboard through the
   graphical window (which appears after the @code(al_set_gfx_mode) call).

   @returns(@true on success, or @false on failure @(but you may decide not to
     check the return value as this function is very unlikely to fail@).)  *)
  FUNCTION al_install_keyboard: BOOLEAN;

(* Removes the keyboard handler, returning control to the operating system.
   You don't normally need to bother calling this, because @code(al_exit) will
   do it for you.  However, you might want to call this during runtime if you
   want to change the keyboard mapping on those platforms were keyboard
   mappings are needed.  You would first modify the configuration variable
   holding the keyboard mapping and then reinstall the keyboard handler. *)
  PROCEDURE al_remove_keyboard;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_keyboard';

(* Wherever possible, Allegro will read the keyboard input asynchronously (ie.
   from inside an interrupt handler), but on some platforms that may not be
   possible, in which case you must call this routine at regular intervals to
   update the keyboard state variables.

   To help you test your keyboard polling code even if you are programming on
   a platform that doesn't require it, after the first time that you call this
   function Allegro will switch into polling mode, so from that point onwards
   you will have to call this routine in order to get any keyboard input at
   all, regardless of whether the current driver actually needs to be polled or
   not.

   The @code(al_keypressed), @code(al_readkey), and @code(al_ureadkey)
   functions call @code(al_poll_keyboard) automatically, so you only need to
   use this function when accessing the @code(al_key) array and
   @code(al_key_shifts) variable.

   @returns(zero on success or a negative on failure @(ie. no keyboard driver
     installed@).) *)
  FUNCTION al_poll_keyboard: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_keyboard';

(* Returns @true if the current keyboard driver is operating in polling mode. *)
  FUNCTION al_keyboard_needs_poll: BOOLEAN;
    INLINE;

(* Returns @true if there are keypresses waiting in the input buffer.  You can
   use this to see if the next call to @code(al_readkey) is going to block or
   to simply wait for the user to press a key while you still update the screen
   possibly drawing some animation.  Example:
   @longcode(#
  WHILE NOT al_keypressed DO
    AnimateLogo (al_screen);
   #) *)
  FUNCTION al_keypressed: BOOLEAN;
    INLINE;

(* Returns the next character from the keyboard buffer, in ASCII format.  If
   the buffer is empty, it waits until a key is pressed.  You can see if there
   are queued keypresses with @code(al_keypressed).

   The low byte of the return value contains the ASCII code of the key, and the
   high byte the scancode.  The scancode remains the same whatever the state of
   the shift, ctrl and alt keys, while the ASCII code is affected by shift and
   ctrl in the normal way (shift changes case, ctrl+letter gives the position
   of that letter in the alphabet, eg. ctrl+A = 1, ctrl+B = 2, etc).  Pressing
   alt+key returns only the scancode, with a zero ASCII code in the low byte.
   For example:
   @longcode(#
VAR
  val: INTEGER;
  ...
  val := al_readkey;

  IF (val AND $ff) = ORD ('d') THEN
    al_message ('You pressed "d"');

  IF (val SHR 8) = AL_KEY_SPACE THEN
    al_message ('You pressed Space');

  IF (val AND $ff) = 3 THEN
    al_message ('You pressed Control+C');

  IF val = (AL_KEY_X LSH 8) THEN
    al_message ('You pressed Alt+X');
   #)

   This function cannot return character values greater than 255.  If you need
   to read Unicode input, use @code(al_ureadkey) instead.
   @seealso(al_install_keyboard) @seealso(al_key) *)
  FUNCTION al_readkey: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'readkey';

(* Returns the next character from the keyboard buffer, in Unicode format.  If
   the buffer is empty, it waits until a key is pressed.  You can see if there
   are queued keypresses with @code(al_keypressed).  The return value contains
   the Unicode value of the key, and the argument will be set to the scancode.
   Unlike @code(al_readkey), this function is able to return character values
   greater than 255.  Example:
   @longcode(#
VAR
  val, scancode: INTEGER;
  ...
  val := al_ureadkey (scancode);
  IF val = $00F1 THEN
    al_message ('You pressed n with tilde');

  IF val = $00DF THEN
    al_message ('You pressed sharp s');
   #)

   You should be able to find Unicode character maps at
   http://www.unicode.org/. *)
  FUNCTION al_ureadkey (VAR scancode: LONGINT): LONGINT;
    INLINE;

(* Stuffs a key into the keyboard buffer, just as if the user had pressed it.
   The parameter is in the same format returned by @code(al_readkey). *)
  PROCEDURE al_simulate_keypress (keycode: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_keypress';

(* Stuffs a key into the keyboard buffer, just as if the user had pressed it.
   The parameter is in the same format returned by @code(al_ureadkey). *)
  PROCEDURE al_simulate_ukeypress (keycode, scancode: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_ukeypress';

(* Empties the keyboard buffer.  Usually you want to use this in your program
   before reading keys to avoid previously buffered keys to be returned by
   calls to @code(al_readkey) or @code(al_ureadkey). *)
  PROCEDURE al_clear_keybuf;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_keybuf';

(* Overrides the state of the keyboard LED indicators.  The parameter is a
   bitmask containing any of the values @code(AL_KB_SCROLOCK_FLAG),
   @code(AL_KB_NUMLOCK_FLAG), and @code(AL_KB_CAPSLOCK_FLAG), or -1 to restore
   the default behavior.

   Note that the led behaviour cannot be guaranteed on some platforms, some
   leds might not react, or none at all.  Therefore you shouldn't rely only on
   them to communicate information to the user, just in case it doesn't get
   through. *)
  PROCEDURE al_set_leds (leds: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_leds';

(* Sets the keyboard repeat rate.  Times are given in milliseconds.  Passing
   zero times will disable the key repeat. *)
  PROCEDURE al_set_keyboard_rate (key_delay, key_repeat: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_keyboard_rate';

(* Converts the given scancode to an ASCII character for that key (mangling
   Unicode values), returning the unshifted uncapslocked result of pressing the
   key, or zero if the key isn't a character-generating key or the lookup can't
   be done.  The lookup cannot be done for keys like the F1-F12 keys or the
   cursor keys, and some drivers will only return approximate values.
   Generally, if you want to display the name of a key to the user, you should
   use the @code(al_scancode_to_name) function. *)
  FUNCTION al_scancode_to_ascii (scancode: LONGINT): LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scancode_to_ascii';

(* This function returns a string pointer containing the name of they key with
   the given scancode.  This is useful if you e.g. let the user choose a key
   for some action, and want to display something more meaningful than just the
   scancode. *)
  FUNCTION al_scancode_to_name (scancode: LONGINT): STRING;
    INLINE;



(* Access to the Allegro's public variables. *)
TYPE
{ @exclude }
  AL_KEY_LIST	 = ARRAY [0..126] OF BYTE;



VAR
(* Array of flags indicating the state of each key, ordered by scancode.
   Wherever possible these values will be updated asynchronously, but if
   @code(al_keyboard_needs_poll) returns @true, you must manually call
   @code(al_poll_keyboard) to update them with the current input state.  The
   scancodes are defined as a series of @code(AL_KEY_* ) constants (and are
   also listed below). For example, you could write:
   @longcode(#
IF  al_key[AL_KEY_SPACE] <> 0 THEN
  WriteLn ('Space is pressed');
   #)

   Note that the array is supposed to represent which keys are physically held
   down and which keys are not, so it is semantically read-only.

   These are the keyboard scancodes:
   @longcode(#
          KEY_A ... KEY_Z,
          KEY_0 ... KEY_9,
          KEY_0_PAD ... KEY_9_PAD,
          KEY_F1 ... KEY_F12,

          KEY_ESC, KEY_TILDE, KEY_MINUS, KEY_EQUALS,
          KEY_BACKSPACE, KEY_TAB, KEY_OPENBRACE, KEY_CLOSEBRACE,
          KEY_ENTER, KEY_COLON, KEY_QUOTE, KEY_BACKSLASH,
          KEY_BACKSLASH2, KEY_COMMA, KEY_STOP, KEY_SLASH,
          KEY_SPACE,

          KEY_INSERT, KEY_DEL, KEY_HOME, KEY_END, KEY_PGUP,
          KEY_PGDN, KEY_LEFT, KEY_RIGHT, KEY_UP, KEY_DOWN,

          KEY_SLASH_PAD, KEY_ASTERISK, KEY_MINUS_PAD,
          KEY_PLUS_PAD, KEY_DEL_PAD, KEY_ENTER_PAD,

          KEY_PRTSCR, KEY_PAUSE,

          KEY_ABNT_C1, KEY_YEN, KEY_KANA, KEY_CONVERT, KEY_NOCONVERT,
          KEY_AT, KEY_CIRCUMFLEX, KEY_COLON2, KEY_KANJI,

          KEY_LSHIFT, KEY_RSHIFT,
          KEY_LCONTROL, KEY_RCONTROL,
          KEY_ALT, KEY_ALTGR,
          KEY_LWIN, KEY_RWIN, KEY_MENU,
          KEY_SCRLOCK, KEY_NUMLOCK, KEY_CAPSLOCK

          KEY_EQUALS_PAD, KEY_BACKQUOTE, KEY_SEMICOLON, KEY_COMMAND
   #)

   Finally, you may notice an @italic(`odd') behaviour of the
   @code(AL_KEY_PAUSE) key.  This key only generates an interrupt when it is
   pressed, not when it is released.  For this reason, Allegro pretends the
   pause key is a @italic(`state') key, which is the only way to make it
   usable. *)
  al_key: AL_KEY_LIST; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'key';

(* Bitmask containing the current state of @code(shift/ctrl/alt), the special
   Windows keys, and the accent escape characters.  Wherever possible this
   value will be updated asynchronously, but if @code(al_keyboard_needs_poll)
   returns @true, you must manually call @code(al_poll_keyboard) to update it
   with the current input state.  This can contain any of the flags:
   @longcode(#
          KB_SHIFT_FLAG
          KB_CTRL_FLAG
          KB_ALT_FLAG
          KB_LWIN_FLAG
          KB_RWIN_FLAG
          KB_MENU_FLAG
          KB_COMMAND_FLAG
          KB_SCROLOCK_FLAG
          KB_NUMLOCK_FLAG
          KB_CAPSLOCK_FLAG
          KB_INALTSEQ_FLAG
          KB_ACCENT1_FLAG
          KB_ACCENT2_FLAG
          KB_ACCENT3_FLAG
          KB_ACCENT4_FLAG
   #) *)
  al_key_shifts: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'key_shifts';

(* The DJGPP keyboard handler provides an 'emergency exit' sequence which you
   can use to kill off your program.  If you are running under DOS this is the
   three finger salute, ctrl+alt+del.  Most multitasking OS's will trap this
   combination before it reaches the Allegro handler, in which case you can use
   the alternative ctrl+alt+end.  If you want to disable this behaviour in
   release versions of your program, set this flag to @code(NOT 0). *)
  al_three_finger_flag: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'three_finger_flag';

(* By default, the capslock, numlock, and scroll-lock keys toggle the keyboard
   LED indicators when they are pressed.  If you are using these keys for input
   in your game (eg. capslock to fire) this may not be desirable, so you can
   set this flag to zero and prevent the LED's being updated. *)
  al_key_led_flag: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'key_led_flag';


(* If set, this function is called by the keyboard handler in response to every
   keyboard event, both presses (including keyboard repeat rate) and releases.
   It will be passed a raw keyboard scancode byte (scancodes are 7 bits long),
   with the top bit (8th bit) clear if the key has been pressed or set if it
   was released.  This routine executes in an interrupt context, so it must be
   used carefully and only if needed. *)
  al_keyboard_lowlevel_callback: AL_INT_PROC; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'keyboard_lowlevel_callback';



(* Key scan-code and flag identifiers. *)
  {$include alkeyid.inc}



(********************
 * Joystick support *
 ********************)

CONST
(* To be used at @code(al_install_joystick). *)
  AL_JOY_TYPE_AUTODETECT = -(1);
  AL_JOY_TYPE_NONE = 0;

(* Maximun number of elements. *)
  AL_MAX_JOYSTICKS = 8;
  AL_MAX_JOYSTICK_AXIS = 3;
  AL_MAX_JOYSTICK_STICKS = 5;
  AL_MAX_JOYSTICK_BUTTONS = 32;

(* joystick status flags. @seealso(AL_JOYSTICK_INFO) *)
  AL_JOYFLAG_DIGITAL = 1;
  AL_JOYFLAG_ANALOGUE = 2;
  AL_JOYFLAG_CALIB_DIGITAL = 4;
  AL_JOYFLAG_CALIB_ANALOGUE = 8;
  AL_JOYFLAG_CALIBRATE = 16;
  AL_JOYFLAG_SIGNED = 32;
  AL_JOYFLAG_UNSIGNED = 64;

  AL_JOYFLAG_ANALOG = AL_JOYFLAG_ANALOGUE;
  AL_JOYFLAG_CALIB_ANALOG = AL_JOYFLAG_CALIB_ANALOGUE;



TYPE
(* This provides both analogue input in the @code(pos) field (ranging from
   -128 to 128 or from 0 to 255, depending on the type of the control), and
   digital values in the @code(d1) and @code(d2) fields.  For example, when
   describing the X-axis position, the @code(pos) field will hold the
   horizontal position of the joystick, @code(d1) will be set if it is moved
   left, and @code(d2) will be set if it is moved right.  Allegro will fill in
   all these values regardless of whether it is using a digital or analogue
   joystick, emulating the pos field for digital inputs by snapping it to the
   min, middle, and maximum positions, and emulating the @code(d1) and
   @code(d2) values for an analogue stick by comparing the current position
   with the centre point. *)
  AL_JOYSTICK_AXIS_INFO = RECORD
    pos : LONGINT; {< analogue axis position. }
  { digital axis position. }
    d1 : LONGINT;
    d2 : LONGINT;
    name : PCHAR; {< description of this axis. }
  END;

(* information about one or more axis (a slider or directional control) *)
  AL_JOYSTICK_STICK_INFO = RECORD
    flags : LONGINT;{< status flags for this input. }
    num_axis : LONGINT; {< how many axes do we have? (note de misspelling). }
  { axis state information. @seealso(AL_JOYSTICK_AXIS_INFO) }
    axis : ARRAY [0..(AL_MAX_JOYSTICK_AXIS)-1] OF AL_JOYSTICK_AXIS_INFO;
    name : PCHAR; {< description of this input. }
  END;

(* information about a joystick button.

   You may wish to display the button names as part of an input configuration
   screen to let the user choose what game function will be performed by each
   button, but in simpler situations you can safely assume that the first two
   elements in the button array will always be the main trigger controls. *)
  AL_JOYSTICK_BUTTON_INFO = RECORD
    b : LONGINT; { 0 not pressed, (NOT 0) pressed. }
    name : PCHAR; { description of this button. }
  END;

(* Pointer to @code(AL_JOYSTICK_INFO). *)
  AL_JOYSTICK_INFOptr = ^AL_JOYSTICK_INFO;
(* information about an entire joystick.

   Each joystick will provide one or more stick inputs, of varying types.
   These can be digital controls which snap to specific positions (eg. a
   gamepad controller, the coolie hat on a Flightstick Pro or Wingman Extreme,
   or a normal joystick which hasn't yet been calibrated), or they can be full
   analogue inputs with a smooth range of motion.  Sticks may also have
   different numbers of axes, for example a normal directional control has two,
   but the Flightstick Pro throttle is only a single axis, and it is possible
   that the system could be extended in the future to support full 3d
   controllers.

   The joystick flags field may contain any combination of the bit flags:
   @definitionList(
     @itemLabel(AL_JOYFLAG_DIGITAL)
     @item(This control is currently providing digital input.)

     @itemLabel(AL_JOYFLAG_ANALOGUE)
     @item(This control is currently providing analogue input.)

     @itemLabel(AL_JOYFLAG_CALIB_DIGITAL)
     @item(This control will be capable of providing digital input once it has
       been calibrated, but is not doing this at the moment.)

     @itemLabel(AL_JOYFLAG_CALIB_ANALOGUE)
     @item(This control will be capable of providing analogue input once it
       has been calibrated, but is not doing this at the moment.)

     @itemLabel(AL_JOYFLAG_CALIBRATE)
     @item(Indicates that this control needs to be calibrated. Many devices
       require multiple calibration steps, so you should call the
       @code(al_calibrate_joystick) function from a loop until this flag is
       cleared.)

     @itemLabel(AL_JOYFLAG_SIGNED)
     @item(Indicates that the analogue axis position is in signed format,
       ranging from -128 to 128. This is the case for all 2d directional
       controls.)

     @itemLabel(AL_JOYFLAG_UNSIGNED)
     @item(Indicates that the analogue axis position is in unsigned format,
       ranging from 0 to 255. This is the case for all 1d throttle controls.)
   ) *)
  AL_JOYSTICK_INFO = RECORD
    flags : LONGINT; {< status flags for this joystick. }
    num_sticks : LONGINT; {<  how many stick inputs? }
    num_buttons : LONGINT; {< how many buttons? }
  { stick state information. @seealso(AL_JOYSTICK_STICK_INFO) }
    stick : ARRAY [0..(AL_MAX_JOYSTICK_STICKS)-1] OF AL_JOYSTICK_STICK_INFO;
  { button state information. @seealso(AL_JOYSTICK_BUTTON_INFO). }
    button : ARRAY [0..(AL_MAX_JOYSTICK_BUTTONS)-1] OF AL_JOYSTICK_BUTTON_INFO;
  END;

{ @exclude }
  AL_JOYSTICK_INFO_LIST = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_JOYSTICK_INFO;



VAR
(* Global array of joystick state information, which is updated by the
   @code(al_poll_joystick) function.  Only the first @code(al_num_joysticks)
   elements will contain meaningful information.

   @seealso(AL_JOYSTICK_INFO)

   A single joystick may provide several different stick inputs, but you can
   safely assume that the first element in the stick array will always be the
   main directional controller.

   Information about each of the stick axis is stored in the
   @code(AL_JOYSTICK_AXIS_INFO) substructure.

   Note for people who spell funny: in case you don't like having to type
   @italic (analogue), there are some aliases in this unit that will allow you
   to write @italic(analog) instead.  *)
  al_joy: AL_JOYSTICK_INFO_LIST; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'joy';

(* Global variable containing the number of active joystick devices.  The
   current drivers support a maximum of eight controllers. *)
  al_num_joysticks: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'num_joysticks';


(* Installs Allegro's joystick handler, and calibrates the centre position
   values.  The type parameter should usually be @code(AL_JOY_TYPE_AUTODETECT),
   or see the platform specific documentation for a list of the available
   drivers.  You must call this routine before using any other joystick
   functions, and you should make sure that all joysticks are in the middle
   position at the time.  Example:
   @longcode(#
al_textout_centre_ex (al_screen, al_font,
		      'Center the joystick and press a key.',
		      AL_SCREEN_W DIV 2, SCREEN_H DIV 2, red_color, -1);
al_readkey;
IF NOT al_install_joystick (AL_JOY_TYPE_AUTODETECT) THEN
  abort_on_error ('Error initialising joystick!');
   #);
   @returns(@true on success.  As soon as you have installed the joystick
     module, you will be able to read the button state and digital @(on/off
     toggle@) direction information, which may be enough for some games.  If
     you want to get full analogue input, though, you need to use the
     @code(al_calibrate_joystick) functions to measure the exact range of the
     inputs.) *)
  FUNCTION al_install_joystick (atype: LONGINT): BOOLEAN;

(* Removes the joystick handler. You don't normally need to bother calling
   this, because @code(al_exit) will do it for you. *)
  PROCEDURE al_remove_joystick; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_joystick';

(* Pass the number of the joystick you want to calibrate as the parameter.

    @returns(a text description for the next type of calibration that will
      be done on the specified joystick, or empty string if no more calibration
      is required.) *)
  FUNCTION al_calibrate_joystick_name (n: LONGINT): STRING;

(* Most joysticks need to be calibrated before they can provide full analogue
   input.  This function performs the next operation in the calibration series
   for the specified stick, assuming that the joystick has been positioned in
   the manner described by a previous call to
   @code(al_calibrate_joystick_name), returning @true on success.  For example,
   a simple routine to fully calibrate all the joysticks might look like:
   @longcode(#
FUNCTION CalibrateJoysticks: BOOLEAN
VAR
  Cnt: INTEGER;
BEGIN
  FOR Cnt := 1 TO al_num_joysticks DO
  BEGIN
    WHILE (al_joy[Cnt - 1].flags AND AL_JOYFLAG_CALIBRATE) <> 0 DO
    BEGIN
      al_textout_ex (..., al_calibrate_joystick_name (Cnt - 1) + ', and press a key', ...);
      al_readkey;
      IF NOT al_calibrate_joystick (i - 1) THEN
      BEGIN
        al_textout_ex (..., 'oops!', ...);
	al_readkey;
	RESULT := FALSE;
	EXIT;
      END;
    END;
  END;
  RESULT := TRUE;
END;
   #)

   @returns(@true on success, @false if the calibration could not be performed
     successfully.) *)
  FUNCTION al_calibrate_joystick (n: LONGINT): BOOLEAN;

(* After all the headache of calibrating the joystick, you may not want to make
   your poor users repeat the process every time they run your program.  Call
   this function to save the joystick calibration data into the specified
   configuration file, from which it can later be read by
   @code(al_load_joystick_data).  Pass a @nil filename to write the data to the
   currently selected configuration file.

   @returns(@true on success, @false if the data could not be saved.) *)
  FUNCTION al_save_joystick_data (filename: STRING): BOOLEAN;

(* Restores calibration data previously stored by @code(al_save_joystick_data)
   or the setup utility.  This sets up all aspects of the joystick code:  you
   don't even need to call @code(al_install_joystick) if you are using this
   function.  Pass an empty filename to read the data from the currently
   selected configuration file.

   @returns(@true on success:  if it fails the joystick state is undefined and
     you must reinitialise it from scratch.) *)
  FUNCTION al_load_joystick_data (filename: STRING): BOOLEAN;

(* The joystick handler is not interrupt driven, so you need to call this
   function every now and again to update the global position values.

   @returns(zero on success or a negative number on failure @(usually because
     no joystick driver was installed@).) *)
  FUNCTION al_poll_joystick: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_joystick';



(*********************
 * Color and palette *
 *********************)

TYPE
(* Pointer to @code(AL_RGB). *)
  AL_RGBptr = ^AL_RGB;

(* Palette entry.  It contains an additional field for the purpose of padding
   but you should not usually care about it.  Read @code(alpalete) unit for a
   description on how to obtain/use this structure. *)
  AL_RGB = RECORD
    r	  : BYTE;
    g	  : BYTE;
    b	  : BYTE;
    filler: BYTE;
  END;



(* Converts colors from a hardware independent format (red, green, and blue
   values ranging 0-255) to the pixel format required by the current video
   mode, calling the preceding 8, 15, 16, 24, or 32-bit makecol functions as
   appropriate.

   @returns(the requested RGB triplet in the current color depth.) *)
  FUNCTION al_makecol (r, g, b: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol';

(* Converts colors from a hardware independent form (red, green, and blue
   values ranging 0-255) into 8-bit color index.  Converting to an 8-bit color
   involves searching the palette to find the closest match, which is quite
   slow unless you have set up an RGB mapping table.

   @returns(the requested RGB triplet in the specified color depth.) *)
  FUNCTION al_makecol8 (r, g, b: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol8';

(* Converts colors from a hardware independent format (red, green, and blue
   values ranging 0-255) to the pixel format required by the specified color
   depth.

   @returns(the requested RGB triplet in the specified color depth.) *)
  FUNCTION al_makecol_depth (color_depth, r, g, b: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol_depth';

(* Convert RGBA colors into display dependent pixel formats.  In anything less
   than a 32-bit mode, these are the same as calling @code(al_makecol) or
   @code(al_makecol_depth), but by using these routines it is possible to
   create 32-bit color values that contain a true 8 bit alpha channel along
   with the red, green, and blue components.  You should only use RGBA format
   colors as the input to @code(al_draw_trans_sprite) or
   @code(al_draw_trans_rle_sprite) after calling @code(al_set_alpha_blender),
   rather than drawing them directly to the screen.

   @returns(the requested RGBA quadruplet.) *)
  FUNCTION al_makeacol (r, g, b, a: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makeacol';
  FUNCTION al_makeacol_depth (color_depth, r, g, b, a: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makeacol_depth';



(* Given a color in the format being used by the current video mode, these
   functions extract one of the red, green, blue, or alpha components (ranging
   0-255), calling the preceding 8, 15, 16, 24, or 32-bit get functions as
   appropriate.  The alpha part is only meaningful for 32-bit pixels. *)
  FUNCTION al_getr (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getr';
  FUNCTION al_getg (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getg';
  FUNCTION al_getb (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getb';
  FUNCTION al_geta (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'geta';



(* Given a color in the format being used by the specified color depth, these
   functions extract one of the red, green, blue, or alpha components (ranging
   0-255). The alpha part is only meaningful for 32-bit pixels. *)
  FUNCTION al_getr_depth (color_depth, c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getr_depth';
  FUNCTION al_getg_depth (color_depth, c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getg_depth';
  FUNCTION al_getb_depth (color_depth, c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getb_depth';
  FUNCTION al_geta_depth (color_depth, c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'geta_depth';



(* Convert color values between the HSV and RGB color spaces.  The RGB values
   range from 0 to 255, hue is from 0 to 360, and saturation and value are from
   0 to 1. *)
  PROCEDURE al_hsv_to_rgb (h, s, v: SINGLE; VAR r, g, b: LONGINT);
    INLINE;
  PROCEDURE al_rgb_to_hsv (r, g, b: LONGINT; VAR h, s, v: SINGLE);
    INLINE;



TYPE
(* Pointer to @code(AL_PALETTE_DICT). *)
  AL_PALETTE_DICTptr = ^AL_PALETTE_DICT;
(* To be used by @code(al_palette_color). *)
  AL_PALETTE_DICT = ARRAY [0..255] OF LONGINT;
VAR
(* Table mapping palette index colors (0-255) into whatever pixel format is
   being used by the current display mode.  In a 256-color mode this just maps
   onto the array index.  In truecolor modes it looks up the specified entry in
   the current palette, and converts that RGB value into the appropriate packed
   pixel format.
   @seealso(al_set_palette) @seealso(al_makecol) @seealso(al_set_color_depth) *)
  al_palette_color: AL_PALETTE_DICTptr;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'palette_color';



CONST
(* To know thet palette size. *)
  AL_PAL_SIZE = 256;



TYPE
(* Pointer to a @code(AL_PALETTE). *)
  AL_PALETTEptr = ^AL_PALETTE;
(* Color palette description for indexed modes (8bpp).  Remember that color
   components are 0-63. *)
  AL_PALETTE = ARRAY [0..AL_PAL_SIZE-1] OF AL_RGB;
(* Pointer to a @code(AL_RGB_MAP). *)
  AL_RGB_MAPptr = ^AL_RGB_MAP;
(* Stores an rgb map to accelerate conversions. @seealso(al_create_rgb_table) *)
  AL_RGB_MAP = RECORD
    data: ARRAY [0..31, 0..31, 0..31] OF BYTE;
  END;



VAR
(* A palette containing solid black colors, used by the fade routines. *)
  al_black_palette: AL_PALETTE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'black_palette';
(* The palette used by the Atari ST low resolution desktop. I'm not quite sure
   why this is still here, except that the original grabber and test programs
   use it.  It is probably the only Atari legacy code left in Allegro, and it
   would be a shame to remove it :-)

   The contents of this palette are 16 colors repeated 16 times.  Color entry
   zero is equal to color entry 16, which is equal to color entry 24, etc.
@table(
  @rowhead(@cell(Index) @cell(Color) @cell(RGB values) )
  @row(@cell( 0) @cell(White       ) @cell(@code(63  63  63)))
  @row(@cell( 1) @cell(Red         ) @cell(@code(63   0   0)))
  @row(@cell( 2) @cell(Green       ) @cell(@code( 0  63   0)))
  @row(@cell( 3) @cell(Yellow      ) @cell(@code(63  63   0)))
  @row(@cell( 4) @cell(Blue        ) @cell(@code( 0   0  63)))
  @row(@cell( 5) @cell(Pink        ) @cell(@code(63   0  63)))
  @row(@cell( 6) @cell(Cyan        ) @cell(@code( 0  63  63)))
  @row(@cell( 7) @cell(Grey        ) @cell(@code(16  16  16)))
  @row(@cell( 8) @cell(Light grey  ) @cell(@code(31  31  31)))
  @row(@cell( 9) @cell(Light red   ) @cell(@code(63  31  31)))
  @row(@cell(10) @cell(Light green ) @cell(@code(31  63  31)))
  @row(@cell(11) @cell(Light yellow) @cell(@code(63  63  31)))
  @row(@cell(12) @cell(Light blue  ) @cell(@code(31  31  63)))
  @row(@cell(13) @cell(Light pink  ) @cell(@code(63  31  63)))
  @row(@cell(14) @cell(Light cyan  ) @cell(@code(31  63  63)))
  @row(@cell(15) @cell(Black       ) @cell(@code( 0   0   0)))
) *)
  al_desktop_palette: AL_PALETTE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'desktop_palette';
(* The default IBM BIOS palette.  This will be automatically selected whenever
   you set a new graphics mode.  The palette contains 16 basic colors plus many
   gradients between them.  If you want to see the values, you can write a
   small Allegro program which saves a screenshot with this palette, or open
   the grabber tool provided with Allegro and create a new palette object,
   which will use this palette by default. *)
  al_default_palette: AL_PALETTE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'default_palette';

(* To speed up reducing RGB values to 8-bit paletted colors, Allegro uses a 32k
   lookup table (5 bits for each color component).  You must set up this table
   before using the gouraud shading routines, and if present the table will
   also vastly accelerate the @code(al_makecol) and some
   @code(al_create_*_table) functions on 8-bit graphic mode.  RGB tables can be
   precalculated with the rgbmap utility, or generated at runtime with
   @code(al_create_rgb_table). *)
  al_rgb_table: AL_RGB_MAPptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_map';



(* Sets the specified palette entry to the specified @code(AL_RGB) triplet.
   Unlike the other palette functions this doesn't do any retrace
   synchronisation, so you should call @code(al_vsync) before it to prevent
   snow problems. *)
  PROCEDURE al_set_color (idx: LONGINT; p: AL_RGBptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color';

(* Sets the entire palette of 256 colors.  You should provide an array of 256
   RGB structures.  Unlike @code(al_set_color), there is no need to call
   @code(al_vsync) before this function. *)
  PROCEDURE al_set_palette (p: AL_PALETTE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_palette';

(* Sets the palette entries between @code(from) and @code(ato) (inclusive:
   pass 0 and 255 to set the entire palette).  If @code(al_vsync) is not zero it
   waits for the vertical retrace, otherwise it sets the colors immediately. *)
  PROCEDURE al_set_palette_range (p: AL_PALETTE; from, ato, vsync: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_palette_range';

(* Retrieves the specified palette entry. *)
  PROCEDURE al_get_color (idx: LONGINT; p: AL_RGBptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color';

(* Retrieves the entire palette of 256 colors.  You should provide a
   @code(AL_PALETTE) to store it in. *)
  PROCEDURE al_get_palette (p: AL_PALETTEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_palette';

(* Retrieves the palette entries between @code(from) and @code(ato)
   (inclusive: pass 0 and 255 to set the entire palette). *)
  PROCEDURE al_get_palette_range (p: AL_PALETTEptr; from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_palette_range';



(* Calculates a temporary palette part way between @code(source) and
   @code(dest), returning it in the @code(aoutput) parameter.  The position between
   the two extremes is specified by the pos value: 0 returns an exact copy of
   source, 64 returns dest, 32 returns a palette half way between the two, etc.
   This routine only affects colors between @code(from) and @code(ato)
   (inclusive: pass 0 and 255 to interpolate the entire palette). *)
  PROCEDURE al_fade_interpolate (source, dest: AL_PALETTE; aoutput: AL_PALETTEptr; apos, from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_interpolate';

(* Gradually fades a part of the palette from the @code(source) palette to the
   @code(dest) palette.  The @code(speed) is from 1 (the slowest) up to 64
   (instantaneous).  This routine only affects colors between @code(from) and
   @code(ato) (inclusive: pass 0 and 255 to fade the entire palette).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_from_range (source, dest: AL_PALETTE; speed, from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_from_range';

(* Gradually fades a part of the palette from a black screen to the specified
   palette.  The @code(speed) is from 1 (the slowest) up to 64
   (instantaneous).  This routine only affects colors between @code(from) and
   @code(ato) (inclusive: pass 0 and 255 to fade the entire palette).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_in_range (p: AL_PALETTE; speed, from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_in_range';

(* Gradually fades a part of the current palette to the @code(dest) palette.
   The @code(speed) is from 1 (the slowest) up to 64 (instantaneous).  This
   routine only affects colors between @code(from) and @code(ato) (inclusive:
   pass 0 and 255 to fade the entire palette).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_out_range (speed, from, ato: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_out_range';

(* Gradually fades from the @code(source) palette to the @code(dest) palette.
   The @code(speed) is from 1 (the slowest) up to 64 (instantaneous).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_from (source, dest: AL_PALETTE; speed: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_from';


(* Gradually fades from a black screen to the specified palette.  The
   @code(speed) is from 1 (the slowest) up to 64 (instantaneous).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_in (p: AL_PALETTE; speed: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_in';

(* Gradually fades from the current palette to a black screen.  The
   @code(speed) is from 1 (the slowest) up to 64 (instantaneous).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. *)
  PROCEDURE al_fade_out (speed: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_out';



(* Ugly hack for use in various dodgy situations where you need to convert
   between paletted and truecolor image formats.  Sets the internal palette
   table in the same way as the @code(al_set_palette) function, so the
   conversion will use the specified palette, but without affecting the
   display hardware in any way.  The previous palette settings are stored in an
   internal buffer, and can be restored by calling @code(al_unselect_palette).
   If you call @code(al_select_palette) again, however, the internal buffer
   will be overwritten. *)
  PROCEDURE al_select_palette (p: AL_PALETTE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'select_palette';

(* Restores the palette tables that were in use before the last call to
   @code(al_select_palette). *)
  PROCEDURE al_unselect_palette; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unselect_palette';


(* Constructs a fake truecolor palette, using three bits for red and green and
   two for the blue.  The @code(al_load_bitmap) function fills the palette
   parameter with this if the file does not contain a palette itself (ie. you
   are reading a truecolor bitmap). *)
  PROCEDURE al_generate_332_palette (p: AL_PALETTEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_332_palette';



(* Searches the specified palette for the closest match to the requested color,
   which are specified in the VGA hardware 0-63 format.  Normally you should
   call @code(al_makecol_depth) instead, but this lower level function may be
   useful if you need to use a palette other than the currently selected one,
   or specifically don't want to use the @code(al_rgb_map) lookup table.

   @returns(the index of the palette for the closest match to the requested
     color.) *)
  FUNCTION al_bestfit_color (pal: AL_PALETTE; r, g, b: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'bestfit_color';

(* Fills the specified RGB mapping table with lookup data for the specified
   palette.  If the @code(callback) function is not @nil, it will be called
   256 times during the calculation, allowing you to display a progress
   indicator. *)
  PROCEDURE al_create_rgb_table (table: AL_RGB_MAPptr; pal: AL_PALETTE;
				 callback: AL_INT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_rgb_table';



(***********
 * Bitmaps *
 ***********)

TYPE
(* Pointer to @code(AL_BITMAP). *)
  AL_BITMAPptr = ^AL_BITMAP;

(* @exclude
  These are for internal low-level access.  DO NOT USE IN YOUR GAMES. *)
  _BMP_BANK_SWITCHER = FUNCTION (bmp: AL_BITMAPptr; lyne: LONGINT): DWORD; CDECL;

(* Stores the contents of a bitmap.

   There is some stuff in the structure that is liable to change and you
   shouldn't use anything except the next.  The @code(w) and @code(h) fields
   can be used to obtain the size of an existing bitmap:
@longcode(#
CONST
  BS = 'Bitmap size: (%dx%d)\n';
VAR
  bmp: AL_BITMAPptr;
  Message: STRING;
BEGIN
  bmp = al_load_bitmap ('file.bmp', pal);
  StrFmt (Message, BS, [bmp^.w, bmp^.h]);
  al_allegro_message (Message);
END.
#)

   The clipping rectangle is inclusive on the left and top (0 allows drawing to
   position 0) but exclusive on the right and bottom (10 allows drawing to
   position 9, but not to 10).  Note this is not the same format as that of the
   clipping API, which takes inclusive coordinates for all four corners.  All
   the values of this structure should be regarded as read-only.  If you want
   to modify the clipping region, please refrain from changing this structure.
   Use @code(al_set_clip_rect) instead. *)
  AL_BITMAP = RECORD
    w, h: LONGINT;		{< width and height in pixels }
    clip: LONGINT;		{< flag if clipping is turned on }
    cl, cr, ct, cb: LONGINT;	{< clip left, right, top and bottom values }
    vtable: AL_GFX_VTABLEptr;	{< drawing functions }
    write_bank: _BMP_BANK_SWITCHER;	{< C func on some machines, asm on i386 }
    read_bank: _BMP_BANK_SWITCHER;	{< C func on some machines, asm on i386 }
    dat: POINTER;		{< the memory we allocated for the bitmap }
    id: DWORD;			{< for identifying sub-bitmaps }
    extra: POINTER;		{< points to a structure with more info }
    x_ofs: LONGINT;		{< horizontal offset (for sub-bitmaps) }
    y_ofs: LONGINT;		{< vertical offset (for sub-bitmaps) }
    seg: LONGINT;		{< bitmap segment }
    line: POINTER;		{< ZERO_SIZE_ARRAY(unsigned char *, line); }
  END;



(* This is used to define call-back parameters for some drawing procedures. *)
  AL_POINT_PROC = PROCEDURE (bmp: AL_BITMAPptr; x, y, c: LONGINT); CDECL;



(* Creates a memory bitmap sized @code(w) by @code(h).  The bitmap will have
   clipping turned on, and the clipping rectangle set to the full size of the
   bitmap.  The image memory will not be cleared, so it will probably contain
   garbage:  you should clear the bitmap before using it.  This routine always
   uses the global pixel format, as specified by calling
   @code(al_set_color_depth).  The minimum height of the bitmap must be 1 and
   width can't be negative.

   @returns(a pointer to the created bitmap, or @nil if the bitmap could not
     be created . Remember to free this bitmap later to avoid memory leaks.) *)
  FUNCTION al_create_bitmap (w, h: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap';

(* Creates a bitmap in a specific color depth (8, 15, 16, 24 or 32 bits per
   pixel).
   @returns(a pointer to the created bitmap, or @nil if the bitmap could not
     be created . Remember to free this bitmap later to avoid memory leaks.)
   @seealso(al_create_bitmap) *)
  FUNCTION al_create_bitmap_ex (bpp, width, height: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap_ex';

(* Creates a sub-bitmap, ie. a bitmap sharing drawing memory with a
   pre-existing bitmap, but possibly with a different size and clipping
   settings.  When creating a sub-bitmap of the mode-X screen, the x position
   must be a multiple of four.  The sub-bitmap width and height can extend
   beyond the right and bottom edges of the parent (they will be clipped), but
   the origin point must lie within the parent region.

   Remember to free the sub bitmap before freeing the parent bitmap to avoid
   memory leaks and potential crashes accessing memory which has been freed.

   @returns(a pointer to the created sub bitmap, or @nil if the sub bitmap
     could not be created.) *)
  FUNCTION al_create_sub_bitmap (parent: AL_BITMAPptr; x, y, w, h: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sub_bitmap';

(* Allocates a video memory bitmap of the specified size.  This can be used to
   allocate offscreen video memory for storing source graphics ready for a
   hardware accelerated blitting operation, or to create multiple video memory
   pages which can then be displayed by calling @code(al_show_video_bitmap).
   Read the introduction of this chapter for a comparison with other types of
   bitmaps and other specific details.

   @bold(Warning:)  video memory bitmaps are usually allocated from the same
   space as the screen bitmap, so they may overlap with it; it is therefore not
   a good idea to use the global screen at the same time as any surfaces
   returned by this function.

   Remember to destroy this bitmap before any subsequent call to
   @code(al_set_gfx_mode).

   @returns(a pointer to the bitmap on success, or @nil if you have run out of
     video ram.) *)
  FUNCTION al_create_video_bitmap (width, height: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_video_bitmap';

(* Allocates a system memory bitmap of the specified size.  Read the
   introduction of this chapter for a comparison with other types of bitmaps
   and other specific details.

   Remember to destroy this bitmap before any subsequent call to
   @code(al_set_gfx_mode).

   @returns(a pointer to the bitmap on success, @nil otherwise.) *)
  FUNCTION al_create_system_bitmap (width, height: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_system_bitmap';

(* Destroys a memory bitmap, sub-bitmap, video memory bitmap, or system bitmap
   when you are finished with it.  If you pass a @nil pointer this function
   won't do anything.

   The bitmap must not have a mouse cursor shown on it at the time it is
   destroyed. *)
  PROCEDURE al_destroy_bitmap (bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_bitmap';



(* @returns(the color depth of the specified bitmap @(8, 15, 16, 24, or 32@).) *)
  FUNCTION al_bitmap_color_depth (bmp: AL_BITMAPptr): LONGINT;
    INLINE;

(* @returns(the mask color for the specified bitmap @(the value which is
    skipped when drawing sprites@).  For 256-color bitmaps this is zero, and
    for truecolor bitmaps it is bright pink @(maximum red and blue, zero
    green@).  A frequent use of this function is to clear a bitmap with the
    mask color so you can later use this bitmap with @code(al_draw_sprite)
    after drawing other stuff on it.) *)
  FUNCTION al_bitmap_mask_color (bmp: AL_BITMAPptr): LONGINT;
    INLINE;

(* @returns(@true if the two bitmaps describe the same drawing surface, ie.
    the pointers are equal, one is a sub-bitmap of the other, or they are both
    sub-bitmaps of a common parent.) *)
  FUNCTION al_is_same_bitmap (bmp1, bmp2: AL_BITMAPptr): BOOLEAN;
    INLINE;

(* @returns(@true if bmp is a memory bitmap, ie. it was created by calling
    @code(al_create_bitmap) or loaded from a grabber datafile or image file.) *)
  FUNCTION al_is_memory_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
    INLINE;

(* @returns(@true if @code(bmp) is the screen bitmap, or a sub-bitmap of the
   screen.) *)
  FUNCTION al_is_screen_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
    INLINE;

(* @returns(@true) if bmp is the screen bitmap, a video memory bitmap, or a
   sub-bitmap of either.) *) 
  FUNCTION al_is_video_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
    INLINE;

(* @returns(@true if bmp is a system bitmap object, or a sub-bitmap of one.) *)
  FUNCTION al_is_system_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
    INLINE;

(* @returns(@true if bmp is a sub-bitmap.) *)
  FUNCTION al_is_sub_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
    INLINE;

(* Acquires the specified video bitmap prior to drawing onto it.  You never
   need to call the function explicitly as it is low level, and will only give
   you a speed up if you know what you are doing.  Using it wrongly may cause
   slowdown, or even lock up your program.

   @bold(Note:) You do never need to use @code(al_acquire_bitmap) on a memory
   bitmap, i.e. a normal bitmap created with @code(al_create_bitmap).  It will
   simply do nothing in that case.

   It still can be useful, because e.g. under the current DirectDraw driver of
   Allegro, most drawing functions need to lock a video bitmap before drawing
   to it.  But doing this is very slow, so you will get much better performance
   if you acquire the screen just once at the start of your main redraw
   function, then call multiple drawing operations which need the bitmap
   locked, and only release it when done.

   Multiple acquire calls may be nested, but you must make sure to match up the
   acquire_bitmap and @code(al_release_bitmap) calls.  Be warned that DirectX
   and X11 programs activate a mutex lock whenever a surface is locked, which
   prevents them from getting any input messages, so you must be sure to
   release all your bitmaps before using any timer, keyboard, or other
   non-graphics routines!

   Note that if you are using hardware accelerated VRAM->VRAM functions, you
   should not call @code(al_acquire_bitmap).  Such functions need an unlocked
   target bitmap under DirectX, so there is now just the opposite case from
   before - if the bitmap is already locked with @code(al_acquire_bitmap), the
   drawing operation has to unlock it.

   @bold(Note:) For backwards compatibility, the unlocking behavior of such
   functions is permanent.  That is, if you call @code(al_acquire_bitmap)
   first, then call e.g. an accelerated blit, the DirectX bitmap will be
   unlocked internally (it won't affect the nesting counter of acquire/release
   calls).

   There is no clear cross-platform way in this Allegro version to know which
   drawing operations need a locked/unlocked state.  For example a normal
   rectfill most probably is accelerated under DirectX, and therefore needs the
   screen unlocked, but an @code(XOR) rectfill, or one with blending
   activated, most probably is not, and therefore locks the screen. And while
   the DirectX driver will do automatic unlocking, there is no such thing under
   X11, where the function is used to synchronize X11 calls from different
   threads.  Your best bet is to never use @code(al_acquire_bitmap) - changes
   are you are doing something in the wrong way if you think you need it.

   @bold(Warning:) This function can be very dangerous to use, since the whole
   program may get locked while the bitmap is locked.  So the lock should only
   be held for a short time, and you should not call anything but drawing
   operations onto the locked video bitmap while a lock is in place.
   Especially don't call things like @code(al_show_mouse) (or
   @code(al_scare_mouse) which calls that) or @code(al_readkey), since it will
   most likely deadlock your entire program. *)
  PROCEDURE al_acquire_bitmap (bmp: AL_BITMAPptr);
    INLINE;

(* Releases a bitmap that was previously locked by calling
   @code(al_acquire_bitmap).  If the bitmap was locked multiple times, you must
   release it the same number of times before it will truly be unlocked. *)
  PROCEDURE al_release_bitmap (bmp: AL_BITMAPptr);
    INLINE;



(* Generates a 256-color palette suitable for making a reduced color version of
   the specified truecolor image.

   @param(rsvdcols points to a table indicating which colors it is allowed to
     modify:  zero for free colors which may be set to whatever the optimiser
     likes, negative values for reserved colors which cannot be used,
     and positive values for fixed palette entries that must not be changed,
     but can be used in the optimisation.)
   @returns(the number of different colors recognised in the provided bitmap,
     zero if the bitmap is not a truecolor image or there wasn't enough memory
     to perform the operation, and negative if there was any internal error in
     the color reduction code.) *)
  FUNCTION  al_generate_optimized_palette (image: AL_BITMAPptr; pal: AL_PALETTEptr; rsvdcols: ARRAY OF BYTE): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_optimized_palette';



(* Loads a bitmap from a file.  The palette data will be stored in the second
   parameter, which should be an @code(AL_PALETTE) structure.  At present this
   function supports BMP, LBM, PCX, and TGA files, determining the type from
   the file extension.

   If the file contains a truecolor image, you must set the video mode or call
   @code(al_set_color_conversion) before loading it.  In this case, if the
   destination color depth is 8-bit, the palette will be generated by calling
   @code(al_generate_optimized_palette) on the bitmap;  otherwise, the
   returned palette will be generated by calling @code(al_generate_332_palette)

   The @code(pal) argument may be @nil.  In this case, the palette data are
   simply not returned.  Additionally, if the file is a truecolor image and the
   destination color depth is 8-bit, the color conversion process will use the
   current palette instead of generating an optimized one.

   Example:
@longcode(#
VAR
  bmp: AL_BITMAPptr;
  palette: AL_PALETTE;
          ...
  bmp := al_load_bitmap ('image.pcx', @palette);
  IF bmp = NIL THEN
    abort_on_error ('Couldn''t load image.pcx!');
          ...
   al_destroy_bitmap (bmp);
#)
     @seealso(al_load_bmp) @seealso(al_load_pcx) @seealso(al_load_tga)
     @seealso(al_load_lbm) @seealso(al_save_bitmap) *)
  FUNCTION al_load_bitmap (filename: STRING; pal: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bitmap';

(* Loads an 8-bit, 16-bit, 24-bit or 32-bit Windows or OS/2 BMP file.  Remember
   that you are responsible for destroying the bitmap when you are finished
   with it to avoid memory leaks.
   @returns(a pointer to the bitmap or @nil on error.)
   @seealso(al_load_bitmap) @seealso(al_load_bmp_pf) *)
  FUNCTION al_load_bmp (filename: STRING; palette: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bmp';

(* Loads a 256-color IFF ILBM/PBM file.  Remember that you are responsible for
   destroying the bitmap when you are finished with it to avoid memory leaks.
   @returns(a pointer to the bitmap or @nil on error.)
   @seealso(al_load_bitmap) @seealso(al_load_lbm_pf) *)
  FUNCTION al_load_lbm (filename: STRING; palette: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_lbm';

(* Loads a 256-color or 24-bit truecolor PCX file. Remember that you are
   responsible for destroying the bitmap when you are finished with it to avoid
   memory leaks.
   @returns(a pointer to the bitmap or @nil on error.)
   @seealso(al_load_bitmap) @seealso(al_load_pcx_pf) *)
  FUNCTION al_load_pcx (filename: STRING; palette: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_pcx';

(* Loads a 256-color, 15-bit hicolor, 24-bit truecolor, or 32-bit
   truecolor+alpha TGA file.  Remember that you are responsible for destroying
   the bitmap when you are finished with it to avoid memory leaks.
   @returns(a pointer to the bitmap or @nil on error.)
   @seealso(al_load_bitmap) @seealso(al_load_tga_pf) *)
  FUNCTION al_load_tga (filename: STRING; palette: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_tga';



(* Writes a bitmap into a file, using the specified palette, which should be an
   @code(AL_PALETTE) structure.  The output format is determined from the
   filename extension: at present this function supports BMP, PCX and TGA
   formats.

   Two things to watch out for:  on some video cards it may be faster to copy
   the screen to a memory bitmap and save the latter, and if you use this to
   dump the screen into a file you may end up with an image much larger than
   you were expecting, because Allegro often creates virtual screens larger
   than the visible screen.  You can get around this by using a sub-bitmap to
   specify which part of the screen to save, eg:
@longcode(#
VAR
  bmp: AL_BITMAPptr;
  palette: AL_PALETTE;
          ...
  al_get_palette (@palette);
  bmp := al_create_sub_bitmap (al_screen, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
  al_save_bitmap ('dump.pcx', bmp, @palette);
  al_destroy_bitmap (bmp);
#)
  *)
  FUNCTION al_save_bitmap (filename: STRING; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_bitmap';

(* Writes a bitmap into a 256-color or 24-bit truecolor BMP file.
   @returns(non-zero on error.)
   @seealso(al_save_bitmap) @seealso(al_save_bmp_pf) *)
  FUNCTION al_save_bmp (filename: STRING; palette: AL_PALETTEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_bmp';

(* Writes a bitmap into a 256-color or 24-bit truecolor PCX file.
   @returns(non-zero on error.)
   @seealso(al_save_bitmap) @seealso(al_save_pcx_pf) *)
  FUNCTION al_save_pcx (filename: STRING; palette: AL_PALETTEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_pcx';

(* Writes a bitmap into a 256-color, 15-bit hicolor, 24-bit truecolor, or
   32-bit truecolor+alpha TGA file.
   @returns(non-zero on error.)
   @seealso(al_save_bitmap) @seealso(al_save_tga_pf) *)
  FUNCTION al_save_tga (filename: STRING; palette: AL_PALETTEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_tga';




(******************
 * Mouse routines *
 ******************)

CONST
(* Indicates that the mouse cursor is the default system cursor, not Allegro's
   custom cursor. @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_NONE		= 0;
(* Selects the custom Allegro cursor, i.e. the one that you set with
   @code(al_set_mouse_sprite). @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_ALLEGRO	= 1;
(* The operating system default arrow cursor.
 @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_ARROW		= 2;
(* The operating system default `busy' cursor (hourglass).
   @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_BUSY		= 3;
(* The operating system default `question' cursor (arrow with question mark).
   @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_QUESTION	= 4;
(* The operating system default `edit' cursor (vertical bar).
   @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_EDIT		= 5;
(* @exclude *)
  AL_NUM_MOUSE_CURSORS		= 6;



VAR
(* Global variable containing the current mouse horizontal position.  Wherever
   possible these values will be updated asynchronously, but if
   @code(al_mouse_needs_poll) returns @true, you must manually call
   @code(al_poll_mouse) to update them with the current input state.

   The position is integer ranging from zero to the right side of the
   screen. *)
  al_mouse_x: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_x';
(* Global variable containing the current mouse vertical position.  Wherever
   possible these values will be updated asynchronously, but if
   @code(al_mouse_needs_poll) returns @true, you must manually call
   @code(al_poll_mouse) to update them with the current input state.

   The position is integer ranging from zero to the bottom side of the
   screen. *)
  al_mouse_y: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_y';
(* Global variable containing the current mouse position.  Wherever possible
   these values will be updated asynchronously, but if
   @code(al_mouse_needs_poll) returns @true, you must manually call
   @code(al_poll_mouse) to update them with the current input state.

   It holds the current vertical wheel position, when using an input driver
   that supports wheel mice. *)
  al_mouse_z: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_z';
(* Global variable containing the current mouse position.  Wherever possible
   these values will be updated asynchronously, but if
   @code(al_mouse_needs_poll) returns @true, you must manually call
   @code(al_poll_mouse) to update them with the current input state.

   It holds the current horizontal wheel position, when using an input driver
   that supports wheel mice. *)
  al_mouse_w: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_w';
(* Global variable containing the current mouse button state.  Wherever
   possible these values will be updated asynchronously, but if
   @code(al_mouse_needs_poll) returns @true, you must manually call
   @code(al_poll_mouse) to update them with the current input state.

   It is a bitfield indicating the state of each button:  bit 0 is the left
   button, bit 1 the right, and bit 2 the middle button.  Additional non
   standard mouse buttons might be available as higher bits in this variable.
   Usage example:
   @longcode(
IF (al_mouse_b AND 1) <> 0 THEN
   WriteLn ('Left button is pressed');
IF (al_mouse_b AND 2) = 0 THEN
   WriteLn ('Right button is not pressed');
   #)
*)
  al_mouse_b: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_b';
(* Global variable containing the current mouse position.  Wherever possible
   these values will be updated asynchronously, but if
   @code(al_mouse_needs_poll) returns @true, you must manually call
   @code(al_poll_mouse) to update them with the current input state.

   It has the current X coordinate in the upper 16 bits and the Y in the lower
   16 bits.  This may be useful in tight polling loops where a mouse interrupt
   could occur between your reading of the two separate variables, since you
   can copy this value into a local variable with a single instruction and then
   split it up at your leisure. Example:
   @longcode(#
VAR
  mpos, mx, my: LONGINT;
  ...
  mpos := al_mouse_pos;
  mx := mpos SHR 16;
  my := mpos AND $0000ffff;
  #)*)
  al_mouse_pos: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_pos';



(* Global variable containing the current mouse sprite. This is read-only, and
   only to be modified using the @code(al_set_mouse_sprite) procedure. *)
  al_mouse_sprite: AL_BITMAPptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_sprite';
(* Global variable containing the current mouse focus point. This is read-only,
   and only to be modified using the @code(al_set_mouse_sprite_focus)
   procedure. *)
  al_mouse_x_focus: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_x_focus';
(* Global variable containing the current mouse focus point. This is read-only,
   and only to be modified using the @code(al_set_mouse_sprite_focus)
   procedure. *)
  al_mouse_y_focus: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_y_focus';



(* Installs the Allegro mouse handler.  You must do this before using any other
   mouse functions.

   @returns(-1 on failure, zero if the mouse handler is already installed @(in
     which case this function does nothing@) or the number of buttons on the
     mouse if the mouse handler has successfully been installed @(ie. this is
     the first time a handler is installed or you have removed the previous
     one@).

     Note that the number of mouse buttons returned by this function is more an
     indication than a physical reality.  With most devices there is no way of
     telling how many buttons there are, and any user can override the number
     of mouse buttons returned by this function with a custom configuration
     file and the variable @code(num_buttons).  Even if this value is
     overridden by the user, the global mouse variables will still report
     whatever the hardware is sending.) *)
  FUNCTION al_install_mouse: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'install_mouse';

(* Removes the mouse handler. You don't normally need to bother calling this,
   because @code(al_exit) will do it for you. *)
  PROCEDURE al_remove_mouse; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_mouse';



(* Wherever possible, Allegro will read the mouse input asynchronously (ie.
   from inside an interrupt handler), but on some platforms that may not be
   possible, in which case you must call this routine at regular intervals to
   update the mouse state variables.  To help you test your mouse polling code
   even if you are programming on a platform that doesn't require it, after the
   first time that you call this function Allegro will switch into polling
   mode, so from that point onwards you will have to call this routine in order
   to get any mouse input at all, regardless of whether the current driver
   actually needs to be polled or not.
   @returns(zero on success, or a negative number on failure @(ie. no mouse
     driver installed@).) *)
  FUNCTION al_poll_mouse: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_mouse';

(* Returns @true if the current mouse driver is operating in polling mode. *)
  FUNCTION al_mouse_needs_poll: BOOLEAN;
    INLINE;



(* After calling this function, Allegro will let the operating system draw the
   mouse cursor instead of doing it itself.  This is not possible with all
   graphics drivers though:  you'll need to check the
   @code(al_gfx_capabilities) flags after calling @code(al_show_mouse) to see
   if this works.  On some platforms, enabling the hardware cursor causes
   @code(al_get_mouse_mickeys) to return only a limited range of values, so you
   should not call this function if you need mouse mickeys. *)
  PROCEDURE al_enable_hardware_cursor; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'enable_hardware_cursor';

(* After calling this function, Allegro will be responsible for drawing the
   mouse cursor rather than the operating system.  On some platforms calling
   @code(al_enable_hardware_cursor) makes the return values of
   @code(al_get_mouse_mickeys) unreliable.  After calling this function,
   @code(al_get_mouse_mickeys) returns reliable results again. *)
  PROCEDURE al_disable_hardware_cursor; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'disable_hardware_cursor';

(* This function allows you to use the operating system's native mouse cursors
   rather than some custom cursor.  You will need to enable this functionality
   by calling @code(al_enable_hardware_cursor) beforehand.  If the operating
   system does not support this functionality, or if it has not been enabled,
   then Allegro will substitute its own cursor images.  You can change these
   substitute images using @code(al_set_mouse_cursor_bitmap).

   Note that the effects of this function are not apparent until
   @code(al_show_mouse) is called.

   To know whether the operating system's native cursor is being used, or if
   Allegro has made a substitution, you can check the
   @code(AL_GFX_SYSTEM_CURSOR) flag in @code(al_gfx_capabilities) after calling
   @code(al_show_mouse).

   The cursor argument selects the type of cursor to be displayed:
   @code(AL_MOUSE_CURSOR_NONE), @code(AL_MOUSE_CURSOR_ALLEGRO),
   @code(AL_MOUSE_CURSOR_ARROW), @code(AL_MOUSE_CURSOR_BUSY),
   @code(AL_MOUSE_CURSOR_QUESTION), @code(AL_MOUSE_CURSOR_EDIT) *)
  PROCEDURE al_select_mouse_cursor (cursor: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'select_mouse_cursor';

(* This function changes the cursor image Allegro uses if
   @code(al_select_mouse_cursor) is called but no native operating system
   cursor can be used, e.g. because you did not call
   @code(al_enable_hardware_cursor).

   The effect of this function will not be apparent until @code(al_show_mouse)
   is called.

   @param(cursor one of: @code(AL_MOUSE_CURSOR_ALLEGRO),
     @code(AL_MOUSE_CURSOR_ARROW), @code(AL_MOUSE_CURSOR_BUSY),
     @code(AL_MOUSE_CURSOR_QUESTION), @code(AL_MOUSE_CURSOR_EDIT) but not
     @code(AL_MOUSE_CURSOR_NONE).)
   @param(bmp can either point to a valid bitmap or it can be @nil.  Passing a
     bitmap makes Allegro use that image in place of its own default
     substitution @(should the operating system's native cursor be
     unavailable@).  The bitmap must remain available for the duration in which
     it could be used.  Passing @nil lets Allegro revert to its default
     substitutions.) *)
  PROCEDURE al_set_mouse_cursor_bitmap (cursor: LONGINT; bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_cursor_bitmap';



(* Tells Allegro to display a mouse pointer on the screen.  This will only work
   if the timer module has been installed.  The mouse pointer will be drawn
   onto the specified bitmap, which should normally be @code(al_screen) (see
   later for information about bitmaps).  To hide the mouse pointer, call
   @code(al_show_mouse @(@nil@)).

   @bold(Warning:) if you draw anything onto the screen while the pointer is
   visible, a mouse movement interrupt could occur in the middle of your
   drawing operation.  If this happens the mouse buffering and graphics drawing
   code will get confused and will leave @italic('mouse droppings') all over
   the screen.  To prevent this, you must make sure you turn off the mouse
   pointer whenever you draw onto the screen.  This is not needed if you are
   using a hardware cursor.

   Note: you must not be showing a mouse pointer on a bitmap at the time that
   the bitmap is destroyed with @code(al_destroy_bitmap), e.g. call
   @code(al_show_mouse @(@nil@);) before destroying the bitmap.  This does not
   apply to @code(al_screen) since you never destroy @code(al_screen) with
   @code(al_destroy_bitmap). *)
  PROCEDURE al_show_mouse (bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'show_mouse';

(* You don't like Allegro's mouse pointer?  No problem.  Use this function to
   supply an alternative of your own.  If you change the pointer and then want
   to get Allegro's lovely arrow back again, call @code(al_set_mouse_sprite
   @(@nil@)).

   As a bonus, @code(al_set_mouse_sprite @(@nil@)) uses the current palette in
   choosing colors for the arrow.  So if your arrow mouse sprite looks ugly
   after changing the palette, call @code(al_set_mouse_sprite @(@nil@)). *)
  PROCEDURE al_set_mouse_sprite (sprite: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_sprite';

(* The mouse focus is the bit of the pointer that represents the actual mouse
   position, ie. the (@code(al_mouse_x), @code(al_mouse_y)) position.  By
   default this is the top left corner of the arrow, but if you are using a
   different mouse pointer you might need to alter it. *)
  PROCEDURE al_set_mouse_sprite_focus (x, y: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_sprite_focus';



(* Moves the mouse to the specified screen position.  It is safe to call even
   when a mouse pointer is being displayed. *)
  PROCEDURE al_position_mouse (x, y: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse';

(* Sets the mouse wheel position variable to the specified value. *)
  PROCEDURE al_position_mouse_z (z: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse_z';

(* Sets the horizontal mouse wheel position variable to the specified value. *)
  PROCEDURE al_position_mouse_w (w: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse_w';

(* Measures how far the mouse has moved since the last call to this function.
   The values of @code(mickeyx) and @code(mickeyy) will become negative if the
   mouse is moved left or up, respectively.  The mouse will continue to
   generate movement mickeys even when it reaches the edge of the screen, so
   this form of input can be useful for games that require an infinite range of
   mouse movement.

   Note that the infinite movement may not work in windowed mode, since under
   some platforms the mouse would leave the window, and may not work at all if
   the hardware cursor is in use. *)
  PROCEDURE al_get_mouse_mickeys (VAR mickeyx, mickeyy: LONGINT);
    INLINE;



(* Helper for hiding the mouse pointer prior to a drawing operation.  This will
   temporarily get rid of the pointer, but only if that is really required (ie.
   the mouse is visible, and is displayed on the physical screen rather than
   some other memory surface, and it is not a hardware or OS cursor).  The
   previous mouse state is stored for subsequent calls to
   @code(al_unscare_mouse). *)
  PROCEDURE al_scare_mouse; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scare_mouse';

(* Like @code(al_scare_mouse), but will only hide the cursor if it is inside
   the specified rectangle.  Otherwise the cursor will simply be frozen
   in place until you call @code(al_unscare_mouse), so it cannot interfere with
   your drawing. *)
  PROCEDURE al_scare_mouse_area (x, y, w, h: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scare_mouse_area';

(* Undoes the effect of a previous call to @code(al_scare_mouse) or
   @code(al_scare_mouse_area), restoring the original pointer state. *)
  PROCEDURE al_unscare_mouse; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unscare_mouse';


(* In case you do not need Allegro's mouse cursor API, which automatically
   emulates a cursor in software if no other cursor is available, you can use
   this low level function to try to display or hide the system cursor
   directly.  The cursor parameter takes the same values as
   @code(al_select_mouse_cursor).  This function is very similar to calling
   @code(al_enable_hardware_cursor), @code(al_select_mouse_cursor) and
   @code(al_show_mouse), but will not try to do anything if no system cursor
   is available.

   The most common use for this function is to just call it once at the
   beginning of the program to tell it to display the system cursor inside the
   Allegro window.  The return value can be used to see if this succeeded or
   not.  On some systems (e.g. DirectX fullscreen) this is not supported and
   the function will always fail, and in other cases only some of the cursors
   will work, or in the case of @code(AL_MOUSE_CURSOR_ALLEGRO), only certain
   bitmap sizes may be supported.

   You never should use @code(al_show_os_cursor) together with the function
   @code(al_show_mouse) and other functions affecting it
   (@code(al_select_mouse_cursor), @code(al_enable_hardware_cursor),
   @code(al_disable_hardware_cursor), @code(al_scare_mouse),
   @code(al_unscare_mouse)).  They implement the standard high level mouse API,
   and don't work together with this low level function.

   @returns(@true if a system cursor is being displayed after the function
     returns, or @false otherwise.) *)
  FUNCTION al_show_os_cursor (cursor: LONGINT): BOOLEAN;
    INLINE;



(****************
 * Graphic mode *
 ****************)

CONST
(* Closes any previously opened graphics mode, making you unable to use the
   global variable @code(al_screen), and in those environments that have
   text modes, sets one previously used or the closest match to that (usually
   80x25).  With this driver the size parameters of @code(al_set_gfx_mode)
   don't mean anything, so you can leave them all to zero or any other number
   you prefer. *)
  AL_GFX_TEXT			= -1;
(* Allegro will try to set the specified resolution with the current color
   depth in fullscreen mode.  Failing that, it will try to repeat the same
   operation in windowed mode.  If the call to @code(al_set_gfx_mode)
   succeeds, you are guaranteed to have set the specified resolution in the
   current color depth, but you don't know if the program is running fullscreen
   or windowed. *)
  AL_GFX_AUTODETECT		=  0;
(* Allegro will try to set the specified resolution with the current color
   depth in fullscreen mode.  If that is not possible, @code(al_set_gfx_mode)
   will fail. *)
  AL_GFX_AUTODETECT_FULLSCREEN	=  1;
(* Allegro will try to set the specified resolution with the current color
   depth in a windowed mode.  If that is not possible, @code(al_set_gfx_mode)
   will fail.  When it comes to windowed modes, the `specified resolution'
   actually means the graphic area your program can draw on, without including
   window decorations (if any).  Note that in windowed modes running with a
   color depth other than the desktop may result in non optimal performance due
   to internal color conversions in the graphic driver. Use
   @code(al_desktop_color_depth) to your advantage in these situations. *)
  AL_GFX_AUTODETECT_WINDOWED	=  2;
(* Using this driver Allegro guarantees that a graphic mode will always be set
   correctly.  It will try to select the resolution that you request, and if
   that fails, it will fall back upon whatever mode is known to be reliable on
   the current platform (this is 640x480 resolution under Windows, the actual
   framebuffer's resolution under Linux if it's supported, etc).  If it
   absolutely cannot set any graphics mode at all, it will return negative as
   usual, meaning that there's no possible video output on the machine, and
   that you should abort your program immediately, possibly after notifying
   this to the user with @code(al_message).  This fake driver is useful for
   situations where you just want to get into some kind of workable display
   mode, and can't be bothered with trying multiple different resolutions and
   doing all the error checking yourself.  Note however, that after a
   successful call to @code(al_set_gfx_mode) with this driver, you cannot make
   any assumptions about the width, height or color depth of the screen:  your
   code will have to deal with this little detail. *)
  AL_GFX_SAFE			= $53414645; { AL_ID('S','A','F','E') }

(* @exclude Graphic capabilities *)
  AL_GFX_CAN_SCROLL			= $00000001;
{ @exclude }
  AL_GFX_CAN_TRIPLE_BUFFER		= $00000002;
(* Indicates that a hardware mouse cursor is in use. When this flag is set, it
   is safe to draw onto the screen without hiding the mouse pointer first.
   Note that not every cursor graphic can be implemented in hardware:  in
   particular VBE/AF only supports 2-color images up to 32x32 in size, where
   the second color is an exact inverse of the first.  This means that Allegro
   may need to switch between hardware and software cursors at any point during
   the execution of your program, so you should not assume that this flag will
   remain constant for long periods of time.  It only tells you whether a
   hardware cursor is in use at the current time, and may change whenever you
   hide/redisplay the pointer. *)
  AL_GFX_HW_CURSOR			= $00000004;
(* Indicates that the normal opaque version of the @code(al_hline) function is
   implemented using a hardware accelerator.  This will improve the performance
   not only of @code(al_hline) itself, but also of many other functions that
   use it as a workhorse, for example @code(al_circlefill) and
   @code(al_floodfill). *)
  AL_GFX_HW_HLINE			= $00000008;
(* Indicates that the XOR version of the @code(al_hline) function, and any
   other functions that use it as a workhorse, are implemented using a
   hardware accelerator (see @code(AL_GFX_HW_HLINE)). *)
  AL_GFX_HW_HLINE_XOR			= $00000010;
(* Indicates that the solid and masked pattern modes of the @code(al_hline)
   function, and any other functions that use it as a workhorse, are
   implemented using a hardware accelerator (see @code(AL_GFX_HW_HLINE)). *)
  AL_GFX_HW_HLINE_SOLID_PATTERN		= $00000020;
(* Indicates that the copy pattern modes of the @code(al_hline) function, and
   any other functions that use it as a workhorse, are implemented using a
   hardware accelerator (see @code(AL_GFX_HW_HLINE)). *)
  AL_GFX_HW_HLINE_COPY_PATTERN		= $00000040;
(* Indicates that the opaque version of the @code(al_rectfill) function, the
   @code(al_clear_bitmap) routine, and @code(al_clear_to_color), are
   implemented using a hardware accelerator. *)
  AL_GFX_HW_FILL			= $00000080;
(* Indicates that the XOR version of the @code(al_rectfill) function is
   implemented using a hardware accelerator  (see @code(AL_GFX_HW_FILL)). *)
  AL_GFX_HW_FILL_XOR			= $00000100;
(* Indicates that the solid and masked pattern modes of the @code(al_rectfill)
   function is implemented using a hardware accelerator  (see
   @code(AL_GFX_HW_FILL)). *)
  AL_GFX_HW_FILL_SOLID_PATTERN		= $00000200;
(* Indicates that the copy pattern mode of the @code(al_rectfill) function
   is implemented using a hardware accelerator  (see @code(AL_GFX_HW_FILL)). *)
  AL_GFX_HW_FILL_COPY_PATTERN		= $00000400;
(* Indicates that the opaque mode @code(al_line) and @code(al_vline)
   functions are implemented using a hardware accelerator. *)
  AL_GFX_HW_LINE			= $00000800;
(* Indicates that the XOR version of the @code(al_line) and @code(al_vline)
   functions are implemented using a hardware accelerator. *)
  AL_GFX_HW_LINE_XOR			= $00001000;
{ @exclude }
  AL_GFX_HW_TRIANGLE			= $00002000;
{ @exclude }
  AL_GFX_HW_TRIANGLE_XOR		= $00004000;
(* Indicates that monochrome character expansion (for text drawing) is
   implemented using a hardware accelerator. *)
  AL_GFX_HW_GLYPH			= $00008000;
(* Indicates that blitting from one part of the screen to another is
   implemented using a hardware accelerator.  If this flag is set, blitting
   within the video memory will almost certainly be the fastest possible way to
   display an image, so it may be worth storing some of your more frequently
   used graphics in an offscreen portion of the video memory. *)
  AL_GFX_HW_VRAM_BLIT			= $00010000;
(* Indicates that the @code(al_masked_blit) routine is capable of a hardware
   accelerated copy from one part of video memory to another, and that
   @code(al_draw_sprite) will use a hardware copy when given a sub-bitmap of
   the screen or a video memory bitmap as the source image.  If this flag is
   set, copying within the video memory will almost certainly be the fastest
   possible way to display an image, so it may be worth storing some of your
   more frequently used sprites in an offscreen portion of the video memory.

   @bold(Warning:)  if this flag is not set, @code(al_masked_blit) and
   @code(al_draw_sprite) will not work correctly when used with a video memory
   source image!  You must only try to use these functions to copy within the
   video memory if they are supported in hardware. *)
  AL_GFX_HW_VRAM_BLIT_MASKED		= $00020000;
(* Indicates that blitting from a memory bitmap onto the screen is being
   accelerated in hardware. *)
  AL_GFX_HW_MEM_BLIT			= $00040000;
(* Indicates that the @code(al_masked_blit) and @code(al_draw_sprite)
   functions are being accelerated in hardware when the source image is a
   memory bitmap and the destination is the physical screen. *)
  AL_GFX_HW_MEM_BLIT_MASKED		= $00080000;
(* Indicates that blitting from a system bitmap onto the screen is being
   accelerated in hardware.  Note that some acceleration may be present even
   if this flag is not set, because system bitmaps can benefit from normal
   memory to screen blitting as well.  This flag will only be set if system
   bitmaps have further acceleration above and beyond what is provided by
   @code(AL_GFX_HW_MEM_BLIT). *)
  AL_GFX_HW_SYS_TO_VRAM_BLIT		= $00100000;
(* Indicates that the @code(al_masked_blit) and @code(al_draw_sprite)
   functions are being accelerated in hardware when the source image is a
   system bitmap and the destination is the physical screen.  Note that some
   acceleration may be present even if this flag is not set, because system
   bitmaps can benefit from normal memory to screen blitting as well.  This
   flag will only be set if system bitmaps have further acceleration above and
   beyond what is provided by @code(AL_GFX_HW_MEM_BLIT_MASKED). *)
  AL_GFX_HW_SYS_TO_VRAM_BLIT_MASKED	= $00200000;
(* Indicates that the mouse cursor is the default system cursor, not Allegro's
   custom cursor. *)
  AL_GFX_SYSTEM_CURSOR			= $00400000;



VAR
(* Bitfield describing the capabilities of the current graphics driver and
   video hardware.  This may contain combination any of the @code(AL_GFX_* )
   flags. *)
  al_gfx_capabilities: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gfx_capabilities';

(* Screen bitmap *)
  al_screen: AL_BITMAPptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'screen';
(* Screen size. *)
  AL_SCREEN_W, AL_SCREEN_H, AL_VIRTUAL_W, AL_VIRTUAL_H: LONGINT;



CONST
(* @exclude Define color conversion modes. *)
  AL_COLORCONV_NONE	= 0;

  AL_COLORCONV_8_TO_15	= 1; {< @exclude }
  AL_COLORCONV_8_TO_16	= 2; {< @exclude }
  AL_COLORCONV_8_TO_24	= 4; {< @exclude }
  AL_COLORCONV_8_TO_32	= 8; {< @exclude }

  AL_COLORCONV_15_TO_8	= $10; {< @exclude }
  AL_COLORCONV_15_TO_16	= $20; {< @exclude }
  AL_COLORCONV_15_TO_24	= $40; {< @exclude }
  AL_COLORCONV_15_TO_32	= $80; {< @exclude }

  AL_COLORCONV_16_TO_8	= $100; {< @exclude }
  AL_COLORCONV_16_TO_15	= $200; {< @exclude }
  AL_COLORCONV_16_TO_24	= $400; {< @exclude }
  AL_COLORCONV_16_TO_32	= $800; {< @exclude }

  AL_COLORCONV_24_TO_8	= $1000; {< @exclude }
  AL_COLORCONV_24_TO_15	= $2000; {< @exclude }
  AL_COLORCONV_24_TO_16	= $4000; {< @exclude }
  AL_COLORCONV_24_TO_32	= $8000; {< @exclude }

  AL_COLORCONV_32_TO_8	= $10000; {< @exclude }
  AL_COLORCONV_32_TO_15	= $20000; {< @exclude }
  AL_COLORCONV_32_TO_16	= $40000; {< @exclude }
  AL_COLORCONV_32_TO_24	= $80000; {< @exclude }

  AL_COLORCONV_32A_TO_8		= $100000; {< @exclude }
  AL_COLORCONV_32A_TO_15	= $200000; {< @exclude }
  AL_COLORCONV_32A_TO_16	= $400000; {< @exclude }
  AL_COLORCONV_32A_TO_24	= $800000; {< @exclude }

  AL_COLORCONV_DITHER_PAL	= $1000000; {< @exclude }
  AL_COLORCONV_DITHER_HI	= $2000000; {< @exclude }
  AL_COLORCONV_KEEP_TRANS	= $4000000; {< @exclude }

  AL_COLORCONV_DITHER	= AL_COLORCONV_DITHER_PAL OR AL_COLORCONV_DITHER_HI; {< @exclude }

  AL_COLORCONV_EXPAND_256	= AL_COLORCONV_8_TO_15 OR AL_COLORCONV_8_TO_16 OR AL_COLORCONV_8_TO_24 OR AL_COLORCONV_8_TO_32; {< @exclude }

  AL_COLORCONV_REDUCE_TO_256	= AL_COLORCONV_15_TO_8 OR AL_COLORCONV_16_TO_8 OR AL_COLORCONV_24_TO_8 OR AL_COLORCONV_32_TO_8 OR AL_COLORCONV_32A_TO_8; {< @exclude }

  AL_COLORCONV_EXPAND_15_TO_16	= AL_COLORCONV_15_TO_16; {< @exclude }

  AL_COLORCONV_REDUCE_16_TO_15	= AL_COLORCONV_16_TO_15; {< @exclude }

  AL_COLORCONV_EXPAND_HI_TO_TRUE = AL_COLORCONV_15_TO_24 OR AL_COLORCONV_15_TO_32 OR AL_COLORCONV_16_TO_24 OR AL_COLORCONV_16_TO_32; {< @exclude }

  AL_COLORCONV_REDUCE_TRUE_TO_HI = AL_COLORCONV_24_TO_15 OR AL_COLORCONV_24_TO_16 OR AL_COLORCONV_32_TO_15 OR AL_COLORCONV_32_TO_16; {< @exclude }

  AL_COLORCONV_24_EQUALS_32	= AL_COLORCONV_24_TO_32 OR AL_COLORCONV_32_TO_24; {< @exclude }

  AL_COLORCONV_TOTAL	= AL_COLORCONV_EXPAND_256 OR AL_COLORCONV_REDUCE_TO_256 OR AL_COLORCONV_EXPAND_15_TO_16 OR AL_COLORCONV_REDUCE_16_TO_15 OR AL_COLORCONV_EXPAND_HI_TO_TRUE OR AL_COLORCONV_REDUCE_TRUE_TO_HI OR AL_COLORCONV_24_EQUALS_32 OR AL_COLORCONV_32A_TO_15 OR AL_COLORCONV_32A_TO_16 OR AL_COLORCONV_32A_TO_24; {< @exclude }

  AL_COLORCONV_PARTIAL	= AL_COLORCONV_EXPAND_15_TO_16 OR AL_COLORCONV_REDUCE_16_TO_15 OR AL_COLORCONV_24_EQUALS_32; {< @exclude }

  AL_COLORCONV_MOST	= AL_COLORCONV_EXPAND_15_TO_16  OR AL_COLORCONV_REDUCE_16_TO_15 OR AL_COLORCONV_EXPAND_HI_TO_TRUE OR AL_COLORCONV_REDUCE_TRUE_TO_HI OR AL_COLORCONV_24_EQUALS_32; {< @exclude }

  AL_COLORCONV_KEEP_ALPHA	= AL_COLORCONV_TOTAL AND NOT (AL_COLORCONV_32A_TO_8 OR AL_COLORCONV_32A_TO_15 OR AL_COLORCONV_32A_TO_16 OR AL_COLORCONV_32A_TO_24); {< @exclude }



(* Sets the pixel format to be used by subsequent calls to
   @code(al_set_gfx_mode) and @code(al_create_bitmap).  Valid depths are 8 (the
   default), 15, 16, 24, and 32 bits.

   Note that the screen color depth won't change until the next successful
   call to @code(al_set_gfx_mode). *)
  PROCEDURE al_set_color_depth (depth: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color_depth';

(* Returns the current pixel format.  This can be very useful to know in order
   to write generic functions which select a different code path internally
   depending on the color depth being used.

   Note that the function returns whatever value you may have set previously
   with @code(al_set_color_depth), which can be different from the current
   color depth of the @code(al_screen) global variable.  If you really need to
   know the color depth of the screen, use @code(al_bitmap_color_depth). *)
  FUNCTION al_get_color_depth: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color_depth';

(* Specifies how to convert images between the various color depths when reading
   graphics from external bitmap files or datafiles.  The mode is a bitmask
   specifying which types of conversion are allowed.  If the appropriate bit is
   set, data will be converted into the current pixel format (selected by
   calling the @code(al_set_color_depth) function), otherwise it will be left
   in the same format as the disk file, leaving you to convert it manually
   before the graphic can be displayed.  The default mode is total conversion,
   so that all images will be loaded in the appropriate format for the current
   video mode. Valid bit flags are:
   @longcode(#
          AL_COLORCONV_NONE                // disable all format
                                           // conversions
          AL_COLORCONV_8_TO_15             // expand 8-bit to 15-bit
          AL_COLORCONV_8_TO_16             // expand 8-bit to 16-bit
          AL_COLORCONV_8_TO_24             // expand 8-bit to 24-bit
          AL_COLORCONV_8_TO_32             // expand 8-bit to 32-bit
          AL_COLORCONV_15_TO_8             // reduce 15-bit to 8-bit
          AL_COLORCONV_15_TO_16            // expand 15-bit to 16-bit
          AL_COLORCONV_15_TO_24            // expand 15-bit to 24-bit
          AL_COLORCONV_15_TO_32            // expand 15-bit to 32-bit
          AL_COLORCONV_16_TO_8             // reduce 16-bit to 8-bit
          AL_COLORCONV_16_TO_15            // reduce 16-bit to 15-bit
          AL_COLORCONV_16_TO_24            // expand 16-bit to 24-bit
          AL_COLORCONV_16_TO_32            // expand 16-bit to 32-bit
          AL_COLORCONV_24_TO_8             // reduce 24-bit to 8-bit
          AL_COLORCONV_24_TO_15            // reduce 24-bit to 15-bit
          AL_COLORCONV_24_TO_16            // reduce 24-bit to 16-bit
          AL_COLORCONV_24_TO_32            // expand 24-bit to 32-bit
          AL_COLORCONV_32_TO_8             // reduce 32-bit RGB to 8-bit
          AL_COLORCONV_32_TO_15            // reduce 32-bit RGB to 15-bit
          AL_COLORCONV_32_TO_16            // reduce 32-bit RGB to 16-bit
          AL_COLORCONV_32_TO_24            // reduce 32-bit RGB to 24-bit
          AL_COLORCONV_32A_TO_8            // reduce 32-bit RGBA to 8-bit
          AL_COLORCONV_32A_TO_15           // reduce 32-bit RGBA to 15-bit
          AL_COLORCONV_32A_TO_16           // reduce 32-bit RGBA to 16-bit
          AL_COLORCONV_32A_TO_24           // reduce 32-bit RGBA to 24-bit
          AL_COLORCONV_DITHER_PAL          // dither when reducing to 8-bit
          AL_COLORCONV_DITHER_HI           // dither when reducing to
                                           // hicolor
          AL_COLORCONV_KEEP_TRANS          // keep original transparency
   #)
   For convenience, the following macros can be used to select common
   combinations of these flags:
   @longcode(#
          AL_COLORCONV_EXPAND_256          // expand 256-color to hi/truecolor
          AL_COLORCONV_REDUCE_TO_256       // reduce hi/truecolor to 256-color
          AL_COLORCONV_EXPAND_15_TO_16     // expand 15-bit hicolor to 16-bit
          AL_COLORCONV_REDUCE_16_TO_15     // reduce 16-bit hicolor to 15-bit
          AL_COLORCONV_EXPAND_HI_TO_TRUE   // expand 15/16-bit to 24/32-bit
          AL_COLORCONV_REDUCE_TRUE_TO_HI   // reduce 24/32-bit to 15/16-bit
          AL_COLORCONV_24_EQUALS_32        // convert between 24- and 32-bit
          AL_COLORCONV_TOTAL               // everything to current format
          AL_COLORCONV_PARTIAL             // convert 15 <-> 16-bit and
                                           // 24 <-> 32-bit
          AL_COLORCONV_MOST                // all but hi/truecolor <-> 256
          AL_COLORCONV_DITHER              // dither during all color reductions
          AL_COLORCONV_KEEP_ALPHA          // convert everything to current format
                                           // unless it would lose alpha information
   #)
   If you enable the @code(AL_COLORCONV_DITHER) flag, dithering will be
   performed whenever truecolor graphics are converted into a hicolor or
   paletted format, including by the @code(al_blit) function, and any
   automatic conversions that take place while reading graphics from disk.
   This can produce much better looking results, but is obviously slower than a
   direct conversion.

   If you intend using converted bitmaps with functions like
   @code(al_masked_blit) or @code(al_draw_sprite), you should specify the
   @code(AL_COLORCONV_KEEP_TRANS) flag.  It will ensure that the masked areas
   in the bitmap before and after the conversion stay exactly the same, by
   mapping transparent colors to each other and adjusting colors which would be
   converted to the transparent color otherwise.  It affects every
   @code(al_blit) operation between distinct pixel formats and every automatic
   conversion. *)
  PROCEDURE al_set_color_conversion (mode: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color_conversion';

(* Returns the current color conversion mode. *)
  FUNCTION al_get_color_conversion: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color_conversion';

(* Switches into graphics mode.  The card parameter should usually be one of
   the Allegro magic drivers (read introduction of this unit) or see the
   platform specific documentation for a list of the available drivers.  The
   @code(w) and @code(h) parameters specify what screen resolution you want.
   The color depth of the graphic mode has to be specified before calling this
   function with @code(al_set_color_depth).

   The @code(v_w) and @code(v_h) parameters specify the minimum virtual
   screen size, in case you need a large virtual screen for hardware scrolling
   or page flipping.  You should set them to zero if you don't care about the
   virtual screen size.

   When you call @code(al_set_gfx_mode), the @code(v_w) and @code(v_h)
   parameters represent the minimum size of virtual screen that is acceptable
   for your program.  The range of possible sizes is usually very restricted,
   and Allegro may end up creating a virtual screen much larger than the one
   you request.  Allowed sizes are driver dependent and some drivers do not
   allow virtual screens that are larger than the visible screen at all:  don't
   assume that whatever you pass will always work.

   Currently, using a big virtual screen for page flipping is considered bad
   practice.  There are platforms which don't support virtual screens bigger
   than the physical screen but can create different video pages to flip back
   and forth.  This means that, if you want page flipping and aren't going to
   use hardware scrolling, you should call @code(al_set_gfx_mode) with (0,0)
   as the virtual screen size and later create the different video pages with
   @code(al_create_video_bitmap).  Otherwise your program will be limited to
   the platforms supporting hardware scrolling.

   After you select a graphics mode, the physical and virtual screen sizes can
   be checked with the variables @code(AL_SCREEN_W), @code(AL_SCREEN_H),
   @code(AL_VIRTUAL_W), and @code(AL_VIRTUAL_H).

   @returns(@true on success.  On failure returns @false and stores a
     description of the problem in @code(al_error).) *)
  FUNCTION al_set_gfx_mode (card, w, h, v_w, v_h: LONGINT): BOOLEAN;

(* Shortcut version of @code(al_acquire_bitmap @(screen@);)
   @seealso(al_acquire_bitmap) *)
  PROCEDURE al_acquire_screen;
    INLINE;

(* Shortcut version of @code(al_release_bitmap @(screen@);)
   @seealso(al_release_bitmap) *)
  PROCEDURE al_release_screen;
    INLINE;

(* Attempts to page flip the hardware screen to display the specified video
   bitmap object, which must be the same size as the physical screen, and
   should have been obtained by calling the @code(al_create_video_bitmap)
   function.

   Allegro will handle any necessary vertical retrace synchronisation when page
   flipping, so you don't need to call @code(al_vsync) before it.  This means
   that @code(al_show_video_bitmap) has the same time delay effects as
   @code(al_vsync) by default.  This can be adjusted with the "disable_vsync"
   config key in the @code([graphics]) section of allegro.cfg.

   @returns(zero on success and non-zero on failure.) *)
  FUNCTION al_show_video_bitmap (bmp: AL_BITMAPptr): LONGINT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'show_video_bitmap';

(* Waits for a vertical retrace to begin.  The retrace happens when the
   electron beam in your monitor has reached the bottom of the screen and is
   moving back to the top ready for another scan.  During this short period the
   graphics card isn't sending any data to the monitor, so you can do things to
   it that aren't possible at other times, such as altering the palette without
   causing flickering (snow).  Allegro will automatically wait for a retrace
   before altering the palette or doing any hardware scrolling, though, so you
   don't normally need to bother with this function. *)
  PROCEDURE al_vsync; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'vsync';


(**********************
 * Drawing primitives *
 **********************)

(* Each bitmap has an associated clipping rectangle, which is the area of the
   image that it is OK to draw onto.  Nothing will be drawn to positions
   outside this space.  This function sets the clipping rectangle for the
   specified bitmap.  Pass the coordinates of the top-left and bottom-right
   corners of the clipping rectangle in this order;  these are both inclusive,
   i.e. @code(al_set_clip_rect @(bitmap, 16, 16, 32, 32@)) will allow drawing
   to (16, 16) and (32, 32), but not to (15, 15) and (33, 33).

   Drawing operations will be performed (at least partially) on the bitmap as
   long as the first coordinates of its clipping rectangle are not greater than
   the second coordinates and its intersection with the actual image is
   non-empty.  If either condition is not fulfilled, drawing will be turned off
   for the bitmap, e.g.: @code(al_set_clip_rect @(bmp, 0, 0, -1, -1@)) will
   disable drawing on @code(bmp).

   Note that passing "out-of-bitmap" coordinates is allowed, but they are
   likely to be altered (and so the coordinates returned by
   @code(al_get_clip_rect) will be different).  However, such modifications are
   guaranteed to preserve the external effect of the clipping rectangle, that
   is not to modify the actual area of the image that it is OK to draw onto. *)
  PROCEDURE al_set_clip_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_clip_rect';

(* Sets the clipping rectangle of the specified bitmap as the intersection of
   its current clipping rectangle and the rectangle described by the four
   coordinates. *)
  PROCEDURE al_add_clip_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'add_clip_rect';

(* Returns the clipping rectangle for the specified bitmap.  *)
  PROCEDURE al_get_clip_rect (bmp: AL_BITMAPptr; VAR x1, y1, x2, y2: LONGINT);

(* Turns on (if state is @true) or off (if state is @false) clipping for the
   specified bitmap.  Turning clipping off may slightly speed up some drawing
   operations (usually a negligible difference, although every little helps)
   but will result in your program dying a horrible death if you try to draw
   beyond the edges of the bitmap. *)
  PROCEDURE al_set_clip_state (bmp: AL_BITMAPptr; state: BOOLEAN);
    INLINE;



(* Drawing modes. *)
CONST
(* Flag for @code(al_drawing_mode).

   The pixels of the bitmap being drawn onto are simply replaced by those
   produced by the drawing function. *)
  AL_DRAW_MODE_SOLID		= 0;
(* Flag for @code(al_drawing_mode).

   Pixels are written to the bitmap with an exclusive-or operation rather than
   a simple copy, so drawing the same shape twice will erase it.  Because it
   involves reading as well as writing the bitmap memory, xor drawing is a lot
   slower than the normal replace mode. *)
  AL_DRAW_MODE_XOR		= 1;
(* Flag for @code(al_drawing_mode).

   Pixels are simply copied from the pattern bitmap onto the destination
   bitmap.  This allows the use of multicolored patterns, and means that the
   color you pass to the drawing routine is ignored.  This is the fastest of
   the patterned modes. *)
  AL_DRAW_MODE_COPY_PATTERN	= 2;
(* Flag for @code(al_drawing_mode).

   Each pixel in the pattern bitmap is compared with the mask color, which is
   zero in 256-color modes or bright pink for truecolor data (maximum red and
   blue, zero green).  If the pattern pixel is solid, a pixel of the color you
   passed to the drawing routine is written to the destination bitmap,
   otherwise a zero is written.  The pattern is thus treated as a monochrome
   bitmask, which lets you use the same pattern to draw different shapes in
   different colors, but prevents the use of multicolored patterns. *)
  AL_DRAW_MODE_SOLID_PATTERN	= 3;
(* Flag for @code(al_drawing_mode).

   It is almost the same as @code(AL_DRAW_MODE_SOLID_PATTERN), but the masked
   pixels are skipped rather than being written as zeros, so the background
   shows through the gaps. *)
  AL_DRAW_MODE_MASKED_PATTERN	= 4;
(* Flag for @code(al_drawing_mode).

   The global @code(al_color_table) table or truecolor blender functions are
   used to overlay pixels on top of the existing image.  This must only be used
   after you have set up the color mapping table (for 256 color modes) or
   blender functions (for truecolor modes).  Because it involves reading as
   well as writing the bitmap memory, translucent drawing is very slow if you
   draw directly to video RAM, so wherever possible you should use a memory
   bitmap instead. *)
  AL_DRAW_MODE_TRANS		= 5;



(* Sets the graphics drawing mode.  This only affects the geometric routines
   like @code(al_putpixel), lines, rectangles, circles, polygons, floodfill,
   etc, not the text output, blitting, or sprite drawing functions.  The mode
   should be one of the following constants:
@unorderedList(
  @item(@code(AL_DRAW_MODE_SOLID))
  @item(@code(AL_DRAW_MODE_XOR))
  @item(@code(AL_DRAW_MODE_COPY_PATTERN))
  @item(@code(AL_DRAW_MODE_SOLID_PATTERN))
  @item(@code(AL_DRAW_MODE_MASKED_PATTERN))
  @item(@code(AL_DRAW_MODE_TRANS))
)
   With the patterned modes, you provide a pattern bitmap which is tiled across
   the surface of the shape.  Allegro stores a pointer to this bitmap rather
   than copying it, so you must not destroy the bitmap while it is still
   selected as the pattern.  The width and height of the pattern must be powers
   of two, but they can be different, eg. a 64x16 pattern is fine, but a 17x3
   one is not.  The pattern is tiled in a grid starting at point
   @code(@(x_anchor, y_anchor@)).  Normally you should just pass zero for these
   values, which lets you draw several adjacent shapes and have the patterns
   meet up exactly along the shared edges.  Zero alignment may look peculiar if
   you are moving a patterned shape around the screen, however, because the
   shape will move but the pattern alignment will not, so in some situations
   you may wish to alter the anchor position. *)
  PROCEDURE al_drawing_mode (mode: LONGINT; pattern: AL_BITMAPptr;
			     x_anchor, y_anchor: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'drawing_mode';

(* This is a shortcut for toggling xor drawing mode on and off.  Calling
   @code(al_xor_mode @(1@)) is equivalent to @code(al_drawing_mode)
   @code(@(AL_DRAW_MODE_XOR, NIL, 0, 0@)).  Calling @code(al_xor_mode @(0@)) is
   equivalent to @code(al_drawing_mode @(A_DRAW_MODE_SOLID, NIL, 0, 0@)). *)
  PROCEDURE al_xor_mode (aOn: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'xor_mode';

(* This is a shortcut for selecting solid drawing mode.  It is equivalent to
   calling @code(al_drawing_mode) @code(@(AL_DRAW_MODE_XOR, NIL, 0, 0@)). *)
  PROCEDURE al_solid_mode; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'solid_mode';



(* Clears the bitmap to color 0. *)
  PROCEDURE al_clear_bitmap (bitmap: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_bitmap';

(* Clears the bitmap to the specified color. *)
  PROCEDURE al_clear_to_color (bitmap: AL_BITMAPptr; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_to_color';

(* Reads a pixel from point (x, y) in the bitmap.

   @returns(-1 if the point lies outside the bitmap @(ignoring the clipping
     rectangle@), otherwise the value of the pixel in the color format of the
     bitmap.

     @bold(Warning:) -1 is also a valid value for pixels contained in 32-bit
     bitmaps with alpha channel @(when R,G,B,A are all equal to 255@) so you
     can't use the test against -1 as a predicate for such bitmaps.  In this
     cases, the only reliable predicate is check if it is inside the bitmap.

     To extract the individual color components, use the getr() / getg() / getb() / geta() family of functions) *)
  FUNCTION  al_getpixel (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
    INLINE;

(* Faster inline version of @code(al_getpixel).  This is specific for 8 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 8bpp) *)
  FUNCTION _al_getpixel (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
    INLINE;

(* Faster inline version of @code(al_getpixel).  This is specific for 15 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 15bpp) *)
  FUNCTION _al_getpixel15 (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
    INLINE;

(* Faster inline version of @code(al_getpixel).  This is specific for 16 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 16bpp) *)
  FUNCTION _al_getpixel16 (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
    INLINE;

(* Faster inline version of @code(al_getpixel).  This is specific for 24 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 24bpp) *)
  FUNCTION _al_getpixel24 (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
    INLINE;

(* Faster inline version of @code(al_getpixel).  This is specific for 32 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 32bpp) *)
  FUNCTION _al_getpixel32 (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
    INLINE;

(* Writes a pixel to the specified position in the bitmap, using the current
   drawing mode and the bitmap's clipping rectangle. *)
  PROCEDURE al_putpixel (bmp: AL_BITMAPptr; x, y, color: LONGINT);
    INLINE;

(* Like the regular @code(al_putpixel), but much faster because it's
   implemented as an inline functions for specific 8 bits color depth.  It
   don't perform any clipping (they will crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel (bmp: AL_BITMAPptr; x, y, color: LONGINT);
    INLINE;

(* Like the regular @code(al_putpixel), but much faster because it's
   implemented as an inline functions for specific 15 bits color depth.  It
   don't perform any clipping (they will crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel15 (bmp: AL_BITMAPptr; x, y, color: LONGINT);
    INLINE;

(* Like the regular @code(al_putpixel), but much faster because it's
   implemented as an inline functions for specific 16 bits color depth.  It
   don't perform any clipping (they will crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel16 (bmp: AL_BITMAPptr; x, y, color: LONGINT);
    INLINE;

(* Like the regular @code(al_putpixel), but much faster because it's
   implemented as an inline functions for specific 24 bits color depth.  It
   don't perform any clipping (they will crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel24 (bmp: AL_BITMAPptr; x, y, color: LONGINT);
    INLINE;

(* Like the regular @code(al_putpixel), but much faster because it's
   implemented as an inline functions for specific 32 bits color depth.  It
   don't perform any clipping (they will crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel32 (bmp: AL_BITMAPptr; x, y, color: LONGINT);
    INLINE;

(* Draws a vertical line onto the bitmap, from point (x, y1) to (x, y2). *)
  PROCEDURE al_vline (bmp: AL_BITMAPptr; x, y1, y2, color: LONGINT);
    INLINE;

(* Draws a horizontal line onto the bitmap, from point (x1, y) to (x2, y). *)
  PROCEDURE al_hline	   (bmp: AL_BITMAPptr; x1, y, x2, color: LONGINT);
    INLINE;

(* Draws a line onto the bitmap, from point (x1, y1) to (x2, y2).
   @seealso(al_fastline) @seealso(al_polygon) @seealso(al_do_line). *)
  PROCEDURE al_line	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
    INLINE;

(* Faster version of the previous function.  Note that pixel correctness is not
   guaranteed for this function. @seealso(al_line) *)
  PROCEDURE al_fastline	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
    INLINE;

(* Draws an outline rectangle with the two points as its opposite corners. *)
  PROCEDURE al_rect	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
    INLINE;

(* Draws a solid, filled rectangle with the two points as its opposite
   corners. *)
  PROCEDURE al_rectfill	   (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
    INLINE;

(* Draws a circle with the specified centre and radius. @seealso(al_ellipse)
   @seealso(al_circlefill) @seealso(al_do_circle) @seealso(al_arc) *)
  PROCEDURE al_circle	   (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);
    INLINE;

(* Draws a filled circle with the specified centre and radius.
   @seealso(al_circle) *)
  PROCEDURE al_circlefill  (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);
    INLINE;

(* Draws an ellipse with the specified centre and radius. @seealso(al_circle)
   @seealso(al_ellipsefill) @seealso(al_arc) @seealso(al_do_ellipse) *)
  PROCEDURE al_ellipse	   (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);
    INLINE;

(* Draws a filled ellipse with the specified centre and radius.
   @seealso(al_ellipse) *)
  PROCEDURE al_ellipsefill (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);
    INLINE;

(* Draws a circular arc.

  Draws a circular arc with centre x, y and radius r, in an anticlockwise
  direction starting from the angle a1 and ending when it reaches a2.  These
  values are specified in 16.16 fixed point format, with 256 equal to a full
  circle, 64 a right angle, etc.  Zero is to the right of the centre point, and
  larger values rotate anticlockwise from there.
  @seealso(al_circle) @seealso(al_do_arc) *)
  PROCEDURE al_arc (bmp: AL_BITMAPptr; x, y: LONGINT; ang1, ang2: AL_FIXED; r, color: LONGINT);
    INLINE;

(* Floodfills an enclosed area, starting at point (x, y), with the specified
   color. *)
  PROCEDURE al_floodfill   (bmp: AL_BITMAPptr; x, y, color: LONGINT);
    INLINE;

(* Draws a filled polygon with an arbitrary number of corners.  Pass the number
   of vertices and an array containing a series of x, y points (a total of
   vertices*2 values).
   @seealso(al_triangle) @seealso(al_polygon3d) @seealso(al_drawing_mode) *)
  PROCEDURE al_polygon (bmp: AL_BITMAPptr; vertices: LONGINT; CONST points: ARRAY OF LONGINT; color: LONGINT);
    INLINE;

(* Draws a filled triangle between the three points.
   @seealso(al_polygon) @seealso(al_triangle3d) @seealso(al_drawing_mode) *)
  PROCEDURE al_triangle (bmp: AL_BITMAPptr; x1, y1, x2, y2, x3, y3, color: LONGINT);
    INLINE;

(* Draws a Bezier spline using the four control points specified in the points
   array.  Read the description of al_calc_spline for information on how to
   build the points array.
   @seealso(al_calc_spline) *)
  PROCEDURE al_spline (bmp: AL_BITMAPptr; CONST points: ARRAY OF LONGINT; color: LONGINT);
    INLINE;

(* Calculates all the points along a line from point (x1, y1) to (x2, y2),
   calling the supplied function for each one.  This will be passed a copy of
   the bmp parameter, the x and y position, and a copy of the d parameter, so
   it is suitable for use with al_putpixel. Example:
@longcode(#
PROCEDURE DrawDustParticle (bmp: AL_BITMAPptr; x, y, d: LONGINT); CDECL;
BEGIN
   ...
END;

  al_do_line (al_screen, 0, 0, AL_SCREEN_W-1, AL_SCREEN_H-2,
              DustStrength, @DrawDustParticle);
#) @seealso(al_do_circle) @seealso(al_do_ellipse) @seealso(al_do_arc) *)
  PROCEDURE al_do_line (bmp: AL_BITMAPptr; x1, y1, x2, y2, d: LONGINT; proc: AL_POINT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_line';

(* Calculates all the points in a circle around point (x, y) with radius r,
   calling the supplied function for each one.  This will be passed a copy of
   the bmp parameter, the x and y position, and a copy of the d parameter, so
   it is suitable for use with al_putpixel. Example:
@longcode(#
PROCEDURE DrawExplosionRing (bmp: AL_BITMAPptr; x, y, d: LONGINT); CDECL;
BEGIN
   ...
END;

  al_do_circle (al_screen, AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
                AL_SCREEN_H DIV 16, FlameColor, @DrawExplosionRing);
#) @seealso(al_do_line) @seealso(al_do_ellipse) @seealso(al_do_arc) *)
  PROCEDURE al_do_circle (bmp: AL_BITMAPptr; x, y, radius, d: LONGINT; proc: AL_POINT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_circle';

(* Calculates all the points in an ellipse around point (x, y) with radius rx
   and ry, calling the supplied function for each one.  This will be passed a
   copy of the bmp parameter, the x and y position, and a copy of the d
   parameter, so it is suitable for use with al_putpixel. Example:
@longcode(#
PROCEDURE DrawExplosionRing (bmp: AL_BITMAPptr; x, y, d: LONGINT); CDECL;
BEGIN
   ...
END;

  al_do_ellipse (al_screen, AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
                AL_SCREEN_H DIV 16, FlameColor, @DrawExplosionRing);
#) @seealso(al_do_line) @seealso(al_do_circle) @seealso(al_do_arc) *)
  PROCEDURE al_do_ellipse (bmp: AL_BITMAPptr; x, y, rx, ry, d: LONGINT; proc: AL_POINT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_ellipse';

(* Calculates all the points in a circular arc around point (x, y) with radius
   r, calling the supplied function for each one.  This will be passed a copy
   of the bmp parameter, the x and y position, and a copy of the d parameter,
   so it is suitable for use with al_putpixel.  The arc will be plotted in an
   anticlockwise direction starting from the angle a1 and ending when it
   reaches a2.  These values are specified in 16.16 fixed point format, with
   256 equal to a full circle, 64 a right angle, etc. Zero is to the right of
   the centre point, and larger values rotate anticlockwise from there.
   Example:
@longcode(#
PROCEDURE DrawExplosionRing (bmp: AL_BITMAPptr; x, y, d: LONGINT); CDECL;
BEGIN
   ...
END;

  al_do_arc (al_screen, AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
                AL_SCREEN_H DIV 16, FlameColor, @DrawExplosionRing);
#) @seealso(al_do_line) @seealso(al_do_circle) @seealso(al_do_ellipse) *)
  PROCEDURE al_do_arc (bmp: AL_BITMAPptr; x, y: LONGINT; ang1, ang2: AL_FIXED; r, d: LONGINT; proc: AL_POINT_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_arc';

(* Calculates a series of npts values along a Bezier spline, storing them in
   the output x and y arrays.  The Bezier curve is specified by the four x/y
   control points in the points array:  points[0] and points[1] contain the
   coordinates of the first control point, points[2] and points[3] are the
   second point, etc.  Control points 0 and 3 are the ends of the spline, and
   points 1 and 2 are guides.  The curve probably won't pass through points 1
   and 2, but they affect the shape of the curve between points 0 and 3 @(the
   lines p0-p1 and p2-p3 are tangents to the spline@).  The easiest way to
   think of it is that the curve starts at p0, heading in the direction of p1,
   but curves round so that it arrives at p3 from the direction of p2.  In
   addition to their role as graphics primitives, spline curves can be useful
   for constructing smooth paths around a series of control points, as in
   exspline.pp.
   @seealso(al_spline) *)
  PROCEDURE al_calc_spline (CONST points: ARRAY OF LONGINT; npts: LONGINT; VAR x, y: ARRAY OF LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'calc_spline';



(*************
 * Text font *
 *************)

TYPE
(* A pointer to a structure holding an Allegro font, usually created beforehand
   with the grabber tool or Allegro's default font.  Read introduction of
   @code(alfont) unit for a description on how to load/destroy fonts, and unit
   @code(altext) for a description on how to show text. *)
  AL_FONTptr = POINTER;



(* Loads a font from a file.  At present, this supports loading fonts from a
   GRX format .fnt file, a 8x8 or 8x16 BIOS format .fnt file, a datafile or any
   bitmap format that can be loaded by @code(al_load_bitmap).

   If the font contains palette information, then the palette is returned in
   the second parameter, which should be an array of 256 @code(AL_RGB)
   structures (a @code(AL_PALETTE)).  The @code(pal) argument may be @nil.
   In this case, the palette data, if present, is simply not returned.

   Note that another way of loading fonts is embedding them into a datafile and
   using the datafile related functions.

   @returns(a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.) *)
  FUNCTION al_load_font (filename: STRING; palette: AL_PALETTEptr; p: POINTER)
	: AL_FONTptr;

(* Tries to grab a font from a bitmap.  The bitmap can be in any format that
   @code(al_load_bitmap) understands.

   The size of each character is determined by the layout of the image, which
   should be a rectangular grid containing all the ASCII characters from space
   (32) up to the tilde (126).  The way the characters are separated depends on
   the color depth of the image file:
   @unorderedList(
     @item(paletted @(8 bit@) image file Use color 0 for the transparent
       portions of the characters and fill the spaces between each letter with
       color 255.)
     @item(High @(15/16 bit@) and true @(24/32 bit@) color image file use
       bright pink @(maximum red and blue, zero green@) for the transparent
       portions of the characters and fill the spaces between each letter with
       bright yellow @(maximum red and green, zero blue@).)
   )
   Note that in each horizontal row the bounding boxes around the characters
   should align and have the same height.

   Probably the easiest way to get to grips with how this works is to load up
   the `demo.dat' file and export the TITLE_FONT into a PCX file.  Have a look
   at the resulting picture in your paint program:  that is the format a font
   should be in.

   Take care with high and true color fonts:  Allegro will convert these to the
   current color depth when you load the font.  If you try to use a font on a
   bitmap with a different color depth Allegro will do color conversions on the
   fly, which will be rather slow.  For optimal performance you should set the
   color depth to the color depth you want to use before loading any fonts.

   @returns(a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.)
   @seealso(al_load_bmp_pf) *)
  FUNCTION al_load_bitmap_font (filename: STRING; palette: AL_PALETTEptr;
    p: POINTER): POINTER;


(* This function is the work-horse of @code(al_load_bitmap_font), and can be
   used to grab a font from a bitmap in memory.  You can use this if you want
   to generate or modify a font at runtime.  The bitmap should follow the
   layout described for @code(al_load_bitmap_font).

   @returns(a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.) *)
  FUNCTION al_grab_font_from_bitmap (bmp: AL_BITMAPptr): AL_FONTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'grab_font_from_bitmap';

(* This function checks if the given font is a color font, as opposed to a
   monochrome font.
   @returns(@true if the font is a color font, @false if it is not.) *)
  FUNCTION al_is_color_font (f: AL_FONTptr): BOOLEAN;
    INLINE;

(* This function checks if the given font is a mono font, as opposed to a
   color font.
   @returns(@true if the font is a monochrome font, @false if it is not.) *)
  FUNCTION al_is_mono_font (f: AL_FONTptr): BOOLEAN;
    INLINE;

(* This function compares the two fonts, which you can use to find out if
   Allegro is capable of merging them.

   @returns(@true if the two fonts are of the same general type @(both are
     color fonts or both are monochrome fonts, for instance@).) *)
  FUNCTION al_is_compatible_font (f1, f2: AL_FONTptr): BOOLEAN;
    INLINE;

(* Frees the memory being used by a font structure.  Don't use this on the
   default global Allegro font or any text routines using it could crash.  You
   should use this only on fonts you have loaded manually after you are done
   with them, to prevent memory leaks in your program. *)
  PROCEDURE al_destroy_font (f: AL_FONTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_font';



(****************
 * Text drawing *
 ****************)

VAR
(* A simple 8x8 fixed size font (the mode 13h BIOS default).  This font
   contains the standard ASCII (U+20 to U+7F), Latin-1 (U+A1 to U+FF), and
   Latin Extended-A (U+0100 to U+017F) character ranges. *)
  al_font: AL_FONTptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'font';
(* When Allegro cannot find a glyph it needs in a font, it will instead output
   the character given in this variable.  By default, this is set to the caret
   symbol, @code(^), but you can change this global to use any other character
   instead.*)
  al_404_char: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_404_char';



(* Writes the string onto the bitmap at given position, using the specified
   font, foreground color and background color.  If the background color is -1,
   then the text is written transparently.  If the foreground color is -1 and a
   color font is in use, it will be drawn using the colors from the original
   font bitmap (the one you imported into the grabber program), which allows
   multicolored text output.  For high and true color fonts, the foreground
   color is ignored and always treated as -1.
   @param(bmp The output bitmap.)
   @param(f The font to render.)
   @param(x Horizontal position.) @param(y Vertical position.)
   @param(color Foreground color.  Set to -1 to use multicolor fonts.)
   @param(bg Background color.  Set to -1 to use transparent background.)
   @seealso(al_textout_centre_ex) @seealso(al_textout_right_ex) @seealso(al_textout_justify_ex)*)
  PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
    INLINE;
(* Like @code(al_textout_ex), but interprets the @code(x) coordinate as the
   centre rather than the left edge of the string.
   @seealso(al_textout_ex) @seealso(al_textout_right_ex) @seealso(al_textout_justify_ex)*)
  PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
    INLINE;
(* Like @code(al_textout_ex), but interprets the @code(x) coordinate as the
   right rather than the left edge of the string.
   @seealso(al_textout_ex) @seealso(al_textout_centre_ex) @seealso(al_textout_justify_ex)*)
  PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
    INLINE;
(* Draws justified text within the region @code(x1-x2).  If the amount of spare
   space is greater than the @code(diff) value, it will give up and draw
   regular left justified text instead.
   @seealso(al_textout_ex) @seealso(al_textout_centre_ex) @seealso(al_textout_right_ex)*)
  PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x1, x2, y, diff, color, bg: LONGINT);
    INLINE;



(* Returns the length (in pixels) of a string in the specified font. *)
  FUNCTION al_text_length (f: AL_FONTptr; str: STRING): LONGINT;
    INLINE;
(* Returns the height (in pixels) of the specified font. *)
  FUNCTION al_text_height (f: AL_FONTptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'text_height';



(*******************************
 * Blitting and sprite drawing *
 *******************************)

CONST
(* Drawing modes for al_draw_sprite_ex. *)
  AL_DRAW_SPRITE_NORMAL_MODE = 0; {< @exclude }
  AL_DRAW_SPRITE_LIT_MODE    = 1; {< @exclude }
  AL_DRAW_SPRITE_TRANS_MODE  = 2; {< @exclude }


(* Flipping modes for al_draw_sprite_ex. *)
  AL_DRAW_SPRITE_NO_FLIP_MODE = 0; {< @exclude }
  AL_DRAW_SPRITE_H_FLIP_MODE  = 1; {< @exclude }
  AL_DRAW_SPRITE_V_FLIP_MODE  = 2; {< @exclude }
  AL_DRAW_SPRITE_VH_FLIP_MODE = 3; {< @exclude }



(* Copies a rectangular area of the source bitmap to the destination bitmap.
   The @code(source_x) and @code(source_y) parameters are the top left corner
   of the area to copy from the source bitmap, and @code(dest_x) and
   @code(dest_y) are the corresponding position in the destination bitmap. This
   routine respects the destination clipping rectangle, and it will also clip
   if you try to blit from areas outside the source bitmap.

   You can blit between any parts of any two bitmaps, even if the two memory
   areas overlap (ie. source and dest are the same, or one is sub-bitmap of the
   other).  You should be aware, however, that a lot of SVGA cards don't
   provide separate read and write banks, which means that blitting from one
   part of the screen to another requires the use of a temporary bitmap in
   memory, and is therefore extremely slow.  As a general rule you should avoid
   blitting from the screen onto itself in SVGA modes.

   If the @code(AL_GFX_HW_VRAM_BLIT) bit in the @code(al_gfx_capabilities)
   flag is set, the current driver supports hardware accelerated blits from one
   part of the screen onto another.  This is extremely fast, so when this flag
   is set it may be worth storing some of your more frequently used graphics in
   an offscreen portion of the video memory.

   Unlike most of the graphics routines, @code(al_blit) allows the source and
   destination bitmaps to be of different color depths, so it can be used to
   convert images from one pixel format to another.  In this case, the behavior
   is affected by the @code(AL_COLORCONV_KEEP_TRANS)
   and @code(AL_COLORCONV_DITHER* ) flags of the current color conversion mode.
   @seealso(al_set_color_conversion) *)
  PROCEDURE al_blit (source, dest: AL_BITMAPptr; source_x, source_y, dest_x, dest_y, width, height: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'blit';

(* Like @code(al_blit), except it can scale images (so the source and
   destination rectangles don't need to be the same size) and requires the
   source and destination bitmaps to be of the same color depth.  This routine
   doesn't do as much safety checking as the regular @code(al_blit):  in
   particular you must take care not to copy from areas outside the source
   bitmap, and you cannot blit between overlapping regions, ie. you must use
   different bitmaps for the source and the destination.  Moreover, the source
   must be a memory bitmap. *)
  PROCEDURE al_stretch_blit (source, dest: AL_BITMAPptr; source_x, source_y, source_width, source_height, dest_x, dest_y, dest_width, dest_height: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stretch_blit';

(* Like @code(al_blit), but skips transparent pixels, which are marked by a
   zero in 256-color modes or bright pink for truecolor data (maximum red and
   blue, zero green), and requires the source and destination bitmaps to be of
   the same color depth.  The source and destination regions must not overlap.

   If the @code(AL_GFX_HW_VRAM_BLIT_MASKED) bit in the
   @code(al_gfx_capabilities) flag is set, the current driver supports hardware
   accelerated masked blits from one part of the screen onto another.  This is
   extremely fast, so when this flag is set it may be worth storing some of
   your more frequently used sprites in an offscreen portion of the video
   memory.

   @bold(Warning:)  if the hardware acceleration flag is not set,
   @code(masked_blit) will not work correctly when used with a source image in
   system or video memory so the latter must be a memory bitmap. *)
  PROCEDURE al_masked_blit (source, dest: AL_BITMAPptr; source_x, source_y, dest_x, dest_y, width, height: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'masked_blit';

(* Like @code(al_masked_blit), except it can scale images (so the source and
   destination rectangles don't need to be the same size).  This routine
   doesn't do as much safety checking as the regular @code(al_masked_blit):
   in particular you must take care not to copy from areas outside the source
   bitmap.  Moreover, the source must be a memory bitmap. *)
  PROCEDURE al_masked_stretch_blit (source, dest: AL_BITMAPptr; source_x, source_y, source_width, source_height, dest_x, dest_y, dest_width, dest_height: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'masked_stretch_blit';



(* Draws a copy of the sprite bitmap onto the destination bitmap at the
   specified position.  This is almost the same as @code(al_blit)
   @code(@(sprite, bmp, 0, 0, x, y, sprite^.w, sprite^.h@)), but it uses a
   masked drawing mode where transparent pixels are skipped, so the background
   image will show through the masked parts of the sprite.  Transparent pixels
   are marked by a zero in 256-color modes or bright pink for truecolor data
   (maximum red and blue, zero green). Example:
@longcode(#
VAR
  SpaceShip: AL_BITMAPptr;

          ...
  al_draw_sprite (al_screen, SpaceShip, x, y);
  #)

  If the @code(AL_GFX_HW_VRAM_BLIT_MASKED) bit in the
  @code(al_gfx_capabilities) flag is set, the current driver supports hardware
  accelerated sprite drawing when the source image is a video memory bitmap or
  a sub-bitmap of the screen.  This is extremely fast, so when this flag is set
  it may be worth storing some of your more frequently used sprites in an
  offscreen portion of the video memory.

  @bold(Warning:)  if the hardware acceleration flag is not set,
  @code(al_draw_sprite) will not work correctly when used with a sprite image
  in system or video memory so the latter must be a memory bitmap.

  Although generally not supporting graphics of mixed color depths, as a
  special case this function can be used to draw 256-color source images onto
  truecolor destination bitmaps, so you can use palette effects on specific
  sprites within a truecolor program.
  @seealso(al_draw_sprite_v_flip) @seealso(al_draw_trans_sprite)
  @seealso(al_draw_lit_sprite) @seealso(al_draw_gouraud_sprite)
  @seealso(al_rotate_sprite) @seealso(al_draw_rle_sprite) *)
  PROCEDURE al_draw_sprite (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
    INLINE;

(* Like @code(al_draw_sprite), except it can stretch the sprite image to the
   specified width and height and requires the sprite image and destination
   bitmap to be of the same color depth.  Moreover, the sprite image must be a
   memory bitmap. *)
  PROCEDURE al_stretch_sprite (bmp, sprite: AL_BITMAPptr; x, y, w, h: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stretch_sprite';

(* Draws the sprite image onto the destination bitmap using the specified
   @code(mode) argument, optionally flipping the sprite in the orientation
   specified by @code(flip) argument.

   @param(mode defines how is sprite going to be drawn on the destination
     bitmap:
@unorderedList(
  @item(@code(AL_DRAW_SPRITE_NORMAL) draws a masked sprite, like
    @code(al_draw_sprite).)
  @item(@code (AL_DRAW_SPRITE_LIT) draws a tinted sprite, like
    @code(al_draw_lit_sprite).)
  @item(@code (AL_DRAW_SPRITE_TRANS) draws a blended sprite, like
    @code(al_draw_trans_sprite). )
)
   )
   @param(flip defines the flipping orientation:
@unorderedList(
  @item(@code(AL_DRAW_SPRITE_NO_FLIP) do not perform flipping.)
  @item(@code(AL_DRAW_SPRITE_H_FLIP) flip horizontally.)
  @item(@code(AL_DRAW_SPRITE_V_FLIP) flip vertically.)
  @item(@code(AL_DRAW_SPRITE_VH_FLIP) flip both vertically and horizontally-)
)
   ) *)
  PROCEDURE al_draw_sprite_ex (bmp, sprite: AL_BITMAPptr; x, y, mode, flip: LONGINT);
    INLINE;

(* This is like @code(al_draw_sprite), but it additionally flip the image
   horizontally.  Flipping horizontally means that the x-axis is reversed,
   between the source and the destination.  This produces exact mirror images,
   which is not the same as rotating the sprite (and it is a lot faster than
   the rotation routine).  The sprite must be a memory bitmap. *)
  PROCEDURE al_draw_sprite_h_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
    INLINE;

(* This is like @code(al_draw_sprite), but it additionally flip the image
   vertically.  Flipping vertically means that the y-axis is reversed,
   between the source and the destination.  This produces exact mirror images,
   which is not the same as rotating the sprite (and it is a lot faster than
   the rotation routine).  The sprite must be a memory bitmap. *)
  PROCEDURE al_draw_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
    INLINE;

(* This is like @code(al_draw_sprite), but it additionally flip the image
   vertically and horizontally.  Flipping vertically means that the y-axis is
   reversed, while flipping horizontally means that de x-axis is reversed,
   between the source and the destination.  This produces exact mirror images,
   which is not the same as rotating the sprite (and it is a lot faster than
   the rotation routine).  The sprite must be a memory bitmap. *)
  PROCEDURE al_draw_sprite_vh_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
    INLINE;

(* Uses the global @code(al_color_table) table or truecolor blender functions
   to overlay the sprite on top of the existing image.  This must only be used
   after you have set up the color mapping table (for 256-color modes) or
   blender functions (for truecolor modes).  Because it involves reading as
   well as writing the bitmap memory, translucent drawing is very slow if you
   draw directly to video RAM, so wherever possible you should use a memory
   bitmap instead. Example:
@longcode(#
VAR
  global_trans_table: AL_COLOR_MAP;

          ...
   al_create_trans_table (@global_trans_table, my_palette,
                             128, 128, 128, NIL);
          ...
   IF al_get_color_depth = 8
     al_color_table := @global_trans_table
   ELSE
     al_set_trans_blender (128, 128, 128, 128);
   al_draw_trans_sprite (buffer, ghost_sprite, x, y);
#)

   The bitmap and sprite must normally be in the same color depth, but as a
   special case you can draw 32 bit RGBA format sprites onto any hicolor or
   truecolor bitmap, as long as you call @code(al_set_alpha_blender) first,
   and you can draw 8-bit alpha images onto a 32-bit RGBA destination, as long
   as you call @code(al_set_write_alpha_blender) first.  As
   @code(al_draw_sprite) this function skips transparent pixels, except if the
   source sprite is an 8-bit image;  if this is the case, you should pay
   attention to properly set up your color map table for index 0.
   @seealso(al_draw_lit_sprite) @seealso(al_color_table) *)
  PROCEDURE al_draw_trans_sprite (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
    INLINE;

(* In 256-color modes, uses the global @code(al_color_table) table to tint the
   sprite image to the specified color or to light it to the level specified by
   'color', depending on the function which was used to build the table
   (@code(al_create_trans_table) or @code(al_create_light_table)), and draws
   the resulting image to the destination bitmap.  In truecolor modes, uses the
   blender functions to light the sprite image using the alpha level specified
   by 'color' (the alpha level which was passed to the blender functions is
   ignored) and draws the resulting image to the destination bitmap.

   @param(c must be in the range [0..255] whatever its actual meaning is.
     This must only be used after you have set up the color mapping table @(for
     256-color modes@) or blender functions @(for truecolor modes@).)
   @seealso(al_draw_sprite) @seealso(al_draw_gouraud_sprite)
   @seealso(al_color_table) *)
  PROCEDURE al_draw_lit_sprite (bmp, sprite: AL_BITMAPptr; x, y, c: LONGINT);
    INLINE;

(* More sophisticated version of @code(al_draw_lit_sprite):  the 'color'
   parameter is not constant across the sprite image anymore but interpolated
   between the four specified corner colors.  The corner values passed to this
   function indicate the strength of the color applied on them, ranging from 0
   (no strength) to 255 (full strength).
   @seealso(al_draw_sprite) @seealso(al_color_table) *)
  PROCEDURE al_draw_gouraud_sprite (bmp, sprite: AL_BITMAPptr; x, y, c1, c2, c3, c4: LONGINT); INLINE;

(* Draws the sprite image onto the bitmap.  It is placed with its top left
   corner at the specified position, then rotated by the specified angle around
   its centre.  The angle is a fixed point 16.16 number in the same format used
   by the fixed point trig routines, with 256 equal to a full circle, 64 a
   right angle, etc.  All rotation functions can draw between any two bitmaps,
   even screen bitmaps or bitmaps of different color depth.

   Positive increments of the angle will make the sprite rotate clockwise. *)
  PROCEDURE al_rotate_sprite (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle: AL_FIXED);
    INLINE;

(* Like @code(al_rotate_sprite), but flips the image vertically before
   rotating it.  To flip horizontally, use this routine but add
   @code(al_itofix @(128@)) to the angle.  To flip in both directions, use
   @code(al_rotate_sprite) and add @code(al_itofix @(128@)) to its angle. *)
  PROCEDURE al_rotate_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle: AL_FIXED);
    INLINE;

(* Like @code(al_rotate_sprite), but stretches or shrinks the image at the
   same time as rotating it. *)
  PROCEDURE al_rotate_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle, scale: AL_FIXED);
    INLINE;

(* Draws the sprite, similar to @code(al_rotate_scaled_sprite) except that it
  flips the sprite vertically first. *)
  PROCEDURE al_rotate_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle, scale: AL_FIXED);
    INLINE;

(* Like @code(al_rotate_sprite), but aligns the point in the sprite given by
   @code(cx, cy) to @code(x, y) in the bitmap, then rotates around this
   point. *)
  PROCEDURE al_pivot_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle: AL_FIXED);
    INLINE;

(* Like @code(al_rotate_sprite_v_flip), but aligns the point in the sprite
   given by @code(cx, cy) to @code(x, y) in the bitmap, then rotates around
   this point. *)
  PROCEDURE al_pivot_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle: AL_FIXED);
    INLINE;

(* Like @code(al_rotate_scaled_sprite), but aligns the point in the sprite
   given by @code(cx, cy) to @code(x, y) in the bitmap, then rotates around
   this point. *)
  PROCEDURE al_pivot_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle, scale: AL_FIXED);
    INLINE;

(* Like @code(al_rotate_scaled_sprite_v_flip), but aligns the point in the
   sprite given by @code(cx, cy) to @code(x, y) in the bitmap, then rotates
   and scales around this point. *)
  PROCEDURE al_pivot_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle, scale: AL_FIXED);
    INLINE;



  PROCEDURE al_rotate_sprite_trans (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_sprite_trans';

  PROCEDURE al_rotate_sprite_v_flip_trans (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_sprite_v_flip_trans';

  PROCEDURE al_rotate_scaled_sprite_trans (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle, scale: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_scaled_sprite_trans';

  PROCEDURE al_rotate_scaled_sprite_v_flip_trans (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle, scale: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_scaled_sprite_v_flip_trans';

  PROCEDURE al_pivot_sprite_trans (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle: AL_FIXED);
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_sprite_trans';

  PROCEDURE al_pivot_sprite_v_flip_trans (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle: AL_FIXED);
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_sprite_v_flip_trans';

  PROCEDURE al_pivot_scaled_sprite_trans (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle, scale: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_scaled_sprite_trans';

  PROCEDURE al_pivot_scaled_sprite_v_flip_trans (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle, scale: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_scaled_sprite_v_flip_trans';

  PROCEDURE al_rotate_sprite_lit (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle: AL_FIXED; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_sprite_lit';

  PROCEDURE al_rotate_sprite_v_flip_lit (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle: AL_FIXED; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_sprite_v_flip_lit';

  PROCEDURE al_rotate_scaled_sprite_lit (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle, scale: AL_FIXED; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_scaled_sprite_lit';

  PROCEDURE al_rotate_scaled_sprite_v_flip_lit (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle, scale: AL_FIXED; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_scaled_sprite_v_flip_lit';

  PROCEDURE al_pivot_sprite_lit (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle: AL_FIXED; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_sprite_lit';

  PROCEDURE al_pivot_sprite_v_flip_lit (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle: AL_FIXED; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_sprite_v_flip_lit';

  PROCEDURE al_pivot_scaled_sprite_lit (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle, scale: AL_FIXED; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_scaled_sprite_lit';

  PROCEDURE al_pivot_scaled_sprite_v_flip_lit (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle, scale: AL_FIXED; color: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_scaled_sprite_v_flip_lit';



(********************************
 * Run-lenth compressed sprites *
 ********************************)

TYPE
(* Ponter to @code(AL_RLE_SPRITE). *)
  AL_RLE_SPRITEptr = ^AL_RLE_SPRITE;
(* An RLE compressed sprite. @seealso(al_get_rle_sprite) *)
  AL_RLE_SPRITE = RECORD
    w, h: LONGINT;	 (*< width and height in pixels *)
    color_depth: LONGINT; (*< color depth of the image *)
    size: LONGINT;	 (*< size of sprite data in bytes *)
    dat: POINTER;
  END;



(* Creates an RLE sprite based on the specified bitmap (which must be a memory
   bitmap).  Remember to free this RLE sprite later to avoid memory leaks.
   @param(bitmap Pointer to the @link(bitmap) used to create the
     sprite.)
   @returns(A pointer to the created RLE sprite, or @nil if it could not be
     created.  Remember to free this RLE sprite later to avoid memory
     leaks.)
   @seealso(al_destroy_rle_sprite) @seealso(al_draw_rle_sprite) *)
  FUNCTION al_get_rle_sprite (bitmap: AL_BITMAPptr): AL_RLE_SPRITEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_rle_sprite';

(* Destroys an RLE sprite structure previously returned by
   @code(al_get_rle_sprite).  If you pass a @nil pointer this function won't do
   anything.  Use this once you are done with an RLE sprite to avoid memory
   leaks in your program.
   @param(sprite The RLE sprite to destroy.) *)
  PROCEDURE al_destroy_rle_sprite (sprite: AL_RLE_SPRITEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_rle_sprite';

(* Draws an RLE sprite onto a bitmap at the specified position.
   @param(bmp Bitmap where the sprite will be draw.)
   @param(spr Sprite to draw.)
   @param(x Horizontal coordinate.) @param(y Vertical coordinate.)
   @seealso(al_draw_sprite) *)
  PROCEDURE al_draw_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
				x, y: LONGINT);
    INLINE;

(* Translucent version of @code(al_draw_rle_sprite).  This must only be used
   after you have set up the color mapping table (for 256-color modes) or
   blender functions (for truecolor modes).  The bitmap and sprite must
   normally be in the same color depth, but as a special case you can draw
   32-bit RGBA format sprites onto any hicolor or truecolor bitmap, as long as
   you call @code(al_set_alpha_blender) first.
   @param(bmp Bitmap where the sprite will be draw.)
   @param(spr Sprite to draw.)
   @param(x Horizontal coordinate.) @param(y Vertical coordinate.)
   @seealso(al_draw_rle_sprite) @seealso(al_color_table)
   @seealso(al_set_trans_blender) *)
  PROCEDURE al_draw_trans_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y: LONGINT);
    INLINE;

(* Tinted version of @code(al_draw_rle_sprite).  This must only be used after
   you have set up the color mapping table (for 256-color modes) or blender
   functions (for truecolor modes).
   @param(bmp Bitmap where the sprite will be draw.)
   @param(spr Sprite to draw.)
   @param(x Horizontal coordinate.) @param(y Vertical coordinate.)
   @param(color Tint color.)
   @seealso(al_draw_rle_sprite) @seealso(al_color_table) *)
  PROCEDURE al_draw_lit_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y, color: LONGINT);
    INLINE;



(******************************************
 * Sound initialization and configuration *
 ******************************************)

CONST
(* Identifier to pass to @code(al_install_sound). *)
  AL_DIGI_AUTODETECT	= -1;
(* Identifier to pass to @code(al_install_sound). *)
  AL_DIGI_NONE		= 0;
(* Identifier to pass to @code(al_install_sound). *)
  AL_MIDI_AUTODETECT	= -1;
(* Identifier to pass to @code(al_install_sound). *)
  AL_MIDI_NONE		=  0;
(* Identifier to pass to @code(al_install_sound). *)
  AL_MIDI_DIGMID	= $44494749; { AL_ID ('DIGI'); }



(* Call this function to specify the number of voices that are to be used by
   the digital and MIDI sound drivers respectively.  This must be done
   @bold(before) calling @code(al_install_sound).  If you reserve too many
   voices, subsequent calls to @code(al_install_sound) will fail.  How many
   voices are available depends on the driver, and in some cases you will
   actually get more than you reserve (eg. the FM synth drivers will always
   provide 9 voices on an OPL2 and 18 on an OPL3, and the SB digital driver
   will round the number of voices up to the nearest power of two).  Pass
   negative values to restore the default settings.  You should be aware that
   the sound quality is usually inversely related to how many voices you use,
   so don't reserve any more than you really need. *)
  PROCEDURE al_reserve_voices (digi_voices, midi_voices: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'reserve_voices';

(* By default, Allegro will play a centered sample at half volume on both the
   left and right channel.  A sample panned to the far right or left will be
   played at maximum volume on that channel only.  This is done so you can play
   a single panned sample without distortion.  If you play multiple samples at
   full volume, the mixing process can result in clipping, a noticeable form of
   distortion.  The more samples, the more likely clipping is to occur, and the
   more clipping, the worse the output will sound.

   If clipping is a problem - or if the output is too quiet - this function can
   be used to adjust the volume of each voice.  You should first check that
   your speakers are at a reasonable volume, Allegro's global volume is at
   maximum (see @code(al_set_volume)), and any other mixers such as the Volume
   Control are set reasonably.  Once you are sure that Allegro's output level
   is unsuitable for your application, use this function to adjust it.

   Each time you increase the parameter by one, the volume of each voice will
   halve.  For example, if you pass 4, you can play up to 16 centred samples at
   maximum volume without distortion.

   If you pass 0 to this function, each centred sample will play at the maximum
   volume possible without distortion, as will all samples played through a
   mono driver.  Samples at the extreme left and right will distort if played
   at full volume.  If you wish to play panned samples at full volume without
   distortion, you should pass 1 to this function.

   Of course this function does not override the volume you specify with
   @code(al_play_sample) or @code(al_set_volume).  It simply alters the
   overall output of the program.  If you play samples at lower volumes, or if
   they are not normalised, then you can play more of them without distortion.

   It is recommended that you hard-code the parameter into your program, rather
   than offering it to the user.  The user can alter the volume with the
   configuration file instead, or you can provide for this with
   @code(al_set_volume). *)
  PROCEDURE al_set_volume_per_voice (scale: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_volume_per_voice';

(* Initialises the sound module.  You should normally pass
   @code(AL_DIGI_AUTODETECT) and @code(AL_MIDI_AUTODETECT) as the driver
   parameters to this function, in which case Allegro will read hardware
   settings from the current configuration file.  This allows the user to
   select different values with the setup utility:  see the @link(configuration
   configuration section) for details.

   @returns (@true if the sound is successfully installed, and @false on
     failure.  If it fails it will store a description of the problem in
     @code(al_error).) *)
  FUNCTION al_install_sound (digi, midi: LONGINT): BOOLEAN;

(* Cleans up after you are finished with the sound routines.  You don't
   normally need to call this, because @code(al_exit) will do it for you. *)
  PROCEDURE al_remove_sound; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_sound';



(* Alters the global sound output volume.  Specify volumes for both digital
   samples and MIDI playback, as integers from 0 to 255, or pass a negative
   value to leave one of the settings unchanged.  Values bigger than 255 will
   be reduced to 255.  This routine will not alter the volume of the hardware
   mixer if it exists (i.e. only your application will be affected). *)
  PROCEDURE al_set_volume (digi, midi: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_volume';

(* Alters the hardware sound output volume.  Specify volumes for both digital
   samples and MIDI playback, as integers from 0 to 255, or pass a negative
   value to leave one of the settings unchanged.  Values bigger than 255 will
   be reduced to 255.  This routine will use the hardware mixer to control the
   volume if it exists (i.e. the volume of all the applications on your machine
   will be affected), otherwise do nothing. *)
  PROCEDURE al_set_hardware_volume (digi, midi: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_hardware_volume';

(* Retrieves the global sound output volume, both for digital samples and MIDI
   playback, as integers from 0 to 255. *)
  PROCEDURE al_get_volume (VAR digi, midi: LONGINT);
    INLINE;

(* Retrieves the hardware sound output volume, both for digital samples and
   MIDI playback, as integers from 0 to 255, or -1 if the information is not
   available. *)
  PROCEDURE al_get_hardware_volume (VAR digi, midi: LONGINT);
    INLINE;



(* Sets the resampling quality of the mixer.  Valid values are the same as the
  @code(quality) config variable.  Please read chapter "Standard config
  variables" for details.  You can call this function at any point in your
  program, even before @code(al_init). *)
  PROCEDURE al_set_mixer_quality (quality: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mixer_quality';

(* Returns the current mixing quality, as specified by the @code(quality)
   config variable, or a previous call to @code(al_set_mixer_quality). *)
  FUNCTION al_get_mixer_quality: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_quality';

(* Returns the mixer frequency, in Hz. *)
  FUNCTION al_get_mixer_frequency: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_frequency';

(* Returns the mixer bit depth (8 or 16). *)
  FUNCTION al_get_mixer_bits: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_bits';

(* Returns the number of output channels. 2 for stereo, 1 for mono, 0 if the
   mixer isn't active. *)
  FUNCTION al_get_mixer_channels: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_channels';

(* Returns the number of voices allocated to the mixer. *)
  FUNCTION al_get_mixer_voices: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_voices';

(* Returns the number of samples per channel in the mixer buffer. *)
  FUNCTION al_get_mixer_buffer_length: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_buffer_length';


(********
 * MIDI *
 ********)

CONST
(* Max number of MIDI voices. *)
  AL_MIDI_VOICES = 64;
(* Max number of MIDI tracks. *)
  AL_MIDI_TRACKS = 32;



TYPE
(* Pointer to @code(AL_MIDI). *)
  AL_MIDIptr = ^AL_MIDI;
(* A structure holding MIDI data.
   @seealso(al_load_midi) @seealso(al_play_midi) @seealso(al_destroy_midi) *)
  AL_MIDI = RECORD
    divisions : LONGINT;		{< number of ticks per quarter note  }
    track : ARRAY[0..(AL_MIDI_TRACKS)-1] OF RECORD
      data : PBYTE;	{< MIDI message stream  }
      len : LONGINT;	{< length of the track data  }
    END;
  END;



VAR
(* Stores the current position (beat number) in the MIDI file, or contains a
   negative number if no music is currently playing.  Useful for synchronising
   animations with the music, and for checking whether a MIDI file has finished
   playing. *)
  al_midi_pos: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_pos';
(* Contains the position in seconds in the currently playing midi.  This is
   useful if you want to display the current song position in seconds, not as
   beat number. *)
  al_midi_time: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_time';
(* The loop start and end points, set by the @code(al_play_looped_midi)
   function.  These may safely be altered while the music is playing, but you
   should be sure they are always set to sensible values (start < end).  If you
   are changing them both at the same time, make sure to alter them in the
   right order in case a MIDI interrupt happens to occur in between your two
   writes!  Setting these values to -1 represents the start and end of the file
   respectively. *)
  al_midi_loop_start: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_loop_start';
  al_midi_loop_end  : LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_loop_end';



(* Loads a MIDI file (handles both format 0 and format 1).
   @returns(a pointer to a @code(AL_MIDI) structure, or @nil on error.
     Remember to free this MIDI file later to avoid memory leaks.) *)
  FUNCTION al_load_midi (filename: STRING): AL_MIDIptr;

(* Destroys a @code(AL_MIDI) structure when you are done with it.  It is safe
   to call this even when the MIDI file might be playing, because it checks and
   will kill it off if it is active.  Use this to avoid memory leaks in your
   program. *)
  PROCEDURE al_destroy_midi (midi: AL_MIDIptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_midi';

(* Starts playing the specified @code(AL_MIDI) file, first stopping whatever
   music was previously playing.  If the @code(loop) flag is set to @true,
   the data will be repeated until replaced with something else, otherwise it
   will stop at the end of the file.  Passing a @nil pointer will stop whatever
   music is currently playing.

   @returns(@false if an error occurs @(this may happen if a patch-caching
     wavetable driver is unable to load the required samples, or at least it
     might in the future when somebody writes some patch-caching wavetable
     drivers :-@)) *)
  FUNCTION al_play_midi (midi: AL_MIDIptr; loop: BOOLEAN): BOOLEAN;
    INLINE;

(* Starts playing a MIDI file with a user-defined loop position.  When the
   player reaches the @code(loop_end) position or the end of the file
   (@code(loop_end) may be -1 to only loop at EOF), it will wind back to the
   @code(loop_start) point.  Both positions are specified in the same beat
   number format as the @code(al_midi_pos) variable.

   @returns(@false if an error occurs, @true otherwise.) *)
  FUNCTION al_play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: LONGINT): BOOLEAN;
    INLINE;

(* Stops whatever music is currently playing. This is the same thing as calling
   @code(al_play_midi @(@nil, @false@)).
   @seealso(al_play_midi)*)
  PROCEDURE al_stop_midi; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stop_midi';

(* Pauses the MIDI player. @seealso(al_midi_resume) @seealso(al_play_midi) *)
  PROCEDURE al_midi_pause; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_pause';

(* Resumes playback of a paused MIDI file. @seealso(al_midi_pause) *)
  PROCEDURE al_midi_resume; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_resume';

(* Seeks to the given @code(al_midi_pos) in the current MIDI file.  If the
   target is earlier in the file than the current @code(al_midi_pos) it seeks
   from the beginning; otherwise it seeks from the current position.

   @returns(zero if it could successfully seek to the requested position.
     Otherwise, a return value of 1 means it stopped playing, and
     @code(al_midi_pos) is set to the negative length of the MIDI file @(so you
     can use this function to determine the length of a MIDI file@).  A return
   value of 2 means the MIDI file looped back to the start.) *)
  FUNCTION al_midi_seek(target: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_seek';

(* This function will simulate playing the given MIDI, from start to end, to
   determine how long it takes to play.  After calling this function,
   @code(al_midi_pos) will contain the negative number of beats, and
   @code(al_midi_time) the length of the midi, in seconds.

   Note that any currently playing midi is stopped when you call this function.
   Usually you would call it before @code(al_play_midi), to get the length of
   the midi to be played.

   @returns(the value of al_midi_time, the length of the midi.)
   @seealso(al_load_midi) @seealso(al_midi_time) @seealso(al_midi_pos)
 *)
  FUNCTION al_get_midi_length (midi: AL_MIDIptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_midi_length';

(* Streams a block of MIDI commands into the player in real-time, allowing you
   to trigger notes, jingles, etc, over the top of whatever MIDI file is
   currently playing. *)
  PROCEDURE al_midi_out (data: POINTER; length: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_out';

(* Forces the MIDI driver to load the entire set of patches ready for use.  You
   will not normally need to call this, because Allegro automatically loads
   whatever data is required for the current MIDI file, but you must call it
   before sending any program change messages via the @code(al_midi_out)
   command.

   @returns(@false if an error occurred.) *)
  FUNCTION al_load_midi_patches: BOOLEAN;
    INLINE;



TYPE
(* Used by @code(al_midi_msg_callback). *)
  AL_MIDI_MSG_CALLBACK_PROC = PROCEDURE (msg, b1, b2: LONGINT); CDECL;
VAR
(* Hook function allowing you to intercept MIDI player events.  If set to
   anything other than @nil, this routine will be called for each MIDI message.
   It will execute in an interrupt handler context, so all the code and data
   they use should be locked, and they must not call any operating system
   functions.  In general you just use these routines to set some flags and
   respond to them later in your mainline code.
   @seealso(al_play_midi) *)
  al_midi_msg_callback: AL_MIDI_MSG_CALLBACK_PROC;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_msg_callback';



(*******************
 * Digital samples *
 *******************)

TYPE
(* Pointer to @code(AL_SAMPLE). *)
  AL_SAMPLEptr = ^AL_SAMPLE;

(* A sample structure, which holds sound data, used by the digital sample
   routines.  You can consider all of these fields as read only except
   @code(priority), @code(loop_start) and @code(loop_end), which you can
   change them for example after loading a sample from disk.

   The variables @code(loop_start) and @code(loop_end) specify the loop
   position in sample units, and are set by default to the start and end of the
   sample.

   If you are creating your own samples on the fly, you might also want to
   modify the raw data of the sample pointed by the @code(data) field.  The
   sample data are always in unsigned format.  This means that if you are
   loading a PCM encoded sound file with signed 16-bit samples, you would have
   to XOR every two bytes (i.e. every sample value) with 0x8000 to change the
   signedness. *)
  AL_SAMPLE = RECORD
    bits : LONGINT;	 {< 8 or 16  }
    stereo : LONGINT;	 {< sample type flag  }
    freq : LONGINT;	 {< sample frequency  }
  (* It is a value from 0 to 255 (by default set to 128) and controls how
     hardware voices on the sound card are allocated if you attempt to play
     more than the driver can handle.  This may be used to ensure that the less
     important sounds are cut off while the important ones are preserved. *)
    priority : LONGINT;
    len : DWORD;	 {< length (in samples)  }
    loop_start : DWORD;	 {< loop start position  }
    loop_end : DWORD;	 {< loop finish position  }
    param : DWORD;	 {< @exclude  }
    data : POINTER;	 {< sample data  }
  END;

(* Used by @code(al_register_sample_file_type). *)
  AL_SAMPLE_LOAD_FUNC = FUNCTION (filename: PCHAR): AL_SAMPLEptr; CDECL;
  AL_SAMPLE_SAVE_FUNC = FUNCTION (filename: PCHAR; spl: AL_SAMPLEptr): LONGINT; CDECL;



(* Loads a sample from a file, supporting both mono and stereo WAV and mono VOC
   files, in 8 or 16-bit formats, as well as formats handled by functions
   registered using @code(al_register_sample_file_type).

   Remember to free this sample later to avoid memory leaks.

   @returns(a pointer to the @code(AL_SAMPLE) or @nil on error.) *)
  FUNCTION al_load_sample (filename: STRING): AL_SAMPLEptr;

(* Loads a sample from a RIFF WAV file.

   Remember to free this sample later to avoid memory leaks.

   @returns(a pointer to the @code(AL_SAMPLE) or @nil on error.)
   @seealso(al_load_wav_pf) *)
  FUNCTION al_load_wav (filename: STRING): AL_SAMPLEptr;

(* Loads a sample from a Creative Labs VOC file.

  Remember to free this sample later to avoid memory leaks.

   @returns(a pointer to the @code(AL_SAMPLE) or @nil on error.)
   @seealso(al_load_voc_pf) *)
  FUNCTION al_load_voc (filename: STRING): AL_SAMPLEptr;



(* Constructs a new sample structure of the specified type.

   Remember to free this sample later to avoid memory leaks. 
   @param(bits can be 8 or 16.)
   @param(stereo can be zero for mono samples and non-zero for stereo samples)
   @param(freq is the frequency in hertz)
   @param(len is the number of samples you want to allocate for the full sound
     buffer.)
   @returns(a pointer to the created sample, or @nil if the sample could not
     be created.) *)
  FUNCTION al_create_sample (bits, stereo, freq, len: LONGINT): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sample';

(* Writes a sample into a file.  The output format is determined from the
   @code(filename) extension.  At present Allegro does not natively support
   the writing of any sample formats, so you must register a custom saver
   routine with @code(al_register_sample_file_type).

   @returns(@true on success, @false otherwise.) *)
  FUNCTION al_save_sample (filename: STRING; spl: AL_SAMPLEptr): BOOLEAN;

(* Destroys a sample structure when you are done with it.  It is safe to call
   this even when the sample might be playing, because it checks and will kill
   it off if it is active.  Use this to avoid memory leaks in your program. *)
  PROCEDURE al_destroy_sample (spl: AL_SAMPLEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_sample';



(* Triggers a sample at the specified volume, pan position, and frequency.  The
   parameters @code(vol) and @code(pan) range from 0 (min/left) to 255
   (max/right).  Frequency is relative rather than absolute:  1000 represents
   the frequency that the sample was recorded at, 2000 is twice this, etc.  If
   @code(loop) is not zero, the sample will repeat until you call
   @code(al_stop_sample), and can be manipulated while it is playing by calling
   @code(al_adjust_sample).

   @returns(the voice number that was allocated for the sample or negative if
     no voices were available.) *)
  FUNCTION al_play_sample (spl: AL_SAMPLEptr; vol, pan, freq, loop: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'play_sample';

(* Alters the parameters of a sample while it is playing (useful for
   manipulating looped sounds).  You can alter the volume, pan, and frequency,
   and can also clear the loop flag, which will stop the sample when it next
   reaches the end of its loop.  The values of the parameters are just like
   those of @code(al_play_sample).  If there are several copies of the same
   sample playing, this will adjust the first one it comes across.  If the
   sample is not playing it has no effect. *)
  PROCEDURE al_adjust_sample (spl: AL_SAMPLEptr; vol, pan, freq, loop: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'adjust_sample';

(* Stop a sample from playing, which is required if you have set a sample going
   in looped mode.  If there are several copies of the sample playing, it will
   stop them all. You must still destroy the sample using
   @code(al_destroy_sample). *)
  PROCEDURE al_stop_sample (spl: AL_SAMPLEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stop_sample';



(* Informs the @code(al_load_sample) and the @code(al_save_sample) functions
   of a new sample file type, providing routines to read and write samples in
   this format (either function may be @nil).  Example:
   @longcode(#
   FUNCTION LoadMP3 (filename: PCHAR): AL_SAMPLEptr; CDECL;
   BEGIN
     ...
   END;

   ...

   al_register_sample_file_type ('mp3', LoadMPT, NIL);
   #) *)
  PROCEDURE al_register_sample_file_type (ext: STRING; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC);



IMPLEMENTATION

(***************
 * Core system *
 ***************)

USES
(* Includes the system driver description. *)
  aldrv;



VAR
(* To be used as "errnum". *)
  NumError: LONGINT;
(* To access to stytem drivers. *)
  system_driver: __AL_SYSTEM_DRIVER__PTR; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

(* Function for internal use. *)
  FUNCTION _install_allegro_version_check (
	system_id: LONGINT; errno_ptr: PLONGINT; atexit_ptr: POINTER;
	version: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

(* Function for messages. *)
  PROCEDURE _allegro_message (CONST msg: PCHAR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_message';

(* Function for close button handler. *)
  FUNCTION _set_close_button_callback (proc: AL_SIMPLE_PROC): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_close_button_callback';



(* Converts four 8 bit values to a packed 32 bit integer ID. *)
  FUNCTION AL_ID (str: SHORTSTRING): LONGINT;
  BEGIN
    AL_ID := (ORD (str[1]) SHL 24) OR (ORD (str[2]) SHL 16)
	     OR (ORD (str[3]) SHL  8) OR  ORD (str[4]);
  END;



(* Initialises the Allegro library. *)
  FUNCTION al_install (system_id: LONGINT): BOOLEAN;
  BEGIN
    al_install := _install_allegro_version_check (system_id, @NumError, NIL,
		  (4 SHL 16) OR (4 SHL 8) OR 0) = 0;
  END;



(* Initialises the Allegro library. *)
  FUNCTION al_init: BOOLEAN;
  BEGIN
    al_init := _install_allegro_version_check (AL_SYSTEM_AUTODETECT, @NumError,
	       NIL, (4 SHL 16) OR (4 SHL 8) OR 0) = 0;
  END;



(* Outputs a message. *)
  PROCEDURE al_message (CONST msg: STRING);
  BEGIN
    _allegro_message (PCHAR (msg));
  END;



(* On platforms that have a close button, this routine installs a callback
   function to handle the close event. *)
  FUNCTION al_set_close_button_callback (proc: AL_SIMPLE_PROC): BOOLEAN;
  BEGIN
    al_set_close_button_callback := (_set_close_button_callback (proc) = 0);
  END;




(* Finds out the currently selected desktop color depth. *)
  FUNCTION al_desktop_color_depth: LONGINT;
  BEGIN
    IF system_driver^.desktop_color_depth <> NIL THEN
      al_desktop_color_depth := (system_driver^.desktop_color_depth ())
    ELSE
      al_desktop_color_depth := 0;
  END;



(* Finds out the currently selected desktop resolution. *)
  FUNCTION al_get_desktop_resolution (VAR w, h: LONGINT): BOOLEAN;
  BEGIN
    IF system_driver^.get_desktop_resolution <> NIL THEN
      al_get_desktop_resolution := (system_driver^.get_desktop_resolution (@w, @h) = 0)
    ELSE
      al_get_desktop_resolution := FALSE;
  END;



(* This routine alters the window title. *)
  PROCEDURE al_set_window_title (CONST title: STRING);
  BEGIN
    IF system_driver^.set_window_title <> NIL THEN
      system_driver^.set_window_title (PCHAR (title));
  END;



(*******************
 * UNICODE support *
 *******************)

  FUNCTION ustrlen (s: PCHAR): LONGINT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_ustrlen (s: STRING): LONGINT;
  BEGIN
    al_ustrlen := ustrlen (PCHAR (s));
  END;



(**********************
 * Configuration file *
 **********************)

  PROCEDURE set_config_file (filename: PCHAR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_file';

  PROCEDURE al_set_config_file (filename: STRING);
  BEGIN
    set_config_file (PCHAR (filename));
  END;



  PROCEDURE override_config_file (filename: PCHAR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'override_config_file';

  PROCEDURE al_override_config_file (filename: STRING);
  BEGIN
    override_config_file (PCHAR (filename));
  END;



  FUNCTION get_config_string (section, name, def: PCHAR): PCHAR; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_string';

  FUNCTION al_get_config_string (section, name, def: STRING): STRING;
  BEGIN
    al_get_config_string := get_config_string (PCHAR (section), PCHAR (name), PCHAR (def));
  END;



  FUNCTION get_config_int (section, name: PCHAR; def: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_int';

  FUNCTION al_get_config_int (section, name: STRING; def: LONGINT): LONGINT;
  BEGIN
    al_get_config_int := get_config_int (PCHAR (section), PCHAR (name), def);
  END;



  FUNCTION get_config_hex (section, name: PCHAR; def: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_hex';

  FUNCTION al_get_config_hex (section, name:STRING; def: LONGINT): LONGINT;
  BEGIN
    al_get_config_hex := get_config_hex (PCHAR (section), PCHAR (name), def);
  END;



  FUNCTION get_config_float (section, name: PCHAR; def: SINGLE): SINGLE; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_float';

  FUNCTION al_get_config_float (section, name: STRING; def: SINGLE): SINGLE;
  BEGIN
    al_get_config_float := get_config_float (PCHAR (section), PCHAR (name), def);
  END;



  FUNCTION get_config_id (section, name: PCHAR; def: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_id';

  FUNCTION al_get_config_id (section, name: STRING; def: LONGINT): LONGINT;
  BEGIN
    al_get_config_id := get_config_id (PCHAR (section), PCHAR (name), def);
  END;



{  FUNCTION get_config_argv (section, name: PCHAR; argc: PLONGINT): PSTRING; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_argv';

  FUNCTION al_get_config_argv (section, name: STRING; argc: PLONGINT): STRINGptr;
  BEGIN
    al_get_config_argv := get_config_argv (PCHAR (section), PCHAR (name), argc);
  END;
}



  PROCEDURE set_config_string (section, name, val: PCHAR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_string';

  PROCEDURE al_set_config_string (section, name, val: STRING);
  BEGIN
    set_config_string (PCHAR (section), PCHAR (name), PCHAR (val));
  END;



  PROCEDURE set_config_int (section, name: PCHAR; val: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_int';

  PROCEDURE al_set_config_int (section, name: STRING; val: LONGINT);
  BEGIN
    set_config_int (PCHAR (section), PCHAR (name), val);
  END;



  PROCEDURE set_config_hex (section, name: PCHAR; val: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_hex';

  PROCEDURE al_set_config_hex (section, name: STRING; val: LONGINT);
  BEGIN
    set_config_hex (PCHAR (section), PCHAR (name), val);
  END;



  PROCEDURE set_config_float (section, name: PCHAR; val: SINGLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_float';

  PROCEDURE al_set_config_float (section, name: STRING; val: SINGLE);
  BEGIN
    set_config_float (PCHAR (section), PCHAR (name), val);
  END;



  PROCEDURE set_config_id (section, name: PCHAR; val: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_id';

  PROCEDURE al_set_config_id (section, name: STRING; val: LONGINT);
  BEGIN
    set_config_id (PCHAR (section), PCHAR (name), val);
  END;



(******************
 * Timer routines *
 ******************)

  FUNCTION install_timer: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_timer: BOOLEAN;
  BEGIN
    al_install_timer := install_timer = 0;
  END;



  FUNCTION install_int_ex (proc: AL_SIMPLE_PROC; speed: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_int_ex (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;
  BEGIN
    al_install_int_ex := install_int_ex (proc, speed) = 0;
  END;



  FUNCTION install_int (proc: AL_SIMPLE_PROC; speed: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_int (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;
  BEGIN
    al_install_int := install_int (proc, speed) = 0;
  END;



  FUNCTION install_param_int_ex (proc: AL_PARAM_PROC; speed: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_param_int_ex (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;
  BEGIN
    al_install_param_int_ex := install_param_int_ex (proc, speed) = 0;
  END;



  FUNCTION install_param_int (proc: AL_PARAM_PROC; speed: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_param_int (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;
  BEGIN
    al_install_param_int := install_param_int (proc, speed) = 0;
  END;



(* Utils for time calculations. *)
CONST
  AL_TIMERS_PER_SECOND = 1193181;



  FUNCTION AL_SECS_TO_TIMER (x: LONGINT): LONGINT;
  BEGIN
    AL_SECS_TO_TIMER := x * AL_TIMERS_PER_SECOND;
  END;



  FUNCTION AL_MSEC_TO_TIMER (x: LONGINT): LONGINT;
  BEGIN
    AL_MSEC_TO_TIMER := x * (AL_TIMERS_PER_SECOND DIV 1000);
  END;



  FUNCTION AL_BPS_TO_TIMER  (x: LONGINT): LONGINT;
  BEGIN
    AL_BPS_TO_TIMER := AL_TIMERS_PER_SECOND DIV x;
  END;



  FUNCTION AL_BPM_TO_TIMER  (x: LONGINT): LONGINT;
  BEGIN
    AL_BPM_TO_TIMER := (60 * AL_TIMERS_PER_SECOND) DIV x;
  END;



(******************
 * Keyboard input *
 ******************)

  FUNCTION install_keyboard: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_keyboard: BOOLEAN;
  BEGIN
    al_install_keyboard := install_keyboard = 0;
  END;



  FUNCTION keyboard_needs_poll: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_keyboard_needs_poll: BOOLEAN;
  BEGIN
    al_keyboard_needs_poll := keyboard_needs_poll <> 0;
  END;



  FUNCTION keypressed: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_keypressed: BOOLEAN;
  BEGIN
    al_keypressed := keypressed <> 0;
  END;



  FUNCTION ureadkey (scancode: PLONGINT): LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_ureadkey (VAR scancode: LONGINT): LONGINT;
  BEGIN
    al_ureadkey := ureadkey (@scancode);
  END;



  FUNCTION scancode_to_name (scancode: LONGINT): PCHAR;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scancode_to_name';

  FUNCTION al_scancode_to_name (scancode: LONGINT): STRING;
  BEGIN
    al_scancode_to_name := scancode_to_name (scancode);
  END;



(********************
 * Joystick support *
 ********************)

  FUNCTION install_joystick (atype: LONGINT): LONGINT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_joystick (atype: LONGINT): BOOLEAN;
  BEGIN
    al_install_joystick := install_joystick (atype) = 0;
  END;



  FUNCTION calibrate_joystick_name (n: LONGINT): PCHAR; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_calibrate_joystick_name (n: LONGINT): STRING;
  VAR
    Tmp: PCHAR;
  BEGIN
    Tmp := calibrate_joystick_name (n);
    IF Tmp <> NIL THEN
      al_calibrate_joystick_name := (Tmp)
    ELSE
      al_calibrate_joystick_name := '';
  END;



  FUNCTION calibrate_joystick (n: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_calibrate_joystick (n: LONGINT): BOOLEAN;
  BEGIN
    al_calibrate_joystick := calibrate_joystick (n) = 0;
  END;



  FUNCTION save_joystick_data (filename: PCHAR): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_save_joystick_data (filename: STRING): BOOLEAN;
  BEGIN
    al_save_joystick_data := save_joystick_data (PCHAR (filename)) = 0;
  END;



  FUNCTION load_joystick_data (filename: PCHAR): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_joystick_data (filename: STRING): BOOLEAN;
  BEGIN
    IF filename <> '' THEN
      al_load_joystick_data := load_joystick_data (PCHAR (filename)) = 0
    ELSE
      al_load_joystick_data := load_joystick_data (NIL) = 0;
  END;



(*********************
 * Color and palette *
 *********************)

  PROCEDURE _hsv_to_rgb_ (h, s, v: SINGLE; r, g, b: PLONGINT); CDECL;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'hsv_to_rgb';

  PROCEDURE _rgb_to_hsv (r, g, b: LONGINT; h, s, v: PSINGLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_to_hsv';



  PROCEDURE al_hsv_to_rgb (h, s, v: SINGLE; VAR r, g, b: LONGINT);
  BEGIN
    _hsv_to_rgb_ (h, s, v, @r, @g, @b);
  END;



  PROCEDURE al_rgb_to_hsv (r, g, b: LONGINT; VAR h, s, v: SINGLE);
  BEGIN
    _rgb_to_hsv (r, g, b, @h, @s, @v);
  END;



(***********
 * Bitmaps *
 ***********)

CONST
(* Identify bitmap type *)
  AL_BMP_ID_VIDEO     = $80000000;
  AL_BMP_ID_SYSTEM    = $40000000;
  AL_BMP_ID_SUB       = $20000000;
  AL_BMP_ID_PLANAR    = $10000000;
  AL_BMP_ID_NOBLIT    = $08000000;
  AL_BMP_ID_LOCKED    = $04000000;
  AL_BMP_ID_AUTOLOCK  = $02000000;
  AL_BMP_ID_MASK      = $01FFFFFF;



  FUNCTION al_bitmap_color_depth (bmp: AL_BITMAPptr): LONGINT;
  BEGIN
    al_bitmap_color_depth := bmp^.vtable^.color_depth;
  END;



  FUNCTION al_bitmap_mask_color (bmp: AL_BITMAPptr): LONGINT;
  BEGIN
    al_bitmap_mask_color := bmp^.vtable^.mask_color;
  END;



  FUNCTION al_is_same_bitmap (bmp1, bmp2: AL_BITMAPptr): BOOLEAN;
  VAR
    m1, m2: DWORD;
  BEGIN
    IF (bmp1 = NIL) OR (bmp2 = NIL) THEN
      al_is_same_bitmap := FALSE
    ELSE
      IF bmp1 = bmp2 THEN
	al_is_same_bitmap := TRUE
      ELSE BEGIN
	m1 := (bmp1^.id AND AL_BMP_ID_MASK);
	m2 := (bmp2^.id AND AL_BMP_ID_MASK);
	al_is_same_bitmap := ((m1 <> 0) AND (m1 = m2));
      END;
  END;



  FUNCTION al_is_memory_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
  BEGIN
    al_is_memory_bitmap := (bmp^.id AND (AL_BMP_ID_VIDEO OR AL_BMP_ID_SYSTEM)) = 0;
  END;



  FUNCTION al_is_screen_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
  BEGIN
    al_is_screen_bitmap := al_is_same_bitmap (bmp, al_screen);
  END;



  FUNCTION al_is_video_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
  BEGIN
    al_is_video_bitmap := (bmp^.id AND AL_BMP_ID_VIDEO) <> 0;
  END;



  FUNCTION al_is_system_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
  BEGIN
    al_is_system_bitmap := (bmp^.id AND AL_BMP_ID_SYSTEM) <> 0;
  END;



  FUNCTION al_is_sub_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
  BEGIN
    al_is_sub_bitmap := (bmp^.id AND AL_BMP_ID_SUB) <> 0;
  END;



  PROCEDURE al_acquire_bitmap (bmp: AL_BITMAPptr);
  BEGIN
    IF bmp <> NIL THEN
      bmp^.vtable^.acquire (bmp);
  END;



  PROCEDURE al_release_bitmap (bmp: AL_BITMAPptr);
  BEGIN
    IF bmp <> NIL THEN
      bmp^.vtable^.release (bmp);
  END;



(******************
 * Mouse routines *
 ******************)

  FUNCTION mouse_needs_poll: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_mouse_needs_poll: BOOLEAN;
  BEGIN
    al_mouse_needs_poll := mouse_needs_poll <> 0;
  END;



  FUNCTION show_os_cursor (cursor: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_show_os_cursor (cursor: LONGINT): BOOLEAN;
  BEGIn
    al_show_os_cursor := show_os_cursor (cursor) = 0;
  END;



  PROCEDURE get_mouse_mickeys (mickeyx, mickeyy: PLONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_get_mouse_mickeys (VAR mickeyx, mickeyy: LONGINT);
  BEGIN
    get_mouse_mickeys (@mickeyx, @mickeyy);
  END;



(****************
 * Graphic mode *
 ****************)

  FUNCTION set_gfx_mode (card, w, h, v_w, v_h: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_set_gfx_mode (card, w, h, v_w, v_h: LONGINT): BOOLEAN;
  VAR
    R: BOOLEAN;
  BEGIN
    R := set_gfx_mode (card, w, h, v_w, v_h) = 0;
    IF R THEN
    BEGIN
      IF al_screen <> NIL THEN
      BEGIN
        AL_SCREEN_W := w;
        AL_SCREEN_H := h;
        AL_VIRTUAL_W := al_screen^.w;
        AL_VIRTUAL_H := al_screen^.h;
      END;
    END;
    al_set_gfx_mode := R;
  END;



  PROCEDURE al_acquire_screen;
  BEGIN
    IF al_screen <> NIL THEN
      al_screen^.vtable^.acquire (al_screen);
  END;



  PROCEDURE al_release_screen;
  BEGIN
    IF al_screen <> NIL THEN
      al_screen^.vtable^.release (al_screen);
  END;



(**********************
 * Drawing primitives *
 **********************)

(* These are for inline&low-level access. *)
TYPE
  BYTEPtr = ^BYTE;



(* Clipping. *)
  PROCEDURE al_get_clip_rect (bmp: AL_BITMAPptr; VAR x1, y1, x2, y2: LONGINT);
  BEGIN
    x1 := bmp^.cl;
    y1 := bmp^.ct;
    x2 := bmp^.cl-1;
    y2 := bmp^.cb-1;
  END;



  PROCEDURE al_set_clip_state (bmp: AL_BITMAPptr; state: BOOLEAN);
  BEGIN
    IF state THEN
      bmp^.clip := -1
    ELSE
      bmp^.clip := 0;
  END;



(* Drawing primitives. *)
  FUNCTION al_getpixel (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
  BEGIN
    al_getpixel := bmp^.vtable^.getpixel (bmp, x, y);
  END;

  FUNCTION _al_getpixel (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
  VAR
    addr: DWORD;
  BEGIN
    addr := bmp^.read_bank (bmp, y);
    _al_getpixel := (BYTEPtr (addr + DWORD (x)))^;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  FUNCTION _al_getpixel15 (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
  VAR
    addr: DWORD;
  BEGIN
    addr := bmp^.read_bank (bmp, y);
    _al_getpixel15 := (PWORD (addr + DWORD (x)))^;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  FUNCTION _al_getpixel16 (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
  VAR
    addr: DWORD;
  BEGIN
    addr := bmp^.read_bank (bmp, y);
    _al_getpixel16 := (PWORD (addr + DWORD (x)))^;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  FUNCTION _al_getpixel24 (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
  VAR
    addr: DWORD;

    FUNCTION READ3BYTES: LONGINT; INLINE;
    BEGIN
      READ3BYTES :=
    {$IFDEF ENDIAN_BIG}
	   ((PLONGINT (addr + DWORD (x)    ))^ SHL 16)
	OR ((PLONGINT (addr + DWORD (x) + 1))^ SHL  8)
	OR ((PLONGINT (addr + DWORD (x) + 2))^       )
    {$ELSE}
	   ((PLONGINT (addr + DWORD (x)    ))^       )
	OR ((PLONGINT (addr + DWORD (x) + 1))^ SHL  8)
	OR ((PLONGINT (addr + DWORD (x) + 2))^ SHL 16)
    {$ENDIF}
      ;
    END;

  BEGIN
    addr := bmp^.read_bank (bmp, y);
    _al_getpixel24 := READ3BYTES;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  FUNCTION _al_getpixel32 (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT;
  VAR
    addr: DWORD;
  BEGIN
    addr := bmp^.read_bank (bmp, y);
    _al_getpixel32 := (PLONGINT (addr + DWORD (x)))^;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  PROCEDURE al_putpixel (bmp: AL_BITMAPptr; x, y, color: LONGINT);
  BEGIN
    bmp^.vtable^.putpixel (bmp, x, y, color);
  END;

  PROCEDURE _al_putpixel (bmp: AL_BITMAPptr; x, y, color: LONGINT);
  VAR
    addr: DWORD;
  BEGIN
    addr := bmp^.write_bank (bmp, y);
    (BYTEPtr (addr + DWORD (x)))^ := color;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  PROCEDURE _al_putpixel15 (bmp: AL_BITMAPptr; x, y, color: LONGINT);
  VAR
    addr: DWORD;
  BEGIN
    addr := bmp^.write_bank (bmp, y);
    (PWORD (addr + DWORD (x)))^ := color;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  PROCEDURE _al_putpixel16 (bmp: AL_BITMAPptr; x, y, color: LONGINT);
  VAR
    addr: DWORD;
  BEGIN
    addr := bmp^.write_bank (bmp, y);
    (PWORD (addr + DWORD (x)))^ := color;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  PROCEDURE _al_putpixel24 (bmp: AL_BITMAPptr; x, y, color: LONGINT);
  VAR
    addr: DWORD;

    PROCEDURE WRITE3BYTES; INLINE;
    BEGIN
    {$IFDEF ENDIAN_BIG}
	(PLONGINT (addr + DWORD (x)    ))^ := (color SHR 16) AND $FF;
	(PLONGINT (addr + DWORD (x) + 1))^ := (color SHR  8) AND $FF;
	(PLONGINT (addr + DWORD (x) + 2))^ :=  color         AND $FF;
    {$ELSE}
	(PLONGINT (addr + DWORD (x)    ))^ :=  color         AND $FF;
	(PLONGINT (addr + DWORD (x) + 1))^ := (color SHR  8) AND $FF;
	(PLONGINT (addr + DWORD (x) + 2))^ := (color SHR 16) AND $FF;
    {$ENDIF}
    END;

  BEGIN
    addr := bmp^.write_bank (bmp, y);
    (BYTEPtr (addr + DWORD (x)))^ := color;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  PROCEDURE _al_putpixel32 (bmp: AL_BITMAPptr; x, y, color: LONGINT);
  VAR
    addr: DWORD;
  BEGIN
    addr := bmp^.write_bank (bmp, y);
    (PLONGINT (addr + DWORD (x)))^ := color;
    bmp^.vtable^.unwrite_bank (bmp);
  END;

  PROCEDURE al_vline (bmp: AL_BITMAPptr; x, y1, y2, color: LONGINT);
  BEGIN
    bmp^.vtable^.vline (bmp, x, y1, y2, color);
  END;

  PROCEDURE al_hline (bmp: AL_BITMAPptr; x1, y, x2, color: LONGINT);
  BEGIN
    bmp^.vtable^.hline (bmp, x1, y, x2, color);
  END;

  PROCEDURE al_line (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
  BEGIN
    bmp^.vtable^.line (bmp, x1, y1, x2, y2, color);
  END;

  PROCEDURE al_fastline (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
  BEGIN
    bmp^.vtable^.fastline (bmp, x1, y1, x2, y2, color);
  END;

  PROCEDURE al_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
  BEGIN
    bmp^.vtable^.rect (bmp, x1, y1, x2, y2, color);
  END;

  PROCEDURE al_rectfill (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: LONGINT);
  BEGIN
    bmp^.vtable^.rectfill (bmp, x1, y1, x2, y2, color);
  END;

  PROCEDURE al_circle (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);
  BEGIN
    bmp^.vtable^.circle (bmp, x, y, r, color);
  END;

  PROCEDURE al_circlefill (bmp: AL_BITMAPptr; x, y, r, color: LONGINT);
  BEGIN
    bmp^.vtable^.circlefill (bmp, x, y, r, color);
  END;

  PROCEDURE al_ellipse (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);
  BEGIN
    bmp^.vtable^.ellipse (bmp, x, y, rx, ry, color);
  END;

  PROCEDURE al_ellipsefill (bmp: AL_BITMAPptr; x, y, rx, ry, color: LONGINT);
  BEGIN
    bmp^.vtable^.ellipsefill (bmp, x, y, rx, ry, color);
  END;

  PROCEDURE al_arc (bmp: AL_BITMAPptr; x, y: LONGINT; ang1, ang2: AL_FIXED; r, color: LONGINT);
  BEGIN
    bmp^.vtable^.arc (bmp, x, y, ang1, ang2, r, color);
  END;

  PROCEDURE al_floodfill (bmp: AL_BITMAPptr; x, y, color: LONGINT);
  BEGIN
    bmp^.vtable^.floodfill (bmp, x, y, color);
  END;

  PROCEDURE al_polygon (bmp: AL_BITMAPptr; vertices: LONGINT; CONST points: ARRAY OF LONGINT; color: LONGINT);
  BEGIN
    bmp^.vtable^.polygon (bmp, vertices, @points[0], color);
  END;

  PROCEDURE al_triangle (bmp: AL_BITMAPptr; x1, y1, x2, y2, x3, y3, color: LONGINT);
  BEGIN
    bmp^.vtable^.triangle (bmp, x1, y1, x2, y2, x3, y3, color);
  END;

  PROCEDURE al_spline (bmp: AL_BITMAPptr; CONST points: ARRAY OF LONGINT; color: LONGINT);
  BEGIN
    bmp^.vtable^.spline (bmp, @points[0], color);
  END;



(*************
 * Text font *
 *************)

  FUNCTION load_font (filename: PCHAR; palette: AL_PALETTEptr; p: POINTER)
	   : AL_FONTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_font (filename: STRING; palette: AL_PALETTEptr; p: POINTER)
	: AL_FONTptr;
  BEGIN
    al_load_font := load_font (PCHAR (filename), palette, p);
  END;



  FUNCTION load_bitmap_font (filename: PCHAR; palette: AL_PALETTEptr;
	p: POINTER): POINTER; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_bitmap_font (filename: STRING; palette: AL_PALETTEptr;
    p: POINTER): POINTER;
  BEGIN
    al_load_bitmap_font := load_bitmap_font (PCHAR (filename), palette, p);
  END;



  FUNCTION is_color_font (f: AL_FONTptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_is_color_font (f: AL_FONTptr): BOOLEAN;
  BEGIN
    al_is_color_font := is_color_font (f) <> 0;
  END;



  FUNCTION is_mono_font (f: AL_FONTptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_is_mono_font (f: AL_FONTptr): BOOLEAN;
  BEGIN
    al_is_mono_font := is_mono_font (f) <> 0;
  END;



  FUNCTION is_compatible_font (f1, f2: AL_FONTptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_is_compatible_font (f1, f2: AL_FONTptr): BOOLEAN;
  BEGIN
    al_is_compatible_font := is_compatible_font (f1, f2) <> 0;
  END;



(****************
 * Text drawing *
 ****************)

  PROCEDURE textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
  BEGIN
    textout_ex (bmp, f, PCHAR (str), x, y, color, bg);
  END;



  PROCEDURE textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
  BEGIN
    textout_centre_ex (bmp, f, PCHAR (str), x, y, color, bg);
  END;


  PROCEDURE textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
  BEGIN
    textout_right_ex (bmp, f, PCHAR (str), x, y, color, bg);
  END;



  PROCEDURE textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x1, x2, y, diff, color, bg: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x1, x2, y, diff, color, bg: LONGINT);
  BEGIN
    textout_justify_ex (bmp, f, PCHAR (str), x1, x2, y, diff, color, bg);
  END;



  FUNCTION text_length (f: AL_FONTptr; str: PCHAR): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_text_length (f: AL_FONTptr; str: STRING): LONGINT;
  BEGIN
    al_text_length := text_length (f, PCHAR (str));
  END;



(*******************************
 * Blitting and sprite drawing *
 *******************************)

(* Sprites. *)
PROCEDURE al_draw_sprite (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
BEGIN
  IF sprite^.vtable^.color_depth = 8 THEN
    bmp^.vtable^.draw_256_sprite (bmp, sprite, x, y)
  ELSE
    bmp^.vtable^.draw_sprite (bmp, sprite, x, y);
END;



PROCEDURE al_draw_sprite_ex (bmp, sprite: AL_BITMAPptr;
			     x, y, mode, flip: LONGINT);
BEGIN
  bmp^.vtable^.draw_sprite_ex (bmp, sprite, x, y, mode, flip);
END;



PROCEDURE al_draw_sprite_h_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
BEGIN
  bmp^.vtable^.draw_sprite_h_flip (bmp, sprite, x, y);
END;



PROCEDURE al_draw_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
BEGIN
  bmp^.vtable^.draw_sprite_v_flip (bmp, sprite, x, y);
END;

PROCEDURE al_draw_sprite_vh_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
BEGIN
  bmp^.vtable^.draw_sprite_vh_flip (bmp, sprite, x, y);
END;



PROCEDURE al_draw_trans_sprite (bmp, sprite: AL_BITMAPptr; x, y: LONGINT);
BEGIN
  IF sprite^.vtable^.color_depth = 32 THEN
    bmp^.vtable^.draw_trans_rgba_sprite (bmp, sprite, x, y)
  ELSE
    bmp^.vtable^.draw_trans_sprite (bmp, sprite, x, y);
END;



PROCEDURE al_draw_lit_sprite (bmp, sprite: AL_BITMAPptr; x, y, c: LONGINT);
BEGIN
  bmp^.vtable^.draw_lit_sprite (bmp, sprite, x, y, c);
END;



PROCEDURE al_draw_gouraud_sprite (bmp, sprite: AL_BITMAPptr; x, y, c1, c2, c3, c4: LONGINT);
BEGIN
  bmp^.vtable^.draw_gouraud_sprite (bmp, sprite, x, y, c1, c2, c3, c4);
END;

PROCEDURE al_rotate_sprite (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * $10000) DIV 2,
						      (y SHL 16) + (sprite^.h * $10000) DIV 2,
						      sprite^.w SHL 15, sprite^.h SHL 15,
						      angle, $10000, 0);
END;



PROCEDURE al_rotate_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * $10000) DIV 2,
						      (y SHL 16) + (sprite^.h * $10000) DIV 2,
						      sprite^.w SHL 15, sprite^.h SHL 15,
						      angle, $10000, NOT 0);
END;



PROCEDURE al_rotate_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle, scale: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * scale) DIV 2,
						      (y SHL 16) + (sprite^.h * scale) DIV 2,
						      sprite^.w SHL 15, sprite^.h SHL 15,
						      angle, scale, 0);
END;



PROCEDURE al_rotate_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: LONGINT; angle, scale: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * scale) DIV 2,
						      (y SHL 16) + (sprite^.h * scale) DIV 2,
						      sprite^.w SHL 15, sprite^.h SHL 15,
						      angle, scale, NOT 0);
END;



PROCEDURE al_pivot_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
						      cx SHL 16, cy SHL 16,
						      angle, $10000, 0);
END;



PROCEDURE al_pivot_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
						      cx SHL 16, cy SHL 16,
						      angle, $10000, NOT 0);
END;



PROCEDURE al_pivot_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle, scale: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
						      cx SHL 16, cy SHL 16,
						      angle, scale, 0);
END;



PROCEDURE al_pivot_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: LONGINT; angle, scale: AL_FIXED);
BEGIN
  bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
						      cx SHL 16, cy SHL 16,
						      angle, scale, NOT 0);
END;



(********************************
 * Run-lenth compressed sprites *
 ********************************)

  PROCEDURE al_draw_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
				x, y: LONGINT);
  BEGIN
    bmp^.vtable^.draw_rle_sprite (bmp, spr, x, y);
  END;

  PROCEDURE al_draw_trans_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y: LONGINT);
  BEGIN
    IF spr^.color_depth = 32 THEN
      bmp^.vtable^.draw_trans_rgba_rle_sprite (bmp, spr, x, y)
    ELSE
      bmp^.vtable^.draw_trans_rle_sprite (bmp, spr, x, y)
  END;

  PROCEDURE al_draw_lit_rle_sprite (bmp: AL_BITMAPptr; spr: AL_RLE_SPRITEptr;
					x, y, color: LONGINT);
  BEGIN
    bmp^.vtable^.draw_lit_rle_sprite (bmp, spr, x, y, color);
  END;



(******************************************
 * Sound initialization and configuration *
 ******************************************)

  FUNCTION install_sound (digi, midi: LONGINT; c: POINTER): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_sound (digi, midi: LONGINT): BOOLEAN;
  BEGIN
    al_install_sound := install_sound (digi, midi, NIL) = 0;
  END;



  PROCEDURE get_volume (digi, midi: PLONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_get_volume (VAR digi, midi: LONGINT);
  BEGIN
    get_volume (@digi, @midi);
  END;



  PROCEDURE get_hardware_volume (digi, midi: PLONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_get_hardware_volume (VAR digi, midi: LONGINT);
  BEGIN
    get_hardware_volume (@digi, @midi);
  END;



(********
 * MIDI *
 ********)

  FUNCTION load_midi (filename: PCHAR): AL_MIDIptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_midi (filename: STRING): AL_MIDIptr;
  BEGIN
    al_load_midi := load_midi (PCHAR (filename));
  END;



  FUNCTION play_midi (midi: AL_MIDIptr; loop: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_play_midi (midi: AL_MIDIptr; loop: BOOLEAN): BOOLEAN;
  BEGIN
    IF loop THEN
      al_play_midi := play_midi (midi, -1) = 0
    ELSE
      al_play_midi := play_midi (midi, 0) = 0;
  END;



  FUNCTION play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: LONGINT): BOOLEAN;
  BEGIN
    al_play_looped_midi := play_looped_midi (midi, loop_start, loop_end) = 0
  END;



  FUNCTION load_midi_patches: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_midi_patches: BOOLEAN;
  BEGIN
    al_load_midi_patches := load_midi_patches = 0;
  END;



(*******************
 * Digital samples *
 *******************)

  FUNCTION load_sample (filename: PCHAR): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_sample (filename: STRING): AL_SAMPLEptr;
  BEGIN
    al_load_sample := load_sample (PCHAR (filename));
  END;



  FUNCTION load_wav (filename: PCHAR): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_wav (filename: STRING): AL_SAMPLEptr;
  BEGIN
    al_load_wav := load_wav (PCHAR (filename));
  END;



  FUNCTION load_voc (filename: PCHAR): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_voc (filename: STRING): AL_SAMPLEptr;
  BEGIN
    al_load_voc := load_voc (PCHAR (filename));
  END;



  FUNCTION save_sample (filename: PCHAR; spl: AL_SAMPLEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_save_sample (filename: STRING; spl: AL_SAMPLEptr): BOOLEAN;
  BEGIN
    al_save_sample := (save_sample (PCHAR (filename), spl) = 0);
  END;



  PROCEDURE register_sample_file_type (ext: PCHAR; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_register_sample_file_type (ext: STRING; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC);
  BEGIN
    register_sample_file_type (PCHAR (ext), load, save);
  END;



INITIALIZATION
{ Create identifiers. }
  AL_U_ASCII	:= AL_ID('ASC8');
  AL_U_ASCII_CP	:= AL_ID('ASCP');
  AL_U_UNICODE	:= AL_ID('UNIC');
  AL_U_UTF8	:= AL_ID('UTF8');
  AL_U_CURRENT	:= AL_ID('cur.');
END.
