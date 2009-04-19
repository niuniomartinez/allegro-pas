UNIT allegro;
(*<Allegro core.

  This is the main module of the Allegro library.  There are very different
  stuff on this unit, but procedures, functions, types, variables and constants
  are grouped to make it easer to find them.  Read the @bold(Introduction)
  section for a brief description of this unit. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$MODE FPC}
 {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

USES
  albase, alvtable;



(***************
 * Core system *
 ***************
 * Defines a collection of procedures, identificators and variables that allows
 * to comunicate with the core system.  That is, error handling, initialization
 * and configuration of the base system. *)

CONST
(* Tells to @link(al_install) that must autodetect the system driver. *)
  AL_SYSTEM_AUTODETECT	= 0;
(* Tells to @link(al_install) that must install a stripped down version of
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
  AL_SUB_VERSION = 3;
(* Defined to the minor version of Allegro.pas.  From a version number like
   4.1.16, this would be defined to the integer 16. *)
  AL_PAS_VERSION = 0;
(* Defined to TRUE if current version is a BETA version.  A BETA version is a
   test version and may be uncomplete or untested. *)
  AL_PAS_IS_BETA = TRUE;
(* Defined to a text string containing all version numbers and maybe some
   additional text. *)
  AL_PAS_VERSION_STR = '4.3.0 SVN';



VAR
(* Stores the last error number. *)
  al_errno: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_errno';
(* Text string used by @link(al_set_gfx_mode), @link(al_install_sound) and
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
  OSTYPE_LINUX := AL_ID('TUX '));
END;
  #) *)
  FUNCTION AL_ID (str: SHORTSTRING): LONGINT;



(* Initialises the Allegro library.  You must call either this or al_init
   before doing anything other.  The functions that can be called before this
   one will be marked explicitly in the documentation, like
   @link(al_set_config_file).

   The available system ID codes will vary from one platform to another, but
   you will almost always want to pass @link(AL_SYSTEM_AUTODETECT).
   Alternatively, @link(AL_SYSTEM_NONE) installs a stripped down version of
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
   mode, only before calling @link(al_set_gfx_mode), or after a
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


(* On platforms that are capable of it, this routine alters the window title
   for your Allegro program.
   @param(title Title string.) *)
  PROCEDURE al_set_window_title (CONST title: STRING);




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
   language translations, are loaded when you call @link(al_init), so if you
   change the encoding format after this, they will be in the wrong format, and
   things will not work properly.  Generally you should only call
   @code(al_set_uformat) once, before @code(al_init), and then leave it on the
   same setting for the duration of your program.

   @param(type Should be one of these values: @link(AL_U_ASCII),
     @link(AL_U_ASCII_CP), @link(AL_U_UNICODE), @link(AL_U_UTF8).) *)
  PROCEDURE al_set_uformat (aType: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_uformat';



(* Finds out what text encoding format is currently selected.  This function is
   probably useful only if you are writing an Allegro addon dealing with text
   strings and you use a different codepath for each possible format.

   @returns(The currently selected text encoding format.)
   @seealso(al_set_uformat) *)
  FUNCTION al_get_uformat: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_uformat';



(* When you select the @link(AL_U_ASCII_CP) encoding mode, a set of tables are
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



(**********************
 * Configuration file *
 **********************)

(* Sets the configuration file to be used by all subsequent config functions.
   (Allegro will not search for this file in other locations as it does with
   allegro.cfg at the time of initialization.)

   All pointers returned by previous calls to @link(al_get_config_string) and
   other related functions are invalidated when you call this function!  You
   can call this function before @link(al_install) to change the configuration
   file, but after @link(al_set_uformat) if you want to use a text encoding
   format other than the default. *)
  PROCEDURE al_set_config_file (filename: STRING);

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
   configuration.  You can e.g. call @link(al_set_config_file), and the
   override file will still be active.  Also the @link(al_flush_config_file)
   function will only affect the current config file (which can be changed with
   @code(al_set_config_file)), never the overriding one specified with this
   function.  The modified override config is written back to disk whenever you
   call @code(al_override_config_file).

    Note that this function and @link(al_override_config_data) are mutually
    exclusive, i.e. calling one will cancel the effects of the other. *)
  PROCEDURE al_override_config_file (filename: STRING);

(* Version of @link(al_override_config_file) which uses a block of data that
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
   later restore the current settings by calling @link(al_pop_config_state).
   This function is mostly intended for internal use by other library
   functions, for example when you specify a config filename to the
   @link(al_save_joystick_data) function, it pushes the config state before
   switching to the file you specified. *)
  PROCEDURE al_push_config_state; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'push_config_state';

(* Pops a configuration state previously stored by @link(al_push_config_state),
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

(* Reads an integer variable from the current config file.  See the comments
   about @link(al_get_config_string). *)
  FUNCTION al_get_config_int (section, name: STRING; def: LONGINT): LONGINT;

(* Reads an integer variable from the current config file, in hexadecimal.
   See the comments about @link(al_get_config_string). *)
  FUNCTION al_get_config_hex (section, name: STRING; def: LONGINT): LONGINT;

(* Reads a floating point variable from the current config file.  See the
   comments about @link(al_get_config_string). *)
  FUNCTION al_get_config_float (section, name: STRING; def: DOUBLE): DOUBLE;

(* Reads a 4-letter driver ID variable from the current config file.  See the
   comments about @link(al_get_config_string). *)
  FUNCTION al_get_config_id (section, name: STRING; def: LONGINT): LONGINT;

(* AFAIK this doesn't work.
  FUNCTION al_get_config_argv (section, name: STRING; argc: PLONGINT): PSTRING; *)

(* Writes a string variable to the current config file, replacing any existing
   value it may have, or removes the variable if @code(val) is empty.  The
   section name may be set to a empty string to write the variable to the root
   of the file, or used to control which section the variable is inserted into.
   The altered file will be cached in memory, and not actually written to disk
   until you call @link(al_exit).  Note that you can only write to files in
   this way, so the function will have no effect if the current config source
   was specified with @link(al_set_config_data) rather than
   @link(al_set_config_file).

   As a special case, variable or section names that begin with a '#' character
   are treated specially and will not be read from or written to the disk.
   Addon packages can use this to store version info or other status
   information into the config module, from where it can be read with the
   @link(al_get_config_string) function. *)
  PROCEDURE al_set_config_string (section, name, val: STRING);

(* Writes an integer variable to the current config file.  See the comments
   about @link(al_set_config_string). *)
  PROCEDURE al_set_config_int (section, name: STRING; val: LONGINT);

(* Writes an integer variable to the current config file, in hexadecimal
   format.  See the comments about @link(al_set_config_string). *)
  PROCEDURE al_set_config_hex (section, name: STRING; val: LONGINT);

(* Writes a floating point variable to the current config file.  See the
   comments about @link(al_set_config_string). *)
  PROCEDURE al_set_config_float (section, name:STRING; val: DOUBLE);

(* Writes a 4-letter driver ID variable to the current config file.  See the
   comments about @link(al_set_config_string). *)
  PROCEDURE al_set_config_id (section, name: STRING; val: LONGINT);



(******************
 * Timer routines *
 ******************)

(* Give the number of seconds between each tick to @link(al_install_int_ex). *)
  FUNCTION AL_SECS_TO_TIMER (x: LONGINT): LONGINT;
(* Give the number of milliseconds between each tick to
   @link(al_install_int_ex). *)
  FUNCTION AL_MSEC_TO_TIMER (x: LONGINT): LONGINT;
(* Give the number of ticks each second to @link(al_install_int_ex). *)
  FUNCTION AL_BPS_TO_TIMER  (x: LONGINT): LONGINT;
(* Give the number of ticks each minute to @link(al_install_int_ex). *)
  FUNCTION AL_BPM_TO_TIMER  (x: LONGINT): LONGINT;



(* Installs the Allegro timer interrupt handler.  You must do this before
   installing any user timer routines, and also before displaying a mouse
   pointer and playing FLI animations or MIDI music.

   @returns(@true on success, or @false on failure @(but you may decide not to
     check the return value as this function is very unlikely to fail@).)  *)
  FUNCTION al_install_timer: BOOLEAN;

(* Removes the Allegro timer handler.  You don't normally need to bother
   calling this, because @link(al_exit) will do it for you. *)
  PROCEDURE al_remove_timer; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_timer';

(* Adds a function to the list of user timer handlers or, if it is already
   installed, retroactively adjusts its speed (i.e makes as though the speed
   change occurred precisely at the last tick).  The speed is given in hardware
   clock ticks, of which there are 1193181 a second. You can convert from other
   time formats to hardware clock ticks with the functions
   @link(AL_SECS_TO_TIMER), @link(AL_MSEC_TO_TIMER), @link(AL_BPS_TO_TIMER) and
   @link(AL_BPM_TO_TIMER).

   There can only be sixteen timers in use at a time, and some other parts of
   Allegro (the mouse pointer display routines, @link(al_rest), the FLI
   player, and the MIDI player) need to install handlers of their own, so you
   should avoid using too many at the same time.  If you call this routine
   without having first installed the timer module, @link(al_install_timer)
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

(* Installs a user timer handler, with the speed given as the number of
   milliseconds between ticks.  This is the same thing as
   @code(al_install_int_ex @(@@proc, AL_MSEC_TO_TIMER @(speed@)@)).  If you call
   this routine without having first installed the timer module,
   @link(al_install_timer) will be called automatically.  Calling again this
   routine with the same timer handler as parameter allows you to adjust its speed.

   @returns(@true on success, or @false if there is no room to add a new user
     timer.) *)
  FUNCTION al_install_int (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;

(* Removes a function from the list of user interrupt routines.
   @link(al_exit) does this automatically. *)
  PROCEDURE al_remove_int (proc: AL_SIMPLE_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_int';

(* Like @link(al_install_int_ex), but the callback routine will be passed a
   copy of the specified void pointer parameter.  To disable the handler, use
   @link(al_remove_param_int) instead of @link(al_remove_int). *)
  FUNCTION al_install_param_int_ex (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;

(* Like @link(al_install_int), but the callback routine will be passed a copy
   of the specified void pointer parameter.  To disable the handler, use
   @link(al_remove_param_int) instead of @link(al_remove_int). *)
  FUNCTION al_install_param_int (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;

(* Like @link(al_remove_int), but for use with timer callbacks that have
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

(* Like @link(al_rest), but for non-zero values continually calls the
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
   @link(al_set_gfx_mode).  This can happen in environments with graphic
   windowed modes, since Allegro usually reads the keyboard through the
   graphical window (which appears after the @code(al_set_gfx_mode) call).

   @returns(@true on success, or @false on failure @(but you may decide not to
     check the return value as this function is very unlikely to fail@).)  *)
  FUNCTION al_install_keyboard: BOOLEAN;

(* Removes the keyboard handler, returning control to the operating system.
   You don't normally need to bother calling this, because @link(al_exit) will
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

   The @link(al_keypressed), @link(al_readkey), and @link(al_ureadkey)
   functions call @code(al_poll_keyboard) automatically, so you only need to
   use this function when accessing the @link(al_key) array and
   @link(al_key_shifts) variable.

   @returns(zero on success or a negative on failure @(ie. no keyboard driver
     installed@).) *)
  FUNCTION al_poll_keyboard: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_keyboard';

(* Returns @true if the current keyboard driver is operating in polling mode. *)
  FUNCTION al_keyboard_needs_poll: BOOLEAN;

(* Returns @true if there are keypresses waiting in the input buffer.  You can
   use this to see if the next call to @link(al_readkey) is going to block or
   to simply wait for the user to press a key while you still update the screen
   possibly drawing some animation.  Example:
   @longcode(#
  WHILE NOT al_keypressed DO
    AnimateLogo (al_screen);
   #) *)
  FUNCTION al_keypressed: BOOLEAN;

(* Returns the next character from the keyboard buffer, in ASCII format.  If
   the buffer is empty, it waits until a key is pressed.  You can see if there
   are queued keypresses with @link(al_keypressed).

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

  IF (val RSH 8) = AL_KEY_SPACE THEN
    al_message ('You pressed Space');

  IF (val AND $ff) = 3 THEN
    al_message ('You pressed Control+C');

  IF val = (AL_KEY_X LSH 8) THEN
    al_message ('You pressed Alt+X');
   #)

   This function cannot return character values greater than 255.  If you need
   to read Unicode input, use @link(al_ureadkey) instead. *)
  FUNCTION al_readkey: LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'readkey';

(* Returns the next character from the keyboard buffer, in Unicode format.  If
   the buffer is empty, it waits until a key is pressed.  You can see if there
   are queued keypresses with @link(al_keypressed).  The return value contains
   the Unicode value of the key, and the argument will be set to the scancode.
   Unlike @link(al_readkey), this function is able to return character values
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

(* Stuffs a key into the keyboard buffer, just as if the user had pressed it.
   The parameter is in the same format returned by @link(al_readkey). *)
  PROCEDURE al_simulate_keypress (keycode: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_keypress';

(* Stuffs a key into the keyboard buffer, just as if the user had pressed it.
   The parameter is in the same format returned by @link(al_ureadkey). *)
  PROCEDURE al_simulate_ukeypress (keycode, scancode: LONGINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_ukeypress';

(* Empties the keyboard buffer.  Usually you want to use this in your program
   before reading keys to avoid previously buffered keys to be returned by
   calls to @link(al_readkey) or @link(al_ureadkey). *)
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
   use the @link(al_scancode_to_name) function. *)
  FUNCTION al_scancode_to_ascii (scancode: LONGINT): LONGINT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scancode_to_ascii';

(* This function returns a string pointer containing the name of they key with
   the given scancode.  This is useful if you e.g. let the user choose a key
   for some action, and want to display something more meaningful than just the
   scancode. *)
  FUNCTION al_scancode_to_name (scancode: LONGINT): STRING;



(* Access to the Allegro's public variables. *)
TYPE
{ @exclude }
  AL_KEY_LIST	 = ARRAY [0..126] OF BYTE;



VAR
(* Array of flags indicating the state of each key, ordered by scancode.
   Wherever possible these values will be updated asynchronously, but if
   @link(al_keyboard_needs_poll) returns @true, you must manually call
   @link(al_poll_keyboard) to update them with the current input state.  The
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
   value will be updated asynchronously, but if @link(al_keyboard_needs_poll)
   returns @true, you must manually call @link(al_poll_keyboard) to update it
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



(* Key scan-code and flag identifiers. *)
  {$include alkeyid.inc}



(********************
 * Joystick support *
 ********************)

CONST
(* To be used at @link(al_install_joystick). *)
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

(* Pointer to @link(AL_JOYSTICK_INFO). *)
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
       @link(al_calibrate_joystick) function from a loop until this flag is
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

{ @ignore }
  AL_JOYSTICK_INFO_LIST = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_JOYSTICK_INFO;



VAR
(* Global array of joystick state information, which is updated by the
   @link(al_poll_joystick) function.  Only the first @link(al_num_joysticks)
   elements will contain meaningful information.

   @seealso(AL_JOYSTICK_INFO)

   A single joystick may provide several different stick inputs, but you can
   safely assume that the first element in the stick array will always be the
   main directional controller.

   Information about each of the stick axis is stored in the
   @link(AL_JOYSTICK_AXIS_INFO) substructure.

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
     @link(al_calibrate_joystick) functions to measure the exact range of the
     inputs.) *)
  FUNCTION al_install_joystick (atype: LONGINT): BOOLEAN;

(* Removes the joystick handler. You don't normally need to bother calling
   this, because @link(al_exit) will do it for you. *)
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
   @link(al_calibrate_joystick_name), returning @true on success.  For example,
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
   @link(al_load_joystick_data).  Pass a @nil filename to write the data to the
   currently selected configuration file.

   @returns(@true on success, @false if the data could not be saved.) *)
  FUNCTION al_save_joystick_data (filename: STRING): BOOLEAN;

(* Restores calibration data previously stored by @link(al_save_joystick_data)
   or the setup utility.  This sets up all aspects of the joystick code:  you
   don't even need to call @link(al_install_joystick) if you are using this
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
(* Pointer to @link(AL_RGB). *)
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
   than a 32-bit mode, these are the same as calling @link(al_makecol) or
   @link(al_makecol_depth), but by using these routines it is possible to
   create 32-bit color values that contain a true 8 bit alpha channel along
   with the red, green, and blue components.  You should only use RGBA format
   colors as the input to @link(al_draw_trans_sprite) or
   @link(al_draw_trans_rle_sprite) after calling @link(al_set_alpha_blender),
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
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getn';
  FUNCTION al_geta (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'geta';



(* Given a color in the format being used by the specified color depth, these
   functions extract one of the red, green, blue, or alpha components (ranging
   0-255). The alpha part is only meaningful for 32-bit pixels. *)
  FUNCTION al_getr_depth (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getr_depth';
  FUNCTION al_getg_depth (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getg_depth';
  FUNCTION al_getb_depth (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getn_depth';
  FUNCTION al_geta_depth (c: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'geta_depth';



(* Convert color values between the HSV and RGB color spaces.  The RGB values
   range from 0 to 255, hue is from 0 to 360, and saturation and value are from
   0 to 1. *)
  PROCEDURE al_hsv_to_rgb (h, s, v: DOUBLE; VAR r, g, b: LONGINT);
  PROCEDURE al_rgb_to_hsv (r, g, b: LONGINT; VAR h, s, v: DOUBLE);



CONST
(* To know thet palette size. *)
  AL_PAL_SIZE = 256;



TYPE
(* Pointer to a @link(AL_PALETTE). *)
  AL_PALETTEptr = ^AL_PALETTE;
(* Color palette description for indexed modes (8bpp).  Remember that color
   components are 0-63. *)
  AL_PALETTE = ARRAY [0..AL_PAL_SIZE-1] OF AL_RGB;
(* Pointer to a @link(AL_RGB_MAP). *)
  AL_RGB_MAPptr = ^AL_RGB_MAP;
(* Speed up reducing RGB values to 8-bit paletted colors. *)
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
   also vastly accelerate the @link(al_makecol) and some
   @code(al_create_*_table) functions on 8-bit graphic mode.  RGB tables can be
   precalculated with the rgbmap utility, or generated at runtime with
   @link(al_create_rgb_table). *)
  al_rgb_table: AL_RGB_MAPptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_map';



(* Sets the specified palette entry to the specified @link(AL_RGB) triplet.
   Unlike the other palette functions this doesn't do any retrace
   synchronisation, so you should call @link(al_vsync) before it to prevent
   snow problems. *)
  PROCEDURE al_set_color (idx: LONGINT; p: AL_RGBptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color';

(* Sets the entire palette of 256 colors.  You should provide an array of 256
   RGB structures.  Unlike @link(al_set_color), there is no need to call
   @link(al_vsync) before this function. *)
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
   @link(AL_PALETTE) to store it in. *)
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
   table in the same way as the @link(al_set_palette) function, so the
   conversion will use the specified palette, but without affecting the
   display hardware in any way.  The previous palette settings are stored in an
   internal buffer, and can be restored by calling @link(al_unselect_palette).
   If you call @code(al_select_palette) again, however, the internal buffer
   will be overwritten. *)
  PROCEDURE al_select_palette (p: AL_PALETTE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'select_palette';

(* Restores the palette tables that were in use before the last call to
   @link(al_select_palette). *)
  PROCEDURE al_unselect_palette; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unselect_palette';


(* Constructs a fake truecolor palette, using three bits for red and green and
   two for the blue.  The @link(al_load_bitmap) function fills the palette
   parameter with this if the file does not contain a palette itself (ie. you
   are reading a truecolor bitmap). *)
  PROCEDURE al_generate_332_palette (p: AL_PALETTEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_332_palette';



(* Searches the specified palette for the closest match to the requested color,
   which are specified in the VGA hardware 0-63 format.  Normally you should
   call @link(al_makecol_depth) instead, but this lower level function may be
   useful if you need to use a palette other than the currently selected one,
   or specifically don't want to use the @link(al_rgb_map) lookup table.

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
(* Pointer to @link(AL_BITMAP). *)
  AL_BITMAPptr = ^AL_BITMAP;

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
   Use @link(al_set_clip_rect) instead. *)
  AL_BITMAP = RECORD
    w, h: LONGINT;		{< width and height in pixels }
    clip: LONGINT;		{< flag if clipping is turned on }
    cl, cr, ct, cb: LONGINT;	{< clip left, right, top and bottom values }
    vtable: AL_GFX_VTABLEptr;	{< drawing functions }
    write_bank: POINTER;		{< C func on some machines, asm on i386 }
    read_bank: POINTER;		{< C func on some machines, asm on i386 }
    dat: POINTER;		{< the memory we allocated for the bitmap }
    id: DWORD;			{< for identifying sub-bitmaps }
    extra: POINTER;		{< points to a structure with more info }
    x_ofs: LONGINT;		{< horizontal offset (for sub-bitmaps) }
    y_ofs: LONGINT;		{< vertical offset (for sub-bitmaps) }
    seg: LONGINT;		{< bitmap segment }
    line: POINTER;		{< ZERO_SIZE_ARRAY(unsigned char *, line); }
  END;



(* Creates a memory bitmap sized @code(w) by @code(h).  The bitmap will have
   clipping turned on, and the clipping rectangle set to the full size of the
   bitmap.  The image memory will not be cleared, so it will probably contain
   garbage:  you should clear the bitmap before using it.  This routine always
   uses the global pixel format, as specified by calling
   @link(al_set_color_depth).  The minimum height of the bitmap must be 1 and
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
   pages which can then be displayed by calling @link(al_show_video_bitmap).
   Read the introduction of this chapter for a comparison with other types of
   bitmaps and other specific details.

   @bold(Warning:)  video memory bitmaps are usually allocated from the same
   space as the screen bitmap, so they may overlap with it; it is therefore not
   a good idea to use the global screen at the same time as any surfaces
   returned by this function.

   Remember to destroy this bitmap before any subsequent call to
   @link(al_set_gfx_mode).

   @returns(a pointer to the bitmap on success, or @nil if you have run out of
     video ram.) *)
  FUNCTION al_create_video_bitmap (width, height: LONGINT): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_video_bitmap';

(* Allocates a system memory bitmap of the specified size.  Read the
   introduction of this chapter for a comparison with other types of bitmaps
   and other specific details.

   Remember to destroy this bitmap before any subsequent call to
   @link(al_set_gfx_mode).

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

(* @returns(the mask color for the specified bitmap @(the value which is
    skipped when drawing sprites@).  For 256-color bitmaps this is zero, and
    for truecolor bitmaps it is bright pink @(maximum red and blue, zero
    green@).  A frequent use of this function is to clear a bitmap with the
    mask color so you can later use this bitmap with @link(al_draw_sprite)
    after drawing other stuff on it.) *)
  FUNCTION al_bitmap_mask_color (bmp: AL_BITMAPptr): LONGINT;

(* @returns(@true if the two bitmaps describe the same drawing surface, ie.
    the pointers are equal, one is a sub-bitmap of the other, or they are both
    sub-bitmaps of a common parent.) *)
  FUNCTION al_is_same_bitmap (bmp1, bmp2: AL_BITMAPptr): BOOLEAN;

(* @returns(@true if bmp is a memory bitmap, ie. it was created by calling
    @link(al_create_bitmap) or loaded from a grabber datafile or image file.) *)
  FUNCTION al_is_memory_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* @returns(@true if @code(bmp) is the screen bitmap, or a sub-bitmap of the
   screen.) *)
  FUNCTION al_is_screen_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* @returns(@true) if bmp is the screen bitmap, a video memory bitmap, or a
   sub-bitmap of either.) *) 
  FUNCTION al_is_video_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* @returns(@true if bmp is a system bitmap object, or a sub-bitmap of one.) *)
  FUNCTION al_is_system_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* @returns(@true if bmp is a sub-bitmap.) *)
  FUNCTION al_is_sub_bitmap (bmp: AL_BITMAPptr): BOOLEAN;

(* Acquires the specified video bitmap prior to drawing onto it.  You never
   need to call the function explicitly as it is low level, and will only give
   you a speed up if you know what you are doing.  Using it wrongly may cause
   slowdown, or even lock up your program.

   @bold(Note:) You do never need to use @code(al_acquire_bitmap) on a memory
   bitmap, i.e. a normal bitmap created with @link(al_create_bitmap).  It will
   simply do nothing in that case.

   It still can be useful, because e.g. under the current DirectDraw driver of
   Allegro, most drawing functions need to lock a video bitmap before drawing
   to it.  But doing this is very slow, so you will get much better performance
   if you acquire the screen just once at the start of your main redraw
   function, then call multiple drawing operations which need the bitmap
   locked, and only release it when done.

   Multiple acquire calls may be nested, but you must make sure to match up the
   acquire_bitmap and @link(al_release_bitmap) calls.  Be warned that DirectX
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
   Especially don't call things like @link(al_show_mouse) (or
   @link(al_scare_mouse) which calls that) or @link(al_readkey), since it will
   most likely deadlock your entire program. *)
  PROCEDURE al_acquire_bitmap (bmp: AL_BITMAPptr);

(* Releases a bitmap that was previously locked by calling
   @link(al_acquire_bitmap).  If the bitmap was locked multiple times, you must
   release it the same number of times before it will truly be unlocked. *)
  PROCEDURE al_release_bitmap (bmp: AL_BITMAPptr);



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
  FUNCTION  al_generate_optimized_palette (image: AL_BITMAPptr; pal: AL_PALETTEptr; rsvdcols: ARRAY OF CHAR): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_optimized_palette';



(* Loads a bitmap from a file.  The palette data will be stored in the second
   parameter, which should be an @link(AL_PALETTE) structure.  At present this
   function supports BMP, LBM, PCX, and TGA files, determining the type from
   the file extension.

   If the file contains a truecolor image, you must set the video mode or call
   @link(al_set_color_conversion) before loading it.  In this case, if the
   destination color depth is 8-bit, the palette will be generated by calling
   @link(al_generate_optimized_palette) on the bitmap;  otherwise, the
   returned palette will be generated by calling @link(al_generate_332_palette)

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
   *)
  FUNCTION al_load_bitmap (filename: STRING; pal: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bitmap';



(* Writes a bitmap into a file, using the specified palette, which should be an
   @link(AL_PALETTE) structure.  The output format is determined from the
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



IMPLEMENTATION

USES
  algraph;



(***************
 * Core system *
 ***************)

TYPE
(* Includes the system driver description. *)
{$include alsysdrv.inc}



VAR
(* To be used as "errnum". *)
  NumError: LONGINT;
(* To access to stytem drivers. *)
  system_driver: __SYSTEM_DRIVER__PTR; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

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
		  (4 SHL 16) OR (3 SHL 8) OR 10) = 0;
  END;



(* Initialises the Allegro library. *)
  FUNCTION al_init: BOOLEAN;
  BEGIN
    al_init := _install_allegro_version_check (AL_SYSTEM_AUTODETECT, @NumError,
	       NIL, (4 SHL 16) OR (3 SHL 8) OR 10) = 0;
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



  FUNCTION get_config_float (section, name: PCHAR; def: DOUBLE): DOUBLE; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_float';

  FUNCTION al_get_config_float (section, name: STRING; def: DOUBLE): DOUBLE;
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



  PROCEDURE set_config_float (section, name: PCHAR; val: DOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_float';

  PROCEDURE al_set_config_float (section, name: STRING; val: DOUBLE);
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
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'calibrate_joystick_name';

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
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_joystick_data';

  FUNCTION al_save_joystick_data (filename: STRING): BOOLEAN;
  BEGIN
    al_save_joystick_data := save_joystick_data (PCHAR (filename)) = 0;
  END;



  FUNCTION load_joystick_data (filename: PCHAR): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_joystick_data';

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

{ Next commented declaration is for "_hsv_to_rgb" but for some reason it
  throws EAccessViolation exception, so this procedure was translated from
  the original C code.
  PROCEDURE _hsv_to_rgb_ (h, s, v: DOUBLE; r, g, b: PLONGINT); CDECL;
  CDECL;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'hsv_to_rgb'; }

  PROCEDURE _rgb_to_hsv (r, g, b: LONGINT; h, s, v: PDOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_to_hsv';



(* al_hsv_to_rgb:
 *   Converts from HSV colorspace to RGB values.  Translated from the original
 *   C code writen by Dave Thomson. *)
  PROCEDURE al_hsv_to_rgb (h, s, v: DOUBLE; VAR r, g, b: LONGINT);
  VAR
    f, x, y, z: DOUBLE;
    i: LONGINT;
  BEGIN
    v := v * 255.0;

    IF s = 0.0 THEN
    BEGIN
    { ok since we don't divide by s, and faster }
      r := TRUNC (v + 0.5); g := r; b := r;
    END
    ELSE BEGIN
      WHILE h >= 360.0 DO
	h := h - 360.0;
	h := h / 60.0;
	IF h < 0.0 THEN
	  h := h + 6.0;

	i := TRUNC (h);
	f := h - i;
	x := v * s;
	y := x * f;
	v := v + 0.5; { round to the nearest integer below }
	z := v - x;

	CASE i OF
	6: BEGIN
	  r := TRUNC (v);
	  g := TRUNC (z + y);
	  b := TRUNC (z);
	END;
	0: BEGIN
	  r := TRUNC (v);
	  g := TRUNC (z + y);
	  b := TRUNC (z);
	END;
	1: BEGIN
	  r := TRUNC (v - y);
	  g := TRUNC (v);
	  b := TRUNC (z);
	END;
	2: BEGIN
	  r := TRUNC (z);
	  g := TRUNC (v);
	  b := TRUNC (z + y);
	END;
	3: BEGIN
	  r := TRUNC (z);
	  g := TRUNC (v - y);
	  b := TRUNC (v);
	END;
	4: BEGIN
	  r := TRUNC (z + y);
	  g := TRUNC (z);
	  b := TRUNC (v);
	END;
	5: BEGIN
	  r := TRUNC (v);
	  g := TRUNC (z);
	  b := TRUNC (v - y);
	END;
      END;
    END;
  END;



  PROCEDURE al_rgb_to_hsv (r, g, b: LONGINT; VAR h, s, v: DOUBLE);
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



INITIALIZATION
{ Create identifiers. }
  AL_U_ASCII	:= AL_ID('ASC8');
  AL_U_ASCII_CP	:= AL_ID('ASCP');
  AL_U_UNICODE	:= AL_ID('UNIC');
  AL_U_UTF8	:= AL_ID('UTF8');
  AL_U_CURRENT	:= AL_ID('cur.');
END.
