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
  albase;



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



IMPLEMENTATION

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



INITIALIZATION
{ Create identifiers. }
  AL_U_ASCII	:= AL_ID('ASC8');
  AL_U_ASCII_CP	:= AL_ID('ASCP');
  AL_U_UNICODE	:= AL_ID('UNIC');
  AL_U_UTF8	:= AL_ID('UTF8');
  AL_U_CURRENT	:= AL_ID('cur.');
END.
