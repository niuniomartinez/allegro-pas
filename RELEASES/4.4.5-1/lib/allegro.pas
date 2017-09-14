UNIT Allegro;
(*<Allegro core.

  This is the main module of the Allegro library.  There are very different
  stuff on this unit, but procedures, functions, types, variables and constants
  are grouped to make it easer to find them.  Read the @code(How to use
  Allegro.pas) section for a brief description of this unit. *)

{$INCLUDE allegro.cfg }

INTERFACE

  USES
    alBase, alFixed, alvtable;


(* The code is distributed in sections.  Each section wraps with a header file
  (approx.). *)


(******************************************************************************
 * base.h
 *      Some global stuff.
 *)

  CONST
  (* Defined to the major version of Allegro.  From a version number like 4.1.16,
     this would be defined to the integer 4. *)
    AL_VERSION = 4;
  (* Defined to the middle version of Allegro.  From a version number like
     4.1.16, this would be defined to the integer 1. *)
    AL_SUB_VERSION = 4;
  (* Defined to the minor version of Allegro.pas.  From a version number like
     4.1.16, this would be defined to the integer 16. *)
    AL_PAS_VERSION = 5;
  (* Defined to TRUE if current version is a BETA version.  A BETA version is a
     test version and may be uncomplete or untested. *)
    AL_PAS_IS_BETA = FALSE;
  (* Defined to a text string containing all version numbers and maybe some
     additional text. *)
    AL_PAS_VERSION_STR = '4.4.5-1';



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
  FUNCTION AL_ID (str: SHORTSTRING): AL_INT32; INLINE;



  VAR
  (* Stores the last error number. *)
    al_errno: AL_INTptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_errno';



(******************************************************************************
 * system.h
 *      System level:  initialization, cleanup, etc.
 *)

  VAR
  (* Text string containing a date and version number for the library, in case
    you want to display these somewhere. *)
    al_id_string: AL_STRptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_id';

  (* Text string used by @link(al_set_gfx_mode), @link(al_install_sound) and
    other functions to report error messages.  If they fail and you want to tell
    the user why, this is the place to look for a description of the problem. *)
    al_error: AL_STRptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_error';

{$INCLUDE alosid.inc }

  VAR
  (* Identifies the Operating System.

    Set by @code(al_init) to one of the values:
@longcode(#
    AL_OSTYPE_UNKNOWN    - unknown, or regular MSDOS

    AL_OSTYPE_WIN3       - Windows 3.1 or earlier
    AL_OSTYPE_WIN95      - Windows 95
    AL_OSTYPE_WIN98      - Windows 98
    AL_OSTYPE_WINME      - Windows ME
    AL_OSTYPE_WINNT      - Windows NT
    AL_OSTYPE_WIN2000    - Windows 2000
    AL_OSTYPE_WINXP      - Windows XP
    AL_OSTYPE_WIN2003    - Windows 2003
    AL_OSTYPE_WINVISTA   - Windows Vista
    AL_OSTYPE_WIN7       - Windows 7

    AL_OSTYPE_OS2        - OS/2
    AL_OSTYPE_WARP       - OS/2 Warp 3

    AL_OSTYPE_LINUX      - Linux
    AL_OSTYPE_SUNOS      - SunOS/Solaris
    AL_OSTYPE_FREEBSD    - FreeBSD
    AL_OSTYPE_NETBSD     - NetBSD
    AL_OSTYPE_IRIX       - IRIX
    AL_OSTYPE_QNX        - QNX
    AL_OSTYPE_UNIX       - Unknown Unix variant

    AL_OSTYPE_BEOS       - BeOS
    AL_OSTYPE_HAIKU      - Haiku OS

    AL_OSTYPE_DARWIN     - Darwin
    AL_OSTYPE_MACOS      - MacOS
    AL_OSTYPE_MACOSX     - MacOS X
 #)
    @seealso(al_init) @seealso(al_os_version) @seealso(al_os_multitasking) *)
    al_os_type: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'os_type';

  (* The major version of the Operating System currently running.  Set by
     @link(al_init). If Allegro for some reason was not able to retrieve the
     version of the Operating System, @code(al_os_version) will be set to -1.

      For example: Under Win98 SE (v4.10.2222) @code(al_os_version) will be set
      to 4.
      @seealso(al_os_type) @seealso(al_os_revision)
      @seealso(al_os_multitasking) *)
    al_os_version: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'os_version';
  (* The minor version of the Operating System currently running.  Set by
     @link(al_init). If Allegro for some reason was not able to retrieve the
     version of the Operating System, @code(al_os_revision) will be set to -1.

      For example: Under Win98 SE (v4.10.2222) @code(al_os_revision) will be
      set to 10.
      @seealso(al_os_type) @seealso(al_os_version)
      @seealso(al_os_multitasking) *)
    al_os_revision: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'os_revision';
  (* Set by @link(al_init) to either @true or @false depending on whether your
     Operating System is multitasking or not.
     @seealso(al_os_type) @seealso(al_os_version) *)
    al_os_multitasking: AL_BOOL;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'os_multitasking';

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
     could be used@).)
   @seealso(al_exit) @seealso(al_set_uformat) @seealso(al_set_config_file)
   @seealso(alUNIX) @seealso(alWin) *)
  FUNCTION al_install (system_id: AL_INT): AL_BOOL;
    INLINE;

(* Function which initialises the Allegro library.  This is the same thing as
   calling @code(al_install @(AL_SYSTEM_AUTODETECT@)).
   @returns(@true on success or @false on failure @(e.g. no system driver
     could be used@).)
   @seealso(al_exit) *)
  FUNCTION al_init: AL_BOOL;
    INLINE;

(* Closes down the Allegro system.  This includes returning the system to text
   mode and removing whatever mouse, keyboard, and timer routines have been
   installed.  You don't normally need to bother making an explicit call to
   this function, because it will be called automatically when your program
   exits.

   Note that after you call this function, other functions like
   al_destroy_bitmap will most likely crash.  This might be a problem for
   some Object Pascal programs, depending where the destructors are called.
   @seealso(al_install) @seealso(al_init) *)
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
  PROCEDURE al_message (CONST msg: AL_STR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_message';

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
  CloseButtonPressed := TRUE;
END;

    ...
  al_init;
  al_set_close_button_callback (@CloseButtonHandler);
    ...
  REPEAT DoStuff UNTIL CloseButtonPressed;
  #)
   @returns(@true on success or @false on failure @(e.g. the feature is not
     supported by the platform@).)
   @seealso(al_set_window_title) *)
  FUNCTION al_set_close_button_callback (proc: AL_SIMPLE_PROC): AL_BOOL;
    INLINE;



(* Detects the CPU type, setting the following global variables. You don't
   normally need to call this, because @link(al_init) will do it for you.
   @seealso(al_cpu_vendor) @seealso(al_cpu_family) @seealso(al_cpu_model)
   @seealso(al_cpu_capabilities) *)
  PROCEDURE al_check_cpu;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'check_cpu';

(* CPU identifiers. *)
  {$INCLUDE alcpuid.inc}

  VAR
  (* On Intel PCs, contains the CPU vendor name if known.  On Mac OSX systems
     this contains the PPC subtype name.  On other platforms, this may be an
     empty string.  You can read this variable after you have called
     @link(al_check_cpu) (which is automatically called by @link(al_init)).
     @seealso(al_cpu_family) @seealso(al_cpu_model)
     @seealso(al_cpu_capabilities) *)
    al_cpu_vendor: AL_STRptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'cpu_vendor';

  (* Contains the CPU type, where applicable.  Allegro defines the following
     CPU family types:
@unorderedList(
  @item(AL_CPU_FAMILY_UNKNOWN  - The type of processor is unknown)
  @item(AL_CPU_FAMILY_I386     - The processor is an Intel-compatible 386)
  @item(AL_CPU_FAMILY_I486     - The processor is an Intel-compatible 486)
  @item(AL_CPU_FAMILY_I586     - The processor is a Pentium or equivalent)
  @item(AL_CPU_FAMILY_I686     - The processor is a Pentium Pro, II, III
                                or equivalent)
  @item(AL_CPU_FAMILY_ITANIUM  - The processor is an Itanium processor)
  @item(AL_CPU_FAMILY_POWERPC  - The processor is a PowerPC processor)
  @item(AL_CPU_FAMILY_EXTENDED - The processor type needs to be read
                                from the cpu_model))
     You can read this variable after you have called @link(al_check_cpu)
     (which is automatically called by @link(al_init)).
     @seealso(al_cpu_vendor) @seealso(al_cpu_model)
     @seealso(al_cpu_capabilities) *)
    al_cpu_family: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'cpu_family';

  (* Contains the CPU submodel, where applicable. Allegro defines at least the
     following CPU family types (see lib/alcpu.inc for a more complete list):
@longcode(#
  AL_CPU_FAMILY_I586:
     AL_CPU_MODEL_PENTIUM, AL_CPU_MODEL_K5, AL_CPU_MODEL_K6

  AL_CPU_FAMILY_I686:
     AL_CPU_MODEL_PENTIUMPRO, AL_CPU_MODEL_PENTIUMII,
     AL_CPU_MODEL_PENTIUMIIIKATMAI, AL_CPU_MODEL_PENTIUMIIICOPPERMINE,
     AL_CPU_MODEL_ATHLON, AL_CPU_MODEL_DURON

  AL_CPU_FAMILY_EXTENDED:
     AL_CPU_MODEL_PENTIUMIV, AL_CPU_MODEL_XEON,
     AL_CPU_MODEL_ATHLON64, AL_CPU_MODEL_OPTERON

  AL_CPU_FAMILY_POWERPC:
     AL_CPU_MODEL_POWERPC_x, for x=601-604, 620, 750, 7400, 7450
#)
     You can read this variable after you have called @link(al_check_cpu)
     (which is automatically called by @link(al_init).  Make sure you check the
     @link(al_cpu_family) and @link(al_cpu_vendor) so you know which models
     make sense to check.
     @seealso(al_cpu_capabilities) *)
    al_cpu_model: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'cpu_model';

  (* Contains CPU flags indicating what features are available on the current
     CPU. The flags can be any combination of these:
     @unorderedList(
       @item(@bold(AL_CPU_ID:) Indicates that the "cpuid" instruction is
         available. If this is set, then all Allegro CPU variables are 100%
	 reliable, otherwise there may be some mistakes.)
       @item(@bold(AL_CPU_FPU:) An FPU is available.)
       @item(@bold(AL_CPU_IA64:) Running on Intel 64 bit CPU)
       @item(@bold(AL_CPU_AMD64:) Running on AMD 64 bit CPU)
       @item(@bold(AL_CPU_MMX:) Intel MMX  instruction set is available.)
       @item(@bold(AL_CPU_MMXPLUS:) Intel MMX+ instruction set is available.)
       @item(@bold(AL_CPU_SSE:) Intel SSE  instruction set is available.)
       @item(@bold(AL_CPU_SSE2:) Intel SSE2 instruction set is available.)
       @item(@bold(AL_CPU_SSE3:) Intel SSE3 instruction set is available.)
       @item(@bold(AL_CPU_3DNOW:) AMD 3DNow! instruction set is available.)
       @item(@bold(AL_CPU_ENH3DNOW:) AMD Enhanced 3DNow! instruction set is
         available.)
       @item(@bold(AL_CPU_CMOV:) Pentium Pro "cmov" instruction is available.)
     )
     You can check for multiple features by OR-ing the flags together.  For
     example, to check if the CPU has an FPU and MMX instructions available,
     you'd do:
@longcode(#
  IF al_cpu_capabilities AND (AL_CPU_FPU OR AL_CPU_MMX = AL_CPU_FPU OR AL_CPU_MMX) THEN
    WriteLn ('CPU has both an FPU and MMX instructions!');
#)
     You can read this variable after you have called @link(al_check_cpu)
     (which is automatically called by @link(al_init).
     @seealso(al_cpu_vendor) @seealso(al_cpu_family) @seealso(al_cpu_model) *)
    al_cpu_capabilities: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'cpu_capabilities';



(******************************************************************************
 * system.inl
 *)

(* On platforms that are capable of it, this routine alters the window title
   for your Allegro program.
   @param(title Title string.)
   @seealso(al_set_close_button_callback) @seealso(al_set_uformat) *)
  PROCEDURE al_set_window_title (CONST title: AL_STR);
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
   available or does not apply.)
   @seealso(al_get_desktop_resolution) @seealso(al_set_color_depth)
   @seealso(al_set_gfx_mode) *)
  FUNCTION al_desktop_color_depth: AL_INT;
    INLINE;

(* Finds out the currently selected desktop resolution.  You can use this
   information to avoid creating windows bigger than the current resolution.
   This is especially important for some windowed drivers which are unable to
   create windows bigger than the desktop.

   Under some OSes, switching to a full screen graphics mode may automatically
   change the desktop resolution.  You have, therefore, to call this function
   before setting any graphics mode in order to retrieve the real desktop
   resolution.
   @param(w Width desktop resolution.) @param(h Height desktop resolution.)
   @returns(@true on success, or @false if this information is not available or
   does not apply, in which case the values stored in the variables you
   provided for `width' and `height' are undefined.)
   @seealso(al_desktop_color_depth) @seealso(al_set_gfx_mode) *)
  FUNCTION al_get_desktop_resolution (OUT w, h: AL_INT): AL_BOOL;
    INLINE;



(******************************************************************************
 * config.h
 *      Configuration access routines.
 *)

(* Sets the configuration file to be used by all subsequent config functions.
   (Allegro will not search for this file in other locations as it does with
   allegro.cfg at the time of initialization.)

   All pointers returned by previous calls to @link(al_get_config_string) and
   other related functions are invalidated when you call this function!  You
   can call this function before @link(al_install) to change the configuration
   file, but after @link(al_set_uformat) if you want to use a text encoding
   format other than the default.
   @seealso(al_set_config_data) @seealso(al_override_config_file)
   @seealso(al_push_config_state) @seealso(al_set_config_string) *)
  PROCEDURE al_set_config_file (CONST filename: AL_STR);
    INLINE;

(* Specifies a block of data to be used by all subsequent config functions,
   which you have already loaded from disk (eg. as part of some more
   complicated format of your own, or in a grabber datafile).  This routine
   makes a copy of the information, so you can safely free the data after
   calling it.
   @seealso(al_set_config_file) @seealso(al_override_config_data)
   @seealso(al_push_config_state) @seealso(al_set_config_string) *)
  PROCEDURE al_set_config_data (CONST data: AL_POINTER; lng: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_data';

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
   override config file, you can call @code(al_override_config_file) with an
   empty string as parameter, so config data will be directly read from the
   current config file again.

   @bold(Note:) The override file is completely independent from the current
   configuration.  You can e.g. call @link(al_set_config_file), and the
   override file will still be active.  Also the @link(al_flush_config_file)
   function will only affect the current config file (which can be changed with
   @code(al_set_config_file)), never the overriding one specified with this
   function.  The modified override config is written back to disk whenever you
   call @code(al_override_config_file).

    Note that this function and @link(al_override_config_data) are mutually
    exclusive, i.e. calling one will cancel the effects of the other. *)
  PROCEDURE al_override_config_file (CONST filename: AL_STR);
    INLINE;

(* Version of @link(al_override_config_file) which uses a block of data that
   has already been read into memory.  The length of the block has to be
   specified in bytes.

   Note that this function and @code(al_override_config_file) are mutually
   exclusive, i.e. calling one will cancel the effects of the other. *)
  PROCEDURE al_override_config_data (CONST data: AL_POINTER; lng: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'override_config_data';

(* Writes the current config file to disk if the contents have changed since
   it was loaded or since the latest call to the function.
   @seealso(al_set_config_file) @seealso(al_override_config_file)
   @seealso(al_push_config_state) *)
  PROCEDURE al_flush_config_file;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'flush_config_file';

(* Pushes the current configuration state (filename, variable values, etc).
   onto an internal stack, allowing you to select some other config source and
   later restore the current settings by calling @link(al_pop_config_state).
   This function is mostly intended for internal use by other library
   functions, for example when you specify a config filename to the
   @link(al_save_joystick_data) function, it pushes the config state before
   switching to the file you specified.
   @seealso(al_set_config_file) *)
  PROCEDURE al_push_config_state;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'push_config_state';

(* Pops a configuration state previously stored by @link(al_push_config_state),
   replacing the current config source with it. *)
  PROCEDURE al_pop_config_state;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pop_config_state';



(* Retrieves a string variable from the current config file.  Example:
@longcode(#
VAR
  Lang: STRING;
  ...
  Lang := al_get_config_string ('system', 'language', 'EN');
  #)
  @param(section Set to an empty string to read variables from the root of the
    file, or used to control which set of parameters (eg. sound or joystick)
    you are interested in reading.)
  @param(name Name of the variable to read.)
  @param(def Default value.)
   @returns(The string string found in the configuration file.
     If the named variable cannot be found, or its entry in the config file is
     empty, the value of @code(def) is returned.)
  @seealso(al_set_config_file) @seealso(al_set_config_string)
  @seealso(al_get_config_int) @seealso(al_get_config_hex)
  @seealso(al_get_config_float) *)
  FUNCTION al_get_config_string (CONST section, name, def: AL_STR): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_string';

(* Reads an integer variable from the current config file.  See the comments
   about @link(al_get_config_string). *)
  FUNCTION al_get_config_int (CONST section, name: AL_STR; def: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_int';

(* Reads an integer variable from the current config file, in hexadecimal.
   See the comments about @link(al_get_config_string). *)
  FUNCTION al_get_config_hex (CONST section, name: AL_STR; def: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_hex';

(* Reads a floating point variable from the current config file.  See the
   comments about @link(al_get_config_string). *)
  FUNCTION al_get_config_float (CONST section, name: AL_STR; def: AL_FLOAT): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_float';

(* Reads a 4-letter driver ID variable from the current config file.  See the
   comments about @link(al_get_config_string). *)
  FUNCTION al_get_config_id (CONST section, name: AL_STR; def: AL_INT32): AL_INT32;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_id';

(* This is pretty complex.
  FUNCTION al_get_config_argv (section, name: STRING; argc: PLONGINT): PSTRING;
 *)

(* Writes a string variable to the current config file, replacing any existing
   value it may have, or removes the variable if @code(val) is empty.  The
   @code(section) name may be set to a empty string to write the variable to
   the root of the file, or used to control which section the variable is
   inserted into.  The altered file will be cached in memory, and not actually
   written to disk until you call @link(al_exit).  Note that you can only write
   to files in this way, so the function will have no effect if the current
   config source was specified with @link(al_set_config_data) rather than
   @link(al_set_config_file).

   As a special case, variable or section names that begin with a '#' character
   are treated specially and will not be read from or written to the disk.
   Addon packages can use this to store version info or other status
   information into the config module, from where it can be read with the
   @code(al_get_config_string) function.
   @seealso(al_set_config_file) @seealso(al_get_config_string)
   @seealso(al_set_config_int) @seealso(al_set_config_hex)
   @seealso(al_set_config_float) @seealso(al_flush_config_file) *)
  PROCEDURE al_set_config_string (CONST section, name, val: AL_STR);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_string';

(* Writes an integer variable to the current config file.  See the comments
   about @link(al_set_config_string). *)
  PROCEDURE al_set_config_int (CONST section, name: AL_STR; val: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_int';

(* Writes an integer variable to the current config file, in hexadecimal
   format.  See the comments about @link(al_set_config_string). *)
  PROCEDURE al_set_config_hex (CONST section, name: AL_STR; val: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_hex';

(* Writes a floating point variable to the current config file.  See the
   comments about @link(al_set_config_string). *)
  PROCEDURE al_set_config_float (CONST section, name: AL_STR; val: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_float';

(* Writes a 4-letter driver ID variable to the current config file.  See the
   comments about @link(al_set_config_string). *)
  PROCEDURE al_set_config_id (CONST section, name: AL_STR; val: AL_INT32);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_id';



(******************************************************************************
 * timer.h
 *      Timer routines
 *)

  VAR
  (* If the retrace simulator is installed, this count is incremented on each
     vertical retrace; otherwise, if the refresh rate is known, the count is
     incremented at the same rate (ignoring retraces); otherwise, it is
     incremented 70 times a second.  This provides a way of controlling the speed
     of your program without installing user timer functions. *)
    al_retrace_count: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'retrace_count';

(* Installs the Allegro timer interrupt handler.  You must do this before
   installing any user timer routines, and also before displaying a mouse
   pointer and playing FLI animations or MIDI music.
   @returns(@true on success, or @false on failure @(but you may decide not to
     check the return value as this function is very unlikely to fail@).)
   @seealso(al_remove_timer) @seealso(al_install_int) *)
  FUNCTION al_install_timer: AL_BOOL;
    INLINE;

(* Removes the Allegro timer handler.  You don't normally need to bother
   calling this, because @link(al_exit) will do it for you.
   @seealso(al_install_timer) *)
  PROCEDURE al_remove_timer;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_timer';

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
     timer.)
   @seealso(al_remove_int) @seealso(al_install_int)
   @seealso(al_install_param_int_ex) *)
  FUNCTION al_install_int_ex (proc: AL_SIMPLE_PROC; speed: AL_LONG): AL_BOOL;
    INLINE;

(* Installs a user timer handler, with the speed given as the number of
   milliseconds between ticks.  This is the same thing as
   @code(al_install_int_ex @(@@proc, AL_MSEC_TO_TIMER @(speed@)@)).  If you call
   this routine without having first installed the timer module,
   @link(al_install_timer) will be called automatically.  Calling again this
   routine with the same timer handler as parameter allows you to adjust its speed.
   @returns(@true on success, or @false if there is no room to add a new user
     timer.)
   @seealso(al_remove_int) @seealso(al_install_int_ex)
   @seealso(al_install_param_int) *)
  FUNCTION al_install_int (proc: AL_SIMPLE_PROC; speed: AL_LONG): AL_BOOL;
    INLINE;

(* Removes a function from the list of user interrupt routines.
   @link(al_exit) does this automatically.
   @seealso(al_install_int) @seealso(al_remove_param_int) *)
  PROCEDURE al_remove_int (proc: AL_SIMPLE_PROC);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_int';

(* Like @link(al_install_int_ex), but the callback routine will be passed a
   copy of the specified void pointer parameter.  To disable the handler, use
   @link(al_remove_param_int) instead of @link(al_remove_int). *)
  FUNCTION al_install_param_int_ex
    (proc: AL_PARAM_PROC; param: AL_VOIDptr; speed: AL_LONG): AL_BOOL;
    INLINE;

(* Like @link(al_install_int), but the callback routine will be passed a copy
   of the specified void pointer parameter.  To disable the handler, use
   @link(al_remove_param_int) instead of @link(al_remove_int). *)
  FUNCTION al_install_param_int
    (proc: AL_PARAM_PROC; param: AL_VOIDptr; speed: AL_LONG): AL_BOOL;
    INLINE;

(* Like @link(al_remove_int), but for use with timer callbacks that have
   parameter values.  If there is more than one copy of the same callback
   active at a time, it identifies which one to remove by checking the
   parameter value (so you can't have more than one copy of a handler using an
   identical parameter).
   @seealso(al_install_param_int) @seealso(al_remove_int) *)
  PROCEDURE al_remove_param_int (proc: AL_PARAM_PROC; param: AL_VOIDptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_param_int';



(* Give the number of seconds between each tick to @link(al_install_int_ex). *)
  FUNCTION AL_SECS_TO_TIMER (CONST x: AL_LONG): AL_LONG; INLINE;
(* Give the number of milliseconds between each tick to
   @link(al_install_int_ex). *)
  FUNCTION AL_MSEC_TO_TIMER (CONST x: AL_LONG): AL_LONG; INLINE;
(* Give the number of ticks each second to @link(al_install_int_ex). *)
  FUNCTION AL_BPS_TO_TIMER  (CONST x: AL_LONG): AL_LONG; INLINE;
(* Give the number of ticks each minute to @link(al_install_int_ex). *)
  FUNCTION AL_BPM_TO_TIMER  (CONST x: AL_LONG): AL_LONG; INLINE;



(* This function waits for the specified number of milliseconds.

  Passing 0 as parameter will not wait, but just yield.  This can be useful in
  order to @italic("play nice") with other processes.  Other values will cause
  CPU time to be dropped on most platforms.  This will look better to users,
  and also does things like saving battery power and making fans less noisy.

  Note that calling this inside your active game loop is a bad idea, as you
  never know when the OS will give you the CPU back, so you could end up
  missing the vertical retrace and skipping frames.  On the other hand, on
  multitasking operating systems it is good form to give up the CPU for a while
  if you will not be using it. *)
  PROCEDURE al_rest (mseconds: AL_UINT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest';

(* Like @link(al_rest), but for non-zero values continually calls the
  specified function while it is waiting for the required time to elapse.  If
  the provided @code(callback) parameter is @nil, this function does exactly
  the same thing as calling @code(al_rest). *)
  PROCEDURE al_rest_callback (mseconds: AL_UINT; callback: AL_PARAM_PROC);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest_callback';



(******************************************************************************
 * keyboard.h
 *      Keyboard routines.
 *)

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
     check the return value as this function is very unlikely to fail@).)
   @seealso(al_remove_keyboard) @seealso(al_poll_keyboard) @seealso(al_key)
   @seealso(al_keypressed) @seealso(al_readkey) @seealso(al_ureadkey)
   @seealso(al_keyboard_lowlevel_callback) @seealso(al_three_finger_flag)
   @seealso(al_key_led_flag) @seealso(al_set_leds)
   @seealso(al_set_keyboard_rate) *)
  FUNCTION al_install_keyboard: AL_BOOL;
    INLINE;

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

   The @link(al_keypressed), @link(al_readkey), and @link(al_ureadkey)
   functions call @code(al_poll_keyboard) automatically, so you only need to
   use this function when accessing the @link(al_key) array and
   @link(al_key_shifts) variable.

   @returns(@false on success or @true on failure @(ie. no keyboard driver
     installed@).) *)
  FUNCTION al_poll_keyboard: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_keyboard';

(* Returns @true if the current keyboard driver is operating in polling mode. *)
  FUNCTION al_keyboard_needs_poll: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'keyboard_needs_poll';



  VAR
  (* If set, this function is called by the keyboard handler in response to every
     keyboard event, both presses (including keyboard repeat rate) and releases.
     It will be passed a raw keyboard scancode byte (scancodes are 7 bits long),
     with the top bit (8th bit) clear if the key has been pressed or set if it
     was released.  This routine executes in an interrupt context, so it must be
     used carefully and only if needed.
    @seealso(al_install_keyboard) *)
    al_keyboard_lowlevel_callback: AL_INT_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'keyboard_lowlevel_callback';

  TYPE
  { @exclude }
    AL_KEY_LIST = ARRAY [0..126] OF AL_BOL8;

  VAR
  (* Array of flags indicating the state of each key, ordered by scancode.
     Wherever possible these values will be updated asynchronously, but if
     @link(al_keyboard_needs_poll) returns @true, you must manually call
     @link(al_poll_keyboard) to update them with the current input state.  The
     scancodes are defined as a series of @code(AL_KEY_* ) constants (and are
     also listed below). For example, you could write:
     @longcode(#
  IF al_key[AL_KEY_SPACE] THEN
    WriteLn ('Space is pressed');
     #)

     Note that the array is supposed to represent which keys are physically held
     down and which keys are not, so it is semantically read-only.

     These are the keyboard scancodes:
     @longcode(#
          AL_KEY_A ... AL_KEY_Z,
          AL_KEY_0 ... AL_KEY_9,
          AL_KEY_0_PAD ... AL_KEY_9_PAD,
          AL_KEY_F1 ... AL_KEY_F12,

          AL_KEY_ESC, AL_KEY_TILDE, AL_KEY_MINUS, AL_KEY_EQUALS,
          AL_KEY_BACKSPACE, AL_KEY_TAB, AL_KEY_OPENBRACE, AL_KEY_CLOSEBRACE,
          AL_KEY_ENTER, AL_KEY_COLON, AL_KEY_QUOTE, AL_KEY_BACKSLASH,
          AL_KEY_BACKSLASH2, AL_KEY_COMMA, AL_KEY_STOP, AL_KEY_SLASH,
          AL_KEY_SPACE,

          AL_KEY_INSERT, AL_KEY_DEL, AL_KEY_HOME, AL_KEY_END, AL_KEY_PGUP,
          AL_KEY_PGDN, AL_KEY_LEFT, AL_KEY_RIGHT, AL_KEY_UP, AL_KEY_DOWN,

          AL_KEY_SLASH_PAD, AL_KEY_ASTERISK, AL_KEY_MINUS_PAD,
          AL_KEY_PLUS_PAD, AL_KEY_DEL_PAD, AL_KEY_ENTER_PAD,

          AL_KEY_PRTSCR, AL_KEY_PAUSE,

          AL_KEY_ABNT_C1, AL_KEY_YEN, AL_KEY_KANA, AL_KEY_CONVERT,
          AL_KEY_NOCONVERT, AL_KEY_AT, AL_KEY_CIRCUMFLEX, AL_KEY_COLON2,
          AL_KEY_KANJI,

          AL_KEY_LSHIFT, AL_KEY_RSHIFT,
          AL_KEY_LCONTROL, AL_KEY_RCONTROL,
          AL_KEY_ALT, AL_KEY_ALTGR,
          AL_KEY_LWIN, AL_KEY_RWIN, AL_KEY_MENU,
          AL_KEY_SCRLOCK, AL_KEY_NUMLOCK, AL_KEY_CAPSLOCK,

          AL_KEY_EQUALS_PAD, AL_KEY_BACKQUOTE, AL_KEY_SEMICOLON, AL_KEY_COMMAND
     #)

     Finally, you may notice an @italic(`odd') behaviour of the
     @code(AL_KEY_PAUSE) key.  This key only generates an interrupt when it is
     pressed, not when it is released.  For this reason, Allegro pretends the
     pause key is a @italic(`state') key, which is the only way to make it
     usable.
     @seealso(al_install_keyboard) @seealso(al_poll_keyboard)
     @seealso(al_key_shifts) *)
    al_key: AL_KEY_LIST;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'key';

  (* Bitmask containing the current state of @code(shift/ctrl/alt), the special
     Windows keys, and the accent escape characters.  Wherever possible this
     value will be updated asynchronously, but if @code(al_keyboard_needs_poll)
     returns @true, you must manually call @code(al_poll_keyboard) to update it
     with the current input state.  This can contain any of the flags:
     @longcode(#
          AL_KB_SHIFT_FLAG
          AL_KB_CTRL_FLAG
          AL_KB_ALT_FLAG
          AL_KB_LWIN_FLAG
          AL_KB_RWIN_FLAG
          AL_KB_MENU_FLAG
          AL_KB_COMMAND_FLAG
          AL_KB_SCROLOCK_FLAG
          AL_KB_NUMLOCK_FLAG
          AL_KB_CAPSLOCK_FLAG
          AL_KB_INALTSEQ_FLAG
          AL_KB_ACCENT1_FLAG
          AL_KB_ACCENT2_FLAG
          AL_KB_ACCENT3_FLAG
          AL_KB_ACCENT4_FLAG
     #)
     @seealso(al_install_keyboard) @seealso(al_poll_keyboard)
     @seealso(al_key) *)
    al_key_shifts: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'key_shifts';

  (* The DJGPP keyboard handler provides an 'emergency exit' sequence which you
     can use to kill off your program.  If you are running under DOS this is the
     three finger salute, ctrl+alt+del.  Most multitasking OS's will trap this
     combination before it reaches the Allegro handler, in which case you can use
     the alternative ctrl+alt+end.  If you want to disable this behaviour in
     release versions of your program, set this flag to @true.
     @seealso(al_install_keyboard) *)
    al_three_finger_flag: AL_BOOL;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'three_finger_flag';

  (* By default, the capslock, numlock, and scroll-lock keys toggle the keyboard
     LED indicators when they are pressed.  If you are using these keys for input
     in your game (eg. capslock to fire) this may not be desirable, so you can
     set this flag to @false and prevent the LED's being updated.
     @seealso(al_install_keyboard) @seealso(al_set_leds) *)
    al_key_led_flag: AL_BOOL;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'key_led_flag';



(* Returns @true if there are keypresses waiting in the input buffer.  You can
   use this to see if the next call to @link(al_readkey) is going to block or
   to simply wait for the user to press a key while you still update the screen
   possibly drawing some animation.  Example:
   @longcode(#
  REPEAT
    AnimateLogo (al_screen)
  UNTIL al_keypressed;
   #)
     @seealso(al_install_keyboard) @seealso(al_readkey)
     @seealso(al_simulate_keypress) @seealso(al_clear_keybuf) *)
  FUNCTION al_keypressed: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'keypressed';

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
  val: LONGINT;
  ...
  val := al_readkey;

  IF (val AND $ff) = ORD ('d') THEN
    al_message ('You pressed "d"');

  IF (val SHR 8) = AL_KEY_SPACE THEN
    al_message ('You pressed Space');

  IF (val AND $ff) = 3 THEN
    al_message ('You pressed Control+C');

  IF val = (AL_KEY_X SHL 8) THEN
    al_message ('You pressed Alt+X');
   #)

   This function cannot return character values greater than 255.  If you need
   to read Unicode input, use @link(al_ureadkey) instead.
   @seealso(al_install_keyboard) @seealso(al_ureadkey) @seealso(al_key)
   @seealso(al_clear_keybuf) @seealso(al_simulate_keypress) *)
  FUNCTION al_readkey: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'readkey';

(* Returns the next character from the keyboard buffer, in Unicode format.  If
   the buffer is empty, it waits until a key is pressed.  You can see if there
   are queued keypresses with @link(al_keypressed).  The return value contains
   the Unicode value of the key, and the argument will be set to the scancode.
   Unlike @link(al_readkey), this function is able to return character values
   greater than 255.  Example:
   @longcode(#
VAR
  val, scancode: LONGINT;
  ...
  val := al_ureadkey (scancode);
  IF val = $00F1 THEN
    al_message ('You pressed n with tilde');

  IF val = $00DF THEN
    al_message ('You pressed sharp s');
   #)

   You should be able to find Unicode character maps at
   http://www.unicode.org/.
   @seealso(al_readkey) *)
  FUNCTION al_ureadkey (OUT scancode: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'ureadkey';

(* Stuffs a key into the keyboard buffer, just as if the user had pressed it.
   The parameter is in the same format returned by @link(al_readkey).
   @seealso(al_simulate_ukeypress) *)
  PROCEDURE al_simulate_keypress (keycode: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_keypress';

(* Stuffs a key into the keyboard buffer, just as if the user had pressed it.
   The parameter is in the same format returned by @link(al_ureadkey).
   @seealso(al_simulate_ukeypress) *)
  PROCEDURE al_simulate_ukeypress (keycode, scancode: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_ukeypress';

(* Empties the keyboard buffer.  Usually you want to use this in your program
   before reading keys to avoid previously buffered keys to be returned by
   calls to @link(al_readkey) or @link(al_ureadkey).
   @seealso(al_install_keyboard) *)
  PROCEDURE al_clear_keybuf;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_keybuf';

(* Overrides the state of the keyboard LED indicators.  The parameter is a
   bitmask containing any of the values @code(AL_KB_SCROLOCK_FLAG),
   @code(AL_KB_NUMLOCK_FLAG), and @code(AL_KB_CAPSLOCK_FLAG), or @code(-1) to
   restore the default behavior.

   Note that the led behaviour cannot be guaranteed on some platforms, some
   leds might not react, or none at all.  Therefore you shouldn't rely only on
   them to communicate information to the user, just in case it doesn't get
   through.
   @seealso(al_install_keyboard) @seealso(al_key_led_flag) *)
  PROCEDURE al_set_leds (leds: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_leds';

(* Sets the keyboard repeat rate.  Times are given in milliseconds.  Passing
   zero times will disable the key repeat.
   @seealso(al_install_keyboard) @seealso(al_readkey) *)
  PROCEDURE al_set_keyboard_rate (key_delay, key_repeat: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_keyboard_rate';

(* Converts the given scancode to an ASCII character for that key (mangling
   Unicode values), returning the unshifted uncapslocked result of pressing the
   key, or zero if the key isn't a character-generating key or the lookup can't
   be done.  The lookup cannot be done for keys like the F1-F12 keys or the
   cursor keys, and some drivers will only return approximate values.
   Generally, if you want to display the name of a key to the user, you should
   use the @link(al_scancode_to_name) function. *)
  FUNCTION al_scancode_to_ascii (scancode: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scancode_to_ascii';

(* This function returns a string pointer containing the name of they key with
   the given scancode.  This is useful if you e.g. let the user choose a key
   for some action, and want to display something more meaningful than just the
   scancode. @seealso(al_scancode_to_ascii)  *)
  FUNCTION al_scancode_to_name (scancode: AL_INT): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scancode_to_name';

  {$INCLUDE alkeyid.inc}

(******************************************************************************
 * joystick.h
 *      Joystick routines.
 *)

  CONST
  (* Attempts to autodetect your joystick hardware. It will use information
     from the configuration file if one is available (this can be created using
     the setup utility or by calling the @code(al_save_joystick_data)
     function), so you can always use @code(AL_JOY_TYPE_AUTODETECT) in your
     code and then select the exact hardware type from the setup program.
     @seealso(al_save_joystick_data) @seealso(al_install_joystick). *)
    AL_JOY_TYPE_AUTODETECT = -1;
  (* Dummy driver for machines without any joystick.
     @seealso(al_install_joystick) *)
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
     with the centre point.
     @seealso(al_joy) @seealso(AL_JOYSTICK_INFO) *)
    AL_JOYSTICK_AXIS_INFO = RECORD
      pos : AL_INT; {< analogue axis position. }
      d1 : AL_BOOL; {<digital axis position. }
      d2 : AL_BOOL; {<digital axis position. }
      name : AL_STRptr; {< description of this axis. }
    END;

  (* information about one or more axis (a slider or directional control)
    @seealso(al_joy) @seealso(AL_JOYSTICK_INFO) *)
    AL_JOYSTICK_STICK_INFO = RECORD
      flags : AL_INT;{< status flags for this input. }
      num_axis : AL_INT; {< how many axes do we have? (note the misspelling). }
    { axis state information. @seealso(AL_JOYSTICK_AXIS_INFO) }
      axis : ARRAY [0..(AL_MAX_JOYSTICK_AXIS)-1] OF AL_JOYSTICK_AXIS_INFO;
      name : AL_STRptr; {< description of this input. }
    END;

  (* information about a joystick button.

     You may wish to display the button names as part of an input configuration
     screen to let the user choose what game function will be performed by each
     button, but in simpler situations you can safely assume that the first two
     elements in the button array will always be the main trigger controls.
    @seealso(al_joy) @seealso(AL_JOYSTICK_INFO) *)
    AL_JOYSTICK_BUTTON_INFO = RECORD
      b : AL_BOOL; {<@false not pressed, @true pressed. }
      name : AL_STRptr; {<description of this button. }
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
       @code(al_calibrate_joystick) function from a loop until this flag is
       cleared.)
       @itemLabel(AL_JOYFLAG_SIGNED)
       @item(Indicates that the analogue axis position is in signed format,
         ranging from -128 to 128. This is the case for all 2d directional
         controls.)
       @itemLabel(AL_JOYFLAG_UNSIGNED)
       @item(Indicates that the analogue axis position is in unsigned format,
         ranging from 0 to 255. This is the case for all 1d throttle controls.)
     ) @seealso(al_joy) *)
    AL_JOYSTICK_INFO = RECORD
      flags : AL_INT; {< status flags for this joystick. }
      num_sticks : AL_INT; {<  how many stick inputs? }
      num_buttons : AL_INT; {< how many buttons? }
    { stick state information. @seealso(AL_JOYSTICK_STICK_INFO) }
      stick : ARRAY [0..(AL_MAX_JOYSTICK_STICKS)-1] OF AL_JOYSTICK_STICK_INFO;
    { button state information. @seealso(AL_JOYSTICK_BUTTON_INFO). }
      button : ARRAY [0..(AL_MAX_JOYSTICK_BUTTONS)-1] OF AL_JOYSTICK_BUTTON_INFO;
    END;

  { List of joysticks. }
    AL_JOYSTICK_INFO_LIST = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_JOYSTICK_INFO;

  VAR
  (* Global array of joystick state information, which is updated by the
     @link(al_poll_joystick) function.  Only the first @link(al_num_joysticks)
     elements will contain meaningful information.

     A single joystick may provide several different stick inputs, but you can
     safely assume that the first element in the stick array will always be the
     main directional controller.

     Information about each of the stick axis is stored in the
     @link(AL_JOYSTICK_AXIS_INFO) substructure.

     Note for people who spell funny: in case you don't like having to type
     @italic (analogue), there are some aliases in this unit that will allow you
     to write @italic(analog) instead. *)
    al_joy: AL_JOYSTICK_INFO_LIST;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'joy';

  (* Global variable containing the number of active joystick devices.  The
     current drivers support a maximum of eight controllers.
     @seealso(al_joy) *)
    al_num_joysticks: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'num_joysticks';

(* Installs Allegro's joystick handler, and calibrates the centre position
   values.  The type parameter should usually be @link(AL_JOY_TYPE_AUTODETECT),
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
   #)
   @returns(@true on success.  As soon as you have installed the joystick
     module, you will be able to read the button state and digital @(on/off
     toggle@) direction information, which may be enough for some games.  If
     you want to get full analogue input, though, you need to use the
     @link(al_calibrate_joystick) functions to measure the exact range of the
     inputs.)
     @seealso(al_remove_joystick) @seealso(al_num_joysticks)
     @seealso(al_load_joystick_data) @seealso(al_poll_joystick)
     @seealso(AL_JOY_TYPE_AUTODETECT) @seealso(alWin) @seealso(alUNIX) *)
  FUNCTION al_install_joystick (CONST atype: AL_INT): AL_BOOL;
    INLINE;

(* Removes the joystick handler. You don't normally need to bother calling
   this, because @link(al_exit) will do it for you.
   @seealso(al_install_joystick) *)
  PROCEDURE al_remove_joystick;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_joystick';

(* Pass the number of the joystick you want to calibrate as the parameter.

    @returns(a text description for the next type of calibration that will
      be done on the specified joystick, or @nil if no more calibration
      is required.)
    @seealso(al_calibrate_joystick) @seealso(al_num_joysticks) *)
  FUNCTION al_calibrate_joystick_name (CONST n: AL_INT): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'calibrate_joystick_name';

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
     successfully.)
    @seealso(al_num_joysticks) *)
  FUNCTION al_calibrate_joystick (n: AL_INT): AL_BOOL;
    INLINE;

(* After all the headache of calibrating the joystick, you may not want to make
   your poor users repeat the process every time they run your program.  Call
   this function to save the joystick calibration data into the specified
   configuration file, from which it can later be read by
   @link(al_load_joystick_data).  Pass a @nil filename to write the data to the
   currently selected configuration file.
   @returns(@true on success, @false if the data could not be saved.)
   @seealso(al_calibrate_joystick) *)
  FUNCTION al_save_joystick_data (CONST filename: AL_STR): AL_BOOL;
    INLINE;

(* Restores calibration data previously stored by @link(al_save_joystick_data)
   or the setup utility.  This sets up all aspects of the joystick code:  you
   don't even need to call @link(al_install_joystick) if you are using this
   function.  Pass an empty filename to read the data from the currently
   selected configuration file.
   @returns(@true on success:  if it fails the joystick state is undefined and
     you must reinitialise it from scratch.)
   @seealso(al_calibrate_joystick) *)
  FUNCTION al_load_joystick_data (CONST filename: AL_STR): AL_BOOL;

(* The joystick handler is not interrupt driven, so you need to call this
   function every now and again to update the global position values.
   @returns(@false on success or @true on failure @(usually because no joystick
     driver was installed@).)
   @seealso(al_install_joystick) @seealso(al_joy) *)
  FUNCTION al_poll_joystick: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_joystick';



(******************************************************************************
 * palette.h
 *      Palette type.
 *)

  TYPE
  (* Pointer to @link(AL_RGB). *)
    AL_RGBptr = ^AL_RGB;
  (* Palette entry.  It contains an additional field for the purpose of padding
     but you should not usually care about it. @seealso(AL_PALETTE) *)
    AL_RGB = RECORD
      r, g, b: AL_UCHAR;
      filler: AL_UCHAR;
    END;

  CONST
  (* To know the palette size. @seealso(AL_PALETTE) *)
    AL_PAL_SIZE = 256;

  TYPE
  (* Pointer to a @link(AL_PALETTE). *)
    AL_PALETTEptr = ^AL_PALETTE;
  (* Color palette description for indexed modes (8bpp).  Remember that color
     components are 0-63. @seealso(AL_PAL_SIZE) @seealso(al_set_palette) *)
    AL_PALETTE = ARRAY [0..AL_PAL_SIZE-1] OF AL_RGB;



(******************************************************************************
 * color.h
 *      Color manipulation routines
 *
 * NOTE: AL_COLOR_MAP description is at alvga unit.
 *)

  VAR
  (* A palette containing solid black colors, used by the fade routines.
    @seealso(al_desktop_palette) @seealso(al_default_palette) *)
    al_black_palette: AL_PALETTE;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'black_palette';
  (* The palette used by the Atari ST low resolution desktop. I'm not quite sure
     why this is still here, except that the original grabber and test programs
     use it.  It is probably the only Atari legacy code left in Allegro, and it
     would be a shame to remove it :-)

     The contents of this palette are 16 colors repeated 16 times.  Color entry
     zero is equal to color entry 16, which is equal to color entry 32, etc.
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
)
    @seealso(al_black_palette) @seealso(al_default_palette) *)
    al_desktop_palette: AL_PALETTE;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'desktop_palette';
  (* The default IBM BIOS palette.  This will be automatically selected whenever
     you set a new graphics mode.  The palette contains 16 basic colors plus many
     gradients between them.  If you want to see the values, you can write a
     small Allegro program which saves a screenshot with this palette, or open
     the grabber tool provided with Allegro and create a new palette object,
     which will use this palette by default.
    @seealso(al_black_palette) @seealso(al_desktop_palette) *)
    al_default_palette: AL_PALETTE;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'default_palette';

  TYPE
  (* Pointer to a @link(AL_RGB_MAP). *)
    AL_RGB_MAPptr = ^AL_RGB_MAP;
  (* Stores an rgb map to accelerate conversions. @seealso(al_create_rgb_table) *)
    AL_RGB_MAP = RECORD
      data: ARRAY [0..31, 0..31, 0..31] OF AL_UCHAR;
    END;

  VAR
  (* To speed up reducing RGB values to 8-bit paletted colors, Allegro uses a 32k
     lookup table (5 bits for each color component).  You must set up this table
     before using the gouraud shading routines, and if present the table will
     also vastly accelerate the @link(al_makecol) and some
     @code(al_create_*_table) functions on 8-bit graphic mode.  RGB tables can be
     precalculated with the rgbmap utility, or generated at runtime with
     @link(al_create_rgb_table).
     @seealso(al_create_trans_table) @seealso(al_create_light_table) *)
    al_rgb_table: AL_RGB_MAPptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_map';

  TYPE
  (* Pointer to @link(AL_PALETTE_DICT). *)
    AL_PALETTE_DICTptr = ^AL_PALETTE_DICT;
  (* To be used by @link(al_palette_color). *)
    AL_PALETTE_DICT = ARRAY [0..255] OF AL_INT;
  VAR
  (* Table mapping palette index colors (0-255) into whatever pixel format is
     being used by the current display mode.  In a 256-color mode this just maps
     onto the array index.  In truecolor modes it looks up the specified entry in
     the current palette, and converts that RGB value into the appropriate packed
     pixel format.
     @seealso(al_set_palette) @seealso(al_makecol) @seealso(al_set_color_depth) *)
    al_palette_color: AL_PALETTE_DICTptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'palette_color';

  (* @exclude For some reason, this is undocumented, so keep it as it is. *)
    al_current_palette: AL_PALETTE ;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'current_palette';

(* Sets the specified palette entry to the specified @link(AL_RGB) triplet.
   Unlike the other palette functions this doesn't do any retrace
   synchronisation, so you should call @link(al_vsync) before it to prevent
   snow problems. @seealso(al_set_palette) @seealso(al_get_color) *)
  PROCEDURE al_set_color (idx: AL_INT; VAR p: AL_RGB);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color';

(* Sets the entire palette of 256 colors.  You should provide an array of 256
   RGB structures.  Unlike @link(al_set_color), there is no need to call
   @link(al_vsync) before this function. @seealso(al_get_palette)
   @seealso(al_set_color) @seealso(al_set_palette_range) *)
  PROCEDURE al_set_palette (VAR p: AL_PALETTE);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_palette';

(* Sets the palette entries between @code(aFrom) and @code(aTo) (inclusive:
   pass 0 and 255 to set the entire palette).  If @code(vsync) is @true it
   waits for the vertical retrace, otherwise it sets the colors immediately.
   @seealso(al_set_color) @seealso(al_set_palette) *)
  PROCEDURE al_set_palette_range
    (VAR p: AL_PALETTE; aFrom, aTo: AL_INT; vsync: AL_BOOL);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_palette_range';



(* Retrieves the specified palette entry.
   @seealso(al_get_palette) @seealso(al_set_color) *)
  PROCEDURE al_get_color (idx: AL_INT; OUT p: AL_RGB);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color';

(* Retrieves the entire palette of 256 colors.
    @seealso(al_get_palette_range) *)
  PROCEDURE al_get_palette (VAR p: AL_PALETTE);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_palette';

(* Retrieves the palette entries between @code(aFrom) and @code(aTo)
   (inclusive: pass 0 and 255 to set the entire palette).
   @seealso(al_get_palette) *)
  PROCEDURE al_get_palette_range (VAR p: AL_PALETTE; aFrom, aTo: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_palette_range';



(* Calculates a temporary palette part way between @code(source) and
   @code(dest), returning it in the @code(aOutput) parameter.  The position between
   the two extremes is specified by the pos value: 0 returns an exact copy of
   source, 64 returns dest, 32 returns a palette half way between the two, etc.
   This routine only affects colors between @code(aFrom) and @code(aTo)
   (inclusive: pass 0 and 255 to interpolate the entire palette).
   @seealso(al_fade_in) @seealso(al_fade_out) @seealso(al_fade_from) *)
  PROCEDURE al_fade_interpolate
    (VAR source, dest, aOutput: AL_PALETTE; aPos, aFrom, aTo: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_interpolate';

(* Gradually fades a part of the palette from the @code(source) palette to the
   @code(dest) palette.  The @code(speed) is from 1 (the slowest) up to 64
   (instantaneous).  This routine only affects colors between @code(from) and
   @code(ato) (inclusive: pass 0 and 255 to fade the entire palette).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. @seealso(al_fade_from) *)
  PROCEDURE al_fade_from_range
    (VAR source, dest: AL_PALETTE; speed, from, ato: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_from_range';

(* Gradually fades a part of the palette from a black screen to the specified
   palette.  The @code(speed) is from 1 (the slowest) up to 64
   (instantaneous).  This routine only affects colors between @code(from) and
   @code(ato) (inclusive: pass 0 and 255 to fade the entire palette).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. @seealso(al_fade_in) *)
  PROCEDURE al_fade_in_range (VAR p: AL_PALETTE; speed, from, ato: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_in_range';

(* Gradually fades a part of the current palette to the @code(dest) palette.
   The @code(speed) is from 1 (the slowest) up to 64 (instantaneous).  This
   routine only affects colors between @code(from) and @code(ato) (inclusive:
   pass 0 and 255 to fade the entire palette).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. @seealso(al_fade_out) *)
  PROCEDURE al_fade_out_range (speed, from, ato: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_out_range';

(* Gradually fades from the @code(source) palette to the @code(dest) palette.
   The @code(speed) is from 1 (the slowest) up to 64 (instantaneous).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. @seealso(al_fade_in) @seealso(al_fade_out)
   @seealso(al_fade_interpolate) @seealso(al_fade_from_range) *)
  PROCEDURE al_fade_from (VAR source, dest: AL_PALETTE; speed: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_from';

(* Gradually fades from a black screen to the specified palette.  The
   @code(speed) is from 1 (the slowest) up to 64 (instantaneous).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. @seealso(al_fade_from) @seealso(al_fade_out)
   @seealso(al_fade_interpolate) @seealso(al_fade_in_range) *)
  PROCEDURE al_fade_in (VAR p: AL_PALETTE; speed: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_in';

(* Gradually fades from the current palette to a black screen.  The
   @code(speed) is from 1 (the slowest) up to 64 (instantaneous).

   Note that this function will block your game while the fade is in effect,
   and it won't work right visually if you are not in an 8 bit color depth
   resolution. @seealso(al_fade_from) @seealso(al_fade_in)
   @seealso(al_fade_interpolate) @seealso(al_fade_out_range) *)
  PROCEDURE al_fade_out (speed: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fade_out';



(* Ugly hack for use in various dodgy situations where you need to convert
   between paletted and truecolor image formats.  Sets the internal palette
   table in the same way as the @link(al_set_palette) function, so the
   conversion will use the specified palette, but without affecting the
   display hardware in any way.  The previous palette settings are stored in an
   internal buffer, and can be restored by calling @link(al_unselect_palette).
   If you call @code(al_select_palette) again, however, the internal buffer
   will be overwritten. *)
  PROCEDURE al_select_palette (VAR p: AL_PALETTE);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'select_palette';

(* Restores the palette tables that were in use before the last call to
   @link(al_select_palette). *)
  PROCEDURE al_unselect_palette;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unselect_palette';



(* Constructs a fake truecolor palette, using three bits for red and green and
   two for the blue.  The @link(al_load_bitmap) function fills the palette
   parameter with this if the file does not contain a palette itself (ie. you
   are reading a truecolor bitmap).
   @seealso(al_generate_optimized_palette) @seealso(al_set_color_depth) *)
  PROCEDURE al_generate_332_palette (VAR p: AL_PALETTE);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_332_palette';

(* Generates a 256-color palette suitable for making a reduced color version of
   the specified truecolor image.
   @param(image The trucolor image to convert.)
   @param(pal The generated palette.)
   @param(rsvdcols points to a table indicating which colors it is allowed to
     modify:  zero for free colors which may be set to whatever the optimiser
     likes, negative values for reserved colors which cannot be used,
     and positive values for fixed palette entries that must not be changed,
     but can be used in the optimisation.)
   @returns(the number of different colors recognised in the provided bitmap,
     zero if the bitmap is not a truecolor image or there wasn't enough memory
     to perform the operation, and negative if there was any internal error in
     the color reduction code.)
   @seealso(al_generate_332_palette) @seealso(al_set_color_depth) *)
  FUNCTION  al_generate_optimized_palette
    (image: AL_BITMAPptr; VAR pal: AL_PALETTE; VAR rsvdcols: ARRAY OF AL_CHAR): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'generate_optimized_palette';



(* Fills the specified RGB mapping table with lookup data for the specified
   palette.  If the @code(callback) function is not @nil, it will be called
   256 times during the calculation, allowing you to display a progress
   indicator. *)
  PROCEDURE al_create_rgb_table
    (VAR table: AL_RGB_MAP; VAR pal: AL_PALETTE; callback: AL_INT_PROC);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_rgb_table';

{ NOTE: Other table creation functions are in unit alvga. }



(* Convert color values between the HSV and RGB color spaces.  The RGB values
   range from 0 to 255, hue is from 0 to 360, and saturation and value are from
   0 to 1. @seealso(al_rgb_to_hsv) *)
  PROCEDURE al_hsv_to_rgb (h, s, v: AL_FLOAT; OUT r, g, b: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'hsv_to_rgb';

(* Convert color values between the HSV and RGB color spaces.  The RGB values
   range from 0 to 255, hue is from 0 to 360, and saturation and value are from
   0 to 1. @seealso(al_hsv_to_rgb) *)
  PROCEDURE al_rgb_to_hsv (r, g, b: AL_INT; OUT h, s, v: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rgb_to_hsv';



(* Searches the specified palette for the closest match to the requested color,
   which are specified in the VGA hardware 0-63 format.  Normally you should
   call @code(al_makecol_depth) instead, but this lower level function may be
   useful if you need to use a palette other than the currently selected one,
   or specifically don't want to use the @code(al_rgb_map) lookup table.

   @returns(the index of the palette for the closest match to the requested
     color.)
   @seealso(al_makecol8) *)
  FUNCTION al_bestfit_color (VAR pal: AL_PALETTE; r, g, b: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'bestfit_color';



(* Converts colors from a hardware independent format (red, green, and blue
   values ranging 0-255) to the pixel format required by the current video
   mode, calling the preceding 8, 15, 16, 24, or 32-bit makecol functions as
   appropriate.

   @returns(the requested RGB triplet in the current color depth.)
   @seealso(al_makeacol) @seealso(al_makecol8) @seealso(al_makecol_depth)
   @seealso(AL_RGB_MAP) @seealso(al_set_color_depth) *)
  FUNCTION al_makecol (r, g, b: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol';

(* Converts colors from a hardware independent form (red, green, and blue
   values ranging 0-255) into 8-bit color index.  Converting to an 8-bit color
   involves searching the palette to find the closest match, which is quite
   slow unless you have set up an RGB mapping table.

   @returns(the requested RGB triplet in the specified color depth.)
   @seealso(al_makecol) @seealso(al_makecol_depth) @seealso(AL_RGB_MAP)
   @seealso(al_set_color_depth) *)
  FUNCTION al_makecol8 (r, g, b: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol8';

(* Converts colors from a hardware independent format (red, green, and blue
   values ranging 0-255) to the pixel format required by the specified color
   depth.

   @returns(the requested RGB triplet in the specified color depth.)
   @seealso(al_makecol) @seealso(al_makecol_depth) @seealso(AL_RGB_MAP)
   @seealso(al_set_color_depth) *)
  FUNCTION al_makecol_depth (color_depth, r, g, b: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makecol_depth';



(* Convert RGBA colors into display dependent pixel formats.  In anything less
   than a 32-bit mode, these are the same as calling @link(al_makecol) or
   @link(al_makecol_depth), but by using these routines it is possible to
   create 32-bit color values that contain a true 8 bit alpha channel along
   with the red, green, and blue components.  You should only use RGBA format
   colors as the input to @link(al_draw_trans_sprite) or
   @link(al_draw_trans_rle_sprite) after calling @link(al_set_alpha_blender),
   rather than drawing them directly to the screen.

   @returns(the requested RGBA quadruplet.) *)
  FUNCTION al_makeacol (r, g, b, a: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makeacol';

(* Convert RGBA colors into display dependent pixel formats.  In anything less
   than a 32-bit mode, these are the same as calling @link(al_makecol) or
   @link(al_makecol_depth), but by using these routines it is possible to
   create 32-bit color values that contain a true 8 bit alpha channel along
   with the red, green, and blue components.  You should only use RGBA format
   colors as the input to @link(al_draw_trans_sprite) or
   @link(al_draw_trans_rle_sprite) after calling @link(al_set_alpha_blender),
   rather than drawing them directly to the screen.

   @returns(the requested RGBA quadruplet.) *)
  FUNCTION al_makeacol_depth (color_depth, r, g, b, a: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'makeacol_depth';



(* Given a color in the format being used by the current video mode, this
   function extracts the red component (ranging 0-255), calling the
   preceding 8, 15, 16, 24, or 32-bit get functions as
   appropriate. @seealso(al_getr_depth) @seealso(al_getg) @seealso(al_getb)
   @seealso(al_geta) *)
  FUNCTION al_getr (c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getr';
(* Given a color in the format being used by the current video mode, this
   function extracts the green component (ranging 0-255), calling the
   preceding 8, 15, 16, 24, or 32-bit get functions as
   appropriate. @seealso(al_getg_depth) @seealso(al_getr) @seealso(al_getb)
   @seealso(al_geta) *)
  FUNCTION al_getg (c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getg';
(* Given a color in the format being used by the current video mode, this
   function extracts the blue component (ranging 0-255), calling the
   preceding 8, 15, 16, 24, or 32-bit get functions as
   appropriate. @seealso(al_getb_depth) @seealso(al_getr) @seealso(al_getg)
   @seealso(al_geta) *)
  FUNCTION al_getb (c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getb';
(* Given a color in the format being used by the current video mode, this
   function extracts the alpha chanel value (ranging 0-255), calling the
   preceding 8, 15, 16, 24, or 32-bit get functions as
   appropriate.  The alpha part is only meaningful for 32-bit pixels.
   @seealso(al_geta_depth) @seealso(al_getr) @seealso(al_getg)
   @seealso(al_getb) *)
  FUNCTION al_geta (c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'geta';



(* Given a color in the format being used by the specified color depth, this
   functions extract the red component (ranging 0-255).
   @seealso(al_getr) @seealso(al_getg_depth) @seealso(al_getb_depth)
   @seealso(al_geta_depth)*)
  FUNCTION al_getr_depth (color_depth, c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getr_depth';
(* Given a color in the format being used by the specified color depth, this
   functions extract the green component (ranging 0-255).
   @seealso(al_getg) @seealso(al_getr_depth) @seealso(al_getb_depth)
   @seealso(al_geta_depth)*)
  FUNCTION al_getg_depth (color_depth, c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getg_depth';
(* Given a color in the format being used by the specified color depth, this
   functions extract the blue component (ranging 0-255).
   @seealso(al_getb) @seealso(al_getr_depth) @seealso(al_getg_depth)
   @seealso(al_geta_depth)*)
  FUNCTION al_getb_depth (color_depth, c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'getb_depth';
(* Given a color in the format being used by the specified color depth, this
   functions extract the alpha channel value (ranging 0-255). The alpha part is
   only meaningful for 32-bit pixels.
   @seealso(al_geta) @seealso(al_getr_depth) @seealso(al_getg_depth)
   @seealso(al_getb_depth)*)
  FUNCTION al_geta_depth (color_depth, c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'geta_depth';



(******************************************************************************
 * gfx.h
 *      Basic graphics support routines.
 *)

  TYPE
  (* Pointer to @code(AL_BITMAP). *)
    AL_BITMAPptr = ^AL_BITMAP;
  (* @exclude
    This is for internal low-level access.  DO NOT USE IT IN YOUR GAMES. *)
    _BMP_BANK_SWITCHER = FUNCTION (bmp: AL_BITMAPptr; lyne: AL_INT): AL_VOIDptr; CDECL;

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
     Use @link(al_set_clip_rect) instead.
     @seealso(al_create_bitmap) @seealso(AL_RLE_SPRITE) *)
    AL_BITMAP = RECORD
      w, h: AL_INT;		{< width and height in pixels }
      clip: AL_BOOL;		{< flag if clipping is turned on }
      cl, cr, ct, cb: AL_INT;	{< clip left, right, top and bottom values }
      vtable: AL_GFX_VTABLEptr;	{< @exclude drawing functions }
      write_bank: _BMP_BANK_SWITCHER;	{< @exclude C func on some machines, asm on i386 }
      read_bank: _BMP_BANK_SWITCHER;	{< @exclude C func on some machines, asm on i386 }
      dat: AL_VOIDptr;		{< @exclude the memory we allocated for the bitmap }
      id: AL_ULONG;		{< for identifying sub-bitmaps }
      extra: AL_VOIDptr;	{< @exclude points to a structure with more info }
      x_ofs: AL_INT;		{< horizontal offset (for sub-bitmaps) }
      y_ofs: AL_INT;		{< vertical offset (for sub-bitmaps) }
      seg: AL_INT;		{< @exclude bitmap segment }
      line: AL_VOIDptr;		{< ZERO_SIZE_ARRAY(unsigned char *, line); }
    END;

  (* This is used to define call-back procedures for some drawing procedures. *)
    AL_POINT_PROC = PROCEDURE (bmp: AL_BITMAPptr; x, y, c: AL_INT); CDECL;

  VAR
  (* Screen bitmap *)
    al_screen: AL_BITMAPptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'screen';
  (* Screen size. *)
    AL_SCREEN_W, AL_SCREEN_H, AL_VIRTUAL_W, AL_VIRTUAL_H: AL_INT;

  CONST
  (* Closes any previously opened graphics mode, making you unable to use the
     global variable @link(al_screen), and in those environments that have
     text modes, sets one previously used or the closest match to that (usually
     80x25).  With this driver the size parameters of @link(al_set_gfx_mode)
     don't mean anything, so you can leave them all to zero or any other number
     you prefer. *)
    AL_GFX_TEXT			= -1;
  (* Allegro will try to set the specified resolution with the current color
     depth in fullscreen mode.  Failing that, it will try to repeat the same
     operation in windowed mode.  If the call to @link(al_set_gfx_mode)
     succeeds, you are guaranteed to have set the specified resolution in the
     current color depth, but you don't know if the program is running fullscreen
     or windowed. *)
    AL_GFX_AUTODETECT		=  0;
  (* Allegro will try to set the specified resolution with the current color
     depth in fullscreen mode.  If that is not possible, @link(al_set_gfx_mode)
     will fail. *)
    AL_GFX_AUTODETECT_FULLSCREEN	=  1;
  (* Allegro will try to set the specified resolution with the current color
     depth in a windowed mode.  If that is not possible, @link(al_set_gfx_mode)
     will fail.  When it comes to windowed modes, the `specified resolution'
     actually means the graphic area your program can draw on, without including
     window decorations (if any).  Note that in windowed modes running with a
     color depth other than the desktop may result in non optimal performance due
     to internal color conversions in the graphic driver. Use
     @link(al_desktop_color_depth) to your advantage in these situations. *)
    AL_GFX_AUTODETECT_WINDOWED	=  2;
  (* Using this driver Allegro guarantees that a graphic mode will always be set
     correctly.  It will try to select the resolution that you request, and if
     that fails, it will fall back upon whatever mode is known to be reliable on
     the current platform (this is 640x480 resolution under Windows, the actual
     framebuffer's resolution under Linux if it's supported, etc).  If it
     absolutely cannot set any graphics mode at all, it will return @false as
     usual, meaning that there's no possible video output on the machine, and
     that you should abort your program immediately, possibly after notifying
     this to the user with @link(al_message).  This fake driver is useful for
     situations where you just want to get into some kind of workable display
     mode, and can't be bothered with trying multiple different resolutions and
     doing all the error checking yourself.  Note however, that after a
     successful call to @link(al_set_gfx_mode) with this driver, you cannot make
     any assumptions about the width, height or color depth of the screen:  your
     code will have to deal with this little detail. *)
    AL_GFX_SAFE			= $53414645; { AL_ID('SAFE') }
  (* No graphics mode. *)
    AL_GFX_NONE			= $4E4F4E45; { AL_ID('NONE') }



  TYPE
  (* Pointer to @code(AL_GFX_MODE). *)
    AL_GFX_MODEptr = ^AL_GFX_MODE;

  (* Graphics mode description. @seealso(AL_GFX_MODE_LIST) *)
    AL_GFX_MODE = RECORD
    (* Mode width in pixels. *)
      width
    (* Mode height in pixels. *),
      height,
    (* Bits per pixel. *)
      bpp: AL_INT;
    END;

  (* Pointer to @code(AL_GFX_MODE_LIST). *)
    AL_GFX_MODE_LISTptr = ^AL_GFX_MODE_LIST;

  (* Graphics mode list. @seealso(al_get_gfx_mode_list) *)
    AL_GFX_MODE_LIST = RECORD
    (* Number of graphics modes. *)
      num_modes: AL_INT;
    (* Pointer to the actual mode list array. *)
      mode: AL_GFX_MODEptr;
    END;

  CONST
  (* @exclude Drawing modes for al_draw_sprite_ex. *)
    AL_DRAW_SPRITE_NORMAL_MODE = 0;
    AL_DRAW_SPRITE_LIT_MODE    = 1; {< @exclude }
    AL_DRAW_SPRITE_TRANS_MODE  = 2; {< @exclude }

  (* @exclude Flipping modes for al_draw_sprite_ex. *)
    AL_DRAW_SPRITE_NO_FLIP_MODE = 0;
    AL_DRAW_SPRITE_H_FLIP_MODE  = 1; {< @exclude }
    AL_DRAW_SPRITE_V_FLIP_MODE  = 2; {< @exclude }
    AL_DRAW_SPRITE_VH_FLIP_MODE = 3; {< @exclude }



(* Color conversion constants. *)
  {$INCLUDE alclrcnv.inc}


  CONST
  (* Indicates that the @link(al_scroll_screen) function may be used with this
     driver. @seealso(al_gfx_capabilities) *)
    AL_GFX_CAN_SCROLL			= $00000001;
  (* Indicates that the @link(al_request_scroll) and @link(al_poll_scroll)
     functions may be used with this driver.  If this flag is not set, it is
     possible that the @link(al_enable_triple_buffer) function may be able to
     activate it. @seealso(al_gfx_capabilities) *)
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
     hide/redisplay the pointer. @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_CURSOR			= $00000004;
  (* Indicates that the normal opaque version of the @link(al_hline) function is
     implemented using a hardware accelerator.  This will improve the performance
     not only of @link(al_hline) itself, but also of many other functions that
     use it as a workhorse, for example @link(al_circlefill) and
     @link(al_floodfill). @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_HLINE			= $00000008;
  (* Indicates that the XOR version of the @link(al_hline) function, and any
     other functions that use it as a workhorse, are implemented using a
     hardware accelerator (see @link(AL_GFX_HW_HLINE)).
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_HLINE_XOR			= $00000010;
  (* Indicates that the solid and masked pattern modes of the @link(al_hline)
     function, and any other functions that use it as a workhorse, are
     implemented using a hardware accelerator (see @link(AL_GFX_HW_HLINE)).
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_HLINE_SOLID_PATTERN	= $00000020;
  (* Indicates that the copy pattern modes of the @link(al_hline) function, and
     any other functions that use it as a workhorse, are implemented using a
     hardware accelerator (see @link(AL_GFX_HW_HLINE)).
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_HLINE_COPY_PATTERN	= $00000040;
  (* Indicates that the opaque version of the @link(al_rectfill) function, the
     @link(al_clear_bitmap) routine, and @link(al_clear_to_color), are
     implemented using a hardware accelerator. @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_FILL			= $00000080;
  (* Indicates that the XOR version of the @link(al_rectfill) function is
     implemented using a hardware accelerator  (see @link(AL_GFX_HW_FILL)).
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_FILL_XOR			= $00000100;
  (* Indicates that the solid and masked pattern modes of the @link(al_rectfill)
     function is implemented using a hardware accelerator  (see
     @link(AL_GFX_HW_FILL)). @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_FILL_SOLID_PATTERN	= $00000200;
  (* Indicates that the copy pattern mode of the @link(al_rectfill) function
     is implemented using a hardware accelerator  (see @link(AL_GFX_HW_FILL)).
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_FILL_COPY_PATTERN		= $00000400;
  (* Indicates that the opaque mode @link(al_line) and @link(al_vline)
     functions are implemented using a hardware accelerator.
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_LINE			= $00000800;
  (* Indicates that the XOR version of the @link(al_line) and @link(al_vline)
     functions are implemented using a hardware accelerator.
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_LINE_XOR			= $00001000;
  { @exclude }
    AL_GFX_HW_TRIANGLE			= $00002000;
  { @exclude }
    AL_GFX_HW_TRIANGLE_XOR		= $00004000;
  (* Indicates that monochrome character expansion (for text drawing) is
     implemented using a hardware accelerator. @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_GLYPH			= $00008000;
  (* Indicates that blitting from one part of the screen to another is
     implemented using a hardware accelerator.  If this flag is set, blitting
     within the video memory will almost certainly be the fastest possible way to
     display an image, so it may be worth storing some of your more frequently
     used graphics in an offscreen portion of the video memory.
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_VRAM_BLIT			= $00010000;
  (* Indicates that the @link(al_masked_blit) routine is capable of a hardware
     accelerated copy from one part of video memory to another, and that
     @link(al_draw_sprite) will use a hardware copy when given a sub-bitmap of
     the screen or a video memory bitmap as the source image.  If this flag is
     set, copying within the video memory will almost certainly be the fastest
     possible way to display an image, so it may be worth storing some of your
     more frequently used sprites in an offscreen portion of the video memory.

     @bold(Warning:)  if this flag is not set, @link(al_masked_blit) and
     @link(al_draw_sprite) will not work correctly when used with a video memory
     source image!  You must only try to use these functions to copy within the
     video memory if they are supported in hardware.
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_VRAM_BLIT_MASKED		= $00020000;
  (* Indicates that blitting from a memory bitmap onto the screen is being
     accelerated in hardware. @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_MEM_BLIT			= $00040000;
  (* Indicates that the @link(al_masked_blit) and @link(al_draw_sprite)
     functions are being accelerated in hardware when the source image is a
     memory bitmap and the destination is the physical screen.
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_MEM_BLIT_MASKED		= $00080000;
  (* Indicates that blitting from a system bitmap onto the screen is being
     accelerated in hardware.  Note that some acceleration may be present even
     if this flag is not set, because system bitmaps can benefit from normal
     memory to screen blitting as well.  This flag will only be set if system
     bitmaps have further acceleration above and beyond what is provided by
     @link(AL_GFX_HW_MEM_BLIT). @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_SYS_TO_VRAM_BLIT		= $00100000;
  (* Indicates that the @link(al_masked_blit) and @link(al_draw_sprite)
     functions are being accelerated in hardware when the source image is a
     system bitmap and the destination is the physical screen.  Note that some
     acceleration may be present even if this flag is not set, because system
     bitmaps can benefit from normal memory to screen blitting as well.  This
     flag will only be set if system bitmaps have further acceleration above and
     beyond what is provided by @link(AL_GFX_HW_MEM_BLIT_MASKED).
     @seealso(al_gfx_capabilities) *)
    AL_GFX_HW_SYS_TO_VRAM_BLIT_MASKED	= $00200000;
  (* Indicates that the mouse cursor is the default system cursor, not Allegro's
     custom cursor. @seealso(al_gfx_capabilities) *)
    AL_GFX_SYSTEM_CURSOR		= $00400000;

  VAR
  (* Bitfield describing the capabilities of the current graphics driver and
     video hardware.  This may contain combination any of the @code(AL_GFX_* )
     flags. *)
    al_gfx_capabilities: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gfx_capabilities';


(* Attempts to create a list of all the supported video modes for a certain
  graphics driver, made up from the @code(AL_GFX_MODE_LIST) structure.  This
  list of video modes is terminated with an @code([ 0, 0, 0 ]) entry.  Note
  that the card parameter must refer to a @italic(real) driver.  This function
  fails if you pass @link(AL_GFX_SAFE), @link(AL_GFX_AUTODETECT), or any other
  "magic" driver.
  @return(A pointer to a list structure of the type @code(AL_GFX_MODE_LIST) or
  @nil if the request could not be satisfied.)
  @seealso(al_destroy_gfx_mode_list) @seealso(al_set_gfx_mode)
  @seealso(al_set_color_depth) *)
  FUNCTION al_get_gfx_mode_list (card: AL_INT): AL_GFX_MODE_LISTptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_gfx_mode_list';

(* Removes the mode list created by @code(al_get_gfx_mode_list) from memory.
   Use this once you are done with the generated mode list to avoid memory
   leaks in your program. @seealso(al_get_gfx_mode_list)
   @seealso(al_set_gfx_mode) @seealso(al_set_color_depth) *)
  PROCEDURE al_destroy_gfx_mode_list (gfx_mode_list: AL_GFX_MODE_LISTptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_gfx_mode_list';

(* Sets the pixel format to be used by subsequent calls to
   @link(al_set_gfx_mode) and @link(al_create_bitmap).  Valid depths are 8 (the
   default), 15, 16, 24, and 32 bits.

   Note that the screen color depth won't change until the next successful
   call to @link(al_set_gfx_mode).
   @seealso(al_get_color_depth) @seealso(al_set_color_conversion)
   @seealso(al_desktop_color_depth) *)
  PROCEDURE al_set_color_depth (depth: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color_depth';

(* Returns the current pixel format.  This can be very useful to know in order
   to write generic functions which select a different code path internally
   depending on the color depth being used.

   Note that the function returns whatever value you may have set previously
   with @link(al_set_color_depth), which can be different from the current
   color depth of the @link(al_screen) global variable.  If you really need to
   know the color depth of the screen, use @link(al_bitmap_color_depth). *)
  FUNCTION al_get_color_depth: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color_depth';

(* Specifies how to convert images between the various color depths when reading
   graphics from external bitmap files or datafiles.  The mode is a bitmask
   specifying which types of conversion are allowed.  If the appropriate bit is
   set, data will be converted into the current pixel format (selected by
   calling the @link(al_set_color_depth) function), otherwise it will be left
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
   paletted format, including by the @link(al_blit) function, and any
   automatic conversions that take place while reading graphics from disk.
   This can produce much better looking results, but is obviously slower than a
   direct conversion.

   If you intend using converted bitmaps with functions like
   @link(al_masked_blit) or @link(al_draw_sprite), you should specify the
   @code(AL_COLORCONV_KEEP_TRANS) flag.  It will ensure that the masked areas
   in the bitmap before and after the conversion stay exactly the same, by
   mapping transparent colors to each other and adjusting colors which would be
   converted to the transparent color otherwise.  It affects every
   @code(al_blit) operation between distinct pixel formats and every automatic
   conversion.
   @seealso(al_set_color_depth) @seealso(al_load_bitmap)
   @seealso(al_load_datafile) @seealso(al_get_color_conversion) *)
  PROCEDURE al_set_color_conversion (mode: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color_conversion';

(* Returns the current color conversion mode. @seealso(al_set_color_conversion) *)
  FUNCTION al_get_color_conversion: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color_conversion';

(* Requests that the next call to @code(al_set_gfx_mode) try to use the
   specified refresh rate, if possible. Not all drivers are able to control
   this at all, and even when they can, not all rates will be possible on all
   hardware, so the actual settings may differ from what you requested. After
   you call @code(al_set_gfx_mode), you can use @code(al_get_refresh_rate) to
   find out what was actually selected. At the moment only some Windows DirectX
   drivers support this function. The speed is specified in Hz, eg. 60, 70. To
   return to the normal default selection, pass a rate value of zero.

   Example:
@longcode(#
     al_request_refresh_rate(60);
     IF  NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
       abort_on_error ('Couldn''t set graphic mode!');
     IF al_get_refresh_rate <> 60 THEN
       abort_on_error ('Couldn''t set refresh rate to 60Hz!');
#)
    @seealso(al_set_gfx_mode) @seealso(al_get_refresh_rate) *)
  PROCEDURE al_request_refresh_rate (rate: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'request_refresh_rate';

(* Returns the current refresh rate, if known (not all drivers are able to
   report this information). Returns zero if the actual rate is unknown.
   @seealso(al_request_refresh_rate) *)
  FUNCTION al_get_refresh_rate: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_refresh_rate';

(* Switches into graphics mode.  The card parameter should usually be one of
   the Allegro magic drivers (read introduction of this unit) or see the
   platform specific documentation for a list of the available drivers.  The
   @code(w) and @code(h) parameters specify what screen resolution you want.
   The color depth of the graphic mode has to be specified before calling this
   function with @link(al_set_color_depth).

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
   @link(al_create_video_bitmap).  Otherwise your program will be limited to
   the platforms supporting hardware scrolling.

   After you select a graphics mode, the physical and virtual screen sizes can
   be checked with the variables @link(AL_SCREEN_W), @link(AL_SCREEN_H),
   @link(AL_VIRTUAL_W), and @link(AL_VIRTUAL_H).

   @returns(@true on success.  On failure returns @false and stores a
     description of the problem in @link(al_error).)
   @seealso(AL_GFX_AUTODETECT)
   @seealso(al_gfx_capabilities) @seealso(al_get_desktop_resolution)
   @seealso(alUNIX) @seealso(alWin) *)
  FUNCTION al_set_gfx_mode (card, w, h, v_w, v_h: AL_INT): AL_BOOL;

(* Requests a hardware scroll request.  Attempts to scroll the hardware screen
   to display a different part of the virtual screen (initially it will be
   positioned at 0, 0, which is the top left corner).  You can use this to move
   the screen display around in a large virtual screen space, or to page flip
   back and forth between two non-overlapping areas of the virtual screen.
   Note that to draw outside the original position in the screen bitmap you
   will have to alter the clipping rectangle with @link(al_set_clip_rect).

   Mode-X scrolling is reliable and will work on any card, other drivers may not
   work or not work reliably.  See the platform-specific section of the docs for
   more information.

   Allegro will handle any necessary vertical retrace synchronisation when
   scrolling the screen, so you don't need to call @link(al_vsync) before it.
   This means that @code(al_scroll_screen) has the same time delay effects as
   @code(al_vsync).
   @return(@false on success, @true if the graphics driver can't handle
     hardware scrolling or the virtual screen is not large enough.)
   @seealso(al_set_gfx_mode) @seealso(al_show_video_bitmap)
   @seealso(al_request_scroll) @seealso(al_request_video_bitmap) *)
  FUNCTION al_scroll_screen (x, y: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scroll_screen';

(* Queues a hardware scroll request with triple buffering.  It requests a
   hardware scroll to the specified position, but returns immediately rather
   than waiting for a retrace.  The scroll will then take place during the next
   vertical retrace, but you can carry on running other code in the meantime
   and use the @link(al_poll_scroll) routine to detect when the flip has
   actually taken place.

   Triple buffering is only possible with certain drivers:  you can look at the
   @link(AL_GFX_CAN_TRIPLE_BUFFER) bit in the @link(al_gfx_capabilities) flag
   to see if it will work with the current driver.
   @return(@false on success, @true Otherwise.)
   @seealso(al_request_video_bitmap) @seealso(al_scroll_screen) *)
  FUNCTION al_request_scroll (x, y: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'requests_scroll';

(* Checks the status of a scroll request with triple buffering.
   @returns(@true if it is still waiting to take place, or @false if the
     requested scroll has already happened.)
   @seealso(al_request_scroll) @seealso(al_request_video_bitmap) *)
  FUNCTION al_poll_scroll: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_scroll';

(* Attempts to page flip the hardware screen to display the specified video
   bitmap object, which must be the same size as the physical screen, and
   should have been obtained by calling the @link(al_create_video_bitmap)
   function.

   Allegro will handle any necessary vertical retrace synchronisation when page
   flipping, so you don't need to call @link(al_vsync) before it.  This means
   that @code(al_show_video_bitmap) has the same time delay effects as
   @code(al_vsync) by default.  This can be adjusted with the "disable_vsync"
   config key in the @code(graphics) section of allegro.cfg.
   @returns(@false on success and @true on failure.) *)
  FUNCTION al_show_video_bitmap (bmp: AL_BITMAPptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'show_video_bitmap';

(* Requests a page flip to display the specified video bitmap object, but
   returns immediately rather than waiting for a retrace.  The flip will then
   take place during the next vertical retrace, but you can carry on running
   other code in the meantime and use the @link(al_poll_scroll) routine to
   detect when the flip has actually taken place.  Triple buffering is only
   possible on certain hardware: see the comments about
   @link(al_request_scroll). Example:
@longcode(#
VAR
  CurrentPage: INTEGER;
  VideoPage: ARRAY [0..2] OF AL_BITMAPptr;
BEGIN
  ...
// Create pages for page flipping
  FOR CurrentPage := Low (VideoPage) TO High (VideoPage) DO
    VideoPage[CurrentPage] := al_create_video_bitmap (SCREEN_W, SCREEN_H);
  CurrentPage := Low (VideoPage);
  ...
// draw the screen and flip pages
  DrawScreen (VideoPage[CurrentPage]);
  WHILE al_poll_scroll DO
    ;
  al_request_video_bitmap (VideoPage[CurrentPage]);
  CurrentPage := (CurrentPage + 1) MOD 3;
  ...
END.
#)
   @returns(@false on success or @true on failure.)
   @seealso(al_gfx_capabilities) @seealso(al_create_video_bitmap)
   @seealso(al_scroll_screen) *)
  FUNCTION al_request_video_bitmap (bitmap: AL_BITMAPptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'request_video_bitmap';

(* Enables triple buffering.  If the @link(AL_GFX_CAN_TRIPLE_BUFFER) bit of the
   @link(al_gfx_capabilities) field is not set, you can attempt to enable it by
   calling this function.  In particular if you are running in mode-X in a
   clean DOS environment, this routine will enable the timer retrace simulator,
   which will activate the triple buffering functions.
   @returns(@true if triple buffering is enabled, @false otherwise.)
   @seealso(al_request_scroll) @seealso(al_request_video_bitmap) *)
  FUNCTION al_enable_triple_buffer: AL_BOOL;
    INLINE;

(* Creates a memory bitmap sized @code(w) by @code(h).  The bitmap will have
   clipping turned on, and the clipping rectangle set to the full size of the
   bitmap.  The image memory will not be cleared, so it will probably contain
   garbage:  you should clear the bitmap before using it.  This routine always
   uses the global pixel format, as specified by calling
   @link(al_set_color_depth).  The minimum height of the bitmap must be 1 and
   width can't be negative.

   @returns(a pointer to the created bitmap, or @nil if the bitmap could not
     be created . Remember to free this bitmap later to avoid memory leaks.)
   @seealso(al_create_bitmap_ex) @seealso(al_create_sub_bitmap)
   @seealso(al_create_video_bitmap) @seealso(al_create_system_bitmap)
   @seealso(al_destroy_bitmap) @seealso(al_set_color_depth)
   @seealso(al_is_memory_bitmap) @seealso(al_clear_bitmap) *)
  FUNCTION al_create_bitmap (w, h: AL_INT): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap';

(* Creates a bitmap in a specific color depth (8, 15, 16, 24 or 32 bits per
   pixel).
   @returns(a pointer to the created bitmap, or @nil if the bitmap could not
     be created . Remember to free this bitmap later to avoid memory leaks.)
   @seealso(al_create_bitmap) *)
  FUNCTION al_create_bitmap_ex (bpp, width, height: AL_INT): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_bitmap_ex';

(* Creates a sub-bitmap, ie. a bitmap sharing drawing memory with a
   pre-existing bitmap, but possibly with a different size and clipping
   settings.  When creating a sub-bitmap of the mode-X screen, the @code(x)
   position must be a multiple of four.  The sub-bitmap width and height can
   extend beyond the right and bottom edges of the parent (they will be
   clipped), but the origin point must lie within the parent region.

   Remember to free the sub bitmap before freeing the parent bitmap to avoid
   memory leaks and potential crashes accessing memory which has been freed.

   @returns(a pointer to the created sub bitmap, or @nil if the sub bitmap
     could not be created.)
   @seealso(al_create_bitmap) *)
  FUNCTION al_create_sub_bitmap (parent: AL_BITMAPptr; x, y, w, h: AL_INT): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sub_bitmap';

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
     video ram.)
   @seealso(al_create_bitmap) @seealso(al_screen)
   @seealso(al_show_video_bitmap) @seealso(al_gfx_capabilities)
   @seealso(al_is_video_bitmap) *)
  FUNCTION al_create_video_bitmap (width, height: AL_INT): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_video_bitmap';

(* Allocates a system memory bitmap of the specified size.  Read the
   introduction of this chapter for a comparison with other types of bitmaps
   and other specific details.

   Remember to destroy this bitmap before any subsequent call to
   @link(al_set_gfx_mode).

   @returns(a pointer to the bitmap on success, @nil otherwise.)
   @seealso(al_create_bitmap) @seealso(al_is_system_bitmap) *)
  FUNCTION al_create_system_bitmap (width, height: AL_INT): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_system_bitmap';

(* Destroys a memory bitmap, sub-bitmap, video memory bitmap, or system bitmap
   when you are finished with it.  If you pass a @nil pointer this function
   won't do anything.

   The bitmap must not have a mouse cursor shown on it at the time it is
   destroyed.
   @seealso(al_create_bitmap) @seealso(al_load_bitmap)
   @seealso(al_show_mouse) *)
  PROCEDURE al_destroy_bitmap (bmp: AL_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_bitmap';

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
   @link(al_get_clip_rect) will be different).  However, such modifications are
   guaranteed to preserve the external effect of the clipping rectangle, that
   is not to modify the actual area of the image that it is OK to draw onto.
   @seealso(al_add_clip_rect) @seealso(al_set_clip_state)
   @seealso(al_get_clip_state) *)
  PROCEDURE al_set_clip_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_clip_rect';

(* Sets the clipping rectangle of the specified bitmap as the intersection of
   its current clipping rectangle and the rectangle described by the four
   coordinates. *)
  PROCEDURE al_add_clip_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'add_clip_rect';

(* Clears the bitmap to color 0. @seealso(al_clear_to_color) *)
  PROCEDURE al_clear_bitmap (bitmap: AL_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_bitmap';

(* Waits for a vertical retrace to begin.  The retrace happens when the
   electron beam in your monitor has reached the bottom of the screen and is
   moving back to the top ready for another scan.  During this short period the
   graphics card isn't sending any data to the monitor, so you can do things to
   it that aren't possible at other times, such as altering the palette without
   causing flickering (snow).  Allegro will automatically wait for a retrace
   before altering the palette or doing any hardware scrolling, though, so you
   don't normally need to bother with this function.
   @seealso(al_gfx_capabilities) @seealso(al_create_video_bitmap) *)
  PROCEDURE al_vsync;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'vsync';

  CONST
  (* Bitfield for relaying graphics driver type information *)
    AL_GFX_TYPE_UNKNOWN    = 0; {<@exclude }
    AL_GFX_TYPE_WINDOWED   = 1; {<@exclude }
    AL_GFX_TYPE_FULLSCREEN = 2; {<@exclude }
    AL_GFX_TYPE_DEFINITE   = 4; {<@exclude }
    AL_GFX_TYPE_MAGIC      = 8; {<@exclude }

  (* Lets you determine the types of operating modes that a specific graphics
    card driver operates in.  It will tell you whether it is a windowed,
    fullscreen, definitely windowed or fullscreen, and/or a magic driver.

    The value returned is a bitfield consisting of these fields:
@unorderedList(
  @item(AL_GFX_TYPE_UNKNOWN)
  @item(AL_GFX_TYPE_WINDOWED)
  @item(AL_GFX_TYPE_FULLSCREEN)
  @item(AL_GFX_TYPE_DEFINITE)
  @item(AL_GFX_TYPE_MAGIC)
)
    The return value will only be equivalent to @code(AL_GFX_TYPE_UNKNOWN) when
    it is a driver unrecognized on that platform, or it is a bogus value. Test
    for the other types by using a bitwise @code(AND). If the driver is windowed
    or fullscreen, it will also have the definite flag set. For example,
@longcode(#
  aGfxType := al_get_gfx_mode_type (AL_GFX_AUTODETECT_WINDOWED);
#)
    @code(aGfxType) would have the @code(AL_GFX_TYPE_WINDOWED),
    @code(AL_GFX_TYPE_DEFINITE), and @code(AL_GFX_TYPE_MAGIC) flags set.

    Allegro needs to be initialized first. Example:
@longcode(@
// Accept the use of only windowed drivers in our selection dialog.
  FUNCTION AcceptWindowed (CardId, w, h, ColorDepth: AL_INT): AL_INT;
  BEGIN
    IF (al_get_gfx_mode_type (CardId) AND AL_GFX_TYPE_WINDOWED) <> 0 THEN
      EXIT (0);
    RESULT := -1;
  END;

// In program:
  al_gfx_mode_select_filter (Card, w, h, ColorDepth, @AcceptWindowed);
#)
    @param(graphics_card Identifier of the graphics card to check.)
    @return(a bitfield describing the graphics mode type.)
    @seealso(al_gfx_mode_select_filter) @seealso(al_get_gfx_mode)
    @seealso(al_set_gfx_mode) @seealso(al_is_windowed_mode) *)
  FUNCTION al_get_gfx_mode_type (graphics_card: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_gfx_mode_type';
(* Lets you determine which graphics driver is currently set by allegro. If no
   graphics driver is set, it will return @code(AL_GFX_NONE).
   @return(the @code(id) of the current graphics driver if there is one, or
     @link(AL_GFX_NONE) if none is set.)
   @seealso(al_set_gfx_mode) @seealso(al_is_windowed_mode)
 *)
  FUNCTION al_get_gfx_mode: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_gfx_mode';

  CONST
    AL_SWITCH_NONE        = 0; {<@exclude }
    AL_SWITCH_PAUSE       = 1; {<@exclude }
    AL_SWITCH_AMNESIA     = 2; {<@exclude }
    AL_SWITCH_BACKGROUND  = 3; {<@exclude }
    AL_SWITCH_BACKAMNESIA = 4; {<@exclude }

    AL_SWITCH_IN  = 0; {<@exclude }
    AL_SWITCH_OUT = 1; {<@exclude }

(* Sets how the program should handle being switched into the background, if
   the user tabs away from it. Not all of the possible modes will be supported
   by every graphics driver on every platform. The available modes are:
@unorderedList(
  @item(@bold(AL_SWITCH_NONE) Disables switching. This is the default in
	single-tasking systems like DOS. It may be supported on other
	platforms, but you should use it with caution, because your users won't
	be impressed if they want to switch away from your program, but you
	don't let them!)
  @item(@bold(AL_SWITCH_PAUSE) Pauses the program whenever it is in the
	background. Execution will be resumed as soon as the user switches back
	to it. This is the default in most fullscreen multitasking
	environments, for example the Linux console, but not under Windows.)
  @item(@bold(AL_SWITCH_AMNESIA) Like @code(AL_SWITCH_PAUSE), but this mode
	doesn't bother to remember the contents of video memory, so the screen,
	and any video bitmaps that you have created, will be erased after the
	user switches away and then back to your program. This is not a
	terribly useful mode to have, but it is the default for the fullscreen
	drivers under Windows because DirectDraw is too dumb to implement
	anything better.)
  @item(@bold(AL_SWITCH_BACKGROUND) The program will carry on running in the
	background, with the screen bitmap temporarily being pointed at a
	memory buffer for the fullscreen drivers. You must take special care
	when using this mode, because bad things will happen if the screen
	bitmap gets changed around when your program isn't expecting it (see
	below).)
  @item(@bold(AL_SWITCH_BACKAMNESIA) Like @code(SWITCH_BACKGROUND), but this
	mode doesn't bother to remember the contents of video memory (see
	@code(AL_SWITCH_AMNESIA)). It is again the only mode supported by the
	fullscreen drivers under Windows that lets the program keep running in
	the background.)
)
    Note that you should be very careful when you are using graphics routines
    in the switching context: you must always call @link(al_acquire_screen)
    before the start of any drawing code onto the screen and not release it
    until you are completely finished, because the automatic locking mechanism
    may not be good enough to work when the program runs in the background or
    has just been raised in the foreground.
    @param(mode The switching mode.)
    @return(@true on success, invalidating at the same time all callbacks
      previously registered with @code(al_set_display_switch_callback). @false
      if the requested mode is not currently possible.)
    @seealso(al_set_display_switch_callback) @seealso(al_get_display_switch_mode)
 *)
  FUNCTION al_set_display_switch_mode (mode: AL_INT): AL_BOOL;
    INLINE;
(* Returns the current display switching mode, in the same format passed to
   @code(al_set_display_switch_mode). @seealso(al_set_display_switch_mode) *)
  FUNCTION al_get_display_switch_mode: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_display_switch_mode';
(* Installs a notification callback for the switching mode that was previously
   selected by calling @code(al_set_display_switch_mode). The direction
   parameter can either be @code(AL_SWITCH_IN) or @code(AL_SWITCH_OUT),
   depending whether you want to be notified about switches away from your
   program or back to your program. You can sometimes install callbacks for
   both directions at the same time, but not every platform supports this. You
   can install several switch callbacks, but no more than eight on any
   platform.
   @param(dir Direction of the notification.)
   @param(cb Callback procedure.)
   @return(@true on success, decreasing the number of empty callback slots by
     one. @false if the request is impossible for the current platform or you
     have reached the maximum number of allowed callbacks.)
   @seealso(al_remove_display_switch_callback) @seealso(al_set_display_switch_mode)
 *)
  FUNCTION al_set_display_switch_callback (dir: AL_INT; cb: AL_SIMPLE_PROC): AL_BOOL;
    INLINE;
(* Removes a notification callback that was previously installed by calling
   @code(al_set_display_switch_callback). All the callbacks will automatically
   be removed when you call @code(al_set_display_switch_mode). You can safely
   call this function even if the callback you want to remove is not installed.
   @param(cb Callback procedure to remove.)
   @seealso(al_set_display_switch_callback)
 *)
  PROCEDURE al_remove_display_switch_callback (cb: AL_SIMPLE_PROC);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_display_switch_callback';



(******************************************************************************
 * gfx.inl
 *)

(* Tells if you are running in windowed mode.  You should not call this
   function if you are not in graphics mode.

   Example:
@longcode(#
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
    abort_on_error ('Couldn''t set graphic mode!');
  IF al_windowed_mode THEN
  BEGIN
    // Windowed mode stuff.
  END
  ELSE BEGIN
    // Fullscreen mode stuff.
  END;
#)
  @return(@true if the current graphics mode is a windowed mode, or @false if
    it is a fullscreen mode.)
  @seealso(al_set_gfx_mode) *)
  FUNCTION al_is_windowed_mode: AL_BOOL;
    INLINE;

(* Clears the bitmap to the specified color. @seealso(al_clear_bitmap)
  @seealso(al_makecol) *)
  PROCEDURE al_clear_to_color (bitmap: AL_BITMAPptr; CONST color: AL_INT);
    INLINE;

(* Returns the color depth of the specified bitmap @(8, 15, 16, 24, or 32@).
  @seealso(al_set_color_depth) @seealso(al_bitmap_mask_color) *)
  FUNCTION al_bitmap_color_depth (CONST bmp: AL_BITMAPptr): AL_INT;
    INLINE;

(* Returns the mask color for the specified bitmap @(the value which is
    skipped when drawing sprites@).  For 256-color bitmaps this is zero, and
    for truecolor bitmaps it is bright pink @(maximum red and blue, zero
    green@).  A frequent use of this function is to clear a bitmap with the
    mask color so you can later use this bitmap with @link(al_draw_sprite)
    after drawing other stuff on it.
    @seealso(al_set_color_depth) @seealso(al_bitmap_color_depth) *)
  FUNCTION al_bitmap_mask_color (CONST bmp: AL_BITMAPptr): AL_INT;
    INLINE;

(* Returns @true if the two bitmaps describe the same drawing surface, ie.
    the pointers are equal, one is a sub-bitmap of the other, or they are both
    sub-bitmaps of a common parent. @seealso(al_create_sub_bitmap) *)
  FUNCTION al_is_same_bitmap (CONST bmp1, bmp2: AL_BITMAPptr): AL_BOOL;

(* Returns @true if bmp is a memory bitmap, ie. it was created by calling
    @link(al_create_bitmap) or loaded from a grabber datafile or image file. *)
  FUNCTION al_is_memory_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
    INLINE;

(* Returns @true if @code(bmp) is the screen bitmap, or a sub-bitmap of the
   screen. @seealso(al_screen) @seealso(al_create_sub_bitmap) *)
  FUNCTION al_is_screen_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
    INLINE;

(* Returns @true if bmp is the screen bitmap, a video memory bitmap, or a
   sub-bitmap of either. @seealso(al_screen)
   @seealso(al_create_video_bitmap) @seealso(al_create_sub_bitmap) *) 
  FUNCTION al_is_video_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
    INLINE;

(* Returns @true if bmp is a system bitmap object, or a sub-bitmap of one.
  @seealso(al_create_system_bitmap) @seealso(al_create_sub_bitmap) *)
  FUNCTION al_is_system_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
    INLINE;

(* Returns @true if bmp is a sub-bitmap. @seealso(al_create_sub_bitmap) *)
  FUNCTION al_is_sub_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
    INLINE;

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
   most likely deadlock your entire program.
   @seealso(al_acquire_screen) @seealso(al_release_screen) *)
  PROCEDURE al_acquire_bitmap (bmp: AL_BITMAPptr);
    INLINE;

(* Releases a bitmap that was previously locked by calling
   @link(al_acquire_bitmap).  If the bitmap was locked multiple times, you must
   release it the same number of times before it will truly be unlocked.
   @seealso(al_acquire_screen) @seealso(al_release_screen) *)
  PROCEDURE al_release_bitmap (bmp: AL_BITMAPptr);
    INLINE;

(* Shortcut version of @code(al_acquire_bitmap @(screen@);)
   @seealso(al_acquire_bitmap) *)
  PROCEDURE al_acquire_screen;
    INLINE;

(* Shortcut version of @code(al_release_bitmap @(screen@);)
   @seealso(al_release_bitmap) *)
  PROCEDURE al_release_screen;
    INLINE;

(* Returns the clipping rectangle for the specified bitmap.
  @seealso(al_set_clip_rect) @seealso(al_add_clip_rect)
  @seealso(al_get_clip_state) *)
  PROCEDURE al_get_clip_rect (bmp: AL_BITMAPptr; VAR x1, y1, x2, y2: AL_INT);
    INLINE;

(* Turns on (if state is @true) or off (if state is @false) clipping for the
   specified bitmap.  Turning clipping off may slightly speed up some drawing
   operations (usually a negligible difference, although every little helps)
   but will result in your program dying a horrible death if you try to draw
   beyond the edges of the bitmap. @seealso(al_set_clip_rect)
   @seealso(al_get_clip_rect) @seealso(al_add_clip_rect)
   @seealso(al_get_clip_state) *)
  PROCEDURE al_set_clip_state (bmp: AL_BITMAPptr; state: AL_BOOL);
    INLINE;

(* Returns the clipping state.
   @seealso(al_get_clip_rect) @seealso(al_add_clip_rect)
   @seealso(al_set_clip_state) *)
  FUNCTION al_get_clip_state (bmp: AL_BITMAPptr): AL_BOOL;
    INLINE;


(******************************************************************************
 * datafile.h
 *      Datafile access routines.
 *
 *      This section includes only functions related with bitmap loading and
 *      saving.  Other datafile.h stuff are in unit alfile.
 *)

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
    abort_on_error ('Could not load image.pcx!');
          ...
 al_destroy_bitmap (bmp);
#)
     @seealso(al_load_bmp) @seealso(al_load_pcx) @seealso(al_load_tga)
     @seealso(al_load_lbm) @seealso(al_save_bitmap) *)
  FUNCTION al_load_bitmap (filename: AL_STR; pal: AL_PALETTEptr): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bitmap';

(* Loads an 8-bit, 16-bit, 24-bit or 32-bit Windows or OS/2 BMP file.  Remember
   that you are responsible for destroying the bitmap when you are finished
   with it to avoid memory leaks.
   @returns(a pointer to the bitmap or @nil on error.)
   @seealso(al_load_bitmap) @seealso(al_load_bmp_pf) *)
  FUNCTION al_load_bmp (filename: AL_STR; pal: AL_PALETTEptr): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bmp';

(* Loads a 256-color IFF ILBM/PBM file.  Remember that you are responsible for
   destroying the bitmap when you are finished with it to avoid memory leaks.
   @returns(a pointer to the bitmap or @nil on error.)
   @seealso(al_load_bitmap) *)
  FUNCTION al_load_lbm (filename: AL_STR; pal: AL_PALETTEptr): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_lbm';

(* Loads a 256-color or 24-bit truecolor PCX file. Remember that you are
   responsible for destroying the bitmap when you are finished with it to avoid
   memory leaks.
   @returns(a pointer to the bitmap or @nil on error.)
   @seealso(al_load_bitmap) @seealso(al_load_pcx_pf) *)
  FUNCTION al_load_pcx (filename: AL_STR; pal: AL_PALETTEptr): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_pcx';

(* Loads a 256-color, 15-bit hicolor, 24-bit truecolor, or 32-bit
   truecolor+alpha TGA file.  Remember that you are responsible for destroying
   the bitmap when you are finished with it to avoid memory leaks.
   @returns(a pointer to the bitmap or @nil on error.)
   @seealso(al_load_bitmap) @seealso(al_load_tga_pf) *)
  FUNCTION al_load_tga (filename: AL_STR; pal: AL_PALETTEptr): AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_tga';



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
  al_get_palette (palette);
  bmp := al_create_sub_bitmap (al_screen, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
  al_save_bitmap ('dump.pcx', bmp, @palette);
  al_destroy_bitmap (bmp);
#)
    @param(filename Where to save the bitmap.)
    @param(bmp Bitmap to be saved.)
    @param(pal Palette of bitmap or @nil if none is used (i.e. true color bitmap).)
    @return(@true on success, @false on failure.)
  *)
  FUNCTION al_save_bitmap (filename: STRING; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): AL_BOOL;
    INLINE;

(* Writes a bitmap into a 256-color or 24-bit truecolor BMP file.
   @returns(non-zero on error.)
   @seealso(al_save_bitmap) @seealso(al_save_bmp_pf) *)
  FUNCTION al_save_bmp (filename: STRING; bmp: AL_BITMAPptr; palette: AL_PALETTEptr): AL_BOOL;
    INLINE;

(* Writes a bitmap into a 256-color or 24-bit truecolor PCX file.
   @seealso(al_save_bitmap) @seealso(al_save_pcx_pf) *)
  FUNCTION al_save_pcx (filename: STRING; bmp: AL_BITMAPptr; palette: AL_PALETTEptr): AL_BOOL;
    INLINE;

(* Writes a bitmap into a 256-color, 15-bit hicolor, 24-bit truecolor, or
   32-bit truecolor+alpha TGA file.
   @seealso(al_save_bitmap) @seealso(al_save_tga_pf) *)
  FUNCTION al_save_tga (filename: STRING; bmp: AL_BITMAPptr; palette: AL_PALETTEptr): AL_BOOL;
    INLINE;



(******************************************************************************
 * mouse.h
 *    Mouse routines.
 *)

CONST
(* Indicates that the mouse cursor is the default system cursor, not Allegro's
   custom cursor. @seealso(al_select_mouse_cursor) *)
  AL_MOUSE_CURSOR_NONE		= 0;
(* Selects the custom Allegro cursor, i.e. the one that you set with
   @link(al_set_mouse_sprite). @seealso(al_select_mouse_cursor) *)
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
(* Global variable containing the current mouse sprite. This is read-only, and
   only to be modified using the @link(al_set_mouse_sprite) procedure. *)
  al_mouse_sprite: AL_BITMAPptr;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_sprite';
(* Global variable containing the current mouse focus point. This is read-only,
   and only to be modified using the @link(al_set_mouse_sprite_focus)
   procedure. *)
  al_mouse_x_focus: AL_INT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_x_focus';
(* Global variable containing the current mouse focus point. This is read-only,
   and only to be modified using the @link(al_set_mouse_sprite_focus)
   procedure. *)
  al_mouse_y_focus: AL_INT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_y_focus';



(* Global variable containing the current mouse horizontal position.  Wherever
   possible these values will be updated asynchronously, but if
   @link(al_mouse_needs_poll) returns @true, you must manually call
   @link(al_poll_mouse) to update them with the current input state.

   The position is integer ranging from zero to the right side of the
   screen. @seealso(al_install_mouse) *)
  al_mouse_x: AL_INT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_x';
(* Global variable containing the current mouse vertical position.  Wherever
   possible these values will be updated asynchronously, but if
   @code(al_mouse_needs_poll) returns @true, you must manually call
   @code(al_poll_mouse) to update them with the current input state.

   The position is integer ranging from zero to the bottom side of the
   screen. @seealso(al_install_mouse) *)
  al_mouse_y: AL_INT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_y';
(* Global variable containing the current mouse position.  Wherever possible
   these values will be updated asynchronously, but if
   @code(al_mouse_needs_poll) returns @true, you must manually call
   @code(al_poll_mouse) to update them with the current input state.

   It holds the current vertical wheel position, when using an input driver
   that supports wheel mice. @seealso(al_install_mouse) *)
  al_mouse_z: AL_INT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_z';
(* Global variable containing the current mouse position.  Wherever possible
   these values will be updated asynchronously, but if
   @code(al_mouse_needs_poll) returns @true, you must manually call
   @code(al_poll_mouse) to update them with the current input state.

   It holds the current horizontal wheel position, when using an input driver
   that supports wheel mice. @seealso(al_install_mouse) *)
  al_mouse_w: AL_INT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_w';
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
   @seealso(al_install_mouse) *)
  al_mouse_b: AL_INT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_b';
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
  mpos, mx, my: INTEGER;
  ...
  mpos := al_mouse_pos;
  mx := mpos SHR 16;
  my := mpos AND $0000ffff;
  #)*)
  al_mouse_pos: AL_INT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_pos';



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
  FUNCTION al_install_mouse: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'install_mouse';

(* Removes the mouse handler. You don't normally need to bother calling this,
   because @link(al_exit) will do it for you. *)
  PROCEDURE al_remove_mouse;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_mouse';



(* Wherever possible, Allegro will read the mouse input asynchronously (ie.
   from inside an interrupt handler), but on some platforms that may not be
   possible, in which case you must call this routine at regular intervals to
   update the mouse state variables.  To help you test your mouse polling code
   even if you are programming on a platform that doesn't require it, after the
   first time that you call this function Allegro will switch into polling
   mode, so from that point onwards you will have to call this routine in order
   to get any mouse input at all, regardless of whether the current driver
   actually needs to be polled or not.
   @returns(@false on success, or @true on failure @(ie. no mouse
     driver installed@).)
   @seealso(al_mouse_needs_poll) @seealso(al_install_mouse) *)
  FUNCTION al_poll_mouse: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_mouse';

(* Returns @true if the current mouse driver is operating in polling mode.
  @seealso(al_poll_mouse) @seealso(al_install_mouse) *)
  FUNCTION al_mouse_needs_poll: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_needs_poll';



(* After calling this function, Allegro will let the operating system draw the
   mouse cursor instead of doing it itself.  This is not possible with all
   graphics drivers though:  you'll need to check the
   @link(al_gfx_capabilities) flags after calling @link(al_show_mouse) to see
   if this works.  On some platforms, enabling the hardware cursor causes
   @link(al_get_mouse_mickeys) to return only a limited range of values, so you
   should not call this function if you need mouse mickeys.
   @seealso(al_set_mouse_sprite) @seealso(al_disable_hardware_cursor)
   @seealso(al_show_mouse) *)
  PROCEDURE al_enable_hardware_cursor;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'enable_hardware_cursor';

(* After calling this function, Allegro will be responsible for drawing the
   mouse cursor rather than the operating system.  On some platforms calling
   @link(al_enable_hardware_cursor) makes the return values of
   @link(al_get_mouse_mickeys) unreliable.  After calling this function,
   @code(al_get_mouse_mickeys) returns reliable results again. *)
  PROCEDURE al_disable_hardware_cursor;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'disable_hardware_cursor';



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
   the bitmap is destroyed with @link(al_destroy_bitmap), e.g. call
   @code(al_show_mouse @(@nil@);) before destroying the bitmap.  This does not
   apply to @link(al_screen) since you never destroy @code(al_screen) with
   @code(al_destroy_bitmap).
   @seealso(al_scare_mouse) *)
  PROCEDURE al_show_mouse (bmp: AL_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'show_mouse';

(* Helper for hiding the mouse pointer prior to a drawing operation.  This will
   temporarily get rid of the pointer, but only if that is really required (ie.
   the mouse is visible, and is displayed on the physical screen rather than
   some other memory surface, and it is not a hardware or OS cursor).  The
   previous mouse state is stored for subsequent calls to
   @link(al_unscare_mouse).
   @seealso(al_show_mouse) @seealso(al_scare_mouse_area) *)
  PROCEDURE al_scare_mouse;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scare_mouse';

(* Like @link(al_scare_mouse), but will only hide the cursor if it is inside
   the specified rectangle.  Otherwise the cursor will simply be frozen
   in place until you call @link(al_unscare_mouse), so it cannot interfere with
   your drawing. *)
  PROCEDURE al_scare_mouse_area (x, y, w, h: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scare_mouse_area';

(* Undoes the effect of a previous call to @link(al_scare_mouse) or
   @link(al_scare_mouse_area), restoring the original pointer state.
   @seealso(al_show_mouse) *)
  PROCEDURE al_unscare_mouse;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unscare_mouse';

(* Moves the mouse to the specified screen position.  It is safe to call even
   when a mouse pointer is being displayed.
   @seealso(al_position_mouse_z) @seealso(al_position_mouse_w) *)
  PROCEDURE al_position_mouse (x, y: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse';

(* Sets the mouse wheel position variable to the specified value.
   @seealso(al_position_mouse) *)
  PROCEDURE al_position_mouse_z (z: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse_z';

(* Sets the horizontal mouse wheel position variable to the specified value.
   @seealso(al_position_mouse) *)
  PROCEDURE al_position_mouse_w (w: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_mouse_w';

(* This function allows you to use the operating system's native mouse cursors
   rather than some custom cursor.  You will need to enable this functionality
   by calling @link(al_enable_hardware_cursor) beforehand.  If the operating
   system does not support this functionality, or if it has not been enabled,
   then Allegro will substitute its own cursor images.  You can change these
   substitute images using @link(al_set_mouse_cursor_bitmap).

   Note that the effects of this function are not apparent until
   @link(al_show_mouse) is called.

   To know whether the operating system's native cursor is being used, or if
   Allegro has made a substitution, you can check the
   @code(AL_GFX_SYSTEM_CURSOR) flag in @link(al_gfx_capabilities) after calling
   @code(al_show_mouse).

   The cursor argument selects the type of cursor to be displayed:
   @link(AL_MOUSE_CURSOR_NONE), @link(AL_MOUSE_CURSOR_ALLEGRO),
   @link(AL_MOUSE_CURSOR_ARROW), @link(AL_MOUSE_CURSOR_BUSY),
   @link(AL_MOUSE_CURSOR_QUESTION), @link(AL_MOUSE_CURSOR_EDIT) *)
  PROCEDURE al_select_mouse_cursor (cursor: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'select_mouse_cursor';

(* This function changes the cursor image Allegro uses if
   @link(al_select_mouse_cursor) is called but no native operating system
   cursor can be used, e.g. because you did not call
   @link(al_enable_hardware_cursor).

   The effect of this function will not be apparent until @link(al_show_mouse)
   is called.

   @param(cursor one of: @link(AL_MOUSE_CURSOR_ALLEGRO),
     @link(AL_MOUSE_CURSOR_ARROW), @link(AL_MOUSE_CURSOR_BUSY),
     @link(AL_MOUSE_CURSOR_QUESTION), @link(AL_MOUSE_CURSOR_EDIT) but not
     @link(AL_MOUSE_CURSOR_NONE).)
   @param(bmp can either point to a valid bitmap or it can be @nil.  Passing a
     bitmap makes Allegro use that image in place of its own default
     substitution @(should the operating system's native cursor be
     unavailable@).  The bitmap must remain available for the duration in which
     it could be used.  Passing @nil lets Allegro revert to its default
     substitutions.) *)
  PROCEDURE al_set_mouse_cursor_bitmap (cursor: AL_INT; bmp: AL_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_cursor_bitmap';

(* The mouse focus is the bit of the pointer that represents the actual mouse
   position, ie. the (@link(al_mouse_x), @link(al_mouse_y)) position.  By
   default this is the top left corner of the arrow, but if you are using a
   different mouse pointer you might need to alter it.
   @seealso(al_set_mouse_sprite) *)
  PROCEDURE al_set_mouse_sprite_focus (x, y: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_sprite_focus';

(* Measures how far the mouse has moved since the last call to this function.
   The values of @code(mickeyx) and @code(mickeyy) will become negative if the
   mouse is moved left or up, respectively.  The mouse will continue to
   generate movement mickeys even when it reaches the edge of the screen, so
   this form of input can be useful for games that require an infinite range of
   mouse movement.

   Note that the infinite movement may not work in windowed mode, since under
   some platforms the mouse would leave the window, and may not work at all if
   the hardware cursor is in use. *)
  PROCEDURE al_get_mouse_mickeys (OUT mickeyx, mickeyy: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mouse_mickeys';

(* You don't like Allegro's mouse pointer?  No problem.  Use this function to
   supply an alternative of your own.  If you change the pointer and then want
   to get Allegro's lovely arrow back again, call @code(al_set_mouse_sprite
   @(@nil@)).

   As a bonus, @code(al_set_mouse_sprite @(@nil@)) uses the current palette in
   choosing colors for the arrow.  So if your arrow mouse sprite looks ugly
   after changing the palette, call @code(al_set_mouse_sprite @(@nil@)).
   @seealso(al_show_mouse) @seealso(al_set_mouse_sprite_focus) *)
  PROCEDURE al_set_mouse_sprite (sprite: AL_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mouse_sprite';

(* In case you do not need Allegro's mouse cursor API, which automatically
   emulates a cursor in software if no other cursor is available, you can use
   this low level function to try to display or hide the system cursor
   directly.  The cursor parameter takes the same values as
   @link(al_select_mouse_cursor).  This function is very similar to calling
   @link(al_enable_hardware_cursor), @code(al_select_mouse_cursor) and
   @link(al_show_mouse), but will not try to do anything if no system cursor
   is available.

   The most common use for this function is to just call it once at the
   beginning of the program to tell it to display the system cursor inside the
   Allegro window.  The return value can be used to see if this succeeded or
   not.  On some systems (e.g. DirectX fullscreen) this is not supported and
   the function will always fail, and in other cases only some of the cursors
   will work, or in the case of @link(AL_MOUSE_CURSOR_ALLEGRO), only certain
   bitmap sizes may be supported.

   You never should use @code(al_show_os_cursor) together with the function
   @code(al_show_mouse) and other functions affecting it
   (@code(al_select_mouse_cursor), @code(al_enable_hardware_cursor),
   @link(al_disable_hardware_cursor), @link(al_scare_mouse),
   @link(al_unscare_mouse)).  They implement the standard high level mouse API,
   and don't work together with this low level function.

   @returns(@true if a system cursor is being displayed after the function
     returns, or @false otherwise.) *)
  FUNCTION al_show_os_cursor (cursor: AL_INT): AL_BOOL;
    INLINE;

(* Tells you whether the mouse pointer is currently on screen.

    This function can be useful to prevent having two mouse pointers on the
    screen at the same time when running your program in windowed mode and
    drawing the mouse pointer yourself.  Other possible uses include the
    ability to pause your game when the mouse goes off of the window, or only
    scrolling the view when the pointer is near the edge of the window, but not
    while off of the window. *)
  FUNCTION al_mouse_on_screen: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_on_screen';



(******************************************************************************
 * draw.h
 *    Drawing and sprite routines.
 *)

  (* Drawing modes. *)
  CONST
  (* Flag for @link(al_drawing_mode).

     The pixels of the bitmap being drawn onto are simply replaced by those
     produced by the drawing function. *)
    AL_DRAW_MODE_SOLID		= 0;
  (* Flag for @link(al_drawing_mode).

     Pixels are written to the bitmap with an exclusive-or operation rather than
     a simple copy, so drawing the same shape twice will erase it.  Because it
     involves reading as well as writing the bitmap memory, xor drawing is a lot
     slower than the normal replace mode. *)
    AL_DRAW_MODE_XOR		= 1;
  (* Flag for @link(al_drawing_mode).

     Pixels are simply copied from the pattern bitmap onto the destination
     bitmap.  This allows the use of multicolored patterns, and means that the
     color you pass to the drawing routine is ignored.  This is the fastest of
     the patterned modes. *)
    AL_DRAW_MODE_COPY_PATTERN	= 2;
  (* Flag for @link(al_drawing_mode).

     Each pixel in the pattern bitmap is compared with the mask color, which is
     zero in 256-color modes or bright pink for truecolor data (maximum red and
     blue, zero green).  If the pattern pixel is solid, a pixel of the color you
     passed to the drawing routine is written to the destination bitmap,
     otherwise a zero is written.  The pattern is thus treated as a monochrome
     bitmask, which lets you use the same pattern to draw different shapes in
     different colors, but prevents the use of multicolored patterns. *)
    AL_DRAW_MODE_SOLID_PATTERN	= 3;
  (* Flag for @link(al_drawing_mode).

     It is almost the same as @link(AL_DRAW_MODE_SOLID_PATTERN), but the masked
     pixels are skipped rather than being written as zeros, so the background
     shows through the gaps. *)
    AL_DRAW_MODE_MASKED_PATTERN	= 4;
  (* Flag for @link(al_drawing_mode).

     The global @link(al_color_table) table or truecolor blender functions are
     used to overlay pixels on top of the existing image.  This must only be used
     after you have set up the color mapping table (for 256 color modes) or
     blender functions (for truecolor modes).  Because it involves reading as
     well as writing the bitmap memory, translucent drawing is very slow if you
     draw directly to video RAM, so wherever possible you should use a memory
     bitmap instead. *)
    AL_DRAW_MODE_TRANS		= 5;



(* Sets the graphics drawing mode.  This only affects the geometric routines
   like @link(al_putpixel), lines, rectangles, circles, polygons, floodfill,
   etc, not the text output, blitting, or sprite drawing functions.  The mode
   should be one of the following constants:
@unorderedList(
  @item(@link(AL_DRAW_MODE_SOLID))
  @item(@link(AL_DRAW_MODE_XOR))
  @item(@link(AL_DRAW_MODE_COPY_PATTERN))
  @item(@link(AL_DRAW_MODE_SOLID_PATTERN))
  @item(@link(AL_DRAW_MODE_MASKED_PATTERN))
  @item(@link(AL_DRAW_MODE_TRANS))
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
   you may wish to alter the anchor position.
   @seealso(al_xor_mode) @seealso(al_solid_mode) @seealso(al_set_trans_blender)
   @seealso(al_color_table) *)
  PROCEDURE al_drawing_mode (mode: AL_INT; pattern: AL_BITMAPptr;
	x_anchor, y_anchor: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'drawing_mode';

(* This is a shortcut for toggling xor drawing mode on and off.  Calling
   @code(al_xor_mode @(@true@)) is equivalent to @code(al_drawing_mode)
   @code(@(AL_DRAW_MODE_XOR, @nil, 0, 0@)).  Calling @code(al_xor_mode @(@false@))
   is equivalent to @code(al_drawing_mode @(A_DRAW_MODE_SOLID, @nil, 0, 0@)).
   @seealso(al_drawing_mode) *)
  PROCEDURE al_xor_mode (aOn: AL_BOOL);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'xor_mode';

(* This is a shortcut for selecting solid drawing mode.  It is equivalent to
   calling @code(al_drawing_mode) @code(@(AL_DRAW_MODE_XOR, NIL, 0, 0@)).
   @seealso(al_drawing_mode) *)
  PROCEDURE al_solid_mode;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'solid_mode';

(* Calculates all the points along a line from point (x1, y1) to (x2, y2),
   calling the supplied function for each one.  This will be passed a copy of
   the bmp parameter, the x and y position, and a copy of the d parameter.
   Example:
@longcode(#
PROCEDURE DrawDustParticle (bmp: AL_BITMAPptr; x, y, d: AL_INT); CDECL;
BEGIN
   ...
END;

  al_do_line (al_screen, 0, 0, AL_SCREEN_W-1, AL_SCREEN_H-2,
              DustStrength, @DrawDustParticle);
#) @seealso(al_do_circle) @seealso(al_do_ellipse) @seealso(al_do_arc) *)
  PROCEDURE al_do_line (bmp: AL_BITMAPptr; x1, y1, x2, y2, d: AL_INT; proc: AL_POINT_PROC);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_line';

(* Calculates all the points in a circle around point (x, y) with radius r,
   calling the supplied function for each one.  This will be passed a copy of
   the bmp parameter, the x and y position, and a copy of the d parameter.
   Example:
@longcode(#
PROCEDURE DrawExplosionRing (bmp: AL_BITMAPptr; x, y, d: AL_INT); CDECL;
BEGIN
   ...
END;

  al_do_circle (al_screen, AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
                AL_SCREEN_H DIV 16, FlameColor, @DrawExplosionRing);
#) @seealso(al_do_line) @seealso(al_do_ellipse) @seealso(al_do_arc) *)
  PROCEDURE al_do_circle (bmp: AL_BITMAPptr; x, y, radius, d: AL_INT; proc: AL_POINT_PROC);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_circle';

(* Calculates all the points in an ellipse around point (x, y) with radius rx
   and ry, calling the supplied function for each one.  This will be passed a
   copy of the bmp parameter, the x and y position, and a copy of the d
   parameter. Example:
@longcode(#
PROCEDURE DrawExplosionRing (bmp: AL_BITMAPptr; x, y, d: AL_INT); CDECL;
BEGIN
   ...
END;

  al_do_ellipse (al_screen, AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
                AL_SCREEN_H DIV 16, FlameColor, @DrawExplosionRing);
#) @seealso(al_do_line) @seealso(al_do_circle) @seealso(al_do_arc) *)
  PROCEDURE al_do_ellipse (bmp: AL_BITMAPptr; x, y, rx, ry, d: AL_INT; proc: AL_POINT_PROC);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_ellipse';

(* Calculates all the points in a circular arc around point (x, y) with radius
   r, calling the supplied function for each one.  This will be passed a copy
   of the bmp parameter, the x and y position, and a copy of the d parameter.
   The arc will be plotted in an anticlockwise direction starting from the
   angle a1 and ending when it reaches a2.  These values are specified in 16.16
   fixed point format, with 256 equal to a full circle, 64 a right angle, etc.
   Zero is to the right of the centre point, and larger values rotate
   anticlockwise from there.
   Example:
@longcode(#
PROCEDURE DrawExplosionRing (bmp: AL_BITMAPptr; x, y, d: AL_INT); CDECL;
BEGIN
   ...
END;

  al_do_arc (al_screen, AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2,
                AL_SCREEN_H DIV 16, FlameColor, @DrawExplosionRing);
#) @seealso(al_do_line) @seealso(al_do_circle) @seealso(al_do_ellipse) *)
  PROCEDURE al_do_arc (bmp: AL_BITMAPptr; x, y: AL_INT; ang1, ang2: AL_FIXED; r, d: AL_INT; proc: AL_POINT_PROC);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_arc';

(* Calculates a series of npts values along a Bezier spline, storing them in
   the output x and y arrays.

   The Bezier curve is specified by the four x/y control points in the points
   array:  points[0] and points[1] contain the coordinates of the first control
   point, points[2] and points[3] are the second point, etc.  Control points 0
   and 3 are the ends of the spline, and points 1 and 2 are guides.  The curve
   probably won't pass through points 1 and 2, but they affect the shape of the
   curve between points 0 and 3 @(the lines p0-p1 and p2-p3 are tangents to the
   spline@).  The easiest way to think of it is that the curve starts at p0,
   heading in the direction of p1, but curves round so that it arrives at p3
   from the direction of p2.

   In addition to their role as graphics primitives, spline curves can be
   useful for constructing smooth paths around a series of control points, as
   in exspline.pp.
   @seealso(al_spline) *)
  PROCEDURE al_calc_spline (VAR points: ARRAY OF AL_INT; npts: AL_INT; VAR x, y: ARRAY OF AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'calc_spline';

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

   If the @link(AL_GFX_HW_VRAM_BLIT) bit in the @link(al_gfx_capabilities)
   flag is set, the current driver supports hardware accelerated blits from one
   part of the screen onto another.  This is extremely fast, so when this flag
   is set it may be worth storing some of your more frequently used graphics in
   an offscreen portion of the video memory.

   Unlike most of the graphics routines, @code(al_blit) allows the source and
   destination bitmaps to be of different color depths, so it can be used to
   convert images from one pixel format to another.  In this case, the behavior
   is affected by the @code(AL_COLORCONV_KEEP_TRANS)
   and @code(AL_COLORCONV_DITHER* ) flags of the current color conversion mode.
   @seealso(al_masked_blit) @seealso(al_stretch_blit)
   @seealso(al_set_color_conversion) *)
  PROCEDURE al_blit (source, dest: AL_BITMAPptr; source_x, source_y, dest_x, dest_y, width, height: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'blit';

(* Like @link(al_blit), but skips transparent pixels, which are marked by a
   zero in 256-color modes or bright pink for truecolor data (maximum red and
   blue, zero green), and requires the source and destination bitmaps to be of
   the same color depth.  The source and destination regions must not overlap.

   If the @link(AL_GFX_HW_VRAM_BLIT_MASKED) bit in the
   @link(al_gfx_capabilities) flag is set, the current driver supports hardware
   accelerated masked blits from one part of the screen onto another.  This is
   extremely fast, so when this flag is set it may be worth storing some of
   your more frequently used sprites in an offscreen portion of the video
   memory.

   @bold(Warning:)  if the hardware acceleration flag is not set,
   @code(al_masked_blit) will not work correctly when used with a source image
   in system or video memory so the latter must be a memory bitmap.
   @seealso(al_masked_stretch_blit) @seealso(al_draw_sprite)
   @seealso(al_bitmap_mask_color) *)
  PROCEDURE al_masked_blit (source, dest: AL_BITMAPptr; source_x, source_y, dest_x, dest_y, width, height: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'masked_blit';

(* Like @link(al_blit), except it can scale images (so the source and
   destination rectangles don't need to be the same size) and requires the
   source and destination bitmaps to be of the same color depth.  This routine
   doesn't do as much safety checking as the regular @code(al_blit):  in
   particular you must take care not to copy from areas outside the source
   bitmap, and you cannot blit between overlapping regions, ie. you must use
   different bitmaps for the source and the destination.  Moreover, the source
   must be a memory bitmap.
   @seealso(al_masked_stretch_blit) @seealso(al_stretch_sprite) *)
  PROCEDURE al_stretch_blit (source, dest: AL_BITMAPptr; source_x, source_y, source_width, source_height, dest_x, dest_y, dest_width, dest_height: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stretch_blit';

(* Like @link(al_masked_blit), except it can scale images (so the source and
   destination rectangles don't need to be the same size).  This routine
   doesn't do as much safety checking as the regular @code(al_masked_blit):
   in particular you must take care not to copy from areas outside the source
   bitmap.  Moreover, the source must be a memory bitmap.
   @seealso(al_blit) @seealso(al_stretch_sprite) *)
  PROCEDURE al_masked_stretch_blit (source, dest: AL_BITMAPptr; source_x, source_y, source_width, source_height, dest_x, dest_y, dest_width, dest_height: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'masked_stretch_blit';

(* Like @link(al_draw_sprite), except it can stretch the sprite image to the
   specified width and height and requires the sprite image and destination
   bitmap to be of the same color depth.  Moreover, the sprite image must be a
   memory bitmap.
   @seealso(al_stretch_blit) @seealso(al_bitmap_mask_color) *)
  PROCEDURE al_stretch_sprite (bmp, sprite: AL_BITMAPptr; x, y, w, h: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stretch_sprite';



(* rotate+trans *)

(* Rotates a sprite.

  Draws the sprite image onto the bitmap.  It is placed with its top left
  corner at the specified position, then rotated by the specified angle around
  its centre.  The angle is a fixed point 16.16 number in the same format used
  by the fixed point trig routines, with 256 equal to a full circle, 64 a right
  angle, etc.  All rotation functions can draw between any two bitmaps, even
  screen bitmaps or bitmaps of different color depth.

  Positive increments of the angle will make the sprite rotate clockwise on the
  screen, as demonstrated by the Allegro example.
  @seealso(al_draw_trans_sprite) @seealso(al_rotate_scaled_sprite_trans)
  @seealso(al_rotate_sprite_v_flip_trans)
  @seealso(al_rotate_scaled_sprite_v_flip_trans) *)
  PROCEDURE al_rotate_sprite_trans (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_sprite_trans';

(* Rotates and flips a sprite.

  Like al_rotate_sprite_trans, but flips the image vertically before rotating
  it.  To flip horizontally, use this routine but add @code(al_itofix @(128@))
  to the angle.  To flip in both directions, use @seealso(al_rotate_sprite) and
  add @code(al_itofix (128)) to its angle. *)
  PROCEDURE al_rotate_sprite_v_flip_trans (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_sprite_v_flip_trans';

(* Rotates and stretches a sprite.

  Like @link(al_rotate_sprite_trans), but stretches or shrinks the image at the
  same time as rotating it. *)
  PROCEDURE al_rotate_scaled_sprite_trans (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_scaled_sprite_trans';

(* Rotates and stretches a sprite.

  Like @link(al_rotate_scaled_sprite_trans), except that it flips the sprite
  vertically first. *)
  PROCEDURE al_rotate_scaled_sprite_v_flip_trans (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_scaled_sprite_v_flip_trans';

(* Rotates a sprite around a specified point.

  Like @link(al_rotate_sprite_trans), but aligns the point in the sprite given
  by @code(cx, cy) to @code(x, y) in the bitmap, then rotates around this
  point. *)

  PROCEDURE al_pivot_sprite_trans (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_sprite_trans';

(* Rotates and flips a sprite around a specified point.

  Like @link(al_rotate_sprite_trans), but aligns the point in the sprite given
  by @code(cx, cy) to @code(x, y) in the bitmap, then rotates around this
  point. *)
  PROCEDURE al_pivot_sprite_v_flip_trans (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_sprite_v_flip_trans';

(* Rotates and stretches a sprite.

  Like @link(al_rotate_scaled_sprite_trans), but aligns the point in the sprite
  given by @code(cx, cy) to @code(x, y) in the bitmap, then rotates and scales
  around this point. *)
  PROCEDURE al_pivot_scaled_sprite_trans (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_scaled_sprite_trans';

(* Rotates and stretches a sprite.

  Like @link(al_rotate_scaled_sprite_v_flip_trans), but aligns the point in the
  sprite given by (cx, cy) to (x, y) in the bitmap, then rotates and scales
  around this point. *)
  PROCEDURE al_pivot_scaled_sprite_v_flip_trans (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_scaled_sprite_v_flip_trans';



(* rotate+lit *)

(* Rotates a sprite.

  Draws the sprite image onto the bitmap.  It is placed with its top left
  corner at the specified position, then rotated by the specified angle around
  its centre.  The angle is a fixed point 16.16 number in the same format used
  by the fixed point trig routines, with 256 equal to a full circle, 64 a right
  angle, etc.  All rotation functions can draw between any two bitmaps, even
  screen bitmaps or bitmaps of different color depth. *)
  PROCEDURE al_rotate_sprite_lit (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED; color: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_sprite_lit';

(* Rotates a sprite.

   Like @seealso(al_rotate_sprite_lit), but flips the image vertically before
   rotating it.  To flip horizontally, use this routine but add
   @code(al_itofix@(128@)) to the angle.  To flip in both directions, use
   @code(al_rotate_sprite_lit) and add @code(al_itofix@(128@)) to its angle. *)
  PROCEDURE al_rotate_sprite_v_flip_lit (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED; color: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_sprite_v_flip_lit';

(* Rotates and stretches a sprite.

  Like @link(al_rotate_sprite_lit), but stretches or shrinks the image at the
  same time as rotating it. *)
  PROCEDURE al_rotate_scaled_sprite_lit (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED; color: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_scaled_sprite_lit';

(* Rotates and stretches a sprite.

  Draws the sprite, similar to @link(al_rotate_scaled_sprite_lit) except that
  it flips the sprite vertically first. *)
  PROCEDURE al_rotate_scaled_sprite_v_flip_lit (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED; color: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rotate_scaled_sprite_v_flip_lit';

(* Rotates a sprite.

  Like @link(al_rotate_sprite_lit), but aligns the point in the sprite given by
  (cx, cy) to (x, y) in the bitmap, then rotates around this point. *)
  PROCEDURE al_pivot_sprite_lit (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED; color: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_sprite_lit';

(* Rotates a sprite.

  Like @link(al_rotate_sprite_v_flip_lit), but aligns the point in the sprite
  given by (cx, cy) to (x, y) in the bitmap, then rotates around this point. *)
  PROCEDURE al_pivot_sprite_v_flip_lit (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED; color: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_sprite_v_flip_lit';

(* Rotates and stretches a sprite.

  Like @link(al_rotate_scaled_sprite_lit), but aligns the point in the sprite
  given by (cx, cy) to (x, y) in the bitmap, then rotates and scales around
  this point. *)
  PROCEDURE al_pivot_scaled_sprite_lit (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED; color: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_scaled_sprite_lit';

(* Rotates and stretches a sprite.

  Like @link(al_rotate_scaled_sprite_v_flip_lit), but aligns the point in the
  sprite given by (cx, cy) to (x, y) in the bitmap, then rotates and scales
  around this point. *)
  PROCEDURE al_pivot_scaled_sprite_v_flip_lit (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED; color: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pivot_scaled_sprite_v_flip_lit';



(******************************************************************************
 * draw.inl
 *)

(* Reads a pixel from point (x, y) in the bitmap.

   @returns(-1 if the point lies outside the bitmap @(ignoring the clipping
     rectangle@), otherwise the value of the pixel in the color format of the
     bitmap.

     @bold(Warning:) -1 is also a valid value for pixels contained in 32-bit
     bitmaps with alpha channel @(when R,G,B,A are all equal to 255@) so you
     can't use the test against -1 as a predicate for such bitmaps.  In this
     cases, the only reliable predicate is check if it is inside the bitmap.

     To extract the individual color components, use the @link(al_getr) /
     @link(al_getg) / @link(al_getb) / @link(al_geta) family of functions)
   @seealso(al_putpixel) @seealso(_al_getpixel) *)
  FUNCTION  al_getpixel (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
    INLINE;

(* Writes a pixel to the specified position in the bitmap, using the current
   drawing mode and the bitmap's clipping rectangle.
   @seealso(al_getpixel) @seealso(_al_putpixel) @seealso(al_drawing_mode)
   @seealso(al_makecol) *)
  PROCEDURE al_putpixel (bmp: AL_BITMAPptr; x, y, color: AL_INT);
    INLINE;

(* Draws a vertical line onto the bitmap, from point (x, y1) to (x, y2).
   @seealso(al_hline) @seealso(al_line) @seealso(al_drawing_mode)
   @seealso(al_makecol) *)
  PROCEDURE al_vline (bmp: AL_BITMAPptr; x, y1, y2, color: AL_INT);
    INLINE;

(* Draws a horizontal line onto the bitmap, from point (x1, y) to (x2, y).
   @seealso(al_vline) @seealso(al_line) @seealso(al_drawing_mode)
   @seealso(al_makecol) *)
  PROCEDURE al_hline (bmp: AL_BITMAPptr; x1, y, x2, color: AL_INT);
    INLINE;

(* Draws a line onto the bitmap, from point (x1, y1) to (x2, y2).
   @seealso(al_fastline) @seealso(al_polygon) @seealso(al_do_line).
   @seealso(al_drawing_mode) @seealso(al_makecol) *)
  PROCEDURE al_line (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);

(* Faster version of the previous function.  Note that pixel correctness is not
   guaranteed for this function. @seealso(al_line) *)
  PROCEDURE al_fastline (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
    INLINE;

(* Draws a solid, filled rectangle with the two points as its opposite
   corners. @seealso(al_triangle) @seealso(al_polygon) @seealso(al_quad3d)
   @seealso(al_drawing_mode) @seealso(al_makecol) *)
  PROCEDURE al_rectfill (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
    INLINE;

(* Draws a filled triangle between the three points.
   @seealso(al_polygon) @seealso(al_triangle3d) @seealso(al_drawing_mode)
   @seealso(al_makecol) *)
  PROCEDURE al_triangle (bmp: AL_BITMAPptr; x1, y1, x2, y2, x3, y3, color: AL_INT);
    INLINE;

(* Draws a filled polygon with an arbitrary number of corners.  Pass the number
   of vertices and an array containing a series of x, y points (a total of
   vertices*2 values).
   @seealso(al_triangle) @seealso(al_polygon3d) @seealso(al_drawing_mode)
   @seealso(al_makecol) *)
  PROCEDURE al_polygon (bmp: AL_BITMAPptr; vertices: AL_INT; CONST points: ARRAY OF AL_INT; color: AL_INT);

(* Draws an outline rectangle with the two points as its opposite corners.
   @seealso(al_rectfill) @seealso(al_drawing_mode) @seealso(al_makecol) *)
  PROCEDURE al_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
    INLINE;

(* Draws a circle with the specified centre and radius. @seealso(al_ellipse)
   @seealso(al_circlefill) @seealso(al_do_circle) @seealso(al_arc)
   @seealso(al_drawing_mode) @seealso(al_makecol) *)
  PROCEDURE al_circle (bmp: AL_BITMAPptr; x, y, r, color: AL_INT);
    INLINE;

(* Draws a filled circle with the specified centre and radius.
   @seealso(al_circle) @seealso(al_drawing_mode) @seealso(al_makecol) *)
  PROCEDURE al_circlefill (bmp: AL_BITMAPptr; x, y, r, color: AL_INT);
    INLINE;

(* Draws an ellipse with the specified centre and radius. @seealso(al_circle)
   @seealso(al_ellipsefill) @seealso(al_arc) @seealso(al_do_ellipse)
   @seealso(al_drawing_mode) @seealso(al_makecol) *)
  PROCEDURE al_ellipse (bmp: AL_BITMAPptr; x, y, rx, ry, color: AL_INT);
    INLINE;

(* Draws a filled ellipse with the specified centre and radius.
   @seealso(al_ellipse) @seealso(al_drawing_mode) @seealso(al_makecol) *)
  PROCEDURE al_ellipsefill (bmp: AL_BITMAPptr; x, y, rx, ry, color: AL_INT);
    INLINE;

(* Draws a circular arc.

  Draws a circular arc with centre x, y and radius r, in an anticlockwise
  direction starting from the angle a1 and ending when it reaches a2.  These
  values are specified in 16.16 fixed point format, with 256 equal to a full
  circle, 64 a right angle, etc.  Zero is to the right of the centre point, and
  larger values rotate anticlockwise from there.
  @seealso(al_circle) @seealso(al_do_arc) @seealso(al_drawing_mode)
  @seealso(al_makecol) *)
  PROCEDURE al_arc (bmp: AL_BITMAPptr; x, y: AL_INT; ang1, ang2: AL_FIXED; r, color: AL_INT);
    INLINE;

(* Draws a Bezier spline using the four control points specified in the points
   array.  Read the description of @link(al_calc_spline) for information on how to
   build the points array. @seealso(al_drawing_mode) @seealso(al_makecol) *)
  PROCEDURE al_spline (bmp: AL_BITMAPptr; CONST points: ARRAY OF AL_INT; color: AL_INT);

(* Floodfills an enclosed area, starting at point (x, y), with the specified
   color. @seealso(al_drawing_mode) @seealso(al_makecol) *)
  PROCEDURE al_floodfill (bmp: AL_BITMAPptr; x, y, color: AL_INT);
    INLINE;



(* Draws a copy of the sprite bitmap onto the destination bitmap at the
   specified position.  This is almost the same as @link(al_blit)
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

  If the @link(AL_GFX_HW_VRAM_BLIT_MASKED) bit in the
  @link(al_gfx_capabilities) flag is set, the current driver supports hardware
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
  PROCEDURE al_draw_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
    INLINE;


(* Draws the sprite image onto the destination bitmap using the specified
   @code(mode) argument, optionally flipping the sprite in the orientation
   specified by @code(flip) argument.
   @param(mode defines how is sprite going to be drawn on the destination
     bitmap:
@unorderedList(
  @item(@code(AL_DRAW_SPRITE_NORMAL) draws a masked sprite, like
    @link(al_draw_sprite).)
  @item(@code(AL_DRAW_SPRITE_LIT) draws a tinted sprite, like
    @link(al_draw_lit_sprite).)
  @item(@code(AL_DRAW_SPRITE_TRANS) draws a blended sprite, like
    @link(al_draw_trans_sprite). )
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
  PROCEDURE al_draw_sprite_ex (bmp, sprite: AL_BITMAPptr; x, y, mode, flip: AL_INT);
    INLINE;

(* This is like @link(al_draw_sprite), but it additionally flip the image
   vertically.  Flipping vertically means that the y-axis is reversed,
   between the source and the destination.  This produces exact mirror images,
   which is not the same as rotating the sprite (and it is a lot faster than
   the rotation routine).  The sprite must be a memory bitmap. *)
  PROCEDURE al_draw_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
    INLINE;

(* This is like @link(al_draw_sprite), but it additionally flip the image
   horizontally.  Flipping horizontally means that the x-axis is reversed,
   between the source and the destination.  This produces exact mirror images,
   which is not the same as rotating the sprite (and it is a lot faster than
   the rotation routine).  The sprite must be a memory bitmap. *)
  PROCEDURE al_draw_sprite_h_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
    INLINE;

(* This is like @link(al_draw_sprite), but it additionally flip the image
   vertically and horizontally.  Flipping vertically means that the y-axis is
   reversed, while flipping horizontally means that de x-axis is reversed,
   between the source and the destination.  This produces exact mirror images,
   which is not the same as rotating the sprite (and it is a lot faster than
   the rotation routine).  The sprite must be a memory bitmap. *)
  PROCEDURE al_draw_sprite_vh_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
    INLINE;

(* Uses the global @link(al_color_table) table or truecolor blender functions
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
   as you call @link(al_set_write_alpha_blender) first.  As
   @link(al_draw_sprite) this function skips transparent pixels, except if the
   source sprite is an 8-bit image;  if this is the case, you should pay
   attention to properly set up your color map table for index 0.
   @seealso(al_draw_lit_sprite) @seealso(al_color_table) *)
  PROCEDURE al_draw_trans_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
    INLINE;

(* In 256-color modes, uses the global @link(al_color_table) table to tint the
   sprite image to the specified color or to light it to the level specified by
   'color', depending on the function which was used to build the table
   (@link(al_create_trans_table) or @link(al_create_light_table)), and draws
   the resulting image to the destination bitmap.  In truecolor modes, uses the
   blender functions to light the sprite image using the alpha level specified
   by 'color' (the alpha level which was passed to the blender functions is
   ignored) and draws the resulting image to the destination bitmap.

   @param(c must be in the range [0..255] whatever its actual meaning is.
     This must only be used after you have set up the color mapping table @(for
     256-color modes@) or blender functions @(for truecolor modes@).)
   @seealso(al_draw_sprite) @seealso(al_draw_gouraud_sprite)
   @seealso(al_color_table) *)
  PROCEDURE al_draw_lit_sprite (bmp, sprite: AL_BITMAPptr; x, y, c: AL_INT);
    INLINE;

(* More sophisticated version of @link(al_draw_lit_sprite):  the @code(color)
   parameter is not constant across the sprite image anymore but interpolated
   between the four specified corner colors.  The corner values passed to this
   function indicate the strength of the color applied on them, ranging from 0
   (no strength) to 255 (full strength).
   @seealso(al_draw_sprite) @seealso(al_color_table) *)
  PROCEDURE al_draw_gouraud_sprite (bmp, sprite: AL_BITMAPptr; x, y, c1, c2, c3, c4: AL_INT);
    INLINE;

(* Draws the sprite image onto the bitmap.  It is placed with its top left
   corner at the specified position, then rotated by the specified angle around
   its centre.  The angle is a fixed point 16.16 number in the same format used
   by the fixed point trig routines, with 256 equal to a full circle, 64 a
   right angle, etc.  All rotation functions can draw between any two bitmaps,
   even screen bitmaps or bitmaps of different color depth.

   Positive increments of the angle will make the sprite rotate clockwise. *)
  PROCEDURE al_rotate_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
    INLINE;

(* Like @link(al_rotate_sprite), but flips the image vertically before
   rotating it.  To flip horizontally, use this routine but add
   @code(al_itofix @(128@)) to the angle.  To flip in both directions, use
   @link(al_rotate_sprite) and add @code(al_itofix @(128@)) to its angle. *)
  PROCEDURE al_rotate_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
    INLINE;

(* Like @link(al_rotate_sprite), but stretches or shrinks the image at the
   same time as rotating it. *)
  PROCEDURE al_rotate_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);
    INLINE;

(* Draws the sprite, similar to @link(al_rotate_scaled_sprite) except that it
  flips the sprite vertically first. *)
  PROCEDURE al_rotate_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);
    INLINE;

(* Like @link(al_rotate_sprite), but aligns the point in the sprite given by
   @code(cx, cy) to @code(x, y) in the bitmap, then rotates around this
   point. *)
  PROCEDURE al_pivot_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
    INLINE;

(* Like @link(al_rotate_sprite_v_flip), but aligns the point in the sprite
   given by @code(cx, cy) to @code(x, y) in the bitmap, then rotates around
   this point. *)
  PROCEDURE al_pivot_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
    INLINE;

(* Like @link(al_rotate_scaled_sprite), but aligns the point in the sprite
   given by @code(cx, cy) to @code(x, y) in the bitmap, then rotates around
   this point. *)
  PROCEDURE al_pivot_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);
    INLINE;

(* Like @link(al_rotate_scaled_sprite_v_flip), but aligns the point in the
   sprite given by @code(cx, cy) to @code(x, y) in the bitmap, then rotates
   and scales around this point. *)
  PROCEDURE al_pivot_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);
    INLINE;



(* Like the regular @link(al_putpixel), but much faster because it's
   implemented as an inline functions for specific 8 bits color depth.  It
   don't perform any clipping (they will crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel (bmp: AL_BITMAPptr; x, y, color: AL_INT);
    INLINE;

(* Faster inline version of @link(al_getpixel).  This is specific for 8 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 8bpp) *)
  FUNCTION _al_getpixel (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
    INLINE;

(* Like the regular @link(al_putpixel), but much faster because it's
   implemented as an inline function for specific 15 bits color depth.  It
   don't perform any clipping (it may crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel15 (bmp: AL_BITMAPptr; x, y, color: AL_INT);
    INLINE;

(* Faster inline version of @link(al_getpixel).  This is specific for 15 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 15bpp) *)
  FUNCTION _al_getpixel15 (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
    INLINE;

(* Like the regular @link(al_putpixel), but much faster because it's
   implemented as an inline function for specific 16 bits color depth.  It
   don't perform any clipping (it may crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel16 (bmp: AL_BITMAPptr; x, y, color: AL_INT);
    INLINE;

(* Faster inline version of @link(al_getpixel).  This is specific for 16 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 16bpp) *)
  FUNCTION _al_getpixel16 (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
    INLINE;

(* Like the regular @link(al_putpixel), but much faster because it's
   implemented as an inline function for specific 24 bits color depth.  It
   don't perform any clipping (it may crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel24 (bmp: AL_BITMAPptr; x, y, color: AL_INT);

(* Faster inline version of @link(al_getpixel).  This is specific for 24 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 24bpp) *)
  FUNCTION _al_getpixel24 (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
    INLINE;

(* Like the regular @link(al_putpixel), but much faster because it's
   implemented as an inline function for specific 32 bits color depth.  It
   don't perform any clipping (it may crash if you try to draw outside the
   bitmap!), and ignore the drawing mode. *)
  PROCEDURE _al_putpixel32 (bmp: AL_BITMAPptr; x, y, color: AL_INT);
    INLINE;

(* Faster inline version of @link(al_getpixel).  This is specific for 32 bits
   color depth and don't do any clipping, so you must make sure the point
   lies inside the bitmap. @returns(the value of the pixel in 32bpp) *)
  FUNCTION _al_getpixel32 (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
    INLINE;



(******************************************************************************
 * font.h
 *     Font loading routines.
 *)

  TYPE
  (* A pointer to a structure holding an Allegro font, usually created beforehand
     with the grabber tool or Allegro's default font.  Read introduction of
     @code(text) for a description on how to load/destroy fonts, and how to
     show text. *)
    AL_FONTptr = AL_POINTER;

(* Search all pixels of a font for alpha values. @seealso(al_is_trans_font) *)
  FUNCTION al_font_has_alpha (f: AL_FONTptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'font_has_alpha';

(* Makes a font use transparency.   That is, each glyph in the font will be
   drawn with @seealso(al_draw_trans_sprite), so you can use the same blenders
   as with @code(al_draw_trans_sprite) to draw the font.  One common use of
   this is to load a bitmap font with an alpha channel, and therefore get
   anti-aliased text output by using Allegro's alpha blender.  Here's an
   example how to do that:
@longcode(#
VAR
  f: AL_FONTptr;
BEGIN
  f := al_load_font ('alphafont.tga', NIL, NIL);
  al_make_trans_font (f);
  al_set_alpha_blender;
  al_textout_centre_ex (al_screen, f, 'Anti-aliased Font!', 320, 240, -1, -1);
  al_destroy_font (f);
END;
#)
   @seealso(al_is_trans_font) @seealso(al_set_alpha_blender) @seealso(al_load_font)
  *)
  PROCEDURE al_make_trans_font (f: AL_FONTptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'make_trans_font';



(* This function checks if the given font is a color font using
   @link(al_draw_trans_sprite) to render glyphs.
   @seealso(al_make_trans_font) @seealso(al_is_color_font)
   @seealso(al_is_mono_font) *)
  FUNCTION al_is_trans_font (f: AL_FONTptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'is_trans_font';

(* This function checks if the given font is a color font, as opposed to a
   monochrome font.
   @returns(@true if the font is a color font, @false if it is not.)
   @seealso(al_is_mono_font) @seealso(al_is_trans_font) *)
  FUNCTION al_is_color_font (f: AL_FONTptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'is_color_font';

(* This function checks if the given font is a mono font, as opposed to a
   color font.
   @returns(@true if the font is a monochrome font, @false if it is not.)
   @seealso(al_is_color_font) @seealso(al_is_trans_font) *)
  FUNCTION al_is_mono_font (f: AL_FONTptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'is_mono_font';

(* This function compares the two fonts, which you can use to find out if
   Allegro is capable of merging them.

   @returns(@true if the two fonts are of the same general type @(both are
     color fonts or both are monochrome fonts, for instance@).) *)
  FUNCTION al_is_compatible_font (f1, f2: AL_FONTptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'is_compatible_font';



(* Loads a font from a file.  At present, this supports loading fonts from a
   GRX format .fnt file, a 8x8 or 8x16 BIOS format .fnt file, a datafile or any
   bitmap format that can be loaded by @link(al_load_bitmap).

   If the font contains palette information, then the palette is returned in
   the second parameter, which should be a pointer to an @link(AL_PALETTE).
   The @code(palette) argument may be @nil.  In this case, the palette data,
   if present, is simply not returned.

   The third parameter can be used to pass specific information to a custom
   loader routine.  Normally, you can just leave this as @nil.  Note that
   another way of loading fonts is embedding them into a datafile and using the
   datafile related functions.
   @returns(a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.)
   @seealso(al_load_bitmap_font) @seealso(al_grab_font_from_bitmap)
   @seealso(alfile) @seealso(al_destroy_font) *)
  FUNCTION al_load_font (filename: AL_STR; pal: AL_PALETTEptr; param: AL_VOIDptr): AL_FONTptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_font';

(* Tries to grab a font from a bitmap.  The bitmap can be in any format that
   @link(al_load_bitmap) understands.

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
   the @code(demo.dat) file and export the @code(TITLE_FONT) into a PCX file.
   Have a look at the resulting picture in your paint program:  that is the
   format a font should be in.

   Take care with high and true color fonts:  Allegro will convert these to the
   current color depth when you load the font.  If you try to use a font on a
   bitmap with a different color depth Allegro will do color conversions on the
   fly, which will be rather slow.  For optimal performance you should set the
   color depth to the color depth you want to use before loading any fonts.

   @returns(a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.)
   @seealso(al_load_font) @seealso(al_destroy_font) *)
  FUNCTION al_load_bitmap_font (filename: AL_STR; pal: AL_PALETTEptr; param: AL_VOIDptr): AL_FONTptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bitmap_font';

(* Loads a font script.  The script file contains a number of lines in the
   format @code(filename start end), which specify the source file for that
   range of characters, the Unicode value of the first character in the range,
   and the end character in the range (optional, if left out, the entire input
   file will be grabbed). If the filename is replaced by a hyphen, more
   characters will be grabbed from the previous input file. For example, the script:
@longcode(#
          ascii.fnt 0x20 0x7F
          - 0xA0 0xFF
          dingbats.fnt 0x1000
#)
   would import the first 96 characters from ascii.fnt as the range 0x20-0x7F,
   the next 96 characters from ascii.fnt as the range 0xA0-0xFF, and the entire
   contents of dingbats.fnt starting at Unicode position 0x1000.
   @returns(A pointer to the font or @nil on error. Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.)
   @seealso(al_load_font) @seealso(al_destroy_font) *)
  FUNCTION al_load_txt_font (CONST filename: AL_STR; pal: AL_PALETTEptr; param: AL_VOIDptr): AL_FONTptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_txt_font';

(* This function is the work-horse of @link(al_load_bitmap_font), and can be
   used to grab a font from a bitmap in memory.  You can use this if you want
   to generate or modify a font at runtime.  The bitmap should follow the
   layout described for @code(al_load_bitmap_font).

   @returns(a pointer to the font or @nil on error.  Remember that you are
     responsible for destroying the font when you are finished with it to avoid
     memory leaks.)
   @seealso(al_destroy_font) *)
  FUNCTION al_grab_font_from_bitmap (bmp: AL_BITMAPptr): AL_FONTptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'grab_font_from_bitmap';



(* Returns the number of character ranges in a font.  You should query each of
   these ranges with @link(al_get_font_range_begin) and
   @link(al_get_font_range_end) to find out what characters are available in
   the font. Example:
@longcode(#
VAR
  f: AL_FONTptr;
  Range, n: INTEGER;
BEGIN
  ...
  Range := al_get_font_ranges (f);
  WriteLn ('The font has ', Range, ' character ranges:');
  FOR N := 0 TO Range - 1 DO
    WriteLn ('Range ',n
      ' from 0x', IntToHex (al_get_font_range_begin (f, n), 3),
      ' to 0x', IntToHex (al_get_font_range_end (f, n), 3));
END;
#)
  @returns(the number of continuous character ranges in a font, or -1 if that
    information is not available.)
  @seealso(al_transpose_font) *)
  FUNCTION al_get_font_ranges (f: AL_FONTptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_font_ranges';

(* Returns the start of a character range in a font.  You can pass -1 for the
   @code(range) parameter if you want to know the start of the whole font
   range, or a number from 0 to (but not including) @link(al_get_font_ranges)
   @code(@(f@)) to get the start of a specific character range in the font.
   @seealso(al_get_font_range_end) @seealso(al_transpose_font) *)
  FUNCTION al_get_font_range_begin (f: AL_FONTptr; range: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_font_range_begin';

(* Returns the last character of a character range in a font.  You can pass -1
   for the range parameter if you want to know the start of the whole font
   range, or a number from 0 to (but not including) @link(al_get_font_ranges)
   @code(@(f@)) to get the start of a specific character range in the font.
   You should check the start and end of all font ranges to see if a specific
   character is actually available in the font.  Not all characters in the
   range returned by @code(al_get_font_range_begin @(f, -1@)) and
   @code(al_get_font_range_end @(f, -1@)) need to be available!
   @seealso(al_get_font_range_begin) @seealso(al_transpose_font) *)
  FUNCTION al_get_font_range_end (f: AL_FONTptr; range: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_font_range_end';

(* Extracts a range of characters from a font.  This function extracts a
   character range from a font and returns a new font that contains only the
   range of characters selected by this function.  You can pass -1 for either
   the lower or upper bound if you want to select all characters from the start
   or to the end of the font. Example:
@longcode(#
VAR
  MyFont, Capitals, FontCopy: AL_FONTptr;
...
  Capitals := al_extract_font_range (MyFont, Ord ('A'), Ord ('Z'));
  FontCopy := al_extract_font_range (MyFont, -1, -1);
...
  al_destroy_font (Capitals);
  al_destroy_font (FontCopy);
#)
  @returns(a pointer to the new font or @nil on error.  Remember that you are
    responsible for destroying the font when you are finished with it to avoid
    memory leaks.)
  @seealso(al_get_font_range_begin) @seealso(al_get_font_range_end)
  @seealso(al_merge_fonts) @seealso(al_transpose_font) *)
  FUNCTION al_extract_font_range (f: AL_FONTptr; start, finish: AL_INT): AL_FONTptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'extract_font_range';

(* Merges two fonts into one font.  This function merges the character ranges
   from two fonts and returns a new font containing all characters in the old
   fonts.  In general, you cannot merge fonts of different types (eg, TrueType
   fonts and bitmapped fonts), but as a special case, this function can promote
   a monochrome bitmapped font to a color font and merge those. Example:
@longcode(#
VAR
  MyFont, MyFancyFont: AL_FONTptr;
  LowerRange, UpperRange, Capitals, TmpFont: AL_FONTptr;
  CombinedFont: AL_FONTptr;
BEGIN
...
// Create a font that contains the capitals from
// the fancy font but other characters from MyFont.
  LowerRange := al_extract_font_range (MyFont, -1, Ord ('A') - 1);
  UpperRange := al_extract_font_range (MyFont, Ord ('Z') + 1, -1);
  Capitals   := al_extract_font_range (MyFancyFont, Ord ('A'), Ord ('Z'));

  TmpFont := al_merge_fonts (LowerRange, Capitals);
  CombinedFont := al_merge_fonts (TmpFont, UpperRange);

// Clean up temporary fonts.
  al_destroy_font (LowerRange);
  al_destroy_font (UpperRange);
  al_destroy_font (Capitals);
  al_destroy_font (TmpFont);
END;
#)
  @returns(a pointer to the new font or @nil on error.  Remember that you are
    responsible for destroying the font when you are finished with it to avoid
    memory leaks.)

  @seealso(al_extract_font_range) @seealso(al_is_trans_font)
  @seealso(al_is_color_font) @seealso(al_is_mono_font)
 *)
  FUNCTION al_merge_fonts (f1, f2: AL_FONTptr): AL_FONTptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'merge_fonts';

(* Transposes all characters in a font.  Example:
VAR
  MyFont, Capitals: AL_FONTptr;
BEGIN
  ...
// Create a font of only capital letters.
  Capitals := al_extract_font_range (MyFont, Ord ('A'), Ord ('Z'));

// Now transpose the characters in the font so that they will be used
//  for the lower case letters a-z.
  al_transpose_font (Capitals, Ord ('a') - Ord ('A'));
  al_textout_ex (al_screen, Capitals, 'allcaps',
             100, 100, al_makecol (255,255,25), 0);
END;
#)
  @return(@true on success, @false on failure.)
  @seealso(al_get_font_range_begin) @seealso(al_get_font_range_end)
  @seealso(al_merge_fonts) @seealso(al_extract_font_range) *)
  FUNCTION al_transpose_font (f: AL_FONTptr; drange: AL_INT): AL_BOOL;
    INLINE;



(******************************************************************************
 * text.h
 *     Text output routines.
 *)

  VAR
  (* A simple 8x8 fixed size font (the mode 13h BIOS default).  This font
     contains the standard ASCII (U+20 to U+7F), Latin-1 (U+A1 to U+FF), and
     Latin Extended-A (U+0100 to U+017F) character ranges.
     @seealso(al_textout_ex) *)
    al_font: AL_FONTptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'font';

  (* When Allegro cannot find a glyph it needs in a font, it will instead output
     the character given in this variable.  By default, this is set to the caret
     symbol, @code(^), but you can change this global to use any other character
     instead. @seealso(al_font) *)
    al_404_char: AL_INT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_404_char';



(* Writes the string onto the bitmap at given position, using the specified
   font, foreground color and background color.  If the background color is -1,
   then the text is written transparently.  If the foreground color is -1 and a
   color font is in use, it will be drawn using the colors from the original
   font bitmap (the one you imported into the grabber program), which allows
   multicolored text output.  For high and true color fonts, the foreground
   color is ignored and always treated as -1.
   @param(bmp The output bitmap.)
   @param(f The font to render.)
   @param(str The text to draw.)
   @param(x Horizontal position.) @param(y Vertical position.)
   @param(color Foreground color.  Set to -1 to use multicolor fonts.)
   @param(bg Background color.  Set to -1 to use transparent background.)
   @seealso(al_textprintf_ex) @seealso(al_textout_centre_ex) @seealso(al_textout_right_ex)
   @seealso(al_textout_justify_ex) @seealso(al_text_height) @(al_text_length) *)
  PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST str: AL_STR; x, y, color, bg: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_ex';

(* Like @code(al_textout_ex), but interprets the @code(x) coordinate as the
   centre rather than the left edge of the string.
   @seealso(al_textout_ex) @seealso(al_textout_right_ex) @seealso(al_textout_justify_ex)*)
  PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST str: AL_STR; x, y, color, bg: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_centre_ex';

(* Like @code(al_textout_ex), but interprets the @code(x) coordinate as the
   right rather than the left edge of the string.
   @seealso(al_textout_ex) @seealso(al_textout_centre_ex) @seealso(al_textout_justify_ex)*)
  PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST str: AL_STR; x, y, color, bg: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_right_ex';

(* Draws justified text within the region @code(x1-x2).  If the amount of spare
   space is greater than the @code(diff) value, it will give up and draw
   regular left justified text instead.
   @seealso(al_textout_ex) @seealso(al_textout_centre_ex) @seealso(al_textout_right_ex)*)
  PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST str: AL_STR; x1, x2, y, diff, color, bg: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'textout_justify_ex';



(* Formatted text output, using a @code(Format) style format string. Example:
@longcode(#
  VAR
    PlayerScore: INTEGER;
  BEGIN
  ...
    al_textprintf_ex (al_screen, al_font, 10, 10, al_makecol (255, 100, 200), -1, 'Score: %d', [PlayerScore]);
  ...
  END;
  #)
  @param(bmp The output bitmap.)
  @param(f The font to render.)
  @param(x Horizontal position.) @param(y Vertical position.)
  @param(color Foreground color.  Set to -1 to use multicolor fonts.)
  @param(bg Background color.  Set to -1 to use transparent background.)
  @param(aFormat The format string.) @param(Params The list of parameters.)
  @seealso(al_textout_ex) @seealso(al_textprintf_centre_ex) @seealso(al_textprintf_right_ex) *)
PROCEDURE al_textprintf_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST x, y, color, bg: AL_INT; CONST aFormat: STRING; Params: ARRAY OF CONST);
{ TODO: Not yet supported (FPC 2.6.4)
  INLINE;
}

(* Like @code(al_textprintf_ex), but interprets the x coordinate as the centre
   rather than the left edge of the string. Example:
@longcode(#
  al_textprintf_centre_ex (al_screen, al_font, AL_SCREEN_W DIV 2, 120,
			   al_makecol (0, 100, 243), -1,
			   'Your best score so far was %d!',
			   [TotalMaxPoints]);
  #)
  @seealso(al_textout_ex) @seealso(al_textout_centre_ex)
  @seealso(al_textprintf_ex) @seealso(al_textprintf_right_ex) *)
PROCEDURE al_textprintf_centre_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST x, y, color, bg: AL_INT; CONST aFormat: STRING; Params: ARRAY OF CONST);
{ TODO: Not yet supported (FPC 2.6.4)
  INLINE;
}

(* Like @code(al_textprintf_ex), but interprets the x coordinate as the right
   rather than the left edge of the string. Example:
@longcode(#
  al_textprintf_right_ex (al_screen, al_font, AL_SCREEN_W -10, 10,
			  al_makecol (200, 200, 20), -1,
			  '%d bullets left', [PlayerAmmo]);
  #)
  @seealso(al_textout_ex) @seealso(al_textout_right_ex)
  @seealso(al_textprintf_ex) @seealso(al_textprintf_centre_ex) *)
PROCEDURE al_textprintf_right_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST x, y, color, bg: AL_INT; CONST aFormat: STRING; Params: ARRAY OF CONST);
{ TODO: Not yet supported (FPC 2.6.4)
  INLINE;
}


(* Returns the length (in pixels) of a string in the specified font.
  @seealso(al_text_height) *)
  FUNCTION al_text_length (CONST f: AL_FONTptr; CONST str: AL_STR): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'text_length';

(* Returns the height (in pixels) of the specified font.
  @seealso(al_text_length) *)
  FUNCTION al_text_height (CONST f: AL_FONTptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'text_height';

(* Frees the memory being used by a font structure.  Don't use this on the
   default global Allegro font or any text routines using it could crash.  You
   should use this only on fonts you have loaded manually after you are done
   with them, to prevent memory leaks in your program.
   @seealso(al_load_font) *)
  PROCEDURE al_destroy_font (f: AL_FONTptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_font';



(******************************************************************************
 * rle.h
 *     RLE sprites.
 *)

  TYPE
  (* Ponter to @code(AL_RLE_SPRITE). *)
    AL_RLE_SPRITEptr = ^AL_RLE_SPRITE;
  (* An RLE compressed sprite. @seealso(al_get_rle_sprite) *)
    AL_RLE_SPRITE = RECORD
      w, h: AL_INT;	 (*< width and height in pixels *)
      color_depth: AL_INT; (*< color depth of the image *)
      size: AL_INT;	 (*< size of sprite data in bytes *)
      dat: AL_POINTER;
    END;

(* Creates an RLE sprite based on the specified bitmap (which must be a memory
   bitmap).  Remember to free this RLE sprite later to avoid memory leaks.
   @param(bitmap Pointer to the bitmap used to create the sprite.)
   @returns(A pointer to the created RLE sprite, or @nil if it could not be
     created.  Remember to free this RLE sprite later to avoid memory
     leaks.)
   @seealso(al_destroy_rle_sprite) @seealso(al_draw_rle_sprite) *)
  FUNCTION al_get_rle_sprite (bitmap: AL_BITMAPptr): AL_RLE_SPRITEptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_rle_sprite';

(* Destroys an RLE sprite structure previously returned by
   @code(al_get_rle_sprite).  If you pass a @nil pointer this function won't do
   anything.  Use this once you are done with an RLE sprite to avoid memory
   leaks in your program.
   @param(sprite The RLE sprite to destroy.) *)
  PROCEDURE al_destroy_rle_sprite (sprite: AL_RLE_SPRITEptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_rle_sprite';



(******************************************************************************
 * rle.inl
 *)

(* Draws an RLE sprite onto a bitmap at the specified position.
   @param(bmp Bitmap where the sprite will be draw.)
   @param(spr Sprite to draw.)
   @param(x Horizontal coordinate.) @param(y Vertical coordinate.)
   @seealso(al_draw_sprite) @seealso(al_get_rle_sprite) *)
  PROCEDURE al_draw_rle_sprite (bmp: AL_BITMAPptr; CONST spr: AL_RLE_SPRITEptr; x, y: AL_INT);
    INLINE;

(* Translucent version of @link(al_draw_rle_sprite).  This must only be used
   after you have set up the color mapping table (for 256-color modes) or
   blender functions (for truecolor modes).  The bitmap and sprite must
   normally be in the same color depth, but as a special case you can draw
   32-bit RGBA format sprites onto any hicolor or truecolor bitmap, as long as
   you call @link(al_set_alpha_blender) first.
   @param(bmp Bitmap where the sprite will be draw.)
   @param(spr Sprite to draw.)
   @param(x Horizontal coordinate.) @param(y Vertical coordinate.)
   @seealso(al_color_table) @seealso(al_set_trans_blender) *)
  PROCEDURE al_draw_trans_rle_sprite (bmp: AL_BITMAPptr; CONST spr: AL_RLE_SPRITEptr; x, y: AL_INT);
    INLINE;

(* Tinted version of @link(al_draw_rle_sprite).  This must only be used after
   you have set up the color mapping table (for 256-color modes) or blender
   functions (for truecolor modes).
   @param(bmp Bitmap where the sprite will be draw.)
   @param(spr Sprite to draw.)
   @param(x Horizontal coordinate.) @param(y Vertical coordinate.)
   @param(color Tint color.)
   @seealso(al_color_table) *)
  PROCEDURE al_draw_lit_rle_sprite (bmp: AL_BITMAPptr; CONST spr: AL_RLE_SPRITEptr; x, y, color: AL_INT);
    INLINE;



(******************************************************************************
 * sound.h
 *     Sound support routines.
 *)

(* Call this function to specify the number of voices that are to be used by
   the digital and MIDI sound drivers respectively.  This must be done
   @bold(before) calling @link(al_install_sound).  If you reserve too many
   voices, subsequent calls to @code(al_install_sound) will fail.  How many
   voices are available depends on the driver, and in some cases you will
   actually get more than you reserve (eg. the FM synth drivers will always
   provide 9 voices on an OPL2 and 18 on an OPL3, and the SB digital driver
   will round the number of voices up to the nearest power of two).  Pass
   negative values to restore the default settings.  You should be aware that
   the sound quality is usually inversely related to how many voices you use,
   so don't reserve any more than you really need.
   @seealso(al_set_volume_per_voice) @seealso(al_get_mixer_voices) *)
  PROCEDURE al_reserve_voices (digi_voices, midi_voices: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'reserve_voices';

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
   maximum (see @link(al_set_volume)), and any other mixers such as the Volume
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
   @link(al_play_sample) or @code(al_set_volume).  It simply alters the
   overall output of the program.  If you play samples at lower volumes, or if
   they are not normalised, then you can play more of them without distortion.

   It is recommended that you hard-code the parameter into your program, rather
   than offering it to the user.  The user can alter the volume with the
   configuration file instead, or you can provide for this with
   @code(al_set_volume).
   @seealso(al_reserve_voices) @seealso(al_install_sound) *)
  PROCEDURE al_set_volume_per_voice (scale: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_volume_per_voice';



(* Initialises the sound module.  You should normally pass
   @link(AL_DIGI_AUTODETECT) and @link(AL_MIDI_AUTODETECT) as the driver
   parameters to this function, in which case Allegro will read hardware
   settings from the current configuration file.  This allows the user to
   select different values with the setup utility:  see the @code(configuration
   section) for details.

   @return(@true if the sound is successfully installed, and @false on
     failure.  If it fails it will store a description of the problem in
     @link(al_error).)
   @seealso(al_remove_sound) @seealso(al_reserve_voices)
   @seealso(al_set_volume) @seealso(al_play_sample) @seealso(al_play_midi)
   @seealso(al_set_mixer_quality) @seealso(alWin) @seealso(alUNIX) *)
  FUNCTION al_install_sound (digi, midi: AL_INT): AL_BOOL;
    INLINE;

(* Cleans up after you are finished with the sound routines.  You don't
   normally need to call this, because @link(al_exit) will do it for you.
   @seealso(al_install_sound) *)
  PROCEDURE al_remove_sound;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_sound';



(* Alters the global sound output volume.  Specify volumes for both digital
   samples and MIDI playback, as integers from 0 to 255, or pass a negative
   value to leave one of the settings unchanged.  Values bigger than 255 will
   be reduced to 255.  This routine will not alter the volume of the hardware
   mixer if it exists (i.e. only your application will be affected).
   @seealso(al_install_sound) @seealso(al_set_hardware_volume) *)
  PROCEDURE al_set_volume (digi, midi: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_volume';

(* Alters the hardware sound output volume.  Specify volumes for both digital
   samples and MIDI playback, as integers from 0 to 255, or pass a negative
   value to leave one of the settings unchanged.  Values bigger than 255 will
   be reduced to 255.  This routine will use the hardware mixer to control the
   volume if it exists (i.e. the volume of all the applications on your machine
   will be affected), otherwise do nothing.
   @seealso(al_install_sound) @seealso(al_set_volume) *)
  PROCEDURE al_set_hardware_volume (digi, midi: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_hardware_volume';



(* Retrieves the global sound output volume, both for digital samples and MIDI
   playback, as integers from 0 to 255.
   @seealso(al_set_volume) @seealso(al_get_hardware_volume) *)
  PROCEDURE al_get_volume (OUT digi, midi: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_volume';

(* Retrieves the hardware sound output volume, both for digital samples and
   MIDI playback, as integers from 0 to 255, or -1 if the information is not
   available.
   @seealso(al_set_hardware_volume) @seealso(al_get_volume) *)
  PROCEDURE al_get_hardware_volume (OUT digi, midi: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_hardware_volume';



(* Sets the resampling quality of the mixer.  Valid values are the same as the
  @code(quality) config variable.  Please read chapter "Standard config
  variables" for details.  You can call this function at any point in your
  program, even before @link(al_init). @seealso(al_get_mixer_quality) *)
  PROCEDURE al_set_mixer_quality (quality: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_mixer_quality';

(* Returns the current mixing quality, as specified by the @code(quality)
   config variable, or a previous call to @link(al_set_mixer_quality). *)
  FUNCTION al_get_mixer_quality: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_quality';

(* Returns the mixer frequency, in Hz. *)
  FUNCTION al_get_mixer_frequency: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_frequency';

(* Returns the mixer bit depth (8 or 16). *)
  FUNCTION al_get_mixer_bits: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_bits';

(* Returns the number of output channels. 2 for stereo, 1 for mono, 0 if the
   mixer isn't active. *)
  FUNCTION al_get_mixer_channels: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_channels';

(* Returns the number of voices allocated to the mixer.
   @seealso(al_reserve_voices) *)
  FUNCTION al_get_mixer_voices: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_voices';

(* Returns the number of samples per channel in the mixer buffer. *)
  FUNCTION al_get_mixer_buffer_length: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_mixer_buffer_length';



(******************************************************************************
 * midi.h
 *     MIDI music routines.
 *)

  CONST
  (* Identifier to pass to @link(al_install_sound). *)
    AL_MIDI_AUTODETECT	= -1;
  (* Identifier to pass to @link(al_install_sound). *)
    AL_MIDI_NONE		=  0;
  (* Identifier to pass to @link(al_install_sound). *)
    AL_MIDI_DIGMID	= $44494749; { AL_ID ('DIGI'); }
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
      divisions : AL_INT;	{< number of ticks per quarter note  }
    { Track data and length. }
      track : ARRAY[0..(AL_MIDI_TRACKS)-1] OF RECORD
	data : AL_UCHARptr;	{ MIDI message stream }
	len : AL_INT;		{ length of the track data }
      END;
    END;

  VAR
  (* Stores the current position (beat number) in the MIDI file, or contains a
     negative number if no music is currently playing.  Useful for synchronising
     animations with the music, and for checking whether a MIDI file has finished
     playing. @seealso(al_play_midi) @seealso(al_midi_msg_callback) *)
    al_midi_pos: AL_LONG;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_pos';
  (* Contains the position in seconds in the currently playing midi.  This is
     useful if you want to display the current song position in seconds, not as
     beat number. @seealso(al_play_midi) @seealso(al_midi_pos)
     @seealso(al_get_midi_length) *)
    al_midi_time: AL_LONG;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_time';
  (* The loop start point, set by the @link(al_play_looped_midi) function.  It
     may safely be altered while the music is playing, but you should be sure
     it's always set to sensible values (start < end).  If you are changing
     both start and end points at the same time, make sure to alter them in the
     right order in case a MIDI interrupt happens to occur in between your two
     writes!  Setting these values to -1 represents the start and end of the file
     respectively. @seealso(al_midi_loop_end) *)
    al_midi_loop_start: AL_LONG;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_loop_start';
  (* The loop end point, set by the @link(al_play_looped_midi) function.  It
     may safely be altered while the music is playing, but you should be sure
     it's always set to sensible values (start < end).  If you are changing
     both start and end points at the same time, make sure to alter them in the
     right order in case a MIDI interrupt happens to occur in between your two
     writes!  Setting these values to -1 represents the start and end of the file
     respectively. @seealso(al_midi_loop_end) *)
    al_midi_loop_end: AL_LONG;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_loop_end';



(* Detects whether the specified MIDI sound device is available.  This function
   must be called @italic(before) @code(al_install_sound).
   @Return(The maximum number of voices that the driver can provide, or zero if
     the hardware is not present.

     There are two special-case return values that you should watch out for: if
     this function returns -1 it is a note-stealing driver (eg. @code(DIGMID))
     that shares voices with the current digital sound driver, and if it
     returns @code($FFFF) it is an external device like an MPU-401 where there
     is no way to determine how many voices are available.)
     @seealso(al_install_sound) @seealso(al_reserve_voices) *)
  FUNCTION al_detect_midi_driver (driver_id: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'detect_midi_driver';

(* Loads a MIDI file (handles both format 0 and format 1).
   @returns(a pointer to a @link(AL_MIDI) structure, or @nil on error.
     Remember to free this MIDI file later to avoid memory leaks.)
   @seealso(al_destroy_midi) @seealso(al_play_midi)
   @seealso(al_get_midi_length) *)
  FUNCTION al_load_midi (CONST filename: AL_STR): AL_MIDIptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_midi';

(* Destroys a @link(AL_MIDI) structure when you are done with it.  It is safe
   to call this even when the MIDI file might be playing, because it checks and
   will kill it off if it is active.  Use this to avoid memory leaks in your
   program. @seealso(al_load_midi) *)
  PROCEDURE al_destroy_midi (midi: AL_MIDIptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_midi';

(* Starts playing the specified MIDI file, first stopping whatever music was
   previously playing.  If the @code(loop) flag is set to @true, the data will
   be repeated until replaced with something else, otherwise it will stop at
   the end of the file.  Passing a @nil pointer will stop whatever music is
   currently playing.

   @returns(@false if an error occurs @(this may happen if a patch-caching
     wavetable driver is unable to load the required samples, or at least it
     might in the future when somebody writes some patch-caching wavetable
     drivers :-@))
   @seealso(al_install_sound) @seealso(al_load_midi) @seealso(al_stop_midi)
   @seealso(al_play_looped_midi) @seealso(al_midi_pause) @seealso(al_midi_seek)
   @seealso(al_midi_pos) @seealso(al_midi_time) @seealso(al_midi_msg_callback) *)
  FUNCTION al_play_midi (midi: AL_MIDIptr; loop: AL_BOOL): AL_BOOL;
    INLINE;

(* Starts playing a MIDI file with a user-defined loop position.  When the
   player reaches the @code(loop_end) position or the end of the file
   (@code(loop_end) may be -1 to only loop at EOF), it will wind back to the
   @code(loop_start) point.  Both positions are specified in the same beat
   number format as the @link(al_midi_pos) variable.

   @returns(@false if an error occurs, @true otherwise.)
   @seealso(al_play_midi) @seealso(al_midi_pos) @seealso(al_midi_loop_start) *)
  FUNCTION al_play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: AL_INT): AL_BOOL;
    INLINE;

(* Stops whatever music is currently playing. This is the same thing as calling
   @code(al_play_midi @(@nil, @false@)).
   @seealso(al_play_midi) @seealso(al_midi_pause) *)
  PROCEDURE al_stop_midi;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stop_midi';

(* Pauses the MIDI player.
   @seealso(al_stop_midi) @seealso(al_midi_resume) @seealso(al_play_midi) *)
  PROCEDURE al_midi_pause;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_pause';

(* Resumes playback of a paused MIDI file. @seealso(al_midi_pause) *)
  PROCEDURE al_midi_resume;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_resume';

(* Seeks to the given @link(al_midi_pos) in the current MIDI file.  If the
   target is earlier in the file than the current @code(al_midi_pos) it seeks
   from the beginning; otherwise it seeks from the current position.
   @returns(zero if it could successfully seek to the requested position.
     Otherwise, a return value of 1 means it stopped playing, and
     @code(al_midi_pos) is set to the negative length of the MIDI file @(so you
     can use this function to determine the length of a MIDI file@).  A return
     value of 2 means the MIDI file looped back to the start.)
   @seealso(al_play_midi) *)
  FUNCTION al_midi_seek (target: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_seek';

(* This function will simulate playing the given MIDI, from start to end, to
   determine how long it takes to play.  After calling this function,
   @link(al_midi_pos) will contain the negative number of beats, and
   @link(al_midi_time) the length of the midi, in seconds.

   Note that any currently playing midi is stopped when you call this function.
   Usually you would call it before @link(al_play_midi), to get the length of
   the midi to be played.
   @returns(the value of al_midi_time, the length of the midi.)
   @seealso(al_load_midi) *)
  FUNCTION al_get_midi_length (midi: AL_MIDIptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_midi_length';

(* Streams a block of MIDI commands into the player in real-time, allowing you
   to trigger notes, jingles, etc, over the top of whatever MIDI file is
   currently playing.
   @seealso(al_install_sound) @seealso(al_load_midi_patches) *)
  PROCEDURE al_midi_out (data: AL_UCHARptr; length: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_out';

(* Forces the MIDI driver to load the entire set of patches ready for use.  You
   will not normally need to call this, because Allegro automatically loads
   whatever data is required for the current MIDI file, but you must call it
   before sending any program change messages via the @link(al_midi_out)
   command.
   @returns(@false if an error occurred.) @seealso(al_install_sound) *)
  FUNCTION al_load_midi_patches: AL_BOOL;
    INLINE;



  TYPE
  (* Used by @link(al_midi_msg_callback). *)
    AL_MIDI_MSG_CALLBACK_PROC = PROCEDURE (msg, b1, b2: AL_INT); CDECL;

  VAR
  (* Hook function allowing you to intercept MIDI player events.  If set to
     anything other than @nil, this routine will be called for each MIDI message.
     It will execute in an interrupt handler context, so all the code and data
     they use should be locked, and they must not call any operating system
     functions.  In general you just use these routines to set some flags and
     respond to them later in your mainline code. @seealso(al_play_midi) *)
    al_midi_msg_callback: AL_MIDI_MSG_CALLBACK_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'midi_msg_callback';



(******************************************************************************
 * digi.h
 *     Digital sound routines.
 *)

  CONST
  (* Identifier to pass to @link(al_install_sound). *)
    AL_DIGI_AUTODETECT	= -1;
  (* Identifier to pass to @link(al_install_sound). *)
    AL_DIGI_NONE	= 0;
  (* Max number of digital voices. *)
    AL_DIGI_VOICES = 64;

  TYPE
  (* Pointer to @link(AL_SAMPLE). *)
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
     signedness.
     @seealso(al_load_sample) @seealso(al_create_sample) *)
    AL_SAMPLE = RECORD
      bits : AL_INT;	 {< 8 or 16  }
      stereo : AL_INT;	 {< sample type flag  }
      freq : AL_INT;	 {< sample frequency  }
    (* It is a value from 0 to 255 (by default set to 128) and controls how
       hardware voices on the sound card are allocated if you attempt to play
       more than the driver can handle.  This may be used to ensure that the less
       important sounds are cut off while the important ones are preserved. *)
      priority : AL_INT;
      len : AL_ULONG;	 {< length (in samples)  }
      loop_start : AL_ULONG;	 {< loop start position  }
      loop_end : AL_ULONG;	 {< loop finish position  }
      param : AL_ULONG;	 {< @exclude  }
      data : AL_VOIDptr;	 {< sample data  }
    END;

  (* Used by @link(al_register_sample_file_type). *)
    AL_SAMPLE_LOAD_FUNC = FUNCTION (CONST filename: AL_STR): AL_SAMPLEptr; CDECL;
  (* Used by @link(al_register_sample_file_type).  Should return zero on
    success or non-zero otherwise. *)
    AL_SAMPLE_SAVE_FUNC = FUNCTION (CONST filename: AL_STR; spl: AL_SAMPLEptr): AL_INT; CDECL;



(* Detects whether the specified digital sound device is available.  This
  function must be called @italic(before) @code(al_install_sound).
  @return(The maximum number of voices that the driver can provide, or zero if
    the hardware is not present.)
  @seealso(al_install_sound) @seealso(al_reserve_voices) *)
  FUNCTION al_detect_digi_driver (driver_id: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'detect_digi_driver';



(* Loads a sample from a file, supporting both mono and stereo WAV and mono VOC
   files, in 8 or 16-bit formats, as well as formats handled by functions
   registered using @link(al_register_sample_file_type).

   Remember to free this sample later to avoid memory leaks.

   @returns(a pointer to the @link(AL_SAMPLE) or @nil on error.)
   @seealso(al_destroy_sample) @seealso(al_load_voc) @seealso(al_load_wav)
   @seealso(al_play_sample) @seealso(al_save_sample) *)
  FUNCTION al_load_sample (CONST filename: AL_STR): AL_SAMPLEptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_sample';

(* Loads a sample from a RIFF WAV file.

   Remember to free this sample later to avoid memory leaks.

   @returns(a pointer to the @code(AL_SAMPLE) or @nil on error.)
   @seealso(al_load_wav_pf) *)
  FUNCTION al_load_wav (CONST filename: AL_STR): AL_SAMPLEptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_wav';

(* Loads a sample from a Creative Labs VOC file.

  Remember to free this sample later to avoid memory leaks.

   @returns(a pointer to the @code(AL_SAMPLE) or @nil on error.)
   @seealso(al_load_voc_pf) *)
  FUNCTION al_load_voc (CONST filename: AL_STR): AL_SAMPLEptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_voc';

(* Writes a sample into a file.  The output format is determined from the
   @code(filename) extension.  At present Allegro does not natively support
   the writing of any sample formats, so you must register a custom saver
   routine with @link(al_register_sample_file_type).

   @returns(@true on success, @false otherwise.)
   @seealso(al_create_sample) @seealso(al_destroy_sample) *)
  FUNCTION al_save_sample (CONST filename: STRING; spl: AL_SAMPLEptr): AL_BOOL;
    INLINE;

(* Constructs a new sample structure of the specified type.

   Remember to free this sample later to avoid memory leaks. 
   @param(bits can be 8 or 16.)
   @param(stereo can be zero for mono samples and non-zero for stereo samples)
   @param(freq is the frequency in hertz)
   @param(len is the number of samples you want to allocate for the full sound
     buffer.)
   @returns(a pointer to the created sample, or @nil if the sample could not
     be created.)
   @seealso(al_load_sample) @seealso(al_destroy_sample) *)
  FUNCTION al_create_sample (bits, stereo, freq, len: AL_INT): AL_SAMPLEptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sample';

(* Destroys a sample structure when you are done with it.  It is safe to call
   this even when the sample might be playing, because it checks and will kill
   it off if it is active.  Use this to avoid memory leaks in your program.
   @seealso(al_load_sample) @seealso(al_create_sample) *)
  PROCEDURE al_destroy_sample (spl: AL_SAMPLEptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_sample';



(* Triggers a sample at the specified volume, pan position, and frequency.  The
   parameters @code(vol) and @code(pan) range from 0 (min/left) to 255
   (max/right).  Frequency is relative rather than absolute:  1000 represents
   the frequency that the sample was recorded at, 2000 is twice this, etc.  If
   @code(loop) is @true, the sample will repeat until you call
   @link(al_stop_sample), and can be manipulated while it is playing by calling
   @code(al_adjust_sample).
   @returns(the voice number that was allocated for the sample or negative if
     no voices were available.) *)
  FUNCTION al_play_sample
    (CONST spl: AL_SAMPLEptr; vol, pan, freq: AL_INT; CONST loop: AL_BOOL): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'play_sample';

(* Stop a sample from playing, which is required if you have set a sample going
   in looped mode.  If there are several copies of the sample playing, it will
   stop them all. You must still destroy the sample using
   @link(al_destroy_sample). @seealso(al_play_sample) *)
  PROCEDURE al_stop_sample (CONST spl: AL_SAMPLEptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'stop_sample';

(* Alters the parameters of a sample while it is playing (useful for
   manipulating looped sounds).  You can alter the volume, pan, and frequency,
   and can also clear the loop flag, which will stop the sample when it next
   reaches the end of its loop.  The values of the parameters are just like
   those of @link(al_play_sample).  If there are several copies of the same
   sample playing, this will adjust the first one it comes across.  If the
   sample is not playing it has no effect. *)
  PROCEDURE al_adjust_sample
    (CONST spl: AL_SAMPLEptr; vol, pan, freq: AL_INT; CONST loop: AL_BOOL);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'adjust_sample';



(* Allocates a soundcard voice and prepares it for playing the specified sample,
   setting up sensible default parameters (maximum volume, centre pan, no change
   of pitch, no looping).  When you are finished with the voice you must free it
   by calling @link(al_deallocate_voice) or @link(al_release_voice).  Allegro
   can manage up to 256 simultaneous voices, but that limit may be lower due to
   hardware reasons.
   @returns(the voice number, or -1 if no voices are available.)
   @seealso(al_reallocate_voice) @seealso(al_load_sample) *)
  FUNCTION al_allocate_voice (CONST spl: AL_SAMPLEptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allocate_voice';

(* Frees a soundcard voice, stopping it from playing and releasing whatever
   resources it is using.
   @seealso(al_allocate_voice) @seealso(al_voice_stop) *)
  PROCEDURE al_deallocate_voice (voice: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'deallocate_voice';

(* Switches an already-allocated voice to use a different sample. Calling
   @code(al_reallocate_voice @(voice, sample@)) is equivalent to:
@longcode(#
  al_deallocate_voice (Voice);
  Voice := al_allocate_voice (Sample);
#)
  @seealso(al_allocate_voice) @seealso(al_load_sample) *)
  PROCEDURE al_reallocate_voice (voice: AL_INT; CONST spl: AL_SAMPLEptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'reallocate_voice';

(* Releases a soundcard voice, indicating that you are no longer interested in
   manipulating it.  The sound will continue to play, and any resources that it
   is using will automatically be freed when it finishes.  This is essentially
   the same as @link(al_deallocate_voice), but it waits for the sound to stop
   playing before taking effect. @seealso(al_allocate_voice) *)
  PROCEDURE al_release_voice (voice: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'release_voice';

(* Activates a voice, using whatever parameters have been set for it.
  @seealso(al_allocate_voice) @seealso(al_voice_stop) *)
  PROCEDURE al_voice_start (voice: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_start';

(* Stops a voice, storing the current position and state so that it may later
   be resumed by calling @link(al_voice_start).
   @seealso(al_deallocate_voice) @seealso(al_release_voice) *)
  PROCEDURE al_voice_stop (voice: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_stop';

(* Sets the priority of a voice (range 0-255). This is used to decide which
   voices should be chopped off, if you attempt to play more than the
   soundcard driver can handle. *)
  PROCEDURE al_voice_set_priority (voice, priority: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_set_priority';

(* Checks whether a voice is currently allocated.
   @return(a pointer to the sample that the voice is using, or @nil if the
   voice is inactive @(ie. it has been deallocated@), or the
   @link(al_release_voice) function has been called and the sample has then
   finished playing.)
 @seealso(al_allocate_voice) @seealso(al_voice_start)
 @seealso(al_voice_get_position) *)
  FUNCTION al_voice_check (voice: AL_INT): AL_SAMPLEptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_check';


  CONST
  (* Plays the sample a single time. This is the default if you don't set the
    loop flag. @seealso(al_voice_set_playmode) *)
    AL_PLAYMODE_PLAY     = 0;
  (* Loops repeatedly through the sample, jumping back to the loop start
    position upon reaching the loop end. @seealso(al_voice_set_playmode) *)
    AL_PLAYMODE_LOOP     = 1;
  (* Plays the sample from beginning to end. This is the default if you don't
    set the backward flag. @seealso(al_voice_set_playmode) *)
    AL_PLAYMODE_FORWARD  = 0;
  (* Reverses the direction of the sample. If you combine this with the loop
    flag, the sample jumps to the loop end position upon reaching the loop
    start (ie. you do not need to reverse the loop start and end values when
    you play the sample in reverse).  @seealso(al_voice_set_playmode) *)
    AL_PLAYMODE_BACKWARD = 2;
  (* When used in combination with the loop flag, causes the sample to change
    direction each time it reaches one of the loop points, so it alternates
    between playing forwards and in reverse. @seealso(al_voice_set_playmode) *)
    AL_PLAYMODE_BIDIR    = 4;

(* Adjusts the loop status of the specified voice.  This can be done while the
   voice is playing, so you can start a sample in looped mode (having set the
   loop start and end positions to the appropriate values), and then clear the
   loop flag when you want to end the sound, which will cause it to continue
   past the loop end, play the subsequent part of the sample, and finish in the
   normal way. The mode parameter is a bitfield containing the following
   values: @code(AL_PLAYMODE_PLAY), @code(AL_PLAYMODE_LOOP),
   @code(AL_PLAYMODE_FORWARD), @code(AL_PLAYMODE_BACKWARD),
   @code(AL_PLAYMODE_BIDIR)
   @seealso(AL_PLAYMODE_PLAY) @seealso(AL_PLAYMODE_LOOP)
   @seealso(AL_PLAYMODE_FORWARD) @seealso(AL_PLAYMODE_BACKWARD)
   @seealso(AL_PLAYMODE_BIDIR)
 *)
  PROCEDURE al_voice_set_playmode (voice, playmode: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_set_playmode';


(* Returns the current position of a voice, in sample units, or -1 if it has
   finished playing. @seealso(al_voice_set_position) *)
  FUNCTION al_voice_get_position (voice: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_get_position';

(* Sets the position of a voice, in sample units.
  @seealso(al_voice_get_position) @seealso(al_voice_set_playmode) *)
  PROCEDURE al_voice_set_position (voice, position: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_set_position';



(* Returns the current volume of the voice, range 0-255. Otherwise it returns
   -1 if that cannot be determined (because it has finished or been preempted
   by a different sound). @seealso(al_voice_set_volume). *)
  FUNCTION al_voice_get_volume (voice: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_get_volume';

(* Sets the volume of the voice, range 0-255.
  @seealso(al_voice_get_volume) @seealso(al_voice_ramp_volume) *)
  PROCEDURE al_voice_set_volume (voice, volume: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_set_volume';

(* Starts a volume ramp (crescendo or diminuendo) from the current volume to
  the specified ending volume, lasting for time milliseconds. The volume is a
  value in the range 0-255.
  @seealso(al_voice_set_volume) @seealso(al_voice_stop_volumeramp) *)
  PROCEDURE al_voice_ramp_volume (voice, tyme, endvol: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_ramp_volume';

(* Interrupts a volume ramp operation. @seealso(al_voice_ramp_volume) *)
  PROCEDURE al_voice_stop_volumeramp (voice: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_stop_volumeramp';



(* Returns the current pitch of the voice, in Hz.
  @seealso(al_voice_set_frequency) *)
  FUNCTION al_voice_get_frequency (voice: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_get_frequency';

(* Sets the pitch of the voice, in Hz.
  @seealso(al_voice_get_frequency) @seealso(al_voice_sweep_frequency) *)
  PROCEDURE al_voice_set_frequency (voice, frequency: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_set_frequency';

(* Starts a frequency sweep (glissando) from the current pitch to the specified
  ending pitch, lasting for time milliseconds.
  @seealso(al_voice_set_frequency) @seealso(al_voice_stop_frequency_sweep) *)
  PROCEDURE al_voice_sweep_frequency (voice, tyme, endfreq: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_sweep_frequency';

(* Interrupts a frequency sweep operation. @seealso(al_voice_sweep_frequency) *)
  PROCEDURE al_voice_stop_frequency_sweep (voice: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_stop_frequency_sweep';



(* Returns the current pan position, from 0 (left) to 255 (right).
  @seealso(al_voice_set_pan) *)
  FUNCTION al_voice_get_pan (voice: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_get_pan';

(* Sets the pan position, ranging from 0 (left) to 255 (right).
  @seealso(al_voice_get_pan) @seealso(al_voice_sweep_pan) *)
  PROCEDURE al_voice_set_pan (voice, pan: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_set_pan';

(* Starts a pan sweep (left <-> right movement) from the current position to
  the specified ending position, lasting for time milliseconds.
  @seealso(al_voice_stop_pan_sweep) @seealso(al_voice_set_pan) *)
  PROCEDURE al_voice_sweep_pan (voice, tyme, endpan: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_sweep_pan';

(* Interrupts a pan sweep operation. @seealso(al_voice_sweep_pan) *)
  PROCEDURE al_voice_stop_pan_sweep (voice: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_stop_pan_sweep';



(* Sets the echo parameters for a voice (not currently implemented). *)
  PROCEDURE al_voice_set_echo (voice, strength, delay: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_set_echo';

(* Sets the tremolo parameters for a voice (not currently implemented). *)
  PROCEDURE al_voice_set_tremolo (voice, rate, depth: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_set_tremolo';

(* Sets the vibrato parameters for a voice (not currently implemented). *)
  PROCEDURE al_voice_set_vibrato (voice, rate, depth: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'voice_set_vibrato';



(* Informs the @link(al_load_sample) and the @link(al_save_sample) functions
   of a new sample file type, providing routines to read and write samples in
   this format (either function may be @nil).  Example:
   @longcode(#
   FUNCTION LoadMP3 (CONST filename: AL_CHARptr): AL_SAMPLEptr; CDECL;
   BEGIN
     ...
   END;

   ...

   al_register_sample_file_type ('mp3', @LoadMPT, NIL);
   #) *)
  PROCEDURE al_register_sample_file_type (CONST ext: AL_STR; load: AL_SAMPLE_LOAD_FUNC; save: AL_SAMPLE_SAVE_FUNC);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'register_sample_file_type';

IMPLEMENTATION

  USES
    sysutils,
  (* Includes the system driver description. *)
    alDrv;

  VAR
  (* To be used as "errnum". *)
    NumError: AL_INT;



(******************************************************************************
 * base.h
 *)

(* Converts four 8 bit values to a packed 32 bit integer ID. *)
  FUNCTION AL_ID (str: SHORTSTRING): AL_INT;
  BEGIN
    AL_ID := (ORD (str[1]) SHL 24) OR (ORD (str[2]) SHL 16)
	     OR (ORD (str[3]) SHL  8) OR  ORD (str[4]);
  END;




(******************************************************************************
 * system.h
 *)

  VAR
  (* To access to stytem drivers. *)
    system_driver: __AL_SYSTEM_DRIVER__PTR; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

(* Function for internal use. *)
  FUNCTION _install_allegro_version_check (
	system_id: AL_INT; errno_ptr: AL_INTptr; atexit_ptr: AL_POINTER;
	version: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;



(* Initialises the Allegro library. *)
  FUNCTION al_install (system_id: AL_INT): AL_BOOL;
  BEGIN
    RESULT := NOT _install_allegro_version_check (
	system_id, @NumError, NIL,
	(AL_VERSION SHL 16) OR (AL_SUB_VERSION SHL 8) OR 0
    )
  END;



(* Initialises the Allegro library. *)
  FUNCTION al_init: AL_BOOL;
  BEGIN
    RESULT := NOT _install_allegro_version_check (
	AL_SYSTEM_AUTODETECT, @NumError, NIL,
	(AL_VERSION SHL 16) OR (AL_SUB_VERSION SHL 8) OR 0
    )
  END;



(* Closes down the Allegro system. *)
{ This is the old way "al_exit" worked.  This block should be removed
  after beta version, or restored if there's any problem (and document
  that problem, since today I don't remember if this was a workaround
  to fix something that I've forgot.

  IsAllegroUp was set by the initialization functions.

  PROCEDURE allegro_exit; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_exit';

  PROCEDURE al_exit;
  BEGIN
    IF IsAllegroUp THEN
    BEGIN
      IsAllegroUp := FALSE;
      allegro_exit;
    END;
  END;
  }


(* Function for close button handler. *)
  FUNCTION _set_close_button_callback (proc: AL_SIMPLE_PROC): AL_BOOL; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_close_button_callback';

(* On platforms that have a close button, this routine installs a callback
   function to handle the close event. *)
  FUNCTION al_set_close_button_callback (proc: AL_SIMPLE_PROC): AL_BOOL;
  BEGIN
    RESULT := NOT _set_close_button_callback (proc)
  END;

(*
 * system.inl
 *)

(* This routine alters the window title. *)
  PROCEDURE al_set_window_title (CONST title: AL_STR);
  BEGIN
    IF system_driver^.set_window_title <> NIL THEN
      system_driver^.set_window_title (title)
  END;



(* Finds out the currently selected desktop color depth. *)
  FUNCTION al_desktop_color_depth: AL_INT;
  BEGIN
    IF system_driver^.desktop_color_depth <> NIL THEN
      RESULT := (system_driver^.desktop_color_depth ())
    ELSE
      RESULT := 0
  END;



(* Finds out the currently selected desktop resolution. *)
  FUNCTION al_get_desktop_resolution (OUT w, h: AL_INT): AL_BOOL;
  BEGIN
    IF system_driver^.get_desktop_resolution <> NIL THEN
      RESULT := NOT system_driver^.get_desktop_resolution (w, h)
    ELSE
      RESULT := FALSE
  END;



(******************************************************************************
 * config.h
 *)

  PROCEDURE set_config_file (CONST filename: AL_STRptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_file';

  PROCEDURE al_set_config_file (CONST filename: AL_STR);
  BEGIN
    IF filename <> '' THEN
      set_config_file (AL_STRptr (filename))
    ELSE
      set_config_file (NIL)
  END;



  PROCEDURE override_config_file (filename: AL_STRptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'override_config_file';

  PROCEDURE al_override_config_file (CONST filename: AL_STR);
  BEGIN
    IF filename <> '' THEN
      override_config_file (AL_STRptr (filename))
    ELSE
      override_config_file (NIL)
  END;



(******************************************************************************
 * timer.h
 *)

  FUNCTION install_timer: AL_BOOL; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_timer: AL_BOOL;
  BEGIN
    RESULT := NOT install_timer
  END;



  FUNCTION install_int_ex (proc: AL_SIMPLE_PROC; speed: AL_LONG): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_int_ex (proc: AL_SIMPLE_PROC; speed: AL_LONG): AL_BOOL;
  BEGIN
    RESULT := NOT install_int_ex (proc, speed)
  END;



  FUNCTION install_int (proc: AL_SIMPLE_PROC; speed: AL_LONG): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_int (proc: AL_SIMPLE_PROC; speed: AL_LONG): AL_BOOL;
  BEGIN
    RESULT := NOT install_int (proc, speed)
  END;



  FUNCTION install_param_int_ex
    (proc: AL_PARAM_PROC; param: AL_VOIDptr; speed: AL_LONG): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_param_int_ex
    (proc: AL_PARAM_PROC; param: AL_VOIDptr; speed: AL_LONG): AL_BOOL;
  BEGIN
    RESULT := NOT install_param_int_ex (proc, param, speed)
  END;



  FUNCTION install_param_int
    (proc: AL_PARAM_PROC; param: AL_VOIDptr; speed: AL_LONG): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_param_int
    (proc: AL_PARAM_PROC; param: AL_VOIDptr; speed: AL_LONG): AL_BOOL;
  BEGIN
    RESULT := NOT install_param_int (proc, param, speed)
  END;



  CONST
    AL_TIMERS_PER_SECOND = 1193181;

  FUNCTION AL_SECS_TO_TIMER (CONST x: AL_LONG): AL_LONG;
  BEGIN
    AL_SECS_TO_TIMER := x * AL_TIMERS_PER_SECOND;
  END;

  FUNCTION AL_MSEC_TO_TIMER (CONST x: AL_LONG): AL_LONG;
  BEGIN
    AL_MSEC_TO_TIMER := x * (AL_TIMERS_PER_SECOND DIV 1000);
  END;

  FUNCTION AL_BPS_TO_TIMER  (CONST x: AL_LONG): AL_LONG;
  BEGIN
    AL_BPS_TO_TIMER := AL_TIMERS_PER_SECOND DIV x;
  END;

  FUNCTION AL_BPM_TO_TIMER  (CONST x: AL_LONG): AL_LONG;
  BEGIN
    AL_BPM_TO_TIMER := (60 * AL_TIMERS_PER_SECOND) DIV x;
  END;



(******************************************************************************
 * keyboard.h
 *)

  FUNCTION install_keyboard: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_keyboard: AL_BOOL;
  BEGIN
    RESULT := NOT install_keyboard;
  END;



(******************************************************************************
 * joystick.h
 *)

  FUNCTION install_joystick (atype: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_joystick (CONST atype: AL_INT): AL_BOOL;
  BEGIN
    RESULT := NOT install_joystick (atype)
  END;



  FUNCTION calibrate_joystick (n: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_calibrate_joystick (n: AL_INT): AL_BOOL;
  BEGIN
    RESULT := NOT calibrate_joystick (n)
  END;



  FUNCTION save_joystick_data (filename: AL_STR): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_save_joystick_data (CONST filename: AL_STR): AL_BOOL;
  BEGIN
    RESULT := NOT save_joystick_data (filename)
  END;



  FUNCTION load_joystick_data (filename: AL_STRptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_joystick_data (CONST filename: AL_STR): AL_BOOL;
  BEGIN
    IF filename <> '' THEN
      RESULT :=  NOT load_joystick_data (AL_STRptr (filename))
    ELSE
      RESULT := NOT load_joystick_data (NIL)
  END;



(******************************************************************************
 * gfx.h
 *)

CONST
(* Identify bitmap type *)
  AL_BMP_ID_VIDEO     = $80000000;
  AL_BMP_ID_SYSTEM    = $40000000;
  AL_BMP_ID_SUB       = $20000000;
{ Unused.
  AL_BMP_ID_PLANAR    = $10000000;
  AL_BMP_ID_NOBLIT    = $08000000;
  AL_BMP_ID_LOCKED    = $04000000;
  AL_BMP_ID_AUTOLOCK  = $02000000;
}
  AL_BMP_ID_MASK      = $01FFFFFF;



  FUNCTION set_gfx_mode (card, w, h, v_w, v_h: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_set_gfx_mode (card, w, h, v_w, v_h: AL_INT): AL_BOOL;
  BEGIN
    IF set_gfx_mode (card, w, h, v_w, v_h) THEN EXIT (FALSE);
    IF al_screen <> NIL THEN
    BEGIN
      AL_SCREEN_W := w;
      AL_SCREEN_H := h;
      AL_VIRTUAL_W := al_screen^.w;
      AL_VIRTUAL_H := al_screen^.h
    END;
    RESULT := TRUE
  END;



  FUNCTION enable_triple_buffer: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_enable_triple_buffer: AL_BOOL;
  BEGIN
    RESULT := NOT enable_triple_buffer
  END;


(* Sets how the program should handle being switched into the background. *)
  FUNCTION set_display_switch_mode (mode: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_set_display_switch_mode (mode: AL_INT): AL_BOOL;
  BEGIN
    RESULT := NOT set_display_switch_mode (mode)
  END;



(* Installs a notification callback for the switching mode. *)
  FUNCTION set_display_switch_callback (dir: AL_INT; cb: AL_SIMPLE_PROC): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_set_display_switch_callback (dir: AL_INT; cb: AL_SIMPLE_PROC): AL_BOOL;
  BEGIN
    RESULT := NOT set_display_switch_callback (dir, cb)
  END;



(******************************************************************************
 * gfx.inl
 *)

(* Tells if you are running in windowed mode. *)
  FUNCTION al_is_windowed_mode: AL_BOOL;
  BEGIN
    RESULT := al_gfx_driver^.windowed
  END;



(* Clears the bitmap to the specified color. @seealso(al_clear_bitmap)*)
  PROCEDURE al_clear_to_color (bitmap: AL_BITMAPptr; CONST color: AL_INT);
  BEGIN
    bitmap^.vtable^.clear_to_color (bitmap, color);
  END;



  FUNCTION al_bitmap_color_depth (CONST bmp: AL_BITMAPptr): AL_INT;
  BEGIN
    RESULT := bmp^.vtable^.color_depth
  END;



  FUNCTION al_bitmap_mask_color (CONST bmp: AL_BITMAPptr): AL_INT;
  BEGIN
    RESULT := bmp^.vtable^.mask_color
  END;



  FUNCTION al_is_same_bitmap (CONST bmp1, bmp2: AL_BITMAPptr): AL_BOOL;
  VAR
    m1, m2: AL_ULONG;
  BEGIN
    IF (bmp1 = NIL) OR (bmp2 = NIL) THEN EXIT (FALSE);
    IF bmp1 = bmp2 THEN EXIT (TRUE);
    m1 := (bmp1^.id AND AL_BMP_ID_MASK);
    m2 := (bmp2^.id AND AL_BMP_ID_MASK);
    RESULT := ((m1 <> 0) AND (m1 = m2))
  END;



  FUNCTION al_is_memory_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
  BEGIN
    RESULT := (bmp^.id AND (AL_BMP_ID_VIDEO OR AL_BMP_ID_SYSTEM)) = 0
  END;



  FUNCTION al_is_screen_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
  BEGIN
    RESULT := al_is_same_bitmap (bmp, al_screen)
  END;



  FUNCTION al_is_video_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
  BEGIN
    RESULT := (bmp^.id AND AL_BMP_ID_VIDEO) <> 0
  END;



  FUNCTION al_is_system_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
  BEGIN
    RESULT := (bmp^.id AND AL_BMP_ID_SYSTEM) <> 0
  END;



  FUNCTION al_is_sub_bitmap (CONST bmp: AL_BITMAPptr): AL_BOOL;
  BEGIN
    RESULT := (bmp^.id AND AL_BMP_ID_SUB) <> 0
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

  PROCEDURE al_get_clip_rect (bmp: AL_BITMAPptr; VAR x1, y1, x2, y2: AL_INT);
  BEGIN
    x1 := bmp^.cl;
    y1 := bmp^.ct;
    x2 := bmp^.cr-1;
    y2 := bmp^.cb-1;
  END;



  PROCEDURE al_set_clip_state (bmp: AL_BITMAPptr; state: AL_BOOL);
  BEGIN
    bmp^.clip := state
  END;



  FUNCTION al_get_clip_state (bmp: AL_BITMAPptr): AL_BOOL;
  BEGIN
    RESULT := bmp^.clip
  END;



(******************************************************************************
 * datafile.h
 *)

  FUNCTION save_bitmap (filename: AL_STR; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_save_bitmap (filename: STRING; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): AL_BOOL;
  BEGIN
    RESULT := NOT save_bitmap (filename, bmp, pal)
  END;

  FUNCTION save_bmp (filename: AL_STR; bmp: AL_BITMAPptr; palette: AL_PALETTEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_save_bmp (filename: STRING; bmp: AL_BITMAPptr; palette: AL_PALETTEptr): AL_BOOL;
  BEGIN
    RESULT := NOT save_bmp (filename, bmp, palette)
  END;

  FUNCTION save_pcx (filename: AL_STR; bmp: AL_BITMAPptr; palette: AL_PALETTEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_save_pcx (filename: STRING; bmp: AL_BITMAPptr; palette: AL_PALETTEptr): AL_BOOL;
  BEGIN
    RESULT := NOT save_pcx (filename, bmp, palette)
  END;

  FUNCTION save_tga (filename: AL_STR; bmp: AL_BITMAPptr; palette: AL_PALETTEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_save_tga (filename: STRING; bmp: AL_BITMAPptr; palette: AL_PALETTEptr): AL_BOOL;
  BEGIN
    RESULT := NOT save_tga (filename, bmp, palette)
  END;



(******************************************************************************
 * mouse.h
 *)

  FUNCTION show_os_cursor (cursor: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_show_os_cursor (cursor: AL_INT): AL_BOOL;
  BEGIn
    RESULT := NOT show_os_cursor (cursor)
  END;



(******************************************************************************
 * draw.inl
 *)

  FUNCTION  al_getpixel (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
  BEGIN
    al_getpixel := bmp^.vtable^.getpixel (bmp, x, y);
  END;

  PROCEDURE al_putpixel (bmp: AL_BITMAPptr; x, y, color: AL_INT);
  BEGIN
    bmp^.vtable^.putpixel (bmp, x, y, color);
  END;


  PROCEDURE al_vline (bmp: AL_BITMAPptr; x, y1, y2, color: AL_INT);
  BEGIN
    bmp^.vtable^.vline (bmp, x, y1, y2, color);
  END;

  PROCEDURE al_hline (bmp: AL_BITMAPptr; x1, y, x2, color: AL_INT);
  BEGIN
    bmp^.vtable^.hline (bmp, x1, y, x2, color);
  END;

  PROCEDURE al_line (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
  BEGIN
    bmp^.vtable^.line (bmp, x1, y1, x2, y2, color);
  END;

  PROCEDURE al_fastline (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
  BEGIN
    bmp^.vtable^.fastline (bmp, x1, y1, x2, y2, color);
  END;

  PROCEDURE al_rectfill (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
  BEGIN
    bmp^.vtable^.rectfill (bmp, x1, y1, x2, y2, color);
  END;

  PROCEDURE al_triangle (bmp: AL_BITMAPptr; x1, y1, x2, y2, x3, y3, color: AL_INT);
  BEGIN
    bmp^.vtable^.triangle (bmp, x1, y1, x2, y2, x3, y3, color);
  END;

  PROCEDURE al_polygon (bmp: AL_BITMAPptr; vertices: AL_INT; CONST points: ARRAY OF AL_INT; color: AL_INT);
  BEGIN
    bmp^.vtable^.polygon (bmp, vertices, @points[0], color);
  END;

  PROCEDURE al_rect (bmp: AL_BITMAPptr; x1, y1, x2, y2, color: AL_INT);
  BEGIN
    bmp^.vtable^.rect (bmp, x1, y1, x2, y2, color);
  END;

  PROCEDURE al_circle (bmp: AL_BITMAPptr; x, y, r, color: AL_INT);
  BEGIN
    bmp^.vtable^.circle (bmp, x, y, r, color);
  END;

  PROCEDURE al_circlefill (bmp: AL_BITMAPptr; x, y, r, color: AL_INT);
  BEGIN
    bmp^.vtable^.circlefill (bmp, x, y, r, color);
  END;

  PROCEDURE al_ellipse (bmp: AL_BITMAPptr; x, y, rx, ry, color: AL_INT);
  BEGIN
    bmp^.vtable^.ellipse (bmp, x, y, rx, ry, color);
  END;

  PROCEDURE al_ellipsefill (bmp: AL_BITMAPptr; x, y, rx, ry, color: AL_INT);
  BEGIN
    bmp^.vtable^.ellipsefill (bmp, x, y, rx, ry, color);
  END;

  PROCEDURE al_arc (bmp: AL_BITMAPptr; x, y: AL_INT; ang1, ang2: AL_FIXED; r, color: AL_INT);
  BEGIN
    bmp^.vtable^.arc (bmp, x, y, ang1, ang2, r, color);
  END;

  PROCEDURE al_spline (bmp: AL_BITMAPptr; CONST points: ARRAY OF AL_INT; color: AL_INT);
  BEGIN
    bmp^.vtable^.spline (bmp, @points[0], color);
  END;

  PROCEDURE al_floodfill (bmp: AL_BITMAPptr; x, y, color: AL_INT);
  BEGIN
    bmp^.vtable^.floodfill (bmp, x, y, color);
  END;



  PROCEDURE al_draw_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  BEGIN
    IF sprite^.vtable^.color_depth = 8 THEN
      bmp^.vtable^.draw_256_sprite (bmp, sprite, x, y)
    ELSE
      bmp^.vtable^.draw_sprite (bmp, sprite, x, y);
  END;

  PROCEDURE al_draw_sprite_ex (bmp, sprite: AL_BITMAPptr; x, y, mode, flip: AL_INT);
  BEGIN
    bmp^.vtable^.draw_sprite_ex (bmp, sprite, x, y, mode, flip);
  END;

  PROCEDURE al_draw_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  BEGIN
    bmp^.vtable^.draw_sprite_v_flip (bmp, sprite, x, y);
  END;

  PROCEDURE al_draw_sprite_h_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  BEGIN
    bmp^.vtable^.draw_sprite_h_flip (bmp, sprite, x, y);
  END;

  PROCEDURE al_draw_sprite_vh_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  BEGIN
    bmp^.vtable^.draw_sprite_vh_flip (bmp, sprite, x, y);
  END;

  PROCEDURE al_draw_trans_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT);
  BEGIN
    IF sprite^.vtable^.color_depth = 32 THEN
      bmp^.vtable^.draw_trans_rgba_sprite (bmp, sprite, x, y)
    ELSE
      bmp^.vtable^.draw_trans_sprite (bmp, sprite, x, y);
  END;

  PROCEDURE al_draw_lit_sprite (bmp, sprite: AL_BITMAPptr; x, y, c: AL_INT);
  BEGIN
    bmp^.vtable^.draw_lit_sprite (bmp, sprite, x, y, c);
  END;

  PROCEDURE al_draw_gouraud_sprite (bmp, sprite: AL_BITMAPptr; x, y, c1, c2, c3, c4: AL_INT);
  BEGIN
    bmp^.vtable^.draw_gouraud_sprite (bmp, sprite, x, y, c1, c2, c3, c4);
  END;

  PROCEDURE al_rotate_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
  BEGIN
    bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * $10000) DIV 2,
							(y SHL 16) + (sprite^.h * $10000) DIV 2,
							sprite^.w SHL 15, sprite^.h SHL 15,
							angle, $10000, 0);
  END;

  PROCEDURE al_rotate_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle: AL_FIXED);
  BEGIN
    bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * $10000) DIV 2,
							(y SHL 16) + (sprite^.h * $10000) DIV 2,
							sprite^.w SHL 15, sprite^.h SHL 15,
							angle, $10000, NOT 0);
  END;

  PROCEDURE al_rotate_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);
  BEGIN
    bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * scale) DIV 2,
							(y SHL 16) + (sprite^.h * scale) DIV 2,
							sprite^.w SHL 15, sprite^.h SHL 15,
							angle, scale, 0);
  END;

  PROCEDURE al_rotate_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y: AL_INT; angle, scale: AL_FIXED);
  BEGIN
    bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, (x SHL 16) + (sprite^.w * scale) DIV 2,
							(y SHL 16) + (sprite^.h * scale) DIV 2,
							sprite^.w SHL 15, sprite^.h SHL 15,
							angle, scale, NOT 0);
  END;

  PROCEDURE al_pivot_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
  BEGIN
    bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
							cx SHL 16, cy SHL 16,
							angle, $10000, 0);
  END;

  PROCEDURE al_pivot_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle: AL_FIXED);
  BEGIN
    bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
							cx SHL 16, cy SHL 16,
							angle, $10000, NOT 0);
  END;

  PROCEDURE al_pivot_scaled_sprite (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);
  BEGIN
    bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
							cx SHL 16, cy SHL 16,
							angle, scale, 0);
  END;

  PROCEDURE al_pivot_scaled_sprite_v_flip (bmp, sprite: AL_BITMAPptr; x, y, cx, cy: AL_INT; angle, scale: AL_FIXED);
  BEGIN
    bmp^.vtable^.pivot_scaled_sprite_flip (bmp, sprite, x SHL 16, y SHL 16,
							cx SHL 16, cy SHL 16,
							angle, scale, NOT 0);
  END;



  PROCEDURE _al_putpixel (bmp: AL_BITMAPptr; x, y, color: AL_INT);
  BEGIN
    (AL_UINT8ptr (bmp^.write_bank (bmp, y)) + x)^ := color;
    bmp^.vtable^.unwrite_bank (bmp)
  END;



  FUNCTION _al_getpixel (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
  BEGIN
    RESULT := (AL_UINT8ptr (bmp^.read_bank (bmp, y)) + x)^;
    bmp^.vtable^.unwrite_bank (bmp)
  END;



  PROCEDURE _al_putpixel15 (bmp: AL_BITMAPptr; x, y, color: AL_INT);
  BEGIN
    (AL_UINT16ptr (bmp^.write_bank (bmp, y)) + x)^ := color;
    bmp^.vtable^.unwrite_bank (bmp);
  END;



  FUNCTION _al_getpixel15 (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
  BEGIN
    RESULT := (AL_UINT16ptr(bmp^.read_bank (bmp, y)) + x)^;
    bmp^.vtable^.unwrite_bank (bmp);
  END;



  PROCEDURE _al_putpixel16 (bmp: AL_BITMAPptr; x, y, color: AL_INT);
  BEGIN
    (AL_UINT16ptr (bmp^.write_bank (bmp, y)) + x)^ := color;
    bmp^.vtable^.unwrite_bank (bmp);
  END;



  FUNCTION _al_getpixel16 (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
  BEGIN
    RESULT := (AL_UINT16ptr(bmp^.read_bank (bmp, y)) + x)^;
    bmp^.vtable^.unwrite_bank (bmp);
  END;



  PROCEDURE _al_putpixel24 (bmp: AL_BITMAPptr; x, y, color: AL_INT);
  VAR
    addr: AL_UCHARptr;
  BEGIN
    addr := bmp^.write_bank (bmp, y);
{$IFDEF ENDIAN_BIG}
    (addr + (x * 3)    )^ := (color SHR 16) AND $FF;
    (addr + (x * 3) + 1)^ := (color SHR  8) AND $FF;
    (addr + (x * 3) + 2)^ :=  color         AND $FF;
{$ELSE}
    (addr + (x * 3)    )^ :=  color         AND $FF;
    (addr + (x * 3) + 1)^ := (color SHR  8) AND $FF;
    (addr + (x * 3) + 2)^ := (color SHR 16) AND $FF;
{$ENDIF}
    bmp^.vtable^.unwrite_bank (bmp);
  END;



  FUNCTION _al_getpixel24 (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
  VAR
    addr: AL_UCHARptr;
  BEGIN
    addr := bmp^.read_bank (bmp, y);
    RESULT :=
    {$IFDEF ENDIAN_BIG}
	   ((addr + (x * 3)    )^ SHL 16)
	OR ((addr + (x * 3) + 1)^ SHL  8)
	OR ((addr + (x * 3) + 2)^       )
    {$ELSE}
	   ((addr + (x * 3)    )^       )
	OR ((addr + (x * 3) + 1)^ SHL  8)
	OR ((addr + (x * 3) + 2)^ SHL 16)
    {$ENDIF}
    ;
    bmp^.vtable^.unwrite_bank (bmp);
  END;



  PROCEDURE _al_putpixel32 (bmp: AL_BITMAPptr; x, y, color: AL_INT);
  BEGIN
    (AL_UINT32ptr (bmp^.write_bank (bmp, y)) + x)^ := color;
    bmp^.vtable^.unwrite_bank (bmp);
  END;



  FUNCTION _al_getpixel32 (bmp: AL_BITMAPptr; x, y: AL_INT): AL_INT;
  BEGIN
    RESULT := (AL_UINT32ptr (bmp^.read_bank (bmp, y)) + x)^;
    bmp^.vtable^.unwrite_bank (bmp);
  END;



(******************************************************************************
 * font.h
 *)

  FUNCTION transpose_font (f: AL_FONTptr; drange: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_transpose_font (f: AL_FONTptr; drange: AL_INT): AL_BOOL;
  BEGIN
    RESULT := NOT transpose_font (f, drange)
  END;



(******************************************************************************
 * text.h
 *)

  PROCEDURE al_textprintf_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST x, y, color, bg: AL_INT; CONST aFormat: STRING; Params: ARRAY OF CONST);
  BEGIN
    al_textout_ex (bmp, f, Format (aformat, Params), x, y, color, bg)
  END;

  PROCEDURE al_textprintf_centre_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST x, y, color, bg: AL_INT; CONST aFormat: STRING; Params: ARRAY OF CONST);
  BEGIN
    al_textout_centre_ex (bmp, f, Format (aformat, Params), x, y, color, bg)
  END;

  PROCEDURE al_textprintf_right_ex (bmp: AL_BITMAPptr; CONST f: AL_FONTptr; CONST x, y, color, bg: AL_INT; CONST aFormat: STRING; Params: ARRAY OF CONST);
  BEGIN
    al_textout_right_ex (bmp, f, Format (aformat, Params), x, y, color, bg)
  END;



(******************************************************************************
 * rle.inl
 *)

  PROCEDURE al_draw_rle_sprite (bmp: AL_BITMAPptr; CONST spr: AL_RLE_SPRITEptr; x, y: AL_INT);
  BEGIN
    bmp^.vtable^.draw_rle_sprite (bmp, spr, x, y);
  END;

  PROCEDURE al_draw_trans_rle_sprite (bmp: AL_BITMAPptr; CONST spr: AL_RLE_SPRITEptr; x, y: AL_INT);
  BEGIN
    IF spr^.color_depth = 32 THEN
      bmp^.vtable^.draw_trans_rgba_rle_sprite (bmp, spr, x, y)
    ELSE
      bmp^.vtable^.draw_trans_rle_sprite (bmp, spr, x, y)
  END;

  PROCEDURE al_draw_lit_rle_sprite (bmp: AL_BITMAPptr; CONST spr: AL_RLE_SPRITEptr; x, y, color: AL_INT);
  BEGIN
    bmp^.vtable^.draw_lit_rle_sprite (bmp, spr, x, y, color);
  END;



(******************************************************************************
 * sound.h
 *)

  FUNCTION install_sound (digi, midi: AL_INT; CONST c: AL_STRptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_install_sound (digi, midi: AL_INT): AL_BOOL;
  BEGIN
    RESULT := NOT install_sound (digi, midi, NIL)
  END;



(******************************************************************************
 * midi.h
 *)

  FUNCTION play_midi (midi: AL_MIDIptr; loop: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_play_midi (midi: AL_MIDIptr; loop: AL_BOOL): AL_BOOL;
  BEGIN
    RESULT := NOT play_midi (midi, loop)
  END;



  FUNCTION play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_play_looped_midi (midi: AL_MIDIptr; loop_start, loop_end: AL_INT): AL_BOOL;
  BEGIN
    RESULT := NOT play_looped_midi (midi, loop_start, loop_end)
  END;



  FUNCTION load_midi_patches: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_load_midi_patches: AL_BOOL;
  BEGIN
    RESULT := NOT load_midi_patches
  END;



(******************************************************************************
 * digi.h
 *)

  FUNCTION save_sample (CONST filename: AL_STR; spl: AL_SAMPLEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_save_sample (CONST filename: STRING; spl: AL_SAMPLEptr): AL_BOOL;
  BEGIN
    RESULT := NOT save_sample (filename, spl)
  END;

INITIALIZATION

FINALIZATION
{ To be sure it closes down Allegro. }
  al_exit;
END.
