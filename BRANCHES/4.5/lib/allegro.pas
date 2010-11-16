UNIT allegro;
(*<Initialization, finalization, error handling and basic configuration of the
  Allegro library. *)

{$include _alcfg.inc }

INTERFACE

(***************
 * Core system *
 ***************
 * Defines a collection of procedures, identificators and variables that allows
 * to comunicate with the core system.  That is, error handling, initialization
 * and configuration of the base system. *)

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
  FUNCTION AL_ID (str: SHORTSTRING): LONGINT; INLINE;



CONST
(* Tells to @link(al_install) that must autodetect the system driver.
   @seealso(AL_SYSTEM_NONE) *)
  AL_SYSTEM_AUTODETECT	= 0;
(* Tells to @link(al_install) that must install a stripped down version of
   Allegro that won't even try to touch the hardware or do anything platform
   specific:  this can be useful for situations where you only want to
   manipulate memory bitmaps, such as the text mode datafile tools or the
   Windows GDI interfacing functions.

   Is equivalent to @code(AL_ID @('NONE'@);).
   @seealso(AL_SYSTEM_AUTODETECT) *)
  AL_SYSTEM_NONE	= $4E4F4E45;


VAR
(* Stores an error message.  You can use it to show the user what's wrong. *)
  al_error: STRING;



(* Initialises the Allegro library.  You must call either this or
   @link(al_init) before doing anything other.  The functions that can be
   called before this one will be marked explicitly in the documentation,
   like @link(al_set_config_file).

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
   @seealso(al_exit) *)
  FUNCTION al_install (system_id: LONGINT): BOOLEAN;

(* Function which initialises the Allegro library.  This is the same thing as
   calling @code(al_install @(AL_SYSTEM_AUTODETECT@)).
   @returns(@true on success or @false on failure @(e.g. no system driver
     could be used@).)
   @seealso(al_install) @seealso(al_exit) *)
  FUNCTION al_init: BOOLEAN; INLINE;

(* Closes down the Allegro system.  This includes returning the system to text
   mode and removing whatever mouse, keyboard, and timer routines have been
   installed.  You don't normally need to bother making an explicit call to
   this function, because the @code(allegro) unit does call it at the
   @code(FINALIZATION) section so it will be called automatically when your
   program exits.

   Note that after you call this function, other functions like
   al_destroy_bitmap will most likely crash.
   @seealso(al_install) *)
  PROCEDURE al_exit;

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

IMPLEMENTATION

USES
  almodule, alsysdrv;

VAR
{ To know if system is up and needs to be finalised. }
  IsAllegroInstalled: BOOLEAN;



(* Converts four 8 bit values to a packed 32 bit integer ID. *)
  FUNCTION AL_ID (str: SHORTSTRING): LONGINT;
  BEGIN
    AL_ID := (ORD (str[1]) SHL 24) OR (ORD (str[2]) SHL 16)
	OR (ORD (str[3]) SHL  8) OR  ORD (str[4]);
  END;



(* Initialises the Allegro library. *)
  FUNCTION al_install (system_id: LONGINT): BOOLEAN;
  BEGIN
    IsAllegroInstalled := TRUE;
    al_error := '';
  { Right now it doesn't do nothing. }
    al_install := TRUE;
  END;



(* Function which initialises the Allegro library.  This is the same thing as
   calling @code(al_install @(AL_SYSTEM_AUTODETECT@)). *)
  FUNCTION al_init: BOOLEAN;
  BEGIN
    al_init := al_install (AL_SYSTEM_AUTODETECT);
  END;



(* Closes down the Allegro system. *)
  PROCEDURE al_exit;
  BEGIN
    IF IsAllegroInstalled THEN
    BEGIN
      _alUninstallAllModules_;
      IsAllegroInstalled := FALSE;
    END;
  END;



(* Outputs a message. *)
  PROCEDURE al_message (CONST msg: STRING);
  BEGIN
  { Right now it's just like WriteLn. }
    WriteLn (msg);
  END;



INITIALIZATION
  IsAllegroInstalled := FALSE;
FINALIZATION
{ To be sure it's a clean exit. }
  IF IsAllegroInstalled THEN
    al_exit;
END.
