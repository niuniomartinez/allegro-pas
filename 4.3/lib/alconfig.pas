UNIT alconfig;
(*< Various parts of Allegro, such as the sound routines and the @link
    (al_load_joystick_data) function, require some configuration information.
    This data is stored in text files as a collection of @code (variable=value)
    lines, along with comments that begin with a @code (#) character and
    continue to the end of the line.  The configuration file may optionally be
    divided into sections, which begin with a @code ([sectionname]) line.  Each
    section has a unique namespace, to prevent variable name conflicts, but any
    variables that aren't in a section are considered to belong to all the
    sections simultaneously.

    Note that variable and section names cannot contain spaces.

    By default the configuration data is read from a file called @code
    (allegro.cfg), which can be located either in the same directory as the
    program executable, or the directory pointed to by the @code (ALLEGRO)
    environment variable.  Under Unix, it also checks for @code
    (~/allegro.cfg), @code (~/.allegrorc), @code (/etc/allegro.cfg), and @code
    (/etc/allegrorc), in that order;  under BeOS only the last two are also
    checked.  MacOS X also checks in the Contents/Resources directory of the
    application bundle, if any, before doing the checks above.

    If you don't like this approach, you can specify any filename you like, or
    use a block of binary configuration data provided by your program (which
    could for example be loaded from a datafile).  You can also extend the
    paths searched for allegro resources with @link
    (al_set_allegro_resource_path).

    You can store whatever custom information you like in the config file,
    along with the standard variables that are used by Allegro (see below).
    Allegro comes with a setup directory where you can find configuration
    programs.  The standalone setup program is likely to be of interest to
    final users.  It allows any user to create an @code (allegro.cfg) file
    without the need to touch a text editor and enter values by hand.  It also
    provides a few basic tests like sound playing for sound card testing. You
    are welcome to include the setup program with your game, either as is or
    with modified graphics to fit better your game.  *)

{$H+}
{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}




INTERFACE

USES
  albase; { Needs some basic definitions. }



(* Sets the configuration file to be used by all subsequent config functions.
   (Allegro will not search for this file in other locations as it does with
   allegro.cfg at the time of initialization.)

   All pointers returned by previous calls to @link (al_get_config_string) and
   other related functions are invalidated when you call this function!  You
   can call this function before @link (al_install) to change the configuration
   file, but after @link (al_set_uformat) if you want to use a text encoding
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
   and IBK instrument file, but the user could still use an @code (allegro.cfg)
   file to specify the port settings and irq numbers.

   The override config file will not only take precedence when reading, but
   will also be used for storing values.  When you are done with using the
   override config file, you can call @code (al_override_config_file) with a
   @nil parameter, so config data will be directly read from the current config
   file again.

   @bold (Note:) The override file is completely independent from the current
   configuration.  You can e.g. call @link (al_set_config_file), and the
   override file will still be active.  Also the @link (al_flush_config_file)
   function will only affect the current config file (which can be changed with
   @code (al_set_config_file)), never the overriding one specified with this
   function.  The modified override config is written back to disk whenever you
   call @code (al_override_config_file).

    Note that this function and @link (al_override_config_data) are mutually
    exclusive, i.e. calling one will cancel the effects of the other. *)
  PROCEDURE al_override_config_file (filename: STRING);

(* Version of @link (al_override_config_file) which uses a block of data that
   has already been read into memory.  The length of the block has to be
   specified in bytes.

   Note that this function and @code (al_override_config_file) are mutually
   exclusive, i.e. calling one will cancel the effects of the other. *)
  PROCEDURE al_override_config_data (data: POINTER; lng: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'override_config_data';

(* Writes the current config file to disk if the contents have changed since
   it was loaded or since the latest call to the function. *)
  PROCEDURE al_flush_config_file; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'flush_config_file';

  PROCEDURE al_push_config_state; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'push_config_state';
  PROCEDURE al_pop_config_state; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pop_config_state';

  FUNCTION al_get_config_string (section, name, def: STRING): STRING;
  FUNCTION al_get_config_int (section, name: STRING; def: LONGINT): LONGINT;
  FUNCTION al_get_config_hex (section, name: STRING; def: LONGINT): LONGINT;
  FUNCTION al_get_config_float (section, name: STRING; def: DOUBLE): DOUBLE;
  FUNCTION al_get_config_id (section, name: STRING; def: LONGINT): LONGINT;
  FUNCTION al_get_config_argv (section, name: STRING; argc: PLONGINT): PSTRING;

  PROCEDURE al_set_config_string (section, name, val: STRING);
  PROCEDURE al_set_config_int (section, name: STRING; val: LONGINT);
  PROCEDURE al_set_config_hex (section, name: STRING; val: LONGINT);
  PROCEDURE al_set_config_float (section, name:STRING; val: DOUBLE);
  PROCEDURE al_set_config_id (section, name: STRING; val: LONGINT);

IMPLEMENTATION

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


  
  FUNCTION get_config_argv (section, name: PCHAR; argc: PLONGINT): PSTRING; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_argv';

  FUNCTION al_get_config_argv (section, name: STRING; argc: PLONGINT): STRINGptr;
  BEGIN
    al_get_config_argv := get_config_argv (PCHAR (section), PCHAR (name), argc);
  END;



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

END.

