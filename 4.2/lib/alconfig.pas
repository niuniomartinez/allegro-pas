UNIT alconfig;
(*
  ______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\ 
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	Configuration file routines.
 *	by Ñuño Martínez <>
 *
 *	See readme.txt for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$LONGSTRINGS ON}
{$ELSE}
{ Assumes Borland Delphi/Turbo. }
 {$A-}
 {$H+}
{$ENDIF}



INTERFACE

USES
  albase; { Needs some basic definitions. }

  PROCEDURE al_set_config_file (filename: AL_STRING);
  PROCEDURE al_set_config_data (data: AL_PTR; lng: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_data';
  PROCEDURE al_override_config_file (filename: AL_STRING);
  PROCEDURE al_override_config_data (data: AL_PTR; lng: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'override_config_data';
  PROCEDURE al_flush_config_file; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'flush_config_file';

  PROCEDURE al_push_config_state; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'push_config_state';
  PROCEDURE al_pop_config_state; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pop_config_state';

  FUNCTION al_get_config_string (section, name, def: AL_STRING): AL_STRING;
  FUNCTION al_get_config_int (section, name: AL_STRING; def: AL_INT): AL_INT;
  FUNCTION al_get_config_hex (section, name: AL_STRING; def: AL_INT): AL_INT;
  FUNCTION al_get_config_float (section, name: AL_STRING; def: AL_FLOAT): AL_FLOAT;
  FUNCTION al_get_config_id (section, name: AL_STRING; def: AL_INT): AL_INT;
  FUNCTION al_get_config_argv (section, name: AL_STRING; argc: AL_INTptr): AL_STRINGptr;

  PROCEDURE al_set_config_string (section, name, val: AL_STRING);
  PROCEDURE al_set_config_int (section, name: AL_STRING; val: AL_INT);
  PROCEDURE al_set_config_hex (section, name: AL_STRING; val: AL_INT);
  PROCEDURE al_set_config_float (section, name:AL_STRING; val: AL_FLOAT);
  PROCEDURE al_set_config_id (section, name: AL_STRING; val: AL_INT);

IMPLEMENTATION

  PROCEDURE set_config_file (filename: PCHAR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_file';

  PROCEDURE al_set_config_file (filename: AL_STRING);
  BEGIN
    set_config_file (PCHAR (filename));
  END;



  PROCEDURE override_config_file (filename: PCHAR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'override_config_file';

  PROCEDURE al_override_config_file (filename: AL_STRING);
  BEGIN
    override_config_file (PCHAR (filename));
  END;


  
  FUNCTION get_config_string (section, name, def: PCHAR): PCHAR; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_string';

  FUNCTION al_get_config_string (section, name, def: AL_STRING): AL_STRING;
  BEGIN
    al_get_config_string := get_config_string (PCHAR (section), PCHAR (name), PCHAR (def));
  END;



  FUNCTION get_config_int (section, name: PCHAR; def: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_int';

  FUNCTION al_get_config_int (section, name: AL_STRING; def: AL_INT): AL_INT;
  BEGIN
    al_get_config_int := get_config_int (PCHAR (section), PCHAR (name), def);
  END;



  FUNCTION get_config_hex (section, name: PCHAR; def: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_hex';

  FUNCTION al_get_config_hex (section, name:AL_STRING; def: AL_INT): AL_INT;
  BEGIN
    al_get_config_hex := get_config_hex (PCHAR (section), PCHAR (name), def);
  END;

  
  FUNCTION get_config_float (section, name: PCHAR; def: AL_FLOAT): AL_FLOAT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_float';

  FUNCTION al_get_config_float (section, name: AL_STRING; def: AL_FLOAT): AL_FLOAT;
  BEGIN
    al_get_config_float := get_config_float (PCHAR (section), PCHAR (name), def);
  END;


  
  FUNCTION get_config_id (section, name: PCHAR; def: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_id';

  FUNCTION al_get_config_id (section, name: AL_STRING; def: AL_INT): AL_INT;
  BEGIN
    al_get_config_id := get_config_id (PCHAR (section), PCHAR (name), def);
  END;


  
  FUNCTION get_config_argv (section, name: PCHAR; argc: AL_INTptr): AL_STRINGptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_config_argv';

  FUNCTION al_get_config_argv (section, name: AL_STRING; argc: AL_INTptr): AL_STRINGptr;
  BEGIN
    al_get_config_argv := get_config_argv (PCHAR (section), PCHAR (name), argc);
  END;



  PROCEDURE set_config_string (section, name, val: PCHAR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_string';

  PROCEDURE al_set_config_string (section, name, val: AL_STRING);
  BEGIN
    set_config_string (PCHAR (section), PCHAR (name), PCHAR (val));
  END;
  

  
  PROCEDURE set_config_int (section, name: PCHAR; val: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_int';

  PROCEDURE al_set_config_int (section, name: AL_STRING; val: AL_INT);
  BEGIN
    set_config_int (PCHAR (section), PCHAR (name), val);
  END;


  
  PROCEDURE set_config_hex (section, name: PCHAR; val: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_hex';

  PROCEDURE al_set_config_hex (section, name: AL_STRING; val: AL_INT);
  BEGIN
    set_config_hex (PCHAR (section), PCHAR (name), val);
  END;



  PROCEDURE set_config_float (section, name: PCHAR; val: AL_FLOAT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_float';

  PROCEDURE al_set_config_float (section, name: AL_STRING; val: AL_FLOAT);
  BEGIN
    set_config_float (PCHAR (section), PCHAR (name), val);
  END;



  PROCEDURE set_config_id (section, name: PCHAR; val: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_config_id';

  PROCEDURE al_set_config_id (section, name: AL_STRING; val: AL_INT);
  BEGIN
    set_config_id (PCHAR (section), PCHAR (name), val);
  END;

END.

