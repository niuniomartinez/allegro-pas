UNIT aldtfile;
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
 *	Datafile access routines.
 *	by Ñuño Martínez <>
 *
 *	Based on the file "datafile.h" of the Allegro library
 *	by Shawn Hargreaves.
 *      Some parts of the code was created by the "h2pas" utility.
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

TYPE
  AL_DATAFILE_PROPERTYptr = ^AL_DATAFILE_PROPERTY;
  AL_DATAFILE_PROPERTY = RECORD
    dat : AL_CHARptr;		{ pointer to the data  }
    ftype : AL_INT;		{ property type  }
  END;

  AL_DATAFILE_PROPERTY_LISTptr = ^AL_DATAFILE_PROPERTY_LIST;
  AL_DATAFILE_PROPERTY_LIST = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_DATAFILE_PROPERTY;
  
  AL_DATAFILE_OBJECTptr = ^AL_DATAFILE_OBJECT;
  AL_DATAFILE_OBJECT = RECORD
    dat : AL_PTR;		{ pointer to the data  }
    ftype : AL_INT;		{ object type  }
    size : AL_INT;		{ size of the object  }
    prop : AL_DATAFILE_PROPERTY_LISTptr; { object properties  }
  END;

  AL_DATAFILEptr = ^AL_DATAFILE;
  AL_DATAFILE = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_DATAFILE_OBJECT;

  FUNCTION al_load_datafile (filename: AL_STRING): AL_DATAFILEptr;
  PROCEDURE al_unload_datafile (dat: AL_DATAFILEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unload_datafile';

  FUNCTION al_load_datafile_object (filename, objectname: AL_STRING): AL_DATAFILE_OBJECTptr;
  PROCEDURE al_unload_datafile_object (dat: AL_DATAFILE_OBJECTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unload_datafile_object';

IMPLEMENTATION

  FUNCTION load_datafile (filename: PCHAR): AL_DATAFILEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_datafile';

  FUNCTION al_load_datafile (filename: AL_STRING): AL_DATAFILEptr;
  BEGIN
    al_load_datafile := load_datafile (PCHAR (filename));
  END;


 
  FUNCTION load_datafile_object (filename, objectname: PCHAR): AL_DATAFILE_OBJECTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_datafile_object';

  FUNCTION al_load_datafile_object (filename, objectname: AL_STRING): AL_DATAFILE_OBJECTptr;
  BEGIN
    al_load_datafile_object := load_datafile_object (PCHAR (filename), PCHAR (objectname));
  END;

END.

