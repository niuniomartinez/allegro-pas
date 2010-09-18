UNIT alfile;
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
 *	File I/O.
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

{$IFNDEF FPC}
 {$IFDEF MSWINDOWS}
{ Delphi needs the Windows unit. }
USES
  albase, { Needs some basic definitions. }
  Windows;
 {$ENDIF}
{$ELSE}
USES
  albase; { Needs some basic definitions. }
{$ENDIF}



(* Procedures and functions to manipulate file names. *)
  PROCEDURE al_get_executable_name (VAR aOutput: AL_STRING; size: AL_INT);
  FUNCTION al_replace_filename (VAR dest: AL_STRING; path, filename: AL_STRING; size: AL_INT): AL_STRING;
  FUNCTION al_replace_extension (VAR dest: AL_STRING; filename, ext: AL_STRING; size: AL_INT): AL_STRING;



(* File input and output. *)
CONST
  AL_F_READ         = 'r';
  AL_F_WRITE        = 'w';
  AL_F_READ_PACKED  = 'rp';
  AL_F_WRITE_PACKED = 'wp';
  AL_F_WRITE_NOPACK = 'w!';

  AL_F_PACK_MAGIC   = $736C6821;    (* magic number for packed files *)
  AL_F_NOPACK_MAGIC = $736C682E;    (* magic number for autodetect *)
  AL_F_EXE_MAGIC    = $736C682B;    (* magic number for appended data *)


TYPE
  AL_PACKFILEptr = AL_PTR;

  FUNCTION al_pack_fopen (filename, mode: AL_STRING): AL_PACKFILEptr;
  FUNCTION al_pack_fclose (f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fclose';
  FUNCTION al_pack_feof (f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_feof';
  FUNCTION al_pack_ferror (f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_ferror';

  FUNCTION al_pack_getc (f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_getc';
  FUNCTION al_pack_putc (c: AL_INT; f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_putc';
  FUNCTION al_pack_igetw (f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_igetw';
  FUNCTION al_pack_igetl (f: AL_PACKFILEptr): AL_LONG; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_igetl';
  FUNCTION al_pack_iputw (w: AL_INT; f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_iputw';
  FUNCTION al_pack_iputl (l: AL_LONG; f: AL_PACKFILEptr): AL_LONG; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_iputl';
  FUNCTION al_pack_mgetw (f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mgetw';
  FUNCTION al_pack_mgetl (f: AL_PACKFILEptr): AL_LONG; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mgetl';
  FUNCTION al_pack_mputw (w: AL_INT; f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mputw';
  FUNCTION al_pack_mputl (l: AL_LONG; f: AL_PACKFILEptr): AL_LONG; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mputl';
  FUNCTION al_pack_fread (p: AL_PTR; n: AL_LONG; f: AL_PACKFILEptr): AL_LONG; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fread';
  FUNCTION al_pack_fwrite (p: AL_PTR; n: AL_LONG; f: AL_PACKFILEptr): AL_LONG; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fwrite';
  FUNCTION al_pack_ungetc (c: AL_INT; f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_ungetc';
  FUNCTION al_pack_fgets (max: AL_INT; f: AL_PACKFILEptr): AL_STRING;
  FUNCTION al_pack_fputs (p: AL_STRING; f: AL_PACKFILEptr): AL_INT;




IMPLEMENTATION

USES
  sysutils;


(* Procedures and functions to manipulate file names. *)

  PROCEDURE get_executable_name (aOutput: PCHAR; size: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_executable_name';

  PROCEDURE al_get_executable_name (VAR aOutput: AL_STRING; size: AL_INT);
  VAR
    Temp: PCHAR;
  BEGIN
    Temp := STRALLOC (size + 1);
    get_executable_name (Temp, size);
    aOutput := STRPAS (Temp);
    STRDISPOSE (Temp);
  END;



  FUNCTION replace_filename (dest, path, filename: PCHAR; size: AL_INT): PCHAR; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'replace_filename';

  FUNCTION al_replace_filename (VAR dest: AL_STRING; path, filename: AL_STRING; size: AL_INT): AL_STRING;
  VAR
    Temp: PCHAR;
  BEGIN
    Temp := STRALLOC (size + 1);
    replace_filename (Temp, PCHAR (path), PCHAR (filename), size);
    dest := STRPAS (Temp);
    STRDISPOSE (Temp);
    al_replace_filename := dest;
  END;



  FUNCTION replace_extension (dest, filename, ext: PCHAR; size: AL_INT): PCHAR; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'replace_filename';

  FUNCTION al_replace_extension (VAR dest: AL_STRING; filename, ext: AL_STRING; size: AL_INT): AL_STRING;
  VAR
    Temp: PCHAR;
  BEGIN
    Temp := StrAlloc (size + 1);
    replace_extension (Temp, PCHAR (filename), PCHAR (ext), size);
    dest := StrPas (Temp);
    StrDispose (Temp);
    al_replace_extension := dest;
  END;


(* File input and output. *)
  FUNCTION pack_fopen (filename, mode: PCHAR): AL_PACKFILEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION pack_fgets (p: AL_CHARptr; max: AL_INT; f: AL_PACKFILEptr): AL_CHARptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION pack_fputs (p: AL_CHARptr; f: AL_PACKFILEptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_pack_fopen (filename, mode: AL_STRING): AL_PACKFILEptr;
  BEGIN
    al_pack_fopen := pack_fopen (PCHAR (filename), PCHAR (mode));
  END;

  FUNCTION al_pack_fgets (max: AL_INT; f: AL_PACKFILEptr): AL_STRING;
  VAR
    Temp: PChar;
  BEGIN
    Temp := STRALLOC (max + 1);
    al_pack_fgets := pack_fgets (Temp, max, f);
    StrDispose (Temp);
  END;

  FUNCTION al_pack_fputs (p: AL_STRING; f: AL_PACKFILEptr): AL_INT;
  BEGIN
    al_pack_fputs := pack_fputs (PChar (p), f);
  END;

END.

