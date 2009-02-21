(* Base definitions to interface with Allegro and the Allegro.pas dynamic
   modules.
   
   It also defines some constants to identify the library. *)
UNIT albase;

{ Defines the frame. }
{$MODE Delphi}
{$MACRO ON}
{$PACKRECORDS C}
{$H+}



INTERFACE

USES
  dynlibs;



CONST
(* Used to identify the library. *)
  AL_VERSION = '4.2.3 SVN';
(* Used to identify the library. *)
  AL_LIBRARY_NAME = 'Allegro.pas 4.2.3 SVN';
(* Used as size of Zero-Sized arrays. *)
  AL_UNKNOWN_SIZE = 1024;



TYPE
(* Define some data types.  They're still here just for backwards compatibility
   but shouldn't be used.  Use Pascal types instead. *)
  AL_PTR	= POINTER;
  AL_CHARptr	= ^AL_CHAR;
  AL_UCHARptr	= ^AL_UCHAR;
  AL_INTptr	= ^AL_INT;
  AL_LONGptr	= ^AL_LONG;
  AL_ULONGptr	= ^AL_ULONG;
  AL_FLOATptr	= ^AL_FLOAT;
  AL_STRINGptr	= ^AL_STRING;

  AL_CHAR	= CHAR;
  AL_UCHAR	= BYTE;
  AL_INT	= LONGINT;
  AL_LONG	= LONGINT;
  AL_ULONG	= DWORD;
  AL_FLOAT	= DOUBLE;
  AL_STRING	= STRING;

(* To define callback parameters. *)
  AL_SIMPLE_PROC = PROCEDURE; CDECL;
(* To define callback parameters. *)
  AL_PARAM_PROC  = PROCEDURE (x: AL_PTR); CDECL;
(* To define callback parameters. *)
  AL_INT_PROC	 = PROCEDURE (x: AL_INT); CDECL;
(* To define callback parameters. *)
  AL_SIMPLE_FUNC = FUNCTION: AL_INT; CDECL;



VAR
(* Identificator of the dynamic library.

   Used internally.  It can be used to access to those Allegro's functions
   that aren't implemented by Allegro.pas. *)
  __al_library_id__: TLibHandle;



IMPLEMENTATION

CONST
{ Access to the dynamic module. }
{$IFDEF MSWINDOWS}
  ALLEGRO_SHARED_LIBRARY_NAME = 'alleg42.dll';
{$ELSE}
  {$IFDEF UNIX}
    {$IFDEF DARWIN}
      {$ERROR Can't compile on MacOS X. }
    {$ELSE}
      ALLEGRO_SHARED_LIBRARY_NAME = 'liballeg-4.2.2.so';
    {$ENDIF}
  {$ELSE}
    {$ERROR Can't compile this platform. }
 (* TODO -oÑuño: Add support for MacOS X dynamic libraries. *)
  {$ENDIF}
{$ENDIF}

INITIALIZATION
{ Loads the library. }
  __al_library_id__ := 0;
  __al_library_id__ := LoadLibrary (PChar(ALLEGRO_SHARED_LIBRARY_NAME));
FINALIZATION
{ Releases the library. }
  IF (__al_library_id__ <> 0) THEN
    FreeLibrary (__al_library_id__);
  __al_library_id__ := 0;
END.
