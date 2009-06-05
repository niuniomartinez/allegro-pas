UNIT albase;
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
 *	Base definitions to interface with Allegro and the Allegro.pas dynamic
 *	modules.
 *
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
  Windows;
 {$ENDIF}
{$ENDIF}



CONST
{ Access to the dynamic modules. }
{$IFDEF MSWINDOWS}
  ALLEGRO_SHARED_LIBRARY_NAME = 'alleg42.dll';
  ALL_PAS_SHARED_LIBRARY_NAME = 'alpas42.dll';
{$ELSE}
 {$ERROR Can't compile this platform.  Please read below. }
 (* TODO -oÑuño: Add support for *NIX shared objects. *)
 (* TODO -oÑuño: Add support for MacOS X dynamic libraries. *)
{$ENDIF}

{ To be used as size of Zero-Sized arrays. }
  AL_UNKNOWN_SIZE = 1024;



TYPE { Define some useful data types. }
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
  AL_FLOAT	= SINGLE;
  AL_STRING	= STRING;

  AL_SIMPLE_PROC = PROCEDURE; CDECL;
  AL_PARAM_PROC  = PROCEDURE (x: AL_PTR); CDECL;
  AL_INT_PROC	 = PROCEDURE (x: AL_INT); CDECL;

  AL_SIMPLE_FUNC = FUNCTION: AL_INT; CDECL;



IMPLEMENTATION

END.

