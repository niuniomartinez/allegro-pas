UNIT al5Base;
(***<Base definitions to interface with Allegro dynamic module.

  This unit includes definitions of data types used by Allegro.  They're used
  internally to be sure they're the correct in any platform (i.e. 32bit or
  64bit).  You may use them if you wish.
 *)
(* Copyright (c) 2012-2018 Guillermo Martínez J. <niunio@users.sourceforge.net>

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

{$INCLUDE allegro5.cfg}

INTERFACE

  CONST
  (* Defines some constants to build the correct names of the library files. *)
{$IFDEF DEBUGMODE}
    _DBG_ = '-debug'; {<@exclude }
{$ELSE}
    _DBG_ = ''; {<@exclude }
{$ENDIF}

{$IF DEFINED(UNIX)}
  {$INCLUDE al5_unix.inc}
{$ELSEIF DEFINED(LINUX)}
  {$INCLUDE al5_unix.inc}
{$ELSEIF DEFINED(WINDOWS)}
  {$INCLUDE al5_win.inc}
{$ENDIF}

{$IF DEFINED(WINDOWS) AND NOT DEFINED(NO_MONOLITH)}
  (* @exclude Builds library name. *)
    ALLEGRO_LIB_NAME = 'allegro_monolith' + _DBG_ + '-5.2.dll';
    ALLEGRO_ACODEC_LIB_NAME = ALLEGRO_LIB_NAME; {<@exclude }
    ALLEGRO_AUDIO_LIB_NAME = ALLEGRO_LIB_NAME; {<@exclude }
    ALLEGRO_COLOR_LIB_NAME = ALLEGRO_LIB_NAME; {<@exclude }
    ALLEGRO_FONT_LIB_NAME = ALLEGRO_LIB_NAME; {<@exclude }
    ALLEGRO_IMAGE_LIB_NAME = ALLEGRO_LIB_NAME; {<@exclude }
    ALLEGRO_NATIVE_DLG_LIB_NAME = ALLEGRO_LIB_NAME; {<@exclude }
    ALLEGRO_PRIMITIVES_LIB_NAME = ALLEGRO_LIB_NAME; {<@exclude }
    ALLEGRO_TTF_LIB_NAME = ALLEGRO_LIB_NAME; {<@exclude}
{$ELSE}
    ALLEGRO_LIB_NAME = _A5_LIB_PREFIX_+'allegro'+_DBG_+_A5_LIB_EXT_; {<@exclude }
    ALLEGRO_ACODEC_LIB_NAME = _A5_LIB_PREFIX_+'allegro_acodec'+_DBG_+_A5_LIB_EXT_; {<@exclude }
    ALLEGRO_AUDIO_LIB_NAME = _A5_LIB_PREFIX_+'allegro_audio'+_DBG_+_A5_LIB_EXT_; {<@exclude }
    ALLEGRO_COLOR_LIB_NAME = _A5_LIB_PREFIX_+'allegro_color'+_DBG_+_A5_LIB_EXT_; {<@exclude }
    ALLEGRO_FONT_LIB_NAME = _A5_LIB_PREFIX_+'allegro_font'+_DBG_+_A5_LIB_EXT_; {<@exclude }
    ALLEGRO_IMAGE_LIB_NAME = _A5_LIB_PREFIX_+'allegro_image'+_DBG_+_A5_LIB_EXT_; {<@exclude }
    ALLEGRO_NATIVE_DLG_LIB_NAME = _A5_LIB_PREFIX_+'allegro_dialog'+_DBG_+_A5_LIB_EXT_; {<@exclude }
    ALLEGRO_PRIMITIVES_LIB_NAME = _A5_LIB_PREFIX_+'allegro_primitives'+_DBG_+_A5_LIB_EXT_; {<@exclude }
    ALLEGRO_TTF_LIB_NAME = _A5_LIB_PREFIX_+'allegro_ttf'+_DBG_+_A5_LIB_EXT_; {<@exclude }
{$ENDIF}



(* Next are definitions of numeric data types.  We may use FPC's ctype unit,
   but Delphi doesn't has it.

   First: it defines some integers with specific lenght.
   Then: it defines the types used by C declarations.
 *)

  TYPE
  (* Generic pointer. *)
    AL_POINTER = POINTER;
  (* Signed 8bit integer values. *)
    AL_INT8 = SHORTINT;
  (* Unsigned 8bit integer values. *)
    AL_UINT8 = BYTE;
  (* Signed 16bit integer values. *)
    AL_INT16 = SMALLINT;
  (* Unsigned 16bit integer values. *)
    AL_UINT16 = WORD;
  (* Signed 32bit integer values. *)
    AL_INT32 = LONGINT;
  (* Unsigned 32bit integer values. *)
    AL_UINT32 = LONGWORD;
  (* Signed 64bit integer values. *)
    AL_INT64 = INT64;
{$IFDEF FPC}
  (* Unsigned 64bit integer values. *)
    AL_UINT64 = QWORD;
{$ELSE}
  {$IFDEF ISDELPHI2007ANDUP}
  (* Unsigned 64bit integer values. *)
    AL_UINT64 = UINT64;
  {$ELSE}
  (* Unsigned 64bit integer values. *)
    AL_UINT64 = INT64;
  {$ENDIF}
{$ENDIF}


{ TODO: Check wich boolean type does it uses. }
  (* Boolean result. *)
    AL_BOOL = LONGBOOL;

  (* Signed 8bit integer.

    Note that it isn't Pascal's CHAR type! *)
    AL_CHAR = AL_INT8;
  (* Unsigned 8bit integer values. *)
    AL_UCHAR = AL_UINT8;
  (* Signed 16bit integer values. *)
    AL_SHORT = AL_INT16;
  (* Unsigned 16bit integer values. *)
    AL_USHORT = AL_UINT16;
  (* Signed 32bit integer values. *)
    AL_INT = AL_INT32;
  (* Unsigned 32bit integer values. *)
    AL_UINT = AL_UINT32;
{$IFDEF CPU64}
  {$IFDEF WINDOWS}
  (* Signed 32/64bit integer values. *)
    AL_LONG = AL_INT32;
  (* Unsigned 32/64bit integer values. *)
    AL_ULONG = AL_UINT32;
  {$ELSE}
  (* Signed 32/64bit integer values. *)
    AL_LONG = AL_INT64;
  (* Unsigned 32/64bit integer values. *)
    AL_ULONG = AL_UINT64;
  {$ENDIF}
  (* size_t equivalent. *)
    AL_SIZE_T = AL_UINT64;
  (* Fake pointer type.  It's needed to pass pointers as integer values in some
     methods. *)
    AL_INTPTR_T = AL_UINT64;
  (* Fake pointer type.  It's needed to pass pointers as integer values in some
     methods. *)
    AL_UINTPTR_T = AL_UINT64;
{$ELSE}
  (* Signed 32/64bit integer values. *)
    AL_LONG = AL_INT32;
  (* Unsigned 32/64bit integer values. *)
    AL_ULONG = AL_UINT32;
  (* size_t equivalent. *)
    AL_SIZE_T = AL_UINT32;
  (* Fake pointer type.  It's needed to pass pointers as integer values in some
     methods. *)
    AL_INTPTR_T = AL_UINT32;
  (* Fake pointer type.  It's needed to pass pointers as integer values in some
     methods. *)
    AL_UINTPTR_T = AL_UINT32;
{$ENDIF}
  (* Another 64bit integer to store sizes. *)
    AL_OFF_T = AL_UINT;

  (* Float value. *)
    AL_FLOAT = SINGLE;
  (* Double value. *)
    AL_DOUBLE = DOUBLE;
  (* Strings.  Used in parameters to convert Pascal's @code(STRING) to C's
    @code(char * )*)
    AL_STR = ANSISTRING;

  (* Pointer. *)
    AL_VOIDptr = AL_POINTER;
  (* Pointer to text strings.  Used to convert Pascal's @code(STRING) to C
    @code(char * ) *)

{$IFDEF ISDELPHI2009ANDUP}
    AL_STRptr = PAnsiChar;
{$ELSE}
    AL_STRptr = PCHAR;
{$ENDIF}
  (* Pointer to integer. *)
    AL_INTptr = ^AL_INT;
  (* Pointer to float. *)
    AL_FLOATptr = ^AL_FLOAT;

IMPLEMENTATION

END.
