unit al5Base;
(***<Base definitions to interface with Allegro dynamic module.

  This unit includes definitions of data types used by Allegro.  They're used
  internally to be sure they're the correct in any platform (i.e. 32bit or
  64bit).  You may use them if you wish.
 *)
(* Copyright (c) 2012-2022 Guillermo Martínez J. <niunio@users.sourceforge.net>

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

interface

  const
  (* Constants for error values. *)
    AL_EDOM   = 1;
    AL_ERANGE = 2;
  (* Defines some constants to build the correct names of the library files. *)
{$IFDEF DEBUGMODE}
    _DBG_ = '-debug'; (***<@exclude *)
{$ELSE}
    _DBG_ = ''; (***<@exclude *)
{$ENDIF}

{$IF DEFINED(UNIX)}
  {$INCLUDE al5_unix.inc}
{$ELSEIF DEFINED(LINUX)}
  {$INCLUDE al5_unix.inc}
{$ELSEIF DEFINED(WINDOWS)}
  {$INCLUDE al5_win.inc}
{$ENDIF}

{$IF DEFINED(WINDOWS) AND NOT DEFINED(NO_MONOLITH)}
  (*** @exclude Builds library name. *)
    ALLEGRO_LIB_NAME = 'allegro_monolith' + _DBG_ + '-5.2.dll';
    ALLEGRO_ACODEC_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
    ALLEGRO_AUDIO_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
    ALLEGRO_COLOR_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
    ALLEGRO_FONT_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
    ALLEGRO_IMAGE_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
    ALLEGRO_MEMFILE_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
    ALLEGRO_NATIVE_DLG_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
    ALLEGRO_PRIMITIVES_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
    ALLEGRO_TTF_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
    ALLEGRO_VIDEO_LIB_NAME = ALLEGRO_LIB_NAME; (***<@exclude *)
{$ELSE}
    ALLEGRO_LIB_NAME = _A5_LIB_PREFIX_+'allegro'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_ACODEC_LIB_NAME = _A5_LIB_PREFIX_+'allegro_acodec'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_AUDIO_LIB_NAME = _A5_LIB_PREFIX_+'allegro_audio'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_COLOR_LIB_NAME = _A5_LIB_PREFIX_+'allegro_color'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_FONT_LIB_NAME = _A5_LIB_PREFIX_+'allegro_font'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_IMAGE_LIB_NAME = _A5_LIB_PREFIX_+'allegro_image'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_MEMFILE_LIB_NAME = _A5_LIB_PREFIX_+'allegro_memfile'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_NATIVE_DLG_LIB_NAME = _A5_LIB_PREFIX_+'allegro_dialog'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_PRIMITIVES_LIB_NAME = _A5_LIB_PREFIX_+'allegro_primitives'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_TTF_LIB_NAME = _A5_LIB_PREFIX_+'allegro_ttf'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
    ALLEGRO_VIDEO_LIB_NAME = _A5_LIB_PREFIX_+'allegro_video'+_DBG_+_A5_LIB_EXT_; (***<@exclude *)
{$ENDIF}



(* Next are definitions of numeric data types.  We may use FPC's ctype unit,
   but Delphi doesn't has it.

   First: it defines some integers with specific lenght.
   Then: it defines the types used by C declarations.
 *)

  type
    AL_POINTER = Pointer;
    AL_INT8 = ShortInt;
    AL_UINT8 = Byte;
    AL_INT16 = SmallInt;
    AL_UINT16 = Word;
    AL_INT32 = LongInt;
    AL_UINT32 = LongWord;
    AL_INT64 = INT64;
{$IFDEF FPC}
    AL_UINT64 = QWord;
{$ELSE}
  {$IFDEF ISDELPHI2007ANDUP}
    AL_UINT64 = UINT64;
  {$ELSE}
    AL_UINT64 = INT64;
  {$ENDIF}
{$ENDIF}


    AL_BOOL = LongBool;
    AL_CHAR = AL_INT8;
    AL_UCHAR = AL_UINT8;
    AL_SHORT = AL_INT16;
    AL_USHORT = AL_UINT16;
    AL_INT = AL_INT32;
    AL_UINT = AL_UINT32;
{$IFDEF CPU64}
  {$IFDEF WINDOWS}
    AL_LONG = AL_INT32;
    AL_ULONG = AL_UINT32;
  {$ELSE}
    AL_LONG = AL_INT64;
    AL_ULONG = AL_UINT64;
  {$ENDIF}
    AL_SIZE_T = AL_UINT64;
    AL_INTPTR_T = AL_UINT64;
    AL_UINTPTR_T = AL_UINT64;
{$ELSE}
    AL_LONG = AL_INT32;
    AL_ULONG = AL_UINT32;
    AL_SIZE_T = AL_UINT32;
    AL_INTPTR_T = AL_UINT32;
    AL_UINTPTR_T = AL_UINT32;
{$ENDIF}
    AL_OFF_T = AL_UINT;
    AL_FLOAT = Single;
    AL_DOUBLE = Double;
    AL_STR = AnsiString;

    AL_VOIDptr = AL_POINTER;
{$IFDEF ISDELPHI2009ANDUP}
    AL_STRptr = PAnsiChar;
{$ELSE}
    AL_STRptr = PChar;
{$ENDIF}
    AL_INTptr = ^AL_INT;
    AL_FLOATptr = ^AL_FLOAT;

    AL_DATA_PTR_T = record
      case Integer of
        0: ( int_value : AL_INTPTR_T  );
        1: ( uint_value: AL_UINTPTR_T );
        2: ( ptr_value : AL_POINTER   );
    end;

implementation

end.
