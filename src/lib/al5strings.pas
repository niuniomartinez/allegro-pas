UNIT al5strings;
(***< Functions to integrate Pascal @code(STRING) with Allegro @link(AL_STR).
  Also implements Allegro's UNICODE support.

  @include(../docs/strings.pds)

  @include(../docs/utf8.pds) *)
(* Copyright (c) 2012-2020 Guillermo Martínez J.

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

{$include allegro5.cfg}

INTERFACE

  USES
    al5base;

(*
 * String manipulation
 *
 * These functions don't exist in Allegro.
 *
 * They exists because "In Delphi 2009, Embarcadero switched the entire RTL
 * over to the UnicodeString type, which represents strings using UTF-16."
 * -- Free Pascal Wiki (https://wiki.freepascal.org/FPC_Unicode_support)
 *****************************************************************************)

  FUNCTION al_string_to_str (CONST aString: SHORTSTRING): AL_STR;
    OVERLOAD; INLINE;
  FUNCTION al_string_to_str (CONST aString: ANSISTRING): AL_STR;
    OVERLOAD; INLINE;
  FUNCTION al_string_to_str (CONST aString: UNICODESTRING): AL_STR;
    OVERLOAD; INLINE;

  FUNCTION al_str_to_string (CONST aString: AL_STR): STRING;
    OVERLOAD; INLINE;
  FUNCTION al_str_to_string (CONST aString: AL_STRptr): STRING;
    OVERLOAD; INLINE;

  FUNCTION al_str_to_shortstring (CONST aString: AL_STR): SHORTSTRING;
    OVERLOAD; INLINE;
  FUNCTION al_str_to_shortstring (CONST aString: AL_STRptr): SHORTSTRING;
    OVERLOAD; INLINE;

  FUNCTION al_str_to_ansistring (CONST aString: AL_STR): ANSISTRING;
    OVERLOAD; INLINE;
  FUNCTION al_str_to_ansistring (CONST aString: AL_STRptr): ANSISTRING;
    OVERLOAD; INLINE;

  FUNCTION al_str_to_unicodestring (CONST aString: AL_STR): UNICODESTRING;
    OVERLOAD; INLINE;
  FUNCTION al_str_to_unicodestring (CONST aString: AL_STRptr): UNICODESTRING;
    OVERLOAD; INLINE;

{ TODO: al_str_to_int, al_str_to_float, al_int_to_str, al_float_to_str... }
{ TODO: al_str_lenth? }

  FUNCTION al_str_format (CONST Fmt: AL_STR; CONST Args : ARRAY OF CONST)
  : AL_STR;



(*
 * utf8.h
 *****************************************************************************)

  {TODO: Documentation says it's not needed as it's used internally.
	Only basic functionality is implemented for convenience. }
  {TODO: There are a lot of stuff to add here, including WIDESTRING and/or
	 UNICODESTRING support. }

  TYPE
  {** @exclude }
    _al_tagbstring = RECORD
      mlen, slen: AL_INT;
      data: AL_VOIDptr;
    END;
  (*** Pointer to @link(ALLEGRO_USTR). *)
    ALLEGRO_USTRptr = ^ALLEGRO_USTR;
    ALLEGRO_USTR = _al_tagbstring;
  (*** Pointer to @link(ALLEGRO_USTR_INFO). *)
    ALLEGRO_USTR_INFOptr = ^ALLEGRO_USTR_INFO;
    ALLEGRO_USTR_INFO = _al_tagbstring;

(* Creating strings. *)
  FUNCTION al_ustr_new (CONST s: AL_STR): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_new_from_buffer (CONST s: AL_STRptr; size: AL_SIZE_T): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* AL_PRINTFUNC(ALLEGRO_USTR *, al_ustr_newf, (const char *fmt, ...), 1, 2); *)
  PROCEDURE al_ustr_free (us: ALLEGRO_USTRptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_cstr (CONST us: ALLEGRO_USTRptr): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_ustr_to_buffer (CONST us: ALLEGRO_USTRptr; buffer: AL_STRptr; size: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_cstr_dup (CONST us: ALLEGRO_USTRptr): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_dup (CONST us: ALLEGRO_USTRptr): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_dup_substr (CONST us: ALLEGRO_USTRptr; start_pos, end_pos: AL_INT): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Predefined string. *)
  FUNCTION al_ustr_empty_string: ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Reference strings. *)
  FUNCTION al_ref_cstr (OUT info: ALLEGRO_USTR_INFO; CONST s: AL_STR): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ref_buffer (OUT info: ALLEGRO_USTR_INFO; CONST s: AL_STRptr;
      size: AL_SIZE_T): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ref_ustr (OUT info: ALLEGRO_USTR_INFO;
      CONST us: ALLEGRO_USTRptr; star_pos, end_pos: AL_INT): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Sizes and offsets *)
  FUNCTION al_ustr_size (CONST us: ALLEGRO_USTRptr): AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_length (CONST us: ALLEGRO_USTRptr): AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_offset (CONST us: ALLEGRO_USTRptr;index: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_next (CONST us: ALLEGRO_USTRptr; VAR aPos: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_prev (CONST us: ALLEGRO_USTRptr; VAR aPos: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Insert *)
  FUNCTION al_ustr_insert_chr (us1: ALLEGRO_USTRptr; aPos: AL_INT; c: AL_INT32)
  : AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Remove *)
  FUNCTION al_ustr_remove_chr (us: ALLEGRO_USTRptr; apos: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Assign *)
  FUNCTION al_ustr_assign (us1: ALLEGRO_USTRptr; CONST us2: ALLEGRO_USTRptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_assign_cstr (us1: ALLEGRO_USTRptr; CONST s: AL_STR): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Compare *)
  FUNCTION al_ustr_equal (CONST us1, us2: ALLEGRO_USTRptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_compare (CONST u, v: ALLEGRO_USTRptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_ncompare (CONST u, v: ALLEGRO_USTRptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Low level UTF-8 functions *)
  FUNCTION al_utf8_width (c: AL_INT32): AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

IMPLEMENTATION

  USES
  {$IFDEF ISDELPHI2009ANDUP}
  { This unit implements SysUtils and Strings using ANSISTRING instead of
    UNICODESTRING, which is the default in modern Delphi compilers. }
    AnsiStrings;
  {$ELSE}
    sysutils;
  {$ENDIF}

(* Converts string. *)
  FUNCTION al_string_to_str (CONST aString: SHORTSTRING): AL_STR;
  BEGIN
    RESULT := AL_STR (aString)
  END;

  FUNCTION al_string_to_str (CONST aString: ANSISTRING): AL_STR;
  BEGIN
  { No conversion needed. }
    RESULT := aString
  END;

  FUNCTION al_string_to_str (CONST aString: UNICODESTRING): AL_STR;
  BEGIN
    RESULT := UTF8Encode (aString)
  END;



  FUNCTION al_str_to_string (CONST aString: AL_STR): STRING;
  BEGIN
  {$IFDEF ISDELPHI2009ANDUP}
    RESULT := al_str_to_unicodestring (aString)
  {$ELSE}
    RESULT := aString
  {$ENDIF}
  END;

  FUNCTION al_str_to_string (CONST aString: AL_STRptr): STRING;
  BEGIN
  {$IFDEF ISDELPHI2009ANDUP}
    RESULT := UTF8ToString (StrPas (aString))
  {$ELSE}
    RESULT := StrPas (aString)
  {$ENDIF}
  END;


  FUNCTION al_str_to_shortstring (CONST aString: AL_STR): SHORTSTRING;
  BEGIN
    RESULT := aString
  END;


  FUNCTION al_str_to_shortstring (CONST aString: AL_STRptr): SHORTSTRING;
  BEGIN
    RESULT := StrPas (aString)
  END;



  FUNCTION al_str_to_ansistring (CONST aString: AL_STR): ANSISTRING;
  BEGIN
    RESULT := aString
  END;

  FUNCTION al_str_to_ansistring (CONST aString: AL_STRptr): ANSISTRING;
  BEGIN
    RESULT := StrPas (aString)
  END;



  FUNCTION al_str_to_unicodestring (CONST aString: AL_STR): UNICODESTRING;
  BEGIN
    RESULT :=
      {$IFDEF ISDELPHI2009ANDUP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF} (aString)
  END;

  FUNCTION al_str_to_unicodestring (CONST aString: AL_STRptr): UNICODESTRING;
  BEGIN
    RESULT := al_str_to_unicodestring (StrPas (aString))
  END;



(* Formats a string with given arguments. *)
  FUNCTION al_str_format (CONST Fmt: AL_STR; CONST Args : ARRAY OF CONST)
    : AL_STR;
  BEGIN
    RESULT := Format (Fmt, Args)
  END;

END.
