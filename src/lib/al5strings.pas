unit al5strings;
(***< Functions to integrate Pascal @code(String) with Allegro @link(AL_STR).
  Also implements Allegro's UNICODE support.

  @include(../docs/strings.pds)

  @include(../docs/utf8.pds) *)
(* Copyright (c) 2012-2024 Guillermo Martínez J.

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

{$Include allegro5.cfg}

interface

  uses
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

  function al_string_to_str (const aString: ShortString): AL_STR;
    overload; inline;
  function al_string_to_str (const aString: AnsiString): AL_STR;
    overload; inline;
  function al_string_to_str (const aString: UnicodeString): AL_STR;
    overload; inline;

  function al_str_to_string (const aString: AL_STR): String;
    overload; inline;
  function al_str_to_string (const aString: AL_STRptr): String;
    overload; inline;

  function al_str_to_shortstring (const aString: AL_STR): ShortString;
    overload; inline;
  function al_str_to_shortstring (const aString: AL_STRptr): ShortString;
    overload; inline;

  function al_str_to_ansistring (const aString: AL_STR): AnsiString;
    overload; inline;
  function al_str_to_ansistring (const aString: AL_STRptr): AnsiString;
    overload; inline;

  function al_str_to_unicodestring (const aString: AL_STR): UnicodeString;
    overload; inline;
  function al_str_to_unicodestring (const aString: AL_STRptr): UnicodeString;
    overload; inline;

{ TODO: al_str_to_int, al_str_to_float, al_int_to_str, al_float_to_str... }
{ TODO: al_str_lenth? }

  function al_str_format (const aFmt: AL_STR; const aArgs : array of const)
  : AL_STR;



(*
 * utf8.h
 *****************************************************************************)

  {TODO: Documentation says it's not needed as it's used internally.
	Only basic functionality is implemented for convenience. }
  {TODO: There are a lot of stuff to add here, including WideString and/or
	 UnicodeString support. }

  type
  {** @exclude }
    _al_tagbstring = record
      mlen, slen: AL_INT;
      data: AL_VOIDptr;
    end;
  (*** Pointer to @link(ALLEGRO_USTR). *)
    ALLEGRO_USTRptr = ^ALLEGRO_USTR;
    ALLEGRO_USTR = _al_tagbstring;
  (*** Pointer to @link(ALLEGRO_USTR_INFO). *)
    ALLEGRO_USTR_INFOptr = ^ALLEGRO_USTR_INFO;
    ALLEGRO_USTR_INFO = _al_tagbstring;

(* Creating strings. *)
  function al_ustr_new (const s: AL_STR): ALLEGRO_USTRptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_new_from_buffer (const s: AL_STRptr; size: AL_SIZE_T): ALLEGRO_USTRptr;
    CDECL; external ALLEGRO_LIB_NAME;
(* AL_PRINTFUNC(ALLEGRO_USTR *, al_ustr_newf, (const char *fmt, ...), 1, 2); *)
  procedure al_ustr_free (us: ALLEGRO_USTRptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_cstr (const us: ALLEGRO_USTRptr): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_ustr_to_buffer (const us: ALLEGRO_USTRptr; buffer: AL_STRptr; size: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_cstr_dup (const us: ALLEGRO_USTRptr): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_dup (const us: ALLEGRO_USTRptr): ALLEGRO_USTRptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_dup_substr (const us: ALLEGRO_USTRptr; start_pos, end_pos: AL_INT): ALLEGRO_USTRptr;
    CDECL; external ALLEGRO_LIB_NAME;

(* Predefined string. *)
  function al_ustr_empty_string: ALLEGRO_USTRptr;
    CDECL; external ALLEGRO_LIB_NAME;

(* Reference strings. *)
  function al_ref_cstr (out info: ALLEGRO_USTR_INFO; const s: AL_STR): ALLEGRO_USTRptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ref_buffer (out info: ALLEGRO_USTR_INFO; const s: AL_STRptr;
      size: AL_SIZE_T): ALLEGRO_USTRptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ref_ustr (out info: ALLEGRO_USTR_INFO;
      const us: ALLEGRO_USTRptr; star_pos, end_pos: AL_INT): ALLEGRO_USTRptr;
    CDECL; external ALLEGRO_LIB_NAME;

(* Sizes and offsets *)
  function al_ustr_size (const us: ALLEGRO_USTRptr): AL_SIZE_T;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_length (const us: ALLEGRO_USTRptr): AL_SIZE_T;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_offset (const us: ALLEGRO_USTRptr;index: AL_INT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_next (const us: ALLEGRO_USTRptr; var aPos: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_prev (const us: ALLEGRO_USTRptr; var aPos: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

(* Insert *)
  function al_ustr_insert_chr (us: ALLEGRO_USTRptr; aPos: AL_INT; c: AL_INT32)
  : AL_SIZE_T;
    CDECL; external ALLEGRO_LIB_NAME;

(* Remove *)
  function al_ustr_remove_chr (us: ALLEGRO_USTRptr; apos: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

(* Assign *)
  function al_ustr_assign (us1: ALLEGRO_USTRptr; const us2: ALLEGRO_USTRptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_assign_cstr (us1: ALLEGRO_USTRptr; const s: AL_STR): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

(* Compare *)
  function al_ustr_equal (const us1, us2: ALLEGRO_USTRptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_compare (const u, v: ALLEGRO_USTRptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ustr_ncompare (const u, v: ALLEGRO_USTRptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;

(* Low level UTF-8 functions *)
  function al_utf8_width (c: AL_INT32): AL_SIZE_T;
    CDECL; external ALLEGRO_LIB_NAME;

implementation

  uses
  {$IfDef ISDELPHI2009ANDUP}
  { This unit implements SysUtils and Strings using AnsiString instead of
    UnicodeString, which is the default in modern Delphi compilers.
  }
    AnsiStrings;
  {$Else}
    sysutils;
  {$EndIf}

(* Convert string. *)
  function al_string_to_str (const aString: ShortString): AL_STR;
  begin
    Result := AL_STR (aString)
  end;

  function al_string_to_str (const aString: AnsiString): AL_STR;
  begin
  { No conversion needed. }
    Result := aString
  end;

  function al_string_to_str (const aString: UnicodeString): AL_STR;
  begin
    Result := UTF8Encode (aString)
  end;



  function al_str_to_string (const aString: AL_STR): String;
  begin
    Result := {$IfDef ISDELPHI2009ANDUP}al_str_to_unicodestring{$EndIf} (aString)
  end;

  function al_str_to_string (const aString: AL_STRptr): String;
  begin
    Result := {$IfDef ISDELPHI2009ANDUP}UTF8ToString{$EndIf} (StrPas (aString))
  end;


  function al_str_to_shortstring (const aString: AL_STR): ShortString;
  begin
    Result := aString
  end;


  function al_str_to_shortstring (const aString: AL_STRptr): ShortString;
  begin
    Result := StrPas (aString)
  end;



  function al_str_to_ansistring (const aString: AL_STR): AnsiString;
  begin
    Result := aString
  end;

  function al_str_to_ansistring (const aString: AL_STRptr): AnsiString;
  begin
    Result := StrPas (aString)
  end;



  function al_str_to_unicodestring (const aString: AL_STR): UnicodeString;
  begin
    Result :=
      {$IfDef ISDELPHI2009ANDUP}UTF8ToString{$Else}UTF8Decode{$EndIf} (aString)
  end;

  function al_str_to_unicodestring (const aString: AL_STRptr): UnicodeString;
  begin
    Result :=
      {$IfDef ISDELPHI2009ANDUP}UTF8ToString{$Else}UTF8Decode{$EndIf} (StrPas (aString))
  end;



(* Formats a string with given arguments. *)
  function al_str_format (const aFmt: AL_STR; const aArgs : array of const)
    : AL_STR;
  begin
    Result := al_string_to_str (Format (al_str_to_ansistring (aFmt), aArgs))
  end;

end.
