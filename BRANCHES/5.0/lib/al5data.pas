UNIT al5data;
(*<Defines few functions to manage data. *)
(*TODO: License. *)

{$include allegro.cfg}

INTERFACE

{$include allegro.inc}

(* Creates a null-terminated string from the given Pascal string.
  @seealso(_al_dispose_str_) *)
  FUNCTION _al_create_null_str_ (CONST Orig: STRING): PCHAR; INLINE;

(* Disposes a null-terminated string created by @link(_al_create_null_str_). *)
  PROCEDURE _al_dispose_str_ (Str: PCHAR); INLINE;

(* Converts from null-terminated string in to a Pascal string. *)
  FUNCTION _al_pchar_to_str_ (CONST Str: PCHAR): STRING; INLINE;

IMPLEMENTATION

  USES
    strings;

(* Creates a null-terminated string from the given Pascal string.
  @seealso(_al_dispose_str_) *)
  FUNCTION _al_create_null_str_ (CONST Orig: STRING): PCHAR;
  BEGIN
    _al_create_null_str_ := StrAlloc (Length (Orig));
    StrPCopy (_al_create_null_str_, Orig);
  END;



(* Disposes a null-terminated string created by @link(_al_create_null_str_). *)
  PROCEDURE _al_dispose_str_ (Str: PCHAR);
  BEGIN
    StrDispose (Str);
  END;



(* Converts from null-terminated string in to a Pascal string. *)
  FUNCTION _al_pchar_to_str_ (CONST Str: PCHAR): STRING;
  BEGIN
    _al_pchar_to_str_ := StrPas (Str);
  END;

END.
