UNIT alfixed;
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
 *	Fixed point type.
 *
 *	by Ñuño Martínez <niunio@users.sourceforge.net>
 *
 *      Seoane <http://delphi.jmrds.com> wrote a patch to make trigonometry
 *      functions compatible with Delphi and may be others non FreePascal
 *      compilers.
 *
 *	See readme.txt for license and copyright information.
 *)

(* TODO: Doesn't check overflows. *)

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
  albase; { Some basic definitions. }

TYPE { Define data types. }
  AL_FIXED	= AL_INT;



(* Conversion. *)
  FUNCTION al_itofix (x: AL_INT): AL_FIXED;
  FUNCTION al_fixtoi (x: AL_FIXED): AL_INT;
  FUNCTION al_ftofix (x: REAL): AL_FIXED;
  FUNCTION al_fixtof (x: AL_FIXED): REAL;

(* Math. *)
  FUNCTION al_fixadd (x, y: AL_FIXED): AL_FIXED;
  FUNCTION al_fixsub (x, y: AL_FIXED): AL_FIXED;
  FUNCTION al_fixmul (x, y: AL_FIXED): AL_FIXED;
  FUNCTION al_fixdiv (x, y: AL_FIXED): AL_FIXED;
  
  FUNCTION al_fixsqrt (x: AL_FIXED): AL_FIXED;

(* Trigonometry. *)
CONST
  al_fixtorad: AL_FIXED = 1608;	   { al_ftofix (2pi/256) }
  al_radtofix: AL_FIXED = 2670177; { al_ftofix (256/2pi) }
  
  FUNCTION al_fixsin (x: AL_FIXED): AL_FIXED;
  FUNCTION al_fixcos (x: AL_FIXED): AL_FIXED;
  FUNCTION al_fixtan (x: AL_FIXED): AL_FIXED;
  FUNCTION al_fixasin (x: AL_FIXED): AL_FIXED;
  FUNCTION al_fixacos (x: AL_FIXED): AL_FIXED;
  FUNCTION al_fixatan (x: AL_FIXED): AL_FIXED;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fixatan';
  FUNCTION al_fixatan2 (x, y: AL_FIXED): AL_FIXED;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'fixatan2';
  
  PROCEDURE __al_inittrig__; { FOR INTERNAL USE ONLY. }


IMPLEMENTATION

USES
  alsystem; { For al_errno. }



TYPE
  TBLptr = ^AL_FIXED;



VAR
  _cos_tbl, _tan_tbl, _acos_tbl: TBLptr;



(* Conversion. *)
FUNCTION al_itofix (x: AL_INT): AL_FIXED;
BEGIN
  al_itofix := x SHL 16;
END;

FUNCTION al_fixtoi (x: AL_FIXED): AL_INT;
BEGIN
  IF x < 0 THEN
  { SHR doesn't keep the sign bit. }
    al_fixtoi := (x SHR 16) OR $FFFF0000
  ELSE
    al_fixtoi := x SHR 16;
END;

FUNCTION al_ftofix (x: REAL): AL_FIXED;
BEGIN
  IF x > 32767.0 THEN
    al_ftofix := $7FFFFFFF
  ELSE IF x < -32767.0 THEN
    al_ftofix := -$7FFFFFFF
  ELSE IF x < 0 THEN
    al_ftofix := TRUNC (x * 65536 - 0.5)
  ELSE
    al_ftofix := TRUNC (x * 65536 + 0.5);
END;

FUNCTION al_fixtof (x: AL_FIXED): REAL;
BEGIN
  al_fixtof := x / 65536.0;
END;

(* Math. *)
FUNCTION al_fixadd (x, y: AL_FIXED): AL_FIXED;
VAR
  R: AL_FIXED;
BEGIN
  R := x + y;
  IF R >= 0 THEN
  BEGIN 
    IF (x < 0) AND (y < 0) THEN
    BEGIN
      al_errno^ := 34; { ERANGE }
      R := -$7FFFFFFF;
    END;
  END
  ELSE BEGIN
    IF (x > 0) AND (y > 0) THEN
    BEGIN
      al_errno^ := 34; { ERANGE }
      R := $7FFFFFFF;
    END;
  END;
  al_fixadd := R;
END;

FUNCTION al_fixsub (x, y: AL_FIXED): AL_FIXED;
VAR
  R: AL_FIXED;
BEGIN
  R := x - y;
  IF R >= 0 THEN
  BEGIN 
    IF (x < 0) AND (y > 0) THEN
    BEGIN
      al_errno^ := 34; { ERANGE }
      R := -$7FFFFFFF;
    END;
  END
  ELSE BEGIN
    IF (x > 0) AND (y < 0) THEN
    BEGIN
      al_errno^ := 34; { ERANGE }
      R := $7FFFFFFF;
    END;
  END;
  al_fixsub := R;
END;

FUNCTION al_fixmul (x, y: AL_FIXED): AL_FIXED;
BEGIN
  al_fixmul := al_ftofix (al_fixtof (x) * al_fixtof (y));
END;
 
FUNCTION al_fixdiv (x, y: AL_FIXED): AL_FIXED;
BEGIN
  IF y = 0 THEN
  BEGIN
    al_errno^ := 34; { ERANGE }
    IF x < 0 THEN
      al_fixdiv := -$7FFFFFFF
    ELSE
      al_fixdiv := $7FFFFFFF;
  END
  ELSE
    al_fixdiv := al_ftofix (al_fixtof (x) / al_fixtof (y));
END;

FUNCTION al_fixsqrt (x: AL_FIXED): AL_FIXED;
BEGIN
  IF x > 0 THEN
    al_fixsqrt := al_ftofix (SQRT (al_fixtof (x)))
  ELSE
    al_fixsqrt := 0;
END;



(* Trigonometry. *)

{ Delphi can't access to the public variables from Allegro, so we need some
  magic to access them. }
  FUNCTION _get_cos_tbl_: TBLptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_tan_tbl_: TBLptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_acos_tbl_: TBLptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;

PROCEDURE __al_inittrig__;
BEGIN
  _cos_tbl := _get_cos_tbl_;
  _tan_tbl := _get_tan_tbl_;
  _acos_tbl:= _get_acos_tbl_;
END;



{ Some platforms have problems accesing to array pointers due strict typing. }
{$IFDEF FPC}

FUNCTION al_fixsin (x: AL_FIXED): AL_FIXED;
BEGIN
  al_fixsin := _cos_tbl[((x - $400000 + $4000) SHR 15) AND $1FF];
END;

FUNCTION al_fixcos (x: AL_FIXED): AL_FIXED;
BEGIN
  al_fixcos := _cos_tbl[((x + $4000) SHR 15) AND $1FF];
END;

FUNCTION al_fixtan (x: AL_FIXED): AL_FIXED;
BEGIN
  al_fixtan := _tan_tbl[((x + $4000) SHR 15) AND $FF];
END;

FUNCTION al_fixasin (x: AL_FIXED): AL_FIXED; 
BEGIN
  IF (-65536 > x) OR (x > 65536) THEN
    al_fixasin := 0
  ELSE
    al_fixasin := $00400000 - _acos_tbl[(x + 65536 + 127) SHR 8];
END;

FUNCTION al_fixacos (x: AL_FIXED): AL_FIXED;
BEGIN
  IF (-65536 > x) OR (x > 65536) THEN
    al_fixacos := 0
  ELSE
    al_fixacos := _acos_tbl[(x + 65536 + 127) SHR 8];
END;



{$ELSE}{ This patch was provided by Seoane. }



  function tabla(Base: TBLptr; Offset: Integer): AL_FIXED;
  begin
    inc(Base,Offset);
    tabla := Base^;
  end;


FUNCTION al_fixsin (x: AL_FIXED): AL_FIXED;
BEGIN
  al_fixsin := tabla (_cos_tbl, ((x - $400000 + $4000) SHR 15) AND $1FF);
END;

FUNCTION al_fixcos (x: AL_FIXED): AL_FIXED;
BEGIN
  al_fixcos := tabla (_cos_tbl, ((x + $4000) SHR 15) AND $1FF);
END;

FUNCTION al_fixtan (x: AL_FIXED): AL_FIXED;
BEGIN
  al_fixtan := tabla (_tan_tbl, ((x + $4000) SHR 15) AND $FF);
END;

FUNCTION al_fixasin (x: AL_FIXED): AL_FIXED; 
BEGIN
  IF (-65536 > x) OR (x > 65536) THEN
    al_fixasin := 0
  ELSE
    al_fixasin := $00400000 - tabla (_acos_tbl, (x + 65536 + 127) SHR 8);
END;

FUNCTION al_fixacos (x: AL_FIXED): AL_FIXED;
BEGIN
  IF (-65536 > x) OR (x > 65536) THEN
    al_fixacos := 0
  ELSE
    al_fixacos := tabla (_acos_tbl, (x + 65536 + 127) SHR 8);
END;

{$ENDIF}

END.
