(* Fixed point maths. *)
UNIT alfixed;

(* TODO: Doesn't check overflows. *)

{ Defines the frame. }
{$MODE Delphi}
{$PACKRECORDS C}
{$H+}



INTERFACE

TYPE
(* This is a fixed point integer which can replace float with similar results
   and is faster than float on low end machines. *)
  AL_FIXED	= LONGINT;



(* Converts an integer to fixed point. *)
  FUNCTION al_itofix (x: LONGINT): AL_FIXED;

(* Converts fixed point to integer, rounding as required to the nearest
   integer. *)
  FUNCTION al_fixtoi (x: AL_FIXED): LONGINT;

(* Converts a floating point value to fixed point. *)
  FUNCTION al_ftofix (x: REAL): AL_FIXED;

(* Converts fixed point to floating point. *)
  FUNCTION al_fixtof (x: AL_FIXED): REAL;

(* Safe function to add fixed point numbers clamping overflow. *)
  FUNCTION al_fixadd (x, y: AL_FIXED): AL_FIXED;

(* Safe function to subtract fixed point numbers clamping underflow. *)
  FUNCTION al_fixsub (x, y: AL_FIXED): AL_FIXED;

(* Multiplies two fixed point values together. *)
  FUNCTION al_fixmul (x, y: AL_FIXED): AL_FIXED;

(* Fixed point division. *)
  FUNCTION al_fixdiv (x, y: AL_FIXED): AL_FIXED;

(* This finds out the non negative square root of `x'. *)
  FUNCTION al_fixsqrt (x: AL_FIXED): AL_FIXED;



(* Trigonometry. *)
CONST
(* This constant gives a ratio which can be used to convert a fixed point
   number in binary angle format to a fixed point number in radians. *)
  al_fixtorad: AL_FIXED = 1608;	   { al_ftofix (2pi/256) }

(* This constant gives a ratio which can be used to convert a fixed point
   number in radians to a fixed point number in binary angle format. *)
  al_radtofix: AL_FIXED = 2670177; { al_ftofix (256/2pi) }

(* This function finds the sine of a value using a lookup table. *)
  FUNCTION al_fixsin (x: AL_FIXED): AL_FIXED;

(* This function finds the cosine of a value using a lookup table. *)
  FUNCTION al_fixcos (x: AL_FIXED): AL_FIXED;
(* This function finds the tangent of a value using a lookup table. *)
  FUNCTION al_fixtan (x: AL_FIXED): AL_FIXED;

(* This function finds the inverse sine of a value using a lookup table. *)
  FUNCTION al_fixasin (x: AL_FIXED): AL_FIXED;

(* This function finds the inverse cosine of a value using a lookup table. *)
  FUNCTION al_fixacos (x: AL_FIXED): AL_FIXED;

VAR
(* This function finds the inverse tangent of a value using a lookup table. *)
  al_fixatan: FUNCTION (x: AL_FIXED): AL_FIXED; CDECL;

(* This is a fixed point version of the libc atan2 () routine. *)
  al_fixatan2: FUNCTION (x, y: AL_FIXED): AL_FIXED; CDECL;



IMPLEMENTATION

USES
  albase,
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

{ This patch was provided by Seoane. }



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



INITIALIZATION
{ Get table address. }
  _cos_tbl := al_get_object_address ('_cos_tbl');
  _tan_tbl := al_get_object_address ('_tan_tbl');
  _acos_tbl:= al_get_object_address ('_acos_tbl');
END.
