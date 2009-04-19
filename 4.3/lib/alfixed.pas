UNIT alfixed;
(*<Fixed point math routines

  Allegro provides some routines for working with fixed point numbers, and
  defines the type @link(AL_FIXED) to be a signed 32-bit integer.  The high
  word is used for the integer part and the low word for the fraction, giving a
  range of -32768 to 32767 and an accuracy of about four or five decimal
  places.  Fixed point numbers can be assigned, compared, added, subtracted,
  negated and shifted (for multiplying or dividing by powers of two) using the
  normal integer operators, but you should take care to use the appropriate
  conversion routines when mixing fixed point with integer or floating point
  values. Writing @code(fixed_point_1 + fixed_point_2) is 0K, but
  @code(fixed_point + integer) is not.

  One of the advantage of fixed point math routines is that you don't require a
  floating point coprocessor to use them.  This was great in the time period of
  i386 and i486 machines, but stopped being so useful with the coming of the
  Pentium class of processors.  From Pentium onwards, CPUs have increased their
  strength in floating point operations, equaling or even surpassing integer
  math performance.  Other advantage is the use of fixed point indexes to
  arrays, wich would be faster than floating point.

  Depending on the type of operations your program may need, using floating
  point types may be faster than fixed types if you are targeting a specific
  machine class.  Allegro comes with a test program in the `allegro/tests'
  directory.  Its `Misc' menu contains a basic profile test which can give you
  an idea of the speed difference between fixed and float types for a few basic
  operations on your machine.  However, don't forget to profile your program in
  real life conditions, tight loop benchmarks are after all artificial.

  The fixed point square root, sin, cos, tan, inverse sin, and inverse cos
  functions are implemented using lookup tables, which are very fast but not
  particularly accurate.  At the moment the inverse tan uses an iterative
  search on the tan table, so it is a lot slower than the others.  Note that on
  machines with very good floating point processors using these functions could
  be slower in real life code due to cache misses:  it may be faster to wait a
  few extra cicles for a floating point sine result rather than wait for the
  CPU to fetch the precalculated table from main memory.  Always profile your
  code.

  Angles are represented in a binary format with 256 equal to a full circle, 64
  being a right angle and so on.  This has the advantage that a simple bitwise
  @code(AND) can be used to keep the angle within the range zero to a full
  circle, eliminating all those tiresome @code(IF angle >= 360) checks.

  Fixed point math is considered "add-on" material and is kept only for
  backwards compatibility.  Whenever a future release of Allegro breaks
  backwards compatibility, fixed point math will likely be moved to a separate
  add-on package for the very few users who still find it convenient and
  useful, and Allegro functions using fixed point math will use other types. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$MODE FPC}
{$ENDIF}

INTERFACE

TYPE
(* This is a fixed point integer which can replace float with similar results
   and is faster than float on low end machines. *)
  AL_FIXED	= LONGINT;



(* Converts an integer to fixed point.  This is the same thing as @code(x SHL
   16).  Remember that overflows (trying to convert an integer greater than
   32767) and underflows (trying to convert an integer lesser than -32768) are
   not detected even in debug builds! The values simply "wrap around".  *)
  FUNCTION al_itofix (x: LONGINT): AL_FIXED;

(* Converts fixed point to integer, rounding as required to the nearest
   integer. *)
  FUNCTION al_fixtoi (x: AL_FIXED): LONGINT;

(* Converts a floating point value to fixed point.  Unlike @link(al_itofix),
   this function clamps values which could overflow the type conversion,
   setting @link(al_errno) to non-zero in the process if this happens. *)
  FUNCTION al_ftofix (x: REAL): AL_FIXED;

(* Converts fixed point to floating point. *)
  FUNCTION al_fixtof (x: AL_FIXED): REAL;



(* Safe function to add fixed point numbers clamping overflow.

   Although fixed point numbers can be added with the normal '+' integer
   operator, that doesn't provide any protection against overflow.  If overflow
   is a problem, you should use this function instead.  It is slower than using
   integer operators, but if an overflow occurs it will set @link(al_errno)
   and clamp the result, rather than just letting it wrap.*)
  FUNCTION al_fixadd (x, y: AL_FIXED): AL_FIXED;

(* Safe function to subtract fixed point numbers clamping underflow.

   Although fixed point numbers can be substracted with the normal 'x' integer
   operator, that doesn't provide any protection against overflow.  If overflow
   is a problem, you should use this function instead.  It is slower than using
   integer operators, but if an overflow occurs it will set @link(al_errno)
   and clamp the result, rather than just letting it wrap.*)
  FUNCTION al_fixsub (x, y: AL_FIXED): AL_FIXED;

(* A fixed point value can be multiplied or divided by an integer with the
   normal `*' and `/' operators.  To multiply two fixed point values, though,
   you must use this function.

   If an overflow occurs, @link(al_errno) will be set and the maximum possible
   value will be returned, but @code(al_errno) is not cleared if the operation
   is successful.  This means that if you are going to test for overflow you
   should set @code(al_errno := 0) before calling @code(al_fixmul). *)
  FUNCTION al_fixmul (x, y: AL_FIXED): AL_FIXED;

(* A fixed point value can be divided by an integer with the normal `/' and
   @code(DIV) operators.  To divide two fixed point values, though, you must
   use this function.

   If an overflow occurs, @link(al_errno) will be set and the maximum possible
   value will be returned, but @code(al_errno) is not cleared if the operation
   is successful.  This means that if you are going to test for overflow you
   should set @code(al_errno := 0) before calling @code(al_fixdiv). *)
  FUNCTION al_fixdiv (x, y: AL_FIXED): AL_FIXED;

(* This finds out the non negative square root of `x'.  If `x' is negative,
   @link(al_errno) is set to @code(EDOM) and the function returns zero. *)
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
  allegro; { For al_errno. }



(* Access to trigonometry tables. *)
TYPE
  TBL = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_FIXED;

VAR
  _cos_tbl: TBL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  _tan_tbl: TBL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  _acos_tbl: TBL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;



(* Conversion. *)
FUNCTION al_itofix (x: LONGINT): AL_FIXED;
BEGIN
  al_itofix := x SHL 16;
END;

FUNCTION al_fixtoi (x: AL_FIXED): LONGINT;
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
      al_errno := 34; { ERANGE }
      R := -$7FFFFFFF;
    END;
  END
  ELSE BEGIN
    IF (x > 0) AND (y > 0) THEN
    BEGIN
      al_errno := 34; { ERANGE }
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
      al_errno := 34; { ERANGE }
      R := -$7FFFFFFF;
    END;
  END
  ELSE BEGIN
    IF (x > 0) AND (y < 0) THEN
    BEGIN
      al_errno := 34; { ERANGE }
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
    al_errno := 34; { ERANGE }
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
  al_fixtan := _tan_tbl [((x + $4000) SHR 15) AND $FF];
END;

FUNCTION al_fixasin (x: AL_FIXED): AL_FIXED; 
BEGIN
  IF (-65536 > x) OR (x > 65536) THEN
    al_fixasin := 0
  ELSE
    al_fixasin := $00400000 - _acos_tbl [(x + 65536 + 127) SHR 8];
END;

FUNCTION al_fixacos (x: AL_FIXED): AL_FIXED;
BEGIN
  IF (-65536 > x) OR (x > 65536) THEN
    al_fixacos := 0
  ELSE
    al_fixacos := _acos_tbl [(x + 65536 + 127) SHR 8];
END;

END.
