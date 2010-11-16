UNIT alfixed;
(*<Defines a fixed point 16.16 type and math functions.

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
  arrays, wich would be faster than floating point, and use bitmasks, wich
  are useless with floating point types.

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
  circle, eliminating all those tiresome @code(IF angle >= 360) checks. *)

{$include _alcfg.inc}

INTERFACE

TYPE
(* This is a fixed point integer which can replace float with similar results
   and is faster than float on low end machines. *)
  AL_FIXED	= LONGINT;

(* Converts an integer to fixed point.  This is the same thing as @code(x SHL
   16).  Remember that overflows (trying to convert an integer greater than
   32767) and underflows (trying to convert an integer lesser than -32768) are
   not detected! The values simply "wrap around".
   @seealso(al_fixtoi) @seealso(al_fixtof) @seealso(al_ftofix) *)
  FUNCTION al_itofix (x: LONGINT): AL_FIXED; INLINE;

(* Converts fixed point to integer.
   @seealso(al_itofix) @seealso(al_fixtof) @seealso(al_ftofix) *)
  FUNCTION al_fixtoi (x: AL_FIXED): LONGINT; INLINE;

(* Converts a floating point value to fixed point.  Unlike @code(al_itofix),
   this function clamps values which could overflow the type conversion,
   setting @code(al_errno) to non-zero in the process if this happens.
   @seealso(al_itofix) @seealso(al_fixtoi) @seealso(al_fixtof) *)
  FUNCTION al_ftofix (x: REAL): AL_FIXED; INLINE;

(* Converts fixed point to floating point.
   @seealso(al_itofix) @seealso(al_fixtoi) @seealso(al_ftofix) *)
  FUNCTION al_fixtof (x: AL_FIXED): REAL; INLINE;



IMPLEMENTATION

USES
  allegro;



(* Conversion. *)
  FUNCTION al_itofix (x: LONGINT): AL_FIXED;
  BEGIN
    al_itofix := x SHL 16;
  END;

  FUNCTION al_fixtoi (x: AL_FIXED): LONGINT;
  BEGIN
  { SHR doesn't keep the sign bit, so... }
    IF x < 0 THEN
      al_fixtoi := (x SHR 16) OR $FFFF0000
    ELSE
      al_fixtoi := x SHR 16;
  { TOD: What about an assembler version? }
  END;

  FUNCTION al_ftofix (x: REAL): AL_FIXED;
  BEGIN
    IF (x > 32767.0) OR (x < -32767.0) THEN
    BEGIN
    { An "infinite" value. }
      al_ftofix := $7FFFFFFF;
    { TODO: Multiple languages as the original Allegro. }
      al_error := 'Overflow converting to fixed point.';
    END
    ELSE IF x < 0 THEN
      al_ftofix := TRUNC (x * 65536 - 0.5)
    ELSE
      al_ftofix := TRUNC (x * 65536 + 0.5);
  END;

  FUNCTION al_fixtof (x: AL_FIXED): REAL;
  BEGIN
    al_fixtof := x / 65536.0;
  END;

END.
