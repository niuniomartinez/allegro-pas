PROGRAM exfixed;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	This program demonstrates how to use fixed point numbers, which are signed
 *	32-bit integers storing the integer part in the upper 16 bits and the decimal
 *	part in the 16 lower bits.  This example also uses the unusual approach of
 *	communicating with the user exclusively via the al_message procedure.
 *
 *	by Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

USES
  sysutils,
{ It needs some Allegro.pas units. }
  allegro,
  alfixed;  { Fixed point. }



VAR
(* Declare three 32 bit (16.16) fixed point variables. *)
  x, y, z: AL_FIXED;
BEGIN { The program starts here. }

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Error initializing Allegro!');
    EXIT;
  END;

{ Convert integers to fixed point like this. }
  x := al_itofix (100);

{ Convert floating point to fixed point like this. }
  y := al_ftofix (3.14);

{ Fixed point variables can be assigned, added, subtracted, negated,
  and compared just like integers, eg: }
  z := x + y;
  al_message (Format ('%f + %f = %f',
	  [al_fixtof (x), al_fixtof (y), al_fixtof (z)]));

{ You can't add integers or floating point to fixed point, though:
  z := x + 3;
  would give the wrong result. }

{ Fixed point variables can be multiplied or divided by integers or
  floating point numbers, eg: }
  z := y * 2;
  al_message (Format ('%f * 2 = %f', [al_fixtof (y), al_fixtof (z)]));

{ You can't multiply or divide two fixed point numbers, though:
  z := x * y;
  would give the wrong result.  Use al_fixmul and al_fixdiv instead, eg: }
  z := al_fixmul (x, y);
  
  al_message (Format ('%f * %f = %f',
	  [al_fixtof (x), al_fixtof (y), al_fixtof (z)]));

{ Fixed point trig and square root are also available, eg: }
  z := al_fixsqrt (x);
  al_message (Format ('al_fixsqrt (%f) = %f', [al_fixtof (x), al_fixtof (z)]));

{ End of the program. }
END.
