program ex_fixed;
(*
 *    Example program for the Allegro library, by Shawn Hargreaves.
 *
 *    This program demonstrates how to use fixed point numbers, which
 *    are signed 32-bit integers storing the integer part in the
 *    upper 16 bits and the decimal part in the 16 lower bits.
 *)
(*
  Copyright (c) 2012-2024 Guillermo Martínez J.

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

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

uses
  Allegro5, sysutils;

(* Helper routine for text output. *)
  procedure PrintF (const aFmt: String; aValues: array of const);
  begin
    WriteLn (format (aFmt, aValues))
  end;

var
(* Declare three 32 bit (16.16) fixed point variables *)
  x, y, z1, z2, z3, z4: AL_FIXED;
begin
  if not al_init then
  begin
    WriteLn ('Could not init Allegro.');
    Halt (1)
  end;
{ convert integers to fixed point like this. }
  x := al_itofix (10);
{ convert floating point to fixed point like this. }
  y := al_ftofix (ALLEGRO_TAU);
{ fixed point variables can be assigned, added, subtracted, negated,
  and compared just like integers, eg: }
  z1 := x + y;
{ you can't add integers or floating point to fixed point, though:
    z := x + 3;
  would give the wrong result. }
{ fixed point variables can be multiplied or divided by integers or
  floating point numbers, eg: }
  z2 := y div 2;
{ you can't multiply or divide two fixed point numbers, though:
    z := x * y;
  would give the wrong result. Use fixmul() and fixdiv() instead, eg: }
  z3 := al_fixmul (x, y);
{ fixed point trig and square root are also available, eg: }
  z4 := al_fixsqrt (x);

  PrintF ('%f + %f = %f', [al_fixtof (x), al_fixtof (y), al_fixtof (z1)]);
  PrintF ('%f / 2 = %f',  [al_fixtof (y), al_fixtof (z2)]);
  PrintF ('%f * %f = %f', [al_fixtof (x), al_fixtof (y), al_fixtof (z3)]);
  PrintF ('al_fixsqrt (%f) = %f', [al_fixtof (x), al_fixtof (z4)]);
  Write ('Press [Intro] to continue...');
  ReadLn
end.
