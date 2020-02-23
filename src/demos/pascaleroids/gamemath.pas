UNIT GameMath;
(*< Defines some helpful math stuff. *)
(*
  Copyright (c) 2019 Guillermo Martínez J.

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

INTERFACE

  USES
    Allegro5;

  CONST
  (* Multiply with degrees (0..360) to get radians. *)
    DEG_TO_RAD = ALLEGRO_TAU / 360;
  (* Multiply with radians to get degrees (0..360). *)
    RAD_TO_DEG = 1 / DEG_TO_RAD;

(* Returns a random number in the given interval. *)
  FUNCTION GetRandomNumber (CONST aMin, aMax: INTEGER): SINGLE;

IMPLEMENTATION

(* Returns a random number in the given interval. *)
  FUNCTION GetRandomNumber (CONST aMin, aMax: INTEGER): SINGLE;
  BEGIN
    RESULT := aMin + (Random * (aMax - aMin + 1))
  END;

END.

