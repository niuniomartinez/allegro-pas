UNIT Asteroids;
(*< Defines all asteroid stuff. *)
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
    Engine;

  TYPE
  (* Defines an asteroid. *)
    TAsteroid = CLASS (TMoveableSprite)
    PUBLIC
    (* Initializes the asteroid.

       This generates a new asteroid shape, sets its initial position and
       velocity. *)
      PROCEDURE Initialize; OVERRIDE;
    END;

IMPLEMENTATION

   USES
     GameMath, Graphics,
     Allegro5;

(*
 * TAsteroid
 ***************************************************************************)

(* Initializes. *)
  PROCEDURE TAsteroid.Initialize;
  CONST
    MIN_RADIUS = 75;
  VAR
    lAngle, lValue: SINGLE;
    Count: INTEGER;
  BEGIN
    fPolygon.Color := al_map_rgb (Random (256), Random (256), Random (256));
    fPolygon.Fill := TRUE;
    INHERITED Initialize;
    SELF.PosX := MAX_WIDTH / 2; SELF.PosY := MAX_HEIGHT / 2;
    SELF.Radius := MIN_RADIUS;
    FOR Count := 1 TO MAX_POINTS DO
    BEGIN
      lAngle := (Count - 1) * ((360 / MAX_POINTS) * DEG_TO_RAD);
      lValue := Random (25) + (MIN_RADIUS - 25);
      fPolygon.AddPoint (cos (lAngle) * lValue, sin (lAngle) * lValue)
    END;

    SELF.Vr := ((Random - 0.5) * DEG_TO_RAD) * 3;
    SELF.Vx := Random (10) - 5; SELF.Vy := Random (10) - 5
  END;

END.

