UNIT Player;
(*< Player stuff. *)
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
    Engine, GameMath;

  CONST
  (* Rotation amount. *)
    ROT_RATE = 15 * DEG_TO_RAD;
  (* Acceleration (thrust). *)
    ACCEL_FACTOR = 1;

  TYPE
  (* The player's ship. *)
    TShip = CLASS (TMoveableSprite)
    PUBLIC
    (* Initializes the sprite. *)
      PROCEDURE Initialize; OVERRIDE;

    (* Rotates the ship to the right. *)
      PROCEDURE RotateRight;
    (* Rotates the ship to the left. *)
      PROCEDURE RotateLeft;
    (* Ship thrust. *)
      PROCEDURE Thrust;
    END;

IMPLEMENTATION

  USES
    Graphics;

(*
 * TShip
 ***************************************************************************)

(* Initializes the sprite. *)
  PROCEDURE TShip.Initialize;
  CONST
    tpx: ARRAY [0..3] OF INTEGER = (14, -7, -7, 13);
    tpy: ARRAY [0..3] OF INTEGER = ( 0, -7,  7,  0);
    SIZE = 13;
  BEGIN
    SELF.Polygon.Color := Graphics.Yelow;
    INHERITED Initialize;
    SELF.Polygon.SetVertices (tpx, tpy);
    SELF.Polygon.Fill := FALSE;
    SELF.Radius := SIZE;
  { Start position. }
    SELF.PosX := MAX_WIDTH / 2; SELF.PosY := MAX_HEIGHT / 2;
    SELF.Vr := 0
  END;



(* Rotates the ship. *)
  PROCEDURE TShip.RotateRight;
  BEGIN
    SELF.Angle := SELF.Angle + ROT_RATE
  END;

  PROCEDURE TShip.RotateLeft;
  BEGIN
    SELF.Angle := SELF.Angle - ROT_RATE
  END;



(* Ship thrust. *)
  PROCEDURE TShip.Thrust;
  BEGIN
    SELF.Vx := SELF.Vx + cos (SELF.Angle) * ACCEL_FACTOR;
    SELF.Vy := SELF.Vy + sin (SELF.Angle) * ACCEL_FACTOR
  END;

END.

