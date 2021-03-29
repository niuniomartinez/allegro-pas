unit Player;
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

interface

  uses
    Engine, GameMath;

  const
  (* Rotation amount. *)
    ROT_RATE = 15 * DEG_TO_RAD;
  (* Acceleration (thrust). *)
    ACCEL_FACTOR = 1;

  type
  (* The player's ship. *)
    TShip = class (TMoveableSprite)
    public
    (* Initializes the sprite. *)
      procedure Initialize; override;

    (* Rotates the ship to the right. *)
      procedure RotateRight;
    (* Rotates the ship to the left. *)
      procedure RotateLeft;
    (* Ship thrust. *)
      procedure Thrust;
    end;

implementation

  uses
    Graphics;

(*
 * TShip
 ***************************************************************************)

(* Initializes the sprite. *)
  procedure TShip.Initialize;
  const
    tpx: array [0..3] of Integer = (14, -7, -7, 13);
    tpy: array [0..3] of Integer = ( 0, -7,  7,  0);
    SIZE = 13;
  begin
    Self.Polygon.Color := Graphics.Yelow;
    inherited Initialize;
    Self.Polygon.SetVertices (tpx, tpy);
    Self.Polygon.Fill := False;
    Self.Radius := SIZE;
  { Start position. }
    Self.PosX := MAX_WIDTH / 2; Self.PosY := MAX_HEIGHT / 2;
    Self.Vr := 0
  end;



(* Rotates the ship. *)
  procedure TShip.RotateRight;
  begin
    Self.Angle := Self.Angle + ROT_RATE
  end;

  procedure TShip.RotateLeft;
  begin
    Self.Angle := Self.Angle - ROT_RATE
  end;



(* Ship thrust. *)
  procedure TShip.Thrust;
  begin
    Self.Vx := Self.Vx + cos (Self.Angle) * ACCEL_FACTOR;
    Self.Vy := Self.Vy + sin (Self.Angle) * ACCEL_FACTOR
  end;

end.

