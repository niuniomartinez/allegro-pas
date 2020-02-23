UNIT Effects;
(*<Implements different effects. *)
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

  CONST
  (* Explosion size. *)
    EXP_SIZE = 13;
  (* Fire length. *)
    FIRE_LENGTH = 23;
  (* Fire life. *)
    FIRE_LIFE = 14;

  TYPE
  (* An explosion. *)
    TExplosion = CLASS (TSprite)
    PRIVATE
      fCnt: INTEGER;
    PUBLIC
    (* Restores the sprite. *)
      PROCEDURE Restore; OVERRIDE;
    (* Updates the explosion. *)
      PROCEDURE Update; OVERRIDE;
    (* Draws the explosion. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* The fire sprite. *)
    TFire = CLASS (TMoveableSprite)
    PRIVATE
      fvx, fvy, { Vector components.  Temporary. }
      fCount: INTEGER;
    PUBLIC
    (* Somebody shooted a fire. *)
      PROCEDURE Shoot (aX, aY, aAngle: INTEGER);
    (* Updates sprite. *)
      PROCEDURE Update; OVERRIDE;
    (* Tests collision with sprite.

       Since @italic(Fire) is a line, collision calculation is slightly
       different. *)
      FUNCTION IntersectWith (aSpr: TSprite): BOOLEAN; OVERRIDE;
    END;



  (* The effects manager. *)
    TEffectManager = CLASS (TManager)
    PRIVATE
      fExplosion: TExplosion;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Creates an explosion. *)
      PROCEDURE AddAsteroidExplosion (CONST aX, aY: INTEGER);
    (* Initializes. *)
      PROCEDURE Initialize; OVERRIDE;
    (* New game. *)
      PROCEDURE NewGame; OVERRIDE;
    (* Updates effects. *)
      PROCEDURE Update; OVERRIDE;
    (* Draw sprites. *)
      PROCEDURE Paint; OVERRIDE;
    END;

IMPLEMENTATION

  USES
    Graphics,
    Allegro5, al5primitives, math;

  CONST
  (* How the explosion grows. *)
    EXP_RATE = 1.7;

(*
 * TExplosion
 ***************************************************************************)

(* Restores. *)
  PROCEDURE TExplosion.Restore;
  BEGIN
    SELF.Visible := TRUE;
    SELF.Active := TRUE;
    Polygon.Color := Yelow;
    fCnt := 0
  END;



(* Updates. *)
  PROCEDURE TExplosion.Update;
  BEGIN
    IF SELF.Active THEN
    BEGIN
      INC (fCnt);
      IF fCnt > EXP_SIZE THEN SELF.Suspend;
    END;
  END;



(* Draws. *)
  PROCEDURE TExplosion.Draw;
  VAR
    Size: REAL;
    TransformationMatrix: ALLEGRO_TRANSFORM;
  BEGIN
    IF SELF.Visible THEN
    BEGIN
      Size := EXP_SIZE + power (EXP_RATE, 1 + fCnt);

      al_build_transform (TransformationMatrix, PosX, PosY, 1, 1, 0);
      al_use_transform (TransformationMatrix);

      al_draw_line (-EXP_SIZE, -Size, EXP_SIZE, -Size, Polygon.Color, 0);
      al_draw_line (-EXP_SIZE,  Size, EXP_SIZE,  Size, Polygon.Color, 0);
      al_draw_line (-Size, -EXP_SIZE, -Size, EXP_SIZE, Polygon.Color, 0);
      al_draw_line ( Size, -EXP_SIZE,  Size, EXP_SIZE, Polygon.Color, 0)
    END
  END;



(*
 * TFire
 ***************************************************************************)

(* Somebody shooted a fire. *)
  PROCEDURE TFire.Shoot (aX, aY, aAngle: INTEGER);
  BEGIN
    SELF.Initialize;

    fvx := TRUNC (FIRE_LENGTH * cos (aAngle));
    fvy := TRUNC (FIRE_LENGTH * sin (aAngle));
    SELF.PosX := aX; SELF.PosY := aY;

    SELF.Polygon.AddPoint (0, 0);
    SELF.Polygon.AddPoint (fvx, fvy);
    SELF.Polygon.AddPoint (0, 0); { Not sure this is needed. }
    SELF.Polygon.Fill := FALSE;
    fCount := 0
  END;



(* Updates. *)
  PROCEDURE TFire.Update;
  BEGIN
    IF SELF.Active THEN
    BEGIN
      INC (fCount);
      IF fCount >= FIRE_LIFE THEN SELF.Suspend ELSE INHERITED Update
    END
  END;



(* Collision. *)
  FUNCTION TFire.IntersectWith (aSpr: TSprite): BOOLEAN;
  VAR
    midptx, midpty: INTEGER;
  BEGIN
    midptx := TRUNC (PosX + PosX + fvx) DIV 2;
    midpty := TRUNC (PosY + PosY + fvy) DIV 2;
    RESULT := SELF.Active AND aSpr.Active
      AND (ABS (aSpr.PosX - midptx  + 2) < aSpr.Radius)
      AND (ABS (aSpr.PosY - midpty  + 2) < aSpr.Radius)
  END;



(*
 * TEffectManager
 ***************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TEffectManager.Create;
  BEGIN
    INHERITED Create;
    fExplosion := TExplosion.Create
  END;



(* Destructor. *)
  DESTRUCTOR TEffectManager.Destroy;
  BEGIN
    fExplosion.Free;
    INHERITED Destroy
  END;



(* Adds an explosion. *)
  PROCEDURE TEffectManager.AddAsteroidExplosion (CONST aX, aY: INTEGER);
  BEGIN
    fExplosion.Restore;
    fExplosion.PosX := aX; fExplosion.PosY := aY
  END;



(* Initializes. *)
  PROCEDURE TEffectManager.Initialize;
  BEGIN
    fExplosion.Suspend;
  END;



(* New game. *)
  PROCEDURE TEffectManager.NewGame;
  BEGIN
    fExplosion.Suspend;
  END;



(* Updates. *)
  PROCEDURE TEffectManager.Update;
  BEGIN
    fExplosion.Update
  END;



(* Draws. *)
  PROCEDURE TEffectManager.Paint;
  BEGIN
    fExplosion.Draw
  END;

END.

