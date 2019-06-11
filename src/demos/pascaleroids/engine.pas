UNIT Engine;
(*< Defines base classes for different parts of the game. *)
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
    Graphics,
    Allegro5;

  CONST
  (* Arena width. *)
    MAX_WIDTH = 640;
  (* Arena height. *)
    MAX_HEIGHT = 480;

  TYPE
  (* Base class for sprites.

     In this game, sprites origin is at the center. *)
    TSprite = CLASS (TObject)
    PRIVATE
      fVisible, fActive: BOOLEAN;
      fPx, fPy, fAngle, fRadius: SINGLE;
      fColor: ALLEGRO_COLOR;

      FUNCTION GetFill: BOOLEAN;
      PROCEDURE SetFill (CONST v: BOOLEAN);
    PROTECTED
      fPolygon: TPolygon;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Initializes the sprite.

       This will reset the polygon and set @link(Visible) and @link(Active) to
       @true. *)
      PROCEDURE Initialize; VIRTUAL;
    (* Suspends sprite activity. *)
      PROCEDURE Suspend;
    (* Restores sprite. *)
      PROCEDURE Restore;

    (* Draws sprite. *)
      PROCEDURE Draw;
    (* Updates the sprite. *)
      PROCEDURE Update; VIRTUAL; ABSTRACT;

    (* Test collision with other sprite. *)
      FUNCTION IntersectWith (aSpr: TSprite): BOOLEAN; VIRTUAL;

    (* Tells if sprite is visible.  Note that non visible sprites would be
       updated and collide. @seealso(Active) *)
      PROPERTY Visible: BOOLEAN READ fVisible WRITE fVisible;
    (* Tells if sprite is active.  This means that the sprite will be updated
       and collisions will be checked. *)
      PROPERTY Active: BOOLEAN READ fActive WRITE fActive;
    (* Sprite horizontal position. *)
      PROPERTY PosX: SINGLE READ fPx WRITE fPx;
    (* Sprite vertical position. *)
      PROPERTY PosY: SINGLE READ fPy WRITE fPy;
    (* Rotation angle. *)
      PROPERTY Angle: SINGLE READ fAngle WRITE fAngle;
    (* Object radius.  Used in collision detection. *)
      PROPERTY Radius: SINGLE READ fRadius WRITE fRadius;
    (* Sprite color. *)
      PROPERTY Color: ALLEGRO_COLOR READ fColor WRITE fColor;
    (* Tells if sprite is filled (@true) or wireframe (@false). *)
      PROPERTY Fill: BOOLEAN READ GetFill WRITE SetFill;
    END;



  (* A sprite that moves in a continuous motion. *)
    TMoveableSprite = CLASS (TSprite)
    PRIVATE
      fVx, fVy, fVr: SINGLE;
    PUBLIC
    (* Updates the sprite.

       Just moves the sprite in the given velocities. *)
      PROCEDURE Update; OVERRIDE;

    (* Velocity in the X axis. *)
      PROPERTY Vx: SINGLE READ fVx WRITE fVx;
    (* Velocity in the Y axis. *)
      PROPERTY Vy: SINGLE READ fVy WRITE fVy;
    (* Rotation velocity. *)
      PROPERTY Vr: SINGLE READ fVr WRITE fVr;
    END;



  (* Base class for managers.

     Managers manages parts of the game (i.e. player, asteroids, effects...)
     and are created and used by the @link(TGameManager). *)
    TManager = CLASS (TObject)
    PUBLIC
    (* Initializes the manager.  It is called when game starts. *)
      PROCEDURE Initialize; VIRTUAL; ABSTRACT;
    (* Updates the content.  It is called once per frame. *)
      PROCEDURE Update; VIRTUAL; ABSTRACT;
    (* Paints the content.  It is called when screen needs to be updated. *)
      PROCEDURE Paint; VIRTUAL; ABSTRACT;
    END;

IMPLEMENTATION

(*
 * TSprite
 ***************************************************************************)

  FUNCTION TSprite.GetFill: BOOLEAN;
  BEGIN
    RESULT := fPolygon.Fill
  END;



  PROCEDURE TSprite.SetFill (CONST v: BOOLEAN);
  BEGIN
    fPolygon.Fill := v
  END;



(* Constructor. *)
  CONSTRUCTOR TSprite.Create;
  BEGIN
    INHERITED Create;
    fPolygon.Reset;
    fRadius := 10;
    SELF.Restore
  END;



(* initializes sprite. *)
  PROCEDURE TSprite.Initialize;
  BEGIN
    SELF.Restore;
    fPolygon.Reset
  END;



(* Suspends sprite. *)
  PROCEDURE TSprite.Suspend;
  BEGIN
    SELF.Visible := FALSE;
    SELF.Active := FALSE
  END;



(* Restores sprite. *)
  PROCEDURE TSprite.Restore;
  BEGIN
    SELF.Visible := TRUE;
    SELF.Active := TRUE
  END;



(* Draws sprite. *)
  PROCEDURE TSprite.Draw;
  VAR
    TransformationMatrix: ALLEGRO_TRANSFORM;
  BEGIN
    IF fVisible THEN
    BEGIN
      al_build_transform (TransformationMatrix, fPx, fPy, 1, 1, fAngle);
      al_use_transform (TransformationMatrix);
      fPolygon.Draw
    END;
  END;



(* Test collision with other sprite. *)
  FUNCTION TSprite.IntersectWith (aSpr: TSprite): BOOLEAN;
  BEGIN
    RESULT := fActive AND aSpr.fActive
          AND (ABS (aSpr.fPx - fPx) < fRadius)
          AND (ABS (aSpr.fPy - fPy) < fRadius)
  END;



(*
 * TMoveableSprite
 ***************************************************************************)

(* Updates sprite. *)
  PROCEDURE TMoveableSprite.Update;
  BEGIN
    IF fActive THEN
    BEGIN
    { Position. }
      fPx := fPx + fVx;
      fPy := fPy + fVy;
    { If center is outside the right side, move to the left side. }
      IF fPx - fRadius > MAX_WIDTH THEN fPx := fPx - (MAX_WIDTH + 2 * fRadius);
    { If center is outside the left side, move to the right side. }
      IF fPx < -fRadius THEN fPx := fPx + (MAX_WIDTH + 2 * fRadius);
    { If center is ouside the bottom side, move to the top. }
      IF fPy - fRadius > MAX_HEIGHT THEN fPY := fPy - (MAX_HEIGHT + 2 * fRadius);
    { If center is outside the top side, move to the bottom side. }
      IF fPy < -fRadius THEN fPy := fPy + (MAX_HEIGHT + 2 * fRadius);
    { Rotation. }
      fAngle := fAngle + fVr
    END
  END;

END.

