unit Engine;
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

interface

  uses
    Graphics,
    Allegro5;

  const
  (* Arena width. *)
    MAX_WIDTH = 640;
  (* Arena height. *)
    MAX_HEIGHT = 480;

  type
  (* Base class for sprites.

     In this game, sprite origin is at the center. *)
    TSprite = class (TObject)
    private
      fVisible, fActive: Boolean;
      fPx, fPy, fAngle, fRadius: Single;
      fPolygon: TPolygon;
      fValue: Integer;
    protected
    (* Sprite polygon. *)
      property Polygon: TPolygon read fPolygon;
    public
    (* Initializes the sprite.

       This will reset the polygon and set @link(Visible) and @link(Active) to
       @true. *)
      procedure Initialize; virtual;
    (* Suspends sprite activity. *)
      procedure Suspend;
    (* Restores sprite. *)
      procedure Restore; virtual;
    (* Draws sprite. *)
      procedure Draw; virtual;
    (* Updates the sprite. *)
      procedure Update; virtual; abstract;
    (* Tests collision with other sprite. *)
      function IntersectWith (aSpr: TSprite): Boolean; virtual;

    (* Tells if sprite is visible.  Note that non visible sprites would be
       updated and collide. @seealso(Active) *)
      property Visible: Boolean read fVisible write fVisible;
    (* Tells if sprite is active.  This means that the sprite will be updated
       and collisions will be checked. *)
      property Active: Boolean read fActive write fActive;
    (* Sprite horizontal position. *)
      property PosX: Single read fPx write fPx;
    (* Sprite vertical position. *)
      property PosY: Single read fPy write fPy;
    (* Rotation angle. *)
      property Angle: Single read fAngle write fAngle;
    (* Object radius.  Used in collision detection. *)
      property Radius: Single read fRadius write fRadius;
    (* Score gained by destroying this sprite. *)
      property Value: Integer read fValue write fValue;
    end;



  (* A sprite that moves in a continuous motion. *)
    TMoveableSprite = class (TSprite)
    private
      fVx, fVy, fVr: Single;
    public
    (* Updates the sprite.

       Just moves the sprite in the given velocities. *)
      procedure Update; override;

    (* Velocity in the X axis. *)
      property Vx: Single read fVx write fVx;
    (* Velocity in the Y axis. *)
      property Vy: Single read fVy write fVy;
    (* Rotation velocity. *)
      property Vr: Single read fVr write fVr;
    end;



  (* Base class for managers.

     Managers manages parts of the game (i.e. player, asteroids, effects...)
     and are created and used by the @link(TGameManager). *)
    TManager = class (TObject)
    public
    (* Initializes the manager.  It is called when program starts. *)
      procedure Initialize; virtual; abstract;
    (* Starts a new game. *)
      procedure NewGame; virtual; abstract;
    (* Updates the content.  It is called once per frame. *)
      procedure Update; virtual; abstract;
    (* Paints the content.  It is called when screen needs to be updated. *)
      procedure Paint; virtual; abstract;
    end;

implementation

(*
 * TSprite
 ***************************************************************************)

(* Initializes sprite. *)
  procedure TSprite.Initialize;
  begin
    Self.Restore;
    fPolygon.Reset
  end;



(* Suspends sprite. *)
  procedure TSprite.Suspend;
  begin
    fVisible := False;
    fActive := False
  end;



(* Restores sprite. *)
  procedure TSprite.Restore;
  begin
    fVisible := True;
    fActive := True
  end;



(* Draws sprite. *)
  procedure TSprite.Draw;
  var
    TransformationMatrix: ALLEGRO_TRANSFORM;
  begin
    if fVisible then
    begin
      al_build_transform (TransformationMatrix, fPx, fPy, 1, 1, fAngle);
      al_use_transform (TransformationMatrix);
      fPolygon.Draw
    end;
  end;



(* Tests collision. *)
  function TSprite.IntersectWith (aSpr: TSprite): Boolean;
  begin
    Result := fActive and aSpr.fActive
          and (ABS (aSpr.fPx - fPx + 2) < fRadius)
          and (ABS (aSpr.fPy - fPy + 2) < fRadius)
  end;



(*
 * TMoveableSprite
 ***************************************************************************)

(* Updates sprite. *)
  procedure TMoveableSprite.Update;
  begin
    if fActive then
    begin
    { Position. }
      fPx := fPx + fVx;
      fPy := fPy + fVy;
    { If center is outside the screen, move to the opposite side. }
      if fPx - fRadius > MAX_WIDTH then fPx := fPx - (MAX_WIDTH + 2 * fRadius);
      if fPx < -fRadius then fPx := fPx + (MAX_WIDTH + 2 * fRadius);
      if fPy - fRadius > MAX_HEIGHT then fPY := fPy - (MAX_HEIGHT + 2 * fRadius);
      if fPy < -fRadius then fPy := fPy + (MAX_HEIGHT + 2 * fRadius);
    { Rotation. }
      fAngle := fAngle + fVr
    end
  end;

end.

