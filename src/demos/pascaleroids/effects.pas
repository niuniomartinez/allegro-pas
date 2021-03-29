unit Effects;
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

interface

  uses
    Engine;

  const
  (* Explosion size. *)
    EXP_SIZE = 13;
  (* Fire length. *)
    FIRE_LENGTH = 23;
  (* Fire life. *)
    FIRE_LIFE = 14;

  type
  (* An explosion. *)
    TExplosion = class (TSprite)
    private
      fCnt: Integer;
    public
    (* Restores the sprite. *)
      procedure Restore; override;
    (* Updates the explosion. *)
      procedure Update; override;
    (* Draws the explosion. *)
      procedure Draw; override;
    end;



  (* The fire sprite. *)
    TFire = class (TMoveableSprite)
    private
      fvx, fvy, { Vector components.  Temporary. }
      fCount: Integer;
    public
    (* Somebody shooted a fire. *)
      procedure Shoot (aX, aY, aAngle: Integer);
    (* Updates sprite. *)
      procedure Update; override;
    (* Tests collision with sprite.

       Since @italic(Fire) is a line, collision calculation is slightly
       different. *)
      function IntersectWith (aSpr: TSprite): Boolean; override;
    end;



  (* The effects manager. *)
    TEffectManager = class (TManager)
    private
      fExplosion: TExplosion;
    public
    (* Constructor. *)
      constructor Create;
    (* Destructor. *)
      destructor Destroy; override;
    (* Creates an explosion. *)
      procedure AddAsteroidExplosion (const aX, aY: Integer);
    (* Initializes. *)
      procedure Initialize; override;
    (* New game. *)
      procedure NewGame; override;
    (* Updates effects. *)
      procedure Update; override;
    (* Draw sprites. *)
      procedure Paint; override;
    end;

implementation

  uses
    Graphics,
    Allegro5, al5primitives, math;

  const
  (* How the explosion grows. *)
    EXP_RATE = 1.7;

(*
 * TExplosion
 ***************************************************************************)

(* Restores. *)
  procedure TExplosion.Restore;
  begin
    Self.Visible := True;
    Self.Active := True;
    Polygon.Color := Yelow;
    fCnt := 0
  end;



(* Updates. *)
  procedure TExplosion.Update;
  begin
    if Self.Active then
    begin
      Inc (fCnt);
      if fCnt > EXP_SIZE then Self.Suspend;
    end;
  end;



(* Draws. *)
  procedure TExplosion.Draw;
  var
    Size: Real;
    TransformationMatrix: ALLEGRO_TRANSFORM;
  begin
    if Self.Visible then
    begin
      Size := EXP_SIZE + power (EXP_RATE, 1 + fCnt);

      al_build_transform (TransformationMatrix, PosX, PosY, 1, 1, 0);
      al_use_transform (TransformationMatrix);

      al_draw_line (-EXP_SIZE, -Size, EXP_SIZE, -Size, Polygon.Color, 0);
      al_draw_line (-EXP_SIZE,  Size, EXP_SIZE,  Size, Polygon.Color, 0);
      al_draw_line (-Size, -EXP_SIZE, -Size, EXP_SIZE, Polygon.Color, 0);
      al_draw_line ( Size, -EXP_SIZE,  Size, EXP_SIZE, Polygon.Color, 0)
    end
  end;



(*
 * TFire
 ***************************************************************************)

(* Somebody shooted a fire. *)
  procedure TFire.Shoot (aX, aY, aAngle: Integer);
  begin
    Self.Initialize;

    fvx := Trunc (FIRE_LENGTH * cos (aAngle));
    fvy := Trunc (FIRE_LENGTH * sin (aAngle));
    Self.PosX := aX; Self.PosY := aY;

    Self.Polygon.AddPoint (0, 0);
    Self.Polygon.AddPoint (fvx, fvy);
    Self.Polygon.AddPoint (0, 0); { Not sure this is needed. }
    Self.Polygon.Fill := False;
    fCount := 0
  end;



(* Updates. *)
  procedure TFire.Update;
  begin
    if Self.Active then
    begin
      Inc (fCount);
      if fCount >= FIRE_LIFE then Self.Suspend else inherited Update
    end
  end;



(* Collision. *)
  function TFire.IntersectWith (aSpr: TSprite): Boolean;
  var
    midptx, midpty: Integer;
  begin
    midptx := Trunc (PosX + PosX + fvx) div 2;
    midpty := Trunc (PosY + PosY + fvy) div 2;
    Result := Self.Active and aSpr.Active
      and (ABS (aSpr.PosX - midptx  + 2) < aSpr.Radius)
      and (ABS (aSpr.PosY - midpty  + 2) < aSpr.Radius)
  end;



(*
 * TEffectManager
 ***************************************************************************)

(* Constructor. *)
  constructor TEffectManager.Create;
  begin
    inherited Create;
    fExplosion := TExplosion.Create
  end;



(* Destructor. *)
  destructor TEffectManager.Destroy;
  begin
    fExplosion.Free;
    inherited Destroy
  end;



(* Adds an explosion. *)
  procedure TEffectManager.AddAsteroidExplosion (const aX, aY: Integer);
  begin
    fExplosion.Restore;
    fExplosion.PosX := aX; fExplosion.PosY := aY
  end;



(* Initializes. *)
  procedure TEffectManager.Initialize;
  begin
    fExplosion.Suspend;
  end;



(* New game. *)
  procedure TEffectManager.NewGame;
  begin
    fExplosion.Suspend;
  end;



(* Updates. *)
  procedure TEffectManager.Update;
  begin
    fExplosion.Update
  end;



(* Draws. *)
  procedure TEffectManager.Paint;
  begin
    fExplosion.Draw
  end;

end.

