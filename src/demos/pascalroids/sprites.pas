unit Sprites;
(* Implements the base classes for sprites. *)
(*
  Copyright (c) 2022 Guillermo Martínez J.

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

  type
  (* Base class for sprites. *)
    TSprite = class (TObject)
    private
      fX, fY, fVx, fVy, fRadius: Double;
      fEnabled: Boolean;
    public
    (* Constructor. *)
      constructor Create; virtual;
    (* Updates sprite. *)
      procedure Update; virtual; abstract;
    (* Draws the sprite. *)
      procedure Draw; virtual; abstract;

    (* Checks if sprite colided with given sprite. *)
      function CollideWith (aSpr: TSprite): Boolean; virtual;

    (* Sprite X coordinate. *)
      property X: Double read fX write fX;
    (* Sprite Y coordinate. *)
      property Y: Double read fY write fY;
    (* Sprite X velocity. *)
      property Vx: Double read fVx write fVx;
    (* Sprite Y velocity. *)
      property Vy: Double read fVy write fVy;
    (* Sprite radius. *)
      property Radius: Double read fRadius write fRadius;
    (* Tells if sprite is enabled.  This property is used by other methods and
       objects to know if they whould update or interact with the sprite.

       Default is @true. *)
      property Enabled: Boolean read fEnabled write fEnabled;
    end;



  (* Manages a group of sprites. *)
    TSpriteManager = class (TObject)
    protected
      fSprites: array of TSprite;
    public
    (* Updates the sprites.

       It updates enabled sprites only. *)
      procedure Update;
    (* Draws all sprites.

       It draws enabled sprites only. *)
      procedure Draw;
    end;

implementation

(*
 * TSprite
 ************************************************************************)

(* Constructor. *)
  constructor TSprite.Create; begin inherited end;



(* Checks collision. *)
  function TSprite.CollideWith (aSpr: TSprite): Boolean;
  begin
  { Use good old Pythagoras. }
    Result := Sqr (Self.X - aSpr.X) + Sqr (Self.Y - aSpr.Y)
            < Sqr (Self.Radius + aSpr.Radius)
  end;



(*
 * TSpriteManager
 ************************************************************************)

(* Updates sprites. *)
  procedure TSpriteManager.Update;
  var
    lSprite: TSprite;
  begin
    for lSprite in fSprites do if lSprite.Enabled then lSprite.Update
  end;



(* Draw sprites. *)
  procedure TSpriteManager.Draw;
  var
    lSprite: TSprite;
  begin
    for lSprite in fSprites do if lSprite.Enabled then lSprite.Draw
  end;

end.
