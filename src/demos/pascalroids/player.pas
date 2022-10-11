unit Player;
(* Implements all player stuff:  ship, controls and laser. *)
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

  uses
    Graphics;

  type
  (* The player sprite. *)
    TShipSprite = class (TSpritePolygon)
    private
      fPolygon: TPolygon;
    public
    (* Constructor. *)
      constructor Create; override;

    (* Updates sprite. *)
      procedure Update; override;
    end;

implementation

(*
 * TShipSprite
 ************************************************************************)

(* Constructor. *)
  constructor TShipSprite.Create;
  const
    ShipDescriptionX: array [1..4] of Integer = (3, -2, -1, -2);
    ShipDescriptionY: array [1..4] of Integer = (0,  2,  0, -2);
  var
    Ndx: Integer;
  begin
    inherited Create;
    fPolygon.Reset;
    fPolygon.Color := clrGreen;
    for Ndx := Low (ShipDescriptionX) to High (ShipDescriptionX) do
      fPolygon.AddVertex (ShipDescriptionX[Ndx]*10, ShipDescriptionY[Ndx]*10);
    Self.Polygon := @fPolygon
  end;



(* Updates. *)
  procedure TShipSprite.Update;
  begin
    Self.X := Self.X + Self.Vx;
    Self.Y := Self.Y + Self.Vy
  end;

end.
