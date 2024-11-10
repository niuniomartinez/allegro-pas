unit Debris;
(* Implements debris. *)
(*
  Copyright (c) 2024 Guillermo Martínez J.

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
    Sprites,
    allegro5;

  type
  (* A debris object. *)
    TDebris = class (TSprite)
    private
      fLife: Integer;
      fColor: ALLEGRO_COLOR;
    public
    (* Initialize debris. *)
      procedure Initialize; override;
    (* Update debris. *)
      procedure Update; override;
    (* Draw debris. *)
      procedure Draw; override;
    end;



  (* A debris manager. *)
    TDebrisManager = class (TSpriteManager)
    public
    (* Constructor. *)
      constructor Create;
    (* Create explosion debris. *)
      procedure Blast (const aX, aY, aVx, aVy: Double); overload;
      procedure Blast (const aX, aY: Double); overload;
    end;

implementation

  uses
    Engine, Graphics,
    al5primitives;

(*
 * TDebris
 ************************************************************************)

  procedure TDebris.Initialize;
  begin
    inherited Initialize;
    fColor := clrWhite;
    fLife := Trunc ((FPS * 1.5) * RandomBetween (0.75, 1))
  end;



  procedure TDebris.Update;
  begin
    Self.Move;
    Dec (fLife);
    Self.Enabled := fLife > 0
  end;



  procedure TDebris.Draw;
  begin
    al_draw_circle (Self.X + 0.5, Self.Y + 0.5, 1, fColor, 1)
  end;



(*
 * TDebrisManager
 ************************************************************************)

  const
    MaxDebris = 30;



  constructor TDebrisManager.Create;
  begin
    inherited Create (MaxDebris, TDebris)
  end;



  procedure TDebrisManager.Blast (const aX, aY, aVx, aVy: Double); inline;
  const
    MaxDebris = 10;
    ExplosionSpeed = 1.75;
  var
    lCnt: Integer;
    lSpr: TSprite;
    lAngle, lSpeed: Double;
  begin
    for lCnt := 1 to RandomBetween (Trunc (MaxDebris * 2 / 3), MaxDebris) do
    begin
      lSpr := Self.NewSprite;
      if not Assigned (lSpr) then Exit; { Not enough sprites. }
      lSpr.Initialize;
      lSpr.X := aX;
      lSpr.Y := aY;
      lAngle := RandomBetween (0, ALLEGRO_TAU);
      lSpeed := (-1) * ExplosionSpeed + RandomBetween (0, 10) / 100;
      lSpr.Vx := (lSpeed * cos (lAngle)) + aVx;
      lSpr.Vy := (lSpeed * sin (lAngle)) + aVy;
    end
  end;



  procedure TDebrisManager.Blast (const aX, aY: Double);
  begin
    Self.Blast (aX, aY, 0, 0)
  end;

end.
