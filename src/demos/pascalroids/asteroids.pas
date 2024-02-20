unit Asteroids;
(* Implements the asteroids. *)
(*
  Copyright (c) 2023 Guillermo Martínez J.

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
    Graphics, Sprites;

  type
  (* The asteroid. *)
    TAsteroidSprite = class (TSpritePolygon)
    public
      type AsteroidSize = (asLarge, asMedium, asSmall);
    private
      fSize: AsteroidSize;
      fAngleVelocity: Double;
    public
    (* Initialize the sprite.

       It enables the sprite and sets radius and rotation speed.
       Note that it sets velocity to 0! *)
      procedure Initialize; override;
    (* Update asteroid. *)
      procedure Update; override;

    (* Asteroid size.  Shoud be assigned before the initialization. *)
      property Size: AsteroidSize read fSize write fSize;
    end;



  (* Manages the asteroids. *)
    TAsteroidsManager = class (TSpriteManager)
    private
      fPolygonLarge, fPolygonMedium, fPolygonSmall: TPolygon;
    public
    (* Constructor. *)
      constructor Create;
    (* Creates a new large asteroid. *)
      procedure NewAsteroid;
    end;

implementation

  uses
    Engine,
    Allegro5, al5base;


  const
  { Next constants are to build the asteroids.

    They were borrowed (with few changes) from the book "Black Art Of Java Game
    Programming" by Joel Fan Sams (Macmillan Computer Publishing 1996).
  }
    MaxRadius = 30;
    SizeRatio = 2;
    Variance = 4; { < The larger, the softer. }
    SmallRatio = Ord (TAsteroidSprite.AsteroidSize.asSmall) * SizeRatio;
    MediumRatio = Ord (TAsteroidSprite.AsteroidSize.asMedium) * SizeRatio;
    LargeRadius = MaxRadius + Trunc (MaxRadius / Variance);
    MediumRadius = MaxRadius div MediumRatio
                 + Trunc (MaxRadius / Variance / MediumRatio);
    SmallRadius = MaxRadius div SmallRatio
                + Trunc (MaxRadius / Variance / SmallRatio);

    MaxSpeed = 2;
    MaxRotation = ALLEGRO_TAU / 125;

(*
 * TAsteroidSprite
 ************************************************************************)

  procedure TAsteroidSprite.Initialize;
  const
    Factor = 10000;
  begin
    inherited Initialize;
    case Self.Size of
    asLarge:
      Self.Radius := LargeRadius;
    asMedium:
      Self.Radius := MediumRadius;
    asSmall:
      Self.Radius := SmallRadius;
    end;
    fAngleVelocity := Random (Trunc (2 * MaxRotation * Factor)) / Factor;
    fAngleVelocity := fAngleVelocity - MaxRotation
  end;



  procedure TAsteroidSprite.Update;
  begin
    Self.Angle := Self.Angle + fAngleVelocity;
    Self.Move;
    Self.WrapAround (Trunc (Self.Radius))
  end;



(*
 * TAsteroidsManager
 ************************************************************************)

  constructor TAsteroidsManager.Create;

    function VertexDeviation (const aDeviation: Integer): Integer; inline;
    begin
      Result := Random (2 * aDeviation) - aDeviation
    end;

  const
    NumVertexLarge = 9;
    NumVertexMedium = 4;
    DegToRad = ALLEGRO_TAU / 360;
    LargeAngle = ALLEGRO_TAU / NumVertexLarge;
    MediumAngle = ALLEGRO_TAU / NumVertexMedium;
  { Number of asteroids. }
    NumLarge = 6; NumMedium = 2 * NumLarge; NumSmall = 2 * NumMedium;
    NumAsteroids = NumLarge + NumMedium + NumSmall;
  var
    lNdx, lDeviation, lDeviationSmall: Integer;
    lVx, lVy: AL_FLOAT;
  begin
  { Create container and reserve sprites. }
    inherited Create (NumAsteroids, TAsteroidSprite);
  { Create asteroid polygons. }
    lDeviation := Trunc (MaxRadius / Variance);
    for lNdx := 1 to NumVertexLarge do
      fPolygonLarge.AddVertex (
        cos (LargeAngle * lNdx) * MaxRadius + VertexDeviation (lDeviation),
        sin (LargeAngle * lNdx) * MaxRadius + VertexDeviation (lDeviation)
      );
    fPolygonLarge.Color := clrWhite;

    lDeviation := MaxRadius * Ord (asMedium) * SizeRatio;
    lDeviation := Trunc (MaxRadius / lDeviation / Variance);
    lDeviationSmall := MaxRadius * Ord (asSmall) * SizeRatio;
    lDeviationSmall := Trunc (MaxRadius / lDeviationSmall / Variance);
    for lNdx := 1 to NumVertexMedium do
    begin
      lVx := cos ((lNdx * MediumAngle) * DegToRad) * MaxRadius;
      lVy := sin ((lNdx * MediumAngle) * DegToRad) * MaxRadius;
      fPolygonMedium.AddVertex (
        (lVx / MediumRatio) + VertexDeviation (lDeviation),
        (lVy / MediumRatio) + VertexDeviation (lDeviation)
      );
      fPolygonSmall.AddVertex (
        (lVx / SmallRatio) + VertexDeviation (lDeviationSmall),
        (lVy / SmallRatio) + VertexDeviation (lDeviationSmall)
      )
    end;
    fPolygonMedium.Color := clrWhite;
    fPolygonSmall.Color := clrWhite
  end;



  procedure TAsteroidsManager.NewAsteroid;

    function AsteroidSpeed: Double; inline;
    begin
      Result := (Random (2 * MaxSpeed * 1000) / 1000) - MaxSpeed
    end;

  var
    lAsteroid: TAsteroidSprite;
  begin
    lAsteroid := TAsteroidSprite (Self.NewSprite);
    if Assigned (lAsteroid) then
    begin
      lAsteroid.Size := asLarge;
      lAsteroid.Polygon := @fPolygonLarge;
      lAsteroid.Initialize;
      repeat
        lAsteroid.Vx := AsteroidSpeed;
        lAsteroid.Vy := AsteroidSpeed
      until (lAsteroid.Vx <> 0) and (lAsteroid.Vy <> 0);
      lAsteroid.X := Random (DisplayWidth);
      lAsteroid.Y := Random (DisplayHeight);
      case Random (4) of
      0:
        lAsteroid.X := -lAsteroid.Radius;
      1:
        lAsteroid.X := DisplayWidth + lAsteroid.Radius;
      2:
        lAsteroid.Y := -lAsteroid.Radius;
      otherwise
        lAsteroid.Y := DisplayHeight + lAsteroid.Radius;
      end
    end
  end;

end.
