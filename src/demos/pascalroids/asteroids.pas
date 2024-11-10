unit Asteroids;
(* Implements the asteroids. *)
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

       It enables the sprite and sets rabdin velocity, radius and rotation
       speed.
     *)
      procedure Initialize; override;
    (* Update asteroid. *)
      procedure Update; override;

    (* Asteroid size.  Shoud be assigned before the initialization. *)
      property Size: AsteroidSize read fSize write fSize;
    end;



  (* Manages the asteroids. *)
    TAsteroidsManager = class (TSpriteManager)
    public
    (* Constructor. *)
      constructor Create;
    (* Creates a new large asteroid. *)
      procedure NewAsteroid;
    (* Divide asteroid in half. *)
      procedure DivideAsteroid (aAsteroid: TAsteroidSprite);
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

    MaxSpeed = 1.5;
    MaxRotation = ALLEGRO_TAU / 125;

  var
  { Asteroid polygons.  Assigned by TAsteroidsManager constructor. }
    fPolygonLarge, fPolygonMedium, fPolygonSmall: TPolygon;


(*
 * TAsteroidSprite
 ************************************************************************)

  procedure TAsteroidSprite.Initialize;
  const
    Factor = 10000;
  begin
    inherited Initialize;
  { Size. }
    case Self.Size of
    asLarge:
      begin
        Self.Radius := LargeRadius;
        Self.Polygon := @fPolygonLarge
      end;
    asMedium:
      begin
        Self.Radius := MediumRadius;
        Self.Polygon := @fPolygonMedium
      end;
    asSmall:
      begin
        Self.Radius := SmallRadius;
        Self.Polygon := @fPolygonSmall
      end;
    end;
  { Rotation. }
    fAngleVelocity := Random (Trunc (2 * MaxRotation * Factor)) / Factor;
    fAngleVelocity := fAngleVelocity - MaxRotation;
  { Speed. }
    repeat
      Self.Vx := RandomBetween (-MaxSpeed, MaxSpeed);
      Self.Vy := RandomBetween (-MaxSpeed, MaxSpeed)
    until (Self.Vx <> 0) or (Self.Vy <> 0)
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
    NumVertexMedium = 6;
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
    fPolygonLarge.Reset;
    fPolygonMedium.Reset;
    fPolygonSmall.Reset;
    lDeviation := Trunc (MaxRadius / Variance);
    for lNdx := 1 to NumVertexLarge do
      fPolygonLarge.AddVertex (
        cos (LargeAngle * lNdx) * MaxRadius + VertexDeviation (lDeviation),
        sin (LargeAngle * lNdx) * MaxRadius + VertexDeviation (lDeviation)
      );

    lDeviation := Trunc (MediumRadius / Variance);
    lDeviationSmall := Trunc (SmallRadius / Variance);
    for lNdx := 1 to NumVertexMedium do
    begin
      lVx := cos (lNdx * MediumAngle) * MaxRadius;
      lVy := sin (lNdx * MediumAngle) * MaxRadius;
      fPolygonMedium.AddVertex (
        (lVx / MediumRatio) + VertexDeviation (lDeviation),
        (lVy / MediumRatio) + VertexDeviation (lDeviation)
      );
      fPolygonSmall.AddVertex (
        (lVx / SmallRatio) + VertexDeviation (lDeviationSmall),
        (lVy / SmallRatio) + VertexDeviation (lDeviationSmall)
      )
    end;
    fPolygonLarge.Color := clrWhite;
    fPolygonMedium.Color := clrWhite;
    fPolygonSmall.Color := clrWhite
  end;



  procedure TAsteroidsManager.NewAsteroid;
  var
    lAsteroid: TAsteroidSprite;
  begin
    lAsteroid := TAsteroidSprite (Self.NewSprite);
    if Assigned (lAsteroid) then
    begin
      lAsteroid.Size := asLarge;
      lAsteroid.Initialize;
    { Make it appear from the borders of the screen. }
      lAsteroid.X := Random (DisplayWidth);
      lAsteroid.Y := Random (DisplayHeight);
      case Random (4) of
      0:
        lAsteroid.X := -lAsteroid.Radius;
      1:
        lAsteroid.X := DisplayWidth + lAsteroid.Radius;
      2:
        lAsteroid.Y := -lAsteroid.Radius;
      else { othewise }
        lAsteroid.Y := DisplayHeight + lAsteroid.Radius;
      end
    end
  end;



  procedure TAsteroidsManager.DivideAsteroid (aAsteroid: TAsteroidSprite);
  var
    lCnt: Integer;

    procedure NewAsteroid (aSize: TAsteroidSprite.AsteroidSize); inline;
    var
      lNewAsteroid: TAsteroidSprite;
    begin
      lNewAsteroid := TAsteroidSprite (Self.NewSprite);
      if Assigned (lNewAsteroid) then
      begin
        lNewAsteroid.X := aAsteroid.X;
        lNewAsteroid.y := aAsteroid.Y;
        lNewAsteroid.size := aSize;
        lNewAsteroid.Initialize
      end
    end;

  begin
    aAsteroid.Enabled := False;
    case aAsteroid.Size of
    asLarge:
      for lCnt := 1 to 2 do NewAsteroid (asMedium);
    asMedium:
      for lCnt := 1 to 2 do NewAsteroid (asSmall);
    end;
  end;

end.
