unit Player;
(* Implements all player stuff:  ship, controls and laser. *)
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
    Engine, Graphics, Sprites;

  type
  (* A Laser. *)
    TLaser = class (TSprite)
    public
    (* Shoot the laser. *)
      procedure Shoot (const aX, aY, aAngle: Single);
    (* Initialize laser sprite. *)
      procedure Initialize; override;
    (* Draw laser. *)
      procedure Draw; override;
    (* Updates laser. *)
      procedure Update; override;
    end;



  (* The player sprite. *)
    TShipSprite = class (TSpritePolygon)
    private
      fPolygon: TPolygon;
      fInput: TPlayerInput;
      fDebris, fLasers: TSpriteManager;

    (* Shoot a laser. *)
      procedure Shoot;
    public
    (* Constructor.

       Note that player doesn't own the lasers. *)
      constructor Create (
        aInput: TPlayerInput;
        aLasers, aDebris: TSpriteManager
      ); overload;
    (* Initializes ship. *)
      procedure Initialize; override;

    (* Update sprite. *)
      procedure Update; override;
    end;

implementation

  uses
    Allegro5, al5primitives;

(*
 * TLaser
 ************************************************************************)
  const
   LaserSpeed = 10;
   LaserRadius = LaserSpeed * 0.5;

  procedure TLaser.Shoot (const aX, aY, aAngle: Single);
  begin
    Self.Initialize;
    Self.X := aX; Self.Y := aY;
    Self.vX := LaserSpeed * cos (aAngle);
    Self.vY := LaserSpeed * sin (aAngle)
  end;



  procedure TLaser.Initialize;
  begin
    inherited Initialize;
    Self.Radius := LaserRadius
  end;



  procedure TLaser.Draw;
  begin
    al_draw_line (
      Self.X - Self.vX, Self.Y - Self.vY, Self.X + Self.vX, Self.Y + Self.vY,
      clrGreen, 1
    );
{$IfDef DEBUGMODE}
    al_draw_circle (Self.X ,Self.Y, Self.Radius, clrBlue, 1);
{$EndIf}
  end;



  procedure TLaser.Update;
  begin
    Self.Move;
    if Self.IsOutside then Self.Enabled := False
  end;



(*
 * TShipSprite
 ************************************************************************)

   const
     RotationSpeed = ALLEGRO_TAU / 64;
     ShipAcceleration = 0.125;
     ShipRadius = 10;
     ShipMargin = 10;

  procedure TShipSprite.Shoot;
  var
    lLaser: TLaser;
  begin
    lLaser := TLaser (fLasers.NewSprite);
    if Assigned (lLaser) Then
      lLaser.Shoot (
        Self.X + (cos (Self.Angle) * ShipRadius),
        Self.Y + (sin (Self.Angle) * ShipRadius),
        Self.Angle
      )
  end;



  constructor TShipSprite.Create (
    aInput: TPlayerInput;
    aLasers, aDebris: TSpriteManager
  );
  const
    ShipDescriptionX: array [1..4] of Integer = (15, -10, -5, -10);
    ShipDescriptionY: array [1..4] of Integer = ( 0,  10,   0, -10);
  var
    Ndx: Integer;
  begin
    inherited Create;
    Self.Radius := ShipRadius;
    fInput := aInput;
    fPolygon.Reset;
    fPolygon.Color := clrWhite;
    for Ndx := Low (ShipDescriptionX) to High (ShipDescriptionX) do
      fPolygon.AddVertex (ShipDescriptionX[Ndx], ShipDescriptionY[Ndx]);
    Self.Polygon := @fPolygon;
    fLasers := aLasers;
    fDebris := aDebris
  end;



  procedure TShipSprite.Initialize;
  begin
    inherited Initialize;
    Self.X := DisplayWidth div 2; Self.Y := DisplayHeight div 2;
    Self.Angle := ALLEGRO_TAU - (ALLEGRO_TAU / 4)
  end;



  procedure TShipSprite.Update;

    procedure CreateEngineTrail; inline;
    const
     TrailSpeed = 1.25;
     TrailDeviation = Trunc ((ALLEGRO_TAU * 0.01) * 1000);
    var
      lSpr: TSprite;
      lAngle, lSpeed: Double;
    begin
      lSpr := fDebris.NewSprite;
      if not Assigned (lSpr) then Exit;

      lSpr.Initialize;
      lSpr.X := Self.X + Self.Radius * cos (Self.Angle) * (-1);
      lSpr.Y := Self.Y + Self.Radius * sin (Self.Angle) * (-1);

      lAngle := Self.Angle + RandomBetween (-10, 10) / 100;
      lSpeed := (-1) * TrailSpeed + RandomBetween (0, 10) / 1000;
      lSpr.Vx := lSpeed * cos (lAngle);
      lSpr.Vy := lSpeed * sin (lAngle)
    end;

  var
    lAxisX, lAxisY, lButton: Integer;
    lSpX, lSpY: Real;
  begin
  { User input. }
    fInput.GetState (lAxisX, lAxisY, lButton);
    if lAxisY > 0 then
    begin
      lSpX := ShipAcceleration * cos (Self.Angle);
      lSpY := ShipAcceleration * sin (Self.Angle);
      Self.Vx := Self.Vx + lSpX;
      Self.Vy := Self.Vy + lSpY;
      if Random (5) = 1 then CreateEngineTrail
    end;
    if lButton <> 0 then Self.Shoot;
  { Position and angle. }
    Self.Angle := Self.Angle + (lAxisX * RotationSpeed);
    Self.Move;
    Self.WrapAround (ShipMargin)
  end;

end.
