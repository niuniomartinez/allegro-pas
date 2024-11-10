unit Game;
(* Defines the Pascalroid game object. *)
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
    Asteroids, Debris, Engine, Graphics, Player, Sprites;

  type
  (* Implements the game scene. *)
    TPascalroidsScene = class (TGameScene)
    private
      fTime: Integer;
      fShip: TShipSprite;
      fLasers: TSpriteManager;
      fAsteroids: TAsteroidsManager;
      fDebris: TDebrisManager;

      fInput: TPlayerInput;
      fScore: Integer;

    (* Manages collision between player ship and an asteroid. *)
      procedure ShipCollidesAsteroid (aShip, aAsteroid: TSprite);
    (* Manages collision between a player's laser and an asteroid. *)
      procedure LaserCollidesAseroid (aLaser, aAsteroid: TSprite);
    public
    (* Enter the scene. *)
      procedure Enter; override;
    (* Game logic. *)
      procedure Update; override;
    (* Draw screen. *)
      procedure Draw; override;
    (* Close the scene. *)
      procedure Leave; override;
    end;



  (* The game object. *)
    TPascalroids = class (TGame)
    private
      fGameScene: TPascalroidsScene;
    public
    (* Constructor. *)
      constructor Create; override;
    (* Destructor. *)
      destructor Destroy; override;

    (* Initialize the program. *)
      function Initialize: Boolean; override;
    end;

implementation

  uses
    Allegro5;



(*
 * TPascalroidsScene
 ************************************************************************)

  const
    MaxLasers = 10;
  (* Time wetween asteroid spawns. *)
    TimeSpawn = 5 * FPS;
  (* How much time to wait for the first asteroid. *)
    FirstSpawn = TimeSpawn div 5;
  (* Score. *)
    AsteroidScore: array [TAsteroidSprite.AsteroidSize] of Integer = (
      1, 5, 10
    );
    ScoreFormat = '%.7d';

  procedure TPascalroidsScene.ShipCollidesAsteroid (aShip, aAsteroid: TSprite);
  begin
    fDebris.Blast (aShip.X, aShip.Y, aShip.Vx / 3, aShip.Vy / 3);
    aShip.Enabled := False;
  { Small asteroids don't support collision. }
    if TAsteroidSprite (aAsteroid).Size = asSmall then
      aAsteroid.Enabled := False
  end;



  procedure TPascalroidsScene.LaserCollidesAseroid (aLaser, aAsteroid: TSprite);
  var
    lAsteroid: TAsteroidSprite absolute aAsteroid;
  begin
    Inc (fScore, AsteroidScore[lAsteroid.Size]);
    fDebris.Blast (aAsteroid.X, aAsteroid.Y);
    aLaser.Enabled := False;
    fAsteroids.DivideAsteroid (lAsteroid)
  end;



  procedure TPascalroidsScene.Enter;
  begin
    inherited Enter;
    fInput := TKeyboardInput.Create;
    fLasers := TSpriteManager.Create (MaxLasers, TLaser);
    fAsteroids := TAsteroidsManager.Create;
    fDebris := TDebrisManager.Create;
    fShip := TShipSprite.Create (fInput, fLasers, fDebris);
    fShip.Initialize;
    fTime := TimeSpawn - FirstSpawn;

    fScore := 0
  end;



  procedure TPascalroidsScene.Update;
  begin
    if Self.Game.Display.UserClickedClose then Self.Game.Terminate;
  { Update objects. }
    if fShip.Enabled then fShip.Update;
    Inc (fTime);
    if fTime >= TimeSpawn then
    begin
      fAsteroids.NewAsteroid;
      fTime := 0
    end;
    fAsteroids.Update;
    fLasers.Update;
    fDebris.Update;
  { Check for collisions. }
    fShip.Polygon^.Color := al_map_rgb (0, 255, 255);
    fAsteroids.CheckCollisions (fLasers, Self.LaserCollidesAseroid);
    fAsteroids.CheckCollisions (fShip, Self.ShipCollidesAsteroid)
  end;



  procedure TPascalroidsScene.Draw;
  begin
    al_clear_to_color (clrBlack);
    fLasers.Draw;
    fAsteroids.Draw;
    fDebris.Draw;
    if fShip.Enabled then fShip.Draw;
    al_draw_text (
      TPascalroids (GameObject).TextFont,
      clrGreen,
      8, 8,
      0,
      al_str_format (ScoreFormat, [fScore])
    )
  end;



  procedure TPascalroidsScene.Leave;
  begin
    fShip.Free;
    fLasers.Free;
    fAsteroids.Free;
    fDebris.Free;
    fInput.Free
  end;



(*
 * TPascalroids
 ************************************************************************)

  constructor TPascalroids.Create;
  begin
    inherited Create;
    fGameScene := TPascalroidsScene.Create
  end;



  destructor TPascalroids.Destroy;
  begin
    fGameScene.Free;
    inherited Destroy
  end;



  function TPascalroids.Initialize: Boolean;
  begin
    Self.display.Title := Concat ('Pascalroids ',ALLEGRO_PAS_VERSION_STR);
    Result := inherited Initialize;
    if Result then Graphics.Initialize;
    Self.Scene := fGameScene
  end;

end.
