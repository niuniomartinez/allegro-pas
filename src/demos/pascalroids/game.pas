unit Game;
(* Defines the Pascalroid game object. *)
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
    Asteroids, Engine, Graphics, Player, Sprites;

  type
  (* Implements the game scene. *)
    TPascalroidsScene = class (TGameScene)
    private
      fShip: TShipSprite;
      fLasers: TSpriteManager;
      fAsteroids: TAsteroidsManager;
      fInput: TPlayerInput;
    public
    (* Initialize the scene. *)
      procedure Initialize; override;
    (* Game logic. *)
      procedure Update; override;
    (* Draw screen. *)
      procedure Draw; override;
    (* Close the scene. *)
      procedure Finalize; override;
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

  procedure TPascalroidsScene.Initialize;
  begin
    inherited Initialize;
    fInput := TKeyboardInput.Create;
    fLasers := TSpriteManager.Create (MaxLasers, TLaser);
    fAsteroids := TAsteroidsManager.Create;
fAsteroids.NewAsteroid;
fAsteroids.NewAsteroid;
fAsteroids.NewAsteroid;
fAsteroids.NewAsteroid;
    fShip := TShipSprite.Create (fInput, fLasers);
    fShip.Initialize
  end;



  procedure TPascalroidsScene.Update;
  begin
    if Self.Game.Display.UserClickedClose then Self.Game.Terminate;
    fShip.Update;
    fAsteroids.Update;
    fLasers.Update
  end;



  procedure TPascalroidsScene.Draw;
  begin
    al_clear_to_color (clrBlack);
    fLasers.Draw;
    fAsteroids.Draw;
    fShip.Draw
  end;



  procedure TPascalroidsScene.Finalize;
  begin
    fShip.Free;
    fLasers.Free;
    fAsteroids.Free;
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
