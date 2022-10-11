unit Game;
(* Defines the Pascalroid game object. *)
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
    Engine, Graphics, Player;

  type
  (* Implements the game scene. *)
    TPascalroidsScene = class (TGameScene)
    private
      fShip: TShipSprite;
    public
    (* Initializes the scene. *)
      procedure Initialize; override;
    (* Game logic. *)
      procedure Update; override;
    (* Draws screen. *)
      procedure Draw; override;
    (* Closes the scene. *)
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

    (* Initializes the program. *)
      function Initialize: Boolean; override;
    end;

implementation

  uses
    Allegro5;



(*
 * TPascalroidsScene
 ************************************************************************)

  procedure TPascalroidsScene.Initialize;
  begin
    inherited Initialize;
    fShip := TShipSprite.Create;
    fShip.X := 100; fShip.Y := 100;
    fShip.Angle := 20
  end;



(* Updates. *)
  procedure TPascalroidsScene.Update;
  begin
    if Self.Game.Display.UserClickedClose then Self.Game.Terminate;
    fShip.Update
  end;



(* Draws screen. *)
  procedure TPascalroidsScene.Draw;
  begin
    al_clear_to_color (clrBlack);
    fShip.Draw
  end;



(* Closes the scene. *)
  procedure TPascalroidsScene.Finalize;
  begin
    fShip.Free
  end;



(*
 * TPascalroids
 ************************************************************************)

(* Constructor. *)
  constructor TPascalroids.Create;
  begin
    inherited Create;
    fGameScene := TPascalroidsScene.Create
  end;



(* Destructor. *)
  destructor TPascalroids.Destroy;
  begin
    fGameScene.Free;
    inherited Destroy
  end;



(* Initializes. *)
  function TPascalroids.Initialize: Boolean;
  begin
    Self.display.Title := Concat ('Pascalroids ',ALLEGRO_PAS_VERSION_STR);
    Result := inherited Initialize;
    if Result then Graphics.Initialize;
    Self.Scene := fGameScene
  end;

end.
