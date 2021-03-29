unit Asteroids;
(*< Defines all asteroid stuff. *)
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
  (* How much big asteroids. *)
    NUM_LARGE = 6;
  (* How much medium asteroids. *)
    NUM_MEDIUM = NUM_LARGE * 2;
  (* How much small asteroids. *)
    NUM_SMALL = NUM_MEDIUM * 2;
  (* Total number of asteroids. *)
    NUM_ASTS = NUM_LARGE + NUM_MEDIUM + NUM_SMALL;


  type
  (* Defines an asteroid. *)
    TAsteroid = class (TMoveableSprite)
    private
    (* Generates asteroid vectors, moving the given templates randomly. *)
      procedure MakeVectors (
        out tpx, tpy: array of Integer;
        const aSize: Integer
      );
    public
    (* Initializes the asteroid.

       This generates a new asteroid shape, sets its initial position and
       velocity. *)
      procedure InitializeAsteroid (const aSize: Integer);
    end;



  (* A power up. *)
    TPowerUp = class (TAsteroid)
    public
    (* Initializes the sprite. *)
      procedure Initialize; override;
    end;



  (* THe asteroid manager. *)
    TAsteroidManager = class (TManager)
    private
      fLevel, fGracePeriod: Integer;
      fAsteroids: array [0..NUM_ASTS - 1] of TAsteroid;
      fPowerUp: TPowerUp;

      function GetAsteroid (const Ndx: Integer): TAsteroid; inline;
    public
    (* Constructor. *)
      constructor Create;
    (* Destructor. *)
      destructor Destroy; override;
    (* Initializes the manager. *)
      procedure Initialize; override;
    (* Starts a new game. *)
      procedure NewGame; override;
    (* Creates a new group of asteroids. *)
      procedure NewBoard (aNum: Integer);
    (* Updates sprites. *)
      procedure Update; override;
    (* Draw sprites. *)
      procedure Paint; override;

    (* Current level. *)
      property Level: Integer read fLevel write fLevel;
    (* Access to asteroids. *)
      property Asteroids[Ndx: Integer]: TAsteroid read GetAsteroid;
    (* Access to power-up. *)
      property PowerUp: TPowerUp read fPowerUp;
    end;

implementation

  uses
    GameMath, Graphics,
    Allegro5;

  const
  (* Ship grace period. *)
    GRACE_PERIOD = 50;
  (* Rotation rate. *)
    MAX_ROTATE_RATE = 8;
  (* Speed. *)
    MAX_VX = 4; MAX_VY = 4;
  (* Max size. *)
    MAX_RADIUS = 30;
  (* Size ratio between large/medium and medium/small asteroids. *)
    SIZE_RATIO = 2;
  (* More size constants. *)
    SMALL = 2; MEDIUM =1; LARGE = 0;
  (* Asteroid  values. *)
    VAL_SMALL = 90; VAL_MEDIUM = 60; VAL_LARGE = 30;
  (* Large/small asteroid size ratio. *)
    SRAT = SMALL * SIZE_RATIO;
  (* Large/medium asteroid size ratio. *)
    MRAT = MEDIUM * SIZE_RATIO;
  (* Variation in the asteroid shape. *)
    AVAR = 1.7;
  (* Used by intersection. *)
    MAX_RAD_LARGE  = MAX_RADIUS + Trunc (MAX_RADIUS / AVAR);
    MAX_RAD_MEDIUM = MAX_RADIUS + Trunc (MAX_RADIUS / AVAR / MRAT);
    MAX_RAD_SMALL  = MAX_RADIUS + Trunc (MAX_RADIUS / AVAR / SRAT);

  var
  (* Templates for asteroids. *)
    px, py: array [0..2, 0..8] of Integer;
(*
 * TAsteroid
 ***************************************************************************)

 (* Generates asteroid vectors, moving the given template randomly. *)
   procedure TAsteroid.MakeVectors (
     out tpx, tpy: array of Integer;
     const aSize: Integer
   );
   var
     lDeviation, Factor, Ndx: Integer;
   begin
   { Calculate maximun deviation. }
     if aSize = LARGE then
       lDeviation := Trunc (MAX_RADIUS / AVAR)
     else begin
     { Reduce deviation as it's a medium or small one. }
       Factor := aSize * SIZE_RATIO;
       lDeviation := Trunc (MAX_RADIUS / Factor / AVAR)
     end;
   { Calculate initial and end point. }
     tpx[0] := Trunc (GetRandomNumber (-lDeviation, lDeviation) + px[aSize][0]);
     tpy[0] := Trunc (GetRandomNumber (-lDeviation, lDeviation) + py[aSize][0]);
   { Close polygon. }
     tpx[High (tpx)] := tpx[0];
     tpx[High (tpy)] := tpy[0];
   { Calculate the rest of the points. }
     for Ndx := Low (tpx) to High (tpx) do
     begin
       tpx[Ndx] := Trunc (GetRandomNumber (-lDeviation, lDeviation) + px[aSize][Ndx]);
       tpy[Ndx] := Trunc (GetRandomNumber (-lDeviation, lDeviation) + py[aSize][Ndx])
     end
   end;



(* Initializes. *)
  procedure TAsteroid.InitializeAsteroid (const aSize: Integer);
  var
    lvx, lvy: array of Integer;
  begin
    inherited Initialize;
    if aSize = LARGE then
    begin
      SetLength (lvx, 9); SetLength (lvy, 9)
    end
    else begin
      SetLength (lvx, 5); SetLength (lvy, 5)
    end;
    Self.MakeVectors (lvx, lvy, aSize);
    Self.Polygon.SetVertices (lvx, lvy);
    case aSize of
      LARGE:  Self.Polygon.Color := LightBlue;
      MEDIUM: Self.Polygon.Color := Cyan;
      SMALL:  Self.Polygon.Color := Magenta;
    end;
    Self.Polygon.Fill := True
  end;



(*
 * TPowerUp
 ***************************************************************************)

(* Initializes. *)
  procedure TPowerUp.Initialize;
  const
    PowerX: array [0..8] of Integer = (
      10, 2, 0, -2, -10, -2, 0, 2, 10
    );
    PowerY: array [0..8] of Integer = (
      0, 2, 10, 2, 0, -2, -10, -2, 0
    );
  begin
    Self.Radius := 8;
    Polygon.Reset;
    Polygon.SetVertices (PowerX, PowerY);
    Polygon.Color := Green
  end;



(*
 * TAsteroidManager
 ***************************************************************************)

  function TAsteroidManager.GetAsteroid (const Ndx: Integer): TAsteroid;
  begin
    Result := fAsteroids[Ndx]
  end;



(* Constructor. *)
  constructor TAsteroidManager.Create;
  var
    Ndx: Integer;
  begin
    inherited Create;
    for Ndx := Low (fAsteroids) to High (fAsteroids) do
      fAsteroids[Ndx] := TAsteroid.Create;
    fPowerUp := TPowerUp.Create
  end;



(* Destructor. *)
  destructor TAsteroidManager.Destroy;
  var
    Ndx: Integer;
  begin
    fPowerUp.Free;
    for Ndx := Low (fAsteroids) to High (fAsteroids) do
      fAsteroids[Ndx].Free;
    inherited Destroy
  end;



(* Initializes. *)
  procedure TAsteroidManager.Initialize;
  const
    btpx: array [0..8] of Integer =
      (0, MAX_RADIUS div 2, MAX_RADIUS, MAX_RADIUS div 2, 0, -MAX_RADIUS div 2,
       -MAX_RADIUS, -MAX_RADIUS div 2, 0);
    btpy: array [0..8] of Integer =
      (MAX_RADIUS, MAX_RADIUS div 2, 0, -MAX_RADIUS div 2, -MAX_RADIUS,
       -MAX_RADIUS div 2, 0, MAX_RADIUS div 2, MAX_RADIUS);
  var
    lpx, lpy: array [0..4] of Integer;
    Ndx: Integer;
  begin
  { Templates. }
    lpx[0] := Trunc (cos (  0 * DEG_TO_RAD) * MAX_RADIUS);
    lpx[1] := Trunc (cos ( 72 * DEG_TO_RAD) * MAX_RADIUS);
    lpx[2] := Trunc (cos (144 * DEG_TO_RAD) * MAX_RADIUS);
    lpx[3] := Trunc (cos (216 * DEG_TO_RAD) * MAX_RADIUS);
    lpx[4] := Trunc (cos (288 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[0] := Trunc (sin (  0 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[1] := Trunc (sin ( 72 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[2] := Trunc (sin (144 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[3] := Trunc (sin (216 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[4] := Trunc (sin (288 * DEG_TO_RAD) * MAX_RADIUS);

    fPowerUp.Initialize;
    for Ndx := 0 to 8 do
    begin
      px[LARGE][Ndx] := btpx[Ndx];
      py[LARGE][Ndx] := btpy[Ndx]
    end;
    for Ndx := 0 to 4 do
    begin
      px[MEDIUM][Ndx] := Trunc (lpx[Ndx] / MRAT);
      py[MEDIUM][Ndx] := Trunc (lpy[Ndx] / MRAT);

      px[SMALL][Ndx] := Trunc (lpx[Ndx] / SMALL);
      py[SMALL][Ndx] := Trunc (lpy[Ndx] / SMALL)
    end;
  { Create asteroids. }
    for Ndx := 0 to NUM_LARGE - 1 do
    begin
      fAsteroids[Ndx].InitializeAsteroid (LARGE);
      fAsteroids[Ndx].Value := VAL_LARGE
    end;
    for Ndx := NUM_LARGE to NUM_LARGE + NUM_MEDIUM - 1 do
    begin
      fAsteroids[Ndx].InitializeAsteroid (MEDIUM);
      fAsteroids[Ndx].Value := VAL_MEDIUM
    end;
    for Ndx := NUM_LARGE + NUM_MEDIUM to NUM_LARGE + NUM_MEDIUM + NUM_SMALL - 1 do
    begin
      fAsteroids[Ndx].InitializeAsteroid (SMALL);
      fAsteroids[Ndx].Value := VAL_SMALL
    end
  end;



(* Starts a game. *)
  procedure TAsteroidManager.NewGame;
  begin
    fLevel := 1;
    Self.NewBoard (fLevel)
  end;



(* Creates a new group of asteroids. *)
  procedure TAsteroidManager.NewBoard (aNum: Integer);
  var
    lNumAsts, Ndx: Integer;
    lx, ly: Single;
  begin
  { Define the number of large asteroids. }
    lNumAsts := (aNum div 2) + 1;
    if lNumAsts > NUM_LARGE then lNumAsts := NUM_LARGE;
  { Position and speed of such asteroids. }
    for Ndx := Low (fAsteroids) to High (fAsteroids) do
    begin
    { Active asteroids. }
      if Ndx < lNumAsts then
      begin
        fAsteroids[Ndx].PosX := Random (MAX_WIDTH - 2 * MAX_RADIUS) + MAX_RADIUS;
        fAsteroids[Ndx].PosY := Random(MAX_HEIGHT - 2 * MAX_RADIUS) + MAX_RADIUS;
        repeat
          lx := GetRandomNumber (-MAX_VX, MAX_VX);
          ly := GetRandomNumber (-MAX_VY, MAX_VY)
        until (lx <> 0) or (ly <> 0);
        fAsteroids[Ndx].Vx := lx; fAsteroids[Ndx].Vy := ly;
        fAsteroids[Ndx].Restore
      end
      else
      { Suspend any other asteroid. }
        fAsteroids[Ndx].Suspend
    end;
  { Power up. }
    if not fPowerUp.Active then
    begin
      fPowerUp.PosX := Random (MAX_WIDTH - 2 * MAX_RADIUS) + MAX_RADIUS;
      fPowerUp.PosY := Random(MAX_HEIGHT - 2 * MAX_RADIUS) + MAX_RADIUS;
      fPowerUp.Restore
    end;
  { Ship grace period. }
    fGracePeriod := GRACE_PERIOD
  end;



(* Updates. *)
  procedure TAsteroidManager.Update;
  var
    lAsteroid: TAsteroid;
  begin
    fPowerUp.Update;
    for lAsteroid IN fAsteroids do lAsteroid.Update
  end;



(* Updates. *)
  procedure TAsteroidManager.Paint;
  var
    lAsteroid: TAsteroid;
  begin
    for lAsteroid IN fAsteroids do lAsteroid.Draw;
    fPowerUp.Draw
  end;

end.

