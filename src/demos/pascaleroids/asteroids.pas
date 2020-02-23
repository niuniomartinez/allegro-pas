UNIT Asteroids;
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

INTERFACE

  USES
    Engine;

  CONST
  (* How much big asteroids. *)
    NUM_LARGE = 6;
  (* How much medium asteroids. *)
    NUM_MEDIUM = NUM_LARGE * 2;
  (* How much small asteroids. *)
    NUM_SMALL = NUM_MEDIUM * 2;
  (* Total number of asteroids. *)
    NUM_ASTS = NUM_LARGE + NUM_MEDIUM + NUM_SMALL;


  TYPE
  (* Defines an asteroid. *)
    TAsteroid = CLASS (TMoveableSprite)
    PRIVATE
    (* Generates asteroid vectors, moving the given templates randomly. *)
      PROCEDURE MakeVectors (
        OUT tpx, tpy: ARRAY OF INTEGER;
        CONST aSize: INTEGER
      );
    PUBLIC
    (* Initializes the asteroid.

       This generates a new asteroid shape, sets its initial position and
       velocity. *)
      PROCEDURE InitializeAsteroid (CONST aSize: INTEGER);
    END;



  (* A power up. *)
    TPowerUp = CLASS (TAsteroid)
    PUBLIC
    (* Initializes the sprite. *)
      PROCEDURE Initialize; OVERRIDE;
    END;



  (* THe asteroid manager. *)
    TAsteroidManager = CLASS (TManager)
    PRIVATE
      fLevel, fGracePeriod: INTEGER;
      fAsteroids: ARRAY [0..NUM_ASTS - 1] OF TAsteroid;
      fPowerUp: TPowerUp;

      FUNCTION GetAsteroid (CONST Ndx: INTEGER): TAsteroid; INLINE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes the manager. *)
      PROCEDURE Initialize; OVERRIDE;
    (* Starts a new game. *)
      PROCEDURE NewGame; OVERRIDE;
    (* Creates a new group of asteroids. *)
      PROCEDURE NewBoard (aNum: INTEGER);
    (* Updates sprites. *)
      PROCEDURE Update; OVERRIDE;
    (* Draw sprites. *)
      PROCEDURE Paint; OVERRIDE;

    (* Current level. *)
      PROPERTY Level: INTEGER READ fLevel WRITE fLevel;
    (* Access to asteroids. *)
      PROPERTY Asteroids[Ndx: INTEGER]: TAsteroid READ GetAsteroid;
    (* Access to power-up. *)
      PROPERTY PowerUp: TPowerUp READ fPowerUp;
    END;

IMPLEMENTATION

  USES
    GameMath, Graphics,
    Allegro5;

  CONST
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
    MAX_RAD_LARGE  = MAX_RADIUS + TRUNC (MAX_RADIUS / AVAR);
    MAX_RAD_MEDIUM = MAX_RADIUS + TRUNC (MAX_RADIUS / AVAR / MRAT);
    MAX_RAD_SMALL  = MAX_RADIUS + TRUNC (MAX_RADIUS / AVAR / SRAT);

  VAR
  (* Templates for asteroids. *)
    px, py: ARRAY [0..2, 0..8] OF INTEGER;
(*
 * TAsteroid
 ***************************************************************************)

 (* Generates asteroid vectors, moving the given template randomly. *)
   PROCEDURE TAsteroid.MakeVectors (
     OUT tpx, tpy: ARRAY OF INTEGER;
     CONST aSize: INTEGER
   );
   VAR
     lDeviation, Factor, Ndx: INTEGER;
   BEGIN
   { Calculate maximun deviation. }
     IF aSize = LARGE THEN
       lDeviation := TRUNC (MAX_RADIUS / AVAR)
     ELSE BEGIN
     { Reduce deviation as it's a medium or small one. }
       Factor := aSize * SIZE_RATIO;
       lDeviation := TRUNC (MAX_RADIUS / Factor / AVAR)
     END;
   { Calculate initial and end point. }
     tpx[0] := TRUNC (GetRandomNumber (-lDeviation, lDeviation) + px[aSize][0]);
     tpy[0] := TRUNC (GetRandomNumber (-lDeviation, lDeviation) + py[aSize][0]);
   { Close polygon. }
     tpx[HIGH (tpx)] := tpx[0];
     tpx[HIGH (tpy)] := tpy[0];
   { Calculate the rest of the points. }
     FOR Ndx := LOW (tpx) TO HIGH (tpx) DO
     BEGIN
       tpx[Ndx] := TRUNC (GetRandomNumber (-lDeviation, lDeviation) + px[aSize][Ndx]);
       tpy[Ndx] := TRUNC (GetRandomNumber (-lDeviation, lDeviation) + py[aSize][Ndx])
     END
   END;



(* Initializes. *)
  PROCEDURE TAsteroid.InitializeAsteroid (CONST aSize: INTEGER);
  VAR
    lvx, lvy: ARRAY OF INTEGER;
  BEGIN
    INHERITED Initialize;
    IF aSize = LARGE THEN
    BEGIN
      SetLength (lvx, 9); SetLength (lvy, 9)
    END
    ELSE BEGIN
      SetLength (lvx, 5); SetLength (lvy, 5)
    END;
    SELF.MakeVectors (lvx, lvy, aSize);
    SELF.Polygon.SetVertices (lvx, lvy);
    CASE aSize OF
      LARGE:  SELF.Polygon.Color := LightBlue;
      MEDIUM: SELF.Polygon.Color := Cyan;
      SMALL:  SELF.Polygon.Color := Magenta;
    END;
    SELF.Polygon.Fill := TRUE
  END;



(*
 * TPowerUp
 ***************************************************************************)

(* Initializes. *)
  PROCEDURE TPowerUp.Initialize;
  CONST
    PowerX: ARRAY [0..8] OF INTEGER = (
      10, 2, 0, -2, -10, -2, 0, 2, 10
    );
    PowerY: ARRAY [0..8] OF INTEGER = (
      0, 2, 10, 2, 0, -2, -10, -2, 0
    );
  BEGIN
    SELF.Radius := 8;
    Polygon.Reset;
    Polygon.SetVertices (PowerX, PowerY);
    Polygon.Color := Green
  END;



(*
 * TAsteroidManager
 ***************************************************************************)

  FUNCTION TAsteroidManager.GetAsteroid (CONST Ndx: INTEGER): TAsteroid;
  BEGIN
    RESULT := fAsteroids[Ndx]
  END;



(* Constructor. *)
  CONSTRUCTOR TAsteroidManager.Create;
  VAR
    Ndx: INTEGER;
  BEGIN
    INHERITED Create;
    FOR Ndx := LOW (fAsteroids) TO HIGH (fAsteroids) DO
      fAsteroids[Ndx] := TAsteroid.Create;
    fPowerUp := TPowerUp.Create
  END;



(* Destructor. *)
  DESTRUCTOR TAsteroidManager.Destroy;
  VAR
    Ndx: INTEGER;
  BEGIN
    fPowerUp.Free;
    FOR Ndx := LOW (fAsteroids) TO HIGH (fAsteroids) DO
      fAsteroids[Ndx].Free;
    INHERITED Destroy
  END;



(* Initializes. *)
  PROCEDURE TAsteroidManager.Initialize;
  CONST
    btpx: ARRAY [0..8] OF INTEGER =
      (0, MAX_RADIUS DIV 2, MAX_RADIUS, MAX_RADIUS DIV 2, 0, -MAX_RADIUS DIV 2,
       -MAX_RADIUS, -MAX_RADIUS DIV 2, 0);
    btpy: ARRAY [0..8] OF INTEGER =
      (MAX_RADIUS, MAX_RADIUS DIV 2, 0, -MAX_RADIUS DIV 2, -MAX_RADIUS,
       -MAX_RADIUS DIV 2, 0, MAX_RADIUS DIV 2, MAX_RADIUS);
  VAR
    lpx, lpy: ARRAY [0..4] OF INTEGER;
    Ndx: INTEGER;
  BEGIN
  { Templates. }
    lpx[0] := TRUNC (cos (  0 * DEG_TO_RAD) * MAX_RADIUS);
    lpx[1] := TRUNC (cos ( 72 * DEG_TO_RAD) * MAX_RADIUS);
    lpx[2] := TRUNC (cos (144 * DEG_TO_RAD) * MAX_RADIUS);
    lpx[3] := TRUNC (cos (216 * DEG_TO_RAD) * MAX_RADIUS);
    lpx[4] := TRUNC (cos (288 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[0] := TRUNC (sin (  0 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[1] := TRUNC (sin ( 72 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[2] := TRUNC (sin (144 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[3] := TRUNC (sin (216 * DEG_TO_RAD) * MAX_RADIUS);
    lpy[4] := TRUNC (sin (288 * DEG_TO_RAD) * MAX_RADIUS);

    fPowerUp.Initialize;
    FOR Ndx := 0 TO 8 DO
    BEGIN
      px[LARGE][Ndx] := btpx[Ndx];
      py[LARGE][Ndx] := btpy[Ndx]
    END;
    FOR Ndx := 0 TO 4 DO
    BEGIN
      px[MEDIUM][Ndx] := TRUNC (lpx[Ndx] / MRAT);
      py[MEDIUM][Ndx] := TRUNC (lpy[Ndx] / MRAT);

      px[SMALL][Ndx] := TRUNC (lpx[Ndx] / SMALL);
      py[SMALL][Ndx] := TRUNC (lpy[Ndx] / SMALL)
    END;
  { Create asteroids. }
    FOR Ndx := 0 TO NUM_LARGE - 1 DO
    BEGIN
      fAsteroids[Ndx].InitializeAsteroid (LARGE);
      fAsteroids[Ndx].Value := VAL_LARGE
    END;
    FOR Ndx := NUM_LARGE TO NUM_LARGE + NUM_MEDIUM - 1 DO
    BEGIN
      fAsteroids[Ndx].InitializeAsteroid (MEDIUM);
      fAsteroids[Ndx].Value := VAL_MEDIUM
    END;
    FOR Ndx := NUM_LARGE + NUM_MEDIUM TO NUM_LARGE + NUM_MEDIUM + NUM_SMALL - 1 DO
    BEGIN
      fAsteroids[Ndx].InitializeAsteroid (SMALL);
      fAsteroids[Ndx].Value := VAL_SMALL
    END
  END;



(* Starts a game. *)
  PROCEDURE TAsteroidManager.NewGame;
  BEGIN
    fLevel := 1;
    SELF.NewBoard (fLevel)
  END;



(* Creates a new group of asteroids. *)
  PROCEDURE TAsteroidManager.NewBoard (aNum: INTEGER);
  VAR
    lNumAsts, Ndx: INTEGER;
    lx, ly: SINGLE;
  BEGIN
  { Define the number of large asteroids. }
    lNumAsts := (aNum DIV 2) + 1;
    IF lNumAsts > NUM_LARGE THEN lNumAsts := NUM_LARGE;
  { Position and speed of such asteroids. }
    FOR Ndx := LOW (fAsteroids) TO HIGH (fAsteroids) DO
    BEGIN
    { Active asteroids. }
      IF Ndx < lNumAsts THEN
      BEGIN
        fAsteroids[Ndx].PosX := Random (MAX_WIDTH - 2 * MAX_RADIUS) + MAX_RADIUS;
        fAsteroids[Ndx].PosY := Random(MAX_HEIGHT - 2 * MAX_RADIUS) + MAX_RADIUS;
        REPEAT
          lx := GetRandomNumber (-MAX_VX, MAX_VX);
          ly := GetRandomNumber (-MAX_VY, MAX_VY)
        UNTIL (lx <> 0) OR (ly <> 0);
        fAsteroids[Ndx].Vx := lx; fAsteroids[Ndx].Vy := ly;
        fAsteroids[Ndx].Restore
      END
      ELSE
      { Suspend any other asteroid. }
        fAsteroids[Ndx].Suspend
    END;
  { Power up. }
    IF NOT fPowerUp.Active THEN
    BEGIN
      fPowerUp.PosX := Random (MAX_WIDTH - 2 * MAX_RADIUS) + MAX_RADIUS;
      fPowerUp.PosY := Random(MAX_HEIGHT - 2 * MAX_RADIUS) + MAX_RADIUS;
      fPowerUp.Restore
    END;
  { Ship grace period. }
    fGracePeriod := GRACE_PERIOD
  END;



(* Updates. *)
  PROCEDURE TAsteroidManager.Update;
  VAR
    lAsteroid: TAsteroid;
  BEGIN
    fPowerUp.Update;
    FOR lAsteroid IN fAsteroids DO lAsteroid.Update
  END;



(* Updates. *)
  PROCEDURE TAsteroidManager.Paint;
  VAR
    lAsteroid: TAsteroid;
  BEGIN
    FOR lAsteroid IN fAsteroids DO lAsteroid.Draw;
    fPowerUp.Draw
  END;

END.

