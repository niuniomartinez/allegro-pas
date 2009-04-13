UNIT bombs;
(* Program: Demo game for the Allegro.pas library.
 * Description: Dropped down bombs.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

INTERFACE

(* InitBombs:
 *   Sets the initial state of the bombs. *)
  PROCEDURE InitBombs;



(* UpdateBombs:
 *   Updates the bombs. *)
  PROCEDURE UpdateBombs;



IMPLEMENTATION

USES
  aldigi,   { Digital sound. }
  alex,     { Alex the Allegator. }
  gamedata, { To acces to de game datafile. }
  play,	    { To know the size of the output screen. }
  sprites,  { Sprites management. }
  tilemap;  { Tilemap management. }



CONST
{ Number of bombs. }
  NUM_BOMBS = 6;



VAR
(* If the bomb is falling it is used to know how many time an sprite was used.
 * If the bomb is exploding it is used to know how many time was exploding. *)
  BombCount: ARRAY [0..NUM_BOMBS-1] OF INTEGER;



(* InitBombs:
 *   Sets the initial state of the bombs. *)
PROCEDURE InitBombs;
BEGIN
{ I know it does nothing. }
END;



(* UpdateBombs:
 *   Updates the bombs. *)
PROCEDURE UpdateBombs;
VAR
  Cnt: INTEGER;
  BombI, AlexX, AlexY: INTEGER;

(* Helper function to change the bomb state. *)
  PROCEDURE StartExploding;
  BEGIN
    SpritePlane[Cnt].Index := BMP_EXPLOSION1;
    DEC (SpritePlane[Cnt].x, 8);
    DEC (SpritePlane[Cnt].y, 8);
    BombCount[Cnt] := 0;
    PlaySoundSample (Cnt, Data^[SND_EXPLODE].dat);
  END;

BEGIN
{ Store Alex values.  Access to a simple integer variable is faster than access
  to an array element in most cases. }
  AlexX := SpritePlane[ALEX_SPR].x;
  AlexY := SpritePlane[ALEX_SPR].y;
{ Update all bombs. }
  FOR Cnt := 0 TO (NUM_BOMBS-1) DO
  BEGIN
    BombI := SpritePlane[Cnt].Index;
  { Check if the bomb is active. }
    IF BombI <> BMP_NONE THEN
    BEGIN
    { If it's not exploding. }
      IF BombI < BMP_EXPLOSION1 THEN
      BEGIN
      { Check if it reaches the ground. }
	IF (CheckGroundCollision (Cnt))
	OR (SpritePlane[Cnt].y > BoardHeight * TSIZE) THEN
	{ Explode}
	  StartExploding
	ELSE BEGIN
	  INC (BombCount[Cnt]);
	  IF BombCount[Cnt] MOD 2 = 0 THEN
	    INC (SpritePlane[Cnt].y);
	{ Check if it's time to change the sprite. }
	  IF BombCount[Cnt] MOD 8 = 0 THEN
	  BEGIN
	    IF SpritePlane[Cnt].Index < BMP_BOMB2 THEN
	      SpritePlane[Cnt].Index := BMP_BOMB2
	    ELSE
	      SpritePlane[Cnt].Index := BMP_BOMB1;
	  END;
	{ Collision with Alex. }
	  IF (ABS (AlexX - SpritePlane[Cnt].x) < 16)
	  AND (ABS (AlexY - SpritePlane[Cnt].y) < 16) THEN
	  BEGIN
	    StartExploding;
	    KillAlex;
	  END;
	END;
      END
      ELSE BEGIN
      { Still sploding? }
	INC (BombCount[Cnt]);
	IF BombCount[Cnt] < 75 THEN
	BEGIN
	  IF BombI < BMP_EXPLOSION2 THEN
	    SpritePlane[Cnt].Index := BMP_EXPLOSION2
	  ELSE
	    SpritePlane[Cnt].Index := BMP_EXPLOSION1;
	{ Collision with Alex.  It's different than the collision test above
	  because Alex and the explosion have different sizes.  This time I'm
	  using the center of each sprite.
	  Note that the distance of collision is smaller than the sprite sizes,
	  wich allows Alex to be near of an explosion.
	  Note also that it checks collision only in the first 24 frames. 
	  That means Alex can walk over the explosion after it starts.  This
	  make the game easer.  (Ken Silverman suggested this). }
	  IF BombCount[Cnt] < 24 THEN
	    IF (ABS ((AlexX + 8) - (SpritePlane[Cnt].x + 16)) < 22) 
	    AND (ABS ((AlexY + 8) - (SpritePlane[Cnt].y + 16)) < 22) THEN
	      KillAlex;
        END
	ELSE
	{ End exploding. }
	  SpritePlane[Cnt].Index := BMP_NONE;
      END;
    END
    ELSE BEGIN
    { Create a new bomb? }
      IF RANDOM (20) = 1 THEN
      BEGIN
        SpritePlane[Cnt].y := -10;
	SpritePlane[Cnt].x := ((((RANDOM (SCREENW) * 2) + SpritePlane[ALEX_SPR].x) - SCREENW) DIV TSIZE);
      { Check if there are ground tiles in the tile where the bomb appears. }
	IF Board[SpritePlane[Cnt].x + 1, 1] < T_BLK1 THEN
	BEGIN
	  SpritePlane[Cnt].x := SpritePlane[Cnt].x * TSIZE;
	  SPritePlane[Cnt].Index := BMP_BOMB1;
	  BombCount[Cnt] := 0;
	END;
      END;
    END;
  END;
END;



END.
