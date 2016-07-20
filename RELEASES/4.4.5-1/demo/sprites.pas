UNIT Sprites;
(* Program: Demo game for the Allegro.pas library.
 * Description: Manages a list of sprites and draws them.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

INTERFACE

  USES
    allegro;

  TYPE
  (* Sprite information. *)
    TSPRITEptr = ^TSPRITE;
    TSPRITE = RECORD
    { Sprite world coordinates. }
      x, y: INTEGER;
      { Index to the RLE description at datafile.  If it's (-1) then this sprite is
      "inactive" and it will not be drawn. }
      Index: INTEGER;
    END;

  CONST
    BMP_NONE = -1; { Inactive sprite. }

  VAR
  (* Array with sprite information.  The sprite '0' is the 'top' sprite, the
     '1' is drawn below the '0', the '2' is drawn below '1' and '0' and so...

     Don't set length directly, call @link (InitSprites) instead. *)
    SpritePlane: ARRAY OF TSPRITE;

(* Starts all sprites.  That is moves them out the board and sets them as
   "inactive".
   @param(NumSprites Number of sprites to use.) *)
  PROCEDURE InitSprites (NumSprites: INTEGER);

(* Checks if the given tile is below the sprite. *)
  FUNCTION CheckDownCollision (CONST SprNdx, TileNdx: INTEGER): BOOLEAN;

(* Same than CheckDownCollision but checks with a set of tiles. *)
  FUNCTION CheckDownCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): BOOLEAN;

(* Checks if the given tile is above the sprite. *)
  FUNCTION CheckUpCollision (CONST SprNdx, TileNdx: INTEGER): BOOLEAN;

(* Same than CheckUpCollision but checks with a set of tiles. *)
  FUNCTION CheckUpCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): BOOLEAN;

(* Checks if the given tile is on the left of the sprite. *)
  FUNCTION CheckLeftCollision (CONST SprNdx, TileNdx: INTEGER): BOOLEAN;

(* Same than CheckLeftCollision but checks with a set of tiles. *)
  FUNCTION CheckLeftCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): BOOLEAN;

(* Checks if the given tile is on the right of the sprite. *)
  FUNCTION CheckRightCollision (CONST SprNdx, TileNdx: INTEGER): BOOLEAN;

(* Same than CheckRightCollision but checks with a set of tiles. *)
  FUNCTION CheckRightCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): BOOLEAN;



(* Checks if the given tile collides with the sprite.  Returns a number
   to identify the quadrants witch the tile is as a combination (OR) of
   the following values:
   0 - No collision at all.
   1 - Top-left quadrant.
   2 - Top-right quadrant.
   4 - Bottom-left quadrant.
   8 - Bottom-Right quadrant.
   As example, if the tile is in both top quadrants, it will return:
    (1 OR 2) = 3 *)
  FUNCTION CheckCollision (CONST SprNdx, TileVal: INTEGER): INTEGER;

(* The same than CheckCollision but checks with a set of tiles. *)
  FUNCTION CheckCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): INTEGER;

(* Draws the sprites in the given bitmap at the given scroll coordinates. *)
  PROCEDURE DrawSprites (Bmp: AL_BITMAPptr; ScrollX, ScrollY: INTEGER);

(* Plays a sound sample changing the stereo 'pan' as the X coordinate
   of the sprite. *)
  PROCEDURE PlaySoundSample (Spr: INTEGER; Sample: AL_SAMPLEptr);



IMPLEMENTATION

USES
  gamedata, { To acces to de game datafile. }
  Tilemap;  { Tilemap management. }

VAR
  LastScrollX: INTEGER; { Needed for sound effects. }



(* Starts all sprites.  That is moves them out the board and sets them as
   "inactive". *)
  PROCEDURE InitSprites (NumSprites: INTEGER);
  VAR
    Cnt: INTEGER;
  BEGIN
    SetLength (SpritePlane, NumSprites);
    FOR Cnt := LOW (SpritePlane) TO HIGH (SpritePlane) DO
    BEGIN
    { This value was used by an old graphic chipset to disable hardware sprite
      planes.  It's used as an homenage. }
      SpritePlane[Cnt].y := -129;
      SpritePlane[Cnt].Index := BMP_NONE;
    END;
  END;



(* Checks if the given tile is below the sprite. *)
  FUNCTION CheckDownCollision (CONST SprNdx, TileNdx: INTEGER): BOOLEAN;
  BEGIN
    CheckDownCollision := FALSE;
  { First, checks if the sprite is just above a tile. }
    IF SpritePlane[SprNdx].y MOD TSIZE = 0 THEN
    BEGIN
    { Checks if the tile is not out of the map. }
      IF SpritePlane[SprNdx].y < (MapHeight - 1) * TSiZE THEN
      BEGIN
      { Checks the tile just below the sprite. }
	IF Map[(SpritePlane[SprNdx].x DIV TSIZE),
		(SpritePlane[SprNdx].y DIV TSIZE) + 1] = TileNdx
	THEN
	  CheckDownCollision := TRUE
	ELSE
	{ Checks if the sprite is in the middle of two tiles. }
	  IF SpritePlane[SprNdx].x MOD TSIZE <> 0 THEN
	  { It is, so checks again with the other tile. }
	    IF (SpritePlane[SprNdx].x DIV TSIZE < MapWidth - 1)
	    AND (Map[(SpritePlane[SprNdx].x DIV TSIZE) + 1,
		   (SpritePlane[SprNdx].y DIV TSIZE) + 1] = TileNdx)
	    THEN
	      CheckDownCollision := TRUE
      END;
    END;
  END;



(* Same than CheckDownCollision but checks with a set of tiles. *)
  FUNCTION CheckDownCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): BOOLEAN;
  VAR
    Tile: INTEGER;
  BEGIN
    CheckDownCollisionWith := FALSE;
  { First, checks if the sprite is just above a tile. }
    IF SpritePlane[SprNdx].y MOD TSIZE = 0 THEN
    BEGIN
    { Checks if the tile is not out of the map. }
      IF SpritePlane[SprNdx].y < (MapHeight - 1) * TSiZE THEN
      BEGIN
      { Checks the tile just below the sprite. }
        Tile := Map[(SpritePlane[SprNdx].x DIV TSIZE),
		    (SpritePlane[SprNdx].y DIV TSIZE) + 1];
      { Check if tile is inside the interval. }
	IF (FromTile <= Tile) AND (Tile <= ToTile) THEN
	  CheckDownCollisionWith := TRUE
	ELSE
	{ Checks if the sprite is in the middle of two tiles and inside map. }
	  IF (SpritePlane[SprNdx].x MOD TSIZE <> 0)
	  AND (SpritePlane[SprNdx].x DIV TSIZE < MapWidth - 1)
	  THEN BEGIN
	  { It is, so checks again with the other tile. }
	    Tile := Map[(SpritePlane[SprNdx].x DIV TSIZE) + 1,
			(SpritePlane[SprNdx].y DIV TSIZE) + 1];
	    CheckDownCollisionWith := (FromTile <= Tile) AND (Tile <= ToTile);
	  END;
      END;
    END;
  END;



(* Checks if the given tile is above the sprite. *)
  FUNCTION CheckUpCollision (CONST SprNdx, TileNdx: INTEGER): BOOLEAN;
  BEGIN
    CheckUpCollision := FALSE;
  { First, checks if the sprite is just above a tile. }
    IF SpritePlane[SprNdx].y MOD TSIZE = 0 THEN
    BEGIN
    { Checks if the tile is not out of the map. }
      IF SpritePlane[SprNdx].y DIV TSIZE > 0 THEN
      BEGIN
      { Checks the tile just above the sprite. }
	IF Map[(SpritePlane[SprNdx].x DIV TSIZE),
		(SpritePlane[SprNdx].y DIV TSIZE) - 1] = TileNdx
	THEN
	  CheckUpCollision := TRUE
	ELSE
	{ Checks if the sprite is in the middle of two tiles. }
	  IF SpritePlane[SprNdx].x MOD TSIZE <> 0 THEN
	  { It is, so checks again with the other tile. }
	    IF (SpritePlane[SprNdx].x DIV TSIZE < MapWidth - 1)
	    AND (Map[(SpritePlane[SprNdx].x DIV TSIZE) + 1,
		   (SpritePlane[SprNdx].y DIV TSIZE) - 1] = TileNdx)
	    THEN
	      CheckUpCollision := TRUE
      END;
    END;
  END;



(* Same than CheckUpCollision but checks with a set of tiles. *)
  FUNCTION CheckUpCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): BOOLEAN;
  VAR
    Tile: INTEGER;
  BEGIN
    CheckUpCollisionWith := FALSE;
  { First, checks if the sprite is just below a tile. }
    IF SpritePlane[SprNdx].y MOD TSIZE = 0 THEN
    BEGIN
    { Checks if the tile is not out of the map. }
      IF SpritePlane[SprNdx].y DIV TSIZE > 0 THEN
      BEGIN
      { Checks the tile just above the sprite. }
        Tile := Map[(SpritePlane[SprNdx].x DIV TSIZE),
		    (SpritePlane[SprNdx].y DIV TSIZE) - 1];
      { Check if tile is inside the interval. }
	IF (FromTile <= Tile) AND (Tile <= ToTile) THEN
	  CheckUpCollisionWith := TRUE
	ELSE
	{ Checks if the sprite is in the middle of two tiles. }
	  IF (SpritePlane[SprNdx].x MOD TSIZE <> 0)
	  AND (SpritePlane[SprNdx].x DIV TSIZE < MapWidth - 1)
	  THEN BEGIN
	  { It is, so checks again with the other tile. }
	    Tile := Map[(SpritePlane[SprNdx].x DIV TSIZE) + 1,
			(SpritePlane[SprNdx].y DIV TSIZE) - 1];
	    CheckUpCollisionWith := (FromTile <= Tile) AND (Tile <= ToTile);
	  END;
      END;
    END;
  END;



(* Checks if the given tile is on the left of the sprite. *)
  FUNCTION CheckLeftCollision (CONST SprNdx, TileNdx: INTEGER): BOOLEAN;
  BEGIN
    CheckLeftCollision := FALSE;
  { First, checks if the sprite is just in the right side of a tile. }
    IF SpritePlane[SprNdx].x MOD TSIZE = 0 THEN
    BEGIN
    { Now, Checks if the tile is inside the map. }
      IF SpritePlane[SprNdx].x DIV TSIZE > 0 THEN
      BEGIN
      { Checks the tile on the left of the sprite. }
	IF Map[(SpritePlane[SprNdx].x DIV TSIZE) - 1,
		(SpritePlane[SprNdx].y DIV TSIZE)] = TileNdx
	THEN
	  CheckLeftCollision := TRUE
	ELSE
	{ Checks if the sprite is in the middle of two tiles. }
	  IF SpritePlane[SprNdx].y MOD TSIZE <> 0 THEN
	  { It is, so checks again with the other tile. }
	    IF (SpritePlane[SprNdx].y DIV TSIZE < MapHeight - 1)
	    AND (Map[(SpritePlane[SprNdx].x DIV TSIZE) - 1,
		   (SpritePlane[SprNdx].y DIV TSIZE) + 1] = TileNdx)
	    THEN
	      CheckLeftCollision := TRUE
      END;
    END;
  END;



(* Same than CheckLeftCollision but checks with a set of tiles. *)
  FUNCTION CheckLeftCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): BOOLEAN;
  VAR
    Tile: INTEGER;
  BEGIN
    CheckLeftCollisionWith := FALSE;
  { First, checks if the sprite is just in the right side of a tile. }
    IF SpritePlane[SprNdx].x MOD TSIZE = 0 THEN
    BEGIN
    { Now, Checks if the tile is inside the map. }
      IF SpritePlane[SprNdx].x DIV TSIZE > 0 THEN
      BEGIN
      { Checks the tile on the left of the sprite. }
	Tile := Map[(SpritePlane[SprNdx].x DIV TSIZE) - 1,
		    (SpritePlane[SprNdx].y DIV TSIZE)];
	IF (FromTile <= Tile) AND (Tile <= ToTile) THEN
	  CheckLeftCollisionWith := TRUE
	ELSE
	{ Checks if the sprite is in the middle of two tiles. }
	  IF (SpritePlane[SprNdx].y MOD TSIZE <> 0)
	  AND (SpritePlane[SprNdx].y DIV TSIZE < MapHeight - 1)
	  THEN BEGIN
	  { It is, so checks again with the other tile. }
	    Tile := Map[(SpritePlane[SprNdx].x DIV TSIZE) - 1,
			(SpritePlane[SprNdx].y DIV TSIZE) + 1];
	    CheckLeftCollisionWith := (FromTile <= Tile) AND (Tile <= ToTile);
	  END;
      END;
    END;
  END;



(* Checks if the given tile is on the right of the sprite. *)
  FUNCTION CheckRightCollision (CONST SprNdx, TileNdx: INTEGER): BOOLEAN;
  BEGIN
    CheckRightCollision := FALSE;
  { First, checks if the sprite is just in the left side of a tile. }
    IF SpritePlane[SprNdx].x MOD TSIZE = 0 THEN
    BEGIN
    { Now, Checks if the tile is inside the map. }
      IF SpritePlane[SprNdx].x < (MapWidth - 1) * TSiZE THEN
      BEGIN
      { Checks the tile on the left of the sprite. }
	IF Map[(SpritePlane[SprNdx].x DIV TSIZE + 1),
		(SpritePlane[SprNdx].y DIV TSIZE)] = TileNdx
	THEN
	  CheckRightCollision := TRUE
	ELSE
	{ Checks if the sprite is in the middle of two tiles. }
	  IF SpritePlane[SprNdx].y MOD TSIZE <> 0 THEN
	  { It is, so checks again with the other tile. }
	    IF (SpritePlane[SprNdx].y DIV TSIZE < MapHeight - 1)
	    AND (Map[(SpritePlane[SprNdx].x DIV TSIZE + 1),
		  (SpritePlane[SprNdx].y DIV TSIZE) + 1] = TileNdx)
	    THEN
	      CheckRightCollision := TRUE
      END;
    END;
  END;



(* Same than CheckRightCollision but checks with a set of tiles. *)
  FUNCTION CheckRightCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): BOOLEAN;
  VAR
    Tile: INTEGER;
  BEGIN
    CheckRightCollisionWith := FALSE;
  { First, checks if the sprite is just in the left side of a tile. }
    IF SpritePlane[SprNdx].x MOD TSIZE = 0 THEN
    BEGIN
    { Now, Checks if the tile is inside the map. }
      IF SpritePlane[SprNdx].x < (MapWidth - 1) * TSiZE THEN
      BEGIN
      { Checks the tile on the left of the sprite. }
	Tile := Map[(SpritePlane[SprNdx].x DIV TSIZE + 1),
		    (SpritePlane[SprNdx].y DIV TSIZE)];
	IF (FromTile <= Tile) AND (Tile <= ToTile) THEN
	  CheckRightCollisionWith := TRUE
	ELSE
	{ Checks if the sprite is in the middle of two tiles. }
	  IF (SpritePlane[SprNdx].y MOD TSIZE <> 0)
	  AND (SpritePlane[SprNdx].y DIV TSIZE < MapHeight - 1)
	  THEN BEGIN
	  { It is, so checks again with the other tile. }
	    Tile := Map[(SpritePlane[SprNdx].x DIV TSIZE + 1),
			(SpritePlane[SprNdx].y DIV TSIZE) + 1];
	    CheckRightCollisionWith := (FromTile <= Tile) AND (Tile <= ToTile);
	  END;
      END;
    END;
  END;



(* Checks if the given tile collides with the sprite.  Returns a number
   to identify the quadrants witch the tile is as a combination (OR) of
   the following values:
    0 - No collision at all.
    1 - Top-left quadrant.
    2 - Top-right quadrant.
    4 - Bottom-left quadrant.
    8 - Bottom-Right quadrant.
   As example, if the tile is in both top quadrants, it will return:
   (1 OR 2) = 3 *)
  FUNCTION CheckCollision (CONST SprNdx, TileVal: INTEGER): INTEGER;
  VAR
    Tmp, Tx, Ty: INTEGER;
  BEGIN
    Tmp := 0;
  { Gets the tile coordinates of the top left pixel of the sprite. }
    Tx := SpritePlane[SprNdx].x DIV TSIZE;
    Ty := SpritePlane[SprNdx].y DIV TSIZE;
  { Checks only if where inside the map and the tile. }
    IF (0 <= Tx) AND (Tx < MapWidth) AND (0 <= Ty) AND (Ty < MapHeight) THEN
      IF Map[Tx, Ty] = TileVal THEN
	Tmp := 1;
    INC (Tx);
    IF (SpritePlane[SprNdx].x MOD TSIZE <> 0)
    AND (0 <= Tx) AND (Tx < MapWidth) AND (0 <= Ty) AND (Ty < MapHeight) THEN
      IF Map[Tx, Ty] = TileVal THEN
	Tmp := Tmp OR 2;
    INC (Ty);
    IF (SpritePlane[SprNdx].x MOD TSIZE <> 0)
    AND (SpritePlane[SprNdx].y MOD TSIZE <> 0)
    AND (0 <= Tx) AND (Tx < MapWidth) AND (0 <= Ty) AND (Ty < MapHeight) THEN
      IF Map[Tx, Ty] = TileVal THEN
	Tmp := Tmp OR 8;
    DEC (Tx);
    IF (SpritePlane[SprNdx].y MOD TSIZE <> 0)
    AND (0 <= Tx) AND (Tx < MapWidth) AND (0 <= Ty) AND (Ty < MapHeight) THEN
      IF Map[Tx, Ty] = TileVal THEN
	Tmp := Tmp OR 4;
  { Result. }
    CheckCollision := Tmp;
  END;



(* The same than CheckCollision but checks with a set of tiles. *)
  FUNCTION CheckCollisionWith (CONST SprNdx, FromTile, ToTile: INTEGER): INTEGER;
  VAR
    Tmp, Tx, Ty: INTEGER;
  BEGIN
    Tmp := 0;
  { Gets the tile coordinates of the top left pixel of the sprite. }
    Tx := SpritePlane[SprNdx].x DIV TSIZE;
    Ty := SpritePlane[SprNdx].y DIV TSIZE;
  { Checks only if where inside the map and the tile. }
    IF (0 <= Tx) AND (Tx < MapWidth) AND (0 <= Ty) AND (Ty < MapHeight) THEN
      IF (FromTile <= Map[Tx, Ty]) AND (Map[Tx, Ty] <= ToTile) THEN
	Tmp := 1;
    INC (Tx);
    IF (SpritePlane[SprNdx].x MOD TSIZE <> 0)
    AND (0 <= Tx) AND (Tx < MapWidth) AND (0 <= Ty) AND (Ty < MapHeight) THEN
      IF (FromTile <= Map[Tx, Ty]) AND (Map[Tx, Ty] <= ToTile) THEN
	Tmp := Tmp OR 2;
    INC (Ty);
    IF (SpritePlane[SprNdx].x MOD TSIZE <> 0)
    AND (SpritePlane[SprNdx].y MOD TSIZE <> 0)
    AND (0 <= Tx) AND (Tx < MapWidth) AND (0 <= Ty) AND (Ty < MapHeight) THEN
      IF (FromTile <= Map[Tx, Ty]) AND (Map[Tx, Ty] <= ToTile) THEN
	Tmp := Tmp OR 8;
    DEC (Tx);
    IF (SpritePlane[SprNdx].y MOD TSIZE <> 0)
    AND (0 <= Tx) AND (Tx < MapWidth) AND (0 <= Ty) AND (Ty < MapHeight) THEN
      IF (FromTile <= Map[Tx, Ty]) AND (Map[Tx, Ty] <= ToTile) THEN
	Tmp := Tmp OR 4;
  { Result. }
    CheckCollisionWith := Tmp;
  END;



(* Draws the sprites in the given bitmap at the given scroll coordinates. *)
  PROCEDURE DrawSprites (Bmp: AL_BITMAPptr; ScrollX, ScrollY: INTEGER);
  VAR
    Cnt: INTEGER;
    Posx, Posy: INTEGER;
    RLESprite: AL_RLE_SPRITEptr;
  BEGIN
    LastScrollX := ScrollX;
    FOR Cnt := HIGH (SpritePlane) DOWNTO LOW (SpritePlane) DO
    BEGIN
    { Draws only active sprites. }
      IF SpritePlane[Cnt].Index > BMP_NONE THEN
      BEGIN
      { Calculates screen coordinates from world ones. }
	Posx := SpritePlane[Cnt].x - ScrollX;
	Posy := SpritePlane[Cnt].y - ScrollY;
      { Gets the RLE description. }
	RLESprite := Data^[SpritePlane[Cnt].Index].dat;
      { Checks if the sprite is inside the output bitmap. }
	IF ((-RLESprite^.w) < Posx) AND ((-RLESprite^.h) < Posy)
	AND (Posx < Bmp^.w) AND (Posy < Bmp^.h) THEN
	{ Draws the sprite. }
	  al_draw_rle_sprite (Bmp, RLESprite, PosX, PosY);
      END;
    END;
  END;



(* Plays a sound sample changing the stereo 'pan' as the X coordinate of the
   sprite. *)
  PROCEDURE PlaySoundSample (Spr: INTEGER; Sample: AL_SAMPLEptr);
  VAR
    Pan: INTEGER;
  BEGIN
  { Calculates the 'pan'. }
    Pan := (SpritePlane[Spr].x - LastScrollX) * 256 DIV 320;
  { Plays the sample. }
    al_play_sample (Sample, 127, Pan, 1000, FALSE);
  END;

END.
