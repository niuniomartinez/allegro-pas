UNIT sprites;
(* Program: Demo game for the Allegro.pas library.
 * Description: Manages a list of sprites and draws them.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

INTERFACE

USES
  allegro;



TYPE
(* TSPRITE:
 *   Sprite information. *)
  TSPRITEptr = ^TSPRITE;
  TSPRITE = RECORD
  { Sprite world coordinates. }
    x, y: INTEGER;
  { Index to the RLE description at datafile.  If it's (-1) then this sprite is
    "inactive" and it will not be drawn. }
    Index: INTEGER;
  END;



CONST
  NUM_SPRITES = 10; { Number of sprites. }
  BMP_NONE = -1; { Inactive sprite. }



VAR
(* SpritePlane:
 *   Array with sprite information.  The sprite '0' is the 'top' sprite, the
 *   '1' is drawn below the '0', the '2' is drawn below '1' and '0' and so... *)
  SpritePlane: ARRAY [0..(NUM_SPRITES - 1)] OF TSPRITE;



  (* InitSprites:
   *   Starts all sprites.  That is moves them out the board and sets them as
   *   "inactive". *)
  PROCEDURE InitSprites;



  (* CheckGroundCollision:
   *   Checks if there's a ground tile below the sprite. *)
  FUNCTION CheckGroundCollision (CONST SprNdx: INTEGER): BOOLEAN;



  (* CheckCelinCollision:
   *   Checks if there's a ceil tile above the sprite. *)
  FUNCTION CheckCeilCollision (CONST SprNdx: INTEGER): BOOLEAN;



  (* CheckLeftCollision:
   *   Checks if there's a ground tile on the left of the sprite. *)
  FUNCTION CheckLeftCollision (CONST SprNdx: INTEGER): BOOLEAN;



  (* CheckRightCollision:
   *   Checks if there's a ground tile on the right of the sprite. *)
  FUNCTION CheckRightCollision (CONST SprNdx: INTEGER): BOOLEAN;



  (* CheckCollisionWith:
   *   Checks if the given tile collides with the sprite.  Returns a number
   *   to identify the quadrants witch the tile is as a combination (OR) of
   *   the following values:
   *   0 - No collision at all.
   *   1 - Top-left quadrant.
   *   2 - Top-right quadrant.
   *   4 - Bottom-left quadrant.
   *   8 - Bottom-Right quadrant.
   *   As example, if the tile is in both top quadrants, it will return:
   *    (1 OR 2) = 3 *)
  FUNCTION CheckCollisionWith (CONST SprNdx, TileVal: INTEGER): INTEGER;



  (* DrawSprites:
   *   Draws the sprites in the given bitmap at the given scroll coordinates. *)
  PROCEDURE DrawSprites (Bmp: AL_BITMAPptr; ScrollX, ScrollY: INTEGER);



  (* PlaySoundSample:
   *   Plays a sound sample changing the stereo 'pan' as the X coordinate
   *   of the sprite. *)
  PROCEDURE PlaySoundSample (Spr: INTEGER; Sample: AL_SAMPLEptr);



IMPLEMENTATION

USES
  gamedata, { To acces to de game datafile. }
  tilemap;  { Tilemap management. }



VAR
  LastScrollX: INTEGER; { Needed for sound effects. }



(* InitSprites:
 *   Starts all sprites.  That is moves them out the board and sets them as
 *   "inactive". *)
PROCEDURE InitSprites;
VAR
  Cnt: INTEGER;
BEGIN
  FOR Cnt := 0 TO (NUM_SPRITES -1) DO
  BEGIN
  { This value was used by an old graphic chipset to disable hardware sprite
    planes.  It's used as an homenage. }
    SpritePlane[Cnt].y := -129;
    SpritePlane[Cnt].Index := BMP_NONE;
  END;
END;




(* CheckGround:
 *   Check if there's a ground tile below the sprite. *)
FUNCTION CheckGroundCollision (CONST SprNdx: INTEGER): BOOLEAN;
BEGIN
  CheckGroundCollision := FALSE;
{ First, check if the sprite is just above a tile. }
  IF SpritePlane[SprNdx].y MOD TSIZE <> 0 THEN
    EXIT;
{ Check if the tile to is not out of the map. }
  IF SpritePlane[SprNdx].y >= (BoardHeight - 1) * TSiZE THEN
    CheckGroundCollision := FALSE
  ELSE
{ Check the tile just below the sprite. }
  IF Board[(SpritePlane[SprNdx].x DIV TSIZE) + 1,
	   (SpritePlane[SprNdx].y DIV TSIZE) + 2] >= T_BLK1 THEN
    CheckGroundCollision := TRUE
  ELSE
  { Check if the sprite is in the middle of two tiles. }
    IF SpritePlane[SprNdx].x MOD TSIZE <> 0 THEN
    { It is, so check again with the other tile. }
      IF Board[(SpritePlane[SprNdx].x DIV TSIZE) + 2,
	       (SpritePlane[SprNdx].y DIV TSIZE) + 2] >= T_BLK1 THEN
	CheckGroundCollision := TRUE
END;



(* CheckCelinCollision:
 *   Check if there's a ceil tile above the sprite. *)
FUNCTION CheckCeilCollision (CONST SprNdx: INTEGER): BOOLEAN;
BEGIN
  CheckCeilCollision := FALSE;
{ First, check if the sprite is just below a tile. }
  IF SpritePlane[SprNdx].y MOD TSIZE <> 0 THEN
    EXIT;
{ Check if the tile is not out of the map. }
  IF SpritePlane[SprNdx].y DIV TSIZE < 1 THEN
    CheckCeilCollision := TRUE
  ELSE
{ Check the tile just above the sprite. }
  IF Board[(SpritePlane[SprNdx].x DIV TSIZE) + 1,
	   (SpritePlane[SprNdx].y DIV TSIZE)] >= T_BLK1 THEN
    CheckCeilCollision := TRUE
  ELSE
  { Check if the sprite is in the middle of two tiles. }
    IF SpritePlane[SprNdx].x MOD TSIZE <> 0 THEN
    { It is, so check again with the other tile. }
      IF Board[(SpritePlane[SprNdx].x DIV TSIZE) + 2,
	       (SpritePlane[SprNdx].y DIV TSIZE)] >= T_BLK1 THEN
	CheckCeilCollision := TRUE
END;



(* CheckLeftCollision:
 *   Check if there's a ground tile on the left of the sprite. *)
FUNCTION CheckLeftCollision (CONST SprNdx: INTEGER): BOOLEAN;
BEGIN
  CheckLeftCollision := FALSE;
{ First, check if the sprite is just in the right side of a tile. }
  IF SpritePlane[SprNdx].x MOD TSIZE <> 0 THEN
    EXIT;
{ Now, Check if the tile to chek is inside the map. }
  IF SpritePlane[SprNdx].x DIV TSIZE < 1 THEN
  BEGIN
    CheckLeftCollision := TRUE;
    EXIT;
  END;
{ Check the tile on the left of the sprite. }
  IF Board[(SpritePlane[SprNdx].x DIV TSIZE),
	   (SpritePlane[SprNdx].y DIV TSIZE) + 1] >= T_BLK1 THEN
    CheckLeftCollision := TRUE
  ELSE
  { Check if the sprite is in the middle of two tiles. }
    IF SpritePlane[SprNdx].y MOD TSIZE <> 0 THEN
    { It is, so check again with the other tile. }
      IF Board[(SpritePlane[SprNdx].x DIV TSIZE),
	       (SpritePlane[SprNdx].y DIV TSIZE) + 2] >= T_BLK1 THEN
	CheckLeftCollision := TRUE
END;



(* CheckRightCollision:
 *   Check if there's a ground tile on the right of the sprite. *)
FUNCTION CheckRightCollision (CONST SprNdx: INTEGER): BOOLEAN;
BEGIN
  CheckRightCollision := FALSE;
{ First, check if the sprite is just in the right side of a tile. }
  IF SpritePlane[SprNdx].x MOD TSIZE <> 0 THEN
    EXIT;
{ Now, Check if the tile to chek is inside the map. }
  IF (SpritePlane[SprNdx].x DIV TSIZE) + 2 > BoardLength THEN
  BEGIN
    CheckRightCollision := TRUE;
    EXIT;
  END;
{ Check the tile on the right of the sprite. }
  IF Board[(SpritePlane[SprNdx].x DIV TSIZE) + 2,
	   (SpritePlane[SprNdx].y DIV TSIZE) + 1] >= T_BLK1 THEN
    CheckRightCollision := TRUE
  ELSE
  { Check if the sprite is in the middle of two tiles. }
    IF SpritePlane[SprNdx].y MOD TSIZE <> 0 THEN
    { It is, so check again with the other tile. }
      IF Board[(SpritePlane[SprNdx].x DIV TSIZE) + 2,
	       (SpritePlane[SprNdx].y DIV TSIZE) + 2] >= T_BLK1 THEN
	CheckRightCollision := TRUE
END;



(* CheckCollisionWith:
 *   Checks if the given tile collides with the sprite.  Returns a number
 *   to identify the quadrants witch the tile is as a combination (OR) of
 *   the following values:
 *   0 - No collision at all.
 *   1 - Top-left quadrant.
 *   2 - Top-right quadrant.
 *   4 - Bottom-left quadrant.
 *   8 - Bottom-Right quadrant.
 *   As example, if the tile is in both top quadrants, it will return:
 *    (1 OR 2) = 3 *)
FUNCTION CheckCollisionWith (CONST SprNdx, TileVal: INTEGER): INTEGER;
VAR
  Tmp, Tx, Ty: INTEGER;
BEGIN
  Tmp := 0;
{ Get the tile coordinates of the top left pixel of the sprite. }
  Tx := (SpritePlane[SprNdx].x DIV TSIZE) + 1;
  Ty := (SpritePlane[SprNdx].y DIV TSIZE) + 1;
{ Check only if where inside the map and the tile. }
  IF (0 < Tx) AND (Tx <= BoardLength) AND (0 < Ty) AND (Ty <= BoardHeight) THEN
    IF Board[Tx, Ty] = TileVal THEN
      Tmp := 1;
  INC (Tx);
  IF (SpritePlane[SprNdx].x MOD TSIZE <> 0)
  AND (0 < Tx) AND (Tx <= BoardLength) AND (0 < Ty) AND (Ty <= BoardHeight) THEN
    IF Board[Tx, Ty] = TileVal THEN
      Tmp := Tmp OR 2;
  INC (Ty);
  IF (SpritePlane[SprNdx].x MOD TSIZE <> 0)
  AND (SpritePlane[SprNdx].y MOD TSIZE <> 0)
  AND (0 < Tx) AND (Tx <= BoardLength) AND (0 < Ty) AND (Ty <= BoardHeight) THEN
    IF Board[Tx, Ty] = TileVal THEN
      Tmp := Tmp OR 8;
  DEC (Tx);
  IF (SpritePlane[SprNdx].y MOD TSIZE <> 0)
  AND (0 < Tx) AND (Tx <= BoardLength) AND (0 < Ty) AND (Ty <= BoardHeight) THEN
    IF Board[Tx, Ty] = TileVal THEN
      Tmp := Tmp OR 4;
{ Result. }
  CheckCollisionWith := Tmp;
END;



(* DrawSprites:
 *   Draws the sprites in the given bitmap at the given scroll coordinates. *)
PROCEDURE DrawSprites (Bmp: AL_BITMAPptr; ScrollX, ScrollY: INTEGER);
VAR
  Cnt: INTEGER;
  Posx, Posy: INTEGER;
  RLESprite: AL_RLE_SPRITEptr;
BEGIN
  LastScrollX := ScrollX;
  FOR Cnt := (NUM_SPRITES - 1) DOWNTO 0 DO
  BEGIN
  { Draw only active sprites. }
    IF SpritePlane[Cnt].Index > BMP_NONE THEN
    BEGIN
    { Calculate screen coordinates from world ones. }
      Posx := SpritePlane[Cnt].x - ScrollX;
      Posy := SpritePlane[Cnt].y - ScrollY;
    { Get the RLE description. }
      RLESprite := Data^[SpritePlane[Cnt].Index].dat;
    { Check if the sprite is inside the output bitmap. }
      IF ((-RLESprite^.w) < Posx) AND ((-RLESprite^.h) < Posy)
      AND (Posx < Bmp^.w) AND (Posy < Bmp^.h) THEN
      { Draw the sprite. }
	al_draw_rle_sprite (Bmp, RLESprite, PosX, PosY);
    END;
  END;
END;



(* PlaySoundSample:
 *   Plays a sound sample changing the stereo 'pan' as the X coordinate
 *   of the sprite. *)
PROCEDURE PlaySoundSample (Spr: INTEGER; Sample: AL_SAMPLEptr);
VAR
  Pan: INTEGER;
BEGIN
{ Calculate the 'pan'. }
  Pan := (SpritePlane[Spr].x - LastScrollX) * 256 DIV 320;
{ Play the sample. }
  al_play_sample (Sample, 127, Pan, 1000, 0);
END;

END.
