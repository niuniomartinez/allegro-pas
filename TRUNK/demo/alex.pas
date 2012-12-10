UNIT alex;
(* Program: Demo game for the Allegro.pas library.
 * Description: Implements the player, personified by Alex the Allegator.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

INTERFACE

  CONST
  { The index of the spriteplane used by Alex. }
    ALEX_SPR = 9;

  VAR
  (* The scroll position is related with Alex position. *)
    ScrollX, ScrollY: INTEGER;
  (* If true, then Alex reaches end. *)
    AlexWin: BOOLEAN;
  (* If true, then Alex is dead. *)
    AlexDead: BOOLEAN;
  (* Number of collected coins. *)
    NumCoins: INTEGER;



(* Initializes Alex. *)
  PROCEDURE InitAlex;

(* Gets user input, check collision, etc. *)
  PROCEDURE UpdateAlex;

(* Used by the enemies to kill Alex. *)
  PROCEDURE KillAlex;



IMPLEMENTATION

USES
  gamedata, { To acces to the game datafile. }
  play,     { To know the size of the output screen. }
  sprites,  { Sprites management. }
  tilemap,  { Tilemap management. }
  allegro;

TYPE
{ Some state values to know wat's Alex doing. }
  TAlexStatus = (STAND, WALKING, JUMPING, FALLING, DEAD);

VAR
  AlexSpr: TSPRITEptr;
  AlexState: TAlexStatus;
(* If Alex is WALKING it is used to know how many time an sprite was used.
 * If Alex is JUMPING it is used to know how high he is. *)
  AlexCount: INTEGER;



(* Initializes Alex. *)
  PROCEDURE InitAlex;
  BEGIN
    AlexSpr := @(SpritePlane[ALEX_SPR]);
  { Puts Alex at the startpoint. }
    AlexSpr^.x := (StartX * TSIZE);
    AlexSpr^.y := (StartY * TSIZE);
  { Initial frame. }
    AlexSpr^.Index := BMP_MAIN_R0;
  { Initial State. }
    AlexState := STAND;
    AlexDead := FALSE;
    AlexWin := FALSE;
    AlexCount := 0;
  END;



(* Gets user input, check collision, etc. *)
  PROCEDURE UpdateAlex;

  (* Helper procedures to change Alex states. *)
    PROCEDURE StartStand;
    BEGIN
      AlexState := STAND;
    { Checks if Alex is facing left or right. }
      IF AlexSpr^.Index < BMP_MAIN_R0 THEN
	AlexSpr^.Index := BMP_MAIN_L0
      ELSE
	AlexSpr^.Index := BMP_MAIN_R0;
    END;

    PROCEDURE StartFall;
    BEGIN
      AlexState := FALLING;
    { Checks if Alex is facing left or right. }
      IF AlexSpr^.Index < BMP_MAIN_R0 THEN
	AlexSpr^.Index := BMP_MAIN_LJ
      ELSE
	AlexSpr^.Index := BMP_MAIN_RJ;
    END;

    PROCEDURE StartJump;
    BEGIN
    { Checks if Alex can jump (there's no ceiling). }
      IF CheckUpCollisionWith (ALEX_SPR, T_BLK1, T_BLK3) THEN
	EXIT;
      AlexState := JUMPING;
      AlexCount := 0;
    { Checks if Alex is facing left or right. }
      IF AlexSpr^.Index < BMP_MAIN_R0 THEN
	AlexSpr^.Index := BMP_MAIN_LJ
      ELSE
	AlexSpr^.Index := BMP_MAIN_RJ;
    { Sound. }
      PlaySoundSample (ALEX_SPR, Data^[SND_JUMP].dat);
    END;

    PROCEDURE StartWalk (SpriteDir: INTEGER); INLINE;
    BEGIN
      AlexState := WALKING;
      AlexSpr ^.Index := SpriteDir;
    END;

  (* Checks if there's a ground to walk over. *)
    FUNCTION CheckGround: BOOLEAN; INLINE;
    BEGIN
      CheckGround := CheckDownCollisionWith (ALEX_SPR, T_BLK1, T_BLK3);
      IF NOT CheckGround THEN
      { Alex will fall or is falling. }
	StartFall;
    END;

  VAR
    Tx, Ty, Tmp: INTEGER;
  BEGIN
  { Gets joystick information. }
    al_poll_joystick;
  { Each state is different. }
    CASE AlexState OF
    STAND:
      IF CheckGround THEN
      BEGIN
      { Checks if Alex is facing left or right. }
	IF AlexSpr^.Index < BMP_MAIN_R0 THEN
	  AlexSpr^.Index := BMP_MAIN_L0
	ELSE
	    AlexSpr^.Index := BMP_MAIN_R0;
      { Checks keyboard. }
	IF (al_key[AL_KEY_RIGHT] <> 0)
	OR (al_joy[0].stick[0].axis[0].d2 <> 0) THEN
	  StartWalk (BMP_MAIN_R0)
	ELSE IF (al_key[AL_KEY_LEFT] <> 0)
	OR (al_joy[0].stick[0].axis[0].d1 <> 0) THEN
	  StartWalk (BMP_MAIN_L0);
	IF (al_key[AL_KEY_SPACE] <> 0) OR (al_joy[0].button[0].b <> 0) THEN
	  StartJump;
      END;
    WALKING:
      IF CheckGround THEN
      BEGIN
      { Checks if Alex wants to jump. }
	IF (al_key[AL_KEY_SPACE] <> 0) OR (al_joy[0].button[0].b <> 0) THEN
	  StartJump
	ELSE
      { Checks walking left. }
	IF ((al_key[AL_KEY_LEFT]<>0) OR (al_joy[0].stick[0].axis[0].d1<>0))
	AND NOT CheckLeftCollisionWith (ALEX_SPR, T_BLK1, T_BLK3) THEN
	BEGIN
	{ Moves Alex. }
	  DEC (AlexSpr^.x);
	{ Checks if the sprite should be changed.  Without this, Alex will move
	  their legs too fast. }
	  INC (AlexCount);
	  IF AlexCount > 10 THEN
	  BEGIN
	    AlexCount := 0;
	  { Next sprite of walking motion. }
	    INC (AlexSpr^.Index);
	    IF AlexSpr^.Index > BMP_MAIN_L3 THEN
	      AlexSpr^.Index := BMP_MAIN_L0;
	  END;
	END
	ELSE
      { Checks Walking right. }
	IF ((al_key[AL_KEY_RIGHT] <> 0) OR (al_joy[0].stick[0].axis[0].d2<>0))
	AND NOT CheckRightCollisionWith (ALEX_SPR, T_BLK1, T_BLK3) THEN
	BEGIN
	{ Moves Alex. }
	  INC (AlexSpr^.x);
	{ Checks if the sprite should be changed.  Without this, Alex will move
	  their legs too fast. }
	  INC (AlexCount);
	  IF AlexCount > 10 THEN
	  BEGIN
	    AlexCount := 0;
	  { Checks if Alex changed his direction.  Otherwise it would do
	    "moon-walks".  Walking to the left side doesn't needs this check
	    because the "to the right animation" is beyond the last "to the
	    left animation" bitmap (BMP_MAIN_L3).  See demo.inc file to see the
	    indexes of each bitmap. }
	    IF AlexSpr^.Index < BMP_MAIN_R0 THEN
	      AlexSpr^.Index := BMP_MAIN_R0
	    ELSE BEGIN
	    { Next sprite of walking motion. }
	      INC (AlexSpr^.Index);
	      IF AlexSpr^.Index > BMP_MAIN_R3 THEN
		AlexSpr^.Index := BMP_MAIN_R0;
	    END;
	  END;
	END
	ELSE
	{ Changes AlexCount.  Without this Alex can do "moon-walks". }
	  AlexCount := 100;
      END;
    JUMPING:
      { Dont check ground. }
      BEGIN
      { Checks if we can jump. }
	IF (AlexCount < 32) AND NOT CheckUpCollisionWith (ALEX_SPR, T_BLK1, T_BLK3) THEN
	BEGIN
	  DEC (AlexSpr^.y, 2);
	  INC (AlexCount);
	  IF AlexCount > 24 THEN
	    INC (AlexSpr^.y);
	  { Can move while jumping. }
	  IF ((al_key[AL_KEY_LEFT]<>0) OR (al_joy[0].stick[0].axis[0].d1<>0))
	  AND NOT CheckLeftCollisionWith (ALEX_SPR, T_BLK1, T_BLK3) THEN
	  BEGIN
	    AlexSpr^.Index := BMP_MAIN_LJ;
	    DEC (AlexSpr^.x);
	  END;
	  IF ((al_key[AL_KEY_RIGHT]<>0) OR (al_joy[0].stick[0].axis[0].d2<>0))
	  AND NOT CheckRightCollisionWith (ALEX_SPR, T_BLK1, T_BLK3) THEN
	  BEGIN
	    AlexSpr^.Index := BMP_MAIN_RJ;
	    INC (AlexSpr^.x);
	  END;
	END
	ELSE
	  StartFall
      END;
    FALLING:
      IF NOT CheckGround THEN
      BEGIN
	INC (AlexSpr^.y, 2);
      { Can move while falling. }
	IF ((al_key[AL_KEY_LEFT] <> 0) OR (al_joy[0].stick[0].axis[0].d1<>0))
	AND NOT CheckLeftCollisionWith (ALEX_SPR, T_BLK1, T_BLK3) THEN
	BEGIN
	  AlexSpr^.Index := BMP_MAIN_LJ;
	  DEC (AlexSpr^.x);
	END;
	IF ((al_key[AL_KEY_RIGHT] <> 0) OR (al_joy[0].stick[0].axis[0].d2<>0))
	AND NOT CheckRightCollisionWith (ALEX_SPR, T_BLK1, T_BLK3) THEN
	BEGIN
	  AlexSpr^.Index := BMP_MAIN_RJ;
	  INC (AlexSpr^.x);
	END;
      { If Alex reaches the bottom of the map, he deads. }
	IF AlexSpr^.y >= (MapHeight * TSIZE) THEN
	  KillAlex;
      END
      ELSE
      { There's ground. }
	StartStand;
    DEAD:
      BEGIN
      { Going left-rigth. }
	IF (AlexSpr^.y AND $00000001) <> 0 THEN
	  IF AlexSpr^.Index < BMP_MAIN_R0 THEN
	    INC (AlexSpr^.x)
	  ELSE
	    DEC (AlexSpr^.x);
      { Going up? }
	IF AlexCount < 50 THEN
	BEGIN
	  DEC (AlexSpr^.y);
	  IF AlexCount < 25 THEN
	    DEC (AlexSpr^.y);
	END
	ELSE IF AlexCount > 65 THEN
	BEGIN
	{ Going down. }
	  INC (AlexSpr^.y, 2);
	  IF AlexCount > 86 THEN
	    INC (AlexSpr^.y);
	  IF AlexSpr^.y > (MapHeight * TSIZE) THEN
	    AlexDead := TRUE;
	END;
	INC (AlexCount);
      END;
    END;
  { Controls map limits. }
    IF AlexSpr^.x < 0 THEN
      AlexSpr^.x := 0;
    IF AlexSpr^.x > (MapWidth - 1) * TSIZE THEN
      AlexSpr^.x := (MapWidth - 1) * TSIZE;
  { Checks if Alex gets coins. }
    IF AlexState <> DEAD THEN
    BEGIN
      Tmp := CheckCollision (ALEX_SPR, T_COIN);
      IF Tmp <> 0 THEN
      BEGIN
	PlaySoundSample (ALEX_SPR, Data^[SND_COIN].dat);
	Tx := AlexSpr^.x DIV TSIZE;
	Ty := AlexSpr^.y DIV TSIZE;
	IF (Tmp AND 1) <> 0 THEN
	  BEGIN
	  INC (NumCoins);
	  Map[Tx, Ty] := T_VOID;
	END;
	IF (Tmp AND 2) <> 0 THEN
	BEGIN
	  INC (NumCoins);
	  Map[Tx + 1, Ty] := T_VOID;
	END;
	IF (Tmp AND 4) <> 0 THEN
	BEGIN
	  INC (NumCoins);
	  Map[Tx, Ty + 1] := T_VOID;
	END;
	IF (Tmp AND 8) <> 0 THEN
	BEGIN
	  INC (NumCoins);
	  Map[Tx + 1, Ty + 1] := T_VOID;
	END;
      END;
    END;
  { Checks if Alex reaches the end of the map. }
    IF NOT AlexDead
    AND (AlexSpr^.x DIV TSIZE = EndX) AND (AlexSpr^.y DIV TSIZE = EndY) THEN
    BEGIN
      PlaySoundSample (ALEX_SPR, Data^[SND_WIN].dat);
      AlexWin := TRUE;
    END;
  { Calculates the scroll position from Alex position. }
    ScrollX := AlexSpr^.x - (SCREENW DIV 2) + (TSIZE DIV 2);
    ScrollY := AlexSpr^.y - (SCREENH DIV 2) + (TSIZE DIV 2);
  END;



(* Used by the enemies to kill Alex. *)
  PROCEDURE KillAlex;
  BEGIN
    IF AlexState <> DEAD THEN
    BEGIN
      AlexState := DEAD;
      AlexCount := 0;
    { Checks if Alex is facing left or right. }
      IF AlexSpr^.Index < BMP_MAIN_R0 THEN
	AlexSpr^.Index := BMP_MAIN_LD
      ELSE
	AlexSpr^.Index := BMP_MAIN_RD;
      PlaySoundSample (ALEX_SPR, Data^[SND_DEAD].dat);
    END;
  END;

END.
