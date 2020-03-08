UNIT Actor;
(*<Defines the @link(TActor) object. *)
(*
  Copyright (c) 2018 Handoko
            (c) 2019-2020 Guillermo Martínez.

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
    Allegro5, al5base;

  CONST
  (* Max number of frames in an animation. *)
    MAX_FRAMES = 10;

  TYPE
  (* Pointer to an animation. *)
    PAnimation = ^TAnimation;
  (* Stores an animation. *)
    TAnimation = RECORD
      Images: array [0..MAX_FRAMES - 1] of ALLEGRO_BITMAPptr;
      Count:  Byte;
      Delay:  Byte;
    END;

  (* Actor activity.  This is, what he's doing. *)
    TActivity = (WalkL, WalkR, AttackL, AttackR, IdleL, IdleR, HitL, HitR, KilledL, KilledR);
  (* Pointer to actor. *)
    PActor = ^TActor;
  (* Defines an actor, with its postion, speed, activity and animation. *)
    TActor = record
      PosX, PosY:      Real;
      MinX, MaxX:      Real;
      Speed:           Real;
      CurrentActivity: TActivity;
      LastActivity:    TActivity;
      AnimImage:       PAnimation;
      AnimProgress:    Byte;
      AnimDelay:       Byte;
    end;



(* Loads an animation.  @return(@true on success, @false on failure.) *)
  FUNCTION LoadAnimation (
    aFileName: AL_STR; CONST aDelay: INTEGER; OUT aAnimation: TAnimation
  ): BOOLEAN;

(* Releases resources used by an animation. *)
  PROCEDURE UnloadAnimation (VAR aAnimation: TAnimation);

IMPLEMENTATION

  USES
    Data,
    al5strings;

(* Loads an animation.  @return(@true on success, @false on failure.) *)
  FUNCTION LoadAnimation (
    aFileName: AL_STR; CONST aDelay: INTEGER; OUT aAnimation: TAnimation
  ): BOOLEAN;
  VAR
    Ndx: INTEGER;
  BEGIN
    aAnimation.Delay := aDelay;
    aFileName := aFileName + '%0.2d.png';
    FOR Ndx := 0 TO MAX_FRAMES - 1 DO
    BEGIN
      aAnimation.Images[Ndx] := LoadBitmap (al_str_format (aFileName, [Ndx]));
      IF aAnimation.Images[Ndx] = NIL THEN
      BEGIN
	aAnimation.Count := Ndx;
	EXIT (Ndx > 0)
      END
    END;
    aAnimation.Count := MAX_FRAMES;
    RESULT := TRUE
  END;



(* Releases resources used by an animation. *)
  PROCEDURE UnloadAnimation (VAR aAnimation: TAnimation);
  VAR
    Ndx: INTEGER;
  BEGIN
    aAnimation.Count := 0;
    FOR Ndx := LOW (aAnimation.Images) TO HIGH (aAnimation.Images) DO
    BEGIN
      IF aAnimation.Images[Ndx] <> NIL THEN
	al_destroy_bitmap (aAnimation.Images[Ndx])
      ELSE
	EXIT;
      aAnimation.Images[Ndx] := NIL
    END
  END;

END.
