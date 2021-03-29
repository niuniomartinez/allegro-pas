unit Actor;
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


interface

  uses
    Allegro5, al5base;

  const
  (* Max number of frames in an animation. *)
    MAX_FRAMES = 10;

  type
  (* Pointer to an animation. *)
    PAnimation = ^TAnimation;
  (* Stores an animation. *)
    TAnimation = record
      Images: array [0..MAX_FRAMES - 1] of ALLEGRO_BITMAPptr;
      Count:  Byte;
      Delay:  Byte;
    end;

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
  function LoadAnimation (
    aFileName: AL_STR; const aDelay: Integer; out aAnimation: TAnimation
  ): Boolean;

(* Releases resources used by an animation. *)
  procedure UnloadAnimation (var aAnimation: TAnimation);

implementation

  uses
    Data,
    al5strings;

(* Loads an animation.  @return(@true on success, @false on failure.) *)
  function LoadAnimation (
    aFileName: AL_STR; const aDelay: Integer; out aAnimation: TAnimation
  ): Boolean;
  var
    Ndx: Integer;
  begin
    aAnimation.Delay := aDelay;
    aFileName := aFileName + '%0.2d.png';
    for Ndx := 0 to MAX_FRAMES - 1 do
    begin
      aAnimation.Images[Ndx] := LoadBitmap (al_str_format (aFileName, [Ndx]));
      if aAnimation.Images[Ndx] = Nil then
      begin
	aAnimation.Count := Ndx;
	Exit (Ndx > 0)
      end
    end;
    aAnimation.Count := MAX_FRAMES;
    Result := True
  end;



(* Releases resources used by an animation. *)
  procedure UnloadAnimation (var aAnimation: TAnimation);
  var
    Ndx: Integer;
  begin
    aAnimation.Count := 0;
    for Ndx := Low (aAnimation.Images) to High (aAnimation.Images) do
    begin
      if aAnimation.Images[Ndx] <> Nil then
	al_destroy_bitmap (aAnimation.Images[Ndx])
      else
	Exit;
      aAnimation.Images[Ndx] := Nil
    end
  end;

end.
