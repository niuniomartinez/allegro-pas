UNIT framectr;
(* Program: Demo game for the Allegro.pas library.
 * Description: Function to control the frame rate.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

{$H+}

INTERFACE

VAR
(* This variable is incremented when the frame should be updated, that is 100
 * times each second. *)
  Tick: INTEGER;



(* InstallFrameRate:
 *   Install the frame controller.  Returns FALSE on failure or TRUE on
 *   success. *)
FUNCTION InstallFrameRate: BOOLEAN;



IMPLEMENTATION

USES
  altimer;  { Time control. }



CONST
  FPS = 100; { Desired framerate. }



(* TickProcedure:
 *   This is called by the Allegro's timer controller.  Increments Tick to tell
 *   how many frames should been updated. *)
PROCEDURE TickProcedure; CDECL;
BEGIN
  INC (Tick);
END;



(* InstallFrameRate:
 *   Installs the frame controller.  Returns FALSE on failure or TRUE on
 *   success. *)
FUNCTION InstallFrameRate: BOOLEAN;
BEGIN
{ Install the frame controller. }
  InstallFrameRate := al_install_int_ex (@TickProcedure, AL_BPS_TO_TIMER (FPS));
END;



END.
