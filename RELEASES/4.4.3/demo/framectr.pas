UNIT framectr;
(* Program: Demo game for the Allegro.pas library.
 * Description: Function to control the frame rate.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

INTERFACE

VAR
(* This variable is incremented when the frame should be updated, that is 100
 * times each second. *)
  Tick: INTEGER;

(* Install the frame controller.  Returns FALSE on failure or TRUE on
   success. *)
  FUNCTION InstallFrameRate: BOOLEAN;



IMPLEMENTATION

USES
  allegro;

CONST
  FPS = 100; { Desired framerate. }



(* This is called by the Allegro's timer controller.  Increments Tick to tell
   how many frames should been updated. *)
  PROCEDURE TickProcedure; CDECL;
  BEGIN
    INC (Tick);
  END;



(* Installs the frame controller.  Returns FALSE on failure or TRUE on
   success. *)
  FUNCTION InstallFrameRate: BOOLEAN;
  BEGIN
  { Install the frame controller. }
    InstallFrameRate := al_install_int_ex (@TickProcedure, AL_BPS_TO_TIMER (FPS));
  END;

END.
