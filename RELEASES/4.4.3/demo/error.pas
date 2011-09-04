UNIT error;
(* Program: Demo game for the Allegro.pas library.
 * File: error.pas
 * Description: To show nice error messages.
 * Author: Ñuño Martínez <>
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

(* Shows a simple error message. *)
PROCEDURE ErrorMessage (Message: STRING);



IMPLEMENTATION

USES
  allegro;



(*   Shows a simple error message. *)
  PROCEDURE ErrorMessage (Message: STRING);

  (* Helper function. *)
    PROCEDURE DrawText (y: INTEGER; aText: STRING);
    BEGIN
      al_textout_centre_ex (al_screen, al_font, aText,
			    AL_SCREEN_W DIV 2, y,
			    al_makecol (255, 255, 255), -1);
    END;

  CONST
    PressKey = 'Press <Esc> key to continue';
  BEGIN
    al_rectfill (al_screen, 0, (AL_SCREEN_H * 2) DIV 5,
	         AL_SCREEN_W, (AL_SCREEN_H * 3) DIV 5, al_makecol (128, 0, 0));
    al_rect (al_screen, 0, (AL_SCREEN_H * 2) DIV 5,
	     AL_SCREEN_W, (AL_SCREEN_H * 3) DIV 5, al_makecol (255, 255, 255));
    DrawText ((AL_SCREEN_H DIV 2) - 12, Message);
    DrawText ((AL_SCREEN_H DIV 2) +  4, PressKey);
    WHILE (al_readkey SHR 8) <> AL_KEY_ESC DO
      al_rest (100);
  END;

END.
