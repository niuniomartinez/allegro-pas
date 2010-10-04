UNIT game;
(* Program: Demo game for the Allegro.pas library.
 * Description: Game main loop.
 * Author: �u�o Mart�nez <niunio@users.sourceforge.net>
 *)

INTERFACE



(* Runs the game main loop. *)
PROCEDURE RunGame;



IMPLEMENTATION

USES
  title, (* The title sequence. *)
  play;  (* The play loop. *)



(* Runs the game main loop. *)
  PROCEDURE RunGame;
  BEGIN
  { The main loop. }
    WHILE RunTitle DO
      PlayGame;
  END;

END.

