UNIT game;
(* Program: Demo game for the Allegro.pas library.
 * Description: Game main loop.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

INTERFACE



(* RunGame:
 *   Runs the game main loop. *)
PROCEDURE RunGame;



IMPLEMENTATION

USES
  title, (* The title sequence. *)
  play;  (* The play loop. *)



(* RunGame:
 *   Runs the game main loop. *)
PROCEDURE RunGame;
BEGIN
{ The main loop. }
  WHILE RunTitle DO
    PlayGame;
END;



END.

