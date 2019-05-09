PROGRAM Project1;
(* A simple columns-like game. *)
(*
  Copyright (c) 2018 Guillermo Martínez J.

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

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

USES
  Allegro5, al5nativedlg,
  Configuration;

(* Initializes the application.  Returns FALSE on error. *)
  FUNCTION Initialize: BOOLEAN;
  BEGIN
  { Initialize Allegro and add-ons. }
    IF NOT al_init THEN
    BEGIN
      al_show_native_message_box (
        NIL, { There's no display yet! }
        'Error initialising',
        'An error occurred while initialising:',
        'Can''t initialize Allegro.  Be sure Allegro is correctly installed.',
        'Quit',
          ALLEGRO_MESSAGEBOX_ERROR
      );
      EXIT (FALSE)
    END;
    Configuration.Load;
    RESULT := TRUE
  END;



(* Finalizes the application, releasing all resources used by the game. *)
  PROCEDURE Finalize;
  BEGIN

  END;

BEGIN
  IF Initialize THEN
  BEGIN
    Finalize
  END
END.
