UNIT Game;
(*<Extends class @link(TCustomGameApplication) completing with the actual
   Game Code. *)
(*
  Copyright (c) 2015 Guillermo Martínez J.

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
    Application;

  TYPE
  (* The final game application class. *)
    TGameApplication = CLASS (TCustomGameApplication)
    PROTECTED
    (* The game flows from here. *)
      PROCEDURE DoRun; OVERRIDE;
    PUBLIC
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes the game. *)
      PROCEDURE Initialize; OVERRIDE;
    END;

  VAR
  (* The game object. *)
    Zombie3DGame: TGameApplication;

IMPLEMENTATION

  USES
    Allegro, sysutils;

(*
 * TGameApplication
 ****************************************************************************)

(* The game flows from here. *)
  PROCEDURE TGameApplication.DoRun;
  BEGIN
    IF al_keypressed THEN SELF.Terminate
  END;



(* Destructor. *)
  DESTRUCTOR TGameApplication.Destroy;
  BEGIN
    INHERITED Destroy
  END;



(* Initializes the game. *)
  PROCEDURE TGameApplication.Initialize;
  BEGIN
    TRY
      INHERITED Initialize;
    { Init graphics. }
      SetGraphicMode (al_desktop_color_depth, 640, 480, FALSE);
      CreateBackbuffer (al_desktop_color_depth, 320, 240);
    EXCEPT
      ON Error: Exception DO
      BEGIN
	ShowException (Error);
	SELF.Terminate
      END
    END
  END;

INITIALIZATION
{ Creates the game object. }
  Zombie3DGame := TGameApplication.Create;
FINALIZATION
{ Game object is destroyed by Application FINALIZATION, but some compilers
  needs FINALIZATION if INITIALIZATION is used, so: }
  { does nothing  };
END.
