UNIT MapApp;
(*<Extends class @link(TCustomGameApplication) completing with the actual map
  editor code. *)
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
    Application, Scene;

  TYPE
  (* The MapMaker application class. *)
    TMapMakerApplication = CLASS (TCustomGameApplication)
    PRIVATE
      fScene: TScene3D;
    PROTECTED
    (* Executes the application. *)
      PROCEDURE DoRun; OVERRIDE;
    PUBLIC
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes the application. *)
      PROCEDURE Initialize; OVERRIDE;

    (* Public access to the current scene. *)
      PROPERTY Scene: TScene3D READ fScene;
    END;



  VAR
  (* Global reference of the application object. *)
    MapMakerApplication: TMapMakerApplication;

IMPLEMENTATION

  USES
    Allegro, sysutils;

(*
 * TMapMakerApplication
 ****************************************************************************)

(* The application code. *)
  PROCEDURE TMapMakerApplication.DoRun;
  BEGIN
    REPEAT
      WHILE (Ticks > 0)AND NOT SELF.Terminated DO
	IF al_keypressed THEN SELF.Terminate
    UNTIL SELF.Terminated
  END;



(* Destructor. *)
  DESTRUCTOR TMapMakerApplication.Destroy;
  BEGIN
    FreeAndNil (fScene);
    INHERITED Destroy
  END;



(* Initializes the application. *)
  PROCEDURE TMapMakerApplication.Initialize;
  CONST
    D_W: ARRAY [0..3] OF INTEGER = (
      320, 640, 800, 1024
    );
    D_H: ARRAY [0..3] OF INTEGER = (
      250, 480, 600, 768
    );
  VAR
    dW, dH, wW, wH, Ndx: INTEGER;
  BEGIN
    TRY
      INHERITED Initialize;
    { Init graphics. }
      wW := 0;
      IF al_get_desktop_resolution (dW, dH) THEN
      BEGIN
	FOR Ndx := LOW (D_W) TO HIGH(D_W) DO
	  IF (dW > D_W[Ndx]) AND (dH > D_H[Ndx]) THEN
	  BEGIN
	    wW := D_W[Ndx]; wH := D_H[Ndx]
	  END
      END
      ELSE BEGIN
	wW := 640; wH := 480
      END;
      SetGraphicMode (al_desktop_color_depth, wW, wH, FALSE);
      CreateBackbuffer (al_desktop_color_depth, 320, 240);
    { Creates dummy scene. }
      fScene := TScene3D.Create
    EXCEPT
      ON Error: Exception DO
      BEGIN
	ShowException (Error);
	SELF.Terminate
      END
    END
  END;

INITIALIZATION
{ Creates the application object. }
  MapMakerApplication := TMapMakerApplication.Create;
FINALIZATION
{ The application object is destroyed by Application FINALIZATION, but some
  compilers needs FINALIZATION if INITIALIZATION is used, so: }
  { does nothing  };
END.
