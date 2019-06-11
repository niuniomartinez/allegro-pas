UNIT Input;
(*< Defines classes to manage user input. *)
(*
  Copyright (c) 2019 Guillermo Martínez J.

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
    Allegro5;

  CONST
  (* Stick in the center. *)
    DIR_NONE = 0;
  (* Stick up. *)
    DIR_UP = 1;
  (* Stick down. *)
    DIR_DOWN = 2;
  (* Stick left. *)
    DIR_LEFT = 4;
  (* Stick right. *)
    DIR_RIGHT = 8;
  (* No button pressed. *)
    BTN_NONE = 0;
  (* Shooting button. *)
    BTN_SHOOT = 1;
  (* Shield button. *)
    BTN_SHIELD = 2;

  TYPE
  (* Base class for user input. *)
    TUserInput = CLASS (TObject)
    PROTECTED
      fDirection, fButton: INTEGER;
    PUBLIC
    (* Initializes the input.  Also used to reset (if necessary). *)
      PROCEDURE Initialize; VIRTUAL; ABSTRACT;
    (* Process the given event.  Returns @true if it consumed the event, @false
       otherwise. *)
      FUNCTION ProcessEvent (CONST aEvent: ALLEGRO_EVENT): BOOLEAN;
        VIRTUAL; ABSTRACT;

    (* A value that represents the direction selected by the user.
       This direction is an @code(OR) combination of the @code(DIR_* )
       constants. *)
      PROPERTY Direction: INTEGER READ fDirection;
    (* The button pressed. *)
      PROPERTY Button: INTEGER READ fButton;
    END;




  (* Keyboard input. *)
    TKeyboardInput = CLASS (TUserInput)
    PRIVATE

    PUBLIC
    (* Initializes the input.  Also used to reset (if necessary). *)
      PROCEDURE Initialize; OVERRIDE;
    (* Process the given event.  Returns @true if it consumed the event, @false
       otherwise. *)
      FUNCTION ProcessEvent (CONST aEvent: ALLEGRO_EVENT): BOOLEAN; OVERRIDE;
    END;

IMPLEMENTATION

(*
 * TKeyboardInput
 ***************************************************************************)

(* Initializes. *)
  PROCEDURE TKeyboardInput.Initialize;
  BEGIN
    fDirection := DIR_NONE; fButton := BTN_NONE
  END;



(* Process the given event. *)
  FUNCTION TKeyboardInput.ProcessEvent (CONST aEvent: ALLEGRO_EVENT): BOOLEAN;
  BEGIN
    CASE aEvent.ftype OF
    ALLEGRO_EVENT_KEY_DOWN:
      CASE aEvent.keyboard.keycode OF
      ALLEGRO_KEY_LEFT:
	BEGIN
	  fDirection := fDirection OR DIR_LEFT;
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_RIGHT:
	BEGIN
	  fDirection := fDirection OR DIR_RIGHT;
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_UP:
	BEGIN
	  fDirection := fDirection OR DIR_UP;
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_DOWN:
	BEGIN
	  fDirection := fDirection OR DIR_DOWN;
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_ALT:
	BEGIN
	  fButton := fButton OR BTN_SHOOT;
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_SPACE:
	BEGIN
	  fButton := fButton OR BTN_SHIELD;
	  EXIT (TRUE)
	END;
      END;
    ALLEGRO_EVENT_KEY_UP:
      CASE aEvent.keyboard.keycode OF
      ALLEGRO_KEY_LEFT:
	BEGIN
	  fDirection := fDirection AND (NOT DIR_LEFT);
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_RIGHT:
	BEGIN
	  fDirection := fDirection AND (NOT DIR_RIGHT);
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_UP:
	BEGIN
	  fDirection := fDirection AND (NOT DIR_UP);
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_DOWN:
	BEGIN
	  fDirection := fDirection AND (NOT DIR_DOWN);
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_ALT:
	BEGIN
	  fButton := fButton AND (NOT BTN_SHOOT);
	  EXIT (TRUE)
	END;
      ALLEGRO_KEY_SPACE:
	BEGIN
	  fButton := fButton AND (NOT BTN_SHIELD);
	  EXIT (TRUE)
	END;
      END;
    END;
    RESULT := FALSE
  END;

END.

