unit Input;
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

interface

  uses
    Allegro5;

  const
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

  type
  (* Base class for user input. *)
    TUserInput = class (TObject)
    protected
      fDirection, fButton: Integer;
    public
    (* Initializes the input.  Also used to reset (if necessary). *)
      procedure Initialize; virtual; abstract;
    (* Process the given event.  Returns @true if it consumed the event, @false
       otherwise. *)
      function ProcessEvent (const aEvent: ALLEGRO_EVENT): Boolean;
        virtual; abstract;

    (* A value that represents the direction selected by the user.
       This direction is an @code(OR) combination of the @code(DIR_* )
       constants. *)
      property Direction: Integer read fDirection;
    (* The button pressed. *)
      property Button: Integer read fButton;
    end;




  (* Keyboard input. *)
    TKeyboardInput = class (TUserInput)
    private

    public
    (* Initializes the input.  Also used to reset (if necessary). *)
      procedure Initialize; override;
    (* Process the given event.  Returns @true if it consumed the event, @false
       otherwise. *)
      function ProcessEvent (const aEvent: ALLEGRO_EVENT): Boolean; override;
    end;

implementation

(*
 * TKeyboardInput
 ***************************************************************************)

(* Initializes. *)
  procedure TKeyboardInput.Initialize;
  begin
    fDirection := DIR_NONE; fButton := BTN_NONE
  end;



(* Process the given event. *)
  function TKeyboardInput.ProcessEvent (const aEvent: ALLEGRO_EVENT): Boolean;
  begin
    case aEvent.ftype of
    ALLEGRO_EVENT_KEY_DOWN:
      case aEvent.keyboard.keycode of
      ALLEGRO_KEY_LEFT:
	begin
	  fDirection := fDirection or DIR_LEFT;
	  Exit (True)
	end;
      ALLEGRO_KEY_RIGHT:
	begin
	  fDirection := fDirection or DIR_RIGHT;
	  Exit (True)
	end;
      ALLEGRO_KEY_UP:
	begin
	  fDirection := fDirection or DIR_UP;
	  Exit (True)
	end;
      ALLEGRO_KEY_DOWN:
	begin
	  fDirection := fDirection or DIR_DOWN;
	  Exit (True)
	end;
      ALLEGRO_KEY_ALT:
	begin
	  fButton := fButton or BTN_SHOOT;
	  Exit (True)
	end;
      ALLEGRO_KEY_SPACE:
	begin
	  fButton := fButton or BTN_SHIELD;
	  Exit (True)
	end;
      end;
    ALLEGRO_EVENT_KEY_UP:
      case aEvent.keyboard.keycode of
      ALLEGRO_KEY_LEFT:
	begin
	  fDirection := fDirection and (not DIR_LEFT);
	  Exit (True)
	end;
      ALLEGRO_KEY_RIGHT:
	begin
	  fDirection := fDirection and (not DIR_RIGHT);
	  Exit (True)
	end;
      ALLEGRO_KEY_UP:
	begin
	  fDirection := fDirection and (not DIR_UP);
	  Exit (True)
	end;
      ALLEGRO_KEY_DOWN:
	begin
	  fDirection := fDirection and (not DIR_DOWN);
	  Exit (True)
	end;
      ALLEGRO_KEY_ALT:
	begin
	  fButton := fButton and (not BTN_SHOOT);
	  Exit (True)
	end;
      ALLEGRO_KEY_SPACE:
	begin
	  fButton := fButton and (not BTN_SHIELD);
	  Exit (True)
	end;
      end;
    end;
    Result := False
  end;

end.

