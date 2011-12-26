UNIT Common;
(* Common stuff for examples. *)

INTERfACE

(* Exits program with error. *)
  PROCEDURE AbortExample (CONST Message: STRING);

IMPLEMENTATION

{ TODO: Native dialog support. }

(* Exits program with error. *)
  PROCEDURE AbortExample (CONST Message: STRING);
  BEGIN
    WriteLn (Message);
    HALT (1);
  END;

END.
