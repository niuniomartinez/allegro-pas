UNIT Configuration;
(*<Stores and manages the configuration.  This includes command line parsing. *)
(*
  Copyright (c) 2018 Handoko
            (c) 2019-2020 Guillermo Martínez.

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
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

INTERFACE

  CONST
  (* Requested screen width. *)
    ScreenWidth     = 500;
  (* Requested screen height. *)
    ScreenHeight    = 300;


  VAR
  (* Tells if running in debug mode. *)
    DebugMode:         BOOLEAN = FALSE;
  (* A cheating:  Player recover health faster. *)
    CheatFastRecover:  BOOLEAN = FALSE;
  (* A cheating:  Player can't be harmed. *)
    CheatStoneSkin:    BOOLEAN = FALSE;

(* Loads configuration and parses command line.
   @returns(@true on success, @false otherwise @(i.e. not able to run@).)*)
  FUNCTION Load: BOOLEAN;

IMPLEMENTATION

  USES
    al5nativedlg, al5strings,
    sysutils;

  FUNCTION Load;
  VAR
    Ndx: INTEGER;
    Command: STRING;
  BEGIN
  { Parses command line. }
    FOR Ndx := 1 TO ParamCount DO
    BEGIN
      Command  := LowerCase (ParamStr (Ndx));
      IF Command = '-debug' THEN DebugMode := TRUE
      ELSE IF Command = '-fastrecover' THEN CheatFastRecover := TRUE
      ELSE IF Command = '-stoneskin' THEN CheatStoneSkin := True
      ELSE BEGIN
      { Unknown command line found. }
        al_show_native_message_box (
          NIL,
          'Error', 'Unknown command:', al_string_to_str (ParamStr (Ndx)),
          '', ALLEGRO_MESSAGEBOX_ERROR
        );
        EXIT (FALSE)
      END
    END;
    RESULT := TRUE
  END;

END.

