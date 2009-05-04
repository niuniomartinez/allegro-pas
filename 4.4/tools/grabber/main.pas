UNIT main;
(* Main dialog and menu. *)

INTERFACE

USES
  algui;



VAR
(* The dialog. *)
  Dialog: ARRAY [0..2] OF AL_DIALOG;



(* Creates the dialog. *)
  FUNCTION Create: BOOLEAN;



IMPLEMENTATION

USES
  allegro;



VAR
  MainMenu: ARRAY [0..1] OF AL_MENU;



  FUNCTION Create: BOOLEAN;
  VAR
    Black, White: LONGINT;
  BEGIN
    Black := al_makecol (0, 0, 0);
    White := al_makecol (255, 255, 255);
  { Creates the menu. }
    al_set_menu_item (MainMenu, 0, '&Exit', NIL, NIL, AL_D_EXIT, NIL);
  { Creates the dialog. }
    al_set_dialog_item (Dialog, 0, @al_d_menu_proc, 10, 10, 100, 100, 0, 0, 0, AL_D_EXIT, 0, 0, @MainMenu[0] , NIL, NIL);
    al_set_dialog_item (Dialog, 1, @al_d_button_proc, 10, 200, 100, 100, Black, White, 0, AL_D_EXIT, 0, 0, PCHAR ('Ok'), NIL, NIL);
    Create := TRUE;
  END;
END.
