UNIT main;
(* Main dialog and menu. *)

INTERFACE

USES
  algui;



VAR
(* The dialog. *)
  Dialog: ARRAY [0..4] OF AL_DIALOG;



(* Creates the dialog and the menues. *)
  FUNCTION Create: BOOLEAN;



IMPLEMENTATION

USES
  allegro;



VAR
{ The menu. }
  MainMenu: ARRAY [0..3] OF AL_MENU;
  FileMenu: ARRAY [0..5] OF AL_MENU;
  HelpMenu: ARRAY [0..1] OF AL_MENU;
{ Pop-up. }
  ObjectMenu: ARRAY [0..9] OF AL_MENU;
  NewObjMenu: ARRAY [0..1] OF AL_MENU;



(* Used as menu-callback. *)
  FUNCTION Quit: LONGINT; CDECL;
  BEGIN
    IF al_alert ('Really Quit?', '', '', '&Yes', '&No', Ord ('y'), Ord ('n'))
       = 1
    THEN
      Quit := AL_D_CLOSE
    ELSE
      Quit := AL_D_O_K;
  END;



(* Creates the dialog and the menues. *)
  FUNCTION Create: BOOLEAN;
  BEGIN
  { Creates the menues. }
    al_set_menu_item (FileMenu, 0,  '&New',   NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (FileMenu, 1, '&Load',   NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (FileMenu, 2, '&Save',   NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (FileMenu, 3,      '',   NIL, NIL,             0, NIL);
    al_set_menu_item (FileMenu, 4, '&Quit', @Quit, NIL,             0, NIL);

    al_set_menu_item (NewObjMenu, 0, '&Bitmap', NIL, NIL, AL_D_DISABLED, NIL);

    al_set_menu_item (ObjectMenu, 0, '&Grab', NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (ObjectMenu, 1, '&Export', NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (ObjectMenu, 2, '&Delete', NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (ObjectMenu, 3, '', NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (ObjectMenu, 4, 'Move &up', NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (ObjectMenu, 5, 'Move &down', NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (ObjectMenu, 6, '', NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (ObjectMenu, 7, '&Rename', NIL, NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (ObjectMenu, 8, '&Add', NIL, @NewObjMenu, 0, NIL);

    al_set_menu_item (HelpMenu, 0, '&About...', NIL, NIL, AL_D_DISABLED, NIL);

    al_set_menu_item (MainMenu, 0,   '&File', NIL,   @FileMenu, 0, NIL);
    al_set_menu_item (MainMenu, 1, '&Object', NIL, @ObjectMenu, 0, NIL);
    al_set_menu_item (MainMenu, 2,   '&Help', NIL,   @HelpMenu, 0, NIL);
  { Creates the dialog. }
    al_set_dialog_item (Dialog, 0, @al_d_clear_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL , NIL, NIL);
    al_set_dialog_item (Dialog, 1, @al_d_menu_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, @MainMenu[0] , NIL, NIL);
    al_set_dialog_item (Dialog, 2, @al_d_yield_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL , NIL, NIL);
  { Configure dialog. }
    al_set_dialog_color (Dialog, al_gui_fg_color, al_gui_mg_color);
    Create := TRUE;
  END;

END.
