PROGRAM exgui;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	This program demonstrates how to use the GUI routines. From
 *	the simple dialog controls that display a text or a bitmap to
 *	more complex multiple choice selection lists, Allegro provides
 *	a framework which can be customised to suit your needs.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Elias Pschernig.
 *
 *	See README file for license and copyright information.
 *)

USES
  Allegro, alBase, alFile, alGUI, alUnicode,
  sysutils;

{$include example.inc}



(* for the al_d_edit_proc object *)
CONST
  LEN = 32;
VAR
  TheString: STRING[255] = 'Change Me!'+#0;

(* for the al_d_text_box_proc object *)
  TheText: STRING =
   'I''m text inside a text box.'+#10+#10+
   'I can have multiple lines.'+#10+#10+
   'If I grow too big to fit into my box, I get a scrollbar to '+
   'the right, so you can scroll me in the vertical direction. I will never '+
   'let you scroll in the horizontal direction, but instead I will try to '+
   'word wrap the text.';

(* for the multiple selection list *)
  Sel: ARRAY [0..9] OF BYTE;

(* for the example bitmap *)
  Datafile: AL_DATAFILEptr;



(* callback function to specify the contents of the listboxes *)
FUNCTION ListboxGetter (Index: LONGINT; ListSize: PLONGINT): PCHAR; CDECL;
VAR
  Strings: ARRAY [0..10] OF PCHAR = (
    'Zero',  'One',   'Two',   'Three', 'Four',  'Five',
    'Six',   'Seven', 'Eight', 'Nine',  'Ten'
   );
BEGIN
   IF Index < 0 THEN
   BEGIN
     ListSize^ := 11;
     ListboxGetter := NIL;
   END
   ELSE
     ListboxGetter := Strings[Index]
END;



(* Used as a menu-callback, and by the quit button. *)
FUNCTION Quit: LONGINT; CDECL;
BEGIN
  IF al_alert ('Really Quit?', '', '', '&Yes', '&No', Ord ('y'), Ord ('n')) = 1
  THEN
    Quit := AL_D_CLOSE
  ELSE
    Quit := AL_D_O_K;
END;



(* A custom dialog procedure, derived from al_d_button_proc.  It intercepts
 * the AL_D_CLOSE return of al_d_button_proc, and calls the function in dp3. *)
FUNCTION MyButtonProc (msg:LONGINT; d:AL_DIALOGptr; c:LONGINT):LONGINT; CDECL;
VAR
  Ret: LONGINT;
  Fn: AL_SIMPLE_FUNC;
BEGIN
  Ret := al_d_button_proc (msg, d, c);
  IF (Ret = AL_D_CLOSE) AND (d^.dp3 <> NIL) THEN
  BEGIN
    Fn := AL_SIMPLE_FUNC (d^.dp3);
    Ret := Fn ();
  END;
  MyButtonProc := Ret;
END;



(* Our about box. *)
FUNCTION About: LONGINT; CDECL;
BEGIN
  al_alert ('* exgui *', '', 'Allegro GUI Example', 'Ok', '', 0, 0);
  About := AL_D_O_K;
END;



(* Another menu callback. *)
FUNCTION MenuCallback: LONGINT; CDECL;
BEGIN
  al_alert ('Selected menu item:', '', al_active_menu^.txt, 'Ok', '', 0, 0);
  MenuCallback := AL_D_O_K;
END;



(* Menu callback which toggles the checked status. *)
FUNCTION CheckCallback: LONGINT; CDECL;
BEGIN
  al_active_menu^.flags := al_active_menu^.flags XOR AL_D_SELECTED; 
  IF (al_active_menu^.flags AND AL_D_SELECTED) <> 0 THEN
    al_active_menu^.txt := 'Checked'
  ELSE
    al_active_menu^.txt := 'Unchecked';
  al_alert ('Menu item has been toggled!', '', '', 'Ok', '', 0, 0);
  CheckCallback := AL_D_O_K;
END;



VAR
(* the submenu *)
  Submenu: ARRAY [0..4] OF AL_MENU;
(* the first menu in the menubar *)
  Menu1: ARRAY [0..3] OF AL_MENU;
(* the second menu in the menubar *)
  Menu2: ARRAY [0..2] OF AL_MENU;
(* the help menu *)
  HelpMenu: ARRAY [0..1] OF AL_MENU;
(* the main menu-bar *)
  TheMenu: ARRAY [0..3] OF AL_MENU;



(* These three functions demonstrate how to query dialog elements. *)
  FUNCTION Info1: LONGINT; CDECL; FORWARD;
  FUNCTION Info2: LONGINT; CDECL; FORWARD;
  FUNCTION Info3: LONGINT; CDECL; FORWARD;



CONST
  LIST_OBJECT     = 26;
  TEXTLIST_OBJECT = 27;
  SLIDER_OBJECT   = 29;
  BITMAP_OBJECT   = 32;
  ICON_OBJECT     = 33;



VAR
(* here it comes - the big bad ugly DIALOG array for our main dialog *)
  TheDialog: ARRAY [0..40] OF AL_DIALOG;



(* defines the menu and the dialog. *)
  PROCEDURE CreateDialog;
  BEGIN
    al_set_menu_item (Submenu,  0, 'Submenu',                 NIL,      NIL, AL_D_DISABLED, NIL);
    al_set_menu_item (Submenu,  1, '',                        NIL,      NIL,             0, NIL);
    al_set_menu_item (Submenu,  2, 'Checked',      @CheckCallback,      NIL, AL_D_SELECTED, NIL);
    al_set_menu_item (Submenu,  3, 'Disabled',                NIL,      NIL, AL_D_DISABLED, NIL);

    al_set_menu_item (Menu1,    0, 'Test &1     1', @MenuCallback,      NIL,             0, NIL);
    al_set_menu_item (Menu1,    1, 'Test &2     2', @MenuCallback,      NIL,             0, NIL);
    al_set_menu_item (Menu1,    2, '&Quit   q/Esc',         @quit,      NIL,             0, NIL);

    al_set_menu_item (Menu2,    0, '&Test',         @MenuCallback,       NIL,            0, NIL);
    al_set_menu_item (Menu2,    1, '&Submenu',                NIL,  @Submenu,            0, NIL);

    al_set_menu_item (HelpMenu, 0, '&About   F1',          @About,       NIL,            0, NIL);

    al_set_menu_item (TheMenu,  0, '&First',                  NIL,    @Menu1,            0, NIL);
    al_set_menu_item (TheMenu,  1, '&Second',                 NIL,    @Menu2,            0, NIL);
    al_set_menu_item (TheMenu,  2, '&Help',                   NIL, @HelpMenu,            0, NIL);

    al_set_dialog_item (TheDialog,  0, @al_d_clear_proc,        0,   0,    0,    0,   0,  0,    0,      0,       0,   0,    NIL,                   NIL, NIL);

    al_set_dialog_item (TheDialog,  1, @al_d_text_proc,         0,  20,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_text_proc'),          NIL, NIL);
    al_set_dialog_item (TheDialog,  2, @al_d_ctext_proc,      318,  20,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_ctext_proc'),         NIL, NIL);
    al_set_dialog_item (TheDialog,  3, @al_d_rtext_proc,      636,  20,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_rtext_proc'),         NIL, NIL);

   (* lots of descriptive text elements *)
    al_set_dialog_item (TheDialog,  4, @al_d_text_proc,         0,   0,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_menu_proc->'),        NIL, NIL);
    al_set_dialog_item (TheDialog,  5, @al_d_text_proc,         0,  40,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_button_proc->'),      NIL, NIL);
    al_set_dialog_item (TheDialog,  6, @al_d_text_proc,         0,  70,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_check_proc->'),       NIL, NIL);
    al_set_dialog_item (TheDialog,  7, @al_d_text_proc,         0, 100,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_radio_proc->'),       NIL, NIL);
    al_set_dialog_item (TheDialog,  8, @al_d_text_proc,         0, 130,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_edit_proc->'),        NIL, NIL);
    al_set_dialog_item (TheDialog,  9, @al_d_text_proc,         0, 150,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_list_proc->'),        NIL, NIL);
    al_set_dialog_item (TheDialog, 10, @al_d_text_proc,         0, 200,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_text_list_proc->'),   NIL, NIL);
    al_set_dialog_item (TheDialog, 11, @al_d_text_proc,         0, 250,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_textbox_proc->'),     NIL, NIL);
    al_set_dialog_item (TheDialog, 12, @al_d_text_proc,         0, 300,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_slider_proc->'),      NIL, NIL);
    al_set_dialog_item (TheDialog, 13, @al_d_text_proc,         0, 330,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_box_proc->'),         NIL, NIL);
    al_set_dialog_item (TheDialog, 14, @al_d_text_proc,         0, 360,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_shadow_box_proc->'),  NIL, NIL);
    al_set_dialog_item (TheDialog, 15, @al_d_text_proc,         0, 390,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_keyboard_proc. Press F1 to see me trigger the about box.'), NIL, NIL);
    al_set_dialog_item (TheDialog, 16, @al_d_text_proc,         0, 410,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_clear_proc. I draw the white background.'), NIL, NIL);
    al_set_dialog_item (TheDialog, 17, @al_d_text_proc,         0, 430,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('al_d_yield_proc. I make us play nice with the OS scheduler.'), NIL, NIL);
    al_set_dialog_item (TheDialog, 18, @al_d_rtext_proc,      636,  40,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('<-d_bitmap_proc'),      NIL, NIL);
    al_set_dialog_item (TheDialog, 19, @al_d_rtext_proc,      636,  80,    0,    0,   0,  0,    0,      0,       0,   0,    PCHAR('<-d_icon_proc'),        NIL, NIL);

   (* a menu bar - note how it auto-calculates its dimension if they are not given *)
    al_set_dialog_item (TheDialog, 20, @al_d_menu_proc,       160,   0,    0,    0,   0,  0,    0,      0,       0,   0,    @TheMenu[0],               NIL, NIL);

   (* some more GUI elements, all of which require you to specify their dimensions *)
    al_set_dialog_item (TheDialog, 21, @al_d_button_proc,     160,  40,  160,   20,   0,  0, ORD('t'),      0,       0,   0,    PCHAR('&Toggle Me!'),     NIL, NIL);
    al_set_dialog_item (TheDialog, 22, @al_d_check_proc,      160,  70,  160,   20,   0,  0, ORD('c'),      0,       0,   0,    PCHAR('&Check Me!'),      NIL, NIL);
    al_set_dialog_item (TheDialog, 23, @al_d_radio_proc,      160, 100,  160,   19,   0,  0, ORD('s'),      0,       0,   0,    PCHAR('&Select Me!'),     NIL, NIL);
    al_set_dialog_item (TheDialog, 24, @al_d_radio_proc,      320, 100,  160,   19,   0,  0, ORD('o'),      0,       0,   0,    PCHAR('&Or Me!'),         NIL, NIL);
    al_set_dialog_item (TheDialog, 25, @al_d_edit_proc,       160, 130,  160,    8,   0,  0,        0,      0,     LEN,   0,    @TheString[1],            NIL, NIL);
    al_set_dialog_item (TheDialog, 26, @al_d_list_proc,       160, 150,  160,   44,   0,  0,        0,      0,       0,   0,    @ListboxGetter,       @sel[0],  NIL);
    al_set_dialog_item (TheDialog, 27, @al_d_text_list_proc,  160, 200,  160,   44,   0,  0,        0,      0,       0,   0,    @ListboxGetter,           NIL, NIL);
    al_set_dialog_item (TheDialog, 28, @al_d_textbox_proc,    160, 250,  160,   48,   0,  0,        0,      0,       0,   0,    PCHAR(TheText),           NIL, NIL);
    al_set_dialog_item (TheDialog, 29, @al_d_slider_proc,     160, 300,  160,   12,   0,  0,        0,      0,     100,   0,    NIL,                      NIL, NIL);
    al_set_dialog_item (TheDialog, 30, @al_d_box_proc,        160, 330,  160,   20,   0,  0,        0,      0,       0,   0,    NIL,                      NIL, NIL);
    al_set_dialog_item (TheDialog, 31, @al_d_shadow_box_proc, 160, 360,  160,   20,   0,  0,        0,      0,       0,   0,    NIL,                      NIL, NIL);

   (* note how we don't fill in the dp field yet, because we first need to load the bitmap *)
    al_set_dialog_item (TheDialog, 32, @al_d_bitmap_proc,     480,  40,   30,   30,   0,  0,        0,      0,       0,   0,    NIL,                   NIL, NIL);
    al_set_dialog_item (TheDialog, 33, @al_d_icon_proc,       480,  80,   30,   30,   0,  0,        0,      0,       0,   0,    NIL,                   NIL, NIL);

   (* the quit and info buttons use our customized dialog procedure, using dp3 as callback *)
    al_set_dialog_item (TheDialog, 34, @MyButtonProc,           0, 450,  160,   20,   0,  0, ORD('q'), AL_D_EXIT,       0,   0,    PCHAR('&Quit'),      NIL, @Quit );
    al_set_dialog_item (TheDialog, 35, @MyButtonProc,         400, 150,  160,   20,   0,  0, ORD('i'), AL_D_EXIT,       0,   0,    PCHAR('&Info'),      NIL, @Info1);
    al_set_dialog_item (TheDialog, 36, @MyButtonProc,         400, 200,  160,   20,   0,  0, ORD('n'), AL_D_EXIT,       0,   0,    PCHAR('I&nfo'),      NIL, @Info2);
    al_set_dialog_item (TheDialog, 37, @MyButtonProc,         400, 300,  160,   20,   0,  0, ORD('f'), AL_D_EXIT,       0,   0,    PCHAR('In&fo'),      NIL, @Info3);

   (* the next two elements don't draw anything *)
    al_set_dialog_item (TheDialog, 38, @al_d_keyboard_proc,     0,   0,    0,    0,   0,  0,    0,      0, AL_KEY_F1,   0,    @About,          NIL, NIL);
    al_set_dialog_item (TheDialog, 39, @al_d_yield_proc,        0,   0,    0,    0,   0,  0,    0,      0,         0,   0,       NIL,          NIL, NIL);
  END;



  FUNCTION Info1: LONGINT; CDECL;
  VAR
    buf1, buf2: STRING;
    i, n: INTEGER;
    s: BOOLEAN;
  BEGIN
    buf2 := '';
    s := FALSE;
    ListboxGetter (-1, @n);
  { query the list proc }
    FOR i := 0 TO (n - 1) DO
    BEGIN
      IF Sel[i] <> 0 THEN
      BEGIN
	buf1 := IntToStr (i);
	buf2 := buf2 + buf1;
	s := TRUE;
      END;
    END;
    IF s THEN
      buf2 := buf2 + ' are in the multiple selection!'
    ELSE
      buf2 := buf2 + 'There is no multiple selection!';
    buf1 := 'Item number '+ IntToStr (TheDialog[LIST_OBJECT].d1) +
	    ' is selected!';
    al_alert ('Info about the list:', buf1, buf2, 'Ok', '', 0, 0);
    Info1 := AL_D_O_K;
  END;



  FUNCTION Info2: LONGINT; CDECL;
  VAR
    buf: STRING;
  BEGIN
  { query the textlist proc }
    buf := 'Item number ' + IntToStr (TheDialog[TEXTLIST_OBJECT].d1) + ' is selected!';
    al_alert ('Info about the text list:', '', buf, 'Ok', '', 0, 0);
    Info2 := AL_D_O_K;
  END;



  FUNCTION Info3: LONGINT; CDECL;
  VAR
    buf: STRING;
  BEGIN
  { query the slider proc }
    buf := 'lider position is ' + IntToStr (TheDialog[SLIDER_OBJECT].d2) + '!';
    al_alert ('Info about the slider:', '', buf, 'Ok', '', 0, 0);
    Info3 := AL_D_O_K;
  END;



VAR
  i: INTEGER;
  buf: STRING;
  Palette: AL_PALETTEptr;
BEGIN
{ initialise everything }
  al_set_uformat (AL_U_ASCII);
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_mouse;
  al_install_timer;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 640, 480, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'+chr(13)+al_error);
      EXIT;
    END;

{ Load the datafile into memory. }
{ TODO: Find a better way to get the path. }
  buf := ExtractFilePath (ParamStr (0)) + 'example.dat';
  datafile := al_load_datafile (buf);
  IF datafile = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error loading example.dat');
    EXIT;
  END;

  Palette := datafile^[THE_PALETTE].dat;
  al_set_palette (Palette^);

{ create dialog and menu. }
  CreateDialog;

{ set up colors }
  al_gui_fg_color := al_makecol(0, 0, 0);
  al_gui_mg_color := al_makecol(128, 128, 128);
  al_gui_bg_color := al_makecol(200, 240, 200);
  al_set_dialog_color (TheDialog, al_gui_fg_color, al_gui_bg_color);

{ white color for al_d_clear_proc and the al_d_?text_procs }
  TheDialog[0].bg := al_makecol(255, 255, 255);
  FOR i := 4 TO 39 DO
    IF (TheDialog[i].proc = @al_d_text_proc)
    OR (TheDialog[i].proc = @al_d_ctext_proc)
    OR (TheDialog[i].proc = @al_d_rtext_proc)
    THEN
      TheDialog[i].bg := TheDialog[0].bg;

{ fill in bitmap pointers }
  TheDialog[BITMAP_OBJECT].dp := datafile^[SILLY_BITMAP].dat;
  TheDialog[ICON_OBJECT].dp := datafile^[SILLY_BITMAP].dat;

{ shift the dialog 2 pixels away from the border }
  al_position_dialog (TheDialog, 2, 2);

{ do the dialog }
  al_do_dialog (TheDialog, -1);

  al_unload_datafile (datafile);
END.
