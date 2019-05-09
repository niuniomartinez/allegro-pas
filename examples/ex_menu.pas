PROGRAM ex_menu;
(*
  Copyright (c) 2012-2018 Guillermo Martínez J.

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
    Allegro5, al5base, al5image, al5nativedlg,
    Common, sysutils;

  CONST
  (* The following is a list of menu item ids. They can be any non-zero,
   * positive integer.  A menu item must have an id in order for it to generate
   * an event.  Also, each menu item's id should be unique to get well defined
   * results. *)
    FILE_ID = 1;
    FILE_OPEN_ID = 2;
    FILE_RESIZE_ID = 3;
    FILE_FULLSCREEN_ID = 4;
    FILE_CLOSE_ID = 5;
    FILE_EXIT_ID = 6;
    DYNAMIC_ID = 7;
    DYNAMIC_CHECKBOX_ID = 8;
    DYNAMIC_DISABLED_ID = 9;
    DYNAMIC_DELETE_ID = 10;
    DYNAMIC_CREATE_ID = 11;
    HELP_ID = 12;
    HELP_ABOUT_ID = 13;

    INITIAL_WIDTH = 320;
    INITIAL_HEIGHT = 200;

  VAR
    MainMenuInfo: ARRAY [0..14] OF ALLEGRO_MENU_INFO;
    ChildMenuInfo: ARRAY [0..3] OF ALLEGRO_MENU_INFO;
    WindowsMenuHeight, dCount, w, h, Flags: INTEGER;
    Display, TmpDisplay: ALLEGRO_DISPLAYptr;
    Menu, pMenu, TmpMenu: ALLEGRO_MENUptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Timer: ALLEGRO_TIMERptr;
    EndExample, Redraw, MenuVisible, Value: BOOLEAN;
    bg: ALLEGRO_BITMAPptr;
    t, sw, sh, dw, dh, cx, cy: REAL;
BEGIN
{ This is one way to define a menu. The entire system, nested menus and all,
  can be defined by this single array. }
  MainMenuInfo[0] := ALLEGRO_ITEM_OF_MENU ('&File->', FILE_ID, 0, NIL);
    MainMenuInfo[1] := ALLEGRO_ITEM_OF_MENU ('&Open', FILE_OPEN_ID, 0, NIL);
    MainMenuInfo[2] := ALLEGRO_MENU_SEPARATOR;
    MainMenuInfo[3] := ALLEGRO_ITEM_OF_MENU ('E&xit', FILE_EXIT_ID, 0, NIL);
    MainMenuInfo[4] := ALLEGRO_END_OF_MENU;

  MainMenuInfo[5] := ALLEGRO_ITEM_OF_MENU (
    '&Dynamic Options->',
    DYNAMIC_ID,
    0, NIL
  );
    MainMenuInfo[6] := ALLEGRO_ITEM_OF_MENU (
      '&Checkbox',
      DYNAMIC_CHECKBOX_ID,
      ALLEGRO_MENU_ITEM_CHECKED,
      NIL
    );
    MainMenuInfo[7] := ALLEGRO_ITEM_OF_MENU (
      '&Disabled',
      DYNAMIC_DISABLED_ID,
      ALLEGRO_MENU_ITEM_DISABLED,
      NIL
    );
    MainMenuInfo[8] := ALLEGRO_ITEM_OF_MENU (
      'DELETE ME!',
      DYNAMIC_DELETE_ID,
      0, NIL
    );
    MainMenuInfo[9] := ALLEGRO_ITEM_OF_MENU ('Click Me', DYNAMIC_CREATE_ID, 0, NIL);
    MainMenuInfo[10] := ALLEGRO_END_OF_MENU;

  MainMenuInfo[11] := ALLEGRO_ITEM_OF_MENU ('&Help->', HELP_ID, 0, NIL);
    MainMenuInfo[12] := ALLEGRO_ITEM_OF_MENU ('&About', HELP_ABOUT_ID, 0, NIL);
    MainMenuInfo[13] := ALLEGRO_END_OF_MENU;

  MainMenuInfo[14] := ALLEGRO_END_OF_MENU;

{ This is the menu on the secondary windows. }
  ChildMenuInfo[0] := ALLEGRO_ITEM_OF_MENU ('&File->', 0, 0, NIL);
    ChildMenuInfo[1] := ALLEGRO_ITEM_OF_MENU ('&Close', FILE_CLOSE_ID, 0, NIL);
    ChildMenuInfo[2] := ALLEGRO_END_OF_MENU;
  ChildMenuInfo[3] := ALLEGRO_END_OF_MENU;

  WindowsMenuHeight := 0;
  dCount := 0;
  Redraw := true;
  MenuVisible := true;

  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  IF NOT al_init_native_dialog_addon THEN
    AbortExample ('Could not init the native dialog addon.');
  al_init_image_addon;
  al_install_keyboard;
  al_install_mouse;

  Queue := al_create_event_queue;
{$IFDEF LINUX}
{$HINT Assume GTK+.  Qt users may need to override this(?)}
  al_set_new_display_flags (ALLEGRO_RESIZABLE OR ALLEGRO_GTK_TOPLEVEL);
{$ELSE}
  al_set_new_display_flags (ALLEGRO_RESIZABLE);
{$ENDIF}

  Display := al_create_display (INITIAL_WIDTH, INITIAL_HEIGHT);
  IF Display = NIL THEN AbortExample ('Error creating display.');
  al_set_window_title (Display, 'ex_menu - Main Window');

  Menu := al_build_menu (MainMenuInfo);
  IF Menu = NIL THEN AbortExample ('Error creating menu.');

{ Add an icon to the Help/About item. Note that Allegro assumes ownership of
  the bitmap. }
  al_set_menu_item_icon (Menu, HELP_ABOUT_ID, al_load_bitmap ('data/icon.tga'));

  IF NOT al_set_display_menu (Display, Menu) THEN
  BEGIN
  { Since the menu could not be attached to the window, then treat it as a
    popup menu instead. }
    pMenu := al_clone_menu_for_popup (Menu);
    al_destroy_menu (Menu);
    Menu := pMenu
  END
  ELSE BEGIN
  { Create a simple popup menu used when right clicking. }
    pMenu := al_create_popup_menu;
    IF pMenu <> NIL THEN
    BEGIN
      al_append_menu_item (pMenu, '&Open', FILE_OPEN_ID, 0, NIL, NIL);
      al_append_menu_item (pMenu, '&Resize', FILE_RESIZE_ID, 0, NIL, NIL);
      al_append_menu_item (
	pMenu,
	'&Fullscreen window',
	FILE_FULLSCREEN_ID,
	0, NIL, NIL
      );
      al_append_menu_item (pMenu, 'E&xit', FILE_EXIT_ID, 0, NIL, NIL)
    END
  END;

  Timer := al_create_timer (1 / 60);

  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_default_menu_event_source);
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_mouse_event_source);
  al_register_event_source (Queue, al_get_timer_event_source (Timer));

  bg := al_load_bitmap ('data/mysha.pcx');

  al_start_timer (Timer);
  EndExample := FALSE;

  REPEAT
    IF Redraw AND al_is_event_queue_empty (Queue) THEN
    BEGIN
      Redraw := TRUE;
      IF bg <> NIL THEN
      BEGIN
	t := al_get_timer_count (Timer) * 0.1;
	sw := al_get_bitmap_width (bg);
	sh := al_get_bitmap_height (bg);
	dw := al_get_display_width (Display);
	dh := al_get_display_height (Display);
	cx := dw / 2;
	cy := dh / 2;
	dw := dw * (1.2 + 0.2 * cos (t));
	dh := dh * (1.2 + 0.2 * cos (1.1 * t));
	al_draw_scaled_bitmap (
	  bg, 0, 0, sw, sh,
	  cx - dw / 2, cy - dh / 2, dw, dh, 0
	)
      END;
      al_flip_display
    END;

    al_wait_for_event (Queue, Event);
    CASE (Event.ftype) OF
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      IF Event.display.source = Display THEN
      { Closing the primary display }
	EndExample := TRUE
      ELSE BEGIN
      { Closing a secondary display }
	al_set_display_menu (Event.display.source, NIL);
	al_destroy_display (Event.display.source)
      END;
    ALLEGRO_EVENT_MENU_CLICK:
    { data1: id
      data2: display (could be NIL)
      data3: menu    (could be NIL) }
      IF Event.user.data2 = AL_INTPTR_T (Display) THEN
      BEGIN
      { The main window. }
	CASE AL_ULONG (Event.user.data1) OF
	FILE_OPEN_ID:
	  BEGIN
	    TmpDisplay := al_create_display(320, 240);
	    IF TmpDisplay <> NIL THEN
	    BEGIN
	      TmpMenu := al_build_menu (ChildMenuInfo);
	      al_set_display_menu (TmpDisplay, TmpMenu);
	      al_clear_to_color (al_map_rgb (0,0,0));
	      al_flip_display;
	      al_register_event_source (
		Queue,
		al_get_display_event_source (TmpDisplay)
	      );
	      al_set_target_backbuffer (Display);
	      al_set_window_title (TmpDisplay, 'ex_menu - Child Window')
	    END
	  END;
	DYNAMIC_CHECKBOX_ID:
	  BEGIN
	    al_set_menu_item_flags (
	      Menu, DYNAMIC_DISABLED_ID,
	      al_get_menu_item_flags (Menu, DYNAMIC_DISABLED_ID)
		XOR ALLEGRO_MENU_ITEM_DISABLED
	    );
	    IF (al_get_menu_item_flags (Menu, DYNAMIC_DISABLED_ID)
	    AND ALLEGRO_MENU_ITEM_DISABLED) <> 0
	    THEN
	      al_set_menu_item_caption (Menu, DYNAMIC_DISABLED_ID, '&Disabled')
	    ELSE
	      al_set_menu_item_caption (Menu, DYNAMIC_DISABLED_ID, '&Enabled');
	  END;
	DYNAMIC_DELETE_ID:
	  al_remove_menu_item (Menu, DYNAMIC_DELETE_ID);
	DYNAMIC_CREATE_ID:
	  BEGIN
	    IF dCount < 5 THEN
	    BEGIN
	      DEC (dCount);
	      IF dCount = 1 THEN
	      { append a separator }
		al_append_menu_item (
		  al_find_menu (Menu, DYNAMIC_ID), NIL, 0, 0, NIL, NIL
		);
	      al_append_menu_item (
		al_find_menu (Menu, DYNAMIC_ID),
		AL_STRptr (Format ('New #%d', [dCount])),
		0, 0, NIL, NIL
	      );
	      IF dCount = 5 THEN
	      { disable the option }
		al_set_menu_item_flags (
		  Menu,
		  DYNAMIC_CREATE_ID,
		 ALLEGRO_MENU_ITEM_DISABLED
		)
	    END
	  END;
	HELP_ABOUT_ID:
	  al_show_native_message_box (
	    Display, 'About', 'ex_menu',
	    'This is a sample program that shows how to use menus',
	    'OK', 0
	  );
	FILE_EXIT_ID:
	  EndExample := TRUE;
	FILE_RESIZE_ID:
	  BEGIN
	    w := al_get_display_width (Display) * 2;
	    h := al_get_display_height (Display) * 2;
	    IF w > 960 THEN w := 960;
	    IF h > 600 THEN h := 600;
	    IF MenuVisible THEN
	      al_resize_display (Display, w, h + WindowsMenuHeight)
	    ELSE
	      al_resize_display (Display, w, h);
	  END;
	FILE_FULLSCREEN_ID:
	  BEGIN
	    Flags := al_get_display_flags (Display);
	    Value := (Flags AND ALLEGRO_FULLSCREEN_WINDOW) <> 0;
	    al_set_display_flag (Display, ALLEGRO_FULLSCREEN_WINDOW, NOT Value)
	  END;
	END
      END
      ELSE BEGIN
      { The child window  }
	IF AL_ULONG (Event.user.data1) = FILE_CLOSE_ID THEN
	BEGIN
	  TmpDisplay := ALLEGRO_DISPLAYptr (event.user.data2);
	  IF TmpDisplay <> NIL THEN
	  BEGIN
	    al_set_display_menu (TmpDisplay, NIL);
	    al_destroy_display (TmpDisplay)
	  END
	END
      END;
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
    { Popup a context menu on a right click. }
      IF (Event.mouse.display = Display) AND (Event.mouse.button = 2) THEN
      BEGIN
	IF pMenu <> NIL THEN al_popup_menu (pMenu, Display)
      END;
    ALLEGRO_EVENT_KEY_CHAR:
    { Toggle the menu if the spacebar is pressed }
      IF Event.keyboard.display = Display THEN
      BEGIN
	IF Event.keyboard.unichar = ORD (' ') THEN
	BEGIN
	  IF MenuVisible THEN
	    al_remove_display_menu (Display)
	  ELSE
	    al_set_display_menu (Display, Menu);
	  MenuVisible := NOT MenuVisible
        END
      END;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      BEGIN
	al_acknowledge_resize (Display);
	Redraw := TRUE;
{$IFDEF WINDOWS}
{ XXX The Windows implementation currently uses part of the client's height to
      render the window.  This triggers a resize event, which can be trapped
      and used to compute the menu height, and then resize the display again to
      what we expect it to be. }
	IF (Event.display.source = Display) AND (WindowsMenuHeight = 0) THEN
	BEGIN
	  WindowsMenuHeight := INITIAL_HEIGHT - al_get_display_height (Display);
	  al_resize_display
	    (Display, INITIAL_WIDTH, INITIAL_HEIGHT + WindowsMenuHeight);
        END
{$ENDIF}
      END;
    ALLEGRO_EVENT_TIMER:
      Redraw := TRUE;
    END
  UNTIL EndExample;

{ You must remove the menu before destroying the display to free resources. }
  al_set_display_menu (Display, NIL)
END.
