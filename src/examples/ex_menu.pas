PROGRAM ex_menu;
(*
  Copyright (c) 2012-2020 Guillermo Martínez J.

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
    allegro5, al5base, al5image, al5nativedlg, al5strings,
    Common;

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
    MAX_WIDTH = 960;
    MAX_HEIGHT = 600;



  VAR
    WindowsMenuHeight, dCount: INTEGER;
    Display: ALLEGRO_DISPLAYptr;
    MainMenu, PopupMenu: ALLEGRO_MENUptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Timer: ALLEGRO_TIMERptr;
    EndExample, Redraw, MenuVisible: BOOLEAN;
    bg: ALLEGRO_BITMAPptr;

(* Initializes the example. *)
  PROCEDURE ProgramInitialization;

    FUNCTION CreateMainMenu: ALLEGRO_MENUptr;
    VAR
      MainMenuInfo: ARRAY [0..14] OF ALLEGRO_MENU_INFO;
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
          'D&ELETE ME!',
          DYNAMIC_DELETE_ID,
          0, NIL
        );
        MainMenuInfo[9] := ALLEGRO_ITEM_OF_MENU (
          'C&lick Me',
          DYNAMIC_CREATE_ID,
          0, NIL
        );
        MainMenuInfo[10] := ALLEGRO_END_OF_MENU;

      MainMenuInfo[11] := ALLEGRO_ITEM_OF_MENU ('&Help->', HELP_ID, 0, NIL);
        MainMenuInfo[12] := ALLEGRO_ITEM_OF_MENU ('&About',HELP_ABOUT_ID,0,NIL);
        MainMenuInfo[13] := ALLEGRO_END_OF_MENU;

      MainMenuInfo[14] := ALLEGRO_END_OF_MENU;
    { Create menu. }
      RESULT := al_build_menu (MainMenuInfo);
    { Add an icon to the Help/About item. Note that Allegro assumes ownership of
      the bitmap. }
      al_set_menu_item_icon (
        RESULT,
        HELP_ABOUT_ID,
        al_load_bitmap ('data/icon.tga')
      )
    END;

    FUNCTION CreatePopupMenu: ALLEGRO_MENUptr;
    BEGIN
      RESULT := al_create_popup_menu;
      IF RESULT <> NIL THEN
      BEGIN
      { Another way to define the menu items. }
        al_append_menu_item (RESULT, '&Open', FILE_OPEN_ID, 0, NIL, NIL);
        al_append_menu_item (RESULT, '&Resize', FILE_RESIZE_ID, 0, NIL, NIL);
        al_append_menu_item (
          RESULT,
          '&Fullscreen window',
          FILE_FULLSCREEN_ID,
          0, NIL, NIL
        );
        al_append_menu_item (RESULT, 'E&xit', FILE_EXIT_ID, 0, NIL, NIL)
      END
    END;

  BEGIN
    IF NOT al_init THEN AbortExample ('Could not init Allegro.');
    IF NOT al_init_native_dialog_addon THEN
      AbortExample ('Could not init the native dialog addon.');
    al_init_image_addon;
    al_install_keyboard;
    al_install_mouse;

    Queue := al_create_event_queue;
{$IFDEF LINUX}
{$HINT Assumming GTK+.  Qt users may need to override this(?)}
    al_set_new_display_flags (ALLEGRO_RESIZABLE OR ALLEGRO_GTK_TOPLEVEL);
{$ELSE}
    al_set_new_display_flags (ALLEGRO_RESIZABLE);
{$ENDIF}

    Display := al_create_display (INITIAL_WIDTH, INITIAL_HEIGHT);
    IF Display = NIL THEN AbortExample ('Error creating display.');
    al_set_window_title (Display, 'ex_menu - Main Window');

    MainMenu := CreateMainMenu;
    IF MainMenu = NIL THEN AbortExample ('Error creating menu.');

    IF NOT al_set_display_menu (Display, MainMenu) THEN
    BEGIN
    { Since the menu could not be attached to the window, then treat it as a
      popup menu instead. }
      PopupMenu := al_clone_menu_for_popup (MainMenu);
      al_destroy_menu (MainMenu);
      MainMenu := PopupMenu
    END
    ELSE
    { A simple popup menu used when right clicking. }
      PopupMenu := CreatePopupMenu;

    Timer := al_create_timer (ALLEGRO_BPS_TO_SECS (60));

    al_register_event_source (Queue, al_get_display_event_source (Display));
    al_register_event_source (Queue, al_get_default_menu_event_source);
    al_register_event_source (Queue, al_get_keyboard_event_source);
    al_register_event_source (Queue, al_get_mouse_event_source);
    al_register_event_source (Queue, al_get_timer_event_source (Timer));

    bg := al_load_bitmap ('data/mysha.pcx');
    MenuVisible := true;
    dCount := 0
  END;



(* Closes a window. *)
  PROCEDURE CloseWindow (aWindow: ALLEGRO_DISPLAYptr);
  BEGIN
    IF aWindow <> NIL THEN
    BEGIN
    { You must remove the menu before destroying the display to free resources. }
      al_set_display_menu (aWindow, NIL);
      al_destroy_display (aWindow)
    END
  END;



(* Releases resources. *)
  PROCEDURE EndProgram;
  BEGIN
    CloseWindow (Display);
    al_destroy_event_queue (Queue);
    al_destroy_bitmap (bg)
  END;



(* Draws background. *)
  PROCEDURE DrawBackground (Picture: ALLEGRO_BITMAPptr);
  VAR
    t, sw, sh, dw, dh, cx, cy: REAL;
  BEGIN
    t := al_get_timer_count (Timer) * 0.1;
    sw := al_get_bitmap_width (Picture);
    sh := al_get_bitmap_height (Picture);
    dw := al_get_display_width (Display);
    dh := al_get_display_height (Display);

    cx := dw / 2;
    cy := dh / 2;
    dw := dw * (1.2 + 0.2 * cos (t));
    dh := dh * (1.2 + 0.2 * cos (1.1 * t));

    al_draw_scaled_bitmap (
      Picture, 0, 0, sw, sh,
      cx - dw / 2, cy - dh / 2, dw, dh, 0
    );
    al_flip_display
  END;



(* Checks if a menu item is enabled. *)
  FUNCTION IsEnabled (aMenu: ALLEGRO_MENUptr; aItem: AL_INTPTR_T): BOOLEAN;
  BEGIN
    RESULT := (al_get_menu_item_flags (aMenu, aItem)
               AND ALLEGRO_MENU_ITEM_DISABLED
              ) = 0
  END;



(* Enables or disables a menu item. *)
  PROCEDURE SetEnabled (
    aMenu: ALLEGRO_MENUptr; aItem: AL_INTPTR_T;
    aEnabled: BOOLEAN
  );
  VAR
    Flags: AL_INT;
  BEGIN
    Flags := al_get_menu_item_flags (aMenu, aItem);
    IF aEnabled THEN
      Flags := Flags AND NOT ALLEGRO_MENU_ITEM_DISABLED
    ELSE
      Flags := Flags OR ALLEGRO_MENU_ITEM_DISABLED;
    al_set_menu_item_flags (amenu, aItem, Flags)
  END;



(* Main window menu item options. *)
  PROCEDURE ProcessMainWindowMenuOptions (aOption: AL_INTPTR_T);

    PROCEDURE AddNewItem;
    BEGIN
      IF dCount < 5 THEN
      BEGIN
        INC (dCount);
        IF dCount = 1 THEN
        { Append a separator }
          al_append_menu_item (
            al_find_menu (MainMenu, DYNAMIC_ID), NIL, 0, 0, NIL, NIL
          );
        al_append_menu_item (
          al_find_menu (MainMenu, DYNAMIC_ID),
          AL_STRptr (al_str_format ('New #&%d', [dCount])),
          0, 0, NIL, NIL
        );
        IF dCount = 5 THEN
        { Disable the option }
          SetEnabled (MainMenu, DYNAMIC_CREATE_ID, FALSE)
      END
    END;

    PROCEDURE CreateChildWindow;
    VAR
      ChildWindow: ALLEGRO_DISPLAYptr;
      ChildMenuInfo: ARRAY [0..3] OF ALLEGRO_MENU_INFO;
      ChildMenu: ALLEGRO_MENUptr;
    BEGIN
      ChildWindow := al_create_display (INITIAL_WIDTH, INITIAL_HEIGHT);
      IF ChildWindow <> NIL THEN
      BEGIN
        al_set_window_title (ChildWindow, 'ex_menu - Child Window');
      { Create and bind child menu. }
        ChildMenuInfo[0] := ALLEGRO_ITEM_OF_MENU ('&File->', 0, 0, NIL);
          ChildMenuInfo[1] := ALLEGRO_ITEM_OF_MENU ('&Close', FILE_CLOSE_ID, 0, NIL);
          ChildMenuInfo[2] := ALLEGRO_END_OF_MENU;
        ChildMenuInfo[3] := ALLEGRO_END_OF_MENU;
        ChildMenu := al_build_menu (ChildMenuInfo);
        al_set_display_menu (ChildWindow, ChildMenu);
      { Clean child window. }
        al_clear_to_color (al_map_rgb (0,0,0));
        al_flip_display;
        al_register_event_source (
          Queue,
          al_get_display_event_source (ChildWindow)
        );
      { Restore target bitmap. }
        al_set_target_backbuffer (Display)
      END
    END;

    PROCEDURE ResizeWindow;
    VAR
      w, h: INTEGER;
    BEGIN
      w := al_get_display_width (Display) * 2;
      h := al_get_display_height (Display) * 2;
      IF w > MAX_WIDTH THEN w := MAX_WIDTH;
      IF h > MAX_HEIGHT THEN h := MAX_HEIGHT;
      IF MenuVisible THEN
        al_resize_display (Display, w, h + WindowsMenuHeight)
      ELSE
        al_resize_display (Display, w, h)
    END;

  BEGIN
    CASE aOption OF
    DYNAMIC_CHECKBOX_ID:
      BEGIN
        IF IsEnabled (MainMenu, DYNAMIC_DISABLED_ID) THEN
        BEGIN
          SetEnabled (MainMenu, DYNAMIC_DISABLED_ID, FALSE);
          al_set_menu_item_caption (MainMenu, DYNAMIC_DISABLED_ID, '&Disabled')
        END
        ELSE BEGIN
          SetEnabled (MainMenu, DYNAMIC_DISABLED_ID, TRUE);
          al_set_menu_item_caption (MainMenu, DYNAMIC_DISABLED_ID, 'En&abled')
        END;
      END;
    DYNAMIC_CREATE_ID:
      AddNewItem;
    DYNAMIC_DELETE_ID:
      al_remove_menu_item (MainMenu, DYNAMIC_DELETE_ID);
    FILE_EXIT_ID:
      EndExample := TRUE;
    FILE_FULLSCREEN_ID:
      al_set_display_flag (
        Display, ALLEGRO_FULLSCREEN_WINDOW,
        (al_get_display_flags (Display) AND ALLEGRO_FULLSCREEN_WINDOW) = 0
      );
    FILE_OPEN_ID:
      CreateChildWindow;
    FILE_RESIZE_ID:
      ResizeWindow;
    HELP_ABOUT_ID:
      al_show_native_message_box (
        Display, 'About', 'ex_menu',
        'This is a sample program that shows how to use menus',
        'OK', 0
      );
    END;
  END;



(* Child menu item options. *)
  PROCEDURE ProcessChildWindowMenuOptions (
    aWindow: ALLEGRO_DISPLAYptr;
    aOption: AL_INTPTR_T
  );
  BEGIN
    IF aOption = FILE_CLOSE_ID THEN CloseWindow (aWindow)
  END;

BEGIN
  ProgramInitialization;

  WindowsMenuHeight := 0;
  Redraw := TRUE;
  EndExample := FALSE;
  al_start_timer (Timer);

  REPEAT
  { Draw background. }
    IF Redraw AND al_is_event_queue_empty (Queue) AND (bg <> NIL) THEN
    BEGIN
      DrawBackground (bg);
      Redraw := FALSE
    END;
  { Dispatch events. }
    al_wait_for_event (Queue, @Event);
    CASE (Event.ftype) OF
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
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      IF Event.display.source = Display THEN
      { Closing the primary display }
        EndExample := TRUE
      ELSE BEGIN
      { Closing a secondary display }
        CloseWindow (Event.display.source)
      END;
    ALLEGRO_EVENT_KEY_CHAR:
    { If main window has the keyboard focus. }
      IF Event.keyboard.display = Display THEN
      BEGIN
      { Toggle the menu if the spacebar is pressed }
        IF Event.keyboard.unichar = ORD (' ') THEN
        BEGIN
          IF MenuVisible THEN
            al_remove_display_menu (Display)
          ELSE
            al_set_display_menu (Display, MainMenu);
          MenuVisible := NOT MenuVisible
        END
      { End the example if Esc is pressed. }
        ELSE IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
          EndExample := TRUE;
      END
      ELSE IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
      { Close a child window. }
        CloseWindow (Event.keyboard.display);
    ALLEGRO_EVENT_MENU_CLICK:
    { data1: id
      data2: display (could be NIL)
      data3: menu    (could be NIL) }
      IF Event.user.data2.ptr_value = Display THEN
        ProcessMainWindowMenuOptions (Event.user.data1.int_value)
      ELSE
        ProcessChildWindowMenuOptions (
          Event.user.data2.ptr_value,
          Event.user.data1.int_value
        );
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
    { Popup a context menu on a right click in the main window. }
      IF (Event.mouse.display = Display) AND (Event.mouse.button = 2) THEN
      BEGIN
        IF PopupMenu <> NIL THEN al_popup_menu (PopupMenu, Display)
      END;
    ALLEGRO_EVENT_TIMER:
      Redraw := TRUE;
    END;
  UNTIL EndExample;

  EndProgram
END.
