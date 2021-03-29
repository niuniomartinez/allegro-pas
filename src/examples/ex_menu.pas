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

  uses
    allegro5, al5base, al5image, al5nativedlg, al5strings,
    Common;

  const
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



  var
    WindowsMenuHeight, dCount: Integer;
    Display: ALLEGRO_DISPLAYptr;
    MainMenu, PopupMenu: ALLEGRO_MENUptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Timer: ALLEGRO_TIMERptr;
    EndExample, Redraw, MenuVisible: Boolean;
    bg: ALLEGRO_BITMAPptr;

(* Initializes the example. *)
  procedure ProgramInitialization;

    function CreateMainMenu: ALLEGRO_MENUptr;
    var
      MainMenuInfo: array [0..14] of ALLEGRO_MENU_INFO;
    begin
    { This is one way to define a menu. The entire system, nested menus and all,
      can be defined by this single array. }
      MainMenuInfo[0] := ALLEGRO_ITEM_OF_MENU ('&File->', FILE_ID, 0, Nil);
        MainMenuInfo[1] := ALLEGRO_ITEM_OF_MENU ('&Open', FILE_OPEN_ID, 0, Nil);
        MainMenuInfo[2] := ALLEGRO_MENU_SEPARATOR;
        MainMenuInfo[3] := ALLEGRO_ITEM_OF_MENU ('E&xit', FILE_EXIT_ID, 0, Nil);
        MainMenuInfo[4] := ALLEGRO_END_OF_MENU;

      MainMenuInfo[5] := ALLEGRO_ITEM_OF_MENU (
        '&Dynamic Options->',
        DYNAMIC_ID,
        0, Nil
      );
        MainMenuInfo[6] := ALLEGRO_ITEM_OF_MENU (
          '&Checkbox',
          DYNAMIC_CHECKBOX_ID,
          ALLEGRO_MENU_ITEM_CHECKED,
          Nil
        );
        MainMenuInfo[7] := ALLEGRO_ITEM_OF_MENU (
          '&Disabled',
          DYNAMIC_DISABLED_ID,
          ALLEGRO_MENU_ITEM_DISABLED,
          Nil
        );
        MainMenuInfo[8] := ALLEGRO_ITEM_OF_MENU (
          'D&ELETE ME!',
          DYNAMIC_DELETE_ID,
          0, Nil
        );
        MainMenuInfo[9] := ALLEGRO_ITEM_OF_MENU (
          'C&lick Me',
          DYNAMIC_CREATE_ID,
          0, Nil
        );
        MainMenuInfo[10] := ALLEGRO_END_OF_MENU;

      MainMenuInfo[11] := ALLEGRO_ITEM_OF_MENU ('&Help->', HELP_ID, 0, Nil);
        MainMenuInfo[12] := ALLEGRO_ITEM_OF_MENU ('&About',HELP_ABOUT_ID,0,Nil);
        MainMenuInfo[13] := ALLEGRO_END_OF_MENU;

      MainMenuInfo[14] := ALLEGRO_END_OF_MENU;
    { Create menu. }
      Result := al_build_menu (MainMenuInfo);
    { Add an icon to the Help/About item. Note that Allegro assumes ownership of
      the bitmap. }
      al_set_menu_item_icon (
        Result,
        HELP_ABOUT_ID,
        al_load_bitmap ('data/icon.tga')
      )
    end;

    function CreatePopupMenu: ALLEGRO_MENUptr;
    begin
      Result := al_create_popup_menu;
      if Result <> Nil then
      begin
      { Another way to define the menu items. }
        al_append_menu_item (Result, '&Open', FILE_OPEN_ID, 0, Nil, Nil);
        al_append_menu_item (Result, '&Resize', FILE_RESIZE_ID, 0, Nil, Nil);
        al_append_menu_item (
          Result,
          '&Fullscreen window',
          FILE_FULLSCREEN_ID,
          0, Nil, Nil
        );
        al_append_menu_item (Result, 'E&xit', FILE_EXIT_ID, 0, Nil, Nil)
      end
    end;

  begin
    if not al_init then AbortExample ('Could not init Allegro.');
    if not al_init_native_dialog_addon then
      AbortExample ('Could not init the native dialog addon.');
    al_init_image_addon;
    al_install_keyboard;
    al_install_mouse;

    Queue := al_create_event_queue;
{$IFDEF LINUX}
{$HINT Assumming GTK+.  Qt users may need to override this(?)}
    al_set_new_display_flags (ALLEGRO_RESIZABLE or ALLEGRO_GTK_TOPLEVEL);
{$ELSE}
    al_set_new_display_flags (ALLEGRO_RESIZABLE);
{$ENDIF}

    Display := al_create_display (INITIAL_WIDTH, INITIAL_HEIGHT);
    if Display = Nil then AbortExample ('Error creating display.');
    al_set_window_title (Display, 'ex_menu - Main Window');

    MainMenu := CreateMainMenu;
    if MainMenu = Nil then AbortExample ('Error creating menu.');

    if not al_set_display_menu (Display, MainMenu) then
    begin
    { Since the menu could not be attached to the window, then treat it as a
      popup menu instead. }
      PopupMenu := al_clone_menu_for_popup (MainMenu);
      al_destroy_menu (MainMenu);
      MainMenu := PopupMenu
    end
    else
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
  end;



(* Closes a window. *)
  procedure CloseWindow (aWindow: ALLEGRO_DISPLAYptr);
  begin
    if aWindow <> Nil then
    begin
    { You must remove the menu before destroying the display to free resources. }
      al_set_display_menu (aWindow, Nil);
      al_destroy_display (aWindow)
    end
  end;



(* Releases resources. *)
  procedure EndProgram;
  begin
    CloseWindow (Display);
    al_destroy_event_queue (Queue);
    al_destroy_bitmap (bg)
  end;



(* Draws background. *)
  procedure DrawBackground (Picture: ALLEGRO_BITMAPptr);
  var
    t, sw, sh, dw, dh, cx, cy: REAL;
  begin
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
  end;



(* Checks if a menu item is enabled. *)
  function IsEnabled (aMenu: ALLEGRO_MENUptr; aItem: AL_INTPTR_T): Boolean;
  begin
    Result := (al_get_menu_item_flags (aMenu, aItem)
               and ALLEGRO_MENU_ITEM_DISABLED
              ) = 0
  end;



(* Enables or disables a menu item. *)
  procedure SetEnabled (
    aMenu: ALLEGRO_MENUptr; aItem: AL_INTPTR_T;
    aEnabled: Boolean
  );
  var
    Flags: AL_INT;
  begin
    Flags := al_get_menu_item_flags (aMenu, aItem);
    if aEnabled then
      Flags := Flags and not ALLEGRO_MENU_ITEM_DISABLED
    else
      Flags := Flags or ALLEGRO_MENU_ITEM_DISABLED;
    al_set_menu_item_flags (amenu, aItem, Flags)
  end;



(* Main window menu item options. *)
  procedure ProcessMainWindowMenuOptions (aOption: AL_INTPTR_T);

    procedure AddNewItem;
    begin
      if dCount < 5 then
      begin
        Inc (dCount);
        if dCount = 1 then
        { Append a separator }
          al_append_menu_item (
            al_find_menu (MainMenu, DYNAMIC_ID), Nil, 0, 0, Nil, Nil
          );
        al_append_menu_item (
          al_find_menu (MainMenu, DYNAMIC_ID),
          AL_STRptr (al_str_format ('New #&%d', [dCount])),
          0, 0, Nil, Nil
        );
        if dCount = 5 then
        { Disable the option }
          SetEnabled (MainMenu, DYNAMIC_CREATE_ID, False)
      end
    end;

    procedure CreateChildWindow;
    var
      ChildWindow: ALLEGRO_DISPLAYptr;
      ChildMenuInfo: array [0..3] of ALLEGRO_MENU_INFO;
      ChildMenu: ALLEGRO_MENUptr;
    begin
      ChildWindow := al_create_display (INITIAL_WIDTH, INITIAL_HEIGHT);
      if ChildWindow <> Nil then
      begin
        al_set_window_title (ChildWindow, 'ex_menu - Child Window');
      { Create and bind child menu. }
        ChildMenuInfo[0] := ALLEGRO_ITEM_OF_MENU ('&File->', 0, 0, Nil);
          ChildMenuInfo[1] := ALLEGRO_ITEM_OF_MENU ('&Close', FILE_CLOSE_ID, 0, Nil);
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
      end
    end;

    procedure ResizeWindow;
    var
      w, h: Integer;
    begin
      w := al_get_display_width (Display) * 2;
      h := al_get_display_height (Display) * 2;
      if w > MAX_WIDTH then w := MAX_WIDTH;
      if h > MAX_HEIGHT then h := MAX_HEIGHT;
      if MenuVisible then
        al_resize_display (Display, w, h + WindowsMenuHeight)
      else
        al_resize_display (Display, w, h)
    end;

  begin
    case aOption OF
    DYNAMIC_CHECKBOX_ID:
      begin
        if IsEnabled (MainMenu, DYNAMIC_DISABLED_ID) then
        begin
          SetEnabled (MainMenu, DYNAMIC_DISABLED_ID, False);
          al_set_menu_item_caption (MainMenu, DYNAMIC_DISABLED_ID, '&Disabled')
        end
        else begin
          SetEnabled (MainMenu, DYNAMIC_DISABLED_ID, True);
          al_set_menu_item_caption (MainMenu, DYNAMIC_DISABLED_ID, 'En&abled')
        end;
      end;
    DYNAMIC_CREATE_ID:
      AddNewItem;
    DYNAMIC_DELETE_ID:
      al_remove_menu_item (MainMenu, DYNAMIC_DELETE_ID);
    FILE_EXIT_ID:
      EndExample := True;
    FILE_FULLSCREEN_ID:
      al_set_display_flag (
        Display, ALLEGRO_FULLSCREEN_WINDOW,
        (al_get_display_flags (Display) and ALLEGRO_FULLSCREEN_WINDOW) = 0
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
    end;
  end;



(* Child menu item options. *)
  procedure ProcessChildWindowMenuOptions (
    aWindow: ALLEGRO_DISPLAYptr;
    aOption: AL_INTPTR_T
  );
  begin
    if aOption = FILE_CLOSE_ID then CloseWindow (aWindow)
  end;

begin
  ProgramInitialization;

  WindowsMenuHeight := 0;
  Redraw := True;
  EndExample := False;
  al_start_timer (Timer);

  repeat
  { Draw background. }
    if Redraw and al_is_event_queue_empty (Queue) and (bg <> Nil) then
    begin
      DrawBackground (bg);
      Redraw := False
    end;
  { Dispatch events. }
    al_wait_for_event (Queue, @Event);
    case (Event.ftype) OF
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      begin
        al_acknowledge_resize (Display);
        Redraw := True;
{$IFDEF WINDOWS}
{ XXX The Windows implementation currently uses part of the client's height to
      render the window.  This triggers a resize event, which can be trapped
      and used to compute the menu height, and then resize the display again to
      what we expect it to be. }
        if (Event.display.source = Display) and (WindowsMenuHeight = 0) then
        begin
          WindowsMenuHeight := INITIAL_HEIGHT - al_get_display_height (Display);
          al_resize_display
            (Display, INITIAL_WIDTH, INITIAL_HEIGHT + WindowsMenuHeight);
        end
{$ENDIF}
      end;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      if Event.display.source = Display then
      { Closing the primary display }
        EndExample := True
      else begin
      { Closing a secondary display }
        CloseWindow (Event.display.source)
      end;
    ALLEGRO_EVENT_KEY_CHAR:
    { If main window has the keyboard focus. }
      if Event.keyboard.display = Display then
      begin
      { Toggle the menu if the spacebar is pressed }
        if Event.keyboard.unichar = Ord (' ') then
        begin
          if MenuVisible then
            al_remove_display_menu (Display)
          else
            al_set_display_menu (Display, MainMenu);
          MenuVisible := not MenuVisible
        end
      { End the example if Esc is pressed. }
        else if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          EndExample := True;
      end
      else if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
      { Close a child window. }
        CloseWindow (Event.keyboard.display);
    ALLEGRO_EVENT_MENU_CLICK:
    { data1: id
      data2: display (could be Nil)
      data3: menu    (could be Nil) }
      if Event.user.data2.ptr_value = Display then
        ProcessMainWindowMenuOptions (Event.user.data1.int_value)
      else
        ProcessChildWindowMenuOptions (
          Event.user.data2.ptr_value,
          Event.user.data1.int_value
        );
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
    { Popup a context menu on a right click in the main window. }
      if (Event.mouse.display = Display) and (Event.mouse.button = 2) then
      begin
        if PopupMenu <> Nil then al_popup_menu (PopupMenu, Display)
      end;
    ALLEGRO_EVENT_TIMER:
      Redraw := True;
    end;
  until EndExample;

  EndProgram
end.
