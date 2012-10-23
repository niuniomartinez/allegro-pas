UNIT algui;
(*<GUI routines

  Allegro contains an object-oriented dialog manager, which was originally
  based on the Atari GEM system (form_do(), objc_draw(), etc: old ST
  programmers will know what we are talking about :-)  You can use the GUI
  as-is to knock out simple interfaces for things like the test program and
  setup utility, or you can use it as a basis for more complicated systems of
  your own.  Allegro lets you define your own object types by writing new
  dialog procedures, so you can take complete control over the visual aspects
  of the interface while still using Allegro to handle input from the mouse,
  keyboard, joystick, etc.

  A GUI dialog is stored as an array of @link(AL_DIALOG) objects.  The array
  should end with an object which has the proc pointer set to @nil.  Each
  object has a flags field which may contain any combination of the bit flags:
  @unorderedList(
    @item(@bold(AL_D_EXIT) - this object should close the dialog when it is
      clicked)
    @item(@bold(AL_D_SELECTED) - this object is selected)
    @item(@bold(AL_D_GOTFOCUS) - this object has got the input focus)
    @item(@bold(AL_D_GOTMOUSE) - the mouse is currently on top of this object)
    @item(@bold(AL_D_HIDDEN) - this object is hidden and inactive)
    @item(@bold(AL_D_DISABLED) - this object is greyed-out and inactive)
    @item(@bold(AL_D_DIRTY) - this object needs to be redrawn)
    @item(@bold(AL_D_INTERNAL) - don't use this! It is for internal use by the
      library...)
    @item(@bold(AL_D_USER) - any powers of two above this are free for your
      own use)
  )

  Each object is controlled by a dialog procedure, which is stored in the
  @code(proc) pointer.  This will be called by the dialog manager whenever any
  action concerning the object is required, or you can call it directly with
  the @link(al_object_message) function.  The dialog procedure should follow
  the form:
@longcode(#
       FUNCTION foo (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
#)
  It will be passed a flag (msg) indicating what action it should perform, a
  pointer to the object concerned (d), and if msg is @code(AL_MSG_CHAR) or
  @code(AL_MSG_XCHAR), the key that was pressed (c).

  The dialog procedure should return one of the values:
  @unorderedList(
    @item(@bold(AL_D_O_K) - normal return status)
    @item(@bold(AL_D_CLOSE) - tells the dialog manager to close the dialog)
    @item(@bold(AL_D_REDRAW) - tells the dialog manager to redraw the entire
      dialog)
    @item(@bold(AL_D_REDRAWME) - tells the dialog manager to redraw the current
      object)
    @item(@bold(AL_D_WANTFOCUS) - requests that the input focus be given to this
      object)
    @item(@bold(AL_D_USED_CHAR) - MSG_CHAR and MSG_XCHAR return this if they used
      the key)
  )

  Dialog procedures may be called with any of the messages:
  @definitionList(
    @itemlabel(AL_MSG_START)
    @item(Tells the object to initialise itself.  The dialog manager sends this
      to all the objects in a dialog just before it displays the dialog.)
    @itemlabel(AL_MSG_END)
    @item(Sent to all objects when closing a dialog, allowing them to perform
      whatever cleanup operations they require.)
    @itemlabel(AL_MSG_DRAW)
    @item(Tells the object to draw itself onto the screen.  The mouse pointer
      will be turned off when this message is sent, so the drawing code does
      not need to worry about it.)
    @itemlabel(AL_MSG_CLICK)
    @item(Informs the object that a mouse button has been clicked while the
      mouse was on top of the object.  Typically an object will perform its own
      mouse tracking as long as the button is held down, and only return from
      this message handler when it is released.

      If you process this message, use the functions @code(al_gui_mouse_* ) to
      read the state of the mouse.)
    @itemlabel(AL_MSG_DCLICK)
    @item(Sent when the user double-clicks on an object.  A @code(AL_MSG_CLICK)
      will be sent when the button is first pressed, then @code(AL_MSG_DCLICK)
      if it is released and pressed again within a short space of time.

      If you process this message, use the functions @code(al_gui_mouse_* ) to
      read the state of the mouse.)
    @itemlabel(AL_MSG_KEY)
    @item(Sent when the keyboard shortcut for the object is pressed, or if
      enter, space, or a joystick button is pressed while it has the input
      focus.)
    @itemlabel(AL_MSG_CHAR)
    @item(When a key is pressed, this message is sent to the object that has
      the input focus, with a @link(al_readkey) format character code (ASCII
      value in the low byte, scancode in the high byte) as the @code(c)
      parameter.  If the object deals with the keypress it should return
      @code(AL_D_USED_CHAR), otherwise it should return @code(AL_D_O_K) to
      allow the default keyboard interface to operate.  If you need to access
      Unicode character input, you should use @code(AL_MSG_UCHAR) instead of
      @code(AL_MSG_CHAR).)
    @itemlabel(AL_MSG_UCHAR)
    @item(If an object ignores the @code(AL_MSG_CHAR) input, this message will
      be sent immediately after it, passed the full Unicode key value as the
      @code(c) parameter.  This enables you to read character codes greater
      than 255, but cannot tell you anything about the scancode:  if you need
      to know that, use @code(AL_MSG_CHAR) instead.  This handler should return
      @code(AL_D_USED_CHAR) if it processed the input, or @code(AL_D_O_K)
      otherwise.)
    @itemlabel(AL_MSG_XCHAR)
    @item(When a key is pressed, Allegro will send a @code(AL_MSG_CHAR) and
      @code(AL_MSG_UCHAR) to the object with the input focus.  If this object
      doesn't process the key @(ie. it returns @code(AL_D_O_K) rather than
      @code(AL_D_USED_CHAR)@), the dialog manager will look for an object with
      a matching keyboard shortcut in the key field, and send it a
      @code(AL_MSG_KEY).  If this fails, it broadcasts a @code(AL_MSG_XCHAR) to
      all objects in the dialog, allowing them to respond to special keypresses
      even when they don't have the input focus.  Normally you should ignore
      this message @(return @code(AL_D_O_K) rather than
      @code(AL_D_USED_CHAR)@), in which case Allegro will perform default
      actions such as moving the focus in response to the arrow keys and
      closing the dialog if @code(Esc) is pressed.)
    @itemlabel(AL_MSG_WANTFOCUS)
    @item(Queries whether an object is willing to accept the input focus.  It
      should return @code(AL_D_WANTFOCUS) if it does, or @code(AL_D_O_K) if it
      isn't interested in getting user input.)
    @itemlabel(AL_MSG_GOTFOCUS, AL_MSG_LOSTFOCUS)
    @item(Sent whenever an object gains or loses the input focus.  These
      messages will always be followed by a @code(AL_MSG_DRAW), to let objects
      display themselves differently when they have the input focus.  If you
      return @code(AL_D_WANTFOCUS) in response to a @code(AL_MSG_LOSTFOCUS)
      event, this will prevent your object from losing the focus when the mouse
      moves off it onto the screen background or some inert object, so it will
      only lose the input focus when some other object is ready to take over
      the focus @(this trick is used by the @link(al_d_edit_proc) object@).)
    @itemlabel(AL_MSG_GOTMOUSE, ALMSG_LOSTMOUSE)
    @item(Sent when the mouse moves on top of or away from an object.  Unlike
      the focus messages, these are not followed by a @code(AL_MSG_DRAW), so if
      the object is displayed differently when the mouse is on top of it, it is
      responsible for redrawing itself in response to these messages.)
    @itemlabel(AL_MSG_IDLE)
    @item(Sent whenever the dialog manager has nothing better to do.)
    @itemlabel(AL_MSG_RADIO)
    @item(Sent by radio button objects to deselect other buttons in the same
      group when they are clicked.  The group number is passed in the @code(c)
      message parameter.)
    @itemlabel(AL_MSG_WHEEL)
    @item(Sent to the focused object whenever the mouse wheel moves.  The
      @code(c) message parameter contains the number of clicks.)
    @itemlabel(AL_MSG_LPRESS, AL_MSG_MPRESS, AL_MSG_RPRESS)
    @item(Sent when the corresponding mouse button is pressed.)
    @itemlabel(AL_MSG_LRELEASE, AL_MSG_MRELEASE, AL_MSG_RRELEASE)
    @item(Sent when the corresponding mouse button is released.)
    @itemlabel(AL_MSG_USER)
    @item(The first free message value. Any numbers from here on
      @(@code(AL_MSG_USER, AL_MSG_USER+1, AL_MSG_USER+2, ... AL_MSG_USER+n)@)
      are free to use for whatever you like.)
    )

  Allegro provides several standard dialog procedures.  You can use these as
  they are to provide simple user interface objects, or you can call them from
  within your own dialog procedures, resulting in a kind of OOP inheritance.
  For instance, you could make an object which calls @code(al_d_button_proc) to
  draw itself, but handles the click message in a different way, or an object
  which calls @code(al_d_button_proc) for everything except drawing itself, so
  it would behave like a normal button but could look completely different.

  Popup or pulldown menus are created as an array of @link(AL_MENU) structures.
  Each menu item contains a text string.  This can use the @code('&') character
  to indicate keyboard shortcuts, or can be an zero-length string to display
  the item as a non-selectable splitter bar.  If the string contains a
  @code(tab) character, any text after this will be right-justified, eg. for
  displaying keyboard shortcut information.  The @code(proc) field of
  @code(AL_MENU) is a function which will be called when the menu item is
  selected, and @code(child) points to another menu, allowing you to create
  nested menus.  Both @code(proc) and @code(child) may be @nil.  The
  @code(proc) function returns an integer which is ignored if the menu was
  brought up by calling @link(al_do_menu), but which is passed back to the
  dialog manager if it was created by a @link(al_d_menu_proc) object.  The
  array of menu items is terminated by an entry with a @nil text pointer.

  Menu items can be disabled (greyed-out) by setting the @code(AL_D_DISABLED)
  bit in the @code(flags) field, and a check mark can be displayed next to them
  by setting the @code(AL_D_SELECTED) bit.  With the default alignment and font
  this will usually overlap the menu text, so if you are going to use checked
  menu items it would be a good idea to prefix all your options with a space or
  two, to ensure there is room for the check. *)

{$INCLUDE allegro.cfg }

INTERFACE

  USES
    albase, allegro;



  TYPE
  (* Pointer to @link(AL_DIALOG). *)
    AL_DIALOGptr = ^AL_DIALOG;

  (* Dialog function object. @seealso(AL_DIALOG) *)
    AL_DIALOG_PROC = FUNCTION (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT; CDECL;

  (* This is the structure which contains a GUI object.
     @seealso(al_do_dialog) *)
    AL_DIALOG = RECORD
      proc: AL_DIALOG_PROC; (*<Dialog procedure (message handler) *)
      x, y, w, h: AL_INT;   (*<Position and size of the object *)
      fg, bg: AL_INT;       (*<Foreground and background colors *)
      key: AL_INT;          (*<Keyboard shortcut (ASCII code) *)
      flags: AL_INT;        (*<Flags about the object state *)
      d1, d2: AL_INT;       (*<Any data the object might require *)
      dp, dp2, dp3: AL_VOIDptr;(*<Pointers to more object data *)
    END;



  (* Pointer to @link(AL_MENU). *)
    AL_MENUptr = ^AL_MENU;

  (* Structure used to hold an entry of a menu.
     @seealso(al_do_menu) *)
    AL_MENU = RECORD
      txt: AL_STRptr; (*<The text to display for the menu item. *)
      proc: AL_SIMPLE_FUNC; (*<Called when menu item is clicked. *)
      child: AL_MENUptr; (*<Nested child menu. *)
      flags: AL_INT; (*<Disabled or checked state. *)
      dp: AL_VOIDptr; (*<Pointer to any data you need. *)
    END;



  (* A structure which holds GUI data used internally by Allegro.
     @seealso(al_init_dialog) *)
    AL_DIALOG_PLAYERptr = AL_POINTER;

  (* A structure which holds GUI data used internally by Allegro.
     @seealso(al_init_menu) *)
    AL_MENU_PLAYERptr = AL_POINTER;

  (* Hook function for @link(al_gui_menu_draw_menu). *)
    AL_MENU_POS_PROC = PROCEDURE (x, y, w, h: AL_INT); CDECL;

  (* Hook function for @link(al_gui_menu_draw_menu_item). *)
    AL_MENU_ITEM_PROC = PROCEDURE (m: AL_MENUptr; x, y, w, h, bar, sel: AL_INT); CDECL;



  CONST
  (* bits for the flags field *)
    AL_D_EXIT     = 1;  (*<Object makes the dialog exit *)
    AL_D_SELECTED = 2;  (*<Object is selected *)
    AL_D_GOTFOCUS = 4;  (*<Object has the input focus *)
    AL_D_GOTMOUSE = 8;  (*<Mouse is on top of object *)
    AL_D_HIDDEN   = 16; (*<Object is not visible *)
    AL_D_DISABLED = 32; (*<Object is visible but inactive *)
    AL_D_DIRTY    = 64; (*<Object needs to be redrawn *)
    AL_D_INTERNAL = 128;(*<Reserved for internal use *)
    AL_D_USER     = 256;(*<From here on is free for your own use *)



  (* return values for the dialog procedures *)
    AL_D_O_K           = 0;        (*<normal exit status *)
    AL_D_CLOSE         = 1;        (*<request to close the dialog *)
    AL_D_REDRAW        = 2;        (*<request to redraw the dialog *)
    AL_D_REDRAWME      = 4;        (*<request to redraw this object *)
    AL_D_WANTFOCUS     = 8;        (*<this object wants the input focus *)
    AL_D_USED_CHAR     = 16;       (*<object has used the keypress *)
    AL_D_REDRAW_ALL    = 32;       (*<request to redraw all active dialogs *)
    AL_D_DONTWANTMOUSE = 64;       (*<this object does not want mouse focus *)



  (* messages for the dialog procedures *)
    AL_MSG_START       = 1;        (*<start the dialog, initialise *)
    AL_MSG_END         = 2;        (*<dialog is finished - cleanup *)
    AL_MSG_DRAW        = 3;        (*<draw the object *)
    AL_MSG_CLICK       = 4;        (*<mouse click on the object *)
    AL_MSG_DCLICK      = 5;        (*<double click on the object *)
    AL_MSG_KEY         = 6;        (*<keyboard shortcut *)
    AL_MSG_CHAR        = 7;        (*<other keyboard input *)
    AL_MSG_UCHAR       = 8;        (*<unicode keyboard input *)
    AL_MSG_XCHAR       = 9;        (*<broadcast character to all objects *)
    AL_MSG_WANTFOCUS   = 10;       (*<does object want the input focus? *)
    AL_MSG_GOTFOCUS    = 11;       (*<got the input focus *)
    AL_MSG_LOSTFOCUS   = 12;       (*<lost the input focus *)
    AL_MSG_GOTMOUSE    = 13;       (*<mouse on top of object *)
    AL_MSG_LOSTMOUSE   = 14;       (*<mouse moved away from object *)
    AL_MSG_IDLE        = 15;       (*<update any background stuff *)
    AL_MSG_RADIO       = 16;       (*<clear radio buttons *)
    AL_MSG_WHEEL       = 17;       (*<mouse wheel moved *)
    AL_MSG_LPRESS      = 18;       (*<mouse left button pressed *)
    AL_MSG_LRELEASE    = 19;       (*<mouse left button released *)
    AL_MSG_MPRESS      = 20;       (*<mouse middle button pressed *)
    AL_MSG_MRELEASE    = 21;       (*<mouse middle button released *)
    AL_MSG_RPRESS      = 22;       (*<mouse right button pressed *)
    AL_MSG_RRELEASE    = 23;       (*<mouse right button released *)
    AL_MSG_WANTMOUSE   = 24;       (*<does object want the mouse? *)
    AL_MSG_USER        = 25;       (*<from here on are free... *)



(* An invisible helper object that yields time slices for the scheduler (if the
   system supports it) when the GUI has nothing to do but waiting for user
   actions.  You should put one instance of this object in each dialog array
   because it may be needed on systems with an unusual scheduling algorithm
   (for instance QNX) in order to make the GUI fully responsive.
   @seealso(al_rest) *)
  FUNCTION al_d_yield_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_yield_proc';
(* This just clears the screen when it is drawn.  Useful as the first object in
   a dialog. *)
  FUNCTION al_d_clear_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_clear_proc';
(* Just draws a box. *)
  FUNCTION al_d_box_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_box_proc';
(* Just draws a box with a shadow. *)
  FUNCTION al_d_shadow_box_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_shadow_box_proc';
(* This draws a bitmap onto the screen, which should be pointed to by the
   @code(dp) field. *)
  FUNCTION al_d_bitmap_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_bitmap_proc';
(* Draws text onto the screen. The @code(dp) field should point to the string
   to display.  Any @code('&') characters in the string will be replaced with
   lines underneath the following character, for displaying keyboard shortcuts
   (as in MS Windows).  To display a single ampersand, put @code('&&').  To
   draw the text in something other than the default font, set the @code(dp2)
   field to point to your custom font data. *)
  FUNCTION al_d_text_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_text_proc';
(* Draws centered text onto the screen.  The @code(dp) field should point to
   the string to display.  Any @code('&') characters in the string will be
   replaced with lines underneath the following character, for displaying
   keyboard shortcuts (as in MS Windows).  To display a single ampersand, put
   @code('&&').  To draw the text in something other than the default font, set
   the @code(dp2) field to point to your custom font data. *)
  FUNCTION al_d_ctext_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_ctext_proc';
(* Draws text right aligned onto the screen.  The @code(dp) field should point
   to the string to display.  Any @code('&') characters in the string will be
   replaced with lines underneath the following character, for displaying
   keyboard shortcuts (as in MS Windows).  To display a single ampersand, put
   @code('&&').  To draw the text in something other than the default font, set
   the @code(dp2) field to point to your custom font data. *)
  FUNCTION al_d_rtext_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_rtext_proc';
(* A button object (the @code(dp) field points to the text string).  This
   object can be selected by clicking on it with the mouse or by pressing its
   keyboard shortcut.  If the @code(AL_D_EXIT) flag is set, selecting it will
   close the dialog, otherwise it will toggle on and off.  Like
   @link(al_d_text_proc), ampersands can be used to display the keyboard
   shortcut of the button. *)
  FUNCTION al_d_button_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_button_proc';
(* This is an example of how you can derive objects from other objects.  Most
   of the functionality comes from @link(al_d_button_proc), but it displays
   itself as a check box.  If the @code(d1) field is non-zero, the text will be
   printed to the right of the check, otherwise it will be on the left.

   @bold(Note:)  the object width should allow space for the text as well as
   the check box (which is square, with sides equal to the object height). *)
  FUNCTION al_d_check_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_check_proc';
(* A radio button object.  A dialog can contain any number of radio button
   groups:  selecting a radio button causes other buttons within the same group
   to be deselected.  The @code(dp) field points to the text string, @code(d1)
   specifies the group number, and @code(d2) is the button style (0=circle,
   1=square). *)
  FUNCTION al_d_radio_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_radio_proc';
(* A bitmap button.  The @code(fg) color is used for the dotted line showing
   focus, and the @code(bg) color for the shadow used to fill in the top and
   left sides of the button when @italic("pressed").  @code(d1) is the
   @italic("push depth"), ie. the number of pixels the icon will be shifted to
   the right and down when selected (default 2) if there is no
   @italic("selected") image.  @code(d2) is the distance by which the dotted
   line showing focus is indented (default 2).  @code(dp) points to a bitmap
   for the icon, while @code(dp2) and @code(dp3) are the selected and disabled
   images respectively (optional, may be @nil). *)
  FUNCTION al_d_icon_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_icon_proc';
(* This is an invisible object for implementing keyboard shortcuts.  You can
   put an ASCII code in the key field of the dialog object (a character such as
  'a' to respond to a simple keypress, or a number 1-26 to respond to a control
  key a-z), or you can put a keyboard scancode in the @code(d1) and/or
  @code(d2) fields.  When one of these keys is pressed, the object will call
  the function pointed to by @code(dp).  This should return an integer, which
  will be passed back to the dialog manager, so it can return @code(AL_D_O_K),
  @code(AL_D_REDRAW), @code(AL_D_CLOSE), etc. *)
  FUNCTION al_d_keyboard_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_keyboard_proc';
(* An editable text object (the @code(dp) field points to the space to store
   the string).  When it has the input focus (obtained by clicking on it with
   the mouse), text can be typed into this object.  The @code(d1) field
   specifies the maximum number of characters that it will accept, and
   @code(d2) is the text cursor position within the string.

   @bold(Note:) @code(dp) must point to a buffer at least @code(@(d1 + 1@) *
   4) bytes long because, depending on the encoding format in use, a single
   character can occupy up to 4 bytes and room must be reserved for the
   terminating null character.  @bold(Be sure it finishes with a null
     character or it will fail!)*)
  FUNCTION al_d_edit_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_edit_proc';
(* A list box object.  This will allow the user to scroll through a list of
   items and to select one by clicking or with the arrow keys.  If the
   @code(AL_D_EXIT) flag is set, double clicking on a list item will close the
   dialog.  The index of the selected item is held in the @code(d1) field, and
   @code(d2) is used to store how far it has scrolled through the list.  The
   @code(dp) field points to a function which will be called to obtain
   information about the contents of the list.  This should follow the form:
   @longcode(#
FUNCTION foobar(index: AL_INT; list_size: AL_INTptr): AL_STRptr; CDECL;
   #)
   If @code(index) is zero or positive, the function should return a pointer to
   the string which is to be displayed at position index in the list.  If
   @code(index) is negative, it should return @nil and @code(list_size) should
   be set to the number of items in the list.

   To create a multiple selection listbox, set the @code(dp2) field to the
   first element of an @code(ARRAY OF BYTE) flags indicating the selection
   state of each list item (non-zero for selected entries).  This table must be
   at least as big as the number of objects in the list! *)
  FUNCTION al_d_list_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_list_proc';
(* Like @link(al_d_list_proc), but allows the user to type in the first few
   characters of a listbox entry in order to select it.  Uses @code(dp3)
   internally, so you mustn't store anything important there yourself. *)
  FUNCTION al_d_text_list_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_text_list_proc';
(* A text box object.  The @code(dp) field points to the text which is to be
   displayed in the box.  If the text is long, there will be a vertical
   scrollbar on the right hand side of the object which can be used to scroll
   through the text.  The default is to print the text with word wrapping, but
   if the @code(AL_D_SELECTED) flag is set, the text will be printed with
   character wrapping.  The @code(d1) field is used internally to store the
   number of lines of text, and @code(d2) is used to store how far it has
   scrolled through the text. *)
  FUNCTION al_d_textbox_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_textbox_proc';
(* A slider control object.  This object holds a value in @code(d2), in the
   range from 0 to @code(d1).  It will display as a vertical slider if height
   is greater than or equal to width, otherwise it will display as a horizontal
   slider.  The @code(dp) field can contain an optional bitmap to use for the
   slider handle, and @code(dp2) can contain an optional callback function,
   which is called each time @code(d2) changes.  The callback function should
   have the following prototype:
   @longcode(#
FUNCTION foo (dp3: AL_VOIDptr; d2: AL_INT): AL_INT; CDECL;
   #)
   The @code(al_d_slider_proc) object will return the value of the callback function.  *)
  FUNCTION al_d_slider_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_slider_proc';
(* This object is a menu bar which will drop down child menus when it is
   clicked or if an alt+key corresponding to one of the shortcuts in the menu
   is pressed.  It ignores a lot of the fields in the dialog structure, in
   particular the color is taken from the @code(al_gui_*_color) variables, and
   the width and height are calculated automatically (the @code(w) and @code(h)
   fields from the @code(AL_DIALOG) are only used as a minimum size.)  The
   @code(dp) field points to an array of menu structures:  see
   @link(al_do_menu) for more information.  The top level menu will be
   displayed as a horizontal bar, but when child menus drop down from it they
   will be in the normal vertical format used by @code(al_do_menu).  When a
   menu item is selected, the return value from the menu callback function is
   passed back to the dialog manager, so your callbacks should return
   @code(AL_D_O_K), @code(AL_D_REDRAW), or @code(AL_D_CLOSE).
   @seealso(al_active_menu) @seealso(al_gui_menu_draw_menu) *)
  FUNCTION al_d_menu_proc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'd_menu_proc';



  VAR
  (* If set, this function will be used by the standard Allegro dialogs.  This
     allows you to customise the look and feel, much like @link(al_gui_fg_color)
     and @link(al_gui_bg_color), but much more flexibly. *)
    al_gui_shadow_box_proc: AL_DIALOG_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_shadow_box_proc';
  (* If set, this function will be used by the standard Allegro dialogs.  This
     allows you to customise the look and feel, much like @link(al_gui_fg_color)
     and @link(al_gui_bg_color), but much more flexibly. *)
    al_gui_ctext_proc: AL_DIALOG_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_ctext_proc';
  (* If set, this function will be used by the standard Allegro dialogs.  This
     allows you to customise the look and feel, much like @link(al_gui_fg_color)
     and @link(al_gui_bg_color), but much more flexibly. *)
    al_gui_button_proc: AL_DIALOG_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_button_proc';
  (* If set, this function will be used by the standard Allegro dialogs.  This
     allows you to customise the look and feel, much like @link(al_gui_fg_color)
     and @link(al_gui_bg_color), but much more flexibly. *)
    al_gui_edit_proc: AL_DIALOG_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_edit_proc';
  (* If set, this function will be used by the standard Allegro dialogs.  This
     allows you to customise the look and feel, much like @link(al_gui_fg_color)
     and @link(al_gui_bg_color), but much more flexibly. *)
    al_gui_list_proc: AL_DIALOG_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_list_proc';
  (* If set, this function will be used by the standard Allegro dialogs.  This
     allows you to customise the look and feel, much like @link(al_gui_fg_color)
     and @link(al_gui_bg_color), but much more flexibly. *)
    al_gui_text_list_proc: AL_DIALOG_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_text_list_proc';

  (* If set, this function will be called whenever a menu needs to be drawn, so
     you can change how menus look.

     It should draw the background of the menu onto screen. *)
    al_gui_menu_draw_menu: AL_MENU_POS_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_menu_draw_menu';
  (* If set, this function will be called whenever a menu needs to be drawn, so
     you can change how menus look.

     It is called once for each menu item that is to be drawn.  @code(bar) will
     be non-zero if the item is part of a top-level horizontal menu bar, and
     @code(sel) will be non-zero if the menu item is selected.  It should also
     draw onto screen. *)
    al_gui_menu_draw_menu_item: AL_MENU_ITEM_PROC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_menu_draw_item';

  (* Global pointer to the most recent activated dialog.  This may be useful if
     an object needs to iterate through a list of all its siblings. *)
    al_active_dialog: AL_DIALOGptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'active_dialog';
  (* When a menu callback procedure is triggered, this will be set to the menu
     item that was selected, so your routine can determine where it was called from. *)
    al_active_menu: AL_MENUptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'active_menu';

  (* If non-zero, the input focus follows the mouse pointer around the dialog,
     otherwise a click is required to move it. *)
    al_gui_mouse_focus: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_mouse_focus';

  (* The foreground color for the standard dialogs (alerts, menus, and the file
     selector).  Defaults to 255.
     @seealso(al_gui_bg_color) @seealso(al_gui_mg_color)
     @seealso(al_set_dialog_color) *)
    al_gui_fg_color: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_fg_color';
  (* The color used for displaying greyed-out dialog objects (with the
     @code(AL_D_DISABLED) flag set). Defaults to 8.
     @seealso(al_gui_fg_color) @seealso(al_gui_bg_color)
     @seealso(al_set_dialog_color) *)
    al_gui_mg_color: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_mg_color';
  (* The background color for the standard dialogs (alerts, menus, and the file
     selector). Defaults to 0.
     @seealso(al_gui_fg_color) @seealso(al_gui_mg_color)
     @seealso(al_set_dialog_color) *)
    al_gui_bg_color: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_bg_color';

  (* If set to a non-zero value, adjusts the keyboard shortcut underscores to
     account for the height of the descenders in your font. *)
    al_gui_font_baseline: AL_INT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_font_baseline';

  (* Hook function, used by the GUI routines whenever they need to access the
     mouse state.  By default it just returns a copy of the @code(al_mouse_x)
     variable, but it could be used to offset or scale the mouse position, or
     read input from a different source entirely. *)
    al_gui_mouse_x: AL_SIMPLE_FUNC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_mouse_x';
  (* Hook function, used by the GUI routines whenever they need to access the
     mouse state.  By default it just returns a copy of the @code(al_mouse_y)
     variable, but it could be used to offset or scale the mouse position, or
     read input from a different source entirely. *)
    al_gui_mouse_y: AL_SIMPLE_FUNC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_mouse_y';
  (* Hook function, used by the GUI routines whenever they need to access the
     mouse state.  By default it just returns a copy of the @code(al_mouse_z)
     variable, but it could be used to offset or scale the mouse position, or
     read input from a different source entirely. *)
    al_gui_mouse_z: AL_SIMPLE_FUNC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_mouse_z';
  (* Hook function, used by the GUI routines whenever they need to access the
     mouse state.  By default it just returns a copy of the @code(al_mouse_b)
     variable, but it could be used to offset or scale the mouse position, or
     read input from a different source entirely. *)
    al_gui_mouse_b: AL_SIMPLE_FUNC;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_mouse_b';


(* This function can be used to change the bitmap surface the GUI routines draw
   to.  This can be useful if you are using a double buffering or page flipping
   system.  Passing @nil will cause the default surface (@link(al_screen)) to
   be used again. @seealso(al_gui_get_screen) *)
  PROCEDURE al_gui_set_screen (bmp: AL_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_set_screen';
(* This function returns the current bitmap surface the GUI routines will use
   for drawing.  Note that this function will return @link(al_screen) if you
   have called @code(al_gui_set_screen @(@nil@)) previously, and will never
   return @nil. @seealso(al_gui_set_screen) *)
  FUNCTION al_gui_get_screen: AL_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gui_get_screen';

(* Helper function for use by the GUI routines.  Draws a text string onto the
   screen, interpreting the @code('&') character as an underbar for displaying
   keyboard shortcuts. @returns(The width of the output string in pixels.)
   @seealso(al_gui_strlen) *)
  FUNCTION al_gui_textout_ex (bmp: AL_BITMAPptr; s: STRING; x, y, color, bg: AL_INT; centre: BOOLEAN): AL_INT;
    INLINE;
(* Helper function for use by the GUI routines. @returns(The length of a string
   in pixels, ignoring @code('&') characters.) *)
  FUNCTION al_gui_strlen (s: STRING): AL_INT;
    INLINE;

(* Defines a dialog item.
   @param(dialog The dialog) @param(item Index to the dialog item)
   @seealso(AL_DIALOG) @seealso(al_do_dialog) *)
  PROCEDURE al_set_dialog_item (VAR dialog: ARRAY OF AL_DIALOG; index: INTEGER;
    proc: AL_DIALOG_PROC; x, y, w, h, fg, bg, key, flags, d1, d2: AL_INT;
    dp, dp2, dp3: AL_POINTER);

(* Moves an array of dialog objects to the specified screen position (specified
   as the top left corner of the dialog). @seealso(al_centre_dialog) *)
  PROCEDURE al_position_dialog (dialog: AL_DIALOGptr; x, y: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'position_dialog';
(* Moves an array of dialog objects so that it is centered in the screen.
   @seealso(al_position_dialog) *)
  PROCEDURE al_centre_dialog (dialog: AL_DIALOGptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'centre_dialog';
(* Sets the foreground and background colors of an array of dialog objects. *)
  PROCEDURE al_set_dialog_color (dialog: AL_DIALOGptr; fg, bg: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_dialog_color';

(* Searches the dialog for the object which has the input focus, returning an
   index or -1 if the focus is not set.  This is useful if you are calling
   @link(al_do_dialog) several times in a row and want to leave the focus in
   the same place it was when the dialog was last displayed, as you can call
   @longcode(#
al_do_dialog (dlg, al_find_dialog_focus (dlg));
   #) @seealso(al_init_dialog) @seealso(al_offer_focus) *)
  FUNCTION al_find_dialog_focus (dialog: AL_DIALOGptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'find_dialog_focus';
(* Offers the input focus to a particular object.  Normally the function sends
   the @code(AL_MSG_WANTFOCUS) message to query whether the object is willing
   to accept the focus.  However, passing any non-zero value as @code(force)
   argument instructs the function to authoritatively set the focus to the
   object. *)
  FUNCTION al_offer_focus (dialog: AL_DIALOGptr; obj: AL_INT; focus_obj: AL_INTptr; force: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'offer_focus';

(* Sends a message to an object and returns the answer it has generated.
   Remember that the first parameter is the dialog object (not a whole array)
   that you wish to send the message to.  For example, to make the second
   object in a dialog draw itself, you might write:@longcode(#
al_object_message (@(dialog[1]), AL_MSG_DRAW, 0);
   #)
   The function will take care of scaring and unscaring the mouse if the
   message is @code(AL_MSG_DRAW). @seealso(al_dialog_message)
   @seealso(al_scare_mouse) @seealso(al_unscare_mouse) *)
  FUNCTION al_object_message (dialog: AL_DIALOGptr; msg, c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'object_message';
(* Sends a message to all the objects in an array.  If any of the dialog
   procedures return values other than @code(AL_D_O_K), it returns the value
   and sets @code(obj) to the index of the object which produced it.
   @seealso(al_object_message) @seealso(al_broadcast_dialog_message) *)
  FUNCTION al_dialog_message (dialog: AL_DIALOGptr; msg, c: AL_INT; obj: AL_INTptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'dialog_message';
(* Broadcasts a message to all the objects in the active dialog.  If any of the
   dialog procedures return values other than @code(AL_D_O_K), it returns that
   value.
   @seealso(al_dialog_message) *)
  FUNCTION al_broadcast_dialog_message (msg, c: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'broadcast_dialog_message';

(* The basic dialog manager function.  This displays a dialog (an array of
   dialog objects, terminated by one with a @nil dialog procedure), and sets
   the input focus to the @code(focus_obj) (-1 if you don't want anything to
   have the focus).  It interprets user input and dispatches messages as they
   are required, until one of the dialog procedures tells it to close the
   dialog, at which point it returns the index of the object that caused it to
   exit, or until ESC is pressed, at which point it returns -1.
   @seealso(al_popup_dialog) @seealso(al_init_dialog)
   @seealso(al_find_dialog_focus) *)
  FUNCTION al_do_dialog (dialog: AL_DIALOGptr; focus_obj: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_dialog';
(* Like @link(al_do_dialog), but it stores the data on the screen before
   drawing the dialog and restores it when the dialog is closed.  The screen
   area to be stored is calculated from the dimensions of the first object in
   the dialog, so all the other objects should lie within this one.
   @seealso(al_init_dialog) *)
  FUNCTION al_popup_dialog (dialog: AL_DIALOGptr; focus_obj: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'popup_dialog';

(* This function provides lower level access to the same functionality as
   @code(al_do_dialog), but allows you to combine a dialog box with your own
   program control structures.  It initialises a dialog, returning a pointer to
   a player object that can be used with @link(al_update_dialog) and
   @link(al_shutdown_dialog).  With these functions, you could implement your
   own version of @link(al_do_dialog) with the lines:
   @longcode(#
VAR
  player: AL_DIALOG_PLAYERptr;
BEGIN
  player := al_init_dialog (dialog, focus_obj);
  WHILE al_update_dialog (player)
    ;
  RESULT := al_shutdown_dialog (player);
END;
   #)
   Note that you are responsible for showing and hiding the mouse cursor, which
   @code(al_do_dialog) would otherwise do for you, or saving and restoring the
   screen contents, as @link(al_popup_dialog) would do for you. *)
  FUNCTION al_init_dialog (dialog: AL_DIALOGptr; focus_obj: AL_INT): AL_DIALOG_PLAYERptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'init_dialog';
(* Updates the status of a dialog object returned by @link(al_init_dialog).
   @returns(@true if the dialog is still active, or @false if it has
     terminated.  Upon a return value of @false, it is up to you whether to
     call @code(al_shutdown_dialog) or to continue execution.)
   @seealso(al_do_dialog) *)
  FUNCTION al_update_dialog (player: AL_DIALOG_PLAYERptr): BOOLEAN;
    INLINE;
(* Destroys a dialog player object returned by @link(al_init_dialog), returning
   the object that caused it to exit (this is the same as the return value from
   @code(al_do_dialog)). *)
  FUNCTION al_shutdown_dialog (player: AL_DIALOG_PLAYERptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'shutdown_dialog';

(* Defines a menu item.
   @param(menu The menu) @param(item Index to the dialog item)
   @seealso(AL_MENU) *)
  PROCEDURE al_set_menu_item (VAR menu: ARRAY OF AL_MENU; index: INTEGER;
    txt: STRING; proc: AL_SIMPLE_FUNC; child: AL_MENUptr; flags: AL_INT;
    dp: AL_POINTER);

(* Displays and animates a popup menu at the specified screen coordinates
   (these will be adjusted if the menu does not entirely fit on the screen).
   @returns(The index of the menu item that was selected, or -1 if the menu was
     cancelled.  Note that the return value cannot indicate selection from
     child menus, so you will have to use the callback functions if you want
     multi-level menus.)
     @seealso(al_d_menu_proc) @seealso(al_init_menu)
     @seealso(al_gui_menu_draw_menu) *)
  FUNCTION al_do_menu (menu: AL_MENUptr; x, y: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'do_menu';

(* This function provides lower level access to the same functionality as
   @link(al_do_menu), but allows you to combine a popup menu with your own
   program control structures.  It initialises a menu, returning a pointer to a
   menu player object that can be used with @link(al_update_menu) and
   @link(al_shutdown_menu).  With these functions, you could implement your own
   version of @code(al_do_menu) with the lines:
   @longcode(#
VAR
  player: AL_MENU_PLAYERptr;
BEGIN
  player := al_init_menu (menu, x, y);
  WHILE al_pdate_menu (player)
    ;
  RESULT := al_shutdown_menu (player);
END;
   #) *)
  FUNCTION al_init_menu (menu: AL_MENUptr; x, y: AL_INT): AL_MENU_PLAYERptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'init_menu';
(* Updates the status of a menu object returned by @link(al_init_menu).
   @returns(@true if the menu is still active, or @false if it has terminated.
     Upon a return value of @false, it is up to you to call
     @link(al_shutdown_menu) or to continue execution.) *)
  FUNCTION al_update_menu (player: AL_MENU_PLAYERptr): BOOLEAN;
    INLINE;
(* Destroys a menu player object returned by @link(al_init_menu), returning the
   index of the menu item that was selected, or -1 if the menu was cancelled
   (this is the same as the return value from @link(al_do_menu)). *)
  FUNCTION al_shutdown_menu (player: AL_MENU_PLAYERptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'shutdown_menu';
(* Displays a popup alert box, containing three lines of text (s1-s3), and with
   either one or two buttons.  The text for these buttons is passed in
   @code(b1) and @code(b2) (@code(b2) may be empty), and the keyboard shortcuts
   in @code(c1) and @code(c2) as ASCII value.
   @returns(1 or 2 depending on which button was clicked.  If the alert is
     dismissed by pressing ESC when ESC is not one of the keyboard shortcuts,
     it treats it as a click on the second button @(this is consistent with the
     common @italic("Ok"), @italic("Cancel") alert@).)
   @seealso(al_alert3) *)
  FUNCTION al_alert (s1, s2, s3, b1, b2: STRING; c1, c2: AL_INT): AL_INT;
(* Like @link(al_alert), but with three buttons. @returns(1, 2, or 3) *)
  FUNCTION al_alert3 (s1, s2, s3, b1, b2, b3: STRING; c1, c2, c3: AL_INT): AL_INT;
(* Displays the Allegro file selector, with the message as caption.  The path
   parameter contains the initial filename to display (this can be used to set
   the starting directory, or to provide a default filename for a save-as
   operation).  The user selection is returned by altering the path string,
   whose maximum length is specified by the length parameter.  Note that it
   should have room for at least 80 characters (not bytes).  The list of files
   is filtered according to the file extensions in the ext parameter.  Passing
   empty string includes all files; @italic('PCX;BMP') includes only files with
   .PCX or .BMP extensions.  If you wish to control files by their attributes,
   one of the fields in the extension list can begin with a slash, followed by
   a set of attribute characters.  Any attribute written on its own, or with a
   @code('+') before it, indicates to include only files which have that
   attribute set.  Any attribute with a @code('-') before it indicates to leave
   out any files with that attribute.  The flag characters are @code('r')
   (read-only), @code('h') (hidden), @code('s') (system), @code('d')
   (directory) and @code('a') (archive).  For example, an extension string of
   @italic("PCX;BMP;/+r-h") will display only PCX or BMP files that are
   read-only and not hidden.  The directories are not affected in the same way
   as the other files by the extension string: the extensions are never taken
   into account for them and the other attributes are taken into account only
   when @code('d') is mentioned in the string; in other words, all directories
   are included when @code('d') is not mentioned in the string.  The file
   selector is stretched to the width and height specified in the @code(w) and
   @code(h) parameters, and to the size of the standard Allegro font.  If
   either the width or height argument is set to zero, it is stretched to the
   corresponding screen dimension. @returns(@false if it was closed with the
     @code(Cancel) button or @true if it was @code(OK)'d.) *)
  FUNCTION al_file_select_ex (message: STRING; VAR path: STRING; ext: STRING; length, w, h: AL_INT): BOOLEAN;

(* Displays the Allegro graphics mode selection dialog, which allows the user
   to select a screen mode and graphics card.  Stores the selection in the
   three variables.  The initial values of @code(card), @code(w), @code(h) are
   not used. @returns(@false if it was closed with the @code(Cancel) button or
   @true if it was @code(OK)'d.)
   @seealso(al_gfx_mode_select_ex) @seealso(al_gfx_mode_select_filter) *)
  FUNCTION al_gfx_mode_select (VAR card, w, h: AL_INT): BOOLEAN;
    INLINE;
(* Extended version of the graphics mode selection dialog, which allows the
   user to select the color depth as well as the resolution and hardware
   driver.  This version of the function reads the initial values from the
   parameters when it activates so you can specify the default values.  In
   fact, you should be sure not to pass in uninitialised values.
   @seealso(al_gfx_mode_select) @seealso(al_gfx_mode_select_filter) *)
  FUNCTION al_gfx_mode_select_ex (VAR card, w, h, color_depth: AL_INT): BOOLEAN;
    INLINE;

TYPE
(* Callback to be used by @code(al_gfx_mode_select_filter). *)
  AL_GFX_SELECT_FN = FUNCTION(c, w, h, d: AL_INT): AL_INT; CDECL;
(* Even more extended version of the graphics mode selection dialog, which
   allows the programmer to customize the contents of the dialog and the user
   to select the color depth as well as the resolution and hardware driver.

   This version of the function reads the initial values from the parameters
   when it activates so you can specify the default values.  In fact, you
   should be sure not to pass in uninitialised values.
   @param(filter will be passed (card, w, h, color_depth) quadruplets and must
   return 0 to let the specified quadruplet be added to the list of displayed
   modes.)
   @seealso(al_gfx_mode_select) @seealso(al_gfx_mode_select_ex) *)
  FUNCTION al_gfx_mode_select_filter (VAR card, w, h, color_depth: AL_INT; filter: AL_GFX_SELECT_FN): BOOLEAN;
    INLINE;



IMPLEMENTATION

  USES
    sysutils;



(* Links to Allegro's functions. *)
  FUNCTION gui_textout_ex (bmp: AL_BITMAPptr; CONST s: AL_STRptr; x, y, color, bg, centre: AL_INT): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION gui_strlen (CONST s: AL_STRptr): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION update_dialog (player: AL_DIALOG_PLAYERptr): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION update_menu (player: AL_MENU_PLAYERptr): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION alert (CONST s1, s2, s3, b1, b2: AL_STRptr; c1, c2: AL_INT): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION alert3 (CONST s1, s2, s3, b1, b2, b3: AL_STRptr; c1, c2, c3: AL_INT): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION file_select_ex (CONST message: AL_STRptr; path: AL_STRptr; CONST ext: AL_STRptr; size, w, h: AL_INT): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION gfx_mode_select (card, w, h: AL_INTptr): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION gfx_mode_select_ex (card, w, h, color_depth: AL_INTptr): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION gfx_mode_select_filter (card, w, h, color_depth: AL_INTptr; filter: AL_POINTER): AL_INT; CDECL;
    EXTERNAL  ALLEGRO_SHARED_LIBRARY_NAME;



  FUNCTION al_gui_textout_ex (bmp: AL_BITMAPptr; s: STRING; x, y, color, bg: AL_INT; centre: BOOLEAN): AL_INT;
  VAR
    Ctr: AL_INT;
  BEGIN
    IF centre THEN
      Ctr := NOT 0
    ELSE
      Ctr := 0;
    al_gui_textout_ex := gui_textout_ex (bmp, AL_STRptr (s), x, y, color, bg, Ctr);
  END;



  FUNCTION al_gui_strlen (s: STRING): AL_INT;
  BEGIN
    al_gui_strlen := gui_strlen (AL_STRptr (s));
  END;



  PROCEDURE al_set_dialog_item (VAR dialog: ARRAY OF AL_DIALOG; index: INTEGER;
    proc: AL_DIALOG_PROC; x, y, w, h, fg, bg, key, flags, d1, d2: AL_INT;
    dp, dp2, dp3: POINTER);
  BEGIN
    dialog[index].proc := proc;
    dialog[index].x   := x;
    dialog[index].y   := y;
    dialog[index].w   := w;
    dialog[index].h   := h;
    dialog[index].fg  := fg;
    dialog[index].bg  := bg;
    dialog[index].key := key;
    dialog[index].flags := flags;
    dialog[index].d1  := d1;
    dialog[index].d2  := d2;
    dialog[index].dp  := dp;
    dialog[index].dp2 := dp2;
    dialog[index].dp3 := dp3;
  END;



  FUNCTION al_update_dialog (player: AL_DIALOG_PLAYERptr): BOOLEAN;
  BEGIN
    al_update_dialog := update_dialog (player) <> 0;
  END;



  PROCEDURE al_set_menu_item (VAR menu: ARRAY OF AL_MENU; index: INTEGER;
    txt: STRING; proc: AL_SIMPLE_FUNC; child: AL_MENUptr; flags: AL_INT;
    dp: POINTER);
  BEGIN
    menu[index].txt   := AL_STRptr (txt);
    menu[index].proc  := proc;
    menu[index].child := child;
    menu[index].flags := flags;
    menu[index].dp    := dp;
  END;



  FUNCTION al_update_menu (player: AL_MENU_PLAYERptr): BOOLEAN;
  BEGIN
    al_update_menu := update_menu (player) <> 0;
  END;



(* Helper funtion to get labels. *)
  FUNCTION GetPChar (LabelText: STRING): AL_STRptr; INLINE;
  BEGIN
    IF LabelText <> '' THEN
      GetPChar := AL_STRptr (LabelText)
    ELSE
      GetPChar := NIL;
  END;

  FUNCTION al_alert (s1, s2, s3, b1, b2: STRING; c1, c2: AL_INT): AL_INT;
  BEGIN
    al_alert := alert (GetPChar (s1), GetPChar (s2), GetPChar (s3),
		       GetPChar (b1), GetPChar (b2), c1, c2);
  END;



  FUNCTION al_alert3 (s1, s2, s3, b1, b2, b3: STRING; c1, c2, c3: AL_INT): AL_INT;
  BEGIN
    al_alert3 := alert3 (GetPChar (s1), GetPChar (s2), GetPChar (s3),
			 GetPChar (b1), GetPChar (b2), GetPChar (b3), c1, c2, c3);
  END;


  FUNCTION al_file_select_ex (message: STRING; VAR path: STRING; ext: STRING; length, w, h: AL_INT): BOOLEAN;
  VAR
    Buffer: AL_STRptr;
  BEGIN
    Buffer := StrAlloc (length + 1);
    StrPCopy (Buffer, path);
    al_file_select_ex := file_select_ex (AL_STRptr (message), Buffer, AL_STRptr (ext), length, w, h) <> 0;
    Path := StrPas (Buffer);
    StrDispose (Buffer);
  END;



  FUNCTION al_gfx_mode_select (VAR card, w, h: AL_INT): BOOLEAN;
  BEGIN
    al_gfx_mode_select := gfx_mode_select (@card, @w, @h) <> 0;
  END;



  FUNCTION al_gfx_mode_select_ex (VAR card, w, h, color_depth: AL_INT): BOOLEAN;
  BEGIN
    al_gfx_mode_select_ex := gfx_mode_select_ex (@card, @w, @h, @color_depth) <> 0;
  END;



  FUNCTION al_gfx_mode_select_filter (VAR card, w, h, color_depth: AL_INT; filter: AL_GFX_SELECT_FN): BOOLEAN;
  BEGIN
    al_gfx_mode_select_filter := gfx_mode_select_filter (@card, @w, @h, @color_depth, filter) <> 0;
  END;

END.
