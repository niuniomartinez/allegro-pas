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
       FUNCTION foo (msg: LONGINT; d: AL_DIALOGptr; c: LONGINT): LONGINT;
#)
  It will be passed a flag (msg) indicating what action it should perform, a
  pointer to the object concerned (d), and if msg is @code(AL_MSG_CHAR) or
  @code(AL_MSG_XCHAR), the key that was pressed (c).  Note that d is a pointer
  to a specific object, and not to the entire dialog.

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
      closing the dialog if @code(ESC) is pressed.)
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
  For instance, you could make an object which calls @link(al_d_button_proc) to
  draw itself, but handles the click message in a different way, or an object
  which calls @code(al_d_button_proc) for everything except drawing itself, so
  it would behave like a normal button but could look completely different. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$MODE FPC}
 {$PACKRECORDS C}
 {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

USES
  albase, allegro;



TYPE
(* Pointer to @link(AL_DIALOG). *)
  AL_DIALOGptr = ^AL_DIALOG;

(* Dialog function object. *)
  AL_DIALOG_PROC = FUNCTION (msg: LONGINT; d: AL_DIALOGptr; c: LONGINT): LONGINT; CDECL;

(* This is the structure which contains a GUI object. *)
  AL_DIALOG = RECORD
    proc: AL_DIALOG_PROC; (*<Dialog procedure (message handler) *)
    x, y, w, h: LONGINT;  (*<Position and size of the object *)
    fg, bg: LONGINT;      (*<Foreground and background colors *)
    key: LONGINT;         (*<Keyboard shortcut (ASCII code) *)
    flags: LONGINT;       (*<Flags about the object state *)
    d1, d2: LONGINT;      (*<Any data the object might require *)
    dp, dp2, dp3: POINTER;(*<Pointers to more object data *)
  END;



IMPLEMENTATION

END.
