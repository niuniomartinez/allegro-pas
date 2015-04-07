UNIT alGUI;
(*<Defines a simple Object Oriented GUI system for Allegro.pas.

  It's much like the Allegro's GUI system but using Object Pascal classes.
 *)

INTERFACE

  USES
    Allegro, contnrs, Classes;

  TYPE
  (* @exclude *)
    TalGUI_Dialog = CLASS;



  (* Alignment values. *)
    TalGUI_Alignment = (
      agaLeft,   (*<Aligned to the left side. *)
      agaCenter, (*<Centered. *)
      agaRight   (*<Aligned to the right. *)
    );



  (* Direction values. *)
    TalGUI_Direction = (
      agdHorizontal, (*<Horizontal direction. *)
      agdVertical    (*<Vertical direction. *)
    );



  (* Defines a base class for GUI styles.

    It introduces methods and properties that will be used by controls to
    render themselves.  The @code(Style) object used is stored by the
    @code(Dialog) object.
    @seealso(TalGUI_Dialog.Style) @seealso(TalGUI_Control.Dialog) *)
    TalGUI_CustomStyle = CLASS (TObject)
    PRIVATE
      fTxtColor, fDisabledColor, fSelectedTxtColor, fBgColor, fSelectedBgColor,
      fBgTextBoxColor, fLightColor, fDarkColor, fBorderColor: LONGINT;
      fTxtFont: AL_FONTptr;
    PUBLIC
    (* Constructor.  Sets default colors and text font. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Sets default colors.  Call this after set graphics mode and define the
      color palette.

      Since it is an abstract method, @code(TalGUI_CustomStyle) itself does
      not implement @code(SetDefaultColors).  Descendent classes such as
      @link(TalGUI_DefaultStyle) implement this method. *)
      PROCEDURE SetDefaultColors; VIRTUAL; ABSTRACT;
    (* Draws a bevel, this is, a border, rectangle or frame.

      Since it is an abstract method, @code(TalGUI_CustomStyle) itself does
      not implement @code(DrawBevel).  Descendent classes such as
      @link(TalGUI_DefaultStyle) implement this method.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of box.)  @param(y1 Top limit of box.)
      @param(x2 Right limit of box.) @param(y2 Bottom limit of box.)
      @param(Raised @true, to draw as raised, if @false to draw as depressed.)
      @seealso(DrawBox)
    *)
      PROCEDURE DrawBevel (Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER;
	Raised: BOOLEAN); VIRTUAL; ABSTRACT;
    (* Draws a box.  Useful for buttons and pannels.

      Since it is an abstract method, @code(TalGUI_CustomStyle) itself does
      not implement @code(DrawBevel).  Descendent classes such as
      @link(TalGUI_DefaultStyle) implement this method.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of box.)  @param(y1 Top limit of box.)
      @param(x2 Right limit of box.) @param(y2 Bottom limit of box.)
      @param(BackColor Background color.  If negative, then it will not draw
        background.)
      @param(Raised @true, to draw the border as raised, if @false to draw it
        as depressed.)
      @seealso(DrawBevel)
     *)
      PROCEDURE DrawBox (
        Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor: INTEGER;
        Raised: BOOLEAN); VIRTUAL; ABSTRACT;
    (* Draws a dialog frame, wich includes a title.

      Since it is an abstract method, @code(TalGUI_CustomStyle) itself does
      not implement @code(DrawDialogFrame).  Descendent classes such as
      @link(TalGUI_DefaultStyle) implement this method.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of window.)  @param(y1 Top limit of window.)
      @param(x2 Right limit of window.) @param(y2 Bottom limit of window.)
      @param(BackColor Background color.  If negative, then it will not draw
        background.)
      @param(Title Text to draw on title.)
      @param(TitleCentered @true, to draw title centered.)
     *)
      PROCEDURE DrawDialogFrame (
	Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor: INTEGER;
	Title: STRING; TitleCentered: BOOLEAN
      ); VIRTUAL; ABSTRACT;
    (* Draws a "trench". *)
      PROCEDURE DrawTrench (Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER);
	VIRTUAL;
    (* Draws a dotted rectangle to show that the control has acquired focus. *)
      PROCEDURE DrawFocusRect (Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER);
	VIRTUAL;
    (* Draws a text. @seealso(TextFont) @seealso(DrawDisabledText) *)
      PROCEDURE DrawText (Bmp: AL_BITMAPptr; CONST Msg: STRING;
	X, Y, Color: LONGINT; Centered: BOOLEAN); VIRTUAL;
    (* Draws a text as disabled.  By default it just calls @code(DrawText) with
       the @code(DisabledTextColor), but descendent classes may draw the text
       with special properties or different text fonts.
      @seealso(TextFont) @seealso(DrawText) *)
      PROCEDURE DrawDisabledText (Bmp: AL_BITMAPptr; CONST Msg: STRING;
	X, Y: LONGINT; Centered: BOOLEAN); VIRTUAL;

    (* Draws a checkbox.

      Since it is an abstract method, @code(TalGUI_CustomStyle) itself does
      not implement @code(DrawCheckbox).  Descendent classes such as
      @link(TalGUI_DefaultStyle) implement this method.
      @param(Bmp Where to draw it.)
      @param(x Left limit of checkbox.) @param(y Top limit of checkbox.)
      @param(Checked Whether to draw it checked or not.)
      @param(Focused Whether to draw it focused or not.)
      @return(Width of the box.  Used to draw label.)
     *)
      FUNCTION DrawCheckbox (
	Bmp: AL_BitmapPtr; x, y: INTEGER; Checked: BOOLEAN
      ): INTEGER; VIRTUAL; ABSTRACT;

    (* Default text color. *)
      PROPERTY TextColor: LONGINT READ fTxtColor WRITE fTxtColor;
    (* Disabled text color. *)
      PROPERTY DisabledTextColor: LONGINT READ fDisabledColor WRITE fDisabledColor;
    (* Selected text color. *)
      PROPERTY SelectedTextColor: LONGINT READ fSelectedTxtColor WRITE fSelectedTxtColor;
    (* Default background color. *)
      PROPERTY BackgroundColor: LONGINT READ fBgColor WRITE fBgColor;
    (* Selected background color. *)
      PROPERTY SelectedBackgroundColor: LONGINT READ fSelectedBgColor WRITE fSelectedBgColor;
    (* Background color for text boxes (i.e. editors, item lists, etc. *)
      PROPERTY BackgroundTextBoxColor: LONGINT READ fBgTextBoxColor WRITE fBgTextBoxColor;
    (* Color used in the light side of 3D drawing.  For example, in 3D buttons. *)
      PROPERTY LightColor: LONGINT READ fLightColor WRITE fLightColor;
    (* Color used in the dark side of 3D drawing.  For example, in 3D buttons. *)
      PROPERTY DarkColor: LONGINT READ fDarkColor WRITE fDarkColor;
    (* Color used in borders. *)
      PROPERTY BorderColor: LONGINT READ fBorderColor WRITE fBorderColor;
    (* Default font. *)
      PROPERTY TextFont: AL_FONTptr READ fTxtFont WRITE fTxtFont;
    END;



  (* Base class for dialog controls. *)
    TalGUI_Control = CLASS (TObject)
    PRIVATE
      fOwner: TalGUI_Dialog;
      fBgColor, fColor, fBdColor: LONGINT;
      fX, fY, fW, fH, fKeyShortCut, fTag: INTEGER;
      fHasFocus, fHasMouse, fEnabled, fRedraw: BOOLEAN;
    PROTECTED
    (* Sets X position. *)
      PROCEDURE SetX (CONST aX: INTEGER); VIRTUAL;
    (* Sets Y position. *)
      PROCEDURE SetY (CONST aY: INTEGER); VIRTUAL;
    (* Sets control width. *)
      PROCEDURE SetWidth (CONST aWidth: INTEGER); VIRTUAL;
    (* Sets control height. *)
      PROCEDURE SetHeight (CONST aHeight: INTEGER); VIRTUAL;
    (* Sets the @code(Enabled) property.  Overriden implementation should call
      this to actually set the @code(Enabled) value. *)
      PROCEDURE SetEnabled (CONST aEnabled: BOOLEAN); VIRTUAL;

    (* Informs the object that a mouse button has been clicked while the mouse
      was on top of the object.  Typically an object will perform its own
      mouse tracking as long as the button is held down, and only return from
      this message handler when it is released.
      @return(@true if message was handled, or @false if control isn't
       interested on it.) *)
      FUNCTION MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN; VIRTUAL;
    (* Sent when the keyboard shortcut for the object is pressed, or if enter,
      space, or a joystick button is pressed while it has the input focus. *)
      PROCEDURE MsgKey; VIRTUAL;
    (* Sent when a key is pressed and the object has the input focus.
      @return(@true if the object deals with the keypress, otherwise it
      should return @false to allow the default keyboard interface to operate.)
      @param(aKey Key pressed with an @code(al_readkey) format character code
      @(ASCII value in the low byte, scancode in the high byte@).) *)
      FUNCTION MsgKeyChar (aKey: INTEGER): BOOLEAN; VIRTUAL;
    (* Sent when an object gains the input focus.  This message will always be
      followed by a call to @link(RedrawMe), to let objects display themselves
      differently when they have the input focus.
      @seealso(MsgLostFocus) *)
      PROCEDURE MsgGotFocus; VIRTUAL;
    (* Sent when an object loses the input focus.  This message will always be
      followed by a call to @link(RedrawMe), to let objects display themselves
      differently when they have the input focus.
      @return(@false to prevent the object from losing the focus when the mouse
	moves off it onto the screen background or some inert object, so it will
	only lose the input focus when some other object is ready to take over
	the focus @(this trick is used by the @code(TalGUIEdit) object@),
	@true otherwise.) @seealso(MsgGotFocus) *)
      FUNCTION MsgLostFocus: BOOLEAN; VIRTUAL;
    (* Sent when mouse moves on top of an object.  Unlike the @italic(got
      focus/lost focus) messages, this message will not be be followed by a
      call to any @code(Draw) method, so if the object is isplayed differently
      when the mouse is on top of it, it should call @link(RedrawMe).
      @seealso(MsgLostMouse) *)
      PROCEDURE MsgGotMouse; VIRTUAL;
    (* Sent when mouse moves away of an object.  Unlike the @italic(focus)
      message, this message will not be be followed by a call to @link(Draw),
      so if the object is isplayed differently when the mouse is on top of it,
      it should call @link(RedrawMe). @seealso(MsgGotMouse) *)
      PROCEDURE MsgLostMouse; VIRTUAL;
    (* Sent whenever the dialog manager has nothing better to do. *)
      PROCEDURE MsgIddle; VIRTUAL;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Initializes the control.  It's called by the @link(TalGUI_Dialog) object
      just before it displays the dialog. *)
      PROCEDURE Initialize; VIRTUAL;
    (* Finalizes the control  It's called when closing the dialog.  Allows to
      perform whatever cleanup operations it requires. *)
      PROCEDURE Finalize; VIRTUAL;
    (* Sets colors to default.  Note that control must be part of a dialog or
       this procedure will fail!
       @seealso(BorderColor) @seealso(BackgroundColor) @seealso(Color) *)
      PROCEDURE SetDefaultColors; VIRTUAL;
    (* Tell the owner dialog that control should be redrawn. *)
      PROCEDURE RedrawMe; VIRTUAL;
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); VIRTUAL;

    (* Queries whether an object is willing to accept the input focus.
      @return(@true if it does, or @false if it isn't interested in getting user
	input.  By default, it returns @false.)
      @seealso(HasFocus) @seealso(MsgGotFocus) @seealso(MsgLostFocus) *)
      FUNCTION WantFocus: BOOLEAN; VIRTUAL;

    (* Tests if given point is inside the control. *)
      FUNCTION Inside (CONST aX, aY: INTEGER): BOOLEAN; VIRTUAL;

    (* Reference to the owner dialog. *)
      PROPERTY Dialog: TalGUI_Dialog READ fOwner;
    (* Color used by border (if any).
      @seealso(BackgroundColor) @seealso(Color) @seealso(SetDefaultColors) *)
      PROPERTY BorderColor: LONGINT READ fBdColor WRITE fBdColor;
    (* Color used by background (if any).
      @seealso(BorderColor) @seealso(Color) @seealso(SetDefaultColors) *)
      PROPERTY BackgroundColor: LONGINT READ fBgColor WRITE fBgColor;
    (* Color.
      @seealso(BorderColor) @seealso(BackgroundColor) @seealso(SetDefaultColors) *)
      PROPERTY Color: LONGINT READ fColor WRITE fColor;
    (* Left position of the component. *)
      PROPERTY X: INTEGER READ fX WRITE fX;
    (* Top position of the component. *)
      PROPERTY Y: INTEGER READ fY WRITE fY;
    (* Width of the component in pixels. *)
      PROPERTY Width: INTEGER READ fW WRITE SetWidth;
    (* Height of the component in pixels. *)
      PROPERTY Height: INTEGER READ fH WRITE SetHeight;
    (* ASCII keyboard shortcut. *)
      PROPERTY KeyShortcut: INTEGER READ fKeyShortCut WRITE fKeyShortCut;
    (* Extra value that can be used to identify the component or to store a
      value that may be useful somewhere. *)
      PROPERTY Tag: INTEGER READ fTag WRITE fTag;
    (* @true if control has input focus, @false otherwise.
      @seealso(MsgGotFocus) @seealso(MsgLostFocus) @seealso(WantFocus) *)
      PROPERTY HasFocus: BOOLEAN READ fHasFocus;
    (* @true if mouse is inside the control, @false otherwise. *)
      PROPERTY HasMouse: BOOLEAN READ fHasMouse;
    (* @true if control is active, @false otherwise.

      Inactive controls can't get input focus and can't be selected.

      By default it's @false *)
      PROPERTY Enabled: BOOLEAN READ fEnabled WRITE SetEnabled;
    END;



  (* An event control handler. *)
    TalGUI_ControlEvent = PROCEDURE (Sender: TalGUI_Control) OF OBJECT;



  (* Manages a list of dialog controls. *)
    TalGUI_ControlList = CLASS (TObject)
    PRIVATE
      fControlList: TFPObjectList;
      fOwner: TalGUI_Dialog;

      FUNCTION GetCount: INTEGER; INLINE;
      FUNCTION GetControl (CONST Ndx: INTEGER): TalGUI_Control; INLINE;
      PROCEDURE SetControl (CONST Ndx: INTEGER; aControl: TalGUI_Control);
	INLINE;
    PUBLIC
    (* Constructor.
      @param(DlgOwner Reference to the dialog that contains the list.) *)
      CONSTRUCTOR Create (DlgOwner: TalGUI_Dialog); VIRTUAL;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Removes all controls from the list. @seealso(Controls) *)
      PROCEDURE Clear; INLINE;
    (* Adds given control to the list.
      @return(Index to of the control.) *)
      FUNCTION Add (aControl: TalGUI_Control): INTEGER; INLINE;
    (* Sets default colors to all contained controls. *)
      PROCEDURE SetDefaultColors;
    (* Changes position of all contained controls.
      @param(DisplacementX How many pixels to move horizontally.)
      @param(DisplacementY How many pixels to move vertically.) *)
      PROCEDURE MoveControls (CONST DisplacementX, DisplacementY: INTEGER);
    (* Draw controls in the given bitmap. *)
      PROCEDURE Draw (BmpOut: AL_BITMAPptr);
    (* Helper method to know if control wants focus.

       This method doesn't just call the @code(WantFocus) method of the control,
       but it also checks other important states. *)
      FUNCTION WantsFocus (CONST Ndx: INTEGER): BOOLEAN; INLINE;

    (* How many controls are in the list.  This may include NULL controls. *)
      PROPERTY Count: INTEGER READ GetCount;
    (* Indexed access to the controls.  Reading it will return the reference to
       the control at position @code(Ndx).  Writting it will set the control at
       position @code(Ndx) but will not destroy the control yet existed.

       Since the array is zero-based, @code(Ndx) should be an integer between 0
       and @code(Count-1).

       It is the default property.
       @seealso(Count) @seealso(Clear) @seealso(Add)
    *)
      PROPERTY Controls[Ndx: INTEGER]: TalGUI_Control
        READ GetControl WRITE SetControl; DEFAULT;
    END;



  (* Defines and manages the dialog. *)
    TalGUI_Dialog = CLASS (TObject)
    PRIVATE
      fBitmap: AL_BITMAPptr;
      fStyle: TalGUI_CustomStyle;
      fControlList : TalGUI_ControlList;
      fFocusIndex: INTEGER;
      fRedrawAll, fClosed: BOOLEAN;

      PROCEDURE SetFocus (CONST NewFocus: INTEGER);
    PROTECTED
    (* Initializes the dialog.

      It is a low level method that may be needed if you override the
      @code(Run) method.  For example:
@longcode(#
  Dialog.Initialize (1);
  REPEAT Dialog.Draw UNTIL Dialog.Update;
  Control := Dialog.Shutdown;
#)
      @param(FocusCtrl Index to the control that has focus at the beginning.)
      @seealso(Update) @seealso(Shutdown) @seealso(Run) *)
      PROCEDURE Initialize (FocusCtrl: INTEGER); VIRTUAL;
    (* Updates the dialog.  You should call this before @code(Initialize).
      @return(@true if the dialog is terminated, or @false if it is still
	active. Upon a return value of @true, it is up to you whether to
	call @code(Shutdown) or to continue execution.)
      @seealso(Initialize) @seealso(Shutdown) @seealso(Run) *)
      FUNCTION Update: BOOLEAN; VIRTUAL;
    (* Finalizes the dialog execution and returns the control that caused it to
      exit or @code(-1) if @code(Esc) key was pressed.
      @seealso(Initialize) @seealso(Update) @seealso(Run) *)
      FUNCTION Shutdown: INTEGER; VIRTUAL;
    PUBLIC
    (* Constructor.

      It sets bitmap to screen and assigns default style. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;

    (* Draw the dialog.  You don't need to do this as @code(Run) will do it for
       you. @seealso(Update) *)
      PROCEDURE Draw;
    (* Executes the dialog loop.

       It sets the input focus to the @code(FocusCtrl) control,  Then it
       interprets user input and dispatches messages as they are required,
       until one of the dialog procedures tells it to close the dialog, at
       which point it returns the index of the object that caused it to exit,
       or until ESC is pressed, at which point it returns -1.
       @param(FocusCtrl Index to the control that has focus at the beginning.)
       @return(Index to the control that has closed the dialog, or @code(-1)
        if ESC key was pressed.)
     *)
       FUNCTION Run (CONST FocusCtrl: INTEGER): INTEGER; VIRTUAL;

    (* Tells the dialog that it should redraw all controls. *)
       PROCEDURE RedrawAll; INLINE;
    (* Closes the dialog. *)
       PROCEDURE Close; INLINE;

    (* Bitmap where the dialog will be drawn.  By default it's the
      @code(al_screen). *)
      PROPERTY Bmp: AL_BITMAPptr READ fBitmap WRITE fBitmap;
    (* Dialog style.  Note that assign it will not destroy the previous style.
     *)
      PROPERTY Style: TalGUI_CustomStyle READ fStyle WRITE fStyle;
    (* Access to the control list of the dialog. *)
      PROPERTY Controls: TalGUI_ControlList READ fControlList;
    (* Index of current focus. *)
      PROPERTY Focus: INTEGER READ fFocusIndex WRITE SetFocus;
    END;




  (* An invisible helper object that yields time slices for the scheduler (if
     the system supports it) when the GUI has nothing to do but waiting for
     user actions.  You should put one instance of this object, or a descendent
     such as @code(TalGUI_ClearScreen), in each dialog because it may be needed
     on systems with an unusual scheduling algorithm (for instance QNX) in
     order to make the GUI fully responsive.
     @seealso(TalGUI_ClearScreen) *)
    TalGUI_Yield = CLASS (TalGUI_Control)
    PROTECTED
    (* Sent whenever the dialog manager has nothing better to do. *)
      PROCEDURE MsgIddle; OVERRIDE;
    END;



  (* This just clears the screen when it is drawn.  Useful as the first object
     in a dialog.

     Since it's a @code(TalGUI_Yield) descendent, such object isn't needed if
     @code(TalGUI_ClearScreen) is used. *)
    TalGUI_ClearScreen = CLASS (TalGUI_Yield)
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;
    END;



  (* Draws a box or panel.

    If BackgroundColor is less than zero, it doesn't draw the background, wich
    is faster. *)
    TalGUI_Box = CLASS (TalGUI_Control)
    PRIVATE
      fRaised: BOOLEAN;

      PROCEDURE SetRaised (CONST aRaised: BOOLEAN); INLINE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates the box. *)
      CONSTRUCTOR CreateBox
	(CONST aX, aY, aW, aH: INTEGER; CONST aRaised: BOOLEAN=TRUE);
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;

    (* @true, to draw the border as raised, @false to draw it as depressed. *)
      PROPERTY Raised: BOOLEAN READ fRaised WRITE SetRaised;
    END;



  (* Draws text onto the dialog. *)
    TalGUI_Label = CLASS (TalGUI_Control)
    PRIVATE
      fCaption: STRING;
      fAlignment: TalGUI_Alignment;

      PROCEDURE SetAlignment (CONST aAlign: TalGUI_Alignment); INLINE;
      PROCEDURE SetCaption   (CONST aCaption: STRING); INLINE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates the label.  If width and height are not set, then they're
      calculated from caption size. *)
      CONSTRUCTOR CreateLabel
	(CONST aCaption: STRING; CONST aX, aY: INTEGER;
	 CONST aW: INTEGER=-1; CONST aH: INTEGER=-1;
	 CONST aAlign: TalGUI_Alignment = agaLeft);
    (* Initializes the control. *)
      PROCEDURE Initialize; OVERRIDE;
    (* Sets colors to default. *)
      PROCEDURE SetDefaultColors; OVERRIDE;
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;
    (* Text alignment. *)
      PROPERTY Alignment: TalGUI_Alignment READ fAlignment WRITE SetAlignment;
    (* Label text. *)
      PROPERTY Caption: STRING READ fCaption WRITE SetCaption;
    END;



  (* Base class for various types of buttons.

    Introduces default behavior and an event handler (@link(onCLick)). *)
    TalGUI_CustomButton = CLASS (TalGUI_Control)
    PRIVATE
      fonClick: TalGUI_ControlEvent;
    PROTECTED
    (* Tells if button is pressed or released.  Useful for drawing method. *)
      fPressed: BOOLEAN;

    (* Sent when the keyboard shortcut for the object is pressed, or if enter,
      space, or a joystick button is pressed while it has the input focus. *)
      PROCEDURE MsgKey; OVERRIDE;
    (* User pressed in button.  Does default behavior, setting the
      @code(fPressed) property and calling the @code(Draw) method. *)
      FUNCTION MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Initializes the control. *)
      PROCEDURE Initialize; OVERRIDE;
    (* Queries whether an object is willing to accept the input focus. *)
      FUNCTION WantFocus: BOOLEAN; OVERRIDE;

    (* Event called when control is clicked by mouse. *)
      PROPERTY onCLick: TalGUI_ControlEvent READ fonClick WRITE fonClick;
    END;



  (* Base class check boxes and radio buttons.

    Introduces default behavior and an event handler (@link(onChange)). *)
    TalGUI_CustomCheckBox = CLASS (TalGUI_Control)
    PRIVATE
      fChecked: BOOLEAN;
      fOnChange: TalGUI_ControlEvent;
      fCaption: STRING;

      PROCEDURE SetCaption (CONST aCaption: STRING); INLINE;
    PROTECTED
    (* Sets the @link(Checked) property value. *)
      PROCEDURE SetChecked (CONST aValue: BOOLEAN); VIRTUAL;

    (* Sent when the keyboard shortcut for the object is pressed, or if enter,
      space, or a joystick button is pressed while it has the input focus. *)
      PROCEDURE MsgKey; OVERRIDE;
    (* User pressed in checkbox.  Does default behavior, setting the
      @code(Checked) property and calling the @code(Draw) method. *)
      FUNCTION MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Queries whether an object is willing to accept the input focus. *)
      FUNCTION WantFocus: BOOLEAN; OVERRIDE;

    (* Caption of checkbox. *)
      PROPERTY Caption: STRING READ fCaption WRITE SetCaption;
    (* Tells if control is checked or not. *)
      PROPERTY Checked: BOOLEAN READ fChecked WRITE SetChecked;

    (* Event called when control is changed. *)
      PROPERTY onChange: TalGUI_ControlEvent READ fOnChange WRITE fOnChange;
    END;



  (* Base class for various slider controls (such as scroll bars and sliders).

    Introduces default properties and event handlers. *)
    TalGUI_CustomSlider = CLASS (TalGUI_Control)
    PRIVATE
      fDirection: TalGUI_Direction;
      fMin, fMax, fPos, fPage: INTEGER;
      fOnChange: TalGUI_ControlEvent;

      PROCEDURE SetDirection (CONST aDir: TalGUI_Direction);
      PROCEDURE SetPos (aPos: INTEGER);
    PROTECTED
    (* Sets minimun value. *)
      PROCEDURE SetMin (CONST aMin: INTEGER); VIRTUAL;
    (* Sets maximun value. *)
      PROCEDURE SetMax (CONST aMax: INTEGER); VIRTUAL;
    (* Sets page size. *)
      PROCEDURE SetPage (CONST aSize: INTEGER); VIRTUAL;
    (* Manages key input.

      It manages cursor keys, and Home, PgUp, PgDown and End. *)
      FUNCTION MsgKeyChar (aKey: INTEGER): BOOLEAN; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Initializes control. *)
      PROCEDURE Initialize; OVERRIDE;
    (* It wants focus. *)
      FUNCTION WantFocus: BOOLEAN; OVERRIDE;

    (* Control direction. *)
      PROPERTY Direction: TalGUI_Direction READ fDirection WRITE SetDirection;
    (* The minimun value, for the top or leftmost position.
      @seealso(Max) @seealso(Page) @seealso(Position) *)
      PROPERTY Min: INTEGER READ fMin WRITE SetMin;
    (* The maximun value, for the bottom or rightmost position.
      @seealso(Min) @seealso(Page) @seealso(Position) *)
      PROPERTY Max: INTEGER READ fMax WRITE SetMax;
    (* Page size.
      @seealso(Min) @seealso(Max) @seealso(Position) *)
      PROPERTY Page: INTEGER READ fPage WRITE SetPage;
    (* The position value of the slider.
      @seealso(Min) @seealso(Max) @seealso(Page) *)
      PROPERTY Position: INTEGER READ fPos WRITE SetPos;

    (* Event handler for any change on @link(Position). *)
      PROPERTY OnChange: TalGUI_ControlEvent READ fOnChange WRITE fOnChange;
    END;



  (* Base class for list of selectable items. *)
    TalGUI_CustomItemListControl = CLASS (TalGUI_Control)
    PRIVATE
      fItemList: TStringList;
      fSelected: INTEGER;
      fOnChange: TalGUI_ControlEvent;

      PROCEDURE SetSelected (Ndx: INTEGER);
    PROTECTED
    (* Sent when a key is pressed and the object has the input focus.
      @return(@true if the object deals with the keypress, otherwise it
      should return @false to allow the default keyboard interface to operate.)
      @param(aKey Key pressed with an @code(al_readkey) format character code
      @(ASCII value in the low byte, scancode in the high byte@).) *)
      FUNCTION MsgKeyChar (aKey: INTEGER): BOOLEAN; OVERRIDE;
    PUBLIC
    (* Constructor.  Creates an empty list. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;

    (* Queries whether an object is willing to accept the input focus. *)
      FUNCTION WantFocus: BOOLEAN; OVERRIDE;

    (* List of items. *)
      PROPERTY Items: TStringList READ fItemList;
    (* Index of the selected item. *)
      PROPERTY Selected: INTEGER READ fSelected WRITE SetSelected;

    (* Event handler for any change on @link(Selected). *)
      PROPERTY OnChange: TalGUI_ControlEvent READ fOnChange WRITE fOnChange;
    END;



  (* Extends @code(TalGUI_CustomStyle) to define a default style.

    It's inspired by the old Windows style. *)
    TalGUI_DefaultStyle = CLASS (TalGUI_CustomStyle)
    PUBLIC
    (* Sets default colors.  Call this after set graphics mode and define the
      color palette. *)
      PROCEDURE SetDefaultColors; OVERRIDE;
    (* Draws a bevel, this is, a border, rectangle or frame.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of box.)  @param(y1 Top limit of box.)
      @param(x2 Right limit of box.) @param(y2 Bottom limit of box.)
      @param(Raised @true, to draw as raised, if @false to draw as depressed.)
      @seealso(DrawBox)
    *)
      PROCEDURE DrawBevel (Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER;
	Raised: BOOLEAN); OVERRIDE;
    (* Draws a box.  Useful for buttons and pannels.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of box.)  @param(y1 Top limit of box.)
      @param(x2 Right limit of box.) @param(y2 Bottom limit of box.)
      @param(BackColor Background color.  If negative, then it will not draw
        background.)
      @param(Raised @true, to draw the border as raised, @false to draw it
        depressed.)
      @seealso(DrawBevel)
     *)
      PROCEDURE DrawBox (
        Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor: INTEGER;
        Raised: BOOLEAN); OVERRIDE;
    (* Draws a dialog frame, wich includes a title.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of window.)  @param(y1 Top limit of window.)
      @param(x2 Right limit of window.) @param(y2 Bottom limit of window.)
      @param(BackColor Background color.  If negative, then it will not draw
        background.)
      @param(Title Text to draw on title.)
      @param(TitleCentered @true, to draw title centered.)
     *)
      PROCEDURE DrawDialogFrame (
        Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor: INTEGER;
        Title: STRING; TitleCentered: BOOLEAN
      ); OVERRIDE;
    (* Draws a text as disabled. *)
      PROCEDURE DrawDisabledText (Bmp: AL_BITMAPptr; CONST Msg:
	STRING; X, Y: LONGINT; Centered: BOOLEAN); OVERRIDE;
    (* Draws a checkbox.
      @param(Bmp Where to draw it.)
      @param(x Left limit of checkbox.)  @param(y Top limit of checkbox.)
      @param(Checked Whether to draw it checked or not.)
      @param(Focused Whether to draw it focused or not.)
      @return(Width of the box.  Used to draw the label.)
     *)
      FUNCTION DrawCheckbox (
	Bmp: AL_BitmapPtr; x, y: INTEGER; Checked: BOOLEAN
      ): INTEGER; OVERRIDE;
    END;

IMPLEMENTATION

  USES
    albase;

(*
 * TalGUI_CustomStyle
 *****************************************************************************)

(* Constructor.  Sets default colors. *)
  CONSTRUCTOR TalGUI_CustomStyle.Create;
  BEGIN
    INHERITED Create;
    SELF.SetDefaultColors;
    fTxtFont := al_font
  END;



(* Draws a "trench". *)
  PROCEDURE TalGUI_CustomStyle.DrawTrench (
    Bmp: AL_BitmapPtr; x1, y1, x2, y2: INTEGER);
  BEGIN
    SELF.DrawBox (Bmp, X1, Y1, X2, Y2, SELF.BackgroundColor, FALSE)
  END;



(* Draws a dotted rectangle to show that the control has acquired focus. *)
  PROCEDURE TalGUI_CustomStyle.DrawFocusRect (Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER);
  VAR
    X, Y: INTEGER;
  BEGIN
    IF X1 > X2 THEN BEGIN X := X1; X1 := X2; X2 := X END;
    IF Y1 > Y2 THEN BEGIN Y := Y1; Y1 := Y2; Y2 := Y END;
  { Done this way to reduce video bank switching (or something). }
    FOR X := 0 TO ((X2 -X1) DIV 2) DO
    BEGIN
      al_putpixel (Bmp, X1 + (X * 2)    , Y1, BorderColor);
      al_putpixel (Bmp, X1 + (X * 2) + 1, Y1, LightColor)
    END;
    FOR X := 0 TO ((X2 -X1) DIV 2) DO
    BEGIN
      al_putpixel (Bmp, X1 + (X * 2)    , Y2, BorderColor);
      al_putpixel (Bmp, X1 + (X * 2) + 1, Y2, LightColor)
    END;
    FOR Y := 1 TO (Y2 - Y1) DO
      IF y MOD 2 = 0 THEN
      BEGIN
        al_putpixel (Bmp, X1, Y1 + Y, BorderColor);
        al_putpixel (Bmp, X2, Y1 + Y, BorderColor)
      END
      ELSE BEGIN
        al_putpixel (Bmp, X1, Y1 + Y, LightColor);
        al_putpixel (Bmp, X2, Y1 + Y, LightColor)
      END
  END;



(* Draws a text. *)
  PROCEDURE TalGUI_CustomStyle.DrawText (Bmp: AL_BITMAPptr; CONST Msg: STRING;
    X, Y, Color: LONGINT; Centered: BOOLEAN);
  BEGIN
  { For some reason, if text is an empty string then it fails (may be because
    optimizations to NIL?). }
    IF Msg = '' THEN EXIT;
    IF Centered THEN
      al_textout_centre_ex (Bmp, fTxtFont, Msg, X, Y, Color, -1)
    ELSE
      al_textout_ex (Bmp, fTxtFont, Msg, X, Y, Color, -1)
  END;



(* Draws a text as disabled. *)
  PROCEDURE TalGUI_CustomStyle.DrawDisabledText (Bmp: AL_BITMAPptr; CONST Msg:
    STRING; X, Y: LONGINT; Centered: BOOLEAN);
  BEGIN
    SELF.DrawText (Bmp, Msg, X, Y, fDisabledColor, Centered)
  END;



(*
 * TalGUI_Control
 *****************************************************************************)

(* Sets control X position. *)
  PROCEDURE TalGUI_Control.SetX (CONST aX: INTEGER);
  BEGIN
    fX := aX;
    SELF.RedrawMe
  END;



(* Sets control Y position. *)
  PROCEDURE TalGUI_Control.SetY (CONST aY: INTEGER);
  BEGIN
    fY := aY;
    SELF.RedrawMe
  END;



(* Sets control width. *)
  PROCEDURE TalGUI_Control.SetWidth (CONST aWidth: INTEGER);
  BEGIN
    fW := aWidth;
    SELF.RedrawMe
  END;



(* Sets control height. *)
  PROCEDURE TalGUI_Control.SetHeight (CONST aHeight: INTEGER);
  BEGIN
    fH := aHeight;
    SELF.RedrawMe
  END;



(* Sets the @code(Enabled) property.  Overriden implementation should call
  his to actually set the @code(Enabled) value. *)
  PROCEDURE TalGUI_Control.SetEnabled (CONST aEnabled: BOOLEAN);
  BEGIN
    fEnabled := aEnabled;
    SELF.RedrawMe
  END;



(* Informs the object that a mouse button has been clicked. *)
  FUNCTION TalGUI_Control.MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN;
  BEGIN
    RESULT := FALSE
  END;



(* Sent when the keyboard shortcut for the object is pressed, or if enter,
  space, or a joystick button is pressed while it has the input focus. *)
  PROCEDURE TalGUI_Control.MsgKey;
  BEGIN
    { Does nothing by default. }
  END;



(* Sent when a key is pressed and the object has the input focus. *)
  FUNCTION TalGUI_Control.MsgKeyChar (aKey: INTEGER): BOOLEAN;
  BEGIN
    RESULT := FALSE
  END;



(* Sent when an object gains the input focus. *)
  PROCEDURE TalGUI_Control.MsgGotFocus;
  BEGIN
    IF NOT fHasFocus THEN
    BEGIN
      fHasFocus := TRUE;
      SELF.RedrawMe
    END
  END;



(* Sent when an object loses the input focus. *)
  FUNCTION TalGUI_Control.MsgLostFocus: BOOLEAN;
  BEGIN
    IF fHasFocus THEN
    BEGIN
      fHasFocus := FALSE;
      SELF.RedrawMe
    END;
    RESULT := TRUE
  END;



(* Sent when mouse moves on top of an object. *)
  PROCEDURE TalGUI_Control.MsgGotMouse;
  BEGIN
    fHasMouse := TRUE
  END;



(* Sent when mouse moves away of an object. *)
  PROCEDURE TalGUI_Control.MsgLostMouse;
  BEGIN
    fHasMouse := FALSE
  END;



(* Sent whenever the dialog manager has nothing better to do. *)
  PROCEDURE TalGUI_Control.MsgIddle;
  BEGIN
    { Does nothing by default. }
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_Control.Create;
  BEGIN
    INHERITED Create;
    fOwner := NIL;
    fBdColor := Random (maxLongint);
    fBgColor := Random (maxLongint);
    fColor   := Random (maxLongint);
    fX := -1; fY := -1; fW := 0; fH := 0; fTag := 0;
    fEnabled := TRUE; fHasFocus := FALSE; fRedraw := FALSE
  END;



(* Initializes the control. *)
  PROCEDURE TalGUI_Control.Initialize;
  BEGIN
    fRedraw := FALSE
  END;



(* Finalizes the control. *)
  PROCEDURE TalGUI_Control.Finalize;
  BEGIN
    ; { Does nothing by default. }
  END;



(* Sets default colors. *)
  PROCEDURE TalGUI_Control.SetDefaultColors;
  BEGIN
    fColor   := fOwner.Style.TextColor;
    fBdColor := fOwner.Style.BorderColor;
    fBgColor := fOwner.Style.BackgroundColor
  END;



(* Control should be redrawn. *)
  PROCEDURE TalGUI_Control.RedrawMe;
  BEGIN fRedraw := TRUE END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_Control.Draw (Bmp: AL_BITMAPptr);
  BEGIN
    { Does nothing by default. }
  END;



(* Queries if wants focus. *)
  FUNCTION TalGUI_Control.WantFocus: BOOLEAN;
  BEGIN
    RESULT := FALSE
  END;



(* Tests if given point is inside the control. *)
  FUNCTION TalGUI_Control.Inside (CONST aX, aY: INTEGER): BOOLEAN;
  BEGIN
    RESULT := (fX <= aX) AND (aX <= fX + fW - 1)
          AND (fY <= aY) AND (aY <= fY + fH - 1)
  END;



(*
 * TalGUI_ControlList
 *****************************************************************************)

  FUNCTION TalGUI_ControlList.GetCount: INTEGER;
  BEGIN
    RESULT := fControlList.Count
  END;



  FUNCTION TalGUI_ControlList.GetControl (CONST Ndx: INTEGER): TalGUI_Control;
  BEGIN
    RESULT := TalGUI_Control (fControlList.Items[Ndx])
  END;



  PROCEDURE TalGUI_ControlList.SetControl (CONST Ndx: INTEGER; aControl: TalGUI_Control);
  BEGIN
  { Remove from list without destroying. }
    fControlList.Extract (fControlList.Items[Ndx]);
  { Set the control. }
    fControlList.Items[Ndx] := aControl;
    aControl.fOwner := SELF.fOwner
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_ControlList.Create (DlgOwner: TalGUI_Dialog);
  BEGIN
    INHERITED Create;
    fControlList := TFPObjectList.Create (TRUE);
    fOwner := DlgOwner
  END;



(* Destructor. *)
  DESTRUCTOR TalGUI_ControlList.Destroy;
  BEGIN
    fControlList.Free;
    INHERITED Destroy
  END;



(* Removes all controls from the list. @seealso(Controls) *)
  PROCEDURE TalGUI_ControlList.Clear;
  BEGIN
    fControlList.Clear
  END;



(* Adds given control to the list. *)
  FUNCTION TalGUI_ControlList.Add (aControl: TalGUI_Control): INTEGER;
  BEGIN
    RESULT := fControlList.Add (aControl);
    aControl.fOwner := SELF.fOwner
  END;



(* Set default colors. *)
  PROCEDURE TalGUI_ControlList.SetDefaultColors;
  VAR
    Ndx: INTEGER;
  BEGIN
    FOR Ndx := 0 TO (fControlList.Count - 1) DO
      IF GetControl (Ndx) <> NIL THEN
	GetControl (Ndx).SetDefaultColors
  END;



(* Changes position of all contained controls. *)
  PROCEDURE TalGUI_ControlList.MoveControls (CONST DisplacementX, DisplacementY: INTEGER);
  VAR
    Ndx: INTEGER;
    aControl: TalGUI_Control;
  BEGIN
    FOR Ndx := 0 TO (fControlList.Count - 1) DO
    BEGIN
      aControl := GetControl (Ndx);
      IF aControl <> NIL THEN
      BEGIN
	aControl.X := aControl.X + DisplacementX;
	aControl.Y := aControl.Y + DisplacementY;
      END
    END
  END;



(* Draw controls in the given bitmap. *)
  PROCEDURE TalGUI_ControlList.Draw (BmpOut: AL_BITMAPptr);
  VAR
    Ndx: INTEGER;
    Control: TalGUI_Control;
  BEGIN
    FOR Ndx := 0 TO (fControlList.Count - 1) DO
    BEGIN
      Control := GetControl (Ndx);
      IF Control <> NIL THEN
      BEGIN
	Control.Draw (BmpOut);
	Control.fRedraw := FALSE
      END
    END
  END;



(* Helper method to know if control wants focus. *)
  FUNCTION TalGUI_ControlList.WantsFocus (CONST Ndx: INTEGER): BOOLEAN;
  VAR
    Control: TalGUI_Control;
  BEGIN
    Control := SELF.GetControl (Ndx);
    RESULT := (Control <> NIL) AND Control.Enabled AND Control.WantFocus
  END;




(*
 * TalGUI_Dialog
 *****************************************************************************)

  PROCEDURE TalGUI_Dialog.SetFocus (CONST NewFocus: INTEGER);
  VAR
    FocusReleased: BOOLEAN;
  BEGIN
    IF (NewFocus <> fFocusIndex)
  { TODO: Should raise an exception if trying to set focus to an unexistent
    control?  If so, then TalGUI_Dialog.Initialize should use a different way
    to set the initial focus control. }
    AND (0 <= NewFocus) AND (NewFocus < fControlList.Count) THEN
      IF fControlList.WantsFocus (NewFocus) THEN
      BEGIN
      { Be careful:  Uninitialized dialogs may have the Focus property
	assigned to an unexistent control. }
	IF (0 <= fFocusIndex) AND (fFocusIndex < fControlList.Count)
	AND fControlList[fFocusIndex].HasFocus
	THEN
	BEGIN
	  FocusReleased := fControlList[fFocusIndex].MsgLostFocus;
	  IF FocusReleased THEN
	    fControlList[fFocusIndex].RedrawMe
	END
	ELSE
	{ If nobody has focus, then it's free. }
	  FocusReleased := TRUE;
	IF FocusReleased THEN
	BEGIN
	  fFocusIndex := NewFocus;
	  fControlList[fFocusIndex].MsgGotFocus;
	  fControlList[fFocusIndex].RedrawMe
	END
      END
  END;



(* Initializes the dialog. *)
  PROCEDURE TalGUI_Dialog.Initialize (FocusCtrl: INTEGER);
  VAR
    Ndx: INTEGER;
  BEGIN
  { Initialize the controls. }
    FOR Ndx := 0 TO fControlList.Count - 1 DO
    BEGIN
      fControlList[Ndx].Initialize;
      fControlList[Ndx].fHasMouse := FALSE;
      fControlList[Ndx].fHasFocus := FALSE
    END;
  { Set focus. }
    IF (0 > FocusCtrl) OR (FocusCtrl >= fControlList.Count) THEN
      FocusCtrl := 0;
    fFocusIndex := FocusCtrl;
    INC (FocusCtrl);
    IF FocusCtrl >= fControlList.Count THEN FocusCtrl := 0;
    WHILE fFocusIndex <> FocusCtrl DO
    BEGIN
      IF fControlList.WantsFocus (FocusCtrl) THEN BREAK;
      INC (FocusCtrl);
      IF FocusCtrl >= fControlList.Count THEN FocusCtrl := 0;
    END;
    SELF.SetFocus (FocusCtrl);
  { Must draw all controls. }
    SELF.RedrawAll;
  { Begin. }
    fClosed := FALSE
  END;



(* Updates the dialog. *)
  FUNCTION TalGUI_Dialog.Update: BOOLEAN;
  VAR
    CtrNdx: INTEGER;
    DoIddle: BOOLEAN;
    KeyPressed: AL_INT;

  { Moves focus to next control. }
    PROCEDURE NextFocus;
    VAR
      NewFocus: INTEGER;
    BEGIN
      IF fFocusIndex < 0 THEN fFocusIndex := 0;
      NewFocus := fFocusIndex + 1;
      IF NewFocus >= fControlList.Count THEN NewFocus := 0;
      WHILE NewFocus <> fFocusIndex DO
      BEGIN
	IF fControlList.WantsFocus (NewFocus) THEN
	BEGIN
	  SetFocus (NewFocus);
	  DoIddle := FALSE;
	  EXIT
	END;
	INC (NewFocus);
	IF NewFocus >= fControlList.Count THEN NewFocus := 0;
      END
    END;

  { Moves focus to previous control. }
    PROCEDURE PreviousFocus;
    VAR
      NewFocus: INTEGER;
    BEGIN
      IF fFocusIndex < 0 THEN fFocusIndex := 0;
      NewFocus := fFocusIndex - 1;
      IF NewFocus < 0 THEN NewFocus := fControlList.Count - 1;
      WHILE NewFocus <> fFocusIndex DO
      BEGIN
	IF fControlList.WantsFocus (NewFocus) THEN
	BEGIN
	  SetFocus (NewFocus);
	  DoIddle := FALSE;
	  EXIT
	END;
	DEC (NewFocus);
	IF NewFocus < 0 THEN NewFocus := fControlList.Count - 1
      END
    END;

  BEGIN
  { Check if dialog was closed. }
    IF fClosed THEN EXIT (TRUE);
    DoIddle := TRUE;
  { Check mouse. }
    IF al_mouse_needs_poll THEN al_poll_mouse;
    FOR CtrNdx := 0 TO (fControlList.Count - 1) DO
    BEGIN
      IF fControlList[CtrNdx].Inside (al_mouse_x, al_mouse_y) THEN
      BEGIN
	DoIddle := FALSE;
	fControlList[CtrNdx].MsgGotMouse;
	IF (al_mouse_b <> 0) AND fControlList[CtrNdx].Enabled THEN
	BEGIN
	  IF fControlList[CtrNdx].MsgClick (al_mouse_x, al_mouse_y, al_mouse_b)
	  THEN BEGIN
	    SetFocus (CtrNdx);
	    DoIddle := FALSE;
	    BREAK
	  END
	END
      END
      ELSE IF fControlList[CtrNdx].fHasMouse THEN
	fControlList[CtrNdx].MsgLostMouse
    END;
  { Check keyboard. }
    IF al_keyboard_needs_poll THEN al_poll_keyboard;
    IF al_keypressed THEN
    BEGIN
      KeyPressed := al_readkey;
    { Current focus may want to manage the key presed. }
      IF (fFocusIndex >= 0) AND fControlList[fFocusIndex].HasFocus
      AND fControlList[fFocusIndex].MsgKeyChar (KeyPressed) THEN
	DoIddle := FALSE
      ELSE BEGIN
      { Control keys. }
	CASE KeyPressed SHR 8 OF
	AL_KEY_ESC:
	  BEGIN
	    SELF.SetFocus (-1);
	    SELF.Close
	  END;
	AL_KEY_TAB:
	  IF (al_key_shifts AND AL_KB_SHIFT_FLAG) <> 0 THEN
	    PreviousFocus
	  ELSE
	    NextFocus;
	AL_KEY_DOWN, AL_KEY_RIGHT:
	  NextFocus;
	AL_KEY_UP, AL_KEY_LEFT:
	  PreviousFocus;
	AL_KEY_ENTER, AL_KEY_SPACE, AL_KEY_ENTER_PAD:
	  IF fFocusIndex >= 0 THEN
	  BEGIN
	    fControlList[fFocusIndex].MsgKey;
	    DoIddle := FALSE
	  END
	END
      END
    END;
  { There's nothing to do.  }
    IF DoIddle THEN FOR CtrNdx := 0 TO (fControlList.Count - 1) DO
      fControlList[CtrNdx].MsgIddle;
  { End. }
    RESULT := fClosed
  END;



(* Finalizes the dialog execution and returns the control that caused it to
  exit or @code(-1) if @code(Esc) key was pressed. *)
  FUNCTION TalGUI_Dialog.Shutdown: INTEGER;
  VAR
    Ndx: INTEGER;
  BEGIN
  { Finalizes the controls. }
    FOR Ndx := 0 TO fControlList.Count - 1 DO
      IF fControlList[Ndx] <> NIL THEN
	fControlList[Ndx].Finalize;
    RESULT := fFocusIndex
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_Dialog.Create;
  BEGIN
    INHERITED Create;
    fControlList := TalGUI_ControlList.Create (SELF);
    fBitmap := al_screen;
    fStyle := TalGUI_DefaultStyle.Create
  END;



(* Destructor. *)
  DESTRUCTOR TalGUI_Dialog.Destroy;
  BEGIN
    fControlList.Free;
    IF fStyle <> NIL THEN fStyle.Free;
    INHERITED Destroy;
  END;



(* Draws the dialog. *)
  PROCEDURE TalGUI_Dialog.Draw;
  VAR
    CtrNdx: INTEGER;
  BEGIN
    IF fRedrawAll THEN
    TRY
      IF al_is_screen_bitmap (fBitmap) THEN al_scare_mouse;
      fControlList.Draw (fBitmap);
      fRedrawAll := FALSE
    FINALLY
      IF al_is_screen_bitmap (fBitmap) THEN al_unscare_mouse
    END
    ELSE FOR CtrNdx := 0 TO (fControlList.Count - 1) DO
      IF fControlList[CtrNdx].fRedraw THEN
      TRY
	IF al_is_screen_bitmap (fBitmap) THEN al_scare_mouse;
	fControlList[CtrNdx].Draw (fBitmap);
	fControlList[CtrNdx].fRedraw := FALSE
      FINALLY
	IF al_is_screen_bitmap (fBitmap) THEN al_unscare_mouse
      END
  END;



(* Executes the dialog loop. *)
  FUNCTION TalGUI_Dialog.Run (CONST FocusCtrl: INTEGER): INTEGER;
  BEGIN
    SELF.Initialize (FocusCtrl);
    REPEAT SELF.Draw UNTIL SELF.Update;
    RESULT := SELF.Shutdown
  END;



(* Set RedrawAll flag. *)
  PROCEDURE TalGUI_Dialog.RedrawAll;
  BEGIN
    fRedrawAll := TRUE
  END;



(* Set Close flag. *)
  PROCEDURE TalGUI_Dialog.Close;
  BEGIN
    fClosed := TRUE
  END;



(*
 * TalGUI_Yield
 *****************************************************************************)

(* Sent whenever the dialog manager has nothing better to do. *)
  PROCEDURE TalGUI_Yield.MsgIddle;
  BEGIN
  { Play fair with Opeating System. }
    al_rest (1)
  END;



(*
 * TalGUI_ClearScreen
 *****************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TalGUI_ClearScreen.Create;
  BEGIN
    INHERITED Create;
    SELF.Color := al_makecol (0, 0, 0)
  END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_ClearScreen.Draw (Bmp: AL_BITMAPptr);
  BEGIN
    al_clear_to_color (Bmp, SELF.Color)
  END;



(*
 * TalGUI_Box
 *****************************************************************************)

  PROCEDURE TalGUI_Box.SetRaised (CONST aRaised: BOOLEAN);
  BEGIN
    fRaised := aRaised;
    SELF.RedrawMe
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_Box.Create;
  BEGIN
    INHERITED Create;
    fRaised := TRUE
  END;



(* Creates the box. *)
  CONSTRUCTOR TalGUI_Box.CreateBox
    (CONST aX, aY, aW, aH: INTEGER; CONST aRaised: BOOLEAN);
  BEGIN
    INHERITED Create;
    X := aX; Y := aY; Width := aW; Height := aH;
    fRaised := aRaised
  END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_Box.Draw (Bmp: AL_BITMAPptr);
  BEGIN
    Dialog.Style.DrawBox (
      Bmp,
      X, Y, X + Width - 1, Y + Height - 1,
      BackgroundColor, fRaised
    );
  END;



(*
 * TalGUI_Label
 *****************************************************************************)

  PROCEDURE TalGUI_Label.SetAlignment (CONST aAlign: TalGUI_Alignment);
  BEGIN
    fAlignment := aAlign; SELF.RedrawMe
  END;



  PROCEDURE TalGUI_Label.SetCaption (CONST aCaption: STRING);
  BEGIN
    fCaption := aCaption; SELF.RedrawMe
  END;



(* Creates the label. *)
  CONSTRUCTOR TalGUI_Label.Create;
  BEGIN
    INHERITED Create;
    fCaption := '';
    fAlignment := agaLeft
  END;



(* Creates the label. *)
  CONSTRUCTOR TalGUI_Label.CreateLabel
    (CONST aCaption: STRING; CONST aX, aY, aW, aH: INTEGER; CONST aAlign: TalGUI_Alignment);
  BEGIN
    INHERITED Create;
    fCaption := aCaption;
    fX := aX; fY := aY; fW := aW; fH := aH;
    fAlignment := aAlign
  END;



(* Initializes the control. *)
  PROCEDURE TalGUI_Label.Initialize;
  BEGIN
    INHERITED Initialize;
    IF fW < 1 THEN
      fW := al_text_length (Dialog.Style.TextFont, fCaption);
    IF fH < 1 THEN
      fH := al_text_height (Dialog.Style.TextFont)
  END;



(* Sets default colors. *)
  PROCEDURE TalGUI_Label.SetDefaultColors;
  BEGIN
    INHERITED SetDefaultColors;
  { By default, it doesn't has background color. }
    BackgroundColor := -1; BorderColor := -1
  END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_Label.Draw (Bmp: AL_BITMAPptr);
  VAR
    pX, pY, tW: INTEGER;
  BEGIN
    IF SELF.BackgroundColor > -1 THEN
      al_rectfill (
        Bmp, X, Y, x + Width - 1, Y + Height - 1,
	SELF.BackgroundColor
      );
    IF SELF.BorderColor > -1 THEN
      al_rect (
        Bmp, X, Y, x + Width - 1, Y + Height - 1,
	SELF.BorderColor
      );
    tW := al_text_length (Dialog.Style.TextFont, fCaption);
    CASE fAlignment OF
    agaLeft:
      pX := X;
    agaCenter:
      pX := X + (Width DIV 2) - (tW DIV 2);
    agaRight:
      pX := X + Width - tW;
    END;
    pY := Y + (Height DIV 2) - (al_text_height (Dialog.Style.TextFont) DIV 2);
    IF SELF.Enabled THEN
      Dialog.Style.DrawText (Bmp, fCaption, pX, pY, SELF.Color, FALSE)
    ELSE
      Dialog.Style.DrawDisabledText (Bmp, fCaption, pX, pY, FALSE)
  END;



(*
 * TalGUI_CustomButton
 *****************************************************************************)

(* Sent when the keyboard shortcut for the object is pressed, or if enter,
  space, or a joystick button is pressed while it has the input focus. *)
  PROCEDURE TalGUI_CustomButton.MsgKey;
  BEGIN
    IF fonClick <> NIL THEN fonClick (SELF)
  END;



(* Button was pressed. *)
  FUNCTION TalGUI_CustomButton.MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN;

    PROCEDURE MyDraw; INLINE;
    BEGIN
      TRY
	IF al_is_screen_bitmap (SELF.Dialog.Bmp) THEN al_scare_mouse;
	SELF.Draw (SELF.Dialog.Bmp)
      FINALLY
	IF al_is_screen_bitmap (SELF.Dialog.Bmp) THEN al_unscare_mouse
      END
    END;

  BEGIN
    fPressed := TRUE;
    MyDraw;
  { Control the control. }
    WHILE al_mouse_b <> 0 DO
    BEGIN
      IF al_mouse_needs_poll THEN al_poll_mouse;
      IF HasMouse THEN
      BEGIN
	IF NOT Inside (al_mouse_x, al_mouse_y) THEN
	BEGIN
	  SELF.MsgLostMouse;
	  fPressed := FALSE;
	  MyDraw
	END
      END
      ELSE BEGIN
	IF Inside (al_mouse_x, al_mouse_y) THEN
	BEGIN
	  SELF.MsgGotMouse;
	  fPressed := TRUE;
	  MyDraw
	END
      END
    END;
    fPressed := FALSE;
    MyDraw;
  { Events. }
    IF Inside (al_mouse_x, al_mouse_y) THEN
    BEGIN
      RESULT := TRUE;
      IF Assigned (fonClick) THEN fonClick (SELF)
    END
    ELSE
      RESULT := FALSE
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_CustomButton.Create;
  BEGIN
    INHERITED Create;
    fonClick := NIL
  END;



(* Initializes the control.  It's called by the @link(TalGUI_Dialog) object
  just before it displays the dialog. *)
  PROCEDURE TalGUI_CustomButton.Initialize;
  BEGIN
    INHERITED Initialize;
    fPressed := FALSE
  END;



(* Queries if wants focus. *)
  FUNCTION TalGUI_CustomButton.WantFocus: BOOLEAN;
  BEGIN
    RESULT := TRUE
  END;



(*
 * TalGUI_CustomCheckBox
 *****************************************************************************)

  PROCEDURE TalGUI_CustomCheckBox.SetCaption (CONST aCaption: STRING);
  BEGIN
    fCaption := aCaption;
    SELF.RedrawMe
  END;



(* Sets the @link(Checked) property value. *)
  PROCEDURE TalGUI_CustomCheckBox.SetChecked (CONST aValue: BOOLEAN);
  BEGIN
    IF aValue <> fChecked THEN
    BEGIN
      IF Assigned (fOnChange) THEN fOnChange (SELF);
      fChecked := aValue;
      RedrawMe
    END
  END;



(* Sent when the keyboard shortcut for the object is pressed, or if enter,
  space, or a joystick button is pressed while it has the input focus. *)
  PROCEDURE TalGUI_CustomCheckBox.MsgKey;
  BEGIN
    SetChecked (NOT fChecked)
  END;



(* Button was pressed. *)
  FUNCTION TalGUI_CustomCheckBox.MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN;

    PROCEDURE MyDraw; INLINE;
    BEGIN
      al_scare_mouse; SELF.Draw (SELF.Dialog.Bmp); al_unscare_mouse
    END;

  BEGIN
  { Control the control. }
    REPEAT IF al_mouse_needs_poll THEN al_poll_mouse UNTIL al_mouse_b = 0;
  { Events. }
    IF Inside (al_mouse_x, al_mouse_y) THEN
    BEGIN
      RESULT := TRUE;
      SetChecked (NOT fChecked)
    END
    ELSE
      RESULT := FALSE
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_CustomCheckBox.Create;
  BEGIN
    INHERITED Create;
    fCaption := '';
    Width := 12; Height := 12
  END;



(* Queries if wants focus. *)
  FUNCTION TalGUI_CustomCheckBox.WantFocus: BOOLEAN;
  BEGIN
    RESULT := TRUE
  END;



(*
 * TalGUI_CustomSlider
 *****************************************************************************)

  PROCEDURE TalGUI_CustomSlider.SetDirection (CONST aDir: TalGUI_Direction);
  BEGIN
    IF fDirection <> aDir THEN
    BEGIN
      fDirection := aDir;
      SELF.RedrawMe
    END
  END;



  PROCEDURE TalGUI_CustomSlider.SetPos (aPos: INTEGER);
  BEGIN
    IF aPos < fMin THEN aPos := fMin;
    IF aPos > fMax THEN aPos := fMax;
    IF fPos <> aPos THEN
    BEGIN
      fPos := aPos;
      IF Assigned (fOnChange) THEN fOnChange (SELF);
      IF fPos < fMin THEN fPos := fMin;
      IF fPos > fMax THEN fPos := fMax;
      SELF.RedrawMe
    END
  END;



  PROCEDURE TalGUI_CustomSlider.SetMin (CONST aMin: INTEGER);
  BEGIN
    IF fMin <> aMin THEN
    BEGIN
      fMin := aMin;
      IF fMin > fMax THEN fMax := fMin;
      IF fPos < fMin THEN fPos := fMin;
      SELF.RedrawMe
    END
  END;



  PROCEDURE TalGUI_CustomSlider.SetMax (CONST aMax: INTEGER);
  BEGIN
    IF fMax <> aMax THEN
    BEGIN
      fMax := aMax;
      IF fMin > fMax THEN fMin := fMax;
      IF fPos > fMax THEN fPos := fMax;
      SELF.RedrawMe
    END
  END;



  PROCEDURE TalGUI_CustomSlider.SetPage (CONST aSize: INTEGER);
  BEGIN
    IF fPage <> aSize THEN
    BEGIN
      fPage := aSize;
      SELF.RedrawMe
    END
  END;



(* Manages key input. *)
  FUNCTION TalGUI_CustomSlider.MsgKeyChar (aKey: INTEGER): BOOLEAN;
  BEGIN
    CASE aKey SHR 8 OF
    AL_KEY_HOME:
      SetPos (fMin);
    AL_KEY_PGUP:
      SetPos (fPos - fPage);
    AL_KEY_UP, AL_KEY_LEFT:
      SetPos (fPos - 1);
    AL_KEY_DOWN, AL_KEY_RIGHT:
      SetPos (fPos + 1);
    AL_KEY_PGDN:
      SetPos (fPos + fPage);
    AL_KEY_END:
      SetPos (fMax);
    ELSE
      EXIT (FALSE);
    END;
    RESULT := TRUE
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_CustomSlider.Create;
  BEGIN
    INHERITED Create;
    SELF.fDirection := agdHorizontal;
    SELF.fMin := 0; SELF.fMax := 100; SELF.fPage := 10; SELF.fPos := 0;
    SELF.fOnChange := NIL
  END;



(* Initializes control. *)
  PROCEDURE TalGUI_CustomSlider.Initialize;
  BEGIN
    INHERITED Initialize;
    IF fPos < fMin THEN fPos := fMin;
    IF fPos > fMax THEN fPos := fMax;
  END;



(* It wants focus. *)
  FUNCTION TalGUI_CustomSlider.WantFocus: BOOLEAN;
  BEGIN
    RESULT := TRUE
  END;



(*
 * TalGUI_CustomItemListControl
 *****************************************************************************)


  PROCEDURE TalGUI_CustomItemListControl.SetSelected (Ndx: INTEGER);
  BEGIN
    IF Ndx < 0 THEN
      Ndx := 0
    ELSE IF Ndx >= fItemList.Count THEN
      Ndx := fItemList.Count - 1;
    IF Ndx <> fSelected THEN
    BEGIN
      fSelected := Ndx;
      IF Assigned (SELF.fOnChange) THEN SELF.fOnChange (SELF);
      SELF.RedrawMe
    END
  END;



(* Manages key input. *)
  FUNCTION TalGUI_CustomItemListControl.MsgKeyChar (aKey: INTEGER): BOOLEAN;
  BEGIN
    CASE aKey SHR 8 OF
    AL_KEY_HOME:
      SetSelected (0);
    AL_KEY_UP:
      SetSelected (fSelected - 1);
    AL_KEY_DOWN:
      SetSelected (fSelected + 1);
    AL_KEY_END:
      SetSelected (fItemList.Count);
    ELSE
      EXIT (FALSE);
    END;
    RESULT := TRUE
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_CustomItemListControl.Create;
  BEGIN
    INHERITED Create;
    fItemList := TStringList.Create
  END;



(* Destructor. *)
  DESTRUCTOR TalGUI_CustomItemListControl.Destroy;
  BEGIN
    fItemList.Free;
    INHERITED Destroy
  END;



(* Queries if wants focus. *)
  FUNCTION TalGUI_CustomItemListControl.WantFocus: BOOLEAN;
  BEGIN
    RESULT := TRUE
  END;



(*
 * TalGUI_DefaultStyle
 *****************************************************************************)

(* Sets default colors.  Call this after set graphics mode and define the
  color palette. *)
  PROCEDURE TalGUI_DefaultStyle.SetDefaultColors;
  BEGIN
    TextColor               := al_makecol (  0,   0,   0);
    DisabledTextColor       := al_makecol (128, 128, 128);
    SelectedTextColor       := al_makecol (255, 255, 255);
    BackgroundColor         := al_makecol (204, 204, 204);
    SelectedBackgroundColor := al_makecol (  0,   0, 128);
    BackgroundTextBoxColor  := al_makecol (255, 255, 255);
    LightColor              := al_makecol (255, 255, 255);
    DarkColor               := al_makecol (102, 102, 102);
    BorderColor             := al_makecol (  0,   0,   0);
  END;



(* Draws a bevel, this is, a border, rectangle or frame. *)
  PROCEDURE TalGUI_DefaultStyle.DrawBevel (
    Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER; Raised: BOOLEAN);
  BEGIN
    IF Raised THEN
    BEGIN
      al_rect (Bmp, x1, y1, x2 - 1, y2 - 1, LightColor);
      al_rect (Bmp, x1 + 1, y1 + 1, x2, y2, DarkColor)
    END
    ELSE BEGIN
      al_rect (Bmp, x1, y1, x2 - 1, y2 - 1, DarkColor);
      al_rect (Bmp, x1 + 1, y1 + 1, x2, y2, LightColor)
    END
  END;



(* Draws a box.  Useful for buttons and pannels. *)
  PROCEDURE TalGUI_DefaultStyle.DrawBox (
    Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor: INTEGER;
    Raised: BOOLEAN);
  VAR
    Cnt, ClrLeft, ClrRight: INTEGER;
  BEGIN
    IF BackColor >= 0 THEN al_rectfill (Bmp, x1, y1, x2, y2, BackColor);
    IF Raised THEN
    BEGIN
      ClrLeft := LightColor; ClrRight := DarkColor
    END
    ELSE BEGIN
      ClrLeft := DarkColor; ClrRight := LightColor
    END;
    FOR Cnt := 1 DOWNTO 0 DO
    BEGIN
      al_hline (Bmp, x1 + Cnt, y2 - Cnt, x2 - Cnt, ClrRight);
      al_vline (Bmp, X2 - Cnt, y1 + Cnt, y2 - Cnt, ClrRight);

      al_hline (Bmp, x1 + Cnt, y1 + Cnt, x2 - Cnt, ClrLeft);
      al_vline (Bmp, X1 + Cnt, y1 + Cnt, y2 - Cnt, ClrLeft);
    END
  END;



(* Draws a dialog frame, wich includes a title. *)
  PROCEDURE TalGUI_DefaultStyle.DrawDialogFrame (
    Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor: INTEGER;
    Title: STRING; TitleCentered: BOOLEAN);
  BEGIN
    al_rectfill (Bmp, x1, y1, x2, y2, BackColor);
    al_rectfill (Bmp, x1, y1, x2, y1 + 16, SelectedBackgroundColor);
    al_rect     (Bmp, x1, y1, x2, y2, BorderColor);

    al_hline (Bmp, x1 + 1, y2 - 1, x2 - 1, DarkColor);
    al_vline (Bmp, X2 - 1, y1 + 1, y2 - 1, DarkColor);

    al_hline (Bmp, x1 + 1, y1 + 1, x2 - 1, LightColor);
    al_vline (Bmp, X1 + 1, y1 + 1, y2 - 1, LightColor);

    IF TitleCentered THEN x1 := (x1 + X2) DIV 2;
    DrawText (Bmp, Title, x1, y1 + 4, SelectedTextColor, TitleCentered)
  END;



(* Draws a text as disabled. *)
  PROCEDURE TalGUI_DefaultStyle.DrawDisabledText (Bmp: AL_BITMAPptr; CONST Msg:
    STRING; X, Y: LONGINT; Centered: BOOLEAN);
  BEGIN
  { For some reason, if text is an empty string then it fails (may be because
    optimizations to NIL?). }
    IF Msg <> '' THEN
    BEGIN
      DrawText (Bmp, Msg, X + 1, Y + 1, LightColor, Centered);
      DrawText (Bmp, Msg, X, Y, DarkColor, Centered)
    END
  END;



(* Draws a checkbox. *)
  FUNCTION TalGUI_DefaultStyle.DrawCheckbox (
    Bmp: AL_BitmapPtr; x, y: INTEGER; Checked: BOOLEAN): INTEGER;
  VAR
    Size, x2, y2: INTEGER;
  BEGIN
  { Nice sizes and positions. }
    INC (x); INC (y);
    Size := ((al_text_height (SELF.TextFont) * 3) DIV 2) - 1;
    x2 := x + Size; y2 := y + Size;
    RESULT := Size + 1;
  { Draw. }
    al_rectfill (Bmp, x, y, x2, y2, SELF.BackgroundTextBoxColor);
    IF Checked THEN
    BEGIN
      Size := al_text_height (SELF.TextFont) DIV 4;
      DrawText (Bmp, 'x', x + Size, y + Size, TextColor, FALSE)
    END;
    al_hline (Bmp, x, y2, x2, LightColor);
    al_vline (Bmp, X2, y, y2, LightColor);
    al_vline (Bmp, X, y, y2, DarkColor);
    al_hline (Bmp, x, y, x2, DarkColor);
  END;

END.
