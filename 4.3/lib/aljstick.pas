UNIT aljstick;
(*< Unlike keyboard or mouse input, which are usually read through hardware
    interrupts by Allegro, joystick input functions have to be polled because
    there are no hardware interrupts for them on most platforms.  This doesn't
    mean that you have to poll the joysticks on each line of code you want to
    read their values, but you should make sure to poll them at least once per
    frame in your game loop.  Otherwise you face the possibility of reading
    stale incorrect data.  *)

{$IFDEF FPC}
{ Free Pascal. }
 {$MODE FPC}
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}
{$H+}



INTERFACE

USES
  albase;



CONST
(* To be used at @link(al_install_joystick). *)
  AL_JOY_TYPE_AUTODETECT = -(1);
  AL_JOY_TYPE_NONE = 0;

(* Maximun number of elements. *)
  AL_MAX_JOYSTICKS = 8;
  AL_MAX_JOYSTICK_AXIS = 3;
  AL_MAX_JOYSTICK_STICKS = 5;
  AL_MAX_JOYSTICK_BUTTONS = 32;

(* joystick status flags. @seealso(AL_JOYSTICK_INFO) *)
  AL_JOYFLAG_DIGITAL = 1;
  AL_JOYFLAG_ANALOGUE = 2;
  AL_JOYFLAG_CALIB_DIGITAL = 4;
  AL_JOYFLAG_CALIB_ANALOGUE = 8;
  AL_JOYFLAG_CALIBRATE = 16;
  AL_JOYFLAG_SIGNED = 32;
  AL_JOYFLAG_UNSIGNED = 64;

  AL_JOYFLAG_ANALOG = AL_JOYFLAG_ANALOGUE;
  AL_JOYFLAG_CALIB_ANALOG = AL_JOYFLAG_CALIB_ANALOGUE;



TYPE
(* This provides both analogue input in the @code(pos) field (ranging from
   -128 to 128 or from 0 to 255, depending on the type of the control), and
   digital values in the @code(d1) and @code(d2) fields.  For example, when
   describing the X-axis position, the @code(pos) field will hold the
   horizontal position of the joystick, @code(d1) will be set if it is moved
   left, and @code(d2) will be set if it is moved right.  Allegro will fill in
   all these values regardless of whether it is using a digital or analogue
   joystick, emulating the pos field for digital inputs by snapping it to the
   min, middle, and maximum positions, and emulating the @code(d1) and
   @code(d2) values for an analogue stick by comparing the current position
   with the centre point. *)
  AL_JOYSTICK_AXIS_INFO = RECORD
    pos : LONGINT; {< analogue axis position. }
  { digital axis position. }
    d1 : LONGINT;
    d2 : LONGINT;
    name : PCHAR; {< description of this axis. }
  END;

(* information about one or more axis (a slider or directional control) *)
  AL_JOYSTICK_STICK_INFO = RECORD
    flags : LONGINT;{< status flags for this input. }
    num_axis : LONGINT; {< how many axes do we have? (note de misspelling). }
  { axis state information. @seealso(AL_JOYSTICK_AXIS_INFO) }
    axis : ARRAY [0..(AL_MAX_JOYSTICK_AXIS)-1] OF AL_JOYSTICK_AXIS_INFO;
    name : PCHAR; {< description of this input. }
  END;

(* information about a joystick button.

   You may wish to display the button names as part of an input configuration
   screen to let the user choose what game function will be performed by each
   button, but in simpler situations you can safely assume that the first two
   elements in the button array will always be the main trigger controls. *)
  AL_JOYSTICK_BUTTON_INFO = RECORD
    b : LONGINT; { 0 not pressed, (NOT 0) pressed. }
    name : PCHAR; { description of this button. }
  END;

(* Pointer to @link(AL_JOYSTICK_INFO). *)
  AL_JOYSTICK_INFOptr = ^AL_JOYSTICK_INFO;
(* information about an entire joystick.

   Each joystick will provide one or more stick inputs, of varying types.
   These can be digital controls which snap to specific positions (eg. a
   gamepad controller, the coolie hat on a Flightstick Pro or Wingman Extreme,
   or a normal joystick which hasn't yet been calibrated), or they can be full
   analogue inputs with a smooth range of motion.  Sticks may also have
   different numbers of axes, for example a normal directional control has two,
   but the Flightstick Pro throttle is only a single axis, and it is possible
   that the system could be extended in the future to support full 3d
   controllers.

   The joystick flags field may contain any combination of the bit flags:
   @definitionList(
     @itemLabel(AL_JOYFLAG_DIGITAL)
     @item(This control is currently providing digital input.)

     @itemLabel(AL_JOYFLAG_ANALOGUE)
     @item(This control is currently providing analogue input.)

     @itemLabel(AL_JOYFLAG_CALIB_DIGITAL)
     @item(This control will be capable of providing digital input once it has
       been calibrated, but is not doing this at the moment.)

     @itemLabel(AL_JOYFLAG_CALIB_ANALOGUE)
     @item(This control will be capable of providing analogue input once it
       has been calibrated, but is not doing this at the moment.)

     @itemLabel(AL_JOYFLAG_CALIBRATE)
     @item(Indicates that this control needs to be calibrated. Many devices
       require multiple calibration steps, so you should call the
       @link(al_calibrate_joystick) function from a loop until this flag is
       cleared.)

     @itemLabel(AL_JOYFLAG_SIGNED)
     @item(Indicates that the analogue axis position is in signed format,
       ranging from -128 to 128. This is the case for all 2d directional
       controls.)

     @itemLabel(AL_JOYFLAG_UNSIGNED)
     @item(Indicates that the analogue axis position is in unsigned format,
       ranging from 0 to 255. This is the case for all 1d throttle controls.)
   ) *)
  AL_JOYSTICK_INFO = RECORD
    flags : LONGINT; {< status flags for this joystick. }
    num_sticks : LONGINT; {<  how many stick inputs? }
    num_buttons : LONGINT; {< how many buttons? }
  { stick state information. @seealso(AL_JOYSTICK_STICK_INFO) }
    stick : ARRAY [0..(AL_MAX_JOYSTICK_STICKS)-1] OF AL_JOYSTICK_STICK_INFO;
  { button state information. @seealso(AL_JOYSTICK_BUTTON_INFO). }
    button : ARRAY [0..(AL_MAX_JOYSTICK_BUTTONS)-1] OF AL_JOYSTICK_BUTTON_INFO;
  END;

  AL_JOYSTICK_INFO_LISTptr = ^AL_JOYSTICK_INFO_LIST;
  AL_JOYSTICK_INFO_LIST = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_JOYSTICK_INFO;



VAR
(* Global array of joystick state information, which is updated by the
   @link(al_poll_joystick) function.  Only the first @link(al_num_joysticks)
   elements will contain meaningful information.

   @seealso(AL_JOYSTICK_INFO)

   A single joystick may provide several different stick inputs, but you can
   safely assume that the first element in the stick array will always be the
   main directional controller.

   Information about each of the stick axis is stored in the
   @link(AL_JOYSTICK_AXIS_INFO) substructure.

   Note for people who spell funny: in case you don't like having to type
   @italic (analogue), there are some aliases in this unit that will allow you
   to write @italic(analog) instead.  *)
  al_joy: AL_JOYSTICK_INFO_LIST; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'joy';

(* Global variable containing the number of active joystick devices.  The
   current drivers support a maximum of eight controllers. *)
  al_num_joysticks: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'num_joysticks';


(* Installs Allegro's joystick handler, and calibrates the centre position
   values.  The type parameter should usually be @code(AL_JOY_TYPE_AUTODETECT),
   or see the platform specific documentation for a list of the available
   drivers.  You must call this routine before using any other joystick
   functions, and you should make sure that all joysticks are in the middle
   position at the time.  Example:
   @longcode(#
al_textout_centre_ex (al_screen, al_font,
		      'Center the joystick and press a key.',
		      AL_SCREEN_W DIV 2, SCREEN_H DIV 2, red_color, -1);
al_readkey;
IF NOT al_install_joystick (AL_JOY_TYPE_AUTODETECT) THEN
  abort_on_error ('Error initialising joystick!');
   #);
   @returns(@true on success.  As soon as you have installed the joystick
     module, you will be able to read the button state and digital @(on/off
     toggle@) direction information, which may be enough for some games.  If
     you want to get full analogue input, though, you need to use the
     @link(al_calibrate_joystick) functions to measure the exact range of the
     inputs.) *)
  FUNCTION al_install_joystick (atype: LONGINT): BOOLEAN;

(* Removes the joystick handler. You don't normally need to bother calling
   this, because @link(al_exit) will do it for you. *)
  PROCEDURE al_remove_joystick; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_joystick';

(* Pass the number of the joystick you want to calibrate as the parameter.

    @returns(a text description for the next type of calibration that will
      be done on the specified joystick, or empty string if no more calibration
      is required.) *)
  FUNCTION al_calibrate_joystick_name (n: LONGINT): STRING;

(* Most joysticks need to be calibrated before they can provide full analogue
   input.  This function performs the next operation in the calibration series
   for the specified stick, assuming that the joystick has been positioned in
   the manner described by a previous call to
   @link(al_calibrate_joystick_name), returning @true on success.  For example,
   a simple routine to fully calibrate all the joysticks might look like:
   @longcode(#
FUNCTION CalibrateJoysticks: BOOLEAN
VAR
  Cnt: INTEGER;
BEGIN
  FOR Cnt := 1 TO al_num_joysticks DO
  BEGIN
    WHILE (al_joy[Cnt - 1].flags AND AL_JOYFLAG_CALIBRATE) <> 0 DO
    BEGIN
      al_textout_ex (..., al_calibrate_joystick_name (Cnt - 1) + ', and press a key', ...);
      al_readkey;
      IF NOT al_calibrate_joystick (i - 1) THEN
      BEGIN
        al_textout_ex (..., 'oops!', ...);
	al_readkey;
	RESULT := FALSE;
	EXIT;
      END;
    END;
  END;
  RESULT := TRUE;
END;
   #)

   @returns(@true on success, @false if the calibration could not be performed
     successfully.) *)
  FUNCTION al_calibrate_joystick (n: LONGINT): BOOLEAN;

(* After all the headache of calibrating the joystick, you may not want to make
   your poor users repeat the process every time they run your program.  Call
   this function to save the joystick calibration data into the specified
   configuration file, from which it can later be read by
   @link(al_load_joystick_data).  Pass a @nil filename to write the data to the
   currently selected configuration file.

   @returns(@true on success, @false if the data could not be saved.) *)
  FUNCTION al_save_joystick_data (filename: STRING): BOOLEAN;

(* Restores calibration data previously stored by @link(al_save_joystick_data)
   or the setup utility.  This sets up all aspects of the joystick code:  you
   don't even need to call @link(al_install_joystick) if you are using this
   function.  Pass an empty filename to read the data from the currently
   selected configuration file.

   @returns(@true on success:  if it fails the joystick state is undefined and
     you must reinitialise it from scratch.) *)
  FUNCTION al_load_joystick_data (filename: STRING): BOOLEAN;

(* The joystick handler is not interrupt driven, so you need to call this
   function every now and again to update the global position values.

   @returns(zero on success or a negative number on failure @(usually because
     no joystick driver was installed@).) *)
  FUNCTION al_poll_joystick: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_joystick';



IMPLEMENTATION

  FUNCTION al_install_joystick (atype: LONGINT): BOOLEAN;
  BEGIN
    al_install_joystick := install_joystick (atype) = 0;
  END;



  FUNCTION calibrate_joystick_name (n: LONGINT): PCHAR; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'calibrate_joystick_name';

  FUNCTION al_calibrate_joystick_name (n: LONGINT): STRING;
  VAR
    Tmp: PCHAR;
  BEGIN
    Tmp := calibrate_joystick_name (n);
    IF Tmp <> NIL THEN
      al_calibrate_joystick_name := (Tmp)
    ELSE
      al_calibrate_joystick_name := '';
  END;



  FUNCTION calibrate_joystick (n: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME

  FUNCTION al_calibrate_joystick (n: LONGINT): BOOLEAN;
  BEGIN
    al_calibrate_joystick := calibrate_joystick (n) = 0;
  END;



  FUNCTION save_joystick_data (filename: PCHAR): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_joystick_data';

  FUNCTION al_save_joystick_data (filename: STRING): BOOLEAN;
  BEGIN
    al_save_joystick_data := save_joystick_data (PCHAR (filename)) = 0;
  END;



  FUNCTION load_joystick_data (filename: PCHAR): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_joystick_data';

  FUNCTION al_load_joystick_data (filename: STRING): BOOLEAN;
  BEGIN
    IF filename <> '' THEN
      al_load_joystick_data := load_joystick_data (PCHAR (filename)) = 0
    ELSE
      al_load_joystick_data := load_joystick_data (NIL) = 0;
  END;

END.
