UNIT Common;
(* Common stuff for examples. *)

INTERFACE

  USES
    allegro5;

(* Exits program with error. *)
  PROCEDURE AbortExample (CONST Message: STRING);

(* Opens a log window. *)
  PROCEDURE OpenLog;

(* Prints a message on the log window. *)
  PROCEDURE LogWrite (Str: STRING);
  PROCEDURE LogWriteLn (Str: STRING);

(* Closes the log window. *)
  PROCEDURE CloseLog (WaitForUser: BOOLEAN);

IMPLEMENTATION

  USES
    al5nativedlg;
  VAR
    TextLog: ALLEGRO_TEXTLOGptr;


(* Exits program with error. *)
  PROCEDURE AbortExample (CONST Message: STRING);
  VAR
    Display: ALLEGRO_DISPLAYptr;
  BEGIN
    //IF al_init_native_dialog_addon THEN
    BEGIN
      IF al_is_system_installed THEN
        Display := al_get_current_display
      ELSE
        Display := NIL;
      al_show_native_message_box (Display, 'Error', 'Cannot run example', Message, '', 0);
    END
    //ELSE
     // WriteLn (stderr, Message)
        ;
    HALT (1);
  END;



(* Opens a log window. *)
  PROCEDURE OpenLog;
  BEGIN
    TextLog := al_open_native_text_log ('Log', 0);
  END;



(* Prints a message on the log window. *)
  PROCEDURE LogWrite (Str: STRING);
  BEGIN
    al_append_native_text_log (TextLog, Str);
  END;

  PROCEDURE LogWriteLn (Str: STRING);
  BEGIN
    al_append_native_text_log (TextLog, Str + #10);
  END;



(* Closes the log window. *)
  PROCEDURE CloseLog (WaitForUser: BOOLEAN);
  VAR
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
  BEGIN
    IF (TextLog <> NIL) AND WaitForUser THEN
    BEGIN
      Queue := al_create_event_queue;
      al_register_event_source (Queue, al_get_native_text_log_event_source(
	TextLog
      ));
      al_wait_for_event (Queue, Event);
      al_destroy_event_queue (Queue);
   END;
   al_close_native_text_log (TextLog);
   TextLog := NIL;
  END;

END.
