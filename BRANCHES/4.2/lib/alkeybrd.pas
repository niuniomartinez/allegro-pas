UNIT alkeybrd;
(*
  ______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\ 
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	Keyboard support.
 *	by Ñuño Martínez <>
 *	Based on the file "keyboard.h" of the Allegro library by Shawn
 *	Hargreaves.  Some parts of the code was created by the "h2pas" utility.
 *
 *	See readme.txt for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$LONGSTRINGS ON}
{$ELSE}
{ Assumes Borland Delphi/Turbo. }
 {$A-}
 {$H+}
{$ENDIF}



INTERFACE

USES
  albase;



(* Functions and procedures. *)
  FUNCTION al_install_keyboard: AL_INT;
  PROCEDURE al_remove_keyboard;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_keyboard';
  FUNCTION al_poll_keyboard: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_keyboard';
  FUNCTION al_keyboard_needs_poll: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'keyboard_needs_poll';
  FUNCTION al_keypressed: AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'keypressed';
  FUNCTION al_readkey: AL_INT; 
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'readkey';
  FUNCTION al_ureadkey (scancode: AL_INTPTR): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'ureadkey';
  PROCEDURE al_simulate_keypress (keycode: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_keypress';
  PROCEDURE al_simulate_ukeypress (keycode, scancode: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'simulate_ukeypress';
  PROCEDURE al_clear_keybuf;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_keybuf';
  PROCEDURE al_set_leds (leds: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_leds';
  PROCEDURE al_set_keyboard_rate (key_delay, key_repeat: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_keyboard_rate';
  FUNCTION al_scancode_to_ascii (scancode: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scancode_to_ascii';
  FUNCTION al_scancode_to_name (scancode: AL_INT): AL_STRING;



(* Access to the Allegro's public variables. *)
TYPE
  AL_KEY_LISTptr = ^AL_KEY_LIST;
  AL_KEY_LIST	 = ARRAY [0..126] OF AL_CHAR;



VAR
  al_key: AL_KEY_LISTptr;
  al_key_shifts: AL_INTptr;
  al_three_finger_flag: AL_INTptr;
  al_key_led_flag: AL_INTptr;



CONST
{ Key scan-code identifiers. }
  AL_KEY_A		= 1;
  AL_KEY_B		= 2;
  AL_KEY_C              = 3;
  AL_KEY_D              = 4;
  AL_KEY_E              = 5;
  AL_KEY_F              = 6;
  AL_KEY_G		= 7;
  AL_KEY_H		= 8;
  AL_KEY_I              = 9;
  AL_KEY_J              = 10;
  AL_KEY_K              = 11;
  AL_KEY_L              = 12;
  AL_KEY_M              = 13;
  AL_KEY_N              = 14;
  AL_KEY_O              = 15;
  AL_KEY_P              = 16;
  AL_KEY_Q              = 17;
  AL_KEY_R              = 18;
  AL_KEY_S              = 19;
  AL_KEY_T              = 20;
  AL_KEY_U              = 21;
  AL_KEY_V              = 22;
  AL_KEY_W              = 23;
  AL_KEY_X              = 24;
  AL_KEY_Y              = 25;
  AL_KEY_Z              = 26;
  AL_KEY_0	        = 27;
  AL_KEY_1	        = 28;
  AL_KEY_2		= 29;
  AL_KEY_3	        = 30;
  AL_KEY_4		= 31;
  AL_KEY_5		= 32;
  AL_KEY_6		= 33;
  AL_KEY_7		= 34;
  AL_KEY_8		= 35;
  AL_KEY_9		= 36;
  AL_KEY_0_PAD		= 37;
  AL_KEY_1_PAD		= 38;
  AL_KEY_2_PAD		= 39;
  AL_KEY_3_PAD		= 40;
  AL_KEY_4_PAD		= 41;
  AL_KEY_5_PAD		= 42;
  AL_KEY_6_PAD		= 43;
  AL_KEY_7_PAD  	= 44;
  AL_KEY_8_PAD  	= 45;
  AL_KEY_9_PAD		= 46;
  AL_KEY_F1		= 47;
  AL_KEY_F2		= 48;
  AL_KEY_F3		= 49;
  AL_KEY_F4		= 50;
  AL_KEY_F5		= 51;
  AL_KEY_F6		= 52;
  AL_KEY_F7		= 53;
  AL_KEY_F8		= 54;
  AL_KEY_F9		= 55;
  AL_KEY_F10		= 56;
  AL_KEY_F11		= 57;
  AL_KEY_F12		= 58;
  AL_KEY_ESC		= 59;
  AL_KEY_TILDE		= 60;
  AL_KEY_MINUS		= 61;
  AL_KEY_EQUALS		= 62;
  AL_KEY_BACKSPACE	= 63;
  AL_KEY_TAB		= 64;
  AL_KEY_OPENBRACE	= 65;
  AL_KEY_CLOSEBRACE	= 66;
  AL_KEY_ENTER		= 67;
  AL_KEY_COLON		= 68;
  AL_KEY_QUOTE		= 69;
  AL_KEY_BACKSLASH	= 70;
  AL_KEY_BACKSLASH2	= 71;
  AL_KEY_COMMA  	= 72;
  AL_KEY_STOP		= 73;
  AL_KEY_SLASH  	= 74;
  AL_KEY_SPACE  	= 75;
  AL_KEY_INSERT  	= 76;
  AL_KEY_DEL  	     	= 77;
  AL_KEY_HOME  	     	= 78;
  AL_KEY_END  	     	= 79;
  AL_KEY_PGUP  	     	= 80;
  AL_KEY_PGDN  	     	= 81;
  AL_KEY_LEFT  		= 82;
  AL_KEY_RIGHT  	= 83;
  AL_KEY_UP  	     	= 84;
  AL_KEY_DOWN  	     	= 85;
  AL_KEY_SLASH_PAD      = 86;
  AL_KEY_ASTERISK       = 87;
  AL_KEY_MINUS_PAD      = 88;
  AL_KEY_PLUS_PAD       = 89;
  AL_KEY_DEL_PAD  	= 90;
  AL_KEY_ENTER_PAD      = 91;
  AL_KEY_PRTSCR  	= 92;
  AL_KEY_PAUSE  	= 93;
  AL_KEY_ABNT_C1  	= 94;
  AL_KEY_YEN  	  	= 95;
  AL_KEY_KANA  	     	= 96;
  AL_KEY_CONVERT  	= 97;
  AL_KEY_NOCONVERT      = 98;
  AL_KEY_AT  	      	= 99;
  AL_KEY_CIRCUMFLEX     = 100;
  AL_KEY_COLON2  	= 101;
  AL_KEY_KANJI  	= 102;
  AL_KEY_EQUALS_PAD	= 103;  { MacOS X }
  AL_KEY_BACKQUOTE	= 104;  { MacOS X }
  AL_KEY_SEMICOLON	= 105;  { MacOS X }
  AL_KEY_COMMAND	= 106;  { MacOS X }
  AL_KEY_UNKNOWN1	= 107;
  AL_KEY_UNKNOWN2	= 108;
  AL_KEY_UNKNOWN3	= 109;
  AL_KEY_UNKNOWN4	= 110;
  AL_KEY_UNKNOWN5	= 111;
  AL_KEY_UNKNOWN6	= 112;
  AL_KEY_UNKNOWN7	= 113;
  AL_KEY_UNKNOWN8	= 114;
  
  AL_KEY_MODIFIERS      = 115;

  AL_KEY_LSHIFT  	= 115;
  AL_KEY_RSHIFT 	= 116;
  AL_KEY_LCONTROL       = 117;
  AL_KEY_RCONTROL       = 118;
  AL_KEY_ALT  	      	= 119;
  AL_KEY_ALTGR  	= 120;
  AL_KEY_LWIN  	      	= 121;
  AL_KEY_RWIN  	      	= 122;
  AL_KEY_MENU  	      	= 123;
  AL_KEY_SCRLOCK  	= 124;
  AL_KEY_NUMLOCK  	= 125;
  AL_KEY_CAPSLOCK       = 126;

  AL_KEY_MAX		= 127;

{ Shift keys flags. }
  AL_KB_SHIFT_FLAG	 = $0001;
  AL_KB_CTRL_FLAG	 = $0002;
  AL_KB_ALT_FLAG         = $0004;
  AL_KB_LWIN_FLAG        = $0008;
  AL_KB_RWIN_FLAG        = $0010;
  AL_KB_MENU_FLAG        = $0020;
  AL_KB_SCROLOCK_FLAG    = $0100;
  AL_KB_NUMLOCK_FLAG     = $0200;
  AL_KB_CAPSLOCK_FLAG    = $0400;
  AL_KB_INALTSEQ_FLAG    = $0800;
  AL_KB_ACCENT1_FLAG     = $1000;
  AL_KB_ACCENT2_FLAG     = $2000;
  AL_KB_ACCENT3_FLAG     = $4000;
  AL_KB_ACCENT4_FLAG     = $8000;



IMPLEMENTATION

(* Delphi can't access to the public variables from Allegro, so we need some
 * magic to access them. *)
FUNCTION install_keyboard: AL_INT;
  CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
FUNCTION _get_key_: AL_KEY_LISTptr;
  CDECL; EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
FUNCTION _get_key_shifts_: AL_INTptr;
  CDECL; EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
FUNCTION _get_three_finger_flag_: AL_INTptr;
  CDECL; EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
FUNCTION _get_key_led_flag_: AL_INTptr;
  CDECL; EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;

FUNCTION al_install_keyboard: AL_INT;
VAR
  R: AL_INT;
BEGIN
  R := install_keyboard;
  IF R = 0 THEN
  BEGIN
  { Get pointers of public variables. }
    al_key := _get_key_;
    al_key_shifts := _get_key_shifts_;
    al_three_finger_flag := _get_three_finger_flag_;
    al_key_led_flag := _get_key_led_flag_;
  END;
  al_install_keyboard := R;
END;



  FUNCTION scancode_to_name (scancode: AL_INT): PCHAR;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scancode_to_name';

  FUNCTION al_scancode_to_name (scancode: AL_INT): AL_STRING;
  BEGIN
    al_scancode_to_name := scancode_to_name (scancode);
  END;

END.
