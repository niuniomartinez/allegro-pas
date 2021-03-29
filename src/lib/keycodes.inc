(*         ______   ___    ___
 *        /\  _  \ /\_ \  /\_ \
 *        \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___
 *         \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\
 *          \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \
 *           \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/
 *            \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/
 *                                           /\____/
 *                                           \_/__/
 *
 *      Keycode constants.
 *
 *      See readme.txt for copyright information.
 *)

{ This is just to help "diff" to find modifications.
#ifndef __al_included_allegro5_keycodes_h
#define __al_included_allegro5_keycodes_h
}



const
(* Note these values are deliberately the same as in Allegro 4.1.x *)
  ALLEGRO_KEY_A		= 1; (***<@exclude *)
  ALLEGRO_KEY_B		= 2; (***<@exclude *)
  ALLEGRO_KEY_C		= 3; (***<@exclude *)
  ALLEGRO_KEY_D		= 4; (***<@exclude *)
  ALLEGRO_KEY_E		= 5; (***<@exclude *)
  ALLEGRO_KEY_F		= 6; (***<@exclude *)
  ALLEGRO_KEY_G		= 7; (***<@exclude *)
  ALLEGRO_KEY_H		= 8; (***<@exclude *)
  ALLEGRO_KEY_I		= 9; (***<@exclude *)
  ALLEGRO_KEY_J		= 10; (***<@exclude *)
  ALLEGRO_KEY_K		= 11; (***<@exclude *)
  ALLEGRO_KEY_L		= 12; (***<@exclude *)
  ALLEGRO_KEY_M		= 13; (***<@exclude *)
  ALLEGRO_KEY_N		= 14; (***<@exclude *)
  ALLEGRO_KEY_O		= 15; (***<@exclude *)
  ALLEGRO_KEY_P		= 16; (***<@exclude *)
  ALLEGRO_KEY_Q		= 17; (***<@exclude *)
  ALLEGRO_KEY_R		= 18; (***<@exclude *)
  ALLEGRO_KEY_S		= 19; (***<@exclude *)
  ALLEGRO_KEY_T		= 20; (***<@exclude *)
  ALLEGRO_KEY_U		= 21; (***<@exclude *)
  ALLEGRO_KEY_V		= 22; (***<@exclude *)
  ALLEGRO_KEY_W		= 23; (***<@exclude *)
  ALLEGRO_KEY_X		= 24; (***<@exclude *)
  ALLEGRO_KEY_Y		= 25; (***<@exclude *)
  ALLEGRO_KEY_Z		= 26; (***<@exclude *)

  ALLEGRO_KEY_0		= 27; (***<@exclude *)
  ALLEGRO_KEY_1		= 28; (***<@exclude *)
  ALLEGRO_KEY_2		= 29; (***<@exclude *)
  ALLEGRO_KEY_3		= 30; (***<@exclude *)
  ALLEGRO_KEY_4		= 31; (***<@exclude *)
  ALLEGRO_KEY_5		= 32; (***<@exclude *)
  ALLEGRO_KEY_6		= 33; (***<@exclude *)
  ALLEGRO_KEY_7		= 34; (***<@exclude *)
  ALLEGRO_KEY_8		= 35; (***<@exclude *)
  ALLEGRO_KEY_9		= 36; (***<@exclude *)

  ALLEGRO_KEY_PAD_0		= 37; (***<@exclude *)
  ALLEGRO_KEY_PAD_1		= 38; (***<@exclude *)
  ALLEGRO_KEY_PAD_2		= 39; (***<@exclude *)
  ALLEGRO_KEY_PAD_3		= 40; (***<@exclude *)
  ALLEGRO_KEY_PAD_4		= 41; (***<@exclude *)
  ALLEGRO_KEY_PAD_5		= 42; (***<@exclude *)
  ALLEGRO_KEY_PAD_6		= 43; (***<@exclude *)
  ALLEGRO_KEY_PAD_7		= 44; (***<@exclude *)
  ALLEGRO_KEY_PAD_8		= 45; (***<@exclude *)
  ALLEGRO_KEY_PAD_9		= 46; (***<@exclude *)

  ALLEGRO_KEY_F1		= 47; (***<@exclude *)
  ALLEGRO_KEY_F2		= 48; (***<@exclude *)
  ALLEGRO_KEY_F3		= 49; (***<@exclude *)
  ALLEGRO_KEY_F4		= 50; (***<@exclude *)
  ALLEGRO_KEY_F5		= 51; (***<@exclude *)
  ALLEGRO_KEY_F6		= 52; (***<@exclude *)
  ALLEGRO_KEY_F7		= 53; (***<@exclude *)
  ALLEGRO_KEY_F8		= 54; (***<@exclude *)
  ALLEGRO_KEY_F9		= 55; (***<@exclude *)
  ALLEGRO_KEY_F10		= 56; (***<@exclude *)
  ALLEGRO_KEY_F11		= 57; (***<@exclude *)
  ALLEGRO_KEY_F12		= 58; (***<@exclude *)

  ALLEGRO_KEY_ESCAPE		= 59; (***<@exclude *)
  ALLEGRO_KEY_TILDE		= 60; (***<@exclude *)
  ALLEGRO_KEY_MINUS		= 61; (***<@exclude *)
  ALLEGRO_KEY_EQUALS		= 62; (***<@exclude *)
  ALLEGRO_KEY_BACKSPACE	= 63; (***<@exclude *)
  ALLEGRO_KEY_TAB		= 64; (***<@exclude *)
  ALLEGRO_KEY_OPENBRACE	= 65; (***<@exclude *)
  ALLEGRO_KEY_CLOSEBRACE	= 66; (***<@exclude *)
  ALLEGRO_KEY_ENTER		= 67; (***<@exclude *)
  ALLEGRO_KEY_SEMICOLON		= 68; (***<@exclude *)
  ALLEGRO_KEY_QUOTE		= 69; (***<@exclude *)
  ALLEGRO_KEY_BACKSLASH	= 70; (***<@exclude *)
  ALLEGRO_KEY_BACKSLASH2	= 71; (***<@exclude DirectInput calls this DIK_OEM_102: "< > | on UK/Germany keyboards" *)
  ALLEGRO_KEY_COMMA		= 72; (***<@exclude *)
  ALLEGRO_KEY_FULLSTOP		= 73; (***<@exclude *)
  ALLEGRO_KEY_SLASH		= 74; (***<@exclude *)
  ALLEGRO_KEY_SPACE		= 75; (***<@exclude *)

  ALLEGRO_KEY_INSERT		= 76; (***<@exclude *)
  ALLEGRO_KEY_DELETE		= 77; (***<@exclude *)
  ALLEGRO_KEY_HOME		= 78; (***<@exclude *)
  ALLEGRO_KEY_END		= 79; (***<@exclude *)
  ALLEGRO_KEY_PGUP		= 80; (***<@exclude *)
  ALLEGRO_KEY_PGDN		= 81; (***<@exclude *)
  ALLEGRO_KEY_LEFT		= 82; (***<@exclude *)
  ALLEGRO_KEY_RIGHT		= 83; (***<@exclude *)
  ALLEGRO_KEY_UP		= 84; (***<@exclude *)
  ALLEGRO_KEY_DOWN		= 85; (***<@exclude *)

   ALLEGRO_KEY_PAD_SLASH	= 86; (***<@exclude *)
   ALLEGRO_KEY_PAD_ASTERISK	= 87; (***<@exclude *)
   ALLEGRO_KEY_PAD_MINUS	= 88; (***<@exclude *)
   ALLEGRO_KEY_PAD_PLUS	= 89; (***<@exclude *)
   ALLEGRO_KEY_PAD_DELETE	= 90; (***<@exclude *)
   ALLEGRO_KEY_PAD_ENTER	= 91; (***<@exclude *)

   ALLEGRO_KEY_PRINTSCREEN	= 92; (***<@exclude *)
   ALLEGRO_KEY_PAUSE		= 93; (***<@exclude *)

   ALLEGRO_KEY_ABNT_C1	= 94; (***<@exclude *)
   ALLEGRO_KEY_YEN		= 95; (***<@exclude *)
   ALLEGRO_KEY_KANA		= 96; (***<@exclude *)
   ALLEGRO_KEY_CONVERT	= 97; (***<@exclude *)
   ALLEGRO_KEY_NOCONVERT	= 98; (***<@exclude *)
   ALLEGRO_KEY_AT		= 99; (***<@exclude *)
   ALLEGRO_KEY_CIRCUMFLEX	= 100; (***<@exclude *)
   ALLEGRO_KEY_COLON2	= 101; (***<@exclude *)
   ALLEGRO_KEY_KANJI		= 102; (***<@exclude *)

   ALLEGRO_KEY_PAD_EQUALS	= 103;	(***<@exclude MacOS X *)
   ALLEGRO_KEY_BACKQUOTE	= 104;	(***<@exclude MacOS X *)
   ALLEGRO_KEY_SEMICOLON2	= 105;	(***<@exclude MacOS X -- TODO: ask lillo what this should be *)
   ALLEGRO_KEY_COMMAND	= 106;	(***<@exclude MacOS X *)

   ALLEGRO_KEY_BACK = 107;        (***<@exclude Android back key *)
   ALLEGRO_KEY_VOLUME_UP = 108; (***<@exclude *)
   ALLEGRO_KEY_VOLUME_DOWN = 109; (***<@exclude *)

   { Android game keys }
   ALLEGRO_KEY_SEARCH       = 110; (***<@exclude *)
   ALLEGRO_KEY_DPAD_CENTER  = 111; (***<@exclude *)
   ALLEGRO_KEY_BUTTON_X     = 112; (***<@exclude *)
   ALLEGRO_KEY_BUTTON_Y     = 113; (***<@exclude *)
   ALLEGRO_KEY_DPAD_UP      = 114; (***<@exclude *)
   ALLEGRO_KEY_DPAD_DOWN    = 115; (***<@exclude *)
   ALLEGRO_KEY_DPAD_LEFT    = 116; (***<@exclude *)
   ALLEGRO_KEY_DPAD_RIGHT   = 117; (***<@exclude *)
   ALLEGRO_KEY_SELECT       = 118; (***<@exclude *)
   ALLEGRO_KEY_START        = 119; (***<@exclude *)
   ALLEGRO_KEY_BUTTON_L1    = 120; (***<@exclude *)
   ALLEGRO_KEY_BUTTON_R1    = 121; (***<@exclude *)
   ALLEGRO_KEY_BUTTON_L2    = 122; (***<@exclude *)
   ALLEGRO_KEY_BUTTON_R2    = 123; (***<@exclude *)
   ALLEGRO_KEY_BUTTON_A     = 124; (***<@exclude *)
   ALLEGRO_KEY_BUTTON_B     = 125; (***<@exclude *)
   ALLEGRO_KEY_THUMBL       = 126; (***<@exclude *)
   ALLEGRO_KEY_THUMBR       = 127; (***<@exclude *)

   ALLEGRO_KEY_UNKNOWN      = 128; (***<@exclude *)

   { All codes up to before ALLEGRO_KEY_MODIFIERS can be freely
     assignedas additional unknown keys, like various multimedia
     and application keys keyboards may have.
   }

   ALLEGRO_KEY_MODIFIERS	= 215; (***<@exclude *)

   ALLEGRO_KEY_LSHIFT	= 215; (***<@exclude *)
   ALLEGRO_KEY_RSHIFT	= 216; (***<@exclude *)
   ALLEGRO_KEY_LCTRL	= 217; (***<@exclude *)
   ALLEGRO_KEY_RCTRL	= 218; (***<@exclude *)
   ALLEGRO_KEY_ALT		= 219; (***<@exclude *)
   ALLEGRO_KEY_ALTGR	= 220; (***<@exclude *)
   ALLEGRO_KEY_LWIN		= 221; (***<@exclude *)
   ALLEGRO_KEY_RWIN		= 222; (***<@exclude *)
   ALLEGRO_KEY_MENU		= 223; (***<@exclude *)
   ALLEGRO_KEY_SCROLLLOCK = 224; (***<@exclude *)
   ALLEGRO_KEY_NUMLOCK	= 225; (***<@exclude *)
   ALLEGRO_KEY_CAPSLOCK	= 226; (***<@exclude *)

  ALLEGRO_KEY_MAX		= 227; (***<@exclude *)

{ Shift keys flags. }
  ALLEGRO_KEYMOD_SHIFT	= $0001; (***<@exclude *)
  ALLEGRO_KEYMOD_CTRL	= $0002; (***<@exclude *)
  ALLEGRO_KEYMOD_ALT 	= $0004; (***<@exclude *)
  ALLEGRO_KEYMOD_LWIN	= $0008; (***<@exclude *)
  ALLEGRO_KEYMOD_RWIN	= $0010; (***<@exclude *)
  ALLEGRO_KEYMOD_MENU	= $0020; (***<@exclude *)
  ALLEGRO_KEYMOD_COMMAND	= $0040; (***<@exclude *)
  ALLEGRO_KEYMOD_SCROLOCK	= $0100; (***<@exclude *)
  ALLEGRO_KEYMOD_NUMLOCK	= $0200; (***<@exclude *)
  ALLEGRO_KEYMOD_CAPSLOCK	= $0400; (***<@exclude *)
  ALLEGRO_KEYMOD_INALTSEQ	= $0800; (***<@exclude *)
  ALLEGRO_KEYMOD_ACCENT1	= $1000; (***<@exclude *)
  ALLEGRO_KEYMOD_ACCENT2	= $2000; (***<@exclude *)
  ALLEGRO_KEYMOD_ACCENT3	= $4000; (***<@exclude *)
  ALLEGRO_KEYMOD_ACCENT4	= $8000; (***<@exclude *)
