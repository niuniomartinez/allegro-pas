UNIT albase;
(*<Defines basic stuff. *)

{$include _alcfg.inc}

INTERFACE

CONST
(* Defined to the major version of Allegro.  From a version number like 4.1.16,
   this would be defined to the integer 4. *)
  AL_VERSION = 4;
(* Defined to the middle version of Allegro.  From a version number like
   4.1.16, this would be defined to the integer 1. *)
  AL_SUB_VERSION = 4;
(* Defined to the minor version of Allegro.pas.  From a version number like
   4.1.16, this would be defined to the integer 16. *)
  AL_PAS_VERSION = 9;
(* Defined to TRUE if current version is a BETA version.  A BETA version is a
   test version and may be uncomplete or untested. *)
  AL_PAS_IS_BETA = TRUE;
(* Defined to a text string containing all version numbers and maybe some
   additional text. *)
  AL_PAS_VERSION_STR = '4.4.9 alpha';

TYPE
(* To define callback variables and parameters. *)
  AL_CALLBACK_PROC = PROCEDURE;

IMPLEMENTATION

END.
