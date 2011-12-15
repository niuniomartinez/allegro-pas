UNIT Allegro5;
(*<Wrapper of the Allegro 5 core library. *)
(*TODO: License. *)

{$include allegro.cfg}

INTERFACE

CONST
(* Name of the dynamicly linked unit.

  @bold(TODO:) This should be defined at the @code(allegro.cfg) file as it's different in each platform.
 *)
  ALLEGRO_LIB_NAME = 'liballegro.so.5.0';

(* Major version of Allegro. *)
  ALLEGRO_VERSION      =   5;
(* Minor version of Allegro. *)
  ALLEGRO_SUB_VERSION  =   0;
(* Revision number of Allegro. *)
  ALLEGRO_WIP_VERSION  =   5;
(* Not sure we need it, but since @link(ALLEGRO_VERSION_STR) contains it:
   0 = SVN
   1 = first release
   2... = hotfixes?

   Note x.y.z (= x.y.z.0) has release number 1, and x.y.z.1 has release
   number 2, just to confuse you.
*)
  ALLEGRO_RELEASE_NUMBER = 0;
(* Packs version number in a simple @code(DWORD) number.
   @seealso(al_install_system)
 *)
  ALLEGRO_VERSION_INT  = (
		          (ALLEGRO_VERSION SHL 24)
		       OR (ALLEGRO_SUB_VERSION SHL 16)
		       OR (ALLEGRO_WIP_VERSION SHL  8)
		       OR  ALLEGRO_RELEASE_NUMBER
  );



(* Initialize the Allegro system.  No other Allegro functions can be called before this (with one or two exceptions).
   @param(version Should always be set to @link(ALLEGRO_VERSION_INT).)
   @param(atexit_ptr If it is non-@nil, and if hasn't been done already, @link(al_uninstall_system) will be registered as an @code(atexit) function.

   @bold(TODO:) Since Pascal doesn't has an @code(atexit) function @(it's a C function with similar functionality than @code(FINALIZATION) section@) may be it should be set always to @nil.  Ask to Allegro's developers about it.)
   @returns(@true if Allegro was successfully initialized by this function call @(or already was initialized previously@), @false if Allegro cannot be used.)
   @seealso(al_init)
 *)
  FUNCTION al_install_system (
    version: DWORD;
    atexit_ptr: POINTER
  ): BOOLEAN; CDECL;

(* Closes down the Allegro system.

   @bold(Note:) @code(al_uninstall_system) can be called without a corresponding @link(al_install_system) call.
 *)
  PROCEDURE al_uninstall_system; CDECL;

(* Like @link(al_install_system), but automatically passes in the version and passes @nil as the @code(atexit_ptr) parameter.
   @seealso(al_install_system)
 *)
  FUNCTION al_init: BOOLEAN;

(* Returns the (compiled) version of the Allegro library.

  The version number is packed into a single integer as groups of 8 bits in the form @code(@(major SHL 24@) OR @(minor SHL 16@) OR @(revision SHL 8@) OR release).

  You can use code like this to extract them:
@longcode(#
VAR
  Version: DWORD;
  Major, Minor, Revision, Release: INTEGER;
BEGIN
  version := al_get_allegro_version;
  major := version SHR 24;
  minor := (version SHR 16) AND $FF;
  revision := (version SHR 8) AND $FF;
  release := version AND $FF;
END;
#)

  The release number is 0 for an unofficial version and 1 or greater for an official release. For example "5.0.2[1]" would be the (first) official 5.0.2 release while "5.0.2[0]" would be a compile of a version from the "5.0.2" branch before the official release.
*)
  FUNCTION al_get_allegro_version: DWORD; CDECL;

IMPLEMENTATION

  FUNCTION al_install_system (version: DWORD; atexit_ptr: POINTER): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_uninstall_system; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_init: BOOLEAN;
  BEGIN
    al_init := al_install_system (ALLEGRO_VERSION_INT, NIL);
  END;

  FUNCTION al_get_allegro_version: DWORD; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

FINALIZATION
  al_uninstall_system;
END.
