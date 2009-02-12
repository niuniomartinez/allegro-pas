(* To test the library. *)
PROGRAM Test;

{ Defines the frame.  From the JEDI-OpenGL library. }
{$MODE Delphi}
{$MACRO ON}
{$H+}

{$IFDEF Windows}
  {$DEFINE extdecl := stdcall}
{$ELSE}
  {$DEFINE extdecl := cdecl}
  {$IFDEF MorphOS}
    {$INLINE ON}
    {$DEFINE GL_UNIT}
  {$ELSE}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}



USES
  dynlibs;



CONST
{$IFDEF Windows}
  LibName = 'alleg42.dll';
{$ELSE}
  LibName = 'liballeg-4.2.2.so';
{$ENDIF}



VAR
(* Function from the library. *)
  allegro_message: PROCEDURE (CONST msg: PCHAR); CDECL;



(* To make calling ease. *)
PROCEDURE al_message (CONST msg: STRING);
BEGIN
  allegro_message (PCHAR (msg));
END;



VAR
  AllegroLibrary: TLibHandle;
BEGIN
{ Loads the library. }
  AllegroLibrary := LoadLibrary (PChar(LibName));
{ Gets the function address. }
  @allegro_message := GetProcAddress (AllegroLibrary, 'allegro_message');
{ Uses the function.  Spaces to separate.  If a carriage return added then
  it doesn't work(?). }
  al_message ('Hello, World!    ');
{ Releases resources. }
  IF (AllegroLibrary <> 0) THEN
    FreeLibrary (AllegroLibrary);
END.
