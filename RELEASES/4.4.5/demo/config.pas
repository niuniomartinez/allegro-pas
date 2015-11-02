UNIT config;
(* Program: Demo game for the Allegro.pas library.
 * Description: Manages the game configuration.
 *		It's a good idea to keep all the configuration values at the
 *		same place.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

INTERFACE



  VAR
  (* Variables containing the configuration. *)
    DoIntro: BOOLEAN; { Shows the intro animation? }
    ScaleSc: DOUBLE; { Scale factor of the game screen. }
    Cheat: BOOLEAN;  { If true, the game is too much easy. }
    FullScreen: BOOLEAN; { If false, windowed. }

(* Gets the configuration values and puts them in its variables. *)
  PROCEDURE GetConfiguration;

IMPLEMENTATION

  USES
    sysutils,
    allegro;



(* Gets the configuration values and puts them in its variables. *)
  PROCEDURE GetConfiguration;
  VAR
    Path: STRING;
    Cnt: INTEGER;
  BEGIN
    Cheat := FALSE;
  { Set an additional config data file which is in the current directory.  So,
    get the executable path (returned by PARAMSTR (0)) and replace the filename
    with the configuration's one. }
    Path :=  ExtractFilePath (PARAMSTR (0)) + 'demo.cfg';
  { Now we can override the file. }
    al_override_config_file (Path);
  { Load the configuration values.  Note: Allegro gets some values from the
    configuration file for internal purposes.  Read the documentation. }
    DoIntro := (al_get_config_int ('', 'jumpstart', 0) = 0);
    FullScreen := (al_get_config_int ('graphics', 'fullscreen', 0) <> 0);
  { Check the command line parameters. }
    FOR Cnt := 1 TO PARAMCOUNT DO
    BEGIN
      IF PARAMSTR (Cnt) = '-jumpstart' THEN
	DoIntro := FALSE
      ELSE IF PARAMSTR (Cnt) = '-cheat' THEN
	Cheat := TRUE
      ELSE IF PARAMSTR (Cnt) = '-windowed' THEN
	FullScreen := FALSE
      ELSE IF PARAMSTR (Cnt) = '-fullscreen' THEN
	FullScreen := TRUE;
    END;
  END;

END.
