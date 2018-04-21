PROGRAM ex_config;
(*
 *    Example program for the Allegro library.
 *
 *    Test config file reading and writing.
 *)
(*
  Copyright (c) 2012-2018 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  USES
    Allegro5, Common,
    sysutils;

  PROCEDURE Test (Name: STRING; Expr: BOOLEAN);
  BEGIN
    IF Expr THEN
      LogWriteLn (Format (' PASS - %s', [Name]))
    ELSE
      LogWriteLn (Format ('!FAIL - %s', [Name]))
  END;



  VAR
    Cfg: ALLEGRO_CONFIGptr;
    Value: STRING;
    IteratorSection: ALLEGRO_CONFIG_SECTIONptr;
    IteratorEntry: ALLEGRO_CONFIG_ENTRYptr;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLog;

  Cfg := al_load_config_file ('data/sample.cfg');
  IF Cfg = NIL THEN
    AbortExample ('Couldn''t load data/sample.cfg');

  Value := al_get_config_value (Cfg, '', 'old_var');
  Test ('global var', Value = 'old global value');

  Value := al_get_config_value (Cfg, 'section', 'old_var');
  Test ('section var', Value = 'old section value');

  Value := al_get_config_value (Cfg, '', 'mysha.xpm');
  Test ('long value', Length (Value) = 1394);

{ Test removing. }
  al_set_config_value (Cfg, 'empty', 'key_remove', 'to be removed');
  al_remove_config_key (Cfg, 'empty', 'key_remove');

  al_set_config_value (Cfg, 'schrödinger', 'box', 'cat');
  al_remove_config_section (Cfg, 'schrödinger');

{ Test whether iterating through our whole sample.cfg returns all
  sections and entries, in order. }

  Value := al_get_first_config_section (Cfg, IteratorSection);
  Test ('section1', Value = '');

  Value := al_get_first_config_entry (Cfg, Value, IteratorEntry);
  Test ('entry1', Value = 'old var');

  Value := al_get_next_config_entry (IteratorEntry);
  Test ('entry2', Value = 'mysha.xpm');

  Value := al_get_next_config_entry (IteratorEntry);
  Test ('entry3', Value = '');

  Value := al_get_next_config_section (IteratorSection);
  Test ('section2', Value = 'section');

  Value := al_get_first_config_entry (Cfg, Value, IteratorEntry);
  Test ('entry4', Value = 'old var');

  Value := al_get_next_config_entry (IteratorEntry);
  Test ('entry5', Value = '');

  Value := al_get_next_config_section (IteratorSection);
  Test ('section3', Value <> '');

  Value := al_get_first_config_entry (Cfg, Value, IteratorEntry);
  Test ('entry6', Value <> '');

  Value := al_get_next_config_entry (IteratorEntry);
  Test ('entry7', Value = '');

  Value := al_get_next_config_section (IteratorSection);
  Test ('empty', Value = 'empty');

  Value := al_get_next_config_entry (IteratorEntry);
  Test ('empty entry', Value = '');

  Value := al_get_next_config_section (IteratorSection);
  Test ('section4', Value = '');



  al_set_config_value (Cfg, '', 'new_var', 'new value');
  al_set_config_value (Cfg, 'section', 'old_var', 'new value');

  Test ('save_config', al_save_config_file ('test.cfg', Cfg));

  LogWriteLn ('Done');

  al_destroy_config (Cfg);

  CloseLog (TRUE);

{ TODO: return passed ? 0 : 1; }
END.

