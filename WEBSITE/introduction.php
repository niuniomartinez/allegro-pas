<?php
## Página de introducción.

# Cargar el marco.
  include_once ('sys/config.php');
  include_once ('sys/agora.php');

# Crear página de salida.  Usa bbCode para formatear.
# Si prefieres no usar bbCode y usar HTML, eliminar la carga del módulo
# "formato" y la llamada en la carga de la vista.
  $Contenido = <<<'intro'
Allegro.pas is a wrapper to allow Pascal language to use the [url="http://alleg.sf.net/"]Allegro game programming library[/url] to create your own games in good old Pascal or the modern Object Pascal programming languages.

Allegro.pas allows you to use sprites, play sound and music, draw directly on the screen or on any-size memory bitmaps, get user input from keyboard, joystick and/or mouse, create 3D graphics, define a GUI and almost anything you need to create your own games.  And you can do it in good old Pascal or the modern Object Pascal.  Also, if you do it the right way, your program will run in Windows and Linux with few or not changes!  It may work even in MacOS systems, but we didn't test it yet. :-/

You can download the [url="http://sourceforge.net/projects/allegro-pas/files/allegro-pas-bin/4.4.4/allegro.pas-demo-4.4.4-bin-win.zip/download"]Game Demo[/url] (Windows and WineHQ), a simple platform game that shows some of the capabilities of Allegro.pas.  It's a very simple game that includes smooth scroll, animated sprites, scoring, music, sound, etc.  It has also a [b]map editor[/b]!  So it's a great example to start with.  Such a simple game isn't enough to show everything that Allegro can offer.  If you want to know more about what you can do with Allegro.pas, read the [url="docs/"]API documentation[/url] or visit the [url="wiki/"]Wiki[/url].

[url="download.php"][img]wiki/lib/exe/fetch.php?media=download.png[/img][/url]
intro;

  try {
    AGORA::CargaModulo ('formato');
    AGORA::CargaVista ('pagina', array (
      'NombrePag' => 'Introduction',
      'Contenido' => AGORA::$Modulos->formato->bbCode ($Contenido)
    ));
  }
  catch (Exception $Error) {
    AGORA::Error ("Error: {$Error->getMessage ()}");
  }
