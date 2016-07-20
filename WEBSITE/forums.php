<?php
## Página que muestra los foros donde me muevo.

# Cargar el marco.
  include_once ('sys/config.php');
  include_once ('sys/agora.php');

# Crear página de salida.  Usa bbCode para formatear.
# Si prefieres no usar bbCode y usar HTML, eliminar la carga del módulo
# "formato" y la llamada en la carga de la vista.
  $Contenido = <<<'forums'
There are some forums you can visit if you need help with Allegro.pas, game programming or Pascal:[list]
  [*][b][url="http://www.pascalgamedevelopment.com/"]Pascal Game Development[/url][/b]  A place with news and a forum for game makers that use Pascal, Object Pascal and derivated (Delphi, Oxygene, Smart Mobile Studio...).
  [*][b][url="http://www.allegro.cc/"]Allegro community[/url][/b]  The official website of the Allegro community.  Most of it for C and C++ users, but support all other languages too.
  [*][b][url="http://www.freepascal.org/"]Free Pascal community[/url][/b]  The official website of the Free Pascal project.  They have forums and mailing lists.
  [*][b][url="http://www.lazarus.freepascal.org/"]Lazarus community[/url][/b]  The official website of the Lazarus project.  They have forums and mailing lists.
[/list]
Since I'm Spanish, I also visit:[list]
  [*][b][url="http://www.clubdelphi.com/"]Club Delphi[/url][/b]  Spanish Delphi community.
[/list]
forums;

# Creamos la página.
  try {
    AGORA::CargaModulo ('formato');
    AGORA::CargaVista ('pagina', array (
      'NombrePag' => 'Forums',
      'Contenido' => AGORA::$Modulos->formato->bbCode ($Contenido)
    ));
  }
  catch (Exception $Error) {
    AGORA::Error ("Error: {$Error->getMessage ()}");
  }
