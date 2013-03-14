<?php
## Página de descripción del SVN.

# Cargar el marco.
  include_once ('sys/config.php');
  include_once ('sys/agora.php');

# Crear página de salida.  Usa bbCode para formatear.
# Si prefieres no usar bbCode y usar HTML, eliminar la carga del módulo
# "formato" y la llamada en la carga de la vista.
  $SVN_URL = 'https://'.AGORA::Config ('SFname', '').'.svn.sourceforge.net/svnroot/'.AGORA::Config ('SFname', '');
  $Contenido = AGORA::Config ('nombre', '')."'s source code is maintained in a SVN repository.  You can get clients for most platforms at [url]https://subversion.apache.org/[/url], and there is good [url=\"https://subversion.apache.org/docs/\"]documentation[/url].

To get the current WIP version, use the next command to:
[code]svn co {$SVN_URL}/TRUNK allegro-pas[/code]

Note that only developers can commit new content to the SVN.

To get older versions, look at the [b]RELEASES[/b] subdirectory:
[code]svn co {$SVN_URL}/RELEASES/<version-num> allegro-pas[/code]
Where [i]<version-num>[/i] is the version you're looking for (eg: 4.4.2, 4.4.3, etc).  Note that not all old versions are available.  You can find those at the [url=\"http://sourceforge.net/projects/allegro-pas/files\"]downloads page[/url].

You can get, also, the unstable version 5 using the next command:
[code]svn co {$SVN_URL}/BRANCHES/5.0 allegro-pas.5[/code]

You can browse the source code online at [url]http://sourceforge.net/p/".AGORA::Config ('SFname', '')."/code/[/url]";

# Creamos la página.
  try {
    AGORA::CargaModulo ('formato');
    AGORA::CargaVista ('pagina', array (
      'NombrePag' => 'SVN',
      'Contenido' => AGORA::$Modulos->formato->bbCode ($Contenido)
    ));
  }
  catch (Exception $Error) {
    AGORA::Error ("Error: {$Error->getMessage ()}");
  }
