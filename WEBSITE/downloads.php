<?php
## Página de descarga de últimas versiones.

# Cargar el marco.
  include_once ('sys/config.php');
  include_once ('sys/agora.php');

# Descripción de la página (puedes dejarlo como está).
  $Descripcion = 'Here you can download the latest stable releases of '.AGORA::Config ('nombre', '').' and documentation.

Instructions on how to install or build '. AGORA::Config ('nombre', '') .' for each of the supported platforms are included in the packages, either in the [b]README[/b] files, or in the [b]docs/[/b] directory.

You can find more versions and more formats (zip, .tar.gz, .tar.bz2) in the [url="https://sourceforge.net/projects/allegro-pas/files/"]SourceForge\'s downloading page[/url].  Also you can find [i]WIP[/i] versions in the [url="svn.php"]SVN repository[/url].';

# Lista de archivos.  La descripción puede usar bbCode.
  $Archivos = array (
    'lib' => array (
      array ('Archivo' => '4.4.5/allegro-pas-demo-4.4.5-bin-win.zip',
        'Desc' => 'Compiled demo game for Windows 32bit', 'Tamanno' => '1.4 MiB'),
      array ('Archivo' => '4.4.5/allegro.pas-4.4.5-bin-win.zip',
        'Desc' => 'Source code, alleg44.dll file and compiled tools for windows. [b]Not Delphi[/b]', 'Tamanno' => '3.6 MiB'),
      array ('Archivo' => '4.4.5/allegro.pas-4.4.5-src-pas.tar.gz',
        'Desc' => 'Source code for all platforms. tar/gz package. [b]Not Delphi[/b]', 'Tamanno' => '2.8 MiB'),
#      array ('Archivo' => 'allegro-pas/4.4.4/allegro.pas-4.4.4-src-pas.tar.bz2',
#        'Desc' => 'Source code for all platforms. tar/bz2 package. [b]Not Delphi[/b]', 'Tamanno' => '2.6 MB'),
      array ('Archivo' => '4.4.5/allegro.pas-4.4.5-src-pas.zip',
        'Desc' => 'Source code for all platforms. ZIP package. [b]Not Delphi[/b]', 'Tamanno' => '2.8 MiB')
    ),
    'doc' => array (
      array ('Archivo' => '4.4.5/allegro-pas-4.4.5-151002-docs-html.tar.gzip',
             'Desc' => 'Documentation in HTML format. tar/gz package.', 'Tamanno' => '347.8 KiB'),
      array ('Archivo' => '4.4.5/allegro-pas-4.4.5-151002-docs-html.zip',
             'Desc' => 'Documentation in HTML format.  Zip package.', 'Tamanno' => '402.4 KiB')
    )
  );
# Creamos la página.
  try {
    AGORA::CargaModulo ('formato'); # Para el bbCode.
    AGORA::CargaVista ('pagina', array (
      'NombrePag' => 'Downloads',
      'Contenido' => AGORA::CargaVista ('downloads', array (
	'Descripcion' => AGORA::$Modulos->formato->bbCode ($Descripcion),
	'Archivos' => $Archivos
      ), TRUE)
    ));
  }
  catch (Exception $Error) {
    AGORA::Error ("Error: {$Error->getMessage ()}");
  }
