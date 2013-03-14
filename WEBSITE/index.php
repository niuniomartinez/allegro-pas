<?php
## Página inicial del sitio.
## Muestra las noticias de SourceForge.

# Cargar el marco.
  include_once ('sys/config.php');
  include_once ('sys/agora.php');

  try {
    AGORA::CargaModulo ('formato'); # Ayudas de formateo.  bbCode.

    $NombreSF = AGORA::Config ('SFname', '');
    if ($NombreSF == '')
      throw new Exception ('¡Debe definir el nombre de SourceForge!');
  # Obtener las noticias.
    AGORA::CargaModulo ('lectorss', 'LectorRSS');
    $Origen = "http://sourceforge.net/p/$NombreSF/news/feed";
    $Noticias = AGORA::$Modulos->LectorRSS->Obtiene ($Origen, 5);
  # Formatear noticias.
    foreach ($Noticias['Items'] as $Key => $Item):
      $Item['Fecha'] = date ('d/m/Y h:m', $Item['Fecha']);
      $Item['Contenido'] = AGORA::$Modulos->formato->SeparaParrafos (
	AGORA::$Modulos->formato->bbCodeLinea (
	  str_replace ('\\', '', $Item['Contenido'])
	)
      );
      $Noticias['Items'][$Key] = $Item;
    endforeach;
  # Resumen de la descripción del proyecto:
      $Descripcion = 'Allegro.pas is a wrapper to allow Pascal language to use the [url="http://alleg.sf.net/"]Allegro game programming library[/url] to create your own games in good old Pascal or the modern Object Pascal programming languages.  Read more details on the [url="introduction.php"]introductory page[/url].';
  # Crear página de salida.
    AGORA::CargaVista ('pagina', array (
      'NombrePag' => 'News',
      'Contenido' => AGORA::CargaVista ('indice', array (
	'Descripcion' => AGORA::$Modulos->formato->bbCode ($Descripcion),
	'Noticias' => $Noticias,
	'UltimaActualizacion' => date ('d/m/Y h:m', $Noticias['FechaActualizacion'])
      ), TRUE)
    ));
  }
  catch (Exception $Error) {
    if ($Error->getCode () == 404)
      AGORA::Error404 ($Origen, 'Can\'t get news from SourceForge.');
    AGORA::Error ("Error: {$Error->getMessage ()}");
  }
