<?php
## Módulo para obtener información vía RSS.
## Utiliza caché.



## El módulo.
  class LECTORSS
  {
  ## Constructor.
    public function __construct ()
    {
    # Se asegura de que el módulo "caché" ha sido cargado.
      if (!isset (AGORA::$Modulos->cache))
	AGORA::CargaModulo ('cache');
    }



  ## Obtiene datos del RSS y devuelve la información en forma de diccionario.
  ## Los índices son:
  ## * Nombre: Nombre del RSS.
  ## * URL: Enlace a la fuente.
  ## * FechaActualizacion: Fecha de la última actualización en tiempo UNIX.
  ## * Items: Diccionario con el contenido RSS en forma de diccionario:
  ##    + Titulo: Título.
  ##    + Fecha: En tiempo UNIX.
  ##    + URL: Enlace asociado.
  ##    + Contenido: Contenido sin formatear.
    public function Obtiene ($URL, $MaxItems = 0, $FiltroXSS = FALSE)
    {
      $Resultado = array ();
      $Noticias = @simplexml_load_string (
	AGORA::$Modulos->cache->LeeArchivo ($URL)
      );
      if ($Noticias === FALSE or !isset ($Noticias->channel))
	throw new Exception ('El origen no es un RSS');
      $Resultado['Nombre'] = (string)$Noticias->channel->title;
      $Resultado['URL'] = (string)$Noticias->channel->link;
      # $Resultado['CodigoIdioma'] = $Noticias->language;
      $Resultado['FechaActualizacion'] = strtotime ($Noticias->channel->lastBuildDate);
    /* ->channel->item no es una lista (array) así que no puede usarse array_slice aquí.
     * TODO: Quizá modificar el bucle "foreach" por un "while" permita hacerlo de forma más eficiente.
      if ($MaxItems > 0)
	$Noticias->channel->item = array_slice ($Noticias->channel->item, 0, $MaxItems);
     */
      foreach ($Noticias->channel->item as $Noticia):
	$Resultado['Items'][] = array (
	  'GUID' => (string)$Noticia->guid,
	  'Titulo' => (string)$Noticia->title,
	  'URL' => (string)$Noticia->link,
	  'Contenido' => (string)$Noticia->description,
	  'Fecha' => strtotime ($Noticia->pubDate)
	);
      endforeach;
      if ($MaxItems > 0)
	$Resultado['Items'] = array_slice ($Resultado['Items'], 0, $MaxItems);
      if ($FiltroXSS)
	$Resultado = AGORA::FiltroXSS ($Resultado);
      return $Resultado;
    }
  };
