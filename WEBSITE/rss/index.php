<?php
## Sustitución del RSS de Gesbit.

# Cargar el marco.
  include_once ('../sys/config.php');
  include_once ('../sys/agora.php');

  try {
  # Obtener las noticias.
    AGORA::CargaModulo ('cache');
    $Origen = "http://sourceforge.net/p/allegro-pas/news/feed";
    echo AGORA::$Modulos->cache->LeeArchivo ($Origen);
  }
  catch (Exception $Error) {
    if ($Error->getCode () == 404)
      AGORA::Error404 ($Origen, 'Can\'t get news from SourceForge.');
    AGORA::Error ("Error: {$Error->getMessage ()}");
  }
