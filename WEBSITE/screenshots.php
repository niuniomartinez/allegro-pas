<?php
## Pantallazos.

# Cargar el marco.
  include_once ('sys/config.php');
  include_once ('sys/agora.php');

  try {
  # Un poco incómodo, pero es que no puede obtenerse la lista desde SF.
    $Pantallazos = array (
      array ('Archivo' => '239790.jpg', 'Desc' => 'Alex the Allegator is ready... The Allegro.pas Demo Game!'),
      array ('Archivo' => 'Captura de pantalla - 171212 - 18:55:02.png', 'Desc' => 'Map editor of the Demo Game'),
      array ('Archivo' => 'screen_1.png', 'Desc' => 'Example use of 3D routines')
    );

  # Crear página de salida.
    AGORA::CargaVista ('pagina', array (
      'NombrePag' => 'Screenshots',
      'Contenido' => AGORA::CargaVista ('screenshots', array (
	'Imagenes' => $Pantallazos
      ), TRUE)
    ));
  }
  catch (Exception $Error) {
    AGORA::Error ("Error: {$Error->getMessage ()}");
  }
