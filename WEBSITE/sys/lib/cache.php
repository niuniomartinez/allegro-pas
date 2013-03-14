<?php
## Un sistema simple para gestionar un caché.

## Directorio "caché".
  define ('DIR_CACHE', realpath (AG_DIR.'cache').'/');

## El módulo.
  class CACHE
  {
  ## Crea el nombre de caché del archivo, incluyendo la ruta.
    private function NombreChache ($NombreOrigen)
    {
      return DIR_CACHE.str_replace (
	array ('/', '\\', ' ', ':', '&', '?', '='),
	'_',
	$NombreOrigen
      );
    }



  ## Dado un nombre de archivo, comprueba si está en caché devolviendo .T. o
  ## .F. dependiendo de si está disponible.  Elimina el archivo si este ha
  ## caducado.
    public function CompruebaCache ($Archivo)
    {
      $ArchivoCache = $this->NombreChache ($Archivo);
      if (file_exists ($ArchivoCache)):
	$Edad = time () - filectime ($ArchivoCache);
	if ($Edad > AGORA::Config ('cache_time', 3600)):
	  @unlink ($ArchivoCache);
	  return FALSE;
	endif;
	return TRUE;
      else:
	return FALSE;
      endif;
    }



  ## Dado un archivo, comprueba si este se encuentra en caché.
  ## Si es así, devuelve el caché.  En caso contrario, actualiza el caché y
  ## devuelve el nuevo contenido.
    public function LeeArchivo ($NombreArchivo)
    {
      if ($this->CompruebaCache ($NombreArchivo)):
	return file_get_contents ($this->NombreChache ($NombreArchivo));
      else:
	$Contenido = @file_get_contents ($NombreArchivo);
	if ($Contenido === FALSE)
	  throw new Exception ("No pudo leerse el archivo $NombreArchivo.", 404);
	if (@file_put_contents ($this->NombreChache ($NombreArchivo), $Contenido, LOCK_EX)
	=== FALSE)
	  throw new Exception ("No pudo crearse el archivo de caché.", 500);
	return $Contenido;
      endif;
    }



  ## Guarda los datos dados en caché para el nombre del archivo indicado.
  ## Devuelve FALSE si hubo algún problema.
    public function GuardaArchivo ($NombreArchivo, $Datos)
    {
      return @file_put_contents ($this->NombreChache ($NombreArchivo), $Datos, LOCK_EX);
    }



  ## Elimina el archivo indicado tanto si está como si no está en caché.
    public function EliminaCache ($NombreArchivo)
    {
      $ArchivoCache = $this->NombreChache ($Archivo);
      if (file_exists ($ArchivoCache))
	@unlink ($ArchivoCache);
    }
  }
