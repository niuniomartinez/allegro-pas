<?php
/**
 * Ágora 2.0
 *
 * Un marco de desarrollo web sencillo.
 */

/**
 * Establece el nivel de error.  En producción debería asignarse "0" para que
 * no devuelva error alguno.  Adignarle "E_STRICT" debería devolver TODOS los
 * errores, aunque a veces no lo hace.
 */
  error_reporting (E_ALL);



/**
 * Identifica la versión de Ágora.
 */
  define ('AG_VERSION', '2.0');

/**
 * Directorio donde se encuentra ágora.
 */
  define ('AG_DIR', dirname (__FILE__).'/');

/**
 * Directorio de vistas.  Esta constante es utilizada por AGORA::CargaVista.
 */
  define ('AG_DIR_VISTAS', AG_DIR.'templates/');

/**
 * Directorio de módulos.  Esta constante es utilizada por AGORA::CargaModulo.
 */
  define ('AG_DIR_MODULOS', AG_DIR.'lib/');



/**
 * Clase contenedora de Ágora.
 *
 * Puede usarse como base para otras clases, creando un objeto o llamando
 * directamente a los métodos estáticos.
 */
  class AGORA
  {
  /**
   * Devuelve el valor de la variable de configuración, si existe, o el valor
   * $Defecto si no existe.
   *
   * La configuración se encuentra en un diccionario global llamado $Config.
   * Este diccionario no es creado por Ágora.
   */
    static public function Config ($NombreVar, $Defecto = FALSE)
    {
      global $Config;

      return isset ($Config[$NombreVar]) ? $Config[$NombreVar] : $Defecto;
    }



  /**
   * Cambia los caracteres '&', '%', '<' y '>' para evitar inyecciones.
   * Es posible que no evite absolutamente todas las inyecciones, pero he hecho
   * algunas pruebas y funciona razonablemente bien, al menos para mi.
   *
   * Otra utilidad que tiene es que si se le pasa código fuente (incluso XML),
   * lo devuelve en un formato que permite mostrarlo sin ningún problema.
   */
    static public function FiltroXSS ($Texto)
    {
    # Si es una lista, la procesa una a una.
      if (is_array ($Texto)):
	while (list ($Ndx) = each ($Texto))
	  $Texto[$Ndx] = self::FiltroXSS ($Texto[$Ndx]);
	return $Texto;
      endif;
    # Realiza el filtrado.
      if (is_string ($Texto))
	return str_replace (
	  array ('&', '%', '<', '>'),
	  array ('&amp;', '&#37;', '&lt;', '&gt;'),
	  $Texto
	);
      else
	return $Texto;
    }



  /**
   * Función de ayuda para obtener elementos de una lista.
   */
    static protected function ElementoLista ($Lista, $Nombre, $FiltroXSS)
    {
    # Si no solicita un nombre, devuelve la lista completa.
      if ($Nombre === NULL and !empty ($Lista)):
	if ($FiltroXSS === TRUE)
	  return self::FiltroXSS ($Lista);
	return $Lista;
      endif;
    # En otro caso, devuelve el elemento solicitado.
      if (!isset ($Lista[$Nombre]))
	return FALSE;
      if ($FiltroXSS === TRUE)
	return self::FiltroXSS ($Lista[$Nombre]);
      return $Lista[$Nombre];
    }



  /**
   * Devuelve el valor del parámetro GET solicitado o .F. si este no se
   * recibió.
   */
    static public function get ($Nombre = NULL, $FiltroXSS = FALSE)
    {
      return self::ElementoLista ($_GET, $Nombre, $FiltroXSS);
    }



  /**
   * Devuelve el valor del parámetro POST solicitado o .F. si este no se
   * recibió.
   */
    static public function post ($Nombre = NULL, $FiltroXSS = FALSE)
    {
      return self::ElementoLista ($_POST, $Nombre, $FiltroXSS);
    }



  /**
   * Devuelve el valor del parámetro POST o GET solicitado o .F. si este no se
   * recibió.
   *
   * Si está en ambas entradas, POST tiene preferencia.
   */
    static public function get_post ($Nombre, $FiltroXSS = FALSE)
    {
      return !isset ($_POST[$Nombre])
	? self::get ($Nombre, $FiltroXSS)
	: self::post ($Nombre, $FiltroXSS);
    }



  /**
   * Establece una huella (/cookie/).
   * La caducidad debe ser indicada en segundos.
   *
   * Utiliza las siguientes variables de configuración (si existen):
   * * RutaHuella:  Indica la ruta (/path/).
   * * DominioHuella: Indica el dominio.
   * * httpsHuella: Indica que debe usarse el protocolo HTTPS.
   * * JavaScriptHuella: Indica si se puede o no acceder al valor a través de
   *   JavaScript.
   * Para saber exactamente qué supone cada opción, ver la documentación del
   * procedimiento PHP setcookie.
   */
    static public function PonHuella ($Nombre, $Valor, $Caducidad = 0)
    {
      if (!setcookie ($Nombre, $Valor, $Caducidad,
/* TODO: No me gustan los nombres de las variables de configuración. */
	self::Config ('RutaHuella', ''),
	self::Config ('DominioHuella', ''),
	self::Config ('httpsHuella', FALSE),
	!self::Config ('JavaScriptHuella', TRUE)
      ))
	throw new Exception ("No pudo establecerse la huella '$Nombre'.");
    }



  /**
   * Devuelve el contenido de una huella o .F. si esta no se encontró.
   */
    static public function TomaHuella ($Nombre, $FiltroXSS = FALSE)
    {
      return self::ElementoLista ($_COOKIE, $Nombre, $FiltroXSS);
    }



  /**
   * Elimina la huella indicada, de existir.
   */
    static public function EliminaHuella ($Nombre)
    {
    # Si es una lista, las elimina todas.
      if (is_array ($Nombre)):
	foreach ($Nombre as $Huella)
	  self::EliminaHuella ($Huella);
      else:
	self::PonHuella ($Nombre, '', time () - 3600);
      endif;
    }



  /**
   * Carga y procesa la vista.
   *
   * @param string $Vista Nombre de la vista.
   * @param array $Vars Diccionario con las variables usadas por la vista.
   * @param bool $Devolver Devolver la vista procesada como cadena (TRUE) o enviar el resultado al cliente (FALSE).
   */
    static public function CargaVista ($_Vista, $_Vars = array (), $_Devolver = FALSE)
    {
      $_NombreArchivo =  AG_DIR_VISTAS."$_Vista.php";
      if (!is_file ($_NombreArchivo))
	throw new Exception ("No se encuentró el archivo '$_NombreArchivo'");
    # Si recibe una lista de parámetros, genera las variables precisas.
    # Nótese que no sobreescribe las ya existentes para impedir la modificación
    # de variables globales.
      if (is_array ($_Vars) and count ($_Vars) > 0)
	extract ($_Vars, EXTR_SKIP);
    # Genera la vista.
      ob_start ();
      if ((bool) @ini_get ('short_open_tag') === FALSE):
      # El intérprete no permite etiqueta de apertura corta ("<?="), así que
      # lo sustituye por "<?php echo ", que es equivalente.  Esto lo he tomado
      # prestado de CodeIgniter 2.
	echo eval (
	# Se usa concatenación para evitar que se identifique la secuencia de
	#apertura o cierre.
	  '?'.'>' . preg_replace (
	    '/;*\s*\?'.'>/',
	    '; ?'.'>',
	    str_replace (
	      '<'.'?=',
	      '<'.'?php echo ',
	      file_get_contents ($_NombreArchivo)
	    )
	  )
	);
      else:
	require ($_NombreArchivo);
      endif;
    # Comprueba si debe devolver el resultado.
      if ($_Devolver):
	$_Resultado = ob_get_contents ();
	ob_end_clean ();
	return $_Resultado;
      endif;
      ob_end_flush ();
    }



  /**
   * Contenedor de objetos de módulo.  Es asignado por AGORA::CargaModulo.
   */
    static $Modulos = NULL;



  /**
   * Carga la definición de clase del archivo indicado y crea un objeto
   * asignándolo como meta-propiedad de AGORA::$Modulos.
   * Recibe el nombre del módulo y, opcionalmente, el nombre del objeto.
   * Observe que el nombre del módulo ha de estar en minúsculas, mientras que
   * el de la clase debe estar en mayúsculas, así, si el módulo se llama
   * "modulo", el archivo se llamará "modulo.php" y la clase "MODULO".
   */
    static public function CargaModulo ($NombreModulo, $NombreObjeto = '')
    {
    # Si el módulo está en un subdirectorio, extrae la ruta y lo separa del
    # nombre de módulo.
      if (strpos ($NombreModulo, '/') === FALSE):
	$RutaModulo = '';
      else:
	$ListaDir = explode ('/', $NombreModulo);
	$NombreModulo = end ($ListaDir);
	unset ($ListaDir[count ($ListaDir) - 1]);
	$RutaModulo = implode ('/', $ListaDir).'/';
      endif;
    # Retoca los nombres.
      if ($NombreObjeto == '') $NombreObjeto = $NombreModulo;
      $NombreClase = strtoupper ($NombreModulo);
      $NombreModulo = strtolower ($NombreModulo);
    # Comprueba si el objeto ya existe.
      if (isset (AGORA::$Modulos->$NombreObjeto))
	throw new Exception ("El objeto $NombreObjeto ya existe");
    # Genera el nombre del archivo.
      $NombreArchivo =  AG_DIR_MODULOS."$RutaModulo$NombreModulo.php";
      if (!is_file ($NombreArchivo))
	throw new Exception ("El módulo '$NombreArchivo' no existe");
    # Carga el archivo.
      include_once ($NombreArchivo);
    # Crea el objeto.
      AGORA::$Modulos->$NombreObjeto = new $NombreClase;
    }



  /**
   * Permite acceder a los módulos de forma más simple si se ha creado un
   * objeto con la clase AGORA.  Así, puede accederse a
   * $MiObjeto->Modulos->Objeto como $MiObjeto->Objeto.
   */
    function __get ($key)
    {
      return AGORA::$Modulos->$key;
    }



  /**
   * Envia al cliente a otra URL.
   */
    static public function Redirigir ($URL)
    {
    # TODO: Comprobar que la URL sea válida.
      header ("Location: $URL");
    }



  /**
   * Lista de códigos de error HTTP.
   */
    static public $ErrorHTTP = array(
	200	=> 'OK',
	201	=> 'Created',
	202	=> 'Accepted',
	203	=> 'Non-Authoritative Information',
	204	=> 'No Content',
	205	=> 'Reset Content',
	206	=> 'Partial Content',

	300	=> 'Multiple Choices',
	301	=> 'Moved Permanently',
	302	=> 'Found',
	304	=> 'Not Modified',
	305	=> 'Use Proxy',
	307	=> 'Temporary Redirect',

	400	=> 'Bad Request',
	401	=> 'Unauthorized',
	403	=> 'Forbidden',
	404	=> 'Not Found',
	405	=> 'Method Not Allowed',
	406	=> 'Not Acceptable',
	407	=> 'Proxy Authentication Required',
	408	=> 'Request Timeout',
	409	=> 'Conflict',
	410	=> 'Gone',
	411	=> 'Length Required',
	412	=> 'Precondition Failed',
	413	=> 'Request Entity Too Large',
	414	=> 'Request-URI Too Long',
	415	=> 'Unsupported Media Type',
	416	=> 'Requested Range Not Satisfiable',
	417	=> 'Expectation Failed',

	500	=> 'Internal Server Error',
	501	=> 'Not Implemented',
	502	=> 'Bad Gateway',
	503	=> 'Service Unavailable',
	504	=> 'Gateway Timeout',
	505	=> 'HTTP Version Not Supported'
      );



  /**
   * Devuelve al cliente una cabecera HTTP determinada.
   * Devuelve a la función llamante el "Texto" definitivo.
   */
    static public function EnviaCabeceraHTTP ($Codigo = 500, $Texto = '')
    {
    # Comprobamos que los códigos sean correctos.
      if ($Codigo == '' or !is_numeric ($Codigo))
	self::Error ('El código de error ha de ser numérico');
    # Obtenemos el mensaje de cabecera, según su código y asignación.
      if ($Texto == ''):
	if (isset (self::$ErrorHTTP[$Codigo])):
	  $Texto = self::$ErrorHTTP[$Codigo];
	else:
	  self::Error ('No se encontró cabecera para el error.&nbsp; Por favor, use el código adecuado o provea su propia cabecera.');
	endif;
      endif;
      if (!isset (self::$ErrorHTTP[$Codigo]))
	$Codigo = 500;
    # Enviamos la cabecera.
      $Protocolo = (isset ($_SERVER['SERVER_PROTOCOL'])) ? $_SERVER['SERVER_PROTOCOL'] : FALSE;
      if (substr (php_sapi_name (), 0, 3) == 'cgi')
	header ("Status: $Codigo $Texto", TRUE);
      elseif ($Protocolo == 'HTTP/1.1' or $Protocolo == 'HTTP/1.0')
	header ("$Protocolo $Codigo $Texto", TRUE, $Codigo);
      else
	header ("HTTP/1.1 $Codigo $Texto", TRUE, $Codigo);
      return $Texto;
    }



  /**
   * Devuelve al cliente una página de error y termina.
   *
   * La página de error es una vista llamada "pag_error.php", y usa las
   * variables siguientes:
   * * codigo: Número del código de error.
   * * nombre: Texto que define el error HTTP.
   * * mensaje: Mensaje de error.
   *
   * Este método también envía el código de error HTTP en la cabecera.
   */
    static public function Error ($Mensaje, $Codigo = 500, $Nombre = '')
    {
    # Enviamos la cabecera.
      $Nombre = self::EnviaCabeceraHTTP ($Codigo, $Nombre);
    # Enviamos la página de error.
      self::CargaVista ('pag_error', array (
	'nombre' => $Nombre,
	'codigo' => $Codigo,
	'mensaje'  => '<p>'.(is_array ($Mensaje) ? implode ('</p><p>', $Mensaje) : $Mensaje).'</p>'
      ));
    # Termina la ejecución.
      exit;
    }



  /**
   * Igual que "Error", solo que devuelve el código de error 404 y usa otra
   * plantilla, llamada "pag_error404.php", que usa las variables "URL" y
   * "Mensaje".
   */
    static public function Error404 ($URL, $Mensaje = 'Página no encontrada')
    {
    # Enviamos la cabecera.
      self::EnviaCabeceraHTTP (404);
    # Enviamos la página de error.
      self::CargaVista ('pag_error404', array (
	'URL' => $URL,
	'mensaje'  => '<p>'.(is_array ($Mensaje) ? implode ('</p><p>', $Mensaje) : $Mensaje).'</p>'
      ));
    # Termina la ejecución.
      exit;
    }
  };
