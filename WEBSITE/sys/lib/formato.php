<?php
## Módulo para formatear textos.
##
## Para formatear el texto utiliza bbCode.
##
## Para usar los emoticonos hay que definir la variable de configuración
## "ruta_caritas".  Si esta variable no existe o es una cadena vacía, entonces
## no muestra a los emoticonos como imágenes.  Ojo, porque la ruta ha de
## terminar con la barra de separación de directorios "/".

  class FORMATO
  {
  # Emoticonos.
    public $Emoticonos = array (
    # Los nombres de archivos siguen cierta "estandarización de facto".
      ':-|' => 'neutral',
      ':-)' => 'smile',
      ':-(' => 'sad',
      ':-D' => 'biggrin',
      ';-D' => 'lol',
      ';-)' => 'wink',
      ':-P' => 'razz',
      ':-/' => 'confused',
      '8-)' => 'cool',
      ':*)' => 'redface',
      ":'(" => 'cry',
      ':-o' => 'surprised',
      ':eek:' => 'eek',
      ':evil:' => 'evil',
      ':idea:' => 'idea',
      ':mad:' => 'mad',
      ':rolleyes:' => 'rolleyes',
      ':twisted:' => 'twisted',
      '(!)' => 'exclaim',
      '(?)' => 'question',
      '=>' => 'arrow',
    # Alternativas.
      ':|' => 'neutral',
      ':)' => 'smile',
      ':(' => 'sad',
      ':D' => 'biggrin',
      ';D' => 'lol',
      ';)' => 'wink',
      ':P' => 'razz',
    /* Da problemas en "http://..."
      ':/' => 'confused',
     */
      ':confused:' => 'confused',
      '8)' => 'cool',
      ':o' => 'surprised',

      '8^|' => 'neutral',
      '8^)' => 'smile',
      '8^(' => 'sad',
      '8^D' => 'biggrin',
      ';^D' => 'lol',
      ';^)' => 'wink',
      '8^P' => 'razz',
      '8^/' => 'confused',
      ':cool:' => 'cool',
      '8^o' => 'surprised'
    );

  ## bbCode que puede ser utilizado "en línea";  esto es, sin párrafos.
  ## La idea es usar estos en comentarios, ya que son algo más seguros.
    private $bbCodeLinea = array(
    # Tipo de letra.
      '|\[b\](.*)\[/b\]|sU' => '<strong>\\1</strong>',
      '|\[i\](.*)\[/i\]|sU' => '<em>\\1</em>',
      '|\[u\](.*)\[/u\]|sU' => '<span style="text-decoration: underline">\\1</span>',
    # Edición.
      '|\[del\](.*)\[/del\]|sU' => '<del>\\1</del>',
      '|\[ins\](.*)\[/ins\]|sU' => '<ins>\\1</ins>',
    # Salto de línea y espacios.
      '|  |U' => '&nbsp; ', '/\[nlbr\]/i'  => '<br />',
    # Comillas.
    /*
      " ''" => ' &laquo;', "'' " => '&raquo; ',
      "''." => '&raquo;.', "'';" => '&raquo;;', "'':" => '&raquo;:',
     */
    # Imágenes.
      '|\[img\]http(.*)\[/img\]|U' => '<img src="http\\1" alt="" title="" />',
      '|\[img="(.*)"\]http(.*)\[/img\]|U' => '<img src="http\\2" alt="\\1" title="\\1" />',
      '|\[img=(.*)\]http(.*)\[/img\]|U' => '<img src="http\\2" alt="\\1" title="\\1" />',
    # Enlaces.  Con cambios para concordancia con wiki.
      '|\[url\]http(.*)\[/url]|U' => '<a class="urlextern" target="_blank" href="http\\1">http\\1</a>',
      '|\[url="http(.*)"\](.*)\[/url]|sU' => '<a class="urlextern" target="_blank" href="http\\1">\\2</a>',
      '|\[url=http(.*)\](.*)\[/url]|sU' => '<a class="urlextern" target="_blank" href="http\\1">\\2</a>',
    # Listas.
      '|\[list\](.*)\[/list\]|sU' => '</p><ul>\\1</ul><p>',
      '|\[list=1\](.*)\[/list\]|sU' => '</p><ul style="list-style-type: decimal">\\1</ul><p>',
      '|\[list=a\](.*)\[/list\]|sU' => '</p><ul style="list-style-type: lower-alpha">\\1</ul><p>',
      '|\[list=I\](.*)\[/list\]|sU' => '</p><ul style="list-style-type: upper-roman">\\1</ul><p>',

      '|\[list="1"\](.*)\[/list\]|sU' => '</p><ul style="list-style-type: decimal">\\1</ul><p>',
      '|\[list="a"\](.*)\[/list\]|sU' => '</p><ul style="list-style-type: lower-alpha">\\1</ul><p>',
      '|\[list="I"\](.*)\[/list\]|sU' => '</p><ul style="list-style-type: upper-roman">\\1</ul><p>',

      '|\[\*\]|sU' => '<li>', # W3C lo da por válido (?).
    # Citas
      '|\[quote](.*)\[/quote\]|sU' => '</p><blockquote><p>\\1</p></blockquote><p>',
      '|\[quote="(.*)"\](.*)\[/quote\]|sU' => '</p><blockquote><p class="cite">\\1:</p><p>\\2</p></blockquote><p>',
      '|\[quote=(.*)\](.*)\[/quote\]|sU' => '</p><blockquote><p class="cite">\\1:</p><p>\\2</p></blockquote><p>'
    );

  ## bbCode a usar en bloques.  Esto es, noticias, blogs, descripciones...
  ## Son etiquetas menos seguras, ya que permiten el uso del pseudo-protocolo
  ## "JavaScript" en los enlaces.
    private $bbCodeBloque = array (
    # Imágenes.
      '|\[img\](.*)\[/img\]|U' => '<img src="\\1" alt="" title="" />',
      '|\[img="(.*)"\](.*)\[/img\]|sU' => '<img src="\\2" alt="\\1" title="\\1" />',
      '|\[img=(.*)\](.*)\[/img\]|sU' => '<img src="\\2" alt="\\1" title="\\1" />',
    # Enlace.
      '|\[url\](.*)\[/url]|U' => '<a target="_blank" href="\\1">\\1</a>',
      '|\[url="(.*)"\](.*)\[/url]|sU' => '<a href="\\1">\\2</a>',
      '|\[url=(.*)\](.*)\[/url]|sU' => '<a href="\\1">\\2</a>',
    # Citas.
      '|\[quote](.*)\[/quote\]|sU' => '</p><blockquote><p>\\1</p></blockquote><p>',
      '#\[quote="(.*)" url="(.*)"\](.*)\[/quote\]#sU' => '</p><blockquote><p class="cite"><a href="\\2">\\1 dijo:</a></p><p>\\3</p></blockquote><p>',
      '|\[quote="(.*)"\](.*)\[/quote\]|sU' => '</p><blockquote><p class="cite">\\1 dijo:</p><p>\\2</p></blockquote><p>',
      '#\[quote=(.*) url=(.*)\](.*)\[/quote\]#sU' => '</p><blockquote><p class="cite"><a href="\\2">\\1 dijo:</a></p><p>\\3</p></blockquote><p>',
      '|\[quote=(.*)\](.*)\[/quote\]|sU' => '</p><blockquote><p class="cite">\\1 dijo:</p><p>\\2</p></blockquote><p>'
    );

  ## Bloques de código, que precisan tratamiento especial.
    private $bbCodeCodigo = array (
      'code' => '</p><pre><code>\\1</code></pre><p>',
      'pre' => '</p><pre>\\1</pre><p>',
      'noparse' => '\\1'
    );

  ## Contienen las listas finales.
    private $bbCode_EnLinea, $bbCode_EnBloque;



  ## Constructor.
    public function __construct ()
    {
    # Construye la lista de emoticonos.
      if (($Ruta = AGORA::Config ('ruta_caritas', '')) != ''):
	foreach ($this->Emoticonos as $Icon => $Archivo):
	  $RutaArchivo = "{$Ruta}icon_$Archivo.gif";
	# Lo añade a la lista de bbCodes.
	  $Patron = '#'.preg_quote ($Icon, '#').'#';
	  $this->bbCodeLinea[$Patron] = "<img src=\"$RutaArchivo\" alt=\"$Icon\" title=\"$Icon\" />";
	endforeach;
      endif;
    # Construye las listas de etiquetas:
      $this->bbCode_EnLinea = array_merge ($this->bbCodeLinea);
      $this->bbCode_EnBloque = array_merge ($this->bbCodeLinea, $this->bbCodeBloque);
    }



  ## División de párrafos.  Identifica dos saltos de línea consecutivos como un
  ## cambio de párrafo, añadiendo "</p><p>" donde corresponda.  Siempre añade
  ## "<p>" y "</p>", incluso si sólo hay un párrafo.
    public function SeparaParrafos ($Entrada)
    {
      return '<p>'.implode ("</p>\n<p>", preg_split ('/[\n\r][\n\r]/', $Entrada)).'</p>';
    }



  ## Realiza sustituciones bbCode "en línea".  Es decir, aquellos elementos
  ## bbCode que pueden utilizarse dentro de un párrafo y que no conforman
  ## un párrafo "per sé".
    public function bbCodeLinea ($Linea, $FiltrarXSS=TRUE)
    {
      if (!is_string ($Linea))
	throw new Exception ('Para formatear texto, ¡hay que pasar un texto!');
      if ($FiltrarXSS)
	$Resultado = AGORA::FiltroXSS ($Linea);
      else
	$Resultado = $Linea;
      foreach ($this->bbCode_EnLinea as $Patron => $Sustituto)
	$Resultado = preg_replace ($Patron, $Sustituto, $Resultado);
      return $Resultado;
    }



  ## Ayuda para el procesado de bbCode.
  ## Dado un [bloque]..[/bloque], elimina todo formato de su contenido y crea
  ## el bloque HTML correspondiente.
    private function bbCode_BloqueLimpio ($Etiqueta, $Formato, $Original, $Formateado)
    {
      $Patron = "|\[{$Etiqueta}\](.*)\[/{$Etiqueta}\]|sU";
    # Identificamos los contenidos para buscarlos luego.
      preg_match_all ($Patron, $Formateado, $Encontrados);
      $Encontrados = $Encontrados[1];
    # Obtenemos los mismos trozos, pero antes de haber sido formateados.
      preg_match_all ($Patron, $Original, $SinFormato);
      $SinFormato = $SinFormato[1];
    # Sustituimos los trozos formateados por los originales.
      $Resultado = str_replace ($Encontrados, $SinFormato, $Formateado);
    # Formateamos en bloque HTML.
      return preg_replace ($Patron, $Formato, $Resultado);
    }



  ## Procesa bbCode completo y de una vez.
    public function bbCode ($Texto)
    {
    # Preparamos el texto.
      $Texto = AGORA::FiltroXSS ($Texto);
    # Dividimos los párrafos.
      $Resultado = $this->SeparaParrafos ($Texto);
    # Ahora procesamos los bloques.
      foreach ($this->bbCode_EnBloque as $Patron => $Sustituto)
	$Resultado = preg_replace ($Patron, $Sustituto, $Resultado);
    # Los bloques [code], [pre] y [noparse] precisan trabajo extra.
      foreach ($this->bbCodeCodigo as $Etiqueta => $Formato)
	$Resultado = $this->bbCode_BloqueLimpio ($Etiqueta, $Formato, $Texto, $Resultado);
    # Eliminamos párrafos vacios.  No los elimina todos, pero casi.
      return preg_replace ('|\<p\>[\n\r]*\</p\>|', '', $Resultado);
    }
  };
