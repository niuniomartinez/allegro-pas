    <?= $Descripcion ?>
    </div>
    <h2 class="sectionedit2" id="news">News</h2>
    <div class="meta">Last modified: <?= $UltimaActualizacion ?></div>
    <div class="level2">
    <p><a href="http://sourceforge.net/p/<?= AGORA::Config ('SFname', '') ?>/news/feed"><img src="wiki/lib/tpl/dokucms/images/button-rss.png" alt="News RSS feed" title="News RSS feed" /></a></p>
<?php
	foreach ($Noticias['Items'] as $Item):
 ?>
	  <h3 class="sectionedit3" id="<?= $Item['GUID'] ?>"><?= $Item['Titulo'] ?></h3>
	  <div class="level3">
	    <p><em>(<?= $Item['Fecha'] ?>)</em></p>
	    <p><?= $Item['Contenido'] ?></p>
	  </div>
<?php
	endforeach;
 ?>
