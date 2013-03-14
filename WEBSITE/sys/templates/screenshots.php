<?php
	foreach ($Imagenes as $Desc):
 ?>
	<p align="center"><a href="https://sourceforge.net/projects/<?= AGORA::Config ('SFname', '') ?>/screenshots/<?= $Desc['Archivo'] ?>"><img src="https://sourceforge.net/projects/<?= AGORA::Config ('SFname', '') ?>/screenshots/<?= $Desc['Archivo'] ?>" width="400" height="300" title="<?= $Desc['Desc'] ?>" /><br /><?= $Desc['Desc'] ?></a></p>
<?php
	endforeach;
 ?>
