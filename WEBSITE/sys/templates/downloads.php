  <?= $Descripcion ?>
</div>

<h2 class="sectionedit2" id="latest_version">Latest version</h2>
<div class="level2">
<p>
These are the packages for the latest version. They work on GNU/Linux and Windows. It would work also on other systems with few modifications. You need only one of the next files.
</p>
<p>
<strong>Note:</strong> Delphi users must use <a href="http://sourceforge.net/projects/allegro-pas/files/4.2.2/" class="urlextern">the old 4.2.x files</a> because latest ones aren't compatible. Compatibility with Delphi should be restored in version 5.0.
</p>
<div class="table sectionedit3"><table class="inline"><tbody>
      <tr class="row0">
	<th align="center">Filename</th>
	<th align="center">Size</th>
	<th align="center">Description</th>
      </tr>
<?php
	foreach ($Archivos['lib'] as $Desc):
 ?>
      <tr>
	<td align="left"><a href="http://sourceforge.net/projects/allegro-pas/files/<?= $Desc['Archivo'] ?>/download"><?= basename ($Desc['Archivo']) ?></a></td>
	<td><?= $Desc['Tamanno'] ?></td>
	<td><?= AGORA::$Modulos->formato->bbCodeLinea ($Desc['Desc']) ?></td>
      </tr>
<?php
	endforeach;
?>
  </tbody></table></div>
</div>

<h2 class="sectionedit2" id="latest_version">Latest documentation</h2>
<div class="level2">
<p>
It's possible to build the documentation from sources using the <a href="http://pasdoc.sipsolutions.net/" class="urlextern" title="http://pasdoc.sipsolutions.net/" target="blank_">pasdoc</a> tool. By the way, some times the documentation should be updated. Here you have the latest documentation (it's the same than the <a href="/allegro-pas/wiki/doku.php?id=documentation:start" class="wikilink1" title="documentation:start">on-line documentation</a>).
</p>

<div class="table sectionedit3"><table class="inline"><tbody>
      <tr class="row0">
	<th align="center">Filename</th>
	<th align="center">Size</th>
	<th align="center">Description</th>
      </tr>
<?php
	foreach ($Archivos['doc'] as $Desc):
 ?>
      <tr>
	<td align="left"><a href="http://sourceforge.net/projects/allegro-pas/files/<?= $Desc['Archivo'] ?>/download"><?= basename ($Desc['Archivo']) ?></a></td>
	<td><?= $Desc['Tamanno'] ?></td>
	<td><?= AGORA::$Modulos->formato->bbCodeLinea ($Desc['Desc']) ?></td>
      </tr>
<?php
	endforeach;
?>
  </tbody></table></div>
