<?php
/**
 * DokuWiki DokuCMS Template
 *
 * @link   http://wiki.splitbrain.org/wiki:tpl:templates
 * @author Andreas Gohr <andi@splitbrain.org>
 * @author Klaus Vormweg <klaus.vormweg@gmx.de>
 */

// must be run from within DokuWiki
if (!defined('DOKU_INC')) die();

// include custom template functions stolen from arctic template
require_once(dirname(__FILE__).'/tpl_functions.php');

echo '
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="', $conf['lang'], '"
 lang="', $conf['lang'], '" dir="', $lang['direction'], '">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <title>
';
tpl_pagetitle();
echo '[', strip_tags($conf['title']), ']
  </title>
';
tpl_metaheaders();
echo tpl_favicon(array('favicon', 'mobile'));
echo '
<!--[if lt IE 7]>
   <style type="text/css">
      div.page { width: 55em !important; }
   </style>
<![endif]-->
</head>

<body>
<div class="dokuwiki">
';
html_msgarea();
echo '
  <div class="stylehead">

    <div class="header">
      <div class="pagename">
';
tpl_link(wl(),$conf['title'],'name="dokuwiki__top" id="dokuwiki__top" accesskey="h" title="[ALT+H]"');
echo '      </div>

      <div class="clearer"></div>
    </div>
';
if($conf['breadcrumbs']){
  echo '    <div class="breadcrumbs">
';
  tpl_breadcrumbs();
  echo '  </div>
';
}

if($conf['youarehere']){
  echo '    <div class="breadcrumbs">
';
  tpl_youarehere();
  echo '    </div>
';
}
echo '
  </div>
';
tpl_flush();

if($ACT != 'diff' && $ACT != 'edit' && $ACT != 'preview' && $ACT != 'admin' && $ACT != 'login' && $ACT != 'logout' && $ACT != 'profile' && $ACT != 'revisions') {
  echo '  <div class="wrap">
     <div class="sidebar">
';
  _tpl_sidebar(); 
  echo '   </div>
     <div class="page">
';
  tpl_content(); 
  echo '   </div>
  </div>
';
} else {
  echo '<div class="wrap" style="background-color: #fff;">
     <div class="page" style="margin-left:0; max-width: 75em;">
';
  tpl_content();
  echo '   </div>
  </div>
';
}
tpl_flush();
echo '
  <div class="stylefoot">
';
if($ACT != 'diff' && $ACT != 'edit' && $ACT != 'preview' && $ACT != 'admin' && $ACT != 'login' && $ACT != 'logout' && $ACT != 'profile' && $ACT != 'revisions') {
  echo '     <div class="homelink">
     <a href="http://wiki.splitbrain.org/wiki:dokuwiki" title="Driven by DokuWiki"><img src="', DOKU_TPL, 'images/button-dw.png" width="80" height="15" alt="Driven by DokuWiki" /></a>
      <a href="', DOKU_BASE, 'feed.php" title="Recent changes RSS feed"><img src="', DOKU_TPL, 'images/button-rss.png" width="80" height="15" alt="Recent changes RSS feed" /></a>
      </div>

    <div class="meta">
'; 
  _tpl_pageinfo();
  echo '  </div>
';
} else {
	echo '
    <div class="meta">
    </div>
';
}
echo '
    <div class="bar" id="bar__bottom">
       <div class="bar-left" id="bar__bottomleft">
';
tpl_button('admin');
if($ACT != 'login' && $ACT != 'logout') { 
  tpl_button('login');
  echo '&nbsp;';
}
if($_SERVER['REMOTE_USER']){
  tpl_button('subscribe');
	tpl_button('profile');
	tpl_button('history');
  tpl_button('revert');
}
if($conf['tpl']['dokucms']['showbacklinks']) {
  tpl_button('backlink');
  echo '&nbsp;';
}
echo '         &nbsp;
       </div>
       <div class="bar-right" id="bar__bottomright">
';
if(!$_SERVER['REMOTE_USER'] && $ACT != 'login' && $ACT != 'logout'){ 
  if(!$conf['tpl']['dokucms']['showsearch']) {  
    tpl_searchform();
  }
  if($conf['tpl']['dokucms']['showmedia']) {   
    tpl_button('media');
  }
} else {
  if($ACT != 'login' && $ACT != 'logout'){
    if($conf['tpl']['dokucms']['showsearch']) {  
      tpl_searchform();
      echo '&nbsp';
    }
    tpl_button('media');
  }
}
tpl_button('edit');
echo '&nbsp;
      </div>
      <div class="clearer"></div>
    </div>

  </div>
';
tpl_license(false);
echo '
</div>

<div class="no">';
/* provide DokuWiki housekeeping, required in all templates */ 
tpl_indexerWebBug();
echo '</div>
</body>
</html>
';
?>