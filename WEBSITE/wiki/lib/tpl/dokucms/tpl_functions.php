<?php
/**
 * DokuWiki Template DokuCMS Functions - adapted from arctic template
 *
 * @license GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author Andreas Gohr <andi@splitbrain.org>
 * @author  Michael Klier <chi@chimeric.de>
 * @author Klaus Vormweg <klaus.vormweg@gmx.de>
 */

// must be run from within DokuWiki
if (!defined('DOKU_INC')) die();

/**
 * Prints the sidebars
 *
 */
function _tpl_sidebar() {
    global $lang;
    global $conf;
    global $ID;

    if(!defined('DOKU_LF')) define('DOKU_LF',"\n");
    $pname = 'sidebar';
    
    $tpl = $conf['template'];

    if($conf['tpl'][$tpl]['sidebar']== 'file')  {
        $ns_sb = _getNsSb($ID);
        if($ns_sb && auth_quickaclcheck($ns_sb) >= AUTH_READ) {
            print '<div class="sidebar_box">' . DOKU_LF;
            print p_sidebar_xhtml($ns_sb) . DOKU_LF;
            print '</div>' . DOKU_LF;
         } elseif(@file_exists(wikiFN($pname)) && auth_quickaclcheck($pname) >= AUTH_READ) {
            print '<div class="sidebar_box">' . DOKU_LF;
            print p_sidebar_xhtml($pname) . DOKU_LF;
            print '</div>' . DOKU_LF;
        }
   } else {
    			print '<div class="sidebar_box">' . DOKU_LF;
    			print '  ' . p_index_xhtml($ID) . DOKU_LF;
    			print '</div>' . DOKU_LF;
	 }	
}

/**
 * searches for namespace sidebars
 */
function _getNsSb($id) {
    $pname = 'sidebar';
    $ns_sb = '';
    $path  = explode(':', $id);
    
    while(count($path) > 0) {
        $ns_sb = implode(':', $path).':'.$pname;
        if(@file_exists(wikiFN($ns_sb))) return $ns_sb;
        array_pop($path);
    }
    
    // nothing found
    return false;
}

/**
 * Removes the TOC of the sidebar pages and 
 * shows a edit button if the user has enough rights
 *
 */
function p_sidebar_xhtml($sb) {
  global $conf;
  $tpl = $conf['template'];
  $data = p_wiki_xhtml($sb,'',false);
  if(auth_quickaclcheck($sb) >= AUTH_EDIT and $conf['tpl'][$tpl]['sidebaredit']) {
    $data .= '<div class="secedit">'.html_btn('secedit',$sb,'',array('do'=>'edit','rev'=>'','post')).'</div>';
  }
  // strip TOC
  $data = preg_replace('/<div class="toc">.*?(<\/div>\n<\/div>)/s', '', $data);
  // replace headline ids for XHTML compliance
  $data = preg_replace('/(<h.*?><a.*?id=")(.*?)(">.*?<\/a><\/h.*?>)/','\1sb_left_\2\3', $data);
  return ($data);
}

/**
 * Renders the Index
 *
 */
function p_index_xhtml($ns) {
  require_once(DOKU_INC.'inc/search.php');
  global $conf;
  global $ID;
  $dir = $conf['datadir'];
  $tpl = $conf['template'];
  if(isset($conf['start'])) {
    $start = $conf['start'];
  } else {
    $start = 'start';
  }

  $ns  = cleanID($ns);
# fixme use appropriate function
  if(empty($ns)){
    $ns = dirname(str_replace(':','/',$ID));
    if($ns == '.') $ns ='';
  }
  $ns  = utf8_encodeFN(str_replace(':','/',$ns));

  $data = array();
  search($data,$conf['datadir'],'search_index',array('ns' => $ns));
  $i = 0;
  foreach($data as $item) {
    if($conf['tpl'][$tpl]['cleanindex']) {
      if($item['id'] == 'playground' or $item['id'] == 'wiki') {
        unset($data[$i]);
      }
    }
    if($item['id'] == 'sidebar' or $item['id'] == $start or preg_match('/:'.$start.'$/',$item['id'])) {
      unset($data[$i]);
    }
    $i++;
  }  

# print index with empty items removed  
  print html_buildlist($data,'idx','_html_list_index','html_li_index');
}

/**
 * Index item formatter
 *
 * User function for html_buildlist()
 *
 */
function _html_list_index($item){
  global $ID;
  global $conf;
  $ret = '';
  if($item['type']=='d'){
    if(@file_exists(wikiFN($item['id'].':'.$conf['start']))) {
      $ret .= html_wikilink($item['id'].':'.$conf['start']);
    } else {
      $ret .= html_wikilink($item['id'].':');
    }
  } else {
    $ret .= html_wikilink(':'.$item['id']);
  }
  return $ret;
}

# dokucms modified version of pageinfo 
function _tpl_pageinfo(){
  global $conf;
  global $lang;
  global $INFO;
  global $ID;
  
  // return if we are not allowed to view the page
  if (!auth_quickaclcheck($ID)) { return; }
  
  // prepare date and path
  $date = dformat($INFO['lastmod']);

  // print it
  if($INFO['exists']){
    print $lang['lastmod'];
    print ': ';
    print $date;
    if($_SERVER['REMOTE_USER']){
      if($INFO['editor']){
        print ' '.$lang['by'].' ';
        print $INFO['editor'];
      }else{
        print ' ('.$lang['external_edit'].')';
      }
      if($INFO['locked']){
        print ' &middot; ';
        print $lang['lockedby'];
        print ': ';
        print $INFO['locked'];
      }
    }
    return true;
  }
  return false;
}
?>
