<?php
  /**
   * MediaWiki Slider extension
   *
   * @version 0.1
   * @author James Rising
   * @link http://www.mediawiki.org/wiki/Extension:Slider
   */

$wgExtensionCredits['parserhook'][] = array(
                'name' => 'Slider',
                'author' => 'James Rising',
                'version' => '0.1',
                'description' => 'Construct wiki pages as presentation slides.',
                );

$wgHooks['ParserFirstCallInit'][] = 'registerSliderHandler';
$wgHooks['ParserAfterTidy'][] = 'parserAfterTidySlider';
 
function registerSliderHandler(Parser &$parser) {
  $parser->setHook('slide', 'slideHandler');
  $parser->setHook('present', 'presentHandler');
  return true;
}

function slideHandler($input, array $args, Parser $parser, PPFrame $frame) {
  global $wgUser, $sliderMarkerList;

  $css = "width: 800px; height: 500px; border: 1px solid red;";

  if (!empty($args['bg']))
    $css .= " background: " . $args['bg'];
  if ($args['center'] == "both" || $args['center'] == 'hori')
    $input = '<center>' . $input . '</center>';

  $title = $parser->getTitle();
  
  global $wgOut;

  $input = preg_replace("/==(.+?)==/", "{h2}$1{/h2}", $input);
  $input = preg_replace("/=(.+?)=/", "{h1}$1{/h1}", $input);
  $output = $wgOut->parse($input);
  $output = str_replace(array("{", "}"), array("<", ">"), $output);

  $page = <<<EOP
<div id="slide_%%SLIDE%%" style="%%CSS%%">
<center><h1>%%TITLE%%</h1></center>

%%OUTPUT%%
</div>
<script type="text/javascript">
\$j(function() {
  var \$all = \$j('#slide_%%SLIDE%%').find('*');
  var displays = \$all.map(function() {
    return \$j(this).css('display');
  });
  \$all.css('display', 'inline-block');
  var widths = \$all.map(function() {
    return \$j(this).width();
  });
  \$all.each(function(ii) {
    \$j(this).css('display', displays[ii]);
  });

  var maxwidth = Math.max.apply(null, widths);
  if (maxwidth < 800) {
    \$j('#slide_%%SLIDE%%').css('font-size', Math.round(100 * 800 / maxwidth) + '%');
  }
});
</script>
EOP;
  $markercount = count($sliderMarkerList);
  $page = str_replace(array('%%SLIDE%%', '%%TITLE%%', '%%CSS%%', '%%OUTPUT%%'),
                      array($markercount, $title, $css, $output), $page);

  $marker = "xx-marker".$markercount."-xx";
  $sliderMarkerList[$markercount] = $page;
  return $marker;
}

function make_bigul($matches) {
  return '<ul><li>' . $matches[1] . '</li></ul>';
}

// XXX: This should generate a save-able page presentation with all slides
function presentHandler($input, array $args, Parser $parser, PPFrame $frame) {
  global $sliderMarkerList;

  $page = <<<EOP
EOP;
  $page = str_replace(array('%%TITLE%%'),
                      array($title), $page);

  $markercount = count($sliderMarkerList);
  $marker = "xx-marker".$markercount."-xx";
  $sliderMarkerList[$markercount] = $page;
  return $marker . $input;
}

function parserAfterTidySlider($parser, &$text) {
  // find markers in $text
  // replace markers with actual output
  global $sliderMarkerList;

  $keys = array();
  $marker_count = count($sliderMarkerList);
 
  for ($i = 0; $i < $marker_count; $i++) {
    $keys[] = 'xx-marker' . $i . '-xx';
  }
 
  $text = str_replace($keys, $sliderMarkerList, $text);
  return true;
}
?>
