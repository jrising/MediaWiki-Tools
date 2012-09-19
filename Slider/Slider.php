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

  $css = "width: 100%; height: 400px; border: 1px solid red;"

    if (!empty($args['bg']))
      $css .= " background: " . $args['bg'];

  $title = $parser->getTitle();
  $title = str_replace(" ", "_", $title);
  
  $page = <<<EOP
<div style="%%CSS%%">
<center><h1>%%TITLE%%</h1></center>

%%INPUT%%
</div>
EOP;
  $page = str_replace(array('%%TITLE%%', '%%CSS%%', '%%INPUT%%'),
                      array($title, $css, $input), $page);

  $markercount = count($sliderMarkerList);
  $marker = "xx-marker".$markercount."-xx";
  $sliderMarkerList[$markercount] = $page;
  return $marker . $input;
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
