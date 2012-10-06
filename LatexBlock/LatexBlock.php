<?php

$wgHooks['ParserFirstCallInit'][] = 'registerLaTeXTag';
$wgHooks['ParserAfterTidy'][] = 'parserAfterTidyLaTeX';

$latexTemplateSimple = <<<EOD
\documentclass[11pt]{amsart}
%\usepackage{geometry}
%\geometry{letterpaper}

\begin{document}

[TEXT]

\end{document}
EOD;

function registerLaTeXTag(Parser &$parser) {
  // When the parser sees the <latex> tag, it executes 
  // the renderLaTeX function (see below)
  $parser->setHook('latex', 'renderLaTeX');
  return true;
}

// Execute 
function renderLaTeX($input, array $args, Parser $parser, PPFrame $frame) {
  global $markerList, $latexTemplateSimple;

  // output data to latex file
  if (isset($args['tmpl']) && $args['tmpl'] == 'none')
    $latex = $input;
  else if (strpos($input, "\begin{document}")) {
    // default processing
    $latex = $input;
    $latex = str_replace(', fullpage]', ']', $latex);
    $latex = preg_replace('/\\\\setlength.+/', '', $latex);
  } else {
    $latex = str_replace('[TEXT]', $input, $latexTemplateSimple);
  }

  $title = $parser->getTitle() . ' ' . count($markerList);
  $title = strtr($title, ' ', '_');
  $dir = 'extensions/LatexBlock/latexed/' . $title;
  mkdir($dir);

  $fp = fopen($dir . '/index.tex', 'w');
  fwrite($fp, $latex);
  fclose($fp);

  $result = shell_exec('extensions/LatexBlock/Hyperlatex-2.7/bin/hyperlatex ' . escapeshellarg($dir . '/index.tex') . ' 2>&1');
  if (is_file($dir . '/index.html')) {
    $output = '<iframe src="' . $dir . '" onload="this.style.height = this.contentWindow.document.body.scrollHeight + 10 + \'px\';" width="100%" height="10">No iframes?</iframe>';
  } else
    $output = 'LaTeX ERROR: ' . $result . 'extensions/LatexBlock/Hyperlatex-2.7/bin/hyperlatex ' . escapeshellarg($dir . '/index.tex') . ' 2>&1';

  $markercount = count($markerList);
  $marker = "xx-marker".$markercount."-xx";
  $markerList[$markercount] = $output;
  return $marker;
}

function parserAfterTidyLaTeX($parser, &$text) {
  // find markers in $text
  // replace markers with actual output
  global $markerList;
  $keys = array();
  $marker_count = count($markerList);
 
  for ($i = 0; $i < $marker_count; $i++) {
    $keys[] = 'xx-marker' . $i . '-xx';
  }
 
  $text = str_replace($keys, $markerList, $text);
  return true;
}
