<?php

$basedir = "lists/";

if ($_GET['list']) {
  $file = $basedir . $_GET['list'] . '.tsv';

  if ($_GET['list'] && $_GET['name'] && $_GET['remove']) {
    // remove name from roster
    echo "remove: ";

    $file = $basedir . $_GET['list'] . '.tsv';
    $name = $_GET['name'];
    $name = strtr($name, "\t\"", " '");
    $check = $name . "\t";

    $fp = fopen($file, "r+");
    if (flock($fp, LOCK_EX)) {
      $lines = array();
      while (($line = fgets($fp)) !== false) {
        if (!(strpos($line, $check) === 0))
          array_push($lines, $line);
      }

      print_r($lines);

      ftruncate($fp, 0);
      foreach ($lines as $key => $line)
        fwrite($fp, $line);

      flock($fp, LOCK_UN);

      echo "done";
    } else
      echo "fail";

    exit(0);
  }

  if ($_GET['list'] && $_GET['name'] && $_GET['info']) {
    // add name to roster
    echo "add: ";

    $name = $_GET['name'];
    $name = strtr($name, "\t\"", " '");
    $info = $_GET['info'];
    $info = strtr($info, "\t\"", " '");

    $line = "${name}\t${info}\n";

    $fp = fopen($file, "a");
    if (flock($fp, LOCK_EX)) {
      fwrite($fp, $line);
      flock($fp, LOCK_UN);
      echo "done";
    } else
      echo "fail";

    exit(0);
  }

  // Just display the file
  $fp = fopen($file, "r");
  if (flock($fp, LOCK_SH)) {
    while (($line = fgets($fp)) !== false)
      echo $line;

    flock($fp, LOCK_UN);
  }
}

?>
