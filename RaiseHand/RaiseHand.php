<?php
        /**
         * MediaWiki RaiseHand extension
         *
         * @version 0.1
         * @author James Rising
         * @link http://www.mediawiki.org/wiki/Extension:RaiseHand
         */
 
        $wgExtensionCredits['parserhook'][] = array(
                'name' => 'RaiseHand',
                'author' => 'James Rising',
                'version' => '0.1',
                'description' => 'Allows users to add their name to a dynamically loaded list.',
                );
 
        $wgHooks['ParserFirstCallInit'][] = 'registerRaiseHandHandler';
        $wgHooks['ParserAfterTidy'][] = 'parserAfterTidyRaiseHand';
 
        function registerRaiseHandHandler(Parser &$parser)
        {
          $parser->setHook('raisehand', 'raiseHandHandler');
          $parser->setHook('handqueue', 'handQueueHandler');
          return true;
        }
 
        function raiseHandHandler($input, array $args, Parser $parser, PPFrame $frame)
        {
          global $wgUser, $raisehandMarkerList;

          if (empty($args['repeat']))
            $repeat = '5000';
          else
            $repeat = $args['repeat'];

          if (empty($args['button'])) {
            $input .= '<a id="raisehand_button" href="#"><img src="/extensions/RaiseHand/hand.png" /></a>';
            $button = 'raisehand_button';
          } else
            $button = $args['button'];

          if (empty($args['lowerer'])) {
            $input .= '<input id="lowerhand_button" type="button" disabled="disabled" value="Lower Hand" />';
            $lowerer = 'lowerhand_button';
          } else
            $lowerer = $args['lowerer'];

          if (empty($args['roster'])) {
            $input .= '<h2>Speaking Queue</h2><div id="raisehand_queue"></div>';
            $roster = 'raisehand_queue';
          } else
            $roster = $args['roster'];

          if (empty($args['getinfo'])) {
            $getinfo = 'return { name: wgUserName, info: "--" };';
          } else
            $getinfo = $args['getinfo'];

          $extrascript = '';

          if (!empty($args['namefill']))
            $extrascript .= "\$j('#" . $args['namefill'] . "').val(wgUserName);\n";

          $title = $parser->getTitle();
          $title = str_replace(" ", "_", $title);
 
          $script = <<<EOS
<script type="text/javascript">
\$j(function() {
  \$j('#%%ROSTER%%').html("Loading queue...  ");
  window.setInterval(function() {
      \$j.ajax({
        url: "extensions/RaiseHand/roster.php",
        cache: false,
        data: { list: '%%TITLE%%' },
        success: function(data) {
            if (!data) {
              html = "No hands raised yet.  ";
              \$j('#%%ROSTER%%').html(html);
              return;
            }
            var html = '<ol>';
            var lines = data.split('\\n');
            for (var ii = 0; ii < lines.length; ii++) {
              if (!lines[ii])
                continue;
              var htmlline = lines[ii].replace('\\t', '</span> <span class="info">');
              html += '<li><span class="name">' + htmlline + '</span></li>';
            }
            \$j('#%%ROSTER%%').html(html);
          }});
    }, %%REPEAT%%);
  \$j('#%%BUTTON%%').click(function(event) {
      event.preventDefault();
      \$j('#%%ROSTER%%').append('<b>Adding to queue...</b>');
      var info = (function() { %%GETINFO%% })();
      if (!info)
        return;  // failure
      info.list = "%%TITLE%%";
      \$j.ajax({
        url: "extensions/RaiseHand/roster.php",
        cache: false,
        data: info
      });
      \$j('#%%LOWERER%%').click(function(event) {
        \$j('#%%ROSTER%%').append('<b>Removing from queue...</b>');

        info.remove = true;
        \$j.ajax({
          url: "extensions/RaiseHand/roster.php",
          cache: false,
          data: info
        });

        \$j('#%%LOWERER%%').attr('disabled', 'disabled');
        \$j('#%%LOWERER%%').unbind();
      });
      \$j('#%%LOWERER%%').removeAttr('disabled');
    });
  %%EXTRAS%%
});
</script>
EOS;
          $script = str_replace(array('%%TITLE%%', '%%ROSTER%%', '%%BUTTON%%', '%%GETINFO%%', '%%REPEAT%%', '%%EXTRAS%%', '%%LOWERER%%'),
                                array($title, $roster, $button, $getinfo, $repeat, $extrascript, $lowerer), $script);

          $markercount = count($raisehandMarkerList);
          $marker = "xx-marker".$markercount."-xx";
          $raisehandMarkerList[$markercount] = $script;
          return $marker . $input;
        }

        function handQueueHandler($input, array $args, Parser $parser, PPFrame $frame)
        {
          global $raisehandMarkerList;

          if (empty($args['repeat']))
            $repeat = '5000';
          else
            $repeat = $args['repeat'];

          if (empty($args['roster'])) {
            $input .= '<h2>Speaking Queue</h2><div id="raisehand_queue"></div>';
            $roster = 'raisehand_queue';
          } else
            $roster = $args['roster'];

          if (empty($args['list']))
            $title = $args['list'];
          else
            $title = $parser->getTitle();
          $title = str_replace(" ", "_", $title);
 
          $script = <<<EOS
<script type="text/javascript">
\$j(function() {
  \$j('#%%ROSTER%%').html("Loading queue...  ");
  window.setInterval(function() {
      \$j.ajax({
        url: "extensions/RaiseHand/roster.php",
        cache: false,
        data: { list: '%%TITLE%%' },
        success: function(data) {
            if (!data) {
              html = "No hands raised yet.  ";
              \$j('#%%ROSTER%%').html(html);
              return;
            }
            var html = '<ol>';
            var lines = data.split('\\n');
            for (var ii = 0; ii < lines.length; ii++) {
              if (!lines[ii])
                continue;
              var htmlline = lines[ii].replace('\\t', '</span> <span class="info">');
              html += '<li><span class="name">' + htmlline + '</span></li>';
            }
            \$j('#%%ROSTER%%').html(html);
          }});
    }, %%REPEAT%%);
});
</script>
EOS;
          $script = str_replace(array('%%TITLE%%', '%%ROSTER%%','%%REPEAT%%'),
                                array($title, $roster, $repeat), $script);

          $markercount = count($raisehandMarkerList);
          $marker = "xx-marker".$markercount."-xx";
          $raisehandMarkerList[$markercount] = $script;
          return $marker . $input;
        }

        function parserAfterTidyRaiseHand($parser, &$text) {
          // find markers in $text
          // replace markers with actual output
          global $raisehandMarkerList;

          $keys = array();
          $marker_count = count($raisehandMarkerList);
 
          for ($i = 0; $i < $marker_count; $i++) {
            $keys[] = 'xx-marker' . $i . '-xx';
          }
 
          $text = str_replace($keys, $raisehandMarkerList, $text);
          return true;
          }
?>
