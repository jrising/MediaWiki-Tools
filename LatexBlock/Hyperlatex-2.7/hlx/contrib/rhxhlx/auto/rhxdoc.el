(TeX-add-style-hook "rhxdoc"
 (function
  (lambda ()
    (TeX-add-symbols
     '("xxlink" 3)
     '("Xlink" 2)
     "HlxIcons"
     "MyName"
     "MyEMail"
     "MyHomePage"
     "MyOfficeHomePage"
     "HlxNavigationPanelTitle")
    (TeX-run-style-hooks
     "fancyhdr"
     "lastpage"
     "color"
     "hyperlatex"
     "latex2e"
     "art10"
     "article"
     "a4paper"
     "rh-hyperlatex"))))

