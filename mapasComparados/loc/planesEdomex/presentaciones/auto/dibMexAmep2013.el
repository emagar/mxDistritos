(TeX-add-style-hook "dibMexAmep2013"
 (lambda ()
    (TeX-add-symbols
     '("e" 1)
     '("smfrac" 2)
     "be"
     "ee"
     "bq"
     "eq"
     "bd"
     "ed"
     "bi"
     "ei"
     "beq"
     "eeq"
     "bc"
     "ec"
     "mc")
    (TeX-run-style-hooks
     "arydshln"
     "multirow"
     "tikz"
     "graphicx"
     "babel"
     "spanish"
     "inputenc"
     "ansinew"
     "fontenc"
     "T1"
     "ae"
     "url"
     "beamerthemesplit"
     "pifont"
     "color"
     "latex2e"
     "beamer10"
     "beamer")))

