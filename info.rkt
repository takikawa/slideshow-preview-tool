#lang setup/infotab

(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names (list "Slideshow Preview"))
(define drracket-tool-icons '(#f))

(define blurb '("Slideshow preview tool for DrRacket"))
(define categories '(devtools))
(define primary-file "tool.rkt")

(define deps '("base" "gui-lib" "drracket-plugin-lib"))
