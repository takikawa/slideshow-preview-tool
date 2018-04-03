#lang racket/unit

(require drracket/tool
         framework
         mrlib/switchable-button
         racket/class
         racket/draw
         racket/gui
         "canvas.rkt")

(import drracket:tool^)
(export drracket:tool-exports^)

(define slideshow-preview-frame-mixin
  (mixin (drracket:unit:frame<%>) ()
    (inherit get-button-panel
             get-definitions-text
             register-toolbar-button)

    (define preview-canvas #f)
    (define preview-visible? #f)
    (define dragable-parent #f)
    (define preview-parent-panel #f)

    (define/public (show-preview)
      (set! preview-visible? #t)
      (send dragable-parent begin-container-sequence)
      (send dragable-parent change-children
            (λ (lst)
              (append (remq preview-canvas lst)
                      (list preview-canvas))))
      ;(send dragable-parent set-percentages '(3/4 1/4))
      (send dragable-parent end-container-sequence))

    (define/private (hide-preview)
      (set! preview-visible? #f)
      (send dragable-parent change-children
            (λ (lst)
              (remq preview-canvas lst))))

    (define/private (toggle-preview)
      (if preview-visible?
          (hide-preview)
          (show-preview)))

    (define/override (make-root-area-container cls parent)
      (set! dragable-parent
            (super make-root-area-container
                   horizontal-panel%
                   parent))
      (define root (make-object cls dragable-parent))
      (set! preview-canvas
            (new preview-canvas% [parent dragable-parent]))
      (send dragable-parent
            change-children
            (λ (lst) (remq preview-canvas lst)))
      root)

    (super-new)

    (define button
      (new switchable-button%
           [label "Slideshow Preview"]
           [bitmap (make-bitmap 1 1)]
           [callback (λ (button)
                       (set-field! definitions-text
                                   preview-canvas
                                   (get-definitions-text))
                       (show-preview))]
           [parent (get-button-panel)]))

    (register-toolbar-button button #:number 44)
    (send (get-button-panel)
          change-children
          (λ (lst)
            (cons button (remq button lst))))))

(define (phase1) (void))
(define (phase2) (void))
(drracket:get/extend:extend-unit-frame slideshow-preview-frame-mixin)