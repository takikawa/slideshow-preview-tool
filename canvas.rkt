#lang racket/base

;; Proof of concept slideshow preview GUI

(require racket/class
         racket/gui
         pict
         pict/snip
         slideshow
         slideshow/slides-to-picts)

(provide preview-canvas%)

;; constants
(define *slide-width* 300)
(define *slide-height* 225)

(define preview-canvas%
  (class editor-canvas%
    ;; DrRacket's text editor object
    (define definitions-text #f)
    ;; The last update time of the slides
    (define last-seconds 0)
    ;; pasteboard to draw into
    (define pasteboard (new pasteboard%))

    (super-new [editor pasteboard])

    ;; path -> (listof pict)
    ;; generate new picts from the slideshow
    (define (get-slides path)
      (define picts #f)
      (define thd
        (thread
         (λ ()
           (set! picts
                 (get-slides-as-picts
                  (path->string path)
                  *slide-width* *slide-height*
                  #t)))))
      (yield thd)
      picts)

    ;; sets the editor to base the preview off of, reset time
    (define/public (set-text! txt)
      (set! definitions-text txt)
      (set! last-seconds 0))

    ;; update the picts in the pasteboard
    (define/public (do-update)
      (define path
        (send definitions-text get-filename))

      (cond [path
             (set! last-seconds (current-seconds))
             ;; save old view for scrolling to
             ;; TODO: it would be more useful to scroll to the current
             ;;       index in the slide list instead
             (define-values (x y w h) (get-current-view))

             (define slides (get-slides path))
             (send pasteboard erase)

             (for/fold ([y 0])
                       ([slide (in-list slides)])
               (define snip
                 (new pict-snip% [pict (frame slide)]))
               (send pasteboard insert snip 10 y)
               (+ y (+ *slide-height* 25)))

             (send this scroll-to x y w h #t)]))

    ;; -> (values real real real real)
    (define (get-current-view)
      (define-values (x y w h)
        (values (box 0) (box 0) (box 0) (box 0)))
      (send (send pasteboard get-admin) get-view x y w h)
      (values (unbox x) (unbox y) (unbox w) (unbox h)))

    (define (notify-callback)
      (when definitions-text
        (define path
          (send definitions-text get-filename))
        
        (when path 
          (define modified
            (file-or-directory-modify-seconds path))
          (when (> modified last-seconds)
            (do-update)))))

    (new timer% [notify-callback notify-callback]
                [interval 500])))
