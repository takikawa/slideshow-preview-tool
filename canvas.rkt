#lang racket/base

;; Proof of concept slideshow preview GUI

(require racket/class
         racket/gui
         pict
         pict/snip
         slideshow
         slideshow/slides-to-picts)

(provide preview-canvas%)

(define preview-canvas%
  (class editor-canvas%
    (field ;; DrRacket's frame (if set)
           [definitions-text #f]
           ;; used to check file modification time
           [last-seconds 0])

    (define pasteboard (new pasteboard%))
    (super-new [editor pasteboard])

    (define (modified-seconds)
      (or (and definitions-text
               (let ([path (send definitions-text get-filename)])
                 (file-or-directory-modify-seconds path)))
          0))

    (define last-picts #f)
    (define (get-slides path)
      (cond [(>= (abs (- (modified-seconds) last-seconds))
                 1)
             (set! last-picts
                   (get-slides-as-picts (path->string path) 300 225 #t))
             last-picts]
            [else last-picts]))

    ;; update the picts in the pasteboard
    (define/public (do-update)
      (define path
        (send definitions-text get-filename))

      (cond [path
             ;; save old view for scrolling to
             (define-values (x y w h) (get-current-view))

             (define slides (get-slides path))
             (send pasteboard erase)

             (for/fold ([y 0])
                       ([slide (in-list slides)])
               (define snip
                 (new pict-snip% [pict (frame slide)]))
               (send pasteboard insert snip 10 y)
               (+ y 250))

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
            (set! last-seconds (current-seconds))
            (do-update)))))

    (new timer% [notify-callback notify-callback]
                [interval 1000])))
