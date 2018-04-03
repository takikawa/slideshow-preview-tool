#lang racket/base

;; Proof of concept slideshow preview GUI

(require racket/class
         racket/gui
         pict
         slideshow
         slideshow/slides-to-picts)

(provide preview-canvas%)

(define preview-canvas%
  (class canvas%
    (field ;; DrRacket's frame (if set)
           [definitions-text #f]
           ;; used to check file modification time
           [last-seconds 0])

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

    (define (paint-callback canvas dc)
      (define path
        (send definitions-text get-filename))

      (cond [path
             (define slides (get-slides path))

             (for/fold ([y 0])
                       ([slide (in-list slides)])
               (draw-pict (frame slide) dc 0 y)
               (+ y 250))]
            [(draw-pict (text "Please load a file or save the buffer to use")
                        dc 0 0)]))

    (super-new [paint-callback paint-callback]
               [style '(vscroll)])

    (define (notify-callback)
      (when definitions-text
        (define path
          (send definitions-text get-filename))
        
        (when path 
          (define modified
            (file-or-directory-modify-seconds path))
          (when (> modified last-seconds)
            (set! last-seconds (current-seconds))
            (send this refresh-now)))))

    (new timer% [notify-callback notify-callback]
                [interval 1000])))
