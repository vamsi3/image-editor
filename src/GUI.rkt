#lang racket/gui
(require 2htdp/image)
(require "image.rkt")

(define x 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ABSTRACTIONS

(define/contract (make_frame name w h)
  (-> string? dimension-integer? dimension-integer? (is-a?/c frame%))
  (new frame% [label name] [width w] [height h]))

(define (sli l min max)
  (new slider%
       [label l]
       [min-value min]
       [max-value max]
       [parent settings_hpanel]
       [style '(horizontal deleted)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main_frame (make_frame "IMAGE EDITOR" 1000 700))

(define options+display_hpanel
  (new horizontal-panel%
      [parent main_frame]))

(new vertical-panel%
      [parent options+display_hpanel]
      [min-width 30]
      [stretchable-width #f])

(define options_vpanel
  (new vertical-panel%
       [parent options+display_hpanel]
       [min-width 100]
       [stretchable-width #f]))

(new horizontal-panel% [parent options_vpanel] [min-height 50][stretchable-height #f])
(define select
  (new button%
       [label "SELECT FILE"]
       [parent options_vpanel]
       [callback (lambda (button event) (begin (set! inp_path (get-file)) (img_set! (bitmap/file inp_path))
                                              (send inp refresh)
                                              (send input load-file inp_path)
                                              (send inp on-paint)))]))

(new horizontal-panel% [parent options_vpanel] [min-height 25][stretchable-height #f])

(define types
  (new choice%
       [label "FUNCTION :    "]
       [parent options_vpanel]
       [choices (list "    "   "FILTER" "CROP" "ROTATE" "GAUSSIAN-BLUR" "PERSPECTIVE"
                      "BRIGHTNESS" "GRAYSCALE" "AUTO-CONTRAST FIX")]
       [callback (lambda (choice event)
                   (set! x (send types get-selection))
                     (function x))]))
  
(new horizontal-panel% [parent options_vpanel] [min-height 70][stretchable-height #f])

(define settings_hpanel (new vertical-panel% [parent options_vpanel] [min-height 120][alignment '(center center)]))

(new horizontal-panel% [parent options_vpanel] [min-height 80])

(define process_panel (new vertical-panel% [parent options_vpanel] [min-height 120][stretchable-height #f]))


(new vertical-panel% [parent options+display_hpanel] [min-width 30] [stretchable-width #f])

(define display_vpanel
  (new vertical-panel%
       [parent options+display_hpanel]
       [min-width 600]))

(define inp
  (new canvas%
      [parent display_vpanel]
      [min-height 350]
      [paint-callback
       (λ (canvas dc)
         (begin
           (send dc draw-bitmap input (- 330 (/ img_width 2)) (- 150 (/ img_height 2)))))]))

(define outp
  (new canvas%
      [parent display_vpanel]
      [min-height 350]
      [paint-callback
       (λ (canvas dc)
         (begin
           (send dc draw-bitmap output (- 330 (/ img_width 2)) (- 150 (/ img_height 2)))))]))
(send main_frame show #t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define inp_path 0)
(define input (make-object bitmap% 350 350))
(define output (make-object bitmap% 350 350))



(define x1 (sli "LEFT" 0 100))
(define x2 (sli "RIGHT" 0 100))
(define y1 (sli "TOP" 0 100))
(define y2 (sli "BOTTOM" 0 100))
(define angle1 (sli "ANGLE :" 0 360))
(define k (sli "BLUR-STRENGTH : " 0 100))
(define x11 (sli "TOP-X-CO-ORDINATE                " 0 100))
(define y11 (sli "TOP-Y-CO-ORDINATE                " 0 100))
(define x21 (sli "BOTTOM-X-CO-ORDINATE        " 0 100))
(define y21 (sli "BOTTOM-Y-CO-ORDINATE        " 0 100))
(define brightness_percent (sli "PERCENTAGE" 0 100))
(define brightness_choice (new radio-box% [label ""] [choices '("Increase" "Decrease")]
                              [parent settings_hpanel][style '(horizontal deleted)]))

(define selecter
  (new button%
       [label "PROCESS THE IMAGE"]
       [parent process_panel]
       [callback (λ (button event)
                   (send outp refresh)
                   (cond [(= x 2)
                          (abs2 cropper x1 x2 y1 y2)]
                        [(= x 3) (abs2 rotater angle1)]
                        [(= x 4) (abs2 gaussian-blur k)]
                        [(= x 5) (abs2 perspective x11 y11 x21 y21)]
                        [(= x 6) (if (= (send brightness_choice get-selection) 0) (abs2 brightness-inc brightness_percent)
                                    (abs2 brightness-dec brightness_percent))]
                        [(= x 7) (abs2 grayscale)]
                        [(= x 8) (abs2 histogram)])
                   (send output load-file "saved.png")
                   (send outp on-paint))]))

(define (function x)
  (send settings_hpanel change-children (λ (l) '()))
  (cond [(= x 1)
       (new button%
           [label "Select COLOR"]
           [parent settings_hpanel]
           [callback (lambda (button event)
                       (let [(color_inp (get-color-from-user))]
                         (abs filter (send color_inp red)
                                      (send color_inp green)
                                      (send color_inp blue))))])]
       [(= x 2) (display-setting x1 x2 y1 y2)]
       [(= x 3) (display-setting angle1)]
       [(= x 4) (display-setting k)]
       [(= x 5) (display-setting x11 y11 x21 y21)]
       [(= x 6) (display-setting brightness_percent brightness_choice)]))
       
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define (display-setting . l) (map (λ (e) (send settings_hpanel add-child e)) l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (abs f . args)
    (save-image (apply f args) "saved.png"))

(define (abs2 f . args)
  (save-image (apply f (map (λ (e) (send e get-value)) args)) "saved.png"))