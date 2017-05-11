#lang racket

(require 2htdp/image)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (list->matrix l)
  (define (img_matrix-help inp out)
    (if (null? inp) out
        (img_matrix-help (drop inp img_width) (append out (list (take inp img_width))))))
  (img_matrix-help l '()))

(define img "")
(define (img_set! i) (set! img i)
  (set! img_list (image->color-list img))
  (set! img_width (image-width img))
  (set! img_height (image-height img))
  (set! img_matrix (list->matrix img_list))
  (set! a  (foldr op '() (reverse (sort (map (lambda (s) (color-red s)) img_list) <))))
  (set! count (reverse a))
  (set! diag (exact-floor (sqrt (+ (* img_height img_height) (* img_width img_width))))))

(define a  '())
(define count '())
(define img_list '())
(define img_width 0)
(define img_height 0)
(define img_matrix '())
(define diag 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ABSTRACTION

(define (transform f . dim)
  (λ arg
    (when (null? dim) (set! dim (list img_width img_height)))
    (color-list->bitmap (apply f arg) (car dim) (cadr dim))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COLOR FILTER / HUE

(define (color-value? x) (and (>= x 0) (<= x 255)))

(define/contract (filter_list r g b)
  (color-value? color-value? color-value? . -> . list?)
  (map (λ (clr) (let [(avg (/ (exact-floor (+ (* 0.2126 (color-red clr))
                                             (* 0.7152 (color-green clr))
                                             (* 0.0722 (color-blue clr)))) 255))]
                  (color (exact-floor (* r avg))
                        (exact-floor (* g avg))
                        (exact-floor (* b avg))))) img_list))

(define filter (transform filter_list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GRAYSCALE
(define (grayscale_list)
  (map (λ (clr) (let [(avg (exact-floor (+ (* 0.2126 (color-red clr))
                                          (* 0.7152 (color-green clr))
                                          (* 0.0722 (color-blue clr)))))]
                  (color avg avg avg))) img_list))

(define grayscale (transform grayscale_list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CROP
(define (crop_list x1 x2 y1 y2)
    (append* (map (lambda (row) (drop (take row x2) x1)) (drop (take img_matrix y2) y1))))

(define cropper
  (λ (x1 x2 y1 y2)
    (let* [(xp1 (exact-floor (* 0.01 img_width x1)))
         (xp2 (exact-floor (* img_width (- 1 (* 0.01 x2)))))
         (yp1 (exact-floor (* 0.01 img_height y1)))
         (yp2 (exact-floor (* img_height (- 1 (* 0.01 y2)))))]
     ((transform crop_list (- xp2 xp1) (- yp2 yp1)) xp1 xp2 yp1 yp2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BRIGHTNESS
; p is percentage
(define percentage? (λ (p) (and (>= p 0) (<= p 100))))

(define/contract (brightness-inc_list p)
  (-> percentage? list?)
  (map (λ (clr) (let [(r (color-red clr))
                      (g (color-green clr))
                      (b (color-blue clr))]
                  (define (tint-clr v)
                    (exact-floor (+ v (* 0.01 p (- 255 v)))))
                  (color (tint-clr r) (tint-clr g) (tint-clr b)))) img_list))
(define brightness-inc (transform brightness-inc_list))

(define/contract (brightness-dec_list p)
  (-> percentage? list?)
  (map (λ (clr) (let [(r (color-red clr))
                      (g (color-green clr))
                      (b (color-blue clr))]
                  (define (shade-clr v)
                    (exact-floor (* 0.01 (- 100 p) v)))
                  (color (shade-clr r) (shade-clr g) (shade-clr b)))) img_list))
(define brightness-dec (transform brightness-dec_list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PERSPECTIVE

(define (perspective_list x1 y1 x2 y2)
  (let* [(a (/ (- img_width (* 2 x1)) 2))
         (c  (/ (- img_width (* 2 x2)) 2))
         (b (- y2 y1))
         (h (sqrt (+ (* (- c a) (- c a)) (* 1.3 b b))))
         (temp1 (/ 1 h))]
    (append* (build-list (exact-floor h) (λ (row) (build-list (* 2 c) (λ (col) (list-ref  (list-ref img_matrix (+ y1 (exact-floor (* row b temp1))))
                                                                                    (+ x2 (exact-floor (+ c (* (- (/ col c) 1) (+ a (* (- c a) row temp1))))))))))))))


(define perspective
  (λ (x1 y1 x2 y2)
    (let* [(xp1 (exact-floor (* 0.01 img_width x1)))
           (xp2 (exact-floor (* img_width 0.01 x2)))
           (yp1 (exact-floor (* 0.01 img_height y1)))
           (yp2 (exact-floor (* img_height 0.01 y2)))
           (a (/ (- img_width (* 2 xp1)) 2))
           (c  (/ (- img_width (* 2 xp2)) 2))
           (b (- yp2 yp1))
           (h (sqrt (+ (* (- c a) (- c a)) (* 1.3 b b))))]
      ((transform perspective_list (- img_width (* 2 xp2)) (exact-floor h)) xp1 yp1 xp2 yp2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rotate_list theta_deg)
  (define theta (* 0.0174532925199 theta_deg))
  (define (f matrix val)
    (if (or (< val 0) (>= val (length matrix))) (list (color 255 255 255 255))
       (list-ref matrix val)))
  (define (g l val)
    (if (or (< val 0) (>= val (length l))) (color 255 255 255 255)
       (list-ref l val)))
  (let [(sint (sin theta))
        (cost (cos theta))
        (x (/ img_width 2))
        (y (/ img_height 2))]
    (append* (build-list diag (λ (row) (build-list diag (λ (col) (g (f img_matrix (exact-floor (+ (- (* (- row x) cost) (* (- col y) sint)) y)))
                                                                            (exact-floor (+ (* (- col y) cost) (* (- row x) sint) x))))))))))

(define (rotater theta) ((transform rotate_list diag diag) theta))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (op x y)
  (define (next-same l)
  (cons (list (caar l) (+ (cadar l) 1)) (cdr l)))
(define (next-different l n k)
  (cons (list n (+ k 1)) l))
  (if (null? y) (cons (list x 1) '())
  (if (= (caar y) x) (next-same y)
      (next-different y x (cadar y)))))

(define (histogram_pixel s)
  (define (his-equi l)
    (define (equi p)
      (cons (car p) (exact-round (/ (* 255 (- (/ (cadr p) (cadar a)) 0)) 1))))
    (map (lambda (p) (equi p)) l))
  (let* [(distri (his-equi count))
         (val (find (color-red s) distri))]
    (color val val val 255)))

(define (find n l)
  (if (null? l) (display n)
  (cond [(= (caar l) n) (cdar l)]
       [else (find n (cdr l))])))

(define (histogram_list) (map (lambda (s) (histogram_pixel s)) img_list))
(define histogram (transform histogram_list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;s

;; Contributin calcualtor for gaussian blur

(define (gaussian-blur_weight x y k)
  (* 0.15915494309189535 (/ (exp (- (/ (+ (* x x) (* y y)) (* 2 k k)))) (* k k))))

(define (img_matrix-element r c)
  (cond [(< r 0) (img_matrix-element (* -1 r) c)]
       [(< c 0) (img_matrix-element r (* -1 c))]
       [(>= r img_width) (img_matrix-element (- img_width 1) c)]
       [(>= c img_height) (img_matrix-element r (- img_height 1))]
       [else (list-ref (list-ref img_matrix c) r)]))

(define (gaussian-blur_element2 x y k)
  (define val-r 0)
  (define val-g 0)
  (define val-b 0)
  (define sum 0)
  (define (gaussian-blur_element-row x y k row)
    (define (gaussian-blur_element-col x y k row col)
      (when (<= col (* 3 k))
        (let* [(w (gaussian-blur_weight row col k))]
          (cond [(and (= row 0) (= col 0)) (set! val-r (+ val-r (* (color-red (img_matrix-element x y)) w)))
                                          (set! val-g (+ val-g (* (color-green (img_matrix-element x y)) w)))
                                          (set! val-b (+ val-b (* (color-blue (img_matrix-element x y)) w)))
                                          (set! sum (+ sum w))]
               [(= row 0) (begin (set! val-r (+ val-r (* (color-red (img_matrix-element x (+ col y))) w)))
                                (set! val-r (+ val-r (* (color-red (img_matrix-element x (- col y))) w)))
                                (set! val-g (+ val-g (* (color-green (img_matrix-element x (+ col y))) w)))
                                (set! val-g (+ val-g (* (color-green (img_matrix-element x (- col y))) w)))
                                (set! val-b (+ val-b (* (color-blue (img_matrix-element x (+ col y))) w)))
                                (set! val-b (+ val-b (* (color-blue (img_matrix-element x (- col y))) w)))
                                (set! sum (+ sum (* 2 w))))]
               [(= col 0) (begin (set! val-r (+ val-r (* (color-red (img_matrix-element (+ row x) y)) w)))
                                (set! val-r (+ val-r (* (color-red (img_matrix-element (- row x) y)) w)))
                                (set! val-g (+ val-g (* (color-green (img_matrix-element (+ row x) y)) w)))
                                (set! val-g (+ val-g (* (color-green (img_matrix-element (- row x) y)) w)))
                                (set! val-b (+ val-b (* (color-blue (img_matrix-element (+ row x) y)) w)))
                                (set! val-b (+ val-b (* (color-blue (img_matrix-element (- row x) y)) w)))
                                (set! sum (+ sum (* 2 w))))]
               [else (begin
                       (set! val-r (+ val-r (* (color-red (img_matrix-element (+ row x) (+ col y))) w)))
                       (set! val-r (+ val-r (* (color-red (img_matrix-element (+ row x) (- col y))) w)))
                       (set! val-r (+ val-r (* (color-red (img_matrix-element (- row x) (+ col y))) w)))
                       (set! val-r (+ val-r (* (color-red (img_matrix-element (- row x) (- col y))) w)))
                       (set! val-g (+ val-g (* (color-green (img_matrix-element (+ row x) (+ col y))) w)))
                       (set! val-g (+ val-g (* (color-green (img_matrix-element (+ row x) (- col y))) w)))
                       (set! val-g (+ val-g (* (color-green (img_matrix-element (- row x) (+ col y))) w)))
                       (set! val-g (+ val-g (* (color-green (img_matrix-element (- row x) (- col y))) w)))
                       (set! val-b (+ val-b (* (color-blue (img_matrix-element (+ row x) (+ col y))) w)))
                       (set! val-b (+ val-b (* (color-blue (img_matrix-element (+ row x) (- col y))) w)))
                       (set! val-b (+ val-b (* (color-blue (img_matrix-element (- row x) (+ col y))) w)))
                       (set! val-b (+ val-b (* (color-blue (img_matrix-element (- row x) (- col y))) w)))
                       (set! sum (+ sum (* 4 w))))]))
        (gaussian-blur_element-col x y k row (+ col 1))))
    (when (<= row (* 3 k))
      (gaussian-blur_element-col x y k row 0)
      (gaussian-blur_element-row x y k (+ row 1))))
  (gaussian-blur_element-row x y k 0)
  (color (exact-floor (/ val-r sum)) (exact-floor (/ val-g sum)) (exact-floor (/ val-b sum))))

(define (gaussian-blur_element x y k)
  (color (gaussian-blur_element x y k color-red)
         (gaussian-blur_element x y k color-green)
         (gaussian-blur_element x y k color-blue)))

(define (gaussian-blur_list k)
  (define (gaussian-blur-helper x y l k)
    (if (= y 0) (if (= x 0) (cons (gaussian-blur_element2 0 0 k) l)
                    (gaussian-blur-helper (- x 1) 0 (cons (gaussian-blur_element2 x 0 k) l) k))
        (if (= x 0) (gaussian-blur-helper (- img_width 1) (- y 1) (cons (gaussian-blur_element2 0 y k) l) k)
            (gaussian-blur-helper (- x 1) y (cons (gaussian-blur_element2 x y k) l) k))))
  (gaussian-blur-helper (- img_width 1) (- img_height 1) '() k))

(define (gaussian-blur k_100)
  (define k (/ k_100 20))
  (color-list->bitmap (gaussian-blur_list k) img_width img_height))

