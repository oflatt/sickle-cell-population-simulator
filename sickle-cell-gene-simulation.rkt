#lang racket
(require rackunit 2htdp/universe 2htdp/image mzlib/string
         (only-in racket/gui/base get-display-size))

;; states are idle, edit-dominant, edit-recessive, edit-malaria, and results


(struct button (name x y code))
(struct exp (state dominant recessive malaria%) #:transparent)

;(define-values (WIDTH-unchanged HEIGHT-unchanged) (get-display-size))
(define WIDTH 950)
(define HEIGHT 950)
(define GEN 0)
;;R stands for results, S is domianant allele and s is recessive sickle-cell allele. If there is -m it means those ones got malaria
(define R-SS 0)
(define R-Ss 0)
(define R-ss 0)
(define R-SS-m 0)
(define R-Ss-m 0)
(define R-ss-m 0)
(define DOUBLE? #f)
(define BUTTONS
  (list
   (button
    'up-dominant
    (/ WIDTH 5)
    (/ HEIGHT 6)
    (overlay
     (above
      (triangle (/ WIDTH 40) "solid" "purple")
      (rectangle (/ WIDTH 160) (/ WIDTH 60) "solid" "purple"))
     (square (/ WIDTH 20) "solid" "black")))
   (button
    'down-dominant
    (/ WIDTH 5)
    (* HEIGHT 2/8)
    (rotate 180
            (overlay
             (above
              (triangle (/ WIDTH 40) "solid" "purple")
              (rectangle (/ WIDTH 160) (/ WIDTH 60) "solid" "purple"))
             (square (/ WIDTH 20) "solid" "black"))))
   (button
    'dominant
    (round (/ WIDTH 80))
    (/ HEIGHT 5)
    (rectangle (/ WIDTH 6) (/ HEIGHT 20) "outline" "black"))
   (button
    'up-recessive
    (* WIDTH (+ 1/15 2/5))
    (* HEIGHT 1/6)
    (overlay
     (above
      (triangle (/ WIDTH 40) "solid" "purple")
      (rectangle (/ WIDTH 160) (/ WIDTH 60) "solid" "purple"))
     (square (/ WIDTH 20) "solid" "black")))
   (button
    'down-recessive
    (* WIDTH (+ 1/15 2/5))
    (* HEIGHT 2/8)
    (rotate 180
            (overlay
             (above
              (triangle (/ WIDTH 40) "solid" "purple")
              (rectangle (/ WIDTH 160) (/ WIDTH 60) "solid" "purple"))
             (square (/ WIDTH 20) "solid" "black"))))
   (button
    'recessive
    (round (* WIDTH (+ 1/80 1/5 1/16)))
    (/ HEIGHT 5)
    (rectangle (/ WIDTH 6) (/ HEIGHT 20) "outline" "black"))
   (button
    'up-malaria
    (* WIDTH (+ 1/15 4/5))
    (* HEIGHT 1/6)
    (overlay
     (above
      (triangle (/ WIDTH 40) "solid" "purple")
      (rectangle (/ WIDTH 160) (/ WIDTH 60) "solid" "purple"))
     (square (/ WIDTH 20) "solid" "black")))
   (button
    'down-malaria
    (* WIDTH (+ 1/15 4/5))
    (* HEIGHT 2/8)
    (rotate 180
            (overlay
             (above
              (triangle (/ WIDTH 40) "solid" "purple")
              (rectangle (/ WIDTH 160) (/ WIDTH 60) "solid" "purple"))
             (square (/ WIDTH 20) "solid" "black"))))
   (button
    'malaria%
    (round (* WIDTH (+ 1/80 3/5 1/16)))
    (/ HEIGHT 5)
    (rectangle (/ WIDTH 6) (/ HEIGHT 20) "outline" "black"))
   (button
    'reset
    (* WIDTH 6/8)
    (/ HEIGHT 15)
    (overlay
     (text "reset" (round (/ WIDTH 40)) "black")
     (rectangle (/ WIDTH 6) (/ HEIGHT 25) "solid" "yellow")))
    (button
    'next
    (- (* WIDTH 1/2) (* WIDTH 1/6))
    (* HEIGHT 1/3)
    (overlay
     (text "Next Generation" (round (/ WIDTH 30)) "black")
     (rectangle (/ WIDTH 3) (/ HEIGHT 20) "solid" "yellow")))
    (button
    'double
    (- (* WIDTH 1/2) (* WIDTH 1/6))
    (* HEIGHT 9/12)
    (overlay
     (text "Double Population" (round (/ WIDTH 30)) "black")
     (rectangle (/ WIDTH 3) (/ HEIGHT 20) "solid" "yellow")))
    (button
     'double-each-time
     (- (* WIDTH 1/2) (* WIDTH 1/4))
     (* HEIGHT 10/12)
     (overlay
      (text "Double Population Each Time" (round (/ WIDTH 30)) "black")
      (rectangle (/ WIDTH 2) (/ HEIGHT 20) "solid" "yellow")))
    (button
     'double-each-time-checkbox
     (+ (* WIDTH 1/2) (* WIDTH 1/4))
     (* HEIGHT 10/12)
     (square (/ HEIGHT 20) "outline" "black"))
    (button
     'warning
     (- (* WIDTH 1/2) (* WIDTH 1/3))
     (* HEIGHT 11/12)
     (text "Larger numbers may cause longer computational time" (round (/ WIDTH 35)) "red"))))

(define (clicked-something s x y k)
  (cond
    [(equal? k "button-down")
     (button-down s x y)]
    [else s]))

(define (button-down s x y)
  (cond
    [(this-button-clicked? (find-this-button BUTTONS 'double-each-time) x y)
     (cond
       [DOUBLE?
        (set! DOUBLE? #f)
        s]
       [else
        (set! DOUBLE? #t)
        s])]
    [(this-button-clicked? (find-this-button BUTTONS 'double-each-time-checkbox) x y)
     (cond
       [DOUBLE?
        (set! DOUBLE? #f)
        s]
       [else
        (set! DOUBLE? #t)
        s])]
    [(this-button-clicked? (find-this-button BUTTONS 'next) x y)
     (begin
       (set! GEN (+ GEN 1))
       (next-generation s))]
    [(this-button-clicked? (find-this-button BUTTONS 'malaria%) x y)
     (exp 'edit-malaria% (exp-dominant s) (exp-recessive s) (exp-malaria% s))]
    [(this-button-clicked? (find-this-button BUTTONS 'reset) x y)
     (begin
       (set! GEN 0)
       (exp 'idle 0 0 50))]
    [(this-button-clicked? (find-this-button BUTTONS 'recessive) x y)
     (exp 'edit-recessive (exp-dominant s) (exp-recessive s) (exp-malaria% s))]
    [(this-button-clicked? (find-this-button BUTTONS 'dominant) x y)
     (exp 'edit-dominant (exp-dominant s) (exp-recessive s) (exp-malaria% s))]
    [(and (this-button-clicked? (find-this-button BUTTONS 'up-dominant) x y) (not (> (exp-dominant s) 9999999)))
     (exp (exp-state s) (+ (exp-dominant s) 1) (exp-recessive s) (exp-malaria% s))]
    [(and (this-button-clicked? (find-this-button BUTTONS 'down-dominant) x y) (> (exp-dominant s) 0))
     (exp (exp-state s) (- (exp-dominant s) 1) (exp-recessive s) (exp-malaria% s))]
    [(and (this-button-clicked? (find-this-button BUTTONS 'up-recessive) x y) (not (> (exp-recessive s) 9999999)))
     (exp (exp-state s) (exp-dominant s) (+ (exp-recessive s) 1) (exp-malaria% s))]
    [(and (this-button-clicked? (find-this-button BUTTONS 'down-recessive) x y) (> (exp-recessive s) 0))
     (exp (exp-state s) (exp-dominant s) (- (exp-recessive s) 1) (exp-malaria% s))]
    [(and (this-button-clicked? (find-this-button BUTTONS 'down-malaria) x y) (> (exp-malaria% s) 0))
     (exp (exp-state s) (exp-dominant s) (exp-recessive s) (- (exp-malaria% s) 1))]
    [(and (this-button-clicked? (find-this-button BUTTONS 'up-malaria) x y) (< (exp-malaria% s) 100))
     (exp (exp-state s) (exp-dominant s) (exp-recessive s) (+ (exp-malaria% s) 1))]
    [(this-button-clicked? (find-this-button BUTTONS 'double) x y)
     (exp (exp-state s) (* 2 (exp-dominant s)) (* 2 (exp-recessive s)) (exp-malaria% s))]
    [else s]))

(define (next-generation s)
  (cond
    [DOUBLE?
     (begin
       (next-gen-figures s)
       (exp 'results (* 2 (+ R-SS R-SS R-Ss R-Ss-m)) (* 2 (+ R-Ss R-Ss-m)) (exp-malaria% s)))]
    [else
     (begin
       (next-gen-figures s)
       (exp 'results (+ R-SS R-SS R-Ss R-Ss-m) (+ R-Ss R-Ss-m) (exp-malaria% s)))]))

(define (next-gen-figures s)
  (begin
    (set-them-all-to-0)
    (begin
      (make-babies (exp-dominant s) (exp-recessive s))
      (apply-malaria s R-SS R-Ss R-ss))))

(define (set-them-all-to-0)
  (set! R-SS 0)
  (set! R-Ss 0)
  (set! R-ss 0)
  (set! R-SS-m 0)
  (set! R-Ss-m 0)
  (set! R-ss-m 0))
              
(define (apply-malaria s d h r)
  (define malaria? (random 100))
  (cond
    [(> d 0)
     (cond
       [(< malaria? (exp-malaria% s))
        (begin
          (set! R-SS (- R-SS 1))
          (begin
            (set! R-SS-m (+ R-SS-m 1))
            (apply-malaria s (- d 1) h r)))]
       [else (apply-malaria s (- d 1) h r)])]
    [(> h 0)
     (cond
       [(< malaria? (exp-malaria% s))
        (begin
          (set! R-Ss (- R-Ss 1))
          (begin
            (set! R-Ss-m (+ R-Ss-m 1))
            (apply-malaria s d (- h 1) r)))]
       [else (apply-malaria s d (- h 1) r)])]
    [(> r 0)
     (cond
       [(< malaria? (exp-malaria% s))
        (begin
          (set! R-ss (- R-ss 1))
          (begin
            (set! R-ss-m (+ R-ss-m 1))
            (apply-malaria s d h (- r 1))))]
       [else (apply-malaria s d h (- r 1))])]
    [else (+ 1 1)]))

(define (make-babies d r)
  (cond
    [(< (+ d r) 2)
     (void)]
    [(= d 0)
     (begin
       (set! R-ss (+ R-ss 1))
       (make-babies d (- r 2)))]
    [(= r 0)
     (begin
       (set! R-SS (+ R-SS 1))
       (make-babies (- d 2) r))]
    [(and (= d 1) (= r 1))
     (begin
       (set! R-Ss (+ R-Ss 1))
       (make-babies (- d 1) (- r 1)))]
    [(= d 1)
     (begin
       (set! R-Ss (+ R-Ss 1))
       (make-babies (- d 1) (- r 1)))]
    [(= r 1)
     (begin
       (set! R-Ss (+ R-Ss 1))
       (make-babies (- d 1) (- r 1)))]
    [else
     (define random-mating
       (list
        (random (+ d r))
        (random (+ d r))))
     (cond
       [(and (>= (- d 1) (first random-mating)) (>= (- d 1) (second random-mating)))
        (begin
          (set! R-SS (+ R-SS 1))
          (make-babies (- d 2) r))]
       [(and (<= d (first random-mating)) (<= d (second random-mating)))
        (begin
          (set! R-ss (+ R-ss 1))
          (make-babies d (- r 2)))]
       [else
        (begin
          (set! R-Ss (+ R-Ss 1))
          (make-babies (- d 1) (- r 1)))])]))

;;takes a name of a button and a list of buttons and gives back a button
(define (find-this-button bs name)
  (cond
    [(empty? bs)
     #f]
    [(equal? name (button-name (first bs)))
     (first bs)]
    [else (find-this-button (rest bs) name)]))

(define (this-button-clicked? b x y)
  (cond
    [(and (> x (button-x b)) (< x (+ (button-x b) (image-width (button-code b)))) (> y (button-y b)) (< y (+ (button-y b) (image-height (button-code b)))))
     #t]
    [else #f]))


;;########################################################################################DRAW##############################################################DRAW
(define (draw-science s)
  (define base 
    (draw-buttons-and-other
     s
     (base-of-draw s)))
  (cond
    [(equal? (exp-state s) 'results)
     (results-draw s base)]
    [(equal? (exp-state s) 'edit-recessive)
     (overlay/xy (rectangle (/ WIDTH 6) (/ HEIGHT 20) "outline" "blue")
                 (- (round (* WIDTH (+ 1/80 1/5 1/16))))
                 (- (/ HEIGHT 5))
                 base)]
    [(equal? (exp-state s) 'edit-dominant)
     (overlay/xy (rectangle (/ WIDTH 6) (/ HEIGHT 20) "outline" "blue")
                 (- (round (/ WIDTH 80)))
                 (- (/ HEIGHT 5))
                 base)]
    [(equal? (exp-state s) 'edit-malaria%)
     (overlay/xy (rectangle (/ WIDTH 6) (/ HEIGHT 20) "outline" "blue")
                 (- (round (* WIDTH (+ 1/80 3/5 1/16))))
                 (- (/ HEIGHT 5))
                 base)]
    [else base]))

(define (results-draw s base)
  (overlay/xy
   (above
    (above
     (beside
      (text (number->string (+ R-SS R-SS-m)) (round (/ HEIGHT 25)) "black")
      (text " had no sickle cell- homozygous for dominant" (round (/ HEIGHT 25)) "black"))
     (beside
      (text (number->string R-SS-m) (round (/ HEIGHT 25)) "black")
      (text " of these got malaria" (round (/ HEIGHT 25)) "black")))
    (above
     (beside
      (text (number->string (+ R-Ss R-Ss-m)) (round (/ HEIGHT 25)) "black")
      (text " had partial sickled cells- heterozygous individuals" (round (/ HEIGHT 25)) "black"))
     (beside
      (text (number->string R-Ss-m) (round (/ HEIGHT 25)) "black")
      (text " of these got malaria" (round (/ HEIGHT 25)) "black")))
    (above
     (beside
      (text (number->string (+ R-ss R-ss-m)) (round (/ HEIGHT 25)) "black")
      (text " had complete sickled cells- homozygous for recessive" (round (/ HEIGHT 25)) "black"))
     (beside
      (text (number->string R-ss-m) (round (/ HEIGHT 25)) "black")
      (text " of these got malaria" (round (/ HEIGHT 25)) "black"))))
   (- (- (* WIDTH 1/2) (/ HEIGHT 2)))
   (- (* HEIGHT 4/10))
   base))

(define (draw-buttons-and-other s pic)
  (overlay/xy
   (beside (text "GEN " (round (/ WIDTH 25)) "black") (text (number->string GEN)  (round (/ WIDTH 25)) "black"))
   (- (round (* WIDTH 6/8)))
   -2
   (overlay/xy
    (text "%" (round (/ WIDTH 35)) "black")
    (- (round (* WIDTH (+ 1/2 23/80)))) (- (round (/ HEIGHT 5)))
    (overlay/xy
     (text (number->string (exp-malaria% s)) (round (/ WIDTH 35)) "black")
     (- (round (* WIDTH (+ 23/80 2/5)))) (- (round (/ HEIGHT 5)))
     (overlay/xy
      (text (number->string (exp-recessive s)) (round (/ WIDTH 35)) "black")
      (- (round (* WIDTH 23/80))) (- (round (/ HEIGHT 5)))
      (overlay/xy
       (text (number->string (exp-dominant s)) (round (/ WIDTH 35)) "black")
       (- (round (/ WIDTH 40))) (- (round (/ HEIGHT 5)))
       (overlay/xy
        (text "# of dominant genes    # of recessive genes       chance of malaria(per individual)" (round (/ WIDTH 40)) "black")
        -1
        (- (/ HEIGHT 8))
        (overlay/xy
         (text "Gene Pool" (round (/ WIDTH 25)) "red")
         (- (* WIDTH 1/8))
         (- (/ HEIGHT 15))
         (draw-buttons BUTTONS pic)))))))))

(define (draw-buttons b pic)
  (cond
    [(empty? b) pic]
    [else
     (draw-buttons
      (rest b)
      (overlay/xy
       (button-code (first b))
       (- (button-x (first b))) (- (button-y (first b)))
       pic))]))

(define (base-of-draw s)
  (define base
    (overlay/xy
     (text "Sickle Cell Anemia Gene Simulator" (round (/ WIDTH 25)) "black")
     -20 -5
     (empty-scene WIDTH HEIGHT)))
  (cond
    [DOUBLE?
     (overlay/xy
      (add-line
       (add-line
        (square (/ HEIGHT 20) "outline" "black")
        10 10 40 40
        (make-pen "red" 8 "solid" "round" "round"))
       10 40 40 10
       (make-pen "red" 8 "solid" "round" "round"))
      (- (+ (* WIDTH 1/2) (* WIDTH 1/4)))
      (- (* HEIGHT 10/12))
      base)]
    [else base]))
      
;;########################################################################################DRAW##############################################################KEY

(define (editing s key)
  (cond
    [(or (equal? (exp-state s) 'edit-dominant) (equal? (exp-state s) 'edit-recessive))
     (normal-key s key)]
    [(equal? (exp-state s) 'edit-malaria%)
     (%key s key)]
    [(cond
       [(equal? key "\r")
        (begin
          (set! GEN (+ GEN 1))
          (next-generation s))]
       [(equal? key "\t")
        (exp 'edit-dominant (exp-dominant s) (exp-recessive s) (exp-malaria% s))]
       [else s])]))

(define (%key s key)
  (cond
    [(equal? key "\r")
     (begin
       (set! GEN (+ GEN 1))
       (next-generation s))]
    [(equal? key "\t")
     (exp 'edit-dominant (exp-dominant s) (exp-recessive s) (exp-malaria% s))]
    [(equal? key "\b")
     (exp (exp-state s) (exp-dominant s) (exp-recessive s) (truncate (/ (exp-malaria% s)10)))]
    [(or (equal? key "1") (equal? key "2") (equal? key "3") (equal? key "4") (equal? key "5") (equal? key "6") (equal? key "7") (equal? key "8") (equal? key "9") (equal? key "0"))
     (cond
       [(< (exp-malaria% s) 10)
        (exp (exp-state s) (exp-dominant s) (exp-recessive s) (+ (* 10 (exp-malaria% s)) (string->number key)))]
       [(and (equal? (exp-malaria% s) 10) (equal? key "0"))
        (exp (exp-state s) (exp-dominant s) (exp-recessive s) 100)]
       [else s])]
    [else s]))


(define (normal-key s key)
  (cond
    [(equal? key "\r")
     (begin
       (set! GEN (+ GEN 1))
       (next-generation s))]
    [(and (equal? key "\t") (equal? (exp-state s) 'edit-recessive))
     (exp 'edit-malaria% (exp-dominant s) (exp-recessive s) (exp-malaria% s))]
    [(and (equal? key "\t") (equal? (exp-state s) 'edit-dominant))
     (exp 'edit-recessive (exp-dominant s) (exp-recessive s) (exp-malaria% s))]
    [(equal? key "\b")
     (cond
       [(equal? (exp-state s) 'edit-recessive)
        (exp (exp-state s) (exp-dominant s) (truncate (/ (exp-recessive s) 10)) (exp-malaria% s))]
       [(equal? (exp-state s) 'edit-dominant)
        (exp (exp-state s) (truncate (/ (exp-dominant s) 10)) (exp-recessive s) (exp-malaria% s))]
       [else s])]
    [(or (equal? key "1") (equal? key "2") (equal? key "3") (equal? key "4") (equal? key "5") (equal? key "6") (equal? key "7") (equal? key "8") (equal? key "9") (equal? key "0"))
     (cond
       [(and (equal? (exp-state s) 'edit-recessive) (not (> (exp-recessive s) 9999999)))
        (exp (exp-state s) (exp-dominant s) (+ (* 10 (exp-recessive s)) (string->number key)) (exp-malaria% s))]
       [(and (equal? (exp-state s) 'edit-dominant) (not (> (exp-dominant s) 9999999)))
        (exp (exp-state s) (+ (* 10 (exp-dominant s)) (string->number key)) (exp-recessive s) (exp-malaria% s))]
       [else s])]
    [else s]))

(check-equal? (normal-key (exp 'edit-recessive 0 0 0) "1") (exp 'edit-recessive 0 1 0))
(check-equal? (normal-key (exp 'edit-recessive 0 10 0) "1") (exp 'edit-recessive 0 101 0))

(big-bang
 (exp 'idle 0 0 50)
 (on-mouse clicked-something)
 (on-draw draw-science)
 (on-key editing))
