;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polynomialcalculator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ******************************************
;; Ace Zhan
;; Side Project
;; Polynomial calculator
;; ******************************************

(define poly1 (list (list 3 4)
                           (list 4 3)
                           (list 0 2)
                           (list 1 1)
                           (list 1 0)))
(define poly2 (list (list 1 2)
                           (list 3 1)
                           (list 1 0)))


;; (make-adja-lst lst) produces a adjacency list
;; adja-list: (listof Int) -> (listof (list Int Int))
;; Requires: lst has an even number of elements
;; Examples:
(check-expect (make-adja-lst (list 2 3 4 5))
              (list (list 2 3) (list 4 5)))

(define (make-adja-lst lst)
  (cond [(empty? lst) empty]
        [else (cons (list (first lst)
                          (second lst))
                    (make-adja-lst (rest (rest lst))))]))


;; (apply-coeff-quo val al) produces a new al with the first value
;;   of each list in al multiplied by val
;; apply-coeff-quo: Num AL -> AL
;; Examples:
(check-expect (apply-coeff-quo 3 (list (list 1 2)
                                       (list 3 4)))
              (list (list 3 2) (list 9 4)))

(define (apply-coeff-quo val al)
  (cond [(empty? al) empty]
        [else (cons (list (* (first (first al)) val)
                          (second (first al)))
                    (apply-coeff-quo val (rest al)))]))


;; (apply-degree-quo val al) produces a new al with val added onto
;;   the second value of each list in al
;; apply-degree-quo: Num AL -> AL
;; Examples:
(check-expect (apply-degree-quo 2 (list (list 3 2)
                                        (list 3 1)))
              (list (list 3 4)
                    (list 3 3)))

(define (apply-degree-quo val al)
  (cond [(empty? al) empty]
         [else (cons (list (first (first al))
                          (+ val (second (first al))))
                    (apply-degree-quo val (rest al)))]))


;; (coeff-quo lst1 lst2) produces the quotient with the first of lst1
;;   as the divisor and the first of lst2 as the dividend
;; coeff-quo: (list Int Int) (list Int Int) -> Num
;; Examples:
(check-expect (coeff-quo (list 1 2) (list 3 4)) 3)

(define (coeff-quo lst1 lst2)
  (/ (first lst2) (first lst1)))


;; (degree-diff lst1 lst2) produces the different between the second of lst1
;;   and the second of lst2
;; degree-quo: (list Int Int) (list Int Int) -> Num
;; Examples:
(check-expect (degree-quo (list 1 2) (list 3 4)) 2)

(define (degree-quo lst1 lst2)
  (- (second lst2) (second lst1)))

;; filler function
;; Example:
(check-expect (filler poly1) (list (list 3 4)
                    (list 4 3)
                    (list 0 2)
                    (list 1 1)
                    (list 1 0)))

(define (filler poly)
  (cond
    [(empty? poly) empty]
    [(empty? (rest poly)) (list (first poly))]
    [(= (sub1 (second (first poly)))
        (second (second poly))) (cons (first poly)
                                      (filler (rest poly)))]
    [else (cons (first poly)
                (cons (list 0 (sub1 (second (first poly))))
                      (filler (rest poly))))]))

;; fill-end function
;; Examples:
(check-expect (fill-end (list (list 2 1)))
              (list (list 2 1)
                    (list 0 0)))
(check-expect (fill-end (list (list 0 0)))
              (list (list 0 0)))

(define (fill-end poly)
  (local [(define (standard-lst val)
            (append (reverse (build-list val (lambda (x) (list 0 (add1 x)))))
                    (list (list 0 0))))
          (define (fill-end-acc poly lst acc)
            (cond [(empty? poly) (append acc lst)]
                  [else (fill-end-acc (rest poly) (rest lst)
                                      (append acc (list (first poly))))]))]
    (fill-end-acc poly (standard-lst (second (first poly))) empty)))


;; (subtract-poly al1 al2) produces a new al with al2 subtracted from al1
;; subtract-poly: AL AL -> (anyof AL Num)
;; Requires: al1 must have a larger degree than al2
;; Example:
(check-expect (subtract-poly poly1 poly2)
              (list (list 3 4) (list 4 3) (list -1 2) (list -2 1)))
              
(define (subtract-poly al1 al2)
  (local [(define (subtract-poly-helper t1 t2)
            (cond [(= (- (first t1) (first t2)) 0) empty]
                  [else (list (- (first t1) (first t2))
                              (second t1))]))
          (define (subtract-poly-acc al1 al2 acc)
            (cond
              [(empty? al2) (append (reverse acc) al1)]
              [(= (second (first al1)) (second (first al2)))
                   (subtract-poly-acc (rest al1)
                                      (rest al2)
                                      (cons (subtract-poly-helper
                                             (first al1)
                                             (first al2))
                                            acc))]
              [else (subtract-poly-acc
                     (rest al1) al2 (cons (first al1) acc))]))]
    
    (filter (lambda (x) (not (empty? x))) (subtract-poly-acc al1 al2 empty))))


;; (divider poly1 poly2) produces the quotient with poly2 as the divisor and
;;   poly1 as the dividend, if it is not divisable, it produces false
;; Examples:
(check-expect (divider (list (list 3 2) (list -5 1)
                             (list 12 0)) (list (list 3 1) (list 1 0)))
              (list (list 14 0)))
(check-expect (divider poly1 poly2) (list (list -30 1)
                                          (list -11 0)))
(check-expect (divider (list (list 1 2)
                             (list 2 1)
                             (list 1 0))
                       (list (list 1 1)
                             (list 1 0))) 0)
(check-expect (divider (list (list 1 0))
                       (list (list 1 0))) 0)


              
(define (divider poly1 poly2)
  (local [(define (divider-helper poly1 poly2)
            (cond
              [(empty? poly1) 0]
              [(< (second (first poly1))
                  (second (first poly2))) poly1]
              [else (divider (subtract-poly poly1
                                            (apply-degree-quo
                                             (degree-quo (first poly2)
                                                         (first poly1))
                                             (apply-coeff-quo
                                              (coeff-quo (first poly2)
                                                         (first poly1))
                                              poly2))) poly2)]))]
    (divider-helper (filler poly1) (fill-end (filler poly2)))))
  