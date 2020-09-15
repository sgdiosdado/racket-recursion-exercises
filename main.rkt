#lang racket

; primo?:
;   Devuelve #t o #f dependiendo si el número es primo
;   Params:
;     n -> int
(define (primo? n [i 2])
  (cond
    [(<= n 3) (> n 1)]
    [(zero? (modulo n i)) #f]
    [(> n (* i i)) #t]
    [else (primo? n (+ i 1))]
      )
  )

; combinaciones:
;   Devuelve las combinaciones sin repetición y sin importar orden de r cosas en un conjuto n
;   utiliza una funciona auxiliar factorial
;   Params:
;     n -> int
;     r -> int
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))
      )
  )
(define (combinaciones n r)
  (/ (factorial n) (* (factorial r) (factorial (- n r))))
  )


; bitor:
;   Devuelve el 'or' lógico de bits entre dos listas
;   Params:
;     l -> list<int>
;     k -> list<int>
(define (bitor l k)
  (if (or (empty? l) (empty? l))
      '()
       (append
        (if (zero? (+ (car l) (car k)))
            (cons 0 '())
            (cons 1 '())
            )
        (bitor (cdr l) (cdr k))
        )
      )
  )

; decimal
;   Devuelve el número decimal a partir de una representación en binario
;   Params:
;     l -> list<int>

(define (decimal l)
  (if (empty? l)
      0
      (+ (* (expt 2 (- (length l) 1)) (car l)) (decimal (cdr l)))
      )
  )







