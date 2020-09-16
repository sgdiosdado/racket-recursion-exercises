#lang racket
(require sdraw)
; A00516971 - Sergio Diosdado
; A00516978 - Iñaki Janeiro

; 1. Función primo?:
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

; 2. Función sumdpar:
;    Regresa la suma de los dígitos pares de un número entero no negativo
;    Parámetro n: Numéro entero no negativo sobre el cuál se hará el cálculo
(define (sumdpar n)
  (if (= n 0)
      0
      (cond
        [(even? ( modulo n 10)) (+(modulo n 10) (sumdpar (quotient n 10)))]
        [else (sumdpar(quotient n 10))]
        )
      )
  )

; 3. combinaciones:
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

; 4. Para cada una de las siguientes representaciones de listas encontrar las dos que faltan
;    Ver archivo adjunto: A00516971_A00516978_tarea3_ejercicio4.pdf


; 5. bitor:
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

; 6. hexadecimal
;      Devuelve una lista con la representacion hexadecimal del número binario dado
;      Parámetro n: Número binario a transformar a hexadecimal

(define (hexadecimal-aux n multiplier sum)
  (cond [(= n 0)(cons  sum '())]
        [(<= multiplier 8)(hexadecimal-aux (quotient n 10) (* 2 multiplier)(+ sum (* (modulo n 10) multiplier)))]
        [else (cons sum (hexadecimal-aux n 1 0))]
     
      ;(cons n (cons (hexadecimal-aux(modulo n 10) 1)))
      
  )
  )
(define (hexadecimal n)
  (reverse (map (lambda (number)
         (cond [(= number 10) 'a]
               [(= number 11) 'b]
               [(= number 12) 'c]
               [(= number 13) 'd]
               [(= number 14) 'e]
               [(= number 15) 'f]
               [else number]
               ))(hexadecimal-aux n 1 0)))
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


; expresion
;    devuelve #t si es una expresion valida (operador numero1 numero2 ...) donde la lista puede ser imbricada
;    Parámetro exp: La lista posiblemente imbricada

(define (expresion-aux-2 exp) ;estado 2 - llega el resto de la lista
  
  (cond
    [(null? exp) #t]
    [(number? (car exp)) (expresion-aux-2 (cdr exp))]
    [(list? (car exp)) (and (expresion-aux-1 (car exp)) (expresion-aux-2 (cdr exp)))]
    [else #f]))

(define (expresion-aux-1 exp) ;estado 1- llega un operador
  (cond [(equal? (car exp) '+) (expresion-aux-2 (cdr exp))]
        [(equal? (car exp) '*) (expresion-aux-2 (cdr exp))]
        [(equal? (car exp) '/) (expresion-aux-2 (cdr exp))]
        [(equal? (car exp) '-) (expresion-aux-2 (cdr exp))]
        [else #f]))
  
(define (expresion? exp)
  (expresion-aux-1 exp)
 )


; inversiontotal
;    Invierte totalmente una lista, y todas sus listas anidadas
;    Parámetro lista:  La lista posiblemente imbricada a revertir

(define (inversiontotal lista)
  (cond [(list? lista) (reverse (map inversiontotal lista))]
        [else lista]))



