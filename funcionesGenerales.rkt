#lang racket
;funciones generales

;intPositive?: Parámetros (algo)
;desc: Comprueba si un número es entero positivo (aunque incluye el 0)
;dom: Cualquier tipo de dato
;rec: Booleano
(define (intPositive? x)
  (if (integer? x)
      (if (>= x 0)
          #t
          #f)
      #f)
  )
;get: Parámetros (lista num)
;desc: Nos da el elemento <num> de la <lista>
;dom: lista X entero
;rec: algo (el elemento que saquemos de la lista)
(define (get lista num)
  (if (list? lista)
      (if (null? lista)
          lista
          (if (= num 0)
              (car lista)
              (get (cdr lista) (- num 1))
              )
          )
      null)
  )
;myAppend: parámetros (lista algo)
;desc: Agrega algo al final de la lista.
;dom: lista X algo
;rec: lista
(define (myAppend lista algo)
  (if (null? lista)
      (cons algo null)
      (cons (car lista) (myAppend (cdr lista) algo))
      )
  )

;find: parámetros (lista X)
;desc: Ve si el elemento x se encuentra dentro de la lista.
;dom: lista x X
;rec: booleano #t si encuentra el elemento en la lista, #f si no.
;tipo recursión: natural
;Encapsulación:
;ite: Iterador para la lista. Parte de length lista - 1.
(define (find lista X)
  (define (findX lista X ite)
    (if (null? lista)
        #f
        (if (= ite -1)
        (findX lista X (- (length lista) 1))
        (if (= ite 0)
            (equal? (get lista ite) X)
            (if (equal? (get lista ite) X)
                #t
                (findX lista X (- ite 1))
                )
            )
        )
    )

    )
  (findX lista X -1)
  )

;myModulo: parámetros (X Y)
;desc: Saca el modulo normal de dos elementos, pero si el resultado es 0,1 o 2, retorna 3.
;Se hace esta función para el createEarth.
;dom: entero X entero
;rec: entero entre el 2 y el número Y-1
(define (myModulo X Y)
  (if (<= (modulo X Y) 2)
      3
      (modulo X Y)
      )
  )



;myRandom: parámetros (seed)
;desc: Es una función para ir modificando seed, y en base a eso, armar la scene.
;dom: entero
;rec: entero
;Estas constantes fueron sacadas de https://en.wikipedia.org/wiki/Linear_congruential_generator
(define a 1103515245)
(define c 12345)
(define m 2147483648)
;Esta función random tuma un xn y obtiene el xn+1 de la secuencia de números aleatorios.
(define myRandom
  (lambda
    (xn)
    (remainder (+ (* a xn) c) m)
  )
  )

;findXY: parámetros (listaXY XY)
;desc: Recibe una lista de XY y un XY, para devolvernos el número donde se encuentra posicionado-
;dom: listaXY X XY
;XY: (x y)
;rec: entero , representando el valor donde se encontraba ese XY.
(define (findXY listaXY XY)
  (define (findXYX listaXY XY ite)
    (if (not (null? listaXY))
        (cond
          [(equal? (get listaXY ite) XY) ite]
          [(= ite (length listaXY) -1)]
          [else (findXYX listaXY XY (+ ite 1))] 
          )
        -1)
    )
  (findXYX listaXY XY 0)
  )

;Para llamar en otros archivos
(provide (all-defined-out))