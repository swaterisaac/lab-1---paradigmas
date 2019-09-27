#lang racket
(require "funcionesGenerales.rkt")
;Suelo: El suelo de la escena, en donde se paran los jugadores y enemigos.

;createFloor: parámetros (X Y)
;desc: Función constructora de floor
;dom:entero X entero
;X: Coordenada en X (nivel en el suelo)
;Y: Coordenada en Y (altura)
;rec: Floor. (X Y)
(define (createFloor X Y)
  (if (and (intPositive? X)
           (intPositive? Y))
      (list X Y)
      null)
  )
;floor?: parámetros (algo)
;desc: Función de pertenencia de floor
;dom: cualquier tipo de dato
;rec: Booleano.
(define (floor? F)
  (if (and
       (list? F)
       (intPositive? (get F 0))
       (intPositive? (get F 1))
       )
      #t
      #f
      )
  )

;createEarth: (conjunto de floor)
;parámetros: (M seed)
;desc: Función de construcción de un conjunto de floor.
;dom: entero X entero (positivos)
;M: Cantidad de floor
;seed: semilla
;rec: conjunto de floor ("F" M (X1 Y1) (X2 Y2) ...)
;Encapsulación: 
;land: Si genera una isla flotante o no. 0 para no, 1 para si.
(define (createEarth M N seed)
  (define (createEarthX M N seed land)
    (if (= M 0)
        null
        (if (= land 0)
            (if (= (modulo seed 8) 0)
                (cons (createFloor M 1) (createEarthX (- M 1) N (modulo seed 5) 1))
                (cons (createFloor M 1) (createEarthX (- M 1) N (myRandom seed) 0))
            )
            ;Si land = 1
            (if (= seed 0)
                (cons (createFloor M 1) (cons (createFloor M (myModulo 172 N)) (createEarthX (- M 1) N (myRandom seed) 0)))
                (cons (createFloor M 1) (cons (createFloor M (myModulo 172 N)) (createEarthX (- M 1) N (- seed 1) 1)))
                )
            )
        )
    )
  (reverse (createEarthX M N seed 0))
  )

;earth?: parámetros (algo)
;desc: Función de pertenencia de earth
;dom: algo
;rec: booleano
(define (earth? algo)
  (define (earth?X algo vef)
    (if (= vef 0)
        (floor? (get algo vef))
        (if (and
             (= vef -1)
             (> (length algo) 0)
             )
            (earth?X algo (- (length algo) 1))
            (if (floor? (get algo vef))
            (earth?X algo (- vef 1))
            #f
            )
            )
        )
    )
  (earth?X algo -1)
  )

;getEarthX: parámetros (earth Nfloor)
;desc: Función selectora de earth. Nos entrega la coordenada X del floor número algo.
;dom: earth X entero
;earth: conjunto de floor
;Nfloor: Número de floor dentro de earth en donde queremos sacar su coordenada en X.
;rec: entero, que representa la coordenada en X
(define (getEarthX earth Nfloor)
  (if (and
       (intPositive? Nfloor)
       (<= Nfloor (length earth))
       (earth? earth)
       )
      (get (get earth Nfloor) 0)
      -1)
  )

;getEarthY: parámetros (earth Nfloor)
;desc: Función selectora de earth. Nos entrega la coordenada X del floor número algo.
;dom: earth X entero
;earth: conjunto de floor
;Nfloor: Número de floor dentro de earth en donde queremos sacar su coordenada en Y.
;rec: entero, que representa la coordenada en Y.
(define (getEarthY earth Nfloor)
  (if (and
       (intPositive? Nfloor)
       (<= Nfloor (length earth))
       (earth? earth)
       )
      (get (get earth Nfloor) 1)
      -1
      )
  )

;isInEarth?: parámetros (earth list)
;desc: Dado un conjunto de floor y una lista de posiciones (tienen el mismo formato),
;ve si la lista de posiciones está encima de alguna posición de earth.
;dom: conjuntoFloor X lista
;rec: booleano
(define (isInEarth? earth lista)
  (define(isInEarth?X earth lista ite)
    (if (= ite (- (length lista) 1))
        (find earth (list (get (get lista ite) 0) (- (get (get lista ite) 1) 1)))
        (and (find earth (list (get (get lista ite) 0) (- (get (get lista ite) 1) 1))) (isInEarth?X earth lista (+ ite 1)))
        )
    )
  (isInEarth?X earth lista 0)
  )

;Para llamar en otros archivos
(provide (all-defined-out))