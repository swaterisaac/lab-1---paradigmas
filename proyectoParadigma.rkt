#lang racket
;Suelo: El suelo de la escena, en donde se paran los jugadores y enemigos.}

;createFloor: parámetros (X Y)
;desc: Función constructora de floor
;dom:entero X entero
;X: Coordenada en X (nivel en el suelo)
;Y: Coordenada en Y (altura)
;rec: Floor. ('F' X Y)
(define (createFloor X Y)
  (list "F" X Y))
;floor?: parámetros (algo)
;desc: Función de pertenencia de floor
;dom: cualquier tipo de dato
;rec: Booleano.
(define (floor? F)
  (if (list? F)
      (if (and
           (and
               (equal? (car F) "F")
               (and (number? (car (cdr F))) (>= (car (cdr F)) 0)))
               (and (number? (car (cdr F))) (>= (car (cdr F)) 0)))
          #t
          #f)
      #f))
                
;createScene:
;Dom: cuatro números enteros positivos y un número entero.
;N y M indican el tamaño del escenario.
;E la cantidad de enemigos.
;D indica la la dificultad.
;seed indica la semilla de la escena.
