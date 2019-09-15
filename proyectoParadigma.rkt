#lang racket
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

;Suelo: El suelo de la escena, en donde se paran los jugadores y enemigos.}

;createFloor: parámetros (X Y)
;desc: Función constructora de floor
;dom:entero X entero
;X: Coordenada en X (nivel en el suelo)
;Y: Coordenada en Y (altura)
;rec: Floor. ('F' X Y)
(define (createFloor X Y)
  (if (and (intPositive? X)
           (intPositive? Y))
      (list "F" X Y)
      null)
  )
;floor?: parámetros (algo)
;desc: Función de pertenencia de floor
;dom: cualquier tipo de dato
;rec: Booleano.
(define (floor? F)
  (if (list? F)
      (if (and
               (equal? (car F) "F")
               (intPositive? (car (cdr F)))
               (intPositive? (car (cdr (cdr F))))
               )
          #t
          #f)
      #f))
;getX: parámetros (floor)
;desc: Recibe un floor y entrega la coordenada X.
;dom: floor
;rec: Coordenada X del floor.

;getY: parámetros (floor)
;desc: Recibe un floor y entrega la coordenada Y.
;dom: floor
;rec: Coordenada Y del floor.


;Player

;createPlayer: parámetros (X Y angle)
;desc: Función constructora de player. OBS: Suma 1 de altura ya que ocupa 2 espacios.
;dom: entero X entero X num
;X: Coordenada en X (suelo)
;Y: Coordenada más baja en Y (altura)
;angle: Ángulo con el que mira el player (0 a 359)
;rec: player. ("P" X Y1 Y2 angle)
(define (createPlayer X Y angle)
  (if (and (intPositive? X)
           (intPositive? Y)
           (number? angle))
      (list "P" X Y (+ Y 1) (modulo angle 360))
      null)
  )
;player?: parámetros (player)
;desc: Función de pertenencia de player.
;dom: Cualquier dato
;rec: booleano
(define (player? P)
  (if (list? P)
      (if (and (equal? (car P) "P")
               (intPositive? (car (cdr P)))
               (intPositive? (car (cdr (cdr P))))
               (intPositive? (car (cdr (cdr (cdr P)))))
               (intPositive? (car (cdr (cdr (cdr (cdr P))))))
               )
          #t
          #f)
      #f)
  )
;getPlayerX: parámetros (player)
;desc: Función selectora de player que saca la coordenada X.
;dom: player
;rec: coordenada X de player (numero entero positivo incluyendo 0)

;getPlayerY: parámetros (player)
;desc: Función selectora de player que saca la coordenada Y.
;dom: player
;rec: coordenada Y de player número entero positivo incluyendo 0)

;Enemy

;createEnemy: parámetros (X Y angle)
;desc: Función constructora de un enemy.
;dom: entero X entero X num
;X: coordenada en X (suelo)
;Y: coordenada base en Y (altura)
;angle: ángulo con el que mira el Enemy.
;rec: Enemy
(define (createEnemy X Y angle)
  (if (not (null? (createPlayer X Y angle)))
      (cons "E" (cdr (createPlayer X Y angle))) 
      null)
  )

;enemy?: parámetros (algo)
;desc: Función de pertenencia de enemy.
;dom: algo
;rec: bool
(define (enemy? E)
  (if (and
       (list? E)
       (equal? (car E) "E")
       (player? (cons "P" (cdr E)))
       )
       #t
       #f
      )
  )
  
;createScene:
;Dom: cuatro números enteros positivos y un número entero.
;N y M indican el tamaño del escenario.
;E la cantidad de enemigos.
;D indica la la dificultad.
;seed indica la semilla de la escena.
