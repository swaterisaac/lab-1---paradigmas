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
               (intPositive? (get F 1))
               (intPositive? (get F 2))
               )
          #t
          #f)
      #f))

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
      (list "P" X Y (modulo angle 360))
      null)
  )
;player?: parámetros (player)
;desc: Función de pertenencia de player.
;dom: Cualquier dato
;rec: booleano
(define (player? P)
  (if (list? P)
      (if (and (equal? (car P) "P")
               (intPositive? (get P 1))
               (intPositive? (get P 2))
               (number? (get P 3))
               )
          #t
          #f)
      #f)
  )
;getPlayerX: parámetros (player)


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


;Bullet

;createBullet: parámetros (X Y angle)
;desc: Función constructora de bullet (representación de proyectil)
;dom: entero X entero X número
;rec: Bullet.

(define (createBullet X Y angle)
  (if (and
         (intPositive? X)
         (intPositive? Y)
         (number? angle)
      )
      (list "B" X Y (modulo angle 360))
      null
      )
  )
;bullet?: parámetros (algo)
;desc: Función de pertenencia de bullet.
;dom: algo
;rec: booleano
(define (bullet? B)
  (if (list? B)
      (if (and
           (equal? "B" (car B))
           (intPositive? (get B 1))
           (intPositive? (get B 2))
           (number? (get B 3))
          )
          #t
          #f
          )
      #f
      )
  )
;isScene?: parámetros (algo)
;desc: Función de pertenencia de elementos de la escena, tales como:
;floor,player,enemy, bullet y scene.
;dom: algo
;rec: booleano
;;;;EN EL CASO DE AGREGAR UN NUEVO ELEMENTO A ESCENA, SE AGREGA A ESTA FUNCIÓN TAMBIÉN.;;;;

(define (isScene? X)
  (if (or
       (floor? X)
       (player? X)
       (enemy? X)
       (bullet? X)
       )
      #t
      #f
      )
  )

;Conseguir coordenadas

;getX: parámetros (elemento), donde elemento es un dato relacionado con la escena
;(floor, player, enemy, bullet)
;dom: elemento de la escena
;rec: La coordenada en X donde se ubica (suelo)
(define (getX X)
  (if (isScene? X)
      (get X 1)
      null
      )
  )

;getY: parámetros (elemento), donde elemento es un dato relacionado con la escena
;(floor, player, enemy, bullet)
;dom: elemento de la escena
;rec: La coordenada en Y donde se ubica (altura)
(define (getY X)
  (if (isScene? X)
      (get X 2)
      null
      )
  )

;getAngle: parámetros (elemento), donde elemento es un dato relacionado con la escena
;(player,enemy o bullet) (se descarta floor)
;desc: Función que nos da el angle de cualquier cosa parte de la escena menos floor.
;dom: elemento de la escena (menos floor)
;rec: El ángulo del elemento en cuestión.
(define (getAngle X)
  (if (and
       (isScene? X)
       (not (floor? X))
       )
      (get X 3)
      null
      )
  )
      
;createScene:
;Dom: entero X entero X entero X entero X num
;N y M indican el tamaño del escenario.
;E la cantidad de enemigos.
;D indica la la dificultad.
;seed indica la semilla de la escena.
;Rec: scene.
;tipo de recursión: Natural.
(define (createScene N M E D seed)
  (define (createSceneX N M E D seed scene stop)
    (if (and
         (intPositive? N)
         (intPositive? M)
         (intPositive? E)
         (and (intPositive? D) (< D 4))
         (number? seed)
         )
        (if (= M 0)
            scene
            (if (= stop 0)
                (if (= (modulo seed 3) 0)
                     (createSceneX N (- M 1) E D seed scene 0)
                     (createSceneX N (- M 1) E D seed (myAppend scene (createFloor M 1)) 1)
                 )
                    (createSceneX N (- M 1) E D seed (myAppend scene (createFloor M 1)) 1)
                )
            )
        null
    )
    )
    (createSceneX N M E D seed (list N M E D) 0)
  )
