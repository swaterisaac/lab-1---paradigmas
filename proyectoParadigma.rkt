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

;Suelo: El suelo de la escena, en donde se paran los jugadores y enemigos.}

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
  (if (list? F)
      (if (and
               (intPositive? (get F 0))
               (intPositive? (get F 1))
               )
          #t
          #f)
      #f))

;createEarth (conjunto de floor)
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
                (cons (createFloor M 1) (cons (createFloor M (modulo 172 N)) (createEarthX (- M 1) N (myRandom seed) 0)))
                (cons (createFloor M 1) (cons (createFloor M (modulo 172 N)) (createEarthX (- M 1) N (- seed 1) 1)))
                )
            )
        )
    )
  (reverse (createEarthX M N seed 0))
  )
;earth?: parámetros(algo)
;desc: función de pertenencia del conjunto de floor.
;dom: algo
;rec: booleano

        

  

;Player

;createPlayer: parámetros (X Y angle)
;desc: Función constructora de player. OBS: Suma 1 de altura ya que ocupa 2 espacios.
;dom: entero X entero X num
;X: Coordenada en X (suelo)
;Y: Coordenada más baja en Y (altura)
;angle: Ángulo con el que mira el player (0 a 359)
;life: Vidas del player
;rec: player. ("P" X Y1 Y2 angle)
(define (createPlayer X Y angle life)
  (if (and (intPositive? X)
           (intPositive? Y)
           (number? angle)
           (intPositive? life)
           )
      (list "P" X Y (modulo angle 360) life)
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
               (intPositive? (get P 4))
               )
          #t
          #f)
      #f)
  )

;Selectoras de player
;getPlayerX: parámetros (player)
;desc: Función selectora de player. Nos entrega su coordenada en X.
;dom: player
;rec: entero, representando la coordenada en X donde se ubica (suelo)
(define (getPlayerX player)
  (if (player? player)
      (get player 1)
      -1
      )
  )

;getPlayerY: parámetros (player)
;desc: Función selectora de player. Nos entrega su coordenada en Y.
;dom: Player
;rec: entero, representando la coordenada en Y donde se ubica (altura)
(define (getPlayerY player)
  (if (player? player)
      (get player 2)
      -1
      )
  )

;getPlayerAngle: parámetros (player)
;desc: Función selectora de player. Nos da su angle.
;dom: player
;rec: numero, representando el angle que posee.
(define (getPlayerAngle player)
  (if (player? player)
      (get player 3)
      -1
      )
  )
;getPlayerLife: parámetros (player)
;desc: Función selectora de player. Nos da cuántas vidas tiene.
;dom: player
;rec: entero, representando las vidas que tiene.
(define (getPlayerLife player)
  (if (player? player)
      (get player 4)
      -1
      )
  )


;Modificadoras de player
;setPlayerX: parámetros (player X)
;desc: Función modificadora de player. Modifica su X.
;dom: player X entero
;rec: player
(define (setPlayerX player X)
  (if (and
      (intPositive? X)
      (player? player)
      )
      (createPlayer X (getPlayerY player) (getPlayerAngle player) (getPlayerLife player))
      (createPlayer 0 0 0 0)
      )
  )
;setPlayerY: parámetros (player Y)
;desc: Función modificadora de player. Modifica su Y (las funciones selectoras están más adelante)
;dom: player X entero
;rec: player
(define (setPlayerY player Y)
  (if (and
      (intPositive? Y)
      (player? player)
      )
      (createPlayer (getPlayerX player) Y (getPlayerAngle player) (getPlayerLife player))
      (createPlayer 0 0 0 0)
      )
  )
;setPlayerAngle: parámetros (player angle)
;desc: Función modificadora de player. Modifica su angle (las funciones selectoras están más adelante)
;dom: player X num
;rec: player
(define (setPlayerAngle player angle)
  (if (and
      (number? angle)
      (player? player)
      )
      (createPlayer (getPlayerX player) (getPlayerY player) angle (getPlayerLife player))
      (createPlayer 0 0 0 0)
      )
  )
;setPlayerLife: parámetros (player life)
;desc: Función modificadora de player. Modifica su life.
;dom: player X entero
;rec: player
(define (setPlayerLife player life)
  (if (and
       (intPositive? life)
       (player? player)
       )
       (createPlayer (getPlayerX player) (getPlayerY player) (getPlayerAngle player) life)
       (createPlayer 0 0 0 0)
       )
  )


;Enemy

;createEnemy: parámetros (X Y angle)
;desc: Función constructora de un enemy.
;dom: entero X entero X num
;X: coordenada en X (suelo)
;Y: coordenada base en Y (altura)
;angle: ángulo con el que mira el Enemy.
;life: vidas del enemigo.
;rec: Enemy
(define (createEnemy X Y angle life)
  (if (not (null? (createPlayer X Y angle)))
      (cons "E" (cdr (createPlayer X Y angle life))) 
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
;Selectoras de enemy
;getEnemyX: parámetros (enemy)
;desc: Función selectora de enemy. Nos entrega su coordenada en X.
;dom: enemy
;rec: entero, representando la coordenada en X donde se ubica (suelo)
(define (getEnemyX enemy)
  (if (enemy? enemy)
      (get enemy 1)
      -1
      )
  )
;getEnemyY: parámetros (enemy)
;desc: Función selectora de Enemy. Nos da su coordenada en Y
;dom: Enemy
;rec: entero, representando la coordenada en Y donde se ubica (altura)
(define (getEnemyY enemy)
  (if (enemy? enemy)
      (get enemy 2)
      -1
      )
  )
;getEnemyAngle: parámetros (enemy)
;desc: Función selectora de enemy. Nos entrega su angle.
;dom: enemy
;rec: numero, representando el angle que posee.
(define (getEnemyAngle enemy)
  (if (enemy? enemy)
      (get enemy 3)
      -1
      )
  )
;getEnemyLife: parámetros (enemy)
;desc: Función selectora de enemy. Nos entrega las vidas que tiene.
;dom: enemy
;rec: entero, representando las vidas que tiene
(define (getEnemyLife enemy)
  (if (enemy? enemy)
      (get enemy 4)
      -1
      )
  )

;Modificadoras de enemy
;setEnemyX: parámetros (enemy X)
;desc: Función modificadora de enemy. Modifica su coordenada en X.
;dom: enemy X entero
;rec: Enemy
(define (setEnemyX enemy X)
  (if (and
       (enemy? enemy)
       (intPositive? X)
       )
      (createEnemy X (getEnemyY enemy) (getEnemyAngle enemy) (getEnemyLife enemy))
      (createEnemy 0 0 0 0)
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
;Selectoras de bullet
;getBulletX: parámetros (bullet)
;desc: Función selectora de bullet. Nos entrega su coordenada en X.
;dom: bullet
;rec: entero, representando la coordenada en X donde se ubica (suelo)
(define (getBulletX bullet)
  (if (bullet? bullet)
      (get bullet 1)
      -1
      )
  )

;getBulletY: parámetros (bullet)
;desc: Función selectora de bullet. Nos da su coordenada en Y.
;dom: bullet
;rec: entero, representando la coordenada en Y donde se ubica (altura)
(define (getBulletY bullet)
  (if (bullet? bullet)
      (get bullet 2)
      -1
      )
  )

;getAngle: parámetros (bullet)
;desc: Función selectora de bullet. Nos da su angle.
;dom: bullet
;rec: numero, representando el angle que posee.
(define (getBulletAngle bullet)
  (if (bullet? bullet)
      (get bullet 3)
      -1
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
