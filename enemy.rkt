#lang racket
(require "player.rkt")
(require "funcionesGenerales.rkt")
(require "floorAndEarth.rkt")

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
  (if (not (null? (createPlayer X Y angle life)))
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
       (= (length E) 5) 
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
;setEnemyY: parámetros (enemy Y)
;desc: Función modificadora de enemy. Modifica su coordenada en Y.
;dom: enemy X entero
;rec: Enemy
(define (setEnemyY enemy Y)
  (if (and
       (enemy? enemy)
       (intPositive? Y)
       )
      (createEnemy (getEnemyX enemy) Y (getEnemyAngle enemy) (getEnemyLife enemy))
      (createEnemy 0 0 0 0)
      )
  )
;setEnemyAngle: parámetros (enemy angle)
;desc: Función modificadora de enemy. Modifica su angle.
;dom: enemy X numero
;rec: Enemy
(define (setEnemyAngle enemy angle)
  (if (and
       (enemy? enemy)
       (number? angle)
       )
      (createEnemy (getEnemyX enemy) (getEnemyY enemy) angle (getEnemyLife enemy))
      (createEnemy 0 0 0 0)
      )
  )
;setEnemyLife: parámetros (enemy life)
;desc: Función modificadora de enemy. Modifica su vida.
;dom: enemy X life
;rec: Enemy
(define (setEnemyLife enemy life)
  (if (and
       (enemy? enemy)
       (intPositive? life)
       )
      (createEnemy (getEnemyX enemy) (getEnemyY enemy) (getEnemyAngle enemy) life)
      (createEnemy 0 0 0 0)
      )
  )



;generateEnemy: parámetros (earth E seed)
;desc: Crea un conjunto de enemigos inicial.
;dom: earth X entero
;earth: Conjunto de floor
;E: Cantidad inicial de enemigos
;seed: semilla
;repeated: Lista que guarda las posiciones enemigas (para que no se repitan)
;rec: lista de enemigos (E (enemy1) (enemy2) ... )

(define (generateEnemy earth E seed)
  (define (generateEnemyX earth E seed repeated)
    (if (and
         (earth? earth)
         (intPositive? E)
         (intPositive? seed)
         )
        (if (= E 0)
            null
            (if (and
                 (not (= (getEarthX earth (modulo seed (length earth))) 1))
                 (not (= (getEarthY earth (modulo seed (length earth))) 2))
                 (not (= (getEarthX earth (modulo seed (length earth))) 2));Para que no comiencen en la misma posicion de player
                 (not (= (getEarthY earth (modulo seed (length earth))) 2))
                 (not (= (getEarthX earth (modulo seed (length earth))) 3))
                 (not (= (getEarthY earth (modulo seed (length earth))) 2))
                 (not (find repeated (modulo seed (length earth))));Para que no se repita
                 )
                (cons (createEnemy
                       (getEarthX earth (modulo seed (length earth)))
                       (+ (getEarthY earth (modulo seed (length earth))) 1)
                       0
                       1
                       )
                      (generateEnemyX earth (- E 1) (myRandom seed) (myAppend repeated (modulo seed (length earth)))))
                (generateEnemyX earth E (myRandom seed) repeated)
                )
            
            )
        null
        )
    )
  (generateEnemyX earth E seed null)
  )
 
;enemies?: parámetros (algo)
;desc: Crea un conjunto de enemigos inicial
;dom: algo
;Encapsulación:
;ite: iterador de la recursión
;rec: booleano
(define (enemies? E)
  (define (enemies?X E ite)
    (if (list? E)
        (cond
          [(null? E) #t]
          [(= ite -1) (enemies?X E (- (length E) 1))]
          [(= ite 0) (enemy? (get E ite))]
          [else (and (enemy? (get E ite)) (enemies?X E (- ite 1)))]
          )
        #f
        )
    )
  (enemies?X E -1)
  )
;getEnemies: parámetros (conjuntoEnemy N)
;desc: Función selectora del conjunto de enemy. Nos permite obtener el enemy N°
;dom: conjuntoEnemy X entero
;N: Número de enemy que queremos conseguir
;rec: enemy seleccionado
(define (getEnemies E N)
  (if (and
       (enemies? E)
       (intPositive? N)
       )
      (get E N)
      null
      )
  )

;setEnemiesX : parámetros (conjuntoEnemy N X)
;desc: Función modificadora del conjunto de enemy. Modifica el X de un enemy especifico.
;dom: conjuntoEnemy X entero X entero
;N: Número de enemy que queremos modificar
;X: Valor a modificar
;rec: conjuntoEnemy
(define (setEnemiesX E N X)
  (define (setEnemiesXX E N X ite)
    (if (and
         (enemies? E)
         (intPositive? N)
         (intPositive? X)
         )
        (cond
          [(= ite N) (cons (setEnemyX (get E ite) X) (setEnemiesXX E N X (+ ite 1)))]
          [(= ite (length E)) null]
          [else (cons (get E ite) (setEnemiesXX E N X (+ ite 1)))]
       )
        null
        )
    )
  (setEnemiesXX E N X 0)
  )
;setEnemiesY : parámetros (conjuntoEnemy N Y)
;desc: Función modificadora del conjunto de enemy. Modifica el Y de un enemy especifico.
;dom: conjuntoEnemy X entero X entero
;N: Número de enemy que queremos modificar
;Y: Valor a modificar
;rec: conjuntoEnemy

(define (setEnemiesY E N Y)
  (define (setEnemiesYX E N Y ite)
    (if (and
         (enemies? E)
         (intPositive? N)
         (intPositive? Y)
         )
        (cond
          [(= ite N) (cons (setEnemyY (get E ite) Y) (setEnemiesYX E N Y (+ ite 1)))]
          [(= ite (length E)) null]
          [else (cons (get E ite) (setEnemiesYX E N Y (+ ite 1)))]
       )
        null
        )
    )
  (setEnemiesYX E N Y 0)
  )
;setEnemiesAngle : parámetros (conjuntoEnemy N angle)
;desc: Función modificadora del conjunto de enemy. Modifica el ángulo de un enemigo especifico.
;dom: conjuntoEnemy X entero X entero
;N: Número de enemy que queremos modificar
;angle: Valor a modificar
;rec: conjuntoEnemy
(define (setEnemiesAngle E N angle)
  (define (setEnemiesAngleX E N angle ite)
    (if (and
         (enemies? E)
         (intPositive? N)
         (number? angle)
         )
        (cond
          [(= ite N) (cons (setEnemyAngle (get E ite) angle) (setEnemiesAngleX E N angle (+ ite 1)))]
          [(= ite (length E)) null]
          [else (cons (get E ite) (setEnemiesAngleX E N angle (+ ite 1)))]
       )
        null
        )
    )
  (setEnemiesAngleX E N angle 0)
  )
;setEnemiesLife : parámetros (conjuntoEnemy N life)
;desc: Función modificadora del conjunto de enemy. Modifica la vida de un enemigo especifico.
;dom: conjuntoEnemy X entero X entero
;N: Número de enemy que queremos modificar
;life: Valor a modificar
;rec: conjuntoEnemy
(define (setEnemiesLife E N life)
  (define (setEnemiesLifeX E N life ite)
    (if (and
         (enemies? E)
         (intPositive? N)
         (intPositive? life)
         )
        (cond
          [(= ite N) (cons (setEnemyLife (get E ite) life) (setEnemiesLifeX E N life(+ ite 1)))]
          [(= ite (length E)) null]
          [else (cons (get E ite) (setEnemiesLifeX E N life (+ ite 1)))]
       )
        null
        )
    )
  (setEnemiesLifeX E N life 0)
  )

;otras funciones enemies
;deleteEnemy: parámetro (conjuntoEnemy N)
;desc: Borra un enemy indicado del conjunto de enemies.
;dom: conjuntoEnemy X entero
;N: Número del enemy que queremos borrar dentro del conjunto
;Encapsulación:
;ite: Iterador de la recursión
;rec: conjuntoEnemy
(define (deleteEnemy E N)
  (define (deleteEnemyX E N ite)
    (if (and
         (enemies? E)
         (< N 3)
         (>= N 0)
         )
        (cond
          [(= ite N) (deleteEnemyX E N (+ ite 1))]
          [(= ite (length E)) null]
          [else (cons (getEnemies E ite) (deleteEnemyX E N (+ ite 1)))]
          )
        null
        )
    )
  (deleteEnemyX E N 0)
  )

;getEnemyXY: parámetro (enemy)
;desc: Nos da una lista de las coordenadas X e Y de enemy. Nos sirve para comparar en la función play.
;dom: enemy
;rec: lista de coordenada X e Y
(define (getEnemyXY E)
  (if (enemy? E)
      (list (getEnemyX E) (getEnemyY E))
      null
      )
  )

;listEnemyXY: parámetro (enemy)
;desc: Nos da una lista de listas de coordenadas XY de un conjuntoEnemy.
;dom:conjuntoEnemy
;rec: Lista de listas
(define (listEnemyXY enemies)
  (define (listEnemyXYX enemies ite)
    (if (not (null? enemies))
        (cond
      [(= ite -1) (listEnemyXYX enemies (- (length enemies) 1))]
      [(= ite 0) (cons (getEnemyXY (getEnemies enemies 0)) null)]
      [else (cons (getEnemyXY (getEnemies enemies ite)) (listEnemyXYX enemies (- ite 1)))]
      )
    null
    )
    )
  (listEnemyXYX enemies -1)
    )

;Para llamar en otros archivos
(provide (all-defined-out))