#lang racket
(require "funcionesGenerales.rkt")
(require "enemy.rkt")

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
  (if (and
      (list? B)
      (not (null? B))
      (equal? "B" (car B))
      (intPositive? (get B 1))
      (intPositive? (get B 2))
      (number? (get B 3))
      )
      #t
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

;getBulletAngle: parámetros (bullet)
;desc: Función selectora de bullet. Nos da su angle.
;dom: bullet
;rec: numero, representando el angle que posee.
(define (getBulletAngle bullet)
  (if (bullet? bullet)
      (get bullet 3)
      -1
      )
  )

;Modificadoras de bullet
;setBulletX: parámetros (bullet X)
;desc: Función modificadora de bullet. Modifica su coordenada en X
;dom: bullet X entero
;rec: bullet
(define (setBulletX bullet X)
  (if (and
       (bullet? bullet)
       (intPositive? X)
       )
      (createBullet X (getBulletY bullet) (getBulletAngle bullet))
      (createBullet 0 0 0)
      )
  )
;setBulletY: parámetros (bullet Y)
;desc: Función modificadora de bullet. Modifica su coordenada en Y.
;dom: bullet X entero
;rec: bullet
(define (setBulletY bullet Y)
  (if (and
       (bullet? bullet)
       (intPositive? Y)
       )
      (createBullet (getBulletX bullet) Y (getBulletAngle bullet))
      (createBullet 0 0 0)
      )
  )
;setBulletAngle: parámetros (bullet angle)
;desc: Función modificadora de bullet. Modifica su angle.
;dom: bullet x num
;rec: bullet
(define (setBulletAngle bullet angle)
  (if (and
       (bullet? bullet)
       (number? angle)
       )
      (createBullet (getBulletX bullet) (getBulletY bullet) angle)
      (createBullet 0 0 0)
      )
  )

;Otras funciones:
;paraMove: parámetros (bullet)
;desc: Función que mueve a bullet en base a su ángulo. Imita dentro
;de lo posible a un movimiento parabólico
;dom: bullet
;rec: bullet
(define (paraMove B)
  (if (bullet? B)
      (let ([ang (getBulletAngle B)])
        (cond
          [(and (<= ang 90)
               (> ang 80))  (setBulletY (setBulletAngle B (- ang 2)) (+ (getBulletY B) 1))]
          [(and (<= ang 80)
                (> ang 40)) (setBulletY (setBulletX (setBulletAngle B (- ang 5)) (+ (getBulletX B) 1)) (+ (getBulletY B) 1))]
          [(and (<= ang 40)
                (>= ang 0)) (setBulletX (setBulletAngle B (- ang 10)) (+ (getBulletX B) 1))]
          [(and (<= ang 359)
                (> ang 310)) (setBulletX (setBulletY (setBulletAngle B (- ang 10)) (- (getBulletY B) 1)) (+ (getBulletX B) 1))]
          [(and (<= ang 310)
                (> ang 270)) (setBulletY (setBulletAngle B (- ang 5)) (- (getBulletY B) 1))]
          [(and (<= ang 270)
                (> ang 260)) (setBulletY (setBulletAngle B (- ang 1)) (- (getBulletY B) 1))]
          [(and (<= ang 260)
               (> ang 210)) (setBulletX (setBulletY (setBulletAngle B (+ ang 5)) (- (getBulletY B) 1)) (- (getBulletX B) 1))]
          [(and (<= ang 210)
                (> ang 150)) (setBulletX (setBulletAngle B (+ ang 10)) (- (getBulletX B) 1))]
          [(and (<= ang 150)
                (> ang 100)) (setBulletX (setBulletY (setBulletAngle B (+ ang 10)) (+ (getBulletY B) 1)) (- (getBulletX B) 1))]
          [(and (<= ang 100)
               (> ang 90)) (setBulletY (setBulletAngle B (+ ang 10)) (+ (getBulletY B) 1))]
          )
        )
      B
      )
  )
                        

;getBulletXY: parámetros (bullet)
;desc: Nos entrega una lista con las coordenadas X e Y de bullet. Sirve para la función play.
;dom: bullet
;rec: Lista de coordenadas X e Y de este bullet.
(define (getBulletXY B)
  (if (bullet? B)
      (list (getBulletX B) (getBulletY B))
      null
      )
  )
      
;createBulletBy: parámetros (enemies N angle)
;desc: Recibe un conjunto de enemy, para que en base a uno
;de los de ese conjunto, se cree una bala (en su misma posición), con
;un ángulo dado.
;dom: conjuntoEnemy X entero X number
;N: Número de enemy del que queremos que salga la bala
;angle: ángulo con el que queremos que salga la bala.
(define (createBulletBy enemies N angle)
  (if (and
       (enemies? enemies)
       (intPositive? N)
       (number? angle)
       )
      (createBullet
       (getEnemyX (getEnemies enemies N))
       (getEnemyY (getEnemies enemies N))
       angle)
      null
      )
  )

;Para llamar en otros archivos
(provide (all-defined-out))