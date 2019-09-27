#lang racket
(require "funcionesGenerales.rkt")

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
  (if (and
       (list? P)
       (= (length P) 5)
       (equal? (car P) "P")
       (intPositive? (get P 1))
       (intPositive? (get P 2))
       (number? (get P 3))
       (intPositive? (get P 4))
       )
      #t
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




;generatePlayer: parámetros (life)
;desc: Función que genera un conjunto de player.
;dom: entero
;life: La cantidad de vida que tiene cada uno de los player
;rec: Conjunto de player.
;nota: Siempre serán 3, parten en las primeras 3 coordenadas.
(define (generatePlayer life)
  (if (intPositive? life)
      (list (createPlayer 1 2 -90 life)
            (createPlayer 2 2 -90 life)
            (createPlayer 3 2 -90 life)
            )
      null
      )
  )
;players?: parámetros (conjuntoPlayer)
;desc: Función de pertenencia del conjunto de player.
;dom: conjunto de player
;rec: booleano
(define (players? P)
  (define (players?X P ite)
    (if (= ite (- (length P) 1))
        (player? (get P ite))
        (and (player? (get P ite)) (players?X P (+ ite 1)))
        )
    )
  (if (null? P)
      #t
      (players?X P 0)
      )
  )
  
  #|(if (and
       (list? P)
       (<= (length P) 3)
       (player? (get P 0))
       (player? (get P 1))
       (player? (get P 2))
       )
      #t
      #f
      )
  )|#
;getPlayers: parámetros (conjuntoPlayer N)
;desc: Funcion selectora del conjunto de player. Nos permite obtener el player N° algo.
;dom: conjuntoPlayer X entero
;N: Número de player que queremos conseguir (0 1 o 2)
;rec: player
(define (getPlayers P N)
  (if (and
       (players? P)
       (< N 3)
       (>= N 0)
       )
      (get P N)
      null
      )
  )
;Modificadora de players:
;setPlayersX: parámetros (conjuntoPlayer N X)
;desc: Función modificadora del conjunto de player. Modifica la coordenada en X de uno de ellos.
;dom: conjuntoPlayer X entero X entero
;N: número de player que queremos modificar
;X: Nueva coordenada en X
;rec: conjuntoPlayer
(define (setPlayersX P N X)
  (define (setPlayersXX P N X ite)
    (if (and
       (players? P)
       (< N 3)
       (>= N 0)
       (intPositive? X)
       )
        (cond
        [(= ite N) (cons (setPlayerX (getPlayers P ite) X)
                       (setPlayersXX P N X (+ ite 1))
                       )]
        [(= ite (length P)) null
                       ]
        [else (cons (getPlayers P ite) (setPlayersXX P N X (+ ite 1)))]
      )
        null
        )
    )
  (setPlayersXX P N X 0)
  )

;setPlayersY: parámetros (conjuntoPlayer N Y)
;desc: Función modificadora del conjunto de player. Modifica la coordenada en Y de uno de ellos.
;dom: conjuntoPlayer X entero X entero
;N: número de player que queremos modificar
;X: Nueva coordenada en Y
;rec: conjuntoPlayer
(define (setPlayersY P N Y)
  (define (setPlayersYX P N Y ite)
    (if (and
       (players? P)
       (< N 3)
       (>= N 0)
       (intPositive? Y)
       )
        (cond
        [(= ite N) (cons (setPlayerY (getPlayers P ite) Y)
                       (setPlayersYX P N Y (+ ite 1))
                       )]
        [(= ite (length P)) null
                       ]
        [else (cons (getPlayers P ite) (setPlayersYX P N Y (+ ite 1)))]
      )
        null
        )
    )
  (setPlayersYX P N Y 0)
  )

;setPlayersAngle: parámetros (conjuntoPlayer N angle)
;desc: Función modificadora del conjunto de player. Modifica el ángulo de un player especifico.
;dom: conjuntoPlayer X entero X entero
;N: número de player que queremos modificar
;X: Nuevo ángulo
;rec: conjuntoPlayer
(define (setPlayersAngle P N angle)
  (define (setPlayersAngleX P N angle ite)
    (if (and
       (players? P)
       (< N 3)
       (>= N 0)
       (number? angle)
       )
        (cond
        [(= ite N) (cons (setPlayerAngle (getPlayers P ite) angle)
                       (setPlayersAngleX P N angle (+ ite 1))
                       )]
        [(= ite (length P)) null
                       ]
        [else (cons (getPlayers P ite) (setPlayersAngleX P N angle (+ ite 1)))]
      )
        null
        )
    )
  (setPlayersAngleX P N angle 0)
  )

;setPlayersLife: parámetros (conjuntoPlayer N life)
;desc: Función modificadora del conjunto de player. Modifica la vida de un player específico.
;dom: conjuntoPlayer X entero X entero
;N: número de player que queremos modificar
;life: Nueva vida
;rec: conjuntoPlayer
(define (setPlayersLife P N life)
  (define (setPlayersLifeX P N life ite)
    (if (and
       (players? P)
       (< N 3)
       (>= N 0)
       (intPositive? life)
       )
        (cond
        [(= ite N) (cons (setPlayerLife (getPlayers P ite) life)
                       (setPlayersLifeX P N life (+ ite 1))
                       )]
        [(= ite (length P)) null
                       ]
        [else (cons (getPlayers P ite) (setPlayersLifeX P N life (+ ite 1)))]
      )
        null
        )
    )
  (setPlayersLifeX P N life 0)
  )

;otras funciones players
;deletePlayer: parámetro (conjuntoPlayer N)
;desc: Borra un player indicado del conjunto de players.
;dom: conjuntoPlayers X Entero
;N: Número del player que queremos borrar dentro del conjunto
;Encapsulación:
;ite: Iterador de la recursión
;rec: conjuntoPlayers
(define (deletePlayer P N)
  (define (deletePlayerX P N ite)
    (if (and
         (players? P)
         (< N 3)
         (>= N 0)
         )
        (cond
          [(= ite N) (deletePlayerX P N (+ ite 1))]
          [(= ite (length P)) null]
          [else (cons (getPlayers P ite) (deletePlayerX P N (+ ite 1)))]
          )
        null
        )
    )
  (deletePlayerX P N 0)
  )
;getPlayerXY: parámetros (player)
;desc: Nos devuelve la coordenada X e Y en forma de lista. Nos sirve para hacer comparaciones en la función play.
;dom: player
;rec: lista de coordenada X e Y
(define (getPlayerXY P)
  (if (player? P)
      (list (getPlayerX P) (getPlayerY P))
      null
      )
  )
(define (listPlayerXY players)
  (define (listPlayerXYX players ite)
    (if (not (null? players))
        (cond
      [(= ite -1) (listPlayerXYX players (- (length players) 1))]
      [(= ite 0) (cons (getPlayerXY (getPlayers players 0)) null)]
      [else (cons (getPlayerXY (getPlayers players ite)) (listPlayerXYX players (- ite 1)))]
      )
    null
    )
    )
  (listPlayerXYX players -1)
    )
;Para llamar en otros archivos
(provide (all-defined-out))