#lang racket

(require "funcionesGenerales.rkt")
(require "floorAndEarth.rkt")
(require "player.rkt")
(require "enemy.rkt")
(require "bullet.rkt")
    
;createScene: parámetros (M N E D seed)
;desc: Función constructora de scene.
;Dom: entero X entero X entero X entero X num
;N y M indican el tamaño del escenario.
;E la cantidad de enemigos.
;D indica la la dificultad.
;seed indica la semilla de la escena.
;Rec: scene.
;tipo de recursión: Natural.
(define (createScene M N E D seed)
    (if (and
         (intPositive? M)
         (intPositive? N)
         (intPositive? E)
         (and (intPositive? D) (< D 3) (> D 0))
         (intPositive? seed)
         (> M (+ 3 E))
         )
        (if (= D 1) ;Dificultad 1 (players con 2 de vida)
            (list "PLAYING" M N E D seed (createEarth M N seed)
                  (generatePlayer 2)
                  (generateEnemy (createEarth M N seed) E seed null)
                  )
            ;Dificultad 2 (players con 1 de vida)
            (list "PLAYING" M N E D seed (createEarth M N seed)
                  (generatePlayer 1)
                  (generateEnemy (createEarth M N seed) E seed null)
                  )
            )
        null
        )
  )
;checkScene: parámetros (algo)
;desc: Función de pertenencia de scene.
;dom: algo
;rec: booleano
;Encapsulación:
;ite1: Entero que nos hace iterar la lista de scene
;ite2: Entero que nos hace iterar las listas dentro de scene
;repeated: Lista que nos guarda cuáles posiciones han recorrido los player y enemigos,
;aparte de revisar si están en el suelo o no.
(define (checkScene S)
  (define (checkSceneX S ite1 ite2 repeated) ;ite1 para scene, ite2 para listas dentro de scene.
    (if (and
         (not (null? S))
         (list? S)
         (> (length S) 7)
         (string? (get S 0))
         (intPositive? (get S 1))
         (intPositive? (get S 2))
         (intPositive? (get S 3))
         (intPositive? (get S 4))
         (intPositive? (get S 5))
         )
        (cond
        [(= ite1 -1) (checkSceneX S (- (length S) 1) (- (length (get S (- (length S) 1))) 1) repeated)]
        [(= ite1 6) (if (<= ite2 0) ;revisamos floor
                        (floor? (get (get S 6) 0))
                        (and (checkSceneX S ite1 (- ite2 1) repeated) (floor? (get (get S ite1) ite2)))
                        )]
        [(= ite1 7) (if (<= ite2 0) ;revisamos player
                        (and (checkSceneX S (- ite1 1) (- (length (get S (- ite1 1))) 1)
                                          (myAppend repeated (list (getPlayerX (get (get S ite1) 0)) (getPlayerY (get (get S ite1) 0)))))
                             (and (player? (get (get S ite1) 0))
                                  (find (get S 6) (list (getPlayerX (get (get S ite1) 0)) (- (getPlayerY (get (get S ite1) 0)) 1)));Player está en suelo
                                  (not (find repeated (list (getPlayerX (get (get S ite1) 0)) (getPlayerY (get (get S ite1) 0)))));Player no tiene posicion repetida
                                  ))
                        (and (checkSceneX S ite1 (- ite2 1)
                                          (myAppend repeated (list (getPlayerX (get (get S ite1) ite2)) (getPlayerY (get (get S ite1) ite2)))));Agrego posicion
                             (and
                             (player? (get (get S ite1) ite2))
                             (find (get S 6) (list (getPlayerX (get (get S ite1) ite2)) (- (getPlayerY (get (get S ite1) ite2)) 1)))
                             (not (find repeated (list (getPlayerX (get (get S ite1) ite2)) (getPlayerY (get (get S ite1) ite2)))))
                             )
                             )
                        )]
        [(= ite1 8) (if (<= ite2 0) ;revisamos enemy
                        (and (checkSceneX S (- ite1 1) (- (length (get S (- ite1 1))) 1)
                                          (myAppend repeated (list (getEnemyX (get (get S ite1) 0)) (getEnemyY (get (get S ite1) 0)))))
                             (and
                             (enemy? (get (get S ite1) 0))
                             (find (get S 6) (list (getEnemyX (get (get S ite1) 0)) (- (getEnemyY (get (get S ite1) 0)) 1)))
                             (not (find repeated (list (getEnemyX (get (get S ite1) 0)) (getEnemyY (get (get S ite1) 0)))))
                             )
                             )
                        (and (checkSceneX S ite1 (- ite2 1)
                                          (myAppend repeated (list (getEnemyX (get (get S ite1) ite2)) (getEnemyY (get (get S ite1) ite2)))))
                             (and
                              (enemy? (get (get S ite1) ite2))
                              (find (get S 6) (list (getEnemyX (get (get S ite1) ite2)) (- (getEnemyY (get (get S ite1) ite2)) 1)))
                              (not (find repeated (list (getEnemyX (get (get S ite1) ite2)) (getEnemyY (get (get S ite1) ite2)))))
                              )
                             )
                        )]
        [(= ite1 9) (if (bullet? (get S ite1));Revisamos si hay bala
                        (checkSceneX S (- ite1 1) (- (length (get S (- ite1 1))) 1) repeated)
                        #f
                        )]
        [else #f])
        #f
        )
    )
  (checkSceneX S -1 0 null)
  )
;Selectoras de Scene:
;getSceneStatus: parámetros (scene)
;desc: Nos entrega el estado de una escena. ("PLAYING","WIN","LOSE" o "DRAW")
;dom: Scene
;rec: Estado de scene. (string)
(define (getSceneStatus scene)
  (if (checkScene scene)
      (get scene 0)
      "null"
      )
  )
;getSceneM: parámetros (scene)
;desc: Nos entrega la cantidad de suelo que tiene un scene.
;dom: Scene
;rec: Cantidad de suelo que tiene. (entero)
(define (getSceneM scene)
  (if (checkScene scene)
      (get scene 1)
      -1
      )
  )

;getSceneN: parámetros (scene)
;desc: Nos entrega la cantidad de altura que tiene un scene.
;dom: Scene
;rec: Cantidad de altura que tiene. (entero)
(define (getSceneN scene)
  (if (checkScene scene)
      (get scene 2)
      -1
      )
  )
;getSceneE: parámetros (scene)
;desc: Nos entrega la cantidad de enemigos que tiene un scene.
;dom: Scene
;rec: Cantidad de enemigos que tiene. (entero)
(define (getSceneE scene)
  (if (checkScene scene)
      (get scene 3)
      -1
      )
  )
;getSceneD: parámetros (scene)
;desc: Nos entrega la dificultad de una scene.
;dom: Scene
;rec: Dificultad de la scene. (1 o 2)
(define (getSceneD scene)
  (if (checkScene scene)
      (get scene 4)
      -1
      )
  )
;getSceneSeed: parámetros (scene)
;desc: Nos entrega la seed de una scene
;dom: Scene
;rec: seed de la scene (entero)
(define (getSceneSeed scene)
  (if (checkScene scene)
      (get scene 5)
      -1
      )
  )
;getSceneEarth: parámetros (scene)
;desc: Nos entrega el conjunto de suelo de una scene.
;dom: Scene
;rec: Earth (lista de floor)
(define (getSceneEarth scene)
  (if (checkScene scene)
      (get scene 6)
      null
      )
  )
;getScenePlayers: parámetros (scene)
;desc: Nos entrega el conjunto de enemigos de una scene.
;dom: scene
;rec: enemies
(define (getScenePlayers scene)
  (if (checkScene scene)
      (get scene 7)
      null
      )
  )
;getSceneEnemies: parámetros (scene)
;desc: Nos entrega el conjunto de enemigos de una scene.
;dom: scene
;rec: enemies
(define (getSceneEnemies scene)
  (if (checkScene scene)
      (get scene 8)
      null
      )
  )
;getSceneBullet: parámetros (scene)
;desc: Nos entrega la bullet (si es que hay) de una scene, si no la hay nos entrega null de todos modos.
;dom: scene
;rec: bullet (si la hay)
(define (getSceneBullet scene)
  (if (checkScene scene)
      (get scene 9);Tal como está hecha la función get, nos entregaría null de todos modos.
      null
      )
  )

;Modificadoras de Scene
;setScenePlayer: parámetros (scene N M O)
;desc: Modifica uno de los player del conjuntoPlayer de Scene.
;dom: scene  X entero X entero X entero
;N: Número de player que queremos modificar.
;M: Valor a modificar
;O: Lo que queremos modificar de ese player
;O = 1: Modificamos su coordenada en X
;O = 2: Modificamos su coordenada en Y
;O = 3: Modificamos su angle
;O = 4: Modificamos su vida
(define (setScenePlayer scene N M O)
  (if (and
       (checkScene scene)
       (intPositive? N)
       (>= N 0)
       (< N 3)
       (intPositive? M)
       (intPositive? O)
       )
      (cond
        [(= O 1) (list (getSceneStatus scene) (getSceneM scene)
                       (getSceneN scene) (getSceneE scene) (getSceneD scene)
                       (getSceneEarth scene) (setPlayersX (getScenePlayers scene) N M)
                       (getSceneEnemies scene))]
        [(= O 2) (list (getSceneStatus scene) (getSceneM scene)
                       (getSceneN scene) (getSceneE scene) (getSceneD scene)
                       (getSceneEarth scene) (setPlayersY (getScenePlayers scene) N M)
                       (getSceneEnemies scene))]
        [(= O 3) (list (getSceneStatus scene) (getSceneM scene)
                       (getSceneN scene) (getSceneE scene) (getSceneD scene)
                       (getSceneEarth scene) (setPlayersAngle (getScenePlayers scene) N M)
                       (getSceneEnemies scene))]
        [(= O 4) (list (getSceneStatus scene) (getSceneM scene)
                       (getSceneN scene) (getSceneE scene) (getSceneD scene)
                       (getSceneEarth scene) (setPlayersLife (getScenePlayers scene) N M)
                       (getSceneEnemies scene))]
        [else scene]
        )
      scene
      )
  )

;setSceneEnemy: parámetros (scene N M O)
;desc: Modifica uno de los enemy del conjuntoEnemy de Scene.
;dom: scene X entero X entero X entero
;N: Número de enemy que queremos modificar.
;M: Valor a modificar:
;O: Lo que queremos modificar de ese enemy.
;O: Lo que queremos modificar de ese player
;O = 1: Modificamos su coordenada en X
;O = 2: Modificamos su coordenada en Y
;O = 3: Modificamos su angle
;O = 4: Modificamos su vida
(define (setSceneEnemy scene N M O)
  (if (and
       (checkScene scene)
       (intPositive? N)
       (>= N 0)
       (< N (getSceneE scene))
       (intPositive? M)
       (intPositive? O)
       )
      (cond
        [(= O 1) (list (getSceneStatus scene) (getSceneM scene)
                       (getSceneN scene) (getSceneE scene) (getSceneD scene)
                       (getSceneEarth scene) (getScenePlayers scene)
                       (setEnemiesX (getSceneEnemies scene) N M))]
        [(= O 2) (list (getSceneStatus scene) (getSceneM scene)
                       (getSceneN scene) (getSceneE scene) (getSceneD scene)
                       (getSceneEarth scene) (getScenePlayers scene)
                       (setEnemiesY (getSceneEnemies scene) N M))]
        [(= O 3) (list (getSceneStatus scene) (getSceneM scene)
                       (getSceneN scene) (getSceneE scene) (getSceneD scene)
                       (getSceneEarth scene) (getScenePlayers scene)
                       (setEnemiesAngle (getSceneEnemies scene) N M))]
        [(= O 4) (list (getSceneStatus scene) (getSceneM scene)
                       (getSceneN scene) (getSceneE scene) (getSceneD scene)
                       (getSceneEarth scene) (getScenePlayers scene)
                       (setEnemiesLife (getSceneEnemies scene) N M))]
        [else scene]
        )
      scene
      )
  )
      
      



;Scene1:
(define S1 (createScene 100 20 3 1 123))
;Para llamar en otros archivos
(provide (all-defined-out))

                    
                             
  



                
        
