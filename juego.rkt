#lang racket
(require "funcionesGenerales.rkt")
(require "floorAndEarth.rkt")
(require "player.rkt")
(require "enemy.rkt")
(require "bullet.rkt")
(require "scene.rkt")



(define play (lambda (scene)
              (lambda (member)
                (lambda (move)
                  (lambda (tf)
                    (lambda (angle)
                      (lambda (seed)
                        (define (playX scene member move tf step bullet)
                          (if (and
                             (checkScene scene)
                             (intPositive? member)
                             (number? move)
                             (< member (getSceneP scene))
                             (>= member 0)
                             (procedure? tf)
                             (number? angle)
                             )
                              
                              (cond
                                ;Step 0: Mover jugador.
                                [(= step 0) (let  ([newS (setScenePlayer scene member (+ (getPlayerX (getPlayers (getScenePlayers scene) member)) move) 1)])
                                              (if (not (checkScene newS))
                                                  (playX (deleteScenePlayer newS member)
                                                   0 0 tf 2 (tf (createBullet (getEnemyX (getEnemies (getSceneEnemies scene) (- (getSceneE scene) 1)))
                                                                              (getEnemyY (getEnemies (getSceneEnemies scene) (- (getSceneE scene) 1)))
                                                                               (modulo (getSceneSeed scene) 360)))) ;Player muere si se queda en una casilla de otro player, enemy o fuera de floor.
                                                  (playX newS 0 0 tf 1 (tf bullet))
                                                  )
                                              )]
                                ;Step 1: Disparar jugador
                                [(= step 1) (cond
                                              ;Bala en enemigo
                                              [(find (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))
                                                ;Buscamos en qué enemigo fue, y borramos ese enemigo.;(findXY (listEnemiesXY (getSceneEnemies scene)) (getBulletXY bullet))]
                                               (playX
                                                (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet)))
                                                0 0 tf 2 (tf (createBullet (getEnemyX (getEnemies (getSceneEnemies scene) (- (getSceneE scene) 1)))
                                                                                                         (getEnemyY (getEnemies (getSceneEnemies scene) (- (getSceneE scene) 1)))
                                                                                                         (modulo (getSceneSeed scene) 360)))
                                                )]
                                              ;Bala en aliado
                                              [(find (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet))
                                               (playX
                                                (deleteScenePlayer scene (findXY (listPlayerXY (getSceneEnemies scene)) (getBulletXY bullet)))
                                                0 0 tf 2 (tf (createBullet (getEnemyX (getEnemies (getSceneEnemies scene) (- (getSceneE scene) 1)))
                                                                                                         (getEnemyY (getEnemies (getSceneEnemies scene) (- (getSceneE scene) 1)))
                                                                                                         (modulo (getSceneSeed scene) 360)))
                                                )]
                               
                                              ;Bala en Earth o fuera de rango
                                              [(or (find (getSceneEarth scene) (getBulletXY bullet)) (and (< (getBulletX bullet) 0) (> (getBulletX bullet) (getSceneM scene)))
                                                   (> (getBulletY bullet) (getSceneN scene)))
                                               (playX
                                                scene 0 0 tf 2 (tf (createBullet (getEnemyX (getEnemies (getSceneEnemies scene) (- (getSceneE scene) 1)))
                                                                                                         (getEnemyY (getEnemies (getSceneEnemies scene) (- (getSceneE scene) 1)))
                                                                                                         (modulo (getSceneSeed scene) 360)))
                                                )]
                                               
                                              ;Bala en recorrido
                                              [else
                                               (playX scene 0 0 tf 2 (tf bullet))]
                                               
                                              )]
                                ;Step 2: Disparar Enemigo
                                [(= step 2) (cond
                                              ;Bala en enemigo
                                              [(find (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))
                                                ;Verificamos status


                                               (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet)))]
                                              ;Bala en aliado
                                              [(find (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet))
                                               ;Verificamos status


                                               (deleteScenePlayer scene (findXY (listPlayerXY (getSceneEnemies scene)) (getBulletXY bullet)))]

                                              ;Bala en Earth o fuera de rango
                                              [(or (find (getSceneEarth scene) (getBulletXY bullet)) (and (< (getBulletX bullet) 0) (> (getBulletX bullet) (getSceneM scene)))
                                                   (> (getBulletY bullet) (getSceneN scene)))
                                               scene]
                                              )]
                                )
                              scene
                              )
                          )
                        (playX scene member move tf 0 (createBullet (getPlayerX (getPlayers (getScenePlayers
                                                                                 scene) member))
                                                                    (getPlayerY (getPlayers (getScenePlayers
                                                                                 scene ) member))
                                                                    angle))
                        )
                      )
                    )
                  )
                )
              )
  )
                                           
                        
                            
         