#lang racket
(require "funcionesGenerales.rkt")
(require "floorAndEarth.rkt")
(require "player.rkt")
(require "enemy.rkt")
(require "bullet.rkt")
(require "scene.rkt")

;play: parámetros (scene member move tf angle seed)
;desc: Función avanzada de scene. Cambia la scene en base a
;parámetros de la función. Como paso 1, movemos al player
;como paso 2, dispara y como paso 3, dispara el enemigo.
;Nos entrega la escena resultante de ese proceso

;dom: scene X entero X entero X proceso X numero X entero
;scene: Scene creada a partir de la función createScene
;member: Número que representa el player que vamos a mover (el primero se pone como 0).

;move: Cantidad de pasos a movernos. Si quedamos en la posición de un aliado, enemigo o fuera de posición, ese player
;muere. Poner una cantidad negativa para moverse hacia la izquierda, positiva para la derecha y 0 para no moverse.

;tf: Función de trayectoria. La hecha se llama paraMove y está ubicada en el archivo de bullet.
;angle: Ángulo de la bala con el que dispara player
;seed: semilla (que no utilizamos en esta función)
;rec: nueva scene
(define play (lambda (scene)
              (lambda (member)
                (lambda (move)
                  (lambda (tf)
                    (lambda (angle)
                      (lambda (seed)
                        (define (playX scene member move tf step bullet)
                          #|(display (list (checkScene scene)
                             (intPositive? member)
                             (number? move)
                             (< member (getSceneP scene))
                             (>= member 0)
                             (procedure? tf)
                             (number? angle)))|#
                          (if (and
                             (checkScene scene)
                             (equal? (getSceneStatus scene) "PLAYING")
                             (intPositive? member)
                             (number? move)
                             (< member (getSceneP scene))
                             (>= member 0)
                             (procedure? tf)
                             (number? angle)
                             )
                              
                              (cond
                                ;Step 0: Mover jugador.
                                [(= step 0)
                                 (cond
                                   [(and (null? (getScenePlayers scene))
                                         (null? (getSceneEnemies scene)))
                                    (setSceneStatus scene "DRAW")]
                                   [(null? (getScenePlayers scene))
                                    (setSceneStatus scene "DEFEAT")
                                    ]
                                   [(null? (getSceneEnemies scene))
                                    (setSceneStatus scene "WIN")
                                    ]
                                   [else
                                    
                                    (let  ([newS (setScenePlayer scene member (+ (getPlayerX (getPlayers (getScenePlayers scene) member)) move) 1)])
                                      (if (not (checkScene newS))
                                          (if (null? (getScenePlayers(deleteScenePlayer newS member)))
                                              ;Player muere si se queda en una casilla de otro player, enemy o fuera de floor.
                                              (setSceneStatus (deleteScenePlayer newS member) "LOSE")
                                              (playX (deleteScenePlayer newS member)
                                                     0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))
                                                     )
                                              )
                                          (playX newS 0 0 tf 1 (tf bullet))
                                          )
                                      )]
                                   )]
                                ;Step 1: Disparar jugador
                                [(= step 1)
                                 (cond 
                                   ;Bala en enemigo
                                   [(find (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))
                                               (if (= (length (getSceneEnemies scene)) 1) ;Muerieron todos los enemigos?
                                                   (deleteSceneEnemy (setSceneStatus scene "WIN") 0)
                                                   ;Buscamos en qué enemigo fue, y borramos ese enemigo.;(findXY (listEnemiesXY (getSceneEnemies scene)) (getBulletXY bullet))]
                                                   (playX
                                                    (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet)))
                                                    0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180)))
                                                   )]
                                              ;Bala en aliado
                                              [(find (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet))
                                               (if (and (= (length (getScenePlayers scene)) 1) (= (getPlayerLife (getPlayers (getScenePlayers scene) 0)) 1))
                                                   (deleteScenePlayer (setSceneStatus scene "LOSE") 0)
                                                   (if (= (getPlayerLife (get (getScenePlayers scene) (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)))) 1)
                                                        (playX
                                                        (deleteScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)))
                                                        0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180)))
                                                       (playX (setScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)) 1 4)
                                                               0 0 tf 2  (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))
                                                        )
                                                       )
                                                   )]
                               
                                              ;Bala en Earth o fuera de rango
                                              [(or (find (getSceneEarth scene) (getBulletXY bullet)) (and (< (getBulletX bullet) 0) (> (getBulletX bullet) (getSceneM scene)))
                                                   (> (getBulletY bullet) (getSceneN scene)))
                                               (playX
                                                scene 0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))
                                                )]
                                               
                                              ;Bala en recorrido
                                              [else 
                                               (playX scene 0 0 tf 1 (tf bullet))]
                                               
                                              )]
                                
                                ;Step 2: Disparar Enemigo
                                [(= step 2) (cond
                                              ;Bala en enemigo
                                              [(find (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))
                                                ;Verificamos status
                                               (if (= (length (getSceneEnemies scene)) 1)
                                                   (deleteSceneEnemy (setSceneStatus scene "WIN") 0)
                                                   (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))))]
                                              ;Bala en aliado
                                              [(find (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet))
                                               ;Verificamos status
                                               (if (and (= (length (getScenePlayers scene)) 1) (= (getPlayerLife (getPlayers (getScenePlayers scene) 0)) 1))
                                                   (deleteScenePlayer (setSceneStatus scene "LOSE") 0)
                                                   (if (= (getPlayerLife (getPlayers (getScenePlayers scene) 0)) 1)
                                                       (deleteScenePlayer scene (findXY (listPlayerXY (getSceneEnemies scene)) (getBulletXY bullet)))
                                                              (setScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)) 1 4)
                                                              ))]
                                              ;Bala en Earth o fuera de rango
                                              [(or (find (getSceneEarth scene) (getBulletXY bullet)) (<= (getBulletX bullet) 0) (> (getBulletX bullet) (getSceneM scene))
                                                   (> (getBulletY bullet) (getSceneN scene)))
                                               scene]
                                              ;Bala en recorrido
                                              [else (playX scene 0 0 tf 2 (tf bullet))]
                                              )]
                                
                                )
                              null
                              )
                          )
                        (playX scene member move tf 0 (createBullet (+ (getPlayerX (getPlayers (getScenePlayers
                                                                                 scene) member)) move)
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

;playLazy: parámetros (scene member move tf t angle seed)
;desc: Función que recibe una escena e instrucciones para jugar con esa escena
;(tal como la función play), solo que lo que va retornando cada llamado recursivo
;es un par de la escena en lazy (pero como décimo parámetro la bala que va recorriendo)
;y el siguiente llamado, así haciendo una lista de promesas, donde cada promesa es una escena
;donde esta vez si se incluyen las balas.
;dom: scene X entero X entero X procedure X algo X number X algo
;scene: Escena donde queremos jugar
;member: Número del 0 al (cant miembros vivos - 1), que indica el player que queramos usar
;move: Número entero que indica la cantidad de pasos que queramos hacer (negativo izq, positivo der)
;tf: Función de trayectoria (Hay una descrita en el archivo bullet.rkt, llamada paraMove
;t: Puede ser lo que sea, ya que este número no es usado en esta función.
;angle: Ángulo con el que el player quiere disparar.
;seed: semilla
(define (playLazy scene member move tf t angle seed) 
  (define (playLazyX scene member move tf step bullet)
    (if (and
         (checkScene scene)
         (equal? (getSceneStatus scene) "PLAYING")
         (intPositive? member)
         (number? move)
         (< member (getSceneP scene))
         (>= member 0)
         (procedure? tf)
         (number? angle)
         )
                              
        (cond
          ;Step 0: Mover jugador.
          [(= step 0)
           
           (cond
             [(and (null? (getScenePlayers scene))
                   (null? (getSceneEnemies scene)))
              (setSceneStatus scene "DRAW")]
             [(null? (getScenePlayers scene))
              (setSceneStatus scene "DEFEAT")
              ]
             [(null? (getSceneEnemies scene))
              (setSceneStatus scene "WIN")
              ]
             [else
                                    
              (let  ([newS (setScenePlayer scene member (+ (getPlayerX (getPlayers (getScenePlayers scene) member)) move) 1)])
                (if (not (checkScene newS))
                    (if (null? (getScenePlayers(deleteScenePlayer newS member)))
                        (setSceneStatus (deleteScenePlayer newS member) "LOSE")


                        ;Retorno 1: Player muere si se queda en una casilla de otro player, enemy o fuera de floor.
                        (cons (lazy (myAppend (deleteScenePlayer newS member) (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))))

                                        (playLazyX (deleteScenePlayer newS member)
                               0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180)))
                                        )
                        ) 


                    ;Retorno 2: Player se movió y pudo disparar
                    (cons (lazy (myAppend newS (tf bullet))) (playLazyX newS 0 0 tf 1 (tf bullet))
                    ))
                )]
             )]
          ;Step 1: Disparar jugador
          [(= step 1)
           
           (cond 
             ;Bala en enemigo
             [(find (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))
              (display (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180)))
              (if (= (length (getSceneEnemies scene)) 1) ;Muerieron todos los enemigos?

                  ;Retorno 3: Murieron todos los enemigos
                  (cons (lazy (deleteSceneEnemy (setSceneStatus scene "WIN") 0)) null)
                  ;Buscamos en qué enemigo fue, y borramos ese enemigo.


                  ;Retorno 4:
                  (cons
                   (lazy (myAppend (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet)))
                                   (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))))
                                   
                   (playLazyX
                   (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet)))
                   0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180)))
                   )
                  )]
             ;Bala en aliado
             [(find (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet))
              
              (if (and (= (length (getScenePlayers scene)) 1) (= (getPlayerLife (getPlayers (getScenePlayers scene) 0)) 1))

                  ;Retorno 5: Mueren todos los aliados
                  (cons (lazy (deleteScenePlayer (setSceneStatus scene "LOSE") 0)) null)
                  (if (= (getPlayerLife (get (getScenePlayers scene) (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)))) 1)

                      ;Retorno 6:
                      (cons
                       (lazy (myAppend (deleteScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)))
                                       (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))))
                        (playLazyX
                       (deleteScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)))
                       0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))))

                      ;Retorno 7:
                      (cons
                       (lazy (myAppend (setScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)) 1 4)
                                       (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))))
                             
                        (playLazyX (setScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)) 1 4)
                             0 0 tf 2  (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180)))
                             )
                      )
                  )]
                               
             ;Bala en Earth o fuera de rango
             [(or (find (getSceneEarth scene) (getBulletXY bullet)) (and (< (getBulletX bullet) 0) (> (getBulletX bullet) (getSceneM scene)))
                  (> (getBulletY bullet) (getSceneN scene)))


              ;Retorno 8:
              
              (cons
               (lazy (myAppend scene (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))))
                (playLazyX
               scene 0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180)))
                
               )]
                                               
             ;Bala en recorrido
             [else
              
              ;Retorno 9:
              (cons
               (lazy (myAppend scene (tf bullet)))
                (playLazyX scene 0 0 tf 1 (tf bullet)))]
                                               
             )]
                                
          ;Step 2: Disparar Enemigo
          [(= step 2) (cond
                        ;Bala en enemigo
                        [(find (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))
                         ;Verificamos status
                         (if (= (length (getSceneEnemies scene)) 1) ;Enemigo dispara a enemigo

                             ;Retorno 10
                             (cons (lazy (deleteSceneEnemy (setSceneStatus scene "WIN") 0)) null)

                             ;Retorno 11:
                             (cons (lazy (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet)))) null)
                             )]
                        ;Bala en aliado
                        [(find (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet))
                         ;Verificamos status
                         (if (and (= (length (getScenePlayers scene)) 1) (= (getPlayerLife (getPlayers (getScenePlayers scene) 0)) 1))

                             ;Retorno 12:
                             (cons (lazy(deleteScenePlayer (setSceneStatus scene "LOSE") 0)) null)
                             (if (= (getPlayerLife (getPlayers (getScenePlayers scene) 0)) 1)

                                 ;Retorno 13:
                                 (cons (lazy (deleteScenePlayer scene (findXY (listPlayerXY (getSceneEnemies scene)) (getBulletXY bullet)))) null)

                                 ;Retorno 14:
                                 (cons (lazy (setScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)) 1 4)) null)
                                 ))]
                        ;Bala en Earth o fuera de rango
                        [(or (find (getSceneEarth scene) (getBulletXY bullet)) (<= (getBulletX bullet) 0) (> (getBulletX bullet) (getSceneM scene))
                             (> (getBulletY bullet) (getSceneN scene)))

                         ;Retorno 15:
                         (cons (lazy scene) null)]
                        ;Bala en recorrido
                        [else
                         
                         ;Retorno 16:
                         (cons (lazy (myAppend scene (tf bullet))) (playLazyX scene 0 0 tf 2 (tf bullet)))]
                        )]
                                
          )
        null
        )
    )
  (playLazyX scene member move tf 0 (createBullet (+ (getPlayerX (getPlayers (getScenePlayers
                                                                          scene) member)) move)
                                              (getPlayerY (getPlayers (getScenePlayers
                                                                       scene ) member))
                                              angle))
  )

;sceneFile: parámetros (scene file)
;desc: Función que recibe una escena, y una fila que queramos representar
;desde 1 a M.
;dom: sceneXentero
;rec: String, representando:
;"-" para nada
;"$" para floor
;"P" para player
;"E" para enemy
;tipo recursión: natural
(define (sceneFile scene file)
  (define (sceneFileX scene file ite)
    (cond
      [(= ite (getSceneM scene))
       (cond
         [(find (getSceneEarth scene) (list ite file))
          "$"]
         [(find (listEnemyXY(getSceneEnemies scene)) (list ite file))
          "E"]
         [(find (listPlayerXY (getScenePlayers scene)) (list ite file))
          "P"]
         [(equal? (getBulletXY (get scene 10)) (list ite file))
          "="]
         [else "-"]
          )]
      [else
       (cond
         [(find (getSceneEarth scene) (list ite file))
          (string-append "$" (sceneFileX scene file (+ ite 1)))]
         [(find (listEnemyXY(getSceneEnemies scene)) (list ite file))
          (string-append "E" (sceneFileX scene file (+ ite 1)))]
         [(find (listPlayerXY (getScenePlayers scene)) (list ite file))
          (string-append "P" (sceneFileX scene file (+ ite 1)))]
         [(equal? (getBulletXY (get scene 10)) (list ite file))
          (string-append "=" (sceneFileX scene file (+ ite 1)))]
         [else
          (string-append "-" (sceneFileX scene file (+ ite 1)))
          ]
         )
       ]
      )
    )
  (sceneFileX scene file 1)
  )

;scene->string: parámetros (scene)
;desc: Función que convierte una escena a un string.
;dom: scene
;rec: string
;tipo recursión: natural
(define (scene->string scene)
  (define (convX scene ite)
    (cond
      [(= ite -1)
       (string-append (getSceneStatus scene) "  ENE:"
                      (number->string (getSceneE scene))
                      "   PL:" (number->string (getSceneP scene))
                      "\n\n\n" (convX scene (getSceneN scene)))
       ]
      [(= ite 1) (sceneFile scene 1)]
      [else (string-append (sceneFile scene ite) "\n" (convX scene (- ite 1)) )]
      )
    )
  (if (checkScene scene)
      (string-append (convX scene -1) "\n")
      "no-scene"
      )
  )


;displayScene: parámetros (scene)
;desc: Función que recibe una scene y la muestra por pantalla.
;dom: Scene
;rec: nada
(define (displayScene scene)
  (display (scene->string scene))
  )

;displayLazy: parámetros (lazyScene N)
;desc: Función que recibe una lista de promesas de scene, y muestra
;la escena número N por pantalla.
;dom: listaPromesasDeScene X entero
;N: Número de la lista de promesas que queremos mostrar
;rec: nada.
(define (displayLazy lazyScene N)
  (if (and
       (andmap promise? lazyScene)
       (intPositive? N)
       (>= N 0)
       (< N (length lazyScene))
       )
      (displayScene (force (get lazyScene N)))
      (display "No es una lista de promesas o te pasaste de largo")
      )
  )

;displayAllLazy: parámetros (lazyScene)
;desc: Función que toma una lista de promesas de scene,
;y las muestra todas por pantalla.
;dom: lista de promesas de scene
;rec: nada
;tipo rec: natural
(define (displayAllLazy lazyScene)
  (define (all lazyScene ite)
    (displayLazy lazyScene ite)
    (if (= ite (- (length lazyScene) 1))
        (display "Acabado\n")
        (all lazyScene (+ ite 1))
        )
    )
  (all lazyScene 0)
  )
        
      
#|(define (playTillLose scene)
  (display (scene->string scene))
  (if (or
       (equal? (getSceneStatus scene) "WIN")
       (equal? (getSceneStatus scene) "LOSE")
       (equal? (getSceneStatus scene) "DRAW")
       )
      null
      (playTillLose ((((((play scene)0)0)paraMove)0)0))
       )
  )|#

;Definiciones de prueba                       
(define A (deletePlayer (generatePlayer 1)0))
(define B (createEarth 10 10 0))
(define C (setEnemiesX (generateEnemy B 1 0) 0 1))
(define D (list "PLAYING" 10 10 1 2 1 0 B A C))
;((((((play D )0 ) 0 )paraMove )0 )0)
         