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

                                              ;Retorno 1: Todos los players mueren.
                                              (setSceneStatus (deleteScenePlayer newS member) "LOSE")

                                              ;Retorno 2: Muere un player pero se sigue jugando.
                                              (playX (deleteScenePlayer newS member)
                                                     0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))
                                                     )
                                              )
                                          ;Retorno 3: Player se pudo mover y dispara.
                                          (playX newS 0 0 tf 1 (tf bullet))
                                          )
                                      )]
                                   )]
                                ;Step 1: Disparar jugador
                                [(= step 1)
                                 (cond
      
                                   ;Bala en enemigo
                                   [(find (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))
                                               (if (= (length (getSceneEnemies scene)) 1);Murieron todos los enemigos?

                                                   ;Retorno 4: Murieron todos los enemigos.
                                                   (deleteSceneEnemy (setSceneStatus scene "WIN") 0)
                                                   
                                                   ;Buscamos en qué enemigo fue, y borramos ese enemigo.
                                                   ;Retorno 5: Murió solo un enemigo.
                                                   (playX
                                                    (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet)))
                                                    0 0 tf 2 (tf (createBulletBy (getSceneEnemies (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))))
                                                (- (getSceneE (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))))
                                                   1) 180)))
                                                   )]
                                              ;Bala en aliado
                                              [(find (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet))
                                               (if (and (= (length (getScenePlayers scene)) 1) (= (getPlayerLife (getPlayers (getScenePlayers scene) 0)) 1))

                                                   ;Retorno 6: Muere un aliado por bala aliada, no queda ningún aliado.
                                                   (deleteScenePlayer (setSceneStatus scene "LOSE") 0)
                                                   (if (= (getPlayerLife (get (getScenePlayers scene) (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)))) 1)
                                                       ;Retorno 7: Muere un aliado por disparo aliado.
                                                        (playX
                                                        (deleteScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)))
                                                        0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180)))

                                                        ;Retorno 8: Se le baja 1 de vida a un aliado por disparo aliado.
                                                       (playX (setScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)) 1 4)
                                                               0 0 tf 2  (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))
                                                        )
                                                       )
                                                   )]
                               
                                              ;Bala en Earth o fuera de rango
                                              [(or (find (getSceneEarth scene) (getBulletXY bullet)) (< (getBulletX bullet) 0) (> (getBulletX bullet) (getSceneM scene))
                                                   (> (getBulletY bullet) (getSceneN scene)))

                                               ;Retorno 9: Bala aliada en tierra o fuera de rango.
                                               (playX
                                                scene 0 0 tf 2 (tf (createBulletBy (getSceneEnemies scene) (- (getSceneE scene) 1) 180))
                                                )]
                                               
                                              ;Bala en recorrido
                                              [else
                                               ;Retorno 10: Bala en recorrido.
                                               (playX scene 0 0 tf 1 (tf bullet))]
                                               
                                              )]
                                
                                ;Step 2: Disparar Enemigo
                                [(= step 2) (cond
                                              ;Bala en enemigo
                                              [(find (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))
                                                ;Verificamos status
                                               (if (= (length (getSceneEnemies scene)) 1)
                                                   
                                                   ;Retorno 11: Enemigo muerto por una bala enemiga, todos los enemigos muertos.
                                                   (deleteSceneEnemy (setSceneStatus scene "WIN") 0)
                                                   
                                                   ;Retorno 12: Enemigo muerto por una bala enemiga.
                                                   (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))))]
                                              ;Bala en aliado
                                              [(find (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet))
                                               ;Verificamos status
                                               (if (and (= (length (getScenePlayers scene)) 1) (= (getPlayerLife (getPlayers (getScenePlayers scene) 0)) 1))
                                                   
                                                   ;Retorno 13: Todos los aliados muertos, el último por bala enemiga.
                                                   (deleteScenePlayer (setSceneStatus scene "LOSE") 0)
                                                   (if (= (getPlayerLife (getPlayers (getScenePlayers scene) 0)) 1)
                                                       ;Retorno 14: Aliado muerto por bala enemiga.
                                                       (deleteScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)))
                                                       ;Retorno 15: Aliado herido por bala enemiga.
                                                              (setScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)) 1 4)
                                                              ))]
                                              ;Bala en Earth o fuera de rango
                                              [(or (find (getSceneEarth scene) (getBulletXY bullet)) (<= (getBulletX bullet) 0) (> (getBulletX bullet) (getSceneM scene))
                                                   (> (getBulletY bullet) (getSceneN scene)))
                                               ;Retorno 16: Bala enemiga fuera de rango o en el suelo
                                               scene]
                                              ;Bala en recorrido
                                              ;Retorno 17: Bala en recorrido.
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
              (cons (lazy (setSceneStatus scene "DRAW")) null)
              ]
             [(null? (getScenePlayers scene))
              (cons (lazy (setSceneStatus scene "DEFEAT")) null)
              ]
             [(null? (getSceneEnemies scene))
              (cons (lazy (setSceneStatus scene "WIN")) null)
              ]
             [else
                                    
              (let  ([newS (setScenePlayer scene member (+ (getPlayerX (getPlayers (getScenePlayers scene) member)) move) 1)])
                (if (not (checkScene newS))
                    (if (null? (getScenePlayers(deleteScenePlayer newS member)))
                        ;Retorno 0: Todos los player muertos.
                        (cons (lazy (setSceneStatus (deleteScenePlayer newS member) "LOSE")) null)


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
      
              (if (= (length (getSceneEnemies scene)) 1) ;Muerieron todos los enemigos?

                  ;Retorno 3: Murieron todos los enemigos
                  (cons (lazy (deleteSceneEnemy (setSceneStatus scene "WIN") 0)) null)
                  ;Buscamos en qué enemigo fue, y borramos ese enemigo.


                  ;Retorno 4:
                  (cons
                   (lazy (myAppend (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet)))
                                   (tf (createBulletBy (getSceneEnemies (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))))
                                                (- (getSceneE (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))))
                                                   1) 180))))
                                   
                   (playLazyX
                   (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet)))
                   0 0 tf 2 (tf (createBulletBy (getSceneEnemies (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))))
                                                (- (getSceneE (deleteSceneEnemy scene (findXY (listEnemyXY (getSceneEnemies scene)) (getBulletXY bullet))))
                                                   1) 180)))
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
             [(or (find (getSceneEarth scene) (getBulletXY bullet)) (< (getBulletX bullet) 0) (> (getBulletX bullet) (getSceneM scene))
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
                                 (cons (lazy (deleteScenePlayer scene (findXY (listPlayerXY (getScenePlayers scene)) (getBulletXY bullet)))) null)

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
         [(equal? (getBulletXY (get scene 10)) (list ite file))
          "="]
         [(find (listEnemyXY(getSceneEnemies scene)) (list ite file))
          "E"]
         [(find (listPlayerXY (getScenePlayers scene)) (list ite file))
          "P"]
         [else "-"]
          )]
      [else
       (cond
         [(find (getSceneEarth scene) (list ite file))
          (string-append "$" (sceneFileX scene file (+ ite 1)))]
         [(equal? (getBulletXY (get scene 10)) (list ite file))
          (string-append "=" (sceneFileX scene file (+ ite 1)))]
         [(find (listEnemyXY(getSceneEnemies scene)) (list ite file))
          (string-append "E" (sceneFileX scene file (+ ite 1)))]
         [(find (listPlayerXY (getScenePlayers scene)) (list ite file))
          (string-append "P" (sceneFileX scene file (+ ite 1)))]
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
                      "\n" (convX scene (getSceneN scene)))
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
        
      
(define (playDemo scene seed)
  ;(displayScene scene)
  (if (or
       (null? scene)
       (equal? (getSceneStatus scene) "WIN")
       (equal? (getSceneStatus scene) "LOSE")
       (equal? (getSceneStatus scene) "DRAW")
       )
      null
      (append (playLazy scene (modulo seed (getSceneP scene))
                        (modulo seed 5) paraMove 0 (modulo seed 90) 0)
              (playDemo ((((((play scene)
                             (modulo seed (getSceneP scene))
                                         )
                            (modulo seed 5)
                                          )
                           paraMove)
                          (modulo seed 90)
                          )
                         0) (myRandom seed)
                                                                         )
              )
      )
  )

;Definiciones de prueba                       
(define A (deletePlayer (generatePlayer 1)0))
(define B (createEarth 10 10 0))
(define C (setEnemiesX (generateEnemy B 1 0) 0 1))
(define S4 (list "PLAYING" 10 10 1 2 1 0 B A C)) ;Enemigo en primera posición, creado artificialmente

#|              EJEMPLOS         (para verlos, solo sacar los comentarios multilinea asociados)           |#


#|
(displayScene S4)     ;displayScene usa scene->string
((((((play S4)0)0)paraMove)0)0)
(define S5 ((((((play S4)0)0)paraMove)180)0));S5 = S4 pero con el primer jugador disparando al enemigo ubicado atrás.
(displayScene S5)
|#

;Representación normal
#|S2
((((((play S2)2)8)paraMove)40)0)
((((((play ((((((play S2)2)8)paraMove)40)0))1)8)paraMove)0)0)
|#

;Representación con strings
#|
(scene->string S2)
(scene->string ((((((play S2)2)8)paraMove)90)0))
(scene->string ((((((play ((((((play S2)2)8)paraMove)40)0))1)8)paraMove)0)0))
|#


#|
;Narrando una triste historia... 


;Mostrandolo en pantalla:
(displayScene S2)
;En este movimiento se mata a un enemigo
(displayScene  ((((((play S2)2)8)paraMove)40)0))
;En este movimiento se le quita una vida a un player.
(displayScene ((((((play ((((((play S2)2)8)paraMove)40)0))1)8)paraMove)0)0))
;En este movimiento el player que le había quitado una vida, lo acaba matando
(displayScene ((((((play ((((((play ((((((play S2)2)8)paraMove)40)0))1)8)paraMove)0)0))1)0)paraMove)0)0))
;Finalmente, el player que está al borde del mapa se tira al vacío.
(displayScene ((((((play ((((((play ((((((play ((((((play S2)2)8)paraMove)40)0))1)8)paraMove)0)0))1)0)paraMove)0)0))0)-1)paraMove)0)0))
|#

#|
;Ejemplos de playLazy:

(define lazy1 (playLazy S1 0 10 paraMove 0 40 0))
(define lazy2 (playLazy S2 0 3 paraMove 0 40 0))
(define lazy3 (playLazy S3 0 5 paraMove 0 80 0))

;prueba:
(displayAllLazy lazy3) ;Player 0 hace un super disparo pero no era suficiente :(
|#


#|
;Ejemplos de playDemo (Usar con escenas pequeñas, si no, son muchas promesas.
(define promiseDemo1 (playDemo S3 10))
(define promiseDemo2 (playDemo S4 0)) ;Aquí los players se mueren cayendo al precipicio, hacia la derecha.
(define promiseDemo3 (playDemo S3 4123))

;OBS: Es difícil encontrar una partida demo donde los players ganen, ya que
;A diferencia de los enemigos, los players en la demo disparan con ángulo aleatorio, los enemigos
;siempre en 180.
;Probar:
(displayAllLazy promiseDemo1)
|#

