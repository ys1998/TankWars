#lang racket
(provide start)

;importing general libraries
(require 2htdp/image
         2htdp/universe)
(require rsound)

;importing component files
(require "maps.rkt")
(require "player.rkt")
(require "weapons.rkt")
(require "game-stage.rkt")
(require "enemies.rkt")
(require "game-gui.rkt")
(require "global-data.rkt")


(define MAP_NO 1)
(define gameover? #f)

;function to place all components of the game on the screen
(define (place-all-components t)
  (cond [(null? (get-enemy-list)) ;checks if all enemies have been killed
         (begin
           (stop)
           (play (rs-read "sounds/intro-music.wav"))
           (sleep 4)
           #f
           
           )]
        [(check-player-dead)  ;checks if player is dead
         (begin
           (stop)
           (play (rs-read "sounds/intro-music.wav"))
           (sleep 4)
           #f
        
           )]
        [gameover? (get-response)]
        [else (place-image empty-image 0 0
                           (place-weapon "cannonball"
                                         (place-explosions
                                          (place-all-enemies
                                           (place-player
                                            (place-weapon "missile"
                                                          (place-weapon "sonicboom"
                                                                        (main-frame (game-map MAP_NO)))))))))]))



;Function to handle key-events
(define (player-key-handler world key-pressed)
  (begin 
    (cond [(key=? key-pressed "left") (rotate-player "left") (move-player)]
          [(key=? key-pressed "right") (rotate-player "right") (move-player)]
          [(key=? key-pressed "up") (accelerate-player) (move-player)]
          [(key=? key-pressed "down") (brake-player) (move-player)]
          [(key=? key-pressed "a") (rotate-turret "left")]
          [(key=? key-pressed "d") (rotate-turret "right")]
          [(key=? key-pressed "rcontrol")
           (cond [(not (weapon-fired? "missile")) (fire-weapon "missile")])]
          [(key=? key-pressed "control")
           (cond [(not (weapon-fired? "cannon")) (fire-weapon "cannon")])]
          [(key=? key-pressed "rshift")
           (cond [(not (weapon-fired? "sonicboom")) (fire-weapon "sonicboom")])]
          
          )
    (place-all-components 0)
    )
  )

;function to handle idle events - i.e. when key is not pressed
(define (player-key-not-pressed t)
  (begin
    ;check-player-dead was here initially
    (brake-player) (move-player)                         ;friction effect
    (update-player-and-list)                             ;updating player's state
    (move-all-enemies player)                            ;executing enemies' AI  
    (move-cannonball) (move-missile) (move-sonicboom)    ;moving player's fired weapons
    (update-explosion-list)                              ;updating the explosions
    (update-enemy-list player)                           ;updating enemies' state
    (place-all-components 0)                             ;placing all components on scene
    )
  )

;function to print the concluding message
(define gameover (scale (/ 500 (image-width (bitmap "images/gameover.png")))
                        (bitmap "images/gameover.png")))
(define mapclear (scale (/ 500 (image-width (bitmap "images/winner.png")))
                        (bitmap "images/winner.png")))
(define (conclusion world)
  (cond [(and (not gameover?) (null? (get-enemy-list)))
         (stop)
         (set! gameover? #t)
         (overlay/align "middle" "middle"
                        (above mapclear
                               (text "Hope you enjoyed playing!" 36 "blue"))
                        (empty-scene 960 660 "white" ))]           
        [(and (not gameover?) (check-player-dead))
         (stop)
         (set! gameover? #t)
         (overlay/align "middle" "middle"
                        (above gameover
                               (text "Better luck next time!" 36 "blue")) 
                        (empty-scene 960 660 "white" ))]
      
        )
  )
     
;function to initiate the game
(define (start st)
  (set! gameover? #f)
  (clear-explosions-list)
  (play (rs-read "sounds/intro-music.wav"))
  (send f2 show #t)
  (set! MAP_NO (get-map-no))
  (create-enemies (get-enemy-set))  ;generating the enemies list
  (create-player)                   ;initialize the player's health
  (big-bang st
            (to-draw place-all-components)
            (on-key player-key-handler)
            (on-tick player-key-not-pressed 0.05) ;executed after every 0.05s
            (name "Tank Wars --- By Yash Shah")
            (stop-when (lambda (w) (equal? w #f)) conclusion)
            (close-on-stop 2)
            )
  (displayln "\nGame-play complete.\nAwaiting response...")
  (if(equal? 'yes (get-response))
     (begin (displayln "Response : Continue playing.") (start 0))
     (displayln "Response : Quit.\nThanks for playing!"))
  )
  
(start 0)

