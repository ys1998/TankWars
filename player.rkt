#lang racket
(provide player
         get-health
         check-player-dead)
(provide change-player
         create-player)
(provide move-player)
(provide rotate-player
         brake-player
         accelerate-player)
(provide rotate-turret)
(provide fire-weapon)
(provide place-player)


;importing standard packages
(require 2htdp/image)

;importing component files
(require "weapons.rkt")
(require "maps.rkt")
(require "global-data.rkt")
(require "game-gui.rkt")


;player's tank elements
(define DEFAULT_POWER 20)
(define DEFAULT_INC 30)
(define DEFAULT_THETA 90)
(define MAX_SPEED 3)
(define MIN_SPEED 0)
(define ACC 0.2)
(define BRAKE 0.1)
(define RADIUS 65)

;structure representing the player's tank is imported from global-data.rkt


;tank body elements
(define t-body (bitmap "images/tank.png"))
(define t-turret (rotate 180 (bitmap "images/turret.png")))
(set! t-body (put-pinhole 45 60 (scale/xy (/ 90 (image-width t-body)) (/ 90 (image-height t-body)) t-body)))
(set! t-turret (overlay/pinhole
                (put-pinhole 11.25 15 (scale/xy (/ 22.5 (image-width t-turret)) (/ 45 (image-height t-turret)) t-turret)) (circle 22.5 "solid" (color 0 0 0 0))))

;tank-body
(define aura-circle (freeze (scale (/ 200 (image-width (bitmap "images/circle.png")))
                                   (bitmap "images/circle.png"))))
(define tank-body (clear-pinhole (overlay/pinhole t-turret t-body)))

;player definition
(define player (TANK (scale 0.75 tank-body)
                     100
                     (- 900 RADIUS)
                     (- 600 RADIUS)
                     0
                     DEFAULT_THETA
                     0
                     (+ 180 DEFAULT_THETA)
                     DEFAULT_INC
                     DEFAULT_POWER
                     )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;FUNCTIONS ASSOCIATED WITH THE PLAYER;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|1|#
;function to update the player's position according to current speed and direction
(define (move-player)
  (begin
    (cond [(> (TANK-x player) 900) (set-TANK-x! player 899)]
          [(< (TANK-x player) 50) (set-TANK-x! player 51)]
          [(> (TANK-y player) 600) (set-TANK-y! player 599)]
          [(< (TANK-y player) 50) (set-TANK-y! player 51)])
    (update-max-speed)
    (cond [(> (TANK-speed player) MAX_SPEED) (set-TANK-speed! player MAX_SPEED)]
          [(< (TANK-speed player) MIN_SPEED) (set-TANK-speed! player MIN_SPEED)]
          )
    (set-TANK-body! player (scale 0.75 (overlay
                                        (clear-pinhole (overlay/pinhole t-turret (rotate (- (TANK-theta-b player) 90) t-body)))
                                        (rotate (TANK-theta-circle player) aura-circle)
                                        )))
    (set-TANK-theta-circle! player (remainder (+ 1 (TANK-theta-circle player)) 360))
    (set-TANK-x! player (+ (TANK-x player) (* (TANK-speed player) (cos (/ (* pi (TANK-theta-b player)) 180)))))
    (set-TANK-y! player (- (TANK-y player) (* (TANK-speed player) (sin (/ (* pi (TANK-theta-b player)) 180)))))
  
    ))

#|1.1|#
;function to be invoked when player dies
(define (check-player-dead)
  (< (TANK-health player) 0))
  

#|2.1|#
;functions to change the player's direction of motion
(define (rotate-player dir)
  (cond [(equal? dir "left") (turn-left-player)]
        [(equal? dir "right") (turn-right-player)])
  )
(define (turn-left-player)
  (accelerate-player)
  (set-TANK-theta-b! player (+ (TANK-theta-b player) 5))
  (set-TANK-theta-t! player (+ (TANK-theta-t player) 5))
  (set-TANK-body! player (rotate 5 (TANK-body player)))
  (set! t-turret (rotate 5 t-turret))
  )
(define (turn-right-player)
  (accelerate-player)
  (set-TANK-theta-b! player (- (TANK-theta-b player) 5))
  (set-TANK-theta-t! player (- (TANK-theta-t player) 5))
  (set-TANK-body! player (rotate -5 (TANK-body player)))
  (set! t-turret (rotate -5 t-turret))
  )

#|2.2|#
;function to change the direction of player's rear cannon
(define (rotate-turret dir)
  (cond [(equal? dir "left") (turn-left-turret)]
        [(equal? dir "right") (turn-right-turret)])
  )
(define (turn-left-turret)
  (set-TANK-theta-t! player (+ (TANK-theta-t player) 5))
  (set! t-turret (rotate 5 t-turret))
  (set-TANK-body! player
                  (scale 0.75 (overlay
                               (clear-pinhole (overlay/pinhole t-turret (rotate (- (TANK-theta-b player) 90) t-body)))
                               (rotate (TANK-theta-circle player) aura-circle)
                               )))
  )
(define (turn-right-turret)
  (set-TANK-theta-t! player (- (TANK-theta-t player) 5))
  (set! t-turret (rotate -5 t-turret))
  (set-TANK-body! player
                  (scale 0.75 (overlay
                               (clear-pinhole (overlay/pinhole t-turret (rotate (- (TANK-theta-b player) 90) t-body)))
                               (rotate (TANK-theta-circle player) aura-circle)
                               )))
  )

#|3|#
;functions to change the player's speed
(define (brake-player)
  (set-TANK-speed! player (- (TANK-speed player) BRAKE))
  )
(define (accelerate-player)
  (set-TANK-speed! player (+ (TANK-speed player) ACC))
  )

#|4|#
;function to update the player's max speed depending on the terrain
(define (update-max-speed)
  (set! MAX_SPEED (get-max-speed (TANK-x player) (TANK-y player) (get-map-no)))
  )

#|5|#
;function to fire a weapon depending upon the argument passed
(define (fire-weapon type)
  (cond [(equal? type "missile") (fire-missile player)]
        [(equal? type "sonicboom") (fire-sonicboom player)]
        [(equal? type "cannon")
         (let* [(selection (set-parameters))
                (fired? (car selection))
                (power (cadr selection))
                (inc (cddr selection))]
           (cond [fired? (fire-cannonball player inc power)]))]
        ))

#|6|#
;function to place the player on a given scene
(define (place-player scn)
  (place-image (TANK-body player)
               (TANK-x player) (TANK-y player)
               scn)
  )

#|7|#
;functions to change player's state variables
(define (change-player p)
  (cond [(TANK? p) (set! player p)]))
(define (change-health-player h)
  (set-TANK-health! player h))
  
#|8|#
;function to return player's health
(define (get-health)
  (TANK-health player))

#|9|#
;function to create a new player
(define (create-player)
  (set! t-body (bitmap "images/tank.png"))
  (set! t-turret (rotate 180 (bitmap "images/turret.png")))
  (set! t-body (put-pinhole 45 60 (scale/xy (/ 90 (image-width t-body)) (/ 90 (image-height t-body)) t-body)))
  (set! t-turret (overlay/pinhole
                  (put-pinhole 11.25 15 (scale/xy (/ 22.5 (image-width t-turret))
                                                  (/ 45 (image-height t-turret))
                                                  t-turret))
                  (circle 22.5 "solid" (color 0 0 0 0))))
  (change-player (TANK (scale 0.75 tank-body)
                       100
                       (- 900 RADIUS)
                       (- 600 RADIUS)
                       0
                       DEFAULT_THETA
                       0
                       (+ 180 DEFAULT_THETA)
                       DEFAULT_INC
                       DEFAULT_POWER
                       )))
