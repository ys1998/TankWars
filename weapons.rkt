#lang racket
(provide place-weapon)
(provide weapon-fired?)
(provide fire-missile
         move-missile)
(provide fire-cannonball
         move-cannonball)
(provide fire-sonicboom
         move-sonicboom)
(provide hit-missile
         hit-cannonball
         hit-sonicboom)

;importing standard libraries
(require 2htdp/image)
(require rsound)

;importing component files
(require "global-data.rkt")

;importing sounds
(define short-cannon-sound (rs-read "sounds/short-cannon.wav"))
(define projectile-sound (rs-read "sounds/projectile.wav"))
(define sonicboom-sound (rs-read "sounds/sonicboom.wav"))
(define blast-sound (rs-read "sounds/blast.wav"))
(define collide-sound (rs-read/clip "sounds/collide.wav" 0 13000))


#| Elements of "player's weapons" |#

;missile elements
(define MISSILE_RANGE 10)
(define missile-blasted #f)
(define missile-body
  (freeze (scale (/ 25 (image-width (bitmap "images/missile.png")))
                 (bitmap "images/missile.png")))) 

;cannonball elements
(define cannonball-body (overlay (bitmap "images/cannonball.png") (circle 25 "solid" (color 0 0 0 0)))) 
(set! cannonball-body (scale (/ 30 (image-height cannonball-body)) cannonball-body))
(define zspeed 0)
(define cannon-blasted #f)

;sonicboom elements
(define SONIC_RANGE 50)
(define sonicboom-body
  (freeze (rotate 180 (bitmap "images/shockwave.png"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Player's Weapons |#

#|1 Short-range missile|#
(define missile
  (WEAPON "missile" missile-body 1 #f 0 0 0 25 90 0 MISSILE_RANGE 10))

#|2 Cannonball (projectile) |#
(define cannonball
  (WEAPON "cannonball" cannonball-body 1 #f 0 0 0 5 0 0 0 30))

#|3 Sonicboom (to push away enemies) |#
(define sonicboom
  (WEAPON "sonicboom" sonicboom-body 1 #f 0 0 0 30 0 0 SONIC_RANGE 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Functions to fire the player's weapons (i.e initiate the weapon) |#
(define (fire-missile player)
  (stop)
  (play short-cannon-sound)
  (set-WEAPON-fired?! missile #t)
  (set! missile-blasted #f)
  (set-WEAPON-x! missile (TANK-x player))
  (set-WEAPON-y! missile (TANK-y player))
  (set-WEAPON-body! missile (rotate (- (TANK-theta-b player) 90) missile-body))
  (set-WEAPON-direction! missile (TANK-theta-b player))
  )

(define (fire-sonicboom player)
  (play sonicboom-sound)
  (set-WEAPON-fired?! sonicboom #t)
  (set-WEAPON-x! sonicboom (TANK-x player))
  (set-WEAPON-y! sonicboom (TANK-y player))
  (set-WEAPON-body! sonicboom (rotate (- (TANK-theta-b player) 90) sonicboom-body))
  (set-WEAPON-direction! sonicboom (TANK-theta-b player))
  )

(define (fire-cannonball player inc power)
  (play projectile-sound)
  (set-WEAPON-fired?! cannonball #t)
  (set-WEAPON-body! cannonball (rotate (- (TANK-theta-t player) 90) cannonball-body))
  (set-WEAPON-scale-factor! cannonball 1)
  (set-WEAPON-x! cannonball (TANK-x player))
  (set-WEAPON-y! cannonball (TANK-y player))
  (set-WEAPON-z! cannonball 20)
  (set-WEAPON-direction! cannonball (TANK-theta-t player))
  (set-WEAPON-inclination! cannonball inc)
  (set-WEAPON-speed! cannonball power)
  (set! cannon-blasted #f)
  (set! zspeed (* power (sin (/ (* pi inc) 180))))
  )

#| Functions to move the player's fired weapons |#
(define (move-missile)
  (if(not missile-blasted)
     (cond [(and (WEAPON-fired? missile) (< 0 (WEAPON-range missile)))
            (begin
              (set-WEAPON-x! missile
                             (+ (WEAPON-x missile) (* (WEAPON-speed missile) (cos (/ (* pi (WEAPON-direction missile)) 180)))))
              (set-WEAPON-y! missile
                             (- (WEAPON-y missile) (* (WEAPON-speed missile) (sin (/ (* pi (WEAPON-direction missile)) 180)))))
              (set-WEAPON-range! missile (- (WEAPON-range missile) 1))
              )]
           [(WEAPON-fired? missile) (set! missile-blasted #t)])
     (begin
       (add-explosion (blast (WEAPON-x missile) (WEAPON-y missile) 1 255 1))
       (set-WEAPON-fired?! missile #f)
       (set-WEAPON-range! missile MISSILE_RANGE)
       (set! missile-blasted #f))
     )
  )

(define (move-sonicboom)
  (cond [(and (WEAPON-fired? sonicboom) (< 0 (WEAPON-range sonicboom)))
         (begin
           (set-WEAPON-x! sonicboom
                          (+ (WEAPON-x sonicboom) (* (WEAPON-speed sonicboom) (cos (/ (* pi (WEAPON-direction sonicboom)) 180)))))
           (set-WEAPON-y! sonicboom
                          (- (WEAPON-y sonicboom) (* (WEAPON-speed sonicboom) (sin (/ (* pi (WEAPON-direction sonicboom)) 180)))))
           (set-WEAPON-range! sonicboom (- (WEAPON-range sonicboom) 1))
           )]
        
        [else (set-WEAPON-fired?! sonicboom #f)
              (set-WEAPON-range! sonicboom SONIC_RANGE)]
        ))

(define (move-cannonball)
  (cond [(and (WEAPON-fired? cannonball) (<= 0 (WEAPON-z cannonball)))
         (begin
           (set-WEAPON-x! cannonball
                          (+ (WEAPON-x cannonball) (* (WEAPON-speed cannonball) (cos (/ (* pi (WEAPON-direction cannonball)) 180)) (cos (/ (* pi (WEAPON-inclination cannonball)) 180)))))
           (set-WEAPON-y! cannonball
                          (- (WEAPON-y cannonball) (* (WEAPON-speed cannonball) (cos (/ (* pi (WEAPON-inclination cannonball)) 180)) (sin (/ (* pi (WEAPON-direction cannonball)) 180)))))
           (set-WEAPON-z! cannonball (+ zspeed (WEAPON-z cannonball)))
           (set! zspeed (- zspeed 1))
           (set-WEAPON-body! cannonball (rotate 15 (WEAPON-body cannonball)))
           (cond [(>= zspeed 0) (set-WEAPON-scale-factor! cannonball (+ (WEAPON-scale-factor cannonball) 0.06))]
                 [else (set-WEAPON-scale-factor! cannonball (- (WEAPON-scale-factor cannonball) 0.06))]
                 ))]
        [(and (WEAPON-fired? cannonball) (< (WEAPON-z cannonball) 0) (not cannon-blasted))
         (begin
           (stop) (play blast-sound)
           (add-explosion (blast (WEAPON-x cannonball) (WEAPON-y cannonball) 2 255 1))
           (set-WEAPON-scale-factor! cannonball 1)
           (set-WEAPON-z! cannonball (- (WEAPON-z cannonball) 1))
           (set! cannon-blasted #t)
           (set-WEAPON-fired?! cannonball #f))
         ]
        
        )
  )
       
  

#| Function to place the given weapon on a given scene |#
(define (place-weapon w scn)
  (cond [(equal? w "missile") (set! w missile)]
        [(equal? w "cannonball") (set! w cannonball)]
        [(equal? w "sonicboom") (set! w sonicboom)]
        )
  (if(and (WEAPON? w) (WEAPON-fired? w))
     (place-image (scale (WEAPON-scale-factor w) (WEAPON-body w))
                  (WEAPON-x w) (WEAPON-y w)
                  scn
                  )
     scn)
  )

#| Function to return whether a given weapon is fired or not |#
(define (weapon-fired? w)
  (cond [(equal? w "missile") (WEAPON-fired? missile)]
        [(equal? w "cannonball") (WEAPON-fired? cannonball)]
        [(equal? w "sonicboom") (WEAPON-fired? sonicboom)]
        [else #f])
  )
  

#| Functions to check whether a given enemy has been hit by a weapon or not |#

;collision with missile
(define (hit-missile e)
  (cond [(wanderer? e)
         (if(and (weapon-fired? "missile") (> (wanderer-radius e) (sqrt (+ (expt (- (wanderer-x e) (WEAPON-x missile)) 2)
                                                                           (expt (- (wanderer-y e) (WEAPON-y missile)) 2)))))
            (begin
              (play collide-sound)
              (set! missile-blasted #t)
              (set-wanderer-health! e (- (wanderer-health e) (WEAPON-damage missile)))
              (if(<= (wanderer-health e) 0)
                 (begin
                   (stop)
                   (play blast-sound)
                   )
                 (set-wanderer-body! e (wanderer-body e)))
              e)
            e)]
        [(tower? e) ;towers are not affected by missiles
         (if(and (weapon-fired? "missile")
                 (> 40 (sqrt (+ (expt (- (tower-x e) (WEAPON-x missile)) 2)
                                (expt (- (tower-y e) (WEAPON-y missile)) 2)))))
            (begin 
              (set! missile-blasted #t)
              e)
            e)]
        [(destroyer? e)
         (if(and (weapon-fired? "missile") (> 30 (sqrt (+ (expt (- (destroyer-x e) (WEAPON-x missile)) 2)
                                                          (expt (- (destroyer-y e) (WEAPON-y missile)) 2)))))
            (begin
              (stop)
              (play collide-sound)
              (set! missile-blasted #t)
              (set-destroyer-health! e (- (destroyer-health e) (WEAPON-damage missile)))
              (if(<= (destroyer-health e) 0) (play blast-sound) (set-destroyer-body! e (destroyer-body e)))
              e)
            e)]
        ))

;collision with cannonball
(define (hit-cannonball e)
  (cond [(wanderer? e)
         (if(and (weapon-fired? "cannonball")
                 (> 0 zspeed)
                 (> 10 (WEAPON-z cannonball))
                 (> (wanderer-radius e) (sqrt (+ (expt (- (wanderer-x e) (WEAPON-x cannonball)) 2)
                                                 (expt (- (wanderer-y e) (WEAPON-y cannonball)) 2)))))
            (begin
              (set! cannon-blasted #t)
              (set-WEAPON-fired?! cannonball #f)
              (stop) (play blast-sound)
              (add-explosion (blast (WEAPON-x cannonball)
                                    (WEAPON-y cannonball)
                                    2 255 0.3))
              (set-wanderer-health! e (- (wanderer-health e) (WEAPON-damage cannonball)))
              (if(<= (wanderer-health e) 0) (play blast-sound) (set-wanderer-body! e (wanderer-body e)))
              e)
            e)]
        [(tower? e)
         (if(and (weapon-fired? "cannonball")
                 (> 40 (sqrt (+ (expt (- (tower-x e) (WEAPON-x cannonball)) 2)
                                (expt (- (tower-y e) (WEAPON-y cannonball)) 2))))
                 (> 0 zspeed)
                 (> 10 (WEAPON-z cannonball)))
            (begin 
              (set! cannon-blasted #t)
              (set-WEAPON-fired?! cannonball #f)
              (stop) (play blast-sound)
              (add-explosion (blast (WEAPON-x cannonball)
                                    (WEAPON-y cannonball)
                                    2 255 0.3))
              (set-tower-health! e (- (tower-health e) (WEAPON-damage cannonball)))
              (if(<= (tower-health e) 0) (play blast-sound) (set-tower-body! e (tower-body e)))
              e)
            e)]
        [(destroyer? e)
         (if(and (weapon-fired? "cannonball")
                 (> 30 (sqrt (+ (expt (- (destroyer-x e) (WEAPON-x cannonball)) 2)
                                (expt (- (destroyer-y e) (WEAPON-y cannonball)) 2))))
                 (> 0 zspeed)
                 (> 10 (WEAPON-z cannonball)))
            (begin
              (set! cannon-blasted #t)
              (set-WEAPON-fired?! cannonball #f)
              (stop) (play blast-sound)
              (add-explosion (blast (WEAPON-x cannonball)
                                    (WEAPON-y cannonball)
                                    2 255 0.3))
              (set-destroyer-health! e (- (destroyer-health e) (WEAPON-damage cannonball)))
              (if(<= (destroyer-health e) 0) (play blast-sound) (set-destroyer-body! e (destroyer-body e)))
              e)
            e)]
        ))

;collision with sonicboom
(define (hit-sonicboom e)
  (cond [(wanderer? e)
         (if(and (weapon-fired? "sonicboom")
                 (> (wanderer-radius e) (sqrt (+ (expt (- (wanderer-x e) (WEAPON-x sonicboom)) 2)
                                                 (expt (- (wanderer-y e) (WEAPON-y sonicboom)) 2)))))
            (begin
              (let [(t (- (* 5 (random 37)) 90))]
                (set-WEAPON-direction! sonicboom (+ (WEAPON-direction sonicboom) t)) ;to produce deflection effect
                (set-WEAPON-body! sonicboom (rotate t (WEAPON-body sonicboom)))
                )
              (set-wanderer-health! e (- (wanderer-health e) (WEAPON-damage sonicboom)))
              (set-wanderer-x! e (- (wanderer-x e) (* 35 (cos (/ (* pi (wanderer-direction e)) 180)))))
              (set-wanderer-y! e (+ (wanderer-y e) (* 35 (sin (/ (* pi (wanderer-direction e)) 180)))))
              (if(<= (wanderer-health e) 0) (play blast-sound) (set-wanderer-body! e (wanderer-body e)))
              e)
            e)]
        [(tower? e)
         (if(and (weapon-fired? "sonicboom")
                 (> 40 (sqrt (+ (expt (- (tower-x e) (WEAPON-x sonicboom)) 2)
                                (expt (- (tower-y e) (WEAPON-y sonicboom)) 2)))))
            (begin
              (let [(t (- (* 5 (random 37)) 90))]
                (set-WEAPON-direction! sonicboom (+ (WEAPON-direction sonicboom) t)) ;to produce deflection effect
                (set-WEAPON-body! sonicboom (rotate t (WEAPON-body sonicboom)))
                )
              (if(<= (tower-health e) 0) (play blast-sound) (set-tower-body! e (tower-body e)))
              e)
            e)]
        [(destroyer? e)
         (if(and (weapon-fired? "sonicboom")
                 (> 30 (sqrt (+ (expt (- (destroyer-x e) (WEAPON-x sonicboom)) 2)
                                (expt (- (destroyer-y e) (WEAPON-y sonicboom)) 2)))))
            (begin
              (let [(t (- (* 5 (random 37)) 90))]
                (set-WEAPON-direction! sonicboom (+ (WEAPON-direction sonicboom) t)) ;to produce deflection effect
                (set-WEAPON-body! sonicboom (rotate t (WEAPON-body sonicboom)))
                )
              (set-destroyer-health! e (- (destroyer-health e) (WEAPON-damage sonicboom)))
              (set-destroyer-x! e (- (destroyer-x e) (* 15 (cos (/ (* pi (destroyer-direction e)) 180)))))
              (set-destroyer-y! e (+ (destroyer-y e) (* 15 (sin (/ (* pi (destroyer-direction e)) 180)))))
              (if(<= (destroyer-health e) 0) (play blast-sound) (set-destroyer-body! e (destroyer-body e)))
              e)
            e)]
              
        ))
              
              
