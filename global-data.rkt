#lang racket
(provide (struct-out TANK))
(provide (struct-out WEAPON))
(provide (struct-out wanderer)
         sample-wanderer
         distance-wanderer)
(provide (struct-out tower)
         sample-tower
         align-cannon
         distance-b-player)
(provide (struct-out destroyer)
         sample-destroyer
         distance-flame-ring
         create-destroyer-body)
(provide  place-explosions
          add-explosion
          update-explosion-list
          clear-explosions-list
          (struct-out blast))


;importing standard libraries
(require 2htdp/image)
         

;structure representing the player
(struct TANK ([body #:mutable]
              [health #:mutable]
              [x #:mutable]
              [y #:mutable]
              [speed #:mutable]
              [theta-b #:mutable]
              [theta-circle #:mutable]
              [theta-t #:mutable]
              [inclination #:mutable]
              [power #:mutable]
              )
  #:transparent
  )

;;;;;;;;;;;;;;;;;;;;;;;;;Explosion Components;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;importing images
(define blast1 (freeze (scale 0.15 (bitmap "images/blast.png"))))
(define blast2 (freeze (scale 0.15 (bitmap "images/blast1.png"))))
(define blast3 (freeze (scale 0.15 (bitmap "images/blast2.png"))))

(define EXPLOSION_LIST '())
(struct blast (x y type [alpha #:mutable] [scale #:mutable]) #:transparent)

#| Associated functions |#

;function to clear explosion list
(define (clear-explosions-list) (set! EXPLOSION_LIST '()))

;function to add an explosion to the explosion list
(define (add-explosion blast)
  (cond[(blast? blast) (set! EXPLOSION_LIST (cons blast EXPLOSION_LIST))]))

;function to update the explosion list
(define (update-explosion-list)
  (define (g x)
    (begin (set-blast-alpha! x (- (blast-alpha x) 10))
           (set-blast-scale! x (+ 0.1 (blast-scale x)))
           x))
  (define (f x y)
    (if(< 10 (blast-alpha x))
       (cons (g x) y)
       y))
  (let [(temp (foldr f '() EXPLOSION_LIST))]
    (set! EXPLOSION_LIST temp)))

;function to place all the explosions on the screen
(define (place-explosions scn)
  (define (f data y)
    (cond [(= 1 (blast-type data)) (place-image (scale (blast-scale data)
                                                       (set-alpha blast1 (blast-alpha data)))
                                                (blast-x data) (blast-y data) y)]
          [(= 2 (blast-type data)) (place-image (scale (blast-scale data)
                                                       (set-alpha blast2 (blast-alpha data)))
                                                (blast-x data) (blast-y data) y)]
          [(= 3 (blast-type data)) (place-image (scale (blast-scale data)
                                                       (set-alpha blast3 (blast-alpha data)))
                                                (blast-x data) (blast-y data) y)]))
  (foldr f scn EXPLOSION_LIST))

;function to set the alpha value of a given image
(define (set-alpha img alpha)
  (let [(color-list (image->color-list img))]
    (color-list->bitmap (map (lambda (c)
                               (if(and (= (color-red c) 255) (= (color-green c) 255) (= (color-green c) 255))
                                  (make-color 255 255 255 0)
                                  (make-color (color-red c) (color-green c) (color-blue c) alpha)))
                             color-list)
                        (image-width img)
                        (image-height img))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Structure representing all the weapons in the game |#
(struct WEAPON (id   ;weapon's id - useful for assigning damage and consequent action
                [body #:mutable]
                [scale-factor #:mutable]         ;for out-of-plane graphics
                [fired? #:mutable]               ;stores #t or #f
                [x #:mutable]
                [y #:mutable]
                [z #:mutable]                    ;direction out of the plane
                [speed #:mutable]
                [direction #:mutable]            ;angle in plane
                [inclination #:mutable]          ;angle made with plane
                [range #:mutable]
                damage
                ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                            

#| ENEMIES DESCRIPTION |#
;1-Wanderer : roams around randomly, follows player if within its range, attacks on contact
;2-Anti-tank tower : can be destroyed only using cannon, medium deadly, fast missiles
;3-Destroyer :  slow but deadly, has high health, flamethrower-weapon 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| WANDERER COMPONENTS |#

;structure
(struct wanderer ([body #:mutable]
                  [x #:mutable]
                  [y #:mutable]
                  [health #:mutable]
                  [direction #:mutable]
                  [speed #:mutable]
                  radius
                  range
                  damage
                  [moving #:mutable]
                  [chasing #:mutable]
                  [steps #:mutable]
                  )
  #:transparent
  )

;body
(define wand-img (bitmap "images/wanderer.png"))
(define wand-body (freeze (underlay (circle 35 "solid" (color 0 0 0 0))
                                    (scale (/ 70 (image-height wand-img)) wand-img))))
;sample-model
(define sample-wanderer (wanderer wand-body 0 0 50 0 0 35 190 5 #f #t 30))


;function to calculate the distance between player and wanderer
(define (distance-wanderer w player)
  (sqrt (+ (expt (- (TANK-x player) (wanderer-x w)) 2) (expt (- (TANK-y player) (wanderer-y w)) 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| GUARD TOWER COMPONENTS |#

;structure
(struct tower ([body #:mutable]
               [bullet-body #:mutable]
               [health #:mutable]
               damage
               x
               y
               [bx #:mutable]
               [by #:mutable] 
               [direction #:mutable]
               [bdirection #:mutable]
               [delay #:mutable]
               )
  #:transparent
  )

;other components
(define MAX_DELAY 100)

;tower body
(define t-body (bitmap "images/tower.png"))
(set! t-body (scale (/ 80 (image-height t-body)) t-body))
;cannon body
(define t-cannon (bitmap "images/tower-cannon.png"))
(set! t-cannon (scale (/ 80 (image-height t-cannon)) t-cannon))
;final tower-body
(define t-body-final (clear-pinhole
                      (overlay/pinhole (overlay/pinhole (put-pinhole (* 0.5 (image-width t-cannon))
                                                                     (- (image-height t-cannon) 16)
                                                                     t-cannon)
                                                        (circle 10 "solid" (color 0 0 0 0)))
                                       t-body)))
                                                            
;bullet body
(define b-body
  (freeze (bitmap "images/energyball.gif"))
  ;(overlay (circle 3 "solid" "white")
  ;         (circle 5 "solid" "red"))
  )

;sample model
(define sample-tower
  (tower t-body-final b-body 150 15 0 0 0 0 0 0 MAX_DELAY))

;function to place the cannon along the tower's direction
(define (align-cannon t)
  (clear-pinhole
   (overlay/pinhole (rotate (- (tower-direction t) 90)
                            (overlay/pinhole (put-pinhole (* 0.5 (image-width t-cannon))
                                                          (- (image-height t-cannon) 16)
                                                          t-cannon)
                                             (circle 10 "solid" (color 0 0 0 0))))
                    t-body))
  )
;function to calculate distance between tower's bullet and player
(define (distance-b-player t player)
  (sqrt (+ (expt (- (TANK-x player) (tower-bx t)) 2)
           (expt (- (TANK-y player) (tower-by t)) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| DESTROYER COMPONENTS |#

;structure
(struct destroyer ([body #:mutable]
                   [x #:mutable]
                   [y #:mutable]
                   [direction #:mutable]
                   [health #:mutable]
                   damage
                   [flame-radius #:mutable]
                   [delay #:mutable]
                   [firing #:mutable] ;stores #t or #f indicating whether the destroyer is firing or not
                   )
  #:transparent
  )

;body
(define d-body (bitmap "images/destroyer.png"))
(set! d-body (scale (/ 80 (image-height d-body)) d-body))

;flame-element
(define flame (bitmap "images/flame.png"))
(set! flame (freeze (scale (/ 40 (image-width flame)) flame)))

;function to create the flame-ring given its radius
(define (create-flame-ring r)
  (create-flame-helper 6 0 r))

(define (create-flame-helper n cntr r)
  (if(= cntr n) (circle r "solid" (color 255 200 0 50))
     (overlay/align "left" "middle" flame 
                    (rotate (/ 360 n) (create-flame-helper n (+ cntr 1) r)))))

;function to create destroyer's body given the destroyer's instance
(define (create-destroyer-body d)
  (cond [(< 0 (destroyer-flame-radius d)) 
         (overlay (rotate (- (destroyer-direction d) 90) d-body)
                  (create-flame-ring (destroyer-flame-radius d)))]
        [else (rotate (- (destroyer-direction d) 90) d-body)]))

;function to find distance between destroyer and player
(define (distance-flame-ring d player)
  (sqrt (+ (expt (- (TANK-x player) (destroyer-x d)) 2)
           (expt (- (TANK-y player) (destroyer-y d)) 2))))

;sample-model
(define sample-destroyer
  (destroyer (overlay d-body (create-flame-ring 0)) 0 0 90 100 3 0 150 #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
