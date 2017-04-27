#lang racket
(provide update-player-and-list)
(provide create-enemies
         place-all-enemies
         move-all-enemies
         get-enemy-list)
(provide update-enemy-list)
(provide type-list-1
         type-list-2
         type-list-3)

;importing standard libraries
(require 2htdp/image)
(require rsound)

;importing component files
(require "global-data.rkt")
(require "weapons.rkt")
(require "player.rkt")

;importing sounds
(define wanderer-sound (rs-read "sounds/wanderer.wav"))
(define tower-sound (rs-read "sounds/guard-tower.wav"))
(define destroyer-sound (rs-read/clip "sounds/destroyer.wav" 5000 10000))


#| Different combinations of enemies for the game |#
(define type-list-0 (list "wanderer" "destroyer" "tower"))
(define type-list-1 (list "wanderer" "wanderer" "wanderer" "wanderer" "wanderer"))
(define type-list-2 (list "tower" "wanderer" "wanderer" "tower"))
(define type-list-3 (list "destroyer" "destroyer" "wanderer"))
(define type-list-4 (list "destroyer" "tower" "tower" "wanderer"))
(define type-list-5 (list "destroyer" "destroyer" "tower" "tower" "tower"))


(define ENEMY-LIST '()) ;list storing all the active enemies of the game
(define (get-enemy-list) ENEMY-LIST)

        
;function to add enemies to the ENEMY-LIST
(define (create-enemies enemy-set)
  (cond [(= enemy-set 0) (add-enemies type-list-0)]
        [(= enemy-set 1) (add-enemies type-list-1)]
        [(= enemy-set 2) (add-enemies type-list-2)]
        [(= enemy-set 3) (add-enemies type-list-3)]
        [(= enemy-set 4) (add-enemies type-list-4)]
        [(= enemy-set 5) (add-enemies type-list-5)]
        ))

(define (add-enemies type-list)
  (set! ENEMY-LIST (add-enemies-helper type-list)))

(define (add-enemies-helper type-list)
  (define (f x y)
    (let [(enemy (cond [(equal? x "wanderer")
                        (struct-copy wanderer sample-wanderer [x (random 900)] [y (random 600)])]
                       [(equal? x "destroyer")
                        (struct-copy destroyer sample-destroyer [x (+ 20 (random 300))] [y (+ 20 (random 300))])]
                       [(equal? x "tower")
                        (struct-copy tower sample-tower [x (+ 20 (random 500))] [y (+ 20 (random 560))])]))]
      (cons enemy y)))
  (foldr f '() type-list))

;function to place all enemies belonging to ENEMY-LIST on a given scene
(define (place-all-enemies scn)
  (place-all-enemies-helper ENEMY-LIST scn))
(define (place-all-enemies-helper l scn)
  (if(null? l) scn
     (cond
       [(wanderer? (car l)) (place-wanderer (car l) (place-all-enemies-helper (cdr l) scn))]
       [(tower? (car l)) (place-tower (car l) (place-all-enemies-helper (cdr l) scn))]
       [(destroyer? (car l)) (place-destroyer (car l) (place-all-enemies-helper (cdr l) scn))]
       )))
  

;function to move all enemies belonging to ENEMY-list
(define (move-all-enemies player)
  (set! ENEMY-LIST (move-all-enemies-helper ENEMY-LIST player)))
(define (move-all-enemies-helper l player)
  (if(null? l) '()
     (cond [(wanderer? (car l))
            (cons (move-wanderer (car l) player) (move-all-enemies-helper (cdr l) player))]
           [(tower? (car l))
            (cons (move-tower (car l) player) (move-all-enemies-helper (cdr l) player))]
           [(destroyer? (car l))
            (cons (move-destroyer (car l) player) (move-all-enemies-helper (cdr l) player))]
           )))

#| Function to update the enemy list by checking for weapon-hits |#

(define (update-enemy-list p)
  (set! ENEMY-LIST (update-enemy-list-helper ENEMY-LIST p)))
(define (update-enemy-list-helper l p)
  (if(null? l) '()
     (cond [(<= (health (car l)) 0)
            (begin
              (cond [(wanderer? (car l)) (add-explosion (blast (wanderer-x (car l))
                                                               (wanderer-y (car l))
                                                               3 255 0.3))]
                    [(tower? (car l)) (add-explosion (blast (tower-x (car l))
                                                            (tower-y (car l))
                                                            3 255 0.3))]
                    [(destroyer? (car l)) (add-explosion (blast (destroyer-x (car l))
                                                                (destroyer-y (car l))
                                                                3 255 0.3))])
              (update-enemy-list-helper (cdr l) p))]
           [else (cons (check-and-update (car l)) (update-enemy-list-helper (cdr l) p))]
           )
     )
  )

;function to find the health of the given enemy
(define (health e)
  (cond [(wanderer? e) (wanderer-health e)]
        [(tower? e) (tower-health e)]
        [(destroyer? e) (destroyer-health e)]
        ))

;function to return the updated enemy if it doesn't hit the player
(define (check-and-update e)
  (hit-sonicboom (hit-missile (hit-cannonball e)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| Functions to move the enemies (the AI part!) |#

;wanderer
(define (move-wanderer w player)
  (begin
    (cond [(> (wanderer-x w) 900) (set-wanderer-x! w 900)]
          [(< (wanderer-x w) 0) (set-wanderer-x! w 0)]
          [(> (wanderer-y w) 600) (set-wanderer-y! w 600)]
          [(< (wanderer-y w) 0) (set-wanderer-y! w 0)])
          
    (cond [(<= (distance-wanderer w player) (wanderer-range w))
           (if(wanderer-chasing w)
              ;when wanderer is chasing the player
              (begin
                (set-wanderer-direction! w (/ (* 180 (atan (- (wanderer-y w) (TANK-y player))
                                                           (- (TANK-x player) (wanderer-x w)))) pi))
                (set-wanderer-body! w (rotate 15 (wanderer-body w)))
                (set-wanderer-x! w (+ (wanderer-x w) (* 4 (cos (/ (* pi (wanderer-direction w)) 180)))))
                (set-wanderer-y! w (- (wanderer-y w) (* 4 (sin (/ (* pi (wanderer-direction w)) 180)))))
                w)
              ;when wanderer is close to player but not chasing it
              (begin
                (set-wanderer-body! w (rotate 15 (wanderer-body w)))
                (set-wanderer-x! w (+ (wanderer-x w) (* 5 (cos (/ (* pi (wanderer-direction w)) 180)))))
                (set-wanderer-y! w (- (wanderer-y w) (* 5 (sin (/ (* pi (wanderer-direction w)) 180)))))
                w))]
          [else
           (begin
             ;when wanderer is far from the player
             (cond
               [(and (wanderer-moving w) (>= (wanderer-steps w) 0))
                (begin
                  (set-wanderer-chasing! w #t)
                  (set-wanderer-x! w (+ (wanderer-x w) (* (wanderer-speed w) (cos (/ (* pi (wanderer-direction w)) 180)))))
                  (set-wanderer-y! w (- (wanderer-y w) (* (wanderer-speed w) (sin (/ (* pi (wanderer-direction w)) 180)))))
                  (set-wanderer-steps! w (- (wanderer-steps w) 1))
                  (set-wanderer-body! w (rotate 10 (wanderer-body w)))
                  w)]
               [(not (wanderer-moving w))
                (begin
                  (set-wanderer-chasing! w #t)
                  (set-wanderer-direction! w (* 5 (random 72)))
                  (set-wanderer-speed! w (+ 2 (random 5)))
                  (set-wanderer-moving! w #t)
                  (set-wanderer-steps! w 30)
                  w)]
               [(< (wanderer-steps w) 0)
                (begin
                  (set-wanderer-chasing! w #t)
                  (set-wanderer-moving! w #f)
                  w)]))])))

;guard-tower
(define (move-tower t player)
  (begin
    (set-tower-direction! t (/ (* 180 (atan (- (tower-y t) (TANK-y player))
                                            (- (TANK-x player) (tower-x t))))
                               pi))
    (set-tower-body! t (align-cannon t))
    (cond [(= 100 (tower-delay t))
           (begin
             (play tower-sound)
             (set-tower-bdirection! t (tower-direction t))
             (set-tower-delay! t (- (tower-delay t) 1))
             (set-tower-bx! t (+ (tower-x t) (* 60 (cos (/ (* pi (tower-direction t)) 180)))))
             (set-tower-by! t (- (tower-y t) (* 60 (sin (/ (* pi (tower-direction t)) 180)))))
             t
             )]
          [(= 0 (tower-delay t))
           (begin
             (set-tower-delay! t 100)
             t)]
          [else
           (begin
             (set-tower-delay! t (- (tower-delay t) 1))
             (set-tower-bx! t (+ (tower-bx t) (* 4 (cos (/ (* pi (tower-bdirection t)) 180)))))
             (set-tower-by! t (- (tower-by t) (* 4 (sin (/ (* pi (tower-bdirection t)) 180)))))
             (set-tower-bullet-body! t (rotate 45 (tower-bullet-body t)))
             t
             )]
          )
    ))
  
;destroyer
(define (move-destroyer d p)
  (if(destroyer-firing d)
     ;i.e if the destroyer is firing the flame ring
     (cond [(> (destroyer-flame-radius d) 250)
            (begin (set-destroyer-flame-radius! d 0)
                   (set-destroyer-firing! d #f)
                   (set-destroyer-delay! d 100)
                   d)]
           [else
            (begin
              (cond [(even? (floor (/ (destroyer-flame-radius d) 2.5))) (play destroyer-sound)])
              (set-destroyer-body! d (create-destroyer-body d))
              (set-destroyer-flame-radius! d (+ 2.5 (destroyer-flame-radius d)))
              d)]
           )
     ;i.e when the destroyer is moving towards the player
     (cond [(> (destroyer-delay d) 0)
            (begin (set-destroyer-delay! d (- (destroyer-delay d) 1))
                   (set-destroyer-direction! d (/ (* 180 (atan (- (destroyer-y d) (TANK-y p))
                                                               (- (TANK-x p) (destroyer-x d)))) pi))
                   (set-destroyer-body! d (create-destroyer-body d))
                   (set-destroyer-x! d (+ (destroyer-x d) (* 2 (cos (/ (* pi (destroyer-direction d)) 180)))))
                   (set-destroyer-y! d (- (destroyer-y d) (* 2 (sin (/ (* pi (destroyer-direction d)) 180)))))
                   d)]
           [else
            (begin (set-destroyer-delay! d 100)
                   (set-destroyer-firing! d #t)
                   (set-destroyer-flame-radius! d 0)
                   d)]
           )
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
#| Functions to place the enemies on the map |#

;wanderer
(define (place-wanderer w scn)
  (place-image (wanderer-body w) (wanderer-x w) (wanderer-y w) scn))

;guard-tower
(define (place-tower t scn)
  (place-image (tower-body t) (tower-x t) (tower-y t)
               (place-image (tower-bullet-body t) (tower-bx t) (tower-by t) scn)))

;destroyer
(define (place-destroyer d scn)
  (place-image (destroyer-body d) (destroyer-x d) (destroyer-y d) scn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| Function to update the player and the enemy list to account for collisions |#

(define (update-player-and-list)
  (update-player)
  (update-list))

(define (update-player)
  (change-player (update-player-helper ENEMY-LIST player)))

(define (update-player-helper l p)
  (if(null? l) p
     (cond [(wanderer? (car l))
            (if(<= (distance-wanderer (car l) p) (+ 20 (wanderer-radius (car l))))
               (begin (set-TANK-health! p (- (TANK-health p)
                                             (wanderer-damage (car l))))
                      (update-player-helper (cdr l) p))
               (update-player-helper (cdr l) p))]
           [(tower? (car l))
            (if(<= (distance-b-player (car l) p) 30)
               (begin
                 (set-TANK-health! p (- (TANK-health p) (tower-damage (car l))))
                 (update-player-helper (cdr l) p))
               (update-player-helper (cdr l) p))]
           [(destroyer? (car l))
            (if(and (destroyer-firing (car l)) (< (distance-flame-ring (car l) p) (destroyer-flame-radius (car l))))
               (begin
                 (set-TANK-health! p (- (TANK-health p) (destroyer-damage (car l))))
                 (update-player-helper (cdr l) p))
               (update-player-helper (cdr l) p))]
           )))

(define (update-list)
  (set! ENEMY-LIST (update-list-helper ENEMY-LIST player)))
(define (update-list-helper l p)
  (if(null? l) '()
     (cond [(wanderer? (car l))
            (if(<= (distance-wanderer (car l) p) (+ 20 (wanderer-radius (car l))))
               (cons (struct-copy wanderer (car l)
                                  [chasing #f]
                                  [direction (+ 180 (wanderer-direction (car l)))])
                     (update-list-helper (cdr l) p))
               (cons (car l) (update-list-helper (cdr l) p))
               )]
           [(tower? (car l))
            (if(<= (distance-b-player (car l) p) 30)
               (cons (struct-copy tower (car l)
                                  [delay 100]
                                  )
                     (update-list-helper (cdr l) p))
               (cons (car l) (update-list-helper (cdr l) p))
               )]
           [(destroyer? (car l)) (cons (car l) (update-list-helper (cdr l) p))]
           ;no change in destroyer if player enters force field
            
           )))
                 
