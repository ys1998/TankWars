#lang racket
(provide set-parameters
         get-enemy-set
         get-map-no
         f2
         get-response)

;importing standard libraries
(require (prefix-in gui: racket/gui/base))
(require (prefix-in y: racket/draw))

(define MAP_NO 1)
(define ENEMY_SET 0)

(define (get-map-no) MAP_NO)
(define (get-enemy-set) ENEMY_SET)

#| Functions to display the menu |#
(define f2 (new gui:dialog%
                [label "Tank Wars --- By Yash Shah"]
                [min-width 400]
                [min-height 60]
                [stretchable-width 0]
                [stretchable-height 0]))
(define c1 (new gui:canvas% [parent f2]))


(define bitmap (y:read-bitmap "images/rsz_main-poster.jpg"))
(define bmp (new gui:message% [parent f2] [label bitmap]))

(define p1 (new gui:horizontal-panel%
                [parent f2]
                [alignment '(center center)]))
(define bf1 (new gui:button%
                 [parent p1] [label "PLAY"]
                 [callback (lambda (b e) (send play-menu show #t))]))
(define bf2 (new gui:button%
                 [parent p1] [label "INSTRUCTIONS"]
                 [callback (lambda (b e)
                             (gui:message-box
                              "Help-menu"
                              "CONTROLS\n*Moving the tank*\n\nSteer left : Left arrow key\nSteer right : Right arrow key\nAccelerate : Up arrow key\nBrake : Down arrow key\nTurning rear-turret to the left : a\nTurning rear-turret to the right : d\n\n*Firing weapons*\n\nShort-range-missile : Right ctrl\nCannon (projectile) : Left ctrl\nSonicboom : Right shift\n\nABOUT THE GAME\n*Enemies*\nWanderer : Moves randomly on the screen until you come within its range of vision - from then on, it doubles up its rpm and chases you. Inflicts damage when it collides with you, and then retreats to prepare for another attack.\n\nGuard-tower : Fires energy-balls towards you at regular time intervals and upto a certain range. Reloads and fires as soon as the existing ball is destroyed. Inflicts damage when the tank is hit by the energy-ball.\nIt is immune towards short-range-missiles and sonicbooms.\n\nDestroyer : Follows you slowly for fixed number of steps - then starts creating a flame-ring around it that expands with time. Inflicts damage proportional to the time our tank spends in this ring's field.\n\n*Weapons*\n\nShort-range-missile : Fired from the front-turret and moves upto a certain range - it explodes thereafter. Inflicts a damage of 10 points on contact.\n\nCannon : Fired from the rear-cannon along its direction. Requires manual setting up of certain parameters before it is fired. Inflicts damage of 30 points when hit - else it explodes on striking the ground.\n\nSonicboom : Fired from the tank's front. Inflicts very less damage on contact but it pushes back mobile enemies - it is deflected thereafter and continues moving for a fixed number of steps."
                              ))]))
(define bf3 (new gui:button%
                 [parent p1] [label "EXIT"]
                 [callback (lambda (b e) (print "Exited game successfully.") (exit))]))

(define play-menu (instantiate gui:dialog% ("Select map and enemies :")))
(define pp1 (new gui:horizontal-panel% [parent play-menu]))
(define pp2 (new gui:horizontal-panel% [parent play-menu]))
(define pp3 (new gui:horizontal-panel% [parent play-menu]
                 [alignment '(right center)]))
(define choice1 (new gui:choice%
                     [label "Map : "]
                     [choices (list "War in the park"
                                    "On an island"
                                    "Trouble on the shore"
                                    "Desert is my battlefield"
                                    "There is marble everywhere")]
                     [parent pp1]
                     [callback (lambda (c e)
                                 (set! MAP_NO (+ 1 (send choice1 get-selection))))]))
(define choice2 (new gui:choice%
                     [label "Enemy-set : "]
                     [choices (list "DEMONSTRATION"
                                    "Easy"
                                    "Intermediate"
                                    "Not-too-hard"
                                    "Difficult"
                                    "God save me!")]
                     [parent pp2]
                     [callback (lambda (c e)
                                 (set! ENEMY_SET (send choice2 get-selection)))]))
(define pb1 (new gui:button%
                 [parent pp3] [label "START"]
                 [callback (lambda (b e)
                             (send f2 show #f)
                             (send play-menu show #f))]))
  

#| Functions and GUI for selecting power and inclination for cannonball |#
(define choice "none")
(define pop-up-menu (instantiate gui:dialog% ("Set parameters :")))
(define horz-panel-1 (new gui:horizontal-panel% [parent pop-up-menu]))
(define horz-panel-2 (new gui:horizontal-panel% [parent pop-up-menu]))
 
(define s1 (new gui:slider%
                [min-value 0]
                [max-value 30]
                [init-value 15]
                [parent horz-panel-1]
                [label "Power "]
                ))
(define s2 (new gui:slider%
                [min-value 0]
                [max-value 90]
                [init-value 45]
                [parent horz-panel-2]
                [label "Angle "]
                ))
(define panel (new gui:horizontal-panel% [parent pop-up-menu]
                   [alignment '(center center)]))
 
(define b1 (new gui:button% [parent panel] [label "Cancel !"]
                [callback (lambda (b e)
                            (set! choice "cancel")
                            (send pop-up-menu show #f))]))
(define b2 (new gui:button% [parent panel] [label "Fire !"]
                [callback (lambda (b e)
                            (set! choice "fire")
                            (send pop-up-menu show #f))]))

(define (cancel-or-fire t)
  (cond [(equal? choice "fire") (cons #t (cons (send s1 get-value) (send s2 get-value)))]
        [(equal? choice "cancel")  (cons #f (cons 0 0))]
        ))
  
;function visible outside
(define (set-parameters)
  (cancel-or-fire (send pop-up-menu show #t)))

#| Function to restart the game |#
(define response 'no)
(define (get-response)
  (send f3 show #t)
  response)
(define f3 (new gui:dialog%
                [label "Tank Wars"]
                [min-width 60]
                [min-height 40]
                [stretchable-width 0]
                [stretchable-height 0]))
(define p1f3 (new gui:horizontal-panel%
                  [parent f3]
                  [alignment '(center center)]))
(define p2f3 (new gui:horizontal-panel%
                  [parent f3]
                  [alignment '(center center)]))
(define mp1 (new gui:message%
                 [label "Want to play the game again?"]
                 [parent p1f3]))
(define b1p2 (new gui:button% [parent p2f3] [label "YES"]
                  [callback (lambda (b e)
                              (set! response 'yes)
                              (send f3 show #f))]))
(define b2p2 (new gui:button% [parent p2f3] [label "NO"]
                  [callback (lambda (b e)
                              (set! response 'no)
                              (send f3 show #f))]))

