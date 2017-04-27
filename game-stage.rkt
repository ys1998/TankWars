#lang racket
;provide statements for exporting necessary functions
(provide get-max-x
         get-max-y)
(provide main-frame)

;importing standard libraries
(require 2htdp/image)

;importing component files
(require "player.rkt")
(require "enemies.rkt")
(require "global-data.rkt")


(define (get-max-x) MAX_X)
(define (get-max-y) MAX_Y)

;scene-creation elements
(define MAX_X 900)
(define MAX_Y 600)
(define BOUNDARY_WIDTH 25)
(define BOUNDARY_TOP
  (freeze (scale/xy (/ 950 (image-width (bitmap "images/frame.png")))
                    (/ 25 (image-height (bitmap "images/frame.png")))
                    (bitmap "images/frame.png"))))
(define BOUNDARY_BOTTOM
  (freeze (rotate 180 (scale/xy (/ 950 (image-width (bitmap "images/frame.png")))
                                (/ 25 (image-height (bitmap "images/frame.png")))
                                (bitmap "images/frame.png")))))

(define BOUNDARY_LEFT
  (freeze (rotate 90 (scale/xy (/ 650 (image-width (bitmap "images/frame.png")))
                               (/ 25 (image-height (bitmap "images/frame.png")))
                               (bitmap "images/frame.png")))))
(define BOUNDARY_RIGHT
  (freeze (rotate 270 (scale/xy (/ 650 (image-width (bitmap "images/frame.png")))
                                (/ 25 (image-height (bitmap "images/frame.png")))
                                (bitmap "images/frame.png")))))



#| Associated functions |#

                                

#|2|#
;function to create a health bar at the bottom of screen
(define (create-health-bar)
  (cond [(>= (get-health) 0)
         (overlay
          (beside (text "    PLAYER HEALTH : " 18 "black")
                  (overlay/align
                   "middle" "middle"
                   (text (number->string (get-health)) 18 "white")
                   (rectangle (* 5 (get-health)) 20 "solid" (get-color (get-health) 100))))
          empty-image)]
        [else (overlay
               (beside (text "    PLAYER HEALTH : " 18 "black")
                       (text "0" 18 "white"))
               empty-image)]
        ))
            

(define (get-color h max-h)
  (let [(b (inexact->exact (floor (* (/ 255 max-h) h))))]
    (color (- 255 b) 0 b)))

#|3|#
;function to create enemy-health bars on top of screen
;MAX 5 ENEMIES PER MAP
(define (create-enemies-health-bar l)
  (if(null? l) empty-image
     (cond [(and (wanderer? (car l)) (>= (wanderer-health (car l)) 0))
            (beside (overlay (text " Wanderer " 16 "white")
                             (rectangle (wanderer-health (car l)) 20 "solid"
                                        (get-color (- 50 (wanderer-health (car l))) 50)))
                    (create-enemies-health-bar (cdr l)))
            ]
           [(and (tower? (car l)) (>= (tower-health (car l)) 0))
            (beside (overlay (text " Tower " 16 "white")
                             (rectangle (tower-health (car l)) 20 "solid"
                                        (get-color (- 150 (tower-health (car l))) 150)))
                    (create-enemies-health-bar (cdr l)))
            ]
           [(and (destroyer? (car l)) (>= (destroyer-health (car l)) 0))
            (beside (overlay (text " Destroyer " 16 "white")
                             (rectangle (destroyer-health (car l)) 20 "solid"
                                        (get-color (- 100 (destroyer-health (car l))) 100)))
                    (create-enemies-health-bar (cdr l)))]
           [else (create-enemies-health-bar (cdr l))]
           )
     )
  )
(define frame 
  (overlay/align "middle" "top"
                 BOUNDARY_TOP
                 (overlay/align "left" "middle"
                                BOUNDARY_LEFT
                                (overlay/align "right" "bottom"
                                               BOUNDARY_RIGHT
                                               (overlay/align "middle" "bottom"
                                                              BOUNDARY_BOTTOM
                                                              empty-image)))))
#|1|#
;function to prepare the game-scene when given the game-map
(define (main-frame game-map)
  (overlay/align "left" "bottom"
                 (create-health-bar)
                 (overlay/align "left" "top"
                                (create-enemies-health-bar (get-enemy-list))
                                (overlay/align "middle" "middle"
                                               frame
                                               (overlay game-map
                                                        (empty-scene (+ MAX_X (* 2 BOUNDARY_WIDTH))
                                                                     (+ MAX_Y (* 2 BOUNDARY_WIDTH))
                                                                     )
                                                        )))))
