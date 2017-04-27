#lang racket
(provide game-map)
(provide get-max-speed)

;importing standard libraries
(require 2htdp/image)

;map elements
(define grass (bitmap "images/grass.png"))
(define water (bitmap "images/water.jpg"))
(define marble (bitmap "images/greyrock.jpg")) 
(define rock (bitmap "images/rock.png")) ;.jpg replaced with .png
(define glue (bitmap "images/glue.jpg"))

;scaling the sizes of these images to the same value
(set! grass (scale/xy (/ 50 (image-width grass)) (/ 50 (image-height grass)) grass))
(set! water (scale/xy (/ 50 (image-width water)) (/ 50 (image-height water)) water))
(set! marble (scale/xy (/ 50 (image-width marble)) (/ 50 (image-height marble)) marble))
(set! rock (scale/xy (/ 50 (image-width rock)) (/ 50 (image-height rock)) rock))
(set! glue (scale/xy (/ 50 (image-width glue)) (/ 50 (image-height glue)) glue))


;the list storing max-possible-speeds of the tank on a given terrain
(define max-speed-list (list 4 1 4.5 2 0.2))

#| ASSOCIATED FUNCTIONS |#

#|1|#
;function to return the max possible speed for a given terrain
(define (get-max-speed x y map-no)
  (let* [(l (get-map-from-no map-no))
         (x-size 50)
         (y-size 50)]
    (list-ref max-speed-list
              (- (list-ref (list-ref l (if(<= (inexact->exact (floor (/ y y-size))) 11)
                                          (inexact->exact (floor (/ y y-size)))
                                          11 ))
                           (if(<= (inexact->exact (floor (/ x x-size))) 17)
                              (inexact->exact (floor (/ x x-size)))
                              17))
                 1))))
  

#|2|#
;function to return the map "list" when given the map-no.
(define (get-map-from-no no)
  (cond [(= no 1) map-1]
        [(= no 2) map-2]
        [(= no 3) map-3]
        [(= no 4) map-4]
        [(= no 5) map-5]
        ))

#|3|#
;function to return the map's "image" when given the map-no.
(define (game-map no)
  (cond [(= no 1) created-map-1]
        [(= no 2) created-map-2]
        [(= no 3) created-map-3]
        [(= no 4) created-map-4]
        [(= no 5) created-map-5]

        ))
#|4|#
;function to create the map's "image" given a list containing the positions of the elements
(define (create-map l)
  (define (f x y)
    (above (create-map-helper x) y))
  (foldr f empty-image l))

(define (create-map-helper l)
  (define (f x y)
    (let [(img (cond [(= x 1) grass]
                     [(= x 2) water]
                     [(= x 3) marble]
                     [(= x 4) rock]
                     [(= x 5) glue]
                     ))]
      (beside img y)))
  (foldr f empty-image l))

;various maps' list used in the game
(define map-1 (list '(3 3 3 2 1 1 1 1 1 1 1 1 1 1 1 1 5 5)
                    '(3 3 3 2 1 1 1 1 1 1 1 1 1 1 1 1 1 5)
                    '(3 3 3 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                    '(5 5 1 1 1 5 4 4 4 4 4 4 5 1 1 1 1 1)
                    '(1 1 1 1 1 4 2 2 2 2 2 2 4 1 1 1 1 1)
                    '(1 1 1 1 1 4 2 2 2 2 2 2 4 1 1 1 1 1)
                    '(1 1 1 1 1 4 2 2 2 2 2 2 4 1 1 1 1 1)
                    '(1 1 1 1 1 4 2 2 2 2 2 2 4 1 1 1 1 1)
                    '(1 1 1 1 1 4 2 2 2 2 2 2 4 1 1 1 5 5)
                    '(1 1 1 1 1 5 4 4 4 4 4 4 5 1 2 3 3 3)
                    '(5 1 1 1 1 1 1 1 1 1 1 1 1 1 2 3 3 3)
                    '(5 5 1 1 1 1 1 1 1 1 1 1 1 1 2 3 3 3)

                    ))
(define map-2 (list '(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
                    '(3 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 2 3)
                    '(3 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 2 3)
                    '(3 2 2 2 2 4 4 4 4 4 4 4 4 2 2 2 2 3)
                    '(3 2 2 2 2 4 1 1 1 1 1 1 4 2 2 2 2 3)
                    '(3 3 3 3 3 4 1 1 5 5 1 1 4 3 3 3 3 3)
                    '(3 3 3 3 3 4 1 1 5 5 1 1 4 3 3 3 3 3)
                    '(3 3 3 3 3 4 1 1 5 5 1 1 4 3 3 3 3 3)
                    '(3 2 2 2 2 4 1 1 1 1 1 1 4 2 2 2 2 3)
                    '(3 2 2 2 2 4 4 4 4 4 4 4 4 2 2 2 2 3)
                    '(3 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 2 3)
                    '(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)

                    ))
(define map-3 (list '(3 3 3 3 3 1 2 2 2 2 2 2 2 2 4 4 4 4)
                    '(3 3 3 3 3 1 2 2 2 2 2 2 2 4 4 4 4 4)
                    '(3 3 3 3 1 1 2 2 2 2 2 2 4 4 4 4 4 4)
                    '(3 3 3 1 1 2 2 2 2 2 2 2 2 4 4 4 4 4)
                    '(3 3 1 1 2 2 2 1 1 2 2 2 2 2 4 2 4 4)
                    '(1 1 1 2 2 2 1 5 5 1 2 2 2 2 2 2 2 4)
                    '(2 2 2 2 2 2 1 5 5 1 2 2 2 2 2 2 2 4)
                    '(2 2 2 2 2 2 2 1 1 2 2 2 2 4 2 2 4 4)
                    '(2 2 2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 4)
                    '(2 2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 4 4)
                    '(2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 4 4 4)
                    '(2 2 2 2 2 2 2 2 4 4 4 4 4 4 4 4 4 4)
                    ))

(define map-4 (list '(4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 2 4 4)
                    '(4 4 4 2 4 4 4 4 4 4 4 4 4 4 2 4 4 4)
                    '(2 4 4 4 2 4 4 4 4 4 4 4 4 2 4 4 4 2)
                    '(4 2 4 3 3 3 3 3 3 3 3 3 3 3 3 4 2 4)
                    '(4 4 2 3 3 1 3 3 3 3 3 1 3 1 3 2 4 4)
                    '(4 4 4 3 3 1 3 3 1 3 1 2 1 3 3 4 4 4)
                    '(4 4 4 3 1 3 1 3 3 1 1 2 1 1 3 4 4 4)
                    '(4 4 2 3 3 3 3 1 3 3 3 1 3 3 3 2 4 4)
                    '(4 2 4 3 3 3 3 3 3 3 3 3 3 3 3 4 2 4)
                    '(2 4 4 4 2 4 4 4 4 4 4 4 4 2 4 4 4 2)
                    '(4 4 4 2 4 4 4 4 4 4 4 4 4 4 2 4 4 4)
                    '(4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 2 4 4)
                    ))

(define map-5 (list '(3 3 3 3 3 3 4 4 4 4 4 4 3 3 3 3 3 3)
                    '(3 3 3 3 3 3 3 4 4 4 4 3 3 3 3 3 3 3)
                    '(3 3 3 3 3 3 3 3 4 4 3 3 3 3 3 3 3 3)
                    '(3 3 3 5 2 2 2 2 2 2 2 2 2 2 5 3 3 3)
                    '(3 3 5 2 3 1 4 1 3 3 3 1 4 3 2 5 3 3)
                    '(3 5 2 3 3 4 1 4 3 3 1 4 1 3 3 2 5 3)
                    '(3 5 2 3 3 1 1 4 3 1 4 1 4 3 3 2 5 3)
                    '(3 3 5 2 3 3 1 3 3 3 3 1 3 3 2 5 3 3)
                    '(3 3 3 5 2 2 2 2 2 2 2 2 2 2 5 3 3 3)
                    '(3 3 3 3 3 3 3 3 4 4 3 3 3 3 3 3 3 3)
                    '(3 3 3 3 3 3 3 4 4 4 4 3 3 3 3 3 3 3)
                    '(3 3 3 3 3 3 4 4 4 4 4 4 3 3 3 3 3 3)
                    ))

;final images of the created maps
(define created-map-1 (freeze (create-map map-1)))
(define created-map-2 (freeze (create-map map-2)))
(define created-map-3 (freeze (create-map map-3)))
(define created-map-4 (freeze (create-map map-4)))
(define created-map-5 (freeze (create-map map-5)))
