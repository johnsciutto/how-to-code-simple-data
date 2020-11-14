;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 4)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 5)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; ===========================================================
;; Data Definitions

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of Invader(s)

(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;; - emtpy
;; - (cons Misile ListOfMissile)
;; interp. a list of Missile(s)

(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (first lom)
              (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ===========================================================
;; Functions

;; start game with (main G0)

(define (main g)
  (big-bang g                                  ; Game
    (on-tick tick-game)                        ; Game -> Game
    (on-key handle-keys)                       ; Game KeyEvent -> Game
    (to-draw render-game)                      ; Game -> Image
    (stop-when game-over? game-over-screen)))   ; Game -> Boolean


;; Game -> Game
;; produce the next game
;; TESTS: This is tested by all the individual tests for each of the parts

; (define (tick-game g) G0) ; stub

; took template from Game

(define (tick-game s)
  (make-game (destroy-invaders (game-missiles s) (create-invaders (next-invaders (game-invaders s))))
             (next-missiles (game-missiles s))
             (next-tank (game-tank s))))


;; Tank -> Tank
;; produce the next tank (move by TANK-SPEED)
(check-expect (next-tank T0)
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)) ; center moving right
(check-expect (next-tank (make-tank (/ WIDTH 2) -1))
              (make-tank (+ (/ WIDTH 2) (* TANK-SPEED -1)) -1)) ; center moving left
(check-expect (next-tank (make-tank WIDTH 1))
              (make-tank (+ WIDTH TANK-SPEED) 1)) ; at right edge moving right
(check-expect (next-tank (make-tank 0 -1))
              (make-tank (* (+ 0 TANK-SPEED) -1) -1)) ; at left edge moving left

;(define (next-tank t) T0) ; stub

; took template from Tank

(define (next-tank t)
  (cond [(> (tank-x t) WIDTH) (make-tank WIDTH 0)]
        [(< (tank-x t) 0) (make-tank 0 0)]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))


;; ListOfInvaders -> ListOfInvaders
;; produce the next list of invaders
(check-expect (next-invaders LOI0) empty)
(check-expect (next-invaders (list (make-invader 10 10 1) (make-invader 20 30 -1) (make-invader 60 100 1)))
              (list (make-invader (+ 10 INVADER-X-SPEED) (+ 10 INVADER-Y-SPEED) 1)
                    (make-invader (- 20 INVADER-X-SPEED) (+ 30 INVADER-Y-SPEED) -1)
                    (make-invader (+ 60 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1)))

;(define (next-invaders loi) LOI0) ; stub

; took template from ListOfInvaders

(define (next-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (next-invaders (rest loi)))]))


;; Game -> Boolean
;; produce true if an invader reaches the bottom of the screen; else produce false
(check-expect (game-over? G0) false)
(check-expect (game-over? (make-game (cons I1 empty) empty T1)) false)
(check-expect (game-over? (make-game (cons I2 empty) empty T1)) true)

; took template from Game

;(define (game-over? g) MTS) ; stub

(define (game-over? g)
  (cond [(empty? (game-invaders g)) false]
        [else
         (invaded? (game-invaders g))]))


;; ListOfInvaders -> Boolean
;; produce true if an invader of the list has landed
(check-expect (invaded? empty) false)
(check-expect (invaded? LOI1) false)
(check-expect (invaded? (list I1 I2)) true)

;(define (invaded? loi) false) ; stub

; took template from ListOfInvaders

(define (invaded? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (invaded? (rest loi)))]))


;; Game -> Image
;; produce the image of "game over"
;; Tests are not necessary for this function since it always places the game over text if executed

;(define (game-over-screen g) BACKGROUND) ; stub

(define (game-over-screen g)
  (place-image (text "GAME-OVER" 45 "red") (/ WIDTH 2) (/ HEIGHT 2) (render-game g)))


;; Invader -> Invader
;; moves the invader by one tick
;; - invaders move in a 45 degree angle
;; - they bounce off walls
(check-expect (move-invader (make-invader 0 50 -1))
              (make-invader 0 (+ 50 INVADER-Y-SPEED) (- -1)))
(check-expect (move-invader (make-invader 1 50 1))
              (make-invader (+ 1 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) 1))
(check-expect (move-invader (make-invader WIDTH 50 1))
              (make-invader WIDTH (+ 50 INVADER-Y-SPEED) (- 1)))
(check-expect (move-invader (make-invader (- WIDTH 1) 50 -1))
              (make-invader (- (- WIDTH 1) INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) -1))
(check-expect (move-invader (make-invader (/ WIDTH 2) 50 1))
              (make-invader (+ (/ WIDTH 2) INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) 1))
(check-expect (move-invader (make-invader (/ WIDTH 2) 50 -1))
              (make-invader (- (/ WIDTH 2) INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) -1))

;(define (move-invader i) I1) ; stub

; took template form Invader

(define (move-invader i)
  (cond [(< (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) 0)
         (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [(> (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) WIDTH)
         (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i)))
                       (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))


;; ListOfInvaders -> ListOfInvaders
;; produces a loi with an additional invader at a INVADER-RATE rate.
;; TESTS: Cannot test for something random

; (define (create-invaders loi) LOI0) ; stub

; took template from ListOfInvaders

(define (create-invaders loi)
  (cond [(< (random 150) INVADE-RATE)
         (cons (make-invader (random WIDTH) 0 (random-direction 1)) loi)]
        [else
         loi]))


;; ListOfMissiles ListOfInvaders -> ListOfInvaders
;; produce a list of invaders without the invaders that have been hit by missiles
(check-expect (destroy-invaders empty empty) empty)
(check-expect (destroy-invaders LOM1 empty) empty)
(check-expect (destroy-invaders empty LOI1) LOI1)
(check-expect (destroy-invaders (list M1 M2 (make-missile 150 60)) (list (make-invader 10 50 -1) (make-invader 60 10 1) (make-invader 150 60 1)))
              (list (make-invader 10 50 -1) (make-invader 60 10 1)))

; (define (destroy-invaders lom loi) LOI0) ; stub

; took template from ListOfInvaders with an extra parameter

(define (destroy-invaders lom loi)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (hit-invader? (first loi) lom)
             (rest loi)
             (cons (first loi) (destroy-invaders lom (rest loi))))]))


;; Invader ListOfMissile -> Boolean
;; produce true if a missile in the lom hit the invader (within the HIT-RANGE)
(check-expect (hit-invader? (make-invader 10 50 1) (list (make-missile 50 50) (make-missile 70 10))) false)
(check-expect (hit-invader? (make-invader 10 50 1) (list (make-missile 10 45) (make-missile 70 10))) true)
(check-expect (hit-invader? (make-invader 10 50 1) (list (make-missile 70 10) (make-missile 10 45))) true)

; (define (hit-invader? i lom) false) ; stub

; took template from ListOfMissile with an extra parameter

(define (hit-invader? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (< (- (invader-x i) HIT-RANGE) (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))
                  (< (- (invader-y i) HIT-RANGE) (missile-y (first lom)) (+ (invader-y i) HIT-RANGE)))
             true
             (hit-invader? i (rest lom)))]))


;; Interger -> Interger
;; produce the given interger randomly switched to a negative value
;; TESTS: Cannot test random results

; (define (random-direction i) 1) ; stub

(define (random-direction i)
  (if (= (modulo (random 40) 2) 1)
      (- i)
      i))


;; ListOfMissiles -> ListOfMissiles
;; produce the next list of missiles
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (list (make-missile 50 100) (make-missile 20 160) (make-missile 60 170)))
              (list (make-missile 50 (- 100 MISSILE-SPEED)) (make-missile 20 (- 160 MISSILE-SPEED)) (make-missile 60 (- 170 MISSILE-SPEED))))

;(define (next-missiles lom) LOM0) ; stub

; took template from ListOfMissiles

(define (next-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (< (missile-y (first lom)) 0)
             (next-missiles (rest lom))
             (cons (move-missile (first lom))
                   (next-missiles (rest lom))))]))


;; Missile -> Missile
;; produce the next missile (move by MISSILE-SPEED)
(check-expect (move-missile (make-missile 150 200)) (make-missile 150 (- 200 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 10 250)) (make-missile 10 (- 250 MISSILE-SPEED)))

;(define (move-missile m) M1) ; stub

; took template from Missile

(define (move-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))


;; Game KeyEvent -> Game
;; produce the appropiate game when either the "left", "right" or
;; " " keys are pressed.
;; - "left" and "right" move the tank
;; - " " fires missiles
(check-expect (handle-keys (make-game empty empty (make-tank (/ WIDTH 2) 1)) "left")           ; tank moving left
              (make-game empty empty (make-tank (/ WIDTH 2) -1))) 
(check-expect (handle-keys (make-game empty empty (make-tank (/ WIDTH 2) -1)) "right")         ; tank moving right
              (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-keys (make-game empty (list M1 M2) (make-tank (/ WIDTH 2) -1)) " ")      ; tank moving left, shot fired
              (make-game empty
                         (list (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)) M1 M2)
                         (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-keys (make-game empty                                                    ; tank moving right, shot fired
                                      (list (make-missile 10 140) (make-missile 50 10))
                                      (make-tank (/ WIDTH 2) 1)) " ")
              (make-game empty
                         (list (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)) (make-missile 10 140) (make-missile 50 10))
                         (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-keys (make-game empty empty (make-tank (/ WIDTH 2) -1)) "k")             ; invalid key pressed
              (make-game empty empty (make-tank (/ WIDTH 2) -1))) 

;(define (handle-keys g ke) G0) ; stub

; took template from HTDW + Game

(define (handle-keys g ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (move-tank-left (game-tank g)))]
        [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (move-tank-right (game-tank g)))]
        [(key=? ke " ")
         (make-game (game-invaders g)
                    (shoot-missile (game-missiles g) (tank-x (game-tank g)))
                    (game-tank g))]
        [else g]))


;; Tank -> Tank
;; produce t moving left (tank-dx -1)
(check-expect (move-tank-left (make-tank 150 -1)) (make-tank 150 -1))
(check-expect (move-tank-left (make-tank 150 1)) (make-tank 150 -1))

;(define (move-tank-left t) T0) ; stub

; took template from Tank

(define (move-tank-left t)
  (make-tank (tank-x t) -1))


;; Tank -> Tank
;; produce t moving right (tank-dx 1)
(check-expect (move-tank-right (make-tank 150 -1)) (make-tank 150 1))
(check-expect (move-tank-right (make-tank 150 1)) (make-tank 150 1))

;(define (move-tank-right t) T0) ; stub

; took template from Tank

(define (move-tank-right t)
  (make-tank (tank-x t) 1))


;; ListOfMissiles Natural -> ListOfMissiles
;; produce the ListOfMissiles with a missile added at x
(check-expect (shoot-missile empty 50) (cons (make-missile 50 (- HEIGHT TANK-HEIGHT/2)) empty))
(check-expect (shoot-missile (list (make-missile 20 60) (make-missile 70 100)) 100)
              (list (make-missile 100 (- HEIGHT TANK-HEIGHT/2)) (make-missile 20 60) (make-missile 70 100)))

;(define (shoot-missile lom x) LOM1) ; stub

; took template from ListOfMissile with one additional parameter

(define (shoot-missile lom x)
  (cond [(empty? lom) (cons (make-missile x (- HEIGHT TANK-HEIGHT/2)) empty)]
        [else
         (cons (make-missile x (- HEIGHT TANK-HEIGHT/2))
               lom)]))


;; Game -> Image
;; render the game on BACKGROUND
(check-expect (render-game (make-game LOI1 LOM1 T1))
              (render-invaders LOI1 (render-missiles LOM1 (render-tank T1))))

;(define (render-game g) BACKGROUND) ; stub

; took template from Game

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g)))))


;; ListOfInvaders Image -> Image
;; produce an image of all the invaders in the list, overlyaing it to itself
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list (make-invader 50 100 1) (make-invader 70 80 -1)) BACKGROUND)
              (place-image INVADER
                           (invader-x (make-invader 50 100 1))
                           (invader-y (make-invader 50 100 1))
                           (place-image INVADER
                                        (invader-x (make-invader 70 80 -1))
                                        (invader-y (make-invader 70 80 -1))
                                        BACKGROUND)))

; took template from ListOfInvaders with an extra parameter

;(define (render-invaders loi) BACKGROUND) ; stub

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) img))]))


;; ListOfMissiles -> Image
;; produce an image of all the missiles on the list and overlay it to itself
(check-expect (render-missiles LOM0 BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list (make-missile 150 300)
                                     (make-missile 20 130)
                                     (make-missile 90 143)) BACKGROUND)
              (place-image MISSILE 150 300
                           (place-image MISSILE 20 130
                                        (place-image MISSILE 90 143 BACKGROUND))))

;(define (render-missiles lom img) img)

; took template from ListOfMissiles

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) img))]))


;; Tank -> Image
;; produce an image of the given tank on the BACKGROUND
(check-expect (render-tank T0)
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank T1)
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t) BACKGROUND) ; stub

; took template from Tank

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
