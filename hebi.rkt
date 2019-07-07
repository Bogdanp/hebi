#!/usr/bin/env gracket
#lang racket/gui

(require racket/async-channel)

;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define GAME-W 400)
(define GAME-H 400)
(define SCALE 20)
(define ROWS (/ GAME-H SCALE))
(define COLS (/ GAME-W SCALE))


;; entities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct entity (row col)
  #:transparent)

(define collision?
  (match-lambda*
    [(list (entity row col)
           (entity row col)) #t]
    [_ #f]))

(module+ test
  (require rackunit)

  (test-case "entities collied when they occupy the same space"
    (check-true (collision? (entity 0 0)
                            (entity 0 0)))
    (check-false (collision? (entity 1 0)
                             (entity 0 0)))))


;; apples ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct apple entity (ts)
  #:transparent)

(define (make-apple)
  (apple (random (sub1 ROWS))
         (random (sub1 COLS))
         (current-inexact-milliseconds)))

(define (apple-points a)
  (inexact->exact
   (round
    ((max 0 (- 10000
               (- (current-inexact-milliseconds) (apple-ts a)))) . / . 1000))))


;; snakes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct part entity ()
  #:transparent)

(define (part-move p direction)
  (define row (entity-row p))
  (define col (entity-col p))
  (define-values (next-row next-col)
    (case direction
      [(north) (values (modulo (sub1 row) ROWS) col)]
      [(south) (values (modulo (add1 row) ROWS) col)]
      [(west)  (values row (modulo (sub1 col) COLS))]
      [(east)  (values row (modulo (add1 col) COLS))]
      [(halt)  (values row col)]))

  (struct-copy part p
               [row #:parent entity next-row]
               [col #:parent entity next-col]))

(struct snake (parts)
  #:transparent)

(define (make-snake locations)
  (snake (map (curry apply part) locations)))

(define (make-initial-snake)
  (make-snake '((9 10)
                (9  9)
                (9  8))))

(define (snake-move s directions)
  (define parts (snake-parts s))
  (snake (map part-move parts (take directions (length parts)))))

(define (snake-grow s)
  (define parts (snake-parts s))
  (snake (append parts (list (last parts)))))

(define (snake-head s)
  (car (snake-parts s)))

(define (snake-body s)
  (cdr (snake-parts s)))

(define (snake-length s)
  (length (snake-parts s)))

(module+ test
  (require rackunit)

  (test-case "all the parts of a snake move together"
    (define s (make-initial-snake))
    (check-equal? (snake-move s '(east east east))
                  (make-snake '((9 11)
                                (9 10)
                                (9  9)))))

  (test-case "the snake teleports at world boundaries"
    (check-equal? (snake-move (make-snake '((9 19)
                                            (9 18)
                                            (9 17)))
                              '(east east east))
                  (make-snake '((9  0)
                                (9 19)
                                (9 18))))

    (check-equal? (snake-move (make-snake '((19 5)
                                            (18 5)
                                            (17 5)))
                              '(south south south))
                  (make-snake '((0  5)
                                (19 5)
                                (18 5)))))

  (test-case "when a snake grows, its last part is duplicated"
    (check-equal? (snake-grow (make-initial-snake))
                  (make-snake '((9 10)
                                (9  9)
                                (9  8)
                                (9  8))))))


;; worlds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct world (snake apple direction directions score)
  #:transparent)

(define (make-initial-world)
  (world (make-initial-snake)
         (make-apple)
         'east
         '(east east east)
         0))

(define (world-difficulty w)
  (max 8 (sqrt ((world-score w) . / . 2))))

(define (world-apply-events w events)
  (define current-direction (world-direction w))
  (for/fold ([w w])
            ([e events])
    (match e
      ['restart (make-initial-world)]
      [(list 'change-direction dir)
       (define can-change-direction?
         (match (cons current-direction dir)
           [(or (cons 'north 'south)
                (cons 'south 'north)
                (cons 'east 'west)
                (cons 'west 'east)) #f]

           [_ #t]))

       (if can-change-direction?
           (struct-copy world w [direction dir])
           w)])))

(module+ test
  (require rackunit)
  (test-case "apply events ignores conflicting directions"
    (define w (make-initial-world))
    (check-equal? (world-direction (world-apply-events w '((change-direction west))))
                  'east)

    ;; the latter 'west direction is ignored since it would conflict with the current direction
    (check-equal? (world-direction (world-apply-events w '((change-direction north)
                                                           (change-direction west))))
                  'north)))

(define (world-place-apple w)
  (cond
    [(world-apple w) w]
    [else (struct-copy world w [apple (make-apple)])]))

(define (world-move-snake w)
  (define snake (world-snake w))
  (define direction (world-direction w))
  (define directions (world-directions w))
  (struct-copy world w
               [snake (snake-move snake directions)]
               [directions (take (cons direction directions)
                                 (snake-length snake))]))

(define (world-move-entities w)
  (let* ([w (world-place-apple w)]
         [w (world-move-snake w)])
    w))

(define (world-handle-apple-collision w)
  (define apple (world-apple w))
  (define snake (world-snake w))
  (define directions (world-directions w))
  (cond
    [(and apple (collision? apple (snake-head snake)))
     (struct-copy world w
                  [apple #f]
                  [score (+ (world-score w) (apple-points apple))]
                  [snake (snake-grow snake)]
                  [directions (append directions (list 'halt))])]

    [else w]))

(define (world-handle-snake-collisions w)
  (define snake (world-snake w))
  (cond
    [(ormap (curry collision? (snake-head snake)) (snake-body snake))
     (make-initial-world)]

    [else w]))

(define (world-handle-collisions w)
  (let* ([w (world-handle-apple-collision w)]
         [w (world-handle-snake-collisions w)])
    w))

(define (world-step w [events null])
  (let* ([w (world-apply-events w events)]
         [w (world-handle-collisions w)]
         [w (world-move-entities w)])
    w))


;; game loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (async-channel->list c)
  (let loop ([e (async-channel-try-get c)]
             [events null])
    (if e
        (loop (async-channel-try-get c) (cons e events))
        (reverse events))))

(define (entity-position e)
  (values
   (* (entity-col e) SCALE)
   (* (entity-row e) SCALE)))

(define (render-apple a dc)
  (when a
    (define-values (x y)
      (entity-position a))

    (send dc set-smoothing 'aligned)
    (send dc set-brush "red" 'solid)
    (send dc set-pen "white" 1 'transparent)
    (send dc draw-rounded-rectangle x y SCALE SCALE 5)))

(define (render-snake-part p dc [color "black"])
  (define-values (x y)
    (entity-position p))

  (send dc set-brush color 'solid)
  (send dc set-pen "white" 1 'solid)
  (send dc draw-rectangle x y SCALE SCALE))

(define (render-snake s dc)
  (render-snake-part (snake-head s) dc "gray")
  (for-each (curryr render-snake-part dc) (snake-body s)))

(define (render-score s dc)
  (send dc draw-text (format "Score: ~a" s) 10 10))

(define (make-game-loop)
  (define events (make-async-channel))
  (define world (make-initial-world))
  (values
   (lambda (cb)
     (let loop ()
       (define next-loop-time
         (alarm-evt (+ (current-inexact-milliseconds)
                       (/ 1000 (world-difficulty world)))))

       (set! world (world-step world (async-channel->list events)))
       (cb)
       (sync next-loop-time)
       (loop)))

   (lambda (e)
     (async-channel-put events e))

   (lambda (dc)
     (send dc clear)
     (render-apple (world-apple world) dc)
     (render-snake (world-snake world) dc)
     (render-score (world-score world) dc))))


;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (define-values (loop-forever enqueue-event render)
    (make-game-loop))

  (define window
    (new (class frame%
           (super-new)
           (define/override (on-subwindow-char _ e)
             (define event
               (match (send e get-key-code)
                 [#\q    ((application-quit-handler))]
                 [#\r    'restart]
                 ['up    '(change-direction north)]
                 ['down  '(change-direction south)]
                 ['left  '(change-direction west)]
                 ['right '(change-direction east)]
                 [_      #f]))

             (and event (enqueue-event event))))
         [label  "Hebi"]
         [width  GAME-W]
         [height GAME-H]))

  (define canvas
    (new canvas%
         [parent window]
         [paint-callback (lambda (_ dc)
                           (render dc))]))

  (send window show #t)
  (void
   (thread
    (lambda _
      (loop-forever (lambda _
                      (send canvas refresh)))))))
