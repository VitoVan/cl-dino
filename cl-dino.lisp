(eval-when (:compile-toplevel :load-toplevel :execute)
  #-clautogui
  (load "cl-autogui.lisp")
  #-clppcre
  (ql:quickload 'cl-ppcre))

(defpackage #:cl-dino
  (:use #:common-lisp #:cl-autogui)
  (:export #:dino-jump
           #:dino-play))

(in-package #:cl-dino)

;; === These keycode values should be changed according to your keyboard ===

(defvar *f5-key* 71)
(defvar *space-key* 65)
(defvar *down-key* 116)
(defparameter *scan-interval* 1/60)

;; === These coordinates values should be changed according to your screen ===

;; game over text position (position of 'G' and 'R')
(defvar *game-over-points* '((385 175) (480 220) (575 173)))
;; background points
(defvar *background-color-point* '(50 150))
;; dino on the ground
(defvar *dino-standing-points* '((207 238) (242 223)))
;; dino bending points
(defvar *dino-bending-points* '((209 240) (262 243)))
;; the block search squre (x y weight height)
(defvar *block-search-squre* '(265 220 500 35))
;; Position to click (focus the game)
(defvar *mouse-focus-point* '(190 150))
;; the y of middle flying bird
(defvar *middle-bird-y* 220)

(defun first-pixel->color (snap-data)
  (cl-autogui::pixel->color snap-data 0 0))

(defun points-exists? (points
                       &key color (test #'(lambda (a b) (not (equal a b)))))
  (let* ((all-points
          (if color
              points
              (push *background-color-point* points)))
         (all-colors
          (apply #'x-get-color all-points))
         (base-color (or color (pop all-colors)))
         (other-colors all-colors))
    (every #'identity (mapcar #'(lambda (c) (funcall test base-color c)) other-colors))))

(defun game-over? ()
  (points-exists? *game-over-points*))

(defun find-block ()
  "find properties of the first block (cactus or bird) in front of dino, (x y)"
  (let* ((x (first *block-search-squre*))
         (y (second *block-search-squre*))
         (w (third *block-search-squre*))
         (h (fourth *block-search-squre*))
         (snap-data (x-snapshot :x x :y y :width w :height h))
         (first-color (first-pixel->color snap-data)))
    (x-find-color first-color  :x x :y y :width w :height h
                  :test #'(lambda (a b) (not (equal a b)))
                  :data snap-data)))

(defun dino-jump ()
  (when (or ; only jump if dino is on the ground.
         (points-exists? *dino-standing-points*)
         (points-exists? *dino-bending-points*))
    (x-key-up *down-key*)
    (x-press *space-key*)
    (sleep 0.1)))

(defun dino-bend ()
  (when (points-exists? *dino-standing-points*) ; only bend if dino is on the ground.
    (x-key-down *down-key*)))

(defparameter *distance-stack* nil)

(defun dino-restart ()
  ;; move to dino and click
  (setf *distance-stack* nil)
  (x-click :x (car *mouse-focus-point*) :y (cadr *mouse-focus-point*))
  (sleep 1)
  (x-press *f5-key*)
  (sleep 1)
  (x-press *space-key*)
  ;; move away, annoying mouse
  (x-move (- (car *mouse-focus-point*) 150) (cadr *mouse-focus-point*)))

(defun find-gap (predicate lst &key (test #'>))
  (if (or
       (null lst)
       (null (cdr lst))
       (not (funcall predicate (car lst)))
       (not (funcall predicate (cadr lst))))
      -1
      (if (funcall test (car lst) (cadr lst))
          1
          (1+ (find-gap predicate (cdr lst) :test test)))))

(defun chunk-list (predicate lst &key (test #'>))
  (labels ((rec (lst acc)
             (let* ((n (find-gap predicate lst :test test))
                    (rest (when (not (equal -1 n)) (nthcdr n lst))))
               (if (and (consp rest) (not (zerop n)))
                   (rec rest (cons (subseq lst 0 n) acc))
                   (nreverse (cons lst acc))))))
    (if lst (rec lst nil) nil)))

(defun map-a/b (fn lst)
  (if (cdr lst)
      (cons (funcall fn (car lst) (cadr lst))
            (when (cddr lst) (map-a/b fn (cdr lst))))
      nil))

(defun avg-speed ()
  "px per second"
  (when (< (length *distance-stack*) 2) (return-from avg-speed 1))
  ;; cut *distance-stack*, only save recent 1000 values, for memory good.
  (when (> (length *distance-stack*) 1000)
    (setf *distance-stack* (subseq *distance-stack* 0 1000)))
  (let ((block-dist-group (chunk-list #'numberp *distance-stack*)))
    (labels ((cal-avg (lst)
               (if (> (length lst) 0)
                   (/ (apply #'+ lst) (length lst))
                   0))
             (cal-speed (dist-list)
               (cal-avg (map-a/b
                         #'(lambda (a b) (/ (- b a) *scan-interval*))
                         dist-list))))
      (round
       (cal-avg
        (mapcar #'cal-speed block-dist-group))))))

(defun jump? (distance speed)
  (when (and (numberp distance) (numberp speed) (not (zerop speed)))
    (<= (/ distance speed) 0.15)))

(defmacro dino-format (destination control-string &rest format-arguments)
  `(format ,destination
           ,(cl-ppcre:regex-replace-all "~S" control-string "~VS")
           ,@(mapcan #'(lambda (x) (list 10 x)) format-arguments)))

(defun dino-play ()
  "Awesome! http://imgur.com/kjRnw5G"
  (dino-restart)
  (loop
     (when (game-over?) (return))
     (let* ((block-position (find-block))
            (block-distance (when block-position
                              (- (car block-position) (first *block-search-squre*))))
            (block-y (cadr block-position))
            (speed (avg-speed))
            (should-jump (jump? block-distance speed)))
       (when block-distance (push block-distance *distance-stack*))
       (dino-format t "Distance: ~S Jump?: ~S Speed: ~S Y-Coord: ~S~%" block-distance should-jump speed block-y)
       (cond
         ((null block-position) nil)
         ((= *middle-bird-y* block-y) (dino-bend))
         ((= block-distance 0) nil)
         (should-jump (dino-jump))))
     (sleep *scan-interval*)))
