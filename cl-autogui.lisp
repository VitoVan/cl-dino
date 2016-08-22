(ql:quickload '(clx zpng))

(defpackage #:cl-autogui
  (:use #:common-lisp #:xlib)
  (:export #:x-position
           #:x-size
           #:x-position
           #:x-move
           #:x-mouse-down
           #:x-mouse-up
           #:x-click
           #:x-dbclick
           #:x-vscroll
           #:x-hscroll
           #:x-scroll
           #:x-key-down
           #:x-key-up
           #:x-press
           #:x-snapshot
           #:x-find-color
           #:x-get-color))

(in-package #:cl-autogui)

(defmacro with-default-display ((display &key (force nil)) &body body)
  `(let ((,display (open-default-display)))
     (unwind-protect
          (unwind-protect
               ,@body
            (when ,force
              (display-force-output ,display)))
       (close-display ,display))))

(defmacro with-default-display-force ((display) &body body)
  `(with-default-display (,display :force t) ,@body))

(defmacro with-default-screen ((screen) &body body)
  (let ((display (gensym)))
    `(with-default-display (,display)
       (let ((,screen (display-default-screen ,display)))
         ,@body))))

(defmacro with-default-window ((window) &body body)
  (let ((screen (gensym)))
    `(with-default-screen (,screen)
       (let ((,window (screen-root ,screen)))
         ,@body))))

(defun x-position ()
  (with-default-window (w)
    (query-pointer w)))

(defun x-size ()
  (with-default-screen (s)
    (values
     (screen-width s)
     (screen-height s))))

(defun x-move (x y)
  (if (and (integerp x) (integerp y))
      (with-default-display-force (d)
        (xtest:fake-motion-event d x y))
      (error "Integer only for position, (x: ~S, y: ~S)" x y)))

(defun mklist (obj)
  (if (and
       (listp obj)
       (not (null obj)))
      obj (list obj)))

(defmacro defun-with-actions (name params actions &body body)
  "This macro defun a function which witch do mouse or keyboard actions,
body is called on each action."
  `(defun ,name ,params
     (mapcar
      #'(lambda (action)
          ,@body)
      (mklist ,actions))))

(defun perform-mouse-action (press? button &key x y)
  (and x y (x-move x y))
  (with-default-display-force (d)
    (xtest:fake-button-event d button press?)))

(macrolet ((def (name actions)
             `(defun-with-actions ,name
                  (&key (button 1) x y)
                  ,actions
                (funcall #'perform-mouse-action
                         action button :x x :y y))))
  (def x-mouse-down t)
  (def x-mouse-up nil)
  (def x-click '(t nil))
  (def x-dbclick '(t nil t nil)))

(defmacro with-scroll (pos neg clicks x y)
  `(let ((button (cond
                   ((= 0 ,clicks) nil)
                   ((> 0 ,clicks) ,pos) ; scroll up/right
                   ((< 0 ,clicks) ,neg)))) ; scroll down/left
     (dotimes (_ (abs ,clicks))
       (x-click :button button :x ,x :y ,y))))

(defun x-vscroll (clicks &key x y)
  (with-scroll 4 5 clicks x y))

(defun x-scroll (clicks &key x y)
  (x-vscroll clicks :x x :y y))

(defun x-hscroll (clicks &key x y)
  (with-scroll 7 6 clicks x y))

(defun perform-key-action (press? keycode) ; use xev to get keycode
  (with-default-display-force (d)
    (xtest:fake-key-event d keycode press?)))

(macrolet ((def (name actions)
             `(defun-with-actions ,name (keycode)
                  ,actions
                (funcall #'perform-key-action
                         action keycode))))
  (def x-key-down t)
  (def x-key-up nil)
  (def x-press '(t nil)))

(defun raw-image->png (data width height)
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha
                             :image-data data))
         (data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
        ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
        (rotatef (aref data y x 0) (aref data y x 2))
        (setf (aref data y x 3) 255)))
    png))

(multiple-value-bind (default-width default-height) (x-size)
  
  (defun x-snapshot (&key (x 0) (y 0)
                       (width default-width) (height default-height)
                       (delay 0)
                       path)
    "Return RGB data array (The dimensions correspond to the height, width,
and pixel components, see comments in x-search-color for more details),
or write to file (PNG only), depend on if you provide the path keyword"
    (sleep delay)
    (with-default-window (w)
      (let ((image
             (raw-image->png
              (get-raw-image w :x x :y y
                             :width width :height height
                             :format :z-pixmap)
              width height)))
        (if path
            (let* ((ext (pathname-type path))
                   (path (if ext path (concatenate 'string path ".png")))
                   (png? (or (null ext) (equal ext "png"))))
              (cond
                (png? (zpng:write-png image path))
                (t (error "Only PNG file is supported"))))
            (zpng:data-array image)))))
  
  (defun x-find-color (rgba &key (x 0) (y 0)
                              (width default-width) (height default-height)
                              (test #'equal)
                              snap-data)
    "Search screen for specific Color (PNG's RGBA mode, where 'A' should be 0~255)"
    (let ((data (or snap-data (x-snapshot :x x :y y :width width :height height))))
      (dotimes (s-x width)
        (dotimes (s-y height)
          (labels ((get-rgba (data x y)
                     (mapcar
                      #'(lambda (i) (aref data y x i))
                      ;; why reversed order? http://xach.com/lisp/zpng/#data-array
                      ;; what is row-major? https://goo.gl/eF1F28
                      '(0 1 2 3))))
            (when (funcall test rgba (get-rgba data s-x s-y))
              (return-from x-find-color (list (+ x s-x) (+ y s-y))))))))))

(defun pixel->color (image-data x y)
  (funcall
   #'(lambda (data) (mapcar
                #'(lambda (i) (aref data y x i))
                '(0 1 2 3)))
   image-data))

(defun x-get-color (&rest coordinates)
  "Get colors by coordinates"
  (with-default-window (w)
    (let* ((x-list (mapcar #'(lambda (c) (car c)) coordinates))
           (y-list (mapcar #'(lambda (c) (cadr c)) coordinates))
           (min-x (apply #'min x-list))
           (max-x (apply #'max x-list))
           (min-y (apply #'min y-list))
           (max-y (apply #'max y-list))
           (width (1+ (- max-x min-x)))
           (height (1+ (- max-y min-y)))
           (x min-x)
           (y min-y)
           (image-data
            (zpng:data-array
             (raw-image->png
              (get-raw-image w :x x :y y
                             :width width :height height
                             :format :z-pixmap)
              width height))))
      (mapcar #'(lambda (cod)
                  (pixel->color image-data (- (car cod) x) (- (cadr cod) y)))
              coordinates))))
