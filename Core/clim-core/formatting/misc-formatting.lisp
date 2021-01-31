;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Useful non-standard formatting functions beyond tables and graphs.

(in-package #:clim-internals)

;;; Cursor position

(defun invoke-with-preserved-cursor-position (stream continuation
                                              &key (preserve-x t) (preserve-y t))
  (multiple-value-bind (old-x old-y) (stream-cursor-position stream)
    (prog1
        (funcall continuation stream)
      (setf (stream-cursor-position stream)
            (if (and preserve-x preserve-y)
                (values old-x old-y)
                (multiple-value-bind (new-x new-y)
                    (stream-cursor-position stream)
                  (values (if preserve-x old-x new-x)
                          (if preserve-y old-y new-y))))))))

(defmacro with-preserved-cursor-x ((stream) &body body)
  (check-type stream symbol)
  (gen-invoke-trampoline
   'invoke-with-preserved-cursor-position
   (list stream) '(:preserve-x t :preserve-y nil) body))

(defmacro with-preserved-cursor-y ((stream) &body body)
  (check-type stream symbol)
  (gen-invoke-trampoline
   'invoke-with-preserved-cursor-position
   (list stream) '(:preserve-x nil :preserve-y t) body))

;;; Drawing keys

(defun invoke-surrounding-output-with-key-border
    (stream continuation
     &key (foreground nil foreground-supplied-p)
          (background nil background-supplied-p))
  (let* ((darkp      (when (not (and foreground-supplied-p background-supplied-p))
                       (< (color-ihs (design-ink +background-ink+ 0 0)) .5)))
         (foreground (cond (foreground-supplied-p foreground)
                           (darkp                 +light-gray+)
                           (t                     +gray20+)))
         (background (cond (background-supplied-p background)
                           (darkp                 +gray20+)
                           (t                     +light-gray+))))
    (with-preserved-cursor-y (stream)
      (stream-increment-cursor-position stream 6 2)
      (surrounding-output-with-border (stream :shape       :rounded
                                              :background  background
                                              :outline-ink +dark-gray+
                                              :radius      2
                                              :padding-y   1
                                              :padding-x   6)
        (with-drawing-options (stream :ink foreground)
          (funcall continuation stream))))))

(defmacro surrounding-output-with-key-border ((stream &key foreground background) &body body)
  (gen-invoke-trampoline
   'invoke-surrounding-output-with-key-border
   (list stream)
   (append (if foreground (list :foreground foreground) '())
           (if background (list :background background) '()))
   body))

(flet ((as-key (thunk stream)
         (with-preserved-cursor-y (stream)
           (surrounding-output-with-key-border (stream)
             (with-text-size (stream :small)
               (funcall thunk stream))))))

  (defun format-gesture-modifiers (modifiers &key (stream *standard-output*)
                                                  print-nothing)
    (cond ((plusp modifiers)
           (flet ((prefix (modifier name)
                    (when (logtest modifier modifiers)
                      (as-key (curry #'write-string name) stream)
                      (write-string "  " stream))))
             (loop :for (nil value name) :in *modifier-keys*
                   :do (prefix value name))))
          (print-nothing
           (write-string "«no modifier»" stream))))

  (defun format-keyboard-gesture (gesture &key (stream *standard-output*))
    (destructuring-bind (type key modifiers) gesture
      (declare (ignore type))
      (format-gesture-modifiers modifiers :stream stream)
      (as-key (curry #'princ key) stream))))

;;; Drawing pointer states

(defun draw-pointer (stream &key (buttons       0)
                                 (radius        10)
                                 (length        5)
                                 (middle-width  (* 1/7 (* 2 radius)))
                                 (middle-height 8)
                                 (scroll-height 4)
                                 (y             (- (sqrt (- (expt radius 2)
                                                            (expt middle-width 2)))))
                                 background)
  (flet ((draw-button (drawn-button thunk)
           (when (logtest drawn-button buttons)
             (let ((ink (make-contrasting-inks
                         8 (1- (integer-length drawn-button)))))
               (funcall thunk :filled t :ink ink)))
           (funcall thunk :filled nil))
         (draw-body (&key filled)
           (cond (filled
                  (draw-circle* stream 0 0 radius :filled      filled
                                                  :start-angle 0
                                                  :end-angle   pi)
                  (draw-rectangle* stream (- radius) 0 radius length))
                 (t
                  (draw-line* stream (- radius) 0 (- radius) length)
                  (draw-line* stream radius     0 radius     length)))
           (draw-circle* stream 0 length radius :filled      filled
                                                :start-angle pi
                                                :end-angle   (* 2 pi))))
    (when background
      (with-drawing-options (stream :ink background)
        (draw-body :filled t)))
    (let* ((radius1/2     (+ radius 1/2))
           (middle-region (make-rectangle*
                           (- 0 middle-width 1/2) (- radius1/2) (+ middle-width 1/2) 1/2))
           (side-clip     (region-difference
                           (make-rectangle*
                            (- radius1/2) (- radius1/2) radius1/2 (+ length radius1/2))
                           middle-region)))
      ;; Right button
      (draw-button +pointer-right-button+
                   (lambda (&rest options)
                     (with-drawing-options (stream :clipping-region side-clip)
                       (apply #'draw-circle* stream 0 0 radius :start-angle 0
                                                               :end-angle   (/ pi 2)
                                                               options)
                       (draw-line* stream middle-width 0 radius 0))))
      ;; Left button
      (draw-button +pointer-left-button+
                   (lambda (&rest options)
                     (with-drawing-options (stream :clipping-region side-clip)
                       (apply #'draw-circle* stream 0 0 radius :start-angle (/ pi 2)
                                                               :end-angle   pi
                                                               options))
                     (draw-line* stream (- middle-width) 0 (- radius) 0)))
      ;; Middle button
      (draw-button +pointer-middle-button+
                   (curry #'draw-rectangle* stream
                          (- middle-width) y middle-width (+ y middle-height))))
    (incf y (+ middle-height 1))
    ;; Scroll up
    (draw-button +pointer-wheel-up+
                 (curry #'draw-rectangle* stream
                        (- middle-width) y middle-width (+ y scroll-height)))
    (incf y scroll-height)
    ;; Scroll down
    (draw-button +pointer-wheel-down+
                 (curry #'draw-rectangle* stream
                        (- middle-width) y middle-width (+ y scroll-height)))
    ;; Body
    (draw-body)))

(defun format-pointer-button (button &key (stream *standard-output*)
                                          (radius 10)
                                          (length 5))
  (with-room-for-graphics (stream :first-quadrant nil :move-cursor nil)
    (draw-pointer stream :buttons button :radius radius :length length))
  (stream-increment-cursor-position stream (+ (* 2 radius) 2) 0))

(defun format-pointer-gesture (gesture &key (stream *standard-output*))
  (destructuring-bind (type button modifiers) gesture
    (declare (ignore type))
    (format-gesture-modifiers modifiers :stream stream)
    (format-pointer-button button :stream stream)))
