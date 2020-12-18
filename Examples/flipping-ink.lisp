;;; ---------------------------------------------------------------------------
;;;   License: BSD-2-Clause.
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Demonstrate the effect of flipping ink on different backgrounds.

(defpackage #:clim-demo.flipping-ink
  (:use
   #:clim-lisp
   #:clim)

  (:export
   #:flipping-ink))

(in-package #:clim-demo.flipping-ink)

(define-application-frame flipping-ink ()
  ()
  (:menu-bar nil)
  (:pane :application :display-function #'display
                      :width            125
                      :height           125
                      :scroll-bars      nil))

(defparameter *ink* (make-flipping-ink +red+ +blue+))

(defun display (frame pane)
  (declare (ignore frame))
  (with-bounding-rectangle* (x1 y1 x2 y2) pane
    (draw-rectangle* pane x1 y1 x2 y2 :ink +blue+))
  (draw-text* pane "JackDaniels" 20 10 :ink *ink* :align-y :top)
  (draw-text* pane "JackDaniels" 20 60 :ink *ink* :align-y :top)
  (draw-rectangle* pane 5 30 15 40 :ink +red+)
  (draw-rectangle* pane 5 45 15 55 :ink +black+)
  (draw-rectangle* pane 10 5 90 85 :ink *ink*)
  (draw-rectangle* pane 60 60 120 120 :ink *ink*)
  (draw-rectangle* pane 40 40 80 80 :ink *ink*))
