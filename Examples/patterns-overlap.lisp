;;; ---------------------------------------------------------------------------
;;;   License: BSD-2-Clause.
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Demonstrate transformed and overlapping patterns.

(defpackage #:clim-demo.patterns-overlap
  (:use
   #:clim-lisp
   #:clim)

  (:export
   #:patterns-overlap))

(in-package #:clim-demo.patterns-overlap)

(define-application-frame patterns-overlap ()
  ()
  (:menu-bar nil)
  (:pane :application :display-function #'display
                      :scroll-bars      nil))

(defparameter *pattern*
  (let ((array (make-array (list 300 300) :initial-element 0))
        (color 1))
    (loop for i from 0 below 300 by 30
          do (dotimes (y 30)
               (dotimes (x 30)
                 (setf (aref array (+ y i) (+ x i)) color)))
             (incf color))
    (make-pattern array (list (make-opacity 0.4) +red+ +green+ +blue+
                              +purple+ +yellow+ +grey+ +seagreen3+
                              +orange+ +dark-blue+ +dark-red+))))

;;; Bug: something wrong with output record size (window is too
;;; big). More apparent if we skip WITH-ROOM-FOR-GRAPHICS, but we want
;;; demo to look good.
(defun display (frame pane)
  (declare (ignore frame))
  (with-room-for-graphics (pane :first-quadrant nil)
    (draw-pattern* pane *pattern* 0 0)
    (draw-pattern* pane
                   (transform-region (make-rotation-transformation* (/ pi 4) 150 150)
                                     *pattern*)
                   0 0)
    ;; Bug: this is wrong, we cut out 5 rectangles!
    (draw-pattern* pane
                   (transform-region (make-scaling-transformation* 1/2 1/2 0 0)
                                     *pattern*)
                   75 250)))

(define-patterns-overlap-command (refresh-patterns-overlap :keystroke #\space) ()
  (format *debug-io* "."))
