;;; ---------------------------------------------------------------------------
;;;   License: BSD-2-Clause.
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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
    (loop for i from 0 below 300
          do (setf (aref array 1   i)   1
                   (aref array 298 i)   1
                   (aref array i     1) 1
                   (aref array i   298) 1))
    (make-pattern array (list (make-opacity 0.4) +red+ +green+ +blue+
                              +purple+ +yellow+ +grey+ +seagreen3+
                              +orange+ +dark-blue+ +dark-red+))))

(defun display (frame pane)
  (declare (ignore frame))
  (draw-pattern* pane *pattern* 0 0)
  (draw-pattern* pane
                 (transform-region (make-rotation-transformation* (/ pi 4) 150 150)
                                   *pattern*)
                 0 0)
  (draw-pattern* pane
                 (transform-region (make-scaling-transformation* 1/2 1/2 0 0)
                                   *pattern*)
                 75 250))

(define-patterns-overlap-command (refresh-patterns-overlap :keystroke #\space) ()
  (format *debug-io* "."))
