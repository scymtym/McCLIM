;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for the drawing functions.

(in-package #:clim-tests)

(def-suite* :mcclim.drawing
  :in :mcclim)

(defun %make-pattern ()
  (let ((array (make-array '(20 20))))
    (loop :for y :below 20
          :do (loop :for x :below 20
                    :when (or (= x 0) (= x 19) (= y 0) (= y 19))
                    :do (setf (aref array y x) 1)
                    :when (and (<= 0 x 10) (<= 0 y 10))
                    :do (setf (aref array y x) 2)))
    (make-pattern array (list +red+ +blue+ +green+))))

(defun draw-patterns (function stream)
  ;; Draw the pattern PATTERN-1 in four ways:
  ;; - just PATTERN-1
  ;; - result of TRANSFORM-REGION with a scaling transformation
  ;; - result of TRANSFORM-REGION with a rotation transformation
  ;; - PATTERN-1 but with a non-identity medium transformation
  (let* ((pattern-1 (%make-pattern))
         (pattern-2 (transform-region
                     (make-scaling-transformation* 1/2 1/2)
                     pattern-1))
         (pattern-3 (transform-region
                     (make-rotation-transformation* (/ pi 4) 10 10)
                     pattern-1)))
    (flet ((draw-it (pattern x y)
             (ecase function
               (draw-design
                (with-translation (stream x y)
                  (funcall function stream pattern)))
               (draw-pattern*
                (funcall function stream pattern x y)))
             (draw-point* stream x y :ink +black+)))
      (draw-it pattern-1 0 0)
      (draw-it pattern-2 30 0)
      (draw-it pattern-3 60 0)
      (with-translation (stream 90 0)
        (with-rotation (stream (/ pi 4))
          (draw-it pattern-1 10 10))))))

(test draw-design.transformed
  "Test drawing transformed patterns with `draw-design'."

  (with-comparison-to-reference (stream "draw-design.transformed")
    (draw-patterns 'draw-design stream)))

(test draw-pattern*.transformed
  "Test drawing transformed patterns with `draw-pattern*'."

  (with-comparison-to-reference (stream "draw-pattern.transformed")
    (draw-patterns 'draw-pattern* stream)))
