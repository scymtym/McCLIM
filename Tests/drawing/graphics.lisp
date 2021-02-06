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

;;; `with-drawing-options'

(test with-drawing-options.clipping.smoke
  "Smoke test for establishing clipping regions using `with-drawing-options'."

  (with-comparison-to-reference* (stream "with-drawing-options.clipping.smoke")
    (draw-rectangle* stream 1/2 1/2 (- 28 1/2) (- 16 1/2) :ink +red+ :filled nil)
    (with-drawing-options (stream :clipping-region (make-rectangle* 3 3 8 8))
      (draw-rectangle* stream 3 3 13 13 :ink +blue+))
    (with-drawing-options (stream :clipping-region (region-difference
                                                    (make-rectangle* 15 3 25 13)
                                                    (make-rectangle* 16 4 20 8)))
      (draw-rectangle* stream 15 3 25 13 :ink +blue+))))

(test with-drawing-options.clipping.stress
  "Test `with-drawing-options' for many combinations of transformation
   and clipping region."

  (with-comparison-to-reference* (stream "with-drawing-options.clipping.stress")
    (draw-rectangle* stream 1/2 1/2 (- 450 1/2) (- 110 1/2) :ink +red+ :filled nil)
    (loop :for x :from 20 :by 16
          :with y = 10
          :for p :from 15 :downto 0 :by 1/2
          :do (with-translation (stream x y)
                (with-drawing-options (stream :transformation (make-rotation-transformation (/ pi 2)))
                  (with-drawing-options (stream :clipping-region (make-rectangle* 0 0 p 13))
                    (draw-text* stream "hi" 0 0 :align-y :top :transform-glyphs t :ink +green+))))

              (with-translation (stream x (* 3 y))
                (with-drawing-options (stream :transformation (make-rotation-transformation (/ pi 2)))
                  (with-drawing-options (stream :clipping-region (make-rectangle* 0 0 13 p))
                    (draw-text* stream "hi" 0 0 :align-y :top :transform-glyphs t :ink +blue+))))

              (with-translation (stream x (* 6 y))
                (with-drawing-options (stream :clipping-region (make-rectangle* 0 0 p 13))
                  (draw-text* stream "hi" 0 0 :align-y :top :transform-glyphs t :ink +green+)))

              (with-translation (stream x (* 9 y))
                (with-drawing-options (stream :clipping-region (make-rectangle* 0 0 13 p))
                  (draw-text* stream "hi" 0 0 :align-y :top :transform-glyphs t :ink +blue+))))))

(test with-drawing-options.line-style
  "Smoke test for establishing a line style using `with-drawing-options'."

  (with-comparison-to-reference* (stream "with-drawing-options.line-style")
    (draw-rectangle* stream 1/2 1/2 (- 78 1/2) (- 28 1/2) :ink +red+ :filled nil)
    (with-drawing-options (stream :line-thickness 3)
      (draw-line* stream 4 4 14 4 :ink +blue+)
      (draw-rectangle* stream 4 14 14 24 :filled nil :ink +blue+))
    (with-drawing-options (stream :line-dashes t)
      (draw-line* stream 24 4 34 4 :ink +blue+)
      (draw-rectangle* stream 24 14 34 24 :filled nil :ink +blue+))
    (with-drawing-options (stream :line-dashes '(6 2))
      (draw-line* stream 44 4 54 4 :ink +blue+)
      (draw-rectangle* stream 44 14 54 24 :filled nil :ink +blue+))
    (with-drawing-options (stream :line-dashes #(6 2))
      (draw-line* stream 64 4 74 4 :ink +blue+)
      (draw-rectangle* stream 64 14 74 24 :filled nil :ink +blue+))))

;;; `draw-design' and `draw-pattern'

(defun %make-pattern ()
  (let ((array (make-array '(20 20))))
    (loop :for y :below 20
          :do (loop :for x :below 20
                    :when (or (= x 0) (= x 19) (= y 0) (= y 19))
                    :do (setf (aref array y x) 1)
                    :when (and (<= 0 x 10) (<= 0 y 10))
                    :do (setf (aref array y x) 2)))
    (make-pattern array (list +red+ +blue+ +green+))))

(defun %make-tile ()
  (let ((array (make-array '(20 20) :initial-element 0)))
    (loop :for y :from 0 :below 10
          :do (loop :for x :from 0 :below 10
                    :do (setf (aref array y        x)        1
                              (aref array (+ y 10) (+ x 10)) 1)))
    (make-rectangular-tile (make-pattern array (list +red+ +blue+)) 20 20)))

(defun draw-patterns (pattern-1 function stream)
  ;; Draw the pattern PATTERN-1 in four ways:
  ;; - just PATTERN-1
  ;; - result of TRANSFORM-REGION with a scaling transformation
  ;; - result of TRANSFORM-REGION with a rotation transformation
  ;; - PATTERN-1 but with a non-identity medium transformation
  (let* ((pattern-2 (transform-region
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
      (draw-rectangle* stream 1/2 1/2 (- 117 1/2) (- 52 1/2) :ink +red+ :filled nil)
      (draw-it pattern-1 4 7)
      (draw-it pattern-2 34 7)
      (draw-it pattern-3 64 7)
      (with-translation (stream 94 7)
        (with-rotation (stream (/ pi 4))
          (draw-it pattern-1 10 10))))))

(test draw-design.transformed
  "Test drawing transformed patterns with `draw-design'."

  (with-comparison-to-reference* (stream "draw-design.transformed.array")
    (draw-patterns (%make-pattern) 'draw-design stream))

  (with-comparison-to-reference* (stream "draw-design.transformed.tile")
    (draw-patterns (%make-tile) 'draw-design stream)))

(test draw-pattern*.transformed
  "Test drawing transformed patterns with `draw-pattern*'."

  (with-comparison-to-reference* (stream "draw-pattern.transformed.array")
    (draw-patterns (%make-pattern) 'draw-pattern* stream))

  (with-comparison-to-reference* (stream "draw-pattern.transformed.tile")
    (draw-patterns (%make-tile) 'draw-pattern* stream)))
