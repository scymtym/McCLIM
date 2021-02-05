;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for output recording.

(in-package #:clim-tests)

(def-suite* :mcclim.extended-stream.recording
  :in :mcclim)

(test setf-output-record-position.smoke
  "Smoke test for the setf output-record-position function."
  ;; Due to the (setf output-record-position) method for
  ;; `draw-text-output-record', this also checks that
  ;; `compose-translation-with-transformation' works in the expected
  ;; way, in particular with respect to the transformation composition
  ;; order.
  (with-comparison-to-reference* (stream "setf-output-record-position")
    (draw-rectangle* stream 1/2 1/2 (- 30 1/2) (- 54 1/2) :ink +red+ :filled nil)
    (with-translation (stream 20 10)
      (with-drawing-options (stream :transformation (make-rotation-transformation (/ pi 2)))
        (draw-point* stream 0 0 :line-thickness 2 :ink +red+)
        (draw-text* stream "hi" 0 0 :align-y :top :transform-glyphs t :ink +green+)
        (draw-polygon* stream #(20 0 40 5 20 10) :ink +blue+)))))

(test with-room-for-graphics.clipping
  "Test interaction of `with-room-for-graphics' with clipping."

  (with-comparison-to-reference* (stream "with-room-for-graphics.clipping")
    (draw-rectangle* stream 1/2 1/2 (- 92 1/2) (- 46 1/2) :ink +red+ :filled nil)
    (labels ((do-it* (first-quadrant height)
               (with-room-for-graphics (stream :first-quadrant first-quadrant
                                               :height         height)
                 (draw-rectangle* stream -10 -10   0  0 :ink +red+)
                 (draw-rectangle* stream   0 -10  10  0 :ink +blue+)
                 (draw-rectangle* stream -10   0   0 10 :ink +green+)
                 (draw-rectangle* stream   0   0  10 10 :ink +white+)))
             (do-it (x y medium-clip first-quadrant height)
               (setf (stream-cursor-position stream) (values x y))
               (multiple-value-bind (x y) (stream-cursor-position stream)
                 (draw-point* stream x y :line-thickness 3 :ink +yellow+))
               (if medium-clip
                   (let ((clip (make-ellipse* (+ x 10) (+ y 10) 10 0 0 10)))
                     (with-drawing-options (stream :clipping-region clip)
                       (do-it* first-quadrant height)))
                   (do-it* first-quadrant height))
               (multiple-value-bind (x y) (stream-cursor-position stream)
                 (draw-point* stream x y :line-thickness 3 :ink +cyan+))))
      ;; Without medium clipping region
      (do-it 2   2 nil nil nil)
      (do-it 24  2 nil t   nil)
      (do-it 2  24 nil nil 15)
      (do-it 24 24 nil t   15)
      ;; With medium clipping region
      (do-it 48  2 t  nil nil)
      (do-it 70  2 t  t   nil)
      (do-it 48 24 t  nil 15)
      (do-it 70 24 t  t   15))))
