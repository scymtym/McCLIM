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
