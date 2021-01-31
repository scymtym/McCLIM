;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for the miscellaneous formatting functions.

(in-package #:clim-tests)

(def-suite* :mcclim.formatting.misc
  :in :mcclim)

(test draw-pointer.smoke
  "Smoke test for establishing clipping regions using `with-drawing-options'."

  (flet ((do-it (args reference)
           (with-comparison-to-reference* (stream reference)
             (draw-rectangle* stream 1/2 1/2 (- 50 1/2) (- 58 1/2) :ink +red+ :filled nil)
             (with-translation (stream 24 24)
               (apply #'climi::draw-pointer stream :radius 20 :length 10 args)))))
    (do-it `(:buttons 0)                        "draw-pointer.none")
    (do-it `(:buttons ,+pointer-left-button+)   "draw-pointer.left")
    (do-it `(:buttons ,+pointer-middle-button+) "draw-pointer.middle")
    (do-it `(:buttons ,+pointer-right-button+)  "draw-pointer.right")
    (do-it `(:background ,+beige+)              "draw-pointer.background")))
