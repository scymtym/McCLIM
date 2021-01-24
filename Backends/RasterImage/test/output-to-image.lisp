;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Test for WITH-OUTPUT-TO-RASTER-IMAGE.

(in-package #:mcclim-raster-image.test)

(in-suite :mcclim-raster-image)

(test with-output-to-raster-image.smoke
  "Smoke test for the WITH-OUTPUT-TO-RASTER-IMAGE macro."

  (flet ((do-it (width height border-width)
           (let* ((name (format nil "with-output-to-raster-image-~A-~A-~A"
                                width height border-width))
                  (pathname (make-pathname :name name :type "png"))
                  (string   (with-output-to-string (*standard-output*)
                              (room))))
             (mcclim-raster-image:with-output-to-raster-image-file
                 (stream pathname :width width
                                  :height height
                                  :border-width border-width)
               (write-string string stream)))))
    (map-product (lambda (&rest args)
                   (finishes (apply #'do-it args)))
                 '(60 :compute) '(60 :compute) '(0 20))))
