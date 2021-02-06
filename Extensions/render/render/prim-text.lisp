(in-package #:mcclim-render-internals)

;;; Converting string into paths

(defun string-primitive-paths (medium x y string align-x align-y transform-glyphs
                               &aux
                               (transformation (sheet-device-transformation (medium-sheet medium)))
                               (font (text-style-mapping (port medium) (medium-text-style medium))))
  (flet ((adjust-positions ()
           (ecase align-x
             (:left)
             (:center (let ((origin-x (climb:text-size medium string :text-style (medium-text-style medium))))
                        (decf x (/ origin-x 2.0))))
             (:right  (let ((origin-x (climb:text-size medium string :text-style (medium-text-style medium))))
                        (decf x origin-x))))
           (ecase align-y
             (:top (incf y (climb:font-ascent font)))
             (:baseline)
             (:center (let* ((ascent (climb:font-ascent font))
                             (descent (climb:font-descent font))
                             (height (+ ascent descent))
                             (middle (- ascent (/ height 2.0s0))))
                        (incf y middle)))
             (:baseline*)
             (:bottom (decf y (climb:font-descent font))))))
    (cond (transform-glyphs
           (adjust-positions)
           (setf (values x y) (transform-position transformation x y)))
          (t
           (setf (values x y) (transform-position transformation x y))
           (adjust-positions))))
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (loop with device-region = (climi::medium-device-region medium)
            with glyph-transformation = (multiple-value-bind (x0 y0)
                                            (transform-position transformation 0 0)
                                          (compose-transformation-with-translation
                                           transformation (- x0) (- y0)))
            for code across (climb:font-string-glyph-codes font string)
            for origin-x fixnum = (round x) then (+ origin-x (glyph-info-advance-width info))
            for origin-y fixnum = (round y) then (+ origin-y (glyph-info-advance-height info))
            for info = (if (null transform-glyphs)
                           (font-glyph-info font code)
                           (font-generate-glyph font code glyph-transformation))
            for dx fixnum = (glyph-info-left info)
            for dy fixnum = (glyph-info-top info)
            for opacity-pixels = (glyph-info-pixarray info)
            for opacity-image = (make-instance 'climi::%ub8-stencil :array opacity-pixels)
            for width = (pattern-width opacity-image)
            for height = (pattern-height opacity-image)
            for transformation = (make-translation-transformation origin-x origin-y)
            do (multiple-value-bind (x1 y1)
                   (transform-position transformation dx (- dy))
                 (with-bounding-rectangle* (min-x min-y max-x max-y)
                     (region-intersection
                      device-region
                      (make-rectangle* x1 y1 (+ x1 width) (+ y1 height)))
                   (%medium-fill-image-mask medium opacity-image
                                            min-x min-y
                                            (- max-x min-x) (- max-y min-y)
                                            (- (round x1)) (- (round y1)))))))))
