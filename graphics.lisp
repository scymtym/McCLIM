;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001 by Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :clim-internals)

(defmethod do-graphics-with-options ((sheet sheet) func &rest options)
  (with-sheet-medium (medium sheet)
    (apply #'do-graphics-with-options-internal medium sheet func options)))

(defmethod do-graphics-with-options ((medium medium) func &rest options)
  (apply #'do-graphics-with-options-internal medium medium func options))

(defmethod do-graphics-with-options ((sheet t) func &rest options)
  (declare (ignore options))
  (if sheet
      (funcall func sheet)))

(defmethod do-graphics-with-options ((pixmap pixmap) func &rest options)
  (with-pixmap-medium (medium pixmap)
    (apply #'do-graphics-with-options-internal medium medium func options)))

(defmethod do-graphics-with-options-internal ((medium medium) orig-medium func
				     &rest args
				     &key ink clipping-region transformation
					  line-unit line-thickness
                                          (line-style nil line-style-p)
					  (line-dashes nil dashes-p)
					  line-joint-shape line-cap-shape
					  (text-style nil text-style-p)
					  (text-family nil text-family-p)
					  (text-face nil text-face-p)
					  (text-size nil text-size-p)
				     &allow-other-keys)
  (declare (ignore args))
  (let ((old-ink (medium-ink medium))
	(old-clip (medium-clipping-region medium))
	(old-transform (medium-transformation medium))
	(old-line-style (medium-line-style medium))
	(old-text-style (medium-text-style medium))
        (changed-line-style line-style-p)
        (changed-text-style text-style-p))
    (unwind-protect
	(progn
          (when (eq ink old-ink) (setf ink nil))
          
	  (if ink
	      (setf (medium-ink medium) ink))
	  (if transformation
	      (setf (medium-transformation medium)
		(compose-transformations old-transform transformation)))

          (when (and clipping-region old-clip
                     (region-equal clipping-region old-clip))
            (setf clipping-region nil))                   

          (if clipping-region
	      (setf (medium-clipping-region medium)
		(region-intersection (if transformation
                                         (transform-region transformation old-clip)
                                       old-clip)
				     clipping-region)))
          (if (null line-style)              
              (setf line-style old-line-style))
	  (when (or line-unit line-thickness dashes-p line-joint-shape line-cap-shape)
            (setf changed-line-style T)
            (setf line-style (make-line-style
                              :unit (or line-unit
                                        (line-style-unit line-style))
                              :thickness (or line-thickness
                                             (line-style-thickness line-style))
                              :dashes (or line-dashes
                                          (line-style-dashes line-style))
                              :joint-shape (or line-joint-shape
                                               (line-style-joint-shape line-style))
                              :cap-shape (or line-cap-shape
                                             (line-style-cap-shape line-style)))))
          (when changed-line-style
            (setf (medium-line-style medium) line-style))
	  (if text-style-p
	      (setf text-style (merge-text-styles text-style
						  (medium-merged-text-style medium)))
	    (setf text-style (medium-merged-text-style medium)))
	  (when (or text-family-p text-face-p text-size-p)
            (setf changed-text-style T)
            (setf text-style (merge-text-styles (make-text-style text-family
                                                                 text-face
                                                                 text-size)
                                                text-style)))
          (when changed-text-style
            (setf (medium-text-style medium) text-style))
          
	  (when orig-medium
	    (funcall func orig-medium)))
        
      (when ink
	(setf (medium-ink medium) old-ink))
      ;; First set transformation, then clipping!
      (when transformation
	(setf (medium-transformation medium) old-transform))
      (when clipping-region
	(setf (medium-clipping-region medium) old-clip))
      (when changed-line-style
        (setf (medium-line-style medium) old-line-style))
      (when changed-text-style 
        (setf (medium-text-style medium) old-text-style)))))

(defmacro with-medium-options ((sheet args)
			       &body body)
  `(flet ((graphics-op (medium)
	    (declare (ignorable medium))
	    ,@body))
     #-clisp (declare (dynamic-extent #'graphics-op))
     (apply #'do-graphics-with-options ,sheet #'graphics-op ,args)))

(defmacro with-drawing-options ((medium &rest drawing-options) &body body)
  (when (eq medium t)
    (setq medium '*standard-output*))
  (check-type medium symbol)
  (let ((gcontinuation (gensym)))
    `(flet ((,gcontinuation (,medium)
             ,@body))
      #-clisp (declare (dynamic-extent #',gcontinuation))
      (apply #'invoke-with-drawing-options
             ,medium #',gcontinuation (list ,@drawing-options)))))

(defmethod invoke-with-drawing-options ((medium medium) continuation
                                        &rest drawing-options
                                        &key ink transformation clipping-region
                                        line-style text-style
					&allow-other-keys)
  (declare (ignore ink transformation clipping-region line-style text-style))
  (with-medium-options (medium drawing-options)
    (funcall continuation medium)))

(defmethod invoke-with-drawing-options ((sheet sheet) continuation &rest drawing-options)
  (with-sheet-medium (medium sheet)
                     (with-medium-options (medium drawing-options)
                                          (funcall continuation sheet))))

;;; Compatibility with real CLIM
(defmethod invoke-with-drawing-options ((sheet t) continuation
					&rest drawing-options)
  (declare (ignore drawing-options))
  (funcall continuation sheet))

(defmethod invoke-with-identity-transformation (medium cont)
  (with-drawing-options (medium 
                         :transformation (invert-transformation
                                          (medium-transformation medium)))
    (funcall cont medium)))

(defmethod invoke-with-local-coordinates (medium cont x y)
  ;; For now we do as real CLIM does.
  ;; Default seems to be the cursor position.
  ;; Moore suggests we use (0,0) if medium is no stream.
  ;;
  ;; Further the specification is vague about possible scalings ...
  ;;
  (unless (and x y)
    (multiple-value-bind (cx cy) (if (extended-output-stream-p medium)
                                     (stream-cursor-position medium)
                                     (values 0 0))
      (setf x (or x cx)
            y (or y cy))))
  (multiple-value-bind (mxx mxy myy myx tx ty)
      (get-transformation (medium-transformation medium))
    (declare (ignore tx ty))
    (with-identity-transformation (medium)
      (with-drawing-options
          (medium :transformation (make-transformation
                                   mxx mxy myy myx
                                   x y))
        (funcall cont medium)))))

(defmethod invoke-with-first-quadrant-coordinates (medium cont x y)
  ;; First we do the same as invoke-with-local-coordinates but rotate and
  ;; deskew it so that it becomes first-quadrant. We do this
  ;; by simply measuring the length of the transfomed x and y "unit vectors". 
  ;; [That is (0,0)-(1,0) and (0,0)-(0,1)] and setting up a transformation
  ;; which features an upward pointing y-axis and a right pointing x-axis with
  ;; a length equal to above measured vectors.
  (unless (and x y)
    (multiple-value-bind (cx cy) (if (extended-output-stream-p medium)
                                     (stream-cursor-position medium)
                                     (values 0 0))
      (setf x (or x cx)
            y (or y cy))))
  (let* ((tr (medium-transformation medium))
         (xlen
          (multiple-value-bind (dx dy) (transform-distance tr 1 0)
            (sqrt (+ (expt dx 2) (expt dy 2)))))
         (ylen
          (multiple-value-bind (dx dy) (transform-distance tr 0 1)
            (sqrt (+ (expt dx 2) (expt dy 2))))))
    (with-identity-transformation (medium)
      (with-drawing-options
          (medium :transformation (make-transformation
                                   xlen 0 0 (- ylen)
                                   x y))
        (funcall cont medium)))))

(defun draw-point (sheet point
		   &rest args
		   &key ink clipping-region transformation
			line-style line-thickness line-unit)
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (multiple-value-bind (x y) (point-position point)
      (medium-draw-point* medium x y))))

(defun draw-point* (sheet x y
		    &rest args
		    &key ink clipping-region transformation
			 line-style line-thickness line-unit)
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (medium-draw-point* medium x y)))

(defun expand-point-seq (point-seq)
  (let ((coord-seq nil))
    (do-sequence (point point-seq)
      (multiple-value-bind (x y) (point-position point)
	(setq coord-seq (list* y x coord-seq))))
    (nreverse coord-seq)))

(defun draw-points (sheet point-seq
		    &rest args
		    &key ink clipping-region transformation
			 line-style line-thickness line-unit)
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (medium-draw-points* medium (expand-point-seq point-seq))))

(defun draw-points* (sheet coord-seq
		     &rest args
		     &key ink clipping-region transformation
			  line-style line-thickness line-unit)
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (medium-draw-points* medium coord-seq)))

(defun draw-line (sheet point1 point2
		  &rest args
		  &key ink clipping-region transformation line-style line-thickness
		       line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (x1 y1) (point-position point1)
      (multiple-value-bind (x2 y2) (point-position point2)
	(medium-draw-line* medium x1 y1 x2 y2)))))

(defun draw-line* (sheet x1 y1 x2 y2
		   &rest args
		   &key ink clipping-region transformation line-style line-thickness
			line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-line* medium x1 y1 x2 y2)))

(defun draw-lines (sheet point-seq
		   &rest args
		   &key ink clipping-region transformation line-style line-thickness
			line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-lines* medium (expand-point-seq point-seq))))

(defun draw-lines* (sheet coord-seq
		    &rest args
		    &key ink clipping-region transformation line-style line-thickness
			 line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-lines* medium coord-seq)))

(defun draw-polygon (sheet point-seq
		     &rest args
		     &key (filled t) (closed t)
			  ink clipping-region transformation line-style line-thickness
			  line-unit line-dashes line-joint-shape line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-joint-shape line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-polygon* medium (expand-point-seq point-seq) closed filled)))

(defun draw-polygon* (sheet coord-seq
		    &rest args
		    &key (filled t) (closed t)
			 ink clipping-region transformation line-style line-thickness
			 line-unit line-dashes line-joint-shape line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-joint-shape line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-polygon* medium coord-seq closed filled)))

(defun draw-rectangle (sheet point1 point2
			&rest args
			&key (filled t)
			     ink clipping-region transformation line-style line-thickness
			     line-unit line-dashes line-joint-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-joint-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (x1 y1) (point-position point1)
      (multiple-value-bind (x2 y2) (point-position point2)
	(medium-draw-rectangle* medium x1 y1 x2 y2 filled)))))

(defun draw-rectangle* (sheet x1 y1 x2 y2
			&rest args
			&key (filled t)
			     ink clipping-region transformation line-style line-thickness
			     line-unit line-dashes line-joint-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-joint-shape))
  (with-medium-options (sheet args)
    (medium-draw-rectangle* medium x1 y1 x2 y2 filled)))

(defun draw-rectangles (sheet points
                        &rest args
                        &key (filled t)
                        ink clipping-region transformation line-style line-thickness
			line-unit line-dashes line-joint-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-joint-shape))
  (with-medium-options (sheet args)
    (loop for point in points
	  nconcing (multiple-value-bind (x y) (point-position point)
		     (list x y)) into position-seq
	  finally (medium-draw-rectangles* medium position-seq filled))))

(defun draw-rectangles* (sheet position-seq
                         &rest args
                         &key (filled t)
                         ink clipping-region transformation line-style line-thickness
                         line-unit line-dashes line-joint-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-joint-shape))
  (with-medium-options (sheet args)
    (medium-draw-rectangles* medium position-seq filled)))

(defun draw-triangle (sheet point1 point2 point3
                      &rest args
                      &key (filled t)
                      ink clipping-region transformation line-style line-thickness
                      line-unit line-dashes line-joint-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape))
  (apply #'draw-polygon sheet (list point1 point2 point3) :filled filled :closed t args))

(defun draw-triangle* (sheet x1 y1 x2 y2 x3 y3
                       &rest args
                       &key (filled t)
                       ink clipping-region transformation line-style line-thickness
                       line-unit line-dashes line-joint-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape))
  (apply #'draw-polygon* sheet (list x1 y1 x2 y2 x3 y3) :filled filled :closed t args))

(defun draw-ellipse (sheet
		     center-point
		     radius-1-dx radius-1-dy radius-2-dx radius-2-dy
		     &rest args
		     &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
			  ink clipping-region transformation
			  line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (center-x center-y) (point-position center-point)
      (medium-draw-ellipse* medium
			    center-x center-y
			    radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			    start-angle end-angle filled))))

(defun draw-ellipse* (sheet
		      center-x center-y
		      radius-1-dx radius-1-dy radius-2-dx radius-2-dy
		      &rest args
		      &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
			   ink clipping-region transformation
			   line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-ellipse* medium
			  center-x center-y
			  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			  start-angle end-angle filled)))

(defun draw-circle (sheet
		    center-point radius
		    &rest args
		    &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
			 ink clipping-region transformation
			 line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (center-x center-y) (point-position center-point)
      (medium-draw-ellipse* medium
			    center-x center-y
			    radius 0 0 radius
			    start-angle end-angle filled))))

(defun draw-circle* (sheet
		     center-x center-y radius
		     &rest args
		     &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
			  ink clipping-region transformation
			  line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-ellipse* medium
			  center-x center-y
			  radius 0 0 radius
			  start-angle end-angle filled)))

(defun draw-text (sheet string point
		   &rest args
		   &key (start 0) (end nil)
			(align-x :left) (align-y :baseline)
			(toward-point nil toward-point-p)
		        transform-glyphs
			ink clipping-region transformation
			text-style text-family text-face text-size)
  (declare (ignore ink clipping-region transformation
		   text-style text-family text-face text-size))
  (with-medium-options (sheet args)
    (multiple-value-bind (x y) (point-position point)
      (multiple-value-bind (toward-x toward-y)
	  (if toward-point-p
	      (point-position toward-point)
	      (values (1+ x) y))
        (medium-draw-text* medium string x y
                           start end
                           align-x align-y
                           toward-x toward-y transform-glyphs)))))

(defun draw-text* (sheet string x y
		   &rest args
		   &key (start 0) (end nil)
			(align-x :left) (align-y :baseline)
			(toward-x (1+ x)) (toward-y y) transform-glyphs
			ink clipping-region transformation
			text-style text-family text-face text-size)
  (declare (ignore ink clipping-region transformation
		   text-style text-family text-face text-size))
  (with-medium-options (sheet args)
    (medium-draw-text* medium string x y
		       start end
		       align-x align-y
		       toward-x toward-y transform-glyphs)))

;; This function belong to the extensions package.
(defun draw-glyph (sheet string x y
		   &rest args
		   &key (align-x :left) (align-y :baseline)
			toward-x toward-y transform-glyphs
			ink clipping-region transformation
			text-style text-family text-face text-size)
" Draws a single character of filled text represented by the given element. 
  element is a character or other object to be translated into a font index.
  The given x and y specify the left baseline position for the character."
  (declare (ignore ink clipping-region transformation
		   text-style text-family text-face text-size))
  (with-medium-options (sheet args)
    (medium-draw-glyph medium string x y
		       align-x align-y
		       toward-x toward-y transform-glyphs)))

(defun draw-arrow (sheet point-1 point-2
		   &rest args
		   &key ink clipping-region transformation
			line-style line-thickness
			line-unit line-dashes line-cap-shape
                        (to-head t) from-head (head-length 10) (head-width 5))
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness
		   line-unit line-dashes line-cap-shape
                   to-head from-head head-length head-width))
  (multiple-value-bind (x1 y1) (point-position point-1)
    (multiple-value-bind (x2 y2) (point-position point-2)
      (apply #'draw-arrow* sheet x1 y1 x2 y2 args))))

(defun draw-arrow* (sheet x1 y1 x2 y2
		   &rest args
		   &key ink clipping-region transformation
			line-style line-thickness
			line-unit line-dashes line-cap-shape
                        (to-head t) from-head (head-length 10) (head-width 5))
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (with-translation (sheet x2 y2)
      (with-rotation (sheet (atan* (- x1 x2)
                                   (- y1 y2)))
        (let* ((end      0.0)
               (start    (sqrt (+ (expt (- x2 x1) 2)
                                  (expt (- y2 y1) 2))))
               (p end)
               (q start)                                  

               (medium     (sheet-medium sheet))
               (line-style (medium-line-style medium))
               (thickness  (line-style-thickness line-style))
               (width/2    (/ head-width 2))

               (a (atan (/ width/2 head-length)))
               (offset     (if (and head-length (not (zerop head-length)))
                               (/ thickness (* 2 (sin a )))
                               0.0))               
               (tip-to-peak (+ head-length offset (- (* thickness 0.5 (sin a)))))) ;; okay, a guess..
         (when to-head   (incf p offset))
         (when from-head (decf q offset))
         (if (and to-head
                  from-head
                  (< (abs (- start end)) (* 2 tip-to-peak)))
             
             (let ((width (* 0.5 (+ head-width thickness)
                             (/ (abs (- start end))
                                (* 2 tip-to-peak)) )))
               (draw-polygon* sheet
                              (list end 0
                                    (/ start 2) width
                                    start 0
                                     (/ start 2) (- width))
                              :filled t
                              :line-thickness 0 ))
             (progn
               (when to-head
                 (draw-polygon* sheet
                                (list (+ p head-length) (- width/2)
                                      p 0
                                      (+ p head-length) width/2)
                                :filled nil
                                :closed nil))
               (when from-head
                 (draw-polygon* sheet
                                (list (- q head-length) (- width/2)
                                      q 0
                                      (- q head-length) width/2)
                                :filled nil
                                :closed nil))
               
               (unless (< q p)
                 (draw-line* sheet q 0 p 0)))) )))))


(defun draw-oval (sheet center-pt x-radius y-radius
		  &rest args
		  &key (filled t) ink clipping-region transformation
                       line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore filled ink clipping-region transformation
		   line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (multiple-value-bind (x1 y1) (point-position center-pt)
    (apply #'draw-oval* sheet x1 y1 x-radius y-radius args)))

(defun draw-oval* (sheet center-x center-y x-radius y-radius
                   &rest args
                   &key (filled t) ink clipping-region transformation
                         line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (check-type x-radius (real 0))
  (check-type y-radius (real 0))
  (if (or (coordinate= x-radius 0) (coordinate= y-radius 0))
      (draw-circle* sheet center-x center-y (max x-radius y-radius)
                    :filled filled)
      (with-medium-options (sheet args)
        (if (coordinate<= y-radius x-radius)
            (let ((x1 (- center-x x-radius)) (x2 (+ center-x x-radius))
                  (y1 (- center-y y-radius)) (y2 (+ center-y y-radius)))              
              (if filled
                  (draw-rectangle* sheet x1 y1 x2 y2)
                  (draw-lines* sheet (list x1 y1 x2 y1 x1 y2 x2 y2)))              
              (draw-circle* sheet x1 center-y y-radius
                            :filled filled
                            :start-angle (* pi 0.5)
                            :end-angle (* pi 1.5))
              (draw-circle* sheet x2 center-y y-radius
                            :filled filled
                            :start-angle (* pi 1.5)
                            :end-angle (* pi 2.5)))
            (with-rotation (sheet (/ pi 2) (make-point center-x center-y))
              (draw-oval* sheet center-x center-y y-radius x-radius
                          :filled filled)) ))))


;;; Pixmap functions

(defmethod copy-to-pixmap ((medium medium) medium-x medium-y width height
                           &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (unless pixmap
    (setq pixmap (allocate-pixmap medium (+ pixmap-x width) (+ pixmap-y height))))
  (medium-copy-area medium medium-x medium-y width height
                    pixmap pixmap-x pixmap-y)
  pixmap)

(defmethod copy-to-pixmap ((sheet sheet) sheet-x sheet-y width height
                           &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (copy-to-pixmap (sheet-medium sheet) sheet-x sheet-y width height
                  pixmap pixmap-x pixmap-y))

(defmethod copy-from-pixmap (pixmap pixmap-x pixmap-y width height
                             (medium medium) medium-x medium-y)
  (medium-copy-area pixmap pixmap-x pixmap-y width height
                    medium medium-x medium-y)
  pixmap)

(defmethod copy-from-pixmap (pixmap pixmap-x pixmap-y width height
                             (sheet sheet) sheet-x sheet-y)
  (medium-copy-area pixmap pixmap-x pixmap-y width height
                    (sheet-medium sheet) sheet-x sheet-y))

(defmethod copy-area ((medium medium) from-x from-y width height to-x to-y)
  (medium-copy-area medium from-x from-y width height
                    medium to-x to-y))

(defmethod copy-area ((sheet sheet) from-x from-y width height to-x to-y)
  (copy-area (sheet-medium sheet) from-x from-y width height to-x to-y))

(defmethod copy-area ((stream stream) from-x from-y width height to-x to-y)
  (if (sheetp stream)
      (copy-area (sheet-medium stream) from-x from-y width height to-x to-y)
    (error "COPY-AREA on a stream is not implemented")))

(defmacro with-output-to-pixmap ((medium-var sheet &key width height) &body body)
  `(let* ((pixmap (allocate-pixmap ,sheet ,width ,height)) ; XXX size might be unspecified -- APD
	  (,medium-var (make-medium (port ,sheet) pixmap))
	  (old-medium (sheet-medium ,sheet)))
     (setf (slot-value pixmap 'medium) ,medium-var) ; hmm, [seems to work] -- BTS
     (setf (%sheet-medium ,sheet) ,medium-var) ;is sheet a sheet-with-medium-mixin? --GB
     (unwind-protect
	 (progn ,@body)
       (setf (%sheet-medium ,sheet) old-medium));is sheet a sheet-with-medium-mixin? --GB
     pixmap))

;;; XXX This seems to be incorrect.
;;; This presumes that your drawing will completely fill the bounding rectangle
;;; of the sheet and will effectively randomize anything that isn't drawn
;;; within it.
;;; FIXME
#+nil
(defmacro with-double-buffering ((sheet) &body body)
  (let ((width (gensym))
	(height (gensym))
	(pixmap (gensym))
	(sheet-mirror (gensym)))
    `(let* ((,width (round (bounding-rectangle-width (sheet-region ,sheet))))
	    (,height (round (bounding-rectangle-height (sheet-region ,sheet))))
            (,(gensym) (progn (format *debug-io* "w-d-b ~A ~A~%" ,width ,height) (finish-output *debug-io*) 0))
	    (,pixmap (allocate-pixmap ,sheet ,width ,height))
	    (,sheet-mirror (sheet-direct-mirror ,sheet)))
       (unwind-protect
	   (progn
	     (setf (sheet-direct-mirror ,sheet) (pixmap-mirror ,pixmap))
	     ,@body
	     (setf (sheet-direct-mirror ,sheet) ,sheet-mirror)
             ; v-- the sheet might have been degrafted while we weren't looking
             ; in which case, we shouldn't blit it back
	     (copy-from-pixmap ,pixmap 0 0 ,width ,height ,sheet 0 0))
	 (deallocate-pixmap ,pixmap)))))

;;; Another attempt

(defun invoke-with-double-buffering (sheet continuation x1 y1 x2 y2)
  (let* ((medium (sheet-medium sheet))
	 (sheet-transform (sheet-native-transformation sheet))
	 (medium-transform (medium-transformation (sheet-medium sheet)))
	 (world-transform (compose-transformations sheet-transform
						   medium-transform)))
    (multiple-value-bind (sheet-x1 sheet-y1)
	(transform-position world-transform x1 y1)
      (multiple-value-bind (sheet-x2 sheet-y2)
	  (transform-position world-transform x2 y2)
	;; Be conservative with the size of the pixmap, including all of
	;; the pixels at the edges.
	(let* ((pixmap-x1 (floor sheet-x1))
	       (pixmap-y1 (floor sheet-y1))
	       (pixmap-x2 (ceiling sheet-x2))
	       (pixmap-y2 (ceiling sheet-y2))
	       (pixmap-width (- pixmap-x2 pixmap-x1))
	       (pixmap-height (- pixmap-y2 pixmap-y1))
	       (current-sheet-region (sheet-region sheet))
	       (sheet-native (compose-transformation-with-translation
			      sheet-transform
			      (- pixmap-x1)
			      (- pixmap-y1)))
	       (pixmap (allocate-pixmap sheet pixmap-width pixmap-height))
	       )
	  (unless pixmap
	    (error "Couldn't allocate pixmap")) 
	  (multiple-value-bind (user-pixmap-x1 user-pixmap-y1)
	      (untransform-position world-transform pixmap-x1 pixmap-y1)
	    (multiple-value-bind (user-pixmap-x2 user-pixmap-y2)
		(untransform-position world-transform pixmap-x2 pixmap-y2)
	      (flet ((set-native (transform region sheet)
		       (%%set-sheet-native-transformation transform sheet)
		       (setf (slot-value sheet 'region) region)
		       (invalidate-cached-regions sheet)
		       (invalidate-cached-transformations sheet)))
		;; Assume that the scaling for the sheet-native
		;; transformation for the pixmap will be the same as that of
		;; the mirror .
		(unwind-protect
		     (letf (((sheet-parent sheet) nil)
			    ((sheet-direct-mirror sheet)
			     (pixmap-mirror pixmap)))
		       (unwind-protect
			    (let ((pixmap-region
				   (make-bounding-rectangle user-pixmap-x1
							    user-pixmap-y1
							    user-pixmap-x2
							    user-pixmap-y2)))
			      (set-native sheet-native pixmap-region  sheet)
			      ;(break)
			      (with-drawing-options
				  (medium :ink (medium-background medium))
				
				(medium-draw-rectangle* medium
							user-pixmap-x1
							user-pixmap-y1
							user-pixmap-x2
							user-pixmap-y2
							t))
			      (funcall continuation sheet
				       user-pixmap-x1 user-pixmap-y1
				       user-pixmap-x2 user-pixmap-y2))
			 (set-native sheet-transform
				     current-sheet-region
				     sheet)))
		  (copy-from-pixmap pixmap 0 0
				    pixmap-width pixmap-height sheet
				    user-pixmap-x1 user-pixmap-y1)
		  (deallocate-pixmap pixmap))))))))))

(defmacro with-double-buffering (((sheet &rest bounds-args)
				  (&rest pixmap-args))
				 &body body)
  (with-gensyms (continuation)
    (let ((cont-form
	   (case (length pixmap-args)
	     (1
	      (with-gensyms (pixmap-x1 pixmap-y1 pixmap-x2 pixmap-y2)
		`(,continuation (,sheet
				 ,pixmap-x1 ,pixmap-y1 ,pixmap-x2 ,pixmap-y2)
		   (let ((,(car pixmap-args)
			  (make-bounding-rectangle ,pixmap-x1 ,pixmap-y1
						   ,pixmap-x2 ,pixmap-y2)))
		     ,@body))))
	     (4
	      `(,continuation (,sheet ,@pixmap-args)
		 ,@body))
	     (otherwise (error "Invalid pixmap-args ~S" pixmap-args)))))
      (case (length bounds-args)
	(1
	 (with-gensyms (x1 y1 x2 y2)
	   `(flet (,cont-form)
	      (declare (dynamic-extent #',continuation))
	      (with-bounding-rectangle* (,x1 ,y1 ,x2 ,y2)
		  ,(car bounds-args)
		(invoke-with-double-buffering ,sheet
					      #',continuation
					      ,x1 ,y1 ,x2 ,y2)))))
	(4
	 `(flet (,cont-form)
	    (declare (dynamic-extent #',continuation))
	    (invoke-with-double-buffering ,sheet #',continuation
					  ,@bounds-args)))
	(otherwise (error "invalid bounds-args ~S" bounds-args))))))



;;; Generic graphic operation methods

(defmacro def-graphic-op (name (&rest args))
  (let ((method-name (symbol-concat '#:medium- name '*)))
    `(eval-when (eval load compile)
       (defmethod ,method-name ((stream sheet) ,@args)
	 (with-sheet-medium (medium stream)
	   (,method-name medium ,@args))))))

(def-graphic-op draw-point (x y))
(def-graphic-op draw-points (coord-seq))
(def-graphic-op draw-line (x1 y1 x2 y2))
(def-graphic-op draw-lines (coord-seq))
(def-graphic-op draw-polygon (coord-seq closed filled))
(def-graphic-op draw-rectangle (left top right bottom filled))
(def-graphic-op draw-rectangles (position-seq filled))
(def-graphic-op draw-ellipse (center-x center-y
				  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				  start-angle end-angle filled))
(def-graphic-op draw-circle (center-x center-y radius start-angle end-angle filled))
(def-graphic-op draw-text (string x y
			       start end
			       align-x align-y
			       toward-x toward-y transform-glyphs))

;;;;
;;;; DRAW-DESIGN
;;;;

(defmethod draw-design (medium (design point) &rest options &key &allow-other-keys)
  (apply #'draw-point* medium (point-x design) (point-y design) options))

(defmethod draw-design (medium (design polyline) &rest options &key &allow-other-keys)
  (apply #'draw-polygon medium (polygon-points design)
         :closed (polyline-closed design)
         :filled nil
         options))

(defmethod draw-design (medium (design polygon) &rest options &key &allow-other-keys)
  (apply #'draw-polygon medium (polygon-points design)
         :closed (polyline-closed design)
         :filled t
         options))

(defmethod draw-design (medium (design line) &rest options &key &allow-other-keys)
  (multiple-value-bind (x1 y1) (line-start-point* design)
    (multiple-value-bind (x2 y2) (line-end-point* design)
      (apply #'draw-line* medium x1 y1 x2 y2 options))))

(defmethod draw-design (medium (design rectangle) &rest options &key &allow-other-keys)
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* design)
    (apply #'draw-rectangle* medium x1 y1 x2 y2 options)))

(defmethod draw-design (medium (design ellipse) &rest options &key &allow-other-keys)
  (multiple-value-bind (cx cy) (ellipse-center-point* design)
    (multiple-value-bind (r1x r1y r2x r2y) (ellipse-radii design)
      (multiple-value-call #'draw-ellipse* medium
                           cx cy r1x r1y r2x r2y
                           :start-angle (ellipse-start-angle design)
                           :end-angle (ellipse-end-angle design)
                           options))))

(defmethod draw-design (medium (design elliptical-arc) &rest options &key &allow-other-keys)
  (multiple-value-bind (cx cy) (ellipse-center-point* design)
    (multiple-value-bind (r1x r1y r2x r2y) (ellipse-radii design)
      (multiple-value-call #'draw-ellipse* medium
                           cx cy r1x r1y r2x r2y
                           :start-angle (ellipse-start-angle design)
                           :end-angle (ellipse-end-angle design)
                           :filled nil
                           options))))

(defmethod draw-design (medium (design standard-region-union) &rest options &key &allow-other-keys)
  (map-over-region-set-regions (lambda (region)
                                 (apply #'draw-design medium region options))
                               design))

(defmethod draw-design (medium (design standard-rectangle-set) &rest options &key &allow-other-keys)
  ;; ### we can do better (faster) than this.
  (map-over-region-set-regions (lambda (region)
                                 (apply #'draw-design medium region options))
                               design))

#+nyi
(defmethod draw-design (medium (design standard-region-intersection) &rest options &key &allow-other-keys)
  )

#+nyi
(defmethod draw-design (medium (design standard-region-difference) &rest options &key &allow-other-keys)
  )

(defmethod draw-design (medium (design (eql +nowhere+)) &rest options &key &allow-other-keys)
  (declare (ignore medium options)
           (ignorable design))
  nil)

(defmethod draw-design ((medium sheet) (design (eql +everywhere+)) &rest options &key &allow-other-keys)
  (apply #'draw-design medium (bounding-rectangle (sheet-region medium)) options))

(defmethod draw-design ((medium medium) (design (eql +everywhere+)) &rest options &key &allow-other-keys)
  (apply #'draw-design medium (bounding-rectangle (sheet-region (medium-sheet medium))) options))

;;;

(defmethod draw-design (medium (color color) &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :ink color options))

(defmethod draw-design (medium (color opacity) &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :ink color options))

(defmethod draw-design (medium (color standard-flipping-ink) &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :ink color options))

(defmethod draw-design (medium (color indirect-ink) &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :ink color options))

;;;;

(defun draw-pattern* (medium pattern x y &key clipping-region transformation)
  ;; Note: I believe the sample implementation in the spec is incorrect.
  ;; --GB
  (check-type pattern pattern)
  (cond ((or clipping-region transformation)
         (with-drawing-options (medium :clipping-region clipping-region
                                       :transformation transformation)
           ;; Now this is totally bogus.
           (medium-draw-pattern* medium pattern x y) ))
        (t
         ;; What happens if there is already a transformation
         ;; applied to the medium?
         (medium-draw-pattern* medium pattern x y))))

(defmethod medium-draw-pattern* (medium pattern x y)
  (let ((width  (pattern-width pattern))
        (height (pattern-height pattern)))
    #+NIL ;; debugging aid.
    (draw-rectangle* medium x y (+ x width) (+ y height)
                     :filled t
                     :ink +red+)
    (draw-rectangle* medium x y (+ x width) (+ y height)
                     :filled t
                     :ink (transform-region
                           (make-translation-transformation x y)
                           pattern))))
