;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2016 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2018,2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Graft for the raster image backend.

(in-package #:mcclim-raster-image)

;;; Units

(defparameter *dot-per-millimeter* 2)

(defun unit-factor (unit graft)
  (ecase unit
    (:device       1)
    (:inches       (/ 0.0393701 *dot-per-millimeter*))
    (:millimeters  (/ *dot-per-millimeter*))
    (:screen-sized (/ (raster-image-graft-width graft)))))

;;; Graft

(defclass raster-image-graft (graft)
  ((width  :initarg :width :reader raster-image-graft-width)
   (height :initarg :height :reader raster-image-graft-height)))

(defmethod graft-orientation ((graft raster-image-graft))
  :graphics)

(defmethod graft-width ((graft raster-image-graft) &key (units :device))
  (* (raster-image-graft-width graft) (unit-factor units graft)))

(defmethod graft-height ((graft raster-image-graft) &key (units :device))
  (* (raster-image-graft-height graft) (unit-factor units graft)))
