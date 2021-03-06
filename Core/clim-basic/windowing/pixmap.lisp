;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 Iban HATCHONDO <hatchond@mei.u-bordeaux.fr>
;;;  (c) copyright 2001 Arnaud Rouanet
;;;  (c) copyright 2002 Brian Spilsbury
;;;  (c) copyright 2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2019 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Backend-independent pixmap functionality.

(in-package #:clim-internals)

(defclass pixmap ()
  ((sheet :initarg :sheet :reader pixmap-sheet)
   (width :initarg :width :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)))

(defgeneric pixmap-mirror (mirrored-pixmap))
(defgeneric allocate-pixmap (sheet width height))
(defgeneric deallocate-pixmap (pixmap))
(defgeneric copy-to-pixmap (medium medium-x medium-y width height
                            &optional pixmap pixmap-x pixmap-y))
(defgeneric copy-from-pixmap (pixmap from-x from-y width height
                              medium medium-x medium-y))
(defgeneric copy-area (medium from-x from-y width height to-x to-y))
(defgeneric medium-copy-area (from-drawable from-x from-y width height
                              to-drawable to-x to-y))

(defclass mirrored-pixmap (pixmap)
  ((port :initform nil :initarg :port :accessor port)
   (medium :initform nil :accessor pixmap-medium)
   (region :initform nil :accessor sheet-region)))

;;; added this. CHECKME -- BTS
(defmethod (setf %sheet-medium) (value (pixmap mirrored-pixmap))
  (setf (slot-value pixmap 'medium) value))

(defmethod invalidate-cached-transformations ((sheet mirrored-pixmap))
  (values))

(defmethod invalidate-cached-regions ((sheet mirrored-pixmap))
  (values))

(defmethod invoke-with-sheet-medium-bound
    (continuation medium (sheet mirrored-pixmap))
  (funcall continuation (pixmap-medium sheet)))

;;; BTS stopped adding. ^-- CHECKME

(defmethod initialize-instance :after ((pixmap mirrored-pixmap) &rest args)
  (declare (ignore args))
  (with-slots (width height region) pixmap
    (setf region (make-bounding-rectangle 0 0 width height))))

(defmethod pixmap-mirror ((pixmap mirrored-pixmap))
  (port-lookup-mirror (port pixmap) pixmap))

(defmethod allocate-pixmap ((sheet sheet) width height)
  (port-allocate-pixmap (port sheet) sheet width height))

(defmethod deallocate-pixmap ((pixmap pixmap))
  (port-deallocate-pixmap (port (medium-sheet pixmap)) pixmap))

(defmethod deallocate-pixmap ((pixmap mirrored-pixmap))
  (port-deallocate-pixmap (port pixmap) pixmap))

(defmethod sheet-native-transformation ((pixmap mirrored-pixmap))
  +identity-transformation+)

(defmethod sheet-native-region ((pixmap mirrored-pixmap))
  (make-rectangle* 0 0
                   (pixmap-width pixmap)
                   (pixmap-height pixmap)))

(defmethod sheet-device-transformation ((pixmap mirrored-pixmap))
  (medium-transformation (pixmap-medium pixmap)))

(defmethod sheet-device-region ((pixmap mirrored-pixmap))
  (region-intersection
   (sheet-native-region pixmap)
   (transform-region
    (sheet-device-transformation pixmap)
    (medium-clipping-region (pixmap-medium pixmap)))))

(defmethod sheet-direct-mirror ((pixmap mirrored-pixmap))
  (port-lookup-mirror (port pixmap) pixmap))

(defmethod sheet-mirror ((pixmap mirrored-pixmap))
  (port-lookup-mirror (port pixmap) pixmap))
