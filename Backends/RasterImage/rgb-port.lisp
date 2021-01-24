;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2016,2017 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2019,2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;  (c) copyright 2020 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Port class for RGB images for the raster image backend.

(in-package :mcclim-raster-image)

;;; Port

(defclass rgb-image-port (raster-image-port)
  ())

(defmethod find-port-type ((type (eql :rgb-image)))
  (values 'rgb-image-port 'identity))

(defmethod realize-mirror ((port rgb-image-port) sheet)
  (setf (sheet-parent sheet) (graft port))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (port-register-mirror port sheet mirror)
    (%make-image mirror sheet)))

;;; Pixmap

(defclass rgb-image-pixmap (image-pixmap-mixin basic-pane)
  ((region :initform +nowhere+)))

(defmethod port-allocate-pixmap ((port rgb-image-port) sheet width height)
  (let ((pixmap (make-instance 'rgb-image-pixmap
                               :sheet sheet
                               :width width
                               :height height
                               :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port rgb-image-port) pixmap)
  (when (climi::port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))
