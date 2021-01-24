;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2016,2017 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A specialized extended output stream for the raster image backend.

(in-package #:mcclim-raster-image)

;;; Stream

(in-package #:mcclim-raster-image)

(defclass raster-image-stream (basic-pane
                               sheet-leaf-mixin
                               sheet-mute-input-mixin
                               permanent-medium-sheet-output-mixin
                               sheet-mute-repainting-mixin
                               updating-output-stream-mixin
                               standard-extended-output-stream
                               standard-output-recording-stream)
  ())

(defgeneric make-raster-image-stream (port))

(defmethod make-raster-image-stream (port)
  (make-instance 'raster-image-stream :port port))

;;; ?

(defmethod pane-viewport ((stream raster-image-stream))
  nil)

(defmethod scroll-extent ((stream raster-image-stream) x y)
  (declare (ignore x y))
  (values))
