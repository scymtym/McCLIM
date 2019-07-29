(cl:in-package #:clim-broadway)

(defclass port2 (mcclim-render-internals::render-port-mixin
                 climi::standard-port)
  ())

;;;

(defclass broadway-medium (mcclim-render-internals::render-medium-mixin
                           basic-medium)
  ())

(defmethod make-medium ((port port2) sheet)
  (make-instance 'broadway-medium
                 ;; :port port
                 ;; :graft (find-graft :port port)
                 :sheet sheet))

;;; Pixmap

(defmethod clim:destroy-mirror ((port port2) (pixmap mcclim-render-internals::image-pixmap-mixin))
  (call-next-method))

(defmethod clim:realize-mirror ((port port2) (pixmap mcclim-render-internals::image-pixmap-mixin))
  ; (setf (sheet-parent pixmap) (graft port))
  (let ((mirror (make-instance 'mcclim-render-internals::image-mirror-mixin)))
    (climi::port-register-mirror port pixmap mirror)
    (mcclim-render-internals::%make-image mirror pixmap)))

(defmethod climb:port-allocate-pixmap ((port port2) sheet width height)
  (let ((pixmap (make-instance 'broadway-pixmap
                               :region (make-rectangle* 0 0 width height)
                               :sheet sheet
                               :width width
                               :height height
                               :port port)))
    (when t ; (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defclass broadway-pixmap (mcclim-render-internals::image-pixmap-mixin
                           basic-pane)
  ())
