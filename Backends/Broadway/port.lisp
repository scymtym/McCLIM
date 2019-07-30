(cl:in-package #:clim-broadway)

(defclass broadway-port (mcclim-render-internals::render-port-mixin
                         climi::standard-port)
  ())

;;;

(defclass broadway-medium (mcclim-render-internals::render-medium-mixin
                           basic-medium)
  ())

(defmethod make-medium ((port broadway-port) sheet)
  (make-instance 'broadway-medium
                 ;; :port port
                 ;; :graft (find-graft :port port)
                 :sheet sheet))

;;; Pixmap

(defmethod clim:destroy-mirror ((port broadway-port)
                                (mirrored-sheet mcclim-render-internals::image-pixmap-mixin))
  (let ((mirror (sheet-direct-mirror mirrored-sheet)))
    (climi::port-unregister-mirror port mirrored-sheet mirror)))

(defmethod clim:realize-mirror ((port broadway-port)
                                (mirrored-sheet mcclim-render-internals::image-pixmap-mixin))
  ; (setf (sheet-parent pixmap) (graft port))
  (let ((mirror (make-instance 'mcclim-render-internals::image-mirror-mixin)))
    (climi::port-register-mirror port mirrored-sheet mirror)
    (mcclim-render-internals::%make-image mirror mirrored-sheet)))

(defmethod climb:port-allocate-pixmap ((port broadway-port) sheet width height)
  (let ((pixmap (make-instance 'broadway-pixmap
                               :region (make-rectangle* 0 0 width height)
                               :sheet sheet
                               :width width
                               :height height
                               :port port)))
    (when t ; (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod climb:port-deallocate-pixmap ((port broadway-port)
                                         (pixmap mcclim-render-internals::image-pixmap-mixin))
  (when (climi::port-lookup-mirror port pixmap)
    (climb:destroy-mirror port pixmap)))

(defclass broadway-pixmap (mcclim-render-internals::image-pixmap-mixin
                           basic-pane)
  ())
