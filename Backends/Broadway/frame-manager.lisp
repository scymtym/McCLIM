(cl:in-package #:clim-broadway)

(defclass broadway-frame-manager (frame-manager)
  ())

(defclass top-level-sheet-pane (mirrored-sheet-mixin
                                climi::top-level-sheet-pane)
  ()
  (:default-initargs
   :device-transformation +identity-transformation+)) ; TODO HACK

(defmethod make-pane-1 ((realizer            broadway-frame-manager)
                        (frame               application-frame)
                        (abstract-class-name (eql 'climi::top-level-sheet-pane))
                        &rest initargs)
  (apply #'make-instance 'top-level-sheet-pane
         :frame   frame
         :manager realizer
         :port    (port frame)
         initargs))

(defmethod make-pane-1 ((realizer            broadway-frame-manager)
                        (frame               application-frame)
                        (abstract-class-name t)
                        &rest initargs)
  (apply #'make-instance (clim-clx::find-concrete-pane-class abstract-class-name)
         :frame   frame
         :manager realizer
         :port    (port frame)
         initargs))
