(cl:in-package #:clim-broadway)

(defclass broadway-frame-manager (frame-manager)
  ())

;;; Frames

(defmethod adopt-frame :before ((frame-manager broadway-frame-manager)
                                (frame         climi::menu-frame))
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (setf (slot-value frame 'climi::left) 1 ; HACK can't result in identity transformation?
          (slot-value frame 'climi::top)  1)))

;;; Panes

(defclass top-level-sheet-pane (; climi::always-repaint-background-mixin
                                mirrored-sheet-mixin
                                climi::double-buffering-mixin
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
