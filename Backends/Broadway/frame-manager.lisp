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

(defclass top-level-sheet-pane (climi::always-repaint-background-mixin
                                mirrored-sheet-mixin
                                climi::top-level-sheet-pane)
  ()
  (:default-initargs
   :device-transformation +identity-transformation+)) ; TODO HACK

(defclass get-frame-data-event (climi::standard-event)
  ((%pixels   :initarg :pixels
              :reader  pixels)
   (%callback :initarg :callback
              :reader  callback)))

(define-application-frame buffer-view ()
  ((%buffer :initarg :buffer :accessor buffer)
   (%count :accessor counter :initform 0))
  (:panes (pixels (make-pane :application)))
  (:layouts
   (:default pixels)))

(defmethod redisplay-frame-pane ((frame buffer-view) pane &key force-p)
  (draw-design pane (make-instance 'clime:image-pattern :array (buffer frame)))
  (setf (stream-cursor-position pane) (values 400 400))
  (let ((*print-length* 3)
        (*print-level* 3))
    (format pane "~X   @~D~%"
            (sb-vm::get-lisp-obj-address (buffer frame))
            (incf (counter frame)))
    (finish-output pane)))

(defvar *buffer-view* nil)

(defmethod handle-event ((client top-level-sheet-pane)
                         (event  get-frame-data-event))
  (let* ((mirror     (sheet-mirror client))
         (old-pixels (                  ; TODO clime:pattern-array
                      slot-value
                      (mcclim-render-internals::image-mirror-image mirror)
                      'climi::array))
         (new-pixels (pixels event))
         (dirty      nil))
    (declare (cl:type argb-pixel-array old-pixels new-pixels))
    #+no (cond ((null *buffer-view*)
           (setf *buffer-view* (make-application-frame 'buffer-view
                                                       :buffer old-pixels
                                                       :width 800 :height 600))
           (bt:make-thread (lambda () (run-frame-top-level *buffer-view*))))
          (t
           (setf (buffer *buffer-view*) old-pixels)
           (redisplay-frame-pane *buffer-view* (find-pane-named *buffer-view* 'pixels))))
    (replace (make-array (array-total-size new-pixels) :element-type 'argb-pixel :displaced-to new-pixels :displaced-index-offset 0)
             (make-array (array-total-size old-pixels) :element-type 'argb-pixel :displaced-to old-pixels :displaced-index-offset 0))
    (rotatef dirty (slot-value mirror 'mcclim-render-internals::dirty-region))
    #+no (rotatef pixels (              ; TODO clime:pattern-array
                          slot-value
                          (mcclim-render-internals::image-mirror-image mirror)
                          'climi::array))
    (setf (                             ; TODO clime:pattern-array
           slot-value
           (mcclim-render-internals::image-mirror-image mirror)
           'climi::array)
          new-pixels)
    (funcall (callback event) old-pixels dirty)))

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
