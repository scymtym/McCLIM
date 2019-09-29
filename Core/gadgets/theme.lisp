(in-package #:clim-internals)

(defclass theme ()
  ((%properties :reader   %properties
                :initform (make-hash-table :test #'eq))))

(defmethod property-value ((property t) (thing theme))
  (gethash property (%properties thing)))

(defmethod (setf property-value) ((new-value t) (property t) (thing theme))
  (setf (gethash property (%properties thing)) new-value))

(defmethod (setf property-value) :around ((new-value t) (property t) (thing theme))
  (let* ((old-value      (property-value property thing))
         (old-properties (list (cons property old-value))))
    (call-next-method)
    (map-over-frames
     (lambda (frame)
       (map-over-sheets
        (lambda (sheet)
          (when (typep sheet 'animated-mixin)
            (add-animation (make-instance 'changing-theme
                                          :gadget         sheet
                                          :old-properties old-properties)
                           sheet)))
        (frame-top-level-sheet frame))))))

(defclass changing-theme (animation)
  ((%old-properties :initarg :old-properties
                    :reader  %old-properties)))

(defmethod property-value ((property t) (thing changing-theme))
  (let ((theme-value (property-value property *theme*)))
    (when-let ((old-cell (assoc property (%old-properties thing))))
      (let ((old-value (cdr old-cell))
            (p (progress thing)))
        (values
         (etypecase old-value
           (real (alexandria:lerp p old-value theme-value))
           (color (compose-over (compose-in theme-value (make-opacity p))
                                old-value)))
         t)))))

(defvar *theme* (make-instance 'theme))
