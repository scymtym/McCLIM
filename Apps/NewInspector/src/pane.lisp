(cl:in-package #:new-inspector)

(defclass inspector-pane (application-pane #+n clim-stream-pane) ; TODO can we be more specific?
  ((%state :reader   state
           :writer   (setf %state))))

(defmethod initialize-instance :before ((instance inspector-pane)
                                        &key
                                        (state nil state-supplied-p)
                                        (root  nil root-supplied-p))
  (declare (ignore state root))
  (unless (or state-supplied-p root-supplied-p)
    (error "~@<Exactly one of the initargs ~S and ~S must be ~
            supplied.~@:>"
           :state :root)))

(defmethod shared-instance :before ((instance   inspector-pane)
                                    (slot-names t)
                                    &key
                                    (state nil state-supplied-p)
                                    (root  nil root-supplied-p))
  (declare (ignore state root))
  (when (and state-supplied-p root-supplied-p)
    (error "~@<The initargs ~S and ~S are mutually exclusive.~@:>"
           :state :root)))

(defmethod shared-initialize :after ((instance   inspector-pane)
                                     (slot-names t)
                                     &key
                                     (state nil state-supplied-p)
                                     (root  nil root-supplied-p))
  (cond (state-supplied-p
         (setf (%state instance) state))
        (root-supplied-p
         (setf (%state instance) (make-instance 'inspector-state
                                                :root-object root)))))

(defmethod redisplay-frame-pane ((frame application-frame)
                                 (pane  inspector-pane)
                                 &key force-p)
  (declare (ignore force-p))
  (present-inspected-object-graph (state pane) pane))
