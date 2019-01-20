(cl:in-package #:new-inspector)

(defclass inspector-pane (application-pane #+n clim-stream-pane) ; TODO can we be more specific?
  ((%state :reader   state
           :writer   (setf %state))))

(defmethod initialize-instance :after ((instance inspector-pane)
                                        &key
                                        (state nil state-supplied-p)
                                        (root  nil root-supplied-p))
  (declare (ignore state root))
  (unless (or state-supplied-p root-supplied-p)
    (setf (%state instance) (make-instance 'inspector-state))
    #+no (error "~@<Exactly one of the initargs ~S and ~S must be ~
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

(defmethod (setf %state) :after ((new-value t) (object inspector-pane))
  (push (lambda (new-root-place)
          (declare (ignore new-root-place))
          (queue-redisplay object))
        (change-hook new-value)))

(defmethod redisplay-frame-pane ((frame application-frame)
                                 (pane  inspector-pane)
                                 &key force-p)
  (declare (ignore force-p))
  (present-inspected-object-graph (state pane) pane))

;;; Redisplay

(defclass redisplay-event (climi::standard-event) ())

(defmethod queue-redisplay ((pane inspector-pane))
  (queue-event pane (make-instance 'redisplay-event :sheet pane)))

(defmethod handle-event ((client inspector-pane) (event redisplay-event))
  (redisplay-frame-pane (pane-frame client) client :force-p t))
