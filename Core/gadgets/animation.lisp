(in-package #:clim-internals)

(defclass animation-tick (standard-event)
  ((%dt :initarg :dt
        :reader dt)))

(defclass animation ()
  ((%gadget :initarg :gadget
            :reader gadget)
   (%progress :accessor progress
              :initform 0.0)))

(defmethod print-object ((object animation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~,0,2F %" (progress object))))

(defmethod update ((animation animation) (event animation-tick))
  (incf (progress animation) (dt event)))

(defmethod effective-gadget-background ((gadget animation))
  nil)

(defclass arming (animation)
  ())

(defmethod effective-gadget-background ((gadget arming))
  (let ((opacity (make-opacity (alexandria:clamp (progress gadget) 0 1))))
    (compose-over (compose-in (gadget-highlight-background (gadget gadget)) opacity)
                  (pane-background (gadget gadget)))))

(defclass disarming (animation)
  ())

(defmethod effective-gadget-background ((gadget disarming))
  (let ((opacity (make-opacity (alexandria:clamp (progress gadget) 0 1))))
    (compose-over (compose-in (pane-background (gadget gadget)) opacity)
                  (gadget-highlight-background (gadget gadget)))))

;;;

(defclass animated-mixin ()
  ((%current-animation :accessor current-animation
                       :initform nil)))

(defmethod (setf current-animation) :after ((new-value t) (object animated-mixin))
  (when new-value
    (animate object)))

(defmethod effective-gadget-background :around ((gadget animated-mixin))
  (if-let ((animation (current-animation gadget)))
    (or (effective-gadget-background animation) (call-next-method))
    (call-next-method)))

(defmethod handle-event ((gadget animated-mixin) (event animation-tick))
  (when-let ((animation (current-animation gadget)))
    (cond ((>= (progress animation) 1)
           (setf (current-animation gadget) nil))
          (t
           (update animation event)
           (dispatch-repaint gadget +everywhere+)))
    ; (setf (clouseau:root-object *inspector* :run-hook-p t) gadget)
    ))

(defmethod transition :after ((gadget animated-mixin) (form not-armed) (to armed))
  (setf (current-animation gadget) (make-instance 'arming :gadget gadget)))

(defmethod transition :after ((gadget animated-mixin) (form armed) (to not-armed))
  (setf (current-animation gadget) (make-instance 'disarming :gadget gadget)))

(defun animate (gadget)
  (bt:make-thread (lambda ()
                    (loop while (current-animation gadget)
                          do (queue-event gadget (make-instance 'animation-tick :sheet gadget :dt .01))
                             (sleep .01)))))
