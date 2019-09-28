(in-package #:clim-internals)

;;; Animation protocol

(defgeneric property-value (property thing)
  (:method ((property t) (thing t))
    0))

(defgeneric update (thing event))

;;; Animated protocol

(defgeneric animations (animated))

(defgeneric add-animation (animation animated))

;;;

(defclass animation-tick (standard-event)
  ((%dt :initarg :dt
        :reader  dt)))

;;;

(defclass animation ()
  ((%gadget   :initarg :gadget ; TODO avoid if possible
              :reader gadget)
   (%progress :initarg  :progress
              :accessor progress
              :initform 0.0)))

(defmethod print-object ((object animation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~,0,2F %" (progress object))))
(compile nil '(lambda () (let* ((old-progress (progress animation))
                                (new-progress old-progress (dt event))))))

(defmethod update ((animation animation) (event animation-tick))
  (let* ((old-progress (progress animation))
         (new-progress (+ old-progress (dt event))))
    (cond ((>= old-progress 1)
           nil)
          ((> new-progress 1)
           (setf (progress animation) 1)
           t)
          (t
           (setf (progress animation) new-progress)
           t))))

;;;

(defclass animated-mixin ()
  ((%current-animation :accessor current-animation
                       :initform nil)
   (%animations        :reader   animations
                       :accessor %animations
                       :initform '())))

(defmethod (setf current-animation) :after ((new-value t) (object animated-mixin))
  (when new-value
    (animate object)))

(defmethod add-animation (animation (animated animated-mixin))
  (push animation (%animations animated))
  (when (alexandria:length= 1 (animations animated))
    (animate animated)))

(defmethod add-or-change-animation (old-class new-class (animated animated-mixin)
                                    &rest initargs &key &allow-other-keys)
  (if-let ((old (find-if (alexandria:of-type old-class) (animations animated))))
    (apply #'change-class old new-class :progress (- 1 (progress old)) initargs)
    (let ((new (apply #'make-instance new-class :gadget animated initargs)))
      (add-animation new animated)
      new)))

(defmethod update ((thing animated-mixin) event)
  (alexandria:when-let* ((old-animations (animations thing))
                         (retired        (remove-if (alexandria:rcurry #'update event)
                                                    old-animations)))
    (setf (%animations thing) (set-difference old-animations retired)))
  (not (null (%animations thing))))

(defmethod handle-event ((gadget animated-mixin) (event animation-tick))
  (when-let ((animation (current-animation gadget)))
    (cond ((>= (progress animation) 1)
           (setf (current-animation gadget) nil))
          (t
           (update animation event)
           (dispatch-repaint gadget +everywhere+))))
  (when (update gadget event)
    (dispatch-repaint gadget +everywhere+)))

(defun animate (gadget)
  (bt:make-thread (lambda ()
                    (loop while (or (current-animation gadget) (animations gadget))
                          do (queue-event gadget (make-instance 'animation-tick :sheet gadget :dt .1))
                             (sleep .03)))))

;;; Background hack

(defmethod effective-gadget-background ((gadget animation))
  nil)

(defmethod effective-gadget-background :around ((gadget animated-mixin))
  (if-let ((animation (first (animations gadget))))
    (or (effective-gadget-background animation) (call-next-method))
    (call-next-method)))

;;; Example animations

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

(defmethod transition :after ((gadget animated-mixin) (form not-armed) (to armed))
  (add-or-change-animation 'disarming 'arming gadget))

(defmethod transition :after ((gadget animated-mixin) (form armed) (to not-armed))
  (add-or-change-animation 'arming 'disarming gadget))

(defclass pressing (animation) ())

(defmethod property-value ((property (eql :pressed)) (thing pressing))
  (progress thing))

(defclass unpressing (animation) ())

(defmethod property-value ((property (eql :pressed)) (thing unpressing))
  (- 1 (progress thing)))

(defmethod transition :after ((gadget t) (from t) (to pressed+armed))
  (add-or-change-animation 'unpressing 'pressing gadget))

(defmethod transition :after ((gadget t) (from pressed+armed) (to t))
  (add-or-change-animation 'pressing 'unpressing gadget))
