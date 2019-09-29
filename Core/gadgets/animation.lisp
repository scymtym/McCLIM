(in-package #:clim-internals)

;;; Animation protocol

(defgeneric property-value (property thing)
  (:method ((property t) (thing t))
    (values nil nil)))

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

(defmethod property-value ((property t) (thing animation))
  (property-value property *theme*))

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
    (if (eq old-class new-class)
        (apply #'reinitialize-instance old :progress 0 initargs)
        (apply #'change-class old new-class :progress (- 1 (progress old)) initargs))
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
                             (sleep .04)))))

(defmethod property-value ((property t) (thing animated-mixin))
  (if-let ((values (loop :for animation :in (animations thing)
                         :for (value valuep) = (multiple-value-list
                                                (property-value property animation))
                         :when valuep
                         :collect value)))
    (values (first values) t)
    (property-value property *theme*)   ; (call-next-method)
    ))

;;; Background hack

(defmethod property-value ((property (eql :effective-foreground-ink)) (thing animated-mixin))
  (multiple-value-bind (value valuep) (call-next-method)
    (if valuep
        (values value t)
        (property-value :foreground-ink thing))))

(defmethod property-value ((property (eql :effective-background-ink)) (thing animated-mixin))
  (multiple-value-bind (value valuep) (call-next-method)
    (if valuep
        (values value t)
        (property-value :background-ink thing))))

(defmethod effective-gadget-foreground ((gadget animated-mixin))
  (multiple-value-bind (value valuep)
      (property-value :effective-foreground-ink gadget)
    (if valuep
        value
        (property-value :foreground-ink *theme*))))

(defmethod effective-gadget-background ((gadget animated-mixin))
  (multiple-value-bind (value valuep)
      (property-value :effective-background-ink gadget)
    (if valuep
        value
        (property-value :background-ink *theme*))))

#+no (defmethod effective-gadget-background ((gadget animation))
       nil)

#+no (defmethod effective-gadget-background :around ((gadget animated-mixin))
  (if-let ((animation (first (animations gadget))))
    (or (effective-gadget-background animation) (call-next-method))
    (call-next-method)))

;;; Example animations

(defclass arming (animation)
  ())

(defmethod property-value ((property (eql :effective-background-ink)) (thing arming))
  (let ((highlight-background-ink (property-value :highlight-background-ink thing))
        (background-ink (property-value :background-ink thing))
        (opacity (make-opacity (alexandria:clamp (progress thing) 0 1))))
    (values (compose-over (compose-in highlight-background-ink opacity)
                          background-ink)
            t)))

(defclass disarming (animation)
  ())

(defmethod property-value ((propery (eql :effective-background-ink)) (thing disarming))
  (let ((highlight-background-ink (property-value :highlight-background-ink thing))
        (background-ink (property-value :background-ink thing))
        (opacity (make-opacity (alexandria:clamp (progress thing) 0 1))))
    (values (compose-over (compose-in background-ink opacity)
                          highlight-background-ink)
            t)))

(defmethod transition :after ((gadget animated-mixin) (form not-armed) (to armed))
  (add-or-change-animation 'disarming 'arming gadget))

(defmethod transition :after ((gadget animated-mixin) (form armed) (to not-armed))
  (add-or-change-animation 'arming 'disarming gadget))

(defclass pressing (animation) ())

(defmethod property-value ((property (eql :pressed)) (thing pressing))
  (values (progress thing) t))

(defclass unpressing (animation) ())

(defmethod property-value ((property (eql :pressed)) (thing unpressing))
  (values (- 1 (progress thing)) t))

(defmethod transition :after ((gadget t) (from t) (to pressed+armed))
  (add-or-change-animation 'unpressing 'pressing gadget))

(defmethod transition :after ((gadget t) (from pressed+armed) (to t))
  (add-or-change-animation 'pressing 'unpressing gadget))
