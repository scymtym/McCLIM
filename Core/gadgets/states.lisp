(in-package #:clim-internals)

(defclass stateful-mixin ()
  ((%state :initarg :state
           :accessor state)))

(defmethod (setf state) :around ((new-value t) (gadget stateful-mixin))
  (let ((old-state (state gadget)))
    (prog1
        (call-next-method)
      (unless (eq new-value old-state)
        (transition gadget old-state new-value)))))

(defmethod transition ((gadget stateful-mixin) from to))

(defmethod transition :around ((gadget stateful-mixin) from to)
  (leave gadget from)
  (prog1
      (call-next-method)
    (leave gadget to)
    ; (setf (clouseau:root-object *inspector* :run-hook-p t) gadget)
    ))

(defmethod enter ((gadget stateful-mixin) state)) ; TODO progn combination?

(defmethod leave ((gadget stateful-mixin) state))

;;;

(defclass state-machine-mixin ()
  ())

(defmethod update-gadget-state ((gadget state-machine-mixin) new-state repaint)
  (when new-state
    (setf (state gadget) new-state))
  (cond ((not repaint))
        ((regionp repaint)
         (dispatch-repaint gadget repaint))
        (t
         (dispatch-repaint gadget (or (pane-viewport-region gadget)
                                      (sheet-region gadget))))))

(defmethod handle-event-using-state ((gadget state-machine-mixin) state event)
  (values nil nil))

(defmethod handle-event-using-state :around ((gadget state-machine-mixin)
                                             state
                                             event)
  (multiple-value-bind (new-state repaint) (call-next-method)
    (update-gadget-state gadget new-state repaint)
    (values new-state repaint)))

;;;

(defclass sfm-gadget-mixin (stateful-mixin state-machine-mixin)
  ()
  (:default-initargs :state (make-instance 'not-armed)))

(defmethod handle-event ((client sfm-gadget-mixin) (event t))
  (if (typep event 'window-repaint-event) ; TODO hack
      (call-next-method)
      (handle-event-using-state client (state client) event)))

;;;

(defclass activate/deactivate-transitions-mixin () ())

(defmethod activate-gadget ((gadget sfm-gadget-mixin))
  (let ((new-state (if (eq gadget (port-pointer-sheet (port gadget)))
                       (make-instance 'not-armed)
                       (make-instance 'armed))))
    (update-gadget-state gadget new-state t)))

(defmethod deactivate-gadget ((gadget sfm-gadget-mixin))
  (update-gadget-state gadget (make-instance 'inactive) t))

(defmethod gadget-active-p ((gadget sfm-gadget-mixin))
  (typep (state gadget) 'active))

;;;

(defclass enter/exit-transitions-mixin () ())

(defmethod handle-event-using-state ((gadget enter/exit-transitions-mixin)
                                     (state not-armed)
                                     (event pointer-enter-event))
  (values (make-instance (armed-state state)) t))

(defmethod handle-event-using-state ((gadget enter/exit-transitions-mixin)
                                     (state armed)
                                     (event pointer-exit-event))
  (values (make-instance (not-armed-state state)) t))

(defmethod gadget-armed-p ((gadget enter/exit-transitions-mixin))
  (typep (state gadget) 'armed)) ; TODO

;;;

(defclass press/release-transitions-mixin () ())

(defmethod handle-event-using-state ((pane  press/release-transitions-mixin)
                                     (state not-armed)
                                     (event pointer-button-press-event))
  (values (make-instance 'pressed+not-armed) t))

(defmethod handle-event-using-state ((pane  press/release-transitions-mixin)
                                     (state armed)
                                     (event pointer-button-press-event))
  (values (make-instance 'pressed+armed) t))

(defmethod handle-event-using-state ((pane  press/release-transitions-mixin)
                                     (state pressed+not-armed)
                                     (event pointer-button-release-event))
  (values (make-instance 'not-armed) t))

(defmethod handle-event-using-state ((pane  press/release-transitions-mixin)
                                     (state pressed+armed)
                                     (event pointer-button-release-event))

  (values (make-instance 'armed) t))

(defmethod handle-event-using-state :around ((pane action-gadget)
                                             (state pressed+armed)
                                             (event pointer-button-release-event)) ; TODO should not be :around
  (multiple-value-prog1
      (call-next-method)
    (activate-callback pane (gadget-client pane) (gadget-id pane))))

;;;

(defclass inactive () ())

(defmethod transition ((gadget t) (from active) (to inactive))
  (note-gadget-deactivated (gadget-client gadget) gadget))

(defclass active () ())

(defmethod transition ((gadget t) (from inactive) (to active))
  (note-gadget-activated (gadget-client gadget) gadget))

(defclass armed (active)
  ((%not-armed-state :allocation :class
                     :reader not-armed-state
                     :initform 'not-armed)))

(defmethod transition ((gadget t) (from not-armed) (to armed))
                                        ; (note-gadget-armed )
  ;; (armed-callback)
  )

(defclass not-armed (active)
  ((%armed-state :allocation :class
                 :reader armed-state
                 :initform 'armed)))

(defmethod transition ((gadget t) (from armed) (to not-armed))
  ;; (disarmed-callback)
  )

(defclass pressed (active) ())

(defclass pressed+not-armed (pressed not-armed)
  ((%armed-state :allocation :class
                 :initform 'pressed+armed)))

(defclass pressed+armed (pressed armed)
  ((%not-armed-state :allocation :class
                     :initform 'pressed+not-armed)))

(defclass pressing (pressed)
  ((%progress :initarg :progress :accessor progress :initform 0)))
