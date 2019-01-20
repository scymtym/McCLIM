(cl:in-package #:new-inspector)

;;; `inspector-state'
;;;
;;; The state is just the tree of inspected places and objects,
;;; starting at the root place.

(defclass inspector-state ()
  ((%root-place  :accessor %root-place)
   ;; Change hook
   (%change-hook :initarg  :change-hook
                 :type     list #|of function|#
                 :accessor change-hook
                 :initform '())))

(defmethod initialize-instance :after ((instance inspector-state)
                                       &key
                                       (root-object nil root-object-supplied-p))
  (declare (ignore root-object))
  (unless root-object-supplied-p
    (setf (root-place instance) (make-instance 'root-place))))

(defmethod shared-initialize :after ((instance   inspector-state)
                                     (slot-names t)
                                     &key
                                     (root-object nil root-object-supplied-p))

  (when root-object-supplied-p
    (setf (root-place instance)
          (make-instance 'root-place :cell root-object))))

(defmethod root-place ((inspector-state inspector-state) &key run-hook?)
  (declare (ignore run-hook?))
  (%root-place inspector-state))

(defmethod (setf root-place) ((new-value t) (inspector-state inspector-state)
                              &key run-hook?)
  (setf (%root-place inspector-state) new-value)
  (when run-hook?
    (when-let ((change-hook (change-hook inspector-state)))
      (map nil (rcurry #'funcall new-value) change-hook))))

(defmethod root-object ((inspector-state inspector-state) &key run-hook?)
  (declare (ignore run-hook?))
  (value (root-place inspector-state)))

(defmethod (setf root-object) ((new-value t) (inspector-state inspector-state)
                               &key run-hook?)
  (setf (root-place inspector-state :run-hook? run-hook?)
        (make-instance 'root-place :cell new-value)))
