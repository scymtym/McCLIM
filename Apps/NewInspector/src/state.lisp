(cl:in-package #:new-inspector)

;;; `inspector-state'
;;;
;;; The state is just the tree of inspected places and objects,
;;; starting at the root place.

(defclass inspector-state ()
  ((%root-place :accessor root-place)))

(defmethod shared-initialize :after ((instance   inspector-state)
                                     (slot-names t)
                                     &key
                                     (root-object nil root-object-supplied-p))
  (when root-object-supplied-p
    (setf (root-place instance)
          (make-instance 'root-place :cell root-object))))
