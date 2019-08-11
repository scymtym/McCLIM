(cl:in-package #:clim-broadway)

(defclass surface-manager ()
  ((%next-surface-id :accessor next-surface-id
                     :type     (integer 1)
                     :initform 1)
   (%surfaces        :reader   surfaces
                     :initform '())
   ;; Focus
   (%focused-surface :accessor focused-surface
                     :initform nil)))

(defmethod make-surface ((surface-manager surface-manager))
  (let ((id (next-surface-id surface-manager)))
    (setf (next-surface-id surface-manager) (1+ id))
    (make-instance 'surface :id id)))
