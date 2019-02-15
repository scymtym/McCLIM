;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

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

(defmethod run-hook ((inspector-state inspector-state) (root-place t))
  (when-let ((change-hook (change-hook inspector-state)))
    (map nil (rcurry #'funcall root-place) change-hook)))

(defmethod root-place ((inspector-state inspector-state) &key run-hook?)
  (declare (ignore run-hook?))
  (%root-place inspector-state))

(defmethod (setf root-place) ((new-value t) (inspector-state inspector-state)
                              &key run-hook?)
  (setf (%root-place inspector-state) new-value)
  (when run-hook?
    (run-hook inspector-state new-value)))

(defmethod root-object ((inspector-state inspector-state) &key run-hook?)
  (declare (ignore run-hook?))
  (value (root-place inspector-state)))

(defmethod (setf root-object) ((new-value t) (inspector-state inspector-state)
                               &key run-hook?)
  (setf (root-place inspector-state :run-hook? run-hook?)
        (make-instance 'root-place :cell new-value)))
