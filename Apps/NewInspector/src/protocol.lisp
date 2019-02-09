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

;;; Place protocol

(defgeneric supportsp (place operation))

(defgeneric accepts-value-p (place value))

(defgeneric valuep (place))

(defgeneric value (place))

(defgeneric (setf value) (new-value place))

(defgeneric make-unbound (place)) ; TODO remove-value?

;;; Object state protocol
;;;
;;; An object state instance holds information pertaining to a
;;; place-object pair.

(defgeneric place (state)
  (:documentation
   "Return the place to which STATE is associated."))

(defgeneric object (state)
  (:documentation
   "Return the object to which STATE is associated."))

(defgeneric state-applicable-p (state object place)
  (:documentation
   "Return true is STATE is suitable for OBJECT in PLACE."))

(defgeneric object-state-class (object place)
  (:documentation
   "Return the name of a state class suitable for OBJECT in PLACE."))

(defgeneric make-object-state (object place)
  (:documentation
   "Return a state instance suitable for OBJECT in PLACE."))

;;; Object inspection protocol

(defvar *place* nil)

(defvar *parent-place*)

(defvar *state*)

(defgeneric inspect-place (place stream))

(defgeneric inspect-object (object stream))

(defgeneric inspect-object-using-state (object state style stream)
  (:documentation
   "Foo bar"))

;;; Default behavior

(defmethod inspect-place ((place t) (stream t))
  (if (not (valuep place))
      (with-style (stream :unbound)
        (write-string "unbound" stream))
      (handler-case
          (let ((value   (value place))
                (*place* place))
            (inspect-object value stream))
        (nil (condition)
          (with-style (stream :error)
            (format stream "Could not inspect place: ~A" condition))))))

(defmethod inspect-object ((object t) (stream t))
  (let* ((place *place*)
         (state (ensure-state object place
                              (lambda ()
                                (make-object-state object place))))
         (style (style state)))
    (with-output-as-presentation (stream state (presentation-type-of state)
                                         :single-box t)
      (let ((*place*        nil)
            (*parent-place* place))
        (inspect-object-using-state object state style stream)))
    state))

;;; Inspector state protocol

(defgeneric root-place (inspector-state &key run-hook?))

(defgeneric (setf root-place) (new-value inspector-state &key run-hook?))

(defgeneric root-object (inspector-state &key run-hook?))

(defgeneric (setf root-object) (new-value inspector-state &key run-hook?))

;;;

(defgeneric present-inspected-object-graph (state stream &key view))

(defmethod present-inspected-object-graph
    (state stream
     &key
     (view (make-instance 'inspector-view)))
  (setf (stream-default-view stream) view) ; TODO restore old default view?
  (inspect-place (root-place state) stream)

  #+old (let* ((root-place  (root-place state))
         (root-object (value root-place))
         (*state*     state)
         (*place*     root-place))
    (setf (stream-default-view stream) view) ; TODO restore old default view?
    (inspect-object root-object stream)))

;;; Backward compatibility

(defgeneric inspect-object-briefly (object stream))
