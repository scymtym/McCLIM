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
;;;
;;; Introspection and mutation of place objects.

(defgeneric supportsp (place operation))

(defgeneric accepts-value-p (place value))

(defgeneric valuep (place))

(defun value-no-error-p (place) ; TODO call this safe-valuep?
  (ignore-errors (valuep place)))

(defgeneric value (place))

(defgeneric (setf value) (new-value place))

(defgeneric remove-value (place))

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
   "Return true if STATE is suitable for OBJECT in PLACE."))

(defgeneric object-state-class (object place)
  (:documentation
   "Return the name of a state class suitable for OBJECT in PLACE."))

(defgeneric make-object-state (object place)
  (:documentation
   "Return a state instance suitable for OBJECT in PLACE."))

;;; Object inspection protocol

(defvar *place* nil)

(defvar *parent-place*)

(defgeneric inspect-place (place stream))

(defgeneric inspect-object (object stream))

(defgeneric inspect-object-using-state (object state style stream)
  (:documentation
   "Present OBJECT to STREAM according to STATE and STYLE.

    STATE stores information that is permanently associated with
    OBJECT.

    STYLE on the other hand consists of transient information such as
    whether OBJECT is should be presented in expanded or collapsed
    form.

    STREAM is the stream to which OBJECT should be presented."))

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

(defvar *seen*)

(defmethod inspect-object ((object t) (stream t))
  (let* ((place *place*)
         (state (ensure-state object place
                              (lambda ()
                                (make-object-state object place))))
         (style (style state)))
    (let ((presentation (with-output-as-presentation (stream state (presentation-type-of state)
                                                             :single-box t)
                          (let ((*place*        nil)
                                (*parent-place* place))
                            (inspect-object-using-state object state style stream)))))

      (let ((occurrences (ensure-gethash object *seen* (cons nil '()))))
        (push presentation (cdr occurrences))
        (setf (slot-value state '%occurrences) occurrences))

      )
    state))

;;; Inspector state protocol

(defgeneric root-place (inspector-state &key run-hook?))

(defgeneric (setf root-place) (new-value inspector-state &key run-hook?))

(defgeneric root-object (inspector-state &key run-hook?))

(defgeneric (setf root-object) (new-value inspector-state &key run-hook?))

(defgeneric change-hook (inspector-state))

;;;

(defgeneric present-inspected-object-graph (state stream &key view))

(defmethod present-inspected-object-graph
    (state stream
     &key
     (view (make-instance 'inspector-view)))
  (setf (stream-default-view stream) view) ; TODO restore old default view?
  (let ((*seen* (make-hash-table :test #'eq)))
    (inspect-place (root-place state) stream))

  #+old (let* ((root-place  (root-place state))
         (root-object (value root-place))
         (*state*     state)
         (*place*     root-place))
    (setf (stream-default-view stream) view) ; TODO restore old default view?
    (inspect-object root-object stream)))

;;; Backward compatibility

(defgeneric inspect-object-briefly (object stream))
