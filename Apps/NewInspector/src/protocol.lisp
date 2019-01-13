(cl:in-package #:new-inspector)

;;; Place protocol

(defgeneric supportsp (place operation))

(defgeneric accepts-value-p (place value))

(defgeneric valuep (place))

(defgeneric value (place))

(defgeneric (setf value) (new-value place))

(defgeneric make-unbound (place)) ; TODO remove-value?

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

(defgeneric root (inspector-state))

#+no (defgeneric object-state (inspector-state))

;;;

(defgeneric present-inspected-object-graph (state stream &key view))

(defmethod present-inspected-object-graph
    (state stream
     &key
     (view (make-instance 'inspector-view)))
  (let* ((root-place  (root-place state))
         (root-object (value root-place))
         (*state*     state)
         (*place*     root-place))
    (setf (stream-default-view stream) view) ; TODO restore old default view?
    (inspect-object root-object stream)))

;;; Backward compatibility

(defgeneric inspect-object-briefly (object stream))
