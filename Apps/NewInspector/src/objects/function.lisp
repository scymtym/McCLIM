(cl:in-package #:new-inspector)

;;; Utilities

(defun tracedp (function)
  (gethash function sb-debug::*traced-funs*))

;;; `method-place'

(defclass method-place (key-value-place)
  (#+no (%qualifiers   :initarg :qualifiers
                       :reader  qualifiers)
   #+no (%specializers :initarg :specializers
                       :reader  specializers)))

(defmethod supportsp ((place method-place) (operation (eql 'setf)))
  nil)

(defmethod supportsp ((place method-place) (operation (eql 'remove-value)))
  t)

(defmethod value ((place method-place))
  (cell place) #+no (find-method (container place) (qualifiers place) (specializers place)))

(defmethod make-unbound ((place method-place))
  (remove-method (container place) (value place))) ; TODO store the method?

(defmethod qualifiers ((place method-place))
  (method-qualifiers (cell place)))

(defmethod specializers ((place method-place))
  (c2mop:method-specializers (cell place)))

;;; Object states

(defclass inspected-function (inspected-object)
  ())

(defmethod make-object-state ((object function) (place t))
  (make-instance 'inspected-function :place place))

(defclass inspected-generic-function (inspected-function
                                      inspected-instance)
  ())

(defmethod make-object-state ((object generic-function) (place t))
  (make-instance 'inspected-generic-function :place place :slot-style nil))

(defclass inspected-method (inspected-instance)
  ())

(defmethod make-object-state ((object method) (place t))
  (make-instance 'inspected-method :place place :slot-style nil))

(defmethod make-object-state ((object method) (place method-place))
  (make-instance 'inspected-method :place place :style :class-only :slot-style nil))

;;; Presentation types

(define-presentation-type inspected-generic-function () ; TODO needed?
  :inherit-from '(and inspected-function inspected-instance))

;;; Object inspection methods

(defmethod inspect-object-using-state ((object function)
                                       (state  inspected-function)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (call-next-method)
  ;; Traced
  (when (tracedp object)
    (write-string " " stream)
    (surrounding-output-with-border (stream :shape      :rounded
                                            :background +light-blue+
                                            :radius     2
                                            :padding    2)
      (with-drawing-options (stream :text-face :roman :text-size :smaller)
        (write-string "traced" stream))))
  ;; Interpreted
  (unless (compiled-function-p object)
    (write-string " " stream)
    (surrounding-output-with-border (stream :shape      :rounded
                                            :background +light-blue+
                                            :radius     2
                                            :padding    2)
      (with-drawing-options (stream :text-face :roman :text-size :smaller)
        (write-string "interpreted" stream)))))

(defmethod inspect-object-using-state ((object function)
                                       (state  inspected-function)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (multiple-value-bind (expression closurep name)
          (function-lambda-expression object)
        (let ((lambda-list (second expression)))
          ;; Name
          (formatting-row (stream)
            (formatting-place (stream nil 'pseudo-place name present inspect)
              (with-style (stream :slot-like)
                (formatting-cell (stream) (write-string "Name" stream))
                (formatting-cell (stream) (present stream)))
              (formatting-cell (stream) (inspect stream))))
          (terpri stream)
          ;; Lambda list
          (formatting-row (stream)
            (formatting-place (stream nil 'pseudo-place lambda-list present inspect)
              (with-style (stream :slot-like)
                (formatting-cell (stream) (write-string "Lambda list" stream))
                (formatting-cell (stream) (present stream)))
              (formatting-cell (stream) (inspect stream))))))
      ;; Type
      (formatting-row (stream)
        (formatting-place (stream nil 'pseudo-place (sb-introspect:function-type object) present inspect) ; TODO fresh list every time? => state is forgotten
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Type" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream))))))
  ;; Documentation
  (with-preserved-cursor-x (stream)
    (print-documentation object stream)))

(defmethod inspect-object-using-state ((object generic-function)
                                       (state  inspected-generic-function)
                                       (style  (eql :expanded-body))
                                       (stream t))
  ;;
  (call-next-method)

  ;; method class
  ;; method combination
  ;; Methods
  (with-section (stream) "Methods"
    (let ((methods (c2mop:generic-function-methods object)))
      (formatting-table (stream)
        (map nil (lambda (method)
                   (formatting-place (stream object 'method-place method present inspect
                                             :place-var place)
                     (formatting-row (stream)
                       (with-style (stream :slot-like)
                         (formatting-cell (stream)
                           (format-items (qualifiers place) :stream stream))
                         (map nil (lambda (specializer)
                                    (formatting-cell (stream)
                                      (typecase specializer
                                        (class (princ (class-name specializer) stream))
                                        (t     (prin1 `(eql ,(c2mop:eql-specializer-object specializer)) stream)))))
                              (specializers place)))
                       (formatting-cell (stream)
                         (present))
                       (formatting-cell (stream)
                         (inspect)))))
             methods))))
  ;; Slots
  (with-section (stream) "Slots"
    (inspect-slots object (slot-style state) stream))
  #+broken (call-next-method))

(defmethod inspect-object-using-state ((object method)
                                       (state  inspected-method)
                                       (style  (eql :class-only))
                                       (stream t))
  (princ (class-name (class-of object)) stream))

;;; Commands

(define-command (com-trace :command-table inspector
                           :name          "Trace Function")
    ((object 'inspected-function))
  (let ((object (object object)))
    (with-command-error-handling ("Could not trace ~A" object)
        (let ((name (nth-value 2 (function-lambda-expression object))))
          (sb-debug::trace-1 name (sb-debug::make-trace-info) object)))))

(define-presentation-to-command-translator inspected-function->com-trace
    (inspected-function com-trace inspector
     :tester ((object) (not (tracedp (object object))))
     :priority -1
     :documentation "Trace function"
     :pointer-documentation ((object stream)
                             (format stream "~@<Trace the function ~A~@:>"
                                     (object object))))
    (object)
  (list object))

(define-command (com-untrace :command-table inspector
                             :name          "Untrace Function")
    ((object 'inspected-function))
  (let ((object (object object)))
    (with-command-error-handling ("Could not untrace ~A" object)
        (let ((name (nth-value 2 (function-lambda-expression object))))
          (sb-debug::untrace-1 name))))) ; TODO

(define-presentation-to-command-translator inspected-function->com-untrace
    (inspected-function com-untrace inspector
     :tester ((object) (tracedp (object object)))
     :priority -1
     :documentation "Untrace function"
     :pointer-documentation ((object stream)
                             (format stream "~@<Untrace the function ~A~@:>"
                                     (object object))))
    (object)
  (list object))

(define-command (com-remove-methods :command-table inspector
                                    :name          "Remove all Methods")
    ((object 'inspected-generic-function))
  (with-command-error-handling ("Could not remove all methods")
      (error "not implemented")))
