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

;;; Utilities

(defun tracedp (function)
  #-sbcl nil
  #+sbcl (gethash function sb-debug::*traced-funs*))

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

(defmethod remove-value ((place method-place))
  (remove-method (container place) (value place))) ; TODO store the method?

(defmethod qualifiers ((place method-place))
  (method-qualifiers (cell place)))

(defmethod specializers ((place method-place))
  (c2mop:method-specializers (cell place)))

;;; Object states

(defclass inspected-function (inspected-object)
  ())

(defmethod object-state-class ((object function) (place t))
  'inspected-function)

(defclass inspected-funcallable-standard-object (inspected-function
                                                 inspected-instance)
  ())

(defmethod object-state-class ((object c2mop:funcallable-standard-object)
                               (place  t))
  'inspected-funcallable-standard-object)

(defclass inspected-generic-function (inspected-funcallable-standard-object)
  ()
  (:default-initargs
   :slot-style nil))

(defmethod object-state-class ((object generic-function) (place t))
  'inspected-generic-function)

(defclass inspected-method (inspected-instance)
  ()
  (:default-initargs
   :slot-style nil))

(defmethod object-state-class ((object method) (place t))
  'inspected-method)

(defmethod make-object-state ((object method) (place method-place))
  (let ((class (object-state-class object place)))
    (make-instance class :place place :style :class-only)))

;;; Presentation types

(define-presentation-type inspected-generic-function () ; TODO needed?
  :inherit-from '(and inspected-function inspected-instance))

;;; Object inspection methods

;;; `function'

(defmethod inspect-object-using-state :after ((object function)
                                              (state  inspected-function)
                                              (style  (eql :expanded-header))
                                              (stream t))
  ;; Traced
  (when (tracedp object)
    (write-string " " stream)
    (badge stream "traced"))
  ;; Interpreted
  (unless (compiled-function-p object)
    (write-string " " stream)
    (badge stream "interpreted")))

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
      #+sbcl
      (formatting-row (stream)
        (formatting-place (stream nil 'pseudo-place (sb-introspect:function-type object) present inspect) ; TODO fresh list every time? => state is forgotten
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Type" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream))))))
  ;; Documentation
  (print-documentation object stream))

;;; `funcallable-standard-object'

(defmethod inspect-object-using-state :after ((object function)
                                              (state  inspected-funcallable-standard-object)
                                              (style  (eql :expanded-header))
                                              (stream t))
  (write-string " " stream)
  (badge stream "fucallable"))

(defmethod inspect-object-using-state ((object c2mop:funcallable-standard-object)
                                       (state  inspected-funcallable-standard-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  ;; Function
  (call-next-method)

  ;; Slots
  (with-section (stream) "Slots"
    (inspect-slots object (slot-style state) stream))
  #+broken (call-next-method))

;;; `generic-function'

(defun inspect-method-list (object methods stream &key generic-function-name)
  (formatting-table (stream)
    (formatting-row (stream)
      (if generic-function-name
          (formatting-header (stream) "Generic function" "Qualifiers" "Specializers")
          (formatting-header (stream) "Qualifiers" "Specializers")))
    (map nil (lambda (method)
               (formatting-place (stream object 'method-place method present inspect
                                         :place-var place)
                 (formatting-row (stream)
                   (with-style (stream :slot-like)
                     (when generic-function-name
                       (formatting-cell (stream)
                         (princ (c2mop:generic-function-name
                                 (c2mop:method-generic-function method))
                                stream)))
                     (formatting-cell (stream)
                       (format-items (qualifiers place) :stream stream))
                     (map nil (lambda (specializer)
                                (formatting-cell (stream)
                                  (with-print-error-handling (stream)
                                    (typecase specializer
                                      (class (inspect-class-as-name specializer stream))
                                      (t     (prin1 `(eql ,(c2mop:eql-specializer-object specializer)) stream)))))) ; TODO should be inspectable
                          (specializers place)))
                   (formatting-cell (stream)
                     (present stream))
                   (formatting-cell (stream)
                     (inspect stream)))))
         methods)))

(defmethod inspect-object-using-state ((object generic-function)
                                       (state  inspected-generic-function)
                                       (style  (eql :expanded-body))
                                       (stream t))
  ;; Funcallable standard object
  (call-next-method)

  ;; method class
  ;; method combination
  ;; Methods
  (with-section (stream) "Methods"
    (let ((methods (c2mop:generic-function-methods object)))
      (with-placeholder-if-emtpy (stream)
        ((null methods)
         "No methods~%")
        (t
         (inspect-method-list object methods stream))))))

;; `method'

(defmethod inspect-object-using-state ((object method)
                                       (state  inspected-method)
                                       (style  (eql :class-only))
                                       (stream t))
  (princ (class-name (class-of object)) stream))

(defmethod inspect-object-using-state ((object method)
                                       (state  inspected-method)
                                       (style  (eql :expanded-body))
                                       (stream t))
  ;; Documentation
  (print-documentation object stream)
  ;; Slots
  (call-next-method))

;;; Commands

#+sbcl
(define-command (com-trace :command-table inspector
                           :name          "Trace Function")
    ((object 'inspected-function))
  (let ((object (object object)))
    (with-command-error-handling ("Could not trace ~A" object)
        (let ((name (nth-value 2 (function-lambda-expression object))))
          (sb-debug::trace-1 name (sb-debug::make-trace-info) object)))))

#+sbcl
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

#+sbcl
(define-command (com-untrace :command-table inspector
                             :name          "Untrace Function")
    ((object 'inspected-function))
  (let ((object (object object)))
    (with-command-error-handling ("Could not untrace ~A" object)
        (let ((name (nth-value 2 (function-lambda-expression object))))
          (sb-debug::untrace-1 name))))) ; TODO

#+sbcl
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
      (error "TODO not implemented")))
