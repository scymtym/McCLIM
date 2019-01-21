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

;;; TODO
;;; name = (setf (find-class ) nil) (setf (find-class new-name) …)
;;; sub/super/meta class
;;; slots
;;; direct methods

;;; finalize, finalized badge

;;; add/remove slots

;;; Object states

(defclass inspected-class (inspected-instance)
  ())

(defmethod make-object-state ((object class) (place t))
  (make-instance 'inspected-class :place place))

;;; Object inspection methods

(defun anonymous-class-p (class)
  (let ((name (class-name class)))
    (or (not name)
        (not (eq (find-class name nil) class)))))

(defmethod inspect-object-using-state ((object class)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (call-next-method)

  (when (anonymous-class-p object)
    (write-char #\Space stream)
    (with-output-as-badge (stream)
      (write-string "anonymous" stream)))

  (write-char #\Space stream)
  (with-output-as-badge (stream)
    (format stream "~:[not~;~] finalized"
            (c2mop:class-finalized-p object))))

(defmethod inspect-object-using-state ((object class)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place (class-name object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Name" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))

        (formatting-place (stream object 'pseudo-place (class-of object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Metaclass" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream))))

      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place (c2mop:class-direct-superclasses object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Superclasses" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))

        (formatting-place (stream object 'pseudo-place (c2mop:class-direct-subclasses object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Subclasses" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream))))))

  (print-documentation object stream)

  (call-next-method)
  )

;;; Commands

(define-command (com-finalize :command-table inspector
                              :name          "Finalize Class")
    ((object 'inspected-class))
  (let ((object (object object)))
    (with-command-error-handling ("Could not finalize ~A" object)
        (c2mop:finalize-inheritance object))))

(define-presentation-to-command-translator inspected-class->com-finalize
    (inspected-class com-finalize inspector
     :tester ((object) (not (c2mop:class-finalized-p (object object))))
     :priority -1
     :documentation "Finalize class"
     :pointer-documentation ((object stream)
                             (format stream "~@<Finalize ~A~@:>"
                                     (object object))))
    (object)
  (list object))
