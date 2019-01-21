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

(declaim (inline package-locked?))
(defun package-locked? (package)
  #+sbcl (sb-ext:package-locked-p package)
  #-sbcl nil)

;;; `symbol-slot-place'

(defclass symbol-slot-place (basic-place) ; TODO make a base class without cell slot
  ())

#+sbcl (defmethod supportsp :around ((place symbol-slot-place) (operation t))
         (and (if-let ((package (symbol-package (container place))))
                (not (package-locked? package))
                t)
              (call-next-method)))

(defmethod supportsp ((place symbol-slot-place) (operation (eql 'remove-value)))
  t)

;;; `symbol-value-place'
;;;
;;; TODO constants
;;; TODO show type
;;; TODO check value against type

(defclass symbol-value-place (symbol-slot-place)
  ())

(defmethod supportsp ((place symbol-value-place) (operation t))
  (and (not (constantp (container place)))
       (call-next-method)))

(defmethod valuep ((place symbol-value-place))
  (boundp (container place)))

(defmethod value ((place symbol-value-place))
  (symbol-value (container place)))

(defmethod (setf value) ((new-value t) (place symbol-value-place))
  (setf (symbol-value (container place)) new-value))

(defmethod make-unbound ((place symbol-value-place))
  (makunbound (container place)))

;;; `symbol-function-place'

(defclass symbol-function-place (symbol-slot-place)
  ())

(defmethod accepts-value-p ((place symbol-function-place) (value t))
  nil)

(defmethod accepts-value-p ((place symbol-function-place) (value function))
  t)

(defmethod valuep ((place symbol-function-place))
  (fboundp (container place)))

(defmethod value ((place symbol-function-place))
  (symbol-function (container place)))

(defmethod (setf value) ((new-value function) (place symbol-function-place))
  (setf (symbol-function (container place)) new-value))

(defmethod make-unbound ((place symbol-function-place))
  (fmakunbound (container place)))

;;; `symbol-type-place'

(defclass symbol-type-place (symbol-slot-place)
  ())

(defmethod accepts-value-p ((place symbol-type-place) (value t))
  nil)

(defmethod accepts-value-p ((place symbol-type-place) (value class))
  t)

(defmethod valuep ((place symbol-type-place))
  (find-class (container place) nil))

(defmethod value ((place symbol-type-place))
  (find-class (container place) nil))

(defmethod (setf value) ((new-value class) (place symbol-type-place))
  (setf (find-class (container place) nil) new-value))

(defmethod make-unbound ((place symbol-type-place))
  (setf (find-class place nil) nil))

;;; Object states

(defclass inspected-package (inspected-object)
  ((%symbol-filter :initarg  :symbol-filter
                   :accessor symbol-filter
                   :initform nil)))

(defmethod (setf symbol-filter) :after ((new-value t)
                                        (object    inspected-package))
  ())

(defmethod make-object-state ((object package) (place t))
  (make-instance 'inspected-package :place place))

;;; Object inspection methods

(defmethod inspect-object-using-state ((object symbol)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (call-next-method)
  (write-char #\Space stream)
  (let ((package (symbol-package object)))
    (cond ((null package)
           (badge stream "uninterned"))
          (t
           (with-output-as-badge (stream)
             (let ((state (nth-value 1 (find-symbol (symbol-name object) package)))) ; TODO ugly
               (write-string (string-downcase state) stream)))))))

(defmethod inspect-object-using-state ((object symbol)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (print-documentation object stream :namespace 'function) ; TODO not just function

  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (symbol-name object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Name" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream)))

      (formatting-place (stream object 'pseudo-place (symbol-package object) present inspect) ; TODO should be mutable
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Package" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))

    (formatting-row (stream)
      (formatting-place (stream object 'symbol-value-place nil present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Value" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream)))

      (formatting-place (stream object 'symbol-function-place nil present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Function" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))

    (formatting-row (stream)
      (formatting-place (stream object 'symbol-type-place nil present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Type" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))
    )
  ;; plist
  )

;; TODO style symbols as table
;; TODO style symbols grouped by external etc.
(defmethod inspect-object-using-state ((object package)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (package-name object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Name" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream)))))

  (print-documentation object stream)
  (terpri stream)

  (with-section (stream) "Symbols"
    (formatting-table (stream)
      (with-drawing-options (stream :text-face :bold)
        (formatting-row (stream)
          (formatting-cell (stream) (write-string "Symbol" stream))
          (formatting-cell (stream) (write-string "Value" stream))
          (formatting-cell (stream) (write-string "Function" stream))))
      (do-symbols (symbol object)
        (when (eq (symbol-package symbol) object)
          (let ((symbol symbol))
            (formatting-row (stream)
              (formatting-place (stream object 'pseudo-place symbol nil inspect* :place-var place)
                (formatting-cell (stream) (inspect* stream))

                (let ((*parent-place* place)) ; TODO this is not
                                        ; acceptable
                  ;; value
                  (formatting-place (stream symbol 'symbol-value-place nil present inspect)
                    ;; (formatting-cell (stream) (present stream))
                    (formatting-cell (stream) (present stream) (inspect stream)))

                  (formatting-place (stream symbol 'symbol-function-place nil present inspect)
                    ;; (formatting-cell (stream) (present stream))
                    (formatting-cell (stream) (present stream) (inspect stream))))))))))))

;; TODO command: trace all symbols
