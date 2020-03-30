;;;; (C) Copyright 2019, 2020 Jan Moringen
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

(cl:in-package #:clim-broadway)

;;; Utilities

(defun lisp-name->js-name (name)
  (substitute-if #\_ (rcurry #'member '(#\- #\?)) name))

(defun call-with-block (thunk stream)
  (write-char #\{ stream)
  (pprint-newline :mandatory stream)
  (pprint-logical-block (stream nil :per-line-prefix "  " :suffix "}")
    (funcall thunk stream))
  (pprint-newline :mandatory stream))

(defmacro with-block ((stream-var) &body body)
  `(call-with-block (lambda (,stream-var) ,@body) ,stream-var))

(defun statement (format-control &rest format-arguments)
  (format t "~?;~@:_" format-control format-arguments))

;;; Class and enum generation

(defmethod generate ((description enum) (target (eql :js)) &key prefix)
  (loop :with pairs = (sort (copy-list (pairs description)) #'< :key #'car)
        :for (value . symbol) :in pairs
        :for name = (lisp-name->js-name (symbol-name symbol))
        :do (format t "const ~@[~A_~]~A = ~D;~%" prefix name value)))

;;; Deserializer generation

(defmethod decoder-name ((description t) &key prefix)
  (format nil "decode_~@[~A_~]~A"
          prefix
          (lisp-name->js-name (string-downcase (name description)))))

(defmethod generate ((description (eql 'boolean)) (target (eql :deserialize/js)) &key)
  (format t "this.decode_bool()"))

(defmethod generate ((description (eql :float32)) (target (eql :deserialize/js)) &key)
  (format t "this.decode_float()"))

(defmethod generate ((description integer) (target (eql :deserialize/js)) &key)
  (format t "this.decode_~:[~;u~]int~D()"
          (= description 1) (* 8 description)))

(defmethod generate ((description (eql 'string)) (target (eql :deserialize/js)) &key)
  (format t "this.decode_string()"))

(defmethod generate ((description cons) (target (eql :deserialize/js)) &key)
  (destructuring-bind (constructor type) description
    (assert (eq constructor 'list))
    (let ((stream *standard-output*))
      (format stream "(function ()")
      (with-block (stream)
        (statement "var length = this.decode_int~D()" (* 8 4))
        (statement "var result = new Array()")
        (format stream "for (i = 0; i < length; ++i)")
        (with-block (stream)
          (format stream "result.push(")
          (let ((*standard-output* stream))
            (generate type target))
          (format stream ");~@:_"))
        (statement "return result"))
      (format stream "())"))))

(defmethod generate ((description symbol) (target (eql :deserialize/js)) &key) ; TODO same for lisp
  (let ((description (symbol-value description)))
    (format t "~A()" (decoder-name description))))

(defmethod generate ((description field) (target (eql :deserialize/js)) &key)
  (format t "result.~A = "
          (lisp-name->js-name (string-downcase (name description))))
  (generate (type description) :deserialize/js)
  (format t ";~@:_"))

(defmethod generate ((description message) (target (eql :deserialize/js)) &key prefix)
  (let ((stream *standard-output*))
    (format stream "~A = function() " (decoder-name description :prefix prefix))
    (with-block (stream)
      (format stream "var result = new Object();~@:_")
      (let ((*standard-output* stream))
        (map nil (rcurry #'generate target) (fields description)))
      (format t "return result;~@:_"))))

(defmethod generate ((description protocol) (target (eql :deserialize/js)) &key)
  (let ((stream *standard-output*))
    (format stream "~A = function () " (decoder-name description))
    (with-block (stream)
      (let ((*standard-output* stream))
        (map nil (rcurry #'generate target) (fields description))
        (statement "var result")
        (format stream "switch (~A) " (lisp-name->js-name (string-downcase (name (first (fields description)))))) ; TODO
        (with-block (stream)
          (map nil (lambda (id message)
                     (format stream "case ~D:~@:_" (lisp-name->js-name (string (cdr id))))
                     (progn ; with-indent (stream)
                       (let ((*standard-output* stream))
                         (statement "result = ~A()" (decoder-name message))
                         (statement "break"))))
               (pairs (ids description)) (messages description)))
        (statement "return result")))

    (map nil (rcurry #'generate target :prefix (lisp-name->js-name (string-downcase (name description))))
         (messages description))))

(defmethod generate ((description protocol) (target (eql :js)) &key)
  (let* ((name   (name description))
         (prefix (format nil "BROADWAY_~A"
                         (lisp-name->js-name (symbol-name name)))))
    (let ((stream *standard-output*))
      (pprint-logical-block (stream nil)
        (let ((*standard-output* stream))
          (format stream "// ~A protocol~@:_~@:_" name)
          (format stream "// Constants~@:_")
          (generate (ids description) target :prefix prefix)
          (format stream "// Serialization~@:_")
          (generate description :deserialize/js))))))
