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

;;; Enum

(defclass enum ()
  ((%pairs :initarg :pairs
           :reader  pairs)))

(defmethod number->symbol ((enum enum) (number t))
  (if-let ((pair (find number (pairs enum) :test #'eql :key #'car)))
    (cdr pair)
    (error "unknown value: ~S" number)))

(defmethod symbol->number ((enum enum) (symbol symbol))
  (if-let ((pair (find symbol (pairs enum) :test #'eq :key #'cdr)))
    (car pair)
    (error "unknown symbol: ~S" symbol)))

(defmacro enum (&rest pairs)
  (flet ((make-pair (key-and-value)
           (destructuring-bind (key value) key-and-value
             `(,value . ,key))))
   `(make-instance 'enum :pairs '(,@(map 'list #'make-pair pairs)))))

(defmacro define-enum (name &rest pairs)
  `(defparameter ,name (enum ,@pairs)))

;;; Message

(defclass field ()
  ((%name :initarg :name
          :reader  name)
   (%type :initarg :type
          :reader  type)))

(defmethod print-object ((object field) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "\"~A\" ~S" (name object) (type object))))

(defclass message ()
  ((%name       :initarg  :name
                :reader   name)
   (%fields     :initarg  :fields
                :reader   fields)
   (%print-spec :initarg  :print-spec
                :reader   print-spec
                :initform nil)))

(defmethod print-object ((object message) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "\"~A\" (~D)" (name object) (length (fields object)))))

(defmacro field (name type)
  (let ((type (case type
                ((1 2 3 4) type)
                (t         type))))
    `(make-instance 'field :name ',name :type ',type)))

(defmacro message (name-and-options &rest fields)
  (destructuring-bind (name &key print-spec)
      (ensure-list name-and-options)
    `(make-instance 'message
                    :name   ',name
                    :fields (list ,@(map 'list (lambda (field)
                                                 `(field ,@field))
                                         fields))
                    ,@(when print-spec
                        `(:print-spec ',print-spec)))))

(defmacro define-message (name &rest fields)
  `(defparameter ,name (message ,name ,@fields)))

;;; Protocol

(defclass protocol ()
  ((%name     :initarg :name
              :reader  name)
   (%fields   :initarg :fields
              :reader  fields)
   (%ids      :initarg :ids
              :reader  ids)
   (%messages :initarg :messages
              :reader  messages)))

(defmacro protocol (name (&rest fields) &rest messages)
  (let ((pairs    '())
        (messages* '()))
    (flet ((process-message (name-id-fields)
             (destructuring-bind ((name id &rest options) &rest fields)
                 name-id-fields
               (push `(,name ,id) pairs)
               (push `(message (,name ,@options) ,@fields) messages*))))
      (map nil #'process-message messages)
      `(make-instance 'protocol
                      :name     ',name
                      :fields   (list ,@(map 'list (lambda (field)
                                                     `(field ,@field))
                                             fields))
                      :ids      (enum ,@pairs)
                      :messages (list ,@messages*)))))

(defmacro define-protocol (name (&rest fields) &rest messages)
  `(defparameter ,name (protocol ,name (,@fields) ,@messages)))
