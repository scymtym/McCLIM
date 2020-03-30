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

(defgeneric generate (description target &key &allow-other-keys))

;;; Class generation

(defmethod generate ((description field) (target (eql :slot)) &key)
  (let* ((name    (name description))
         (initarg (make-keyword name))
         (type    (type description))
         (type    (etypecase type
                    ((member boolean string) type)
                    ((eql :float32)          'single-float)
                    ((member 1 2 3 4)        `(unsigned-byte ,(* 8 type)))
                    (symbol                  (let ((pairs (symbol-value type)))
                                               `(member ,@(map 'list #'cdr pairs))))
                    (cons                    (first type)))))
    `(,name :initarg  ,initarg
            :type     ,type
            :reader   ,name)))

(defmethod generate ((description message) (target (eql :class))
                     &key message-protocol-class)
  `(defclass ,(name description) (,message-protocol-class)
     (,@(map 'list (rcurry #'generate :slot) (fields description)))))

(defmethod generate ((description protocol) (target (eql :class)) &key)
  (let ((protocol-class-name (symbolicate (name description) '#:-message)))
    `(progn
       (defclass ,protocol-class-name () ())
       ,@(map 'list (rcurry #'generate :class
                            :message-protocol-class protocol-class-name)
              (messages description)))))

;;; Size computation generation

(defmethod generate ((description (eql 'boolean)) (target (eql :size)) &key)
  1)

(defmethod generate ((description (eql :float32)) (target (eql :size)) &key)
  4)

(defmethod generate ((description integer) (target (eql :size)) &key)
  description)

(defmethod generate ((description (eql 'string)) (target (eql :size)) &key)
  `(+ 4 (* 4 (ceiling (length (babel:string-to-octets value :encoding :utf-8)) 4)))  ; TODO hack
  #+maybe `(+ 4 (length (babel:string-to-octets value :encoding :utf-8))))

(defmethod generate ((description cons) (target (eql :size)) &key)
  (destructuring-bind (constructor type) description
    (assert (eq constructor 'list))
    `(flet ((size-of-element (value)
              ,(generate type target)))
       (+ ,(generate 4 :size)
          (reduce #'+ value :key #'size-of-element)))))

(defmethod generate ((description symbol) (target (eql :size)) &key)
  (let ((description (symbol-value description)))
    (generate description target)))

(defmethod generate ((description field) (target (eql :size)) &key)
  (if (typep (type description) '(or (eql string) cons))
      `(let ((value (,(name description) value))) ; TODO hack
         ,(generate (type description) target))
      (generate (type description) target)))

(defmethod generate ((description message) (target (eql :size)) &key)
  `(+ ,@(map 'list (rcurry #'generate target) (fields description))))

(defmethod generate ((description protocol) (target (eql :size)) &key)
  (let* ((fields        (fields description))
         (protocol-size (map 'list (rcurry #'generate :size) fields)))
    `(+ ,@protocol-size
        (etypecase value
          ,@(map 'list (lambda (message)
                         `(,(name message)
                           ,(generate message :size)))
             (messages description))))))

;;; Deserialization generation

(defmethod generate ((description (eql 'boolean)) (target (eql :deserialize)) &key)
  `(prog1
       (= 1 (aref buffer offset))
     (incf offset 1)))

(defmethod generate ((description (eql :float32)) (target (eql :deserialize)) &key)
  `(prog1
       (nibbles:ieee-single-ref/be buffer offset)
     (incf offset 4)))

(defmethod generate ((description integer) (target (eql :deserialize)) &key)
  (ecase description
    (1 `(prog1
            (aref buffer offset)
          (incf offset 1)))
    (2 `(prog1
            (nibbles:ub16ref/be buffer offset)
          (incf offset 2)))
    (4 `(prog1
            (nibbles:ub32ref/be buffer offset)
          (incf offset 4)))))

(defmethod generate ((description field) (target (eql :deserialize)) &key)
  (let* ((name    (name description))
         (initarg (make-keyword name))
         (type    (type description)))
    `(,initarg ,(generate type target))))

(defmethod generate ((description message) (target (eql :deserialize)) &key)
  `(make-instance ',(name description)
                  ,@(mappend (rcurry #'generate target)
                             (fields description))))

(defmethod generate ((description protocol) (target (eql :deserialize)) &key)
  (let ((fields (fields description))
        (ids    (ids description)))
    `(let (,@(map 'list (lambda (field)
                          `(,(name field) ,(generate (type field) target)))
                  fields))
       (case ,(name (first fields))
         ,@(map 'list (lambda (message)
                        (let* ((name   (name message))
                               (number (symbol->number ids name)))
                          `(,number ,(generate message target))))
            (messages description))))))

;;; Serialization generation

(defmethod generate ((description (eql 'boolean)) (target (eql :serialize)) &key)
  `(prog1
       (setf (aref buffer offset) (if value 1 0))
     (incf offset 1)))

(defmethod generate ((description (eql :float32)) (target (eql :serialize)) &key)
  `(prog1
       (setf (nibbles:ieee-single-ref/le buffer offset) value)
     (incf offset 4)))

(defmethod generate ((description integer) (target (eql :serialize)) &key)
  (ecase description
    (1 `(prog1
            (setf (aref buffer offset) value)
          (incf offset 1))) ; TODO all offsets are statically known
    (2 `(prog1
            (setf (nibbles:ub16ref/le buffer offset) value)
          (incf offset 2)))
    (4 `(prog1
            (setf (nibbles:ub32ref/le buffer offset) value)
          (incf offset 4)))))

(defmethod generate ((description (eql 'string)) (target (eql :serialize)) &key)
  `(let* ((octets (babel:string-to-octets value :encoding :utf-8))
          (length (length octets)))
     (setf (nibbles:ub32ref/le buffer offset) length)
     (incf offset 4)
     (setf (subseq buffer offset) octets)
     (incf offset (* 4 (ceiling length 4)))
     #+maybe (incf offset length)))

(defmethod generate ((description cons) (target (eql :serialize)) &key)
  (assert (eq (first description) 'list))
  `(flet ((serialize-element (value)
            ,(generate (second description) target)))
     (setf (nibbles:ub32ref/le buffer offset) (length value))
     (incf offset 4)
     (map nil #'serialize-element value)))

(defmethod generate ((description symbol) (target (eql :serialize)) &key) ; TODO more general method, does not depend on TARGET
  (let ((description (symbol-value description)))
    (generate description target :buffer nil :initial-offset nil)))

(defmethod generate ((description field) (target (eql :serialize)) &key)
  (let* ((name   (name description))
         (reader name)
         (type   (type description)))
    `(let ((value (,reader value)))
       ,(generate type target))))

(defmethod generate ((description message) (target (eql :serialize)) &key)
  `(progn
     ,@(map 'list (rcurry #'generate target) (fields description))))

(defmethod generate ((description protocol) (target (eql :serialize))
                     &key (buffer t) (initial-offset '0))
  (let* ((fields        (fields description))
         (protocol-size (map 'list (rcurry #'generate :size) fields))
         (ids           (ids description)))
    `(progn
       (etypecase value
         ,@(map 'list (lambda (message)
                        (let* ((name   (name message))
                               (number (symbol->number ids name))
                               (size   (generate message :size)))
                          `(,name
                            (let (,@(when buffer
                                      `((buffer (nibbles:make-octet-vector
                                                 (+ ,@protocol-size ,size)))))
                                  ,@(when initial-offset
                                      `((offset ,initial-offset))))
                              ;; (declare (dynamic-extent buffer))
                              (let ((value ,number))
                                ,(generate (type (first fields)) target))
                              ,@(map 'list (lambda (field)
                                             `(let ((value ,(name field)))
                                                ,(generate (type field) target)))
                                     (rest fields))
                              ,(generate message target)
                              ;; (write-sequence buffer stream)
                              buffer
                              ))))
            (messages description))))))

;;; Print

(defmethod generate ((description message) (target (eql 'print-object)) &key)
  (when-let ((print-spec (print-spec description)))
    (destructuring-bind (format &rest fields) print-spec
      (list `(defmethod print-object ((object ,(name description)) stream)
               (print-unreadable-object (object stream :type t :identity t)
                 (format stream ,format
                         ,@(map 'list (lambda (field)
                                        (let ((field (or (find field (fields description)
                                                               :key #'name)
                                                         (error "~S does not name a field of ~A"
                                                                field description))))
                                          `(,(name field) object)))
                                fields))))))))

(defmethod generate ((description protocol) (target (eql 'print-object)) &key)
  (let ((methods (mappend (rcurry #'generate 'print-object) (messages description))))
    `(progn ,@methods)))
