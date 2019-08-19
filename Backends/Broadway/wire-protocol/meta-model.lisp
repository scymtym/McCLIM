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

;;; Class

(defgeneric generate (description target &key &allow-other-keys))

(defmethod generate ((description field) (target (eql :slot)) &key)
  (let* ((name    (name description))
         (initarg (make-keyword name))
         (type    (type description))
         (type    (ecase type
                    ((boolean string) type)
                    (:float32         'single-float)
                    ((1 2 3 4)        `(unsigned-byte ,(* 8 type)))
                    (symbol           (let ((pairs (symbol-value type)))
                                        `(member ,@(map 'list #'cdr pairs)))))))
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

;;; Size

(defmethod generate ((description (eql 'boolean)) (target (eql :size)) &key)
  1)

(defmethod generate ((description (eql :float32)) (target (eql :size)) &key)
  4)

(defmethod generate ((description integer) (target (eql :size)) &key)
  description)

(defmethod generate ((description (eql 'string)) (target (eql :size)) &key)
  `(+ 4 (* 4 (ceiling (length (babel:string-to-octets value :encoding :utf-8)) 4)))) ; TODO hack

(defmethod generate ((description field) (target (eql :size)) &key)
  (if (eq (type description) 'string)
      `(let ((value (,(name description) value))) ; TODO hack
         ,(generate (type description) target))
      (generate (type description) target)))

(defmethod generate ((description message) (target (eql :size)) &key)
  `(+ ,@(map 'list (rcurry #'generate target) (fields description))))

;;;

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

;;; Serialize

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
     (incf offset length)))

(defmethod generate ((description field) (target (eql :serialize)) &key)
  (let* ((name   (name description))
         (reader name)
         (type   (type description)))
    `(let ((value (,reader value)))
       ,(generate type target))))

(defmethod generate ((description message) (target (eql :serialize)) &key)
  `(progn
     ,@(map 'list (rcurry #'generate target) (fields description))))

(defmethod generate ((description protocol) (target (eql :serialize)) &key)
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
                            (let ((buffer (nibbles:make-octet-vector
                                           (+ ,@protocol-size ,size)))
                                  (offset 0))
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
                                        (let ((field (find field (fields description)
                                                           :key #'name)))
                                          `(,(name field) object)))
                                fields))))))))

(defmethod generate ((description protocol) (target (eql 'print-object)) &key)
  (let ((methods (mappend (rcurry #'generate 'print-object) (messages description))))
    `(progn ,@methods)))
