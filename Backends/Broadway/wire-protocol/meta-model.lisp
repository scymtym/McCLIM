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
  (if-let ((pair (find symbol (pairs enum) :test #'eql :key #'cdr)))
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

(defclass message ()
  ((%name   :initarg :name
            :reader  name)
   (%fields :initarg :fields
            :reader  fields)))

(defmacro field (name type)
  (let ((type (case type
                ((1 2 3 4) type)
                (t         type))))
    `(make-instance 'field :name ',name :type ',type)))

(defmacro message (name &rest fields)
  `(make-instance 'message
                  :name   ',name
                  :fields (list ,@(map 'list (lambda (field)
                                               `(field ,@field))
                                       fields))))

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
             (destructuring-bind ((name id) &rest fields) name-id-fields
               (push `(,name ,id) pairs)
               (push `(message ,name ,@fields) messages*))))
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

;;;

(defmethod generate ((description field) (target (eql :slot)))
  (let* ((name    (name description))
         (initarg (make-keyword name))
         (type    (type description))
         (type    (ecase type
                    (boolean   type)
                    (:float32  'single-float)
                    ((1 2 3 4) `(unsigned-byte ,(* 8 type)))
                    (symbol    `(member ,@(map 'list #'cdr (pairs (symbol-value type))))))))
    `(,name :initarg  ,initarg
            :type     ,type
            :reader   ,name)))

(defmethod generate ((description message) (target (eql :class)))
  `(defclass ,(name description) ()
     (,@(map 'list (rcurry #'generate :slot) (fields description)))))

(defmethod generate ((description protocol) (target (eql :class)))
  `(progn ,@(map 'list (rcurry #'generate :class) (messages description))))

;;; Size

(defmethod generate ((description (eql 'boolean)) (target (eql :size)))
  1)

(defmethod generate ((description (eql :float32)) (target (eql :size)))
  4)

(defmethod generate ((description integer) (target (eql :size)))
  description)

(defmethod generate ((description field) (target (eql :size)))
  (generate (type description) target))

(defmethod generate ((description message) (target (eql :size)))
  (reduce #'+ (fields description) :key (rcurry #'generate target)))

;;;

(defmethod generate ((description (eql 'boolean)) (target (eql :deserialize)))
  `(prog1
       (= 1 (aref buffer offset))
     (incf offset 1)))

(defmethod generate ((description (eql :float32)) (target (eql :deserialize)))
  `(prog1
       (nibbles:ieee-single-ref/be buffer offset)
     (incf offset 4)))

(defmethod generate ((description integer) (target (eql :deserialize)))
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

(defmethod generate ((description field) (target (eql :deserialize)))
  (let* ((name    (name description))
         (initarg (make-keyword name))
         (type    (type description)))
    `(,initarg ,(generate type target))))

(defmethod generate ((description message) (target (eql :deserialize)))
  `(make-instance ',(name description)
                  ,@(mappend (rcurry #'generate target)
                             (fields description))))

(defmethod generate ((description protocol) (target (eql :deserialize)))
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

(defmethod generate ((description (eql 'boolean)) (target (eql :serialize)))
  `(prog1
       (setf (aref buffer offset) (if value 1 0))
     (incf offset 1)))

(defmethod generate ((description (eql :float32)) (target (eql :serialize)))
  `(prog1
       (setf (nibbles:ieee-single-ref/le buffer offset) value)
     (incf offset 4)))

(defmethod generate ((description integer) (target (eql :serialize)))
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

(defmethod generate ((description field) (target (eql :serialize)))
  (let* ((name   (name description))
         (reader name)
         (type   (type description)))
    `(let ((value (,reader value)))
       ,(generate type target))))

(defmethod generate ((description message) (target (eql :serialize)))
  `(progn
     ,@(map 'list (rcurry #'generate target) (fields description))))

(defmethod generate ((description protocol) (target (eql :serialize)))
  (let* ((fields        (fields description))
         (protocol-size (reduce #'+ fields :key (rcurry #'generate :size)))
         (ids           (ids description)))
    `(progn
       (typecase value
         ,@(map 'list (lambda (message)
                        (let* ((name   (name message))
                               (number (symbol->number ids name))
                               (size   (generate message :size)))
                          `(,name
                            (let ((buffer (nibbles:make-octet-vector
                                           ,(+ protocol-size size)))
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

