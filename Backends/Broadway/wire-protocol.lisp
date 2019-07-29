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

;;; Server -> client operations

(define-protocol operation
    ((opcode 1)
     (serial 4))

  ((grab-pointer 0)
   (id           2)
   (owner-event? boolean))

  ((ungrab-pointer 1))

  ((new-surface 2)
   (id     2)
   (x      2)
   (y      2)
   (width  2)
   (height 2)
   (temp?  boolean))

  ((show-surface 3)
   (id 2))

  ((hide-surface 4)
   (id 2))

  ((raise-surface 5)
   (id 2))

  ((lower-surface 6)
   (id 2))

  ((destroy-surface 7)
   (id 2))

  ((move-resize 8)
   (id     2)
   (flags  1) ; TODO
   (x      2) ; TODO only if "has-pos"
   (y      2)
   (width  2) ; TODO only if "has-size"
   (height 2))

  ((set-transient-for 9)
   (id        2)
   (parent-id 2))

  ((disconnected 10)
   )

  #+unused ((surface-update 11))

  ((set-show-keyboard 12)
   (show 2))

  ((upload-texture 13)
   (id   4)
   (size 4)
   ; (data octets)
   )

  ((release-texture 14)
   (id 4))

  ((set-nodes 15)
   (id   2)
   (size 4))

  ((roundtrip 16)
   (id  2)
   (tag 4)))

#.`(progn
     ,(generate operation :class)
     (defun serialize-operation (serial operation)
       (let ((value operation))
         ,(generate operation :serialize))))

(generate operation :class)
(generate operation :serialize)


;;; Client -> server events

(define-protocol event
    ((opcode    4)
     (serial    4)
     (timestamp 4))

  ((enter 0)
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4)
   (?          4))

  ((leave 1)
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4)
   (?          4))

  ((pointer-move 2)
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4))

  ((button-press 3)
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4)
   (button     4))

  ((button-release 4)
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4)
   (button     4))

  ((touch 5))

  ((scroll 6))

  ((key-press 7))

  ((key-release 8))

  ((grab-notify 9))

  ((ungrab-notify 10))

  ((configure-notify 11)
   (surface 4)
   (x       4)
   (y       4)
   (width   4)
   (height  4))

  ((screen-size-changed 12)
   (width  4)
   (height 4))

  ((focus               13))

  ((roundtrip-notify 14)))



(define-message pointer-move
)


#.`(progn
     ,(generate event :class)
     (defun deserialize-event (buffer offset)
       (values ,(generate event :deserialize) offset)))

(generate event :deserialize)


;;; Nodes

(define-enum *node-kind*
    (:texture         0)
  (:container       1)
  (:color           2)
  (:border          3)
  (:outset-shadow   4)
  (:inset-shadow    5)
  (:rounded-clip    6)
  (:linear-gradient 7)
  (:shadow          8)
  (:opacity         9)
  (:clip            10)
  (:transform       11)
  (:debug           12)
  (reuse           13))

(define-protocol make-node
    ((type 4)
     (id   4))

  ((texture 0)
   (x      :float32)
   (y      :float32)
   (width  :float32)
   (height :float32)
   (id     4))

  ((container 1))

  ((color 2)
   (x      :float32)
   (y      :float32)
   (width  :float32)
   (height :float32)
   (red    1)
   (green  1)
   (blue   1)
   (alpha  1))

  ((reuse 13)))

#.`(progn
     ,(generate make-node :class)
     (defun serialize-make-node (id node)
       (let ((value node))
         ,(generate make-node :serialize))))

(define-protocol node-operation
    ((opcode 4))

  ((insert-node 0)
   (parent-id           4)
   (previous-sibling-id 4))

  ((remove-node 1)
   (id 4))

  ((move-after-child 2)
   (parent-id           4)
   (previous-sibling-id 4)
   (reused-node-id      4))

  ((patch-texture 3))
  ((patch-transform 4)))


#.`(progn
     ,(generate node-operation :class)
     (defun serialize-node-operation (node)
       (let ((value node))
         ,(generate node-operation :serialize))))


(define-enum *display-operation*
  (:replace-child      0)
  (:append-child       1)
  (:insert-after-child 2)
  (:append-root        3)
  (:show-surface       4)
  (:hide-surface       5)
  (:delete-node        6)
  (:move-node          7)
  (:resize-node        8)
  (:restack-surfaces   9)
  (:delete-surface     10)
  (:change-texture     11)
  (:change-transform   12))
