(cl:in-package #:new-inspector)

;;; `basic-place'

(defclass basic-place ()
  ((%children  :accessor children
               :initform (make-hash-table :test #'eql))
   (%container :initarg  :container
               :reader   container)
   (%cell      :initarg  :cell
               :reader   cell)
   (%state     :initarg  :state
               :accessor state
               :initform nil)))

(defmethod ensure-child ((cell  t)
                         (class t)
                         (place basic-place)
                         (thunk function))
  (symbol-macrolet ((by-class (gethash cell (children place))))
    (or (assoc-value by-class class :test #'eq)
        (setf (assoc-value by-class class :test #'eq) (funcall thunk)))))

(defmethod ensure-state ((object t) (place basic-place) (thunk function))
  (or (state place)
      (setf (state place) (funcall thunk))))

(defmethod supportsp ((place basic-place) (operation (eql 'setf)))
  t)

(defmethod supportsp ((place basic-place) (operation (eql 'remove-value)))
  nil)

(defmethod accepts-value-p ((place basic-place) (value t))
  t)

(defmethod valuep ((place basic-place))
  t)

;;; `root-place'

(defclass root-place (basic-place)
  ((%container :initform nil)
   (%cell      :reader   value
               :writer   (setf value))))

(defmethod valuep ((place root-place))
  (slot-boundp place '%cell))

(defmethod (setf state) :after ((new-value t) (place root-place))
  (setf (style new-value) :expanded))

;;; `pseudo-place'

(defclass pseudo-place (basic-place)
  ())

(defmethod supportsp ((place pseudo-place) (operation (eql 'setf)))
  nil)

(defmethod value ((place pseudo-place))
  (cell place))

;;; `accessor-place'

(defclass accessor-place (basic-place)
  ())

(defmethod value ((place accessor-place))
  (funcall (cell place) (container place)))

(defmethod (setf value) ((new-value t) (place accessor-place))
  (funcall (fdefinition `(setf ,(cell place))) (container place)))

;;; `sequence-element-place'

(defclass sequence-element-place (basic-place)
  ())

;;; `key-value-place'

(defclass key-value-place (basic-place)
  ())

;;; `key-place'

(defclass key-place (key-value-place)
  ())

;;; `value-place'

(defclass value-place (key-value-place)
  ())
