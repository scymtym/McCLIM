(cl:in-package #:new-inspector)

(defclass vector-bound-place (basic-place)
  ())

(defmethod accepts-value-p ((place vector-bound-place) (value t))
  (typep value '(integer 0 (#.array-total-size-limit))))

;;; `vector-total-size-place'

(defclass vector-total-size-place (vector-bound-place)
  ())

(defmethod supportsp ((place vector-total-size-place) (operation (eql 'setf)))
  (adjustable-array-p (container place)))

(defmethod value ((place vector-total-size-place))
  (array-total-size (container place)))

(defmethod (setf value) ((new-value integer) (place vector-total-size-place))
  (let ((vector (container place)))
    (when (array-has-fill-pointer-p vector)
      (minf (fill-pointer vector) (max 0 (1- new-value))))
    (adjust-array vector new-value)))

;;; `vector-fill-pointer-place'

(defclass vector-fill-pointer-place (vector-bound-place)
  ())

(defmethod accepts-value-p ((place vector-fill-pointer-place) (value t))
  (and (call-next-method)
       (<= (array-total-size (container place)))))

(defmethod value ((place vector-fill-pointer-place))
  (fill-pointer (container place)))

(defmethod (setf value) ((new-value integer) (place vector-fill-pointer-place))
  (setf (fill-pointer (container place)) new-value))

;;; `vector-element-place'

(defclass vector-element-place (sequence-element-place)
  ())

(defmethod supportsp ((place     vector-element-place) ; TODO is hits the default?
                      (operation (eql 'remove-value)))
  nil)

(defmethod value ((place vector-element-place))
  (aref (container place) (cell place)))

(defmethod (setf value) (new-value (place vector-element-place))
  (setf (aref (container place) (cell place)) new-value))

;;; `adjustable-vector-element-place'

(defclass adjustable-vector-element-place (vector-element-place)
  ())

(defmethod supportsp ((place     adjustable-vector-element-place)
                      (operation (eql 'remove-value)))
  t)

(defmethod make-unbound ((place adjustable-vector-element-place))
  (let* ((container (container place))
         (length    (array-total-size container))
         (index     (cell place)))
    (minf (fill-pointer container) (1- length))
    (when (> length 1)
      (loop :for i :from index :to (- length 2)
            :do (setf (aref container i) (aref container (1+ i)))))
    (adjust-array container (1- length))))

;;; Object inspection methods

(defmethod inspect-object-using-state ((object vector)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (format stream "~:[~;Adjustable ~]Vector" (adjustable-array-p object)))

(Defmethod inspect-object-using-state ((object vector)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (let ((fill-pointer (when (array-has-fill-pointer-p object)
                        (fill-pointer object))))
    (with-preserved-cursor-x (stream)
      (formatting-table (stream)
        (formatting-row (stream)
          (formatting-place (stream nil 'pseudo-place (array-element-type object) present inspect)
            (with-style (stream :slot-like)
              (formatting-cell (stream) (write-string "Element type" stream))
              (formatting-cell (stream) (present stream)))
            (formatting-cell (stream) (inspect stream))))
        (formatting-row (stream)
          (formatting-place (stream object 'vector-total-size-place nil present inspect)
            (with-style (stream :slot-like)
              (formatting-cell (stream) (write-string "Total size" stream))
              (formatting-cell (stream) (present stream)))
            (formatting-cell (stream) (inspect stream))))
        (when fill-pointer
          (formatting-row (stream)
            (formatting-place (stream object 'vector-fill-pointer-place nil present inspect)
              (with-style (stream :slot-like)
                (formatting-cell (stream) (write-string "Fill pointer" stream))
                (formatting-cell (stream) (present stream)))
              (formatting-cell (stream) (inspect stream)))))))

    (with-section (stream) "Elements"
      (let ((place-class (if (adjustable-array-p object)
                             'adjustable-vector-element-place
                             'vector-element-place)))
        (flet ((format-element (stream i)
                 (formatting-place-cell (stream)
                     (object place-class i present inspect)
                   (present)
                   (write-char #\Space stream)
                   (inspect))))
          (formatting-item-list (stream :n-columns 1)
            (loop :for i :from 0 :below (array-total-size object)
                  :if (and fill-pointer (>= i fill-pointer))
                  :do (with-drawing-options (stream :ink +light-gray+) ; TODO style
                        (format-element stream i))
                  :else
                  :do (format-element stream i))))))))

;; TODO displaced
