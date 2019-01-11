(cl:in-package #:new-inspector)

;; `hash-table-key-place'

(defclass hash-table-key-place (key-place)
  ())

(defmethod supportsp ((place     hash-table-key-place)
                      (operation (eql 'remove-value)))
  t)

(defmethod value ((place hash-table-key-place))
  (cell place))

(defmethod (setf value) (new-value (place hash-table-key-place))
  (let* ((hash-table (container place))
         (old-key    (cell place))
         (old-value  (gethash old-key hash-table)))
    (remhash old-key hash-table)
    (setf (gethash new-value hash-table) old-value)))

(defmethod make-unbound ((place hash-table-key-place))
  (remhash (cell place) (container place)))

;; `hash-table-value-place'

(defclass hash-table-value-place (value-place)
  ())

(defmethod supportsp ((place     hash-table-value-place)
                      (operation (eql 'remove-value)))
  t)

(defmethod value ((place hash-table-value-place))
  (gethash (cell place) (container place)))

(defmethod (setf value) (new-value (place hash-table-value-place))
  (setf (gethash (cell place) (container place)) new-value))

(defmethod make-unbound ((place hash-table-value-place))
  (remhash (cell place) (container place)))

;;; Object state

(defmethod make-object-state ((object hash-table)
                              (place  t))
  (make-instance 'inspected-object :place place))

;;; Object inspection methods

(defmethod inspect-object-using-state ((object hash-table)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (with-style (stream :header)
    (princ (class-name (class-of object)) stream)))

(defmethod inspect-object-using-state ((object hash-table)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place (hash-table-test object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Test" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))
        #+sbcl (formatting-place (stream object 'pseudo-place (sb-ext:hash-table-synchronized-p object) present inspect)
                 (with-style (stream :slot-like)
                   (formatting-cell (stream) (write-string "Synchronized" stream))
                   (formatting-cell (stream) (present stream)))
                 (formatting-cell (stream) (inspect stream))))
      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place (hash-table-count object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Count" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))
        (formatting-place (stream object 'pseudo-place (hash-table-size object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Size" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream))))
      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place (hash-table-rehash-size object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Rehash Size" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))
        (formatting-place (stream object 'pseudo-place (hash-table-rehash-threshold object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Rehash Threshold" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream))))))
  (with-section (stream) "Entries"
    (formatting-table (stream)
      (maphash
       (lambda (key value)
         (declare (ignore value))
         (formatting-row (stream)
           (formatting-place-cell (stream)
               (object 'hash-table-key-place key present inspect)
             (with-style (stream :slot-like)
               (present stream)
               (inspect stream)))
           (formatting-place (stream object 'hash-table-value-place key present inspect)
             (formatting-cell (stream :align-y :center) (present))
             (formatting-cell (stream :align-y :center) (inspect)))))
       object))))
