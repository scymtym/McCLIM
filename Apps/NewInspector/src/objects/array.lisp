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

(defmethod supportsp ((place     vector-element-place) ; TODO is this the default?
                      (operation (eql 'remove-value)))
  nil)

(defmethod value ((place vector-element-place))
  (row-major-aref (container place) (cell place)))

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

(defmethod inspect-object-using-state ((object vector)
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
                   (present stream)
                   (write-char #\Space stream)
                   (inspect stream))))
          (formatting-item-list (stream :n-columns 1)
            (loop :for i :from 0 :below (array-total-size object)
                  :if (and fill-pointer (>= i fill-pointer))
                  :do (with-drawing-options (stream :ink +light-gray+) ; TODO style
                        (format-element stream i))
                  :else
                  :do (format-element stream i))))))))

;; TODO displaced
(defmethod inspect-object-using-state ((object array)
                                       (state  inspected-object)
                                       (styel  (eql :expanded-body))
                                       (stream t))
  (with-section (stream) "Elements"
    (case (array-rank object)
      (2
       (let ((row-count    (array-dimension object 0))
             (column-count (array-dimension object 1)))
         (formatting-table (stream)
           (loop :for row :from 0 :below row-count
                 :do (formatting-row (stream)
                       (loop :for column :from 0 :below column-count
                             :for i = (array-row-major-index object row column)
                             :do (formatting-cell (stream)
                                   (formatting-place-cell (stream)
                                       (object 'vector-element-place i present inspect)
                                     (present stream)
                                     (write-char #\Space stream)
                                     (inspect stream))))))))))))
