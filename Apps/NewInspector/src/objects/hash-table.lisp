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

(defclass inspected-hash-table (inspected-object)
  ())

(defmethod make-object-state ((object hash-table) (place t))
  (make-instance 'inspected-hash-table :place place))

;;; Object inspection methods

(defmethod inspect-object-using-state ((object hash-table)
                                       (state  inspected-hash-table)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (with-style (stream :header)
    (princ (class-name (class-of object)) stream)))

(defun draw-hash-table-diagram (stream hash-table)
  (let* ((width                  300)
         (height                 16)
         (size                   (hash-table-size hash-table))
         (rehash-size            (hash-table-rehash-size hash-table))
         (rehash-threshold       (hash-table-rehash-threshold hash-table))
         (max                    (max size
                                      (* size rehash-size)
                                      (* size rehash-threshold)))
         (size-ratio             (/ size max))
         (rehash-size-ratio      (/ (* size rehash-size) max))
         (rehash-threshold-ratio (/ (* size rehash-threshold) max))
         (count-ratio            (/ (hash-table-count hash-table) max)))
    (draw-rectangle* stream 0 0 (* count-ratio width) height :ink (make-contrasting-inks 2 0))

    (draw-rectangle* stream 0 0 (* rehash-size-ratio width) height :filled nil :line-dashes #(2 2))

    (draw-rectangle* stream 0 0 (* size-ratio width) height :filled nil)

    (draw-line* stream (* rehash-threshold-ratio width) 0 (* rehash-threshold-ratio width) height
                :ink (make-contrasting-inks 2 1))))

(defmethod inspect-object-using-state ((object hash-table)
                                       (state  inspected-hash-table)
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

  (with-room-for-graphics (stream)
    (draw-hash-table-diagram stream object))

  (with-section (stream) "Entries"
    (with-placeholder-if-emtpy (stream)
      ((zerop (hash-table-count object)) ; TODO don't call count twice
       "No entries~%")
      (t
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
          object))))))

;;; Commands

(define-command (com-clear-hash-table :command-table inspector
                                      :name          "Clear Hash-table")
    ((object inspected-hash-table))
  (clrhash (object object)))

(define-presentation-to-command-translator inspected-hash-table->com-clear-hash-table
    (inspected-hash-table com-clear-hash-table inspector
     :tester ((object) (plusp (hash-table-count (object object))))
     :priority -1
     :documentation "Clear hash-table entries"
     :pointer-documentation ((object stream)
                             (format stream "~@<Clear all entries of ~A~@:>"
                                     (object object))))
    (object)
  (list object))
