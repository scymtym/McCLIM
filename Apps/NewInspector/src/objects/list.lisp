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

;;; Places

(defclass car-place (basic-place)
  ())

(defmethod value ((place car-place))
  (car (cell place)))

(defmethod (setf value) (new-value (place car-place))
  (setf (car (cell place)) new-value))

(defclass cdr-place (basic-place)
  ())

(defmethod value ((place cdr-place))
  (cdr (cell place)))

(defmethod (setf value) (new-value (place cdr-place))
  (setf (cdr (cell place)) new-value))

(defclass list-element-place (sequence-element-place
                              car-place)
  ())

(defmethod supportsp ((place list-element-place) (operation (eql 'remove-value)))
  t)

(defmethod remove-value ((place list-element-place))
  (loop :for predecessor :on (container place)
        :for middle = (rest predecessor)
        :for successor = (rest middle)
        :when (eq middle (cell place))
        :do (setf (cdr predecessor) successor)
            (return)))

(defclass alist-element-place (list-element-place)
  ())

(defmethod remove-value ((place alist-element-place))
  (delete (cell place) (container place)))

(defclass alist-key-place (key-place
                           alist-element-place)
  ())

(defclass alist-value-place (value-place
                             cdr-place
                             alist-element-place)
  ())

;;; Object states

(defclass inspected-improper-list (inspected-sequence)
  ())

(defclass inspected-proper-list (inspected-sequence)
  ())

(defclass inspected-alist (inspected-proper-list)
  ())

(defmethod object-state-class ((object cons) (place t))
  (cond ((not (alexandria:proper-list-p object))
         'inspected-improper-list)
        ((and (not (alexandria:length= 1 object))
              (every (alexandria:of-type '(cons (not cons) t)) object))
         'inspected-alist)
        (t
         'inspected-proper-list)))

;;; Object inspection methods

;; Inspect a cons cell in a traditional, plain-text format. The only
;; difference between this and simply using the Lisp printer is that
;; this code takes advantage of CLIM's tables and presentations to
;; make the list as interactive as you would expect.

;; Inspect a cons cell in a traditional, plain-text format. The only
;; difference between this and simply using the Lisp printer is that
;; this code takes advantage of CLIM's tables and presentations to
;; make the list as interactive as you would expect.

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-object) ; TODO state
                                       (style  (eql :graph))
                                       (stream t))
  ;; Inspect a cons cell in a fancy graphical way. The inconvenient
  ;; part is that this necessarily involves quite a bit of clicking to
  ;; show a moderately-sized list.
  (if (null (cdr object))
      (formatting-table (stream)
        (formatting-column (stream)
          (formatting-cell (stream)
            (with-output-as-presentation (stream object 'cons)
              (draw-rectangle* stream 0 0 20 10 :filled nil))
            (draw-line* stream 10 0 10 10)
            (draw-arrow* stream 5 5 5 30)
            (draw-line* stream 10 10 20 0))
          (formatting-cell (stream)
            (inspect-object (car object) stream))))
      (formatting-table (stream)
        (formatting-row (stream)
          (formatting-cell (stream)
            (formatting-table (stream)
              (formatting-column (stream)
                (formatting-cell (stream)
                  (with-output-as-presentation (stream object 'cons)
                    (draw-rectangle* stream 0 0 20 10 :filled nil))
                  (draw-line* stream 10 0 10 10)
                  (draw-arrow* stream 5 5 5 30)
                  (draw-arrow* stream 15 5 40 5))
                (formatting-cell (stream)
                  (inspect-object (car object) stream)))))
          (formatting-cell (stream)
            (inspect-object (cdr object) stream))))))

;; TODO surround whole thing with with-output-as-presentation (stream object 'cons) ?
(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-alist)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (print-sequence-header
   stream "alist-shaped list" (length object) (start state) (end state)))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-alist)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (let ((length (length object)))
    (multiple-value-bind (start end truncated?)
        (effective-bounds state length)
      (with-preserved-cursor-x (stream)
        (formatting-table (stream)   ; inspector-table (object stream)
          (loop :for i :from start :below end
                :for cell :in (nthcdr start object)
                :for (key . value) = cell
                :do (formatting-row (stream)
                      (with-style (stream :slot-like)
                        (formatting-place-cell (stream :align-y :center)
                            (object 'alist-key-place cell present inspect)
                          (present stream)
                          (inspect stream)))
                      (formatting-place
                          (stream object 'alist-value-place cell present inspect)
                        (formatting-cell (stream :align-x :center :align-y :center)
                          (present stream))
                        (formatting-cell (stream :align-y :center)
                          (inspect stream)))))))
      (when truncated?
        (note-truncated stream length (- end start))))))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-proper-list)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (print-sequence-header
   stream "proper list" (length object) (start state) (end state)))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-proper-list)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (let ((length (length object)))
    (multiple-value-bind (start end truncated?) (effective-bounds state length)
      (with-preserved-cursor-x (stream)
        (formatting-item-list (stream :n-columns 1)
          (loop :for i :below (- end start)
                :for cell :on (nthcdr start object)
                :do (formatting-place-cell (stream)
                        (object 'list-element-place cell present inspect)
                      (present)
                      (write-char #\Space stream)
                      (inspect)))))
      (when truncated?
        (note-truncated stream length (- end start))))))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-improper-list)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (format stream "improper list"))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-improper-list)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream :background +red+)
    (formatting-row (stream)
      (formatting-cell (stream)
        (princ "(" stream))
      (loop :for cell :on object
            :for (car . cdr) = (when (listp cell) cell)
            :for i :from 0
            :while (consp (cdr cell))
            :do (formatting-place-cell (stream)
                    (object 'list-element-place cell present inspect)
                  (present)
                  (inspect))
                (formatting-cell (stream) (write-string " " stream))
            :finally (cond ((null cdr)
                            (formatting-place-cell (stream)
                                (object 'list-element-place cell present inspect)
                              (inspect))
                            (formatting-cell (stream) (princ ")" stream))
                            t)
                           ((atom cdr)
                            (formatting-place-cell (stream)
                                (object 'list-element-place cell present inspect)
                              (present)
                              (inspect))
                            (formatting-cell (stream)
                              (with-drawing-options (stream :text-face :bold :ink +forest-green+)
                                (princ "." stream)))
                            (formatting-place-cell (stream)
                                (object 'cdr-place cell present inspect)
                              (present)
                              (inspect))
                            (formatting-cell (stream) (princ ")" stream))
                            t)
                           ((and *print-length* (>= i *print-length*))
                            (with-output-as-presentation (stream cell 'long-list-tail)
                              (formatting-cell (stream) (princ "...)" stream)))
                            t)
                           (t nil))))))

#+no (defmethod inspect-object-using-state ((object cons)
                                       (state  )
                                       (style  :graph)
                                       (stream t))
  ;; Decide how to display the cons by looking in cons-cell-dico
  (cond ((as-cells-p (object-state object (state *application-frame*)))
         (inspect-cons-as-cells object stream))
        ((not (alexandria:proper-list-p object))
         (inspect-cons-as-improper-list object stream))
        ((and (not (alexandria:length= 1 object))
              (every (alexandria:of-type '(cons (not cons) t)) object))
         (inspect-cons-as-alist object stream))
        (t
         (inspect-cons-as-proper-list object stream))))
