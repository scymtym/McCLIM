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

;;; Utilities

(defun end-or-length (end length)
  (if end
      (min end length)
      length))

(defun print-sequence-header (stream kind length start end)
  (format stream "~A of length ~:D" kind length)
  (let ((end (end-or-length end length)))
    (when (or (plusp start) (< end length))
      (write-char #\Space stream)
      (with-style (stream :note)
        (format stream "~:D … ~:D shown" start end)))))

(defun note-truncated (stream length shown-count)
  (with-style (stream :note)
    (format stream "~:D element~:P not shown" (- length shown-count))))

;;; Object states

(defclass inspected-sequence (inspected-object)
  ((%start :initarg  :start
           :accessor start
           :initform 0)
   (%end   :initarg  :end
           :accessor end
           :initform 30)))

(defmethod effective-bounds ((state inspected-sequence) (length integer))
  (let ((start (start state))
        (end   (end-or-length (end state) length)))
    (values start end (or (plusp start) (< end length)))))

(defmethod inspect-object-using-state ((object sequence)
                                       (state  inspected-sequence)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (print-sequence-header
   stream (type-of object) (length object) (start state) (end state)))

;;; Commands

(define-command (com-show-all :command-table inspector
                              :name          "Show all elements")
    ((object 'inspected-sequence :gesture (:select :priority -1)))
  (setf (end object) nil))

(define-command (com-show-next :command-table inspector
                               :name          "Show next page of elements")
    ((object 'inspected-sequence :gesture (:select
                                        ; TODO :documentation
                                           :priority -1)))
  (let ((delta (- (end object) (start object))))
    (incf (start object) delta)
    (incf (end object)   delta)))

(define-command (com-show-twice-as-many :command-table inspector
                                        :name          "Show twice as many elements")
    ((object 'inspected-sequence :gesture (:select
                                        ; TODO :documentation
                                           :priority -1)))
  (setf (end object) (* 2 (end object))))
