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

(defun call-with-preserved-cursor-x (thunk stream)
  (let ((old-x (stream-cursor-position stream)))
    (prog1
        (funcall thunk stream)
      (setf (stream-cursor-position stream)
            (values old-x (nth-value 1 (stream-cursor-position stream)))))))

(defmacro with-preserved-cursor-x ((stream) &body body)
  (check-type stream symbol)
  `(call-with-preserved-cursor-x (lambda (,stream) ,@body) ,stream))

(defun call-with-preserved-cursor-y (thunk stream)
  (let ((old-y (nth-value 1 (stream-cursor-position stream))))
    (prog1
        (funcall thunk stream)
      (setf (stream-cursor-position stream)
            (values (stream-cursor-position stream) old-y)))))

(defmacro with-preserved-cursor-y ((stream) &body body)
  (check-type stream symbol)
  `(call-with-preserved-cursor-y (lambda (,stream) ,@body) ,stream))

;;; Styles

(defmacro with-style ((stream style &rest extra-drawing-options &key)
                      &body body)
  (let ((args (ecase style
                (:header    '(                   :text-face :bold))
                (:changable '(:ink +dark-violet+))
                (:slot-like '(:ink +dark-orange+                    :text-size :small))
                (:unbound   '(:ink +dark-gray+   :text-face :italic))
                (:error     '(:ink +dark-red+    :text-face :italic)))))
    `(with-drawing-options (,stream ,@args ,@extra-drawing-options)
       ,@body)))

(defun call-with-section (body-thunk title-thunk stream)
  (with-preserved-cursor-x (stream)
    (with-style (stream :header)
      (funcall title-thunk stream))
    (fresh-line stream)
    (indenting-output (stream "    ")
      (funcall body-thunk stream)
      #+TODO-this-illustrates-the-indentation-problem (progn
        (format stream "start~%")
        (format stream "start~%")
        (with-preserved-cursor-x (stream) (funcall body-thunk stream))
        (format stream "end~%")
        (format stream "end~%")))))

(defmacro with-section ((stream) title &body body)
  (check-type stream symbol)
  `(call-with-section
    (lambda (,stream) ,@body)
    (lambda (,stream)
      ,(typecase title
         (string `(write-string ,title ,stream))
         (t      title)))
    ,stream))

(defun call-with-placeholder-if-empty (test-thunks empty-thunks non-empty-thunk stream)
  (or (some (lambda (test-thunk empty-thunk)
              (when (funcall test-thunk)
                (with-style (stream :unbound)
                  (funcall empty-thunk stream))
                t))
            test-thunks empty-thunks)
      (funcall non-empty-thunk stream)))

(defmacro with-placeholder-if-emtpy ((stream) &body clauses)
  (check-type stream symbol)
  (loop :for (test . body) :in clauses
        :unless (eq test t)
          :collect `(lambda () ,test) :into test-thunks
          :and :collect `(lambda (,stream)
                           ,@(typecase body
                               ((cons string) `((format ,stream ,(first body))))
                               (t             body)))
                 :into empty-thunks
        :finally (return `(call-with-placeholder-if-empty
                           (list ,@test-thunks) (list ,@empty-thunks)
                           (lambda (,stream) ,@body)
                           ,stream))))

;;; Tables

(defmacro formatting-header ((stream) &body columns)
  (check-type stream symbol)
  `(with-style (,stream :header)
     (formatting-row (,stream)
       ,@(map 'list (lambda (column)
                      `(formatting-cell (,stream)
                         ,(typecase column
                            (string `(write-string ,column ,stream))
                            (t      column))))
              columns))))

;;; Badges

(defun call-with-output-as-badge (thunk stream)
  (with-preserved-cursor-y (stream)
    (surrounding-output-with-border (stream :shape      :rounded
                                            :background +light-blue+
                                            :radius     2
                                            :padding    2)
      (with-drawing-options (stream :text-face :roman :text-size :smaller)
        (funcall thunk stream)))))

(defmacro with-output-as-badge ((stream) &body body)
  (check-type stream symbol)
  `(call-with-output-as-badge (lambda (,stream) ,@body) ,stream))

(defun badge (stream format-control &rest format-arguments)
  (with-output-as-badge (stream)
    (apply #'format stream format-control format-arguments)))

;;; Object border

(defun color-for-bar (depth)
  (let ((limit (contrasting-inks-limit nil)))
    (make-contrasting-inks limit (mod depth limit))))

(defun call-with-object-border (thunk stream depth)
  (surrounding-output-with-border
      (stream :padding        2
              :shape          :rectangle
              :line-thickness 2
              :line-dashes    '(1 1)
              :left-ink       (color-for-bar (1- depth))
              :top-ink        nil
              :right-ink      nil
              :bottom-ink     nil)
    (funcall thunk stream)))

(defmacro with-object-border ((stream depth) &body body)
  `(call-with-object-border (lambda (,stream) ,@body) ,stream ,depth))

;;; Safety

;; TODO can we make this constant?
(defvar *standard-pprint-dispatch* (with-standard-io-syntax *print-pprint-dispatch*))

(defun call-with-safe-and-terse-printing (thunk)
  (let ((*print-circle*          t)
        (*print-length*          3)
        (*print-level*           3)
        (*print-pprint-dispatch* *standard-pprint-dispatch*))
    (funcall thunk)))

(defun call-with-print-error-handling (thunk stream)
  (handler-case
      (funcall thunk)
    (error ()
      (with-style (stream :error)
        (write-string "error printing object" stream)))))

(defmacro with-print-error-handling ((stream) &body body)
  `(call-with-print-error-handling (lambda () ,@body) ,stream))
