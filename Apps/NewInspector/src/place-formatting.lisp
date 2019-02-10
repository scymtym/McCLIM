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

(defun call-with-place-inspector (thunk state place-class stream)
  (flet ((make-place (cell)
           (make-instance place-class :container state :cell cell))
         (present-place (place)
           (present place 'place :stream stream))
         (inspect-place (place)
           (let ((*place* place))
             (inspect-object (value place) stream))))
    (funcall thunk #'make-place #'present-place #'inspect-place)))

(defmacro with-place-inspector ((state place-class stream) (make inspect present) &body body)
  (with-gensyms (make-var inspect-var present-var)
    `(call-with-place-inspector
      (lambda (,make-var ,inspect-var ,present-var)
        (flet ((,make (cell)
                 (funcall ,make-var cell))
               (,inspect (place)
                 (funcall ,inspect-var place))
               (,present (place)
                 (funcall ,present-var place)))
          ,@body))
      ,state ,place-class ,stream)))

(defmacro formatting-place-list ((container place-class stream &rest args &key) &body body)
  (once-only (container place-class stream)
    `(formatting-item-list (stream ,@args)
       (macrolet ((formatting-place-cell (stream-var cell place-var inspect present)
                      `(formatting-cell (,stream-var)
                         (let ((,place-var (ensure-child ,cell ,place-class *parent-place*
                                                         (lambda ()
                                                           (make-instance place-class
                                                                          :container ,container
                                                                          :cell      ,cell)))))
                           (flet ((,present (stream)
                                    (present ,place-var 'place :stream stream))
                                  (,inspect (stream)
                                    (let ((*place* ,place-var))
                                      (inspect-object )))))))))
         ,@body))))

(defmacro formatting-place-cell ((stream &rest args)
                                 (container place-class cell present inspect
                                  &key (place-var (gensym "PLACE")))
                                 &body body)
  (once-only (container place-class stream)
    `(formatting-cell (,stream ,@args)
       (let ((,place-var (ensure-child ,container ,cell ,place-class *parent-place*
                                       (lambda ()
                                         (make-instance ,place-class
                                                        :container ,container
                                                        :cell      ,cell)))))
         (flet ((,present (&optional (stream ,stream))
                  (present ,place-var 'place :stream stream))
                (,inspect (&optional (stream ,stream))
                  (let ((*place* ,place-var))
                    (inspect-object (value ,place-var) stream))))
           ,@body)))))

(defmacro formatting-place ((stream container place-class cell present inspect
                             &key (place-var (gensym "PLACE")))
                            &body body)
  (once-only (container place-class stream)
    `(let ((,place-var (ensure-child ,container ,cell ,place-class *parent-place*
                                     (lambda ()
                                       (make-instance ,place-class
                                                      :container ,container
                                                      :cell      ,cell)))))
       (flet (,@(when present
                  `((,present (&optional (stream ,stream))
                              (present ,place-var 'place :stream stream))))
              ,@(when inspect
                  `((,inspect (&optional (stream ,stream))
                              (inspect-place ,place-var stream)))))
         ,@body))))