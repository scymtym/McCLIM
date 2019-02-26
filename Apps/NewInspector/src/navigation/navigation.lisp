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

(defclass history ()
  ((%elements :initarg  :elements
              :accessor elements
              :initform '())))

(defmethod push-element ((element t) (history history))
  (push element (elements history)))

(defmethod pop-element ((history history))
  (pop (elements history))
  (first (elements history)))

(defclass history-pane (application-pane)
  ((%history :initarg  :history
             :reader   history
             :initform (make-instance 'history)))
  (:default-initargs
   :end-of-line-action :scroll))

(define-presentation-type element ())

(define-presentation-method present ((object t)
                                     (type   element)
                                     (stream t)
                                     (view   t)
                                     &key)
  (princ object stream))

#+no (define-presentation-type history ())

#+no (define-presentation-method )

(defmethod redisplay-frame-pane ((frame application-frame)
                                 (pane  history-pane)
                                 &key force-p)
  (declare (ignore force-p))
  (let ((first? t))
    (format-items (reverse (elements (history pane)))
                  :printer (lambda (element stream)
                             (if first?
                                 (setf first? nil)
                                 (write-string "»" stream)
                                 )
                             (present element 'element
                                      :stream     stream
                                      :single-box t))
                  :stream  pane
                  :n-rows  1)))

(define-command-table navigation)

(define-command (com-select :command-table navigation
                            :name          "Inspect object")
    ((object inspected-object :gesture :describe))
  (let* ((frame   *application-frame*)
         (state   (state (find-pane-named frame 'inspector)))
         (history (history (find-pane-named frame 'history))))
    (push-element (root-place state) history)
    (setf (root-place state) (make-instance 'root-place :cell (object object))))) ; TODO getting the object can fail

(define-command (com-visit :command-table navigation
                           :name          t)
    ((object element :gesture :select))
  (let* ((frame   *application-frame*)
         (state   (state (find-pane-named frame 'inspector)))
         (history (history (find-pane-named frame 'history))))
    (setf (root-place state) object)))

(define-command (com-back :command-table navigation
                          :name          t
                          :keystroke     (#\l :meta))
    ()
  (let* ((frame   *application-frame*)
         (state   (state (find-pane-named frame 'inspector)))
         (history (history (find-pane-named frame 'history))))
    (setf (root-place state) (pop-element history))))
