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

(defclass inspector-pane (application-pane #+n clim-stream-pane) ; TODO can we be more specific?
  ((%state :reader   state
           :writer   (setf %state))))

(defmethod shared-instance :before ((instance   inspector-pane)
                                    (slot-names t)
                                    &key
                                    (state nil state-supplied-p)
                                    (root  nil root-supplied-p))
  (declare (ignore state root))
  (when (and state-supplied-p root-supplied-p)
    (error "~@<The initargs ~S and ~S are mutually exclusive.~@:>"
           :state :root)))

(defmethod initialize-instance :after ((instance inspector-pane)
                                        &key
                                        (state nil state-supplied-p)
                                        (root  nil root-supplied-p))
  (declare (ignore state root))
  (unless (or state-supplied-p root-supplied-p)
    (setf (%state instance) (make-instance 'inspector-state))))

(defmethod shared-initialize :after ((instance   inspector-pane)
                                     (slot-names t)
                                     &key
                                     (state nil state-supplied-p)
                                     (root  nil root-supplied-p))
  (cond (state-supplied-p
         (setf (%state instance) state))
        (root-supplied-p
         (setf (%state instance) (make-instance 'inspector-state
                                                :root-object root)))))

(defmethod (setf %state) :after ((new-value t) (object inspector-pane))
  (push (lambda (new-root-place)
          (declare (ignore new-root-place))
          (queue-redisplay object))
        (change-hook new-value)))

(defmethod redisplay-frame-pane ((frame application-frame)
                                 (pane  inspector-pane)
                                 &key force-p)
  (declare (ignore force-p))
  (present-inspected-object-graph (state pane) pane))

;;; Redisplay

(defclass redisplay-event (climi::standard-event) ())

(defmethod queue-redisplay ((pane inspector-pane))
  (queue-event pane (make-instance 'redisplay-event :sheet pane)))

(defmethod handle-event ((client inspector-pane) (event redisplay-event))
  (redisplay-frame-pane (pane-frame client) client :force-p t))
