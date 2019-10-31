;;;; (C) Copyright 2019 Jan Moringen
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

(cl:in-package #:clim-broadway)

(defclass surface-manager ()
  ((%next-surface-id :accessor next-surface-id
                     :type     (integer 1)
                     :initform 1)
   (%surfaces        :accessor surfaces
                     :initform '())
   ;; Focus
   (%focused-surface :accessor focused-surface
                     :initform nil)
   ;; Operations on surfaces
   (%operation       :accessor operation
                     :initform nil)))

(defmethod make-surface ((surface-manager surface-manager) &key name sheet)
  (let* ((id      (next-surface-id surface-manager))
         (surface (make-instance 'surface :id id :name name :sheet sheet)))
    (setf (next-surface-id surface-manager) (1+ id))
    (push surface (surfaces surface-manager))
    surface))

(defmethod destroy-surface* ((surface-manager surface-manager) (surface surface)) ; TODO fix name
  (when (eq (focused-surface surface-manager) surface)
    (setf (focused-surface surface-manager) nil))
  (removef (surfaces surface-manager) surface))

;;; Focus management

(defmethod (setf focused-surface) :around ((new-value t) (object surface-manager))
  (let ((old-value (focused-surface object)))
    (when (and old-value (not (eq old-value new-value)))
      (setf (focusedp old-value) nil))
    (call-next-method)
    (when (and new-value (not (eq old-value new-value)))
      (setf (focusedp new-value) t))))

;;; Positioning and resizing

(defclass surface-operation ()
  ((%sheet            :initarg :sheet
                      :reader  sheet)
   (%cursor-initial-x :initarg :cursor-initial-x
                      :reader  cursor-initial-x)
   (%cursor-initial-y :initarg :cursor-initial-y
                      :reader  cursor-initial-y)))

(defclass dragging-surface (surface-operation)
  ((%surface-initial-x :initarg :surface-initial-x
                       :reader  surface-initial-x)
   (%surface-initial-y :initarg :surface-initial-y
                       :reader  surface-initial-y)))

(defclass resizing-surface (surface-operation)
  ((%surface-initial-width  :initarg :surface-initial-width
                            :reader  surface-initial-width)
   (%surface-initial-height :initarg :surface-initial-height
                            :reader  surface-initial-height)))

(defun find-event-surface (event surface-manager) ; TODO this lookup is done in multiple places
  (let ((id (surface event)))
    (when (plusp id)
      (find id (surfaces surface-manager) :key #'id))))

(defun pointer-over-close-button-p (event surface)
  (let* ((close-button (nth 4 (nodes surface)))
         (data         (data close-button)))
    ; (update-node close-button :color #xffc0c0c0)
    (when (and (<= (x data) (win-x event) (+ (x data) (width data)))
               (<= (y data) (win-y event) (+ (y data) (height data))))
      close-button)))

(defun pointer-over-bottom-right-corner-p (event surface)
  (let ((border (data (nth 1 (nodes surface)))))
    (and (<= (- (width border) 8) (win-x event) (width border))
         (<= (- (height border) 8) (win-y event) (height border)))))

(defmethod handle-event ((client surface-manager) (event button-press))
  (let ((surface (find-event-surface event client)))
    ;; SURFACE can be NIL. This can take the focus away from a focused
    ;; surface.
    (setf (focused-surface client) surface)

    (cond ((not surface)
           nil)
          ((pointer-over-close-button-p event surface)
           (let ((sheet (sheet surface)))
             (dispatch-event sheet (make-instance 'window-manager-delete-event
                                                  :sheet sheet)))
           t)
          ((pointer-over-bottom-right-corner-p event surface)
           (let ((sheet (sheet surface)))
             (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region sheet)
               (setf (operation client)
                     (make-instance 'resizing-surface
                                    :sheet                  sheet
                                    :cursor-initial-x       (root-x event)
                                    :cursor-initial-y       (root-y event)
                                    :surface-initial-width  (- x2 x1)
                                    :surface-initial-height (- y2 y1))))))
          ((<= (win-y event) 20)
           (let* ((sheet (sheet surface))
                  (port  (port sheet)))
             ;; Raise the surface.
             (with-port-locked (port)
               (appendf (queued-operations port)
                        (list (lambda (connection)
                                (raise-surface connection (id surface))))))
             ;; Initiate the dragging operation.
             (setf (operation client)
                   (multiple-value-bind (x y)
                       (transform-position (sheet-transformation sheet) 0 0)
                     (make-instance 'dragging-surface
                                    :sheet             sheet
                                    :cursor-initial-x  (root-x event)
                                    :cursor-initial-y  (root-y event)
                                    :surface-initial-x x
                                    :surface-initial-y y))))
           t))))

(defmethod handle-event ((client surface-manager) (event button-release))
  (if (operation client)
      (progn
        (setf (operation client) nil)
        t)
      nil))

(defmethod handle-event ((client surface-manager) (event pointer-move))
  (let ((operation (operation client)))
    (cond ((typep operation 'dragging-surface)
           (setf (sheet-transformation (sheet operation))
                 (make-translation-transformation
                  (+ (surface-initial-x operation) (- (root-x event) (cursor-initial-x operation)))
                  (+ (surface-initial-y operation) (- (root-y event) (cursor-initial-y operation)))))
           t)
          #+no ((when-let* ((surface      (find-event-surface event client))
                            (close-button (pointer-over-close-button-p event surface)))
                  (update-node close-button :color #xffffffff)
                  t))

          ((typep operation 'resizing-surface)
           (let ((sheet (sheet operation)))
             (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region sheet)
               (setf (sheet-region sheet )
                     (make-rectangle*
                      x1
                      y1
                      (+ x1
                         (surface-initial-width operation)
                         (- (root-x event) (cursor-initial-x operation)))
                      (+ y1
                         (surface-initial-height operation)
                         (- (root-y event) (cursor-initial-y operation))))))))

          (t
           nil))))

;;; Events

(defmethod handle-event ((client surface-manager) (event t))
  nil)
