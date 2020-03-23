;;;; (C) Copyright 2019, 2020 Jan Moringen
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

(defclass broadway-port (climi::standard-port
                         mcclim-render-internals::render-port-mixin)
  ((%listen-port       :initarg  :listen-port
                       :reader   listen-port
                       :writer   (setf %listen-port))
   ;;
   (%surface-manager   :reader   surface-manager
                       :initform (make-instance 'surface-manager))
   ;;
   (%queued-operations :accessor queued-operations
                       :initform '())
   (%pointer           :reader   port-pointer
                       :writer   (setf %port-pointer))))

(defmethod initialize-instance :after ((instance broadway-port)
                                       &key
                                       server-path)
  (destructuring-bind (type &key (port 9090)) server-path
    (declare (ignore type))
    ;;
    (push (make-instance 'broadway-frame-manager :port instance)
          (slot-value instance 'climi::frame-managers))

    ;;
    (setf (%port-pointer instance) (make-instance 'broadway-pointer :port instance))

    ;;
    (setf (%listen-port instance) port)
    (setf (slot-value instance 'climi::event-process)
          (clim-sys:make-process
           (curry #'run-server
                  :port     (listen-port instance)
                  :the-port instance)
           :name (format nil "~A's event process" instance)))))

#+later (defun parse-path (path)
  (destructuring-bind (type &key (port 9090)) path
    (list type :listen-port port)))

(setf (get :broadway :server-path-parser) 'identity ; 'parse-path
      (get :broadway :port-type)          'broadway-port)

;;; Medium

(defmethod make-medium ((port broadway-port) sheet)
  (make-instance 'broadway-medium
                 ;; :port port
                 ;; :graft (find-graft :port port)
                 :sheet sheet))

;;; Graft

(defmethod graft ((port broadway-port))
  (first (climi::port-grafts port)))

(defmethod graft-height ((graft graft) &key units)
  (bounding-rectangle-height graft))

(defmethod graft-width ((graft graft) &key units)
  (bounding-rectangle-width graft))

;;; Sheet

(defclass surface1 (mcclim-render-internals::image-mirror-mixin)
  ((%id              :initarg  :id
                     :reader   id)
                                                  ;(%texture-id )
   (%name            :initarg  :name
                     :accessor name)
   ;; Textures
   (%next-texture-id :initarg  :next-texture-id
                     :type     (integer 1)
                     :accessor next-texture-id
                     :initform 1)
   (%textures        :reader   textures
                     :initform (make-array 0 :adjustable t :fill-pointer t))
   (%texture->node   :reader   %texture->node
                     :initform (make-hash-table :test #'eq))
   ;; Nodes
   (%next-node-id    :initarg  :next-node-id
                     :type     (integer 1)
                     :accessor next-node-id
                     :initform 1)
   (%root            :initarg  :root
                     :accessor root)
   (%nodes           :reader   nodes
                     :initform (make-array 0 :adjustable t :fill-pointer t))
   ;; Back buffers
   (%old-back-buffer :accessor old-back-buffer
                     :initform nil)
   (%new-back-buffer :accessor new-back-buffer
                     :initform nil)
   ;; HACK
   (%createdp        :accessor createdp
                     :initform nil)))

(defmethod realize-mirror ((port broadway-port) (mirrored-sheet t))
                                        ; (setf (sheet-parent pixmap) (graft port))
  (let* ((mirror (make-surface (surface-manager port)
                               :name  (clime:sheet-pretty-name mirrored-sheet) ; TODO sheet may be unnamed
                               :sheet mirrored-sheet))
         (id     (id mirror))
         (showp  (sheet-enabled-p mirrored-sheet)))

    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (connection)
                       (new-surface connection id 0 0 100 100)
                       (when showp
                         (show-surface connection id))
                       (setf (createdp mirror) t)))))
    (climi::port-register-mirror port mirrored-sheet mirror)

    ; (make-texture mirror 0 0 0 0)
    #+no (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (stream)
                       (loop :for image = (mcclim-render-internals::image-mirror-image mirror)
                             :until image
                             :do (sleep .1)
                             :finally (progn (sleep .1) (upload-image stream (id (first-elt (textures mirror))) image)))))))

    (sleep .1)

    mirror))

(defmethod destroy-mirror ((port broadway-port) (mirrored-sheet t))
  (let* ((mirror (climi::port-lookup-mirror port mirrored-sheet))
         (id     (id mirror)))

    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (stream)
                       (destroy-surface* (surface-manager port) mirror)
                       (destroy-surface stream id)))))

    (climi::port-unregister-mirror port mirrored-sheet mirror)))

(defmethod climb:port-set-mirror-name ((port broadway-port) (mirror t) (name t))
  (setf (name mirror) name))

(defmethod climb:port-set-mirror-transformation ((port                  broadway-port)
                                                 (mirror                surface)
                                                 (mirror-transformation t))
  (let ((sheet (climi::port-lookup-sheet port mirror)))
    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (connection)
                       (with-bounding-rectangle* (x1 y1 x2 y2)
                           (transform-region (climi::%sheet-mirror-transformation sheet)
                                             (climi::%sheet-mirror-region sheet))
                         (resize-surface connection (id mirror) x1 y1 (- x2 x1) (- y2 y1)))))))))

(defmethod climb:port-set-mirror-region ((port          broadway-port)
                                         (mirror        surface)
                                         (mirror-region t))
  (let* ((surface mirror)
         (sheet   (climi::port-lookup-sheet port mirror)))
    (with-bounding-rectangle* (x1 y1 x2 y2)
        (transform-region (climi::%sheet-mirror-transformation sheet)
                          mirror-region)
      (with-port-locked (port)
        (appendf (queued-operations port)
                 (list (lambda (connection)
                         (let* ((border-width     4)
                                (title-bar-height 20)

                                (width            (1+ (- x2 x1)))
                                (height           (1+ (- y2 y1)))

                                (effective-x1     (- x1 border-width))
                                (effective-y1     (- y1 title-bar-height border-width))
                                (effective-x2     (+ x2 border-width))
                                (effective-y2     (+ y2 border-width))
                                (effective-width  (- effective-x2 effective-x1))
                                (effective-height (- effective-y2 effective-y1)))

                           (setf (old-back-buffer surface) (make-array (list height width) :element-type 'argb-pixel)
                                 (new-back-buffer surface) (make-array (list height width) :element-type 'argb-pixel))

                           (print (list effective-x1 effective-y1 effective-width effective-height))
                           (resize-surface connection (id surface)
                                           (max 0 effective-x1) (max 0 effective-y1)
                                           effective-width effective-height)

                           (unless (and (= (width surface) (- x2 x1))
                                        (= (height surface) (- y2 y1)))
                             (apply #'resize-surface-nodes (tree surface)
                                    (append (nodes surface) (list width height))))))))))))

(defmethod climb:port-enable-sheet ((port  broadway-port)
                                    (sheet mirrored-sheet-mixin))
  (let* ((mirror (sheet-mirror sheet))
         (id     (id mirror)))

    (repaint-sheet sheet +everywhere+)

    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (stream)
                       (show-surface stream id)))))))

(defmethod climb:port-disable-sheet ((port  broadway-port)
                                     (sheet mirrored-sheet-mixin))
  (let* ((mirror (sheet-mirror sheet))
         (id     (id mirror)))

    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (stream)
                       (hide-surface stream id)))))))

;;; Pixmap

(defmethod clim:destroy-mirror ((port broadway-port)
                                (mirrored-sheet mcclim-render-internals::image-pixmap-mixin))
  (let ((mirror (sheet-direct-mirror mirrored-sheet)))
    (climi::port-unregister-mirror port mirrored-sheet mirror)))

(defmethod clim:realize-mirror ((port broadway-port)
                                (mirrored-sheet mcclim-render-internals::image-pixmap-mixin))
  ; (setf (sheet-parent pixmap) (graft port))
  (let ((mirror (make-instance 'mcclim-render-internals::image-mirror-mixin)))
    (climi::port-register-mirror port mirrored-sheet mirror)
    (mcclim-render-internals::%make-image mirror mirrored-sheet)))

(defmethod climb:port-allocate-pixmap ((port broadway-port) sheet width height)
  (let ((pixmap (make-instance 'broadway-pixmap
                               :region (make-rectangle* 0 0 width height)
                               :sheet sheet
                               :width width
                               :height height
                               :port port)))
    (when t ; (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod climb:port-deallocate-pixmap ((port broadway-port)
                                         (pixmap mcclim-render-internals::image-pixmap-mixin))
  (when (climi::port-lookup-mirror port pixmap)
    (climb:destroy-mirror port pixmap)))

(defclass broadway-pixmap (mcclim-render-internals::image-pixmap-mixin
                           basic-pane)
  ())

;;; Keyboard input focus

(defmethod climb:port-frame-keyboard-input-focus ((port  broadway-port)
                                                  (frame application-frame))
  (frame-properties frame 'focus))

(defmethod (setf climb:port-frame-keyboard-input-focus) ((focus t)
                                                         (port  broadway-port)
                                                         (frame application-frame))
  (setf (frame-properties frame 'focus) focus))

;;; Cursor

(defmethod set-sheet-pointer-cursor ((port broadway-port) sheet cursor)
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (connection)
                       (set-cursor connection (id mirror) cursor)))))))

;;; Pointer

(defclass broadway-pointer (standard-pointer)
  ((%port     :initarg  :port
              :reader   port)
   (%position :accessor %pointer-position)))

(defmethod pointer-position ((pointer broadway-pointer))
  (values-list (%pointer-position pointer)))

(defmethod climb:synthesize-pointer-motion-event ((pointer broadway-pointer))
  (make-instance 'pointer-motion-event
                 :timestamp 0
                 :pointer 0 :button 0 :modifier-state 0
                 :x 0 :y 0 :graft-x 0 :graft-y 0
                 :sheet (climi::port-pointer-sheet (port pointer))))
