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
          (clim-sys:make-process (curry #'run-server
                                        :port     (listen-port instance)
                                        :the-port instance)))))

#+later (defun parse-path (path)
  (destructuring-bind (type &key (port 9090)) path
    (list type :listen-port port)))

(setf (get :broadway :server-path-parser) 'identity ; 'parse-path
      (get :broadway :port-type)          'broadway-port)

;;;

(defclass broadway-medium (mcclim-render-internals::render-medium-mixin
                           basic-medium)
  ())

(defmethod make-medium ((port broadway-port) sheet)
  (make-instance 'broadway-medium
                 ;; :port port
                 ;; :graft (find-graft :port port)
                 :sheet sheet))

;;; Graph

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
                     :initform nil)
   (%upload-pending-p :accessor upload-pending-p
                      :initform nil)))

#+unused (defmethod make-node ((surface surface) (data t) &key parent)
  (let ((id (next-node-id surface)))
    (setf (next-node-id surface) (1+ id))
    (make-instance 'node :id id :data data :parent parent)))

(defmethod make-node ((surface surface) (data texture) &key parent)
  (declare (ignore parent))
  (let ((node (call-next-method)))
    (setf (gethash data (%texture->node surface)) node)
    node))

#+unused (defmethod make-texture ((surface surface) x y width height)
  (let ((id (next-texture-id surface)))
    (setf (next-texture-id surface) (1+ id))
    (let ((texture (make-instance 'texture :x      (float x      1.0f0)
                                           :y      (float y      1.0f0)
                                           :width  (float width  1.0f0)
                                           :height (float height 1.0f0)
                                           :id     id)))
      (vector-push-extend texture (textures surface))
      texture)))

#+unused (defclass node ()
  ((%id       :initarg  :id
              :reader   id)
   (%data     :initarg  :data
              :reader   data)
   ;;
   (%parent   :initarg  :parent
              :reader   parent)
   (%children :initarg  :children
              :accessor children
              :initform '())))

(defmethod clim:destroy-mirror ((port           broadway-port)
                                (mirrored-sheet t))
  (let* ((mirror (climi::port-lookup-mirror port mirrored-sheet))
         (id     (id mirror)))

    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (stream)
                       (destroy-surface* (surface-manager port) mirror)
                       (destroy-surface stream id)))))

    (climi::port-unregister-mirror port mirrored-sheet mirror)))

(defmethod clim:realize-mirror ((port           broadway-port)
                                (mirrored-sheet t))
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
        (print (transform-region (climi::%sheet-mirror-transformation sheet)
                                 mirror-region))
      (with-port-locked (port)
        (appendf (queued-operations port)
                 (list (lambda (connection)
                         (let* ((border-width     4)

                                (width            (1+ (- x2 x1)))
                                (height           (1+ (- y2 y1)))

                                (effective-x1     x1)
                                (effective-y1     y1)
                                (effective-x2     (+ x2 (* 2 border-width)))
                                (effective-y2     (+ y2 20 (* 2 border-width)))
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
                             (setf (values (textures surface) (tiles surface))
                                   (apply #'resize-surface-nodes (tree surface)
                                          (append (nodes surface) (list width height)))))))))))))

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
