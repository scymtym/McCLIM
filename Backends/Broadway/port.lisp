(cl:in-package #:clim-broadway)

(defclass broadway-port (climi::standard-port
                         climi::standard-event-port-mixin
                         mcclim-render-internals::render-port-mixin)
  ((%listen-port       :initarg  :listen-port
                       :reader   listen-port
                       :writer   (setf %listen-port))
   (%next-surface-id   :accessor next-surface-id
                       :initform 0)
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

(defclass surface (mcclim-render-internals::image-mirror-mixin)
  ((%id :initarg :id
        :reader  id)
                                        ;(%texture-id )
   ))

(defmethod clim:destroy-mirror ((port           broadway-port)
                                (mirrored-sheet t))
  (let* ((mirror (climi::port-lookup-mirror port mirrored-sheet))
         (id     (id mirror)))

    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (stream)
                       (destroy-surface stream id)))))

    (climi::port-unregister-mirror port mirrored-sheet mirror)))

(defmethod clim:realize-mirror ((port           broadway-port)
                                (mirrored-sheet t))
                                        ; (setf (sheet-parent pixmap) (graft port))
  (let* ((id     (incf (next-surface-id port)))
         (mirror (make-instance 'surface :id id))
         (showp  (sheet-enabled-p mirrored-sheet)))

    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (stream)
                       (new-surface stream id (random 500) (random 500) 100 100)
                       (when showp
                         (show-surface stream id))
                       #+no (set-nodes stream id (list (make-instance 'color :x 0.0f0 :y 0.0f0 :width 100.0f0 :height 100.0f0
                                                                             :red (random 255) :green (random 255) :blue (random 255)
                                                                             :alpha 128)))))))
    (climi::port-register-mirror port mirrored-sheet mirror)

    (distribute-event port (make-instance 'window-repaint-event :sheet mirrored-sheet :region +everywhere+))

    (with-port-locked (port)
      (appendf (queued-operations port)
               (list (lambda (stream)
                       (loop :for image = (mcclim-render-internals::image-mirror-image mirror)
                             :until image
                             :do (sleep .1)
                             :finally (progn (sleep .1)  (upload-image stream id image)))))))

    (sleep .1)

    mirror))

(defmethod climb:port-set-mirror-region ((port          broadway-port)
                                         (mirror        surface)
                                         (mirror-region t))
  (let ((id (id mirror)))
    (with-bounding-rectangle* (x1 y1 x2 y2) mirror-region
      (let* ((corner-radius    8)
             (border-width     4)
             (border-color     #xff0000ff)

             (width            (- x2 x1))
             (height           (- y2 y1))

             (effective-x1     x1)
             (effective-y1     y1)
             (effective-x2     (+ x2 (* 2 border-width)))
             (effective-y2     (+ y2 20 (* 2 border-width)))
             (effective-width  (- effective-x2 effective-x1))
             (effective-height (- effective-y2 effective-y1)))

        (with-port-locked (port)
          (appendf (queued-operations port)
                   (list (lambda (stream)
                           #+no (destroy-surface stream id)
                           #+no (new-surface stream id x1 y1 (- x2 x1) (- y2 y1))
                           (resize-surface stream id effective-x1 effective-y1 effective-width effective-height)

                           (set-nodes stream id (list (make-instance 'border :x             (float 0                1.0f0)
                                                                             :y             (float 0                1.0f0)
                                                                             :width         (float effective-width  1.0f0)
                                                                             :height        (float effective-height 1.0f0)

                                                                             :top-radius-x    (float corner-radius 1.0f0)
                                                                             :top-radius-y    (float corner-radius 1.0f0)
                                                                             :right-radius-x  (float corner-radius 1.0f0)
                                                                             :right-radius-y  (float corner-radius 1.0f0)
                                                                             :bottom-radius-x (float corner-radius 1.0f0)
                                                                             :bottom-radius-y (float corner-radius 1.0f0)
                                                                             :left-radius-x   (float corner-radius 1.0f0)
                                                                             :left-radius-y   (float corner-radius 1.0f0)

                                                                             :top-width     (float border-width 1.0f0)
                                                                             :right-width   (float border-width 1.0f0)
                                                                             :bottom-width  (float border-width 1.0f0)
                                                                             :left-width    (float border-width 1.0f0)

                                                                             :top-color     border-color
                                                                             :right-color   border-color
                                                                             :bottom-color  border-color
                                                                             :left-color    border-color))
                                      :new-node-id 3)
                           (set-nodes stream id (list (make-instance 'color :x      (float 0     1.0f0)
                                                                            :y      (float 0     1.0f0)
                                                                            :width  (float width 1.0f0)
                                                                            :height (float 20    1.0f0)
                                                                            :red    128
                                                                            :green  128
                                                                            :blue   255
                                                                            :alpha  200))
                                      :new-node-id 2 :parent-id 3)
                           (set-nodes stream id (list (make-instance 'texture :x      (float 0      1.0f0)
                                                                              :y      (float 20     1.0f0)
                                                                              :width  (float width  1.0f0)
                                                                              :height (float height 1.0f0)
                                                                              :id     id))
                                      :new-node-id 1 :parent-id 3)))))))))

(defmethod climb:port-enable-sheet ((port  broadway-port)
                                    (sheet mirrored-sheet-mixin))
  (let* ((mirror (sheet-mirror sheet))
         (id     (id mirror)))

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

(defmethod set-sheet-pointer-cursor ((port broadway-port) sheet cursor))

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
                 :pointer 0 :x 0 :y 0 :graft-x 0 :graft-y 0
                 :button 0 :modifier-state 0
                 :sheet   (climi::port-pointer-sheet (port pointer))))

;;; Events

(defmethod distribute-event :around ((port broadway-port) (event pointer-event))
  (call-next-method)

  (with-simple-restart (continue "Skip the texture update")
    (when-let* ((sheet  (climi::port-pointer-sheet port))
                (mirror (sheet-mirror sheet))
                (id     (id mirror)))
      (repaint-sheet sheet +everywhere+)
      (with-port-locked (port)
        (when (null (queued-operations port))
         (appendf (queued-operations port)
                  (list (lambda (stream)
                          (loop :for image = (mcclim-render-internals::image-mirror-image mirror)
                                :until image
                                :finally (upload-image stream id image))
                          (patch-texture stream id 1 id)
                                        ; (write-operation stream 0 (make-instance 'roundtrip :id 0 :tag 0 ))
                          ))))))))
