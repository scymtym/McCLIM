(cl:in-package #:clim-broadway)

(defun run-server (&key (port 8080) the-port)
  (usocket:with-socket-listener (server-socket "0.0.0.0" port)
    (loop :for client-socket = (usocket:socket-accept server-socket :element-type :default)
          :do (serve client-socket the-port))))

(defun serve (socket port)
  (let ((stream (usocket:socket-stream socket)))
    (destructuring-bind (verb path version)
        (split-sequence:split-sequence
         #\Space (remove #\Return (read-line stream)))
      (print (list verb path version))
      (alexandria:switch (path :test #'string=)
        ("/client.html" (unwind-protect
                             (serve-client socket)
                          (usocket:socket-close socket)))
        ("/broadway.js" (unwind-protect
                             (serve-js socket)
                          (usocket:socket-close socket)))
        ("/socket"      (bt:make-thread (lambda ()
                                          (unwind-protect
                                               (serve-socket socket port)
                                            (usocket:socket-close socket)))
                                        :name "websocket worker"))
        (otherwise      (usocket:socket-close socket))))))

(defvar *client*
  (read-file-into-string
   (merge-pathnames "client.html" #.(or *compile-file-truename*
                                        *load-truename*))))

(defun serve-client (socket)
  (let ((stream (usocket:socket-stream socket)))
    (write-crlf-line "HTTP/1.1 200 OK" stream)
    (write-crlf-line "Content-type: text/html" stream)
    (write-crlf-line "" stream)\*
    (write-string *client* stream)))

(defvar *js*
  (read-file-into-string
   (merge-pathnames "broadway.js" #.(or *compile-file-truename*
                                        *load-truename*))))

(defun serve-js (socket)
  (let ((stream (usocket:socket-stream socket)))
    (write-crlf-line "HTTP/1.1 200 OK" stream)
    (write-crlf-line "Content-type: application/javascript" stream)
    (write-crlf-line "" stream)
    (write-string (read-file-into-string
                   (merge-pathnames "broadway.js" #.(or *compile-file-truename*
                                                        *load-truename*)))
                  stream)
    (write-string (read-file-into-string
                   (merge-pathnames "buffer.js" #.(or *compile-file-truename*
                                                      *load-truename*)))
                  stream)))

(defun upload-image (connection id image)
  (let* ((width  (pattern-width image))
         (height (pattern-height image))
         (array  (let ((a (clime:pattern-array image))
                       (b (make-array (* 3 width height) :element-type 'nibbles:octet)))
                   (loop :for y :below height
                         :do (loop :for x :below width
                                   :do (setf (aref b (+ (* 3 (+ (* y width) x)) 0)) (ldb (byte 8 24) (aref a y x))
                                             (aref b (+ (* 3 (+ (* y width) x)) 1)) (ldb (byte 8  16) (aref a y x))
                                             (aref b (+ (* 3 (+ (* y width) x)) 2)) (ldb (byte 8 8) (aref a y x)))))
                   b))
         (data (let ((stream (flexi-streams:make-in-memory-output-stream))
                     (png    (make-instance 'zpng:png
                                            :width width
                                            :height height
                                            :image-data array)))
                 (zpng:write-png-stream png stream)
                 (flexi-streams:get-output-stream-sequence stream))))
    (upload-texture connection id data)))

#+no (defun make-texture (port i text)
  (let* ((pixmap (climb:port-allocate-pixmap port :sheet 100 100)))
    (draw-rectangle* pixmap 0 0 100 100 :ink +black+)
    (draw-rectangle* pixmap 2 2 98 98 :ink +white+)
    (draw-circle* pixmap 80 80 20 :ink +yellow+)
    (draw-text* pixmap "McCLIM" 20 20 :ink +red+)
    (draw-text* pixmap "Broadway" (+ 20 (* 10 (sin (/ i 10)))) 40 :ink +green+)
    (draw-text* pixmap text 20 60 :ink (make-contrasting-inks 8 (mod (floor i 4) 8)))

    (prog1
        (let* ((width  (bounding-rectangle-width pixmap))
               (height (bounding-rectangle-height pixmap))
               (array  (let ((a (clime:pattern-array (mcclim-render-internals::image-mirror-image (sheet-mirror pixmap))))
                             (b (make-array (* 3 width height) :element-type 'nibbles:octet)))
                         (loop :for y :below height
                               :do (loop :for x :below width
                                         :do (setf (aref b (+ (* 3 (+ (* y width) x)) 0)) (ldb (byte 8 24) (aref a y x))
                                                   (aref b (+ (* 3 (+ (* y width) x)) 1)) (ldb (byte 8  16) (aref a y x))
                                                   (aref b (+ (* 3 (+ (* y width) x)) 2)) (ldb (byte 8 8) (aref a y x)))))
                         b))
               (stream (flexi-streams:make-in-memory-output-stream))
               (png    (make-instance 'zpng:png
                                      :width width
                                      :height height
                                      :image-data array)))
          (zpng:write-png-stream png stream)
          (flexi-streams:get-output-stream-sequence stream))

      (port-deallocate-pixmap port pixmap))))

(defun handle-incoming-message (payload)
  ;; (utilities.binary-dump:binary-dump payload :base 16 :offset-base 16)
  ;; (fresh-line)
  (let ((offset 0))
    (loop :while (< (+ offset 12) (length payload))
          :for (event new-offset) = (multiple-value-list
                                     (deserialize-event payload offset))
          :collect (print event)
          :do (setf offset new-offset))))

(defun handle-incoming-event (port event)
  (with-simple-restart (continue "Skip the event")
    (typecase event
      (screen-size-changed
       (setf (slot-value (graft port) 'region)
             (make-bounding-rectangle 0 0 (width event) (height event))))

      (pointer-move
       (setf (%pointer-position (port-pointer port)) (list (root-x event) (root-y event)))

       (when-let ((sheet (surface->sheet port (surface event))))
         (distribute-event
          port (make-instance 'pointer-motion-event
                              :sheet sheet
                              :x (root-x event)
                              :y (- (root-y event) 20)
                              :graft-x (root-x event)
                              :graft-y (root-y event)
                              :modifier-state 0))))

      ((or button-press button-release)
       (when-let ((sheet (surface->sheet port (surface event))))
         (distribute-event
          port (make-instance (if (typep event 'button-press)
                                  'pointer-button-press-event
                                  'pointer-button-release-event)
                              :sheet sheet
                              :x (root-x event)
                              :y (- (root-y event) 20)
                              :graft-x (root-x event)
                              :graft-y (root-y event)
                              :button (ecase (button event)
                                        (1 +pointer-left-button+)
                                        (2 +pointer-middle-button+)
                                        (3 +pointer-right-button+))
                              :modifier-state 0))))

      (scroll
       (when-let ((sheet (surface->sheet port (surface event))))
         (distribute-event
          port (make-instance 'climi::pointer-scroll-event
                              :sheet sheet
                              :x (root-x event)
                              :y (- (root-y event) 20)
                              :graft-x (root-x event)
                              :graft-y (root-y event)
                              :delta-x 0
                              :delta-y (case (direction event)
                                         (0 -1)
                                         (1 1))
                              :modifier-state 0))))

      ((or key-press key-release)
       (when-let ((sheet (climi::port-pointer-sheet port)))
         (let ((character (case (keysym event)
                            (65293 #\Return)
                            (65288 #\Backspace)
                            (t     (code-char (keysym event))))))
           (distribute-event
            port (make-instance (if (typep event 'key-press)
                                    'key-press-event
                                    'key-release-event)
                                :sheet sheet
                                :x 0    ; (root-x eent)
                                :y 0    ; (- (root-y eent) 20)
                                :key-name (char-name character)
                                :key-character character
                                :modifier-state 0))))))))

(defun serve-socket (socket port)
  (let* ((stream     (usocket:socket-stream socket))
         (connection (make-instance 'connection :stream stream)))
    (establish-websocket socket)

    (loop (with-simple-restart (continue "Skip the operation")
            (when-let ((op (with-port-locked (port)
                             (pop (queued-operations port)))))
              (funcall op connection)))
          (put-buffer connection)
          (sleep 1.01)
          (loop :while (listen stream)
                :for (payload opcode) = (multiple-value-list (read-frame stream))
                :when payload
                :do (loop :for message :in (handle-incoming-message payload)
                          :do (handle-incoming-event port message))))))

(defun surface->sheet (port id)
  (when-let* ((mirror->sheet (slot-value port 'climi::mirror->sheet))
              (surface       (find id (remove-if-not (of-type 'surface)
                                                     (hash-table-keys mirror->sheet))
                                   :test #'eql :key #'id)))
    (gethash surface mirror->sheet)))
