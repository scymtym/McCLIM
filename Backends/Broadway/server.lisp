(cl:in-package #:clim-broadway)

(defun run-server (&key (port 8080))
  (usocket:with-socket-listener (server-socket "0.0.0.0" port)
    (loop :for client-socket = (usocket:socket-accept server-socket :element-type :default)
          :do (serve client-socket))))

(defun serve (socket)
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
                                               (serve-socket socket)
                                            (usocket:socket-close socket)))
                                        :name "websocket worker"))
        (otherwise      (usocket:socket-close socket))))))

(defvar *client*
  (read-file-into-string
   (merge-pathnames "client.html" #.(or *compile-file-truename*
                                        *load-truename*))))

(defun write-crlf-line (string stream)
  (write-string string stream)
  (write-char #\Return stream)
  (write-char #\Linefeed stream))

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
                  stream)))

(defun read-header-line (stream)
  (let* ((line  (remove #\Return (read-line stream)))
         (colon (position #\: line))
         (key   (when colon (subseq line 0 colon)))
         (value (when colon (string-trim '(#\Space) (subseq line (1+ colon))))))
    (when colon
      (values key value))))

(defparameter *web-socket-key-magic*
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defun websocket-response-key (key)
  (let ((digest (ironclad:make-digest :sha1)))
    (ironclad:digest-sequence digest (sb-ext:string-to-octets key))
    (ironclad:digest-sequence digest (sb-ext:string-to-octets *web-socket-key-magic*))
    (base64:usb8-array-to-base64-string (ironclad:produce-digest digest))))

(assert (string= (websocket-response-key "x3JJHMbDL1EzLkh9GBhXDw==")
                 "HSmrc0sMlYUkAGmm5OPpG2HaGWk="))

(defun write-frame (opcode payload stream &key mask?)
  (let ((length (length payload))
        (buffer (make-array 4 :element-type '(unsigned-byte 8)))

        (final? t))
    (setf (ldb (byte 1 7) (aref buffer 0)) (if final? 1 0)
          (ldb (byte 3 0) (aref buffer 0)) opcode)
    (setf (ldb (byte 1 7) (aref buffer 1)) (if mask? 1 0)
          (ldb (byte 7 0) (aref buffer 1)) (cond ((> length 65535)
                                                  127)
                                                 ((> length 125)
                                                  126)
                                                 (t length)))
    (when (> length 125)
      (setf (aref buffer 2) (ldb (byte 8 8) length)
            (aref buffer 3) (ldb (byte 8 0) length)))

    ; (fresh-line)
    ; (format t "server -> client:~%")
    ; (utilities.binary-dump:binary-dump (concatenate 'nibbles:octet-vector buffer payload) :base 16 :offset-base 16 :print-type t)
    ; (fresh-line)

    (write-sequence buffer stream :end (if (> length 125) 4 2))
    (write-sequence payload stream)))

(defun write-message (opcode serial payload stream)
  (let ((header (nibbles:make-octet-vector 5)))
    (setf (aref header 0) opcode)
    (setf (nibbles:ub32ref/le header 1) serial)
    (write-frame 2 (concatenate 'nibbles:octet-vector header payload) stream)))

(defun new-surface (stream id x y width height)
  (write-frame 2 (serialize-operation
                  0 (make-instance 'new-surface :id id :x x :y y :width width :height height :temp? nil))
               stream)
  (force-output stream))

(defun show-surface (stream id)
  (write-frame 2 (serialize-operation
                  0 (make-instance 'show-surface :id id))
               stream)
  #+old (let ((buffer (nibbles:make-octet-vector 2)))
          (setf (nibbles:ub16ref/le buffer 0)  id)
          (write-message 3 0 buffer stream))
  (force-output stream))

(defun upload-texture (stream id data)
  (let ((header (serialize-operation
                 0 (make-instance 'upload-texture :id id :size (length data)))))
    (write-frame 2 (concatenate 'nibbles:octet-vector header data) stream))
  (force-output stream))

(defun set-nodes (stream surface-id nodes &key delete)
  (let* ((node-deletions  (when delete
                            (serialize-node-operation (make-instance 'remove-node :id 1))))
         (node-insertions (serialize-node-operation (make-instance 'insert-node :parent-id 0 :previous-sibling-id 0)))
         (nodes           (serialize-make-node
                           1 #+no (make-instance 'color
                                            :x 20.0f0 :y 20.0f0 :width 20.0f0 :height 20.0f0
                                            :red 255 :green 0 :blue 0 :alpha 0)
                             (first nodes)))
         (header          (serialize-operation
                           0 (make-instance 'set-nodes :id   surface-id
                                                       :size (truncate
                                                              (+ (if node-deletions
                                                                     (length node-deletions)
                                                                     0)
                                                                 (length node-insertions)
                                                                 (length nodes))
                                                              4)))))
    (write-frame 2 (concatenate 'nibbles:octet-vector
                                header
                                (or node-deletions (nibbles:octet-vector))
                                node-insertions
                                nodes)
                 stream)
    (force-output stream)))

(defun handle-incoming-message (payload)
                                        ; (utilities.binary-dump:binary-dump payload :base 16 :offset-base 16)
                                        ; (fresh-line)

  (let ((offset 0))
    (loop :while (< (+ offset 12) (length payload))
          :for (event new-offset) = (multiple-value-list
                                     (deserialize-event payload offset))
          :collect (print event)
          :do (setf offset new-offset))))

(defun unmask (payload)
  (let ((result (nibbles:make-octet-vector (- (length payload) 4))))

    ; (format t "masked:~%")
    ; (utilities.binary-dump:binary-dump payload :base 16 :offset-base 16)
    ; (fresh-line)

    (loop :for offset :below (- (length payload) 4)
          :for mask-byte = (aref payload (mod offset 4))
          :for payload-byte = (aref payload (+ 4 offset))
          :do (setf (aref result offset) (logxor mask-byte payload-byte)))

    ; (format t "unmasked:~%")
    ; (utilities.binary-dump:binary-dump result :base 16 :offset-base 16)
    ; (fresh-line)

    result))

(defvar *port* (make-instance 'broadway-port))

(defun make-texture (i text)
  (let* ((port   *port*)
         (pixmap (climb:port-allocate-pixmap port :sheet 100 100)))
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

(defun serve-socket (socket)
  (let ((stream (usocket:socket-stream socket))
        host origin secure-origin secure-key)
    (loop :for (key value) = (multiple-value-list (read-header-line stream))
          :while key
          :do (list key value)
          :when (equal key "Host")
          :do (setf host value)
          :when (equal key "Origin")
          :do (setf origin value)
          :when (equal key "Sec-WebSocket-Origin")
          :do (setf secure-origin value)
          :when (equal key "Sec-WebSocket-Key")
          :do (setf secure-key value))
                                        ; (print (list host origin secure-origin secure-key))

    (write-crlf-line "HTTP/1.1 101 Switching Protocol" stream)
    (write-crlf-line "Upgrade: websocket" stream)
    (write-crlf-line "Connection: Upgrade" stream)
    (write-crlf-line (format nil "Sec-WebSocket-Accept: ~A" (websocket-response-key secure-key)) stream)
    (when origin
      (write-crlf-line (format nil "Sec-WebSocket-Origin: ~A" origin) stream))
    (write-crlf-line (format nil "Sec-WebSocket-Location: ws://~A/socket" "localhost") stream)
    (write-crlf-line "Sec-WebSocket-Protocol: broadway" stream)
    (write-crlf-line "" stream)
    (force-output stream)

    (setf (usocket:socket-option socket :tcp-nodelay) t)


    (new-surface stream 1 10 10 640 480)
    (show-surface stream 1)

    (upload-texture stream 5 (make-texture 0 ""))
    (set-nodes stream 1 (list (make-instance 'texture
                                             :x 20.0f0 :y 20.0f0 :width 100.0f0 :height 100.0f0
                                             :id 5)))

    (loop :with x = 0
          :with y = 0
          :with text = (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
          :do (sleep .01)
              (loop :while (listen stream)
                    :do (let* ((buffer (make-array 2 :element-type '(unsigned-byte 8)))
                               (length (read-sequence buffer stream)))
                          (when (zerop length)
                            (return))
                                        ; (fresh-line)
                                        ; (format t "client -> server:~%")
                                        ; (utilities.binary-dump:binary-dump buffer :base 16 :offset-base 16 :end length)
                                        ; (fresh-line)

                          (let* ((mask?   (logbitp 7 (aref buffer 1)))
                                 (length  (+ (ldb (byte 7 0) (aref buffer 1))
                                             (if mask? 4 0)))
                                 (payload (nibbles:make-octet-vector length)))
                            #+no (format t "  final? ~A opcode ~A~%"
                                         (ldb (byte 1 7) (aref buffer 0))
                                         (ldb (byte 3 0) (aref buffer 0)))
                                        ; (format t "  mask? ~A payload length ~A~%" mask? length)
                            (when (plusp length)
                              (loop :for offset = 0 :then (read-sequence payload stream :start offset)
                                    :while (< offset length))
                              (loop :for message :in (handle-incoming-message (if mask? (unmask payload) payload))

                                    :when (typep message 'pointer-move)
                                    :do (setf x (root-x message)
                                              y (root-y message))

                                    :when (typep message 'key-press)
                                    :do (vector-push-extend (code-char (keysym message)) text)

                                    :do (upload-texture stream 5 (make-texture x text))
                                        (set-nodes stream 1 (list (make-instance 'texture
                                                                                 :x (float x 1.0f0)
                                                                                 :y (float y 1.0f0)
                                                                                 :width 100.0f0
                                                                                 :height 100.0f0
                                                                                 :id 5))
                                                   :delete t)))))))))
