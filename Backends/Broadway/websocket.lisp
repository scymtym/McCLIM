;;;; See https://tools.ietf.org/html/rfc6455

(cl:in-package #:clim-broadway)

(defun write-crlf-line (string stream)
  (write-string string stream)
  (write-char #\Return stream)
  (write-char #\Linefeed stream))

;;; Establishing

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

(defun establish-websocket (socket)
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

    socket))

;;; Sending

(defun frame-size (payload-length)
  (cond ((> payload-length 65535) :large)
        ((> payload-length 125)   :medium)
        (t                        :small)))

(defun write-frame-header (stream opcode payload-length &key mask?)
  (let ((size   (frame-size payload-length))
        (buffer (nibbles:make-octet-vector 10))
        (final? t))
    (declare (dynamic-extent buffer))
    (setf (ldb (byte 1 7) (aref buffer 0)) (if final? 1 0)
          (ldb (byte 3 0) (aref buffer 0)) opcode)
    (setf (ldb (byte 1 7) (aref buffer 1)) (if mask? 1 0)
          (ldb (byte 7 0) (aref buffer 1)) (ecase size
                                             (:large  127)
                                             (:medium 126)
                                             (:small  payload-length)))
    (case size
      (:large  (setf (nibbles:ub64ref/be buffer 2) payload-length))
      (:medium (setf (nibbles:ub16ref/be buffer 2) payload-length)))
    (write-sequence buffer stream :end (ecase size
                                         (:large  10)
                                         (:medium  4)
                                         (:small   2)))))

(defun write-frame (opcode payload stream &key mask?) ; TODO stream should be first
  (let ((length (length payload))
        (buffer (make-array 10 :element-type '(unsigned-byte 8)))

        (final? t))
    (setf (ldb (byte 1 7) (aref buffer 0)) (if final? 1 0)
          (ldb (byte 3 0) (aref buffer 0)) opcode)
    (setf (ldb (byte 1 7) (aref buffer 1)) (if mask? 1 0)
          (ldb (byte 7 0) (aref buffer 1)) (cond ((> length 65535)
                                                  127)
                                                 ((> length 125)
                                                  126)
                                                 (t length)))
    (cond ((> length 65535)
           (setf (nibbles:ub64ref/be buffer 2) length))
          ((> length 125)
           (setf (nibbles:ub16ref/be buffer 2) length)))

    ;; (fresh-line)
    ;; (format t "server -> client:~%")
    ;; (utilities.binary-dump:binary-dump (concatenate 'nibbles:octet-vector buffer payload) :base 16 :offset-base 16 :print-type t)
    ;; (fresh-line)

    (write-sequence buffer stream :end (cond ((> length 65535)
                                              10)
                                             ((> length 125)
                                              4)
                                             (t
                                              2)))
    (write-sequence payload stream)))

;;; Receiving

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

(defun read-frame (stream)
  (let* ((buffer (nibbles:make-octet-vector 2))
         (length (read-sequence buffer stream)))
    (when (zerop length)
      (return-from read-frame (values nil nil)))

    ;; (fresh-line)
    ;; (format t "client -> server:~%")
    ;; (utilities.binary-dump:binary-dump buffer :base 16 :offset-base 16 :end length)
    ;; (fresh-line)

    (let* ((opcode  (ldb (byte 3 0) (aref buffer 0)))
           (mask?   (logbitp 7 (aref buffer 1)))
           (length  (+ (ldb (byte 7 0) (aref buffer 1))
                       (if mask? 4 0)))
           (payload (nibbles:make-octet-vector length)))
      #+no (format t "  final? ~A opcode ~A~%"
                   (ldb (byte 1 7) (aref buffer 0))
                   opcode)
      ;; (format t "  mask? ~A payload length ~A~%" mask? length)

      (when (zerop length)
        (return-from read-frame (values nil opcode)))
      (loop :for offset = 0 :then (read-sequence payload stream :start offset)
            :while (< offset length))
      (values (if mask? (unmask payload) payload) opcode))))
