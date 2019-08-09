(cl:in-package #:clim-broadway)

(defclass connection ()
  ((%stream        :initarg  :stream
                   :reader   stream*)
   ;; Output
   (%serial        :initarg  :serial
                   :accessor serial
                   :initform 0)
   (%output-length :accessor output-length
                   :initform 0)
   (%output-chunks :reader   output-chunks
                   :initform (make-array 0 :adjustable t :fill-pointer 0)))
  (:default-initargs
   :stream (error "Missing required initarg :STREAM")))

;;; Input



;;; Output

(defmethod append-message-chunk ((connection connection) (chunk simple-array))
  (vector-push-extend chunk (output-chunks connection))
  (incf (output-length connection) (length chunk)))

(defmethod send-message ((connection connection))
  (let ((stream (stream* connection))
        (chunks (output-chunks connection)))
    (write-frame-header stream 2 (output-length connection))
    (map nil (rcurry #'write-sequence stream) chunks)
    (force-output stream)
    (setf (output-length connection) 0
          (fill-pointer chunks)      0)))

(defun write-operation (connection operation)
  (let ((serial (1- (incf (serial connection)))))
    (append-message-chunk connection (serialize-operation serial operation))
    (send-message connection)))
