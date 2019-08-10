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

(defmethod prepend-message-chunk ((connection connection) (chunk vector))
  (let ((chunks (output-chunks connection)))
    (vector-push-extend nil chunks)
    (setf (subseq chunks 1) (subseq chunks 0 (1- (fill-pointer chunks))))
    (setf (aref chunks 0) chunk))
  (incf (output-length connection) (length chunk)))

(defmethod append-message-chunk ((connection connection) (chunk vector))
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

;;; Operation output

(defmethod prepend-message-chunk ((connection connection)
                                  (chunk      operation-message))
  (let ((serial (1- (incf (serial connection)))))
    (prepend-message-chunk connection (serialize-operation serial chunk))))

(defmethod append-message-chunk ((connection connection)
                                 (chunk      operation-message))
  (let ((serial (1- (incf (serial connection)))))
    (append-message-chunk connection (serialize-operation serial chunk))))

(defun write-operation (connection operation)
  (append-message-chunk connection operation)
  (send-message connection))
