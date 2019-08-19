(cl:in-package #:clim-broadway)

(defclass connection ()
  ((%stream         :initarg  :stream
                    :reader   stream*)
   ;; Input
   (%modifier-state :accessor modifier-state
                    :initform 0)
   ;; Output
   (%serial         :initarg  :serial
                    :accessor serial
                    :initform 0)
   (%output-length  :accessor output-length
                    :initform 0)
   (%output-chunks  :reader   output-chunks
                    :initform (make-array 0 :adjustable t :fill-pointer 0))
   ;; Encoder
   (%encoder        :reader   encoder
                    :initform (make-encoder)))
  (:default-initargs
   :stream (error "Missing required initarg :STREAM")))

;;; Input

(defmethod receive-frame ((connection connection))
  (multiple-value-bind (payload opcode) (read-frame (stream* connection))
    (when (eql opcode 2)
      payload)))

(defmethod decode-frame ((connection connection) (payload t))
  (declare (cl:type nibbles:simple-octet-vector payload))
  (loop :with length = (length payload)
        :for offset :of-type array-index = 0 :then new-offset
        :while (< (+ offset 12) length)
        :for (event new-offset) = (multiple-value-list
                                   (deserialize-event payload offset))

        :when (and (typep event 'key-press)
                   (= (keysym event) 65507)) ; control
        :do (setf (modifier-state connection)
                  (logior +control-key+ (modifier-state connection)))

        :when (and (typep event 'key-release)
                   (= (keysym event) 65507))
        :do (setf (modifier-state connection)
                  (logandc1 +control-key+ (modifier-state connection)))

        :when (and (typep event 'key-press)
                   (= (keysym event) 65513)) ; alt
        :do (setf (modifier-state connection)
                  (logior +meta-key+ (modifier-state connection)))

        :when (and (typep event 'key-release)
                   (= (keysym event) 65513))
        :do (setf (modifier-state connection)
                  (logandc1 +meta-key+ (modifier-state connection)))

        ; :do (:update-inspector)

        :unless (and (typep event '(or key-press key-release))
                     (member (keysym event) '(65507 65513)))
        :collect (print event)))

(defmethod receive-message ((connection connection))
  (when-let ((payload (receive-frame connection)))
    (decode-frame connection payload)))

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
