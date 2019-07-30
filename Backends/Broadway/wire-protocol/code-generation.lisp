(cl:in-package #:clim-broadway)

;;; Server -> client operations
;;;
;;; We only sent these to the client, so we only need the serializer.

#.`(progn
     ,(generate operation :class)
     (defun serialize-operation (serial operation)
       (let ((value operation))
         ,(generate operation :serialize))))

(defun new-surface (stream id x y width height)
  (write-frame 2 (serialize-operation
                  0 (make-instance 'new-surface :id id :x x :y y :width width :height height :temp? nil))
               stream)
  (force-output stream))

(defun show-surface (stream id)
  (write-frame 2 (serialize-operation
                  0 (make-instance 'show-surface :id id))
               stream)
  (force-output stream))

(defun hide-surface (stream id)
  (write-frame 2 (serialize-operation
                  0 (make-instance 'hide-surface :id id))
               stream)
  (force-output stream))

(defun destroy-surface (stream id)
  (write-frame 2 (serialize-operation 0 (make-instance 'destroy-surface :id id)) stream)
  (force-output stream))

(defun resize-surface (stream id x y width height)
  (write-frame 2 (serialize-operation
                  0 (make-instance 'move-resize :id id :flags 3 :x x :y y :width width :height height))
               stream)
  (force-output stream))

(defun upload-texture (stream id data)
  (let ((header (serialize-operation
                 0 (make-instance 'upload-texture :id id :size (length data)))))
    (write-frame 2 (concatenate 'nibbles:octet-vector header data) stream))
  (force-output stream))


;;; Node creation operations
;;;
;;; We only sent these to the client, so we only need the serializer.

#.`(progn
     ,(generate make-node :class)
     (defun serialize-make-node (id node)
       (let ((value node))
         ,(generate make-node :serialize))))

;;; Node operations
;;;
;;; We only sent these to the client, so we only need the serializer.

#.`(progn
     ,(generate node-operation :class)
     (defun serialize-node-operation (node)
       (let ((value node))
         ,(generate node-operation :serialize))))

;;; Client -> server events
;;;
;;; We only receive these from the client, so we only need the
;;; deserializer.

#.`(progn
     ,(generate event :class)
     (defun deserialize-event (buffer offset)
       (values ,(generate event :deserialize) offset)))

(defmethod print-object ((object pointer-move) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A, ~A [~A, ~A]"
            (root-x object) (root-y object) (win-x object) (win-y object))))

(defmethod print-object ((object key-press) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((keysym (keysym object)))
      (format stream "~C [~D]" (code-char keysym) keysym))))

(defmethod print-object ((object key-release) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((keysym (keysym object)))
      (format stream "~C [~D]" (code-char keysym) keysym))))
