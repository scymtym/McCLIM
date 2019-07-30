(cl:in-package #:clim-broadway)

;;; Server -> client operations
;;;
;;; We only sent these to the client, so we only need the serializer.

#.`(progn
     ,(generate operation :class)
     (defun serialize-operation (serial operation)
       (let ((value operation))
         ,(generate operation :serialize))))

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
