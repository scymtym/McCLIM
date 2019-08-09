(cl:in-package #:clim-broadway)

;;; Server -> client operations
;;;
;;; We only sent these to the client, so we only need the serializer.

#.`(progn
     ,(generate operation :class)
     ,(generate operation 'print-object)
     (defun serialize-operation (serial operation)
       (let ((value operation))
         ,(generate operation :serialize))))

(defun new-surface (connection id x y width height)
  (write-operation connection (make-instance 'new-surface :id id :x x :y y :width width :height height :temp? nil)))

(defun show-surface (connection id)
  (write-operation connection (make-instance 'show-surface :id id)))

(defun hide-surface (connection id)
  (write-operation connection (make-instance 'hide-surface :id id)))

(defun destroy-surface (connection id)
  (write-operation connection (make-instance 'destroy-surface :id id)))

(defun resize-surface (connection id x y width height)
  (write-operation connection (make-instance 'move-resize :id id :flags 3 :x x :y y :width width :height height)))

(defun upload-texture (connection id data)
  (let ((header (serialize-operation
                 0 (print (make-instance 'upload-texture :id id :size (length data))))))
    (write-frame 2 (concatenate 'nibbles:octet-vector header data) connection))
  (force-output connection))

(defun set-nodes (connection surface-id nodes
                  &key
                  (new-node-id         1)
                  (parent-id           0)
                  (previous-sibling-id 0)
                  delete)
  (let* ((node-deletions  (when delete
                            (serialize-node-operation (make-instance 'remove-node :id 1))))
         (node-insertions (serialize-node-operation (make-instance 'insert-node
                                                                   :parent-id           parent-id
                                                                   :previous-sibling-id previous-sibling-id)))
         (nodes           (serialize-make-node new-node-id (first nodes)))
         (header          (serialize-operation
                           0 (print (make-instance 'set-nodes :id   surface-id
                                                              :size (truncate
                                                                     (+ (if node-deletions
                                                                            (length node-deletions)
                                                                            0)
                                                                        (length node-insertions)
                                                                        (length nodes))
                                                                     4))))))
    ;; (fresh-line )
    ;; (utilities.binary-dump:binary-dump nodes :base 16 :offset-base 16 :print-type t)
    ;; (fresh-line)

    (write-frame 2 (concatenate 'nibbles:octet-vector
                                header
                                (or node-deletions (nibbles:octet-vector))
                                node-insertions
                                nodes)
                 connection)
    (force-output connection)))

(defun patch-texture (connection surface-id node-id texture-id)
  (let* ((node-operation (serialize-node-operation (make-instance 'patch-texture
                                                                  :node-id    node-id
                                                                  :texture-id texture-id)))
         (header          (serialize-operation
                           0 (print (make-instance 'set-nodes :id   surface-id
                                                              :size (truncate (length node-operation) 4))))))
    (write-frame 2 (concatenate 'nibbles:octet-vector header node-operation) connection)
    (force-output connection)))

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
     ,(generate event 'print-object)
     (defun deserialize-event (buffer offset)
       (values ,(generate event :deserialize) offset)))
