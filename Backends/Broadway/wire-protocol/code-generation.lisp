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
  (append-message-chunk connection (make-instance 'upload-texture :id id :size (length data)))
  (append-message-chunk connection data)
  (send-message connection))

(defun set-nodes (connection surface-id nodes
                  &key
                  (new-node-id         1)
                  (parent-id           0)
                  (previous-sibling-id 0)
                  delete)
  ;; (fresh-line )
  ;; (utilities.binary-dump:binary-dump nodes :base 16 :offset-base 16 :print-type t)
  ;; (fresh-line)

  (when delete
    (append-message-chunk connection (serialize-node-operation (make-instance 'remove-node :id 1))))
  (append-message-chunk connection (serialize-node-operation (print (make-instance 'insert-node
                                                                             :parent-id           parent-id
                                                                             :previous-sibling-id previous-sibling-id))))
  (map nil (lambda (node)
             (print (list new-node-id :-> node))
             (append-message-chunk connection (serialize-make-node new-node-id node)))
       nodes)
  (prepend-message-chunk connection (print (make-instance 'set-nodes :id   surface-id
                                                               :size (truncate
                                                                      (+ (output-length connection))
                                                                      4))))
  (send-message connection))

(defun set-nodes2 (connection surface-id operations)
  (loop :for operation = (pop operations)
        :while operation
        :do (append-message-chunk connection (serialize-node-operation (print operation)))
        :when (typep operation 'insert-node)
          :do (append-message-chunk connection (serialize-make-node (pop operations) (print (pop operations)))))

  (prepend-message-chunk
   connection (print (make-instance 'set-nodes :id   surface-id
                                               :size (truncate
                                                      (+ (output-length connection))
                                                      4))))
  (send-message connection))

(defun patch-texture (connection surface-id node-id texture-id)
  (append-message-chunk connection (serialize-node-operation
                                    (make-instance 'patch-texture
                                                   :node-id    node-id
                                                   :texture-id texture-id)))
  (prepend-message-chunk connection (make-instance 'set-nodes :id   surface-id
                                                              :size (truncate (output-length connection) 4)))
  (send-message connection))

(defvar *previous* nil)

#+old (defun put-buffer (connection surface)
  (append-message-chunk connection (make-instance 'put-buffer))

  (let* ((image  (mcclim-render-internals::image-mirror-image surface))
         (a      (clime:pattern-array image))
         (width  (array-dimension a 1))
         (height (array-dimension a 0))
         #+no (b      (make-array (* width height) :element-type '(unsigned-byte 32))))
    #+no (loop :for y :below height
               :do (loop :for x :below width
                         :do (setf (aref b (+ (* 3 (+ (* y width) x)) 0)) (ldb (byte 8 24) (aref a y x))
                                   (aref b (+ (* 3 (+ (* y width) x)) 1)) (ldb (byte 8  16) (aref a y x))
                                   (aref b (+ (* 3 (+ (* y width) x)) 2)) (ldb (byte 8 8) (aref a y x)))))
    (print (list width height))
    (append-message-chunk connection (let ((e (encode-buffer a (or *previous* a) width height)))
                                       (subseq (encoder-buffer e) 0 (1- (encoder-index e)))))
    (setf *previous* (copy-array a)))

        (send-message connection))

(defun put-buffer (connection pixels)
  (append-message-chunk connection (make-instance 'put-buffer))

  (let* ((width  (array-dimension pixels 1))
         (height (array-dimension pixels 0))
         #+no (b      (make-array (* width height) :element-type '(unsigned-byte 32))))
    #+no (loop :for y :below height
               :do (loop :for x :below width
                         :do (setf (aref b (+ (* 3 (+ (* y width) x)) 0)) (ldb (byte 8 24) (aref a y x))
                                   (aref b (+ (* 3 (+ (* y width) x)) 1)) (ldb (byte 8  16) (aref a y x))
                                   (aref b (+ (* 3 (+ (* y width) x)) 2)) (ldb (byte 8 8) (aref a y x)))))
    (print (list width height))
    (append-message-chunk connection (let ((e (encode-buffer pixels (or *previous* pixels) width height)))
                                       (subseq (encoder-buffer e) 0 (1- (encoder-index e)))))
    (setf *previous* (copy-array pixels)))

  (send-message connection))

;;; Node creation operations
;;;
;;; We only sent these to the client, so we only need the serializer.

#.`(progn
     ,(generate make-node :class)
     ,(generate make-node 'print-object)
     (defun serialize-make-node (id node)
       (let ((value node))
         ,(generate make-node :serialize))))

;;; Node operations
;;;
;;; We only sent these to the client, so we only need the serializer.

#.`(progn
     ,(generate node-operation :class)
     ,(generate node-operation 'print-object)
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
