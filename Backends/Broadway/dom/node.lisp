(cl:in-package #:clim-broadway)

(defclass tree ()
  ((%root         :reader   root
                  :writer   (setf %root))
   ;; Ids
   (%next-node-id :initarg  :next-node-id
                  :type     (integer 1)
                  :accessor next-node-id
                  :initform 1)
   (%id->node     :reader   %id->node
                  :initform (make-hash-table)))
  (:default-initargs
   :root (error "Missing required initarg :ROOT")))

(defmethod shared-initialize :after ((instance   tree)
                                     (slot-names t)
                                     &key
                                     (root nil root-supplied-p))
  (when root-supplied-p
    (setf (%root instance) (make-node root instance))))

(defmethod find-node ((id integer) (tree tree))
  (gethash id (%id->node tree)))

(defmethod (setf find-node) ((new-value t) (id integer) (tree tree))
  (setf (gethash id (%id->node tree)) new-value))

(defmethod add-node ((node t) (tree tree))
  (setf (find-node (id node) tree) node)
  (setf (%tree node) tree))

(defmethod make-node ((data t) (tree tree) &key parent)
  (let ((id (next-node-id tree)))
    (setf (next-node-id tree) (1+ id))
    (let ((node (make-instance 'node :id id :data data)))
      (when parent
        (add-child node parent))
      (add-node node tree)
      node)))

(defclass node ()
  ((%id       :initarg  :id
              :reader   id)
   (%data     :initarg  :data
              :reader   data)
   ;;
   (%tree     :initarg  :tree ; TODO store tree in parent slot of root node?
              :reader   tree
              :writer   (setf %tree))
   (%parent   :initarg  :parent
              :reader   parent
              :writer   (setf %parent)
              :initform nil)
   (%children :reader   children
              :initform (make-array 0 :adjustable t :fill-pointer t)))
  (:default-initargs
   :id (error "Missing required initarg :ID")))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~D ~A"
            (id object)
            (class-name (class-of (data object))))))

(defmethod add-child ((child node) (parent node))
  (vector-push-extend child (children parent))
  (setf (%parent child) parent)
  (add-node child (tree parent)))

(defvar *inspector*)
(defvar *tree* )

(let* ((root (make-instance 'node :id 0))
       (tree (make-instance 'tree :root root)))
  (add-child (make-instance 'node :id 1) root)

  (let* ((old clim:*default-server-path*))
    (setf clim:*default-server-path* :clx)
    (unwind-protect
         (setf *inspector* (nth-value
                            1 (clouseau:inspect tree :new-process t)))
      (setf clim:*default-server-path* old))))

(defun make-tree-1 ()
  (let* ((tree (make-instance 'tree :root nil))
         (root (root tree))
         (n1   (make-node nil tree :parent root))
         (n2   (make-node nil tree :parent n1))
         (n3   (make-node nil tree :parent n1))
         (n4   (make-node nil tree :parent n1)))
    (make-node nil tree :parent n3)
    tree))

(defun make-tree-2 ()
  (let* ((tree (make-instance 'tree :root nil))
         (root (root tree))
         (n1   (make-node nil tree :parent root)))
    (make-node nil tree :parent n1)
    (make-node nil tree :parent n1)
    tree))

;;;

(defmethod clouseau:inspect-object-using-state ((object tree)
                                                (state  clouseau::inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream t))
  (format-graph-from-root
   (root object)
   (lambda (object stream)
     (clouseau:formatting-place (object 'clouseau:pseudo-place object nil present-value)
       (present-value stream)))
   #'children
   :stream stream :orientation :vertical))

(let* ((old-tree (make-tree-2))
       (new-tree (make-surface-tree (make-instance 'outset-shadow)
                                    (make-instance 'border)
                                    (make-instance 'color)
                                    (make-tiles 300 300 128))                     ; (make-tree-1)
         ))
  (setf (clouseau:root-object *inspector* :run-hook-p t)
        (list (cons :old old-tree)
              (cons :new new-tree)
              (cons :ops (synchronize old-tree new-tree))
              (cons :ops (synchronize new-tree new-tree)))))
