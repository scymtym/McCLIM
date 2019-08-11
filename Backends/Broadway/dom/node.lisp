(cl:in-package #:clim-broadway)

(defclass tree ()
  ((%root         :reader   root
                  :writer   (setf %root)
                  :initform nil)
   ;; Ids
   (%next-node-id :initarg  :next-node-id
                  :type     (integer 1)
                  :accessor next-node-id
                  :initform 1)
   (%id->node     :reader   %id->node
                  :initform (make-hash-table))))

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
