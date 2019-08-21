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
   (%id->node     :initarg  :id->node
                  :reader   %id->node
                  :initform (make-hash-table))
   ;; State
   (%dirty-p      :initarg  :dirty-p ; TODO dirtyp
                  :accessor dirty-p
                  :initform nil)))

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
  (setf (find-node (id node) tree) node
        (%tree node)               tree
        (dirty-p tree)             t))

(defmethod make-node ((data t) (tree tree) &key parent)
  (let ((id (next-node-id tree)))
    (setf (next-node-id tree) (1+ id))
    (let ((node (make-instance 'node :id id :data data)))
      (when parent
        (add-child node parent))
      (add-node node tree)
      node)))

;;; `node'

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
   (%children :initarg  :children
              :reader   children
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

(defmethod (setf dirty-p) ((new-value t) (object node))
  (setf (dirty-p (tree object)) new-value))

(defmethod update-node ((node node) &rest initargs)
  (apply #'reinitialize-instance (data node) initargs)
  (setf (dirty-p node) t))

;;; Copying

(defmethod copy :around ((thing t) (seen hash-table))
  (values (ensure-gethash thing seen (call-next-method)) seen))

(defmethod copy ((thing tree) seen)
  (let ((new-tree (make-instance 'tree)))
    (setf (gethash thing seen) new-tree)
    (let ((id->node (make-hash-table)))
      (maphash (lambda (id node)
                 (setf (gethash id id->node) (copy node seen)))
               (%id->node thing))
      (setf (%root new-tree) (copy (root thing) seen))
      (reinitialize-instance new-tree :next-node-id (next-node-id thing)
                                      :id->node     id->node
                                      :dirty-p      (dirty-p thing)))))

(defmethod copy ((thing node) seen)
  (let ((new-node (make-instance 'node :id 0)))
    (setf (gethash thing seen) new-node)
    (reinitialize-instance
     new-node :id       (id thing)
              :data     (copy (data thing) seen)
              :tree     (copy (tree thing) seen)
              :parent   (when-let ((parent (parent thing)))
                          (copy parent seen))
              :children (let ((children (children thing)))
                          (map-into (make-array (length children)
                                                :adjustable   t
                                                :fill-pointer t)
                                    (rcurry #'copy seen)
                                    children)))))

(defmethod copy ((thing standard-object) seen)
  (let* ((class (class-of thing))
         (slots (c2mop:class-slots class))
         (new   (allocate-instance class)))
    (loop :for slot :in slots
          :for name = (c2mop:slot-definition-name slot)
          :do (setf (slot-value new name) (slot-value thing name)))
    new))
