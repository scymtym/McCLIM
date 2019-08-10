(cl:in-package #:clim-broadway)

(defmethod nodes-equivalent/shallow ((left node) (right node))
  (and (= (id left) (id right))
       (eq (class-of (data left)) (class-of (data right)))))

(defmethod nodes-equivalent/children ((left node) (right node))
  (let ((left-children  (children left))
        (right-children (children right)))
    (and (= (length left-children) (length right-children))
         (every #'nodes-equivalent left-children right-children))))

(defmethod nodes-equivalent ((left node) (right node))
  (and (nodes-equivalent/shallow left right)
       (nodes-equivalent/children left right)))

(defun synchronize (old-tree new-tree)
  (let ((result '()))
    (labels ((maybe-id (node)
               (if node (id node) 0))
             (remove-node (node)
               (push (make-instance 'remove-node :id (id node)) result))
             (insert-node (parent &key after)
               (push (make-instance 'insert-node
                                    :parent-id           (maybe-id parent)
                                    :previous-sibling-id (maybe-id after))
                     result))
             (sync-nodes (new-nodes old-nodes)
               (loop :for sibling  =   nil :then (node new-node old-node
                                                       sibling)
                     :for new-node :in new-nodes
                     :for old-node =   (pop old-nodes)))
             (node (new-node old-node &optional sibling)
               ;; let ((old-node (find-node (id new-node) old-tree)))
               (cond ((not old-node)
                      (insert-node (parent new-node) :after sibling)
                      (sync-nodes (coerce (children new-node) 'list) '()))

                     ((not (nodes-equivalent/shallow new-node old-node))
                      (remove-node old-node)
                      (insert-node (parent new-node) :after sibling)
                      (sync-nodes (coerce (children new-node) 'list) (coerce (children old-node) 'list)))

                     ((not (nodes-equivalent/children new-node old-node))
                      (sync-nodes (coerce (children new-node) 'list) (coerce (children old-node) 'list)))

                     (t                 ; completely equivalent
                      ))
               new-node))
      (node (root new-tree) (root old-tree)))
    (nreverse result)))
