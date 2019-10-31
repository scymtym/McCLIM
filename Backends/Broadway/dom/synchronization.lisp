;;;; (C) Copyright 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clim-broadway)

(defmethod nodes-equivalent/shallow ((left node) (right node))
  (and (= (id left) (id right))
       (nodes-equivalent (data left) (data right))))

(defmethod nodes-equivalent/children ((left node) (right node))
  (let ((left-children  (children left))
        (right-children (children right)))
    (and (= (length left-children) (length right-children))
         (every #'nodes-equivalent left-children right-children))))

(defmethod nodes-equivalent ((left node) (right node))
  (and (nodes-equivalent/shallow left right)
       (nodes-equivalent/children left right)))

(defun synchronize (old-tree new-tree)
  (let ((result (make-array 10 :adjustable t :fill-pointer 0)))
    (labels ((maybe-id (node)
               (if node (id node) 0))
             (remove-node (node)
               (vector-push-extend (make-instance 'remove-node :id (id node)) result))
             (insert-node (node &key after (data (data node)))
               (vector-push-extend
                (make-instance 'insert-node
                               :parent-id           (maybe-id (parent node))
                               :previous-sibling-id (maybe-id after))
                result)
               (vector-push-extend (id node) result)
               (vector-push-extend data result))
             (reuse-node (node &key after)
               (insert-node node :after after :data (make-instance 'reuse)))
             (sync-nodes (new-nodes old-nodes &key parent-removed-p)
               (loop :for sibling  =   nil :then (node new-node old-node
                                                       :sibling          sibling
                                                       :parent-removed-p parent-removed-p)
                     :for new-node :in new-nodes
                     :for old-node =   (pop old-nodes)))
             (node (new-node old-node &key sibling parent-removed-p)
               ;; let ((old-node (find-node (id new-node) old-tree)))
               (cond ((not old-node)
                      (insert-node new-node :after sibling)
                      (sync-nodes (coerce (children new-node) 'list) '()))

                     ((not (nodes-equivalent/shallow new-node old-node))
                      (unless parent-removed-p
                        (remove-node old-node))
                      (insert-node new-node :after sibling)
                      (sync-nodes (coerce (children new-node) 'list) (coerce (children old-node) 'list) ; TODO
                                  :parent-removed-p t))

                     ((not (nodes-equivalent/children new-node old-node))
                      (sync-nodes (coerce (children new-node) 'list) (coerce (children old-node) 'list)
                                  :parent-removed-p parent-removed-p))

                     ;; equivalent
                     (parent-removed-p
                      (reuse-node old-node :after sibling))

                     (t                 ; completely equivalent
                      ))
               new-node))
      (node (root new-tree) (root old-tree)))
    (coerce result 'list))) ; TODO temp

;;;

(defmethod nodes-equivalent ((left standard-object) (right standard-object))
  (let ((class (class-of left)))
    (and (eq class (class-of right))
         (loop :for slot :in (c2mop:class-slots class)
               :for name = (c2mop:slot-definition-name slot)
               :always (equal (slot-value left name) (slot-value right name))))))
