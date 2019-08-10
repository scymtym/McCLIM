(cl:in-package #:clim-broadway)

(defclass surface ()
  ((%name :initarg  :name
          :accessor name)
   ;; Nodes
   (%tree :initarg :tree)))

(defclass tile ()
  ((%region :initarg :region
            :reader  region)))

(defun map-intervals (function limit interval-size)
  (loop :for start = 0 :then end
        :for end :from interval-size :to limit :by interval-size
        :do (funcall function start end)
        :finally (funcall function start limit)))

(defun map-tile-rectangles (function width height tile-size)
  (map-intervals (lambda (x1 x2)
                   (map-intervals (lambda (y1 y2)
                                    (funcall function x1 x2 y1 y2))
                                  height tile-size))
                 width tile-size))

(defun make-tiles (width height tile-size)
  (let ((tiles '()))
    (map-tile-rectangles
     (lambda (x1 x2 y1 y2)
       (push (make-instance 'tile
                            :region (make-rectangle* x1 x2 y1 y2))
             tiles))
     width height tile-size)
    tiles))

(make-tiles 300 300 128)

(defun make-surface-tree (shadow border title-bar tiles)
  (let* ((tree        (make-instance 'tree :root shadow))
         (shadow-node (root tree))
         (border-node (make-node border tree :parent shadow-node)))
    (make-node title-bar tree :parent border-node)
    (map nil (lambda (tile)
               (with-bounding-rectangle* (x1 y1 x2 y2) (region tile)
                 (let ((texture (make-instance 'texture
                                               :id     1
                                               :x      (float x1        1.0f0)
                                               :y      (float y1        1.0f0)
                                               :width  (float (- x2 x1) 1.0f0)
                                               :height (float (- y2 y1) 1.0f0))))
                   (make-node texture tree :parent border-node))))
         tiles)
    tree))

(make-surface-tree (make-instance 'outset-shadow)
                   (make-instance 'border)
                   (make-instance 'color)
                   (make-tiles 300 300 128))
