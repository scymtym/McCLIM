(cl:in-package #:clim-broadway)

(defclass surface (surface1)
  ((%name        :initarg  :name
                 :accessor name)
   ;; Nodes
   (%tree        :reader   tree
                 :writer   (setf %tree))
   (%nodes       :reader   nodes
                 :writer   (setf %nodes))
   ;; Tiles
   (%textures    :accessor textures)
   (%tiles       :accessor tiles)))

(defmethod initialize-instance :after ((instance surface) &key)
  (setf (values (%tree instance) (%nodes instance)) (make-surface-tree)))

(defun make-surface-tree ()
  (multiple-value-bind (shadow border title-bar content) (make-surface-nodes)
    (let* ((tree           (make-instance 'tree :root shadow))
           (shadow-node    (root tree))
           (border-node    (make-node border    tree :parent shadow-node))
           (title-bar-node (make-node title-bar tree :parent border-node))
           (content-node   (make-node content   tree :parent border-node)))
      (values tree (list shadow-node border-node title-bar-node content-node)))))

(defun make-surface-nodes ()
  (let* ((corner-radius    8)
         (border-width     4)
         (border-color     #xff0000ff))
    (values (make-instance 'outset-shadow
                           :x               0.0f0
                           :y               0.0f0
                           :width           1.0f0
                           :height          1.0f0

                           :top-radius-x    (float corner-radius 1.0f0)
                           :top-radius-y    (float corner-radius 1.0f0)
                           :right-radius-x  (float corner-radius 1.0f0)
                           :right-radius-y  (float corner-radius 1.0f0)
                           :bottom-radius-x (float corner-radius 1.0f0)
                           :bottom-radius-y (float corner-radius 1.0f0)
                           :left-radius-x   (float corner-radius 1.0f0)
                           :left-radius-y   (float corner-radius 1.0f0)

                           :red             0
                           :green           0
                           :blue            0
                           :alpha           128

                           :dx              4.0f0
                           :dy              4.0f0
                           :spread          4.0f0
                           :blur            4.0f0)
            (make-instance 'border :x               0.0f0
                                   :y               0.0f0
                                   :width           1.0f0
                                   :height          1.0f0

                                   :top-radius-x    (float corner-radius 1.0f0)
                                   :top-radius-y    (float corner-radius 1.0f0)
                                   :right-radius-x  (float corner-radius 1.0f0)
                                   :right-radius-y  (float corner-radius 1.0f0)
                                   :bottom-radius-x (float corner-radius 1.0f0)
                                   :bottom-radius-y (float corner-radius 1.0f0)
                                   :left-radius-x   (float corner-radius 1.0f0)
                                   :left-radius-y   (float corner-radius 1.0f0)

                                   :top-width     (float border-width 1.0f0)
                                   :right-width   (float border-width 1.0f0)
                                   :bottom-width  (float border-width 1.0f0)
                                   :left-width    (float border-width 1.0f0)

                                   :top-color     border-color
                                   :right-color   border-color
                                   :bottom-color  border-color
                                   :left-color    border-color)
            (make-instance 'color :x      0.0f0
                                  :y      0.0f0
                                  :width  1.0f0
                                  :height 20.0f0

                                  :red    128
                                  :green  128
                                  :blue   255
                                  :alpha  200)
            (make-instance 'color :x      0.0f0
                                  :y      20.0f0
                                  :width  1.0f0
                                  :height 1.0f0

                                  :red    128
                                  :green  255
                                  :blue   128
                                  :alpha  200)
            #+no (make-instance 'transform :kind  0
                                      :dx    0.0f0
                                      :dy   20.0f0))))

(defun resize-surface-nodes (tree shadow border title-bar content width height)
  ; (declare (ignore content))
  (let* ((border-width     4)

         (effective-width  (+ width (* 2 border-width)))
         (effective-height (+ height 20 (* 2 border-width)))

         (tiles            (make-tiles width height 256)))
    (reinitialize-instance (data shadow)    :width  (float effective-width  1.0f0)
                                            :height (float effective-height 1.0f0))
    (reinitialize-instance (data border)    :width  (float effective-width  1.0f0)
                                            :height (float effective-height 1.0f0))
    (reinitialize-instance (data title-bar) :width  (float width            1.0f0))
    (reinitialize-instance (data content)   :width  (float width            1.0f0)
                                            :height (float height           1.0f0))
    (values (make-tile-nodes tree content tiles) tiles)))

(defun make-tile-nodes (tree parent tiles)
  (let ((id 0))
    (map 'list (lambda (tile)
                 (with-bounding-rectangle* (x1 y1 x2 y2) (region tile)
                   (let ((texture (make-instance  'texture
                                                  :id     (incf id)
                                                  :x      (float x1        1.0f0)
                                                  :y      (float y1        1.0f0)
                                                  :width  (float (- x2 x1) 1.0f0)
                                                  :height (float (- y2 y1) 1.0f0)

                                                  ; :red    (random 255)
                                                  ; :green  (random 255)
                                                  ; :blue   (random 255)
                                                  ; :alpha  200
                                                  )))
                     (make-node texture tree :parent parent))))
         tiles)))
