(cl:in-package #:clim-broadway)

(defclass surface (surface1)
  ((%name        :initarg  :name
                 :accessor name)
   ;; Nodes
   (%old-tree    :accessor old-tree
                 :initform (make-instance 'tree))
   (%tree        :reader   tree
                 :writer   (setf %tree))
   (%nodes       :reader   nodes
                 :writer   (setf %nodes))
   ;; Tiles
   (%textures    :accessor textures)
   (%tiles       :accessor tiles)))

(defmethod initialize-instance :after ((instance surface) &key)
  (setf (values (%tree instance) (%nodes instance)) (make-surface-tree instance)))

(defun make-surface-tree (surface)
  (multiple-value-bind (shadow border title-bar title-text close-button content)
      (make-surface-nodes surface)
    (let* ((tree              (make-instance 'tree :root shadow))
           (shadow-node       (root tree))
           (border-node       (make-node border       tree :parent shadow-node))
           (title-bar-node    (make-node title-bar    tree :parent border-node))
           (title-text-node   (make-node title-text   tree :parent title-bar-node))
           (close-button-node (make-node close-button tree :parent title-bar-node))
           (content-node      (make-node content      tree :parent border-node)))
      (values tree (list shadow-node border-node title-bar-node title-text-node close-button-node content-node)))))

(defun make-surface-nodes (surface)
  (let* ((corner-radius    8)

         (border-width     4)
         (border-color     #xff404040)

         (title-bar-color  #xff404040)
         (title-bar-height 20)

         (title-text-color #xffc0c0c0))
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
            (make-instance 'border
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

                           :top-width     (float border-width 1.0f0)
                           :right-width   (float border-width 1.0f0)
                           :bottom-width  (float border-width 1.0f0)
                           :left-width    (float border-width 1.0f0)

                           :top-color     border-color
                           :right-color   border-color
                           :bottom-color  border-color
                           :left-color    border-color)
            (make-instance 'color
                           :x      0.0f0
                           :y      0.0f0
                           :width  1.0f0
                           :height (float title-bar-height 0.0f0)

                           :red    (ldb (byte 8  0) title-bar-color)
                           :green  (ldb (byte 8  8) title-bar-color)
                           :blue   (ldb (byte 8 16) title-bar-color)
                           :alpha  (ldb (byte 8 24) title-bar-color))
            (make-instance 'text
                           :x      0.0f0
                           :y      0.0f0
                           :width  1.0f0
                           :height (float title-bar-height 0.0f0)

                           :color  title-text-color
                           :text   (name surface))
            (make-instance 'text
                           :x      0.0f0
                           :y      0.0f0
                           :width  (float title-bar-height 0.0f0)
                           :height (float title-bar-height 0.0f0)

                           :color  title-text-color
                           :text   "×")
            #+no (make-instance 'color :x      0.0f0
                                       :y      20.0f0
                                       :width  1.0f0
                                       :height 1.0f0

                                       :red    128
                                       :green  255
                                       :blue   128
                                       :alpha  200)
            (make-instance 'canvas :x      0.0f0
                                   :y      (float title-bar-height 0.0f0)
                                   :width  1.0f0
                                   :height 1.0f0)
            #+no (make-instance 'transform :kind  0
                                      :dx    0.0f0
                                      :dy   20.0f0))))

(defun resize-surface-nodes (tree shadow border title-bar title-text close-button content width height)
  ; (declare (ignore content))
  (let* ((border-width     4)

         (title-bar-height 20)

         (effective-width  (+ width (* 2 border-width)))
         (effective-height (+ height title-bar-height (* 2 border-width)))

         ; (tiles            (make-tiles width height 256))
         )
    (reinitialize-instance (data shadow)       :width  (float effective-width  1.0f0)
                                               :height (float effective-height 1.0f0))
    (reinitialize-instance (data border)       :width  (float effective-width  1.0f0)
                                               :height (float effective-height 1.0f0))
    (reinitialize-instance (data title-bar)    :width  (float width            1.0f0))
    (reinitialize-instance (data title-text)   :width  (float width            1.0f0))
    (reinitialize-instance (data close-button) :x      (float (- width title-bar-height) 1.0f0))
    (reinitialize-instance (data content)      :width  (float width            1.0f0)
                                               :height (float height           1.0f0))
    ; (values (make-tile-nodes tree content tiles) tiles)
    ))

(defmethod (setf name) :after ((new-value string) (object surface))
  (let ((title-text (nth 3 (nodes object))))
    (reinitialize-instance (data title-text) :text new-value)))

;;; Old

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
