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

(defclass surface (surface1)
  ((%name        :initarg  :name
                 :accessor name)
   (%sheet       :initarg  :sheet
                 :reader   sheet)
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

(defmethod nodes-dirty-p ((surface surface))
  (dirty-p (tree surface)))

(defmethod (setf nodes-dirty-p) ((new-value t) (surface surface))
  (setf (dirty-p (tree surface)) new-value))

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

                           :color  title-bar-color)
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
                                   :height 1.0f0
                                   :id     (id surface))
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
    (update-node shadow       :width  (float effective-width  1.0f0)
                              :height (float effective-height 1.0f0))
    (update-node border       :width  (float effective-width  1.0f0)
                              :height (float effective-height 1.0f0))
    (update-node title-bar    :width  (float width            1.0f0))
    (update-node title-text   :width  (float width            1.0f0))
    (update-node close-button :x      (float (- width title-bar-height) 1.0f0))
    (update-node content      :width  (float width            1.0f0)
                              :height (float height           1.0f0))
                                        ; (values (make-tile-nodes tree content tiles) tiles)
    ))

(defmethod width ((object surface))
  (width (data (nth 4 (nodes object)))))

(defmethod height ((object surface))
  (height (data (nth 4 (nodes object)))))

(defmethod (setf name) :after ((new-value string) (object surface))
  (let ((title-text (nth 3 (nodes object))))
    (update-node title-text :text new-value)))

(defmethod (setf focusedp) ((new-value t) (object surface))
  (let ((color     (if new-value #xff303030 #xff606060))
        (border    (nth 1 (nodes object)))
        (title-bar (nth 2 (nodes object))))
    (update-node border :top-color    color
                        :right-color  color
                        :left-color   color
                        :bottom-color color)
    (update-node title-bar :color color)))

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
