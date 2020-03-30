;;;; (C) Copyright 2019, 2020 Jan Moringen
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

(defclass broadway-medium (basic-medium
                           climb:multiline-text-medium-mixin
                           climb:font-rendering-medium-mixin)
  (#+maybe (%port :initarg :port
                  :reader  port)
   (%ink-dirty-p :initarg :ink-dirty-p ; TODO make a bitfield for the whole state
                 :accessor ink-dirty-p
                 :initform nil)
   (%text-style-dirty-p :initarg :text-style-dirty-p ; TODO make a bitfield for the whole state
                 :accessor text-style-dirty-p
                 :initform nil)))

(defvar *medium*)

#+no (let ((climi::*default-server-path* :clx-ttf))
  (clouseau:inspect *medium*))

(defmethod synchronize-graphics-state ((medium broadway-medium))
  (when (ink-dirty-p medium)
    (let* ((sheet   (medium-sheet medium))
           (port    (port sheet))
           (surface (climi::port-lookup-mirror port sheet))
           (ink     (medium-ink medium)))
      (multiple-value-bind (red green blue alpha)
          (clime:color-rgba (climi::design-ink ink 0 0))
        (push (make-instance 'set-color :red   (floor red   1/255)
                                        :green (floor green 1/255)
                                        :blue  (floor blue  1/255)
                                        :alpha (floor alpha 1/255))
              (queued-operations surface))))
    (setf (ink-dirty-p medium) nil))

  (when (text-style-dirty-p medium)
    (let* ((sheet      (medium-sheet medium))
           (port       (port sheet))
           (surface    (climi::port-lookup-mirror port sheet))
           (text-style (medium-merged-text-style medium))) ; TODO expensive
      (multiple-value-bind (family face size)
          (text-style-components text-style)
        (push (make-instance 'set-font :family "Arial"
                                       :size   (case size
                                                 (:small   8.0f0)
                                                 (:normal 10.0f0)
                                                 (:huge   12.0f0)
                                                 (t       (float size 1.0f0))))
              (queued-operations surface))))
    (setf (text-style-dirty-p medium) nil)))

;; TODO track actual client-side state (i.e. only state that has been
;; transmitted to the client)
(defmethod (setf medium-ink) :after ((new-value t) (medium broadway-medium))
  (setf (ink-dirty-p medium) t))

(defmethod (setf medium-text-style) :after ((new-value t) (medium broadway-medium))
  (setf (text-style-dirty-p medium) t))

(defmethod medium-clear-area ((medium broadway-medium) left top right bottom)
                                        ; (break)
  (let* ((sheet   (medium-sheet medium))
         (port    (port sheet))
         (surface (climi::port-lookup-mirror port sheet)))
    (push (make-instance 'set-color :red 128 :green 128 :blue 128 :alpha 255)
          (queued-operations surface))
    ;; TODO context.clearRect()
    (push (make-instance 'draw-rectangle :x1 (float left 1.0f0)
                                         :y1 (float top 1.0f0)
                                         :x2 (float (- right left) 1.0f0) ; TODO
                                         :y2 (float (- bottom top) 1.0f0))
          (queued-operations surface))))

(defmethod medium-draw-point* ((medium broadway-medium) x y)
  (let* ((sheet   (medium-sheet medium))
         (port    (port sheet))
         (surface (climi::port-lookup-mirror port sheet)))

    (synchronize-graphics-state medium)

    (multiple-value-bind (x y)
        (transform-position (sheet-native-transformation sheet) x y)
      #+no (push (make-instance 'draw-line :x1 (float x1 1.0f0)
                                      :y1 (float y1 1.0f0)
                                      :x2 (float x2 1.0f0)
                                      :y2 (float y2 1.0f0))
            (queued-operations surface)))))

(defmethod medium-draw-line* ((medium broadway-medium) x1 y1 x2 y2)
  (let* ((sheet   (medium-sheet medium))
         (port    (port sheet))
         (surface (climi::port-lookup-mirror port sheet)))

    (synchronize-graphics-state medium)

    (multiple-value-bind (x1 y1)
        (transform-position (sheet-native-transformation sheet) x1 y1)
      (multiple-value-bind (x2 y2)
          (transform-position (sheet-native-transformation sheet) x2 y2)

        (push (make-instance 'draw-line :x1 (float x1 1.0f0)
                                        :y1 (float y1 1.0f0)
                                        :x2 (float x2 1.0f0)
                                        :y2 (float y2 1.0f0))
              (queued-operations surface))))))

(defmethod medium-draw-rectangle* ((medium broadway-medium) x1 y1 x2 y2 filled)
  (when-let* ((sheet   (medium-sheet medium))
              (port    (port sheet))
              (surface (climi::port-lookup-mirror port sheet)))

    (synchronize-graphics-state medium)

    (multiple-value-bind (x1 y1 x2 y2)
        (transform-rectangle* (sheet-native-transformation sheet) x1 y1 x2 y2)
      (push (make-instance 'draw-rectangle :x1 (float x1 1.0f0)
                                           :y1 (float y1 1.0f0)
                                           :x2 (float (- x2 x1) 1.0f0) ; TODO
                                           :y2 (float (- y2 y1) 1.0f0))
            (queued-operations surface)))
    (log:info "~D queued operation~:P" (length (queued-operations surface)))))

(defmethod medium-draw-polygon* ((medium broadway-medium) coord-seq closed filled)
  (let* ((sheet   (medium-sheet medium))
         (port    (port sheet))
         (surface (climi::port-lookup-mirror port sheet)))

    (synchronize-graphics-state medium)

    #+no (multiple-value-bind (x1 y1 x2 y2)
        (transform- (sheet-native-transformation sheet) x1 y1 x2 y2)
      (push (make-instance 'draw-rectangle :x1 (float x1 1.0f0)
                                           :y1 (float y1 1.0f0)
                                           :x2 (float (- x2 x1) 1.0f0) ; TODO
                                           :y2 (float (- y2 y1) 1.0f0))
            (queued-operations surface)))))

(defmethod medium-draw-ellipse* ((medium broadway-medium) x y radius-1-dx radius-1-dy radius-2-dx radius-2-dy start-angle end-angle filled)
  (let* ((sheet   (medium-sheet medium))
         (port    (port sheet))
         (surface (climi::port-lookup-mirror port sheet)))

    (synchronize-graphics-state medium)

    #+no (multiple-value-bind (x1 y1 x2 y2)
        (transform-rectangle* (sheet-native-transformation sheet) x1 y1 x2 y2)
      (push (make-instance 'draw-rectangle :x1 (float x1 1.0f0)
                                           :y1 (float y1 1.0f0)
                                           :x2 (float (- x2 x1) 1.0f0) ; TODO
                                           :y2 (float (- y2 y1) 1.0f0))
            (queued-operations surface)))))

(defmethod climb:font-text-extents ((medium broadway-medium) text
                                    &key start end)
  )

(defmethod medium-draw-text* ((medium broadway-medium) string x y start end
                              align-x align-y toward-x toward-y transform-glyphs)
  (let* ((sheet   (medium-sheet medium))
         (port    (port sheet))
         (surface (climi::port-lookup-mirror port sheet)))

    ; (setf (text-align medium) align-x) => context.textAlign = "center"
    ; (setf (text-align medium) align-y)
    (synchronize-graphics-state medium)

    (multiple-value-bind (x y)
        (transform-position (sheet-native-transformation sheet) x y)
      (push (make-instance 'draw-text :x    (float x 1.0f0)
                                      :y    (float y 1.0f0)
                                      :text (subseq string start end))
            (queued-operations surface)))))
