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

(defclass html-push-button (clim:push-button
                            mirrored-sheet-mixin)
  ())

(defmethod make-tree ((sheet html-push-button))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-native-region sheet)
    (make-instance 'text :x      (float x1 0.0f0)
                         :y      (float y1 0.0f0)
                         :width  (float (- x2 x1) 0.0f0)
                         :height (float (- y2 y1) 0.0f0)

                         :color  #xffd0d0d0

                         :text   (clim:gadget-label sheet))))

(defmethod find-concrete-pane-class ((realizer broadway-frame-manager)
                                     (pane-type (eql :push-button))
                                     &optional errorp)
  (find-class 'html-push-button errorp))
