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

(defclass broadway-frame-manager (frame-manager)
  ())

;;; Frames

(defmethod adopt-frame :before ((frame-manager broadway-frame-manager)
                                (frame         climi::menu-frame))
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (setf (slot-value frame 'climi::left) 1 ; HACK can't result in identity transformation?
          (slot-value frame 'climi::top)  1)))

;;; Panes

(defclass top-level-sheet-pane (; climi::always-repaint-background-mixin
                                mirrored-sheet-mixin
                                climi::double-buffering-mixin
                                climi::top-level-sheet-pane)
  ()
  (:default-initargs
   :device-transformation +identity-transformation+)) ; TODO HACK

(defmethod make-pane-1 ((realizer            broadway-frame-manager)
                        (frame               application-frame)
                        (abstract-class-name (eql 'climi::top-level-sheet-pane))
                        &rest initargs)
  (apply #'make-instance 'top-level-sheet-pane
         :frame   frame
         :manager realizer
         :port    (port frame)
         initargs))

(defmethod make-pane-1 ((realizer            broadway-frame-manager)
                        (frame               application-frame)
                        (abstract-class-name t)
                        &rest initargs)
  (apply #'make-instance (clim-clx::find-concrete-pane-class abstract-class-name)
         :frame   frame
         :manager realizer
         :port    (port frame)
         initargs))
