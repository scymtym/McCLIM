;;;  Copyright 2018 Jan Moringen (jmoringe@techfak.uni-bielefeld.de

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clim-internals)

(deftype orientation ()
  '(member :vertical :horizontal))

(declaim (type orientation *orientation*))
(defvar *orientation*)

(defmacro with-orientation ((orientation) &body body)
  `(let ((*orientation* ,orientation))
     ,@body))

(declaim (inline bounding-rectangle*/orientation))
(defun bounding-rectangle*/orientation (region)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (ecase *orientation*
      (:vertical (values y1 x1 y2 x2))
      (:horizontal (values x1 y1 x2 y2)))))

;; :vertical => major is height
(defun make-space-requirement/orientation (&key
                                           (min-major 0) ; height
                                           (min-minor 0)
                                           (major min-major)
                                           (minor min-minor)
                                           (max-major +fill+)
                                           (max-minor +fill+))
  (ecase *orientation*
    (:vertical (make-space-requirement :min-width min-minor
                                       :min-height min-major
                                       :width minor
                                       :height major
                                       :max-width max-minor
                                       :max-height max-major))
    (:horizontal (make-space-requirement :min-width min-major
                                         :min-height min-minor
                                         :width major
                                         :height minor
                                         :max-width max-major
                                         :max-height max-minor))))
