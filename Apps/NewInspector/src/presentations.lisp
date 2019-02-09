;;;; Copyright (C) 2018, 2019 Jan Moringen
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

(cl:in-package #:new-inspector)

;;; `inspector-view'
;;;
;;; Basically just something that is distinguishable from
;;; `textual-view'.

(defclass inspector-view (textual-view)
  ())

;;; `changable'
;;;
;;; Indicates that the place represented by the presentation can be
;;; changed (by assigning a different value or removing the value).

(define-presentation-type changable ()) ; TODO I think this is not used

(define-presentation-method present :around ((object t)
                                             (type   changable)
                                             (stream t)
                                             (view   textual-view)
                                             &key)
  (with-style (stream :changable) (call-next-method)))

(define-presentation-method present ((object t) ; TODO used? even harmful?
                                     (type   changable)
                                     (stream t)
                                     (view   textual-view)
                                     &key)
  (let ((cell (cell object)))
    (present cell (presentation-type-of cell) :stream stream :view view)))

;;;

(define-presentation-type slot-like ()) ; TODO I think this is not used

(define-presentation-method present :around (object
                                             (type slot-like)
                                             stream
                                             (view textual-view)
                                             &key)
  (with-style (stream :slot-like) (call-next-method)))

;;;

(define-presentation-type place () ; TODO do we need this?
  ; :inherit-from 'changable
  )

;;; Place presentations

(define-presentation-method present :around ((object basic-place)
                                             (type   place)
                                             (stream t)
                                             (view   inspector-view)
                                             &key)
  (if (or (supportsp object 'setf) (supportsp object 'remove-value))
      (with-style (stream :changable) (call-next-method))
      (call-next-method)))

(define-presentation-method present ((object basic-place) ; TODO macro
                                     (type   place)
                                     (stream t)
                                     (view   inspector-view)
                                     &key)
  (write-char #\→ stream))

(define-presentation-method present ((object sequence-element-place)
                                     (type   place)
                                     (stream t)
                                     (view   inspector-view)
                                     &key)
  (write-char #\⁃ stream))

(define-presentation-method present ((object key-place)
                                     (type   place)
                                     (stream t)
                                     (view   inspector-view)
                                     &key)
  (write-char #\• stream))

(define-presentation-method present ((object value-place)
                                     (type   place)
                                     (stream t)
                                     (view   inspector-view)
                                     &key)
  (write-char #\→ stream))

;;; `inspected-object'

(defclass inspected-object ()
  ((%place :initarg  :place
           :reader   place)
   (%style :initarg  :style
           :accessor style
           :initform :brief)))

(defmethod object ((object inspected-object))
  (value (place object)))

(defmethod state-applicable-p ((state t) (object t) (place t))
  (eq (class-name (class-of state)) (object-state-class object place))) ; TODO compare the metaobjects?

(defmethod object-state-class ((object t) (place t))
  'inspected-object)

(defmethod make-object-state ((object t) (place t))
  (let ((class (object-state-class object place)))
    (make-instance class)))

(define-presentation-type inspected-object ()) ; TODO probably not needed
