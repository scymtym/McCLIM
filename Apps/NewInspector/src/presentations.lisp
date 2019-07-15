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

;;; Place presentations
;;;
;;; These visually represent the in which an inspected object
;;; resides. Also indicates whether the place can be changed (by
;;; assigning a different value or removing the value).

(define-presentation-type place ())

(define-presentation-method present :around ((object basic-place)
                                             (type   place)
                                             (stream t)
                                             (view   inspector-view)
                                             &key)
  ;; Present the place with the "changable" style if its value can be
  ;; changed or removed.
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
           :initform :brief)
   (%occurrences :accessor occurrences
                 :initform '())))

(defmethod object ((object inspected-object))
  (value (place object)))

(defmethod state-applicable-p ((state t) (object t) (place t))
  (eq (class-name (class-of state)) (object-state-class object place))) ; TODO compare the metaobjects?

(defmethod object-state-class ((object t) (place t))
  'inspected-object)

(defmethod make-object-state ((object t) (place t))
  (let ((class (object-state-class object place)))
    (make-instance class :place place)))

(define-presentation-type inspected-object ()) ; TODO probably not needed

;;; Indicating circular structure using presentation highlighting

(flet ((map-other-occurrences (function presentation)
         (map nil (lambda (other)
                    (unless (eq other presentation)
                      (funcall function other)))
              (cdr (occurrences (presentation-object presentation)))))
       (draw-arrow (stream from to ink)
         (multiple-value-bind (x1 y1) (bounding-rectangle-position from)
           (multiple-value-bind (x2 y2) (bounding-rectangle-position to)
             (let ((design (mcclim-bezier:make-bezier-curve*
                            (list x1              y1
                                  (lerp .3 x1 x2) y1
                                  x2              (lerp .7 y1 y2)
                                  x2              y2))))
               (mcclim-bezier:draw-bezier-design*
                stream design :ink ink :line-thickness 2)
               (draw-arrow* stream
                            x2 (lerp .99 y1 y2)
                            x2               y2
                            :ink ink :line-thickness 2))))
         #+no (multiple-value-call #'draw-arrow* stream
                (bounding-rectangle-position from)
                (bounding-rectangle-position to)
                :ink ink :line-thickness 2)))

  (define-presentation-method highlight-presentation
    :after ((type   inspected-object)
            (record t)
            (stream t)
            (state  (eql :highlight)))

    (let ((i 0))
      (map-other-occurrences
       (lambda (other-presentation)
         (let ((ink (make-contrasting-inks 8 (mod i 8))))
           (when (zerop i)
             (multiple-value-call #'draw-circle* stream
               (bounding-rectangle-position record) 5 :ink ink))
           (draw-arrow stream record other-presentation ink))
         (incf i))
       record)))

  (define-presentation-method highlight-presentation
    :after ((type   inspected-object)
            (record t)
            (stream t)
            (state  (eql :unhighlight)))
    (let ((i      0)
          (region +nowhere+))
      (map-other-occurrences
       (lambda (other-presentation)
         (let ((new-region
                 (with-output-to-output-record (stream)
                   (when (zerop i)
                     (multiple-value-call #'draw-circle* stream
                       (bounding-rectangle-position record) 5))
                   ;; TODO can we just use the design as the region here?
                   (draw-arrow stream record other-presentation +black+))))
           (setf region (region-union region new-region)))
         (incf i))
       record)
      (unless (eq region +nowhere+)
        (repaint-sheet stream region)))))
