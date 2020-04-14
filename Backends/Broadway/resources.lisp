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

(defun resource-filename (name)
  (merge-pathnames
   name (merge-pathnames "static/" #.(or *compile-file-truename*
                                         *load-truename*))))

(defclass resource ()
  ((%name      :initarg  :name
               :reader   name)
   (%filename  :reader   filename
               :accessor %filename)
   (%content   :reader   content
               :accessor %content)
   (%timestamp :reader   timestamp
               :accessor %timestamp)))

(defmethod shared-initialize :after
    ((instance resource) (slot-names t)
     &key (name      nil name-supplied?)
          (filename  (when name-supplied?
                       (resource-filename name))
                     filename-supplied?)
          (content   (when (or name-supplied? filename-supplied?)
                       (read-file-into-string filename))
                     content-supplied?)
          (timestamp (when (or name-supplied? filename-supplied?)
                       (file-write-date filename))
                     timestamp-supplied?))
  (let ((new-file? (or name-supplied? filename-supplied?)))
    (when new-file?
      (setf (%filename instance) filename))
    (when (or new-file? content-supplied?)
      (setf (%content instance) content))
    (when (or new-file? timestamp-supplied?)
      (setf (%timestamp instance) timestamp))))

(defmethod print-object ((object resource) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write-string (name object) stream)))

(defmethod maybe-reload ((resource resource))
  (let* ((filename   (filename resource))
         (write-date (file-write-date filename)))
    (when (> write-date (timestamp resource))
      (format *trace-output* "Reloading ~A~%" resource)
      (setf (%content resource)   (read-file-into-string filename)
            (%timestamp resource) write-date))))

(defvar *resources* (make-hash-table :test #'equal))

(defun find-resource (name)
  (gethash name *resources*))

(defun (setf find-resource) (new-value name)
  (setf (gethash name *resources*) new-value))

(defun maybe-reload-resources ()
  (maphash-values #'maybe-reload *resources*))

(defmacro define-resource (name)
  `(setf (find-resource ',name) (make-instance 'resource :name ',name)))

;;; Resource definitions

(define-resource "client.html")

(define-resource "broadway.js")
