;;;; Copyright (C) 2019 Jan Moringen
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

;;; Object inspection methods

(defmethod inspect-object-using-state ((object character)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (format stream "~A ~A" (type-of object) object) ; TODO type inspectable?
  (macrolet ((attribute (predicate label)
               `(when (,predicate object)
                  (write-char #\Space stream)
                  (badge stream ,label))))
    (attribute graphic-char-p "graphic")
    (attribute digit-char-p   "digit")
    (attribute alpha-char-p   "alpha")
    (attribute alphanumericp  "alphanumeric")
    (attribute upper-case-p   "upper-case")
    (attribute lower-case-p   "lower-case")))

(defmethod inspect-object-using-state ((object character)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (char-name object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Name" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))
    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (char-code object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Code" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))
    (when-let ((weight (digit-char-p object)))
      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place weight present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Weight" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))))))
