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

;;; Object inspection methods

(defmethod inspect-object-using-state ((object number)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (format stream "~A ~A" (type-of object) object)) ; TODO type inspectable?

(defmethod inspect-object-using-state ((object integer)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  )

(defmethod inspect-object-using-state ((object ratio)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (numerator object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Numerator" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream)))

      (formatting-place (stream object 'pseudo-place (denominator object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Denominator" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))))

(defmethod inspect-object-using-state ((object float)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (multiple-value-bind (significand exponent sign) (decode-float object)
      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place sign present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Sign" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream))))

      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place significand present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Significand" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream))))

      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place exponent present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Exponent" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))))

    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (float-radix object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Radix" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))))

(defmethod inspect-object-using-state ((object complex)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (type-of (realpart object)) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Part type" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))

    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (realpart object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Real part" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream)))

      (formatting-place (stream object 'pseudo-place (imagpart object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Imaginary part" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))

    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (abs object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Magnitude" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream)))

      (formatting-place (stream object 'pseudo-place (phase object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Phase" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))))
