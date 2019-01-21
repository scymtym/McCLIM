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

;;;

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :brief))
                                       (stream t))
  (call-with-safe-and-terse-printing
   (lambda () (prin1 object stream))))

;;; Expanded

(defmethod inspect-object-using-state :around ((object t)
                                               (state  inspected-object)
                                               (style  (eql :expanded))
                                               (stream t))
  (with-object-border (stream 0)
    (call-next-method object state style stream)))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :expanded))
                                       (stream t))
  (formatting-table (stream)
    (formatting-column (stream)
      (formatting-row (stream)
        (formatting-cell (stream)
          (with-style (stream :header)
            (inspect-object-using-state object state :expanded-header stream))))
      (formatting-row (stream)
        (formatting-cell (stream)
          (inspect-object-using-state object state :expanded-body stream))))))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (call-with-safe-and-terse-printing
   (lambda () (prin1 object stream))))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t)))
