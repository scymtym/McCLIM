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

;;;

(defun print-documentation (object stream)
  (when-let ((documentation (handler-case (documentation object t)
                              (error ())
                              (warning ()))))
    (with-preserved-cursor-x (stream)
      (surrounding-output-with-border (stream :shape      :rectangle
                                              :padding    2
                                              :background +beige+
                                              :outline-ink +light-goldenrod+
                                              :filled     t)
        (write-string documentation stream)))))
