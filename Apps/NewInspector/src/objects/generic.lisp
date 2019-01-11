(cl:in-package #:new-inspector)

;;; Utilities

(defun call-with-safe-and-terse-printing (thunk)
  (let ((*print-circle* t)
        (*print-length* 3)
        (*print-level*  3))
    (funcall thunk)))

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
  (surrounding-output-with-border (stream :shape :rectangle)
    (call-next-method)))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :expanded))
                                       (stream t))
  (formatting-table (stream)
    (formatting-column (stream)
      (formatting-cell (stream)
        (with-drawing-options (stream :text-face :bold)
          (inspect-object-using-state object state :expanded-header stream)))
      (formatting-cell (stream)
        (inspect-object-using-state object state :expanded-body stream)))))

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

(defun print-documentation (object pane)
  "Print OBJECT's documentation, if any, to PANE"
  (when-let ((documentation (handler-case (documentation object t)
                              (error ())
                              (warning ()))))
    (surrounding-output-with-border (pane :shape      :rectangle
                                          :padding    2
                                          :background +beige+
                                          :outline-ink +light-goldenrod+
                                          :filled     t)
      (write-string documentation pane))))
